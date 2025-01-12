pub mod map;
pub mod visit;

use arrayvec::ArrayVec;
use bitvec::array::BitArray;
use map::Map;
use visit::{Enter, Visit};

use super::{HexDivNode, NodeRef, Parent};
use crate::hex_div::{
    bounds::{Bounds, CachedBounds},
    extent::{HasCachedExtent, Splittable},
    splits::{SplitList, Splits},
};

/// An [`Iterator`] over a pair of [`Bounds`] and contents of a [`HexDivNode`].
///
/// All [`Bounds`] are guaranteed to be returned in a depth-first manner. Only iterators that follow
/// this order may implement this trait.
///
/// This trait itself however does allows skipping certain nodes via
/// [`HexDivNodeIterator::skip_children`] and [`HexDivNodeIterator::only_child`]. Note however, that
/// an iterator adaptor that uses these functions to filter out nodes may no longer implement
/// [`HexDivNodeIterator`]. E.g. [`Visit`], which does just that, does not implement
/// [`HexDivNodeIterator`], but [`Map`] does, since it still returns the same [`Bounds`] with a
/// modified [`HexDivNodeIterator::Node`].
pub trait HexDivNodeIterator: Iterator<Item = (CachedBounds, Self::Node)> {
    /// The node-related value that is returned along with each [`Bounds`].
    type Node;

    /// Skip certain child nodes based on the given `enter`.
    fn enter(&mut self, enter: Option<Enter>) {
        match enter {
            None => self.skip_children(),
            Some(Enter::Only { child }) => self.only_child(child),
            Some(Enter::All) => {}
        }
    }

    /// Skips over the children of the previous node.
    ///
    /// # Panics
    ///
    /// Panics if the previous node cannot be entered, since it will get skipped automatically.
    ///
    /// Panics if [`HexDivNodeIterator::skip_children`] or [`HexDivNodeIterator::only_child`] was
    /// already called before calling [`Iter::next`].
    fn skip_children(&mut self);

    /// Only enters the child with the given `index` of the previous node.
    ///
    /// # Panics
    ///
    /// Panics if the previous node cannot be entered.
    ///
    /// Panics if [`HexDivNodeIterator::skip_children`] or [`HexDivNodeIterator::only_child`] was
    /// already called before calling [`Iter::next`].
    ///
    /// Panics if the index is out of bounds.
    fn only_child(&mut self, index: u8);

    /// Efficiently filters out entire parent nodes based on a callback.
    ///
    /// This calls [`HexDivNodeIterator::skip_children`] and [`HexDivNodeIterator::only_child`]
    /// based on the callback.
    ///
    /// The resulting iterator is no longer a [`HexDivNodeIterator`], since it no longer returns all
    /// [`Bounds`], which [`HexDivNodeIterator`] requires.
    fn visit<V>(self, visit: V) -> Visit<Self, V>
    where
        Self: Sized,
        V: FnMut(Splittable<CachedBounds>, Parent<Self::Node>) -> Option<Enter>,
    {
        Visit::new(self, visit)
    }

    /// Takes a closure and creates an iterator which calls that closure on each node.
    ///
    /// Similar to [`Iterator::map`], but does not modifiy the [`Bounds`].
    ///
    /// The resulting iterator is still [`HexDivNodeIterator`], since the [`Bounds`] are still
    /// valid.
    fn hex_div_map<B, F>(self, f: F) -> Map<Self, F>
    where
        Self: Sized,
        F: FnMut(Bounds, Self::Node) -> B,
    {
        Map::new(self, f)
    }
}

pub trait IntoHexDivNodeIterator: IntoIterator<IntoIter: HexDivNodeIterator> {}
impl<T: IntoIterator<IntoIter: HexDivNodeIterator>> IntoHexDivNodeIterator for T {}

/// An iterator that iterates over the nodes of a [`HexDivNode`].
///
/// By default, all nodes are visited:
///
/// ```
/// # use unbound_lib::hex_div::{
/// #     extent::Extent,
/// #     node::{HexDivNode, Node},
/// # };
/// let root = Node::<u32>::with_default(Extent::ONE);
///
/// for (bounds, node) in &root {
///     // all parent and leaf nodes are yielded
///     // in this example this is of course only the root leaf node itself
/// }
/// ```
///
/// However, since [`HexDivNode`]'s are intended to cover potentially huge areas, this is often not
/// feasible. For this reason this type provides various means to skip over specific nodes including
/// all of their children.
///
/// The most straightforward way is to use [`Self::visit`]. It combines the iterator with a callback
/// that can decide which nodes to enter.
///
/// ```
/// # use unbound_lib::hex_div::{
/// #     extent::Extent,
/// #     node::{iter::HexDivNodeIterator, HexDivNode, Node},
/// # };
/// # let root = Node::<u32>::with_default(Extent::ONE);
/// let filtered = root.iter().visit(|bounds, node| {
///     // filter e.g. based on bounds
///     None
/// });
///
/// for (bounds, node) in filtered {
///     // callback prevents uninteresting parent nodes from being entered
/// }
/// ```
///
/// Unfortunately, there are cases where this approach doesn't work. Most notably, when trying to
/// iterate multiple [`HexDivNode`]s at once (similar to e.g. [`std::iter::zip`]) this can lead to
/// issues, if the two visit callbacks need to communicate with each other. While this can in theory
/// be solved with e.g. [interior mutability], there is a way to avoid the callback entirely:
///
/// ```
/// # use unbound_lib::hex_div::{
/// #     extent::Extent,
/// #     node::{iter::HexDivNodeIterator, HexDivNode, IsParent, Node},
/// # };
/// # let root = Node::<u32>::with_default(Extent::ONE);
/// let mut iter = root.iter();
/// while let Some((bounds, node)) = iter.next() {
///     // manual implementation of the visit callback from the previous example
///     if node.is_parent() {
///         iter.skip_children();
///     }
/// }
/// ```
///
/// This is exactly what [`Visit`] does under the hood; see [`Visit::next`] for the implementation.
///
/// Besides [`Self::skip_children`], which is fairly straightforward, there is also
/// [`Self::only_child`], which can focus the iteration down a single chain of specific children
/// while efficiently skipping over the remaining (up to) 63 neighboring nodes for each layer:
///
/// ```
/// # use unbound_lib::hex_div::{
/// #     extent::Extent,
/// #     node::{iter::HexDivNodeIterator, HexDivNode, IsParent, Node},
/// # };
/// # let root = Node::<u32>::with_default(Extent::ONE);
/// let mut iter = root.iter();
/// while let Some((bounds, node)) = iter.next() {
///     // only enter child 42
///     if node.is_parent() {
///         iter.only_child(42);
///     }
/// }
/// ```
///
/// Note, that [`Self::only_child`] really only supports visiting a single child for any given
/// parent node. If even just a second child is of interest, the default behavior of the iterator,
/// which visits all children, must be used.
///
/// [interior mutability]: https://doc.rust-lang.org/reference/interior-mutability.html
#[derive(Debug)]
pub struct Iter<'a, T> {
    /// Represents the iterator's current position within the [`HexDivNode`].
    ///
    /// The position has two possible cases:
    ///
    /// 1. The bounds' extent matches that of the last node in [`Self::parents`]:
    ///    - This indicates that the parent node has just been yielded.
    ///    - Unless [`Self::skip_children`] or [`Self::only_child`] is called, the next call to
    ///      [`Self::next`] will yield the first child.
    /// 2. The bounds' extent matches the extent of the *children* of the last node in
    ///    [`Self::parents`]:
    ///    - This indicates that this node is known to be unnecessary/impossible to enter, though
    ///      it has not yet been yielded.
    ///    - The next call to [`Self::next`] will yield this node.
    ///    - [`Self::skip_children`] and [`Self::only_child`] will panic in this state.
    bounds: CachedBounds,
    /// Indicates, that the root of the [`HexDivNode`] should be yielded.
    ///
    /// Upon the first call to [`Self::next`] it is either cleared or moved into [`Self::parents`].
    root: Option<&'a T>,
    /// Contains the full path of parent nodes describing the iterator's position.
    parents: ArrayVec<Parent<&'a T>, { SplitList::MAX }>,
    /// An extension to [`Self::parents`] that indicates whether only a single child was entered.
    enter_only: BitArray<u16>,
}

impl<'a, T: HexDivNode> Iter<'a, T> {
    pub(super) fn new(root: &'a T) -> Self {
        Self {
            bounds: CachedBounds::with_extent_at_origin(root.cached_extent()),
            root: Some(root),
            parents: ArrayVec::new(),
            enter_only: BitArray::default(),
        }
    }

    /// Panics with an appropriate message for trying to skip or enter if there is no parent node.
    fn panic_skip_or_enter(&mut self) -> ! {
        if self.root.is_some() {
            panic!("iteration not yet started");
        } else {
            panic!("iteration already over");
        }
    }

    /// Checks, if [`Self::root`] is still set and, if so, returns and also clears it.
    fn take_root(&mut self) -> Option<(CachedBounds, NodeRef<'a, T>)> {
        self.root.take().map(|root| {
            let bounds = self.bounds;
            if let Some(parent) = Parent::new(root) {
                self.parents.push(parent);
            }
            (bounds, NodeRef::Node(root))
        })
    }

    /// Advances the iterator, assuming [`Self::root`] is empty.
    fn advance(&mut self) -> Option<(CachedBounds, NodeRef<'a, T>)> {
        let parent = *self.parents.last()?;
        let splits = parent.extent().child_splits();

        let (bounds, index, child) = if self.bounds.extent() == *parent.extent() {
            let child = parent.get_child(0);
            self.bounds = self.bounds.with_extent_unchecked(child.cached_extent());
            (self.bounds, 0, child)
        } else {
            let index = self.bounds.child_index(splits);
            (self.bounds, index, parent.get_child(index))
        };

        assert_ne!(self.bounds.extent(), *parent.extent());

        if let Some(parent) = child.as_parent() {
            self.parents.push(parent);
        } else {
            self.next_neighbor(index, splits);
        }

        Some((bounds, child))
    }

    /// Moves [`Self::bounds`] to the next neighbor.
    ///
    /// - `index` should be the current index of the bounds within the last parent.
    /// - `splits` should be the splits of the last parent.
    ///
    /// If already on the last child or [`Self::enter_only`] is set, unsplits the bounds, pops the
    /// last parent and recursively calls itself again.
    fn next_neighbor(&mut self, mut index: u8, mut splits: Splits) {
        loop {
            if self.enter_only[self.parents.len() - 1] {
                self.enter_only.set(self.parents.len() - 1, false);
                self.bounds = self.bounds.with_child_index(splits, 0);
            } else {
                let next_index = (index + 1) % splits.volume();
                self.bounds = self.bounds.with_child_index(splits, next_index);
                if next_index != 0 {
                    break;
                }
            }

            self.bounds = *self
                .bounds
                .unsplit_first_child_unchecked(splits)
                .expect("iteration should not exceed initial node size");
            self.parents.pop();

            let Some(parent) = self.parents.last() else {
                break;
            };

            splits = parent.extent().child_splits();
            index = self.bounds.child_index(splits);
        }
    }
}

impl<T> Clone for Iter<'_, T> {
    fn clone(&self) -> Self {
        Self {
            bounds: self.bounds,
            root: self.root,
            parents: self.parents.clone(),
            enter_only: self.enter_only,
        }
    }
}

impl<T> Default for Iter<'_, T> {
    fn default() -> Self {
        Self {
            bounds: Default::default(),
            root: Default::default(),
            parents: Default::default(),
            enter_only: Default::default(),
        }
    }
}

impl<'a, T: HexDivNode> Iterator for Iter<'a, T> {
    type Item = (CachedBounds, NodeRef<'a, T>);

    fn next(&mut self) -> Option<Self::Item> {
        self.take_root().or_else(|| self.advance())
    }
}

impl<'a, T: HexDivNode> HexDivNodeIterator for Iter<'a, T> {
    type Node = NodeRef<'a, T>;

    fn skip_children(&mut self) {
        let Some(parent) = self.parents.last() else {
            self.panic_skip_or_enter();
        };
        assert!(self.bounds.extent() == *parent.extent());
        self.parents.pop();

        if let Some(parent) = self.parents.last() {
            let splits = parent.extent().child_splits();
            let index = self.bounds.child_index(splits);
            self.next_neighbor(index, splits);
        }
    }

    fn only_child(&mut self, index: u8) {
        let Some(parent) = self.parents.last() else {
            self.panic_skip_or_enter();
        };
        assert!(self.bounds.extent() == *parent.extent());
        let child_extent = parent.get_child(index).cached_extent();
        self.bounds = self
            .bounds
            .with_extent_unchecked(child_extent)
            .with_child_index(parent.cached_extent().child_splits(), index);
        self.enter_only.set(self.parents.len() - 1, true);
    }
}

#[cfg(test)]
mod tests {
    use glam::UVec3;
    use itertools::{assert_equal, Itertools};

    use super::*;
    use crate::hex_div::{
        extent::Extent,
        node::{
            bool::{BitNode, Count, NoBitCache},
            builder::{build_sphere_octant, BuildAction, Builder},
            NodeDataRef,
        },
    };

    #[test]
    fn iter_yields_all_nodes() {
        let root = build_sphere_octant::<BitNode>(6);

        let nodes = root.iter().counts_by(|(_, node)| node.data());

        assert_eq!(nodes[&NodeDataRef::Parent(&(), NoBitCache)], 483);
        assert_eq!(nodes[&NodeDataRef::Leaf(false)], 15_081);
        assert_eq!(nodes[&NodeDataRef::Leaf(true)], 15_349);
    }

    #[test]
    fn visit_all_yields_all_nodes() {
        let root = build_sphere_octant::<BitNode>(6);

        let nodes = root
            .iter()
            .visit(|_, _| Some(Enter::All))
            .counts_by(|(_, node)| node.data());

        assert_eq!(nodes[&NodeDataRef::Parent(&(), NoBitCache)], 483);
        assert_eq!(nodes[&NodeDataRef::Leaf(false)], 15_081);
        assert_eq!(nodes[&NodeDataRef::Leaf(true)], 15_349);
    }

    #[test]
    fn visit_none_yields_only_the_root_node() {
        let root = build_sphere_octant::<BitNode>(6);

        let nodes = root.iter().visit(|_, _| None);

        let bounds = CachedBounds::with_extent_at_origin(root.cached_extent());
        assert_equal(nodes, [(bounds, NodeRef::Node(&root))]);
    }

    #[test]
    fn visit_only_yields_specific_nodes() {
        let root = build_sphere_octant::<BitNode<Count>>(6);
        let bounds = CachedBounds::with_extent_at_origin(root.cached_extent());

        assert_equal(
            root.iter()
                .visit(|_, _| Some(Enter::Only { child: 0 }))
                .map(|(bounds, node)| (bounds.strip_cache(), node)),
            [
                (bounds.strip_cache(), NodeRef::Node(&root)),
                (bounds.first_child().unwrap(), root.get_child(0)),
            ],
        );

        assert_equal(
            root.iter()
                .visit(|_, _| Some(Enter::Only { child: 63 }))
                .map(|(bounds, node)| (bounds.strip_cache(), node)),
            [
                (bounds.strip_cache(), NodeRef::Node(&root)),
                (bounds.child(63).unwrap(), root.get_child(63)),
            ],
        );

        // child_chain is calculated via:
        // 64 / sqrt(3) ~= 36.95      ->  0b10__________01__________00
        // child index along diagonal -> [0b10_10_10, 0b01_01_01, 0b00_00_00]
        // convert to base 10         -> [        42,         21,          0]

        let mut child_chain = [42, 21, 0]
            .into_iter()
            .map(|child| Some(Enter::Only { child }));
        let nodes = root
            .iter()
            .visit(|_, _| child_chain.next().unwrap())
            .map(|(_, node)| node.data());
        assert_equal(
            nodes,
            [
                NodeDataRef::Parent(&(), Count(142_105)),
                NodeDataRef::Parent(&(), Count(656)),
                NodeDataRef::Parent(&(), Count(10)),
                NodeDataRef::Leaf(true), // true since we used 36; for 37 would be false
            ],
        );
        assert_equal(child_chain, []);
    }

    #[test]
    #[should_panic = "iteration not yet started"]
    fn skip_children_panics_if_iteration_is_not_yet_started() {
        let root: BitNode = HexDivNode::with_default(Extent::ONE);
        let mut iter = root.iter();

        iter.skip_children();
    }

    #[test]
    #[should_panic = "iteration not yet started"]
    fn only_child_panics_if_iteration_is_not_yet_started() {
        let root: BitNode = HexDivNode::with_default(Extent::ONE);
        let mut iter = root.iter();

        iter.only_child(0);
    }

    #[test]
    #[should_panic = "iteration already over"]
    fn skip_children_panics_if_iteration_is_already_over() {
        let root: BitNode = HexDivNode::with_default(Extent::ONE);
        let mut iter = root.iter();
        assert!(iter.next().is_some());

        iter.skip_children();
    }

    #[test]
    #[should_panic = "iteration already over"]
    fn only_child_panics_if_iteration_is_already_over() {
        let root: BitNode = HexDivNode::with_default(Extent::ONE);
        let mut iter = root.iter();
        assert!(iter.next().is_some());

        iter.only_child(0);
    }

    #[test]
    #[should_panic = "child index out of bounds"]
    fn only_child_panics_if_the_index_is_out_of_bounds() {
        // 2x1x1 BitNode with [true, false]
        let root: BitNode = Builder::new(Extent::from_splits([1, 0, 0]).unwrap().compute_cache())
            .build(|bounds| match bounds.to_point() {
                Some(UVec3 { x: 0, y: 0, z: 0 }) => BuildAction::Fill(true),
                Some(UVec3 { x: 1, y: 0, z: 0 }) => BuildAction::Fill(false),
                Some(_) => unreachable!(),
                None => BuildAction::Split(()),
            });
        let mut iter = root.iter();
        assert!(iter.next().is_some());

        iter.only_child(2);
    }
}

use arrayvec::ArrayVec;
use bitvec::array::BitArray;
use educe::Educe;

use super::{
    bounds::Bounds,
    extent::{SplitList, Splits},
    visit::{Enter, VisitNode},
    HexDivNode, NodeRef, ParentNodeRef,
};

/// An iterator that iterates over the nodes of a [`HexDivNode`].
///
/// By default, all nodes are visited:
///
/// ```
/// # use unbound_lib::hex_div::{extent::Extent, node::Node, HexDiv};
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
/// # use unbound_lib::hex_div::{extent::Extent, node::Node, visit::Enter, HexDiv};
/// # let root = Node::<u32>::with_default(Extent::ONE);
/// let filtered = root.iter().visit(|node| {
///     // filter e.g. based on bounds
///     Enter::None
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
/// # use unbound_lib::hex_div::{extent::Extent, node::Node, HexDiv};
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
/// # use unbound_lib::hex_div::{extent::Extent, node::Node, HexDiv};
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
#[derive(Educe)]
#[educe(Clone, Default, Debug)]
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
    bounds: Bounds,
    /// Indicates, that the root of the [`HexDivNode`] should be yielded.
    ///
    /// Upon the first call to [`Self::next`] it is either cleared or moved into [`Self::parents`].
    root: Option<&'a T>,
    /// Contains the full path of parent nodes describing the iterator's position.
    parents: ArrayVec<ParentNodeRef<'a, T>, { SplitList::MAX }>,
    /// An extension to [`Self::parents`] that indicates whether only a single child was entered.
    enter_only: BitArray<u16>,
}

impl<'a, T: HexDivNode> Iter<'a, T> {
    pub(super) fn new(root: &'a T) -> Self {
        Self {
            bounds: root.extent().into(),
            root: Some(root),
            parents: ArrayVec::new(),
            enter_only: BitArray::default(),
        }
    }

    /// Filters out entire parent nodes based on a callback.
    ///
    /// This calls [`Self::skip_children`] and [`Self::only_child`] based on the callback.
    pub fn visit<V: FnMut(VisitNode<'a, T>) -> Enter>(self, visit: V) -> Visit<'a, T, V> {
        Visit {
            iter: self,
            visit,
            prev_node: None,
        }
    }

    /// Skips over the children of the previous node.
    ///
    /// # Panics
    ///
    /// Panics if the previous node cannot be entered, since it will get skipped automatically.
    ///
    /// Panics if [`Self::skip_children`] or [`Self::only_child`] was already called before calling
    /// [`Iter::next`].
    pub fn skip_children(&mut self) {
        let Some(parent) = self.parents.last() else {
            self.panic_skip_or_enter();
        };
        assert!(self.bounds.extent() == parent.get().extent());
        self.parents.pop();

        if let Some(parent) = self.parents.last() {
            let splits = parent.get().splits();
            let index = self.bounds.child_index(splits);
            self.next_neighbor(index, splits);
        }
    }

    /// Only enters the child with the given `index` of the previous node.
    ///
    /// # Panics
    ///
    /// Panics if the previous node cannot be entered.
    ///
    /// Panics if [`Self::skip_children`] or [`Self::only_child`] was already called before calling
    /// [`Iter::next`].
    ///
    /// Panics if the index is out of bounds.
    pub fn only_child(&mut self, index: u8) {
        let Some(parent) = self.parents.last() else {
            self.panic_skip_or_enter();
        };
        let parent = parent.get();
        assert!(self.bounds.extent() == parent.extent());
        let splits = parent.splits();
        self.bounds = self
            .bounds
            .split_extent(splits)
            .with_child_index(splits, index);
        self.enter_only.set(self.parents.len() - 1, true);
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
    fn take_root(&mut self) -> Option<(Bounds, NodeRef<'a, T>)> {
        self.root.take().map(|root| {
            let bounds = self.bounds;
            if let Some(parent) = ParentNodeRef::new(root) {
                self.parents.push(parent);
            }
            (bounds, NodeRef::Node(root))
        })
    }

    /// Advances the iterator, assuming [`Self::root`] is empty.
    fn advance(&mut self) -> Option<(Bounds, NodeRef<'a, T>)> {
        let parent = self.parents.last()?.get();
        let splits = parent.splits();

        let (bounds, index, child) = if self.bounds.extent() == parent.extent() {
            self.bounds = self.bounds.split_extent(splits);
            (self.bounds, 0, parent.get_child(0))
        } else {
            let index = self.bounds.child_index(splits);
            (self.bounds, index, parent.get_child(index))
        };

        assert_ne!(self.bounds.extent(), parent.extent());

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

            self.bounds.unsplit_unchecked(splits);
            self.parents.pop();

            let Some(parent) = self.parents.last() else {
                break;
            };

            splits = parent.get().splits();
            index = self.bounds.child_index(splits);
        }
    }
}

impl<'a, T: HexDivNode> Iterator for Iter<'a, T> {
    type Item = (Bounds, NodeRef<'a, T>);

    fn next(&mut self) -> Option<Self::Item> {
        self.take_root().or_else(|| self.advance())
    }
}

pub struct Visit<'a, T: HexDivNode, V> {
    iter: Iter<'a, T>,
    visit: V,
    prev_node: Option<VisitNode<'a, T>>,
}

impl<'a, T: HexDivNode, V: FnMut(VisitNode<'a, T>) -> Enter> Iterator for Visit<'a, T, V> {
    type Item = (Bounds, NodeRef<'a, T>);

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(node) = self.prev_node.take() {
            match (self.visit)(node) {
                Enter::None => self.iter.skip_children(),
                Enter::Only { child } => self.iter.only_child(child),
                Enter::All => {}
            }
        }
        self.iter.next().inspect(|&(bounds, node)| {
            if let Some(node) = node.as_parent() {
                self.prev_node = Some(VisitNode { bounds, node })
            }
        })
    }
}

#[cfg(test)]
mod tests {
    use glam::UVec3;
    use itertools::{assert_equal, Itertools};

    use super::*;
    use crate::hex_div::{
        builder::{build_sphere_octant, BuildAction, Builder},
        extent::Extent,
        node::bool::{BitNode, Count, NoBitCache},
        NodeDataRef,
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
            .visit(|_| Enter::All)
            .counts_by(|(_, node)| node.data());

        assert_eq!(nodes[&NodeDataRef::Parent(&(), NoBitCache)], 483);
        assert_eq!(nodes[&NodeDataRef::Leaf(false)], 15_081);
        assert_eq!(nodes[&NodeDataRef::Leaf(true)], 15_349);
    }

    #[test]
    fn visit_none_yields_only_the_root_node() {
        let root = build_sphere_octant::<BitNode>(6);

        let nodes = root.iter().visit(|_| Enter::None);

        let bounds = root.extent().into();
        assert_equal(nodes, [(bounds, NodeRef::Node(&root))]);
    }

    #[test]
    fn visit_only_yields_specific_nodes() {
        let root = build_sphere_octant::<BitNode<(), Count>>(6);
        let bounds = Bounds::from(root.extent());
        let splits = root.splits();

        let nodes = root.iter().visit(|_| Enter::Only { child: 0 });
        assert_equal(
            nodes,
            [
                (bounds, NodeRef::Node(&root)),
                (
                    bounds.split_extent(splits).with_child_index(splits, 0),
                    root.get_child(0),
                ),
            ],
        );

        let nodes = root.iter().visit(|_| Enter::Only { child: 63 });
        assert_equal(
            nodes,
            [
                (bounds, NodeRef::Node(&root)),
                (
                    bounds.split_extent(splits).with_child_index(splits, 63),
                    root.get_child(63),
                ),
            ],
        );

        // child_chain is calculated via:
        // 64 / sqrt(3) ~= 36.95      ->  0b10__________01__________00
        // child index along diagonal -> [0b10_10_10, 0b01_01_01, 0b00_00_00]
        // convert to base 10         -> [        42,         21,          0]

        let mut child_chain = [42, 21, 0].into_iter().map(|child| Enter::Only { child });
        let nodes = root
            .iter()
            .visit(|_| child_chain.next().unwrap())
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
        let root: BitNode = Builder::new(Extent::from_splits([1, 0, 0]).unwrap()).build(|bounds| {
            match bounds.to_point() {
                Some(UVec3 { x: 0, y: 0, z: 0 }) => BuildAction::Fill(true),
                Some(UVec3 { x: 1, y: 0, z: 0 }) => BuildAction::Fill(false),
                Some(_) => unreachable!(),
                None => BuildAction::Split(()),
            }
        });
        let mut iter = root.iter();
        assert!(iter.next().is_some());

        iter.only_child(2);
    }
}

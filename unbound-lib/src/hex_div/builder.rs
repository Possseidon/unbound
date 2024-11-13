use std::mem::take;

use arrayvec::ArrayVec;
use educe::Educe;

use super::{
    bounds::Bounds,
    extent::{Extent, SplitList, Splits},
    HexDiv,
};

/// Allows building a [`HexDiv`] incrementally or from a callback.
#[derive(Clone, Debug)]
pub struct Builder<T: HexDiv> {
    /// The bounds that will be processed by the next call to [`Self::step`].
    bounds: Bounds,
    /// The number of splits
    split_list: SplitList,
    /// Contains all nodes that were built.
    nodes: Vec<T>,
    /// Contains all parent nodes that are not yet built.
    parents: ArrayVec<T::Parent, { SplitList::MAX }>,
}

impl<T: HexDiv> Builder<T> {
    /// Constructs a new [`Builder`] that can build [`HexDiv`]s with the given `extent`.
    pub fn new(extent: Extent) -> Self {
        Self::with_scratch(extent, Default::default())
    }

    /// Constructs a new [`Builder`] that reuses the given [`Scratch`] allocations.
    pub fn with_scratch(extent: Extent, scratch: Scratch<T>) -> Self {
        Self {
            bounds: extent.into(),
            split_list: extent.to_split_list(),
            nodes: scratch.nodes,
            parents: ArrayVec::new(),
        }
    }

    /// Returns the bounds that will be process by the next call to [`Self::step`].
    pub fn bounds(&self) -> Bounds {
        self.bounds
    }

    /// Builds a [`HexDiv`] using the given `build` callback.
    ///
    /// This is just a convencience for calling [`Self::step`] with [`Self::bounds`] until it
    /// returns [`Some`]. This means, it can be called, even if a few steps were already done by
    /// calling [`Self::step`] manually.
    ///
    /// Similar to [`Self::step`], [`Self::build`] can be called again to build another [`HexDiv`]
    /// of the same type and extent, reusing its existing [`Scratch`] allocations.
    ///
    /// # Panics
    ///
    /// Panics for invalid [`BuildAction`]s; see [`Self::step`].
    pub fn build(&mut self, mut build: impl FnMut(Bounds) -> BuildAction<T>) -> T
    where
        T: Clone,
        T::Leaf: Clone + Eq,
    {
        loop {
            if let Some(root) = self.step(build(self.bounds)) {
                break root;
            }
        }
    }

    /// Performs the given build `action` at [`Self::bounds`].
    ///
    /// This simply delegates to [`Self::leaf_step`], [`Self::node_step`] and [`Self::parent_step`].
    ///
    /// Returns [`Some`] if the [`HexDiv`] was fully built and further calls will start building a
    /// new [`HexDiv`] from scratch, though reusing [`Scratch`] allocations.
    ///
    /// # Panics
    ///
    /// Panics if `action` contains a [`BuildAction::Node`] with a mismatched extent or
    /// [`BuildAction::Split`] if [`Self::bounds`] covers a single point which cannot be split.
    pub fn step(&mut self, action: BuildAction<T>) -> Option<T>
    where
        T: Clone,
        T::Leaf: Clone + Eq,
    {
        match action {
            BuildAction::Fill(leaf) => self.leaf_step(leaf),
            BuildAction::Node(node) => self.node_step(node),
            BuildAction::Split(parent) => {
                self.parent_step(parent);
                None
            }
        }
    }

    /// Shortcut for calling [`Self::node_step`] for a leaf node with the correct extent.
    ///
    /// Unlike [`Self::node_step`] this can never panic, since the extent is assigned automatically.
    pub fn leaf_step(&mut self, leaf: T::Leaf) -> Option<T>
    where
        T: Clone,
        T::Leaf: Clone + Eq,
    {
        self.node_step(T::new(self.bounds.extent(), leaf))
    }

    /// Sets the current [`Self::bounds`] to the given `node` and advances [`Self::bounds`].
    ///
    /// Returns [`Some`] if the [`HexDiv`] was fully built and further calls will start building a
    /// new [`HexDiv`] from scratch, though reusing [`Scratch`] allocations.
    ///
    /// # Panics
    ///
    /// Panics if the given `node` has a mismatched extent.
    pub fn node_step(&mut self, mut node: T) -> Option<T>
    where
        T: Clone,
        T::Leaf: Clone + Eq,
    {
        let extent = self.bounds.extent();
        assert_eq!(node.extent(), extent, "node extent mismatch");

        loop {
            if self.parents.is_empty() {
                break Some(node);
            }

            let splits = self.split_list.level(self.parents.len() - 1);

            if let Some(next_bounds) = self.bounds.next_bounds_within(splits) {
                self.bounds = next_bounds;
                self.nodes.push(node);
                break None;
            }

            let parent_extent = self.bounds.extent().unsplit(splits);
            self.bounds = self.bounds.floor_to_extent(parent_extent);

            let first_child = self.nodes.len() - (usize::from(splits.volume() - 1));
            let children = self.nodes.drain(first_child..).chain([node]);
            let parent = self.parents.pop().expect("parents should not be empty");
            node = T::from_children(children, splits, parent);
        }
    }

    /// Pushes the given `parent` onto the stack and advances [`Self::bounds`].
    ///
    /// # Panics
    ///
    /// Panics if the current bounds cannot be split due to covering a single point.
    pub fn parent_step(&mut self, parent: T::Parent) {
        assert!(!self.bounds.is_point(), "attempt to split a single point");
        let splits = self.split_list.level(self.parents.len());
        self.bounds = self.bounds.split_extent(splits);
        self.parents.push(parent);
    }

    /// Takes the [`Scratch`] allocations from this [`Builder`].
    ///
    /// The builder can still be used afterwards; it will simply allocate new [`Scratch`] space.
    ///
    /// # Panics
    ///
    /// Panics if the [`Scratch`] space is in use.
    pub fn take_scratch(&mut self) -> Scratch<T> {
        assert!(self.nodes.is_empty());
        Scratch {
            nodes: take(&mut self.nodes),
        }
    }

    /// Replaces the current [`Scratch`] allocations with the given one.
    pub fn insert_scratch(&mut self, scratch: Scratch<T>) {
        self.nodes = scratch.nodes;
    }
}

/// Used as input to [`Builder::step`] to decide what to build.
pub enum BuildAction<T: HexDiv> {
    /// Fills the bounds with the given value.
    ///
    /// This is just a convenience for [`BuildAction::Node`] without having to pass the extent.
    Fill(T::Leaf),
    /// The given node is taken as is and not split any further.
    Node(T),
    /// A parent node is created, resulting in further callbacks.
    Split(T::Parent),
}

/// Contains scratch space (allocations) for a [`Builder`].
///
/// Contained [`Vec`]s are always empty, since they are only used to keep their allocated capacity.
///
/// Intentionally does not implement [`Clone`], since cloning a [`Vec`] does not clone its capacity.
#[derive(Educe)]
#[educe(Default)]
pub struct Scratch<T> {
    nodes: Vec<T>,
}

impl<T> std::fmt::Debug for Scratch<T> {
    /// Prints the capacity of [`Self::nodes`] rather than its content.
    ///
    /// The contents should always be empty, so that's not useful for debugging.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Scratch")
            .field("nodes_capacity", &self.nodes.capacity())
            .finish()
    }
}

impl<T> Scratch<T> {
    /// Allocates enough [`Scratch`] space, so that no further allocations are necessary.
    pub fn with_capacity_for(extent: Extent) -> Self {
        Self {
            nodes: Vec::with_capacity(scratch_node_capacity_for(extent)),
        }
    }
}

/// Returns the minimum capacity that avoids allocations for the worst case of the given `extent`.
const fn scratch_node_capacity_for(extent: Extent) -> usize {
    let (full_splits, rest) = extent.full_splits_and_rest();
    full_splits as usize * (Splits::MAX_VOLUME_USIZE - 1) + (1 << rest) - 1
}

/// Builds a [`BitNodeWithCount`] containing a sphere octant.
///
/// This is very useful to get a sufficiently complex [`HexDiv`] for testing.
///
/// Purely uses integer maths, so the output for any given `splits` is fully deterministic.
#[cfg(test)]
pub(super) fn build_sphere_octant<T: HexDiv<Leaf = bool, Parent = ()> + Clone>(splits: u8) -> T {
    let extent = Extent::from_splits([splits; 3]).unwrap();
    let max_distance = 1 << splits;
    let max_distance_squared = max_distance * max_distance;

    let is_inside = |point: glam::UVec3| point.length_squared() < max_distance_squared;

    Builder::new(extent).build(|bounds| {
        if is_inside(bounds.max()) {
            BuildAction::Fill(true)
        } else if !is_inside(bounds.min()) {
            BuildAction::Fill(false)
        } else {
            BuildAction::Split(())
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hex_div::{
        node::bool::{BitNode, BitNodeWithCount, Count},
        NodeDataRef,
    };

    #[test]
    fn sphere_octant() {
        let root = build_sphere_octant::<BitNodeWithCount>(8);

        let NodeDataRef::Parent(_, Count(volume)) = root.as_data() else {
            panic!("should be a parent node");
        };

        // 256³ * PI / 6 ~= 8_784_529
        // deterministic result is guaranteed, since calculation only uses integer maths
        assert_eq!(volume, 8_861_665);
    }

    #[test]
    #[should_panic = "attempt to split a single point"]
    fn step_with_split_panics_for_point_nodes() {
        Builder::<BitNode>::new(Extent::ONE).step(BuildAction::Split(()));
    }

    #[test]
    #[should_panic = "node extent mismatch"]
    fn step_with_node_panics_for_mismatched_extent() {
        Builder::<BitNode>::new(Extent::ONE)
            .step(BuildAction::Node(BitNode::new(Extent::MAX, true)));
    }

    #[test]
    fn scratch_with_max_capacity_does_not_allocate() {
        const EXTENT: Extent = Extent::MAX;

        let scratch = Scratch::with_capacity_for(EXTENT);
        let initial_capacity = scratch.nodes.capacity();

        let scratch = build_with_max_capacity(EXTENT, scratch);

        assert_eq!(scratch.nodes.capacity(), initial_capacity);
    }

    #[test]
    fn scratch_nodes_max_capacity_cannot_be_reduced() {
        const EXTENT: Extent = Extent::MAX;

        // try with one less scratch space and ensure it allocates
        const TOO_SMALL_CAPACITY: usize = scratch_node_capacity_for(EXTENT) - 1;

        let scratch = Scratch {
            nodes: Vec::with_capacity(TOO_SMALL_CAPACITY),
        };
        assert_eq!(
            scratch.nodes.capacity(),
            TOO_SMALL_CAPACITY,
            "over-allocation invalidates this test"
        );

        let scratch = build_with_max_capacity(EXTENT, scratch);

        assert!(scratch.nodes.capacity() > TOO_SMALL_CAPACITY);
    }

    /// Builds an octree that only contains a single `true` at [`Bounds::MAX_POINT`].
    ///
    /// This has the effect of requiring the maximum possible amount of capacity inside `scratch`.
    fn build_with_max_capacity(extent: Extent, scratch: Scratch<BitNode>) -> Scratch<BitNode> {
        let max_point = extent.size() - 1;
        let mut builder = Builder::with_scratch(extent, scratch);
        builder.build(|bounds| {
            if bounds.to_point() == Some(max_point) {
                BuildAction::Fill(true)
            } else if bounds.contains(max_point) {
                BuildAction::Split(())
            } else {
                BuildAction::Fill(false)
            }
        });
        builder.take_scratch()
    }
}

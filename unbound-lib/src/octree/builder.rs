use std::mem::take;

use arrayvec::ArrayVec;
use derive_where::derive_where;

use super::{
    bounds::OctreeBounds,
    extent::{OctreeExtent, OctreeSplitList, OctreeSplits},
    OctreeNode,
};

/// Allows building an [`OctreeNode`] incrementally.
pub struct Builder<T: OctreeNode> {
    bounds: OctreeBounds,
    split_list: OctreeSplitList,
    /// Contains all nodes that were built.
    nodes: Vec<T>,
    /// Contains all parent nodes that are not yet built.
    parents: ArrayVec<T::Parent, { OctreeSplitList::MAX }>,
}

impl<T: OctreeNode> Builder<T> {
    pub fn new(extent: OctreeExtent) -> Self {
        Self::with_scratch(extent, Default::default())
    }

    /// Returns the bounds that will be process by the next call to [`Self::step`].
    pub fn bounds(&self) -> OctreeBounds {
        self.bounds
    }

    pub fn step(&mut self, build: impl FnOnce(OctreeBounds) -> BuildAction<T>) -> Option<T> {
        self.step_with_scratch(build).map(|(node, _)| node)
    }

    pub fn build(&mut self, build: impl FnMut(OctreeBounds) -> BuildAction<T>) -> T {
        self.build_with_scratch(build).0
    }

    pub fn with_scratch(extent: OctreeExtent, scratch: Scratch<T>) -> Self {
        Self {
            bounds: extent.into(),
            split_list: extent.to_split_list(),
            nodes: scratch.nodes,
            parents: ArrayVec::new(),
        }
    }

    pub fn step_with_scratch(
        &mut self,
        build: impl FnOnce(OctreeBounds) -> BuildAction<T>,
    ) -> Option<(T, Scratch<T>)> {
        match build(self.bounds) {
            BuildAction::Fill(leaf) => self.push_node(T::new(self.bounds.extent(), leaf)),
            BuildAction::Node(node) => self.push_node(node),
            BuildAction::Split(parent) => {
                self.push_parent(parent);
                None
            }
        }
    }

    pub fn build_with_scratch(
        &mut self,
        mut build: impl FnMut(OctreeBounds) -> BuildAction<T>,
    ) -> (T, Scratch<T>) {
        loop {
            if let Some(result) = self.step_with_scratch(&mut build) {
                break result;
            }
        }
    }

    fn push_node(&mut self, mut node: T) -> Option<(T, Scratch<T>)> {
        let extent = self.bounds.extent();
        assert_eq!(node.extent(), extent);

        loop {
            if self.parents.is_empty() {
                break Some((node, self.take_scratch()));
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

    fn push_parent(&mut self, parent: <T as OctreeNode>::Parent) {
        let splits = self.split_list.level(self.parents.len());
        self.bounds = self.bounds.split_extent(splits);
        self.parents.push(parent);
    }

    fn take_scratch(&mut self) -> Scratch<T> {
        self.nodes.clear();
        Scratch {
            nodes: take(&mut self.nodes),
        }
    }
}

pub enum BuildAction<T: OctreeNode> {
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
#[derive_where(Default)]
pub struct Scratch<T> {
    nodes: Vec<T>,
}

impl<T> std::fmt::Debug for Scratch<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Scratch")
            .field("nodes_capacity", &self.nodes.capacity())
            .finish()
    }
}

impl<T> Scratch<T> {
    /// Allocates enough [`Scratch`] space, so that no further allocations are necessary.
    pub fn with_capacity_for(extent: OctreeExtent) -> Self {
        Self {
            nodes: Vec::with_capacity(scratch_node_capacity_for(extent)),
        }
    }
}

const fn scratch_node_capacity_for(extent: OctreeExtent) -> usize {
    let (full_splits, rest) = extent.full_splits_and_rest();
    full_splits as usize * (OctreeSplits::MAX_VOLUME_USIZE - 1) + (1 << rest) - 1
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::octree::node::bool::Node;

    /// Builds an octree that only contains a single `true` at [`OctreeBounds::MAX_POINT`].
    ///
    /// This has the effect of requiring the maximum possible amount of capacity inside `scratch`.
    fn build_with_max_capacity(extent: OctreeExtent, scratch: Scratch<Node>) -> Scratch<Node> {
        let max_point = extent.size() - 1;
        let (_, scratch) =
            Builder::<Node>::with_scratch(extent, scratch).build_with_scratch(|bounds| {
                if bounds.to_point() == Some(max_point) {
                    BuildAction::Fill(true)
                } else if bounds.contains(max_point) {
                    BuildAction::Split(())
                } else {
                    BuildAction::Fill(false)
                }
            });
        scratch
    }

    #[test]
    fn scratch_with_max_capacity_does_not_allocate() {
        const EXTENT: OctreeExtent = OctreeExtent::MAX;

        let scratch = Scratch::with_capacity_for(EXTENT);
        let initial_capacity = scratch.nodes.capacity();

        let scratch = build_with_max_capacity(EXTENT, scratch);

        assert_eq!(scratch.nodes.capacity(), initial_capacity);
    }

    #[test]
    fn scratch_nodes_max_capacity_cannot_be_reduced() {
        const EXTENT: OctreeExtent = OctreeExtent::MAX;

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
}

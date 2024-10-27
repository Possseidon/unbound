use std::iter::FusedIterator;

use arrayvec::ArrayVec;
use glam::UVec3;

use super::{
    bounds::OctreeBounds,
    extent::{OctreeExtent, OctreeSplitList},
    visit::Visit,
    NodeRef, OctreeNode, ParentNodeRef,
};

/// Allows iterating over an octree.
///
/// Does not implement the [`Iterator`] trait, since [`Visit`] is passed separately to each call to
/// [`Self::next`]. This makes it possible to use a mutable reference to the same [`Visit`] object
/// while zipping over multiple iterators.
pub struct Iter<'a, T: OctreeNode> {
    /// The current state of iteration.
    state: State<'a, T>,
}

impl<'a, T: OctreeNode> Iter<'a, T> {
    pub(crate) fn new(root: &'a T) -> Self {
        Self {
            state: State::ReturnRoot(root),
        }
    }

    /// Returns the next node in the octree.
    ///
    /// Nodes are returned in a depth-first manner.
    pub fn next(&mut self, mut visit: impl Visit<T>) -> Option<(OctreeBounds, NodeRef<'a, T>)> {
        match &mut self.state {
            State::ReturnRoot(root) => {
                let item = (root.extent().into(), root.as_node_ref());
                self.state = State::AskEnterRoot(root);
                Some(item)
            }
            State::AskEnterRoot(root) => {
                if let NodeRef::Parent(parent) = root.as_node_ref() {
                    if visit.enter(root.extent().into(), parent) {
                        let root_entered = RootEntered::new(parent);
                        let bounds = root_entered.bounds;
                        let child = root.get_child(0);
                        self.state = State::RootEntered(root_entered);
                        return Some((bounds, child));
                    }
                }

                self.state = State::RootSkipped;
                None
            }
            State::RootEntered(root_entered) => root_entered.advance(&mut visit),
            State::RootSkipped => None,
        }
    }

    pub fn with_visit<V: Visit<T>>(self, visit: V) -> IterWithVisit<'a, T, V> {
        IterWithVisit { iter: self, visit }
    }
}

/// Allows iterating over an octree using the [`Iterator`] trait.
pub struct IterWithVisit<'a, T: OctreeNode, V: Visit<T>> {
    iter: Iter<'a, T>,
    /// Decides whether a specific parent node should be entered or skipped over.
    visit: V,
}

impl<'a, T: OctreeNode, V: Visit<T>> IterWithVisit<'a, T, V> {
    pub fn leaves(self) -> impl FusedIterator<Item = (OctreeBounds, T::LeafRef<'a>)> {
        self.filter_map(|(bounds, node)| match node {
            NodeRef::Leaf(leaf) => Some((bounds, leaf)),
            NodeRef::Parent(_) => None,
        })
    }

    pub fn parents(self) -> impl FusedIterator<Item = (OctreeBounds, ParentNodeRef<'a, T>)> {
        self.filter_map(|(bounds, node)| match node {
            NodeRef::Leaf(_) => None,
            NodeRef::Parent(parent) => Some((bounds, parent)),
        })
    }
}

impl<'a, T: OctreeNode, V: Visit<T>> Iterator for IterWithVisit<'a, T, V> {
    type Item = (OctreeBounds, NodeRef<'a, T>);

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next(&mut self.visit)
    }
}

impl<'a, T: OctreeNode, V: Visit<T>> FusedIterator for IterWithVisit<'a, T, V> {}

/// The state required to iterate an octree.
enum State<'a, T> {
    /// The initial state returned when iteration starts.
    ///
    /// Returns the root when [`Iter::next`] is called.
    ReturnRoot(&'a T),
    /// The state after the root node has been yielded, asking if the root node should be entered.
    AskEnterRoot(&'a T),
    /// Used for the remainder of iteration if the root node had to be entered.
    RootEntered(RootEntered<'a, T>),
    /// Marks the end of iteration if the root node was not entered.
    RootSkipped,
}

/// Used for iteration once the root node has been entered.
///
/// `parents` being empty indicates the end of iteration.
struct RootEntered<'a, T> {
    /// Contains all nodes for which [`Visit::enter`] returns `true`.
    ///
    /// Once all children of a node have been traversed, the node is popped again.
    parents: ArrayVec<ParentNodeRef<'a, T>, { OctreeSplitList::MAX }>,
    /// The current bounds.
    bounds: OctreeBounds,
    /// Contains the full list of splits based on the extent of the root node.
    split_list: OctreeSplitList,
}

impl<'a, T: OctreeNode> RootEntered<'a, T> {
    fn new(parent: ParentNodeRef<'a, T>) -> Self {
        let root_extent = parent.0.extent();

        let mut parents = ArrayVec::new();
        parents.push(parent);

        let split_list = root_extent.to_split_list();

        RootEntered {
            parents,
            bounds: root_extent.split(split_list.levels[0]).into(),
            split_list,
        }
    }

    fn advance(&mut self, visit: &mut impl Visit<T>) -> Option<(OctreeBounds, NodeRef<'a, T>)> {
        self.try_enter(visit).or_else(|| self.next_node())
    }

    fn try_enter(&mut self, visit: &mut impl Visit<T>) -> Option<(OctreeBounds, NodeRef<'a, T>)> {
        let last_parent = self.parents.last()?;
        let bounds = self.bounds;
        let index = bounds.small_index_within(last_parent.0.extent());
        let child = last_parent.get_child(index).as_parent()?;

        if !visit.enter(bounds, child) {
            return None;
        }

        let splits = self.split_list.levels[self.parents.len()];
        self.parents.push(child);
        self.bounds = self.bounds.split_extent(splits);
        Some((self.bounds, child.get_child(0)))
    }

    fn next_node(&mut self) -> Option<(OctreeBounds, NodeRef<'a, T>)> {
        while let Some(&parent) = self.parents.last() {
            if let Some(item) = self.next_neighbor(parent) {
                return Some(item);
            }

            self.pop(parent.0.extent());
        }

        None
    }

    fn next_neighbor(
        &mut self,
        parent: ParentNodeRef<'a, T>,
    ) -> Option<(OctreeBounds, NodeRef<'a, T>)> {
        let parent_extent = parent.0.extent();
        self.bounds.next_bounds_within(parent_extent).map(|bounds| {
            let index = bounds.small_index_within(parent_extent);
            (bounds, parent.get_child(index))
        })
    }

    fn pop(&mut self, parent_extent: OctreeExtent) {
        self.parents.pop();
        self.bounds = self.bounds.floor_to_extent(parent_extent);
    }
}

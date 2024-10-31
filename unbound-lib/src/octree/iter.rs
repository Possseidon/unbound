use std::iter::FusedIterator;

use arrayvec::ArrayVec;
use bitvec::{array::BitArray, BitArr};
use glam::UVec3;

use super::{
    bounds::OctreeBounds,
    extent::{OctreeExtent, OctreeSplitList, OctreeSplits},
    visit::{Enter, VisitNode},
    NodeRef, OctreeNode, ParentNodeRef,
};

/// Allows iterating over an octree.
///
/// Does not implement the [`Iterator`] trait, since [`Visit`] is passed separately to each call to
/// [`Self::next`]. This makes it possible to use a mutable reference to the same [`Visit`] object
/// while zipping over multiple iterators.
pub struct Iter<'a, T> {
    /// The current state of iteration.
    state: State<'a, T>,
}

impl<'a, T: OctreeNode> Iter<'a, T> {
    pub(super) fn new(root: &'a T) -> Self {
        Self {
            state: State::ReturnRoot(root),
        }
    }

    /// Returns the next node in the octree.
    ///
    /// Nodes are returned in a depth-first manner.
    pub fn next(
        &mut self,
        visit: impl FnOnce(VisitNode<T>) -> Enter,
    ) -> Option<(OctreeBounds, NodeRef<'a, T>)> {
        match &mut self.state {
            State::ReturnRoot(root) => {
                let item = (root.extent().into(), NodeRef::Node(*root));
                self.state = State::AskEnterRoot(root);
                Some(item)
            }
            State::AskEnterRoot(root) => {
                let Some((root_entered, index)) = RootEntered::new(*root, visit) else {
                    self.state = State::RootSkipped;
                    return None;
                };

                let bounds = root_entered.bounds;
                let child = root.get_child(index);
                self.state = State::RootEntered(root_entered);
                Some((bounds, child))
            }
            State::RootEntered(root_entered) => root_entered.advance(visit),
            State::RootSkipped => None,
        }
    }

    pub fn with_visit<V: FnMut(VisitNode<T>) -> Enter>(self, visit: V) -> IterWithVisit<'a, T, V> {
        IterWithVisit { iter: self, visit }
    }
}

/// Allows iterating over an octree using the [`Iterator`] trait.
pub struct IterWithVisit<'a, T, V> {
    iter: Iter<'a, T>,
    /// Decides whether a specific parent node should be entered or skipped over.
    visit: V,
}

impl<'a, T: OctreeNode, V: FnMut(VisitNode<T>) -> Enter> Iterator for IterWithVisit<'a, T, V> {
    type Item = (OctreeBounds, NodeRef<'a, T>);

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next(&mut self.visit)
    }
}

impl<'a, T: OctreeNode, V: FnMut(VisitNode<T>) -> Enter> FusedIterator for IterWithVisit<'a, T, V> {}

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
    /// Contains all nodes for which at least one child node is [`Visit::enter`]ed.
    ///
    /// Once all children of a node have been traversed, the node is popped again.
    parents: ArrayVec<ParentNodeRef<'a, T>, { OctreeSplitList::MAX }>,
    /// All parent nodes for which [all](`Enter::All`) child nodes should be entered.
    ///
    /// E.g. the LSB is `1` if the children of the outermost, i.e. first, parent should be entered.
    ///
    /// If instead, the bit is `0`, only a specific child was entered and the remaining children of
    /// this particular parent can be skipped over completely when doing the backtracking.
    fully_entered_parents: BitArr!(for OctreeSplitList::MAX),
    /// The current bounds.
    bounds: OctreeBounds,
    /// Contains the full list of splits based on the extent of the root node.
    split_list: OctreeSplitList,
}

impl<'a, T: OctreeNode> RootEntered<'a, T> {
    fn new(root: &'a T, visit: impl FnOnce(VisitNode<T>) -> Enter) -> Option<(Self, u8)> {
        let parent = ParentNodeRef::new(root)?;

        let mut parents = ArrayVec::new();
        parents.push(parent);

        let root_extent = parent.get().extent();
        let split_list = root_extent.to_split_list();
        let splits = split_list.level(0);

        let (all, index) = Self::visit(visit, parent, splits)?;

        let mut fully_entered_parents = BitArray::default();
        fully_entered_parents.set(0, all);

        let root_entered = RootEntered {
            parents,
            fully_entered_parents,
            bounds: OctreeBounds::from_extent(root_extent).split_to_index(splits, index),
            split_list,
        };
        Some((root_entered, index))
    }

    fn advance(
        &mut self,
        visit: impl FnOnce(VisitNode<T>) -> Enter,
    ) -> Option<(OctreeBounds, NodeRef<'a, T>)> {
        self.try_enter(visit).or_else(|| self.next_node())
    }

    fn try_enter(
        &mut self,
        visit: impl FnOnce(VisitNode<T>) -> Enter,
    ) -> Option<(OctreeBounds, NodeRef<'a, T>)> {
        let last_parent = self.parents.last()?;
        let bounds = self.bounds;
        let index = bounds.small_index_within(last_parent.get().extent());
        let parent = last_parent.get().get_child(index).as_parent()?;

        let splits = self.split_list.level(self.parents.len());
        let (all, index) = Self::visit(visit, parent, splits)?;
        self.fully_entered_parents.set(self.parents.len() - 1, all);

        self.parents.push(parent);
        self.bounds = self.bounds.split_to_index(splits, index);
        Some((self.bounds, parent.get().get_child(index)))
    }

    fn next_node(&mut self) -> Option<(OctreeBounds, NodeRef<'a, T>)> {
        while let Some(&parent) = self.parents.last() {
            if self.fully_entered_parents[self.parents.len() - 1] {
                if let Some(item) = self.next_neighbor(parent) {
                    return Some(item);
                }
            }

            self.pop(parent.get().extent());
        }

        None
    }

    fn next_neighbor(
        &mut self,
        parent: ParentNodeRef<'a, T>,
    ) -> Option<(OctreeBounds, NodeRef<'a, T>)> {
        self.bounds
            .next_bounds_within(self.split_list.level(self.parents.len() - 1))
            .map(|bounds| {
                let index = bounds.small_index_within(parent.get().extent());
                (bounds, parent.get().get_child(index))
            })
    }

    fn pop(&mut self, parent_extent: OctreeExtent) {
        self.parents.pop();
        self.bounds = self.bounds.floor_to_extent(parent_extent);
    }

    fn visit(
        visit: impl FnOnce(VisitNode<T>) -> Enter,
        parent: ParentNodeRef<'a, T>,
        splits: OctreeSplits,
    ) -> Option<(bool, u8)> {
        match visit(VisitNode {
            node: parent,
            min: UVec3::ZERO,
            splits,
        }) {
            Enter::None => None,
            Enter::Only { child } => Some((false, child)),
            Enter::All => Some((true, 0)),
        }
    }
}

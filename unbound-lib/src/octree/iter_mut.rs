use std::mem::replace;

use glam::UVec3;

use super::{
    bounds::OctreeBounds,
    extent::{OctreeExtent, OctreeSplitList},
    node::Node,
    stack::ParentNodeMutStack,
    visit::{AllLeaves, VisitMut},
    NodeDataRef, NodeMut, OctreeNode, ParentNodeMut,
};
use crate::octree::NodeDataMut;

/// Mutably iterates over nodes in an octree.
///
/// Iteration has to be done manually via e.g.:
///
/// ```
/// # use unbound_lib::octree::{extent::OctreeExtent, node::Node, visit::AllLeaves, OctreeNode};
/// let mut octree = Node::<i32>::new(OctreeExtent::ONE, 0);
/// let mut iter = octree.iter_mut(AllLeaves);
/// while let Some(node) = iter.next() {
///     // process node...
/// }
/// ```
///
/// [`Iterator`] cannot be implemented, since the returned value from [`Self::next`] has stricter
/// lifetime constraints than is possible via [`Iterator`].
///
/// To summarize the issue; mutable iterators can only be implemented via the [`Iterator`] trait if
/// the returned references are distinct. E.g. [`std::slice::IterMut`] works, since each returned
/// element is a separate item within the slice. However, there is no mutable variant of e.g.
/// [`slice::windows`], since that would have to return mutable references to the same element
/// across different calls to next.
pub struct IterMut<'a, T: OctreeNode> {
    /// The current state of iteration.
    state: State<'a, T>,
}

impl<'a, T: OctreeNode> IterMut<'a, T> {
    pub(crate) fn new(root: &'a mut T) -> Self {
        Self {
            state: State::Root {
                yielded: false,
                root,
            },
        }
    }

    #[allow(
        clippy::should_implement_trait,
        reason = "cannot implement Iterator due to lifetime"
    )]
    pub fn next(&mut self, visit: impl VisitMut<T>) -> Option<NodeMutAt<T>> {
        self.normalize();
        self.advance(visit);
        self.current()
    }

    fn advance(&mut self, visit: impl VisitMut<T>) {
        match &mut self.state {
            State::Root {
                yielded: yielded @ false,
                ..
            } => *yielded = true,
            State::Root { yielded: true, .. } => {
                let State::Root { root, .. } = replace(&mut self.state, State::RootSkipped) else {
                    unreachable!()
                };
                self.state = if root.extent() != OctreeExtent::ONE {
                    State::RootEntered(RootEntered::new(root))
                } else {
                    State::RootSkipped
                };
            }
            State::RootEntered(root_entered) => root_entered.advance(visit),
            State::RootSkipped => {}
        }
    }

    fn current(&mut self) -> Option<NodeMutAt<T>> {
        match &mut self.state {
            State::Root { yielded, root } => {
                assert!(*yielded, "current must be called after advance");
                let extent = root.extent();
                Some(NodeMutAt::new(UVec3::ZERO, extent, root))
            }
            State::RootEntered(root_entered) => root_entered.current(),
            State::RootSkipped => None,
        }
    }

    fn normalize(&mut self) {
        if let State::RootEntered(root_entered) = self.state {
            root_entered.normalize()
        }
    }
}

pub enum NodeMutAt<'a, T: OctreeNode> {
    Bounds(BoundedNodeMut<'a, T>),
    Point { point: UVec3, leaf: T::LeafMut<'a> },
}

impl<'a, T: OctreeNode> NodeMutAt<'a, T> {
    fn new(min: UVec3, extent: OctreeExtent, node: &mut T) -> NodeMutAt<T> {
        if extent == OctreeExtent::ONE {
            NodeMutAt::Point {
                point: min,
                leaf: if let NodeDataMut::Leaf(leaf) = node.as_data_mut() {
                    leaf
                } else {
                    panic!("point node should be a leaf")
                },
            }
        } else {
            NodeMutAt::Bounds(BoundedNodeMut {
                bounds: OctreeBounds::new(min, extent),
                node,
            })
        }
    }
}

pub struct BoundedNodeMut<'a, T: OctreeNode> {
    bounds: OctreeBounds,
    node: &'a mut T,
}

enum State<'a, T: OctreeNode> {
    Root {
        yielded: bool,
        root: &'a mut T,
    },
    /// Used for the remainder of iteration if the root node had to be entered.
    RootEntered(RootEntered<'a, T>),
    /// Marks the end of iteration if the root node was not entered.
    RootSkipped,
}

/// Used for iteration once the root node has been entered.
///
/// `parents` being empty indicates the end of iteration.
struct RootEntered<'a, T: OctreeNode> {
    /// Contains all nodes for which [`Visit::enter`] returns `true`.
    ///
    /// Once all children of a node have been traversed, the node is popped again.
    parents: ParentNodeMutStack<'a, T>,
    /// The [`OctreeBounds::min`] of the current bounds.
    ///
    /// Stored separately, to save on space for `remaining_splits`.
    min: UVec3,
    /// The [`OctreeBounds::extent`] of the current bounds.
    ///
    /// Stored separately, to save on space for `remaining_splits`.
    extent: OctreeExtent,
    /// The number of splits that can still be applied to the current node.
    ///
    /// Additionally, the most significant bit is set if the iterator was newly constructed and
    /// is cleared once [`Iter::next`] was called for the first time.
    remaining_splits: u8,
    /// Contains the full list of splits based on the extent of the root node.
    split_list: OctreeSplitList,
    new_node: Option<NewNode<T>>,
}

impl<'a, T: OctreeNode> RootEntered<'a, T> {
    fn new(root: &'a mut T) -> Self {
        let root_extent = root.extent();

        let (split_list, split_count) = root_extent.to_split_list();
        let remaining_splits = split_count - 1;

        let extent = root_extent.split(split_list.levels[usize::from(remaining_splits)]);

        let mut parents = ParentNodeMutStack::new();
        let mut new_node = None;
        match root.as_node_mut() {
            NodeMut::Leaf(leaf) => {
                new_node = Some(T::new(extent, T::clone_leaf(T::freeze_leaf(leaf))))
            }
            NodeMut::Parent(parent) => parents.push(parent),
        }

        RootEntered {
            parents,
            min: UVec3::ZERO,
            extent,
            remaining_splits,
            split_list,
            new_node,
        }
    }

    fn normalize(&mut self) {
        if let Some(new_node) = self.new_node {
            match new_node.as_data() {
                NodeDataRef::Leaf(leaf) if leaf != self.old_leaf => todo!("split"),
                _ => todo!("split"),
            }
        }
    }

    fn advance(&mut self, visit: impl VisitMut<T>) {
        todo!();
    }

    fn current(&mut self) -> Option<NodeMutAt<T>> {
        Some(NodeMutAt::new(
            self.min,
            self.extent,
            if let Some(new_node) = &mut self.new_node {
                new_node
            } else {
                let parent = self.parents.last_mut()?;
                let bounds = OctreeBounds::new(self.min, self.extent);
                let index = bounds.small_index_within(parent.0.extent());
                if let NodeMut::Parent(parent) = parent.0.get_child_mut_unchecked(index) {
                    parent.0
                } else {
                    panic!("node should be a parent node")
                }
            },
        ))
    }
}

impl<'a, T: OctreeNode> Drop for RootEntered<'a, T> {
    fn drop(&mut self) {
        while self.parents.last().is_some() {
            todo!("normalized pop")
        }
    }
}

struct NewNode<T: OctreeNode> {
    /// The original value when the leaf was entered.
    old: T::Leaf,
    /// A reference to this node is handed out and checked for changes with [`Self::old`].
    new: T,
}

impl<T: OctreeNode> NewNode<T> {
    fn new(extent: OctreeExtent, leaf: T::Leaf) -> Self {
        Self {
            old: leaf.clone(),
            new: T::new(extent, leaf),
        }
    }

    fn changed(&self) -> bool {
        if let NodeDataRef::Leaf(leaf) = self.new.as_data() {
            !T::leaf_eq(&self.old, leaf)
        } else {
            true
        }
    }
}

fn test() {
    let mut node = Node::<i32>::new(OctreeExtent::ONE, 42);
    let mut iter = IterMut::new(&mut node);
    let a = iter.next(AllLeaves);
    a;
    let b = iter.next(AllLeaves);
    let c = iter.next(AllLeaves);
    drop(iter);
    node;
}

use std::{mem::replace, num::NonZeroU8};

use bitvec::BitArr;
use glam::UVec3;

use super::{
    bounds::OctreeBounds,
    extent::{OctreeExtent, OctreeSplitList},
    node::Node,
    stack::MutStack,
    visit::{AllLeaves, Split, SplitDefault, VisitMut},
    NodeDataRef, NodeMut, NodeRef, OctreeNode,
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
pub struct IterMut<'a, T: OctreeNode, S: Split<T>> {
    /// The current state of iteration.
    state: State<'a, T, S>,
}

impl<'a, T: OctreeNode, S: Split<T>> IterMut<'a, T, S> {
    pub(crate) fn new(root: &'a mut T, split: S) -> Self {
        Self {
            state: State::Root {
                yielded: false,
                root,
                split,
            },
        }
    }

    #[allow(
        clippy::should_implement_trait,
        reason = "cannot implement Iterator due to lifetime"
    )]
    pub fn next(&mut self, visit: impl VisitMut<T>) -> Option<NodeMutAt<T>> {
        self.split_if_new_node_changed();
        self.advance(visit);
        self.current()
    }

    fn advance(&mut self, mut visit: impl VisitMut<T>) {
        match &mut self.state {
            State::Root {
                yielded: yielded @ false,
                ..
            } => *yielded = true,
            State::Root { yielded: true, .. } => {
                let State::Root { root, split, .. } = replace(&mut self.state, State::RootSkipped)
                else {
                    unreachable!()
                };
                self.state = if root.extent() != OctreeExtent::ONE {
                    if visit.enter(root.extent().into(), root.as_node_ref()) {
                        State::RootEntered(RootEntered::new(root, split))
                    } else {
                        State::RootSkipped
                    }
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
            State::Root { yielded, root, .. } => {
                assert!(*yielded, "current must be called after advance");
                let extent = root.extent();
                Some(NodeMutAt::new(UVec3::ZERO, root))
            }
            State::RootEntered(root_entered) => root_entered.current(),
            State::RootSkipped => None,
        }
    }

    fn split_if_new_node_changed(&mut self) {
        if let State::RootEntered(root_entered) = &mut self.state {
            root_entered.split_if_new_node_changed();
        }
    }
}

pub enum NodeMutAt<'a, T: OctreeNode> {
    Bounds(BoundedNodeMut<'a, T>),
    Point { point: UVec3, leaf: T::LeafMut<'a> },
}

impl<'a, T: OctreeNode> NodeMutAt<'a, T> {
    fn new(min: UVec3, node: &'a mut T) -> Self {
        if node.extent() == OctreeExtent::ONE {
            Self::Point {
                point: min,
                leaf: if let NodeDataMut::Leaf(leaf) = node.as_data_mut() {
                    leaf
                } else {
                    panic!("point node should be a leaf")
                },
            }
        } else {
            Self::Bounds(BoundedNodeMut { min, node })
        }
    }
}

pub struct BoundedNodeMut<'a, T> {
    min: UVec3,
    node: &'a mut T,
}

impl<'a, T: OctreeNode> BoundedNodeMut<'a, T> {
    pub fn bounds(&self) -> OctreeBounds {
        OctreeBounds::new(self.min, self.node.extent())
    }

    // TODO: mutators must make sure node.extent doesn't change!
}

enum State<'a, T: OctreeNode, S: Split<T>> {
    Root {
        yielded: bool,
        root: &'a mut T,
        split: S,
    },
    /// Used for the remainder of iteration if the root node had to be entered.
    RootEntered(RootEntered<'a, T, S>),
    /// Marks the end of iteration if the root node was not entered.
    RootSkipped,
}

/// Used for iteration once the root node has been entered.
///
/// `parents` being empty indicates the end of iteration.
struct RootEntered<'a, T: OctreeNode, S: Split<T>> {
    /// Contains all nodes that have been entered.
    ///
    /// The actual "current" node depends on where [`Self::bounds`] is located within the last
    /// node in [`Self::parents`].
    ///
    /// If [`VisitMut::enter`] returns `true` for:
    ///
    /// - ... a parent node, then the parent node is pushed.
    /// - ... a proper leaf node, then that leaf node is pushed and `virtual_node` is populated.
    /// - ... a non-node leaf, then only `virtual_node` is populated.
    ///
    /// Once all children of a node have been traversed, the node is popped again.
    parents: MutStack<'a, T>,
    fully_entered_parents: BitArr!(for OctreeSplitList::MAX),
    /// The position of the node that got returned by the last call to [`IterMut::next`].
    ///
    /// Positions of parents can be obtained by calling [`OctreeBounds::floor_min_to_extent`] with
    /// the extent of the parent in question.
    min: UVec3,
    /// Contains the full list of splits based on the extent of the root node.
    split_list: OctreeSplitList,
    /// Populated whenever a leaf node is entered.
    virtual_node: Option<VirtualNode<T>>,
    /// Returns parent data for when virtual nodes turn into real nodes.
    split: S,
}

impl<'a, T: OctreeNode, S: Split<T>> RootEntered<'a, T, S> {
    fn new(root: &'a mut T, split: S) -> Self {
        let root_extent = root.extent();

        let split_list = root_extent.to_split_list();

        let extent = root_extent.split(split_list.levels[0]);

        let virtual_node = if let NodeMut::Leaf(leaf) = root.as_node_mut() {
            Some(VirtualNode {
                depth: NonZeroU8::MIN,
                node: T::new(extent, T::clone_leaf(T::freeze_leaf(leaf))),
            })
        } else {
            None
        };

        Self {
            parents: MutStack::new(root),
            min: UVec3::ZERO,
            split_list,
            virtual_node,
            split,
        }
    }

    /// Returns the extent of the last non-virtual node.
    ///
    /// This is the last parent's extent split once.
    fn last_non_virtual_extent(
        parents: &MutStack<T>,
        split_list: &OctreeSplitList,
    ) -> OctreeExtent {
        parents
            .last()
            .expect("parents should not be empty")
            .extent()
            .split(split_list.levels[parents.len() - 1])
    }

    fn split_if_new_node_changed(&mut self) {
        let Some(VirtualNode { depth, node }) = &self.virtual_node else {
            return; // no virtual node, so no need to split
        };

        let old_leaf = match self
            .parents
            .last()
            .expect("parents should not be empty")
            .as_node_ref()
        {
            NodeRef::Leaf(leaf) => leaf,
            NodeRef::Parent(parent) => {
                let extent = Self::last_non_virtual_extent(&self.parents, &self.split_list);
                let index = OctreeBounds::new_floored(self.min, extent)
                    .small_index_within(parent.0.extent());
                if let NodeRef::Leaf(leaf) = parent.get_child(index) {
                    leaf
                } else {
                    panic!("node should be a leaf")
                }
            }
        };

        if let NodeDataRef::Leaf(new_leaf) = node.as_data() {
            if T::leaf_eq(new_leaf, old_leaf) {
                return;
            }
        }

        let virtual_node = self.virtual_node.take().expect("virtual node should exist");
        let virtual_node_bounds = OctreeBounds::new(self.min, virtual_node.extent());

        let old_leaf = T::clone_leaf(old_leaf);
        let old_leaf = T::leaf_ref(&old_leaf);

        loop {
            let extent = Self::last_non_virtual_extent(&self.parents, &self.split_list);
            if extent == virtual_node_bounds.extent() {
                break;
            }
            let parent = self
                .split
                .split(OctreeBounds::new_floored(self.min, extent), old_leaf);
            let mut last_parent = self
                .parents
                .last_mut()
                .expect("parents should not be empty");
            last_parent.split_into_nodes_unchecked(extent, parent);
            self.parents
                .push(match last_parent.get_child_mut_unchecked(index) {
                    NodeMut::Leaf(leaf) => todo!(),
                    NodeMut::Node(parent_node_mut) => todo!(),
                });
        }

        let parent = self.split.split(virtual_node_bounds, old_leaf);

        let mut last_parent = self
            .parents
            .last_mut()
            .expect("parents should not be empty");

        if let NodeDataRef::Leaf(virtual_leaf) = virtual_node.as_data() {
            last_parent.split_into_leaves_unchecked(virtual_node_bounds.extent(), parent);
            match last_parent.get_child_mut_unchecked(index) {
                NodeMut::Leaf(leaf) => T::set_leaf(leaf, virtual_leaf),
                _ => panic!("node should be a leaf"),
            }
        } else {
            last_parent.split_into_nodes_unchecked(virtual_node_bounds.extent(), parent);
            last_parent.set_child(index, virtual_node);
        }
    }

    fn advance(&mut self, mut visit: impl VisitMut<T>) {
        if let Some(new_node) = &self.virtual_node {
            if !self.bounds.is_point() {
                let bounds = OctreeBounds::new(self.min, self.non_virtual_extent);
                if visit.enter(bounds, new_node.as_node_ref()) {
                    self.split();
                    return;
                }
            }

            todo!("inc index");
            return;
        }

        let Some(parent) = self.parents.last() else {
            return;
        };

        if !self.bounds.is_point() {
            let index = self.bounds.small_index_within(parent.extent());
            let node = parent.get_child(index);
            if visit.enter(self.bounds, node) {
                match node {
                    NodeRef::Leaf(leaf) => {
                        let extent = self
                            .bounds
                            .extent()
                            .split(self.split_list.levels[self.parents.len()]);
                        self.virtual_node = Some(VirtualNode {
                            node: T::new(extent, T::clone_leaf(leaf)),
                            min: self.bounds.min(),
                            splits: NonZeroU8::MIN,
                        });
                    }
                    NodeRef::Parent(parent_node_ref) => {
                        let NodeMut::Node(node) = parent.get_child_mut_unchecked(index) else {
                            panic!("node should be a parent");
                        };
                        self.parents.push(node.0);
                    }
                }
                return;
            }
        }

        todo!("inc index");
    }

    fn current(&mut self) -> Option<NodeMutAt<T>> {
        Some(
            if let Some(VirtualNode { node, .. }) = &mut self.virtual_node {
                if node.extent() == OctreeExtent::ONE {
                    NodeMutAt::Point {
                        point: self.min,
                        leaf: if let NodeDataMut::Leaf(leaf) = node.as_data_mut() {
                            leaf
                        } else {
                            panic!("point node should be a leaf")
                        },
                    }
                } else {
                    NodeMutAt::Bounds(BoundedNodeMut {
                        min: self.min,
                        node,
                    })
                }
            } else {
                let extent = Self::last_non_virtual_extent(&mut self.parents, &self.split_list);
                let parent = self.parents.last_mut()?;
                let index = OctreeBounds::new(self.min, extent).small_index_within(parent.extent());
                NodeMutAt::Bounds(BoundedNodeMut {
                    min: self.min,
                    node: if let NodeMut::Node(parent) = parent.get_child_mut_unchecked(index) {
                        parent.0
                    } else {
                        panic!("node should be a parent")
                    },
                })
            },
        )
    }
}

impl<'a, T: OctreeNode, S: Split<T>> Drop for RootEntered<'a, T, S> {
    fn drop(&mut self) {
        let normalized_after = self.parents.len();
        self.split_if_new_node_changed();
        self.parents.pop_after(normalized_after);

        while let Some(parent) = self.parents.pop() {
            parent.renormalize();
        }
    }
}

struct VirtualNode<T> {
    depth: NonZeroU8,
    node: T,
}

fn test() {
    let mut node = Node::<i32>::new(OctreeExtent::ONE, 42);
    let mut iter = IterMut::new(&mut node, SplitDefault);
    let a = iter.next(AllLeaves);
    a;
    let b = iter.next(AllLeaves);
    let c = iter.next(AllLeaves);
    drop(iter);
    node;
}

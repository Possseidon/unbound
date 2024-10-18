// pub mod bool;

use std::sync::Arc;

use derive_where::derive_where;

use super::{
    extent::OctreeExtent, NodeDataMut, NodeDataMutParent, NodeDataRef, NodeMut, NodeRef, OctreeNode,
};

/// A node within an octree, either holding a leaf with a value of type `T` or more [`Node`]s.
///
/// Unlike with regular octrees, nodes don't always split into a `2x2x2` of other nodes. Instead,
/// nodes can have any cuboid shape as long as:
///
/// - Its width, height and depth are all powers of two
/// - The volume doesn't exceed 64
///
/// So basically, any cuboid that can be split in half up to 6 times along any combination of axes.
///
/// # Variants
///
/// - `Leaf` is a single leaf holding a value of type `T`
/// - `Parent<N>` holds `N` child nodes, representing `log2(N)` splits
/// - `Leaves<N>` is an optimization for `Parent<N>` nodes that only hold leaves
///
/// Arrays of nodes are stored in separate variants as opposed to something like a [`Vec<T>`] or
/// `Box<[T]>`. While this is a bit inconvenient to work with, it saves on memory (which quickly
/// adds up) and also improves type-safety.
///
/// Additionally, parent nodes are stored in [`Arc`]s, making clone very cheap, as long as the
/// contents of the node is not modified.
#[derive(Clone, Debug)]
#[derive_where(Hash, PartialEq, Eq; T, P)]
pub struct Node<T, P = (), C = ()>(Repr<T, P, C>);

impl<T: Clone + Eq, P: Clone, C: Clone> OctreeNode for Node<T, P, C> {
    type Leaf = T;

    type LeafRef<'a>
        = &'a T
    where
        T: 'a;

    type LeafMut<'a>
        = &'a mut T
    where
        T: 'a;

    fn freeze_leaf(leaf: Self::LeafMut<'_>) -> Self::LeafRef<'_> {
        leaf
    }

    type Parent = P;

    type Cache = C;

    type CacheRef<'a>
        = &'a C
    where
        C: 'a;

    fn new(extent: OctreeExtent, leaf: Self::Leaf) -> Self {
        Self(Repr::Leaf(extent, leaf))
    }

    fn extent(&self) -> OctreeExtent {
        match self.0 {
            Repr::Leaf(extent, _)
            | Repr::Leaves2(extent, _)
            | Repr::Leaves4(extent, _)
            | Repr::Leaves8(extent, _)
            | Repr::Leaves16(extent, _)
            | Repr::Leaves32(extent, _)
            | Repr::Leaves64(extent, _)
            | Repr::Parent2(extent, _)
            | Repr::Parent4(extent, _)
            | Repr::Parent8(extent, _)
            | Repr::Parent16(extent, _)
            | Repr::Parent32(extent, _)
            | Repr::Parent64(extent, _) => extent,
        }
    }

    fn as_data(&self) -> NodeDataRef<Self> {
        let (parent, cache) = match &self.0 {
            Repr::Leaf(_, leaf) => return NodeDataRef::Leaf(leaf),
            Repr::Leaves2(_, node) => (&node.parent, &node.cache),
            Repr::Leaves4(_, node) => (&node.parent, &node.cache),
            Repr::Leaves8(_, node) => (&node.parent, &node.cache),
            Repr::Leaves16(_, node) => (&node.parent, &node.cache),
            Repr::Leaves32(_, node) => (&node.parent, &node.cache),
            Repr::Leaves64(_, node) => (&node.parent, &node.cache),
            Repr::Parent2(_, node) => (&node.parent, &node.cache),
            Repr::Parent4(_, node) => (&node.parent, &node.cache),
            Repr::Parent8(_, node) => (&node.parent, &node.cache),
            Repr::Parent16(_, node) => (&node.parent, &node.cache),
            Repr::Parent32(_, node) => (&node.parent, &node.cache),
            Repr::Parent64(_, node) => (&node.parent, &node.cache),
        };
        NodeDataRef::Parent(parent, cache)
    }

    fn as_data_mut_parent(&mut self) -> NodeDataMutParent<Self> {
        let (parent, cache) = match &mut self.0 {
            Repr::Leaf(_, leaf) => return NodeDataMutParent::Leaf(&*leaf),
            Repr::Leaves2(_, node) => Arc::make_mut(node).parent_mut_and_cache(),
            Repr::Leaves4(_, node) => Arc::make_mut(node).parent_mut_and_cache(),
            Repr::Leaves8(_, node) => Arc::make_mut(node).parent_mut_and_cache(),
            Repr::Leaves16(_, node) => Arc::make_mut(node).parent_mut_and_cache(),
            Repr::Leaves32(_, node) => Arc::make_mut(node).parent_mut_and_cache(),
            Repr::Leaves64(_, node) => Arc::make_mut(node).parent_mut_and_cache(),
            Repr::Parent2(_, node) => Arc::make_mut(node).parent_mut_and_cache(),
            Repr::Parent4(_, node) => Arc::make_mut(node).parent_mut_and_cache(),
            Repr::Parent8(_, node) => Arc::make_mut(node).parent_mut_and_cache(),
            Repr::Parent16(_, node) => Arc::make_mut(node).parent_mut_and_cache(),
            Repr::Parent32(_, node) => Arc::make_mut(node).parent_mut_and_cache(),
            Repr::Parent64(_, node) => Arc::make_mut(node).parent_mut_and_cache(),
        };
        NodeDataMutParent::Parent(parent, cache)
    }

    fn as_data_mut(&mut self) -> NodeDataMut<Self> {
        let (parent, cache) = match &mut self.0 {
            Repr::Leaf(_, leaf) => return NodeDataMut::Leaf(leaf),
            Repr::Leaves2(_, node) => Arc::make_mut(node).parent_mut_and_cache(),
            Repr::Leaves4(_, node) => Arc::make_mut(node).parent_mut_and_cache(),
            Repr::Leaves8(_, node) => Arc::make_mut(node).parent_mut_and_cache(),
            Repr::Leaves16(_, node) => Arc::make_mut(node).parent_mut_and_cache(),
            Repr::Leaves32(_, node) => Arc::make_mut(node).parent_mut_and_cache(),
            Repr::Leaves64(_, node) => Arc::make_mut(node).parent_mut_and_cache(),
            Repr::Parent2(_, node) => Arc::make_mut(node).parent_mut_and_cache(),
            Repr::Parent4(_, node) => Arc::make_mut(node).parent_mut_and_cache(),
            Repr::Parent8(_, node) => Arc::make_mut(node).parent_mut_and_cache(),
            Repr::Parent16(_, node) => Arc::make_mut(node).parent_mut_and_cache(),
            Repr::Parent32(_, node) => Arc::make_mut(node).parent_mut_and_cache(),
            Repr::Parent64(_, node) => Arc::make_mut(node).parent_mut_and_cache(),
        };
        NodeDataMut::Parent(parent, cache)
    }

    fn get_child(&self, index: u8) -> NodeRef<Self> {
        let index = usize::from(index);
        match &self.0 {
            Repr::Leaf(_, _) => panic!("leaf nodes have no children"),
            Repr::Leaves2(_, node) => NodeRef::Leaf(&node.leaves[index]),
            Repr::Leaves4(_, node) => NodeRef::Leaf(&node.leaves[index]),
            Repr::Leaves8(_, node) => NodeRef::Leaf(&node.leaves[index]),
            Repr::Leaves16(_, node) => NodeRef::Leaf(&node.leaves[index]),
            Repr::Leaves32(_, node) => NodeRef::Leaf(&node.leaves[index]),
            Repr::Leaves64(_, node) => NodeRef::Leaf(&node.leaves[index]),
            Repr::Parent2(_, node) => node.children[index].as_node_ref(),
            Repr::Parent4(_, node) => node.children[index].as_node_ref(),
            Repr::Parent8(_, node) => node.children[index].as_node_ref(),
            Repr::Parent16(_, node) => node.children[index].as_node_ref(),
            Repr::Parent32(_, node) => node.children[index].as_node_ref(),
            Repr::Parent64(_, node) => node.children[index].as_node_ref(),
        }
    }

    fn get_child_mut_unchecked(&mut self, index: u8) -> NodeMut<Self> {
        let index = usize::from(index);
        match &mut self.0 {
            Repr::Leaf(_, _) => panic!("leaf nodes have no children"),
            Repr::Leaves2(_, node) => NodeMut::Leaf(&mut Arc::make_mut(node).leaves[index]),
            Repr::Leaves4(_, node) => NodeMut::Leaf(&mut Arc::make_mut(node).leaves[index]),
            Repr::Leaves8(_, node) => NodeMut::Leaf(&mut Arc::make_mut(node).leaves[index]),
            Repr::Leaves16(_, node) => NodeMut::Leaf(&mut Arc::make_mut(node).leaves[index]),
            Repr::Leaves32(_, node) => NodeMut::Leaf(&mut Arc::make_mut(node).leaves[index]),
            Repr::Leaves64(_, node) => NodeMut::Leaf(&mut Arc::make_mut(node).leaves[index]),
            Repr::Parent2(_, node) => Arc::make_mut(node).children[index].as_node_mut(),
            Repr::Parent4(_, node) => Arc::make_mut(node).children[index].as_node_mut(),
            Repr::Parent8(_, node) => Arc::make_mut(node).children[index].as_node_mut(),
            Repr::Parent16(_, node) => Arc::make_mut(node).children[index].as_node_mut(),
            Repr::Parent32(_, node) => Arc::make_mut(node).children[index].as_node_mut(),
            Repr::Parent64(_, node) => Arc::make_mut(node).children[index].as_node_mut(),
        }
    }
}

#[derive(Clone, Debug)]
#[derive_where(Hash, PartialEq, Eq; T, P)]
enum Repr<T, P, C> {
    Leaf(OctreeExtent, T),
    Leaves2(OctreeExtent, Arc<LeavesNode<T, 2, P, C>>),
    Leaves4(OctreeExtent, Arc<LeavesNode<T, 4, P, C>>),
    Leaves8(OctreeExtent, Arc<LeavesNode<T, 8, P, C>>),
    Leaves16(OctreeExtent, Arc<LeavesNode<T, 16, P, C>>),
    Leaves32(OctreeExtent, Arc<LeavesNode<T, 32, P, C>>),
    Leaves64(OctreeExtent, Arc<LeavesNode<T, 64, P, C>>),
    Parent2(OctreeExtent, Arc<ParentNode<T, 2, P, C>>),
    Parent4(OctreeExtent, Arc<ParentNode<T, 4, P, C>>),
    Parent8(OctreeExtent, Arc<ParentNode<T, 8, P, C>>),
    Parent16(OctreeExtent, Arc<ParentNode<T, 16, P, C>>),
    Parent32(OctreeExtent, Arc<ParentNode<T, 32, P, C>>),
    Parent64(OctreeExtent, Arc<ParentNode<T, 64, P, C>>),
}

/// Holds storage for `N` leaf nodes of type `T`, parent data `P` and a cached value `C`.
///
/// Cached data is intentionally ignored by [`Eq`] and friends, since it does not carry any extra
/// information that is not already present in `leaves`.
#[derive(Clone, Copy, Debug)]
#[derive_where(Hash, PartialEq, Eq; T, P)]
pub(crate) struct LeavesNode<T, const N: usize, P, C> {
    leaves: [T; N],
    parent: P,
    #[derive_where(skip(EqHashOrd))]
    cache: C,
}

impl<T, const N: usize, P, C> LeavesNode<T, N, P, C> {
    fn parent_mut_and_cache(&mut self) -> (&mut P, &C) {
        (&mut self.parent, &self.cache)
    }
}

/// Holds storage for `N` child [`Node`]s, parent data `P` and a cached value `C`.
///
/// Cached data is intentionally ignored by [`Eq`] and friends, since it does not carry any extra
/// information that is not already present in `leaves`.
#[derive(Clone, Debug)]
#[derive_where(Hash, PartialEq, Eq; T, P)]
pub(crate) struct ParentNode<T, const N: usize, P, C> {
    children: [Node<T, P, C>; N],
    parent: P,
    #[derive_where(skip(EqHashOrd))]
    cache: C,
}

impl<T, const N: usize, P, C> ParentNode<T, N, P, C> {
    fn parent_mut_and_cache(&mut self) -> (&mut P, &C) {
        (&mut self.parent, &self.cache)
    }
}

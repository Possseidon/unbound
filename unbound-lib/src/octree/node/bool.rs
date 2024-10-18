use std::{
    hash::{Hash, Hasher},
    sync::Arc,
};

use crate::octree::{extent::OctreeExtent, NodeDataMut, NodeDataRef, NodeRef, OctreeNode};

/// An octree node that stores [`bool`]s as single bits where it makes sense.
///
/// Should only be used if `size_of::<P>() <= ptr - 4` and if `P` is cheap to clone, which the size
/// limit more or less implies, since "expensive to clone" generally also means storing some
/// pointer.
///
/// If `P` is bigger, [`NodeWithLargeParent`] remains the same size no matter how big `P`
/// is, at the cost of "leaves" nodes containing an extra indirection.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Node<P = ()>(Repr<P>);

impl<P> OctreeNode for Node<P> {
    type Leaf = bool;
    type LeafRef<'a> = bool where Self: 'a;
    type LeafMut<'a> = BoolMut<'a> where Self: 'a;

    type Parent = P;

    type Cache = ();
    type CacheRef<'a> = () where Self: 'a;

    fn new(extent: OctreeExtent, leaf: Self::Leaf) -> Self {
        Self(Repr::Leaf(extent, leaf))
    }

    fn extent(&self) -> OctreeExtent {
        match self.0 {
            Repr::Leaf(extent, _)
            | Repr::Leaves(extent, _, _)
            | Repr::Parent2(extent, _, _)
            | Repr::Parent4(extent, _, _)
            | Repr::Parent8(extent, _, _)
            | Repr::Parent16(extent, _, _)
            | Repr::Parent32(extent, _, _)
            | Repr::Parent64(extent, _, _) => extent,
        }
    }

    fn as_data(&self) -> NodeDataRef<Self> {
        match &self.0 {
            Repr::Leaf(_, leaf) => NodeDataRef::Leaf(*leaf),
            Repr::Leaves(_, parent, _)
            | Repr::Parent2(_, parent, _)
            | Repr::Parent4(_, parent, _)
            | Repr::Parent8(_, parent, _)
            | Repr::Parent16(_, parent, _)
            | Repr::Parent32(_, parent, _)
            | Repr::Parent64(_, parent, _) => NodeDataRef::Parent(parent, ()),
        }
    }

    fn get_child(&self, index: u8) -> Option<NodeRef<Self>> {
        let index = usize::from(index);
        Some(match &self.0 {
            Repr::Leaf(_, _) => return None,
            Repr::Leaves(_, _, leaves) => NodeRef::Leaf(1 << index & *leaves != 0),
            Repr::Parent2(_, _, children) => children[index].as_node_ref(),
            Repr::Parent4(_, _, children) => children[index].as_node_ref(),
            Repr::Parent8(_, _, children) => children[index].as_node_ref(),
            Repr::Parent16(_, _, children) => children[index].as_node_ref(),
            Repr::Parent32(_, _, children) => children[index].as_node_ref(),
            Repr::Parent64(_, _, children) => children[index].as_node_ref(),
        })
    }
}

/// An octree node that stores [`bool`]s and keeps track of the total number of set bits.
///
/// The count requires an extra [`u128`] per "proper" parent node when compared to [`Node`].
///
/// Should only be used if `size_of::<P>() <= ptr - 4` and if `P` is cheap to clone, which the size
/// limit more or less implies, since "expensive to clone" generally also means storing some
/// pointer.
///
/// If `P` is bigger, [`NodeWithLargeParentAndCount`] remains the same size no matter how big `P`
/// is, at the cost of "leaves" nodes containing an extra indirection.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct NodeWithCount<P = ()>(ReprWithCount<P>);

impl<P> OctreeNode for NodeWithCount<P> {
    type Leaf = bool;
    type LeafRef<'a> = bool where Self: 'a;
    type LeafMut<'a> = BoolMut<'a> where Self: 'a;

    type Parent = P;

    type Cache = u128;
    type CacheRef<'a> = u128 where Self: 'a;

    fn new(extent: OctreeExtent, leaf: Self::Leaf) -> Self {
        Self(ReprWithCount::Leaf(extent, leaf))
    }

    fn extent(&self) -> OctreeExtent {
        match self.0 {
            ReprWithCount::Leaf(extent, _)
            | ReprWithCount::Leaves(extent, _, _)
            | ReprWithCount::Parent2(extent, _, _)
            | ReprWithCount::Parent4(extent, _, _)
            | ReprWithCount::Parent8(extent, _, _)
            | ReprWithCount::Parent16(extent, _, _)
            | ReprWithCount::Parent32(extent, _, _)
            | ReprWithCount::Parent64(extent, _, _) => extent,
        }
    }

    fn as_data(&self) -> NodeDataRef<Self> {
        let (parent, count) = match &self.0 {
            ReprWithCount::Leaf(_, leaf) => return NodeDataRef::Leaf(*leaf),
            ReprWithCount::Leaves(_, parent, leaves) => (parent, leaves.count_ones().into()),
            ReprWithCount::Parent2(_, parent, node) => (parent, node.count),
            ReprWithCount::Parent4(_, parent, node) => (parent, node.count),
            ReprWithCount::Parent8(_, parent, node) => (parent, node.count),
            ReprWithCount::Parent16(_, parent, node) => (parent, node.count),
            ReprWithCount::Parent32(_, parent, node) => (parent, node.count),
            ReprWithCount::Parent64(_, parent, node) => (parent, node.count),
        };
        NodeDataRef::Parent(parent, count)
    }

    fn get_child(&self, index: u8) -> Option<NodeRef<Self>> {
        let index = usize::from(index);
        Some(match &self.0 {
            ReprWithCount::Leaf(_, _) => return None,
            ReprWithCount::Leaves(_, _, leaves) => NodeRef::Leaf(1 << index & *leaves != 0),
            ReprWithCount::Parent2(_, _, node) => node.children[index].as_node_ref(),
            ReprWithCount::Parent4(_, _, node) => node.children[index].as_node_ref(),
            ReprWithCount::Parent8(_, _, node) => node.children[index].as_node_ref(),
            ReprWithCount::Parent16(_, _, node) => node.children[index].as_node_ref(),
            ReprWithCount::Parent32(_, _, node) => node.children[index].as_node_ref(),
            ReprWithCount::Parent64(_, _, node) => node.children[index].as_node_ref(),
        })
    }
}

/// A [`Node`] that can store large `P` without bloating the size of the node itself.
///
/// This has the cost of introducing an extra indirection for [`NodeWithLargeParent::Leaves`].
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct NodeWithLargeParent<P>(ReprWithLargeParent<P>);

impl<P> OctreeNode for NodeWithLargeParent<P> {
    type Leaf = bool;
    type LeafRef<'a> = bool where Self: 'a;
    type LeafMut<'a> = BoolMut<'a> where Self: 'a;

    type Parent = P;

    type Cache = ();
    type CacheRef<'a> = () where Self: 'a;

    fn new(extent: OctreeExtent, leaf: Self::Leaf) -> Self {
        Self(ReprWithLargeParent::Leaf(extent, leaf))
    }

    fn extent(&self) -> OctreeExtent {
        match self.0 {
            ReprWithLargeParent::Leaf(extent, _)
            | ReprWithLargeParent::Leaves(extent, _)
            | ReprWithLargeParent::Parent2(extent, _)
            | ReprWithLargeParent::Parent4(extent, _)
            | ReprWithLargeParent::Parent8(extent, _)
            | ReprWithLargeParent::Parent16(extent, _)
            | ReprWithLargeParent::Parent32(extent, _)
            | ReprWithLargeParent::Parent64(extent, _) => extent,
        }
    }

    fn as_data(&self) -> NodeDataRef<Self> {
        NodeDataRef::Parent(
            match &self.0 {
                ReprWithLargeParent::Leaf(_, leaf) => return NodeDataRef::Leaf(*leaf),
                ReprWithLargeParent::Leaves(_, node) => &node.1,
                ReprWithLargeParent::Parent2(_, node) => &node.parent,
                ReprWithLargeParent::Parent4(_, node) => &node.parent,
                ReprWithLargeParent::Parent8(_, node) => &node.parent,
                ReprWithLargeParent::Parent16(_, node) => &node.parent,
                ReprWithLargeParent::Parent32(_, node) => &node.parent,
                ReprWithLargeParent::Parent64(_, node) => &node.parent,
            },
            (),
        )
    }

    fn get_child(&self, index: u8) -> Option<NodeRef<Self>> {
        let index = usize::from(index);
        Some(match &self.0 {
            ReprWithLargeParent::Leaf(_, _) => return None,
            ReprWithLargeParent::Leaves(_, leaves) => NodeRef::Leaf(1 << index & leaves.0 != 0),
            ReprWithLargeParent::Parent2(_, node) => node.children[index].as_node_ref(),
            ReprWithLargeParent::Parent4(_, node) => node.children[index].as_node_ref(),
            ReprWithLargeParent::Parent8(_, node) => node.children[index].as_node_ref(),
            ReprWithLargeParent::Parent16(_, node) => node.children[index].as_node_ref(),
            ReprWithLargeParent::Parent32(_, node) => node.children[index].as_node_ref(),
            ReprWithLargeParent::Parent64(_, node) => node.children[index].as_node_ref(),
        })
    }
}

/// A combination of [`NodeWithLargeParent`] and [`NodeWithCount`].
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct NodeWithLargeParentAndCount<P>(ReprWithLargeParentAndCount<P>);

impl<P> OctreeNode for NodeWithLargeParentAndCount<P> {
    type Leaf = bool;
    type LeafRef<'a> = bool where Self: 'a;
    type LeafMut<'a> = BoolMut<'a> where Self: 'a;

    type Parent = P;

    type Cache = u128;
    type CacheRef<'a> = u128 where Self: 'a;

    fn new(extent: OctreeExtent, leaf: Self::Leaf) -> Self {
        Self(ReprWithLargeParentAndCount::Leaf(extent, leaf))
    }

    fn extent(&self) -> OctreeExtent {
        match self.0 {
            ReprWithLargeParentAndCount::Leaf(extent, _)
            | ReprWithLargeParentAndCount::Leaves(extent, _)
            | ReprWithLargeParentAndCount::Parent2(extent, _)
            | ReprWithLargeParentAndCount::Parent4(extent, _)
            | ReprWithLargeParentAndCount::Parent8(extent, _)
            | ReprWithLargeParentAndCount::Parent16(extent, _)
            | ReprWithLargeParentAndCount::Parent32(extent, _)
            | ReprWithLargeParentAndCount::Parent64(extent, _) => extent,
        }
    }

    fn as_data(&self) -> NodeDataRef<Self> {
        let (parent, count) = match &self.0 {
            ReprWithLargeParentAndCount::Leaf(_, leaf) => return NodeDataRef::Leaf(*leaf),
            ReprWithLargeParentAndCount::Leaves(_, leaves) => {
                let (bits, parent) = &**leaves;
                (parent, bits.count_ones().into())
            }
            ReprWithLargeParentAndCount::Parent2(_, node) => (&node.parent, node.count),
            ReprWithLargeParentAndCount::Parent4(_, node) => (&node.parent, node.count),
            ReprWithLargeParentAndCount::Parent8(_, node) => (&node.parent, node.count),
            ReprWithLargeParentAndCount::Parent16(_, node) => (&node.parent, node.count),
            ReprWithLargeParentAndCount::Parent32(_, node) => (&node.parent, node.count),
            ReprWithLargeParentAndCount::Parent64(_, node) => (&node.parent, node.count),
        };
        NodeDataRef::Parent(parent, count)
    }

    fn get_child(&self, index: u8) -> Option<NodeRef<Self>> {
        let index = usize::from(index);
        Some(match &self.0 {
            ReprWithLargeParentAndCount::Leaf(_, _) => return None,
            ReprWithLargeParentAndCount::Leaves(_, leaves) => {
                NodeRef::Leaf(1 << index & leaves.0 != 0)
            }
            ReprWithLargeParentAndCount::Parent2(_, node) => node.children[index].as_node_ref(),
            ReprWithLargeParentAndCount::Parent4(_, node) => node.children[index].as_node_ref(),
            ReprWithLargeParentAndCount::Parent8(_, node) => node.children[index].as_node_ref(),
            ReprWithLargeParentAndCount::Parent16(_, node) => node.children[index].as_node_ref(),
            ReprWithLargeParentAndCount::Parent32(_, node) => node.children[index].as_node_ref(),
            ReprWithLargeParentAndCount::Parent64(_, node) => node.children[index].as_node_ref(),
        })
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
enum Repr<P> {
    Leaf(OctreeExtent, bool),
    Leaves(OctreeExtent, P, u64),
    Parent2(OctreeExtent, P, Arc<[Node<P>; 2]>),
    Parent4(OctreeExtent, P, Arc<[Node<P>; 4]>),
    Parent8(OctreeExtent, P, Arc<[Node<P>; 8]>),
    Parent16(OctreeExtent, P, Arc<[Node<P>; 16]>),
    Parent32(OctreeExtent, P, Arc<[Node<P>; 32]>),
    Parent64(OctreeExtent, P, Arc<[Node<P>; 64]>),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
enum ReprWithCount<P> {
    Leaf(OctreeExtent, bool),
    Leaves(OctreeExtent, P, u64),
    Parent2(OctreeExtent, P, Arc<ParentNodeWithCount<2, P>>),
    Parent4(OctreeExtent, P, Arc<ParentNodeWithCount<4, P>>),
    Parent8(OctreeExtent, P, Arc<ParentNodeWithCount<8, P>>),
    Parent16(OctreeExtent, P, Arc<ParentNodeWithCount<16, P>>),
    Parent32(OctreeExtent, P, Arc<ParentNodeWithCount<32, P>>),
    Parent64(OctreeExtent, P, Arc<ParentNodeWithCount<64, P>>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct ParentNodeWithCount<const N: usize, P> {
    /// The total number of set bits across all children.
    ///
    /// Can neither be `0` nor the maximum value (depending on the current size), since those would
    /// be stored as [`NodeWithCount::Leaf`].
    ///
    /// Skipped by [`Hash`], since the same `children` always lead to the same `count`.
    ///
    /// On the other hand, it makes a lot of sense for [`Eq`] to make use of it, since it has the
    /// potential to very quickly short-circuit the comparison of large octrees.
    count: u128,
    /// The child nodes.
    children: [NodeWithCount<P>; N],
}

impl<const N: usize, P: Hash> Hash for ParentNodeWithCount<N, P> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.children.hash(state);
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
enum ReprWithLargeParent<P> {
    Leaf(OctreeExtent, bool),
    Leaves(OctreeExtent, Arc<(u64, P)>),
    Parent2(OctreeExtent, Arc<LargeParentNode<2, P>>),
    Parent4(OctreeExtent, Arc<LargeParentNode<4, P>>),
    Parent8(OctreeExtent, Arc<LargeParentNode<8, P>>),
    Parent16(OctreeExtent, Arc<LargeParentNode<16, P>>),
    Parent32(OctreeExtent, Arc<LargeParentNode<32, P>>),
    Parent64(OctreeExtent, Arc<LargeParentNode<64, P>>),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
struct LargeParentNode<const N: usize, P> {
    children: [NodeWithLargeParent<P>; N],
    parent: P,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
enum ReprWithLargeParentAndCount<P> {
    Leaf(OctreeExtent, bool),
    Leaves(OctreeExtent, Arc<(u64, P)>),
    Parent2(OctreeExtent, Arc<LargeParentNodeWithCount<2, P>>),
    Parent4(OctreeExtent, Arc<LargeParentNodeWithCount<4, P>>),
    Parent8(OctreeExtent, Arc<LargeParentNodeWithCount<8, P>>),
    Parent16(OctreeExtent, Arc<LargeParentNodeWithCount<16, P>>),
    Parent32(OctreeExtent, Arc<LargeParentNodeWithCount<32, P>>),
    Parent64(OctreeExtent, Arc<LargeParentNodeWithCount<64, P>>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct LargeParentNodeWithCount<const N: usize, P> {
    /// The total number of set bits across all children.
    ///
    /// Can neither be `0` nor the maximum value (depending on the current size), since those would
    /// be stored as [`NodeWithCount::Leaf`].
    ///
    /// Skipped by [`Hash`], since the same `children` always lead to the same `count`.
    ///
    /// On the other hand, it makes a lot of sense for [`Eq`] to make use of it, since it has the
    /// potential to very quickly short-circuit the comparison of large octrees.
    count: u128,
    /// The child nodes.
    children: [NodeWithLargeParentAndCount<P>; N],
    parent: P,
}

impl<const N: usize, P: Hash> Hash for LargeParentNodeWithCount<N, P> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.children.hash(state);
        self.parent.hash(state);
    }
}

pub struct BoolMut<'a>(ReprBoolMut<'a>);

enum ReprBoolMut<'a> {
    Leaf(&'a mut bool),
    Leaves(&'a mut u64, u8),
}

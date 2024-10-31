use std::{
    hash::{Hash, Hasher},
    sync::Arc,
};

use arrayvec::ArrayVec;

use crate::octree::{
    cache::{CacheInput, OctreeCache},
    extent::{OctreeExtent, OctreeSplits},
    NodeDataRef, NodeRef, OctreeNode,
};

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

impl<P: Clone> OctreeNode for Node<P> {
    type Leaf = bool;
    type LeafRef<'a> = bool;
    type LeavesBuilder = LeavesBuilder;

    type Parent = P;

    type Cache<'a> = NoCache;

    fn new(extent: OctreeExtent, leaf: Self::Leaf) -> Self {
        Self(Repr::Leaf(extent, leaf))
    }

    fn from_leaves_unchecked(
        _total_splits: u8,
        extent: OctreeExtent,
        _child_extent: OctreeExtent,
        leaves: Self::LeavesBuilder,
        parent: Self::Parent,
    ) -> Self {
        Self(Repr::Leaves(extent, parent, leaves.leaves))
    }

    fn from_nodes_unchecked(
        total_splits: u8,
        extent: OctreeExtent,
        _child_extent: OctreeExtent,
        nodes: ArrayVec<Self, { OctreeSplits::MAX_VOLUME_USIZE }>,
        parent: Self::Parent,
    ) -> Self {
        Self(match total_splits {
            1 => Repr::from_nodes(Repr::Parent1, extent, nodes, parent),
            2 => Repr::from_nodes(Repr::Parent2, extent, nodes, parent),
            3 => Repr::from_nodes(Repr::Parent3, extent, nodes, parent),
            4 => Repr::from_nodes(Repr::Parent4, extent, nodes, parent),
            5 => Repr::from_nodes(Repr::Parent5, extent, nodes, parent),
            6 => Repr::from_nodes(Repr::Parent6, extent, nodes, parent),
            _ => panic!("invalid number of splits"),
        })
    }

    fn extent(&self) -> OctreeExtent {
        match self.0 {
            Repr::Leaf(extent, _)
            | Repr::Leaves(extent, _, _)
            | Repr::Parent1(extent, _, _)
            | Repr::Parent2(extent, _, _)
            | Repr::Parent3(extent, _, _)
            | Repr::Parent4(extent, _, _)
            | Repr::Parent5(extent, _, _)
            | Repr::Parent6(extent, _, _) => extent,
        }
    }

    fn as_data(&self) -> NodeDataRef<Self> {
        match &self.0 {
            Repr::Leaf(_, leaf) => NodeDataRef::Leaf(*leaf),
            Repr::Leaves(_, parent, _)
            | Repr::Parent1(_, parent, _)
            | Repr::Parent2(_, parent, _)
            | Repr::Parent3(_, parent, _)
            | Repr::Parent4(_, parent, _)
            | Repr::Parent5(_, parent, _)
            | Repr::Parent6(_, parent, _) => NodeDataRef::Parent(parent, ()),
        }
    }

    fn into_leaf(self) -> Result<Self::Leaf, Self> {
        if let Repr::Leaf(_, leaf) = self.0 {
            Ok(leaf)
        } else {
            Err(self)
        }
    }

    fn get_child(&self, index: u8) -> NodeRef<Self> {
        let index = usize::from(index);
        match &self.0 {
            Repr::Leaf(..) => panic!("leaf nodes have no children"),
            Repr::Leaves(_, _, leaves) => NodeRef::Leaf(1 << index & *leaves != 0),
            Repr::Parent1(_, _, children) => NodeRef::Node(&children[index]),
            Repr::Parent2(_, _, children) => NodeRef::Node(&children[index]),
            Repr::Parent3(_, _, children) => NodeRef::Node(&children[index]),
            Repr::Parent4(_, _, children) => NodeRef::Node(&children[index]),
            Repr::Parent5(_, _, children) => NodeRef::Node(&children[index]),
            Repr::Parent6(_, _, children) => NodeRef::Node(&children[index]),
        }
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

impl<P: Clone> OctreeNode for NodeWithCount<P> {
    type Leaf = bool;
    type LeafRef<'a> = bool;
    type LeavesBuilder = LeavesBuilder;

    type Parent = P;

    type Cache<'a> = Count;

    fn new(extent: OctreeExtent, leaf: Self::Leaf) -> Self {
        Self(ReprWithCount::Leaf(extent, leaf))
    }

    fn from_leaves_unchecked(
        _total_splits: u8,
        extent: OctreeExtent,
        _child_extent: OctreeExtent,
        leaves: Self::LeavesBuilder,
        parent: Self::Parent,
    ) -> Self {
        Self(ReprWithCount::Leaves(extent, parent, leaves.leaves))
    }

    fn from_nodes_unchecked(
        total_splits: u8,
        extent: OctreeExtent,
        child_extent: OctreeExtent,
        nodes: ArrayVec<Self, { OctreeSplits::MAX_VOLUME_USIZE }>,
        parent: Self::Parent,
    ) -> Self {
        use ReprWithCount as Repr;
        Self(match total_splits {
            1 => Repr::from_nodes(Repr::Parent1, extent, child_extent, nodes, parent),
            2 => Repr::from_nodes(Repr::Parent2, extent, child_extent, nodes, parent),
            3 => Repr::from_nodes(Repr::Parent3, extent, child_extent, nodes, parent),
            4 => Repr::from_nodes(Repr::Parent4, extent, child_extent, nodes, parent),
            5 => Repr::from_nodes(Repr::Parent5, extent, child_extent, nodes, parent),
            6 => Repr::from_nodes(Repr::Parent6, extent, child_extent, nodes, parent),
            _ => panic!("invalid number of splits"),
        })
    }

    fn extent(&self) -> OctreeExtent {
        match self.0 {
            ReprWithCount::Leaf(extent, _)
            | ReprWithCount::Leaves(extent, _, _)
            | ReprWithCount::Parent1(extent, _, _)
            | ReprWithCount::Parent2(extent, _, _)
            | ReprWithCount::Parent3(extent, _, _)
            | ReprWithCount::Parent4(extent, _, _)
            | ReprWithCount::Parent5(extent, _, _)
            | ReprWithCount::Parent6(extent, _, _) => extent,
        }
    }

    fn as_data(&self) -> NodeDataRef<Self> {
        match &self.0 {
            ReprWithCount::Leaf(_, leaf) => return NodeDataRef::Leaf(*leaf),
            ReprWithCount::Leaves(_, parent, leaves) => {
                NodeDataRef::Parent(parent, Count(leaves.count_ones().into()))
            }
            ReprWithCount::Parent1(_, parent, node) => NodeDataRef::Parent(parent, node.count),
            ReprWithCount::Parent2(_, parent, node) => NodeDataRef::Parent(parent, node.count),
            ReprWithCount::Parent3(_, parent, node) => NodeDataRef::Parent(parent, node.count),
            ReprWithCount::Parent4(_, parent, node) => NodeDataRef::Parent(parent, node.count),
            ReprWithCount::Parent5(_, parent, node) => NodeDataRef::Parent(parent, node.count),
            ReprWithCount::Parent6(_, parent, node) => NodeDataRef::Parent(parent, node.count),
        }
    }

    fn into_leaf(self) -> Result<Self::Leaf, Self> {
        if let ReprWithCount::Leaf(_, leaf) = self.0 {
            Ok(leaf)
        } else {
            Err(self)
        }
    }

    fn get_child(&self, index: u8) -> NodeRef<Self> {
        let index = usize::from(index);
        match &self.0 {
            ReprWithCount::Leaf(_, _) => panic!("leaf nodes have no children"),
            ReprWithCount::Leaves(_, _, leaves) => NodeRef::Leaf(1 << index & *leaves != 0),
            ReprWithCount::Parent1(_, _, node) => NodeRef::Node(&node.children[index]),
            ReprWithCount::Parent2(_, _, node) => NodeRef::Node(&node.children[index]),
            ReprWithCount::Parent3(_, _, node) => NodeRef::Node(&node.children[index]),
            ReprWithCount::Parent4(_, _, node) => NodeRef::Node(&node.children[index]),
            ReprWithCount::Parent5(_, _, node) => NodeRef::Node(&node.children[index]),
            ReprWithCount::Parent6(_, _, node) => NodeRef::Node(&node.children[index]),
        }
    }
}

/// A [`Node`] that can store large `P` without bloating the size of the node itself.
///
/// This has the cost of introducing an extra indirection for [`NodeWithLargeParent::Leaves`].
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct NodeWithLargeParent<P>(ReprWithLargeParent<P>);

impl<P: Clone> OctreeNode for NodeWithLargeParent<P> {
    type Leaf = bool;
    type LeafRef<'a> = bool;
    type LeavesBuilder = LeavesBuilder;

    type Parent = P;

    type Cache<'a> = NoCache;

    fn new(extent: OctreeExtent, leaf: Self::Leaf) -> Self {
        Self(ReprWithLargeParent::Leaf(extent, leaf))
    }

    fn from_leaves_unchecked(
        _total_splits: u8,
        extent: OctreeExtent,
        _child_extent: OctreeExtent,
        leaves: Self::LeavesBuilder,
        parent: Self::Parent,
    ) -> Self {
        Self(ReprWithLargeParent::Leaves(
            extent,
            Arc::new((leaves.leaves, parent)),
        ))
    }

    fn from_nodes_unchecked(
        total_splits: u8,
        extent: OctreeExtent,
        _child_extent: OctreeExtent,
        nodes: ArrayVec<Self, { OctreeSplits::MAX_VOLUME_USIZE }>,
        parent: Self::Parent,
    ) -> Self {
        use ReprWithLargeParent as Repr;
        Self(match total_splits {
            1 => Repr::from_nodes(Repr::Parent1, extent, nodes, parent),
            2 => Repr::from_nodes(Repr::Parent2, extent, nodes, parent),
            3 => Repr::from_nodes(Repr::Parent3, extent, nodes, parent),
            4 => Repr::from_nodes(Repr::Parent4, extent, nodes, parent),
            5 => Repr::from_nodes(Repr::Parent5, extent, nodes, parent),
            6 => Repr::from_nodes(Repr::Parent6, extent, nodes, parent),
            _ => panic!("invalid number of splits"),
        })
    }

    fn extent(&self) -> OctreeExtent {
        match self.0 {
            ReprWithLargeParent::Leaf(extent, _)
            | ReprWithLargeParent::Leaves(extent, _)
            | ReprWithLargeParent::Parent1(extent, _)
            | ReprWithLargeParent::Parent2(extent, _)
            | ReprWithLargeParent::Parent3(extent, _)
            | ReprWithLargeParent::Parent4(extent, _)
            | ReprWithLargeParent::Parent5(extent, _)
            | ReprWithLargeParent::Parent6(extent, _) => extent,
        }
    }

    fn as_data(&self) -> NodeDataRef<Self> {
        match &self.0 {
            ReprWithLargeParent::Leaf(_, leaf) => NodeDataRef::Leaf(*leaf),
            ReprWithLargeParent::Leaves(_, node) => NodeDataRef::Parent(&node.1, ()),
            ReprWithLargeParent::Parent1(_, node) => NodeDataRef::Parent(&node.parent, ()),
            ReprWithLargeParent::Parent2(_, node) => NodeDataRef::Parent(&node.parent, ()),
            ReprWithLargeParent::Parent3(_, node) => NodeDataRef::Parent(&node.parent, ()),
            ReprWithLargeParent::Parent4(_, node) => NodeDataRef::Parent(&node.parent, ()),
            ReprWithLargeParent::Parent5(_, node) => NodeDataRef::Parent(&node.parent, ()),
            ReprWithLargeParent::Parent6(_, node) => NodeDataRef::Parent(&node.parent, ()),
        }
    }

    fn into_leaf(self) -> Result<Self::Leaf, Self> {
        if let ReprWithLargeParent::Leaf(_, leaf) = self.0 {
            Ok(leaf)
        } else {
            Err(self)
        }
    }

    fn get_child(&self, index: u8) -> NodeRef<Self> {
        let index = usize::from(index);
        match &self.0 {
            ReprWithLargeParent::Leaf(_, _) => panic!("leaf nodes have no children"),
            ReprWithLargeParent::Leaves(_, leaves) => NodeRef::Leaf(1 << index & leaves.0 != 0),
            ReprWithLargeParent::Parent1(_, node) => NodeRef::Node(&node.children[index]),
            ReprWithLargeParent::Parent2(_, node) => NodeRef::Node(&node.children[index]),
            ReprWithLargeParent::Parent3(_, node) => NodeRef::Node(&node.children[index]),
            ReprWithLargeParent::Parent4(_, node) => NodeRef::Node(&node.children[index]),
            ReprWithLargeParent::Parent5(_, node) => NodeRef::Node(&node.children[index]),
            ReprWithLargeParent::Parent6(_, node) => NodeRef::Node(&node.children[index]),
        }
    }
}

/// A combination of [`NodeWithLargeParent`] and [`NodeWithCount`].
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct NodeWithLargeParentAndCount<P>(ReprWithLargeParentAndCount<P>);

impl<P: Clone> OctreeNode for NodeWithLargeParentAndCount<P> {
    type Leaf = bool;
    type LeafRef<'a> = bool;
    type LeavesBuilder = LeavesBuilder;

    type Parent = P;

    type Cache<'a> = Count;

    fn new(extent: OctreeExtent, leaf: Self::Leaf) -> Self {
        Self(ReprWithLargeParentAndCount::Leaf(extent, leaf))
    }

    fn from_leaves_unchecked(
        _total_splits: u8,
        extent: OctreeExtent,
        _child_extent: OctreeExtent,
        leaves: Self::LeavesBuilder,
        parent: Self::Parent,
    ) -> Self {
        Self(ReprWithLargeParentAndCount::Leaves(
            extent,
            Arc::new((leaves.leaves, parent)),
        ))
    }

    fn from_nodes_unchecked(
        total_splits: u8,
        extent: OctreeExtent,
        child_extent: OctreeExtent,
        nodes: ArrayVec<Self, { OctreeSplits::MAX_VOLUME_USIZE }>,
        parent: Self::Parent,
    ) -> Self {
        use ReprWithLargeParentAndCount as Repr;
        Self(match total_splits {
            1 => Repr::from_nodes(Repr::Parent1, extent, child_extent, nodes, parent),
            2 => Repr::from_nodes(Repr::Parent2, extent, child_extent, nodes, parent),
            3 => Repr::from_nodes(Repr::Parent3, extent, child_extent, nodes, parent),
            4 => Repr::from_nodes(Repr::Parent4, extent, child_extent, nodes, parent),
            5 => Repr::from_nodes(Repr::Parent5, extent, child_extent, nodes, parent),
            6 => Repr::from_nodes(Repr::Parent6, extent, child_extent, nodes, parent),
            _ => panic!("invalid number of splits"),
        })
    }

    fn extent(&self) -> OctreeExtent {
        match self.0 {
            ReprWithLargeParentAndCount::Leaf(extent, _)
            | ReprWithLargeParentAndCount::Leaves(extent, _)
            | ReprWithLargeParentAndCount::Parent1(extent, _)
            | ReprWithLargeParentAndCount::Parent2(extent, _)
            | ReprWithLargeParentAndCount::Parent3(extent, _)
            | ReprWithLargeParentAndCount::Parent4(extent, _)
            | ReprWithLargeParentAndCount::Parent5(extent, _)
            | ReprWithLargeParentAndCount::Parent6(extent, _) => extent,
        }
    }

    fn as_data(&self) -> NodeDataRef<Self> {
        use ReprWithLargeParentAndCount as Repr; // shortened to avoid wrapping in match arms
        match &self.0 {
            Repr::Leaf(_, leaf) => NodeDataRef::Leaf(*leaf),
            Repr::Leaves(_, leaves) => {
                let (bits, parent) = &**leaves;
                NodeDataRef::Parent(parent, Count(bits.count_ones().into()))
            }
            Repr::Parent1(_, node) => NodeDataRef::Parent(&node.parent, node.count),
            Repr::Parent2(_, node) => NodeDataRef::Parent(&node.parent, node.count),
            Repr::Parent3(_, node) => NodeDataRef::Parent(&node.parent, node.count),
            Repr::Parent4(_, node) => NodeDataRef::Parent(&node.parent, node.count),
            Repr::Parent5(_, node) => NodeDataRef::Parent(&node.parent, node.count),
            Repr::Parent6(_, node) => NodeDataRef::Parent(&node.parent, node.count),
        }
    }

    fn into_leaf(self) -> Result<Self::Leaf, Self> {
        if let ReprWithLargeParentAndCount::Leaf(_, leaf) = self.0 {
            Ok(leaf)
        } else {
            Err(self)
        }
    }

    fn get_child(&self, index: u8) -> NodeRef<Self> {
        let index = usize::from(index);
        match &self.0 {
            ReprWithLargeParentAndCount::Leaf(_, _) => panic!("leaf nodes have no children"),
            ReprWithLargeParentAndCount::Leaves(_, leaves) => {
                NodeRef::Leaf(1 << index & leaves.0 != 0)
            }
            ReprWithLargeParentAndCount::Parent1(_, node) => NodeRef::Node(&node.children[index]),
            ReprWithLargeParentAndCount::Parent2(_, node) => NodeRef::Node(&node.children[index]),
            ReprWithLargeParentAndCount::Parent3(_, node) => NodeRef::Node(&node.children[index]),
            ReprWithLargeParentAndCount::Parent4(_, node) => NodeRef::Node(&node.children[index]),
            ReprWithLargeParentAndCount::Parent5(_, node) => NodeRef::Node(&node.children[index]),
            ReprWithLargeParentAndCount::Parent6(_, node) => NodeRef::Node(&node.children[index]),
        }
    }
}

#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct NoCache;

impl<'a, T> OctreeCache<'a, T> for NoCache {
    type Ref = ();

    fn compute_cache(
        _: OctreeExtent,
        _: impl IntoIterator<Item = CacheInput<'a, T, Self>>,
    ) -> Self {
        Self
    }
}

#[derive(Default)]
pub struct LeavesBuilder {
    leaves: u64,
    len: u8,
}

impl Iterator for LeavesBuilder {
    type Item = bool;

    fn next(&mut self) -> Option<Self::Item> {
        (self.len != 0).then(|| {
            let result = self.leaves & 1 != 0;
            self.leaves >>= 1;
            self.len -= 1;
            result
        })
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.len.into();
        (len, Some(len))
    }
}

impl ExactSizeIterator for LeavesBuilder {}

impl Extend<bool> for LeavesBuilder {
    fn extend<T: IntoIterator<Item = bool>>(&mut self, iter: T) {
        for item in iter {
            if self.len == OctreeSplits::MAX_VOLUME {
                panic!("capacity overflow");
            }
            if item {
                self.leaves |= 1 << self.len;
            }
            self.len += 1;
        }
    }
}

impl FromIterator<bool> for LeavesBuilder {
    fn from_iter<T: IntoIterator<Item = bool>>(iter: T) -> Self {
        let mut result = Self::default();
        result.extend(iter);
        result
    }
}

/// The cached number of `true` in an [`OctreeNode`].
#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Count(pub u128);

impl<'a> OctreeCache<'a, bool> for Count {
    type Ref = Count;

    fn compute_cache(
        extent: OctreeExtent,
        inputs: impl IntoIterator<Item = CacheInput<'a, bool, Self>>,
    ) -> Self {
        Self(
            inputs
                .into_iter()
                .map(|input| match input {
                    CacheInput::Leaf(true) => extent.volume(),
                    CacheInput::Leaf(false) => 0,
                    CacheInput::Cache(Count(count)) => count,
                })
                .sum(),
        )
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
enum Repr<P> {
    Leaf(OctreeExtent, bool),
    Leaves(OctreeExtent, P, u64),
    Parent1(OctreeExtent, P, Arc<[Node<P>; 2]>),
    Parent2(OctreeExtent, P, Arc<[Node<P>; 4]>),
    Parent3(OctreeExtent, P, Arc<[Node<P>; 8]>),
    Parent4(OctreeExtent, P, Arc<[Node<P>; 16]>),
    Parent5(OctreeExtent, P, Arc<[Node<P>; 32]>),
    Parent6(OctreeExtent, P, Arc<[Node<P>; 64]>),
}

impl<P> Repr<P> {
    fn from_nodes<const N: usize>(
        new: fn(OctreeExtent, P, Arc<[Node<P>; N]>) -> Self,
        extent: OctreeExtent,
        nodes: ArrayVec<Node<P>, { OctreeSplits::MAX_VOLUME_USIZE }>,
        parent: P,
    ) -> Self {
        let children = array_init::from_iter(nodes).expect("leaves should have correct length");
        new(extent, parent, Arc::new(children))
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
enum ReprWithCount<P> {
    Leaf(OctreeExtent, bool),
    Leaves(OctreeExtent, P, u64),
    Parent1(OctreeExtent, P, Arc<ParentNodeWithCount<2, P>>),
    Parent2(OctreeExtent, P, Arc<ParentNodeWithCount<4, P>>),
    Parent3(OctreeExtent, P, Arc<ParentNodeWithCount<8, P>>),
    Parent4(OctreeExtent, P, Arc<ParentNodeWithCount<16, P>>),
    Parent5(OctreeExtent, P, Arc<ParentNodeWithCount<32, P>>),
    Parent6(OctreeExtent, P, Arc<ParentNodeWithCount<64, P>>),
}

impl<P: Clone> ReprWithCount<P> {
    fn from_nodes<const N: usize>(
        new: fn(OctreeExtent, P, Arc<ParentNodeWithCount<N, P>>) -> Self,
        extent: OctreeExtent,
        child_extent: OctreeExtent,
        nodes: ArrayVec<NodeWithCount<P>, { OctreeSplits::MAX_VOLUME_USIZE }>,
        parent: P,
    ) -> Self {
        let children = array_init::from_iter(nodes).expect("leaves should have correct length");
        let count = Count::compute_cache(child_extent, children.iter().map(CacheInput::from_node));
        new(
            extent,
            parent,
            Arc::new(ParentNodeWithCount { count, children }),
        )
    }
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
    count: Count,
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
    Parent1(OctreeExtent, Arc<LargeParentNode<2, P>>),
    Parent2(OctreeExtent, Arc<LargeParentNode<4, P>>),
    Parent3(OctreeExtent, Arc<LargeParentNode<8, P>>),
    Parent4(OctreeExtent, Arc<LargeParentNode<16, P>>),
    Parent5(OctreeExtent, Arc<LargeParentNode<32, P>>),
    Parent6(OctreeExtent, Arc<LargeParentNode<64, P>>),
}

impl<P> ReprWithLargeParent<P> {
    fn from_nodes<const N: usize>(
        new: fn(OctreeExtent, Arc<LargeParentNode<N, P>>) -> Self,
        extent: OctreeExtent,
        nodes: ArrayVec<NodeWithLargeParent<P>, { OctreeSplits::MAX_VOLUME_USIZE }>,
        parent: P,
    ) -> Self {
        let children = array_init::from_iter(nodes).expect("leaves should have correct length");
        new(extent, Arc::new(LargeParentNode { children, parent }))
    }
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
    Parent1(OctreeExtent, Arc<LargeParentNodeWithCount<2, P>>),
    Parent2(OctreeExtent, Arc<LargeParentNodeWithCount<4, P>>),
    Parent3(OctreeExtent, Arc<LargeParentNodeWithCount<8, P>>),
    Parent4(OctreeExtent, Arc<LargeParentNodeWithCount<16, P>>),
    Parent5(OctreeExtent, Arc<LargeParentNodeWithCount<32, P>>),
    Parent6(OctreeExtent, Arc<LargeParentNodeWithCount<64, P>>),
}

impl<P: Clone> ReprWithLargeParentAndCount<P> {
    fn from_nodes<const N: usize>(
        new: fn(OctreeExtent, Arc<LargeParentNodeWithCount<N, P>>) -> Self,
        extent: OctreeExtent,
        child_extent: OctreeExtent,
        nodes: ArrayVec<NodeWithLargeParentAndCount<P>, { OctreeSplits::MAX_VOLUME_USIZE }>,
        parent: P,
    ) -> Self {
        let children = array_init::from_iter(nodes).expect("leaves should have correct length");
        let count = Count::compute_cache(child_extent, children.iter().map(CacheInput::from_node));
        new(
            extent,
            Arc::new(LargeParentNodeWithCount {
                count,
                children,
                parent,
            }),
        )
    }
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
    count: Count,
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

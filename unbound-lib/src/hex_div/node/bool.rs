use std::{
    hash::{Hash, Hasher},
    sync::Arc,
};

use arrayvec::ArrayVec;
use educe::Educe;

use crate::hex_div::{
    bounds::Bounds,
    cache::{Cache, CacheIn},
    extent::{Extent, Splits},
    iter::Iter,
    HexDiv, NodeDataRef, NodeRef,
};

/// A compact representation for a [`HexDiv`] node storing [`bool`]s.
#[derive(Educe)]
#[educe(Clone, Debug, Hash, PartialEq, Eq)]
pub struct BitNode<P = ()>(Repr<P>);

impl<P> HexDiv for BitNode<P> {
    type Leaf = bool;
    type LeafRef<'a> = bool;
    type LeavesBuilder = LeavesBuilder;

    type Parent = P;

    type Cache<'a> = NoCache;

    fn new(extent: Extent, leaf: Self::Leaf) -> Self {
        Self(Repr::Leaf(extent, leaf))
    }

    fn from_leaves_unchecked(
        extent: Extent,
        splits: Splits,
        leaves: Self::LeavesBuilder,
        parent: Self::Parent,
    ) -> Self {
        Self(Repr::Leaves(
            extent,
            splits,
            Arc::new((leaves.leaves, parent)),
        ))
    }

    fn from_nodes_unchecked(
        extent: Extent,
        splits: Splits,
        nodes: ArrayVec<Self, { Splits::MAX_VOLUME_USIZE }>,
        parent: Self::Parent,
    ) -> Self {
        Self(match splits.total() {
            1 => Repr::from_nodes(Repr::Parent1, extent, splits, nodes, parent),
            2 => Repr::from_nodes(Repr::Parent2, extent, splits, nodes, parent),
            3 => Repr::from_nodes(Repr::Parent3, extent, splits, nodes, parent),
            4 => Repr::from_nodes(Repr::Parent4, extent, splits, nodes, parent),
            5 => Repr::from_nodes(Repr::Parent5, extent, splits, nodes, parent),
            6 => Repr::from_nodes(Repr::Parent6, extent, splits, nodes, parent),
            _ => panic!("invalid number of splits"),
        })
    }

    fn extent(&self) -> Extent {
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

    fn splits(&self) -> Splits {
        match self.0 {
            Repr::Leaf(..) => panic!("leaf nodes have no splits"),
            Repr::Leaves(_, splits, _)
            | Repr::Parent1(_, splits, _)
            | Repr::Parent2(_, splits, _)
            | Repr::Parent3(_, splits, _)
            | Repr::Parent4(_, splits, _)
            | Repr::Parent5(_, splits, _)
            | Repr::Parent6(_, splits, _) => splits,
        }
    }

    fn as_data(&self) -> NodeDataRef<Self> {
        match &self.0 {
            Repr::Leaf(_, leaf) => NodeDataRef::Leaf(*leaf),
            Repr::Leaves(_, _, node) => NodeDataRef::Parent(&node.1, ()),
            Repr::Parent1(_, _, node) => NodeDataRef::Parent(&node.parent, ()),
            Repr::Parent2(_, _, node) => NodeDataRef::Parent(&node.parent, ()),
            Repr::Parent3(_, _, node) => NodeDataRef::Parent(&node.parent, ()),
            Repr::Parent4(_, _, node) => NodeDataRef::Parent(&node.parent, ()),
            Repr::Parent5(_, _, node) => NodeDataRef::Parent(&node.parent, ()),
            Repr::Parent6(_, _, node) => NodeDataRef::Parent(&node.parent, ()),
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
            Repr::Leaf(_, _) => panic!("leaf nodes have no children"),
            Repr::Leaves(_, _, leaves) => NodeRef::Leaf(1 << index & leaves.0 != 0),
            Repr::Parent1(_, _, node) => NodeRef::Node(&node.children[index]),
            Repr::Parent2(_, _, node) => NodeRef::Node(&node.children[index]),
            Repr::Parent3(_, _, node) => NodeRef::Node(&node.children[index]),
            Repr::Parent4(_, _, node) => NodeRef::Node(&node.children[index]),
            Repr::Parent5(_, _, node) => NodeRef::Node(&node.children[index]),
            Repr::Parent6(_, _, node) => NodeRef::Node(&node.children[index]),
        }
    }
}

impl<'a, P> IntoIterator for &'a BitNode<P> {
    type Item = (Bounds, NodeRef<'a, BitNode<P>>);
    type IntoIter = Iter<'a, BitNode<P>>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

/// A [`BitNode`] with [cached](`HexDiv::Cache`) total number of `true` leaves.
///
/// Requires an extra [`u128`] per "proper" parent node when compared to [`BitNode`].
#[derive(Educe)]
#[educe(Clone, Debug, Hash, PartialEq, Eq)]
pub struct BitNodeWithCount<P = ()>(ReprWithCount<P>);

impl<P> HexDiv for BitNodeWithCount<P> {
    type Leaf = bool;
    type LeafRef<'a> = bool;
    type LeavesBuilder = LeavesBuilder;

    type Parent = P;

    type Cache<'a> = Count;

    fn new(extent: Extent, leaf: Self::Leaf) -> Self {
        Self(ReprWithCount::Leaf(extent, leaf))
    }

    fn from_leaves_unchecked(
        extent: Extent,
        splits: Splits,
        leaves: Self::LeavesBuilder,
        parent: Self::Parent,
    ) -> Self {
        Self(ReprWithCount::Leaves(
            extent,
            splits,
            Arc::new((leaves.leaves, parent)),
        ))
    }

    fn from_nodes_unchecked(
        extent: Extent,
        splits: Splits,
        nodes: ArrayVec<Self, { Splits::MAX_VOLUME_USIZE }>,
        parent: Self::Parent,
    ) -> Self {
        Self(match splits.total() {
            1 => ReprWithCount::from_nodes(ReprWithCount::Parent1, extent, splits, nodes, parent),
            2 => ReprWithCount::from_nodes(ReprWithCount::Parent2, extent, splits, nodes, parent),
            3 => ReprWithCount::from_nodes(ReprWithCount::Parent3, extent, splits, nodes, parent),
            4 => ReprWithCount::from_nodes(ReprWithCount::Parent4, extent, splits, nodes, parent),
            5 => ReprWithCount::from_nodes(ReprWithCount::Parent5, extent, splits, nodes, parent),
            6 => ReprWithCount::from_nodes(ReprWithCount::Parent6, extent, splits, nodes, parent),
            _ => panic!("invalid number of splits"),
        })
    }

    fn extent(&self) -> Extent {
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

    fn splits(&self) -> Splits {
        match self.0 {
            ReprWithCount::Leaf(..) => panic!("leaf nodes have no splits"),
            ReprWithCount::Leaves(_, splits, _)
            | ReprWithCount::Parent1(_, splits, _)
            | ReprWithCount::Parent2(_, splits, _)
            | ReprWithCount::Parent3(_, splits, _)
            | ReprWithCount::Parent4(_, splits, _)
            | ReprWithCount::Parent5(_, splits, _)
            | ReprWithCount::Parent6(_, splits, _) => splits,
        }
    }

    fn as_data(&self) -> NodeDataRef<Self> {
        match &self.0 {
            ReprWithCount::Leaf(_, leaf) => NodeDataRef::Leaf(*leaf),
            ReprWithCount::Leaves(_, _, leaves) => {
                let (bits, parent) = &**leaves;
                NodeDataRef::Parent(parent, Count(bits.count_ones().into()))
            }
            ReprWithCount::Parent1(_, _, node) => NodeDataRef::Parent(&node.parent, node.count),
            ReprWithCount::Parent2(_, _, node) => NodeDataRef::Parent(&node.parent, node.count),
            ReprWithCount::Parent3(_, _, node) => NodeDataRef::Parent(&node.parent, node.count),
            ReprWithCount::Parent4(_, _, node) => NodeDataRef::Parent(&node.parent, node.count),
            ReprWithCount::Parent5(_, _, node) => NodeDataRef::Parent(&node.parent, node.count),
            ReprWithCount::Parent6(_, _, node) => NodeDataRef::Parent(&node.parent, node.count),
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
            ReprWithCount::Leaves(_, _, leaves) => NodeRef::Leaf(1 << index & leaves.0 != 0),
            ReprWithCount::Parent1(_, _, node) => NodeRef::Node(&node.children[index]),
            ReprWithCount::Parent2(_, _, node) => NodeRef::Node(&node.children[index]),
            ReprWithCount::Parent3(_, _, node) => NodeRef::Node(&node.children[index]),
            ReprWithCount::Parent4(_, _, node) => NodeRef::Node(&node.children[index]),
            ReprWithCount::Parent5(_, _, node) => NodeRef::Node(&node.children[index]),
            ReprWithCount::Parent6(_, _, node) => NodeRef::Node(&node.children[index]),
        }
    }
}

impl<'a, P> IntoIterator for &'a BitNodeWithCount<P> {
    type Item = (Bounds, NodeRef<'a, BitNodeWithCount<P>>);
    type IntoIter = Iter<'a, BitNodeWithCount<P>>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

/// A [`BitNode`] that stores its [`HexDiv::Parent`] inline instead of in the [`Arc`].
///
/// This type should only be used if `P` fits in a single byte. Common use-cases for this are
/// storing some small number, some `enum` variant or a small bitfield.
///
/// However, prefer [`BitNode`] if [`HexDiv::Parent`] is a ZST such as [`BitNode`]'s default of
/// `()`.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct BitNodeWithInlineParent<P>(ReprWithInlineParent<P>);

impl<P> HexDiv for BitNodeWithInlineParent<P> {
    type Leaf = bool;
    type LeafRef<'a> = bool;
    type LeavesBuilder = LeavesBuilder;

    type Parent = P;

    type Cache<'a> = NoCache;

    fn new(extent: Extent, leaf: Self::Leaf) -> Self {
        Self(ReprWithInlineParent::Leaf(extent, leaf))
    }

    fn from_leaves_unchecked(
        extent: Extent,
        splits: Splits,
        leaves: Self::LeavesBuilder,
        parent: Self::Parent,
    ) -> Self {
        Self(ReprWithInlineParent::Leaves(
            extent,
            splits,
            parent,
            leaves.leaves,
        ))
    }

    fn from_nodes_unchecked(
        extent: Extent,
        splits: Splits,
        nodes: ArrayVec<Self, { Splits::MAX_VOLUME_USIZE }>,
        parent: Self::Parent,
    ) -> Self {
        use ReprWithInlineParent as Repr;
        Self(match splits.total() {
            1 => Repr::from_nodes(Repr::Parent1, extent, splits, nodes, parent),
            2 => Repr::from_nodes(Repr::Parent2, extent, splits, nodes, parent),
            3 => Repr::from_nodes(Repr::Parent3, extent, splits, nodes, parent),
            4 => Repr::from_nodes(Repr::Parent4, extent, splits, nodes, parent),
            5 => Repr::from_nodes(Repr::Parent5, extent, splits, nodes, parent),
            6 => Repr::from_nodes(Repr::Parent6, extent, splits, nodes, parent),
            _ => panic!("invalid number of splits"),
        })
    }

    fn extent(&self) -> Extent {
        match self.0 {
            ReprWithInlineParent::Leaf(extent, _)
            | ReprWithInlineParent::Leaves(extent, _, _, _)
            | ReprWithInlineParent::Parent1(extent, _, _, _)
            | ReprWithInlineParent::Parent2(extent, _, _, _)
            | ReprWithInlineParent::Parent3(extent, _, _, _)
            | ReprWithInlineParent::Parent4(extent, _, _, _)
            | ReprWithInlineParent::Parent5(extent, _, _, _)
            | ReprWithInlineParent::Parent6(extent, _, _, _) => extent,
        }
    }

    fn splits(&self) -> Splits {
        match self.0 {
            ReprWithInlineParent::Leaf(..) => panic!("leaf nodes have no splits"),
            ReprWithInlineParent::Leaves(_, splits, _, _)
            | ReprWithInlineParent::Parent1(_, splits, _, _)
            | ReprWithInlineParent::Parent2(_, splits, _, _)
            | ReprWithInlineParent::Parent3(_, splits, _, _)
            | ReprWithInlineParent::Parent4(_, splits, _, _)
            | ReprWithInlineParent::Parent5(_, splits, _, _)
            | ReprWithInlineParent::Parent6(_, splits, _, _) => splits,
        }
    }

    fn as_data(&self) -> NodeDataRef<Self> {
        match &self.0 {
            ReprWithInlineParent::Leaf(_, leaf) => NodeDataRef::Leaf(*leaf),
            ReprWithInlineParent::Leaves(_, _, parent, _)
            | ReprWithInlineParent::Parent1(_, _, parent, _)
            | ReprWithInlineParent::Parent2(_, _, parent, _)
            | ReprWithInlineParent::Parent3(_, _, parent, _)
            | ReprWithInlineParent::Parent4(_, _, parent, _)
            | ReprWithInlineParent::Parent5(_, _, parent, _)
            | ReprWithInlineParent::Parent6(_, _, parent, _) => NodeDataRef::Parent(parent, ()),
        }
    }

    fn into_leaf(self) -> Result<Self::Leaf, Self> {
        if let ReprWithInlineParent::Leaf(_, leaf) = self.0 {
            Ok(leaf)
        } else {
            Err(self)
        }
    }

    fn get_child(&self, index: u8) -> NodeRef<Self> {
        let index = usize::from(index);
        match &self.0 {
            ReprWithInlineParent::Leaf(..) => panic!("leaf nodes have no children"),
            ReprWithInlineParent::Leaves(_, _, _, leaves) => {
                NodeRef::Leaf(1 << index & *leaves != 0)
            }
            ReprWithInlineParent::Parent1(_, _, _, children) => NodeRef::Node(&children[index]),
            ReprWithInlineParent::Parent2(_, _, _, children) => NodeRef::Node(&children[index]),
            ReprWithInlineParent::Parent3(_, _, _, children) => NodeRef::Node(&children[index]),
            ReprWithInlineParent::Parent4(_, _, _, children) => NodeRef::Node(&children[index]),
            ReprWithInlineParent::Parent5(_, _, _, children) => NodeRef::Node(&children[index]),
            ReprWithInlineParent::Parent6(_, _, _, children) => NodeRef::Node(&children[index]),
        }
    }
}

impl<'a, P> IntoIterator for &'a BitNodeWithInlineParent<P> {
    type Item = (Bounds, NodeRef<'a, BitNodeWithInlineParent<P>>);
    type IntoIter = Iter<'a, BitNodeWithInlineParent<P>>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

/// A combination of [`BitNodeWithCount`] and [`BitNodeWithInlineParent`].
///
/// See both types for their respective caveats.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct BitNodeWithInlineParentAndCount<P>(ReprWithBoth<P>);

impl<P> HexDiv for BitNodeWithInlineParentAndCount<P> {
    type Leaf = bool;
    type LeafRef<'a> = bool;
    type LeavesBuilder = LeavesBuilder;

    type Parent = P;

    type Cache<'a> = Count;

    fn new(extent: Extent, leaf: Self::Leaf) -> Self {
        Self(ReprWithBoth::Leaf(extent, leaf))
    }

    fn from_leaves_unchecked(
        extent: Extent,
        splits: Splits,
        leaves: Self::LeavesBuilder,
        parent: Self::Parent,
    ) -> Self {
        Self(ReprWithBoth::Leaves(extent, splits, parent, leaves.leaves))
    }

    fn from_nodes_unchecked(
        extent: Extent,
        splits: Splits,
        nodes: ArrayVec<Self, { Splits::MAX_VOLUME_USIZE }>,
        parent: Self::Parent,
    ) -> Self {
        Self(match splits.total() {
            1 => ReprWithBoth::from_nodes(ReprWithBoth::Parent1, extent, splits, nodes, parent),
            2 => ReprWithBoth::from_nodes(ReprWithBoth::Parent2, extent, splits, nodes, parent),
            3 => ReprWithBoth::from_nodes(ReprWithBoth::Parent3, extent, splits, nodes, parent),
            4 => ReprWithBoth::from_nodes(ReprWithBoth::Parent4, extent, splits, nodes, parent),
            5 => ReprWithBoth::from_nodes(ReprWithBoth::Parent5, extent, splits, nodes, parent),
            6 => ReprWithBoth::from_nodes(ReprWithBoth::Parent6, extent, splits, nodes, parent),
            _ => panic!("invalid number of splits"),
        })
    }

    fn extent(&self) -> Extent {
        match self.0 {
            ReprWithBoth::Leaf(extent, _)
            | ReprWithBoth::Leaves(extent, _, _, _)
            | ReprWithBoth::Parent1(extent, _, _, _)
            | ReprWithBoth::Parent2(extent, _, _, _)
            | ReprWithBoth::Parent3(extent, _, _, _)
            | ReprWithBoth::Parent4(extent, _, _, _)
            | ReprWithBoth::Parent5(extent, _, _, _)
            | ReprWithBoth::Parent6(extent, _, _, _) => extent,
        }
    }

    fn splits(&self) -> Splits {
        match self.0 {
            ReprWithBoth::Leaf(..) => panic!("leaf nodes have no splits"),
            ReprWithBoth::Leaves(_, splits, _, _)
            | ReprWithBoth::Parent1(_, splits, _, _)
            | ReprWithBoth::Parent2(_, splits, _, _)
            | ReprWithBoth::Parent3(_, splits, _, _)
            | ReprWithBoth::Parent4(_, splits, _, _)
            | ReprWithBoth::Parent5(_, splits, _, _)
            | ReprWithBoth::Parent6(_, splits, _, _) => splits,
        }
    }

    fn as_data(&self) -> NodeDataRef<Self> {
        match &self.0 {
            ReprWithBoth::Leaf(_, leaf) => return NodeDataRef::Leaf(*leaf),
            ReprWithBoth::Leaves(_, _, parent, leaves) => {
                NodeDataRef::Parent(parent, Count(leaves.count_ones().into()))
            }
            ReprWithBoth::Parent1(_, _, parent, node) => NodeDataRef::Parent(parent, node.count),
            ReprWithBoth::Parent2(_, _, parent, node) => NodeDataRef::Parent(parent, node.count),
            ReprWithBoth::Parent3(_, _, parent, node) => NodeDataRef::Parent(parent, node.count),
            ReprWithBoth::Parent4(_, _, parent, node) => NodeDataRef::Parent(parent, node.count),
            ReprWithBoth::Parent5(_, _, parent, node) => NodeDataRef::Parent(parent, node.count),
            ReprWithBoth::Parent6(_, _, parent, node) => NodeDataRef::Parent(parent, node.count),
        }
    }

    fn into_leaf(self) -> Result<Self::Leaf, Self> {
        if let ReprWithBoth::Leaf(_, leaf) = self.0 {
            Ok(leaf)
        } else {
            Err(self)
        }
    }

    fn get_child(&self, index: u8) -> NodeRef<Self> {
        let index = usize::from(index);
        match &self.0 {
            ReprWithBoth::Leaf(_, _) => panic!("leaf nodes have no children"),
            ReprWithBoth::Leaves(_, _, _, leaves) => NodeRef::Leaf(1 << index & *leaves != 0),
            ReprWithBoth::Parent1(_, _, _, node) => NodeRef::Node(&node.children[index]),
            ReprWithBoth::Parent2(_, _, _, node) => NodeRef::Node(&node.children[index]),
            ReprWithBoth::Parent3(_, _, _, node) => NodeRef::Node(&node.children[index]),
            ReprWithBoth::Parent4(_, _, _, node) => NodeRef::Node(&node.children[index]),
            ReprWithBoth::Parent5(_, _, _, node) => NodeRef::Node(&node.children[index]),
            ReprWithBoth::Parent6(_, _, _, node) => NodeRef::Node(&node.children[index]),
        }
    }
}

#[derive(Educe)]
#[educe(Clone, Debug, Hash, PartialEq, Eq)]
enum Repr<P> {
    Leaf(Extent, bool),
    Leaves(Extent, Splits, Arc<(u64, P)>),
    Parent1(Extent, Splits, Arc<ParentNode<2, P>>),
    Parent2(Extent, Splits, Arc<ParentNode<4, P>>),
    Parent3(Extent, Splits, Arc<ParentNode<8, P>>),
    Parent4(Extent, Splits, Arc<ParentNode<16, P>>),
    Parent5(Extent, Splits, Arc<ParentNode<32, P>>),
    Parent6(Extent, Splits, Arc<ParentNode<64, P>>),
}

type NewReprParent<P, const N: usize> = fn(Extent, Splits, Arc<ParentNode<N, P>>) -> Repr<P>;

impl<P> Repr<P> {
    fn from_nodes<const N: usize>(
        new: NewReprParent<P, N>,
        extent: Extent,
        splits: Splits,
        nodes: ArrayVec<BitNode<P>, { Splits::MAX_VOLUME_USIZE }>,
        parent: P,
    ) -> Self {
        let children = array_init::from_iter(nodes).expect("leaves should have correct length");
        new(extent, splits, Arc::new(ParentNode { children, parent }))
    }
}

#[derive(Educe)]
#[educe(Clone, Debug, Hash, PartialEq, Eq)]
enum ReprWithCount<P> {
    Leaf(Extent, bool),
    Leaves(Extent, Splits, Arc<(u64, P)>),
    Parent1(Extent, Splits, Arc<ParentNodeWithCount<2, P>>),
    Parent2(Extent, Splits, Arc<ParentNodeWithCount<4, P>>),
    Parent3(Extent, Splits, Arc<ParentNodeWithCount<8, P>>),
    Parent4(Extent, Splits, Arc<ParentNodeWithCount<16, P>>),
    Parent5(Extent, Splits, Arc<ParentNodeWithCount<32, P>>),
    Parent6(Extent, Splits, Arc<ParentNodeWithCount<64, P>>),
}

type NewReprWithCountParent<P, const N: usize> =
    fn(Extent, Splits, Arc<ParentNodeWithCount<N, P>>) -> ReprWithCount<P>;

impl<P> ReprWithCount<P> {
    fn from_nodes<const N: usize>(
        new: NewReprWithCountParent<P, N>,
        extent: Extent,
        splits: Splits,
        nodes: ArrayVec<BitNodeWithCount<P>, { Splits::MAX_VOLUME_USIZE }>,
        parent: P,
    ) -> Self {
        let children = array_init::from_iter(nodes).expect("leaves should have correct length");
        new(
            extent,
            splits,
            Arc::new(ParentNodeWithCount {
                count: Count::compute_cache(
                    extent.split(splits),
                    children.iter().map(CacheIn::from_node),
                ),
                children,
                parent,
            }),
        )
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
enum ReprWithInlineParent<P> {
    Leaf(Extent, bool),
    Leaves(Extent, Splits, P, u64),
    Parent1(Extent, Splits, P, Arc<[BitNodeWithInlineParent<P>; 2]>),
    Parent2(Extent, Splits, P, Arc<[BitNodeWithInlineParent<P>; 4]>),
    Parent3(Extent, Splits, P, Arc<[BitNodeWithInlineParent<P>; 8]>),
    Parent4(Extent, Splits, P, Arc<[BitNodeWithInlineParent<P>; 16]>),
    Parent5(Extent, Splits, P, Arc<[BitNodeWithInlineParent<P>; 32]>),
    Parent6(Extent, Splits, P, Arc<[BitNodeWithInlineParent<P>; 64]>),
}

type NewReprWithInlineParentParent<P, const N: usize> =
    fn(Extent, Splits, P, Arc<[BitNodeWithInlineParent<P>; N]>) -> ReprWithInlineParent<P>;

impl<P> ReprWithInlineParent<P> {
    fn from_nodes<const N: usize>(
        new: NewReprWithInlineParentParent<P, N>,
        extent: Extent,
        splits: Splits,
        nodes: ArrayVec<BitNodeWithInlineParent<P>, { Splits::MAX_VOLUME_USIZE }>,
        parent: P,
    ) -> Self {
        let children = array_init::from_iter(nodes).expect("leaves should have correct length");
        new(extent, splits, parent, Arc::new(children))
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
enum ReprWithBoth<P> {
    Leaf(Extent, bool),
    Leaves(Extent, Splits, P, u64),
    Parent1(Extent, Splits, P, Arc<ParentNodeWithBoth<2, P>>),
    Parent2(Extent, Splits, P, Arc<ParentNodeWithBoth<4, P>>),
    Parent3(Extent, Splits, P, Arc<ParentNodeWithBoth<8, P>>),
    Parent4(Extent, Splits, P, Arc<ParentNodeWithBoth<16, P>>),
    Parent5(Extent, Splits, P, Arc<ParentNodeWithBoth<32, P>>),
    Parent6(Extent, Splits, P, Arc<ParentNodeWithBoth<64, P>>),
}

type NewReprWithBothParent<P, const N: usize> =
    fn(Extent, Splits, P, Arc<ParentNodeWithBoth<N, P>>) -> ReprWithBoth<P>;

impl<P> ReprWithBoth<P> {
    fn from_nodes<const N: usize>(
        new: NewReprWithBothParent<P, N>,
        extent: Extent,
        splits: Splits,
        nodes: ArrayVec<BitNodeWithInlineParentAndCount<P>, { Splits::MAX_VOLUME_USIZE }>,
        parent: P,
    ) -> Self {
        let children = array_init::from_iter(nodes).expect("leaves should have correct length");
        new(
            extent,
            splits,
            parent,
            Arc::new(ParentNodeWithBoth {
                count: Count::compute_cache(
                    extent.split(splits),
                    children.iter().map(CacheIn::from_node),
                ),
                children,
            }),
        )
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
struct ParentNode<const N: usize, P> {
    children: [BitNode<P>; N],
    parent: P,
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
    children: [BitNodeWithCount<P>; N],
    parent: P,
}

impl<const N: usize, P: Hash> Hash for ParentNodeWithCount<N, P> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // count intentionally skipped
        self.children.hash(state);
        self.parent.hash(state);
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct ParentNodeWithBoth<const N: usize, P> {
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
    children: [BitNodeWithInlineParentAndCount<P>; N],
}

impl<const N: usize, P: Hash> Hash for ParentNodeWithBoth<N, P> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // count intentionally skipped
        self.children.hash(state);
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
            if self.len == Splits::MAX_VOLUME {
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

#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct NoCache;

impl<'a, T> Cache<'a, T> for NoCache {
    type Ref = ();

    fn compute_cache(_: Extent, _: impl IntoIterator<Item = CacheIn<'a, T, Self>>) -> Self {
        Self
    }
}

/// The cached number of `true` in an [`OctreeNode`].
#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Count(pub u128);

impl<'a> Cache<'a, bool> for Count {
    type Ref = Count;

    fn compute_cache(
        extent: Extent,
        inputs: impl IntoIterator<Item = CacheIn<'a, bool, Self>>,
    ) -> Self {
        Self(
            inputs
                .into_iter()
                .map(|input| match input {
                    CacheIn::Leaf(true) => extent.volume(),
                    CacheIn::Leaf(false) => 0,
                    CacheIn::Cache(Count(count)) => count,
                })
                .sum(),
        )
    }
}

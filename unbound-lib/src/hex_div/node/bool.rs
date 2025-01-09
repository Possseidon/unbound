use std::{
    hash::{Hash, Hasher},
    sync::Arc,
};

use arrayvec::ArrayVec;
use educe::Educe;

use super::{iter::Iter, HexDivNode, NodeDataRef, NodeRef};
use crate::hex_div::{
    bounds::Bounds,
    cache::{Cache, CacheIn},
    extent::Extent,
    splits::Splits,
};

/// A compact representation of a [`HexDivNode`] storing [`bool`]s.
///
/// The final leaves nodes are stored as `u64` rather than `[bool; 64]`.
#[derive(Educe)]
#[educe(Clone, Debug, Hash, PartialEq, Eq)]
pub struct BitNode<P = (), C = NoBitCache>(Repr<P, C>);

impl<P, C: BitCache> BitNode<P, C> {
    /// Whether none of the bits are set.
    pub fn none(&self) -> bool {
        matches!(self.as_data(), NodeDataRef::Leaf(false))
    }

    /// Whether all bits are set.
    pub fn all(&self) -> bool {
        matches!(self.as_data(), NodeDataRef::Leaf(true))
    }

    /// Whether at least one bit is set.
    pub fn any(&self) -> bool {
        !self.none()
    }
}

impl<P, C: BitCache> HexDivNode for BitNode<P, C> {
    type Leaf = bool;
    type LeafRef<'a> = bool;
    type LeavesBuilder = LeavesBuilder;

    type Parent = P;

    type Cache<'a> = C;

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
            Repr::Leaves(_, _, leaves) => {
                let (bits, parent) = &**leaves;
                NodeDataRef::Parent(parent, C::compute_cache_from_bits(*bits))
            }
            Repr::Parent1(_, _, node) => NodeDataRef::Parent(&node.parent, node.cache),
            Repr::Parent2(_, _, node) => NodeDataRef::Parent(&node.parent, node.cache),
            Repr::Parent3(_, _, node) => NodeDataRef::Parent(&node.parent, node.cache),
            Repr::Parent4(_, _, node) => NodeDataRef::Parent(&node.parent, node.cache),
            Repr::Parent5(_, _, node) => NodeDataRef::Parent(&node.parent, node.cache),
            Repr::Parent6(_, _, node) => NodeDataRef::Parent(&node.parent, node.cache),
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

#[derive(Educe)]
#[educe(Clone, Debug, Hash, PartialEq, Eq)]
enum Repr<P, C> {
    Leaf(Extent, bool),
    Leaves(Extent, Splits, Arc<(u64, P)>),
    Parent1(Extent, Splits, Arc<ParentNode<2, P, C>>),
    Parent2(Extent, Splits, Arc<ParentNode<4, P, C>>),
    Parent3(Extent, Splits, Arc<ParentNode<8, P, C>>),
    Parent4(Extent, Splits, Arc<ParentNode<16, P, C>>),
    Parent5(Extent, Splits, Arc<ParentNode<32, P, C>>),
    Parent6(Extent, Splits, Arc<ParentNode<64, P, C>>),
}

type NewRepr<const N: usize, P, C> = fn(Extent, Splits, Arc<ParentNode<N, P, C>>) -> Repr<P, C>;

impl<P, C: BitCache> Repr<P, C> {
    fn from_nodes<const N: usize>(
        new: NewRepr<N, P, C>,
        extent: Extent,
        splits: Splits,
        nodes: ArrayVec<BitNode<P, C>, { Splits::MAX_VOLUME_USIZE }>,
        parent: P,
    ) -> Self {
        let children = array_init::from_iter(nodes).expect("leaves should have correct length");
        new(
            extent,
            splits,
            Arc::new(ParentNode {
                cache: C::compute_cache(
                    extent.split(splits),
                    children.iter().map(CacheIn::from_node),
                ),
                children,
                parent,
            }),
        )
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct ParentNode<const N: usize, P, C> {
    /// The cached value for this node.
    ///
    /// Skipped by [`Hash`], since the same `children` always lead to the same cached value.
    ///
    /// On the other hand, it makes a lot of sense for [`Eq`] to make use of it, since it has the
    /// potential to very quickly short-circuit the comparison of large [`BitNode`]s.
    cache: C,
    children: [BitNode<P, C>; N],
    parent: P,
}

impl<const N: usize, P: Hash, C> Hash for ParentNode<N, P, C> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // count intentionally skipped
        self.children.hash(state);
        self.parent.hash(state);
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

/// An extension on top of [`Cache`] for [`BitNode`].
pub trait BitCache: for<'a> Cache<bool, Ref<'a> = Self> + Copy {
    /// Computes the cache from the internal representation.
    ///
    /// This way it can take advantage of things like [`u64::count_ones`].
    ///
    /// For nodes with less than `64` children, MSBs are padded with `0`.
    fn compute_cache_from_bits(bits: u64) -> Self;
}

/// The default [`BitCache`] for [`BitNode`] that doesn't cache anything.
#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct NoBitCache;

impl<T> Cache<T> for NoBitCache {
    type Ref<'a> = NoBitCache;

    fn compute_cache<'a>(_: Extent, _: impl IntoIterator<Item = CacheIn<'a, T, Self>>) -> Self {
        Self
    }
}

impl BitCache for NoBitCache {
    fn compute_cache_from_bits(_: u64) -> Self {
        Self
    }
}

/// A [`BitCache`] for [`BitNode`] that caches the total number of `true`.
#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Count(pub u128);

impl Cache<bool> for Count {
    type Ref<'a> = Count;

    fn compute_cache<'a>(
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

impl BitCache for Count {
    fn compute_cache_from_bits(value: u64) -> Self {
        Self(value.count_ones().into())
    }
}

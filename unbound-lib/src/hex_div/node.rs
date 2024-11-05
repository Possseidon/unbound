pub mod bool;

use std::sync::Arc;

use arrayvec::ArrayVec;
use educe::Educe;

use super::{
    cache::{Cache, CacheIn},
    extent::{Extent, Splits},
    HexDiv, NodeDataRef, NodeRef,
};

/// A node within an octree, either holding a leaf with a value of type `T` or more [`Node`]s.
///
/// Unlike with regular octrees, nodes don't always split into a `2x2x2` of other nodes. Instead,
/// nodes can have any cuboid shape as long as it can be split up to
/// [`OctreeSplits::MAX_TOTAL`](super::extent::OctreeSplits::MAX_TOTAL) times.
///
/// # Variants
///
/// - `Leaf` is a single leaf holding a value of type `T`
/// - `Parent<N>` holds `N²` child nodes across `N` splits
/// - `Leaves<N>` is an optimization for `Parent<N>` nodes that only hold leaves
///
/// Arrays of nodes are stored in separate variants as opposed to something like a [`Vec<T>`] or
/// `Box<[T]>`. While this is a bit inconvenient to work with, it saves on memory (which quickly
/// adds up) and also improves type-safety.
///
/// Additionally, parent nodes are stored in [`Arc`]s, making clones very cheap.
#[derive(Educe)]
#[educe(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Node<T, P = (), C = NoCache>(Repr<T, P, C>);

impl<T, P, C> HexDiv for Node<T, P, C>
where
    T: Clone + Eq,
    P: Clone,
    C: Clone + for<'a> Cache<'a, &'a T, Ref = &'a C>,
{
    type Leaf = T;

    type LeafRef<'a>
        = &'a T
    where
        T: 'a;

    type LeavesBuilder = ArrayVec<T, { Splits::MAX_VOLUME_USIZE }>;

    type Parent = P;

    type Cache<'a>
        = C
    where
        T: 'a;

    fn new(extent: Extent, leaf: Self::Leaf) -> Self {
        Self(Repr::Leaf(extent, leaf))
    }

    fn from_leaves_unchecked(
        total_splits: u8,
        extent: Extent,
        child_extent: Extent,
        leaves: Self::LeavesBuilder,
        parent: Self::Parent,
    ) -> Self {
        Self(match total_splits {
            1 => Repr::from_leaves(Repr::Leaves1, extent, child_extent, leaves, parent),
            2 => Repr::from_leaves(Repr::Leaves2, extent, child_extent, leaves, parent),
            3 => Repr::from_leaves(Repr::Leaves3, extent, child_extent, leaves, parent),
            4 => Repr::from_leaves(Repr::Leaves4, extent, child_extent, leaves, parent),
            5 => Repr::from_leaves(Repr::Leaves5, extent, child_extent, leaves, parent),
            6 => Repr::from_leaves(Repr::Leaves6, extent, child_extent, leaves, parent),
            _ => panic!("invalid number of splits"),
        })
    }

    fn from_nodes_unchecked(
        total_splits: u8,
        extent: Extent,
        child_extent: Extent,
        nodes: ArrayVec<Self, { Splits::MAX_VOLUME_USIZE }>,
        parent: Self::Parent,
    ) -> Self {
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

    fn extent(&self) -> Extent {
        match self.0 {
            Repr::Leaf(extent, _)
            | Repr::Leaves1(extent, _)
            | Repr::Leaves2(extent, _)
            | Repr::Leaves3(extent, _)
            | Repr::Leaves4(extent, _)
            | Repr::Leaves5(extent, _)
            | Repr::Leaves6(extent, _)
            | Repr::Parent1(extent, _)
            | Repr::Parent2(extent, _)
            | Repr::Parent3(extent, _)
            | Repr::Parent4(extent, _)
            | Repr::Parent5(extent, _)
            | Repr::Parent6(extent, _) => extent,
        }
    }

    fn as_data(&self) -> NodeDataRef<Self> {
        match &self.0 {
            Repr::Leaf(_, leaf) => return NodeDataRef::Leaf(leaf),
            Repr::Leaves1(_, node) => NodeDataRef::Parent(&node.parent, &node.cache),
            Repr::Leaves2(_, node) => NodeDataRef::Parent(&node.parent, &node.cache),
            Repr::Leaves3(_, node) => NodeDataRef::Parent(&node.parent, &node.cache),
            Repr::Leaves4(_, node) => NodeDataRef::Parent(&node.parent, &node.cache),
            Repr::Leaves5(_, node) => NodeDataRef::Parent(&node.parent, &node.cache),
            Repr::Leaves6(_, node) => NodeDataRef::Parent(&node.parent, &node.cache),
            Repr::Parent1(_, node) => NodeDataRef::Parent(&node.parent, &node.cache),
            Repr::Parent2(_, node) => NodeDataRef::Parent(&node.parent, &node.cache),
            Repr::Parent3(_, node) => NodeDataRef::Parent(&node.parent, &node.cache),
            Repr::Parent4(_, node) => NodeDataRef::Parent(&node.parent, &node.cache),
            Repr::Parent5(_, node) => NodeDataRef::Parent(&node.parent, &node.cache),
            Repr::Parent6(_, node) => NodeDataRef::Parent(&node.parent, &node.cache),
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
            Repr::Leaves1(_, node) => NodeRef::Leaf(&node.leaves[index]),
            Repr::Leaves2(_, node) => NodeRef::Leaf(&node.leaves[index]),
            Repr::Leaves3(_, node) => NodeRef::Leaf(&node.leaves[index]),
            Repr::Leaves4(_, node) => NodeRef::Leaf(&node.leaves[index]),
            Repr::Leaves5(_, node) => NodeRef::Leaf(&node.leaves[index]),
            Repr::Leaves6(_, node) => NodeRef::Leaf(&node.leaves[index]),
            Repr::Parent1(_, node) => NodeRef::Node(&node.children[index]),
            Repr::Parent2(_, node) => NodeRef::Node(&node.children[index]),
            Repr::Parent3(_, node) => NodeRef::Node(&node.children[index]),
            Repr::Parent4(_, node) => NodeRef::Node(&node.children[index]),
            Repr::Parent5(_, node) => NodeRef::Node(&node.children[index]),
            Repr::Parent6(_, node) => NodeRef::Node(&node.children[index]),
        }
    }
}

#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct NoCache;

impl<'a, T> Cache<'a, T> for NoCache {
    type Ref = &'a NoCache;

    fn compute_cache(_: Extent, _: impl IntoIterator<Item = CacheIn<'a, T, Self>>) -> Self {
        Self
    }
}

#[derive(Educe)]
#[educe(Clone, Debug, Hash, PartialEq, Eq)]
enum Repr<T, P, C> {
    Leaf(Extent, T),
    Leaves1(Extent, Arc<LeavesNode<T, 2, P, C>>),
    Leaves2(Extent, Arc<LeavesNode<T, 4, P, C>>),
    Leaves3(Extent, Arc<LeavesNode<T, 8, P, C>>),
    Leaves4(Extent, Arc<LeavesNode<T, 16, P, C>>),
    Leaves5(Extent, Arc<LeavesNode<T, 32, P, C>>),
    Leaves6(Extent, Arc<LeavesNode<T, 64, P, C>>),
    Parent1(Extent, Arc<ParentNode<T, 2, P, C>>),
    Parent2(Extent, Arc<ParentNode<T, 4, P, C>>),
    Parent3(Extent, Arc<ParentNode<T, 8, P, C>>),
    Parent4(Extent, Arc<ParentNode<T, 16, P, C>>),
    Parent5(Extent, Arc<ParentNode<T, 32, P, C>>),
    Parent6(Extent, Arc<ParentNode<T, 64, P, C>>),
}

type NewLeaves<T, const N: usize, P, C> = fn(Extent, Arc<LeavesNode<T, N, P, C>>) -> Repr<T, P, C>;

type NewParent<T, const N: usize, P, C> = fn(Extent, Arc<ParentNode<T, N, P, C>>) -> Repr<T, P, C>;

impl<T, P, C> Repr<T, P, C>
where
    T: Clone + Eq,
    P: Clone,
    C: Clone + for<'a> Cache<'a, &'a T, Ref = &'a C>,
{
    fn from_leaves<const N: usize>(
        new: NewLeaves<T, N, P, C>,
        extent: Extent,
        child_extent: Extent,
        leaves: ArrayVec<T, { Splits::MAX_VOLUME_USIZE }>,
        parent: P,
    ) -> Self {
        let leaves = array_init::from_iter(leaves).expect("leaves should have correct length");
        let cache = C::compute_cache(child_extent, leaves.iter().map(CacheIn::Leaf));
        new(
            extent,
            Arc::new(LeavesNode {
                leaves,
                parent,
                cache,
            }),
        )
    }

    fn from_nodes<const N: usize>(
        new: NewParent<T, N, P, C>,
        extent: Extent,
        child_extent: Extent,
        nodes: ArrayVec<Node<T, P, C>, { Splits::MAX_VOLUME_USIZE }>,
        parent: P,
    ) -> Self {
        let children = array_init::from_iter(nodes).expect("leaves should have correct length");
        let cache = C::compute_cache(child_extent, children.iter().map(CacheIn::from_node));
        new(
            extent,
            Arc::new(ParentNode {
                children,
                parent,
                cache,
            }),
        )
    }
}

/// Holds storage for `N` leaf nodes of type `T`, parent data `P` and a cached value `C`.
///
/// Cached data is intentionally ignored by [`Eq`] and friends, since it does not carry any extra
/// information that is not already present in `leaves`.
#[derive(Educe)]
#[educe(Clone, Copy, Debug, Hash, PartialEq, Eq)]
struct LeavesNode<T, const N: usize, P, C> {
    leaves: [T; N],
    parent: P,
    #[educe(Hash(ignore), Eq(ignore))]
    cache: C,
}

/// Holds storage for `N` child [`Node`]s, parent data `P` and a cached value `C`.
///
/// Cached data is intentionally ignored by [`Eq`] and friends, since it does not carry any extra
/// information that is not already present in `leaves`.
#[derive(Educe)]
#[educe(Clone, Debug, Hash, PartialEq, Eq)]
struct ParentNode<T, const N: usize, P, C> {
    children: [Node<T, P, C>; N],
    parent: P,
    #[educe(Hash(ignore), Eq(ignore))]
    cache: C,
}

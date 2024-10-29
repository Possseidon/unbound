pub mod bounds;
pub mod cache;
pub mod extent;
pub mod iter;
pub mod iter_mut;
pub mod iter_mut_parent;
pub mod node;
mod stack;
pub mod visit;

use cache::OctreeCache;
use derive_where::derive_where;
use extent::OctreeExtent;
use iter::Iter;
use iter_mut::IterMut;
use visit::Split;

// TODO: Consider a custom Debug impl for octree nodes that forward to some generic implementation
// on OctreeNode

/// An owned octree node holding leaves of type [`OctreeNode::Leaf`].
///
/// Unlike with normal octrees, the side lengths do not have to be equal. In fact, this type can be
/// used perfectly fine as a quadtree as well.
///
/// Octree subdivisions are not limited to `2x2x2` and instead store up to 64 child nodes. This will
/// usually result in subdivisions of size `4x4x4`, but could also be e.g. `8x8x1`, `2x4x8` or
/// `2x1x1`.
///
/// Parent nodes can hold arbitrary additional data of type [`OctreeNode::Parent`] as well as an
/// immutable cached value of type [`OctreeNode::Cache`] which is calculated based on leaf nodes and
/// already calculated cached values.
///
/// Since an octree is usually made up of a lot of nodes, types implementing this trait should
/// generally strive for a size of two pointers.
pub trait OctreeNode: Sized {
    /// The type of leaf values that this node can hold.
    type Leaf: Clone;

    /// A reference to a leaf, usually `&Self::Leaf`.
    ///
    /// Allows e.g. storing [`bool`]s as single bits by using [`bool`] as `LeafRef`.
    type LeafRef<'a>: Copy
    where
        Self::Leaf: 'a;

    /// A mutable reference to a leaf, usually `&mut Self::Leaf`.
    ///
    /// Allows e.g. storing [`bool`]s as single bits by using some wrapper type to mutate.
    type LeafMut<'a>
    where
        Self::Leaf: 'a;

    /// Strips the mutability off of the given leaf reference.
    fn freeze_leaf(leaf: Self::LeafMut<'_>) -> Self::LeafRef<'_>;

    /// Clones a [`OctreeNode::LeafRef`] into an owned [`OctreeNode::Leaf`].
    fn clone_leaf(leaf: Self::LeafRef<'_>) -> Self::Leaf;

    /// Checks for equality between the values of two [`OctreeNode::LeafRef`]s.
    fn leaf_eq(lhs: Self::LeafRef<'_>, rhs: Self::LeafRef<'_>) -> bool;

    fn leaf_ref(leaf: &Self::Leaf) -> Self::LeafRef<'_>;

    /// The type of extra data that parent nodes can hold.
    type Parent: Clone;

    /// The type of cached leaf value.
    ///
    /// Calculated from [`OctreeNode::Leaf`] and already calculated [`OctreeNode::Cache`].
    type Cache<'a>: OctreeCache<'a, Self::LeafRef<'a>>
    where
        Self::Leaf: 'a;

    /// Constructs a new [`OctreeNode`] of the specified `extent` holding just the given `leaf`.
    fn new(extent: OctreeExtent, leaf: Self::Leaf) -> Self;

    /// Construtcs a new [`OctreeNode`] of the specified `extent` with the [`Default`] leaf value.
    fn with_default(extent: OctreeExtent) -> Self
    where
        Self::Leaf: Default,
    {
        Self::new(extent, Self::Leaf::default())
    }

    /// Returns the extent of this [`OctreeNode`].
    fn extent(&self) -> OctreeExtent;

    /// Returns a reference to the underlying data of this [`OctreeNode`].
    ///
    /// - For leaf nodes: [`OctreeNode::LeafRef`]
    /// - For parent nodes: [`&OctreeNode::Parent`](OctreeNode::Parent), [`OctreeNode::CacheRef`]
    fn as_data(&self) -> NodeDataRef<Self>;

    /// Returns a reference to the underlying data of this [`OctreeNode`] with the parent mutable.
    ///
    /// - For leaf nodes: [`OctreeNode::LeafRef`]
    /// - For parent nodes: [`&mut OctreeNode::Parent`](OctreeNode::Parent),
    ///   [`OctreeNode::CacheRef`]
    fn as_data_mut_parent(&mut self) -> NodeDataMutParent<Self>;

    /// Returns a mutable reference to the underlying data of this [`OctreeNode`].
    ///
    /// - For leaf nodes: [`OctreeNode::LeafMut`]
    /// - For parent nodes: [`&mut OctreeNode::Parent`](OctreeNode::Parent),
    ///   [`OctreeNode::CacheRef`]
    fn as_data_mut(&mut self) -> NodeDataMut<Self>;

    /// Fills the entire node with the given leaf while keeping the original extent.
    fn fill(&mut self, leaf: Self::Leaf) {
        *self = Self::new(self.extent(), leaf);
    }

    /// Returns an iterator that traverses the nodes in the octree depth-first.
    fn iter(&self) -> Iter<Self> {
        Iter::new(self)
    }

    // fn iter_mut_parent(&mut self) -> IterMutParent<Self> {
    //     IterMutParent::new(self)
    // }

    /// Returns an iterator that mutably traverses the nodes in the octree depth-first.
    ///
    /// Unlike [`OctreeNode::iter`], leaf nodes can also be entered.
    ///
    /// The iterator will ensure that the octree remains normalized after it is dropped.
    fn iter_mut<S: Split<Self>>(&mut self, split: S) -> IterMut<Self, S> {
        IterMut::new(self, split)
    }

    /// Returns a reference to one of the children of this node.
    ///
    /// # Panics
    ///
    /// Panics if `index` is out of bounds or when called on a leaf node.
    fn get_child(&self, index: u8) -> NodeRef<Self>;

    /// Returns a mutable reference to one of the children of this node.
    ///
    /// Usually, a user should hold only a mutable reference to the root of an octree. Mutable
    /// access to a child node risks breaking the parent node's invariants. This is why there is no
    /// `get_child_mut_parent` function, as it's difficult to ensure `&mut self` only grants mutable
    /// access to the parent while restricting access to leaf data.
    ///
    /// For safe mutation of child nodes, use methods such as [`OctreeNode::fill`] or
    /// [`OctreeNode::iter_mut`], which guarantee that the octree's invariants are upheld. This
    /// function is intended as an internal detail for implementing these safe abstractions and
    /// should not be used directly.
    ///
    /// # Panics
    ///
    /// Panics if `index` is out of bounds or when called on a leaf node.
    fn get_child_mut_unchecked(&mut self, index: u8) -> NodeMut<Self>;

    /// Split a leaf node into a parent node that can only hold child leaves.
    ///
    /// The resulting node is denormalized, as each child leaf contains the same value.
    ///
    /// # Panics
    ///
    /// Panics if called on a non-leaf node.
    ///
    /// Panics if `inner_extent` results in more than the maximum number of splits.
    fn split_into_leaves_unchecked(&mut self, inner_extent: OctreeExtent, parent: Self::Parent);

    /// Splits a leaf node into a parent node that can hold arbitrary child nodes.
    ///
    /// The resulting node is denormalized, as each child node contains the same leaf value.
    ///
    /// While this function could have a default implementation by calling
    /// [`OctreeNode::split_into_leaves_unchecked`] followed by
    /// [`OctreeNode::convert_leaves_into_nodes_unchecked`], this can usually be implemented more
    /// efficiently if done manually.
    ///
    /// # Panics
    ///
    /// Panics if called on a non-leaf node.
    ///
    /// Panics if `inner_extent` results in more than the maximum number of splits.
    fn split_into_nodes_unchecked(&mut self, inner_extent: OctreeExtent, parent: Self::Parent);

    /// Converts a parent node holding leaves into a parent node holding arbitrary child nodes.
    ///
    /// # Panics
    ///
    /// Panics if called on a non-leaves node.
    fn convert_leaves_into_nodes_unchecked(&mut self, inner_extent: OctreeExtent);

    /// Renormalizes this node, which is necessary after unchecked mutations.
    ///
    /// - Nodes that hold the same value in each leaf are merged into a single leaf.
    /// - Nodes that can hold arbitrary nodes but only hold leaves are turned into leaves nodes.
    ///
    /// Note, that only the node that this function directly called on is normalized. Child nodes
    /// are assumed to already be normalized for performance reasons.
    ///
    /// If nodes are turned into leaves, the cached value is copied over and *not* recomputed.
    /// Always call [`OctreeNode::recompute_cache`] after mutations to non-leaf nodes.
    fn renormalize(&mut self);

    /// Recalculates the cache value for the node, which is necessary after unchecked mutations.
    ///
    /// Assumes the caches of child nodes are already up-to-date.
    ///
    /// # Panics
    ///
    /// Panics if called on a leaf node.
    fn recompute_cache(&mut self, inner_extent: OctreeExtent);
}

type CacheRef<'a, T> =
    <<T as OctreeNode>::Cache<'a> as OctreeCache<'a, <T as OctreeNode>::LeafRef<'a>>>::Ref;

/// A reference to leaf, parent and cache data of a node.
#[derive_where(Clone, Copy)]
#[derive_where(Debug, Hash, PartialEq, Eq; T::LeafRef<'a>, T::Parent, CacheRef<'a, T>)]
pub enum NodeDataRef<'a, T: OctreeNode + 'a> {
    Leaf(T::LeafRef<'a>),
    Parent(&'a T::Parent, CacheRef<'a, T>),
}

impl<'a, T: OctreeNode> NodeDataRef<'a, T> {
    pub fn is_leaf(self) -> bool {
        matches!(self, Self::Leaf(..))
    }

    pub fn is_parent(self) -> bool {
        matches!(self, Self::Parent(..))
    }
}

/// A reference to leaf, parent and cache data of a node with mutable access to the parent.
#[derive_where(Debug, Hash, PartialEq, Eq; T::LeafRef<'a>, T::Parent, CacheRef<'a, T>)]
pub enum NodeDataMutParent<'a, T: OctreeNode + 'a> {
    Leaf(T::LeafRef<'a>),
    Parent(&'a mut T::Parent, CacheRef<'a, T>),
}

/// A mutable reference to leaf, parent and cache data of a node.
///
/// Cache data remains immutable, since it cannot be changed manually.
#[derive_where(Debug, Hash, PartialEq, Eq; T::LeafMut<'a>, T::Parent, CacheRef<'a, T>)]
pub enum NodeDataMut<'a, T: OctreeNode + 'a> {
    Leaf(T::LeafMut<'a>),
    Parent(&'a mut T::Parent, CacheRef<'a, T>),
}

/// A reference to a node within an octree.
///
/// [`Self::Leaf`] is used exclusively if a leaf is part of a leaves nodes rather than its own node.
#[derive_where(Clone, Copy)]
#[derive_where(Debug, Hash, PartialEq, Eq; T, T::LeafRef<'a>)]
pub enum NodeRef<'a, T: OctreeNode + 'a> {
    Node(&'a T),
    Leaf(T::LeafRef<'a>),
}

impl<'a, T: OctreeNode> NodeRef<'a, T> {
    pub fn data(self) -> NodeDataRef<'a, T> {
        match self {
            Self::Node(node) => node.as_data(),
            Self::Leaf(leaf) => NodeDataRef::Leaf(leaf),
        }
    }

    pub fn as_parent(self) -> Option<ParentNodeRef<'a, T>> {
        if let Self::Node(node) = self {
            ParentNodeRef::new(node)
        } else {
            None
        }
    }
}

/// A mutable reference to a node within an octree.
///
/// [`Self::Leaf`] is used exclusively if a leaf is part of a leaves nodes rather than its own node.
///
/// [`Self::Leaf`] also means, that the leaf cannot be turned into a proper node, since that would
/// require changing the parent to a proper parent node.
#[derive_where(Debug, Hash, PartialEq, Eq; T, T::LeafMut<'a>)]
pub enum NodeMut<'a, T: OctreeNode + 'a> {
    Node(&'a mut T),
    Leaf(T::LeafMut<'a>),
}

/// A reference to a node that is known to have children.
#[derive_where(Clone, Copy)]
#[derive(Debug, Hash, PartialEq, Eq)]
pub struct ParentNodeRef<'a, T>(&'a T);

impl<'a, T: OctreeNode> ParentNodeRef<'a, T> {
    pub fn new(node: &'a T) -> Option<Self> {
        node.as_data().is_parent().then(|| Self(node))
    }

    pub fn get(self) -> &'a T {
        self.0
    }
}

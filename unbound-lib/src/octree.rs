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

    /// The type of extra data that parent nodes can hold.
    type Parent: Clone;

    /// The type of cached leaf value.
    ///
    /// Calculated from [`OctreeNode::Leaf`] and already calculated [`OctreeNode::Cache`].
    type Cache<'a>: OctreeCache<Self::LeafRef<'a>>
    where
        Self::Leaf: 'a;

    /// A reference to a cached value, usually `&Self::Cache`.
    ///
    /// Allows e.g. calculating the count of set [`bool`]s on the fly rather than storing it.
    ///
    /// There is no `CacheMut`, since cache should always be calculated via the [`OctreeCache`]
    /// trait and never be mutated directly.
    type CacheRef<'a>: Copy
    where
        Self::Leaf: 'a,
        Self::Cache<'a>: 'a;

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

    /// Returns the node itself as a [`NodeRef`].
    fn as_node_ref(&self) -> NodeRef<Self> {
        if let NodeDataRef::Leaf(leaf) = self.as_data() {
            NodeRef::Leaf(leaf)
        } else {
            NodeRef::Parent(ParentNodeRef(self))
        }
    }

    /// Returns the node itself as a [`NodeMut`].
    fn as_node_mut(&mut self) -> NodeMut<Self> {
        // borrow checker forbids returning self after matching on as_data_mut
        if self.as_data().is_leaf() {
            let NodeDataMut::Leaf(leaf) = self.as_data_mut() else {
                unreachable!()
            };
            NodeMut::Leaf(leaf)
        } else {
            NodeMut::Parent(ParentNodeMut(self))
        }
    }

    /// Fills the entire node with the given leaf while keeping the original extent.
    fn fill(&mut self, leaf: Self::Leaf) {
        *self = Self::new(self.extent(), leaf);
    }

    /// Returns an iterator that traverses the nodes in the octree depth-first.
    ///
    /// Specific parent nodes can be skipped using [`Visit`]. This is technically not necessary,
    /// but dramatically speeds up iteration by allowing to skip entering parent nodes.
    ///
    /// - Use [`AllLeaves`](visit::AllLeaves) to iterate over all leaves
    /// - Use [`Within`](visit::Within) to skip entering nodes outside some bounds
    ///
    /// Custom implementations of [`Visit`] can be used for more complex scenarios.
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
    /// - Use [`AllLeaves`](visit::AllLeaves) iterate all nodes but never enter/split leaf nodes
    ///
    /// Custom implementations of [`VisitMut`] can be used for more complex scenarios.
    ///
    /// The iterator will ensure that the octree remains normalized through iteration.
    fn iter_mut(&mut self) -> IterMut<Self> {
        IterMut::new(self)
    }

    /// Returns a [`NodeRef`] to one of the children of this node.
    ///
    /// # Panics
    ///
    /// Panics if `index` is out of bounds. It will also panic if called on a leaf node, which can
    /// be seen as a node that has zero children, therefore resulting in an "out of bounds" for any
    /// `index` including `0`.
    fn get_child(&self, index: u8) -> NodeRef<Self>;

    /// Returns a [`NodeMut`] to one of the children of this node.
    ///
    /// Usually, a user should only ever hold a mutable reference to the root of an octree. Getting
    /// mutable access to a child node can easily break the invariants of the parent node. That is
    /// also the reason, as to why there is no `get_child_mut_parent`, since there is no easy way to
    /// limit a `&mut self` to only have mutable access to the parent but not leaf data.
    ///
    /// Mutations can be done safely via e.g. [`OctreeNode::fill`] or [`OctreeNode::iter_mut`],
    /// which can guarantee that the invariant is upheld on their own. This function should be
    /// seen as an implementation detail and is required to implement these safe abstractions.
    ///
    /// # Panics
    ///
    /// Panics if `index` is out of bounds. It will also panic if called on a leaf node, which can
    /// be seen as a node that has zero children, therefore resulting in an "out of bounds" for any
    /// `index` including `0`.
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
    fn renormalize(&mut self);

    /// Recalculates the cache value for the node, which is necessary after unchecked mutations.
    ///
    /// Assumes the caches of child nodes are already up-to-date.
    fn recompute_cache(&mut self, inner_extent: OctreeExtent);
}

/// A reference to leaf, parent and cache data of a node.
#[derive_where(Clone, Copy)]
#[derive_where(Debug, Hash, PartialEq, Eq; T::LeafRef<'a>, T::Parent, T::CacheRef<'a>)]
pub enum NodeDataRef<'a, T: OctreeNode + 'a> {
    Leaf(T::LeafRef<'a>),
    Parent(&'a T::Parent, T::CacheRef<'a>),
}

impl<T: OctreeNode> NodeDataRef<'_, T> {
    pub fn is_leaf(self) -> bool {
        matches!(self, Self::Leaf(_))
    }

    pub fn is_parent(self) -> bool {
        matches!(self, Self::Parent(..))
    }
}

/// A reference to leaf, parent and cache data of a node with mutable access to the parent.
#[derive_where(Debug, Hash, PartialEq, Eq; T::LeafRef<'a>, T::Parent, T::CacheRef<'a>)]
pub enum NodeDataMutParent<'a, T: OctreeNode + 'a> {
    Leaf(T::LeafRef<'a>),
    Parent(&'a mut T::Parent, T::CacheRef<'a>),
}

impl<T: OctreeNode> NodeDataMutParent<'_, T> {
    pub fn is_leaf(self) -> bool {
        matches!(self, Self::Leaf(_))
    }

    pub fn is_parent(self) -> bool {
        matches!(self, Self::Parent(..))
    }
}

/// A mutable reference to leaf, parent and cache data of a node.
///
/// Cache data remains immutable, since it cannot be changed manually.
#[derive_where(Debug, Hash, PartialEq, Eq; T::LeafMut<'a>, T::Parent, T::CacheRef<'a>)]
pub enum NodeDataMut<'a, T: OctreeNode + 'a> {
    Leaf(T::LeafMut<'a>),
    Parent(&'a mut T::Parent, T::CacheRef<'a>),
}

impl<T: OctreeNode> NodeDataMut<'_, T> {
    pub fn is_leaf(self) -> bool {
        matches!(self, Self::Leaf(_))
    }

    pub fn is_parent(self) -> bool {
        matches!(self, Self::Parent(..))
    }
}

/// A reference to a child node, distinguishing between leaf and parent nodes.
#[derive_where(Clone, Copy)]
#[derive_where(Debug, Hash, PartialEq, Eq; T, T::LeafRef<'a>)]
pub enum NodeRef<'a, T: OctreeNode + 'a> {
    Leaf(T::LeafRef<'a>),
    Parent(ParentNodeRef<'a, T>),
}

impl<'a, T: OctreeNode> NodeRef<'a, T> {
    pub fn is_leaf(self) -> bool {
        matches!(self, Self::Leaf(_))
    }

    pub fn is_parent(self) -> bool {
        matches!(self, Self::Parent(..))
    }

    pub fn as_parent(self) -> Option<ParentNodeRef<'a, T>> {
        if let Self::Parent(parent) = self {
            Some(parent)
        } else {
            None
        }
    }
}

/// A reference to a node that is known to have children.
#[derive_where(Clone, Copy)]
#[derive(Debug, Hash, PartialEq, Eq)]
pub struct ParentNodeRef<'a, T>(&'a T);

impl<'a, T: OctreeNode> ParentNodeRef<'a, T> {
    /// Returns a [`NodeRef`] to one of the children of this node.
    ///
    /// # Panics
    ///
    /// Panics if `index` is out of bounds.
    pub fn get_child(self, index: u8) -> NodeRef<'a, T> {
        self.0.get_child(index)
    }
}

/// A mutable reference to a child node, distinguishing between leaf and parent nodes.
#[derive_where(Debug, Hash, PartialEq, Eq; T, T::LeafMut<'a>)]
pub enum NodeMut<'a, T: OctreeNode + 'a> {
    Leaf(T::LeafMut<'a>),
    Parent(ParentNodeMut<'a, T>),
}

impl<'a, T: OctreeNode> NodeMut<'a, T> {
    pub fn is_leaf(self) -> bool {
        matches!(self, Self::Leaf(_))
    }

    pub fn is_parent(self) -> bool {
        matches!(self, Self::Parent(..))
    }

    pub fn as_leaf(self) -> Option<T::LeafMut<'a>> {
        if let Self::Leaf(leaf) = self {
            Some(leaf)
        } else {
            None
        }
    }
}

/// A mutable reference to a node that is known to have children.
#[derive(Debug, Hash, PartialEq, Eq)]
pub struct ParentNodeMut<'a, T>(&'a mut T);

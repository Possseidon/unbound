pub mod bounds;
pub mod extent;
mod node;
pub mod visit;

use std::ops::ControlFlow;

use derive_where::derive_where;
use extent::{OctreeExtent, OctreeSplitBuffer};
use node::Node;
use visit::{OctreeVisitor, OctreeVisitorMut};

/// An octree with leaves of type `T` with side lengths that must be powers of two.
///
/// Unlike with normal octrees, the side lengths do not have to be equal. In fact, this type can be
/// used perfectly fine as a quadtree as well.
///
/// Octree subdivisions are not limited to `2x2x2` and instead store up to 64 child nodes. This will
/// usually result in subdivisions of size `4x4x4`, but could also be e.g. `8x8x1`, `2x4x8` or
/// `2x1x1`. Note, that the subdivision layout is not stored within each node, but instead is
/// calculated on the fly when traversing the octree (based on the full extent of the octree).
///
/// Parent nodes can hold arbitrary additional data of type `P` as well as an immutable cached value
/// `C` that is calculated based on leaf nodes and other cached values.
///
/// TODO: Consider a custom Debug impl using visit; keep the Debug impl on Node as is for, well,
/// debugging purposes, but don't actually use it
#[derive_where(Clone; T)]
#[derive(Debug)]
#[derive_where(Hash, PartialEq, Eq; T, P)]
pub struct Octree<T, P = (), C = ()> {
    /// The extent of the octree.
    extent: OctreeExtent,
    /// The root node of the octree.
    root: Node<T, P, C>,
}

impl<T, P, C> Octree<T, P, C> {
    /// Wraps the provided `leaf` value in an [`Octree`] with the specified `extent`.
    pub const fn new(extent: OctreeExtent, leaf: T) -> Self {
        Self {
            extent,
            root: Node::Leaf(leaf),
        }
    }

    /// Wraps the [`Default`] value of `T` in an [`Octree`] with the specified `extent`.
    pub fn with_default(extent: OctreeExtent) -> Self
    where
        T: Default,
    {
        Self::new(extent, T::default())
    }

    /// The extent of the [`Octree`].
    pub fn extent(&self) -> OctreeExtent {
        self.extent
    }

    /// Provides direct access to the root node of the octree.
    ///
    /// See [`Self::visit`] and [`Self::visit_mut`] for full traversal.
    pub fn root(&self) -> OctreeNode<T, P, C> {
        self.root.get()
    }

    /// Clears the [`Octree`], replacing it with the single given `leaf` value.
    pub fn fill(&mut self, leaf: T) {
        self.root = Node::Leaf(leaf);
    }

    /// Traverses the [`Octree`] immutably in a depth-first manner.
    pub fn visit<V: OctreeVisitor<Leaf = T, Parent = P, Cache = C>>(
        &self,
        visitor: &mut V,
    ) -> ControlFlow<V::Break> {
        let mut buffer = OctreeSplitBuffer::EMPTY;
        let splits = self.extent.to_splits(&mut buffer);
        self.root.visit(visitor, self.extent.into(), splits)
    }

    /// Traverses the [`Octree`] mutably in a depth-first manner.
    pub fn visit_mut<V: OctreeVisitorMut<Leaf = T, Parent = P, Cache = C>>(
        &mut self,
        visitor: &mut V,
    ) -> ControlFlow<V::Break>
    where
        T: Clone + Eq,
        P: Clone,
        C: Clone + OctreeCache<T>,
    {
        let mut buffer = OctreeSplitBuffer::EMPTY;
        let splits = self.extent.to_splits(&mut buffer);
        self.root.visit_mut(visitor, self.extent.into(), splits)
    }
}

/// An immutable reference to the values of a node in an [`Octree`].
///
/// Intentionally does _not_ implement [`Eq`] and friends, since it is unclear as to whether that
/// should include cache or not.
#[derive(Debug)]
#[derive_where(Clone, Copy)]
pub enum OctreeNode<'a, T, P, C> {
    Leaf(&'a T),
    Parent { parent: &'a P, cache: &'a C },
}

impl<'a, T, P, C> OctreeNode<'a, T, P, C> {
    /// Whether this is a leaf node.
    pub fn is_leaf(self) -> bool {
        matches!(self, Self::Leaf(_))
    }

    /// Returns the value of a leaf node.
    pub fn leaf(self) -> Option<&'a T> {
        if let Self::Leaf(leaf) = self {
            Some(leaf)
        } else {
            None
        }
    }

    /// Whether this is a parent node.
    pub fn is_parent(self) -> bool {
        matches!(self, Self::Parent { .. })
    }

    /// Returns the value of the parent node.
    pub fn parent(self) -> Option<&'a P> {
        if let Self::Parent { parent, .. } = self {
            Some(parent)
        } else {
            None
        }
    }

    /// Returns the cached value of a parent node.
    pub fn cache(self) -> Option<&'a C> {
        if let Self::Parent { cache, .. } = self {
            Some(cache)
        } else {
            None
        }
    }

    /// Returns both parent value and cached value of a parent node.
    pub fn parent_and_cache(self) -> Option<(&'a P, &'a C)> {
        if let Self::Parent { parent, cache } = self {
            Some((parent, cache))
        } else {
            None
        }
    }
}

/// A value used to cache leaf related properties in an octree.
pub trait OctreeCache<T>: Sized {
    /// Computes a cached value from leaf values `T` and already cached values of parent nodes.
    ///
    /// `child_extent` is usually used as a sort of multiplier for leaf values. E.g. to cache the
    /// number of set bits in an octree of bools, you would count `true` not just as `1` but as the
    /// volume of the `child_extent`.
    fn compute_cache<'a>(
        children: impl ExactSizeIterator<Item = LeafOrCache<'a, T, Self>>,
        child_extent: OctreeExtent,
    ) -> Self
    where
        T: 'a,
        Self: 'a;
}

/// A reference to either a leaf value `T` or a cached value `C`.
pub enum LeafOrCache<'a, T, C> {
    Leaf(&'a T),
    Cache(&'a C),
}

impl<T> OctreeCache<T> for () {
    fn compute_cache<'a>(
        _children: impl ExactSizeIterator<Item = LeafOrCache<'a, T, Self>>,
        _child_extent: OctreeExtent,
    ) -> Self
    where
        T: 'a,
        Self: 'a,
    {
    }
}

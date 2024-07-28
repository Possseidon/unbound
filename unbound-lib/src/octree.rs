pub mod bounds;
pub mod extent;
mod node;
pub mod visit;

use std::ops::ControlFlow;

use derive_where::derive_where;
use extent::{OctreeExtent, OctreeSplitBuffer};
use node::Node;
use visit::{OctreeVisitor, OctreeVisitorMut};

/// An octree storing values of type `T` with side lengths that must be powers of two.
///
/// Unlike with normal octrees, the side lengths do not have to be equal. In fact, this type can be
/// used perfectly fine as a quadtree as well.
///
/// Octree subdivisions are not limited to `2x2x2` and instead store up to 64 child nodes. This will
/// usually result in subdivisions of size `4x4x4`, but could also be e.g. `8x8x1`, `2x4x8` or
/// `2x1x1`. Note, that the subdivision layout is not stored within each node, but instead is
/// calculated on the fly when traversing the octree (based on the full extent of the octree).
#[derive(Clone, Debug, Default, Hash, PartialEq, Eq)]
pub struct Octree<T, Cache = NoCache> {
    /// The extent of the octree.
    extent: OctreeExtent,
    /// The root node of the octree.
    root: Node<T, Cache>,
}

impl<T, Cache> Octree<T, Cache> {
    /// Wraps the provided `value` in an [`Octree`] with the specified `extent`.
    pub const fn new(extent: OctreeExtent, value: T) -> Self {
        Self {
            extent,
            root: Node::Value(value),
        }
    }

    /// Constructs an [`Octree`] with the specified `extent` filled by the [`Default`] value of `T`.
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

    /// If the [`Octree`] holds a single value, returns that value.
    pub fn value(&self) -> Option<&T> {
        self.root.value()
    }

    /// If the [`Octree`] holds multiple values, returns the cached value.
    pub fn cache(&self) -> Option<&Cache> {
        self.root.cache()
    }

    /// Returns the (possibly cached) value of the [`Octree`].
    pub fn value_or_cache(&self) -> ValueOrCache<T, Cache> {
        self.root.value_or_cache()
    }

    /// Fills the [`Octree`] with the given value.
    pub fn fill(&mut self, value: T) {
        self.root = Node::Value(value);
    }

    /// Traverses the [`Octree`] immutably in a depth-first manner.
    pub fn visit<V: OctreeVisitor<Value = T, Cache = Cache>>(
        &self,
        visitor: &mut V,
    ) -> ControlFlow<V::Break> {
        let mut buffer = OctreeSplitBuffer::EMPTY;
        let splits = self.extent.to_splits(&mut buffer);
        self.root.visit(visitor, self.extent.into(), splits)
    }

    /// Traverses the [`Octree`] mutably in a depth-first manner.
    pub fn visit_mut<V: OctreeVisitorMut<Value = T, Cache = Cache>>(
        &mut self,
        visitor: &mut V,
    ) -> ControlFlow<V::Break>
    where
        T: Clone + Eq,
        Cache: Clone + OctreeCache<T>,
    {
        let mut buffer = OctreeSplitBuffer::EMPTY;
        let splits = self.extent.to_splits(&mut buffer);
        self.root.visit_mut(visitor, self.extent.into(), splits)
    }
}

#[derive(Debug, Hash, PartialEq, Eq)]
#[derive_where(Clone, Copy)]
pub enum ValueOrCache<'a, T, Cache> {
    Value(&'a T),
    Cache(&'a Cache),
}

impl<'a, T, Cache> ValueOrCache<'a, T, Cache> {
    pub fn value(self) -> Option<&'a T> {
        if let ValueOrCache::Value(value) = self {
            Some(value)
        } else {
            None
        }
    }

    pub fn cache(self) -> Option<&'a Cache> {
        if let ValueOrCache::Cache(cache) = self {
            Some(cache)
        } else {
            None
        }
    }
}

pub trait OctreeCache<T>: Sized {
    // TODO: Add single extent parameter, so that the elements in `values` can be used.
    //       -> All values always have the same extent.
    fn compute_cache<'a>(values: impl Iterator<Item = ValueOrCache<'a, T, Self>>) -> Self
    where
        Self: 'a,
        T: 'a;
}

#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct NoCache;

impl<T> OctreeCache<T> for NoCache {
    fn compute_cache<'a>(_values: impl Iterator<Item = ValueOrCache<'a, T, Self>>) -> Self
    where
        Self: 'a,
        T: 'a,
    {
        Self
    }
}

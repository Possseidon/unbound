use std::ops::ControlFlow;

use derive_where::derive_where;
use glam::UVec3;

use super::{
    bounds::OctreeBounds, extent::OctreeExtent, node::Node, Octree, OctreeCache, ValueOrCache,
};

/// Contains callbacks for [`Octree`] traversal.
///
/// - [`OctreeVisitor::value`] is called for areas that contain a single value.
/// - [`OctreeVisitor::node`] is called for areas that contain multiple different values.
///
/// This trait could work for both DFS or BFS, but [`Octree::visit`] uses DFS.
pub trait OctreeVisitor {
    /// The value type; i.e. `T` in [`Octree<T>`].
    type Value;

    /// The type of cached values; i.e. `Cache` in [`Octree<T, Cache>`].
    type Cache;

    /// The type within the [`ControlFlow::Break`] used to break visitation.
    type Break;

    /// Called for areas within the [`Octree`] that contain a single value.
    fn value(&mut self, value: OctreeValue<Self::Value>) -> ControlFlow<Self::Break>;

    /// Called for areas within the [`Octree`] that contain multiple different values.
    ///
    /// Return [`VisitNode::Enter`] to enter the area, resulting in further callbacks.
    fn node(
        &mut self,
        node: OctreeNode<Self::Value, Self::Cache>,
    ) -> ControlFlow<Self::Break, VisitNode>;
}

/// Immutable access to an area within an [`Octree`] that contains a single value.
#[derive(Debug)]
#[derive_where(Clone, Copy)]
pub struct OctreeValue<'a, T> {
    /// The location of the value within the [`Octree`].
    bounds: OctreeBounds,
    /// A reference to the value in the [`Octree`].
    value: &'a T,
}

impl<'a, T> OctreeValue<'a, T> {
    pub(crate) fn new(bounds: OctreeBounds, value: &'a T) -> Self {
        Self { bounds, value }
    }

    /// The area within the [`Octree`] that this value covers.
    pub fn bounds(self) -> OctreeBounds {
        self.bounds
    }

    /// The value in the [`Octree`].
    pub fn value(self) -> &'a T {
        self.value
    }

    /// Clones the area into an owned [`Octree`].
    ///
    /// The [`From`] trait can also be used.
    pub fn into_octree<Cache>(self) -> Octree<T, Cache>
    where
        T: Clone,
    {
        self.into()
    }
}

impl<T: Clone, Cache> From<OctreeValue<'_, T>> for Octree<T, Cache> {
    fn from(octree: OctreeValue<T>) -> Self {
        Self {
            root: Node::Value(octree.value.clone()),
            extent: octree.bounds.extent(),
        }
    }
}

/// An immutable reference to a non-leaf node in an [`Octree`].
#[derive(Debug)]
#[derive_where(Clone, Copy)]
pub struct OctreeNode<'a, T, Cache> {
    /// The location of the node within the [`Octree`].
    bounds: OctreeBounds,
    /// The referenced node in the [`Octree`].
    ///
    /// Never [`Node::Value`], since [`OctreeValue`] handles that case.
    node: &'a Node<T, Cache>,
}

impl<'a, T, Cache> OctreeNode<'a, T, Cache> {
    pub(crate) fn new(bounds: OctreeBounds, node: &'a Node<T, Cache>) -> Self {
        Self { bounds, node }
    }

    /// The area within the [`Octree`] that this node covers.
    pub fn bounds(self) -> OctreeBounds {
        self.bounds
    }

    /// The cached value for this area in the [`Octree`].
    pub fn cache(self) -> &'a Cache
    where
        Cache: Clone + OctreeCache<T>,
    {
        self.node.cache().expect("node should not be a value")
    }

    /// Clones the area into an owned [`Octree`].
    ///
    /// The [`From`] trait can also be used.
    ///
    /// Cloning is always cheap due to nodes being stored in [`Arc`](std::sync::Arc)s.
    pub fn into_octree(self) -> Octree<T, Cache>
    where
        T: Clone,
        Cache: Clone,
    {
        self.into()
    }
}

impl<T: Clone, Cache: Clone> From<OctreeNode<'_, T, Cache>> for Octree<T, Cache> {
    fn from(octree: OctreeNode<T, Cache>) -> Self {
        Self {
            root: octree.node.clone(),
            extent: octree.bounds.extent(),
        }
    }
}

/// Contains callbacks for [`Octree`] traversal and modification.
///
/// - [`OctreeVisitorMut::point`] is called for single points.
/// - [`OctreeVisitorMut::node`] is called for areas covering multiple points.
///
/// This trait could work for both DFS or BFS, but [`Octree::visit_mut`] uses DFS.
pub trait OctreeVisitorMut {
    /// The value type; i.e. `T` in [`Octree<T>`].
    type Value;

    /// The type of cached values; i.e. `Cache` in [`Octree<T, Cache>`].
    type Cache;

    /// The type within the [`ControlFlow::Break`] used to break visitation.
    type Break;

    /// Called for single points within the [`Octree`].
    fn point(&mut self, value: OctreePointMut<Self::Value>) -> ControlFlow<Self::Break>;

    /// Called for areas covering multiple points within the [`Octree`].
    ///
    /// Return [`VisitNode::Enter`] to enter the area, resulting in further callbacks.
    fn node(
        &mut self,
        node: OctreeNodeMut<Self::Value, Self::Cache>,
    ) -> ControlFlow<Self::Break, VisitNode>;
}

/// A mutable reference to a single point within an [`Octree`].
#[derive(Debug)]
pub struct OctreePointMut<'a, T> {
    /// The location of the point within the [`Octree`].
    point: UVec3,
    /// A mutable reference to the value in the [`Octree`].
    value: &'a mut T,
}

impl<'a, T> OctreePointMut<'a, T> {
    pub(crate) fn new(point: UVec3, value: &'a mut T) -> Self {
        Self { point, value }
    }

    /// The location of the point within the [`Octree`].
    pub fn point(&self) -> UVec3 {
        self.point
    }

    /// A mutable reference to the value in the [`Octree`].
    pub fn value(&mut self) -> &mut T {
        self.value
    }

    /// Clones the area into an owned [`Octree`].
    ///
    /// The [`From`] trait can also be used.
    pub fn into_octree<Cache>(&self) -> Octree<T, Cache>
    where
        T: Clone,
    {
        self.into()
    }
}

impl<T: Clone, Cache> From<&OctreePointMut<'_, T>> for Octree<T, Cache> {
    fn from(octree: &OctreePointMut<T>) -> Self {
        Self {
            root: Node::Value(octree.value.clone()),
            extent: OctreeExtent::ONE,
        }
    }
}

/// A mutable reference to an area within an [`Octree`] that covers multiple points.
#[derive(Debug)]
pub struct OctreeNodeMut<'a, T, Cache> {
    /// The location of the node within the [`Octree`].
    bounds: OctreeBounds,
    /// A mutable reference to the node in the [`Octree`].
    node: &'a mut Node<T, Cache>,
}

impl<'a, T, Cache> OctreeNodeMut<'a, T, Cache> {
    pub(crate) fn new(bounds: OctreeBounds, node: &'a mut Node<T, Cache>) -> Self {
        Self { bounds, node }
    }

    /// The area within the [`Octree`] that this node covers.
    pub fn bounds(&self) -> OctreeBounds {
        self.bounds
    }

    /// The value for this area in the [`Octree`].
    pub fn value(&'a self) -> Option<&'a T> {
        self.node.value()
    }

    /// The cached value for this area in the [`Octree`].
    pub fn cache(&self) -> Option<&Cache>
    where
        Cache: Clone + OctreeCache<T>,
    {
        self.node.cache()
    }

    /// The (possibly cached) value for this area in the [`Octree`].
    pub fn value_or_cache(&self) -> ValueOrCache<T, Cache> {
        self.node.value_or_cache()
    }

    /// Fills the entire area with the given value.
    pub fn fill(&mut self, value: T)
    where
        T: Eq,
    {
        *self.node = Node::Value(value);
    }

    /// Replaces the entire area with the given `octree`.
    ///
    /// # Panics
    ///
    /// Panics if the extent of the given `octree` doesn't match.
    pub fn set(&mut self, octree: Octree<T, Cache>)
    where
        T: Eq,
    {
        assert!(self.bounds.extent() == octree.extent);
        *self.node = octree.root;
    }

    /// Clones the area into an owned [`Octree`].
    ///
    /// The [`From`] trait can also be used.
    ///
    /// Cloning is always cheap due to nodes being stored in [`Arc`](std::sync::Arc)s.
    pub fn into_octree(&self) -> Octree<T, Cache>
    where
        T: Clone,
        Cache: Clone,
    {
        self.into()
    }
}

impl<T: Clone, Cache: Clone> From<&OctreeNodeMut<'_, T, Cache>> for Octree<T, Cache> {
    fn from(octree: &OctreeNodeMut<T, Cache>) -> Self {
        Self {
            root: octree.node.clone(),
            extent: octree.bounds.extent(),
        }
    }
}

/// [`Octree`] control flow for nodes that can be entered.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum VisitNode {
    /// Skips over this node without entering it.
    Skip,
    /// Enters this node, causing additional calls on the visitor.
    Enter,
}

impl VisitNode {
    pub fn is_skip(&self) -> bool {
        matches!(self, Self::Skip)
    }

    pub fn is_enter(&self) -> bool {
        matches!(self, Self::Enter)
    }
}

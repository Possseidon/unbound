use std::ops::ControlFlow;

use glam::UVec3;

use super::{bounds::OctreeBounds, extent::OctreeExtent, node::Node, OctreeChunk, OctreeNode};

/// Contains callbacks for [`OctreeChunk`] traversal.
///
/// This trait could work for both DFS or BFS, but [`Octree::visit`] uses DFS.
pub trait OctreeVisitor {
    /// The type of value that leaf nodes hold; i.e. `T` in [`Octree<T>`].
    type Leaf;

    /// The type of extra data that parent nodes hold; i.e. `P` in [`Octree<T, P>`].
    type Parent;

    /// The type of cached values on parent nodes; i.e. `C` in [`Octree<T, P, C>`].
    type Cache;

    /// The type within the [`ControlFlow::Break`] used to break visitation.
    type Break;

    /// Called for leaf nodes within the [`OctreeChunk`].
    fn leaf(&mut self, leaf: LeafRef<Self::Leaf>) -> ControlFlow<Self::Break>;

    /// Called for parent nodes within the [`OctreeChunk`].
    ///
    /// Return [`VisitParent::Enter`] to enter the node, resulting in further callbacks.
    fn parent(
        &mut self,
        parent: ParentRef<Self::Leaf, Self::Parent, Self::Cache>,
    ) -> ControlFlow<Self::Break, VisitParent>;
}

/// Immutable access to a leaf node within an [`OctreeChunk`].
pub struct LeafRef<'a, T> {
    /// The location of the leaf within the [`OctreeChunk`].
    bounds: OctreeBounds,
    /// A reference to the value of this leaf.
    leaf: &'a T,
}

impl<'a, T> LeafRef<'a, T> {
    pub(crate) fn new(bounds: OctreeBounds, leaf: &'a T) -> Self {
        Self { bounds, leaf }
    }

    /// The area within the [`OctreeChunk`] that this leaf covers.
    pub fn bounds(self) -> OctreeBounds {
        self.bounds
    }

    /// The value of this leaf.
    pub fn leaf(self) -> &'a T {
        self.leaf
    }

    /// Clones the leaf into an owned [`OctreeChunk`].
    ///
    /// The [`From`] trait can also be used.
    pub fn into_octree<P, C>(self) -> OctreeChunk<T, P, C>
    where
        T: Clone,
    {
        self.into()
    }
}

impl<T: Clone, P, C> From<LeafRef<'_, T>> for OctreeChunk<T, P, C> {
    fn from(octree: LeafRef<T>) -> Self {
        Self {
            root: Node::Leaf(octree.leaf.clone()),
            extent: octree.bounds.extent(),
        }
    }
}

/// An immutable reference to a parent node in an [`OctreeChunk`].
pub struct ParentRef<'a, T, P, C> {
    /// The location of the parent node within the [`OctreeChunk`].
    bounds: OctreeBounds,
    /// The referenced parent node in the [`OctreeChunk`].
    ///
    /// Never [`Node::Leaf`], since [`LeafRef`] handles that case.
    node: &'a Node<T, P, C>,
}

impl<'a, T, P, C> ParentRef<'a, T, P, C> {
    pub(crate) fn new(bounds: OctreeBounds, node: &'a Node<T, P, C>) -> Self {
        Self { bounds, node }
    }

    /// The area within the [`OctreeChunk`] that this parent node covers.
    pub fn bounds(self) -> OctreeBounds {
        self.bounds
    }

    /// The data on this parent node.
    pub fn parent(self) -> &'a P {
        self.node.get().parent().expect("should be a parent node")
    }

    /// Clones the parent node into an owned [`OctreeChunk`].
    ///
    /// The [`From`] trait can also be used.
    ///
    /// Cloning is always cheap due to nodes being stored in [`Arc`](std::sync::Arc)s.
    pub fn into_octree(self) -> OctreeChunk<T, P, C>
    where
        T: Clone,
    {
        self.into()
    }
}

impl<T: Clone, P, C> From<ParentRef<'_, T, P, C>> for OctreeChunk<T, P, C> {
    fn from(octree: ParentRef<T, P, C>) -> Self {
        Self {
            root: octree.node.clone(),
            extent: octree.bounds.extent(),
        }
    }
}

/// Contains callbacks for [`OctreeChunk`] traversal and modification.
///
/// This trait could work for both DFS or BFS, but [`Octree::visit_mut`] uses DFS.
pub trait OctreeVisitorMut {
    /// The type of value that leaf nodes hold; i.e. `T` in [`Octree<T>`].
    type Leaf;

    /// The type of extra data that parent nodes hold; i.e. `P` in [`Octree<T, P>`].
    type Parent;

    /// The type of cached values on parent nodes; i.e. `C` in [`Octree<T, P, C>`].
    type Cache;

    /// The type within the [`ControlFlow::Break`] used to break visitation.
    type Break;

    /// Called for leaf nodes that cover a single point within the [`OctreeChunk`].
    fn point(&mut self, point: PointMut<Self::Leaf>) -> ControlFlow<Self::Break>;

    /// Called for parent nodes within the [`OctreeChunk`].
    ///
    /// Return [`VisitParent::Enter`] to enter the node, resulting in further callbacks.
    fn parent(
        &mut self,
        node: NodeMut<Self::Leaf, Self::Parent, Self::Cache>,
    ) -> ControlFlow<Self::Break, VisitParent>;

    /// Called if [`OctreeVisitorMut::parent`] returns [`VisitParent::Enter`] on a leaf node.
    fn split(&mut self, leaf: LeafRef<Self::Leaf>) -> Self::Parent;
}

/// A mutable reference to a leaf node that covers a single point within an [`OctreeChunk`].
pub struct PointMut<'a, T> {
    /// The location of the leaf within the [`OctreeChunk`].
    point: UVec3,
    /// A mutable reference to the value of this leaf.
    leaf: &'a mut T,
}

impl<'a, T> PointMut<'a, T> {
    pub(crate) fn new(point: UVec3, leaf: &'a mut T) -> Self {
        Self { point, leaf }
    }

    /// The point within the [`OctreeChunk`] that this leaf is located at.
    pub fn point(&self) -> UVec3 {
        self.point
    }

    /// A mutable reference to the value of this leaf.
    pub fn leaf(&mut self) -> &mut T {
        self.leaf
    }

    /// Clones the leaf into an owned [`OctreeChunk`].
    ///
    /// The [`From`] trait can also be used.
    pub fn into_octree<P, C>(&self) -> OctreeChunk<T, P, C>
    where
        T: Clone,
    {
        self.into()
    }
}

impl<T: Clone, P, C> From<&PointMut<'_, T>> for OctreeChunk<T, P, C> {
    fn from(octree: &PointMut<T>) -> Self {
        Self {
            root: Node::Leaf(octree.leaf.clone()),
            extent: OctreeExtent::ONE,
        }
    }
}

/// A mutable reference to a non-point node in an [`OctreeChunk`].
pub struct NodeMut<'a, T, P, C> {
    /// The location of the node within the [`OctreeChunk`].
    bounds: OctreeBounds,
    /// The referenced node in the [`OctreeChunk`].
    node: &'a mut Node<T, P, C>,
}

impl<'a, T, P, C> NodeMut<'a, T, P, C> {
    pub(crate) fn new(bounds: OctreeBounds, node: &'a mut Node<T, P, C>) -> Self {
        Self { bounds, node }
    }

    /// The area within the [`OctreeChunk`] that this node covers.
    pub fn bounds(&self) -> OctreeBounds {
        self.bounds
    }

    /// Returns an immutable reference to the contents of the node.
    pub fn get(&self) -> OctreeNode<T, P, C> {
        self.node.get()
    }

    /// Returns the leaf value of this node if it is a leaf node.
    pub fn as_leaf(&mut self) -> Option<&mut T> {
        self.node.leaf_mut()
    }

    /// Replaces the entire area with the given `octree`.
    ///
    /// # Panics
    ///
    /// Panics if the extent of the given `octree` doesn't match.
    pub fn set(&mut self, octree: OctreeChunk<T, P, C>) {
        assert!(self.bounds.extent() == octree.extent);
        *self.node = octree.root;
    }

    /// Clears the node, replacing it with the single given `leaf` value.
    pub fn fill(&mut self, leaf: T) {
        *self.node = Node::Leaf(leaf);
    }

    /// Returns the parent data of this node if it is a parent node.
    pub fn as_parent(&mut self) -> Option<&mut P>
    where
        T: Clone,
        P: Clone,
        C: Clone,
    {
        self.node.parent_mut()
    }

    /// Clones the area into an owned [`OctreeChunk`].
    ///
    /// The [`From`] trait can also be used.
    ///
    /// Cloning is always cheap due to nodes being stored in [`Arc`](std::sync::Arc)s.
    pub fn into_octree(&self) -> OctreeChunk<T, P, C>
    where
        T: Clone,
    {
        self.into()
    }
}

impl<T: Clone, P, C> From<&NodeMut<'_, T, P, C>> for OctreeChunk<T, P, C> {
    fn from(octree: &NodeMut<T, P, C>) -> Self {
        Self {
            root: octree.node.clone(),
            extent: octree.bounds.extent(),
        }
    }
}

/// [`OctreeChunk`] control flow for nodes that can be entered.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum VisitParent {
    /// Skips over this node without entering it.
    Skip,
    /// Enters this node, causing additional calls on the visitor.
    Enter,
}

impl VisitParent {
    /// Whether the parent's child nodes should be skipped.
    pub fn is_skip(&self) -> bool {
        matches!(self, Self::Skip)
    }

    /// Whether the parent's child nodes should be entered.
    pub fn is_enter(&self) -> bool {
        matches!(self, Self::Enter)
    }
}

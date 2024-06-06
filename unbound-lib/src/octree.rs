pub mod bounds;
pub mod extent;
mod node;
pub mod visit;

use bounds::OctreeBounds;
use extent::{OctreeExtent, OctreeSplitBuffer};
use glam::UVec3;
use node::Node;
use visit::{OctreeVisitor, OctreeVisitorMut};

/// An octree storing values of type `T` with side lengths that must be powers of two.
///
/// The internal representation does not just use a basic octree, but is instead stores multiple
/// levels within the same node.
#[derive(Clone, Debug, Default, Hash, PartialEq, Eq)]
pub struct Octree<T> {
    /// The root node of the octree.
    root: Node<T>,
    /// The extent of the octree.
    extent: OctreeExtent,
}

impl<T> Octree<T> {
    /// Wraps the provided `value` in an [`Octree`] with the specified `extent`.
    pub const fn new(value: T, extent: OctreeExtent) -> Self {
        Self {
            root: Node::Value(value),
            extent,
        }
    }

    /// The extent of this octree.
    pub fn extent(&self) -> OctreeExtent {
        self.extent
    }

    /// Traverses the octree with the given `visitor`.
    ///
    /// This is the base-primitive for all immutable octree operations.
    pub fn visit(&self, visitor: &mut impl OctreeVisitor<Value = T, Bounds = OctreeBounds>) {
        let mut buffer = OctreeSplitBuffer::EMPTY;
        let splits = self.extent.to_splits(&mut buffer);
        self.root.visit(visitor, self.extent.into(), splits)
    }

    /// Traverses the octree with the given `visitor` and returns `true` if it was modified.
    ///
    /// This is the base-primitive for all mutable octree operations.
    pub fn visit_mut(
        &mut self,
        visitor: &mut impl OctreeVisitorMut<Value = T, Bounds = OctreeBounds, Pos = UVec3>,
    ) -> bool
    where
        T: Clone + PartialEq,
    {
        let mut buffer = OctreeSplitBuffer::EMPTY;
        let splits = self.extent.to_splits(&mut buffer);
        self.root.visit_mut(visitor, self.extent.into(), splits)
    }
}

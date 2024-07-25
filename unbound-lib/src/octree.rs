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
/// Unlike with normal octrees, the side lengths do not have to be equal. In fact, this type can be
/// used perfectly fine as a quadtree as well.
///
/// Octree subdivisions are not limited to `2x2x2` and instead store up to 64 child nodes. This will
/// usually result in subdivisions of size `4x4x4`, but could also be e.g. `8x8x1`, `2x4x8` or
/// `2x1x1`. Note, that the subdivision layout is not stored within each node, but instead is
/// calculated on the fly when traversing the octree (based on the full extent of the octree).
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

    /// Constructs an [`Octree`] with the specified `extent` filled by the [`Default`] value of `T`.
    pub fn with_default(extent: OctreeExtent) -> Self
    where
        T: Default,
    {
        Self::new(T::default(), extent)
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
        T: Clone + Eq,
    {
        let mut buffer = OctreeSplitBuffer::EMPTY;
        let splits = self.extent.to_splits(&mut buffer);
        self.root.visit_mut(visitor, self.extent.into(), splits)
    }
}

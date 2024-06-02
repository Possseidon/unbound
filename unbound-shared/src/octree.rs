pub mod bounds;
pub mod extent;
pub mod map;
mod node;
pub mod set;
pub mod visit;

use bevy::math::{BVec3, U16Vec3};
use extent::OctreeExtent;
use node::Node;
use visit::{OctreeVisitor, OctreeVisitorMut};

/// An octree storing values of type `T` with side lengths that must be powers of two.
///
/// Implemented as a recursive data structure, which makes modifications very straightforward, but
/// is not very efficient in terms of memory usage and cache locality.
#[derive(Clone, Debug, Default, Hash, PartialEq, Eq)]
pub struct Octree<T> {
    root: Node<T>,
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

    pub fn extent(&self) -> OctreeExtent {
        self.extent
    }

    pub fn visit(&self, mut visitor: impl OctreeVisitor<Value = T>) {
        self.root.visit(&mut visitor, self.extent.into());
    }

    pub fn visit_mut(&mut self, mut visitor: impl OctreeVisitorMut<Value = T>) -> bool
    where
        T: Clone + PartialEq,
    {
        self.root.visit_mut(&mut visitor, self.extent.into())
    }
}

fn find_max(v: U16Vec3) -> BVec3 {
    v.cmpeq(U16Vec3::splat(v.max_element()))
}

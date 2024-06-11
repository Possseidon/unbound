pub mod bounds;
pub mod extent;
mod node;
pub mod visit;

use bounds::OctreeBounds;
use extent::{OctreeExtent, OctreeSplitBuffer};
use glam::UVec3;
use node::Node;
use visit::{OctreeFinder, OctreeVisitor, OctreeVisitorMut};

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
    /// Constructs an [`Octree`] with the specified `extent` filled by the [`Default`] value of `T`.
    pub fn new(extent: OctreeExtent) -> Self
    where
        T: Default,
    {
        Self {
            root: Node::Value(T::default()),
            extent,
        }
    }

    /// The extent of this octree.
    pub fn extent(&self) -> OctreeExtent {
        self.extent
    }

    pub fn find(self, bounds: OctreeBounds) -> Self {
        let mut buffer = OctreeSplitBuffer::EMPTY;
        let splits = self.extent.to_splits(&mut buffer);
        self.root.find(finder, self.extent.into(), splits);
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

#[cfg(test)]
mod tests {
    use visit::VisitBoundsMut;

    use super::*;

    #[test]
    fn dummy() {
        struct MyVisitor;

        impl OctreeVisitorMut for MyVisitor {
            type Value = u32;
            type Bounds = OctreeBounds;
            type Pos = UVec3;

            fn visit_value(&mut self, pos: Self::Pos, _value: &Self::Value) -> Option<Self::Value> {
                if pos.cmple(UVec3::splat(2)).all() {
                    Some(1)
                } else {
                    Some(2)
                }
            }

            fn visit_bounds(
                &mut self,
                bounds: Self::Bounds,
                _value: Option<&Self::Value>,
            ) -> VisitBoundsMut<Self::Value> {
                if bounds.max().cmple(UVec3::splat(2)).all() {
                    VisitBoundsMut::Fill(1)
                } else if bounds.min().cmpgt(UVec3::splat(2)).any() {
                    VisitBoundsMut::Fill(2)
                } else {
                    VisitBoundsMut::Split
                }
            }
        }

        let extent = OctreeExtent::MAX;
        // let extent = OctreeExtent::from_size_log2([6, 5, 4]).unwrap();
        let mut octree = Octree::new(0, extent);
        octree.visit_mut(&mut MyVisitor);
        println!("{octree:?}");
        panic!();
    }
}

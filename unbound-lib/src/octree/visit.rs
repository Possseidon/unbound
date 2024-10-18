use super::{bounds::OctreeBounds, NodeRef, OctreeNode, ParentNodeRef};

pub trait Visit<T: OctreeNode> {
    /// Should return `true` if this `parent` node should be entered.
    fn enter(&mut self, bounds: OctreeBounds, parent: ParentNodeRef<T>) -> bool;
}

impl<T: OctreeNode, V: Visit<T>> Visit<T> for &mut V {
    fn enter(&mut self, bounds: OctreeBounds, parent: ParentNodeRef<T>) -> bool {
        (*self).enter(bounds, parent)
    }
}

pub trait VisitMut<T: OctreeNode> {
    /// Should return `true` if this `node` should be entered.
    ///
    /// Despite being for mutable iteration, mutations are intentionally not yet allowed here.
    fn enter(&mut self, bounds: OctreeBounds, node: NodeRef<T>) -> bool;

    /// Called whenever a leaf node is entered and split into a parent node.
    fn split(&mut self, bounds: OctreeBounds, node: NodeRef<T>) -> T::Parent;
}

impl<T: OctreeNode, V: VisitMut<T>> VisitMut<T> for &mut V {
    fn enter(&mut self, bounds: OctreeBounds, node: NodeRef<T>) -> bool {
        (*self).enter(bounds, node)
    }

    fn split(&mut self, bounds: OctreeBounds, node: NodeRef<T>) -> <T as OctreeNode>::Parent {
        (*self).split(bounds, node)
    }
}

/// Iterates over all nodes in the octree.
pub struct AllLeaves;

impl<T: OctreeNode> Visit<T> for AllLeaves {
    fn enter(&mut self, _: OctreeBounds, _: ParentNodeRef<T>) -> bool {
        true
    }
}

impl<T: OctreeNode> VisitMut<T> for AllLeaves {
    fn enter(&mut self, _: OctreeBounds, node: NodeRef<T>) -> bool {
        node.is_parent()
    }

    fn split(&mut self, _: OctreeBounds, _: NodeRef<T>) -> T::Parent {
        unreachable!("leaf nodes should not be entered")
    }
}

/// Skips over nodes that are not contained within the given bounds.
pub struct Within {
    pub bounds: OctreeBounds,
}

impl<T: OctreeNode> Visit<T> for Within {
    fn enter(&mut self, bounds: OctreeBounds, _: ParentNodeRef<T>) -> bool {
        bounds.overlaps(self.bounds)
    }
}

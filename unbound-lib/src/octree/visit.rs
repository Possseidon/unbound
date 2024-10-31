use derive_where::derive_where;
use glam::UVec3;

use super::{bounds::OctreeBounds, extent::OctreeSplits, OctreeNode, ParentNodeRef};
use crate::math::bounds::UBounds3;

/// Iterates over all nodes in the octree.
pub fn all<T>(_: VisitNode<T>) -> Enter {
    Enter::All
}

/// Skips over nodes that lie outside of `target`.
pub fn within<T: OctreeNode>(target: UBounds3) -> impl Fn(VisitNode<T>) -> Enter {
    move |node| Enter::within(node.bounds(), node.splits, target)
}

/// Skips over nodes that lie outside or inside `target`.
pub fn until<T: OctreeNode>(target: UBounds3) -> impl Fn(VisitNode<T>) -> Enter {
    move |node| Enter::until(node.bounds(), node.splits, target)
}

#[derive_where(Clone, Copy)]
#[derive(Debug, Hash, PartialEq, Eq)]
pub struct VisitNode<'a, T> {
    pub(super) node: ParentNodeRef<'a, T>,
    pub(super) min: UVec3,
    pub(super) splits: OctreeSplits,
}

impl<'a, T: OctreeNode> VisitNode<'a, T> {
    /// The node that is being visited.
    pub fn node(self) -> ParentNodeRef<'a, T> {
        self.node
    }

    /// The bounds of [`Self::node`].
    pub fn bounds(self) -> OctreeBounds {
        OctreeBounds::new(self.min, self.node.get().extent())
    }

    /// Describes the layout of [`Self::node`]'s children.
    ///
    /// [`OctreeSplits::NONE`] for leaf nodes.
    pub fn splits(self) -> OctreeSplits {
        self.splits
    }
}

pub enum Enter {
    /// Skips the node without yielding any of its children.
    None,
    /// Yields only the child with the given index.
    Only { child: u8 },
    /// Yields all children.
    All,
}

impl Enter {
    fn within(bounds: OctreeBounds, splits: OctreeSplits, target: UBounds3) -> Self {
        if bounds.to_ubounds3().is_disjoint(target) {
            return Self::None;
        }

        let child_extent = bounds.extent().split(splits);

        let child_bounds = OctreeBounds::new_floored(target.lower(), child_extent);
        let upper_bounds_min = OctreeBounds::floor_min_to_extent(target.upper() - 1, child_extent);
        if child_bounds.min() != upper_bounds_min {
            return Self::All;
        }

        Self::Only {
            child: child_bounds.small_index_within(bounds.extent()),
        }
    }

    fn until(bounds: OctreeBounds, splits: OctreeSplits, target: UBounds3) -> Self {
        if target.encloses(bounds.to_ubounds3()) {
            return Self::None;
        }

        Self::within(bounds, splits, target)
    }
}

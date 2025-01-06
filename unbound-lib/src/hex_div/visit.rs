use educe::Educe;
use glam::UVec3;

use super::{bounds::Bounds, extent::Splits, HexDivNode, ParentNodeRef};
use crate::math::bounds::UBounds3;

/// Skips over nodes that lie outside of `target`.
pub fn within<T: HexDivNode>(target: UBounds3) -> impl Fn(VisitNode<T>) -> Enter {
    move |node| Enter::within(node.bounds(), node.node().get().splits(), target)
}

/// Skips over nodes that lie outside or inside `target`.
pub fn until<T: HexDivNode>(target: UBounds3) -> impl Fn(VisitNode<T>) -> Enter {
    move |node| Enter::until(node.bounds(), node.node().get().splits(), target)
}

#[derive(Educe)]
#[educe(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct VisitNode<'a, T> {
    pub(super) bounds: Bounds,
    pub(super) node: ParentNodeRef<'a, T>,
}

impl<'a, T: HexDivNode> VisitNode<'a, T> {
    /// The bounds of [`Self::node`].
    pub fn bounds(self) -> Bounds {
        self.bounds
    }

    /// The node that is being visited.
    pub fn node(self) -> ParentNodeRef<'a, T> {
        self.node
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Enter {
    /// Skips the node without yielding any of its children.
    None,
    /// Yields only the child with the given index.
    Only { child: u8 },
    /// Yields all children.
    All,
}

impl Enter {
    fn within(bounds: Bounds, splits: Splits, target: UBounds3) -> Self {
        if target.upper().cmpeq(UVec3::splat(0)).any() || bounds.to_ubounds3().is_disjoint(target) {
            return Self::None;
        }

        let child_extent = bounds.extent().split(splits);

        let child_bounds = Bounds::new_floored(target.lower(), child_extent);
        let upper_bounds_min = Bounds::floor_min_to_extent(target.upper() - 1, child_extent);
        if child_bounds.min() != upper_bounds_min {
            return Self::All;
        }

        Self::Only {
            child: child_bounds.child_index(splits),
        }
    }

    fn until(bounds: Bounds, splits: Splits, target: UBounds3) -> Self {
        if target.encloses(bounds.to_ubounds3()) {
            return Self::None;
        }

        Self::within(bounds, splits, target)
    }
}

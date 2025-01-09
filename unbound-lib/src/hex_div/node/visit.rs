use educe::Educe;

use super::{HexDivNode, ParentNodeRef};
use crate::{
    hex_div::{bounds::Bounds, extent::Extent, splits::Splits},
    math::bounds::UBounds3,
};

pub fn within<T: HexDivNode>(
    target: UBounds3,
    enter_target: bool,
) -> impl Fn(VisitNode<T>) -> Enter {
    move |node| node.enter_within(target, enter_target)
}

pub fn until<T: HexDivNode>(target: UBounds3) -> impl Fn(VisitNode<T>) -> Enter {
    within(target, false)
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

    pub fn enter_within(self, target: UBounds3, enter_target: bool) -> Enter {
        let bounds = self.bounds.to_ubounds3();
        let node = self.node;
        Enter::within(bounds, node.extent(), node.splits(), target, enter_target)
    }

    pub fn enter_until(self, target: UBounds3) -> Enter {
        self.enter_within(target, false)
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
    /// Constructs an [`Enter`] that skips over nodes outside of `target`.
    ///
    /// - `bounds` is the (possibly clamped) bounds of the node
    /// - `node_extent` is the full `extent` of the node
    /// - `splits` is the number of splits of the node
    /// - `enter_target` indicates whether `target` and its children should also be entered
    pub fn within(
        bounds: UBounds3,
        node_extent: Extent,
        splits: Splits,
        target: UBounds3,
        enter_target: bool,
    ) -> Self {
        if !enter_target && target.encloses(bounds) {
            return Self::None;
        }

        if target.is_empty() || bounds.is_disjoint(target) {
            return Self::None;
        }

        let child_extent = node_extent.split(splits);

        let child_bounds = Bounds::with_extent_at(child_extent, target.lower());
        let upper_bounds_min = Bounds::min_with_extent_at(child_extent, target.upper() - 1);
        if child_bounds.min() != upper_bounds_min {
            return Self::All;
        }

        Self::Only {
            child: child_bounds.child_index(splits),
        }
    }
}

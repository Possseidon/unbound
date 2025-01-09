use educe::Educe;
use glam::UVec3;

use super::{
    node::{visit::Enter, HexDivNode},
    ParentHexDivRef,
};
use crate::math::bounds::UBounds3;

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
#[educe(Clone, Copy, Debug)]
pub struct VisitNode<'a, T> {
    pub(super) lower: UVec3,
    pub(super) node: ParentHexDivRef<'a, T>,
}

impl<'a, T: HexDivNode> VisitNode<'a, T> {
    pub fn lower(self) -> UVec3 {
        self.lower
    }

    /// The bounds of [`Self::node`].
    pub fn bounds(self) -> UBounds3 {
        UBounds3::with_size_at(self.node.bounds.size(), self.lower)
    }

    /// The node that is being visited.
    pub fn node(self) -> ParentHexDivRef<'a, T> {
        self.node
    }

    pub fn enter_within(self, target: UBounds3, enter_target: bool) -> Enter {
        let bounds = self.bounds();
        let node = self.node.root;
        Enter::within(bounds, node.extent(), node.splits(), target, enter_target)
    }

    pub fn enter_until(self, target: UBounds3) -> Enter {
        self.enter_within(target, false)
    }
}

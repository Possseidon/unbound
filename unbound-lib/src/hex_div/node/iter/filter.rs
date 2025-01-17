pub mod bounds;
pub mod children;
pub mod leaves;
pub mod nodes;

use super::HexDivIterator;
use crate::{
    hex_div::{
        bounds::{Bounds, CachedBounds},
        extent::CachedExtent,
    },
    math::bounds::UBounds3,
};

/// Various common filtering options that can be [applied](Self::apply) on a [`HexDivIterator`].
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Filter {
    /// Skip the current node, its children, neither or both.
    Yield {
        /// Whether the node itself should be yielded.
        ///
        /// If `false`, calls either [`HexDivIterator::skip_node`] or [`HexDivIterator::enter`]
        /// depending on whether `children` should be entered.
        node: bool,
        /// Whether this node's children (if any) should be yielded.
        ///
        /// If `false`, calls [`HexDivIterator::skip_children`], unless the `node` is also skipped
        /// in which case only [`HexDivIterator::skip_node`] is called.
        children: bool,
    },
    /// Tries to enter the first child node.
    ///
    /// Calls [`HexDivIterator::enter`].
    Enter,
    /// Tries to focus to a specific `child` node.
    ///
    /// Calls [`HexDivIterator::focus`].
    Focus { child: u8 },
}

impl Filter {
    /// Yields the node itself as well as its children (if any).
    pub const YIELD: Self = Self::Yield {
        node: true,
        children: true,
    };

    /// Yields the node itself but skips its children (if any).
    pub const SKIP_CHILDREN: Self = Self::Yield {
        node: true,
        children: false,
    };

    /// Tries to skip ahead to the first child.
    ///
    /// If there are no children, skips to the next neighbor instead.
    pub const YIELD_CHILDREN: Self = Self::Yield {
        node: false,
        children: true,
    };

    /// Skips the entire node including its children (if any).
    pub const SKIP: Self = Self::Yield {
        node: false,
        children: false,
    };

    /// Applies the filter on the given `iter` and returns whether that caused it to advanced.
    ///
    /// # Panics
    ///
    /// See the respective [`HexDivIterator`] functions for when they may panic.
    pub fn apply(self, iter: &mut impl HexDivIterator) -> bool {
        match self {
            Self::Yield {
                node: true,
                children,
            } => {
                if !children {
                    iter.skip_children();
                }
                false
            }
            Self::Yield {
                node: false,
                children,
            } => {
                if !children || !iter.enter() {
                    iter.skip_node();
                }
                true
            }
            Self::Enter => iter.enter(),
            Self::Focus { child } => iter.focus(child),
        }
    }

    /// Constructs a [`Filter`] to yield nodes within `target`.
    ///
    /// Efficiently makes use of [`Filter::Focus`] if `target` only covers a single child node
    /// within `node_bounds`.
    ///
    /// If `top_level` is `true`, [`Filter::SKIP_CHILDREN`] is returned if `node_bounds` is fully
    /// enclosed within `target`, preventing their children to be entered as well.
    pub fn within_node(node_bounds: CachedBounds, target: Bounds, top_level: bool) -> Self {
        if top_level && target.encloses(node_bounds.strip_cache()) {
            Self::SKIP_CHILDREN
        } else if target.is_disjoint(node_bounds.strip_cache()) {
            Self::SKIP
        } else {
            Self::focus(node_bounds.extent(), target.to_ubounds3())
        }
    }

    /// Constructs a [`Filter`] to yield nodes within `target`.
    ///
    /// `bounds` may be clamped to be smaller than the `node_extent`.
    ///
    /// See [`Self::within_node`] for more info.
    pub fn within(
        bounds: UBounds3,
        node_extent: CachedExtent,
        target: UBounds3,
        top_level: bool,
    ) -> Self {
        if top_level && target.encloses(bounds) {
            Self::SKIP_CHILDREN
        } else if target.is_disjoint(bounds) {
            Self::SKIP
        } else {
            Self::focus(node_extent, target)
        }
    }

    /// If `target` covers a single child of `node_extent`, focuses onto that.
    ///
    /// Otherwise, all children must be [`Filter::Enter`]ed.
    ///
    /// As an optimizations, [`Self::YIELD`] is returned if `node_extent` does not have any
    /// children.
    pub fn focus(node_extent: CachedExtent, target: UBounds3) -> Self {
        let Some(child_extent) = node_extent.child_extent() else {
            return Self::YIELD;
        };
        let child_bounds = Bounds::with_extent_at(child_extent, target.lower());
        let upper_bounds_min = Bounds::min_with_extent_at(child_extent, target.upper() - 1);
        if child_bounds.min() == upper_bounds_min {
            Self::Focus {
                child: child_bounds.child_index(node_extent.child_splits()),
            }
        } else {
            Self::Enter
        }
    }
}

impl Default for Filter {
    fn default() -> Self {
        Self::YIELD
    }
}

pub mod bounds;
pub mod builder;
pub mod cache;
pub mod extent;
// pub mod iter;
pub mod node;
pub mod splits;
// pub mod visit;

use std::fmt;

use extent::Extent;
use glam::UVec3;
// use iter::Iter;
use node::{iter::HexDivIterator, HexDivDebug, HexDivNode};

use crate::math::bounds::UBounds3;

/// A [`HexDivNode`] without the power-of-two size limitation.
///
/// Non power-of-two sizes are achieved by interpreting parts of the underlying [`HexDivNode`] as
/// padding.
///
/// Does not implement [`Hash`](std::hash::Hash). Since [`Eq`] ignores the padding, calculating a
/// matching hash efficiently is quite difficult.
#[derive(Clone, Copy)]
pub struct HexDiv<T> {
    /// The area within [`Self::root`] that is not padding.
    ///
    /// Always guaranteed to lie within the bounds of [`Self::root`].
    bounds: UBounds3,
    /// The root node.
    root: T,
}

impl<T: HexDivNode> HexDiv<T> {
    /// Constructs a [`HexDiv`] of the given size holding the given leaf value.
    ///
    /// Returns [`None`] if `size` is `0` along any axis.
    ///
    /// For non power-of-two sizes, padding will be created in anicipation for growth into the
    /// _positive_ directions.
    ///
    /// # Panics
    ///
    /// Panics if `size` is bigger than [`Extent::MAX`].
    pub fn new(size: UVec3, leaf: T::Leaf) -> Option<Self> {
        size.cmpne(UVec3::ZERO).all().then(|| Self {
            root: T::new(
                Extent::ceil_from_size(size)
                    .expect("HexDiv too big")
                    .compute_cache(),
                leaf,
            ),
            bounds: UBounds3::with_size_at_origin(size),
        })
    }

    pub fn from_node(node: T) -> Self {
        Self {
            bounds: UBounds3::with_size_at_origin(node.cached_extent().size()),
            root: node,
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = (UBounds3, NodeRef<T>)> {
        self.root
            .iter()
            .filter_bounds(within(self.bounds))
            .map(|(bounds, node)| (bounds, node))
    }
}

impl<T: HexDivNode> fmt::Debug for HexDiv<T>
where
    for<'a> HexDivDebug<'a, T>: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("HexDiv")
            .field("bounds", &self.bounds)
            .field("root", &self.root.debug())
            .finish()
    }
}

impl<T: HexDivNode> PartialEq for HexDiv<T> {
    fn eq(&self, other: &Self) -> bool {
        self.bounds.size() == other.bounds.size()
        // TODO: && self.iter().zip(other).all(|(_, lhs, rhs)| lhs == rhs)
    }
}

impl<T: HexDivNode> Eq for HexDiv<T> {}

impl<T: HexDivNode> From<T> for HexDiv<T> {
    fn from(node: T) -> Self {
        Self::from_node(node)
    }
}

/// References a node within a [`HexDiv`].
#[derive(Debug)]
pub enum HexDivNodeRef<'a, T: HexDivNode> {
    Node(HexDivRef<'a, T>),
    Leaf(T::LeafRef<'a>),
}

impl<T: HexDivNode> Clone for HexDivNodeRef<'_, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: HexDivNode> Copy for HexDivNodeRef<'_, T> {}

#[derive(Debug)]
#[allow(dead_code)] // TODO: remove
pub struct HexDivRef<'a, T> {
    bounds: UBounds3,
    root: &'a T,
}

impl<T: HexDivNode> HexDivRef<'_, T> {
    // pub fn iter(&self) -> Iter<T> {
    //     Iter::new(self.bounds, self.root)
    // }
}

impl<T> Clone for HexDivRef<'_, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for HexDivRef<'_, T> {}

pub mod bounds;
pub mod builder;
pub mod cache;
pub mod extent;
pub mod iter;
pub mod node;
pub mod splits;
pub mod visit;

use std::{fmt, ops::Deref};

use extent::Extent;
use glam::UVec3;
use iter::Iter;
use node::{HexDivDebug, HexDivNode};

use crate::math::bounds::UBounds3;

/// A [`HexDivNode`] without the power-of-two size limitation.
///
/// Non power-of-two sizes are achieved by interpreting parts of the underlying [`HexDivNode`] as
/// padding.
///
/// TODO: If I implement [`Hash`], [`Eq`] they should ignore the padding.
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
            root: T::new(Extent::ceil_from_size(size).expect("HexDiv too big"), leaf),
            bounds: UBounds3::with_size_at_origin(size),
        })
    }

    pub fn from_node(node: T) -> Self {
        Self {
            bounds: UBounds3::with_size_at_origin(node.extent().size()),
            root: node,
        }
    }

    pub fn iter(&self) -> Iter<T> {
        Iter::new(self.bounds, &self.root)
    }
}

impl<T: HexDivNode> From<T> for HexDiv<T> {
    fn from(node: T) -> Self {
        Self::from_node(node)
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

/// References a node within a [`HexDiv`].
#[derive(Debug)]
pub enum HexDivNodeRef<'a, T: HexDivNode> {
    Node(HexDivRef<'a, T>),
    Leaf(T::LeafRef<'a>),
}

impl<'a, T: HexDivNode> Clone for HexDivNodeRef<'a, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'a, T: HexDivNode> Copy for HexDivNodeRef<'a, T> {}

#[derive(Debug)]
pub struct HexDivRef<'a, T> {
    bounds: UBounds3,
    root: &'a T,
}

impl<'a, T: HexDivNode> HexDivRef<'a, T> {
    pub fn iter(&self) -> Iter<T> {
        Iter::new(self.bounds, self.root)
    }
}

impl<'a, T> Clone for HexDivRef<'a, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'a, T> Copy for HexDivRef<'a, T> {}

/// A [`HexDivRef`] that is known to have children.
#[derive(Debug)]
pub struct ParentHexDivRef<'a, T>(HexDivRef<'a, T>);

impl<'a, T: HexDivNode> ParentHexDivRef<'a, T> {
    pub fn new(hex_div: HexDivRef<'a, T>) -> Option<Self> {
        hex_div.root.as_data().is_parent().then_some(Self(hex_div))
    }
}

impl<'a, T> Clone for ParentHexDivRef<'a, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'a, T> Copy for ParentHexDivRef<'a, T> {}

impl<'a, T> Deref for ParentHexDivRef<'a, T> {
    type Target = HexDivRef<'a, T>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

use std::fmt;

use glam::UVec3;

use super::{
    bounds::Bounds,
    extent::Extent,
    node::{
        builder::{BuildAction, Builder as NodeBuilder, Scratch},
        HexDivNode,
    },
    HexDiv,
};
use crate::math::bounds::UBounds3;

/// Allows building a [`HexDiv`] incrementally or from a callback.
pub struct Builder<T: HexDivNode> {
    bounds: UBounds3,
    inner: NodeBuilder<T>,
}

impl<T: HexDivNode> Clone for Builder<T>
where
    NodeBuilder<T>: Clone,
{
    fn clone(&self) -> Self {
        Self {
            bounds: self.bounds,
            inner: self.inner.clone(),
        }
    }
}

impl<T: HexDivNode> fmt::Debug for Builder<T>
where
    NodeBuilder<T>: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Builder")
            .field("bounds", &self.bounds)
            .field("inner", &self.inner)
            .finish()
    }
}

impl<T: HexDivNode> Builder<T> {
    /// TODO: Allow specifying the padding manually
    pub fn new(size: UVec3) -> Self {
        Self::with_scratch(size, Default::default())
    }

    pub fn with_scratch(size: UVec3, scratch: Scratch<T>) -> Self {
        Self {
            bounds: UBounds3::with_size_at_origin(size),
            inner: NodeBuilder::with_scratch(
                Extent::ceil_from_size(size).expect("HexDiv too big"),
                scratch,
            ),
        }
    }

    /// Returns the bounds that will be processed by the next call to [`Self::step`].
    ///
    /// Unlike with the underlying [`NodeBuilder`], these bounds might be empty in the case of it
    /// hovering over padding.
    pub fn bounds(&self) -> UBounds3 {
        Self::transform_bounds(self.bounds, self.inner.bounds())
    }

    /// Builds using the given `build` callback.
    ///
    /// The bounds in the callback are guaranteed to never be empty.
    ///
    /// Requires the leaf type to be [`Default`], since it is used for padding.
    pub fn build(&mut self, build: impl FnMut(UBounds3) -> BuildAction<T>) -> HexDiv<T>
    where
        T::Leaf: Clone + Eq + Default,
    {
        self.build_with_padding(build, &Default::default())
    }

    /// Builds using the given `build` callback.
    ///
    /// The bounds in the callback are guaranteed to never be empty.
    pub fn build_with_padding(
        &mut self,
        mut build: impl FnMut(UBounds3) -> BuildAction<T>,
        padding: &T::Leaf,
    ) -> HexDiv<T>
    where
        T::Leaf: Clone + Eq,
    {
        self.build_with_manual_padding(|bounds| {
            if bounds.is_empty() {
                BuildAction::Fill(padding.clone())
            } else {
                build(bounds)
            }
        })
    }

    /// Builds using the given `build` callback.
    ///
    /// The bounds in the callback can be empty, which means that the underlying [`NodeBuilder`] is
    /// currently hovering over padding that has to be filled with some arbitrary value.
    pub fn build_with_manual_padding(
        &mut self,
        mut build: impl FnMut(UBounds3) -> BuildAction<T>,
    ) -> HexDiv<T>
    where
        T::Leaf: Clone + Eq,
    {
        HexDiv {
            bounds: self.bounds,
            root: self
                .inner
                .build(|bounds| build(Self::transform_bounds(self.bounds, bounds))),
        }
    }

    pub fn step(&mut self, action: BuildAction<T>) -> Option<T>
    where
        T::Leaf: Clone + Eq,
    {
        self.inner.step(action)
    }

    pub fn leaf_step(&mut self, leaf: T::Leaf) -> Option<T>
    where
        T::Leaf: Clone + Eq,
    {
        self.inner.leaf_step(leaf)
    }

    pub fn node_step(&mut self, node: T) -> Option<T>
    where
        T::Leaf: Clone + Eq,
    {
        self.inner.node_step(node)
    }

    pub fn parent_step(&mut self, parent: T::Parent) {
        self.inner.parent_step(parent);
    }

    pub fn take_scratch(&mut self) -> Scratch<T> {
        self.inner.take_scratch()
    }

    pub fn insert_scratch(&mut self, scratch: Scratch<T>) {
        self.inner.insert_scratch(scratch);
    }

    /// Transforms bounds from [`Self::inner`] to the actual bounds.
    ///
    /// Can return empty bounds if [`Self::inner`] is hovering over padding.
    fn transform_bounds(bounds: UBounds3, inner_bounds: Bounds) -> UBounds3 {
        inner_bounds.to_ubounds3().clamp(bounds) - bounds.lower()
    }
}

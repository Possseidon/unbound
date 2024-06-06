use glam::UVec3;

use crate::{
    math::bounds::IBounds3,
    octree::{extent::OctreeExtent, Octree},
};

/// A map from [`IVec3`] to values of type `T` stored as an [`Octree`].
///
/// This extends [`Octree`] with the ability to allow for sizes that are _not_ powers of two.
/// The internal [`Octree`] is rounded up to the next power of two and accessors automatically strip
/// away any values outside the [`OctreeMap`]'s bounds.
///
/// Additionally, bounds don't have to start at the origin; the bounds can be any arbitrary
/// [`IVec3`], as long as the calculated [`OctreeExtent`] isn't larger than [`OctreeExtent::MAX`].
pub struct OctreeMap<T> {
    /// The underlying power-of-two octree that stores the values.
    octree: Octree<T>,
    /// The range of positions that this [`OctreeMap`] covers.
    bounds: IBounds3,
    /// The origin of [`Self::bounds`] within [`Self::octree`].
    origin: UVec3,
}

impl<T> OctreeMap<T> {
    pub fn new(bounds: IBounds3) -> Option<Self>
    where
        T: Default,
    {
        Some(Self {
            octree: Octree::new(T::default(), OctreeExtent::from_size(bounds.extent())?),
            bounds,
            origin: UVec3::ZERO,
        })
    }

    pub fn bounds(&self) -> IBounds3 {
        self.bounds
    }
}

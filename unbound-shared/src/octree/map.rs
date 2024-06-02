use bevy::math::{IVec3, UVec3};

use super::{extent::OctreeExtent, Octree};

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
    /// The minimum position that can be stored (inclusive).
    min: IVec3,
    /// The maximum position that can be stored (inclusive).
    max: IVec3,
}

impl<T> OctreeMap<T> {
    pub fn new(value: T, min: IVec3, max: IVec3) -> Option<Self> {
        if min.cmple(max).all() {
            let octree = Octree::new(value, OctreeExtent::from_size(size_from_min_max(min, max))?);
            Some(Self { octree, min, max })
        } else {
            None
        }
    }

    pub fn min(&self) -> IVec3 {
        self.min
    }

    pub fn max(&self) -> IVec3 {
        self.max
    }

    pub fn size(&self) -> UVec3 {
        size_from_min_max(self.min, self.max)
    }
}

/// Returns the size of the given inclusive `min` and `max` range as an [`UVec3`].
///
/// # Panics
///
/// Panics if `min > max`.
fn size_from_min_max(min: IVec3, max: IVec3) -> UVec3 {
    assert!(min.cmple(max).all());
    // signed wrap-around around for very large sizes, but the cast to `u32` fixes it
    max.wrapping_sub(min).as_uvec3() + 1
}

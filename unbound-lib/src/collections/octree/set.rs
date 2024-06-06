use glam::UVec3;

use crate::{
    math::bounds::IBounds3,
    octree::{extent::OctreeExtent, Octree},
};

/// A highly space efficient version of [`OctreeMap<bool>`](super::map::OctreeMap).
pub struct OctreeSet {
    /// Stores entire 4x4x4 regions of bits per value.
    octree: Octree<u64>,
    /// The range of positions that this [`OctreeSet`] covers.
    bounds: IBounds3,
    /// The origin of [`Self::bounds`] within [`Self::octree`].
    origin: UVec3,
}

impl OctreeSet {
    pub fn new(bounds: IBounds3) -> Option<Self> {
        Some(Self {
            octree: Octree::new(0, OctreeExtent::from_size(todo!())?),
            bounds,
            origin: UVec3::ZERO,
        })
    }

    pub fn bounds(&self) -> IBounds3 {
        self.bounds
    }
}

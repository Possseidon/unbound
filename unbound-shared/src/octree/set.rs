use glam::IVec3;

use super::Octree;

/// A space highly space efficient version of [`OctreeMap<bool>`](super::map::OctreeMap).
pub struct OctreeSet {
    /// Stores entire 4x4x4 per octree entry.
    octree: Octree<u64>,
    /// The minimum position that can be stored (inclusive).
    min: IVec3,
    /// The maximum position that can be stored (inclusive).
    max: IVec3,
}

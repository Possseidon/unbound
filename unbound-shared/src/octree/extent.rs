use bevy::math::{BVec3, U16Vec3, UVec3};

use super::find_max;

/// The extent of an octree, i.e. how often it can be split along the different axes.
///
/// This basically stores the maximum size of the octree in log2. This also means, that only
/// sizes that are a power of two can be stored.
#[derive(Clone, Copy, Default, Hash, PartialEq, Eq)]
pub struct OctreeExtent {
    /// Uses `5` bits per axis to store width, height and depth.
    ///
    /// The most significant bit is always set to `0`.
    pub(crate) size_log2: u16,
}

impl OctreeExtent {
    /// The smallest subdivision of an octree that can no longer be split.
    pub const ONE: Self = Self { size_log2: 0 };

    /// The largest possible size of the root of an octree.
    pub const MAX: Self = Self { size_log2: 0x7FFF };

    /// The maximum size along each axis as log2.
    pub const MAX_SIZE_LOG2: u8 = 31;

    pub const fn from_size_log2(size: U16Vec3) -> Option<Self> {
        if size.x <= Self::MAX_SIZE_LOG2 as u16
            && size.y <= Self::MAX_SIZE_LOG2 as u16
            && size.z <= Self::MAX_SIZE_LOG2 as u16
        {
            Some(Self {
                size_log2: size.x | (size.y << 5) | (size.z << 10),
            })
        } else {
            None
        }
    }

    /// Creates a new [`OctreeExtent`] for the given size.
    ///
    /// Rounds the size up to the closest representable size, i.e. the next power of two.
    ///
    /// Returns [`None`] if the size is zero or the resulting [`OctreeExtent`] would be larger
    /// than [`OctreeExtent::MAX`].
    pub const fn from_size(size: UVec3) -> Option<Self> {
        pub(crate) const fn round(value: u32) -> Option<u16> {
            match value {
                0 => None,
                1 => Some(0),
                _ => {
                    let rounded = (value - 1).ilog2() + 1;
                    if rounded <= OctreeExtent::MAX_SIZE_LOG2 as u32 {
                        Some(rounded as u16)
                    } else {
                        None
                    }
                }
            }
        }

        if let (Some(width_log2), Some(height_log2), Some(depth_log2)) =
            (round(size.x), round(size.y), round(size.z))
        {
            Some(Self {
                size_log2: width_log2 | (height_log2 << 5) | (depth_log2 << 10),
            })
        } else {
            None
        }
    }

    /// Returns the width, height and depth of this [`OctreeExtent`] in log2.
    pub const fn size_log2(self) -> U16Vec3 {
        U16Vec3::new(
            self.size_log2 & 0x1F,
            self.size_log2 >> 5 & 0x1F,
            self.size_log2 >> 10 & 0x1F,
        )
    }

    /// Returns the width, height and depth of this [`OctreeExtent`].
    pub const fn size(self) -> UVec3 {
        let size_log2 = self.size_log2();
        UVec3::new(1 << size_log2.x, 1 << size_log2.y, 1 << size_log2.z)
    }

    /// Returns which axes are the longest.
    pub fn longest_axes(self) -> BVec3 {
        find_max(self.size_log2())
    }

    /// Halfs the longest axes and returns which axes were halfed.
    ///
    /// Returns [`None`] for [`OctreeExtent::ONE`].
    pub fn half_longest(self) -> Option<(Self, BVec3)> {
        let axes = self.longest_axes();
        Some((self.half(axes)?, axes))
    }

    pub fn next_split(self) -> Option<OctreeSplit> {
        (self != Self::ONE).then(|| match self.longest_axes().bitmask().count_ones() {
            1 => OctreeSplit::Half,
            2 => OctreeSplit::Quarter,
            3 => OctreeSplit::Octant,
            _ => unreachable!(),
        })
    }

    /// Halfs the size along the given axes.
    ///
    /// Returns [`None`] for [`OctreeExtent::ONE`].
    pub fn half(mut self, axes: BVec3) -> Option<Self> {
        if self == Self::ONE {
            return None;
        }

        let size_log2 = self.size_log2();
        if axes.x {
            self.size_log2 &= 0b00000_00000_11111;
            self.size_log2 |= size_log2.x - 1;
        }
        if axes.y {
            self.size_log2 &= 0b00000_11111_00000;
            self.size_log2 |= (size_log2.y - 1) << 5;
        }
        if axes.z {
            self.size_log2 &= 0b11111_00000_00000;
            self.size_log2 |= (size_log2.z - 1) << 10;
        }
        Some(self)
    }
}

impl std::fmt::Debug for OctreeExtent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("OctreeExtent")
            .field("size_log2", &self.size_log2())
            .finish()
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum OctreeSplit {
    Half,
    Quarter,
    Octant,
}

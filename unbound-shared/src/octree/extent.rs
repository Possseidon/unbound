use bevy::math::UVec3;

/// The extent of an octree, i.e. how often it can be split along the different axes.
///
/// This basically stores the maximum size of the octree in log2. This also means, that only
/// sizes that are a power of two can be stored.
#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq)]
pub struct OctreeExtent {
    size_log2: [u8; 3],
}

impl OctreeExtent {
    /// The smallest subdivision of an octree that can no longer be split.
    pub const ONE: Self = Self { size_log2: [0; 3] };

    /// The largest possible size of the root of an octree.
    pub const MAX: Self = Self {
        size_log2: [Self::MAX_SIZE_LOG2; 3],
    };

    /// The maximum size along each axis as log2.
    pub const MAX_SIZE_LOG2: u8 = 31;

    pub const fn from_size_log2(size_log2: [u8; 3]) -> Option<Self> {
        if size_log2[0] <= Self::MAX_SIZE_LOG2
            && size_log2[1] <= Self::MAX_SIZE_LOG2
            && size_log2[2] <= Self::MAX_SIZE_LOG2
        {
            Some(Self { size_log2 })
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
        const fn round(value: u32) -> Option<u8> {
            match value {
                0 => None,
                1 => Some(0),
                _ => {
                    let rounded = (value - 1).ilog2() + 1;
                    if rounded <= OctreeExtent::MAX_SIZE_LOG2 as u32 {
                        Some(rounded as u8)
                    } else {
                        None
                    }
                }
            }
        }

        if let (Some(x), Some(y), Some(z)) = (round(size.x), round(size.y), round(size.z)) {
            Some(Self {
                size_log2: [x, y, z],
            })
        } else {
            None
        }
    }

    /// Returns the width, height and depth of this [`OctreeExtent`] in log2.
    pub const fn size_log2(self) -> [u8; 3] {
        self.size_log2
    }

    /// Returns the width, height and depth of this [`OctreeExtent`].
    pub const fn size(self) -> UVec3 {
        UVec3::new(
            1 << self.size_log2[0],
            1 << self.size_log2[1],
            1 << self.size_log2[2],
        )
    }

    /// Splits the [`OctreeExtent`] towards a cube.
    ///
    /// Returns two things:
    ///
    /// 1. The split extent
    /// 2. How many of these split extents would fit in the original extent
    ///
    /// The total number of splits can be limited by `max_splits`. Splits along multiple axes at the
    /// same time also count multiple times towards this limit.
    ///
    /// Additionally, splits only occur on all of the longest axes simultaneously, which results in
    /// the "split towards a cube" behavior. If a cube _can_ be reached, the result _will_ be a
    /// cube, even if `max_splits` was not yet reached.
    ///
    /// This also means, that if `max_splits` is less than `3`, this function will not be able to
    /// split cubes at all, since that requires at least `3` simultaneous splits.
    pub fn split_towards_cube(mut self, mut max_splits: u8) -> (Self, [u8; 3]) {
        let mut splits = [0; 3];
        loop {
            let size_log2 = self.size_log2();
            if size_log2 == [0; 3] {
                break;
            }
            let max_size_log2 = size_log2[0].max(size_log2[1]).max(size_log2[2]);
            let x = size_log2[0] == max_size_log2;
            let y = size_log2[1] == max_size_log2;
            let z = size_log2[2] == max_size_log2;
            let new_splits = u8::from(x) + u8::from(y) + u8::from(z);
            let Some(remaining_splits) = max_splits.checked_sub(new_splits) else {
                break;
            };
            max_splits = remaining_splits;
            if x {
                splits[0] += 1;
                self.size_log2[0] -= 1;
            }
            if y {
                splits[1] += 1;
                self.size_log2[1] -= 1;
            }
            if z {
                splits[2] += 1;
                self.size_log2[2] -= 1;
            }
        }
        (self, splits)
    }

    /// Calculates the optimal splits for an octree.
    ///
    /// The outermost split will be the last item in the returned slice.
    ///
    /// A single entry will only split up to 6 times combined throughout all axes.
    pub(crate) fn to_splits(self, splits: &mut [[u8; 3]; 15]) -> &[[u8; 3]] {
        let mut index = 0;

        let mut rest = self.size_log2;

        let mut split = [0; 3];

        const SPLITS_PER_LAYER: u8 = 6;
        let mut remaining_splits = SPLITS_PER_LAYER;

        while rest != [0; 3] {
            for i in 0..3 {
                if let Some(value) = rest[i].checked_sub(1) {
                    rest[i] = value;
                    split[i] += 1;
                    remaining_splits -= 1;

                    if remaining_splits == 0 {
                        splits[index] = split;
                        index += 1;
                        remaining_splits = SPLITS_PER_LAYER;
                        split = [0; 3];
                    }
                }
            }
        }

        if split != [0; 3] {
            splits[index] = split;
            index += 1;
        }

        &splits[0..index]
    }
}

impl From<OctreeExtentCompact> for OctreeExtent {
    fn from(value: OctreeExtentCompact) -> Self {
        Self {
            size_log2: [
                (value.size_log2 & 0x1F) as u8,
                (value.size_log2 >> 5 & 0x1F) as u8,
                (value.size_log2 >> 10 & 0x1F) as u8,
            ],
        }
    }
}

/// A compact version of [`OctreeExtent`].
///
/// Slower than [`OctreeExtent`] but takes up 2 instead of 3 bytes of space, which doesn't sound
/// like much, but can add up if stored in bulk.
#[derive(Clone, Copy, Default, Hash, PartialEq, Eq)]
pub struct OctreeExtentCompact {
    /// Uses `5` bits per axis to store width, height and depth.
    ///
    /// The most significant bit is always set to `0`.
    size_log2: u16,
}

impl From<OctreeExtent> for OctreeExtentCompact {
    fn from(value: OctreeExtent) -> Self {
        Self {
            size_log2: (value.size_log2[0] as u16)
                | (value.size_log2[1] as u16) << 5
                | (value.size_log2[2] as u16) << 10,
        }
    }
}

impl std::fmt::Debug for OctreeExtentCompact {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("OctreeExtent")
            .field("size_log2", &OctreeExtent::from(*self))
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn dummy() {
        let mut splits = [[0; 3]; 15];
        let splits = OctreeExtent::from_size_log2([10, 5, 4])
            .unwrap()
            .to_splits(&mut splits);
        assert_eq!(splits, &[[2; 3], [2; 3], [5, 1, 0], [1, 0, 0]]);
    }
}

use glam::{uvec3, UVec3};

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

    pub const MAX_VOLUME: u128 = Self::MAX.volume();

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
        uvec3(
            1 << self.size_log2[0],
            1 << self.size_log2[1],
            1 << self.size_log2[2],
        )
    }

    pub const fn volume(self) -> u128 {
        let [x, y, z] = self.size_log2;
        let total_splits = x + y + z;
        1 << total_splits
    }

    /// Calculates the optimal splits for an octree.
    ///
    /// The number of splits is returned along with the buffer and the outermost split will be the
    /// last item in the buffer.
    ///
    /// A single entry will only split up to 6 times combined throughout all axes.
    pub fn to_split_list(self) -> (OctreeSplitList, u8) {
        let mut buffer = OctreeSplitList::EMPTY;

        let mut index = 0;
        let mut remaining_splits = self.size_log2;
        let mut current = OctreeSplits::NONE;

        const SPLITS_PER_LAYER: u8 = 6;
        let mut remaining_splits_for_layer = SPLITS_PER_LAYER;

        while remaining_splits != [0; 3] {
            for (i, remaining_split) in remaining_splits.iter_mut().enumerate() {
                if let Some(value) = remaining_split.checked_sub(1) {
                    *remaining_split = value;
                    current.splits[i] += 1;
                    remaining_splits_for_layer -= 1;

                    if remaining_splits_for_layer == 0 {
                        buffer.levels[index] = current;
                        index += 1;
                        remaining_splits_for_layer = SPLITS_PER_LAYER;
                        current = OctreeSplits::NONE;
                    }
                }
            }
        }

        if current != OctreeSplits::NONE {
            buffer.levels[index] = current;
            index += 1;
        }

        // at most MAX_SPLITS, which fits in a u8
        (buffer, index as u8)
    }

    /// Splits the [`OctreeExtent`] `splits` times along the corresponding axes.
    ///
    /// Use [`OctreeExtent::to_splits`] to calculate the optimal splits.
    ///
    /// # Panics
    ///
    /// Panics if the [`OctreeExtent`] cannot be split `splits` times.
    pub fn split(self, splits: OctreeSplits) -> Self {
        const MSG: &str = "extent should be splittable";
        Self {
            size_log2: [
                self.size_log2[0].checked_sub(splits.splits[0]).expect(MSG),
                self.size_log2[1].checked_sub(splits.splits[1]).expect(MSG),
                self.size_log2[2].checked_sub(splits.splits[2]).expect(MSG),
            ],
        }
    }
}

impl From<OctreeExtentCompact> for OctreeExtent {
    fn from(value: OctreeExtentCompact) -> Self {
        Self {
            size_log2: [
                (value.size_log2 >> 10 & 0x1F) as u8,
                (value.size_log2 >> 5 & 0x1F) as u8,
                (value.size_log2 & 0x1F) as u8,
            ],
        }
    }
}

#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq)]
pub struct OctreeSplits {
    splits: [u8; 3],
}

impl OctreeSplits {
    const NONE: Self = Self { splits: [0; 3] };

    pub const fn total(self) -> u8 {
        self.splits[0] + self.splits[1] + self.splits[2]
    }

    pub const fn x(self) -> u8 {
        self.splits[0]
    }

    pub const fn y(self) -> u8 {
        self.splits[1]
    }

    pub const fn z(self) -> u8 {
        self.splits[2]
    }
}

#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq)]
pub struct OctreeSplitList {
    pub levels: [OctreeSplits; Self::MAX],
}

impl OctreeSplitList {
    /// The maximum number of splits that can be stored in this buffer.
    ///
    /// This is also the maximum number of splits that are possible in total for an
    /// [`OctreeExtent::MAX`].
    ///
    /// TODO: Make sure this is used correctly. I might need some - 1 in some places
    pub const MAX: usize = 16;

    /// An empty [`OctreeSplitList`] for use with [`OctreeExtent::to_splits`].
    pub const EMPTY: Self = Self {
        levels: [OctreeSplits::NONE; Self::MAX],
    };
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
            size_log2: (value.size_log2[0] as u16) << 10
                | (value.size_log2[1] as u16) << 5
                | (value.size_log2[2] as u16),
        }
    }
}

impl std::fmt::Debug for OctreeExtentCompact {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("OctreeExtent")
            .field("size_log2", &OctreeExtent::from(*self))
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn octree_extent_to_splits() {
        let (splits, len) = OctreeExtent::from_size_log2([5, 10, 4])
            .unwrap()
            .to_split_list();

        assert_eq!(len, 4);

        let mut iter = splits.levels.iter();
        assert_eq!(iter.next(), Some(&OctreeSplits { splits: [2, 2, 2] }));
        assert_eq!(iter.next(), Some(&OctreeSplits { splits: [2, 2, 2] }));
        assert_eq!(iter.next(), Some(&OctreeSplits { splits: [1, 5, 0] }));
        assert_eq!(iter.next(), Some(&OctreeSplits { splits: [0, 1, 0] }));
    }
}

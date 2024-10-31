use glam::{uvec3, UVec3};

/// The extent of an octree, i.e. how often it can be split along the different axes.
///
/// This basically stores the maximum size of the octree in log2. This also means, that only
/// sizes that are a power of two can be stored.
#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq)]
pub struct OctreeExtent {
    splits: [u8; 3],
}

impl OctreeExtent {
    /// The smallest subdivision of an octree that can no longer be split.
    pub const ONE: Self = Self { splits: [0; 3] };

    /// The largest possible size of the root of an octree.
    pub const MAX: Self = Self {
        splits: [Self::MAX_SPLITS; 3],
    };

    /// The maximum number of splits along each axis.
    pub const MAX_SPLITS: u8 = 31;

    pub const fn from_splits(splits: [u8; 3]) -> Option<Self> {
        if splits[0] <= Self::MAX_SPLITS
            && splits[1] <= Self::MAX_SPLITS
            && splits[2] <= Self::MAX_SPLITS
        {
            Some(Self { splits })
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
                    if rounded <= OctreeExtent::MAX_SPLITS as u32 {
                        Some(rounded as u8)
                    } else {
                        None
                    }
                }
            }
        }

        if let (Some(x), Some(y), Some(z)) = (round(size.x), round(size.y), round(size.z)) {
            Some(Self { splits: [x, y, z] })
        } else {
            None
        }
    }

    /// Returns the number of times the [`OctreeExtent`] can be split across the different axes.
    ///
    /// This is the same as the number of splits along each axis.
    pub const fn splits(self) -> [u8; 3] {
        self.splits
    }

    /// Returns the total number of times that this [`OctreeExtent`] can be split across all axes.
    pub const fn total_splits(&self) -> u8 {
        let [x, y, z] = self.splits;
        x + y + z
    }

    /// Returns the width, height and depth of this [`OctreeExtent`].
    pub const fn size(self) -> UVec3 {
        uvec3(
            1 << self.splits[0],
            1 << self.splits[1],
            1 << self.splits[2],
        )
    }

    /// Returns the volume of the [`OctreeExtent`].
    pub const fn volume(self) -> u128 {
        1 << self.total_splits()
    }

    /// Returns the number of full splits along with the remaining number of leftover splits.
    pub const fn full_splits_and_rest(self) -> (u8, u8) {
        let total_splits = self.total_splits();
        (
            total_splits / OctreeSplits::MAX_TOTAL,
            total_splits % OctreeSplits::MAX_TOTAL,
        )
    }

    /// Calculates the optimal splits for an octree.
    ///
    /// The outermost split is returned first in the resulting list and can contain between `1` to
    /// [`OctreeSplits::MAX_TOTAL`] splits along any combination of axes. All remaining entries are
    /// guaranteed to contain exactly [`OctreeSplits::MAX_TOTAL`] splits. The remaining entries
    /// contain no splits, i.e. [`OctreeSplits::NONE`].
    pub fn to_split_list(self) -> OctreeSplitList {
        let mut buffer = OctreeSplitList::default();

        let mut index = self.total_splits().div_ceil(OctreeSplits::MAX_TOTAL);
        let mut remaining_splits = self.splits;
        let mut current = OctreeSplits::NONE;

        let mut remaining_splits_for_layer = OctreeSplits::MAX_TOTAL;

        while remaining_splits != [0; 3] {
            for (i, remaining_split) in remaining_splits.iter_mut().enumerate() {
                if let Some(value) = remaining_split.checked_sub(1) {
                    *remaining_split = value;
                    current.splits[i] += 1;
                    remaining_splits_for_layer -= 1;

                    if remaining_splits_for_layer == 0 {
                        index -= 1;
                        buffer.levels[usize::from(index)] = current;
                        remaining_splits_for_layer = OctreeSplits::MAX_TOTAL;
                        current = OctreeSplits::NONE;
                    }
                }
            }
        }

        if current != OctreeSplits::NONE {
            index -= 1;
            buffer.levels[usize::from(index)] = current;
        }

        assert_eq!(index, 0);

        buffer
    }

    /// Splits the [`OctreeExtent`] `splits` times along the corresponding axes.
    ///
    /// Use [`Self::to_split_list`] to calculate the optimal splits.
    ///
    /// # Panics
    ///
    /// Panics if the [`OctreeExtent`] cannot be split `splits` times.
    pub fn split(self, splits: OctreeSplits) -> Self {
        const MSG: &str = "extent should be splittable";
        Self {
            splits: [
                self.splits[0].checked_sub(splits.splits[0]).expect(MSG),
                self.splits[1].checked_sub(splits.splits[1]).expect(MSG),
                self.splits[2].checked_sub(splits.splits[2]).expect(MSG),
            ],
        }
    }

    /// Doubles the [`OctreeExtent`] `splits` times along the corresponding axes.
    ///
    /// This is the inverse to [`Self::split`].
    ///
    /// # Panics
    ///
    /// Panics if the new [`OctreeExtent`] exceeds [`Self::MAX`].
    pub fn unsplit(self, splits: OctreeSplits) -> Self {
        Self::from_splits([
            self.splits[0] + splits.splits[0],
            self.splits[1] + splits.splits[1],
            self.splits[2] + splits.splits[2],
        ])
        .expect("max extent exceeded")
    }
}

impl From<OctreeExtentCompact> for OctreeExtent {
    fn from(value: OctreeExtentCompact) -> Self {
        Self {
            splits: [
                (value.splits >> 10 & 0x1F) as u8,
                (value.splits >> 5 & 0x1F) as u8,
                (value.splits & 0x1F) as u8,
            ],
        }
    }
}

/// The number of splits across different axes to get from an [`OctreeExtent`] to its child extent.
///
/// Has a similar representation to [`OctreeExtent`] itself, but is guaranteed to contain at most
/// `6` [`Self::total`] splits and therefore also a [`Self::volume`] of `64`.
#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq)]
pub struct OctreeSplits {
    splits: [u8; 3],
}

impl OctreeSplits {
    /// An [`OctreeSplits`] without any splits.
    pub const NONE: Self = Self { splits: [0; 3] };

    /// The maximum number of total splits.
    pub const MAX_TOTAL: u8 = 6;

    /// The maximum possible volume.
    pub const MAX_VOLUME: u8 = 1 << Self::MAX_TOTAL;

    /// The maximum possible volume as a [`usize`].
    pub const MAX_VOLUME_USIZE: usize = 1 << Self::MAX_TOTAL;

    pub const fn total(self) -> u8 {
        self.splits[0] + self.splits[1] + self.splits[2]
    }

    pub const fn volume(self) -> u8 {
        1 << self.total()
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

    /// Splits the given index into separate x, y and z components.
    ///
    /// Each component is as big as defined by the number of splits.
    pub const fn split_index(self, index: u8) -> [u8; 3] {
        let zyx = index;
        let zy = zyx >> self.splits[0];
        [
            zyx & !(u8::MAX << self.splits[0]),
            zy & !(u8::MAX << self.splits[1]),
            zy >> self.splits[1],
        ]
    }

    /// Merges separate x, y and z components into a single index.
    ///
    /// Each component is as big as defined by the number of splits.
    pub const fn merge_index(self, indices: [u8; 3]) -> u8 {
        indices[0]
            | (indices[1] << self.splits[0])
            | (indices[2] << (self.splits[0] + self.splits[1]))
    }
}

#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq)]
pub struct OctreeSplitList {
    levels: [OctreeSplits; Self::MAX],
}

impl OctreeSplitList {
    /// The maximum number of splits that can be stored in this buffer.
    ///
    /// This is also the maximum number of splits that are possible in total for an
    /// [`OctreeExtent::MAX`].
    pub const MAX: usize = 16;

    pub const fn level(self, index: usize) -> OctreeSplits {
        self.levels[index]
    }
}

/// A compact version of [`OctreeExtent`].
///
/// Slower than [`OctreeExtent`] but takes up 2 instead of 3 bytes of space, which doesn't sound
/// like much, but can add up if stored in bulk and can potentially help with alignment.
#[derive(Clone, Copy, Default, Hash, PartialEq, Eq)]
pub struct OctreeExtentCompact {
    /// Uses `5` bits per axis to store width, height and depth.
    ///
    /// The most significant bit is always set to `0`.
    splits: u16,
}

impl From<OctreeExtent> for OctreeExtentCompact {
    fn from(value: OctreeExtent) -> Self {
        Self {
            splits: (value.splits[0] as u16) << 10
                | (value.splits[1] as u16) << 5
                | (value.splits[2] as u16),
        }
    }
}

impl std::fmt::Debug for OctreeExtentCompact {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("OctreeExtentCompact")
            .field("splits", &OctreeExtent::from(*self).splits)
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn octree_extent_to_splits() {
        let splits = OctreeExtent::from_splits([5, 10, 4])
            .unwrap()
            .to_split_list();

        let mut iter = splits.levels.into_iter();
        assert_eq!(iter.next(), Some(OctreeSplits { splits: [0, 1, 0] }));
        assert_eq!(iter.next(), Some(OctreeSplits { splits: [1, 5, 0] }));
        assert_eq!(iter.next(), Some(OctreeSplits { splits: [2, 2, 2] }));
        assert_eq!(iter.next(), Some(OctreeSplits { splits: [2, 2, 2] }));

        for rest in iter {
            assert_eq!(rest, OctreeSplits { splits: [0, 0, 0] });
        }
    }
}

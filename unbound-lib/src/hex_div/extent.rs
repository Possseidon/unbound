use glam::{uvec3, UVec3};

use super::splits::Splits;

/// The extent of a [`HexDivNode`](super::HexDivNode).
///
/// The length along each individual axis is always a power of two and also stored as such in log2.
#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq)]
pub struct Extent {
    /// The number of splits along each axis, which is the same as the size in log2.
    splits: [u8; 3],
}

impl Extent {
    /// `1x1x1`, the smallest possible [`Extent`].
    pub const ONE: Self = Self { splits: [0; 3] };

    /// The largest possible size of a [`HexDivNode`](super::HexDivNode).
    pub const MAX: Self = Self {
        splits: [Self::MAX_SPLITS; 3],
    };

    /// The maximum number of splits along each axis.
    pub const MAX_SPLITS: u8 = 31;

    /// Constructs an [`Extent`] with the given number of `splits`.
    ///
    /// Returns [`None`] if any number of splits exceeds [`Self::MAX_SPLITS`].
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

    /// Constructs the smallest possible [`Extent`] that is at least `size` big.
    ///
    /// Returns [`None`] if any side length of `size` is `0` or requires more splits than
    /// [`Extent::MAX_SPLITS`].
    pub const fn ceil_from_size(size: UVec3) -> Option<Self> {
        const fn ceil_log2(length: u32) -> Option<u8> {
            let result = 32u32 - length.wrapping_sub(1).leading_zeros();
            if result <= Extent::MAX_SPLITS as u32 {
                Some(result as u8)
            } else {
                None
            }
        }

        if let (Some(x), Some(y), Some(z)) =
            (ceil_log2(size.x), ceil_log2(size.y), ceil_log2(size.z))
        {
            Some(Self { splits: [x, y, z] })
        } else {
            None
        }
    }

    /// Returns the number of times the [`Extent`] can be split along the different axes.
    pub const fn splits(self) -> [u8; 3] {
        self.splits
    }

    /// Returns the total number of times that this [`Extent`] can be split along all axes.
    ///
    /// I.e. the sum of [`Self::splits`].
    pub const fn total_splits(&self) -> u8 {
        let [x, y, z] = self.splits;
        x + y + z
    }

    /// Returns the width, height and depth of this [`Extent`].
    pub const fn size(self) -> UVec3 {
        uvec3(
            1 << self.splits[0],
            1 << self.splits[1],
            1 << self.splits[2],
        )
    }

    /// Returns the volume of the [`Extent`].
    ///
    /// Calculated very efficiently by just summing the total number of splits and a bitshift.
    pub const fn volume(self) -> u128 {
        1 << self.total_splits()
    }

    /// Returns the number of full [`Splits`] and number of leftover splits in the outer [`Splits`].
    pub const fn full_splits_and_rest(self) -> (u8, u8) {
        let total_splits = self.total_splits();
        (
            total_splits / Splits::MAX_TOTAL,
            total_splits % Splits::MAX_TOTAL,
        )
    }

    /// Splits the [`Extent`] `splits` times along the corresponding axes.
    ///
    /// # Panics
    ///
    /// Panics if the [`Extent`] cannot be split `splits` times.
    pub fn split(self, splits: Splits) -> Self {
        const MSG: &str = "extent should be splittable";
        Self {
            splits: [
                self.splits[0].checked_sub(splits.x()).expect(MSG),
                self.splits[1].checked_sub(splits.y()).expect(MSG),
                self.splits[2].checked_sub(splits.z()).expect(MSG),
            ],
        }
    }

    /// Doubles the [`Extent`] `splits` times along the corresponding axes.
    ///
    /// This is the inverse to [`Self::split`].
    ///
    /// # Panics
    ///
    /// Panics if the new [`Extent`] exceeds [`Self::MAX`] along any axis.
    pub fn unsplit(self, splits: Splits) -> Self {
        Self::from_splits([
            self.splits[0] + splits.x(),
            self.splits[1] + splits.y(),
            self.splits[2] + splits.z(),
        ])
        .expect("max extent exceeded")
    }
}

impl From<ExtentCompact> for Extent {
    fn from(value: ExtentCompact) -> Self {
        Self {
            splits: [
                (value.splits >> 10 & 0x1F) as u8,
                (value.splits >> 5 & 0x1F) as u8,
                (value.splits & 0x1F) as u8,
            ],
        }
    }
}

/// A compact version of [`Extent`].
///
/// Slower than [`Extent`] but takes up 2 instead of 3 bytes of space, which doesn't sound like
/// much, but can add up if stored in bulk and can potentially help with alignment.
#[derive(Clone, Copy, Default, Hash, PartialEq, Eq)]
pub struct ExtentCompact {
    /// Uses `5` bits per axis to store width, height and depth.
    ///
    /// The remaining, most significant bit is always set to `0`.
    splits: u16,
}

impl From<Extent> for ExtentCompact {
    fn from(value: Extent) -> Self {
        Self {
            splits: (value.splits[0] as u16) << 10
                | (value.splits[1] as u16) << 5
                | (value.splits[2] as u16),
        }
    }
}

impl std::fmt::Debug for ExtentCompact {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("ExtentCompact")
            .field("splits", &Extent::from(*self).splits)
            .finish()
    }
}

use std::{hash::Hash, num::NonZeroU8};

use super::extent::{CachedExtent, Extent};

/// The splits to get the [`Extent`] of a [`HexDivNode`](super::HexDivNode) node's child.
///
/// Has a similar representation to [`Extent`] itself, but is guaranteed to contain at most `6`
/// [`Self::total`] splits and therefore also has a [`Self::volume`] of at most `64`; the maximum
/// number of child nodes of a [`HexDivNode`](super::HexDivNode).
#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq)]
pub struct Splits {
    x: u8,
    y: u8,
    z: u8,
}

impl Splits {
    /// No splits; mainly used as end-marker and padding in [`SplitList`].
    pub const NONE: Self = Self { x: 0, y: 0, z: 0 };

    /// The maximum number of total splits.
    pub const MAX_TOTAL: NonZeroU8 = NonZeroU8::new(6).unwrap();

    /// The maximum possible volume.
    pub const MAX_VOLUME: NonZeroU8 = NonZeroU8::new(1 << Self::MAX_TOTAL.get()).unwrap();

    /// The maximum possible volume as a [`usize`].
    pub const MAX_VOLUME_USIZE: usize = 1 << Self::MAX_TOTAL.get();

    /// Constructs [`Splits`] for the given `extent`.
    pub const fn compute(extent: Extent) -> Self {
        let Some(child_extent) = extent.compute_child_extent() else {
            return Self::NONE;
        };
        let [x, y, z] = extent.splits();
        let [cx, cy, cz] = child_extent.splits();
        Self {
            x: x - cx,
            y: y - cy,
            z: z - cz,
        }
    }

    /// The total number of splits across all axes.
    pub const fn total(self) -> u8 {
        self.x() + self.y() + self.z()
    }

    /// The number of child nodes of a [`HexDivNode`](super::HexDivNode).
    pub const fn volume(self) -> u8 {
        1 << self.total()
    }

    /// The number of splits along the `x` axis.
    pub const fn x(self) -> u8 {
        self.x
    }

    /// The number of splits along the `y` axis.
    pub const fn y(self) -> u8 {
        self.y
    }

    /// The number of splits along the `z` axis.
    pub const fn z(self) -> u8 {
        self.z
    }

    /// Converts the splits into a [`u32`] that can be compared in `const` contexts.
    pub const fn to_u32(self) -> u32 {
        u32::from_le_bytes([self.x, self.y, self.z, 0])
    }

    /// Splits the given child `index` into separate `x`, `y` and `z` positions.
    ///
    /// [`Splits`] indicates how many bits each component takes up in the `index`.
    ///
    /// # Panics
    ///
    /// Panics if the child index is out of bounds.
    pub const fn split_index(self, index: u8) -> [u8; 3] {
        let zyx = index;
        let zy = zyx >> self.x();
        let z = zy >> self.y();
        let result = [zyx & !(u8::MAX << self.x()), zy & !(u8::MAX << self.y()), z];
        assert!(z >> self.z() == 0, "child index out of bounds");
        result
    }

    /// Merges separate `x`, `y` and `z` positions into a single child index.
    ///
    /// This is the inverse operation to [`Self::split_index`].
    ///
    /// # Panics
    ///
    /// Panics if `x`, `y` or `z` is out of bounds.
    pub const fn merge_index(self, [x, y, z]: [u8; 3]) -> u8 {
        assert!(x >> self.x() == 0, "x index out of bounds");
        assert!(y >> self.y() == 0, "y index out of bounds");
        assert!(z >> self.z() == 0, "z index out of bounds");

        x | (y << self.x()) | (z << (self.x() + self.y()))
    }
}

/// Contains all [`Splits`] of a [`HexDivNode`](super::HexDivNode) until nodes are `1x1x1` in size.
#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq)]
pub struct SplitList {
    levels: [Splits; Self::MAX],
}

impl SplitList {
    /// The maximum number of [`Splits`] necessary to create a [`SplitList`] for [`Extent::MAX`].
    pub const MAX: usize = 16;

    /// Constructs a [`SplitList`] from the given `extent` all the way down to [`Extent::ONE`].
    ///
    /// The outer split is returned first in the resulting [`SplitList`] and can contain anywhere
    /// between `0` to [`Splits::MAX_TOTAL`] splits along any combination of axes.
    ///
    /// All following entries are guaranteed to contain exactly [`Splits::MAX_TOTAL`] splits.
    ///
    /// A [`Splits::NONE`] marks the end of the [`SplitList`]. All remaining entries are also padded
    /// with [`Splits::NONE`].
    pub fn compute(mut extent: CachedExtent) -> Self {
        let mut result = Self::default();

        for level in &mut result.levels {
            *level = extent.child_splits();
            let Some(child_extent) = extent.child_extent() else {
                break;
            };
            extent = child_extent.compute_cache();
        }

        result
    }

    /// Returns [`Splits`] at a given level, starting at `0`.
    ///
    /// This function acts as if the [`SplitList`] was padded with infinitely many [`Splits::NONE`].
    /// I.e. it never panics, not even for a `level` of [`usize::MAX`].
    pub const fn level(self, level: usize) -> Splits {
        if level < self.levels.len() {
            self.levels[level]
        } else {
            Splits::NONE
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn split_list_new() {
        let splits = SplitList::compute(Extent::from_splits([5, 10, 4]).unwrap().compute_cache());

        let mut iter = splits.levels.into_iter();
        assert_eq!(iter.next(), Some(Splits { x: 0, y: 1, z: 0 }));
        assert_eq!(iter.next(), Some(Splits { x: 1, y: 5, z: 0 }));
        assert_eq!(iter.next(), Some(Splits { x: 2, y: 2, z: 2 }));
        assert_eq!(iter.next(), Some(Splits { x: 2, y: 2, z: 2 }));

        for rest in iter {
            assert_eq!(rest, Splits::NONE);
        }
    }
}

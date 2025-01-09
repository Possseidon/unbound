use super::extent::Extent;

/// The splits to get the [`Extent`] of a [`HexDivNode`](super::HexDivNode) node's child.
///
/// Has a similar representation to [`Extent`] itself, but is guaranteed to contain at most `6`
/// [`Self::total`] splits and therefore also has a [`Self::volume`] of at most `64`; the maximum
/// number of child nodes of a [`HexDivNode`](super::HexDivNode).
#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq)]
pub struct Splits {
    splits: [u8; 3],
}

impl Splits {
    /// No splits; mainly used as end-marker and padding in [`SplitList`].
    pub const NONE: Self = Self { splits: [0; 3] };

    /// The maximum number of total splits.
    pub const MAX_TOTAL: u8 = 6;

    /// The maximum possible volume.
    pub const MAX_VOLUME: u8 = 1 << Self::MAX_TOTAL;

    /// The maximum possible volume as a [`usize`].
    pub const MAX_VOLUME_USIZE: usize = 1 << Self::MAX_TOTAL;

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
        self.splits[0]
    }

    /// The number of splits along the `y` axis.
    pub const fn y(self) -> u8 {
        self.splits[1]
    }

    /// The number of splits along the `z` axis.
    pub const fn z(self) -> u8 {
        self.splits[2]
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

    /// Constructs a [`SplitList`] for a [`HexDivNode`](super::HexDivNode) with the given `extent`.
    ///
    /// The outer split is returned first in the resulting [`SplitList`] and can contain anywhere
    /// between `0` to [`Splits::MAX_TOTAL`] splits along any combination of axes.
    ///
    /// All following entries are guaranteed to contain exactly [`Splits::MAX_TOTAL`] splits.
    ///
    /// A [`Splits::NONE`] marks the end of the [`SplitList`]. All remaining entries are also padded
    /// with [`Splits::NONE`].
    pub fn new(extent: Extent) -> Self {
        let mut split_list = Self::default();

        let mut index = extent.total_splits().div_ceil(Splits::MAX_TOTAL);
        let mut remaining_splits = extent.splits();
        let mut current = Splits::NONE;

        let mut remaining_splits_for_layer = Splits::MAX_TOTAL;

        while remaining_splits != [0; 3] {
            for (i, remaining_split) in remaining_splits.iter_mut().enumerate() {
                if let Some(value) = remaining_split.checked_sub(1) {
                    *remaining_split = value;
                    current.splits[i] += 1;
                    remaining_splits_for_layer -= 1;

                    if remaining_splits_for_layer == 0 {
                        index -= 1;
                        split_list.levels[usize::from(index)] = current;
                        remaining_splits_for_layer = Splits::MAX_TOTAL;
                        current = Splits::NONE;
                    }
                }
            }
        }

        if current != Splits::NONE {
            index -= 1;
            split_list.levels[usize::from(index)] = current;
        }

        assert_eq!(index, 0);

        split_list
    }

    /// Returns [`Splits`] at a given level, starting at `0`.
    pub const fn level(self, level: usize) -> Splits {
        self.levels[level]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn split_list_new() {
        let splits = SplitList::new(Extent::from_splits([5, 10, 4]).unwrap());

        let mut iter = splits.levels.into_iter();
        assert_eq!(iter.next(), Some(Splits { splits: [0, 1, 0] }));
        assert_eq!(iter.next(), Some(Splits { splits: [1, 5, 0] }));
        assert_eq!(iter.next(), Some(Splits { splits: [2, 2, 2] }));
        assert_eq!(iter.next(), Some(Splits { splits: [2, 2, 2] }));

        for rest in iter {
            assert_eq!(rest, Splits::NONE);
        }
    }
}

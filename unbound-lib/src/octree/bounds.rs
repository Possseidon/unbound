use glam::{uvec3, UVec3};

use super::extent::{OctreeExtent, OctreeSplits};

/// The location of a node within an octree.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct OctreeBounds {
    /// The position within the octree where the bounds start.
    ///
    /// Must be a multiple of [`Self::extent`] and at most [`i32::MAX`].
    min: UVec3,
    /// The extent of the bounds.
    extent: OctreeExtent,
}

impl OctreeBounds {
    /// Returns whether the given `min` and `extent` would form a valid [`OctreeBounds`].
    ///
    /// Valid [`OctreeBounds`] require that the bounds' `min` is a multiple of its `extent`.
    ///
    /// Since [`OctreeExtent`] is limited to powers of two, this can easily be checked by making
    /// sure the lower bits are set to zero.
    pub const fn is_valid(min: UVec3, extent: OctreeExtent) -> bool {
        let floored = Self::floor_min_to_extent(min, extent);
        floored.x == min.x && floored.y == min.y && floored.z == min.z
    }

    /// Floors the given `min` so that it forms a valid [`OctreeBounds`] with the given `extent`.
    ///
    /// Since [`OctreeExtent`] is limited to powers of two, this can easily be achieved by clearing
    /// some of the lower bits.
    pub const fn floor_min_to_extent(mut min: UVec3, extent: OctreeExtent) -> UVec3 {
        let bits_to_clear = extent.size_log2();
        min.x &= u32::MAX << bits_to_clear[0];
        min.y &= u32::MAX << bits_to_clear[1];
        min.z &= u32::MAX << bits_to_clear[2];
        min
    }

    /// Constructs an [`OctreeBounds`] with the given `min` and `extent`.
    ///
    /// If `debug_assertions` are enabled, this panics if the given values are invalid.
    pub const fn new(min: UVec3, extent: OctreeExtent) -> Self {
        debug_assert!(Self::is_valid(min, extent));
        Self { min, extent }
    }

    /// Constructs an [`OctreeBounds`] with the given `min` and `extent`.
    ///
    /// Returns [`None`] if the given values do not form a [valid](Self::is_valid) [`OctreeBounds`].
    pub const fn checked_new(min: UVec3, extent: OctreeExtent) -> Option<Self> {
        if Self::is_valid(min, extent) {
            Some(Self { min, extent })
        } else {
            None
        }
    }

    /// Constructs an [`OctreeBounds`] with the specified `extent` at the origin.
    pub const fn from_extent(extent: OctreeExtent) -> Self {
        Self {
            min: UVec3::ZERO,
            extent,
        }
    }

    /// Constructs an [`OctreeBounds`] with [`OctreeExtent::ONE`] at the specified `point`.
    pub const fn from_point(point: UVec3) -> Self {
        Self {
            min: point,
            extent: OctreeExtent::ONE,
        }
    }

    /// Returns whether this [`OctreeBounds`] covers a single point.
    ///
    /// In other words, if [`Self::extent`] is [`OctreeExtent::ONE`].
    pub const fn is_point(self) -> bool {
        matches!(self.extent, OctreeExtent::ONE)
    }

    /// If the bounds only cover a single point, returns that point.
    pub const fn to_point(self) -> Option<UVec3> {
        if self.is_point() {
            Some(self.min)
        } else {
            None
        }
    }

    /// The lower bound (inclusive).
    pub const fn min(self) -> UVec3 {
        self.min
    }

    /// The upper bound (inclusive).
    pub const fn max(self) -> UVec3 {
        // purely using wrapping_add and wrapping_sub for const; it can never actually overflow
        self.min
            .wrapping_add(self.extent.size().wrapping_sub(UVec3::splat(1)))
    }

    /// Whether the specified `point` is part of the bounds.
    pub const fn contains(self, point: UVec3) -> bool {
        let max = self.max();
        (self.min.x <= point.x && point.x <= max.x)
            && (self.min.y <= point.y && point.y <= max.y)
            && (self.min.z <= point.z && point.z <= max.z)
    }

    /// Whether the bounds fully enclose `other`.
    ///
    /// Inclusive, so an [`OctreeBounds`] always encloses itself.
    pub const fn encloses(self, other: Self) -> bool {
        let self_max = self.max();
        let other_max = other.max();
        (self.min.x <= other.min.x && other_max.x <= self_max.x)
            && (self.min.y <= other.min.y && other_max.y <= self_max.y)
            && (self.min.z <= other.min.z && other_max.z <= self_max.z)
    }

    /// Whether the bounds have any point in common with `other`.
    pub const fn overlaps(self, other: Self) -> bool {
        let self_max = self.max();
        let other_max = other.max();
        (self.min.x <= other_max.x && other.min.x <= self_max.x)
            && (self.min.y <= other_max.y && other.min.y <= self_max.y)
            && (self.min.z <= other_max.z && other.min.z <= self_max.z)
    }

    /// The extent of the bounds.
    pub const fn extent(self) -> OctreeExtent {
        self.extent
    }

    /// Updates the extent and rounds `min` down so that it remains a multiple of `extent`.
    pub fn with_extent_and_floor(mut self, extent: OctreeExtent) -> Self {
        self.min = Self::floor_min_to_extent(self.min, extent);
        self.extent = extent;
        self
    }

    /// Allows iterating these bounds across all possible values within the given extent.
    ///
    /// Iterates in x, y, z order.
    ///
    /// Returns [`None`] if the extent cannot be advanced any further.
    pub const fn next_min_within(self, extent: OctreeExtent) -> Option<UVec3> {
        // the bits that are actually counting; once these bits reach zero "overflow" occured
        let count_mask_len = extent.size_log2();
        let amount_log2 = self.extent.size_log2();

        let mut min = self.min;

        // advance x
        min.x += 1 << amount_log2[0];
        let x_count_mask = u32::MAX << count_mask_len[0];
        if min.x & x_count_mask != 0 {
            return Some(min);
        }

        // x overflowed; advance y
        min.x = min.x - x_count_mask - 1;
        min.y += 1 << amount_log2[1];
        let y_count_mask = u32::MAX << count_mask_len[1];
        if min.y & y_count_mask != 0 {
            return Some(min);
        }

        // y overflowed; advance z
        min.y = min.y - y_count_mask - 1;
        min.z += 1 << amount_log2[2];
        let z_count_mask = u32::MAX << count_mask_len[2];
        if min.z & z_count_mask != 0 {
            return Some(min);
        }

        None
    }

    /// Returns the index of these bounds within the given `extent`.
    pub const fn index_within(self, extent: OctreeExtent) -> u128 {
        let inner_bit_offsets = self.extent.size_log2();
        let outer_bit_offsets = extent.size_log2();

        let x_bit_width = outer_bit_offsets[0] - inner_bit_offsets[0];
        let y_bit_width = outer_bit_offsets[1] - inner_bit_offsets[1];
        let z_bit_width = outer_bit_offsets[2] - inner_bit_offsets[2];

        let x_mask = !(u32::MAX << x_bit_width);
        let y_mask = !(u32::MAX << y_bit_width);
        let z_mask = !(u32::MAX << z_bit_width);

        let x = ((self.min.x >> inner_bit_offsets[0]) & x_mask) as u128;
        let y = ((self.min.y >> inner_bit_offsets[1]) & y_mask) as u128;
        let z = ((self.min.z >> inner_bit_offsets[2]) & z_mask) as u128;

        x | y << x_bit_width | z << y_bit_width
    }

    /// Returns the index of these bounds within the given `extent` as a [`u8`].
    ///
    /// If `debug_assertions` are enabled, this panics if the result does not fit in a [`u8`].
    pub fn small_index_within(self, extent: OctreeExtent) -> u8 {
        if cfg!(debug_assertions) {
            self.index_within(extent)
                .try_into()
                .expect("OctreeBounds index should fit in a u8")
        } else {
            let inner_bit_offsets = self.extent.size_log2();
            let outer_bit_offsets = extent.size_log2();

            let x_bit_width = outer_bit_offsets[0] - inner_bit_offsets[0];
            let y_bit_width = outer_bit_offsets[1] - inner_bit_offsets[1];
            let z_bit_width = outer_bit_offsets[2] - inner_bit_offsets[2];

            let x_mask = !(u32::MAX << x_bit_width);
            let y_mask = !(u32::MAX << y_bit_width);
            let z_mask = !(u32::MAX << z_bit_width);

            let x = ((self.min.x >> inner_bit_offsets[0]) & x_mask) as u8;
            let y = ((self.min.y >> inner_bit_offsets[1]) & y_mask) as u8;
            let z = ((self.min.z >> inner_bit_offsets[2]) & z_mask) as u8;

            x | y << x_bit_width | z << y_bit_width
        }
    }

    /// Splits the extent of the bounds according to `splits`, keeping [`Self::min`] as is.
    ///
    /// # Panics
    ///
    /// Panics if [`Self::extent`] cannot be split `splits` times.
    pub fn split_extent(mut self, splits: OctreeSplits) -> Self {
        self.extent = self.extent.split(splits);
        self
    }

    /// Splits the bounds according to `splits` and returns an iterator over the resulting bounds.
    pub fn split(self, splits: OctreeSplits) -> OctreeBoundsSplit {
        let total_splits = splits.total();
        // limited to 7; only up to 6 splits are actually used
        assert!(total_splits < 8);
        OctreeBoundsSplit {
            origin: self.min,
            extent: self.extent.split(splits),
            x_splits: splits.x(),
            y_splits: splits.y(),
            index: 0,
            end: 1 << total_splits,
        }
    }
}

impl From<OctreeExtent> for OctreeBounds {
    fn from(extent: OctreeExtent) -> Self {
        Self::from_extent(extent)
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct OctreeBoundsSplit {
    /// The origin (point with smallest x, y and z) of the outer bounds that are to be split.
    origin: UVec3,
    /// The extent of the inner bounds that are yielded.
    extent: OctreeExtent,
    /// The number of splits along the x axis.
    x_splits: u8,
    /// The number of splits along the y axis.
    y_splits: u8,
    /// The current position of the iterator.
    index: u8,
    /// Marks the end of iteration.
    end: u8,
}

impl OctreeBoundsSplit {
    /// The origin (point with smallest x, y and z) of the outer bounds that are to be split.
    pub fn origin(self) -> UVec3 {
        self.origin
    }

    /// The extent of the inner bounds that are yielded.
    pub fn extent(self) -> OctreeExtent {
        self.extent
    }

    /// Unpacks a compact index into a [`UVec3`].
    fn unpack_index(mut self) -> [u8; 3] {
        let x = self.index & ((1 << self.x_splits) - 1);
        self.index >>= self.x_splits;
        let y = self.index & ((1 << self.y_splits) - 1);
        self.index >>= self.y_splits;
        let z = self.index;
        [x, y, z]
    }
}

impl Iterator for OctreeBoundsSplit {
    type Item = OctreeBounds;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index == self.end {
            return None;
        }

        let [x, y, z] = self.unpack_index();

        // increment after unpacking
        self.index += 1;

        let size_log2 = self.extent.size_log2();
        let offset = uvec3(
            u32::from(x) << size_log2[0],
            u32::from(y) << size_log2[1],
            u32::from(z) << size_log2[2],
        );

        Some(OctreeBounds {
            min: self.origin + offset,
            extent: self.extent,
        })
    }
}

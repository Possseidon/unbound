use glam::UVec3;

use super::extent::{Extent, Splits};
use crate::math::bounds::UBounds3;

/// The location of a node within an octree.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct Bounds {
    /// The position within the octree where the bounds start.
    ///
    /// Must be a multiple of [`Self::extent`] and at most [`i32::MAX`].
    min: UVec3,
    /// The extent of the bounds.
    extent: Extent,
}

impl Bounds {
    pub const MIN_POINT: UVec3 = UVec3::ZERO;
    pub const MAX_POINT: UVec3 = Extent::MAX.size().wrapping_sub(UVec3::ONE);

    /// Returns whether the given `min` and `extent` would form a valid [`OctreeBounds`].
    ///
    /// Valid [`OctreeBounds`] require that the bounds' `min` is a multiple of its `extent`.
    ///
    /// Since [`OctreeExtent`] is limited to powers of two, this can easily be checked by making
    /// sure the lower bits are set to zero.
    pub const fn is_valid(min: UVec3, extent: Extent) -> bool {
        const MAX: u32 = Bounds::MAX_POINT.x;
        let max = min.saturating_add(extent.size());
        let floored = Self::floor_min_to_extent(min, extent);
        (max.x <= MAX && max.y <= MAX && max.z <= MAX)
            && (floored.x == min.x && floored.y == min.y && floored.z == min.z)
    }

    /// Floors the given `min` so that it forms a valid [`OctreeBounds`] with the given `extent`.
    ///
    /// Since [`OctreeExtent`] is limited to powers of two, this can easily be achieved by clearing
    /// some of the lower bits.
    pub const fn floor_min_to_extent(mut min: UVec3, extent: Extent) -> UVec3 {
        let bits_to_clear = extent.splits();
        min.x &= u32::MAX << bits_to_clear[0];
        min.y &= u32::MAX << bits_to_clear[1];
        min.z &= u32::MAX << bits_to_clear[2];
        min
    }

    /// Constructs an [`OctreeBounds`] with the given `min` and `extent`.
    ///
    /// If `debug_assertions` are enabled, this panics if the given values are invalid.
    pub const fn new(min: UVec3, extent: Extent) -> Self {
        debug_assert!(Self::is_valid(min, extent));
        Self { min, extent }
    }

    /// Constructs an [`OctreeBounds`] with the given `min` and `extent`.
    ///
    /// Returns [`None`] if the given values do not form a [valid](Self::is_valid) [`OctreeBounds`].
    pub const fn checked_new(min: UVec3, extent: Extent) -> Option<Self> {
        if Self::is_valid(min, extent) {
            Some(Self { min, extent })
        } else {
            None
        }
    }

    pub fn new_floored(min: UVec3, extent: Extent) -> Self {
        Self {
            min: Self::floor_min_to_extent(min, extent),
            extent,
        }
    }

    /// Constructs an [`OctreeBounds`] with the specified `extent` at the origin.
    pub const fn from_extent(extent: Extent) -> Self {
        Self {
            min: UVec3::ZERO,
            extent,
        }
    }

    /// Constructs an [`OctreeBounds`] with [`OctreeExtent::ONE`] at the specified `point`.
    ///
    /// If `debug_assertions` are enabled, this panics if the given `point` is invalid.
    pub const fn from_point(point: UVec3) -> Self {
        Self::new(point, Extent::ONE)
    }

    /// Returns whether this [`OctreeBounds`] covers a single point.
    ///
    /// In other words, if [`Self::extent`] is [`OctreeExtent::ONE`].
    pub const fn is_point(self) -> bool {
        matches!(self.extent, Extent::ONE)
    }

    /// If the bounds only cover a single point, returns that point.
    pub const fn to_point(self) -> Option<UVec3> {
        if self.is_point() {
            Some(self.min)
        } else {
            None
        }
    }

    pub const fn to_ubounds3(self) -> UBounds3 {
        UBounds3::with_extent_at(self.min, self.extent().size())
    }

    /// The lower bound (inclusive).
    pub const fn min(self) -> UVec3 {
        self.min
    }

    /// The upper bound (inclusive).
    pub const fn max(self) -> UVec3 {
        // purely using wrapping_add and wrapping_sub for const; it can never actually overflow
        self.min
            .wrapping_add(self.extent.size().wrapping_sub(UVec3::ONE))
    }

    /// The extent of the bounds.
    pub const fn extent(self) -> Extent {
        self.extent
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

    /// Updates the extent and rounds `min` down so that it remains a multiple of `extent`.
    pub fn with_extent_and_floor(mut self, extent: Extent) -> Self {
        self.min = Self::floor_min_to_extent(self.min, extent);
        self.extent = extent;
        self
    }

    /// Allows iterating these bounds across all possible values within the given extent.
    ///
    /// Iterates in x, y, z order.
    ///
    /// Returns [`None`] if the extent cannot be advanced any further.
    pub const fn next_min_within(self, splits: Splits) -> Option<UVec3> {
        let inner_splits = self.extent.splits();

        let mut min = self.min;

        // advance x
        min.x += 1 << inner_splits[0];
        let x_count_mask = !(u32::MAX << (inner_splits[0] + splits.x()));
        if min.x & x_count_mask != 0 {
            return Some(min);
        }

        // x overflowed; advance y
        min.x -= x_count_mask + 1;
        min.y += 1 << inner_splits[1];
        let y_count_mask = !(u32::MAX << (inner_splits[1] + splits.y()));
        if min.y & y_count_mask != 0 {
            return Some(min);
        }

        // y overflowed; advance z
        min.y -= y_count_mask + 1;
        min.z += 1 << inner_splits[2];
        let z_count_mask = !(u32::MAX << (inner_splits[2] + splits.z()));
        if min.z & z_count_mask != 0 {
            return Some(min);
        }

        None
    }

    /// Returns the index of these bounds within the given `extent`.
    pub const fn index_within(self, extent: Extent) -> u128 {
        let ([x_bit_width, y_bit_width], [x, y, z]) = self.small_index_helper(extent);
        x as u128 | (y as u128) << x_bit_width | (z as u128) << y_bit_width
    }

    /// Returns the index of these bounds within the given `extent` as a [`u8`].
    ///
    /// If `debug_assertions` are enabled, this panics if the result does not fit in a [`u8`].
    pub fn small_index_within(self, extent: Extent) -> u8 {
        if cfg!(debug_assertions) {
            self.index_within(extent)
                .try_into()
                .expect("OctreeBounds index should fit in a u8")
        } else {
            let ([x_bit_width, y_bit_width], [x, y, z]) = self.small_index_helper(extent);
            x as u8 | (y as u8) << x_bit_width | (z as u8) << y_bit_width
        }
    }

    const fn small_index_helper(self, extent: Extent) -> ([u8; 2], [u32; 3]) {
        let inner_bit_offsets = self.extent.splits();
        let outer_bit_offsets = extent.splits();

        let x_bit_width = outer_bit_offsets[0] - inner_bit_offsets[0];
        let y_bit_width = outer_bit_offsets[1] - inner_bit_offsets[1];
        let z_bit_width = outer_bit_offsets[2] - inner_bit_offsets[2];

        let x = self.min.x >> inner_bit_offsets[0] & !(u32::MAX << x_bit_width);
        let y = self.min.y >> inner_bit_offsets[1] & !(u32::MAX << y_bit_width);
        let z = self.min.z >> inner_bit_offsets[2] & !(u32::MAX << z_bit_width);

        ([x_bit_width, y_bit_width], [x, y, z])
    }

    /// Splits the extent of the bounds according to `splits`, keeping [`Self::min`] as is.
    ///
    /// # Panics
    ///
    /// Panics if [`Self::extent`] cannot be split `splits` times.
    pub fn split_extent(mut self, splits: Splits) -> Self {
        self.extent = self.extent.split(splits);
        self
    }

    pub fn next_bounds_within(self, splits: Splits) -> Option<Self> {
        self.next_min_within(splits).map(|min| Self {
            min,
            extent: self.extent,
        })
    }

    pub fn floor_to_extent(self, extent: Extent) -> Self {
        Self {
            min: Self::floor_min_to_extent(self.min, extent),
            extent,
        }
    }

    pub fn split_to_index(mut self, splits: Splits, index: u8) -> Bounds {
        self.extent = self.extent.split(splits);

        let indices = splits.split_index(index);
        let index_bit_offset = self.extent.splits();

        self.min.x |= (indices[0] as u32) << index_bit_offset[0];
        self.min.y |= (indices[1] as u32) << index_bit_offset[1];
        self.min.z |= (indices[2] as u32) << index_bit_offset[2];

        self
    }
}

impl From<Extent> for Bounds {
    fn from(extent: Extent) -> Self {
        Self::from_extent(extent)
    }
}

use glam::{uvec3, UVec3};

use super::{extent::Extent, splits::Splits};
use crate::math::bounds::UBounds3;

/// The location and size of a [`HexDivNode`](super::HexDivNode).
#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq)]
pub struct Bounds {
    /// The position of the lower corner of the bounds.
    ///
    /// Must be a multiple of [`Self::extent`] and at most [`i32::MAX`].
    min: UVec3,
    /// The extent of the bounds.
    extent: Extent,
}

impl Bounds {
    /// The smallest possible point within a [`HexDivNode`](super::HexDivNode).
    pub const MIN_POINT: UVec3 = UVec3::ZERO;

    /// The biggest possible point in a maximum size [`HexDivNode`](super::HexDivNode).
    pub const MAX_POINT: UVec3 = Extent::MAX.size().wrapping_sub(UVec3::ONE);

    /// Constructs [`Bounds`] with the given `min` and `extent`.
    ///
    /// # Panics
    ///
    /// Panics if the given values do not form [valid bounds](Self::is_valid).
    pub const fn new(min: UVec3, extent: Extent) -> Self {
        assert!(Self::is_valid(min, extent), "invalid bounds");
        Self { min, extent }
    }

    /// Constructs [`Bounds`] with the given `min` and `extent`.
    ///
    /// Returns [`None`] if the given values do not form [valid bounds](Self::is_valid).
    pub const fn checked_new(min: UVec3, extent: Extent) -> Option<Self> {
        if Self::is_valid(min, extent) {
            Some(Self { min, extent })
        } else {
            None
        }
    }

    /// Constructs [`Bounds`] with the given `min` and `extent`.
    ///
    /// This will still panic if `debug_assertions` are enabled.
    pub const fn new_unchecked(min: UVec3, extent: Extent) -> Self {
        debug_assert!(Self::is_valid(min, extent), "invalid bounds");
        Self { min, extent }
    }

    /// Constructs [`Bounds`] with the given `extent` that contain `point`.
    pub fn with_extent_at(extent: Extent, point: UVec3) -> Self {
        Self {
            min: Self::min_with_extent_at(extent, point),
            extent,
        }
    }

    /// Constructs [`Bounds`] with the specified `extent` at the origin.
    ///
    /// This always results in [valid bounds](Self::is_valid), since `0` is a multiple of any
    /// `extent`.
    pub const fn with_extent_at_origin(extent: Extent) -> Self {
        Self {
            min: UVec3::ZERO,
            extent,
        }
    }

    /// Constructs [`Bounds`] of [`Extent::ONE`] at the specified `point`.
    ///
    /// # Panics
    ///
    /// Panics if the given `point` is bigger than [`Self::MAX_POINT`] on any axis.
    pub const fn point(point: UVec3) -> Self {
        Self::new(point, Extent::ONE)
    }

    /// The lower bound (inclusive).
    pub const fn min(self) -> UVec3 {
        self.min
    }

    /// The upper bound (inclusive).
    pub const fn max(self) -> UVec3 {
        // purely using wrapping_add and wrapping_sub for const; it can never actually overflow
        // (assuming the bounds are valid)
        self.min
            .wrapping_add(self.extent.size().wrapping_sub(UVec3::ONE))
    }

    /// The extent of the [`Bounds`].
    pub const fn extent(self) -> Extent {
        self.extent
    }

    /// Whether the [`Bounds`] cover a single point.
    ///
    /// I.e., if [`Self::extent`] is [`Extent::ONE`].
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

    /// Converts the [`Bounds`] into a [`UBounds3`] covering the same area.
    pub const fn to_ubounds3(self) -> UBounds3 {
        UBounds3::with_size_at(self.extent().size(), self.min)
    }

    /// Whether the specified `point` is part of the [`Bounds`].
    pub const fn contains(self, point: UVec3) -> bool {
        let max = self.max();
        (self.min.x <= point.x && point.x <= max.x)
            && (self.min.y <= point.y && point.y <= max.y)
            && (self.min.z <= point.z && point.z <= max.z)
    }

    /// Whether all points of `other` are part of the [`Bounds`].
    pub const fn encloses(self, other: Self) -> bool {
        let self_max = self.max();
        let other_max = other.max();
        (self.min.x <= other.min.x && other_max.x <= self_max.x)
            && (self.min.y <= other.min.y && other_max.y <= self_max.y)
            && (self.min.z <= other.min.z && other_max.z <= self_max.z)
    }

    /// Whether the two [`Bounds`] have any point in common.
    pub const fn overlaps(self, other: Self) -> bool {
        let self_max = self.max();
        let other_max = other.max();
        (self.min.x <= other_max.x && other.min.x <= self_max.x)
            && (self.min.y <= other_max.y && other.min.y <= self_max.y)
            && (self.min.z <= other_max.z && other.min.z <= self_max.z)
    }

    /// Resizes the [`Bounds`] to the given `extent`.
    ///
    /// - If `extent` is smaller, the resulting bounds will cover the first child at that level
    /// - If `extent` is bigger, the resulting bounds will be the parent at that level
    ///
    /// While not really a common use-case, this is done on a per-axis basis (for simplicity). I.e.
    /// one axis can be shrunk to a child node while another axis might be expanded to a parent.
    pub const fn resize(self, extent: Extent) -> Self {
        Self {
            min: Self::min_with_extent_at(extent, self.min),
            extent,
        }
    }

    /// Splits the extent of the bounds according to `splits`, keeping [`Self::min`] the same.
    ///
    /// # Panics
    ///
    /// Panics if [`Self::extent`] cannot be split `splits` times.
    pub fn split_extent(mut self, splits: Splits) -> Self {
        self.extent = self.extent.split(splits);
        self
    }

    /// Expands the [`Bounds`] to its parent using its `parent_splits`.
    ///
    /// This may only be called if `self` is the very first child of the resulting parent.
    ///
    /// This will still panic if `debug_assertions` are enabled.
    pub fn unsplit_first_child_unchecked(self, parent_splits: Splits) -> Self {
        let extent = self.extent.unsplit(parent_splits);
        debug_assert!(
            Self::is_valid(self.min, extent),
            "should be the first child"
        );
        Self {
            min: self.min,
            extent,
        }
    }

    /// Returns the position of the next child based on the given `parent_splits`.
    ///
    /// Children are iterated in `x`, `y`, `z` order with [`None`] indicating the end of iteration.
    ///
    /// Use [`Self::next_min_within`] if you only need the position of the child bounds. The extent
    /// will always match [`Self::extent`], since all children of a parent have the same size.
    pub fn next_bounds_within(self, parent_splits: Splits) -> Option<Self> {
        self.next_min_within(parent_splits).map(|min| Self {
            min,
            extent: self.extent,
        })
    }

    /// Similar to [`Self::next_bounds_within`], but only returns [`Self::min`].
    pub const fn next_min_within(self, parent_splits: Splits) -> Option<UVec3> {
        let inner_splits = self.extent.splits();

        let mut min = self.min;

        // advance x
        min.x += 1 << inner_splits[0];
        let x_count_mask = !(u32::MAX << (inner_splits[0] + parent_splits.x()));
        if min.x & x_count_mask != 0 {
            return Some(min);
        }

        // x overflowed; advance y
        min.x -= x_count_mask + 1;
        min.y += 1 << inner_splits[1];
        let y_count_mask = !(u32::MAX << (inner_splits[1] + parent_splits.y()));
        if min.y & y_count_mask != 0 {
            return Some(min);
        }

        // y overflowed; advance z
        min.y -= y_count_mask + 1;
        min.z += 1 << inner_splits[2];
        let z_count_mask = !(u32::MAX << (inner_splits[2] + parent_splits.z()));
        if min.z & z_count_mask != 0 {
            return Some(min);
        }

        None
    }

    /// Returns the child index based on the given `parent_splits`.
    pub const fn child_index(self, parent_splits: Splits) -> u8 {
        let [x, y, z] = self.child_offset(parent_splits);
        x | y << parent_splits.x() | z << (parent_splits.x() + parent_splits.y())
    }

    /// Returns the `[x, y, z]` offset based on the given `parent_splits`.
    pub const fn child_offset(self, parent_splits: Splits) -> [u8; 3] {
        let bit_offset = self.extent.splits();
        [
            (self.min.x >> bit_offset[0]) as u8 & !(u8::MAX << parent_splits.x()),
            (self.min.y >> bit_offset[1]) as u8 & !(u8::MAX << parent_splits.y()),
            (self.min.z >> bit_offset[2]) as u8 & !(u8::MAX << parent_splits.z()),
        ]
    }

    /// Returns the neighboring child at `index` based on the given `parent_splits`.
    pub const fn with_child_index(self, parent_splits: Splits, index: u8) -> Self {
        self.with_child_offset(parent_splits, parent_splits.split_index(index))
    }

    /// Returns the neighboring child at `[x, y, z]` based on the given `parent_splits`.
    pub const fn with_child_offset(self, parent_splits: Splits, [x, y, z]: [u8; 3]) -> Self {
        assert!(x >> parent_splits.x() == 0, "x out of bounds");
        assert!(y >> parent_splits.y() == 0, "y out of bounds");
        assert!(z >> parent_splits.z() == 0, "z out of bounds");

        let bit_offset = self.extent.splits();
        let x_index_mask = !(!(u32::MAX << parent_splits.x()) << bit_offset[0]);
        let y_index_mask = !(!(u32::MAX << parent_splits.y()) << bit_offset[1]);
        let z_index_mask = !(!(u32::MAX << parent_splits.z()) << bit_offset[2]);
        Self {
            min: uvec3(
                self.min.x & x_index_mask | (x as u32) << bit_offset[0],
                self.min.y & y_index_mask | (y as u32) << bit_offset[1],
                self.min.z & z_index_mask | (z as u32) << bit_offset[2],
            ),
            extent: self.extent,
        }
    }

    /// Whether the given `min` and `extent` would form a valid [`Bounds`].
    ///
    /// [`Bounds`] are valid if `min` is a multiple of `extent`.
    ///
    /// Since [`Extent`] is guaranteed to be all powers of two, this can easily be checked by making
    /// sure the lower bits are set to zero.
    pub const fn is_valid(min: UVec3, extent: Extent) -> bool {
        const MAX: u32 = Bounds::MAX_POINT.x;
        let max = min.saturating_add(extent.size().wrapping_sub(UVec3::ONE));
        let floored = Self::min_with_extent_at(extent, min);
        (max.x <= MAX && max.y <= MAX && max.z <= MAX)
            && (floored.x == min.x && floored.y == min.y && floored.z == min.z)
    }

    /// Returns the [`Self::min`] of the [`Bounds`] with the given `extent` that contain `point`.
    pub const fn min_with_extent_at(extent: Extent, mut point: UVec3) -> UVec3 {
        let bits_to_clear = extent.splits();
        point.x &= u32::MAX << bits_to_clear[0];
        point.y &= u32::MAX << bits_to_clear[1];
        point.z &= u32::MAX << bits_to_clear[2];
        point
    }
}

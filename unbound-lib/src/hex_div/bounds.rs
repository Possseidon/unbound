use std::hash::{Hash, Hasher};

use glam::{uvec3, BVec3, UVec3};

use super::{
    extent::{CachedExtent, Extent, HasCachedExtent, HasExtent, Splittable},
    splits::Splits,
};
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
    pub const fn with_extent_at(extent: Extent, point: UVec3) -> Self {
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
    pub const fn point_uncached(point: UVec3) -> Bounds {
        Bounds::new(point, Extent::ONE_UNCACHED)
    }

    /// Constructs [`CachedBounds`] of [`Extent::ONE`] at the specified `point`.
    ///
    /// # Panics
    ///
    /// Panics if the given `point` is bigger than [`Self::MAX_POINT`] on any axis.
    pub const fn point(point: UVec3) -> CachedBounds {
        CachedBounds::new(point, Extent::ONE)
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
        matches!(self.extent, Extent::ONE_UNCACHED)
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
        let diff = Self::min_with_extent_at(self.extent, point).wrapping_sub(self.min);
        matches!(diff, UVec3::ZERO)
    }

    /// Whether all points of `other` are part of the [`Bounds`].
    pub const fn encloses(self, other: Self) -> bool {
        // const workaround for self.min == other.min
        if let UVec3::ZERO = self.min.wrapping_sub(other.min) {
            let BVec3 { x, y, z } = self.extent.cmpge(other.extent);
            x && y && z
        } else {
            self.contains(other.min)
        }
    }

    /// Whether the two [`Bounds`] have any point in common.
    pub const fn intersects(self, other: Self) -> bool {
        self.contains(other.min) || other.contains(self.min)
    }

    /// Whether the two [`Bounds`] have no point in common.
    pub const fn is_disjoint(self, other: Self) -> bool {
        !self.intersects(other)
    }

    /// Returns the intersection between the two [`Bounds`].
    ///
    /// Returns [`None`] if there is no intersection ([`Bounds`] can never be empty).
    pub const fn intersection(self, other: Self) -> Option<Self> {
        // const workaround for self.min == other.min
        if let UVec3::ZERO = self.min.wrapping_sub(other.min) {
            Some(Self {
                min: self.min,
                extent: self.extent.intersection(other.extent),
            })
        } else if self.contains(other.min) {
            Some(other)
        } else if other.contains(self.min) {
            Some(self)
        } else {
            None
        }
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

    /// Returns the [`Bounds`] of the first child.
    ///
    /// Returns [`None`] if the [`Bounds`] has [`Extent::ONE`], which has no children.
    pub const fn compute_first_child(mut self) -> Option<Self> {
        if let Some(extent) = self.extent.compute_child_extent() {
            self.extent = extent;
            Some(self)
        } else {
            None
        }
    }

    /// Expands the [`Bounds`] to its parent using its `parent_splits`.
    ///
    /// Returns [`None`] if the resulting [`Extent`] exceeds [`Extent::MAX`].
    ///
    /// This may only be called if `self` is the very first child of the resulting parent.
    ///
    /// This will still panic if `debug_assertions` are enabled.
    pub const fn unsplit_first_child_unchecked(
        self,
        parent_splits: Splits,
    ) -> Option<Splittable<CachedBounds>> {
        let Some(extent) = self.extent.parent_extent(parent_splits) else {
            return None;
        };
        let extent = extent.as_inner().strip_cache();
        debug_assert!(
            Self::is_valid(self.min, extent),
            "should be the first child"
        );
        Some(Splittable::new_unchecked_const(CachedBounds {
            bounds: Self { extent, ..self },
            child_splits: parent_splits,
        }))
    }

    /// Returns the position of the next child based on the given `parent_splits`.
    ///
    /// Children are iterated in `x`, `y`, `z` order with [`None`] indicating the end of iteration.
    ///
    /// Use [`Self::next_min_within`] if you only need the position of the child bounds. The extent
    /// will always match [`Self::extent`], since all children of a parent have the same size.
    pub const fn next_bounds_within(self, parent_splits: Splits) -> Option<Self> {
        if let Some(min) = self.next_min_within(parent_splits) {
            Some(Self {
                min,
                extent: self.extent,
            })
        } else {
            None
        }
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
        x | (y << parent_splits.x()) | (z << (parent_splits.x() + parent_splits.y()))
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
                self.min.x & x_index_mask | ((x as u32) << bit_offset[0]),
                self.min.y & y_index_mask | ((y as u32) << bit_offset[1]),
                self.min.z & z_index_mask | ((z as u32) << bit_offset[2]),
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

    /// Updates the bounds with the given `extent`.
    pub const fn with_extent_unchecked(self, extent: Extent) -> Self {
        debug_assert!(Self::is_valid(self.min, extent));
        Self { extent, ..self }
    }

    /// Converts the [`Bounds`] into a [`CachedBounds`] which caches the number of child splits.
    pub const fn compute_cache(self) -> CachedBounds {
        CachedBounds {
            bounds: self,
            child_splits: Splits::compute(self.extent()),
        }
    }

    /// Converts the [`Bounds`] into a [`CachedBounds`] with the given number of `child_splits`.
    pub const fn with_cache_unchecked(self, child_splits: Splits) -> CachedBounds {
        debug_assert!(child_splits.to_u32() == Splits::compute(self.extent()).to_u32());
        CachedBounds {
            bounds: self,
            child_splits,
        }
    }
}

impl HasExtent for Bounds {
    fn extent(&self) -> Extent {
        (*self).extent()
    }
}

/// Contains both a node's [`Bounds`] as well as the cached number of [`Splits`].
#[derive(Clone, Copy, Debug, Default)]
pub struct CachedBounds {
    /// The [`Bounds`] of the [`HexDivNode`].
    bounds: Bounds,
    /// The number of child nodes.
    ///
    /// [`Splits::NONE`] iff the [`Extent`] of [`Self::bounds`] is [`Extent::ONE`].
    child_splits: Splits,
}

impl CachedBounds {
    /// Returns a regular [`Bounds`] without the cache.
    pub const fn strip_cache(self) -> Bounds {
        self.bounds
    }

    /// Returns the cached number of child [`Splits`].
    pub const fn child_splits(self) -> Splits {
        self.child_splits
    }

    /// [`Bounds::new`] but taking a [`CachedExtent`].
    pub const fn new(min: UVec3, extent: CachedExtent) -> Self {
        Self {
            bounds: Bounds::new(min, extent.strip_cache()),
            child_splits: extent.child_splits(),
        }
    }

    /// [`Bounds::checked_new`] but taking a [`CachedExtent`].
    pub const fn checked_new(min: UVec3, extent: CachedExtent) -> Option<Self> {
        let Some(bounds) = Bounds::checked_new(min, extent.strip_cache()) else {
            return None;
        };
        Some(Self {
            bounds,
            child_splits: extent.child_splits(),
        })
    }

    /// [`Bounds::new_unchecked`] but taking a [`CachedExtent`].
    pub const fn new_unchecked(min: UVec3, extent: CachedExtent) -> Self {
        Self {
            bounds: Bounds::new_unchecked(min, extent.strip_cache()),
            child_splits: extent.child_splits(),
        }
    }

    /// [`Bounds::with_extent_at`] but taking a [`CachedExtent`].
    pub const fn with_extent_at(extent: CachedExtent, point: UVec3) -> Self {
        Self {
            bounds: Bounds::with_extent_at(extent.strip_cache(), point),
            child_splits: extent.child_splits(),
        }
    }

    /// [`Bounds::with_extent_at_origin`] but taking a [`CachedExtent`].
    pub const fn with_extent_at_origin(extent: CachedExtent) -> Self {
        Self {
            bounds: Bounds::with_extent_at_origin(extent.strip_cache()),
            child_splits: extent.child_splits(),
        }
    }

    /// Delegates to [`Bounds::min`].
    pub const fn min(self) -> UVec3 {
        self.bounds.min()
    }

    /// Delegates to [`Bounds::max`].
    pub const fn max(self) -> UVec3 {
        self.bounds.max()
    }

    /// [`Bounds::extent`] but returning a [`CachedExtent`].
    pub const fn extent(self) -> CachedExtent {
        CachedExtent::from_cached_bounds(self)
    }

    /// Delegates to [`Bounds::extent`].
    pub const fn extent_uncached(self) -> Extent {
        self.bounds.extent()
    }

    /// Delegates to [`Bounds::is_point`].
    pub const fn is_point(self) -> bool {
        self.bounds.is_point()
    }

    /// Delegates to [`Bounds::to_point`].
    pub const fn to_point(self) -> Option<UVec3> {
        self.bounds.to_point()
    }

    /// Delegates to [`Bounds::to_ubounds3`].
    pub const fn to_ubounds3(self) -> UBounds3 {
        self.bounds.to_ubounds3()
    }

    /// Delegates to [`Bounds::contains`].
    pub const fn contains(self, point: UVec3) -> bool {
        self.bounds.contains(point)
    }

    /// Delegates to [`Bounds::encloses`].
    pub const fn encloses(self, other: Bounds) -> bool {
        self.bounds.encloses(other)
    }

    /// Delegates to [`Bounds::intersects`].
    pub const fn intersects(self, other: Bounds) -> bool {
        self.bounds.intersects(other)
    }

    /// Delegates to [`Bounds::is_disjoint`].
    pub const fn is_disjoint(self, other: Bounds) -> bool {
        self.bounds.is_disjoint(other)
    }

    /// Delegates to [`Bounds::intersection`].
    pub const fn intersection(self, other: Bounds) -> Option<Bounds> {
        self.bounds.intersection(other)
    }

    /// [`Bounds::resize`] but taking a [`CachedExtent`] to also update the cached child [`Splits`].
    pub const fn resize(self, extent: CachedExtent) -> Self {
        Self {
            bounds: self.bounds.resize(extent.strip_cache()),
            child_splits: extent.child_splits(),
        }
    }

    /// Delegates to [`Bounds::unsplit_first_child_unchecked`].
    pub const fn unsplit_first_child_unchecked(
        self,
        parent_splits: Splits,
    ) -> Option<Splittable<Self>> {
        self.bounds.unsplit_first_child_unchecked(parent_splits)
    }

    /// Delegates to [`Bounds::next_bounds_within`].
    ///
    /// Copies over the cache, since the extent remains unchanged.
    pub const fn next_bounds_within(self, parent_splits: Splits) -> Option<Self> {
        if let Some(bounds) = self.bounds.next_bounds_within(parent_splits) {
            Some(Self { bounds, ..self })
        } else {
            None
        }
    }

    /// Delegates to [`Bounds::next_min_within`].
    pub const fn next_min_within(self, parent_splits: Splits) -> Option<UVec3> {
        self.bounds.next_min_within(parent_splits)
    }

    /// Delegates to [`Bounds::child_index`].
    pub const fn child_index(self, parent_splits: Splits) -> u8 {
        self.bounds.child_index(parent_splits)
    }

    /// Delegates to [`Bounds::child_offset`].
    pub const fn child_offset(self, parent_splits: Splits) -> [u8; 3] {
        self.bounds.child_offset(parent_splits)
    }

    /// Delegates to [`Bounds::with_child_index`].
    ///
    /// Copies over the cache, since the extent remains unchanged.
    pub const fn with_child_index(self, parent_splits: Splits, index: u8) -> Self {
        let bounds = self.bounds.with_child_index(parent_splits, index);
        Self { bounds, ..self }
    }

    /// Delegates to [`Bounds::with_child_offset`].
    ///
    /// Copies over the cache, since the extent remains unchanged.
    pub const fn with_child_offset(self, parent_splits: Splits, offset: [u8; 3]) -> Self {
        let bounds = self.bounds.with_child_offset(parent_splits, offset);
        Self { bounds, ..self }
    }

    /// [`Bounds::with_extent_unchecked`] but taking a [`CachedExtent`] to also update the child
    /// [`Splits`].
    pub const fn with_extent_unchecked(self, extent: CachedExtent) -> Self {
        Self {
            bounds: self.bounds.with_extent_unchecked(extent.strip_cache()),
            child_splits: extent.child_splits(),
        }
    }

    /// An optimized version of [`Bounds::compute_first_child`].
    ///
    /// This does not require calculating the number of child [`Splits`], since it is cached.
    pub const fn first_child(self) -> Option<Bounds> {
        let extent = self.bounds.extent();
        if let Extent::ONE_UNCACHED = extent {
            return None;
        }
        let Some(child_extent) = extent
            .with_cache_unchecked(self.child_splits)
            .child_extent()
        else {
            return None;
        };
        Some(Bounds {
            extent: child_extent,
            min: self.bounds.min,
        })
    }

    /// Returns a specific child [`Bounds`] by `index`.
    ///
    /// Returns [`None`] if the [`CachedBounds`] has no children.
    pub const fn child(self, index: u8) -> Option<Bounds> {
        let Some(first_child) = self.first_child() else {
            return None;
        };
        Some(first_child.with_child_index(self.child_splits, index))
    }
}

impl Splittable<CachedBounds> {
    /// Fowards [`Splittable`] from [`CachedBounds`] to [`CachedExtent`].
    pub const fn extent(self) -> Splittable<CachedExtent> {
        Splittable::new_unchecked_const((*self.as_inner()).extent())
    }
}

impl Hash for CachedBounds {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.bounds.hash(state);
        // intentionally ignore splits
    }
}

impl PartialEq for CachedBounds {
    fn eq(&self, other: &Self) -> bool {
        self.bounds == other.bounds
        // intentionally ignore splits
    }
}

impl Eq for CachedBounds {}

impl HasExtent for CachedBounds {
    fn extent(&self) -> Extent {
        self.extent_uncached()
    }
}

impl HasCachedExtent for CachedBounds {
    fn cached_extent(&self) -> CachedExtent {
        (*self).extent()
    }
}

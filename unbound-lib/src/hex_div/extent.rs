use std::{
    hash::{Hash, Hasher},
    num::{NonZeroU128, NonZeroU8},
    ops::Deref,
};

use glam::{bvec3, uvec3, BVec3, UVec3};

use super::{bounds::CachedBounds, splits::Splits};

/// The extent of a [`HexDivNode`](super::HexDivNode).
///
/// The length along each individual axis is always a power of two and also stored as such in log2.
#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq)]
pub struct Extent {
    x: u8,
    y: u8,
    z: u8,
}

impl Extent {
    /// Uncached version of [`Extent::ONE`].
    pub const ONE_UNCACHED: Self = Self { x: 0, y: 0, z: 0 };

    /// `1x1x1`, the smallest possible [`Extent`].
    pub const ONE: CachedExtent = CachedExtent {
        extent: Self::ONE_UNCACHED,
        child_splits: Splits::NONE,
    };

    /// Uncached version of [`Extent::MAX`].
    pub const MAX_UNCACHED: Self = Self {
        x: Self::MAX_SPLITS,
        y: Self::MAX_SPLITS,
        z: Self::MAX_SPLITS,
    };

    /// The largest possible size of a [`HexDivNode`](super::HexDivNode).
    pub const MAX: CachedExtent = Self::MAX_UNCACHED.compute_cache();

    /// The maximum number of splits along each axis.
    pub const MAX_SPLITS: u8 = 31;

    /// Constructs an [`Extent`] with the given number of `splits`.
    ///
    /// Returns [`None`] if any number of splits exceeds [`Self::MAX_SPLITS`].
    pub const fn from_splits([x, y, z]: [u8; 3]) -> Option<Self> {
        if x <= Self::MAX_SPLITS && y <= Self::MAX_SPLITS && z <= Self::MAX_SPLITS {
            Some(Self { x, y, z })
        } else {
            None
        }
    }

    /// Constructs the smallest possible [`Extent`] that is at least `size` big.
    ///
    /// Returns [`None`] if any side length of `size` is `0` or requires more splits than
    /// [`Extent::MAX_SPLITS`].
    pub const fn ceil_from_size(UVec3 { x, y, z }: UVec3) -> Option<Self> {
        const fn ceil_log2(length: u32) -> Option<u8> {
            let result = 32u32 - length.wrapping_sub(1).leading_zeros();
            if result <= Extent::MAX_SPLITS as u32 {
                Some(result as u8)
            } else {
                None
            }
        }

        if let (Some(x), Some(y), Some(z)) = (ceil_log2(x), ceil_log2(y), ceil_log2(z)) {
            Some(Self { x, y, z })
        } else {
            None
        }
    }

    /// Returns the number of times the [`Extent`] can be split along the different axes.
    pub const fn splits(self) -> [u8; 3] {
        [self.x, self.y, self.z]
    }

    /// Returns the total number of times that this [`Extent`] can be split along all axes.
    ///
    /// I.e. the sum of [`Self::splits`].
    pub const fn total_splits(self) -> u8 {
        self.x + self.y + self.z
    }

    /// Returns the width, height and depth of this [`Extent`].
    pub const fn size(self) -> UVec3 {
        uvec3(1 << self.x, 1 << self.y, 1 << self.z)
    }

    /// Returns the volume of the [`Extent`].
    ///
    /// Calculated very efficiently by just summing the total number of splits and a bitshift.
    pub const fn volume(self) -> NonZeroU128 {
        NonZeroU128::new(1 << self.total_splits()).expect("total_splits should not exceed 93")
    }

    /// Returns the number of full [`Splits`] and number of leftover splits in the outer [`Splits`].
    pub const fn full_splits_and_rest(self) -> (u8, u8) {
        let total_splits = self.total_splits();
        (
            total_splits / Splits::MAX_TOTAL.get(),
            total_splits % Splits::MAX_TOTAL.get(),
        )
    }

    /// Returns the total number of child splits.
    ///
    /// Returns [`None`] if the [`Extent`] is [`Extent::ONE`] and cannot be split.
    pub const fn total_child_splits(self) -> Option<NonZeroU8> {
        if let Extent::ONE_UNCACHED = self {
            return None;
        }
        let Some(splits) = NonZeroU8::new(self.total_splits() % Splits::MAX_TOTAL.get()) else {
            return Some(Splits::MAX_TOTAL);
        };
        Some(splits)
    }

    /// Computes the [`Extent`] of child nodes.
    pub const fn compute_child_extent(mut self) -> Option<Self> {
        let Some(total_child_splits) = self.total_child_splits() else {
            return None;
        };
        let mut total_child_splits = total_child_splits.get();

        let [low, mid, high] = self.splits_sorted_mut();

        let low_diff = *mid - *low;
        let high_diff = *high - *mid;

        let cut_top = min_u8_const(high_diff, total_child_splits);
        *high -= cut_top;
        total_child_splits -= cut_top;

        let cut_mid = min_u8_const(low_diff, total_child_splits / 2);
        *mid -= cut_mid;
        *high -= cut_mid;
        total_child_splits -= cut_mid * 2;

        let cut_all = total_child_splits / 3;
        let extra = total_child_splits % 3;
        *low -= cut_all;
        *mid -= cut_all + (extra > 1) as u8;
        *high -= cut_all + (extra > 0) as u8;

        Some(self)
    }

    /// Returns the parent [`Extent`] based on the given number of `splits`.
    ///
    /// Returns [`None`] if the new [`Extent`] exceeds [`Self::MAX`] along any axis.
    pub const fn parent_extent(self, splits: Splits) -> Option<Splittable<CachedExtent>> {
        let Some(extent) = Self::from_splits([
            self.x + splits.x(),
            self.y + splits.y(),
            self.z + splits.z(),
        ]) else {
            return None;
        };
        Some(Splittable::new_unchecked_const(CachedExtent {
            extent,
            child_splits: splits,
        }))
    }

    /// Returns the intersection of the two [`Extent`]s.
    pub const fn intersection(self, other: Self) -> Self {
        Self {
            x: min_u8_const(self.x, other.x),
            y: min_u8_const(self.y, other.y),
            z: min_u8_const(self.z, other.z),
        }
    }

    /// `self == other` per axis.
    pub const fn cmpeq(self, other: Self) -> BVec3 {
        bvec3(self.x == other.x, self.y == other.y, self.z == other.z)
    }

    /// `self != other` per axis.
    pub const fn cmpne(self, other: Self) -> BVec3 {
        bvec3(self.x != other.x, self.y != other.y, self.z != other.z)
    }

    /// `self < other` per axis.
    pub const fn cmplt(self, other: Self) -> BVec3 {
        bvec3(self.x < other.x, self.y < other.y, self.z < other.z)
    }

    /// `self <= other` per axis.
    pub const fn cmple(self, other: Self) -> BVec3 {
        bvec3(self.x <= other.x, self.y <= other.y, self.z <= other.z)
    }

    /// `self > other` per axis.
    pub const fn cmpgt(self, other: Self) -> BVec3 {
        bvec3(self.x > other.x, self.y > other.y, self.z > other.z)
    }

    /// `self >= other` per axis.
    pub const fn cmpge(self, other: Self) -> BVec3 {
        bvec3(self.x >= other.x, self.y >= other.y, self.z >= other.z)
    }

    /// Converts the [`Extent`] into a [`CachedExtent`] which caches the number of child splits.
    pub const fn compute_cache(self) -> CachedExtent {
        CachedExtent {
            extent: self,
            child_splits: Splits::compute(self),
        }
    }

    /// Converts the [`Extent`] into a [`CachedExtent`] with the given number of `child_splits`.
    pub const fn with_cache_unchecked(self, child_splits: Splits) -> CachedExtent {
        debug_assert!(child_splits.to_u32() == Splits::compute(self).to_u32());
        CachedExtent {
            extent: self,
            child_splits,
        }
    }

    /// Returns mutable references to the number of splits in ascending order.
    const fn splits_sorted_mut(&mut self) -> [&mut u8; 3] {
        let [mut low, mut mid, mut high] = [&mut self.x, &mut self.y, &mut self.z];
        if *low > *mid {
            [low, mid] = [mid, low];
        }
        if *low > *high {
            [low, high] = [high, low];
        }
        if *mid > *high {
            [mid, high] = [high, mid];
        }
        [low, mid, high]
    }
}

impl PartialEq<CachedExtent> for Extent {
    fn eq(&self, other: &CachedExtent) -> bool {
        *self == other.strip_cache()
    }
}

impl HasExtent for Extent {
    fn extent(&self) -> Extent {
        *self
    }
}

impl From<ExtentCompact> for Extent {
    fn from(value: ExtentCompact) -> Self {
        Self {
            x: ((value.splits >> 10) & 0x1F) as u8,
            y: ((value.splits >> 5) & 0x1F) as u8,
            z: (value.splits & 0x1F) as u8,
        }
    }
}

/// Contains both a node's [`Extent`] as well as the cached number of [`Splits`].
///
/// While it would have been quite convenient for this type to implement [`Deref<Target = Extent>`],
/// it intentionally does not. Reason being, that it makes it possible to call functions for which
/// [`CachedExtent`] provides better alternatives.
#[derive(Clone, Copy, Debug, Default)]
pub struct CachedExtent {
    /// The extent of the node.
    extent: Extent,
    /// The number of child nodes.
    ///
    /// [`Splits::NONE`] iff [`Self::extent`] is [`Extent::ONE`].
    child_splits: Splits,
}

impl CachedExtent {
    /// Returns a regular [`Extent`] without the cache.
    pub const fn strip_cache(self) -> Extent {
        self.extent
    }

    /// Returns the cached number of child [`Splits`].
    pub const fn child_splits(self) -> Splits {
        self.child_splits
    }

    /// Delegates to [`Extent::splits`].
    pub const fn splits(self) -> [u8; 3] {
        self.extent.splits()
    }

    /// Delegates to [`Extent::total_splits`].
    pub const fn total_splits(self) -> u8 {
        self.extent.total_splits()
    }

    /// Delegates to [`Extent::size`].
    pub const fn size(self) -> UVec3 {
        self.extent.size()
    }

    /// Delegates to [`Extent::volume`].
    pub const fn volume(self) -> NonZeroU128 {
        self.extent.volume()
    }

    /// Delegates to [`Extent::full_splits_and_rest`].
    pub const fn full_splits_and_rest(self) -> (u8, u8) {
        self.extent.full_splits_and_rest()
    }

    /// An optimized version of [`Extent::total_child_splits`].
    ///
    /// Unlike [`Extent::compute_child_extent`], [`Extent::total_child_splits`] is already decently
    /// fast, but it's still better to just sum the cached child [`Splits`].
    pub const fn total_child_splits(self) -> Option<NonZeroU8> {
        NonZeroU8::new(self.child_splits.total())
    }

    /// An optimized version of [`Extent::compute_child_extent`].
    ///
    /// This just has to subtract the cached number of child [`Splits`] from the [`Extent`]. This
    /// function is the main reason as for why [`CachedExtent`] exists.
    pub const fn child_extent(self) -> Option<Extent> {
        if let Extent::ONE_UNCACHED = self.extent {
            None
        } else {
            Some(Splittable::new_unchecked_const(self).child_extent())
        }
    }

    /// Delegates to [`Extent::parent_extent`].
    pub const fn parent_extent(self, splits: Splits) -> Option<Splittable<CachedExtent>> {
        self.extent.parent_extent(splits)
    }

    /// Delegates to [`Extent::intersection`].
    ///
    /// Note, that the resulting [`Extent`] is no loner cached.
    pub const fn intersection(self, other: Self) -> Extent {
        self.extent.intersection(other.extent)
    }

    /// Delegates to [`Extent::cmpeq`].
    pub const fn cmpeq(self, other: Self) -> BVec3 {
        self.extent.cmpeq(other.extent)
    }

    /// Delegates to [`Extent::cmpne`].
    pub const fn cmpne(self, other: Self) -> BVec3 {
        self.extent.cmpne(other.extent)
    }

    /// Delegates to [`Extent::cmplt`].
    pub const fn cmplt(self, other: Self) -> BVec3 {
        self.extent.cmplt(other.extent)
    }

    /// Delegates to [`Extent::cmple`].
    pub const fn cmple(self, other: Self) -> BVec3 {
        self.extent.cmple(other.extent)
    }

    /// Delegates to [`Extent::cmpgt`].
    pub const fn cmpgt(self, other: Self) -> BVec3 {
        self.extent.cmpgt(other.extent)
    }

    /// Delegates to [`Extent::cmpge`].
    pub const fn cmpge(self, other: Self) -> BVec3 {
        self.extent.cmpge(other.extent)
    }

    /// Takes the cached number of child [`Splits`] from the `bounds` but strips its position.
    pub const fn from_cached_bounds(bounds: CachedBounds) -> Self {
        Self {
            extent: bounds.strip_cache().extent(),
            child_splits: bounds.child_splits(),
        }
    }
}

impl Hash for CachedExtent {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.extent.hash(state);
        // intentionally ignore splits
    }
}

impl PartialEq for CachedExtent {
    fn eq(&self, other: &Self) -> bool {
        self.extent == other.extent
        // intentionally ignore splits
    }
}

impl PartialEq<Extent> for CachedExtent {
    fn eq(&self, other: &Extent) -> bool {
        self.strip_cache() == *other
    }
}

impl Eq for CachedExtent {}

impl HasExtent for CachedExtent {
    fn extent(&self) -> Extent {
        self.extent
    }
}

impl Splittable<CachedExtent> {
    /// Updates the signature of [`CachedExtent::child_extent`], since it can never return [`None`].
    pub const fn child_extent(self) -> Extent {
        Extent {
            x: self.0.extent.x - self.0.child_splits.x(),
            y: self.0.extent.y - self.0.child_splits.y(),
            z: self.0.extent.z - self.0.child_splits.z(),
        }
    }
}

/// A type that is known to be splittable due to not having an [`Extent`] of [`Extent::ONE`].
///
/// Despite the name, this does not provide functionality to actually _do_ any concrete splitting.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Splittable<T>(T);

impl<T> Splittable<T> {
    /// Workaround for [`Deref`] in `const` contexts.
    pub const fn as_inner(&self) -> &T {
        &self.0
    }

    /// [`Self::new_unchecked`] that can be used in `const` contexts.
    ///
    /// Take care; this does not even check when `debug_assertions` are enabled, since it cannot
    /// call [`HasExtent::is_splittable`].
    pub const fn new_unchecked_const(has_extent: T) -> Self {
        Self(has_extent)
    }
}

impl<T: HasExtent> Splittable<T> {
    /// Wraps the given `has_extent` in [`Splittable`].
    ///
    /// Returns [`None`] if `has_extent` is not [splittable](HasExtent::is_splittable).
    pub fn new(has_extent: T) -> Option<Self> {
        has_extent.is_splittable().then_some(Self(has_extent))
    }

    /// Wraps the given `has_extent` in [`Splittable`], assuming it is not [`Extent::ONE`].
    pub fn new_unchecked(has_extent: T) -> Self {
        debug_assert!(has_extent.is_splittable());
        Self(has_extent)
    }
}

impl<T> Deref for Splittable<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/// A type that has a queryable [`Extent`].
pub trait HasExtent {
    /// Returns the [`Extent`].
    fn extent(&self) -> Extent;

    /// Returns whether [`HasExtent::extent`] can be split; i.e. is not [`Extent::ONE`].
    fn is_splittable(&self) -> bool {
        self.extent() != Extent::ONE_UNCACHED
    }
}

/// Extension on top of [`HasExtent`] for types that store a [`CachedExtent`].
pub trait HasCachedExtent: HasExtent {
    /// Returns the [`CachedExtent`].
    fn cached_extent(&self) -> CachedExtent;
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
            splits: ((value.x as u16) << 10) | ((value.y as u16) << 5) | (value.z as u16),
        }
    }
}

impl std::fmt::Debug for ExtentCompact {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let Extent { x, y, z } = Extent::from(*self);
        f.debug_struct("ExtentCompact")
            .field("x", &x)
            .field("y", &y)
            .field("z", &z)
            .finish()
    }
}

const fn min_u8_const(a: u8, b: u8) -> u8 {
    if a < b {
        a
    } else {
        b
    }
}

#[cfg(test)]
mod tests {
    use itertools::iproduct;

    use super::*;

    #[test]
    fn extent_one_compute_child_extent() {
        assert_eq!(Extent::ONE_UNCACHED.compute_child_extent(), None);
    }

    #[test]
    fn extent_compute_child_extent() {
        for extent in iproduct!(
            0..=Extent::MAX_SPLITS,
            0..=Extent::MAX_SPLITS,
            0..=Extent::MAX_SPLITS,
        )
        .skip(1)
        .map(|(x, y, z)| Extent::from_splits([x, y, z]).unwrap())
        {
            let child_extent = extent.compute_child_extent().unwrap();
            let child_splits = extent.total_child_splits().unwrap().get();
            assert_eq!(
                child_extent.total_splits(),
                extent.total_splits() - child_splits
            );
        }
    }
}

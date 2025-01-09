use std::ops::{Add, AddAssign, Sub, SubAssign};

use glam::UVec3;

/// Unsigned integer axis-aligned bounds in 3D-space.
#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq)]
pub struct UBounds3 {
    /// The inclusive lower limit of the bounds.
    lower: UVec3,
    /// The exclusive upper limit of the bounds.
    upper: UVec3,
}

impl UBounds3 {
    /// Constructs [`UBounds3`] from the given inclusive `lower` and exclusive `upper`.
    ///
    /// # Panics
    ///
    /// Panics if `lower` exceeds `upper` along any axis.
    pub const fn new(lower: UVec3, upper: UVec3) -> Self {
        if let Some(bounds) = Self::checked_new(lower, upper) {
            bounds
        } else {
            panic!("lower bounds must not exceed upper bounds");
        }
    }

    /// Constructs [`UBounds3`] from the given inclusive `lower` and exclusive `upper`.
    ///
    /// Returns [`None`] if `lower` exceeds `upper` along any axis.
    pub const fn checked_new(lower: UVec3, upper: UVec3) -> Option<Self> {
        if lower.x <= upper.x && lower.y <= upper.y && lower.z <= upper.z {
            Some(Self { lower, upper })
        } else {
            None
        }
    }

    /// Constructs [`UBounds3`] with the given `size` located at the origin.
    pub const fn with_size_at_origin(size: UVec3) -> Self {
        Self {
            lower: UVec3::ZERO,
            upper: size,
        }
    }

    /// Constructs [`UBounds3`] with the given `size` and `lower` point.
    ///
    /// # Panics
    ///
    /// Panics if the resulting upper bounds exceeds [`UVec3::MAX`] on any axis.
    pub const fn with_size_at(size: UVec3, lower: UVec3) -> Self {
        if let Some(bounds) = Self::checked_new(lower, lower.wrapping_add(size)) {
            bounds
        } else {
            panic!("upper bounds must not exceed UVec3::MAX");
        }
    }

    /// Constructs new [`UBounds3`] covering the single given `point`.
    ///
    /// Note, that this does not return empty bounds at the given `point` but instead bounds with a
    /// size of `1x1x1`.
    ///
    /// # Panics
    ///
    /// Panics if the `point` is [`UVec3::MAX`] on any axis.
    pub const fn point(point: UVec3) -> Self {
        if let Some(bounds) = Self::checked_new(point, point.wrapping_add(UVec3::ONE)) {
            bounds
        } else {
            panic!("point must be less than UVec3::MAX");
        }
    }

    /// The inclusive lower limit of the bounds.
    pub const fn lower(self) -> UVec3 {
        self.lower
    }

    /// The exclusive upper limit of the bounds.
    pub const fn upper(self) -> UVec3 {
        self.upper
    }

    /// The size of the bounds.
    pub const fn size(self) -> UVec3 {
        // only using wrapping_sub for const, can never overflow
        self.upper.wrapping_sub(self.lower)
    }

    /// Whether the [`UBounds3`] are empty along _any_ axis.
    ///
    /// I.e., not only `0x0x0` but also e.g. `0x1x2` is considered "empty".
    pub fn is_empty(self) -> bool {
        self.lower.cmpeq(self.upper).any()
    }

    /// Whether the
    pub const fn contains(self, point: UVec3) -> bool {
        (self.lower.x <= point.x && point.x < self.upper.x)
            && (self.lower.y <= point.y && point.y < self.upper.y)
            && (self.lower.z <= point.z && point.z < self.upper.z)
    }

    pub const fn overlaps(self, other: Self) -> bool {
        (self.lower.x < other.upper.x && other.lower.x < self.upper.x)
            && (self.lower.y < other.upper.y && other.lower.y < self.upper.y)
            && (self.lower.z < other.upper.z && other.lower.z < self.upper.z)
    }

    pub const fn is_disjoint(self, other: Self) -> bool {
        !self.overlaps(other)
    }

    pub const fn encloses(self, other: Self) -> bool {
        (self.lower.x <= other.lower.x && other.upper.x <= self.upper.x)
            && (self.lower.y <= other.lower.y && other.upper.y <= self.upper.y)
            && (self.lower.z <= other.lower.z && other.upper.z <= self.upper.z)
    }

    /// Returns the lower and upper bounds of `self` clamped to `within`.
    ///
    /// I.e. if `within` encloses `self`, `self` is returned unchanged. Otherwise `self` is cut off
    /// so that `within` does enclose it.
    ///
    /// If `within` is disjoint from `self`, the clamped bounds will be empty but lie on the
    /// boundary of `within` closest to `self`.
    pub fn clamp(self, within: Self) -> Self {
        Self {
            lower: self.lower.clamp(within.lower, within.upper),
            upper: self.upper.clamp(within.lower, within.upper),
        }
    }
}

impl AddAssign<UVec3> for UBounds3 {
    fn add_assign(&mut self, rhs: UVec3) {
        self.lower += rhs;
        self.upper += rhs;
    }
}

impl Add<UVec3> for UBounds3 {
    type Output = Self;

    fn add(mut self, rhs: UVec3) -> Self {
        self += rhs;
        self
    }
}

impl SubAssign<UVec3> for UBounds3 {
    fn sub_assign(&mut self, rhs: UVec3) {
        self.lower -= rhs;
        self.upper -= rhs;
    }
}

impl Sub<UVec3> for UBounds3 {
    type Output = Self;

    fn sub(mut self, rhs: UVec3) -> Self {
        self -= rhs;
        self
    }
}

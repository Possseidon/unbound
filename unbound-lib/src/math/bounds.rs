use glam::{IVec3, UVec3};

/// Unsigned integer axis-aligned bounds in 3D-space.
#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq)]
pub struct UBounds3 {
    /// The inclusive lower limit of the bounds.
    lower: UVec3,
    /// The exclusive upper limit of the bounds.
    upper: UVec3,
}

impl UBounds3 {
    pub const ZERO: Self = Self::with_extent_at_origin(UVec3::ZERO);
    pub const UNIT: Self = Self::with_extent_at_origin(UVec3::ONE);
    pub const FULL: Self = Self::new(UVec3::MIN, UVec3::MAX);

    pub const fn checked_new(lower: UVec3, upper: UVec3) -> Option<Self> {
        if lower.x <= upper.x && lower.y <= upper.y && lower.z <= upper.z {
            Some(Self { lower, upper })
        } else {
            None
        }
    }

    pub const fn new(lower: UVec3, upper: UVec3) -> Self {
        if let Some(bounds) = Self::checked_new(lower, upper) {
            bounds
        } else {
            panic!("lower bounds must not exceed upper bounds");
        }
    }

    pub const fn with_extent_at_origin(extent: UVec3) -> Self {
        Self {
            lower: UVec3::ZERO,
            upper: extent,
        }
    }

    pub const fn with_extent_at(offset: UVec3, extent: UVec3) -> Self {
        if let Some(bounds) = Self::checked_new(offset, offset.wrapping_add(extent)) {
            bounds
        } else {
            panic!("upper bounds must not exceed UVec3::MAX");
        }
    }

    pub const fn from_point(point: UVec3) -> Self {
        if let Some(bounds) = Self::checked_new(point, UVec3::ONE) {
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

    /// The extent/size of the bounds.
    pub const fn extent(self) -> UVec3 {
        // only using wrapping_sub for const, can never overflow
        self.upper.wrapping_sub(self.lower)
    }

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
}

/// Signed integer axis-aligned bounds in 3D-space.
#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq)]
pub struct IBounds3 {
    lower: IVec3,
    upper: IVec3,
}

impl IBounds3 {
    // TODO: Copy UBounds3 interface
}

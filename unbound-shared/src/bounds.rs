use glam::{ivec3, uvec3, IVec3, UVec3};

/// Integer axis-aligned bounds in 3D-space defined by a lower and upper bound.
///
/// [`Self::lower`] is guaranteed to be less than or equal to [`Self::upper`].
#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq)]
pub struct IBounds3 {
    /// The lower bound of the cube (inclusive).
    lower: IVec3,
    /// The upper bound of the cube (exclusive).
    upper: IVec3,
}

impl IBounds3 {
    /// Empty bounds, located at the origin.
    pub const EMPTY: Self = Self {
        lower: IVec3::ZERO,
        upper: IVec3::ZERO,
    };

    /// Constructs an [`IBounds3`] from `lower` (inclusive) and `upper` (exclusive).
    ///
    /// Returns [`None`] if any component of `upper` is less than `lower`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use glam::ivec3;
    /// # use unbound_shared::bounds::IBounds3;
    /// let lower = ivec3(0, 0, 0);
    /// let upper = ivec3(1, 1, 1);
    /// let bounds = IBounds3::new(lower, upper);
    /// assert!(bounds.is_some());
    /// ```
    pub const fn new(lower: IVec3, upper: IVec3) -> Option<Self> {
        if lower.x <= upper.x && lower.y <= upper.y && lower.z <= upper.z {
            Some(Self { lower, upper })
        } else {
            None
        }
    }

    /// Constructs an [`IBounds3`] containing only the given `point`.
    ///
    /// Returns [`None`] if any component of the `point` is [`i32::MAX`], since [`IBounds3`] stores
    /// the upper bound exclusively.
    pub const fn from_point(point: IVec3) -> Option<Self> {
        if let (Some(x), Some(y), Some(z)) = (
            point.x.checked_add(1),
            point.y.checked_add(1),
            point.z.checked_add(1),
        ) {
            Some(Self {
                lower: point,
                upper: ivec3(x, y, z),
            })
        } else {
            None
        }
    }

    /// The inclusive lower bounds of this [`IBounds3`].
    pub const fn lower(self) -> IVec3 {
        self.lower
    }

    /// The exclusive upper bounds of this [`IBounds3`].
    pub const fn upper(self) -> IVec3 {
        self.upper
    }

    /// The extent (width, height and depth) of the bounds.
    pub const fn extent(self) -> UVec3 {
        let size = self.upper.wrapping_sub(self.lower);
        uvec3(size.x as u32, size.y as u32, size.z as u32)
    }

    /// Whether the specified `point` is part of this [`IBounds3`].
    pub const fn contains(self, point: IVec3) -> bool {
        (self.lower.x <= point.x && point.x < self.upper.x)
            && (self.lower.y <= point.y && point.y < self.upper.y)
            && (self.lower.z <= point.z && point.z < self.upper.z)
    }

    /// Whether this [`IBounds3`] fully encloses `other`.
    ///
    /// Inclusive, so an [`IBounds3`] encloses itself.
    pub const fn encloses(self, other: Self) -> bool {
        (self.lower.x <= other.lower.x && other.upper.x <= self.upper.x)
            && (self.lower.y <= other.lower.y && other.upper.y <= self.upper.y)
            && (self.lower.z <= other.lower.z && other.upper.z <= self.upper.z)
    }

    /// Whether this [`IBounds3`] has any point in common with `other`.
    pub const fn overlaps(self, other: Self) -> bool {
        (self.lower.x < other.upper.x && other.lower.x < self.upper.x)
            && (self.lower.y < other.upper.y && other.lower.y < self.upper.y)
            && (self.lower.z < other.upper.z && other.lower.z < self.upper.z)
    }

    /// Maps a point `lower..upper` to an offset `0..size`.
    ///
    /// Returns [`None`] if `point` lies outside the valid range of the [`IBounds3`].
    pub const fn map_uvec3(self, point: IVec3) -> Option<UVec3> {
        self.map_uvec3_with_origin(point, UVec3::ZERO)
    }

    /// Maps an offset `0..size` to a point `lower..upper`.
    ///
    /// Returns [`None`] if the resulting point lies outside the valid range of the [`IBounds3`].
    pub const fn unmap_uvec3(self, offset: UVec3) -> Option<IVec3> {
        self.unmap_uvec3_with_origin(offset, UVec3::ZERO)
    }

    /// Maps a point `lower..upper` to an offset `origin..origin + extent`.
    ///
    /// Returns [`None`] if `point` lies outside the valid range of the [`IBounds3`].
    pub const fn map_uvec3_with_origin(self, point: IVec3, origin: UVec3) -> Option<UVec3> {
        if self.contains(point) {
            let offset = point.wrapping_sub(self.lower);
            Some(uvec3(offset.x as u32, offset.y as u32, offset.z as u32).wrapping_add(origin))
        } else {
            None
        }
    }

    /// Maps an offset `origin..origin + extent` to a point `lower..upper`.
    ///
    /// Returns [`None`] if the resulting point lies outside the valid range of the [`IBounds3`].
    pub const fn unmap_uvec3_with_origin(self, offset: UVec3, origin: UVec3) -> Option<IVec3> {
        let offset = offset.wrapping_sub(origin);
        let offset = ivec3(offset.x as i32, offset.y as i32, offset.z as i32);
        let point = self.lower.wrapping_add(offset);
        if self.contains(point) {
            Some(point)
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ibounds3_contains() {
        let bounds = IBounds3::new(ivec3(1, 2, 3), ivec3(7, 6, 5)).unwrap();
        assert!(bounds.contains(ivec3(1, 2, 3)));
        assert!(bounds.contains(ivec3(6, 5, 4)));

        assert!(!bounds.contains(ivec3(0, 2, 3)));
        assert!(!bounds.contains(ivec3(1, 1, 3)));
        assert!(!bounds.contains(ivec3(1, 2, 2)));

        assert!(!bounds.contains(ivec3(7, 5, 4)));
        assert!(!bounds.contains(ivec3(6, 6, 4)));
        assert!(!bounds.contains(ivec3(6, 5, 5)));
    }

    #[test]
    fn ibounds3_encloses_overlaps() {
        let outer = IBounds3::new(ivec3(0, 0, 0), ivec3(3, 3, 3)).unwrap();
        let inner = IBounds3::new(ivec3(1, 1, 1), ivec3(2, 2, 2)).unwrap();
        let overlap = IBounds3::new(ivec3(1, 1, 1), ivec3(4, 4, 4)).unwrap();

        assert!(outer.encloses(outer));
        assert!(outer.encloses(inner));
        assert!(!outer.encloses(overlap));
        assert!(!inner.encloses(outer));
        assert!(!overlap.encloses(outer));

        assert!(outer.overlaps(outer));
        assert!(outer.overlaps(inner));
        assert!(outer.overlaps(overlap));
        assert!(inner.overlaps(outer));
        assert!(overlap.overlaps(outer));
    }

    #[test]
    fn ibounds3_map_uvec3() {
        let bounds = IBounds3::new(ivec3(-1, -2, -3), ivec3(3, 4, 5)).unwrap();

        assert_eq!(bounds.map_uvec3(ivec3(-1, -2, -3)), Some(uvec3(0, 0, 0)));
        assert_eq!(bounds.map_uvec3(ivec3(0, 0, 0)), Some(uvec3(1, 2, 3)));
        assert_eq!(bounds.map_uvec3(ivec3(2, 3, 4)), Some(uvec3(3, 5, 7)));

        assert_eq!(bounds.map_uvec3(ivec3(-2, -2, -3)), None);
        assert_eq!(bounds.map_uvec3(ivec3(-1, -3, -3)), None);
        assert_eq!(bounds.map_uvec3(ivec3(-1, -2, -4)), None);

        assert_eq!(bounds.map_uvec3(ivec3(3, 3, 4)), None);
        assert_eq!(bounds.map_uvec3(ivec3(2, 4, 4)), None);
        assert_eq!(bounds.map_uvec3(ivec3(2, 3, 5)), None);
    }

    #[test]
    fn ibounds3_unmap_uvec3() {
        let bounds = IBounds3::new(ivec3(-1, -2, -3), ivec3(3, 4, 5)).unwrap();

        assert_eq!(bounds.unmap_uvec3(uvec3(0, 0, 0)), Some(ivec3(-1, -2, -3)));
        assert_eq!(bounds.unmap_uvec3(uvec3(1, 2, 3)), Some(ivec3(0, 0, 0)));
        assert_eq!(bounds.unmap_uvec3(uvec3(3, 5, 7)), Some(ivec3(2, 3, 4)));

        assert_eq!(bounds.unmap_uvec3(uvec3(u32::MAX, 0, 0)), None);
        assert_eq!(bounds.unmap_uvec3(uvec3(0, u32::MAX, 0)), None);
        assert_eq!(bounds.unmap_uvec3(uvec3(0, 0, u32::MAX)), None);

        assert_eq!(bounds.unmap_uvec3(uvec3(4, 5, 7)), None);
        assert_eq!(bounds.unmap_uvec3(uvec3(3, 6, 7)), None);
        assert_eq!(bounds.unmap_uvec3(uvec3(3, 5, 8)), None);
    }

    #[test]
    fn ibounds3_map_uvec3_with_origin() {
        let bounds = IBounds3::new(ivec3(-1, -2, -3), ivec3(3, 4, 5)).unwrap();
        let check = |point| bounds.map_uvec3_with_origin(point, uvec3(1, 2, 3));

        assert_eq!(check(ivec3(-1, -2, -3)), Some(uvec3(1, 2, 3)));
        assert_eq!(check(ivec3(0, 0, 0)), Some(uvec3(2, 4, 6)));
        assert_eq!(check(ivec3(2, 3, 4)), Some(uvec3(4, 7, 10)));

        assert_eq!(check(ivec3(-2, -2, -3)), None);
        assert_eq!(check(ivec3(-1, -3, -3)), None);
        assert_eq!(check(ivec3(-1, -2, -4)), None);

        assert_eq!(check(ivec3(3, 3, 4)), None);
        assert_eq!(check(ivec3(2, 4, 4)), None);
        assert_eq!(check(ivec3(2, 3, 5)), None);
    }

    #[test]
    fn ibounds3_unmap_uvec3_with_origin() {
        let bounds = IBounds3::new(ivec3(-1, -2, -3), ivec3(3, 4, 5)).unwrap();
        let check = |point| bounds.unmap_uvec3_with_origin(point, uvec3(1, 2, 3));

        assert_eq!(check(uvec3(1, 2, 3)), Some(ivec3(-1, -2, -3)));
        assert_eq!(check(uvec3(2, 4, 6)), Some(ivec3(0, 0, 0)));
        assert_eq!(check(uvec3(4, 7, 10)), Some(ivec3(2, 3, 4)));

        assert_eq!(check(uvec3(0, 2, 3)), None);
        assert_eq!(check(uvec3(1, 1, 3)), None);
        assert_eq!(check(uvec3(1, 2, 2)), None);

        assert_eq!(check(uvec3(5, 7, 10)), None);
        assert_eq!(check(uvec3(4, 8, 10)), None);
        assert_eq!(check(uvec3(4, 7, 11)), None);
    }
}

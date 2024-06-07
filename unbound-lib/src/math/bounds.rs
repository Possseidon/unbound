use glam::{ivec3, uvec3, IVec3, UVec3};

/// Integer axis-aligned bounds in 3D-space defined by a lower and upper bound.
///
/// [`Self::min`] is guaranteed to be less than or equal to [`Self::max`]. This means, that bounds
/// cannot be empty along any of the axes.
///
/// [`Default`] returns [`Self::UNIT`], containing only [`IVec3::ZERO`].
#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq)]
pub struct IBounds3 {
    /// The lower bound of the cube (inclusive).
    min: IVec3,
    /// The upper bound of the cube (inclusive).
    max: IVec3,
}

impl IBounds3 {
    /// An [`IBounds3`] that only contains [`IVec3::ZERO`]
    pub const UNIT: Self = Self {
        min: IVec3::ZERO,
        max: IVec3::ZERO,
    };

    /// Constructs an [`IBounds3`] from the given `min` and `max` (both inclusive).
    ///
    /// Returns [`None`] if any component of `max` is less than `min`.
    ///
    /// Also returns [`None`] if any component of the [`Self::extent`] of the bounds would exceed
    /// [`UVec3::MAX`]. This can only happen if one of the components has a `min` of [`i32::MIN`]
    /// with a `max` of [`i32::MAX`].
    ///
    /// # Examples
    ///
    /// ```
    /// # use glam::{ivec3, uvec3, IVec3};
    /// # use unbound_lib::math::bounds::IBounds3;
    /// let min = ivec3(-1, -1, -1);
    /// let max = ivec3(1, 1, 1);
    /// let bounds = IBounds3::new(min, max).unwrap();
    /// assert_eq!(bounds.extent(), uvec3(3, 3, 3));
    ///
    /// let bounds_denorm = IBounds3::new(max, min); // swapped min and max
    /// assert_eq!(bounds_denorm, None);
    ///
    /// let bounds_too_big = IBounds3::new(IVec3::MIN, IVec3::MAX);
    /// assert_eq!(bounds_too_big, None);
    /// ```
    pub const fn new(min: IVec3, max: IVec3) -> Option<Self> {
        if min.x > max.x || min.y > max.y || min.z > max.z {
            return None;
        }
        if min.x == i32::MIN && max.x == i32::MAX
            || min.y == i32::MIN && max.y == i32::MAX
            || min.z == i32::MIN && max.z == i32::MAX
        {
            return None;
        }
        Some(Self { min, max })
    }

    /// Constructs an [`IBounds3`] containing only the given `point`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use glam::{ivec3, uvec3};
    /// # use unbound_lib::math::bounds::IBounds3;
    /// let point = ivec3(1, 2, 3);
    /// let bounds = IBounds3::from_point(point);
    /// assert!(bounds.contains(point));
    /// assert_eq!(bounds.extent(), uvec3(1, 1, 1));
    /// ```
    pub const fn from_point(point: IVec3) -> Self {
        Self {
            min: point,
            max: point,
        }
    }

    /// The inclusive lower bounds of the bounds.
    pub const fn min(self) -> IVec3 {
        self.min
    }

    /// The inclusive upper bounds of the bounds.
    pub const fn max(self) -> IVec3 {
        self.max
    }

    /// The extent (width, height and depth) of the bounds.
    ///
    /// All components are guaranteed to be greater than zero, since [`IBounds3`] cannot be empty.
    pub const fn extent(self) -> UVec3 {
        let size = self.max.wrapping_sub(self.min);
        uvec3(size.x as u32 + 1, size.y as u32 + 1, size.z as u32 + 1)
    }

    /// Whether the specified `point` is part of the bounds.
    pub const fn contains(self, point: IVec3) -> bool {
        (self.min.x <= point.x && point.x <= self.max.x)
            && (self.min.y <= point.y && point.y <= self.max.y)
            && (self.min.z <= point.z && point.z <= self.max.z)
    }

    /// Whether the bounds fully encloses `other`.
    ///
    /// Inclusive, so an [`IBounds3`] always encloses itself.
    pub const fn encloses(self, other: Self) -> bool {
        (self.min.x <= other.min.x && other.max.x <= self.max.x)
            && (self.min.y <= other.min.y && other.max.y <= self.max.y)
            && (self.min.z <= other.min.z && other.max.z <= self.max.z)
    }

    /// Whether the bounds have any point in common with `other`.
    pub const fn overlaps(self, other: Self) -> bool {
        (self.min.x <= other.max.x && other.min.x <= self.max.x)
            && (self.min.y <= other.max.y && other.min.y <= self.max.y)
            && (self.min.z <= other.max.z && other.min.z <= self.max.z)
    }

    /// Maps a point `min..max` to an offset `0..size`.
    ///
    /// Returns [`None`] if `point` lies outside the valid range of the bounds.
    pub const fn map_uvec3(self, point: IVec3) -> Option<UVec3> {
        self.map_uvec3_with_origin(point, UVec3::ZERO)
    }

    /// Maps an offset `0..size` to a point `min..max`.
    ///
    /// Returns [`None`] if the resulting point lies outside the valid range of the bounds.
    pub const fn unmap_uvec3(self, offset: UVec3) -> Option<IVec3> {
        self.unmap_uvec3_with_origin(offset, UVec3::ZERO)
    }

    /// Maps a point `min..max` to an offset `origin..origin + extent`.
    ///
    /// Returns [`None`] if `point` lies outside the valid range of the bounds.
    pub const fn map_uvec3_with_origin(self, point: IVec3, origin: UVec3) -> Option<UVec3> {
        if self.contains(point) {
            let offset = point.wrapping_sub(self.min);
            Some(uvec3(offset.x as u32, offset.y as u32, offset.z as u32).wrapping_add(origin))
        } else {
            None
        }
    }

    /// Maps an offset `origin..origin + extent` to a point `min..max`.
    ///
    /// Returns [`None`] if the resulting point lies outside the valid range of the [`IBounds3`].
    pub const fn unmap_uvec3_with_origin(self, offset: UVec3, origin: UVec3) -> Option<IVec3> {
        let offset = offset.wrapping_sub(origin);
        let offset = ivec3(offset.x as i32, offset.y as i32, offset.z as i32);
        let point = self.min.wrapping_add(offset);
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
        let bounds = IBounds3::new(ivec3(1, 2, 3), ivec3(6, 5, 4)).unwrap();
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
        let offset = IBounds3::new(ivec3(1, 1, 1), ivec3(4, 4, 4)).unwrap();

        assert!(outer.encloses(outer));
        assert!(outer.encloses(inner));
        assert!(!inner.encloses(outer));
        assert!(!outer.encloses(offset));
        assert!(!offset.encloses(outer));

        assert!(outer.overlaps(outer));
        assert!(outer.overlaps(inner));
        assert!(inner.overlaps(outer));
        assert!(outer.overlaps(offset));
        assert!(offset.overlaps(outer));

        assert!(outer.overlaps(IBounds3::UNIT));
        assert!(!inner.overlaps(IBounds3::UNIT));
        assert!(!offset.overlaps(IBounds3::UNIT));
    }

    #[test]
    fn ibounds3_map_uvec3() {
        let bounds = IBounds3::new(ivec3(-1, -2, -3), ivec3(2, 3, 4)).unwrap();

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
        let bounds = IBounds3::new(ivec3(-1, -2, -3), ivec3(2, 3, 4)).unwrap();

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
        let bounds = IBounds3::new(ivec3(-1, -2, -3), ivec3(2, 3, 4)).unwrap();
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
        let bounds = IBounds3::new(ivec3(-1, -2, -3), ivec3(2, 3, 4)).unwrap();
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

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
    /// Constructs an [`OctreeBounds`] with the specified `extent` at the origin.
    pub const fn from_extent(extent: OctreeExtent) -> Self {
        Self {
            min: UVec3::ZERO,
            extent,
        }
    }

    /// Constructs an [`OctreeBounds`] with [`OctreeExtent::ONE`] at the specified `pos`.
    pub const fn from_point(point: UVec3) -> Self {
        Self {
            min: point,
            extent: OctreeExtent::ONE,
        }
    }

    /// If the bounds only cover a single point, returns that point.
    pub const fn to_point(self) -> Option<UVec3> {
        if let OctreeExtent::ONE = self.extent {
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
        let [x, y, z] = self.extent.size_log2();
        uvec3(
            self.min.x + (1 << x) - 1,
            self.min.y + (1 << y) - 1,
            self.min.z + (1 << z) - 1,
        )
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

    /// The extent of the bounds.
    pub const fn extent(self) -> OctreeExtent {
        self.extent
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

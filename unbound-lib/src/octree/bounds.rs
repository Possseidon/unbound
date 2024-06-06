use glam::{uvec3, UVec3};

use super::extent::{OctreeExtent, OctreeSplits};

/// The location of a node within an octree.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct OctreeBounds {
    /// The position within the octree where the bounds start.
    ///
    /// Must be a multiple of [`Self::extent`] and at most [`i32::MAX`].
    pos: UVec3,
    /// The extent of the bounds.
    extent: OctreeExtent,
}

impl OctreeBounds {
    pub const fn from_extent(extent: OctreeExtent) -> Self {
        Self {
            pos: UVec3::ZERO,
            extent,
        }
    }

    pub const fn from_point(pos: UVec3) -> Self {
        Self {
            pos,
            extent: OctreeExtent::ONE,
        }
    }

    pub const fn to_point(self) -> Option<UVec3> {
        if let OctreeExtent::ONE = self.extent {
            Some(self.pos)
        } else {
            None
        }
    }

    pub const fn pos(self) -> UVec3 {
        self.pos
    }

    pub const fn extent(self) -> OctreeExtent {
        self.extent
    }

    /// Returns up to 6 splits (64 sub-bounds) in Z-order.
    ///
    /// Positions of each split are yielded by the [`OctreeBoundsSplit`] iterator. The extent of the
    /// sub-bounds is returned separate, since it is the same for all.
    ///
    /// See [`OctreeExtent::split_towards_cube`] for more info on splitting.
    pub fn split(self, splits: OctreeSplits) -> OctreeBoundsSplit {
        let total_splits = splits.total();
        // limited to 7; only up to 6 splits are actually used
        assert!(total_splits < 8);
        OctreeBoundsSplit {
            origin: self.pos,
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
            pos: self.origin + offset,
            extent: self.extent,
        })
    }
}

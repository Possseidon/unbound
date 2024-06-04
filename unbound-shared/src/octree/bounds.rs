use bevy::math::UVec3;

use super::extent::OctreeExtent;

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
    /// Having position and extent split like this also avoids having to unnecessarly convert to
    /// positions to [`OctreeOffset`]s.
    ///
    /// See [`OctreeExtent::split_towards_cube`] for more info on splitting.
    pub fn split(self, splits: [u8; 3]) -> (OctreeBoundsSplit, OctreeExtent) {
        todo!()
        // let extent: OctreeExtent = todo!("calculate from self and splits");
        // let split = OctreeBoundsSplit::new(self.pos, extent.size_log2(), splits);
        // (split, extent)
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
    fn new(origin: UVec3, extent: OctreeExtent, splits: [u8; 3]) -> Self {
        let total_splits = splits[0] + splits[1] + splits[2];
        // limited to 7; only up to 6 splits are actually used
        assert!(total_splits < 8);
        Self {
            origin,
            extent,
            x_splits: splits[0],
            y_splits: splits[1],
            index: 0,
            end: 1 << total_splits,
        }
    }

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
    type Item = UVec3;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index == self.end {
            return None;
        }

        let [x, y, z] = self.unpack_index();

        // increment after unpacking
        self.index += 1;

        let size_log2 = self.extent.size_log2();
        let offset = UVec3::new(
            u32::from(x) << size_log2[0],
            u32::from(y) << size_log2[1],
            u32::from(z) << size_log2[2],
        );

        Some(self.origin + offset)
    }
}

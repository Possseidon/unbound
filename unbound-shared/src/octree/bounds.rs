use bevy::math::UVec3;

use super::extent::{OctreeExtent, OctreeSplit};

/// The location of a node within an octree.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct OctreeBounds {
    /// The position within the octree where the bounds start.
    ///
    /// Must be a multiple of [`Self::extent`] and at most [`i32::MAX`].
    offset: UVec3,
    /// The extent of the bounds.
    extent: OctreeExtent,
}

impl OctreeBounds {
    pub const fn from_extent(extent: OctreeExtent) -> Self {
        Self {
            offset: UVec3::ZERO,
            extent,
        }
    }

    pub const fn from_point(pos: UVec3) -> Self {
        Self {
            offset: pos,
            extent: OctreeExtent::ONE,
        }
    }

    pub const fn to_point(self) -> Option<UVec3> {
        if let OctreeExtent::ONE = self.extent {
            Some(self.offset)
        } else {
            None
        }
    }

    pub const fn offset(self) -> UVec3 {
        self.offset
    }

    pub const fn extent(self) -> OctreeExtent {
        self.extent
    }

    pub fn split(self) -> Option<OctreeBoundsSplit> {
        Some(match self.extent.next_split()? {
            OctreeSplit::Half => {
                OctreeBoundsSplit::Half(self.split_in_half().expect("bounds should split in half"))
            }
            OctreeSplit::Quarter => OctreeBoundsSplit::Quarter(
                self.split_into_quarters()
                    .expect("bounds should split into quarters"),
            ),
            OctreeSplit::Octant => OctreeBoundsSplit::Octant(
                self.split_into_octants()
                    .expect("bounds should split into octants"),
            ),
        })
    }

    /// Splits the bounds in half along the longest axis.
    ///
    /// Returns [`None`] if there are multiple longest axes.
    ///
    /// The half with the original offset is returned first.
    pub fn split_in_half(self) -> Option<[Self; 2]> {
        let (extent, halfed) = self.extent.half_longest()?;
        (halfed.bitmask().count_ones() == 1).then_some([
            self.with_extent_and_offset(extent, UVec3::ZERO),
            self.with_extent_and_offset(extent, UVec3::select(halfed, extent.size(), UVec3::ZERO)),
        ])
    }

    /// Splits the bounds in quarters along the longest two axes.
    ///
    /// Returns [`None`] if there aren't exactly two longest axes.
    ///
    /// The returned quarters are ordered lexicographically by their X, Y and finally Z offset
    /// (one of which is skipped, since all quarters share one of the three axes).
    pub fn split_into_quarters(self) -> Option<[Self; 4]> {
        let (extent, halfed) = self.extent.half_longest()?;
        let (a, b) = match halfed.bitmask() {
            0b011 => (UVec3::X, UVec3::Y),
            0b101 => (UVec3::X, UVec3::Z),
            0b110 => (UVec3::Y, UVec3::Z),
            _ => return None,
        };

        Some([
            self.with_extent_and_offset(extent, UVec3::ZERO),
            self.with_extent_and_offset(extent, a),
            self.with_extent_and_offset(extent, b),
            self.with_extent_and_offset(extent, a + b),
        ])
    }

    /// Splits the bounds in octants.
    ///
    /// Returns [`None`] if any axis has a different length or if the extent is
    /// [`OctreeExtent::ONE`].
    ///
    /// The returned octants are ordered lexicographically by their X, Y and finally Z offset.
    pub fn split_into_octants(self) -> Option<[Self; 8]> {
        let (extent, halfed) = self.extent.half_longest()?;
        halfed.all().then_some([
            self.with_extent_and_offset(extent, UVec3::ZERO),
            self.with_extent_and_offset(extent, UVec3::X),
            self.with_extent_and_offset(extent, UVec3::Y),
            self.with_extent_and_offset(extent, UVec3::Z),
            self.with_extent_and_offset(extent, UVec3::X + UVec3::Y),
            self.with_extent_and_offset(extent, UVec3::X + UVec3::Z),
            self.with_extent_and_offset(extent, UVec3::Y + UVec3::Z),
            self.with_extent_and_offset(extent, UVec3::ONE),
        ])
    }

    pub(crate) fn with_extent_and_offset(self, extent: OctreeExtent, offset: UVec3) -> Self {
        Self {
            offset: self.offset + offset,
            extent,
        }
    }
}

impl From<OctreeExtent> for OctreeBounds {
    fn from(extent: OctreeExtent) -> Self {
        Self::from_extent(extent)
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum OctreeBoundsSplit {
    Half([OctreeBounds; 2]),
    Quarter([OctreeBounds; 4]),
    Octant([OctreeBounds; 8]),
}

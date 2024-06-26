use std::{
    cmp::Ordering,
    fmt::Debug,
    hash::{Hash, Hasher},
    iter::FusedIterator,
    mem::size_of,
};

use bitvec::{bitarr, field::BitField, order::Lsb0, BitArr};
use enum_map::Enum;

use crate::math::enums::Corner3;

/// Identifies a single octant within an octree represented as a list of [`Corner3`].
///
/// This includes any octant nested in other octants as well as the octree root.
#[derive(Clone, Copy, Default, Eq)]
pub struct Octant {
    /// Stores 5 bits for the length followed by up to 30 sets of of 3 bits for each corner.
    data: BitArr!(for OCTANT_BITS, in u8),
}

const _: () = assert!(size_of::<Octant>() == 12);

const OCTANT_LEN_BITS: usize = 5;
const OCTANT_CORNER_BITS: usize = 3;
const OCTANT_BITS: usize = OCTANT_LEN_BITS + OctreeDepth::MAX.0 as usize * OCTANT_CORNER_BITS;

impl Octant {
    /// An [`Octant`] pointing to the root of an octree.
    pub const ROOT: Self = Self {
        data: bitarr!(u8, Lsb0; 0; OCTANT_BITS),
    };

    /// Constructs a new [`Octant`] pointing to the given octant relative to the root of the octree.
    pub fn new(corner: Corner3) -> Self {
        Self::ROOT.with_corner(corner)
    }

    /// The number of corner traversals to reach this octant from the root of the octree.
    pub fn depth(self) -> OctreeDepth {
        OctreeDepth(self.data.get(0..OCTANT_LEN_BITS).unwrap().load_le())
    }

    /// Whether the octant represents the root of the octree.
    pub fn is_root(self) -> bool {
        self.depth().0 == 0
    }

    /// Returns the parent octant.
    pub fn parent(mut self) -> Option<Self> {
        self.set_depth(OctreeDepth(self.depth().0.checked_sub(1)?));
        Some(self)
    }

    /// Returns the corner of the innermost octant.
    pub fn last_corner(self) -> Option<Corner3> {
        if self.is_root() {
            None
        } else {
            Some(self.get_corner(OctreeDepth(self.depth().0 - 1)))
        }
    }

    /// Returns a child octant at the given corner.
    ///
    /// # Panics
    ///
    /// Panics if the maximum depth has been exceeded. The reason behind a panic instead of
    /// returning e.g. a [`Result`] is that octrees are generally not used directly but wrapped by a
    /// different type. The responsibility for ensuring that the depth is not exceeded falls on that
    /// wrapper type.
    pub fn with_corner(mut self, corner: Corner3) -> Self {
        let index = self.depth();
        self.set_depth(OctreeDepth::new(index.0 + 1).expect("octant should not exceed max depth"));
        self.set_corner(index, corner);
        self
    }

    /// Returns the next (or previous) neighboring corner of this octant if any.
    pub fn next_neighbor<const REVERSE: bool>(mut self) -> Option<Self> {
        let (dir, last_corner) = if REVERSE {
            (-1isize, Corner3::Origin)
        } else {
            (1isize, Corner3::XYZ)
        };
        if self.is_root() {
            return None;
        }
        let last = OctreeDepth(self.depth().0 - 1);
        let corner = self.get_corner(last);
        if corner == last_corner {
            return None;
        }
        self.set_corner(
            last,
            Corner3::from_usize(
                corner
                    .into_usize()
                    .checked_add_signed(dir)
                    .expect("should not overflow"),
            ),
        );
        Some(self)
    }

    /// Returns the next (or previous) octant.
    ///
    /// Similar to [`Octant::next_neighbor`], but returns the next [`Octant`] of the parent if there
    /// is no immediate neighbor.
    pub fn next_octant<const REVERSE: bool>(mut self) -> Option<Self> {
        loop {
            if let Some(neighbor) = self.next_neighbor::<REVERSE>() {
                break Some(neighbor);
            }
            if let Some(parent) = self.parent() {
                self = parent;
            } else {
                break None;
            }
        }
    }

    fn set_depth(&mut self, depth: OctreeDepth) {
        self.data
            .get_mut(0..OCTANT_LEN_BITS)
            .unwrap()
            .store_le(depth.0);
    }

    fn get_corner(self, depth: OctreeDepth) -> Corner3 {
        let index = OCTANT_LEN_BITS + usize::from(depth.0) * OCTANT_CORNER_BITS;
        Corner3::from_usize(
            self.data
                .get(index..index + OCTANT_CORNER_BITS)
                .unwrap()
                .load_le(),
        )
    }

    fn set_corner(&mut self, depth: OctreeDepth, corner: Corner3) {
        let index = OCTANT_LEN_BITS + usize::from(depth.0) * OCTANT_CORNER_BITS;
        self.data
            .get_mut(index..index + OCTANT_CORNER_BITS)
            .unwrap()
            .store_le(corner.into_usize());
    }
}

impl Debug for Octant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list().entries(*self).finish()
    }
}

impl Hash for Octant {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.depth().hash(state);
        for corner in *self {
            corner.hash(state);
        }
    }
}

impl PartialEq for Octant {
    fn eq(&self, other: &Self) -> bool {
        // implemented manually, since unused bits in data are undefined
        self.into_iter().eq(*other)
    }
}

impl PartialOrd for Octant {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Octant {
    fn cmp(&self, other: &Self) -> Ordering {
        self.into_iter().cmp(*other)
    }
}

impl IntoIterator for Octant {
    type Item = Corner3;
    type IntoIter = OctantCorners;

    fn into_iter(self) -> Self::IntoIter {
        OctantCorners {
            depth: OctreeDepth::MIN,
            octant: self,
        }
    }
}

/// An iterator over the corners that have to be traversed to reach a specific [`Octant`].
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct OctantCorners {
    /// The current depth, starting at zero and counting up.
    depth: OctreeDepth,
    /// The octant whose corners to iterate over.
    octant: Octant,
}

impl Iterator for OctantCorners {
    type Item = Corner3;

    fn next(&mut self) -> Option<Self::Item> {
        if self.depth != self.octant.depth() {
            let corner = self.octant.get_corner(self.depth);
            self.depth.0 += 1;
            Some(corner)
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.len(), Some(self.len()))
    }

    fn count(self) -> usize {
        self.len()
    }

    fn nth(&mut self, n: usize) -> Option<Self::Item> {
        self.depth = if let Ok(n) = u8::try_from(n) {
            self.depth.saturating_add(n)
        } else {
            self.octant.depth()
        };
        self.next()
    }
}

impl ExactSizeIterator for OctantCorners {
    fn len(&self) -> usize {
        usize::from(self.octant.depth().0 - self.depth.0)
    }
}

impl FusedIterator for OctantCorners {}

/// The depth of an octant within an octree.
#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct OctreeDepth(u8);

impl OctreeDepth {
    /// Indicates the root of an octree.
    pub const MIN: Self = Self(0);
    /// The maximum possible depth is 30.
    ///
    /// This allows for a resolution of slightly over 1 billion along each axis while keeping
    /// [`OctantCorners`] small and efficient.
    pub const MAX: Self = Self(30);

    /// Constructs a new [`OctreeDepth`], returning [`None`] if the value is out of range.
    pub const fn new(index: u8) -> Option<Self> {
        if index <= Self::MAX.0 {
            Some(Self(index))
        } else {
            None
        }
    }

    /// Returns the depth as a [`u8`].
    pub const fn get(self) -> u8 {
        self.0
    }

    /// Adds the given value to the depth, saturating at the maximum.
    pub const fn saturating_add(self, rhs: u8) -> Self {
        let result = self.0.saturating_add(rhs);
        if result > Self::MAX.0 {
            Self(Self::MAX.0)
        } else {
            Self(result)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn root() {
        let root = Octant::ROOT;
        assert!(root.is_root());
        assert_eq!(root.depth().0, 0);
    }

    #[test]
    fn new() {
        let corner = Corner3::Origin;
        let octant = Octant::new(corner);
        assert!(!octant.is_root());
        assert_eq!(octant.depth().0, 1);
        assert_eq!(octant.get_corner(OctreeDepth(0)), corner);
    }

    #[test]
    fn depth() {
        let corner = Corner3::Origin;
        let octant = Octant::new(corner);
        assert_eq!(octant.depth().0, 1);
        let child = octant.with_corner(corner);
        assert_eq!(child.depth().0, 2);
    }

    #[test]
    fn parent() {
        let octant = Octant::new(Corner3::Origin);
        assert!(octant.parent().unwrap().is_root());
    }

    #[test]
    fn corner_max_depth() {
        let mut octant = Octant::ROOT;
        for _ in 0..OctreeDepth::MAX.0 {
            octant = octant.with_corner(Corner3::Origin);
        }
    }

    #[test]
    #[should_panic(expected = "octant should not exceed max depth")]
    fn corner_exceeding_max_depth() {
        let mut octant = Octant::ROOT;
        for _ in 0..OctreeDepth::MAX.0 + 1 {
            octant = octant.with_corner(Corner3::Origin);
        }
    }

    #[test]
    fn octant_debug() {
        assert_eq!(format!("{:?}", Octant::ROOT), "[]");

        let octant = Octant::new(Corner3::Origin);
        assert_eq!(format!("{:?}", octant), "[Origin]");

        let octant = Octant::new(Corner3::Y).with_corner(Corner3::XZ);
        assert_eq!(format!("{:?}", octant), "[Y, XZ]");
    }

    #[test]
    fn octant_eq_edge_case() {
        // if PartialEq was just derived, this assertion would fail
        assert_eq!(Octant::new(Corner3::XYZ).parent(), Some(Octant::ROOT));
    }

    #[test]
    fn octant_corners() {
        let mut corners = Octant::new(Corner3::Origin)
            .with_corner(Corner3::XYZ)
            .into_iter();
        assert_eq!(corners.len(), 2);
        assert_eq!(corners.next(), Some(Corner3::Origin));
        assert_eq!(corners.len(), 1);
        assert_eq!(corners.next(), Some(Corner3::XYZ));
        assert_eq!(corners.len(), 0);
        assert_eq!(corners.next(), None);
    }

    #[test]
    #[allow(clippy::iter_nth_zero)]
    fn octant_corners_nth() {
        let corners = Octant::new(Corner3::Origin)
            .with_corner(Corner3::XYZ)
            .into_iter();

        assert_eq!(corners.clone().nth(0), Some(Corner3::Origin));
        assert_eq!(corners.clone().nth(1), Some(Corner3::XYZ));
        assert_eq!(corners.clone().nth(2), None);
    }

    #[test]
    fn octant_corners_nth_max_depth() {
        let mut octant = Octant::ROOT;
        for _ in 0..OctreeDepth::MAX.0 - 1 {
            octant = octant.with_corner(Corner3::Origin);
        }
        let corners = octant.with_corner(Corner3::XYZ).into_iter();
        let last = usize::from(OctreeDepth::MAX.0 - 1);
        assert_eq!(corners.clone().nth(last - 1), Some(Corner3::Origin));
        assert_eq!(corners.clone().nth(last), Some(Corner3::XYZ));
        assert_eq!(corners.clone().nth(last + 1), None);
    }

    #[test]
    fn octree_depth() {
        assert_eq!(OctreeDepth::new(OctreeDepth::MIN.0), Some(OctreeDepth::MIN));
        assert_eq!(OctreeDepth::new(OctreeDepth::MAX.0), Some(OctreeDepth::MAX));
        assert_eq!(OctreeDepth::new(OctreeDepth::MAX.0 + 1), None);
    }

    #[test]
    fn octant_next_neighbor_root() {
        assert_eq!(Octant::ROOT.next_neighbor::<false>(), None);
        assert_eq!(Octant::ROOT.next_neighbor::<true>(), None);
    }

    #[test]
    fn octant_next_neighbor() {
        let neighbor = Octant::new(Corner3::Origin).next_neighbor::<false>();
        assert_eq!(neighbor, Some(Octant::new(Corner3::X)));

        let neighbor = Octant::new(Corner3::XYZ).next_neighbor::<true>();
        assert_eq!(neighbor, Some(Octant::new(Corner3::YZ)));
    }

    #[test]
    fn octant_next_neighbor_none() {
        assert_eq!(Octant::new(Corner3::XYZ).next_neighbor::<false>(), None);
        assert_eq!(Octant::new(Corner3::Origin).next_neighbor::<true>(), None);
    }

    #[test]
    fn octant_next_octant() {
        let next_octant = Octant::new(Corner3::Origin).next_octant::<false>();
        assert_eq!(next_octant, Some(Octant::new(Corner3::X)));
    }

    #[test]
    fn octant_next_octant_from_parent() {
        let next_octant = Octant::new(Corner3::Origin)
            .with_corner(Corner3::XYZ)
            .next_octant::<false>();
        assert_eq!(next_octant, Some(Octant::new(Corner3::X)));

        let next_octant = Octant::new(Corner3::Origin)
            .with_corner(Corner3::XYZ)
            .with_corner(Corner3::XYZ)
            .with_corner(Corner3::XYZ)
            .with_corner(Corner3::XYZ)
            .next_octant::<false>();
        assert_eq!(next_octant, Some(Octant::new(Corner3::X)));
    }
}

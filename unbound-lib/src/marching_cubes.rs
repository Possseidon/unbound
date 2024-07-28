use std::{cmp::Ordering, hash::Hash, iter};

use array_init::array_init;
use arrayvec::ArrayVec;
use glam::{IVec3, Vec3};
use itertools::Itertools;

use crate::math::enums::{Axis3, Corner3, Corners3, Facing3, Facings3};

/// Contains generated lookup information for building 3D meshes using a marching cubes algorithm.
///
/// Keep in mind, that this type takes up **5KiB** on the stack, as it is fully stack-allocated.
#[derive(Clone, Debug, Eq)]
pub struct MarchingCubes {
    lookup: Lookup,
}

impl MarchingCubes {
    /// Generates the lookup table.
    pub fn generate() -> Self {
        Self {
            lookup: generate_corner_planes(),
        }
    }

    /// Given a set of solid corners of a 3D cube, returns the corresponding set of planes that
    /// divides this cube into separate volumes.
    ///
    /// E.g. a single solid corner results in a single triangle along the center of the three edges
    /// next to that corner.
    pub fn planes(&self, corners: Corners3) -> &Planes {
        &self.lookup[corners.as_usize()]
    }
}

impl Default for MarchingCubes {
    fn default() -> Self {
        Self::generate()
    }
}

impl Hash for MarchingCubes {
    fn hash<H: std::hash::Hasher>(&self, _state: &mut H) {}
}

impl PartialEq for MarchingCubes {
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}

impl PartialOrd for MarchingCubes {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for MarchingCubes {
    fn cmp(&self, _other: &Self) -> Ordering {
        Ordering::Equal
    }
}

/// The planes that divide a 3D cube into separate volumes.
///
/// The underlying marching cubes algorithm guarantees, that there are at most 5 dividing planes.
pub type Planes = ArrayVec<Plane, 5>;

/// One of the up to 5 dividing planes of a 3D cube, defined by three [`PlanePoint`]s.
pub type Plane = [PlanePoint; 3];

/// A single point on a plane that is part of a marching cubes cube division.
///
/// The most straightforward way to turn a [`PlanePoint`] into an actual 3D position is to use the
/// [`From`] trait to convert it into a [`Vec3`].
///
/// Alternatively, [`PlanePoint::with_density`] allows you to shift the plane relative to the solid
/// points that were used to generate this plane. The [`From`] trait simply uses a density of `0.5`.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct PlanePoint {
    /// Stores both corner and facing information.
    ///
    /// From least to most significant bits:
    ///
    /// - 3 bits for [`Self::corner`]
    /// - 3 bits for [`Self::facing`]
    /// - 2 bits that are unused and always zero
    corner_facing: u8,
}

impl PlanePoint {
    /// The solid corner that this point on the plane divides from.
    pub fn corner(self) -> Corner3 {
        match self.corner_facing & 0b111 {
            0 => Corner3::Origin,
            1 => Corner3::X,
            2 => Corner3::Y,
            3 => Corner3::XY,
            4 => Corner3::Z,
            5 => Corner3::XZ,
            6 => Corner3::YZ,
            7 => Corner3::XYZ,
            _ => unreachable!(),
        }
    }

    /// The non-solid corner, to which the [`PlanePoint::facing`] direction points to.
    pub fn opposite_corner(self) -> Corner3 {
        self.corner().flipped(self.axis())
    }

    /// The facing direction pointing from the solid [`PlanePoint::corner`] to a non-solid point.
    pub fn facing(self) -> Facing3 {
        match self.corner_facing >> 3 {
            0 => Facing3::NegX,
            1 => Facing3::PosX,
            2 => Facing3::NegY,
            3 => Facing3::PosY,
            4 => Facing3::NegZ,
            5 => Facing3::PosZ,
            _ => unreachable!(),
        }
    }

    /// Similar to [`PlanePoint::facing`], but only returns the axis.
    ///
    /// The direction is already dictated by [`PlanePoint::corner`], so this might be sufficient.
    pub fn axis(self) -> Axis3 {
        self.facing().axis()
    }

    /// Turns this [`PlanePoint`] into a [`Vec3`]. The plane is shifted by `density` away from the
    /// solid points that were used to generate this plane.
    ///
    /// Alternatively the [`From`] trait can be used and simply uses a density of `0.5`.
    ///
    /// If you always use `0.5` the resulting mesh will basically have an angle of 45° between
    /// planes, which is expected from a conventional marching cubes algorithm. However, by using
    /// varying densities (e.g. by doing something similar to how anti-aliasing works for raster
    /// images) allows for less steep angles and an overall smoother mesh.
    pub fn with_density(self, density: f32) -> Vec3 {
        Vec3::from(self.corner()) + Vec3::from(self.facing()) * density
    }

    fn new(corner: Corner3, facing: Facing3) -> PlanePoint {
        Self {
            corner_facing: (corner as u8) | (facing as u8) << 3,
        }
    }

    fn from_corner_and_axis(corner: Corner3, axis: Axis3) -> PlanePoint {
        Self::new(corner, corner.facing_along_axis(axis).flipped())
    }

    fn from_corner_and_axis_flipped(corner: Corner3, axis: Axis3) -> PlanePoint {
        Self::new(corner.flipped(axis), corner.facing_along_axis(axis))
    }
}

impl From<PlanePoint> for Vec3 {
    fn from(value: PlanePoint) -> Self {
        value.with_density(0.5)
    }
}

type Loops = ArrayVec<Loop, 4>;
type Loop = ArrayVec<PlanePoint, 7>;
type Lines = ArrayVec<Line, 12>;

type Lookup = [Planes; 256];

fn generate_corner_planes() -> Lookup {
    array_init(|corners| generate_planes(Corners3::from_usize(corners)))
}

fn generate_planes(corners: Corners3) -> Planes {
    Planes::from_iter(generate_loops(corners).into_iter().flat_map(|line_loop| {
        let mut point_iter = line_loop.into_iter();
        let t1 = point_iter.next().unwrap();
        point_iter.tuple_windows().map(move |(t2, t3)| [t1, t2, t3])
    }))
}

fn generate_loops(corners: Corners3) -> Loops {
    let mut lines = generate_lines(corners);
    Loops::from_iter(iter::from_fn(move || {
        lines.pop().map(|line| {
            let mut line_loop = Loop::from_iter([line.start, line.stop]);
            'outer: loop {
                for (index, other_line) in lines.iter().enumerate() {
                    if other_line.start == *line_loop.last().unwrap() {
                        let done = other_line.stop == *line_loop.first().unwrap();
                        if !done {
                            line_loop.push(other_line.stop);
                        }
                        lines.remove(index);

                        if done {
                            break 'outer;
                        }

                        break;
                    }
                }
            }
            line_loop
        })
    }))
}

fn generate_lines(corners: Corners3) -> Lines {
    Lines::from_iter(
        Facings3::all()
            .into_iter()
            .flat_map(|facing| -> ArrayVec<Line, 2> {
                let masked_corners = corners & Corners3::from(facing);
                let (cross_y, cross_z) = facing.axis().cross_axes();

                match masked_corners.len() {
                    0 | 4 => Default::default(),
                    1 => {
                        let corner = masked_corners.iter().next().unwrap();
                        ArrayVec::from_iter([Line {
                            start: PlanePoint::from_corner_and_axis(corner, cross_z),
                            stop: PlanePoint::from_corner_and_axis(corner, cross_y),
                        }
                        .flip_if(corner.is_even())])
                    }
                    2 => {
                        let mut corner_iter = masked_corners.iter();
                        let corner1 = corner_iter.next().unwrap();
                        let corner2 = corner_iter.next().unwrap();
                        if corner1.connects_to(corner2) {
                            let mut normal = corner1
                                .facing_corner(corner2)
                                .unwrap()
                                .cross(facing)
                                .unwrap();
                            let flip = IVec3::from(facing)
                                .cross(IVec3::from(corner1) + IVec3::from(corner2) - 1)
                                .cmple(IVec3::ZERO)
                                .all();
                            if flip {
                                normal = normal.flipped();
                            }
                            ArrayVec::from_iter([Line {
                                start: PlanePoint::new(corner1, normal),
                                stop: PlanePoint::new(corner2, normal),
                            }
                            .flip_if(flip)])
                        } else {
                            ArrayVec::from_iter([
                                Line {
                                    start: PlanePoint::from_corner_and_axis(corner1, cross_z),
                                    stop: PlanePoint::from_corner_and_axis(corner1, cross_y),
                                }
                                .flip_if(corner1.is_even()),
                                Line {
                                    start: PlanePoint::from_corner_and_axis(corner2, cross_z),
                                    stop: PlanePoint::from_corner_and_axis(corner2, cross_y),
                                }
                                .flip_if(corner2.is_even()),
                            ])
                        }
                    }
                    3 => {
                        let corner = (Corners3::from(facing) - corners).iter().next().unwrap();
                        ArrayVec::from_iter([Line {
                            start: PlanePoint::from_corner_and_axis_flipped(corner, cross_z),
                            stop: PlanePoint::from_corner_and_axis_flipped(corner, cross_y),
                        }
                        .flip_if(!corner.is_even())])
                    }
                    _ => unreachable!(),
                }
            }),
    )
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct Line {
    start: PlanePoint,
    stop: PlanePoint,
}

impl Line {
    fn flip_if(self, flip: bool) -> Self {
        if flip {
            Self {
                start: self.stop,
                stop: self.start,
            }
        } else {
            self
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn marching_cubes() {
        let mc = MarchingCubes::generate();
        for corners in (0..256).map(Corners3::from_usize) {
            let planes = mc.planes(corners);
            assert_eq!(
                planes.is_empty(),
                corners.is_empty() || corners == Corners3::all(),
                "only empty or full cubes have no planes",
            );
            for plane in planes {
                for plane_point in plane {
                    assert!(
                        corners.contains(plane_point.corner()),
                        "{plane_point:?} should be contained in {corners:?}",
                    );
                    assert!(
                        !corners.contains(plane_point.opposite_corner()),
                        "{plane_point:?} should not be contained in {corners:?}",
                    );
                }
            }
        }
    }
}

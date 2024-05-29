use std::ops::{Index, IndexMut};

use bevy::math::{
    BVec2, BVec3, BVec4, DVec2, DVec3, DVec4, IVec2, IVec3, IVec4, UVec2, UVec3, UVec4, Vec2, Vec3,
    Vec3A, Vec4,
};
use enum_map::Enum;
use enumset::{EnumSet, EnumSetType};

macro_rules! impl_from_for_scalar {
    { $enum_type:ident for $value_type:ident { $( $variant:ident => $value:expr, )* } } => {
        impl From<$enum_type> for $value_type {
            fn from(value: $enum_type) -> Self {
                match value { $(
                    <$enum_type>::$variant => $value as _,
                )* }
            }
        }
    };
    { $enum_type:ident for [ $( $value_type:ident ),* ] $values:tt } => { $(
        impl_from_for_scalar! {
            $enum_type for $value_type
            $values
        }
    )* };
}

macro_rules! impl_from_for_vec {
    { $enum_type:ident for $value_type:ident {
        $( $variant:ident => ( $( $value:expr ),* ), )*
    } } => {
        impl From<$enum_type> for $value_type {
            fn from(value: $enum_type) -> Self {
                match value {
                    $( <$enum_type>::$variant => Self::new( $( $value as _ ),* ), )*
                }
            }
        }

        impl TryFrom<$value_type> for $enum_type {
            type Error = ();

            fn try_from(value: $value_type) -> Result<Self, Self::Error> {
                $( if value == $value_type::new( $( $value as _ ),* ) {
                    return Ok(<$enum_type>::$variant);
                } )*
                Err(())
            }
        }
    };
    { $enum_type:ident for [ $( $value_type:ident ),* ] $values:tt } => { $(
        impl_from_for_vec! {
            $enum_type for $value_type
            $values
        }
    )* };
}

macro_rules! impl_index_for_vec {
    { $axis_type:ident for $base_type:ident: $vector_type:ident {
        $( $axis_name:ident => $axis_field:ident, )*
    } } => {
        impl Index<$axis_type> for $vector_type {
            type Output = $base_type;

            fn index(&self, index: $axis_type) -> &Self::Output {
                match index {
                    $( $axis_type::$axis_name => &self.$axis_field, )*
                }
            }
        }

        impl IndexMut<$axis_type> for $vector_type {
            fn index_mut(&mut self, index: $axis_type) -> &mut Self::Output {
                match index {
                    $( $axis_type::$axis_name => &mut self.$axis_field, )*
                }
            }
        }
    };
    { $axis_type:ident for [
        $( $base_type:ident: $( $vector_type:ident ),* ; )*
    ] $axes:tt } => { $( $(
        impl_index_for_vec! {
            $axis_type for $base_type: $vector_type
            $axes
        }
    )* )* };
}

/// A one-dimensional axis; i.e. only a single one, namely [`Axis1::X`].
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Enum, EnumSetType)]
#[enumset(no_super_impls)]
pub enum Axis1 {
    X,
}

impl_from_for_scalar! {
    Axis1 for [f32, f64, i32, u32] {
        X => 1,
    }
}

impl_index_for_vec! {
    Axis1 for [
        f32: Vec2, Vec3, Vec3A, Vec4;
        f64: DVec2, DVec3, DVec4;
        i32: IVec2, IVec3, IVec4;
        u32: UVec2, UVec3, UVec4;
        bool: BVec2, BVec3, BVec4;

    ] {
        X => x,
    }
}

/// A set of one-dimensional axes.
pub type Axes1 = EnumSet<Axis1>;

/// A two-dimensional axis; `X` or `Y`.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Enum, EnumSetType)]
#[enumset(no_super_impls)]
pub enum Axis2 {
    X,
    Y,
}

impl_from_for_vec! {
    Axis2 for [Vec2, DVec2, IVec2, UVec2] {
        X => (1, 0),
        Y => (0, 1),
    }
}

impl_index_for_vec! {
    Axis2 for [
        f32: Vec2, Vec3, Vec3A, Vec4;
        f64: DVec2, DVec3, DVec4;
        i32: IVec2, IVec3, IVec4;
        u32: UVec2, UVec3, UVec4;
        bool: BVec2, BVec3, BVec4;

    ] {
        X => x,
        Y => y,
    }
}

/// A set of two-dimensional axes.
pub type Axes2 = EnumSet<Axis2>;

/// A three-dimensional axis; `X`, `Y`, or `Z`.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Enum, EnumSetType)]
#[enumset(no_super_impls)]
pub enum Axis3 {
    X,
    Y,
    Z,
}

impl Axis3 {
    /// Returns the other two axes in a rotationally symmetrical order (think cross-product).
    pub fn cross_axes(self) -> (Self, Self) {
        match self {
            Self::X => (Self::Y, Self::Z),
            Self::Y => (Self::Z, Self::X),
            Self::Z => (Self::X, Self::Y),
        }
    }
}

impl_from_for_vec! {
    Axis3 for [Vec3, Vec3A, DVec3, IVec3, UVec3] {
        X => (1, 0, 0),
        Y => (0, 1, 0),
        Z => (0, 0, 1),
    }
}

impl_index_for_vec! {
    Axis3 for [
        f32: Vec3, Vec3A, Vec4;
        f64: DVec3, DVec4;
        i32: IVec3, IVec4;
        u32: UVec3, UVec4;
        bool: BVec3, BVec4;

    ] {
        X => x,
        Y => y,
        Z => z,
    }
}

/// A set of three-dimensional axes.
pub type Axes3 = EnumSet<Axis3>;

/// A four-dimensional axis; `X`, `Y`, `Z`, or `W`.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Enum, EnumSetType)]
#[enumset(no_super_impls)]
pub enum Axis4 {
    X,
    Y,
    Z,
    W,
}

impl_from_for_vec! {
    Axis4 for [Vec4, DVec4, IVec4, UVec4] {
        X => (1, 0, 0, 0),
        Y => (0, 1, 0, 0),
        Z => (0, 0, 1, 0),
        W => (0, 0, 0, 1),
    }
}

impl_index_for_vec! {
    Axis4 for [
        f32: Vec4;
        f64: DVec4;
        i32: IVec4;
        u32: UVec4;
        bool: BVec4;

    ] {
        X => x,
        Y => y,
        Z => z,
        W => w,
    }
}

/// A set of four-dimensional axes.
pub type Axes4 = EnumSet<Axis4>;

/// A one-dimensional facing direction; either left or right.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Enum, EnumSetType)]
#[enumset(no_super_impls)]
pub enum Facing1 {
    X0,
    X1,
}

impl Facing1 {
    /// Returns the opposite facing direction.
    pub fn flipped(self) -> Self {
        match self {
            Self::X0 => Self::X1,
            Self::X1 => Self::X0,
        }
    }

    /// Returns the axis which the facing direction is along.
    pub fn axis(self) -> Axis1 {
        match self {
            Self::X0 | Self::X1 => Axis1::X,
        }
    }
}

impl From<Axis1> for Facing1 {
    fn from(value: Axis1) -> Self {
        match value {
            Axis1::X => Self::X1,
        }
    }
}

impl_from_for_scalar! {
    Facing1 for [f32, f64, i32] {
        X0  => -1,
        X1 =>  1,
    }
}

pub type Facings1 = EnumSet<Facing1>;

/// A two-dimensional facing direction; left, right, up or down.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Enum, EnumSetType)]
#[enumset(no_super_impls)]
pub enum Facing2 {
    X0,
    X1,
    Y0,
    Y1,
}

impl Facing2 {
    /// Returns the opposite facing direction.
    pub fn flipped(self) -> Self {
        match self {
            Self::X0 => Self::X1,
            Self::X1 => Self::X0,
            Self::Y0 => Self::Y1,
            Self::Y1 => Self::Y0,
        }
    }

    /// Returns the axis which the facing direction is along.
    pub fn axis(self) -> Axis2 {
        match self {
            Facing2::X0 | Facing2::X1 => Axis2::X,
            Facing2::Y0 | Facing2::Y1 => Axis2::Y,
        }
    }
}

impl From<Axis2> for Facing2 {
    fn from(value: Axis2) -> Self {
        match value {
            Axis2::X => Self::X1,
            Axis2::Y => Self::Y1,
        }
    }
}

impl_from_for_vec! {
    Facing2 for [Vec2, DVec2, IVec2] {
        X0 => (-1,  0),
        X1 => ( 1,  0),
        Y0 => ( 0, -1),
        Y1 => ( 0,  1),
    }
}

pub type Facings2 = EnumSet<Facing2>;

/// A three-dimensional facing direction; left, right, up, down, back or front.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Enum, EnumSetType)]
#[enumset(no_super_impls)]
pub enum Facing3 {
    X0,
    X1,
    Y0,
    Y1,
    Z0,
    Z1,
}

impl Facing3 {
    /// Returns the opposite facing direction.
    pub fn flipped(self) -> Self {
        match self {
            Self::X0 => Self::X1,
            Self::X1 => Self::X0,
            Self::Y0 => Self::Y1,
            Self::Y1 => Self::Y0,
            Self::Z0 => Self::Z1,
            Self::Z1 => Self::Z0,
        }
    }

    /// Performs the cross product between two facing directions.
    pub fn cross(self, other: Self) -> Option<Self> {
        IVec3::from(self).cross(other.into()).try_into().ok()
    }

    /// Whether the facing direction points in the positive direction.
    pub fn is_positive(self) -> bool {
        matches!(self, Self::X1 | Self::Y1 | Self::Z1)
    }

    /// Whether the facing direction points in the negative direction.
    pub fn is_negative(self) -> bool {
        !self.is_positive()
    }

    /// Returns the axis which the facing direction is along.
    pub fn axis(self) -> Axis3 {
        match self {
            Facing3::X0 | Facing3::X1 => Axis3::X,
            Facing3::Y0 | Facing3::Y1 => Axis3::Y,
            Facing3::Z0 | Facing3::Z1 => Axis3::Z,
        }
    }
}

impl From<Axis3> for Facing3 {
    fn from(value: Axis3) -> Self {
        match value {
            Axis3::X => Self::X1,
            Axis3::Y => Self::Y1,
            Axis3::Z => Self::Z1,
        }
    }
}

impl_from_for_vec! {
    Facing3 for [Vec3, Vec3A, DVec3, IVec3] {
        X0 => (-1,  0,  0),
        X1 => ( 1,  0,  0),
        Y0 => ( 0, -1,  0),
        Y1 => ( 0,  1,  0),
        Z0 => ( 0,  0, -1),
        Z1 => ( 0,  0,  1),
    }
}

pub type Facings3 = EnumSet<Facing3>;

/// A corner of a 2D square.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Enum, EnumSetType)]
#[enumset(no_super_impls)]
pub enum Corner2 {
    X0Y0,
    X1Y0,
    X0Y1,
    X1Y1,
}

impl From<Facing2> for Corners2 {
    fn from(value: Facing2) -> Self {
        match value {
            Facing2::X0 => Corner2::X0Y0 | Corner2::X0Y1,
            Facing2::X1 => Corner2::X1Y0 | Corner2::X1Y1,
            Facing2::Y0 => Corner2::X0Y0 | Corner2::X1Y0,
            Facing2::Y1 => Corner2::X0Y1 | Corner2::X1Y1,
        }
    }
}

impl_from_for_vec! {
    Corner2 for [Vec2, DVec2, IVec2, UVec2] {
        X0Y0 => (0, 0),
        X1Y0 => (1, 0),
        X0Y1 => (0, 1),
        X1Y1 => (1, 1),
    }
}

pub type Corners2 = EnumSet<Corner2>;

/// A corner of a 3D cube.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Enum, EnumSetType)]
#[enumset(no_super_impls)]
pub enum Corner3 {
    X0Y0Z0,
    X1Y0Z0,
    X0Y1Z0,
    X1Y1Z0,
    X0Y0Z1,
    X1Y0Z1,
    X0Y1Z1,
    X1Y1Z1,
}

impl Corner3 {
    /// Returns the corner flipped along the given axis.
    ///
    /// E.g. flipping [`Corner3::X0Y0Z0`] along [`Axis3::X`] returns
    /// [`Corner3::X1Y0Z0`].
    pub fn flipped(self, axis: Axis3) -> Corner3 {
        match (self as u8) ^ (1 << axis as u8) {
            0 => Self::X0Y0Z0,
            1 => Self::X1Y0Z0,
            2 => Self::X0Y1Z0,
            3 => Self::X1Y1Z0,
            4 => Self::X0Y0Z1,
            5 => Self::X1Y0Z1,
            6 => Self::X0Y1Z1,
            7 => Self::X1Y1Z1,
            _ => unreachable!(), // should get optimized away
        }
    }

    /// Extracts the facing direction for a single axis of the [`Corner3`].
    ///
    /// I.e. when starting from the center of a cube, which direction (along the given axis) would
    /// you need to move to get closer to the [`Corner3`].
    pub fn facing_along_axis(self, axis: Axis3) -> Facing3 {
        if (self as u8) & (1 << axis as u8) == 0 {
            match axis {
                Axis3::X => Facing3::X0,
                Axis3::Y => Facing3::Y0,
                Axis3::Z => Facing3::Z0,
            }
        } else {
            match axis {
                Axis3::X => Facing3::X1,
                Axis3::Y => Facing3::Y1,
                Axis3::Z => Facing3::Z1,
            }
        }
    }

    /// Returns the axis that the two corners make up.
    ///
    /// Returns [`None`] if the points are the same or diagnoal.
    pub fn axis_corner(self, other: Corner3) -> Option<Axis3> {
        match self as u8 ^ other as u8 {
            0b001 => Some(Axis3::X),
            0b010 => Some(Axis3::Y),
            0b100 => Some(Axis3::Z),
            _ => None,
        }
    }

    /// Returns the direction you would have to move to get to `other`.
    ///
    /// Returns [`None`] if the points are the same or if movement along multiple axes is necessary.
    pub fn facing_corner(self, other: Corner3) -> Option<Facing3> {
        self.axis_corner(other).map(|axis| {
            if (self as u8 ^ other as u8) & self as u8 == 0 {
                Facing3::from(axis).flipped()
            } else {
                axis.into()
            }
        })
    }

    /// Whether the number of single axis moves required reach [`Corner3::X0Y0Z0`] is even.
    pub fn is_even(self) -> bool {
        matches!(
            self,
            Corner3::X0Y0Z0 | Corner3::X1Y1Z0 | Corner3::X0Y1Z1 | Corner3::X1Y0Z1
        )
    }

    /// Whether the number of single axis moves required reach [`Corner3::X0Y0Z0`] is odd.
    pub fn is_odd(self) -> bool {
        !self.is_even()
    }

    /// Whether the two corners are connected by an edge.
    pub fn connects_to(self, other: Self) -> bool {
        self.axis_corner(other).is_some()
    }
}

impl_from_for_vec! {
    Corner3 for [Vec3, Vec3A, DVec3, IVec3, UVec3] {
        X0Y0Z0 => (0, 0, 0),
        X1Y0Z0 => (1, 0, 0),
        X0Y1Z0 => (0, 1, 0),
        X1Y1Z0 => (1, 1, 0),
        X0Y0Z1 => (0, 0, 1),
        X1Y0Z1 => (1, 0, 1),
        X0Y1Z1 => (0, 1, 1),
        X1Y1Z1 => (1, 1, 1),
    }
}

pub type Corners3 = EnumSet<Corner3>;

impl From<Facing3> for Corners3 {
    fn from(value: Facing3) -> Self {
        match value {
            Facing3::X0 => Corner3::X0Y0Z0 | Corner3::X0Y1Z0 | Corner3::X0Y0Z1 | Corner3::X0Y1Z1,
            Facing3::X1 => Corner3::X1Y0Z0 | Corner3::X1Y1Z0 | Corner3::X1Y0Z1 | Corner3::X1Y1Z1,
            Facing3::Y0 => Corner3::X0Y0Z0 | Corner3::X1Y0Z0 | Corner3::X0Y0Z1 | Corner3::X1Y0Z1,
            Facing3::Y1 => Corner3::X0Y1Z0 | Corner3::X1Y1Z0 | Corner3::X0Y1Z1 | Corner3::X1Y1Z1,
            Facing3::Z0 => Corner3::X0Y0Z0 | Corner3::X1Y0Z0 | Corner3::X0Y1Z0 | Corner3::X1Y1Z0,
            Facing3::Z1 => Corner3::X0Y0Z1 | Corner3::X1Y0Z1 | Corner3::X0Y1Z1 | Corner3::X1Y1Z1,
        }
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Enum, EnumSetType)]
#[enumset(no_super_impls)]
pub enum Edge2 {
    X0,
    X1,
    Y0,
    Y1,
}

pub type Edges2 = EnumSet<Edge2>;

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Enum, EnumSetType)]
#[enumset(no_super_impls)]
pub enum Edge3 {
    X0Y0,
    X1Y0,
    X0Y1,
    X1Y1,
    Y0Z0,
    Y1Z0,
    Y0Z1,
    Y1Z1,
    X0Z1,
    X1Z1,
    X0Z0,
    X1Z0,
}

pub type Edges3 = EnumSet<Edge3>;

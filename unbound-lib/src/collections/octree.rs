use glam::{BVec3, IVec3, UVec3};

use crate::octree::{extent::OctreeExtent, OctreeChunk};

/// An octree of arbitrary size, including zero on all or only some axes.
///
/// Unlike [`OctreeChunk`], the size is not limited to powers of two. This is achieved by
/// introducing some padding to the underlying [`OctreeChunk`] that is not actually relevant to the
/// [`Octree`] itself.
///
/// Any point that is less than `min_padding` or greater than `max_padding` on _any_ axis contains
/// padding, which is not part of the relevant data in the [`Octree`].
pub struct Octree<T, P = (), C = ()>(Repr<T, P, C>);

impl<T, P, C> Octree<T, P, C> {
    const EMPTY: Self = Octree(Repr::Empty { size: UVec3::ZERO });

    /// The maximum size of an [`Octree`].
    ///
    /// While hidden from the user, this includes padding, so the actual maximum (with worst-case
    /// padding) is only half this size plus one.
    const MAX_SIZE: UVec3 = OctreeExtent::MAX.size();

    /// Constructs a new [`Octree`] of specified size, filled with the given value.
    ///
    /// # Panics
    ///
    /// Panics if `size` is greater than [`OctreeExtent::MAX`] on any axis.
    pub fn new(size: UVec3, value: T) -> Self {
        Self::with_growth_hint(size, value, GrowthHint::default())
    }

    /// Constructs a new [`Octree`] with a specific `growth_hint`.
    ///
    /// If the octree is known to mostly expand into the positive direction (similar to a [`Vec`]),
    /// [`GrowthHint::Positive`] might make sense.
    ///
    /// Note, that the growth hint is only relevant for side lengths that are _not_ powers of two.
    ///
    /// # Panics
    ///
    /// Panics if `size` is greater than [`OctreeExtent::MAX`] on any axis.
    pub fn with_growth_hint(size: UVec3, value: T, growth_hint: GrowthHint) -> Self {
        if size.cmpeq(UVec3::ZERO).any() {
            Self(Repr::Empty { size })
        } else {
            let extent = OctreeExtent::from_size(size).expect(OCTREE_SIZE_ERROR);
            let padding = extent.size() - size;
            let (min_padding, max_padding) = growth_hint.split_padding(padding);
            Self(Repr::Octree {
                octree: OctreeChunk::new(extent, value),
                min_padding,
                max_padding,
            })
        }
    }

    pub fn size(&self) -> UVec3 {
        match &self.0 {
            Repr::Empty { size } => *size,
            Repr::Octree {
                octree,
                min_padding,
                max_padding,
            } => octree.extent().size() - *min_padding - *max_padding,
        }
    }

    pub fn truncate(self, bounds: UBounds3) -> Self {}

    pub fn resize(self, bounds: IBounds3, fill: T) -> Self {}
}

struct IBounds3 {
    origin: IVec3,
    size: UVec3,
}

struct UBounds3 {
    origin: UVec3,
    size: UVec3,
}

enum Repr<T, P, C> {
    Empty {
        /// At least one of the components is guaranteed to be `0`.
        size: UVec3,
    },
    Octree {
        /// Holds the data of the [`Octree`].
        octree: OctreeChunk<T, P, C>,
        /// Amount of padding relative to the origin of the `octree`.
        min_padding: UVec3,
        /// Amount of padding relative to the opposite of the origin of the `octree`.
        ///
        /// Note, that this is [`UVec3::ZERO`] if there is no padding, since it is relative to the
        /// _opposite_ of the origin rather than to the real origin.
        max_padding: UVec3,
    },
}

#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum GrowthHint {
    /// Optimize padding (if necessary) for growth in any direction.
    #[default]
    Any,
    /// Optimize padding (if necessary) for growth into the positive direction.
    Positive,
    /// Optimize padding (if necessary) for growth into the negative direction.
    Negative,
}

impl GrowthHint {
    fn split_padding(self, padding: UVec3) -> (UVec3, UVec3) {
        match self {
            Self::Any => (padding / 2, (padding + 1) / 2),
            Self::Positive => (UVec3::ZERO, padding),
            Self::Negative => (padding, UVec3::ZERO),
        }
    }
}

const OCTREE_SIZE_ERROR: &str = "size should be at most OctreeExtent::MAX on all axes";

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn split_padding_any() {
        let padding = UVec3::new(3, 5, 7);
        let (min_padding, max_padding) = GrowthHint::Any.split_padding(padding);
        assert_eq!(min_padding, UVec3::new(1, 2, 3));
        assert_eq!(max_padding, UVec3::new(2, 3, 4));
    }

    #[test]
    fn split_padding_positive() {
        let padding = UVec3::new(3, 5, 7);
        let (min_padding, max_padding) = GrowthHint::Positive.split_padding(padding);
        assert_eq!(min_padding, UVec3::ZERO);
        assert_eq!(max_padding, padding);
    }

    #[test]
    fn split_padding_negative() {
        let padding = UVec3::new(3, 5, 7);
        let (min_padding, max_padding) = GrowthHint::Negative.split_padding(padding);
        assert_eq!(min_padding, padding);
        assert_eq!(max_padding, UVec3::ZERO);
    }
}

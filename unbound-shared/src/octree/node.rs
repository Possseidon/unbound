use enum_map::EnumMap;
use itertools::{repeat_n, Itertools};
use replace_with::replace_with_or_abort;

use super::octant::{Octant, OctantCorners};
use crate::math_enums::Corner3;

/// A node within an octree holding either a value or a split into 8 octants which are also nodes.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub(crate) enum Node<T> {
    /// A leaf node holding a `T`.
    Value(T),
    /// A node that was split into 8 octants.
    Octants(Octants<T>),
}

impl<T: Default> Default for Node<T> {
    fn default() -> Self {
        Self::Value(Default::default())
    }
}

impl<T> Node<T> {
    /// Returns the value at the specified octant.
    ///
    /// Returns [`None`] if the specified octant contains multiple different values.
    pub(crate) fn get(&self, octant: Octant) -> Option<&T> {
        let mut current = self;
        let mut corners = octant.into_iter();
        loop {
            match current {
                Node::Value(value) => break Some(value),
                Node::Octants(octants) => current = &octants[corners.next()?],
            }
        }
    }

    /// Sets the specified `octant` to the provided `value`.
    ///
    /// Returns `true` if the parent node must be checked for merging.
    pub(crate) fn set(&mut self, mut corners: OctantCorners, value: T) -> bool
    where
        T: Clone + PartialEq,
    {
        if let Self::Value(existing_value) = self {
            if existing_value != &value {
                if corners.len() == 0 {
                    *self = Self::Value(value);
                    true
                } else {
                    *self.multi_split(corners) = Self::Value(value);
                    false
                }
            } else {
                false
            }
        } else if let Some(corner) = corners.next() {
            if self.octants_mut()[corner].set(corners, value) {
                if let Ok(Some(_)) = self
                    .octants()
                    .values()
                    .map(Self::as_value)
                    .all_equal_value()
                {
                    replace_with_or_abort(self, |node| {
                        Self::Value(
                            node.into_octants()
                                .into_values()
                                .next()
                                .unwrap()
                                .into_value(),
                        )
                    });
                    true
                } else {
                    false
                }
            } else {
                false
            }
        } else {
            *self = Self::Value(value);
            true
        }
    }

    /// Splits along the given corners and returns a mutable reference to the final [`Node`].
    ///
    /// # Panics
    ///
    /// Panics if the initial octant is already split.
    fn multi_split(&mut self, corners: OctantCorners) -> &mut Self
    where
        T: Clone,
    {
        assert!(matches!(self, Self::Value(_)));
        let mut current = self;
        for corner in corners {
            current.split();
            current = if let Self::Octants(octants) = current {
                &mut octants[corner]
            } else {
                unreachable!()
            };
        }
        current
    }

    /// Splits the node into 8 octants, [cloning](Clone) the original value.
    ///
    /// # Panics
    ///
    /// Panics if the octant is already split.
    fn split(&mut self)
    where
        T: Clone,
    {
        if let Self::Value(_) = self {
            replace_with_or_abort(self, |node| {
                Self::Octants(Box::new(EnumMap::from_array(
                    array_init::from_iter(repeat_n(node.into_value(), 8).map(Self::Value)).unwrap(),
                )))
            });
        } else {
            panic!("node already split");
        }
    }

    fn as_value(&self) -> Option<&T> {
        if let Self::Value(value) = self {
            Some(value)
        } else {
            None
        }
    }

    /// Consumes `self` and returns its contained value.
    ///
    /// # Panics
    ///
    /// Panics if the node doesn't hold a value.
    fn into_value(self) -> T {
        if let Self::Value(value) = self {
            value
        } else {
            panic!("should contain a value")
        }
    }

    fn into_octants(self) -> Octants<T> {
        if let Self::Octants(octants) = self {
            octants
        } else {
            panic!("should contain a split")
        }
    }

    /// Returns a reference to the octants of a split node.
    ///
    /// # Panics
    ///
    /// Panics if the node is not split.
    fn octants(&self) -> &Octants<T> {
        if let Self::Octants(octants) = self {
            octants
        } else {
            panic!("should contain a split")
        }
    }

    /// Returns a mutable reference to the octants of a split node.
    ///
    /// # Panics
    ///
    /// Panics if the node is not split.
    fn octants_mut(&mut self) -> &mut Octants<T> {
        if let Self::Octants(octants) = self {
            octants
        } else {
            panic!("should contain a split")
        }
    }
}

pub(crate) type Octants<T> = Box<EnumMap<Corner3, Node<T>>>;

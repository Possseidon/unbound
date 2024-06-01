mod node;
pub mod octant;

use node::Node;
use octant::Octant;

/// An octree storing values of type `T`.
///
/// Implemented as a recursive data structure, which makes modifications very straightforward, but
/// is not very efficient in terms of memory usage and cache locality.
#[derive(Clone, Debug, Default, Hash, PartialEq, Eq)]
pub struct Octree<T> {
    root: Node<T>,
}

impl<T> Octree<T> {
    /// Wraps the given `value` in an [`Octree`].
    pub fn new(value: T) -> Self {
        Self {
            root: Node::Value(value),
        }
    }

    /// Returns the value at the specified octant.
    ///
    /// Returns [`None`] if the specified octant contains multiple different values.
    pub fn get(&self, octant: Octant) -> Option<&T> {
        self.root.get(octant)
    }

    /// Sets the specified `octant` to the provided `value`.
    ///
    /// Values must be [`Clone`], since values are duplicated when an octant is split.
    ///
    /// Values must be [`PartialEq`], since octants merge back together if all its child nodes have
    /// the same value.
    pub fn set(&mut self, octant: Octant, value: T)
    where
        T: Clone + PartialEq,
    {
        // ignore the return value regarding merging; the root has no parent
        self.root.set(octant.into_iter(), value);
    }
}

#[cfg(test)]
mod tests {
    use octant::OctreeDepth;

    use super::*;
    use crate::math_enums::Corner3;

    #[test]
    fn octree_new() {
        let octree = Octree::new(0);
        assert_eq!(octree.get(Octant::ROOT), Some(&0));
    }

    #[test]
    fn octree_set_root() {
        let mut octree = Octree::new(0);
        octree.set(Octant::ROOT, 1);
        assert_eq!(octree.get(Octant::ROOT), Some(&1));
    }

    #[test]
    fn octree_set_corner() {
        let mut octree = Octree::new(0);
        octree.set(Octant::new(Corner3::X0Y0Z0), 1);
        assert_eq!(octree.get(Octant::new(Corner3::X0Y0Z0)), Some(&1));
        assert_eq!(octree.get(Octant::new(Corner3::X0Y0Z1)), Some(&0));
    }

    #[test]
    fn octree_get_none() {
        let mut octree = Octree::new(0);
        octree.set(Octant::new(Corner3::X0Y0Z0), 1);
        // should return None, since the root node contains both 0 and 1
        assert_eq!(octree.get(Octant::ROOT), None);
    }

    #[test]
    fn octree_set_merges() {
        let mut octree = Octree::new(0);
        octree.set(Octant::new(Corner3::X0Y0Z0), 1);
        octree.set(Octant::new(Corner3::X0Y0Z0), 0);
        // make sure the octant was actually merged; just checking the value is not enough
        assert!(matches!(octree.root, Node::Value(0)))
    }

    #[test]
    fn octree_set_deep() {
        let mut octree = Octree::new(0);
        let a = Octant::new(Corner3::X0Y0Z0);
        let b = a.with_corner(Corner3::X0Y0Z0);
        octree.set(b, 1);
        assert_eq!(octree.get(b), Some(&1));
        assert_eq!(octree.get(a), None);
        assert_eq!(octree.get(a.with_corner(Corner3::X0Y0Z1)), Some(&0));
    }

    #[test]
    fn octree_set_max_depth() {
        // fills an octree with values according to the current depth
        // - starts out with an octree containing 0
        // - puts a 1 in the first octant
        // - puts a 2 in that octants first octant
        // - etc...

        let mut octree = Octree::new(0);
        let mut octant = Octant::ROOT;
        for i in 1..=OctreeDepth::MAX.get() {
            octant = octant.with_corner(Corner3::X0Y0Z0);
            octree.set(octant, i);
        }

        let mut octant = Octant::ROOT;
        for i in 0..OctreeDepth::MAX.get() {
            assert_eq!(octree.get(octant), None);
            assert_eq!(octree.get(octant.with_corner(Corner3::X0Y0Z1)), Some(&i));
            octant = octant.with_corner(Corner3::X0Y0Z0);
        }

        assert_eq!(octree.get(octant), Some(&OctreeDepth::MAX.get()));
    }
}

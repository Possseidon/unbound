pub mod iterator;
mod node;
pub mod octant;

use iterator::{OctreeIterator, OctreeValueIterator};
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
    pub const fn new(value: T) -> Self {
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

    /// Returns an iterator over all values with their respective octant.
    ///
    /// Iterated octants are relative to the given `octant`.
    ///
    /// Iteration order is by octant.
    ///
    /// This also works if the given `octant` does not exist as an actual leaf node. The iterator
    /// will simply yield [`Octant::ROOT`] with the corresponding value.
    pub fn values(&self, octant: Octant) -> OctreeValueIterator<T> {
        match self.root.find_node(octant) {
            Ok(node) => OctreeValueIterator::iterator(node),
            Err((_, _, value)) => OctreeValueIterator::value(value),
        }
    }
}

impl<'a, T> IntoIterator for &'a Octree<T> {
    type Item = (Octant, &'a T);
    type IntoIter = OctreeIterator<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        OctreeIterator::new(&self.root)
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

    #[test]
    fn octree_iterate_homogeneous() {
        let octree = Octree::new(0);

        let mut iter = octree.into_iter();
        assert_eq!(iter.next(), Some((Octant::ROOT, &0)));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn octree_iterate() {
        let mut octree = Octree::new(0);
        octree.set(Octant::new(Corner3::X0Y0Z0), 1);
        octree.set(Octant::new(Corner3::X0Y1Z0), 2);

        let mut iter = octree.into_iter();
        assert_eq!(iter.next(), Some((Octant::new(Corner3::X0Y0Z0), &1)));
        assert_eq!(iter.next(), Some((Octant::new(Corner3::X1Y0Z0), &0)));
        assert_eq!(iter.next(), Some((Octant::new(Corner3::X0Y1Z0), &2)));
        assert_eq!(iter.next(), Some((Octant::new(Corner3::X1Y1Z0), &0)));
        assert_eq!(iter.next(), Some((Octant::new(Corner3::X0Y0Z1), &0)));
        assert_eq!(iter.next(), Some((Octant::new(Corner3::X1Y0Z1), &0)));
        assert_eq!(iter.next(), Some((Octant::new(Corner3::X0Y1Z1), &0)));
        assert_eq!(iter.next(), Some((Octant::new(Corner3::X1Y1Z1), &0)));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn octree_iterate_deeper() {
        let mut octree = Octree::new(0);
        let a = Octant::new(Corner3::X0Y1Z0);
        let b = a.with_corner(Corner3::X0Y0Z1);
        octree.set(a, 1);
        octree.set(b, 2);

        let mut iter = octree.into_iter();
        assert_eq!(iter.next(), Some((Octant::new(Corner3::X0Y0Z0), &0)));
        assert_eq!(iter.next(), Some((Octant::new(Corner3::X1Y0Z0), &0)));
        assert_eq!(iter.next(), Some((a.with_corner(Corner3::X0Y0Z0), &1)));
        assert_eq!(iter.next(), Some((a.with_corner(Corner3::X1Y0Z0), &1)));
        assert_eq!(iter.next(), Some((a.with_corner(Corner3::X0Y1Z0), &1)));
        assert_eq!(iter.next(), Some((a.with_corner(Corner3::X1Y1Z0), &1)));
        assert_eq!(iter.next(), Some((b, &2)));
        assert_eq!(iter.next(), Some((a.with_corner(Corner3::X1Y0Z1), &1)));
        assert_eq!(iter.next(), Some((a.with_corner(Corner3::X0Y1Z1), &1)));
        assert_eq!(iter.next(), Some((a.with_corner(Corner3::X1Y1Z1), &1)));
        assert_eq!(iter.next(), Some((Octant::new(Corner3::X1Y1Z0), &0)));
        assert_eq!(iter.next(), Some((Octant::new(Corner3::X0Y0Z1), &0)));
        assert_eq!(iter.next(), Some((Octant::new(Corner3::X1Y0Z1), &0)));
        assert_eq!(iter.next(), Some((Octant::new(Corner3::X0Y1Z1), &0)));
        assert_eq!(iter.next(), Some((Octant::new(Corner3::X1Y1Z1), &0)));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn octree_iterate_max_depth() {
        let mut max_depth_octant = Octant::ROOT;
        for _ in 0..OctreeDepth::MAX.get() {
            max_depth_octant = max_depth_octant.with_corner(Corner3::X0Y0Z0);
        }
        let mut octree = Octree::new(0);
        octree.set(max_depth_octant, 1);

        let mut iter = octree.into_iter();
        assert_eq!(iter.next(), Some((max_depth_octant, &1)));
        assert_eq!(iter.count(), usize::from(OctreeDepth::MAX.get()) * 7);
        assert!(iter.all(|(_, i)| i == &0));
    }

    #[test]
    fn octree_iterate_bidirectional() {
        let mut octree = Octree::new(0);
        let a = Octant::new(Corner3::X0Y1Z0);
        let b = a.with_corner(Corner3::X0Y0Z1);
        octree.set(a, 1);
        octree.set(b, 2);

        let mut iter = octree.into_iter();
        assert_eq!(iter.next(), Some((Octant::new(Corner3::X0Y0Z0), &0)));
        assert_eq!(iter.next(), Some((Octant::new(Corner3::X1Y0Z0), &0)));
        assert_eq!(iter.next_back(), Some((Octant::new(Corner3::X1Y1Z1), &0)));
        assert_eq!(iter.next_back(), Some((Octant::new(Corner3::X0Y1Z1), &0)));
        assert_eq!(iter.next_back(), Some((Octant::new(Corner3::X1Y0Z1), &0)));
        assert_eq!(iter.next_back(), Some((Octant::new(Corner3::X0Y0Z1), &0)));
        assert_eq!(iter.next(), Some((a.with_corner(Corner3::X0Y0Z0), &1)));
        assert_eq!(iter.next(), Some((a.with_corner(Corner3::X1Y0Z0), &1)));
        assert_eq!(iter.next(), Some((a.with_corner(Corner3::X0Y1Z0), &1)));
        assert_eq!(iter.next_back(), Some((Octant::new(Corner3::X1Y1Z0), &0)));
        assert_eq!(iter.next_back(), Some((a.with_corner(Corner3::X1Y1Z1), &1)));
        assert_eq!(iter.next(), Some((a.with_corner(Corner3::X1Y1Z0), &1)));
        assert_eq!(iter.next_back(), Some((a.with_corner(Corner3::X0Y1Z1), &1)));
        assert_eq!(iter.next(), Some((b, &2)));
        assert_eq!(iter.next(), Some((a.with_corner(Corner3::X1Y0Z1), &1)));
        assert_eq!(iter.next_back(), None);
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn octree_values_root() {
        let mut octree = Octree::new(0);
        octree.set(Octant::new(Corner3::X0Y0Z0), 1);

        let mut iter = octree.values(Octant::ROOT);
        assert_eq!(iter.next(), Some((Octant::new(Corner3::X0Y0Z0), &1)));
        assert_eq!(iter.next(), Some((Octant::new(Corner3::X1Y0Z0), &0)));
        assert_eq!(iter.next(), Some((Octant::new(Corner3::X0Y1Z0), &0)));
        assert_eq!(iter.next(), Some((Octant::new(Corner3::X1Y1Z0), &0)));
        assert_eq!(iter.next(), Some((Octant::new(Corner3::X0Y0Z1), &0)));
        assert_eq!(iter.next(), Some((Octant::new(Corner3::X1Y0Z1), &0)));
        assert_eq!(iter.next(), Some((Octant::new(Corner3::X0Y1Z1), &0)));
        assert_eq!(iter.next(), Some((Octant::new(Corner3::X1Y1Z1), &0)));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn octree_values_partial() {
        let mut octree = Octree::new(0);
        let a = Octant::new(Corner3::X0Y0Z0);
        let b = a.with_corner(Corner3::X1Y0Z0);
        octree.set(a, 1);
        octree.set(b, 2);

        // yielded octants are relative to `a`
        let mut iter = octree.values(a);
        assert_eq!(iter.next(), Some((Octant::new(Corner3::X0Y0Z0), &1)));
        assert_eq!(iter.next(), Some((Octant::new(Corner3::X1Y0Z0), &2)));
        assert_eq!(iter.next(), Some((Octant::new(Corner3::X0Y1Z0), &1)));
        assert_eq!(iter.next(), Some((Octant::new(Corner3::X1Y1Z0), &1)));
        assert_eq!(iter.next(), Some((Octant::new(Corner3::X0Y0Z1), &1)));
        assert_eq!(iter.next(), Some((Octant::new(Corner3::X1Y0Z1), &1)));
        assert_eq!(iter.next(), Some((Octant::new(Corner3::X0Y1Z1), &1)));
        assert_eq!(iter.next(), Some((Octant::new(Corner3::X1Y1Z1), &1)));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn octree_values_not_split() {
        let mut octree = Octree::new(0);
        let a = Octant::new(Corner3::X0Y0Z0);
        octree.set(a, 1);

        let mut iter = octree.values(a.with_corner(Corner3::X0Y0Z0));
        assert_eq!(iter.next(), Some((Octant::ROOT, &1)));
        assert_eq!(iter.next(), None);
    }
}

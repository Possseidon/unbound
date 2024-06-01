use derive_where::derive_where;

use super::{
    node::{Node, Octants},
    octant::Octant,
};
use crate::math_enums::Corner3;

/// An iterator over [`Octant`]s and their values within an octree.
#[derive(Debug)]
#[derive_where(Clone, Copy)]
pub struct OctreeIterator<'a, T> {
    /// The root node that is being iterated.
    root: &'a Node<T>,
    /// The head and tail octants.
    ///
    /// [`None`] if iteration has finished.
    position: Option<Position<'a, T>>,
}

impl<'a, T> OctreeIterator<'a, T> {
    pub(crate) fn new(root: &'a Node<T>) -> Self {
        Self {
            root,
            position: Some(Position {
                head: OctantNode::find::<false>(root),
                tail: OctantNode::find::<true>(root),
            }),
        }
    }
}

impl<'a, T> Iterator for OctreeIterator<'a, T> {
    type Item = (Octant, &'a T);

    fn next(&mut self) -> Option<Self::Item> {
        let position = self.position.as_mut()?;
        let result = Some((position.head.octant, position.head.value));
        if position.head.octant != position.tail.octant {
            position.head.move_next::<false>(self.root);
        } else {
            self.position = None;
        }
        result
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        if self.position.is_some() {
            (1, None)
        } else {
            (0, Some(0))
        }
    }
}

impl<'a, T> DoubleEndedIterator for OctreeIterator<'a, T> {
    fn next_back(&mut self) -> Option<Self::Item> {
        let position = self.position.as_mut()?;
        let result = Some((position.tail.octant, position.tail.value));
        if position.head.octant != position.tail.octant {
            position.tail.move_next::<true>(self.root);
        } else {
            self.position = None;
        }
        result
    }
}

#[derive(Debug)]
#[derive_where(Clone, Copy)]
struct Position<'a, T> {
    /// The head octant, advanced by [`OctreeIterator::next`].
    head: OctantNode<'a, T>,
    /// The tail octant, advanced by [`OctreeIterator::next_back`].
    tail: OctantNode<'a, T>,
}

#[derive(Debug)]
#[derive_where(Clone, Copy)]
struct OctantNode<'a, T> {
    /// An octant within [`OctreeIterator::root`].
    octant: Octant,
    value: &'a T,
    /// Contains all sibling octants, including [`Self::octant`] itself.
    ///
    /// [`None`] if [`Self::octant`] is [`Octant::ROOT`], since the root node has no siblings.
    ///
    /// This makes it possible to find sibling octants without having to go through
    /// [`OctreeIterator::root`]. It is however merely an optimization and not strictly necessary.
    ///
    /// However it does _not_ make it easier to find the parent of the octant to go back up a
    /// level. That still requires going through [`OctreeIterator::root`].
    siblings: Option<&'a Octants<T>>,
}

impl<'a, T> OctantNode<'a, T> {
    /// Finds the first (or last) [`Node::Value`] in the octree.
    fn find<const REVERSE: bool>(mut node: &'a Node<T>) -> Self {
        let corner = if REVERSE {
            Corner3::X1Y1Z1
        } else {
            Corner3::X0Y0Z0
        };

        let mut octant = Octant::ROOT;
        let mut siblings = None;
        loop {
            match node {
                Node::Value(value) => {
                    break Self {
                        octant,
                        value,
                        siblings,
                    }
                }
                Node::Octants(octants) => {
                    siblings = Some(octants);
                    node = &octants[corner];
                    octant = octant.with_corner(corner);
                }
            }
        }
    }

    /// Assumes that there is a next [`Octant`] and advances to it.
    fn move_next<const REVERSE: bool>(&mut self, root: &'a Node<T>) {
        let corner = if REVERSE {
            Corner3::X1Y1Z1
        } else {
            Corner3::X0Y0Z0
        };

        self.octant = if let Some(next_octant) = self.octant.next_neighbor::<REVERSE>() {
            next_octant
        } else {
            let next_octant = self
                .octant
                .parent() // immediately call parent, since we know that next_neighbor was None
                .expect("should not be root")
                .next_octant::<REVERSE>()
                .expect("should not be the last octant");
            self.siblings = Some(
                root.find_node(next_octant.parent().expect("should not be root"))
                    .ok()
                    .expect("should exist")
                    .octants(),
            );
            next_octant
        };

        let next_corner = self.octant.last_corner().expect("should not be root");
        let mut node = &self.siblings.unwrap()[next_corner];
        loop {
            match node {
                Node::Value(value) => {
                    self.value = value;
                    break;
                }
                Node::Octants(octants) => {
                    self.siblings = Some(octants);
                    node = &octants[corner];
                    self.octant = self.octant.with_corner(corner);
                }
            }
        }
    }
}

/// An iterator over a specific [`Octant`]s within an octree.
///
/// Also has special handling for if the [`Octant`] is not split in the octree.
#[derive(Debug)]
#[derive_where(Clone, Copy)]
pub struct OctreeValueIterator<'a, T>(OctreeValueIteratorImpl<'a, T>);

impl<'a, T> OctreeValueIterator<'a, T> {
    pub(crate) fn iterator(root: &'a Node<T>) -> Self {
        Self(OctreeValueIteratorImpl::Iterator(OctreeIterator::new(root)))
    }

    pub(crate) fn value(value: &'a T) -> Self {
        Self(OctreeValueIteratorImpl::Value(Some(value)))
    }
}

impl<'a, T> Iterator for OctreeValueIterator<'a, T> {
    type Item = (Octant, &'a T);

    fn next(&mut self) -> Option<Self::Item> {
        match &mut self.0 {
            OctreeValueIteratorImpl::Iterator(iterator) => iterator.next(),
            OctreeValueIteratorImpl::Value(value) => {
                value.take().map(|value| (Octant::ROOT, value))
            }
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        match &self.0 {
            OctreeValueIteratorImpl::Iterator(iterator) => iterator.size_hint(),
            OctreeValueIteratorImpl::Value(Some(_)) => (1, None),
            OctreeValueIteratorImpl::Value(None) => (0, Some(0)),
        }
    }
}

impl<'a, T> DoubleEndedIterator for OctreeValueIterator<'a, T> {
    fn next_back(&mut self) -> Option<Self::Item> {
        match &mut self.0 {
            OctreeValueIteratorImpl::Iterator(iterator) => iterator.next_back(),
            OctreeValueIteratorImpl::Value(value) => {
                value.take().map(|value| (Octant::ROOT, value))
            }
        }
    }
}

#[derive(Debug)]
#[derive_where(Clone, Copy)]
enum OctreeValueIteratorImpl<'a, T> {
    Iterator(OctreeIterator<'a, T>),
    Value(Option<&'a T>),
}

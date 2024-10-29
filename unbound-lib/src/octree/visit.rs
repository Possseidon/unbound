use derive_where::derive_where;
use glam::UVec3;

use super::{bounds::OctreeBounds, extent::OctreeSplits, NodeRef, OctreeNode, ParentNodeRef};
use crate::math::bounds::UBounds3;

pub trait Visit<T: OctreeNode> {
    /// Decides, whether the given `node` should be entered or not.
    ///
    /// As an optimization, for the common case where only a single child node is relevant,
    /// [`Enter::Only`] can return the index of that node.
    fn enter(&mut self, node: VisitNode<T>) -> Enter;
}

impl<T: OctreeNode, V: Visit<T>> Visit<T> for &mut V {
    fn enter(&mut self, node: VisitNode<T>) -> Enter {
        (*self).enter(node)
    }
}

#[derive_where(Clone, Copy)]
#[derive(Debug, Hash, PartialEq, Eq)]
pub struct VisitNode<'a, T> {
    pub(crate) node: ParentNodeRef<'a, T>,
    pub(crate) min: UVec3,
    pub(crate) splits: OctreeSplits,
}

impl<'a, T: OctreeNode> VisitNode<'a, T> {
    /// The node that is being visited.
    pub fn node(self) -> ParentNodeRef<'a, T> {
        self.node
    }

    /// The bounds of [`Self::node`].
    pub fn bounds(self) -> OctreeBounds {
        OctreeBounds::new(self.min, self.node.get().extent())
    }

    /// Describes the layout of [`Self::node`]'s children.
    ///
    /// [`OctreeSplits::NONE`] for leaf nodes.
    pub fn splits(self) -> OctreeSplits {
        self.splits
    }
}

pub trait VisitMut<T: OctreeNode> {
    /// Decides, whether the given `node` should be entered or not.
    ///
    /// As an optimization, for the common case where only a single child node is relevant,
    /// [`Enter::Only`] can return the index of that node.
    ///
    /// Despite being for mutable iteration, node mutations are intentionally not yet allowed here.
    fn enter(&mut self, node: VisitMutNode<T>) -> Enter;
}

impl<T: OctreeNode, V: VisitMut<T>> VisitMut<T> for &mut V {
    fn enter(&mut self, node: VisitMutNode<T>) -> Enter {
        (*self).enter(node)
    }
}

#[derive_where(Clone, Copy)]
#[derive_where(Debug, Hash, PartialEq, Eq; NodeRef<'a, T>)]
pub struct VisitMutNode<'a, T: OctreeNode> {
    node: NodeRef<'a, T>,
    /// Unlike [`VisitNode`], the whole bounds must be stored, since [`NodeRef::Leaf`] does not
    /// know its extent.
    bounds: OctreeBounds,
    splits: OctreeSplits,
}

impl<'a, T: OctreeNode> VisitMutNode<'a, T> {
    /// The node that is being visited.
    pub fn node(self) -> NodeRef<'a, T> {
        self.node
    }

    /// The bounds of [`Self::node`].
    pub fn bounds(self) -> OctreeBounds {
        self.bounds
    }

    /// Describes the layout of [`Self::node`]'s children.
    ///
    /// [`OctreeSplits::NONE`] for leaf nodes.
    pub fn splits(self) -> OctreeSplits {
        self.splits
    }
}

pub enum Enter {
    /// Skips the node without yielding any of its children.
    None,
    /// Yields only the child with the given index.
    Only { child: u8 },
    /// Yields all children.
    All,
}

impl Enter {
    /// Returns which child to enter based on the given `target` within `splits` of `bounds`.
    ///
    /// Keeps entering all children once `target` has been reached. Use [`Self::until`] to stop once
    /// the target encloses the node's bounds.
    fn within(bounds: OctreeBounds, splits: OctreeSplits, target: UBounds3) -> Self {
        if bounds.to_ubounds3().is_disjoint(target) {
            return Self::None;
        }

        let child_extent = bounds.extent().split(splits);

        let child_bounds = OctreeBounds::new_floored(target.lower(), child_extent);
        let upper_bounds_min = OctreeBounds::floor_min_to_extent(target.upper() - 1, child_extent);
        if child_bounds.min() != upper_bounds_min {
            return Self::All;
        }

        Self::Only {
            child: child_bounds.small_index_within(bounds.extent()),
        }
    }

    /// Returns which child to enter based on the given `target` within `splits` of `bounds`.
    ///
    /// Does not continue entering bounds once the nodes bounds are fully enclosed by `target`.
    fn until(bounds: OctreeBounds, splits: OctreeSplits, target: UBounds3) -> Self {
        if target.encloses(bounds.to_ubounds3()) {
            return Enter::None;
        }

        Self::within(bounds, splits, target)
    }
}

/// Iterates over all nodes in the octree.
pub struct AllLeaves;

impl<T: OctreeNode> Visit<T> for AllLeaves {
    fn enter(&mut self, _: VisitNode<T>) -> Enter {
        Enter::All
    }
}

impl<T: OctreeNode> VisitMut<T> for AllLeaves {
    fn enter(&mut self, node: VisitMutNode<T>) -> Enter {
        if node.node().data().is_parent() {
            Enter::All
        } else {
            Enter::None
        }
    }
}

/// Enters all nodes within [`Self::target`].
///
/// This includes all nodes that are fully enclosed by [`Self::target`].
pub struct WithinBounds {
    pub target: UBounds3,
}

impl<T: OctreeNode> Visit<T> for WithinBounds {
    fn enter(&mut self, node: VisitNode<T>) -> Enter {
        Enter::within(node.bounds(), node.splits, self.target)
    }
}

impl<T: OctreeNode> VisitMut<T> for WithinBounds {
    fn enter(&mut self, node: VisitMutNode<T>) -> Enter {
        Enter::within(node.bounds(), node.splits, self.target)
    }
}

/// Enters nodes to reach [`Self::target`] and then stops.
///
/// This is similar to [`WithinBounds`] but stops entering nodes that are fully enclosed by
/// [`Self::target`].
pub struct UntilBounds {
    pub target: UBounds3,
}

impl<T: OctreeNode> Visit<T> for UntilBounds {
    fn enter(&mut self, node: VisitNode<T>) -> Enter {
        Enter::until(node.bounds(), node.splits, self.target)
    }
}

impl<T: OctreeNode> VisitMut<T> for UntilBounds {
    fn enter(&mut self, node: VisitMutNode<T>) -> Enter {
        Enter::until(node.bounds(), node.splits, self.target)
    }
}

pub trait Split<T: OctreeNode> {
    /// Called whenever a leaf node is entered and split into a parent node.
    fn split(&mut self, bounds: OctreeBounds, leaf: T::LeafRef<'_>) -> T::Parent;
}

impl<T: OctreeNode, S: Split<T>> Split<T> for &mut S {
    fn split(&mut self, bounds: OctreeBounds, leaf: T::LeafRef<'_>) -> <T as OctreeNode>::Parent {
        (*self).split(bounds, leaf)
    }
}

pub struct SplitDefault;

impl<T: OctreeNode> Split<T> for SplitDefault
where
    T::Parent: Default,
{
    fn split(&mut self, _: OctreeBounds, _: T::LeafRef<'_>) -> T::Parent {
        Default::default()
    }
}

pub struct PanicOnSplit;

impl<T: OctreeNode> Split<T> for PanicOnSplit {
    fn split(&mut self, _: OctreeBounds, _: T::LeafRef<'_>) -> T::Parent {
        panic!("should not split")
    }
}

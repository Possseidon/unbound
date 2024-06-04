use bevy::math::UVec3;

use super::bounds::OctreeBounds;
use crate::change_tracking::Mut;

/// A visitor for an octree.
pub trait OctreeVisitor {
    /// The type of the value stored in the octree.
    type Value;

    /// Called for each value in the octree, i.e. leaf nodes in the octree.
    fn visit_value(&mut self, bounds: OctreeBounds, value: &Self::Value);

    /// Called when a split was ecountered, i.e. non-leaf nodes in the octree.
    ///
    /// Returns whether the split should be entered or skipped.
    fn visit_split(&mut self, bounds: OctreeBounds) -> VisitSplit;
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum VisitSplit {
    Skip,
    Enter,
}

pub trait OctreeVisitorMut {
    type Value;

    fn visit_single_value_mut(&mut self, pos: UVec3, value: &mut Mut<Self::Value>);

    fn visit_value_mut(&mut self, bounds: OctreeBounds, value: &mut Mut<Self::Value>)
        -> VisitValue;

    fn update_value(&mut self, bounds: OctreeBounds, value: &mut Self::Value);

    fn visit_split_mut(&mut self, bounds: OctreeBounds) -> VisitSplitMut<Self::Value>;
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum VisitValue {
    Next,
    Split,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum VisitSplitMut<T> {
    Skip,
    Enter,
    Fill(T),
}

#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum VisitMutResult {
    /// The octree was not changed.
    #[default]
    Unchanged,
    /// The octree was changed.
    Changed,
}

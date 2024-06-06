use crate::change_tracking::Mut;

/// Allows for efficient traversal over an octree.
pub trait OctreeVisitor {
    /// The type of the values stored in the octree.
    type Value;
    /// The type used to indicate specific regions within the octree.
    type Bounds;

    /// Called for bounds that have one homogeneous value.
    fn visit_bounds(&mut self, bounds: Self::Bounds, value: &Self::Value);
    /// Should return `true` if the visitor should enter the split at the given bounds.
    ///
    /// Likewise it should return `false` if the bounds are not of interest to the visitor.
    fn visit_split(&mut self, bounds: Self::Bounds) -> bool;
}

/// Allows for efficient traversal and modification of an octree.
pub trait OctreeVisitorMut {
    /// The type of the values stored in the octree.
    type Value;
    /// The type used to indicate specific regions within the octree.
    type Bounds;
    /// The type used to indicate specific points within the octree.
    type Pos;

    /// Called for values that can no longer be split.
    ///
    /// This prevents accidentally returning `true` from [`OctreeVisitorMut::visit_bounds_mut`] if
    /// the bounds can no longer be split.
    fn visit_value_mut(&mut self, pos: Self::Pos, value: &mut Mut<Self::Value>);
    /// Called for bounds that have one homogeneous value.
    ///
    /// Should return `true` if there is no longer one unique value for these bounds, upon which the
    /// bounds are split up.
    fn visit_bounds_mut(&mut self, bounds: Self::Bounds, value: &mut Mut<Self::Value>) -> bool;
    /// Should return whether the visitor should enter, skip or fill the split.
    fn visit_split_mut(&mut self, bounds: Self::Bounds) -> VisitSplit<Self::Value>;
}

/// Describes how the visitation should continue when a split was encountered.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum VisitSplit<T> {
    /// Skips this split.
    Skip,
    /// Enters this split, causing additional visit calls.
    Enter,
    /// Sets the split to one homogeneous value and does **not** enter it.
    Fill(T),
}

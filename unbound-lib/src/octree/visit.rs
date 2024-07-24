/// Allows for efficient traversal over an octree.
pub trait OctreeVisitor {
    /// The type of the values stored in the octree.
    type Value;
    /// The type used to indicate specific regions within the octree.
    type Bounds;

    /// Called for homogeneous bounds.
    fn visit_bounds(&mut self, bounds: Self::Bounds, value: &Self::Value);
    /// Called for heterogeneous bounds.
    ///
    /// Should return whether the visitor should skip or enter the split.
    fn visit_split(&mut self, bounds: Self::Bounds) -> VisitSplit;
}

/// Describes how the visitation should continue when a split was encountered.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum VisitSplit {
    /// Skips this split.
    Skip,
    /// Enters this split, causing additional visit calls.
    Enter,
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
    /// If [`Some`] is returned, `value` is updated, otherwise `value` remains unchanged.
    ///
    /// It is perfectly fine to return [`Some`] with a copy of the original `value`. The value will
    /// be checked for changes internally as a safe-guard.
    fn visit_value(&mut self, pos: Self::Pos, value: &Self::Value) -> Option<Self::Value>;
    /// Called for bounds.
    ///
    /// `value` is set if the bounds contain one homogeneous value.
    ///
    /// Should return whether the visitor should skip, fill or split the bounds.
    ///
    /// It is perfectly fine to return [`VisitBoundsMut::Fill`] with the same original `value`. The
    /// value will be checked for changes internally as a safe-guard.
    fn visit_bounds(
        &mut self,
        bounds: Self::Bounds,
        value: Option<&Self::Value>,
    ) -> VisitBoundsMut<Self::Value>;
}

/// Describes how the visitation should continue when homogeneous bounds were encountered.
///
/// Basically has the same structure as [`VisitSplitMut`] but with slightly different variant names
/// to make their semantic meaning more accurate.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum VisitBoundsMut<T> {
    /// Skips these bounds without modifying it.
    Skip,
    /// Fills the bounds with the value and then skips over it.
    Fill(T),
    /// Splits these bounds up, causing additional visit calls.
    Split,
}

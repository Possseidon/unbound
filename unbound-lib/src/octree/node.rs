use std::iter::repeat;

use array_init::map_array_init;
use glam::UVec3;
use itertools::zip_eq;

use super::{
    bounds::{OctreeBounds, OctreeBoundsSplit},
    extent::OctreeSplits,
    visit::{OctreeVisitor, OctreeVisitorMut, VisitBoundsMut, VisitSplit},
};

/// A node within an octree, either holding a value of type `T` or more [`Node`]s.
///
/// Unlike with regular octrees, nodes don't always split into a `2x2x2` of other nodes. Instead,
/// nodes can have any cuboid shape as long as:
///
/// - Its width, height and depth are all powers of two
/// - The volume doesn't exceed 64
///
/// So basically, any cuboid that can be split in half up to 6 times along any combination of axes.
///
/// # Variants
///
/// - `Value` is the basic "leaf" node of the octree that holds the actual values
/// - `Split<N>` holds `N` splits, resulting in `2^N` child nodes
/// - `Values<N>` is an optimization for `Split<N>` nodes that only hold values
///
/// Arrays of nodes (and values) are stored as `Box<[T; N]>` spread across different enum variants
/// to avoid fat pointers, increasing the total size of [`Node`] itself. This is admittedly a bit
/// annyoing to work with, but at least it also improves type-safety by preventing arrays of
/// non-power-of-two sizes.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub(crate) enum Node<T> {
    Value(T),
    Values2(Box<[T; 2]>),
    Values4(Box<[T; 4]>),
    Values8(Box<[T; 8]>),
    Values16(Box<[T; 16]>),
    Values32(Box<[T; 32]>),
    Values64(Box<[T; 64]>),
    Split2(Box<[Node<T>; 2]>),
    Split4(Box<[Node<T>; 4]>),
    Split8(Box<[Node<T>; 8]>),
    Split16(Box<[Node<T>; 16]>),
    Split32(Box<[Node<T>; 32]>),
    Split64(Box<[Node<T>; 64]>),
}

impl<T> Node<T> {
    /// Recursively visits all splits and values using the given `visitor`.
    pub(crate) fn visit(
        &self,
        visitor: &mut impl OctreeVisitor<Value = T, Bounds = OctreeBounds>,
        bounds: OctreeBounds,
        splits: &[OctreeSplits],
    ) {
        match self {
            Self::Value(value) => visitor.visit_bounds(bounds, value),
            Self::Values2(values) => Self::visit_values(visitor, bounds, values, splits),
            Self::Values4(values) => Self::visit_values(visitor, bounds, values, splits),
            Self::Values8(values) => Self::visit_values(visitor, bounds, values, splits),
            Self::Values16(values) => Self::visit_values(visitor, bounds, values, splits),
            Self::Values32(values) => Self::visit_values(visitor, bounds, values, splits),
            Self::Values64(values) => Self::visit_values(visitor, bounds, values, splits),
            Self::Split2(nodes) => Self::visit_split(visitor, bounds, nodes, splits),
            Self::Split4(nodes) => Self::visit_split(visitor, bounds, nodes, splits),
            Self::Split8(nodes) => Self::visit_split(visitor, bounds, nodes, splits),
            Self::Split16(nodes) => Self::visit_split(visitor, bounds, nodes, splits),
            Self::Split32(nodes) => Self::visit_split(visitor, bounds, nodes, splits),
            Self::Split64(nodes) => Self::visit_split(visitor, bounds, nodes, splits),
        }
    }

    /// Calls [`OctreeVisitor::visit_bounds`] for each value in `values`.
    fn visit_values<const N: usize>(
        visitor: &mut impl OctreeVisitor<Value = T, Bounds = OctreeBounds>,
        bounds: OctreeBounds,
        values: &[T; N],
        splits: &[OctreeSplits],
    ) {
        let bounds_split = bounds.split(*splits.last().expect("splits should not be empty"));
        for (split_bounds, value) in zip_eq(bounds_split, values) {
            visitor.visit_bounds(split_bounds, value);
        }
    }

    /// If a call to [`OctreeVisitor::visit_split`] returns `true`, recurses into that node.
    fn visit_split<const N: usize>(
        visitor: &mut impl OctreeVisitor<Value = T, Bounds = OctreeBounds>,
        bounds: OctreeBounds,
        nodes: &[Self; N],
        splits: &[OctreeSplits],
    ) {
        match visitor.visit_split(bounds) {
            VisitSplit::Skip => {}
            VisitSplit::Enter => {
                let (bounds_split, remaining_splits) = Self::next_splits(bounds, splits);
                for (node, split_bounds) in zip_eq(nodes, bounds_split) {
                    node.visit(visitor, split_bounds, remaining_splits);
                }
            }
        }
    }

    /// Returns a [`OctreeBoundsSplit`] iterator along with a slice of the remaining splits.
    fn next_splits(
        bounds: OctreeBounds,
        splits: &[OctreeSplits],
    ) -> (OctreeBoundsSplit, &[OctreeSplits]) {
        let (last_splits, remaining_splits) =
            splits.split_last().expect("splits should not be empty");
        let bounds_split = bounds.split(*last_splits);
        (bounds_split, remaining_splits)
    }

    /// Returns a `Node::Values<N>` for `total_splits` using the given `values`.
    fn values_from_vec(total_splits: u8, values: Vec<T>) -> Self {
        match total_splits {
            1 => Self::values_from_vec_impl(values, Self::Values2),
            2 => Self::values_from_vec_impl(values, Self::Values4),
            3 => Self::values_from_vec_impl(values, Self::Values8),
            4 => Self::values_from_vec_impl(values, Self::Values16),
            5 => Self::values_from_vec_impl(values, Self::Values32),
            6 => Self::values_from_vec_impl(values, Self::Values64),
            _ => unreachable!(),
        }
    }

    /// Returns a `Node::Values<N>` via `new` using the given `values`.
    fn values_from_vec_impl<const N: usize>(values: Vec<T>, new: fn(Box<[T; N]>) -> Self) -> Self {
        new(values
            .into_boxed_slice()
            .try_into()
            .ok()
            .expect("nodes should have correct len"))
    }

    /// Returns a `Node::Split<N>` for `total_splits` using the given `nodes`.
    fn split_from_vec(total_splits: u8, nodes: Vec<Node<T>>) -> Self {
        match total_splits {
            1 => Self::split_from_vec_impl(nodes, Self::Split2),
            2 => Self::split_from_vec_impl(nodes, Self::Split4),
            3 => Self::split_from_vec_impl(nodes, Self::Split8),
            4 => Self::split_from_vec_impl(nodes, Self::Split16),
            5 => Self::split_from_vec_impl(nodes, Self::Split32),
            6 => Self::split_from_vec_impl(nodes, Self::Split64),
            _ => unreachable!(),
        }
    }

    /// Returns a `Node::Split<N>` via `new` using the given `nodes`.
    fn split_from_vec_impl<const N: usize>(
        nodes: Vec<Self>,
        new: fn(Box<[Self; N]>) -> Self,
    ) -> Self {
        new(nodes
            .into_boxed_slice()
            .try_into()
            .ok()
            .expect("nodes should have correct len"))
    }
}

impl<T: Clone> Node<T> {
    /// Returns a `Node::Values<N>` if all nodes contain a single [`Node::Value`].
    fn values_if_all_value(nodes: &[Self]) -> Option<Self> {
        if nodes.iter().all(|node| matches!(node, Self::Value(_))) {
            Some(match nodes.len() {
                2 => Self::values_from_nodes(nodes, Self::Values2),
                4 => Self::values_from_nodes(nodes, Self::Values4),
                8 => Self::values_from_nodes(nodes, Self::Values8),
                16 => Self::values_from_nodes(nodes, Self::Values16),
                32 => Self::values_from_nodes(nodes, Self::Values32),
                64 => Self::values_from_nodes(nodes, Self::Values64),
                _ => unreachable!(),
            })
        } else {
            None
        }
    }

    /// Assumes all `nodes` to be [`Node::Value`] and returns a `Node::Values<N>` using `new`.
    ///
    /// # Panics
    ///
    /// Panics if the length of `nodes` does not match `new` or if there is any non-[`Node::Value`].
    fn values_from_nodes<const N: usize>(nodes: &[Self], new: fn(Box<[T; N]>) -> Self) -> Self {
        new(Box::new(map_array_init(
            nodes.try_into().expect("nodes should have the correct len"),
            |node| {
                if let Self::Value(value) = node {
                    // clone could technically be avoided; but tricky
                    value.clone()
                } else {
                    panic!("all nodes should be values")
                }
            },
        )))
    }
}

impl<T: PartialEq> Node<T> {
    /// Updates `value` to `new_value` if it differes.
    ///
    /// Returns `true` if the value has to be updated.
    fn update_if_changed(value: &mut T, new_value: T) -> bool {
        let changed = new_value != *value;
        if changed {
            *value = new_value;
        }
        changed
    }

    /// Returns the first value if all values are the same.
    ///
    /// Otherwise returns an [`Err`] with the original values.
    fn value_from_values_vec(values: Vec<T>) -> Result<T, Vec<T>> {
        let (first_value, rest) = values.split_first().expect("values should not be empty");
        if rest.iter().all(|value| value == first_value) {
            Ok(values.into_iter().next().unwrap())
        } else {
            Err(values)
        }
    }

    /// Returns a reference to the first value if all values are the same.
    fn value_from_values(values: &[T]) -> Option<&T> {
        let (first_value, rest) = values.split_first().expect("values should not be empty");
        rest.iter()
            .all(|value| value == first_value)
            .then_some(first_value)
    }

    /// Returns a reference to the first value if all nodes contain the same single value.
    fn value_from_nodes(nodes: &[Self]) -> Option<&T> {
        let (Self::Value(first_value), rest) =
            nodes.split_first().expect("nodes should not be empty")
        else {
            return None;
        };
        rest.iter()
            .all(|node| matches!(node, Self::Value(value) if value == first_value))
            .then_some(first_value)
    }
}

impl<T: Clone + PartialEq> Node<T> {
    /// Recursively visits all splits and values using the given `visitor` mutably.
    ///
    /// Returns `true` if any changes were made to the node.
    pub(crate) fn visit_mut(
        &mut self,
        visitor: &mut impl OctreeVisitorMut<Value = T, Bounds = OctreeBounds, Pos = UVec3>,
        bounds: OctreeBounds,
        splits: &[OctreeSplits],
    ) -> bool {
        let result = match self {
            Self::Value(value) => Self::visit_value_mut(visitor, bounds, value, splits),
            Self::Values2(values) => Self::visit_values_mut(visitor, bounds, values, splits),
            Self::Values4(values) => Self::visit_values_mut(visitor, bounds, values, splits),
            Self::Values8(values) => Self::visit_values_mut(visitor, bounds, values, splits),
            Self::Values16(values) => Self::visit_values_mut(visitor, bounds, values, splits),
            Self::Values32(values) => Self::visit_values_mut(visitor, bounds, values, splits),
            Self::Values64(values) => Self::visit_values_mut(visitor, bounds, values, splits),
            Self::Split2(nodes) => Self::visit_split_mut(visitor, bounds, nodes, splits),
            Self::Split4(nodes) => Self::visit_split_mut(visitor, bounds, nodes, splits),
            Self::Split8(nodes) => Self::visit_split_mut(visitor, bounds, nodes, splits),
            Self::Split16(nodes) => Self::visit_split_mut(visitor, bounds, nodes, splits),
            Self::Split32(nodes) => Self::visit_split_mut(visitor, bounds, nodes, splits),
            Self::Split64(nodes) => Self::visit_split_mut(visitor, bounds, nodes, splits),
        };
        match result {
            VisitMutResult::Replace(node) => {
                *self = node;
                true
            }
            VisitMutResult::Changed(changed) => changed,
        }
    }

    /// Visits the given `value` and splits it if the visitor asks to do so.
    ///
    /// Returns...
    ///
    /// - `Changed(false)` if the value did not change.
    /// - `Changed(true)` if the value changed (even if it got split and then merged again).
    /// - `Replace(Values<N>)` if value was split into values.
    /// - `Replace(Split<N>)` if values was split into nodes.
    ///
    /// Never returns `Replace(Value)`, since that is already covered by `Changed`.
    fn visit_value_mut(
        visitor: &mut impl OctreeVisitorMut<Value = T, Bounds = OctreeBounds, Pos = UVec3>,
        bounds: OctreeBounds,
        value: &mut T,
        splits: &[OctreeSplits],
    ) -> VisitMutResult<T> {
        if let Some(pos) = bounds.to_point() {
            return VisitMutResult::Changed(
                visitor
                    .visit_value_mut(pos, value)
                    .map_or(false, |new_value| Self::update_if_changed(value, new_value)),
            );
        }

        match visitor.visit_bounds_mut(bounds, Some(value)) {
            VisitBoundsMut::Skip => VisitMutResult::Changed(false),
            VisitBoundsMut::Fill(new_value) => {
                VisitMutResult::Changed(Self::update_if_changed(value, new_value))
            }
            VisitBoundsMut::Split => {
                let (&last_splits, remaining_splits) =
                    splits.split_last().expect("splits should not be empty");
                let bounds_split = bounds.split(last_splits);
                let total_splits = last_splits.total();

                let mut values = Vec::new();
                let mut nodes = Vec::new();

                for (already_processed, split_bounds) in bounds_split.enumerate() {
                    let mut inner_value = value.clone();
                    match Self::visit_value_mut(
                        visitor,
                        split_bounds,
                        &mut inner_value,
                        remaining_splits,
                    ) {
                        VisitMutResult::Changed(false) => {
                            if !nodes.is_empty() {
                                nodes.push(Self::Value(inner_value));
                            } else if !values.is_empty() {
                                values.push(inner_value);
                            }
                        }
                        VisitMutResult::Changed(true) => {
                            if !nodes.is_empty() {
                                nodes.push(Self::Value(inner_value));
                            } else {
                                if values.is_empty() {
                                    // transition value -> values
                                    values.reserve_exact(1 << total_splits);
                                    values.extend(repeat(&*value).take(already_processed).cloned())
                                }
                                values.push(inner_value);
                            }
                        }
                        VisitMutResult::Replace(node) => {
                            if nodes.is_empty() {
                                // transition value(s) -> nodes
                                nodes.reserve_exact(1 << total_splits);
                                if values.is_empty() {
                                    // transition value -> nodes
                                    nodes.extend(
                                        repeat(&*value)
                                            .take(already_processed)
                                            .cloned()
                                            .map(Self::Value),
                                    );
                                } else {
                                    // transition values -> nodes
                                    nodes.extend(values.drain(..).map(Self::Value));
                                }
                            }
                            nodes.push(node);
                        }
                    }
                }

                if !nodes.is_empty() {
                    VisitMutResult::Replace(Self::split_from_vec(total_splits, nodes))
                } else if !values.is_empty() {
                    match Self::value_from_values_vec(values) {
                        Ok(new_value) => {
                            VisitMutResult::Changed(Self::update_if_changed(value, new_value))
                        }
                        Err(values) => {
                            VisitMutResult::Replace(Self::values_from_vec(total_splits, values))
                        }
                    }
                } else {
                    VisitMutResult::Changed(false)
                }
            }
        }
    }

    /// If a call to [`OctreeVisitorMut::visit_split_mut`] returns `true`, recurses into the values.
    ///
    /// Returns...
    ///
    /// - `Changed(false)` if nothing changed.
    /// - `Changed(true)` if any of the values changed, but could not be merged.
    /// - `Replace(Value)` if the bounds got filled or if all values could be merged.
    /// - `Replace(Split<N>)` if any value had to be split.
    ///
    /// Never returns `Replace(Values<N>)`, since all of those cases are already covered by
    /// `Changed`.
    fn visit_values_mut<const N: usize>(
        visitor: &mut impl OctreeVisitorMut<Value = T, Bounds = OctreeBounds, Pos = UVec3>,
        bounds: OctreeBounds,
        values: &mut [T; N],
        splits: &[OctreeSplits],
    ) -> VisitMutResult<T> {
        match visitor.visit_bounds_mut(bounds, None) {
            VisitBoundsMut::Skip => VisitMutResult::Changed(false),
            VisitBoundsMut::Fill(value) => VisitMutResult::Replace(Node::Value(value)),
            VisitBoundsMut::Split => {
                let (&last_splits, remaining_splits) =
                    splits.split_last().expect("splits should not be empty");
                let bounds_split = bounds.split(last_splits);
                let total_splits = last_splits.total();

                let mut any_changed = false;
                let mut nodes = Vec::new();

                for (index, split_bounds) in bounds_split.enumerate() {
                    match Self::visit_value_mut(
                        visitor,
                        split_bounds,
                        &mut values[index],
                        remaining_splits,
                    ) {
                        VisitMutResult::Changed(changed) => {
                            if nodes.is_empty() {
                                any_changed |= changed;
                            } else {
                                nodes.push(Self::Value(values[index].clone()));
                            }
                        }
                        VisitMutResult::Replace(Self::Value(new_value)) => {
                            if nodes.is_empty() {
                                values[index] = new_value;
                            } else {
                                nodes.push(Self::Value(new_value));
                            }
                        }
                        VisitMutResult::Replace(new_node) => {
                            if nodes.is_empty() {
                                // transition values -> nodes
                                nodes.reserve_exact(1 << total_splits);
                                let already_processed = index;
                                nodes.extend(
                                    values
                                        .iter()
                                        .take(already_processed)
                                        .cloned()
                                        .map(Self::Value),
                                );
                            }
                            nodes.push(new_node);
                        }
                    }
                }

                if !nodes.is_empty() {
                    VisitMutResult::Replace(Self::split_from_vec(total_splits, nodes))
                } else if any_changed {
                    if let Some(value) = Self::value_from_values(values) {
                        // clone could technically be avoided; but tricky
                        VisitMutResult::Replace(Self::Value(value.clone()))
                    } else {
                        VisitMutResult::Changed(true)
                    }
                } else {
                    VisitMutResult::Changed(false)
                }
            }
        }
    }

    /// If a call to [`OctreeVisitorMut::visit_split_mut`] returns `true`, recurses into that node.
    ///
    /// Returns...
    ///
    /// - `Changed(false)` if nothing changed.
    /// - `Changed(true)` if any of the nodes changed, but could not be merged.
    /// - `Replace(Value)` if the bounds got filled in or if all nodes could be merged.
    /// - `Replace(Values<N>)` if each node itself could be merged.
    ///
    /// Never returns `Changed(Split<N>)`, since all of that cases are already covered by `Changed`.
    fn visit_split_mut<const N: usize>(
        visitor: &mut impl OctreeVisitorMut<Value = T, Bounds = OctreeBounds, Pos = UVec3>,
        bounds: OctreeBounds,
        nodes: &mut [Self; N],
        splits: &[OctreeSplits],
    ) -> VisitMutResult<T> {
        match visitor.visit_bounds_mut(bounds, None) {
            VisitBoundsMut::Skip => VisitMutResult::Changed(false),
            VisitBoundsMut::Fill(value) => VisitMutResult::Replace(Node::Value(value)),
            VisitBoundsMut::Split => {
                let (bounds_split, remaining_splits) = Self::next_splits(bounds, splits);
                let changed = zip_eq(nodes.iter_mut(), bounds_split).fold(
                    false,
                    |acc, (node, split_bounds)| {
                        // bitor to prevent skipping nodes
                        acc | node.visit_mut(visitor, split_bounds, remaining_splits)
                    },
                );
                if changed {
                    if let Some(value) = Self::value_from_nodes(nodes) {
                        // clone could technically be avoided; but tricky
                        VisitMutResult::Replace(Self::Value(value.clone()))
                    } else if let Some(values) = Self::values_if_all_value(nodes) {
                        VisitMutResult::Replace(values)
                    } else {
                        VisitMutResult::Changed(true)
                    }
                } else {
                    VisitMutResult::Changed(false)
                }
            }
        }
    }
}

impl<T: Default> Default for Node<T> {
    fn default() -> Self {
        Self::Value(Default::default())
    }
}

enum VisitMutResult<T> {
    Changed(bool),
    Replace(Node<T>),
}

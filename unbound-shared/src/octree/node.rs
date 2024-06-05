use glam::UVec3;

use super::{
    bounds::OctreeBounds,
    extent::OctreeSplits,
    visit::{OctreeVisitor, OctreeVisitorMut},
};
use crate::change_tracking::Mut;

/// A node within an octree, either holding a value or is split into more nodes.
///
/// Nodes can be split in half, quarters or octants and with up to a total of 6 splits along any
/// axis per [`Node`]. Note that octants require 3 splits, so only 2 can be stored at once.
///
/// There is a hierarchy of which splits are allowed:
///
/// - A series of halfs along the same axis
/// - A series of quarters along the same axis
/// - A series of octants
///
/// I.e. it is not possible to half an octant. If halfing (followed by quartering) is required to
/// reach a cube, those have to happen first.
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
    pub(crate) fn visit(
        &self,
        visitor: &mut impl OctreeVisitor<Value = T, Bounds = OctreeBounds>,
        bounds: OctreeBounds,
        splits: &[OctreeSplits],
    ) {
        match self {
            Self::Value(value) => visitor.visit_bounds(bounds, value),
            Self::Values2(values) => Self::visit_values(visitor, bounds, values),
            Self::Values4(values) => Self::visit_values(visitor, bounds, values),
            Self::Values8(values) => Self::visit_values(visitor, bounds, values),
            Self::Values16(values) => Self::visit_values(visitor, bounds, values),
            Self::Values32(values) => Self::visit_values(visitor, bounds, values),
            Self::Values64(values) => Self::visit_values(visitor, bounds, values),
            Self::Split2(nodes) => Self::visit_split(visitor, bounds, nodes),
            Self::Split4(nodes) => Self::visit_split(visitor, bounds, nodes),
            Self::Split8(nodes) => Self::visit_split(visitor, bounds, nodes),
            Self::Split16(nodes) => Self::visit_split(visitor, bounds, nodes),
            Self::Split32(nodes) => Self::visit_split(visitor, bounds, nodes),
            Self::Split64(nodes) => Self::visit_split(visitor, bounds, nodes),
        }
    }

    fn visit_values<const N: usize>(
        visitor: &mut impl OctreeVisitor<Value = T>,
        bounds: OctreeBounds,
        values: &[T; N],
    ) {
        // bounds.split()
    }

    fn visit_split<const N: usize>(
        visitor: &mut impl OctreeVisitor<Value = T>,
        bounds: OctreeBounds,
        nodes: &[Self; N],
    ) {
        todo!()
    }

    pub(crate) fn visit_mut(
        &mut self,
        visitor: &mut impl OctreeVisitorMut<Value = T, Bounds = OctreeBounds, Pos = UVec3>,
        bounds: OctreeBounds,
        splits: &[OctreeSplits],
    ) -> bool {
        let result = match self {
            Self::Value(value) => Self::visit_value_mut(visitor, bounds, value),
            Self::Values2(values) => Self::visit_values_mut(visitor, bounds, values),
            Self::Values4(values) => Self::visit_values_mut(visitor, bounds, values),
            Self::Values8(values) => Self::visit_values_mut(visitor, bounds, values),
            Self::Values16(values) => Self::visit_values_mut(visitor, bounds, values),
            Self::Values32(values) => Self::visit_values_mut(visitor, bounds, values),
            Self::Values64(values) => Self::visit_values_mut(visitor, bounds, values),
            Self::Split2(nodes) => Self::visit_split_mut(visitor, bounds, nodes),
            Self::Split4(nodes) => Self::visit_split_mut(visitor, bounds, nodes),
            Self::Split8(nodes) => Self::visit_split_mut(visitor, bounds, nodes),
            Self::Split16(nodes) => Self::visit_split_mut(visitor, bounds, nodes),
            Self::Split32(nodes) => Self::visit_split_mut(visitor, bounds, nodes),
            Self::Split64(nodes) => Self::visit_split_mut(visitor, bounds, nodes),
        };
        match result {
            VisitMutResult::Replace(node) => {
                *self = node;
                true
            }
            VisitMutResult::Changed(changed) => changed,
        }
    }

    fn visit_value_mut(
        visitor: &mut impl OctreeVisitorMut<Value = T, Bounds = OctreeBounds, Pos = UVec3>,
        bounds: OctreeBounds,
        value: &mut T,
    ) -> VisitMutResult<T> {
        let mut value = Mut::new(value);
        if let Some(pos) = bounds.to_point() {
            visitor.visit_value_mut(pos, &mut value);
            VisitMutResult::Changed(Mut::changed(&value))
        } else if visitor.visit_bounds_mut(bounds, &mut value) {
            // split it
            // delegate to visit_split_mut
            VisitMutResult::Replace(todo!())
        } else {
            VisitMutResult::Changed(Mut::changed(&value))
        }
    }

    fn visit_values_mut<const N: usize>(
        visitor: &mut impl OctreeVisitorMut<Value = T>,
        bounds: OctreeBounds,
        values: &mut [T; N],
    ) -> VisitMutResult<T> {
        todo!()
    }

    fn visit_split_mut<const N: usize>(
        visitor: &mut impl OctreeVisitorMut<Value = T>,
        bounds: OctreeBounds,
        nodes: &[Node<T>; N],
    ) -> VisitMutResult<T> {
        todo!()
    }
}

impl<T: Default> Default for Node<T> {
    fn default() -> Self {
        Self::Value(Default::default())
    }
}

#[derive(Clone, Copy, Debug)]
struct Distinct;

impl PartialEq for Distinct {
    fn eq(&self, _: &Self) -> bool {
        false
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
enum SplitResult<T> {
    Keep { changed: bool },
    Merge(T),
}

enum VisitMutResult<T> {
    Replace(Node<T>),
    Changed(bool),
}

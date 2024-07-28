use std::{ops::ControlFlow, sync::Arc};

use array_init::map_array_init;
use derive_where::derive_where;
use itertools::zip_eq;

use super::{
    bounds::{OctreeBounds, OctreeBoundsSplit},
    extent::OctreeSplits,
    visit::{
        OctreeNode, OctreeNodeMut, OctreePointMut, OctreeValue, OctreeVisitor, OctreeVisitorMut,
    },
    OctreeCache, ValueOrCache,
};

// TODO: Move Cache in the Arc
// Values<T, Cache, N> and Split<T, Cache, N> types that ignore Cache for Hash, PartialEq, Eq

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
/// - `Split<N>` holds `N` child nodes, representing `log2(N)` splits
/// - `Values<N>` is an optimization for `Split<N>` nodes that only hold values
///
/// Arrays of nodes (and values) are stored as `Arc<[T; N]>` spread across different enum variants
/// to avoid fat pointers, increasing the total size of [`Node`] itself. This is admittedly a bit
/// annyoing to work with, but at least it also improves type-safety by preventing arrays of
/// non-power-of-two sizes.
#[derive(Clone, Debug)]
#[derive_where(Hash, PartialEq, Eq; T)]
pub(crate) enum Node<T, Cache> {
    Value(T),
    Values2(Arc<[T; 2]>, #[derive_where(skip)] Cache),
    Values4(Arc<[T; 4]>, #[derive_where(skip)] Cache),
    Values8(Arc<[T; 8]>, #[derive_where(skip)] Cache),
    Values16(Arc<[T; 16]>, #[derive_where(skip)] Cache),
    Values32(Arc<[T; 32]>, #[derive_where(skip)] Cache),
    Values64(Arc<[T; 64]>, #[derive_where(skip)] Cache),
    Split2(Arc<[Self; 2]>, #[derive_where(skip)] Cache),
    Split4(Arc<[Self; 4]>, #[derive_where(skip)] Cache),
    Split8(Arc<[Self; 8]>, #[derive_where(skip)] Cache),
    Split16(Arc<[Self; 16]>, #[derive_where(skip)] Cache),
    Split32(Arc<[Self; 32]>, #[derive_where(skip)] Cache),
    Split64(Arc<[Self; 64]>, #[derive_where(skip)] Cache),
}

impl<T, Cache> Node<T, Cache> {
    /// Traverses this node using the `visitor`.
    pub(crate) fn visit<V: OctreeVisitor<Value = T, Cache = Cache>>(
        &self,
        visitor: &mut V,
        bounds: OctreeBounds,
        splits: &[OctreeSplits],
    ) -> ControlFlow<V::Break> {
        if let Self::Value(value) = self {
            visitor.value(OctreeValue::new(bounds, value))
        } else if visitor.node(OctreeNode::new(bounds, self))?.is_enter() {
            match self {
                Self::Value(_) => unreachable!(),
                Self::Values2(values, _) => Self::visit_values(visitor, bounds, splits, values),
                Self::Values4(values, _) => Self::visit_values(visitor, bounds, splits, values),
                Self::Values8(values, _) => Self::visit_values(visitor, bounds, splits, values),
                Self::Values16(values, _) => Self::visit_values(visitor, bounds, splits, values),
                Self::Values32(values, _) => Self::visit_values(visitor, bounds, splits, values),
                Self::Values64(values, _) => Self::visit_values(visitor, bounds, splits, values),
                Self::Split2(nodes, _) => Self::visit_split(visitor, bounds, splits, nodes),
                Self::Split4(nodes, _) => Self::visit_split(visitor, bounds, splits, nodes),
                Self::Split8(nodes, _) => Self::visit_split(visitor, bounds, splits, nodes),
                Self::Split16(nodes, _) => Self::visit_split(visitor, bounds, splits, nodes),
                Self::Split32(nodes, _) => Self::visit_split(visitor, bounds, splits, nodes),
                Self::Split64(nodes, _) => Self::visit_split(visitor, bounds, splits, nodes),
            }
        } else {
            ControlFlow::Continue(())
        }
    }

    /// Traverses each value in `values` using the `visitor`.
    fn visit_values<const N: usize, V: OctreeVisitor<Value = T, Cache = Cache>>(
        visitor: &mut V,
        bounds: OctreeBounds,
        splits: &[OctreeSplits],
        values: &[T; N],
    ) -> ControlFlow<V::Break> {
        zip_eq(
            bounds.split(*splits.last().expect("splits should not be empty")),
            values,
        )
        .try_for_each(|(split_bounds, value)| visitor.value(OctreeValue::new(split_bounds, value)))
    }

    /// Traverses each node in `nodes` using the `visitor`.
    fn visit_split<const N: usize, V: OctreeVisitor<Value = T, Cache = Cache>>(
        visitor: &mut V,
        bounds: OctreeBounds,
        splits: &[OctreeSplits],
        nodes: &[Self; N],
    ) -> ControlFlow<V::Break> {
        let (bounds_split, remaining_splits) = Self::next_splits(bounds, splits);
        zip_eq(bounds_split, nodes).try_for_each(|(split_bounds, node)| {
            node.visit(visitor, split_bounds, remaining_splits)
        })
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

    /// Returns a `Node::Values<N>` using the given `values`.
    fn values_from_vec(values: Vec<T>) -> Self
    where
        Cache: OctreeCache<T>,
    {
        match values.len() {
            2 => Self::values_from_vec_impl(values, Self::Values2),
            4 => Self::values_from_vec_impl(values, Self::Values4),
            8 => Self::values_from_vec_impl(values, Self::Values8),
            16 => Self::values_from_vec_impl(values, Self::Values16),
            32 => Self::values_from_vec_impl(values, Self::Values32),
            64 => Self::values_from_vec_impl(values, Self::Values64),
            _ => unreachable!(),
        }
    }

    /// Returns a `Node::Values<N>` via `new` using the given `values`.
    fn values_from_vec_impl<const N: usize>(
        values: Vec<T>,
        new: fn(Arc<[T; N]>, Cache) -> Self,
    ) -> Self
    where
        Cache: OctreeCache<T>,
    {
        let cache = Cache::compute_cache(values.iter().map(ValueOrCache::Value));
        let values = <Box<[T; N]>>::try_from(values.into_boxed_slice())
            .ok()
            .expect("values should have correct len")
            .into();
        new(values, cache)
    }

    /// Returns a `Node::Split<N>` using the given `nodes`.
    fn split_from_vec(nodes: Vec<Self>) -> Self
    where
        Cache: OctreeCache<T>,
    {
        match nodes.len() {
            2 => Self::split_from_vec_impl(nodes, Self::Split2),
            4 => Self::split_from_vec_impl(nodes, Self::Split4),
            8 => Self::split_from_vec_impl(nodes, Self::Split8),
            16 => Self::split_from_vec_impl(nodes, Self::Split16),
            32 => Self::split_from_vec_impl(nodes, Self::Split32),
            64 => Self::split_from_vec_impl(nodes, Self::Split64),
            _ => unreachable!(),
        }
    }

    /// Returns a `Node::Split<N>` via `new` using the given `nodes`.
    fn split_from_vec_impl<const N: usize>(
        nodes: Vec<Self>,
        new: fn(Arc<[Self; N]>, Cache) -> Self,
    ) -> Self
    where
        Cache: OctreeCache<T>,
    {
        let cache = Cache::compute_cache(nodes.iter().map(Self::value_or_cache));
        let nodes = <Box<[Self; N]>>::try_from(nodes.into_boxed_slice())
            .ok()
            .expect("nodes should have correct len")
            .into();
        new(nodes, cache)
    }

    /// If the node holds a single value, returns that value.
    pub(crate) fn value(&self) -> Option<&T> {
        self.value_or_cache().value()
    }

    /// If the node holds multiple values, returns the cached value.
    pub(crate) fn cache(&self) -> Option<&Cache> {
        self.value_or_cache().cache()
    }

    /// Returns the (possibly cached) value of the node.
    pub(crate) fn value_or_cache(&self) -> ValueOrCache<T, Cache> {
        match self {
            Self::Value(value) => ValueOrCache::Value(value),
            Self::Values2(_, cache)
            | Self::Values4(_, cache)
            | Self::Values8(_, cache)
            | Self::Values16(_, cache)
            | Self::Values32(_, cache)
            | Self::Values64(_, cache)
            | Self::Split2(_, cache)
            | Self::Split4(_, cache)
            | Self::Split8(_, cache)
            | Self::Split16(_, cache)
            | Self::Split32(_, cache)
            | Self::Split64(_, cache) => ValueOrCache::Cache(cache),
        }
    }

    /// Returns a `Node::Values<N>` if all nodes contain a single [`Node::Value`].
    fn values_if_all_value(nodes: &[Self]) -> Option<Self>
    where
        T: Clone,
        Cache: OctreeCache<T>,
    {
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
    fn values_from_nodes<const N: usize>(
        nodes: &[Self],
        new: fn(Arc<[T; N]>, Cache) -> Self,
    ) -> Self
    where
        T: Clone,
        Cache: OctreeCache<T>,
    {
        let cache = Cache::compute_cache(nodes.iter().map(Self::value_or_cache));
        let node = Arc::new(map_array_init(
            nodes.try_into().expect("nodes should have the correct len"),
            |node| {
                if let Self::Value(value) = node {
                    // clone could technically be avoided; but tricky
                    value.clone()
                } else {
                    panic!("all nodes should be values")
                }
            },
        ));
        new(node, cache)
    }

    /// Returns a reference to the first value if all values are the same.
    fn value_from_values(values: &[T]) -> Option<&T>
    where
        T: Eq,
    {
        let (first_value, rest) = values.split_first().expect("values should not be empty");
        rest.iter()
            .all(|value| value == first_value)
            .then_some(first_value)
    }

    /// Returns a reference to the first value if all nodes contain the same single value.
    fn value_from_nodes(nodes: &[Self]) -> Option<&T>
    where
        T: Eq,
    {
        let (Self::Value(first_value), rest) =
            nodes.split_first().expect("nodes should not be empty")
        else {
            return None;
        };
        rest.iter()
            .all(|node| matches!(node, Self::Value(value) if value == first_value))
            .then_some(first_value)
    }

    /// Recursively visits all splits and values using the given `visitor` mutably.
    pub(crate) fn visit_mut<V: OctreeVisitorMut<Value = T, Cache = Cache>>(
        &mut self,
        visitor: &mut V,
        bounds: OctreeBounds,
        splits: &[OctreeSplits],
    ) -> ControlFlow<V::Break>
    where
        T: Clone + Eq,
        Cache: Clone + OctreeCache<T>,
    {
        if let Some(point) = bounds.to_point() {
            visitor.point(OctreePointMut::new(
                point,
                if let Node::Value(value) = self {
                    value
                } else {
                    panic!("point nodes should contain a single value")
                },
            ))
        } else if visitor.node(OctreeNodeMut::new(bounds, self))?.is_enter() {
            let (control_flow, node) = match self {
                Self::Value(value) => Self::visit_value_mut(visitor, bounds, splits, value),
                Self::Values2(values, _) => Self::visit_values_mut(visitor, bounds, splits, values),
                Self::Values4(values, _) => Self::visit_values_mut(visitor, bounds, splits, values),
                Self::Values8(values, _) => Self::visit_values_mut(visitor, bounds, splits, values),
                Self::Values16(values, _) => {
                    Self::visit_values_mut(visitor, bounds, splits, values)
                }
                Self::Values32(values, _) => {
                    Self::visit_values_mut(visitor, bounds, splits, values)
                }
                Self::Values64(values, _) => {
                    Self::visit_values_mut(visitor, bounds, splits, values)
                }
                Self::Split2(nodes, _) => Self::visit_split_mut(visitor, bounds, splits, nodes),
                Self::Split4(nodes, _) => Self::visit_split_mut(visitor, bounds, splits, nodes),
                Self::Split8(nodes, _) => Self::visit_split_mut(visitor, bounds, splits, nodes),
                Self::Split16(nodes, _) => Self::visit_split_mut(visitor, bounds, splits, nodes),
                Self::Split32(nodes, _) => Self::visit_split_mut(visitor, bounds, splits, nodes),
                Self::Split64(nodes, _) => Self::visit_split_mut(visitor, bounds, splits, nodes),
            };
            if let Some(node) = node {
                *self = node;
            }
            control_flow
        } else {
            ControlFlow::Continue(())
        }
    }

    fn visit_value_mut<V: OctreeVisitorMut<Value = T, Cache = Cache>>(
        visitor: &mut V,
        bounds: OctreeBounds,
        splits: &[OctreeSplits],
        value: &T,
    ) -> (ControlFlow<V::Break>, Option<Self>)
    where
        T: Clone + Eq,
        Cache: Clone + OctreeCache<T>,
    {
        let (&last_splits, remaining_splits) =
            splits.split_last().expect("splits should not be empty");
        let bounds_split = bounds.split(last_splits);
        let mut builder = NodeBuilder::new(value, last_splits.total());

        let mut control_flow = ControlFlow::Continue(());
        for split_bounds in bounds_split {
            let mut node = Self::Value(value.clone());
            control_flow = node.visit_mut(visitor, split_bounds, remaining_splits);
            builder.push(node);
            if control_flow.is_break() {
                break;
            }
        }

        (control_flow, builder.build())
    }

    fn visit_values_mut<const N: usize, V: OctreeVisitorMut<Value = T, Cache = Cache>>(
        visitor: &mut V,
        bounds: OctreeBounds,
        splits: &[OctreeSplits],
        values: &mut Arc<[T; N]>,
    ) -> (ControlFlow<V::Break>, Option<Self>)
    where
        T: Clone + Eq,
        Cache: Clone + OctreeCache<T>,
    {
        let (&last_splits, remaining_splits) =
            splits.split_last().expect("splits should not be empty");
        let bounds_split = bounds.split(last_splits);
        let total_count = 1 << last_splits.total();

        let mut control_flow = ControlFlow::Continue(());
        let mut nodes = Vec::new();
        for (index, split_bounds) in bounds_split.enumerate() {
            let mut node = Self::Value(values[index].clone());
            control_flow = node.visit_mut(visitor, split_bounds, remaining_splits);
            if !nodes.is_empty() {
                nodes.push(node);
            } else if let Self::Value(new_value) = node {
                Arc::make_mut(values)[index] = new_value;
            } else {
                nodes.reserve_exact(total_count);
                nodes.extend(values[..index].iter().cloned().map(Self::Value));
                nodes.push(node);
            }
            if control_flow.is_break() {
                break;
            }
        }

        let node = if !nodes.is_empty() {
            nodes.extend(values[nodes.len()..].iter().cloned().map(Self::Value));
            Some(Self::split_from_vec(nodes))
        } else {
            Self::value_from_values(&**values).map(|value| Self::Value(value.clone()))
        };

        (control_flow, node)
    }

    fn visit_split_mut<const N: usize, V: OctreeVisitorMut<Value = T, Cache = Cache>>(
        visitor: &mut V,
        bounds: OctreeBounds,
        splits: &[OctreeSplits],
        nodes: &mut Arc<[Self; N]>,
    ) -> (ControlFlow<V::Break>, Option<Self>)
    where
        T: Clone + Eq,
        Cache: Clone + OctreeCache<T>,
    {
        let (&last_splits, remaining_splits) =
            splits.split_last().expect("splits should not be empty");
        let bounds_split = bounds.split(last_splits);

        let control_flow =
            zip_eq(bounds_split, Arc::make_mut(nodes)).try_for_each(|(split_bounds, node)| {
                node.visit_mut(visitor, split_bounds, remaining_splits)
            });

        let node = if let Some(value) = Self::value_from_nodes(&**nodes) {
            Some(Self::Value(value.clone()))
        } else {
            Self::values_if_all_value(&**nodes)
        };

        (control_flow, node)
    }
}

impl<T: Default, Cache> Default for Node<T, Cache> {
    fn default() -> Self {
        Self::Value(Default::default())
    }
}

struct NodeBuilder<'a, T, Cache> {
    original: &'a T,
    total_count: usize,
    state: NodeBuilderState<T, Cache>,
}

impl<'a, T, Cache> NodeBuilder<'a, T, Cache> {
    fn new(value: &'a T, total_splits: u8) -> Self {
        Self {
            original: value,
            total_count: 1 << total_splits,
            state: NodeBuilderState::Unchanged { count: 0 },
        }
    }

    fn push(&mut self, node: Node<T, Cache>)
    where
        T: Clone + Eq,
    {
        match &mut self.state {
            NodeBuilderState::Unchanged { count } => {
                if let Node::Value(value) = node {
                    if value != *self.original {
                        if *count == 0 {
                            self.state = NodeBuilderState::Value {
                                current: value,
                                count: 1,
                            };
                        } else {
                            let mut values = Vec::with_capacity(self.total_count);
                            values.resize(*count, self.original.clone());
                            values.push(value);
                            self.state = NodeBuilderState::Values(values);
                        }
                    } else {
                        *count += 1;
                    }
                } else {
                    let mut nodes = Vec::with_capacity(self.total_count);
                    nodes.resize_with(*count, || Node::Value(self.original.clone()));
                    nodes.push(node);
                    self.state = NodeBuilderState::Nodes(nodes);
                }
            }
            NodeBuilderState::Value { current, count } => {
                if let Node::Value(value) = node {
                    if value != *current {
                        let mut values = Vec::with_capacity(self.total_count);
                        values.resize_with(*count, || current.clone());
                        values.push(value);
                        self.state = NodeBuilderState::Values(values);
                    } else {
                        *count += 1;
                    }
                } else {
                    let mut nodes = Vec::with_capacity(self.total_count);
                    nodes.resize_with(*count, || Node::Value(current.clone()));
                    nodes.push(node);
                    self.state = NodeBuilderState::Nodes(nodes);
                }
            }
            NodeBuilderState::Values(values) => {
                if let Node::Value(value) = node {
                    values.push(value);
                } else {
                    let mut nodes = Vec::with_capacity(self.total_count);
                    nodes.extend(values.drain(..).map(Node::Value));
                    nodes.push(node);
                    self.state = NodeBuilderState::Nodes(nodes);
                }
            }
            NodeBuilderState::Nodes(nodes) => nodes.push(node),
        }
    }

    fn build(self) -> Option<Node<T, Cache>>
    where
        T: Clone,
        Cache: OctreeCache<T>,
    {
        match self.state {
            NodeBuilderState::Unchanged { .. } => None,
            NodeBuilderState::Value { current, count } => {
                if count == self.total_count {
                    Some(Node::Value(current))
                } else {
                    let mut values = Vec::with_capacity(self.total_count);
                    values.resize_with(count, || current.clone());
                    values.resize_with(self.total_count, || self.original.clone());
                    Some(Node::values_from_vec(values))
                }
            }
            NodeBuilderState::Values(mut values) => {
                values.resize_with(self.total_count, || self.original.clone());
                Some(Node::values_from_vec(values))
            }
            NodeBuilderState::Nodes(mut nodes) => {
                nodes.resize_with(self.total_count, || Node::Value(self.original.clone()));
                Some(Node::split_from_vec(nodes))
            }
        }
    }
}

enum NodeBuilderState<T, Cache> {
    Unchanged { count: usize },
    Value { current: T, count: usize },
    Values(Vec<T>),
    Nodes(Vec<Node<T, Cache>>),
}

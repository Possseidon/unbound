use std::{ops::ControlFlow, sync::Arc};

use arrayvec::ArrayVec;
use derive_where::derive_where;
use itertools::{repeat_n, zip_eq};

use super::{
    bounds::{OctreeBounds, OctreeBoundsSplit},
    extent::OctreeSplits,
    visit::{
        OctreeNode, OctreeNodeMut, OctreePointMut, OctreeValue, OctreeVisitor, OctreeVisitorMut,
    },
    OctreeCache, ValueOrCache,
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
/// - `Split<N>` holds `N` child nodes, representing `log2(N)` splits
/// - `Values<N>` is an optimization for `Split<N>` nodes that only hold values
///
/// Arrays of nodes (and values) are effectively stored as `Arc<[_; N]>` spread across different
/// enum variants to avoid fat pointers, increasing the total size of [`Node`] itself. This is
/// admittedly a bit annyoing to work with, but at least it also improves type-safety by preventing
/// arrays of non-power-of-two sizes.
#[derive(Clone, Debug)]
#[derive_where(Hash, PartialEq, Eq; T)]
pub(crate) enum Node<T, Cache> {
    Value(T),
    Values2(Arc<Values<2, T, Cache>>),
    Values4(Arc<Values<4, T, Cache>>),
    Values8(Arc<Values<8, T, Cache>>),
    Values16(Arc<Values<16, T, Cache>>),
    Values32(Arc<Values<32, T, Cache>>),
    Values64(Arc<Values<64, T, Cache>>),
    Split2(Arc<Split<2, T, Cache>>),
    Split4(Arc<Split<4, T, Cache>>),
    Split8(Arc<Split<8, T, Cache>>),
    Split16(Arc<Split<16, T, Cache>>),
    Split32(Arc<Split<32, T, Cache>>),
    Split64(Arc<Split<64, T, Cache>>),
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
                Self::Values2(values) => Self::visit_values(visitor, bounds, splits, values),
                Self::Values4(values) => Self::visit_values(visitor, bounds, splits, values),
                Self::Values8(values) => Self::visit_values(visitor, bounds, splits, values),
                Self::Values16(values) => Self::visit_values(visitor, bounds, splits, values),
                Self::Values32(values) => Self::visit_values(visitor, bounds, splits, values),
                Self::Values64(values) => Self::visit_values(visitor, bounds, splits, values),
                Self::Split2(split) => Self::visit_split(visitor, bounds, splits, split),
                Self::Split4(split) => Self::visit_split(visitor, bounds, splits, split),
                Self::Split8(split) => Self::visit_split(visitor, bounds, splits, split),
                Self::Split16(split) => Self::visit_split(visitor, bounds, splits, split),
                Self::Split32(split) => Self::visit_split(visitor, bounds, splits, split),
                Self::Split64(split) => Self::visit_split(visitor, bounds, splits, split),
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
        values: &Values<N, T, Cache>,
    ) -> ControlFlow<V::Break> {
        zip_eq(
            bounds.split(*splits.last().expect("splits should not be empty")),
            &values.values,
        )
        .try_for_each(|(split_bounds, value)| visitor.value(OctreeValue::new(split_bounds, value)))
    }

    /// Traverses each node in `nodes` using the `visitor`.
    fn visit_split<const N: usize, V: OctreeVisitor<Value = T, Cache = Cache>>(
        visitor: &mut V,
        bounds: OctreeBounds,
        splits: &[OctreeSplits],
        split: &Split<N, T, Cache>,
    ) -> ControlFlow<V::Break> {
        let (bounds_split, remaining_splits) = Self::next_splits(bounds, splits);
        zip_eq(bounds_split, &split.nodes).try_for_each(|(split_bounds, node)| {
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
    fn values_from_vec(values: SplitVec<T>) -> Self
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
        values: SplitVec<T>,
        new: fn(Arc<Values<N, T, Cache>>) -> Self,
    ) -> Self
    where
        Cache: OctreeCache<T>,
    {
        new(Arc::new(Values {
            cache: Cache::compute_cache(values.iter().map(ValueOrCache::Value)),
            values: array_init::from_iter(values).expect("values should have correct len"),
        }))
    }

    /// Returns a `Node::Split<N>` using the given `nodes`.
    fn split_from_vec(nodes: SplitVec<Self>) -> Self
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
        nodes: SplitVec<Self>,
        new: fn(Arc<Split<N, T, Cache>>) -> Self,
    ) -> Self
    where
        Cache: OctreeCache<T>,
    {
        new(Arc::new(Split {
            cache: Cache::compute_cache(nodes.iter().map(Self::value_or_cache)),
            nodes: array_init::from_iter(nodes).expect("nodes should have correct len"),
        }))
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
            Self::Values2(values) => ValueOrCache::Cache(&values.cache),
            Self::Values4(values) => ValueOrCache::Cache(&values.cache),
            Self::Values8(values) => ValueOrCache::Cache(&values.cache),
            Self::Values16(values) => ValueOrCache::Cache(&values.cache),
            Self::Values32(values) => ValueOrCache::Cache(&values.cache),
            Self::Values64(values) => ValueOrCache::Cache(&values.cache),
            Self::Split2(split) => ValueOrCache::Cache(&split.cache),
            Self::Split4(split) => ValueOrCache::Cache(&split.cache),
            Self::Split8(split) => ValueOrCache::Cache(&split.cache),
            Self::Split16(split) => ValueOrCache::Cache(&split.cache),
            Self::Split32(split) => ValueOrCache::Cache(&split.cache),
            Self::Split64(split) => ValueOrCache::Cache(&split.cache),
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
        new: fn(Arc<Values<N, T, Cache>>) -> Self,
    ) -> Self
    where
        T: Clone,
        Cache: OctreeCache<T>,
    {
        new(Arc::new(Values {
            cache: Cache::compute_cache(nodes.iter().map(Self::value_or_cache)),
            values: array_init::from_iter(
                nodes
                    .iter()
                    .map(|node| node.value().expect("all nodes should be values").clone()),
            )
            .expect("nodes should have correct len"),
        }))
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
                Self::Values2(values) => Self::visit_values_mut(visitor, bounds, splits, values),
                Self::Values4(values) => Self::visit_values_mut(visitor, bounds, splits, values),
                Self::Values8(values) => Self::visit_values_mut(visitor, bounds, splits, values),
                Self::Values16(values) => Self::visit_values_mut(visitor, bounds, splits, values),
                Self::Values32(values) => Self::visit_values_mut(visitor, bounds, splits, values),
                Self::Values64(values) => Self::visit_values_mut(visitor, bounds, splits, values),
                Self::Split2(nodes) => Self::visit_split_mut(visitor, bounds, splits, nodes),
                Self::Split4(nodes) => Self::visit_split_mut(visitor, bounds, splits, nodes),
                Self::Split8(nodes) => Self::visit_split_mut(visitor, bounds, splits, nodes),
                Self::Split16(nodes) => Self::visit_split_mut(visitor, bounds, splits, nodes),
                Self::Split32(nodes) => Self::visit_split_mut(visitor, bounds, splits, nodes),
                Self::Split64(nodes) => Self::visit_split_mut(visitor, bounds, splits, nodes),
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
        values: &mut Arc<Values<N, T, Cache>>,
    ) -> (ControlFlow<V::Break>, Option<Self>)
    where
        T: Clone + Eq,
        Cache: Clone + OctreeCache<T>,
    {
        let (&last_splits, remaining_splits) =
            splits.split_last().expect("splits should not be empty");
        let bounds_split = bounds.split(last_splits);

        let mut control_flow = ControlFlow::Continue(());
        let mut nodes = SplitVec::new();
        for (index, split_bounds) in bounds_split.enumerate() {
            let mut node = Self::Value(values.values[index].clone());
            control_flow = node.visit_mut(visitor, split_bounds, remaining_splits);
            if !nodes.is_empty() {
                nodes.push(node);
            } else if let Self::Value(new_value) = node {
                Arc::make_mut(values).values[index] = new_value;
            } else {
                nodes.extend(values.values[..index].iter().cloned().map(Self::Value));
                nodes.push(node);
            }
            if control_flow.is_break() {
                break;
            }
        }

        let node = if !nodes.is_empty() {
            nodes.extend(
                values.values[nodes.len()..]
                    .iter()
                    .cloned()
                    .map(Self::Value),
            );
            Some(Self::split_from_vec(nodes))
        } else {
            Self::value_from_values(&values.values).map(|value| Self::Value(value.clone()))
        };

        (control_flow, node)
    }

    fn visit_split_mut<const N: usize, V: OctreeVisitorMut<Value = T, Cache = Cache>>(
        visitor: &mut V,
        bounds: OctreeBounds,
        splits: &[OctreeSplits],
        split: &mut Arc<Split<N, T, Cache>>,
    ) -> (ControlFlow<V::Break>, Option<Self>)
    where
        T: Clone + Eq,
        Cache: Clone + OctreeCache<T>,
    {
        let (&last_splits, remaining_splits) =
            splits.split_last().expect("splits should not be empty");
        let bounds_split = bounds.split(last_splits);

        let control_flow = zip_eq(bounds_split, &mut Arc::make_mut(split).nodes).try_for_each(
            |(split_bounds, node)| node.visit_mut(visitor, split_bounds, remaining_splits),
        );

        let node = if let Some(value) = Self::value_from_nodes(&split.nodes) {
            Some(Self::Value(value.clone()))
        } else {
            Self::values_if_all_value(&split.nodes)
        };

        (control_flow, node)
    }
}

impl<T: Default, Cache> Default for Node<T, Cache> {
    fn default() -> Self {
        Self::Value(Default::default())
    }
}

#[derive(Clone, Debug)]
#[derive_where(Hash, PartialEq, Eq; T)]
pub(crate) struct Values<const N: usize, T, Cache> {
    values: [T; N],
    #[derive_where(skip)]
    cache: Cache,
}

#[derive(Clone, Debug)]
#[derive_where(Hash, PartialEq, Eq; T)]
pub(crate) struct Split<const N: usize, T, Cache> {
    nodes: [Node<T, Cache>; N],
    #[derive_where(skip)]
    cache: Cache,
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
                            let mut values = SplitVec::new_const();
                            values.extend(repeat_n(self.original, *count).cloned());
                            values.push(value);
                            self.state = NodeBuilderState::Values(values);
                        }
                    } else {
                        *count += 1;
                    }
                } else {
                    let mut nodes = SplitVec::new_const();
                    nodes.extend(repeat_n(self.original, *count).cloned().map(Node::Value));
                    nodes.push(node);
                    self.state = NodeBuilderState::Nodes(nodes);
                }
            }
            NodeBuilderState::Value { current, count } => {
                if let Node::Value(value) = node {
                    if value != *current {
                        let mut values = SplitVec::new_const();
                        values.extend(repeat_n(&*current, *count).cloned());
                        values.push(value);
                        self.state = NodeBuilderState::Values(values);
                    } else {
                        *count += 1;
                    }
                } else {
                    let mut nodes = SplitVec::new_const();
                    nodes.extend(repeat_n(&*current, *count).cloned().map(Node::Value));
                    nodes.push(node);
                    self.state = NodeBuilderState::Nodes(nodes);
                }
            }
            NodeBuilderState::Values(values) => {
                if let Node::Value(value) = node {
                    values.push(value);
                } else {
                    let mut nodes = SplitVec::new_const();
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
                    let mut values = SplitVec::new_const();
                    values.extend(repeat_n(current, count));
                    let remaining = self.total_count - values.len();
                    values.extend(repeat_n(self.original, remaining).cloned());
                    Some(Node::values_from_vec(values))
                }
            }
            NodeBuilderState::Values(mut values) => {
                let remaining = self.total_count - values.len();
                values.extend(repeat_n(self.original, remaining).cloned());
                Some(Node::values_from_vec(values))
            }
            NodeBuilderState::Nodes(mut nodes) => {
                let remaining = self.total_count - nodes.len();
                nodes.extend(repeat_n(self.original, remaining).cloned().map(Node::Value));
                Some(Node::split_from_vec(nodes))
            }
        }
    }
}

enum NodeBuilderState<T, Cache> {
    Unchanged { count: usize },
    Value { current: T, count: usize },
    Values(SplitVec<T>),
    Nodes(SplitVec<Node<T, Cache>>),
}

/// An [`ArrayVec`] capable of holding up to the maximum number of splits in a [`Node`].
type SplitVec<T> = ArrayVec<T, 64>;

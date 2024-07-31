use std::{ops::ControlFlow, sync::Arc};

use arrayvec::ArrayVec;
use derive_where::derive_where;
use itertools::{repeat_n, zip_eq};

use super::{
    bounds::OctreeBounds,
    extent::{OctreeExtent, OctreeSplits},
    visit::{LeafRef, NodeMut, OctreeVisitor, OctreeVisitorMut, ParentRef, PointMut},
    LeafOrCache, OctreeCache, OctreeNode,
};

/// A node within an octree, either holding a leaf with a value of type `T` or more [`Node`]s.
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
/// - `Leaf` is a single leaf holding a value of type `T`
/// - `Parent<N>` holds `N` child nodes, representing `log2(N)` splits
/// - `Leaves<N>` is an optimization for `Parent<N>` nodes that only hold leaves
///
/// Arrays of nodes are stored in separate variants as opposed to something like a [`Vec<T>`] or
/// `Box<[T]>`. While this is a bit inconvenient to work with, it saves on memory (which quickly
/// adds up) and also improves type-safety.
///
/// Additionally, parent nodes are stored in [`Arc`]s, making clone very cheap, as long as the
/// contents of the node is not modified.
#[derive_where(Clone; T)]
#[derive(Debug)]
#[derive_where(Hash, PartialEq, Eq; T, P)]
pub(crate) enum Node<T, P, C> {
    Leaf(T),
    Leaves2(Arc<LeavesNode<T, 2, P, C>>),
    Leaves4(Arc<LeavesNode<T, 4, P, C>>),
    Leaves8(Arc<LeavesNode<T, 8, P, C>>),
    Leaves16(Arc<LeavesNode<T, 16, P, C>>),
    Leaves32(Arc<LeavesNode<T, 32, P, C>>),
    Leaves64(Arc<LeavesNode<T, 64, P, C>>),
    Parent2(Arc<ParentNode<T, 2, P, C>>),
    Parent4(Arc<ParentNode<T, 4, P, C>>),
    Parent8(Arc<ParentNode<T, 8, P, C>>),
    Parent16(Arc<ParentNode<T, 16, P, C>>),
    Parent32(Arc<ParentNode<T, 32, P, C>>),
    Parent64(Arc<ParentNode<T, 64, P, C>>),
}

impl<T, P, C> Node<T, P, C> {
    /// Returns an immutable reference to the contents of the node.
    pub(crate) fn get(&self) -> OctreeNode<T, P, C> {
        match self {
            Self::Leaf(leaf) => OctreeNode::Leaf(leaf),
            Self::Leaves2(leaves) => leaves.get(),
            Self::Leaves4(leaves) => leaves.get(),
            Self::Leaves8(leaves) => leaves.get(),
            Self::Leaves16(leaves) => leaves.get(),
            Self::Leaves32(leaves) => leaves.get(),
            Self::Leaves64(leaves) => leaves.get(),
            Self::Parent2(parent) => parent.get(),
            Self::Parent4(parent) => parent.get(),
            Self::Parent8(parent) => parent.get(),
            Self::Parent16(parent) => parent.get(),
            Self::Parent32(parent) => parent.get(),
            Self::Parent64(parent) => parent.get(),
        }
    }

    /// Traverses this node and its children using the given `visitor`.
    pub(crate) fn visit<V: OctreeVisitor<Leaf = T, Parent = P, Cache = C>>(
        &self,
        visitor: &mut V,
        bounds: OctreeBounds,
        splits: &[OctreeSplits],
    ) -> ControlFlow<V::Break> {
        if let Self::Leaf(leaf) = self {
            visitor.leaf(super::visit::LeafRef::new(bounds, leaf))
        } else if visitor.parent(ParentRef::new(bounds, self))?.is_enter() {
            match self {
                Self::Leaf(_) => unreachable!(),
                Self::Leaves2(leaves) => Self::visit_leaves(visitor, bounds, splits, leaves),
                Self::Leaves4(leaves) => Self::visit_leaves(visitor, bounds, splits, leaves),
                Self::Leaves8(leaves) => Self::visit_leaves(visitor, bounds, splits, leaves),
                Self::Leaves16(leaves) => Self::visit_leaves(visitor, bounds, splits, leaves),
                Self::Leaves32(leaves) => Self::visit_leaves(visitor, bounds, splits, leaves),
                Self::Leaves64(leaves) => Self::visit_leaves(visitor, bounds, splits, leaves),
                Self::Parent2(parent) => Self::visit_parent(visitor, bounds, splits, parent),
                Self::Parent4(parent) => Self::visit_parent(visitor, bounds, splits, parent),
                Self::Parent8(parent) => Self::visit_parent(visitor, bounds, splits, parent),
                Self::Parent16(parent) => Self::visit_parent(visitor, bounds, splits, parent),
                Self::Parent32(parent) => Self::visit_parent(visitor, bounds, splits, parent),
                Self::Parent64(parent) => Self::visit_parent(visitor, bounds, splits, parent),
            }
        } else {
            ControlFlow::Continue(())
        }
    }

    /// Provides mutable access to the leaf value.
    ///
    /// # Caution
    ///
    /// The parent of this node must be aware of any changes to ensure the octree stays normalized.
    pub(crate) fn leaf_mut(&mut self) -> Option<&mut T> {
        if let Self::Leaf(leaf) = self {
            Some(leaf)
        } else {
            None
        }
    }

    /// Provides mutable access to the parent value.
    ///
    /// Unlike [`Self::leaf_mut`], mutating this value cannot break the invariants of the octree,
    /// since only leaf nodes dictate the structure of the octree.
    pub(crate) fn parent_mut(&mut self) -> Option<&mut P>
    where
        T: Clone,
        P: Clone,
        C: Clone,
    {
        match self {
            Self::Leaf(_) => None,
            Self::Leaves2(leaves) => Some(&mut Arc::make_mut(leaves).parent),
            Self::Leaves4(leaves) => Some(&mut Arc::make_mut(leaves).parent),
            Self::Leaves8(leaves) => Some(&mut Arc::make_mut(leaves).parent),
            Self::Leaves16(leaves) => Some(&mut Arc::make_mut(leaves).parent),
            Self::Leaves32(leaves) => Some(&mut Arc::make_mut(leaves).parent),
            Self::Leaves64(leaves) => Some(&mut Arc::make_mut(leaves).parent),
            Self::Parent2(parent) => Some(&mut Arc::make_mut(parent).parent),
            Self::Parent4(parent) => Some(&mut Arc::make_mut(parent).parent),
            Self::Parent8(parent) => Some(&mut Arc::make_mut(parent).parent),
            Self::Parent16(parent) => Some(&mut Arc::make_mut(parent).parent),
            Self::Parent32(parent) => Some(&mut Arc::make_mut(parent).parent),
            Self::Parent64(parent) => Some(&mut Arc::make_mut(parent).parent),
        }
    }

    /// Traverses this node and its children using the given `visitor`.
    pub(crate) fn visit_mut<V: OctreeVisitorMut<Leaf = T, Parent = P, Cache = C>>(
        &mut self,
        visitor: &mut V,
        bounds: OctreeBounds,
        splits: &[OctreeSplits],
    ) -> ControlFlow<V::Break>
    where
        T: Clone + Eq,
        P: Clone,
        C: Clone + OctreeCache<T>,
    {
        if let Some(point) = bounds.to_point() {
            visitor.point(PointMut::new(
                point,
                if let Node::Leaf(leaf) = self {
                    leaf
                } else {
                    panic!("point nodes should contain a single value")
                },
            ))
        } else if visitor.parent(NodeMut::new(bounds, self))?.is_enter() {
            let (control_flow, node) = match self {
                Self::Leaf(leaf) => {
                    let parent = visitor.split(LeafRef::new(bounds, leaf));
                    Self::visit_leaf_mut(visitor, bounds, splits, leaf, parent)
                }
                Self::Leaves2(leaves) => Self::visit_leaves_mut(visitor, bounds, splits, leaves),
                Self::Leaves4(leaves) => Self::visit_leaves_mut(visitor, bounds, splits, leaves),
                Self::Leaves8(leaves) => Self::visit_leaves_mut(visitor, bounds, splits, leaves),
                Self::Leaves16(leaves) => Self::visit_leaves_mut(visitor, bounds, splits, leaves),
                Self::Leaves32(leaves) => Self::visit_leaves_mut(visitor, bounds, splits, leaves),
                Self::Leaves64(leaves) => Self::visit_leaves_mut(visitor, bounds, splits, leaves),
                Self::Parent2(parent) => Self::visit_parent_mut(visitor, bounds, splits, parent),
                Self::Parent4(parent) => Self::visit_parent_mut(visitor, bounds, splits, parent),
                Self::Parent8(parent) => Self::visit_parent_mut(visitor, bounds, splits, parent),
                Self::Parent16(parent) => Self::visit_parent_mut(visitor, bounds, splits, parent),
                Self::Parent32(parent) => Self::visit_parent_mut(visitor, bounds, splits, parent),
                Self::Parent64(parent) => Self::visit_parent_mut(visitor, bounds, splits, parent),
            };

            if let Some(node) = node {
                *self = node;
            }

            control_flow
        } else {
            ControlFlow::Continue(())
        }
    }

    /// Traverses each leaf in `leaves` using the given `visitor`.
    fn visit_leaves<const N: usize, V: OctreeVisitor<Leaf = T>>(
        visitor: &mut V,
        bounds: OctreeBounds,
        splits: &[OctreeSplits],
        leaves: &LeavesNode<T, N, P, C>,
    ) -> ControlFlow<V::Break> {
        zip_eq(
            bounds.split(*splits.last().expect("splits should not be empty")),
            &leaves.leaves,
        )
        .try_for_each(|(split_bounds, leaf)| visitor.leaf(LeafRef::new(split_bounds, leaf)))
    }

    /// Traverses each child in `parent` using the given `visitor`.
    fn visit_parent<const N: usize, V: OctreeVisitor<Leaf = T, Parent = P, Cache = C>>(
        visitor: &mut V,
        bounds: OctreeBounds,
        splits: &[OctreeSplits],
        parent: &ParentNode<T, N, P, C>,
    ) -> ControlFlow<V::Break> {
        let (last_splits, remaining_splits) =
            splits.split_last().expect("splits should not be empty");
        let bounds_split = bounds.split(*last_splits);
        zip_eq(bounds_split, &parent.children).try_for_each(|(split_bounds, child)| {
            child.visit(visitor, split_bounds, remaining_splits)
        })
    }

    /// Converts the given `leaves` to a `Node::Leaves<N>`.
    fn leaves_from_vec(leaves: ParentVec<T>, leaf_extent: OctreeExtent, parent: P) -> Self
    where
        C: OctreeCache<T>,
    {
        match leaves.len() {
            2 => Self::leaves_from_vec_impl(leaves, leaf_extent, parent, Self::Leaves2),
            4 => Self::leaves_from_vec_impl(leaves, leaf_extent, parent, Self::Leaves4),
            8 => Self::leaves_from_vec_impl(leaves, leaf_extent, parent, Self::Leaves8),
            16 => Self::leaves_from_vec_impl(leaves, leaf_extent, parent, Self::Leaves16),
            32 => Self::leaves_from_vec_impl(leaves, leaf_extent, parent, Self::Leaves32),
            64 => Self::leaves_from_vec_impl(leaves, leaf_extent, parent, Self::Leaves64),
            _ => unreachable!(),
        }
    }

    /// Converts the given `leaves` to a `Node::Leaves<N>` using the given constructor.
    fn leaves_from_vec_impl<const N: usize>(
        leaves: ParentVec<T>,
        leaf_extent: OctreeExtent,
        parent: P,
        new: fn(Arc<LeavesNode<T, N, P, C>>) -> Self,
    ) -> Self
    where
        C: OctreeCache<T>,
    {
        new(Arc::new(LeavesNode {
            cache: C::compute_cache(leaves.iter().map(LeafOrCache::Leaf), leaf_extent),
            leaves: array_init::from_iter(leaves).expect("leaves should have correct len"),
            parent,
        }))
    }

    /// Converts the given `children` to a `Node::Parent<N>`.
    fn parent_from_vec(children: ParentVec<Self>, child_extent: OctreeExtent, parent: P) -> Self
    where
        C: OctreeCache<T>,
    {
        match children.len() {
            2 => Self::parent_from_vec_impl(children, child_extent, parent, Self::Parent2),
            4 => Self::parent_from_vec_impl(children, child_extent, parent, Self::Parent4),
            8 => Self::parent_from_vec_impl(children, child_extent, parent, Self::Parent8),
            16 => Self::parent_from_vec_impl(children, child_extent, parent, Self::Parent16),
            32 => Self::parent_from_vec_impl(children, child_extent, parent, Self::Parent32),
            64 => Self::parent_from_vec_impl(children, child_extent, parent, Self::Parent64),
            _ => unreachable!(),
        }
    }

    /// Converts the given `children` to a `Node::Parent<N>` using the given constructor.
    fn parent_from_vec_impl<const N: usize>(
        children: ParentVec<Self>,
        child_extent: OctreeExtent,
        parent: P,
        new: fn(Arc<ParentNode<T, N, P, C>>) -> Self,
    ) -> Self
    where
        C: OctreeCache<T>,
    {
        new(Arc::new(ParentNode {
            cache: C::compute_cache(children.iter().map(Self::leaf_or_cache), child_extent),
            children: array_init::from_iter(children).expect("children should have correct len"),
            parent,
        }))
    }

    /// Assumes the node to be a leaf, returning its value.
    ///
    /// # Panics
    ///
    /// Panics if the node is not a leaf node.
    fn unwrap_leaf(&self) -> &T {
        if let Node::Leaf(leaf) = self {
            leaf
        } else {
            panic!("node should be a leaf")
        }
    }

    /// Returns the value of leaf nodes and the cached value for parent nodes.
    fn leaf_or_cache(&self) -> LeafOrCache<T, C> {
        match self {
            Self::Leaf(leaf) => LeafOrCache::Leaf(leaf),
            Self::Leaves2(leaves) => LeafOrCache::Cache(&leaves.cache),
            Self::Leaves4(leaves) => LeafOrCache::Cache(&leaves.cache),
            Self::Leaves8(leaves) => LeafOrCache::Cache(&leaves.cache),
            Self::Leaves16(leaves) => LeafOrCache::Cache(&leaves.cache),
            Self::Leaves32(leaves) => LeafOrCache::Cache(&leaves.cache),
            Self::Leaves64(leaves) => LeafOrCache::Cache(&leaves.cache),
            Self::Parent2(parent) => LeafOrCache::Cache(&parent.cache),
            Self::Parent4(parent) => LeafOrCache::Cache(&parent.cache),
            Self::Parent8(parent) => LeafOrCache::Cache(&parent.cache),
            Self::Parent16(parent) => LeafOrCache::Cache(&parent.cache),
            Self::Parent32(parent) => LeafOrCache::Cache(&parent.cache),
            Self::Parent64(parent) => LeafOrCache::Cache(&parent.cache),
        }
    }

    /// Converts the given `children` to a `Node::Leaves<N>` if possible; i.e. if all are leaves.
    fn leaves_if_all_leaf(children: &[Self], child_extent: OctreeExtent, parent: P) -> Option<Self>
    where
        T: Clone,
        C: OctreeCache<T>,
    {
        if children.iter().all(|node| matches!(node, Self::Leaf(_))) {
            Some(match children.len() {
                2 => Self::leaves_from_nodes(children, child_extent, parent, Self::Leaves2),
                4 => Self::leaves_from_nodes(children, child_extent, parent, Self::Leaves4),
                8 => Self::leaves_from_nodes(children, child_extent, parent, Self::Leaves8),
                16 => Self::leaves_from_nodes(children, child_extent, parent, Self::Leaves16),
                32 => Self::leaves_from_nodes(children, child_extent, parent, Self::Leaves32),
                64 => Self::leaves_from_nodes(children, child_extent, parent, Self::Leaves64),
                _ => unreachable!(),
            })
        } else {
            None
        }
    }

    /// Converts the given `children` to a `Node::Leaves<N>`.
    ///
    /// # Panics
    ///
    /// Panics if the length of `nodes` does not match `new` or if there are any non-leaf nodes.
    fn leaves_from_nodes<const N: usize>(
        children: &[Self],
        child_extent: OctreeExtent,
        parent: P,
        new: fn(Arc<LeavesNode<T, N, P, C>>) -> Self,
    ) -> Self
    where
        T: Clone,
        C: OctreeCache<T>,
    {
        new(Arc::new(LeavesNode {
            cache: C::compute_cache(children.iter().map(Self::leaf_or_cache), child_extent),
            leaves: array_init::from_iter(children.iter().map(|node| node.unwrap_leaf().clone()))
                .expect("nodes should have correct len"),
            parent,
        }))
    }

    /// Returns a reference to the first leaf, if all `leaves` contain the [same](Eq) value.
    fn leaf_from_leaves(leaves: &[T]) -> Option<&T>
    where
        T: Eq,
    {
        let (first_leaf, rest) = leaves.split_first().expect("leaves should not be empty");
        rest.iter()
            .all(|leaf| leaf == first_leaf)
            .then_some(first_leaf)
    }

    /// Returns a reference to the first leaf value, if all `children` contain the [same](Eq) value.
    ///
    /// Returns [`None`] if there are any non-leaf nodes.
    fn leaf_from_nodes(children: &[Self]) -> Option<&T>
    where
        T: Eq,
    {
        let (Self::Leaf(first_leaf), rest) =
            children.split_first().expect("nodes should not be empty")
        else {
            return None;
        };
        rest.iter()
            .all(|node| matches!(node, Self::Leaf(leaf) if leaf == first_leaf))
            .then_some(first_leaf)
    }

    /// Virtually splits the given `leaf` and traverses its children using the given `visitor`.
    ///
    /// Returns either an updated [`Node`] or [`None`] if the leaf node should remain unchanged.
    fn visit_leaf_mut<V: OctreeVisitorMut<Leaf = T, Parent = P, Cache = C>>(
        visitor: &mut V,
        bounds: OctreeBounds,
        splits: &[OctreeSplits],
        leaf: &T,
        parent: P,
    ) -> (ControlFlow<V::Break>, Option<Self>)
    where
        T: Clone + Eq,
        P: Clone,
        C: Clone + OctreeCache<T>,
    {
        let (&last_splits, remaining_splits) =
            splits.split_last().expect("splits should not be empty");
        let bounds_split = bounds.split(last_splits);
        let mut builder = NodeBuilder::new(leaf, last_splits.total());

        let mut control_flow = ControlFlow::Continue(());
        for split_bounds in bounds_split {
            let mut node = Self::Leaf(leaf.clone());
            control_flow = node.visit_mut(visitor, split_bounds, remaining_splits);
            builder.push(node);
            if control_flow.is_break() {
                break;
            }
        }

        (control_flow, builder.build(bounds_split.extent(), parent))
    }

    /// Traverses each leaf in `leaves` using the given `visitor`.
    ///
    /// Returns either an updated [`Node`] or [`None`] if the node should remain unchanged.
    fn visit_leaves_mut<const N: usize, V: OctreeVisitorMut<Leaf = T, Parent = P, Cache = C>>(
        visitor: &mut V,
        bounds: OctreeBounds,
        splits: &[OctreeSplits],
        leaves: &mut Arc<LeavesNode<T, N, P, C>>,
    ) -> (ControlFlow<V::Break>, Option<Self>)
    where
        T: Clone + Eq,
        P: Clone,
        C: Clone + OctreeCache<T>,
    {
        let (&last_splits, remaining_splits) =
            splits.split_last().expect("splits should not be empty");
        let bounds_split = bounds.split(last_splits);

        let mut control_flow = ControlFlow::Continue(());
        let mut nodes = ParentVec::new();
        for (index, split_bounds) in bounds_split.enumerate() {
            let mut node = Self::Leaf(leaves.leaves[index].clone());
            control_flow = node.visit_mut(visitor, split_bounds, remaining_splits);
            if !nodes.is_empty() {
                nodes.push(node);
            } else if let Self::Leaf(new_leaf) = node {
                Arc::make_mut(leaves).leaves[index] = new_leaf;
            } else {
                nodes.extend(leaves.leaves[..index].iter().cloned().map(Self::Leaf));
                nodes.push(node);
            }
            if control_flow.is_break() {
                break;
            }
        }

        let node = if !nodes.is_empty() {
            nodes.extend(leaves.leaves[nodes.len()..].iter().cloned().map(Self::Leaf));
            Some(Self::parent_from_vec(
                nodes,
                bounds_split.extent(),
                leaves.parent.clone(), // TODO: consider avoiding the clone via take_mut
            ))
        } else {
            Self::leaf_from_leaves(&leaves.leaves).map(|leaf| Self::Leaf(leaf.clone()))
        };

        (control_flow, node)
    }

    /// Traverses each child node of `parent` using the given `visitor`.
    ///
    /// Returns either an updated [`Node`] or [`None`] if the node should remain unchanged.
    fn visit_parent_mut<const N: usize, V: OctreeVisitorMut<Leaf = T, Parent = P, Cache = C>>(
        visitor: &mut V,
        bounds: OctreeBounds,
        splits: &[OctreeSplits],
        parent: &mut Arc<ParentNode<T, N, P, C>>,
    ) -> (ControlFlow<V::Break>, Option<Self>)
    where
        T: Clone + Eq,
        P: Clone,
        C: Clone + OctreeCache<T>,
    {
        let (&last_splits, remaining_splits) =
            splits.split_last().expect("splits should not be empty");
        let bounds_split = bounds.split(last_splits);

        let control_flow = zip_eq(bounds_split, &mut Arc::make_mut(parent).children).try_for_each(
            |(split_bounds, node)| node.visit_mut(visitor, split_bounds, remaining_splits),
        );

        let node = if let Some(leaf) = Self::leaf_from_nodes(&parent.children) {
            Some(Self::Leaf(leaf.clone()))
        } else {
            Self::leaves_if_all_leaf(
                &parent.children,
                bounds_split.extent(),
                parent.parent.clone(), // TODO: consider avoiding the clone via take_mut
            )
        };

        (control_flow, node)
    }
}

/// Holds storage for `N` leaf nodes of type `T`, parent data `P` and a cached value `C`.
///
/// Cached data is intentionally ignored by [`Eq`] and friends, since it does not carry any extra
/// information that is not already present in `leaves`.
#[derive(Clone, Copy, Debug)]
#[derive_where(Hash, PartialEq, Eq; T, P)]
pub(crate) struct LeavesNode<T, const N: usize, P, C> {
    leaves: [T; N],
    parent: P,
    #[derive_where(skip(EqHashOrd))]
    cache: C,
}

impl<T, const N: usize, P, C> LeavesNode<T, N, P, C> {
    fn get(&self) -> OctreeNode<T, P, C> {
        OctreeNode::Parent {
            parent: &self.parent,
            cache: &self.cache,
        }
    }
}

/// Holds storage for `N` child [`Node`]s, parent data `P` and a cached value `C`.
///
/// Cached data is intentionally ignored by [`Eq`] and friends, since it does not carry any extra
/// information that is not already present in `leaves`.
#[derive(Clone, Debug)]
#[derive_where(Hash, PartialEq, Eq; T, P)]
pub(crate) struct ParentNode<T, const N: usize, P, C> {
    children: [Node<T, P, C>; N],
    parent: P,
    #[derive_where(skip(EqHashOrd))]
    cache: C,
}

impl<T, const N: usize, P, C> ParentNode<T, N, P, C> {
    fn get(&self) -> OctreeNode<T, P, C> {
        OctreeNode::Parent {
            parent: &self.parent,
            cache: &self.cache,
        }
    }
}

/// Used to build `Node`s incrementally.
///
/// This is intended to be used when an existing leaf node gets split by a visitor.
struct NodeBuilder<'a, T, P, C> {
    /// The original leaf value that is being split and traversed.
    original: &'a T,
    /// The total number of leafs that should be returned at the end.
    total_count: usize,
    /// Used to keep track of values that were pushed to the builder.
    state: NodeBuilderState<T, P, C>,
}

impl<'a, T, P, C> NodeBuilder<'a, T, P, C> {
    /// Start building a new node based on the given `leaf` value.
    fn new(leaf: &'a T, total_splits: u8) -> Self {
        Self {
            original: leaf,
            total_count: 1 << total_splits,
            state: NodeBuilderState::Unchanged { count: 0 },
        }
    }

    /// Pushes a new node onto the builder.
    ///
    /// This might (clone)[Clone] nodes, but tries to avoid it by only incrementing internal
    /// counters if the [same](Eq) leaf value is pushed multiple times.
    fn push(&mut self, node: Node<T, P, C>)
    where
        T: Clone + Eq,
    {
        match &mut self.state {
            NodeBuilderState::Unchanged { count } => {
                if let Node::Leaf(leaf) = node {
                    if leaf != *self.original {
                        if *count == 0 {
                            self.state = NodeBuilderState::Value {
                                current: leaf,
                                count: 1,
                            };
                        } else {
                            let mut leaves = ParentVec::new_const();
                            leaves.extend(repeat_n(self.original, *count).cloned());
                            leaves.push(leaf);
                            self.state = NodeBuilderState::Leaves(leaves);
                        }
                    } else {
                        *count += 1;
                    }
                } else {
                    let mut nodes = ParentVec::new_const();
                    nodes.extend(repeat_n(self.original, *count).cloned().map(Node::Leaf));
                    nodes.push(node);
                    self.state = NodeBuilderState::Nodes(nodes);
                }
            }
            NodeBuilderState::Value { current, count } => {
                if let Node::Leaf(leaf) = node {
                    if leaf != *current {
                        let mut leaves = ParentVec::new_const();
                        leaves.extend(repeat_n(&*current, *count).cloned());
                        leaves.push(leaf);
                        self.state = NodeBuilderState::Leaves(leaves);
                    } else {
                        *count += 1;
                    }
                } else {
                    let mut nodes = ParentVec::new_const();
                    nodes.extend(repeat_n(&*current, *count).cloned().map(Node::Leaf));
                    nodes.push(node);
                    self.state = NodeBuilderState::Nodes(nodes);
                }
            }
            NodeBuilderState::Leaves(leaves) => {
                if let Node::Leaf(leaf) = node {
                    leaves.push(leaf);
                } else {
                    let mut nodes = ParentVec::new_const();
                    nodes.extend(leaves.drain(..).map(Node::Leaf));
                    nodes.push(node);
                    self.state = NodeBuilderState::Nodes(nodes);
                }
            }
            NodeBuilderState::Nodes(nodes) => nodes.push(node),
        }
    }

    /// Fills any missing [`Self::push`] with the original value and builds the node.
    ///
    /// Returns [`None`] if all pushed values matched the original.
    fn build(self, leaf_extent: OctreeExtent, parent: P) -> Option<Node<T, P, C>>
    where
        T: Clone,
        C: OctreeCache<T>,
    {
        match self.state {
            NodeBuilderState::Unchanged { .. } => None,
            NodeBuilderState::Value { current, count } => {
                if count == self.total_count {
                    Some(Node::Leaf(current))
                } else {
                    let mut leaves = ParentVec::new_const();
                    leaves.extend(repeat_n(current, count));
                    let remaining = self.total_count - leaves.len();
                    leaves.extend(repeat_n(self.original, remaining).cloned());
                    Some(Node::leaves_from_vec(leaves, leaf_extent, parent))
                }
            }
            NodeBuilderState::Leaves(mut leaves) => {
                let remaining = self.total_count - leaves.len();
                leaves.extend(repeat_n(self.original, remaining).cloned());
                Some(Node::leaves_from_vec(leaves, leaf_extent, parent))
            }
            NodeBuilderState::Nodes(mut nodes) => {
                let remaining = self.total_count - nodes.len();
                nodes.extend(repeat_n(self.original, remaining).cloned().map(Node::Leaf));
                Some(Node::parent_from_vec(nodes, leaf_extent, parent))
            }
        }
    }
}

enum NodeBuilderState<T, P, C> {
    /// Indicates `count` pushes with the original value and nothing else.
    Unchanged { count: usize },
    /// Indicates that `current` was pushed `count` times and nothing else.
    Value { current: T, count: usize },
    /// Contains pushed leaf node values.
    Leaves(ParentVec<T>),
    /// Contains pushed nodes.
    Nodes(ParentVec<Node<T, P, C>>),
}

/// An [`ArrayVec`] capable of holding up to the maximum number of splits in a [`Node`].
type ParentVec<T> = ArrayVec<T, 64>;

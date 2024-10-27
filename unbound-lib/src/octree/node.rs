// pub mod bool;

use std::sync::Arc;

use derive_where::derive_where;
use itertools::Itertools;
use replace_with::replace_with_or_abort;

use super::{
    cache::{CacheInput, OctreeCache},
    extent::OctreeExtent,
    NodeDataMut, NodeDataMutParent, NodeDataRef, NodeMut, NodeRef, OctreeNode,
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
#[derive(Clone, Debug)]
#[derive_where(Hash, PartialEq, Eq; T, P)]
pub struct Node<T, P = (), C = NoCache>(Repr<T, P, C>);

impl<T, P, C> OctreeNode for Node<T, P, C>
where
    T: Clone + Eq,
    P: Clone,
    C: for<'a, 'b> OctreeCache<'a, &'b T, Ref = &'a C> + Eq,
{
    type Leaf = T;

    type LeafRef<'a>
        = &'a T
    where
        T: 'a;

    type LeafMut<'a>
        = &'a mut T
    where
        T: 'a;

    fn freeze_leaf(leaf: Self::LeafMut<'_>) -> Self::LeafRef<'_> {
        leaf
    }

    fn clone_leaf(leaf: Self::LeafRef<'_>) -> Self::Leaf {
        leaf.clone()
    }

    fn leaf_eq(lhs: Self::LeafRef<'_>, rhs: Self::LeafRef<'_>) -> bool {
        lhs == rhs
    }

    type Parent = P;

    type Cache<'a>
        = C
    where
        T: 'a;

    fn new(extent: OctreeExtent, leaf: Self::Leaf) -> Self {
        Self(Repr::Leaf(extent, leaf))
    }

    fn extent(&self) -> OctreeExtent {
        match self.0 {
            Repr::Leaf(extent, _)
            | Repr::Leaves2(extent, _)
            | Repr::Leaves4(extent, _)
            | Repr::Leaves8(extent, _)
            | Repr::Leaves16(extent, _)
            | Repr::Leaves32(extent, _)
            | Repr::Leaves64(extent, _)
            | Repr::Parent2(extent, _)
            | Repr::Parent4(extent, _)
            | Repr::Parent8(extent, _)
            | Repr::Parent16(extent, _)
            | Repr::Parent32(extent, _)
            | Repr::Parent64(extent, _) => extent,
        }
    }

    fn as_data(&self) -> NodeDataRef<Self> {
        let (parent, cache) = match &self.0 {
            Repr::Leaf(_, leaf) => return NodeDataRef::Leaf(leaf),
            Repr::Leaves2(_, node) => (&node.parent, &node.cache),
            Repr::Leaves4(_, node) => (&node.parent, &node.cache),
            Repr::Leaves8(_, node) => (&node.parent, &node.cache),
            Repr::Leaves16(_, node) => (&node.parent, &node.cache),
            Repr::Leaves32(_, node) => (&node.parent, &node.cache),
            Repr::Leaves64(_, node) => (&node.parent, &node.cache),
            Repr::Parent2(_, node) => (&node.parent, &node.cache),
            Repr::Parent4(_, node) => (&node.parent, &node.cache),
            Repr::Parent8(_, node) => (&node.parent, &node.cache),
            Repr::Parent16(_, node) => (&node.parent, &node.cache),
            Repr::Parent32(_, node) => (&node.parent, &node.cache),
            Repr::Parent64(_, node) => (&node.parent, &node.cache),
        };
        NodeDataRef::Parent(parent, cache)
    }

    fn as_data_mut_parent(&mut self) -> NodeDataMutParent<Self> {
        let (parent, cache) = match &mut self.0 {
            Repr::Leaf(_, leaf) => return NodeDataMutParent::Leaf(&*leaf),
            Repr::Leaves2(_, node) => Arc::make_mut(node).parent_mut_and_cache(),
            Repr::Leaves4(_, node) => Arc::make_mut(node).parent_mut_and_cache(),
            Repr::Leaves8(_, node) => Arc::make_mut(node).parent_mut_and_cache(),
            Repr::Leaves16(_, node) => Arc::make_mut(node).parent_mut_and_cache(),
            Repr::Leaves32(_, node) => Arc::make_mut(node).parent_mut_and_cache(),
            Repr::Leaves64(_, node) => Arc::make_mut(node).parent_mut_and_cache(),
            Repr::Parent2(_, node) => Arc::make_mut(node).parent_mut_and_cache(),
            Repr::Parent4(_, node) => Arc::make_mut(node).parent_mut_and_cache(),
            Repr::Parent8(_, node) => Arc::make_mut(node).parent_mut_and_cache(),
            Repr::Parent16(_, node) => Arc::make_mut(node).parent_mut_and_cache(),
            Repr::Parent32(_, node) => Arc::make_mut(node).parent_mut_and_cache(),
            Repr::Parent64(_, node) => Arc::make_mut(node).parent_mut_and_cache(),
        };
        NodeDataMutParent::Parent(parent, cache)
    }

    fn as_data_mut(&mut self) -> NodeDataMut<Self> {
        let (parent, cache) = match &mut self.0 {
            Repr::Leaf(_, leaf) => return NodeDataMut::Leaf(leaf),
            Repr::Leaves2(_, node) => Arc::make_mut(node).parent_mut_and_cache(),
            Repr::Leaves4(_, node) => Arc::make_mut(node).parent_mut_and_cache(),
            Repr::Leaves8(_, node) => Arc::make_mut(node).parent_mut_and_cache(),
            Repr::Leaves16(_, node) => Arc::make_mut(node).parent_mut_and_cache(),
            Repr::Leaves32(_, node) => Arc::make_mut(node).parent_mut_and_cache(),
            Repr::Leaves64(_, node) => Arc::make_mut(node).parent_mut_and_cache(),
            Repr::Parent2(_, node) => Arc::make_mut(node).parent_mut_and_cache(),
            Repr::Parent4(_, node) => Arc::make_mut(node).parent_mut_and_cache(),
            Repr::Parent8(_, node) => Arc::make_mut(node).parent_mut_and_cache(),
            Repr::Parent16(_, node) => Arc::make_mut(node).parent_mut_and_cache(),
            Repr::Parent32(_, node) => Arc::make_mut(node).parent_mut_and_cache(),
            Repr::Parent64(_, node) => Arc::make_mut(node).parent_mut_and_cache(),
        };
        NodeDataMut::Parent(parent, cache)
    }

    fn get_child(&self, index: u8) -> NodeRef<Self> {
        let index = usize::from(index);
        match &self.0 {
            Repr::Leaf(_, _) => panic!("leaf nodes have no children"),
            Repr::Leaves2(_, node) => NodeRef::Leaf(&node.leaves[index]),
            Repr::Leaves4(_, node) => NodeRef::Leaf(&node.leaves[index]),
            Repr::Leaves8(_, node) => NodeRef::Leaf(&node.leaves[index]),
            Repr::Leaves16(_, node) => NodeRef::Leaf(&node.leaves[index]),
            Repr::Leaves32(_, node) => NodeRef::Leaf(&node.leaves[index]),
            Repr::Leaves64(_, node) => NodeRef::Leaf(&node.leaves[index]),
            Repr::Parent2(_, node) => node.children[index].as_node_ref(),
            Repr::Parent4(_, node) => node.children[index].as_node_ref(),
            Repr::Parent8(_, node) => node.children[index].as_node_ref(),
            Repr::Parent16(_, node) => node.children[index].as_node_ref(),
            Repr::Parent32(_, node) => node.children[index].as_node_ref(),
            Repr::Parent64(_, node) => node.children[index].as_node_ref(),
        }
    }

    fn get_child_mut_unchecked(&mut self, index: u8) -> NodeMut<Self> {
        let index = usize::from(index);
        match &mut self.0 {
            Repr::Leaf(_, _) => panic!("leaf nodes have no children"),
            Repr::Leaves2(_, node) => NodeMut::Leaf(&mut Arc::make_mut(node).leaves[index]),
            Repr::Leaves4(_, node) => NodeMut::Leaf(&mut Arc::make_mut(node).leaves[index]),
            Repr::Leaves8(_, node) => NodeMut::Leaf(&mut Arc::make_mut(node).leaves[index]),
            Repr::Leaves16(_, node) => NodeMut::Leaf(&mut Arc::make_mut(node).leaves[index]),
            Repr::Leaves32(_, node) => NodeMut::Leaf(&mut Arc::make_mut(node).leaves[index]),
            Repr::Leaves64(_, node) => NodeMut::Leaf(&mut Arc::make_mut(node).leaves[index]),
            Repr::Parent2(_, node) => Arc::make_mut(node).children[index].as_node_mut(),
            Repr::Parent4(_, node) => Arc::make_mut(node).children[index].as_node_mut(),
            Repr::Parent8(_, node) => Arc::make_mut(node).children[index].as_node_mut(),
            Repr::Parent16(_, node) => Arc::make_mut(node).children[index].as_node_mut(),
            Repr::Parent32(_, node) => Arc::make_mut(node).children[index].as_node_mut(),
            Repr::Parent64(_, node) => Arc::make_mut(node).children[index].as_node_mut(),
        }
    }

    fn split_into_leaves_unchecked(&mut self, inner_extent: OctreeExtent, parent: Self::Parent) {
        let Repr::Leaf(outer_extent, leaf) = &self.0 else {
            panic!("node should be a leaf");
        };

        let splits = outer_extent.total_splits() - inner_extent.total_splits();
        self.0 = match splits {
            1 => Repr::new_leaves(Repr::Leaves2, *outer_extent, leaf, parent),
            2 => Repr::new_leaves(Repr::Leaves4, *outer_extent, leaf, parent),
            3 => Repr::new_leaves(Repr::Leaves8, *outer_extent, leaf, parent),
            4 => Repr::new_leaves(Repr::Leaves16, *outer_extent, leaf, parent),
            5 => Repr::new_leaves(Repr::Leaves32, *outer_extent, leaf, parent),
            6 => Repr::new_leaves(Repr::Leaves64, *outer_extent, leaf, parent),
            _ => panic!("invalid number of splits"),
        };
    }

    fn split_into_nodes_unchecked(&mut self, inner_extent: OctreeExtent, parent: Self::Parent) {
        let Repr::Leaf(outer_extent, leaf) = &self.0 else {
            panic!("node should be a leaf");
        };

        let splits = outer_extent.total_splits() - inner_extent.total_splits();
        self.0 = match splits {
            1 => Repr::new_parent(Repr::Parent2, *outer_extent, inner_extent, leaf, parent),
            2 => Repr::new_parent(Repr::Parent4, *outer_extent, inner_extent, leaf, parent),
            3 => Repr::new_parent(Repr::Parent8, *outer_extent, inner_extent, leaf, parent),
            4 => Repr::new_parent(Repr::Parent16, *outer_extent, inner_extent, leaf, parent),
            5 => Repr::new_parent(Repr::Parent32, *outer_extent, inner_extent, leaf, parent),
            6 => Repr::new_parent(Repr::Parent64, *outer_extent, inner_extent, leaf, parent),
            _ => panic!("invalid number of splits"),
        };
    }

    fn convert_leaves_into_nodes_unchecked(&mut self, inner_extent: OctreeExtent) {
        if !matches!(
            self.0,
            Repr::Leaves2(..)
                | Repr::Leaves4(..)
                | Repr::Leaves8(..)
                | Repr::Leaves16(..)
                | Repr::Leaves32(..)
                | Repr::Leaves64(..)
        ) {
            // checked up-front to avoid panic in replace_with_or_abort which would cause an
            // abort
            panic!("node should contain leave nodes")
        }
        replace_with_or_abort(&mut self.0, |repr| match repr {
            Repr::Leaves2(outer_extent, leaves) => {
                Repr::new_parent_from_leaves(Repr::Parent2, outer_extent, inner_extent, leaves)
            }
            Repr::Leaves4(outer_extent, leaves) => {
                Repr::new_parent_from_leaves(Repr::Parent4, outer_extent, inner_extent, leaves)
            }
            Repr::Leaves8(outer_extent, leaves) => {
                Repr::new_parent_from_leaves(Repr::Parent8, outer_extent, inner_extent, leaves)
            }
            Repr::Leaves16(outer_extent, leaves) => {
                Repr::new_parent_from_leaves(Repr::Parent16, outer_extent, inner_extent, leaves)
            }
            Repr::Leaves32(outer_extent, leaves) => {
                Repr::new_parent_from_leaves(Repr::Parent32, outer_extent, inner_extent, leaves)
            }
            Repr::Leaves64(outer_extent, leaves) => {
                Repr::new_parent_from_leaves(Repr::Parent64, outer_extent, inner_extent, leaves)
            }
            _ => unreachable!(),
        });
    }

    fn renormalize(&mut self) {
        if self.can_merge_into_leaf() {
            replace_with_or_abort(&mut self.0, |repr| match repr {
                Repr::Leaf(..) => panic!("node should not be a leaf"),
                Repr::Leaves2(extent, leaves) => Repr::first_leaf_from_leaves(extent, leaves),
                Repr::Leaves4(extent, leaves) => Repr::first_leaf_from_leaves(extent, leaves),
                Repr::Leaves8(extent, leaves) => Repr::first_leaf_from_leaves(extent, leaves),
                Repr::Leaves16(extent, leaves) => Repr::first_leaf_from_leaves(extent, leaves),
                Repr::Leaves32(extent, leaves) => Repr::first_leaf_from_leaves(extent, leaves),
                Repr::Leaves64(extent, leaves) => Repr::first_leaf_from_leaves(extent, leaves),
                Repr::Parent2(_, parent) => Repr::first_node_from_parent(parent),
                Repr::Parent4(_, parent) => Repr::first_node_from_parent(parent),
                Repr::Parent8(_, parent) => Repr::first_node_from_parent(parent),
                Repr::Parent16(_, parent) => Repr::first_node_from_parent(parent),
                Repr::Parent32(_, parent) => Repr::first_node_from_parent(parent),
                Repr::Parent64(_, parent) => Repr::first_node_from_parent(parent),
            });
        } else if self.can_use_leaves() {
            replace_with_or_abort(&mut self.0, |repr| match repr {
                Repr::Parent2(extent, parent) => {
                    Repr::new_leaves_from_parent(Repr::Leaves2, extent, parent)
                }
                Repr::Parent4(extent, parent) => {
                    Repr::new_leaves_from_parent(Repr::Leaves4, extent, parent)
                }
                Repr::Parent8(extent, parent) => {
                    Repr::new_leaves_from_parent(Repr::Leaves8, extent, parent)
                }
                Repr::Parent16(extent, parent) => {
                    Repr::new_leaves_from_parent(Repr::Leaves16, extent, parent)
                }
                Repr::Parent32(extent, parent) => {
                    Repr::new_leaves_from_parent(Repr::Leaves32, extent, parent)
                }
                Repr::Parent64(extent, parent) => {
                    Repr::new_leaves_from_parent(Repr::Leaves64, extent, parent)
                }
                _ => panic!("node should be a parent"),
            });
        }
    }

    fn recompute_cache(&mut self, inner_extent: OctreeExtent) {
        match &mut self.0 {
            Repr::Leaf(..) => {}
            Repr::Leaves2(_, leaves) => Repr::recompute_leaves_cache(inner_extent, leaves),
            Repr::Leaves4(_, leaves) => Repr::recompute_leaves_cache(inner_extent, leaves),
            Repr::Leaves8(_, leaves) => Repr::recompute_leaves_cache(inner_extent, leaves),
            Repr::Leaves16(_, leaves) => Repr::recompute_leaves_cache(inner_extent, leaves),
            Repr::Leaves32(_, leaves) => Repr::recompute_leaves_cache(inner_extent, leaves),
            Repr::Leaves64(_, leaves) => Repr::recompute_leaves_cache(inner_extent, leaves),
            Repr::Parent2(_, parent) => Repr::recompute_parent_cache(inner_extent, parent),
            Repr::Parent4(_, parent) => Repr::recompute_parent_cache(inner_extent, parent),
            Repr::Parent8(_, parent) => Repr::recompute_parent_cache(inner_extent, parent),
            Repr::Parent16(_, parent) => Repr::recompute_parent_cache(inner_extent, parent),
            Repr::Parent32(_, parent) => Repr::recompute_parent_cache(inner_extent, parent),
            Repr::Parent64(_, parent) => Repr::recompute_parent_cache(inner_extent, parent),
        }
    }
}

impl<T, P, C> Node<T, P, C> {
    fn can_merge_into_leaf(&mut self) -> bool
    where
        T: Eq,
    {
        match &self.0 {
            Repr::Leaf(..) => false,
            Repr::Leaves2(_, leaves) => leaves.leaves.iter().all_equal(),
            Repr::Leaves4(_, leaves) => leaves.leaves.iter().all_equal(),
            Repr::Leaves8(_, leaves) => leaves.leaves.iter().all_equal(),
            Repr::Leaves16(_, leaves) => leaves.leaves.iter().all_equal(),
            Repr::Leaves32(_, leaves) => leaves.leaves.iter().all_equal(),
            Repr::Leaves64(_, leaves) => leaves.leaves.iter().all_equal(),
            Repr::Parent2(_, parent) => Self::all_leaves_equal(&parent.children),
            Repr::Parent4(_, parent) => Self::all_leaves_equal(&parent.children),
            Repr::Parent8(_, parent) => Self::all_leaves_equal(&parent.children),
            Repr::Parent16(_, parent) => Self::all_leaves_equal(&parent.children),
            Repr::Parent32(_, parent) => Self::all_leaves_equal(&parent.children),
            Repr::Parent64(_, parent) => Self::all_leaves_equal(&parent.children),
        }
    }

    fn can_use_leaves(&mut self) -> bool {
        match &self.0 {
            Repr::Leaf(..) => false,
            Repr::Parent2(_, parent) => Self::all_leaves(&parent.children),
            Repr::Parent4(_, parent) => Self::all_leaves(&parent.children),
            Repr::Parent8(_, parent) => Self::all_leaves(&parent.children),
            Repr::Parent16(_, parent) => Self::all_leaves(&parent.children),
            Repr::Parent32(_, parent) => Self::all_leaves(&parent.children),
            Repr::Parent64(_, parent) => Self::all_leaves(&parent.children),
            _ => false,
        }
    }

    fn all_leaves_equal(children: &[Self]) -> bool
    where
        T: Eq,
    {
        children
            .iter()
            .map(|node| {
                if let Repr::Leaf(_, leaf) = &node.0 {
                    Ok(leaf)
                } else {
                    Err(Distinct)
                }
            })
            .all_equal()
    }

    fn all_leaves(children: &[Self]) -> bool {
        children.iter().all(|node| matches!(node.0, Repr::Leaf(..)))
    }
}

#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct NoCache;

impl<'a, T> OctreeCache<'a, T> for NoCache {
    type Ref = &'a NoCache;

    fn compute_cache(
        _: OctreeExtent,
        _: impl IntoIterator<Item = CacheInput<'a, T, Self>>,
    ) -> Self {
        Self
    }
}

#[derive(Clone, Debug)]
#[derive_where(Hash, PartialEq, Eq; T, P)]
enum Repr<T, P, C> {
    Leaf(OctreeExtent, T),
    Leaves2(OctreeExtent, Arc<LeavesNode<T, 2, P, C>>),
    Leaves4(OctreeExtent, Arc<LeavesNode<T, 4, P, C>>),
    Leaves8(OctreeExtent, Arc<LeavesNode<T, 8, P, C>>),
    Leaves16(OctreeExtent, Arc<LeavesNode<T, 16, P, C>>),
    Leaves32(OctreeExtent, Arc<LeavesNode<T, 32, P, C>>),
    Leaves64(OctreeExtent, Arc<LeavesNode<T, 64, P, C>>),
    Parent2(OctreeExtent, Arc<ParentNode<T, 2, P, C>>),
    Parent4(OctreeExtent, Arc<ParentNode<T, 4, P, C>>),
    Parent8(OctreeExtent, Arc<ParentNode<T, 8, P, C>>),
    Parent16(OctreeExtent, Arc<ParentNode<T, 16, P, C>>),
    Parent32(OctreeExtent, Arc<ParentNode<T, 32, P, C>>),
    Parent64(OctreeExtent, Arc<ParentNode<T, 64, P, C>>),
}

type NewLeaves<T, const N: usize, P, C> =
    fn(OctreeExtent, Arc<LeavesNode<T, N, P, C>>) -> Repr<T, P, C>;

type NewParent<T, const N: usize, P, C> =
    fn(OctreeExtent, Arc<ParentNode<T, N, P, C>>) -> Repr<T, P, C>;

impl<T, P, C> Repr<T, P, C> {
    fn first_leaf_from_leaves<const N: usize>(
        extent: OctreeExtent,
        leaves: Arc<LeavesNode<T, N, P, C>>,
    ) -> Self
    where
        T: Clone,
        P: Clone,
        C: Clone,
    {
        Self::Leaf(
            extent,
            Arc::unwrap_or_clone(leaves)
                .leaves
                .into_iter()
                .next()
                .expect("leaves should not be empty"),
        )
    }

    fn first_node_from_parent<const N: usize>(parent: Arc<ParentNode<T, N, P, C>>) -> Self
    where
        T: Clone,
        P: Clone,
        C: Clone,
    {
        Arc::unwrap_or_clone(parent)
            .children
            .into_iter()
            .next()
            .expect("leaves should not be empty")
            .0
    }

    fn new_leaves_from_parent<const N: usize>(
        new: NewLeaves<T, N, P, C>,
        extent: OctreeExtent,
        parent: Arc<ParentNode<T, N, P, C>>,
    ) -> Self
    where
        T: Clone,
        P: Clone,
        C: Clone,
    {
        let parent = Arc::unwrap_or_clone(parent);
        new(
            extent,
            Arc::new(LeavesNode {
                leaves: parent.children.map(|node| {
                    if let Self::Leaf(_, leaf) = node.0 {
                        leaf
                    } else {
                        unreachable!("node should be a leaf")
                    }
                }),
                parent: parent.parent,
                cache: parent.cache,
            }),
        )
    }

    fn new_leaves<const N: usize>(
        new: NewLeaves<T, N, P, C>,
        outer_extent: OctreeExtent,
        leaf: &T,
        parent: P,
    ) -> Self
    where
        T: Clone,
        C: for<'a, 'b> OctreeCache<'a, &'b T>,
    {
        new(
            outer_extent,
            Arc::new(LeavesNode {
                leaves: array_init::array_init(|_| leaf.clone()),
                parent,
                cache: C::compute_cache(outer_extent, [CacheInput::Leaf(leaf)]),
            }),
        )
    }

    fn new_parent<const N: usize>(
        new: NewParent<T, N, P, C>,
        outer_extent: OctreeExtent,
        inner_extent: OctreeExtent,
        leaf: &T,
        parent: P,
    ) -> Self
    where
        T: Clone,
        C: for<'a, 'b> OctreeCache<'a, &'b T>,
    {
        new(
            outer_extent,
            Arc::new(ParentNode {
                children: array_init::array_init(|_| Node(Self::Leaf(inner_extent, leaf.clone()))),
                parent,
                cache: C::compute_cache(outer_extent, [CacheInput::Leaf(leaf)]),
            }),
        )
    }

    fn new_parent_from_leaves<const N: usize>(
        new: NewParent<T, N, P, C>,
        outer_extent: OctreeExtent,
        inner_extent: OctreeExtent,
        leaves: Arc<LeavesNode<T, N, P, C>>,
    ) -> Self
    where
        T: Clone,
        P: Clone,
        C: Clone,
    {
        let leaves = Arc::unwrap_or_clone(leaves);
        new(
            outer_extent,
            Arc::new(ParentNode {
                children: leaves
                    .leaves
                    .map(|leaf| Node(Self::Leaf(inner_extent, leaf))),
                parent: leaves.parent,
                cache: leaves.cache,
            }),
        )
    }

    fn recompute_leaves_cache<const N: usize>(
        inner_extent: OctreeExtent,
        leaves: &mut Arc<LeavesNode<T, N, P, C>>,
    ) where
        T: Clone,
        P: Clone,
        C: for<'a, 'b> OctreeCache<'a, &'b T> + Eq,
    {
        let cache = C::compute_cache(inner_extent, leaves.leaves.iter().map(CacheInput::Leaf));
        if cache != leaves.cache {
            Arc::make_mut(leaves).cache = cache;
        }
    }

    fn recompute_parent_cache<const N: usize>(
        inner_extent: OctreeExtent,
        parent: &mut Arc<ParentNode<T, N, P, C>>,
    ) where
        T: Clone,
        P: Clone,
        C: for<'a, 'b> OctreeCache<'a, &'b T, Ref = &'a C> + Eq,
    {
        let cache = C::compute_cache(
            inner_extent,
            parent.children.iter().map(|node| match &node.0 {
                Repr::Leaf(_, leaf) => CacheInput::Leaf(leaf),
                Repr::Leaves2(_, arc) => CacheInput::Cache(&arc.cache),
                Repr::Leaves4(_, arc) => CacheInput::Cache(&arc.cache),
                Repr::Leaves8(_, arc) => CacheInput::Cache(&arc.cache),
                Repr::Leaves16(_, arc) => CacheInput::Cache(&arc.cache),
                Repr::Leaves32(_, arc) => CacheInput::Cache(&arc.cache),
                Repr::Leaves64(_, arc) => CacheInput::Cache(&arc.cache),
                Repr::Parent2(_, arc) => CacheInput::Cache(&arc.cache),
                Repr::Parent4(_, arc) => CacheInput::Cache(&arc.cache),
                Repr::Parent8(_, arc) => CacheInput::Cache(&arc.cache),
                Repr::Parent16(_, arc) => CacheInput::Cache(&arc.cache),
                Repr::Parent32(_, arc) => CacheInput::Cache(&arc.cache),
                Repr::Parent64(_, arc) => CacheInput::Cache(&arc.cache),
            }),
        );
        if cache != parent.cache {
            Arc::make_mut(parent).cache = cache;
        }
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
    fn parent_mut_and_cache(&mut self) -> (&mut P, &C) {
        (&mut self.parent, &self.cache)
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
    fn parent_mut_and_cache(&mut self) -> (&mut P, &C) {
        (&mut self.parent, &self.cache)
    }
}

struct Distinct;

impl PartialEq for Distinct {
    fn eq(&self, _: &Self) -> bool {
        false
    }
}

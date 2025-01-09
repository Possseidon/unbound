pub mod bool;
pub mod builder;
pub mod iter;
pub mod visit;

use std::{
    iter::{once, repeat_n},
    ops::Deref,
    sync::Arc,
};

use arrayvec::ArrayVec;
use educe::Educe;
use iter::Iter;
use itertools::zip_eq;

use super::{
    bounds::Bounds,
    cache::{Cache, CacheIn},
    extent::Extent,
    splits::Splits,
};

/// A node within an octree, either holding a leaf with a value of type `T` or more [`Node`]s.
///
/// Unlike with regular octrees, nodes don't always split into a `2x2x2` of other nodes. Instead,
/// nodes can have any cuboid shape as long as it can be split up to [`Splits::MAX_TOTAL`] times.
///
/// # Variants
///
/// - `Leaf` is a single leaf holding a value of type `T`
/// - `Parent<N>` holds `N²` child nodes across `N` splits
/// - `Leaves<N>` is an optimization for `Parent<N>` nodes that only hold leaves
///
/// Arrays of nodes are stored in separate variants as opposed to something like a [`Vec<T>`] or
/// `Box<[T]>`. While this is a bit inconvenient to work with, it saves on memory (which quickly
/// adds up) and also improves type-safety.
///
/// Additionally, parent nodes are stored in [`Arc`]s, making clones very cheap.
#[derive(Educe)]
#[educe(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Node<T, P = (), C = NoCache>(Repr<T, P, C>);

impl<T, P, C> HexDivNode for Node<T, P, C>
where
    C: for<'a> Cache<&'a T, Ref<'a> = &'a C>,
{
    type Leaf = T;

    type LeafRef<'a>
        = &'a T
    where
        T: 'a;

    type LeavesBuilder = ArrayVec<T, { Splits::MAX_VOLUME_USIZE }>;

    type Parent = P;

    type Cache<'a>
        = C
    where
        T: 'a;

    fn new(extent: Extent, leaf: Self::Leaf) -> Self {
        Self(Repr::Leaf(extent, leaf))
    }

    fn from_leaves_unchecked(
        extent: Extent,
        splits: Splits,
        leaves: Self::LeavesBuilder,
        parent: Self::Parent,
    ) -> Self {
        Self(match splits.total() {
            1 => Repr::from_leaves(Repr::Leaves1, extent, splits, leaves, parent),
            2 => Repr::from_leaves(Repr::Leaves2, extent, splits, leaves, parent),
            3 => Repr::from_leaves(Repr::Leaves3, extent, splits, leaves, parent),
            4 => Repr::from_leaves(Repr::Leaves4, extent, splits, leaves, parent),
            5 => Repr::from_leaves(Repr::Leaves5, extent, splits, leaves, parent),
            6 => Repr::from_leaves(Repr::Leaves6, extent, splits, leaves, parent),
            _ => panic!("invalid number of splits"),
        })
    }

    fn from_nodes_unchecked(
        extent: Extent,
        splits: Splits,
        nodes: ArrayVec<Self, { Splits::MAX_VOLUME_USIZE }>,
        parent: Self::Parent,
    ) -> Self {
        Self(match splits.total() {
            1 => Repr::from_nodes(Repr::Parent1, extent, splits, nodes, parent),
            2 => Repr::from_nodes(Repr::Parent2, extent, splits, nodes, parent),
            3 => Repr::from_nodes(Repr::Parent3, extent, splits, nodes, parent),
            4 => Repr::from_nodes(Repr::Parent4, extent, splits, nodes, parent),
            5 => Repr::from_nodes(Repr::Parent5, extent, splits, nodes, parent),
            6 => Repr::from_nodes(Repr::Parent6, extent, splits, nodes, parent),
            _ => panic!("invalid number of splits"),
        })
    }

    fn extent(&self) -> Extent {
        match self.0 {
            Repr::Leaf(extent, _)
            | Repr::Leaves1(extent, _, _)
            | Repr::Leaves2(extent, _, _)
            | Repr::Leaves3(extent, _, _)
            | Repr::Leaves4(extent, _, _)
            | Repr::Leaves5(extent, _, _)
            | Repr::Leaves6(extent, _, _)
            | Repr::Parent1(extent, _, _)
            | Repr::Parent2(extent, _, _)
            | Repr::Parent3(extent, _, _)
            | Repr::Parent4(extent, _, _)
            | Repr::Parent5(extent, _, _)
            | Repr::Parent6(extent, _, _) => extent,
        }
    }

    fn splits(&self) -> Splits {
        match self.0 {
            Repr::Leaf(..) => panic!("leaf nodes have no splits"),
            Repr::Leaves1(_, splits, _)
            | Repr::Leaves2(_, splits, _)
            | Repr::Leaves3(_, splits, _)
            | Repr::Leaves4(_, splits, _)
            | Repr::Leaves5(_, splits, _)
            | Repr::Leaves6(_, splits, _)
            | Repr::Parent1(_, splits, _)
            | Repr::Parent2(_, splits, _)
            | Repr::Parent3(_, splits, _)
            | Repr::Parent4(_, splits, _)
            | Repr::Parent5(_, splits, _)
            | Repr::Parent6(_, splits, _) => splits,
        }
    }

    fn as_data(&self) -> NodeDataRef<Self> {
        match &self.0 {
            Repr::Leaf(_, leaf) => NodeDataRef::Leaf(leaf),
            Repr::Leaves1(_, _, node) => NodeDataRef::Parent(&node.parent, &node.cache),
            Repr::Leaves2(_, _, node) => NodeDataRef::Parent(&node.parent, &node.cache),
            Repr::Leaves3(_, _, node) => NodeDataRef::Parent(&node.parent, &node.cache),
            Repr::Leaves4(_, _, node) => NodeDataRef::Parent(&node.parent, &node.cache),
            Repr::Leaves5(_, _, node) => NodeDataRef::Parent(&node.parent, &node.cache),
            Repr::Leaves6(_, _, node) => NodeDataRef::Parent(&node.parent, &node.cache),
            Repr::Parent1(_, _, node) => NodeDataRef::Parent(&node.parent, &node.cache),
            Repr::Parent2(_, _, node) => NodeDataRef::Parent(&node.parent, &node.cache),
            Repr::Parent3(_, _, node) => NodeDataRef::Parent(&node.parent, &node.cache),
            Repr::Parent4(_, _, node) => NodeDataRef::Parent(&node.parent, &node.cache),
            Repr::Parent5(_, _, node) => NodeDataRef::Parent(&node.parent, &node.cache),
            Repr::Parent6(_, _, node) => NodeDataRef::Parent(&node.parent, &node.cache),
        }
    }

    fn into_leaf(self) -> Result<Self::Leaf, Self> {
        if let Repr::Leaf(_, leaf) = self.0 {
            Ok(leaf)
        } else {
            Err(self)
        }
    }

    fn get_child(&self, index: u8) -> NodeRef<Self> {
        let index = usize::from(index);
        match &self.0 {
            Repr::Leaf(..) => panic!("leaf nodes have no children"),
            Repr::Leaves1(_, _, node) => NodeRef::Leaf(&node.leaves[index]),
            Repr::Leaves2(_, _, node) => NodeRef::Leaf(&node.leaves[index]),
            Repr::Leaves3(_, _, node) => NodeRef::Leaf(&node.leaves[index]),
            Repr::Leaves4(_, _, node) => NodeRef::Leaf(&node.leaves[index]),
            Repr::Leaves5(_, _, node) => NodeRef::Leaf(&node.leaves[index]),
            Repr::Leaves6(_, _, node) => NodeRef::Leaf(&node.leaves[index]),
            Repr::Parent1(_, _, node) => NodeRef::Node(&node.children[index]),
            Repr::Parent2(_, _, node) => NodeRef::Node(&node.children[index]),
            Repr::Parent3(_, _, node) => NodeRef::Node(&node.children[index]),
            Repr::Parent4(_, _, node) => NodeRef::Node(&node.children[index]),
            Repr::Parent5(_, _, node) => NodeRef::Node(&node.children[index]),
            Repr::Parent6(_, _, node) => NodeRef::Node(&node.children[index]),
        }
    }
}

impl<'a, T, P, C> IntoIterator for &'a Node<T, P, C>
where
    C: for<'b> Cache<&'b T, Ref<'b> = &'b C>,
{
    type Item = (Bounds, NodeRef<'a, Node<T, P, C>>);
    type IntoIter = Iter<'a, Node<T, P, C>>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct NoCache;

impl<T> Cache<T> for NoCache {
    type Ref<'a> = &'a NoCache;

    fn compute_cache<'a>(_: Extent, _: impl IntoIterator<Item = CacheIn<'a, T, Self>>) -> Self {
        Self
    }
}

#[derive(Educe)]
#[educe(Clone, Debug, Hash, PartialEq, Eq)]
enum Repr<T, P, C> {
    Leaf(Extent, T),
    Leaves1(Extent, Splits, Arc<LeavesNode<T, 2, P, C>>),
    Leaves2(Extent, Splits, Arc<LeavesNode<T, 4, P, C>>),
    Leaves3(Extent, Splits, Arc<LeavesNode<T, 8, P, C>>),
    Leaves4(Extent, Splits, Arc<LeavesNode<T, 16, P, C>>),
    Leaves5(Extent, Splits, Arc<LeavesNode<T, 32, P, C>>),
    Leaves6(Extent, Splits, Arc<LeavesNode<T, 64, P, C>>),
    Parent1(Extent, Splits, Arc<ParentNode<T, 2, P, C>>),
    Parent2(Extent, Splits, Arc<ParentNode<T, 4, P, C>>),
    Parent3(Extent, Splits, Arc<ParentNode<T, 8, P, C>>),
    Parent4(Extent, Splits, Arc<ParentNode<T, 16, P, C>>),
    Parent5(Extent, Splits, Arc<ParentNode<T, 32, P, C>>),
    Parent6(Extent, Splits, Arc<ParentNode<T, 64, P, C>>),
}

type NewLeaves<T, const N: usize, P, C> =
    fn(Extent, Splits, Arc<LeavesNode<T, N, P, C>>) -> Repr<T, P, C>;

type NewParent<T, const N: usize, P, C> =
    fn(Extent, Splits, Arc<ParentNode<T, N, P, C>>) -> Repr<T, P, C>;

impl<T, P, C> Repr<T, P, C>
where
    C: for<'a> Cache<&'a T, Ref<'a> = &'a C>,
{
    fn from_leaves<const N: usize>(
        new: NewLeaves<T, N, P, C>,
        extent: Extent,
        splits: Splits,
        leaves: ArrayVec<T, { Splits::MAX_VOLUME_USIZE }>,
        parent: P,
    ) -> Self {
        let leaves = array_init::from_iter(leaves).expect("leaves should have correct length");
        let cache = C::compute_cache(extent.split(splits), leaves.iter().map(CacheIn::Leaf));
        new(
            extent,
            splits,
            Arc::new(LeavesNode {
                leaves,
                parent,
                cache,
            }),
        )
    }

    fn from_nodes<const N: usize>(
        new: NewParent<T, N, P, C>,
        extent: Extent,
        splits: Splits,
        nodes: ArrayVec<Node<T, P, C>, { Splits::MAX_VOLUME_USIZE }>,
        parent: P,
    ) -> Self {
        let children = array_init::from_iter(nodes).expect("leaves should have correct length");
        let cache = C::compute_cache(
            extent.split(splits),
            children.iter().map(CacheIn::from_node),
        );
        new(
            extent,
            splits,
            Arc::new(ParentNode {
                children,
                parent,
                cache,
            }),
        )
    }
}

/// Holds storage for `N` leaf nodes of type `T`, parent data `P` and a cached value `C`.
///
/// Cached data is intentionally ignored by [`Eq`] and friends, since it does not carry any extra
/// information that is not already present in `leaves`.
#[derive(Educe)]
#[educe(Clone, Copy, Debug, Hash, PartialEq, Eq)]
struct LeavesNode<T, const N: usize, P, C> {
    leaves: [T; N],
    parent: P,
    #[educe(Hash(ignore), Eq(ignore))]
    cache: C,
}

/// Holds storage for `N` child [`Node`]s, parent data `P` and a cached value `C`.
///
/// Cached data is intentionally ignored by [`Eq`] and friends, since it does not carry any extra
/// information that is not already present in `leaves`.
#[derive(Educe)]
#[educe(Clone, Debug, Hash, PartialEq, Eq)]
struct ParentNode<T, const N: usize, P, C> {
    children: [Node<T, P, C>; N],
    parent: P,
    #[educe(Hash(ignore), Eq(ignore))]
    cache: C,
}

/// A region/node within an [Octree] inspired sparse 3D array.
///
/// # Trait Implementations
///
/// This module also contains implementors of this trait:
///
/// - [`Node`] as a general purpose implementation that works for all types
/// - [`bool::BitNode`] which is limited to [`prim@bool`], but has very efficient storage
///
/// Since a [`HexDivNode`] is usually made up of a lot of nodes, the concrete node type should
/// generally strive for a small footprint; two pointers ideally.
///
/// # Not an [Octree]
///
/// When compared to a normal [Octree], a [`HexDivNode`]:
///
/// - Is more flexible in its shape
/// - Is better optimized for cache locality by having nodes store more direct children
///
/// An [Octree] is generally limited to a cube shape with a side length that is a power of two. A
/// [`HexDivNode`] lifts the "cube shape" restriction, allowing any cuboid shape, only requiring
/// each individual side length to be a power of two. This has the nice benefit, that a
/// [`HexDivNode`] can also be used in place of a [Quadtree] (a 2D [Octree]) or even for just a 1D
/// strip.
///
/// # Parent Nodes
///
/// Parent nodes can hold arbitrary additional data of type [`HexDivNode::Parent`]. Note, that
/// parent nodes, including this parent data, will be removed automatically if all of their children
/// contain the same value. If your use-case requires parent data to influence the [`HexDivNode`]
/// structure, it should probably be [`HexDivNode::Leaf`] data in a separate [`HexDivNode`] instead.
///
/// This [`HexDivNode::Parent`] data can be used to e.g. cache arbitrary things that, unlike
/// [`HexDivNode::Cache`], are not just an aggregation of its own [`HexDivNode::Leaf`] data. Again,
/// keep in mind though, that it will disappear if the parent node is merged into a single leaf.
///
/// # [`Cache`]
///
/// Parent nodes can hold an immutable cached value of type [`HexDivNode::Cache`], which is
/// automatically derived from its children. E.g. [`Count`](bool::Count) caches the total number of
/// `true` leaves across all children.
///
/// [Octree]: https://en.wikipedia.org/wiki/Octree
/// [Quadtree]: https://en.wikipedia.org/wiki/Quadtree
pub trait HexDivNode: Sized {
    /// The type of leaf values that this [`HexDivNode`] holds.
    ///
    /// To actually be useful, leaves must be:
    ///
    /// - [`Clone`] for when a leaf node is split into a parent node
    /// - [`Eq`] to know if a parent node can be merged back into a leaf node
    type Leaf;

    /// A reference to a [`HexDivNode::Leaf`], usually just `&Self::Leaf`.
    ///
    /// The main use-case for this customization point is to avoid having to hand out `&bool`, which
    /// is impossible if [`prim@bool`] leaves are stored as bits in e.g. a [`u64`].
    type LeafRef<'a>: Copy
    where
        Self::Leaf: 'a;

    /// Used by [`HexDivNode::from_leaves_unchecked`].
    type LeavesBuilder: FromIterator<Self::Leaf>
        + Extend<Self::Leaf>
        + IntoIterator<Item = Self::Leaf>;

    /// Arbitrary data for parent nodes.
    type Parent;

    /// Can be queried cheaply for any parent node.
    type Cache<'a>: Cache<Self::LeafRef<'a>>
    where
        Self::Leaf: 'a;

    /// Constructs a [`HexDivNode`] of the specified `extent` filled with the given `leaf`.
    fn new(extent: Extent, leaf: Self::Leaf) -> Self;

    /// Construtcs a [`HexDivNode`] of the specified `extent` filled with the [`Default`] leaf.
    fn with_default(extent: Extent) -> Self
    where
        Self::Leaf: Default,
    {
        Self::new(extent, Default::default())
    }

    /// Constructs a new instance from a set of `children`.
    ///
    /// # Panics
    ///
    /// Panics if the number of `children` does not match the number of `splits`.
    ///
    /// Panics if not all `children` share the same extent.
    fn from_children(
        mut children: impl Iterator<Item = Self>,
        splits: Splits,
        parent: Self::Parent,
    ) -> Self
    where
        Self::Leaf: Clone + Eq,
    {
        let first = children.next().expect("children should not be empty");
        let child_extent = first.extent();
        let extent = child_extent.unsplit(splits);

        // ensure all children have same extent
        let children = children.inspect(|child| assert_eq!(child.extent(), child_extent));

        // ensure there is exactly count children
        let mut children = zip_eq(1..splits.volume(), children);

        match first.into_leaf() {
            Ok(first) => {
                for (already_processed, child) in &mut children {
                    match child.into_leaf() {
                        Ok(leaf) => {
                            if leaf != first {
                                let mut leaves = repeat_n(first, already_processed.into())
                                    .chain([leaf])
                                    .collect::<Self::LeavesBuilder>();

                                for (_, child) in &mut children {
                                    match child.into_leaf() {
                                        Ok(leaf) => {
                                            leaves.extend([leaf]);
                                        }
                                        Err(child) => {
                                            return Self::from_nodes_unchecked(
                                                extent,
                                                splits,
                                                leaves
                                                    .into_iter()
                                                    .map(|leaf| Self::new(child_extent, leaf))
                                                    .chain([child])
                                                    .chain(children.map(|(_, child)| child))
                                                    .collect(),
                                                parent,
                                            );
                                        }
                                    }
                                }

                                return Self::from_leaves_unchecked(extent, splits, leaves, parent);
                            }
                        }
                        Err(child) => {
                            return Self::from_nodes_unchecked(
                                extent,
                                splits,
                                repeat_n(first, already_processed.into())
                                    .map(|leaf| Self::new(child_extent, leaf))
                                    .chain([child])
                                    .chain(children.map(|(_, child)| child))
                                    .collect(),
                                parent,
                            );
                        }
                    }
                }

                Self::new(extent, first)
            }
            Err(first) => Self::from_nodes_unchecked(
                extent,
                splits,
                once(first)
                    .chain(children.map(|(_, child)| child))
                    .collect(),
                parent,
            ),
        }
    }

    /// Do not call this directly! Use [`HexDivNode::from_children`] instead.
    ///
    /// Constructs a new instance from a [`HexDivNode::LeavesBuilder`].
    fn from_leaves_unchecked(
        extent: Extent,
        splits: Splits,
        leaves: Self::LeavesBuilder,
        parent: Self::Parent,
    ) -> Self;

    /// Do not call this directly! Use [`HexDivNode::from_children`] instead.
    ///
    /// Constructs a new instance from a list of nodes.
    fn from_nodes_unchecked(
        extent: Extent,
        splits: Splits,
        nodes: ArrayVec<Self, { Splits::MAX_VOLUME_USIZE }>,
        parent: Self::Parent,
    ) -> Self;

    /// Returns the extent of this node.
    fn extent(&self) -> Extent;

    /// Returns the layout of child nodes in terms of [`Splits`].
    ///
    /// # Panics
    ///
    /// Panics when called on a leaf node.
    ///
    /// One could argue, that [`Splits::NONE`] should be returned in that case, but calling it on a
    /// leaf might easily happen unintentionally from logic errors and panicking instead can catch
    /// those bugs early.
    fn splits(&self) -> Splits;

    /// Returns a reference to the underlying data of this node.
    ///
    /// - For leaf nodes: [`HexDivNode::LeafRef`]
    /// - For parent nodes: [`&HexDivNode::Parent`](HexDivNode::Parent), [`Cache::Ref`]
    fn as_data(&self) -> NodeDataRef<Self>;

    /// Unwraps a leaf node into [`HexDivNode::Leaf`].
    ///
    /// If the node is not a leaf, the node is returned unchanged as an [`Err`].
    fn into_leaf(self) -> Result<Self::Leaf, Self>;

    /// Fills the entire node with the given leaf value.
    fn fill(&mut self, leaf: Self::Leaf) {
        *self = Self::new(self.extent(), leaf);
    }

    /// Returns an iterator that traverses nodes depth-first.
    fn iter(&self) -> Iter<Self> {
        Iter::new(self)
    }

    /// Returns a reference to one of the children of this node.
    ///
    /// # Panics
    ///
    /// Panics if `index` is out of bounds or when called on a leaf node.
    fn get_child(&self, index: u8) -> NodeRef<Self>;

    /// Returns a generic [`Debug`](std::fmt::Debug) implementation with concise output.
    ///
    /// The implementation only requires leaf, parent and cache data to implement
    /// [`Debug`](std::fmt::Debug) and does not need the node type itself to implement it.
    fn debug(&self) -> HexDivDebug<Self> {
        HexDivDebug { node: self }
    }
}

/// Shorthand to access the [`Cache::Ref`] on a [`HexDivNode::Cache`].
pub type CacheRef<'a, T> =
    <<T as HexDivNode>::Cache<'a> as Cache<<T as HexDivNode>::LeafRef<'a>>>::Ref<'a>;

/// A reference to the data of a [`HexDivNode`] node.
#[derive(Educe)]
#[educe(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum NodeDataRef<'a, T: HexDivNode>
where
    T::Leaf: 'a,
{
    Leaf(T::LeafRef<'a>),
    Parent(&'a T::Parent, CacheRef<'a, T>),
}

impl<T: HexDivNode> NodeDataRef<'_, T> {
    pub fn is_leaf(self) -> bool {
        matches!(self, Self::Leaf(..))
    }

    pub fn is_parent(self) -> bool {
        matches!(self, Self::Parent(..))
    }
}

/// A reference to a [`HexDivNode`] node.
///
/// [`NodeRef::Leaf`] is used (exclusively) for virtual nodes used by leaves nodes.
#[derive(Educe)]
#[educe(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum NodeRef<'a, T: HexDivNode> {
    Node(&'a T),
    Leaf(T::LeafRef<'a>),
}

impl<'a, T: HexDivNode> NodeRef<'a, T> {
    pub fn data(self) -> NodeDataRef<'a, T> {
        match self {
            Self::Node(node) => node.as_data(),
            Self::Leaf(leaf) => NodeDataRef::Leaf(leaf),
        }
    }

    pub fn as_parent(self) -> Option<ParentNodeRef<'a, T>> {
        if let Self::Node(node) = self {
            ParentNodeRef::new(node)
        } else {
            None
        }
    }

    pub fn is_parent(self) -> bool {
        self.as_parent().is_some()
    }
}

/// A reference to a [`HexDivNode`] node that is known to have children.
#[derive(Educe)]
#[educe(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct ParentNodeRef<'a, T>(&'a T);

impl<'a, T: HexDivNode> ParentNodeRef<'a, T> {
    pub fn new(node: &'a T) -> Option<Self> {
        node.as_data().is_parent().then_some(Self(node))
    }
}

impl<'a, T> Deref for ParentNodeRef<'a, T> {
    type Target = &'a T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

pub struct HexDivDebug<'a, T> {
    node: &'a T,
}

impl<'a, T: HexDivNode> std::fmt::Debug for HexDivDebug<'a, T>
where
    T::LeafRef<'a>: std::fmt::Debug,
    T::Parent: std::fmt::Debug,
    CacheRef<'a, T>: std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.node.as_data() {
            NodeDataRef::Leaf(leaf) => f
                .debug_struct("Leaf")
                .field("extent", &self.node.extent())
                .field("leaf", &leaf)
                .finish(),
            NodeDataRef::Parent(parent, cache) => f
                .debug_struct("Parent")
                .field("extent", &self.node.extent())
                .field("parent", parent)
                .field("cache", &cache)
                .field("children", &(ChildrenDebug { node: self.node }))
                .finish(),
        }
    }
}

struct ChildrenDebug<'a, T> {
    node: &'a T,
}

impl<'a, T: HexDivNode> std::fmt::Debug for ChildrenDebug<'a, T>
where
    T::LeafRef<'a>: std::fmt::Debug,
    T::Parent: std::fmt::Debug,
    CacheRef<'a, T>: std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut debug_list = f.debug_list();
        for index in 0..self.node.extent().total_splits() {
            match self.node.get_child(index) {
                NodeRef::Node(node) => debug_list.entry(&HexDivDebug { node }),
                NodeRef::Leaf(leaf) => debug_list.entry(&leaf),
            };
        }
        debug_list.finish()
    }
}

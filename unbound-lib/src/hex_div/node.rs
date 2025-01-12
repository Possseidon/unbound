pub mod bool;
pub mod builder;
pub mod iter;

use std::{
    fmt,
    hash::{Hash, Hasher},
    iter::{once, repeat_n},
    mem::discriminant,
    ops::Deref,
    sync::Arc,
};

use arrayvec::ArrayVec;
use iter::Iter;
use itertools::zip_eq;

use super::{
    bounds::CachedBounds,
    cache::{Cache, CacheIn},
    extent::{CachedExtent, Extent, HasCachedExtent, HasExtent, Splittable},
    splits::Splits,
};

/// A node within an octree, either holding a leaf with a value of type `T` or more [`Node`]s.
///
/// Unlike with regular octrees, nodes don't always split into a `2x2x2` of other nodes. Instead,
/// nodes can have any cuboid shape as long as it can be split up to [`Splits::MAX_TOTAL`] times.
///
/// # [`Eq`] and [`Hash`]
///
/// As an optimization, [`PartialEq`]/[`Eq`] first compare the [`Node`]'s [`Cache`], since it is
/// fully dependent on the leaf data and generally allows for fast short circuiting. This however
/// requires the [`Cache`] to be [`PartialEq`]/[`Eq`] itself, despite it not strictly being
/// necessary or always desireable to compare the [`Cache`].
///
/// To resolve these two issues, `C` can be wrapped in [`CacheAlwaysEq`], which basically just
/// disables `C` from being compared (all `C` are not considered equal, no matter their value).
///
/// [`Hash`]ing a node on the other hand never hashes its [`Cache`]. The cache itself is already
/// derived from existing data (the child nodes) so hashing the cache would not add any extra value
/// to the node's hash.
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
#[derive(Debug, PartialEq, Eq)]
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

    fn new(extent: CachedExtent, leaf: Self::Leaf) -> Self {
        Self(Repr::Leaf(extent, leaf))
    }

    fn from_leaves_unchecked(
        extent: Splittable<CachedExtent>,
        leaves: Self::LeavesBuilder,
        parent: Self::Parent,
    ) -> Self {
        Self(
            match extent.total_child_splits().expect("invalid extent").get() {
                1 => Repr::from_leaves(Repr::Leaves1, extent, leaves, parent),
                2 => Repr::from_leaves(Repr::Leaves2, extent, leaves, parent),
                3 => Repr::from_leaves(Repr::Leaves3, extent, leaves, parent),
                4 => Repr::from_leaves(Repr::Leaves4, extent, leaves, parent),
                5 => Repr::from_leaves(Repr::Leaves5, extent, leaves, parent),
                6 => Repr::from_leaves(Repr::Leaves6, extent, leaves, parent),
                _ => unreachable!(),
            },
        )
    }

    fn from_nodes_unchecked(
        extent: Splittable<CachedExtent>,
        nodes: ArrayVec<Self, { Splits::MAX_VOLUME_USIZE }>,
        parent: Self::Parent,
    ) -> Self {
        Self(
            match extent.total_child_splits().expect("invalid extent").get() {
                1 => Repr::from_nodes(Repr::Parent1, extent, nodes, parent),
                2 => Repr::from_nodes(Repr::Parent2, extent, nodes, parent),
                3 => Repr::from_nodes(Repr::Parent3, extent, nodes, parent),
                4 => Repr::from_nodes(Repr::Parent4, extent, nodes, parent),
                5 => Repr::from_nodes(Repr::Parent5, extent, nodes, parent),
                6 => Repr::from_nodes(Repr::Parent6, extent, nodes, parent),
                _ => unreachable!(),
            },
        )
    }

    fn as_data(&self) -> NodeDataRef<Self> {
        match &self.0 {
            Repr::Leaf(_, leaf) => NodeDataRef::Leaf(leaf),
            Repr::Leaves1(_, node) => NodeDataRef::Parent(&node.parent, &node.cache),
            Repr::Leaves2(_, node) => NodeDataRef::Parent(&node.parent, &node.cache),
            Repr::Leaves3(_, node) => NodeDataRef::Parent(&node.parent, &node.cache),
            Repr::Leaves4(_, node) => NodeDataRef::Parent(&node.parent, &node.cache),
            Repr::Leaves5(_, node) => NodeDataRef::Parent(&node.parent, &node.cache),
            Repr::Leaves6(_, node) => NodeDataRef::Parent(&node.parent, &node.cache),
            Repr::Parent1(_, node) => NodeDataRef::Parent(&node.parent, &node.cache),
            Repr::Parent2(_, node) => NodeDataRef::Parent(&node.parent, &node.cache),
            Repr::Parent3(_, node) => NodeDataRef::Parent(&node.parent, &node.cache),
            Repr::Parent4(_, node) => NodeDataRef::Parent(&node.parent, &node.cache),
            Repr::Parent5(_, node) => NodeDataRef::Parent(&node.parent, &node.cache),
            Repr::Parent6(_, node) => NodeDataRef::Parent(&node.parent, &node.cache),
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
        let child_extent =
            |extent: &CachedExtent| extent.child_extent().expect("node should have children");
        match &self.0 {
            Repr::Leaf(..) => panic!("leaf nodes have no children"),
            Repr::Leaves1(extent, node) => NodeRef::Leaf(child_extent(extent), &node.leaves[index]),
            Repr::Leaves2(extent, node) => NodeRef::Leaf(child_extent(extent), &node.leaves[index]),
            Repr::Leaves3(extent, node) => NodeRef::Leaf(child_extent(extent), &node.leaves[index]),
            Repr::Leaves4(extent, node) => NodeRef::Leaf(child_extent(extent), &node.leaves[index]),
            Repr::Leaves5(extent, node) => NodeRef::Leaf(child_extent(extent), &node.leaves[index]),
            Repr::Leaves6(extent, node) => NodeRef::Leaf(child_extent(extent), &node.leaves[index]),
            Repr::Parent1(_, node) => NodeRef::Node(&node.children[index]),
            Repr::Parent2(_, node) => NodeRef::Node(&node.children[index]),
            Repr::Parent3(_, node) => NodeRef::Node(&node.children[index]),
            Repr::Parent4(_, node) => NodeRef::Node(&node.children[index]),
            Repr::Parent5(_, node) => NodeRef::Node(&node.children[index]),
            Repr::Parent6(_, node) => NodeRef::Node(&node.children[index]),
        }
    }
}

impl<T: Clone, P, C> Clone for Node<T, P, C> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<T: Hash, P: Hash, C> Hash for Node<T, P, C> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl<'a, T, P, C> IntoIterator for &'a Node<T, P, C>
where
    C: for<'b> Cache<&'b T, Ref<'b> = &'b C>,
{
    type Item = (CachedBounds, NodeRef<'a, Node<T, P, C>>);
    type IntoIter = Iter<'a, Node<T, P, C>>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<T, P, C> HasExtent for Node<T, P, C> {
    fn extent(&self) -> Extent {
        self.cached_extent().strip_cache()
    }
}

impl<T, P, C> HasCachedExtent for Node<T, P, C> {
    fn cached_extent(&self) -> CachedExtent {
        match self.0 {
            Repr::Leaf(extent, _) => extent,
            Repr::Leaves1(extent, _)
            | Repr::Leaves2(extent, _)
            | Repr::Leaves3(extent, _)
            | Repr::Leaves4(extent, _)
            | Repr::Leaves5(extent, _)
            | Repr::Leaves6(extent, _)
            | Repr::Parent1(extent, _)
            | Repr::Parent2(extent, _)
            | Repr::Parent3(extent, _)
            | Repr::Parent4(extent, _)
            | Repr::Parent5(extent, _)
            | Repr::Parent6(extent, _) => *extent,
        }
    }
}

impl<T, P, C> IsParent for Node<T, P, C> {
    fn is_parent(&self) -> bool {
        !matches!(self.0, Repr::Leaf(..))
    }
}

impl<T: HexDivNode> Parent<&T> {
    pub fn extent(&self) -> Splittable<CachedExtent> {
        Splittable::new_unchecked(self.cached_extent())
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

#[derive(Debug, PartialEq, Eq)]
enum Repr<T, P, C> {
    Leaf(CachedExtent, T),
    Leaves1(Splittable<CachedExtent>, Arc<LeavesNode<T, 2, P, C>>),
    Leaves2(Splittable<CachedExtent>, Arc<LeavesNode<T, 4, P, C>>),
    Leaves3(Splittable<CachedExtent>, Arc<LeavesNode<T, 8, P, C>>),
    Leaves4(Splittable<CachedExtent>, Arc<LeavesNode<T, 16, P, C>>),
    Leaves5(Splittable<CachedExtent>, Arc<LeavesNode<T, 32, P, C>>),
    Leaves6(Splittable<CachedExtent>, Arc<LeavesNode<T, 64, P, C>>),
    Parent1(Splittable<CachedExtent>, Arc<ParentNode<T, 2, P, C>>),
    Parent2(Splittable<CachedExtent>, Arc<ParentNode<T, 4, P, C>>),
    Parent3(Splittable<CachedExtent>, Arc<ParentNode<T, 8, P, C>>),
    Parent4(Splittable<CachedExtent>, Arc<ParentNode<T, 16, P, C>>),
    Parent5(Splittable<CachedExtent>, Arc<ParentNode<T, 32, P, C>>),
    Parent6(Splittable<CachedExtent>, Arc<ParentNode<T, 64, P, C>>),
}

type NewLeaves<T, const N: usize, P, C> =
    fn(Splittable<CachedExtent>, Arc<LeavesNode<T, N, P, C>>) -> Repr<T, P, C>;

type NewParent<T, const N: usize, P, C> =
    fn(Splittable<CachedExtent>, Arc<ParentNode<T, N, P, C>>) -> Repr<T, P, C>;

impl<T, P, C> Repr<T, P, C>
where
    C: for<'a> Cache<&'a T, Ref<'a> = &'a C>,
{
    fn from_leaves<const N: usize>(
        new: NewLeaves<T, N, P, C>,
        extent: Splittable<CachedExtent>,
        leaves: ArrayVec<T, { Splits::MAX_VOLUME_USIZE }>,
        parent: P,
    ) -> Self {
        let leaves = array_init::from_iter(leaves).expect("leaves should have correct length");
        let cache = C::compute_cache(extent.child_extent(), leaves.iter().map(CacheIn::Leaf));
        new(
            extent,
            Arc::new(LeavesNode {
                leaves,
                parent,
                cache,
            }),
        )
    }

    fn from_nodes<const N: usize>(
        new: NewParent<T, N, P, C>,
        extent: Splittable<CachedExtent>,
        nodes: ArrayVec<Node<T, P, C>, { Splits::MAX_VOLUME_USIZE }>,
        parent: P,
    ) -> Self {
        let children = array_init::from_iter(nodes).expect("leaves should have correct length");
        let cache = C::compute_cache(
            extent.child_extent(),
            children.iter().map(CacheIn::from_node),
        );
        new(
            extent,
            Arc::new(ParentNode {
                children,
                parent,
                cache,
            }),
        )
    }
}

impl<T: Clone, P, C> Clone for Repr<T, P, C> {
    fn clone(&self) -> Self {
        match self {
            Self::Leaf(extent, leaf) => Self::Leaf(*extent, leaf.clone()),
            Self::Leaves1(extent, node) => Self::Leaves1(*extent, node.clone()),
            Self::Leaves2(extent, node) => Self::Leaves2(*extent, node.clone()),
            Self::Leaves3(extent, node) => Self::Leaves3(*extent, node.clone()),
            Self::Leaves4(extent, node) => Self::Leaves4(*extent, node.clone()),
            Self::Leaves5(extent, node) => Self::Leaves5(*extent, node.clone()),
            Self::Leaves6(extent, node) => Self::Leaves6(*extent, node.clone()),
            Self::Parent1(extent, node) => Self::Parent1(*extent, node.clone()),
            Self::Parent2(extent, node) => Self::Parent2(*extent, node.clone()),
            Self::Parent3(extent, node) => Self::Parent3(*extent, node.clone()),
            Self::Parent4(extent, node) => Self::Parent4(*extent, node.clone()),
            Self::Parent5(extent, node) => Self::Parent5(*extent, node.clone()),
            Self::Parent6(extent, node) => Self::Parent6(*extent, node.clone()),
        }
    }
}

impl<T: Hash, P: Hash, C> Hash for Repr<T, P, C> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        discriminant(self).hash(state);
        match self {
            Self::Leaf(extent, leaf) => {
                extent.hash(state);
                leaf.hash(state);
            }
            Self::Leaves1(extent, node) => {
                extent.hash(state);
                node.hash(state);
            }
            Self::Leaves2(extent, node) => {
                extent.hash(state);
                node.hash(state);
            }
            Self::Leaves3(extent, node) => {
                extent.hash(state);
                node.hash(state);
            }
            Self::Leaves4(extent, node) => {
                extent.hash(state);
                node.hash(state);
            }
            Self::Leaves5(extent, node) => {
                extent.hash(state);
                node.hash(state);
            }
            Self::Leaves6(extent, node) => {
                extent.hash(state);
                node.hash(state);
            }
            Self::Parent1(extent, node) => {
                extent.hash(state);
                node.hash(state);
            }
            Self::Parent2(extent, node) => {
                extent.hash(state);
                node.hash(state);
            }
            Self::Parent3(extent, node) => {
                extent.hash(state);
                node.hash(state);
            }
            Self::Parent4(extent, node) => {
                extent.hash(state);
                node.hash(state);
            }
            Self::Parent5(extent, node) => {
                extent.hash(state);
                node.hash(state);
            }
            Self::Parent6(extent, node) => {
                extent.hash(state);
                node.hash(state);
            }
        }
    }
}

/// Holds storage for `N` leaf nodes of type `T`, parent data `P` and a cached value `C`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct LeavesNode<T, const N: usize, P, C> {
    cache: C,
    leaves: [T; N],
    parent: P,
}

impl<T: Hash, const N: usize, P: Hash, C> Hash for LeavesNode<T, N, P, C> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // intentionally ignore cache
        self.leaves.hash(state);
        self.parent.hash(state);
    }
}

/// Holds storage for `N` child [`Node`]s, parent data `P` and a cached value `C`.
#[derive(Clone, Debug, PartialEq, Eq)]
struct ParentNode<T, const N: usize, P, C> {
    cache: C,
    children: [Node<T, P, C>; N],
    parent: P,
}

impl<T: Hash, const N: usize, P: Hash, C> Hash for ParentNode<T, N, P, C> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // intentionally ignore cache
        self.children.hash(state);
        self.parent.hash(state);
    }
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
pub trait HexDivNode: HasCachedExtent + IsParent + Sized {
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
    fn new(extent: CachedExtent, leaf: Self::Leaf) -> Self;

    /// Construtcs a [`HexDivNode`] of the specified `extent` filled with the [`Default`] leaf.
    fn with_default(extent: CachedExtent) -> Self
    where
        Self::Leaf: Default,
    {
        Self::new(extent, Default::default())
    }

    /// Constructs a [`HexDivNode`] from a set of `children`.
    ///
    /// Prefer using [`Builder`](builder::Builder), which is far more flexible and less error prone.
    ///
    /// # Panics
    ///
    /// Panics if the number of `children` does not match the number of `splits`.
    ///
    /// Panics if not all `children` share the same extent.
    ///
    /// Panics if the resulting node would be bigger than [`Extent::MAX`].
    fn from_children(
        mut children: impl Iterator<Item = Self>,
        splits: Splits,
        parent: Self::Parent,
    ) -> Self
    where
        Self::Leaf: Clone + Eq,
    {
        let first = children.next().expect("children should not be empty");
        let child_extent = first.cached_extent();
        let extent = child_extent
            .parent_extent(splits)
            .expect("HexDivNode too big");

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

                                return Self::from_leaves_unchecked(extent, leaves, parent);
                            }
                        }
                        Err(child) => {
                            return Self::from_nodes_unchecked(
                                extent,
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

                Self::new(*extent, first)
            }
            Err(first) => Self::from_nodes_unchecked(
                extent,
                once(first)
                    .chain(children.map(|(_, child)| child))
                    .collect(),
                parent,
            ),
        }
    }

    /// Do not call this directly!
    ///
    /// Constructs a [`HexDivNode`] from a [`HexDivNode::LeavesBuilder`].
    fn from_leaves_unchecked(
        extent: Splittable<CachedExtent>,
        leaves: Self::LeavesBuilder,
        parent: Self::Parent,
    ) -> Self;

    /// Do not call this directly!
    ///
    /// Constructs a [`HexDivNode`] from a list of nodes.
    fn from_nodes_unchecked(
        extent: Splittable<CachedExtent>,
        nodes: ArrayVec<Self, { Splits::MAX_VOLUME_USIZE }>,
        parent: Self::Parent,
    ) -> Self;

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
        *self = Self::new(self.cached_extent(), leaf);
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
pub enum NodeDataRef<'a, T: HexDivNode<Leaf: 'a>> {
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

impl<'a, T: HexDivNode<Leaf: 'a>> Clone for NodeDataRef<'a, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'a, T: HexDivNode<Leaf: 'a>> Copy for NodeDataRef<'a, T> {}

impl<'a, T: HexDivNode<Leaf: 'a>> fmt::Debug for NodeDataRef<'a, T>
where
    T::LeafRef<'a>: fmt::Debug,
    T::Parent: fmt::Debug,
    CacheRef<'a, T>: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Leaf(leaf) => f.debug_tuple("Leaf").field(leaf).finish(),
            Self::Parent(parent, cache) => {
                f.debug_tuple("Parent").field(parent).field(cache).finish()
            }
        }
    }
}

impl<'a, T: HexDivNode> Hash for NodeDataRef<'a, T>
where
    T::LeafRef<'a>: Hash,
    T::Parent: Hash,
    CacheRef<'a, T>: Hash,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        discriminant(self).hash(state);
        match self {
            Self::Leaf(leaf) => {
                leaf.hash(state);
            }
            Self::Parent(parent, cache) => {
                parent.hash(state);
                cache.hash(state);
            }
        }
    }
}

impl<'a, T: HexDivNode> PartialEq for NodeDataRef<'a, T>
where
    T::LeafRef<'a>: PartialEq,
    T::Parent: PartialEq,
    CacheRef<'a, T>: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Leaf(leaf), Self::Leaf(other_leaf)) => leaf == other_leaf,
            (Self::Parent(parent, cache), Self::Parent(other_parent, other_cache)) => {
                parent == other_parent && cache == other_cache
            }
            _ => false,
        }
    }
}

impl<'a, T: HexDivNode> Eq for NodeDataRef<'a, T>
where
    T::LeafRef<'a>: Eq,
    T::Parent: Eq,
    CacheRef<'a, T>: Eq,
{
}

/// A reference to a [`HexDivNode`] node.
///
/// [`NodeRef::Leaf`] is used (exclusively) for virtual nodes used by leaves nodes.
#[derive(Debug, Hash, PartialEq, Eq)]
pub enum NodeRef<'a, T: HexDivNode> {
    Node(&'a T),
    /// Not [`CachedExtent`] since it is not already stored in the [`HexDivNode`].
    Leaf(Extent, T::LeafRef<'a>),
}

impl<'a, T: HexDivNode> NodeRef<'a, T> {
    pub fn data(self) -> NodeDataRef<'a, T> {
        match self {
            Self::Node(node) => node.as_data(),
            Self::Leaf(_, leaf) => NodeDataRef::Leaf(leaf),
        }
    }

    pub fn as_parent(&self) -> Option<Parent<&'a T>> {
        if let Self::Node(node) = self {
            Parent::new(node)
        } else {
            None
        }
    }
}

impl<T: HexDivNode> Clone for NodeRef<'_, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: HexDivNode> Copy for NodeRef<'_, T> {}

impl<T: HexDivNode> HasExtent for NodeRef<'_, T> {
    fn extent(&self) -> Extent {
        self.cached_extent().strip_cache()
    }
}

impl<T: HexDivNode> HasCachedExtent for NodeRef<'_, T> {
    fn cached_extent(&self) -> CachedExtent {
        match self {
            Self::Node(node) => node.cached_extent(),
            Self::Leaf(extent, _) => extent.compute_cache(),
        }
    }
}

impl<T: HexDivNode> IsParent for NodeRef<'_, T> {
    fn is_parent(&self) -> bool {
        matches!(self, Self::Node(node) if node.is_parent())
    }
}

pub struct HexDivDebug<'a, T> {
    node: &'a T,
}

impl<'a, T: HexDivNode> fmt::Debug for HexDivDebug<'a, T>
where
    T::LeafRef<'a>: fmt::Debug,
    T::Parent: fmt::Debug,
    CacheRef<'a, T>: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.node.as_data() {
            NodeDataRef::Leaf(leaf) => f
                .debug_struct("Leaf")
                .field("extent", &self.node.cached_extent())
                .field("leaf", &leaf)
                .finish(),
            NodeDataRef::Parent(parent, cache) => f
                .debug_struct("Parent")
                .field("extent", &self.node.cached_extent())
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

impl<'a, T: HexDivNode> fmt::Debug for ChildrenDebug<'a, T>
where
    T::LeafRef<'a>: fmt::Debug,
    T::Parent: fmt::Debug,
    CacheRef<'a, T>: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut debug_list = f.debug_list();
        for index in 0..self.node.extent().total_splits() {
            match self.node.get_child(index) {
                NodeRef::Node(node) => debug_list.entry(&HexDivDebug { node }),
                // TODO: LeafDebug that prints { extent: _, leaf: 42 } or just 42 if extent is ONE
                NodeRef::Leaf(extent, leaf) => debug_list.entry(&(extent, leaf)),
            };
        }
        debug_list.finish()
    }
}

/// Can be used to wrap a [`Node`]'s [`Cache`] to prevent it from being compared.
///
/// See [`Node`] for more info.
///
/// Intentionall does not implement [`Hash`], [`PartialOrd`] and [`Ord`], since this is only meant
/// to be used for [`Node`], which does not make use of any of these traits.
#[derive(Clone, Copy, Debug, Default)]
pub struct CacheAlwaysEq<C>(pub C);

impl<C> PartialEq for CacheAlwaysEq<C> {
    fn eq(&self, _: &Self) -> bool {
        true
    }
}

impl<C> Eq for CacheAlwaysEq<C> {}

/// A type that is known to be a parent node with child nodes.
///
/// This differs from [`Splittable`], in that [`Splittable`] merely indicates, that the [`Extent`]
/// itself can be split, while [`Parent`] goes one step further, requiring the node to not be a leaf
/// node.
///
/// A node not being [`Splittable`] implies, that it must be a leaf node and cannot be a [`Parent`].
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Parent<T>(T);

impl<T> Deref for Parent<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T: Clone> Parent<&T> {
    fn cloned(&self) -> Parent<T> {
        Parent(self.0.clone())
    }
}

impl<T: IsParent> Parent<T> {
    pub fn new(inner: T) -> Option<Self> {
        inner.is_parent().then_some(Self(inner))
    }

    pub fn new_unchecked(inner: T) -> Self {
        debug_assert!(inner.is_parent());
        Self(inner)
    }
}

/// A type that knows if it is a parent node.
pub trait IsParent {
    fn is_parent(&self) -> bool;
}

impl<T: IsParent> IsParent for &T {
    fn is_parent(&self) -> bool {
        (*self).is_parent()
    }
}

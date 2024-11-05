pub mod bounds;
pub mod builder;
pub mod cache;
pub mod extent;
pub mod iter;
pub mod node;
pub mod visit;

use std::iter::{once, repeat_n};

use arrayvec::ArrayVec;
use cache::Cache;
use educe::Educe;
use extent::{Extent, SplitList, Splits};
use iter::Iter;
use itertools::zip_eq;

/// An [Octree] inspired sparse 3D array.
///
/// # Trait Implementations
///
/// The [`node`] submodule contains implementors of this trait:
///
/// - [`node::Node`] as a general purpose implementation that works for all types
/// - [`node::bool::Node`] which is limited to [`bool`], but has very efficient storage
///
/// Since a [`HexDiv`] is usually made up of a lot of nodes, the concrete node type should generally
/// strive for a small footprint; two pointers ideally.
///
/// # Not an [Octree]
///
/// When compared to a normal [Octree], a [`HexDiv`]:
///
/// - Is more flexible in its shape
/// - Is better optimized for cache locality by having nodes store more direct children
///
/// An [Octree] is generally limited to a cube shape with a side length that is a power of two. A
/// [`HexDiv`] lifts the "cube shape" restriction, allowing any cuboid shape, only requiring each
/// individual side length to be a power of two. This has the nice benefit, that a [`HexDiv`] can
/// also be used in place of a [Quadtree] (a 2D [Octree]) or even for just a 1D strip.
///
/// # Parent Nodes
///
/// Parent nodes can hold arbitrary additional data of type [`HexDiv::Parent`].
///
/// TODO: Use cases?
///
/// Additionally, parent nodes can also hold an immutable cached value of type [`HexDiv::Cache`],
/// which is automatically derived from its children. E.g. [`node::bool::NodeWithCount`] caches the
/// total number of `true` leaves in its parent nodes.
///
/// [Octree]: https://en.wikipedia.org/wiki/Octree
/// [Quadtree]: https://en.wikipedia.org/wiki/Quadtree
pub trait HexDiv: Clone {
    /// The type of leaf values that this [`HexDiv`] holds.
    ///
    /// Leaves must be:
    ///
    /// - [`Clone`] for when a leaf node is split into a parent node
    /// - [`Eq`] to know if a parent node can be merged back into a leaf node
    type Leaf: Clone + Eq;

    /// A reference to a [`HexDiv::Leaf`], usually just `&Self::Leaf`.
    ///
    /// The main use-case for this customization point is to avoid having to hand to `&bool`, which
    /// is impossible if [`bool`] leaves are stored as bits in e.g. a [`u64`].
    type LeafRef<'a>: Copy
    where
        Self::Leaf: 'a;

    /// Used by [`HexDiv::from_leaves_unchecked`].
    type LeavesBuilder: FromIterator<Self::Leaf>
        + Extend<Self::Leaf>
        + IntoIterator<Item = Self::Leaf>;

    /// Arbitrary data for parent nodes.
    type Parent;

    /// Can be queried cheaply for any parent node.
    type Cache<'a>: Cache<'a, Self::LeafRef<'a>>
    where
        Self::Leaf: 'a;

    /// Constructs a new instance of the specified `extent` filled with the given `leaf`.
    fn new(extent: Extent, leaf: Self::Leaf) -> Self;

    /// Construtcs a new instance of the specified `extent` filled with the [`Default`] leaf value.
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
    ) -> Self {
        let total_splits = splits.total();
        let count = 1 << total_splits;

        let first = children.next().expect("children should not be empty");
        let child_extent = first.extent();
        let extent = child_extent.unsplit(splits);

        // ensure all children have same extent
        let children = children.inspect(|child| assert_eq!(child.extent(), child_extent));

        // ensure there is exactly count children
        let mut children = zip_eq(1..count, children);

        match first.into_leaf() {
            Ok(first) => {
                for (already_processed, child) in &mut children {
                    match child.into_leaf() {
                        Ok(leaf) => {
                            if leaf != first {
                                let mut leaves = repeat_n(first, already_processed)
                                    .chain([leaf])
                                    .collect::<Self::LeavesBuilder>();

                                for (_, child) in &mut children {
                                    match child.into_leaf() {
                                        Ok(leaf) => {
                                            leaves.extend([leaf]);
                                        }
                                        Err(child) => {
                                            return Self::from_nodes_unchecked(
                                                total_splits,
                                                extent,
                                                child_extent,
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

                                return Self::from_leaves_unchecked(
                                    total_splits,
                                    extent,
                                    child_extent,
                                    leaves,
                                    parent,
                                );
                            }
                        }
                        Err(child) => {
                            return Self::from_nodes_unchecked(
                                total_splits,
                                extent,
                                child_extent,
                                repeat_n(Self::new(child_extent, first), already_processed)
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
                total_splits,
                extent,
                child_extent,
                once(first)
                    .chain(children.map(|(_, child)| child))
                    .collect(),
                parent,
            ),
        }
    }

    /// Do not call this directly! Use [`HexDiv::from_children`] instead.
    ///
    /// Constructs a new instance from a [`HexDiv::LeavesBuilder`].
    fn from_leaves_unchecked(
        total_splits: u8,
        extent: Extent,
        child_extent: Extent,
        leaves: Self::LeavesBuilder,
        parent: Self::Parent,
    ) -> Self;

    /// Do not call this directly! Use [`HexDiv::from_children`] instead.
    ///
    /// Constructs a new instance from a list of nodes.
    fn from_nodes_unchecked(
        total_splits: u8,
        extent: Extent,
        child_extent: Extent,
        nodes: ArrayVec<Self, { Splits::MAX_VOLUME_USIZE }>,
        parent: Self::Parent,
    ) -> Self;

    /// Returns the extent of this node.
    fn extent(&self) -> Extent;

    /// Returns a reference to the underlying data of this node.
    ///
    /// - For leaf nodes: [`HexDiv::LeafRef`]
    /// - For parent nodes: [`&HexDiv::Parent`](HexDiv::Parent), [`Cache::Ref`]
    fn as_data(&self) -> NodeDataRef<Self>;

    /// Unwraps a leaf node into [`HexDiv::Leaf`].
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

    /// Returns a [`Debug`](std::fmt::Debug) implementation with concise output.
    fn debug(&self) -> HexDivDebug<Self> {
        HexDivDebug {
            node: self,
            split_list: self.extent().to_split_list(),
            split_index: 0,
        }
    }
}

/// Shorthand to access the [`Cache::Ref`] on a [`HexDiv::Cache`].
pub type CacheRef<'a, T> = <<T as HexDiv>::Cache<'a> as Cache<'a, <T as HexDiv>::LeafRef<'a>>>::Ref;

/// A reference to the data of a [`HexDiv`] node.
#[derive(Educe)]
#[educe(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum NodeDataRef<'a, T: HexDiv + 'a> {
    Leaf(T::LeafRef<'a>),
    Parent(&'a T::Parent, CacheRef<'a, T>),
}

impl<'a, T: HexDiv> NodeDataRef<'a, T> {
    pub fn is_leaf(self) -> bool {
        matches!(self, Self::Leaf(..))
    }

    pub fn is_parent(self) -> bool {
        matches!(self, Self::Parent(..))
    }
}

/// A reference to a [`HexDiv`] node.
///
/// [`NodeRef::Leaf`] is used (exclusively) for virtual nodes used by leaves nodes.
#[derive(Educe)]
#[educe(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum NodeRef<'a, T: HexDiv + 'a> {
    Node(&'a T),
    Leaf(T::LeafRef<'a>),
}

impl<'a, T: HexDiv> NodeRef<'a, T> {
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
}

/// A reference to a [`HexDiv`] node that is known to have children.
#[derive(Educe)]
#[educe(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct ParentNodeRef<'a, T>(&'a T);

impl<'a, T: HexDiv> ParentNodeRef<'a, T> {
    pub fn new(node: &'a T) -> Option<Self> {
        node.as_data().is_parent().then_some(Self(node))
    }

    pub fn get(self) -> &'a T {
        self.0
    }
}

pub struct HexDivDebug<'a, T> {
    node: &'a T,
    split_list: SplitList,
    split_index: usize,
}

impl<'a, T: HexDiv> std::fmt::Debug for HexDivDebug<'a, T>
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
                .field(
                    "children",
                    &(ChildrenDebug {
                        node: self.node,
                        split_list: self.split_list,
                        split_index: self.split_index,
                    }),
                )
                .finish(),
        }
    }
}

struct ChildrenDebug<'a, T> {
    node: &'a T,
    split_list: SplitList,
    split_index: usize,
}

impl<'a, T: HexDiv> std::fmt::Debug for ChildrenDebug<'a, T>
where
    T::LeafRef<'a>: std::fmt::Debug,
    T::Parent: std::fmt::Debug,
    CacheRef<'a, T>: std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let count = self.split_list.level(self.split_index).volume();
        let mut debug_list = f.debug_list();
        for index in 0..count {
            match self.node.get_child(index) {
                NodeRef::Node(node) => debug_list.entry(&HexDivDebug {
                    node,
                    split_list: self.split_list,
                    split_index: self.split_index + 1,
                }),
                NodeRef::Leaf(leaf) => debug_list.entry(&leaf),
            };
        }
        debug_list.finish()
    }
}

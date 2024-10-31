pub mod bounds;
pub mod builder;
pub mod cache;
pub mod extent;
pub mod iter;
pub mod node;
pub mod visit;

use std::iter::{once, repeat_n};

use arrayvec::ArrayVec;
use cache::OctreeCache;
use derive_where::derive_where;
use extent::{OctreeExtent, OctreeSplitList, OctreeSplits};
use iter::Iter;
use itertools::zip_eq;

// TODO: Consider a custom Debug impl for octree nodes that forward to some generic implementation
// on OctreeNode

/// An owned octree node holding leaves of type [`OctreeNode::Leaf`].
///
/// Unlike with normal octrees, the side lengths do not have to be equal. In fact, this type can be
/// used perfectly fine as a quadtree as well.
///
/// Octree subdivisions are not limited to `2x2x2` and instead store up to 64 child nodes. This will
/// usually result in subdivisions of size `4x4x4`, but could also be e.g. `8x8x1`, `2x4x8` or
/// `2x1x1`.
///
/// Parent nodes can hold arbitrary additional data of type [`OctreeNode::Parent`] as well as an
/// immutable cached value of type [`OctreeNode::Cache`] which is calculated based on leaf nodes and
/// already calculated cached values.
///
/// Since an octree is usually made up of a lot of nodes, types implementing this trait should
/// generally strive for a size of two pointers.
pub trait OctreeNode: Clone {
    /// The type of leaf values that this node can hold.
    type Leaf: Clone + Eq;

    /// A reference to a leaf, usually `&Self::Leaf`.
    ///
    /// Allows e.g. storing [`bool`]s as single bits by using [`bool`] as `LeafRef`.
    type LeafRef<'a>: Copy
    where
        Self::Leaf: 'a;

    type LeavesBuilder: FromIterator<Self::Leaf>
        + Extend<Self::Leaf>
        + IntoIterator<Item = Self::Leaf>;

    /// The type of extra data that parent nodes can hold.
    type Parent: Clone;

    /// The type of cached leaf value.
    ///
    /// Calculated from [`OctreeNode::Leaf`] and already calculated [`OctreeNode::Cache`].
    type Cache<'a>: OctreeCache<'a, Self::LeafRef<'a>>
    where
        Self::Leaf: 'a;

    /// Constructs a new [`OctreeNode`] of the specified `extent` holding just the given `leaf`.
    fn new(extent: OctreeExtent, leaf: Self::Leaf) -> Self;

    /// Construtcs a new [`OctreeNode`] of the specified `extent` with the [`Default`] leaf value.
    fn with_default(extent: OctreeExtent) -> Self
    where
        Self::Leaf: Default,
    {
        Self::new(extent, Self::Leaf::default())
    }

    /// Constructs a new [`OctreeNode`] from a set of `children`.
    ///
    /// # Panics
    ///
    /// Panics if the number of `children` does not match the new `extent`.
    fn from_children(
        mut children: impl Iterator<Item = Self>,
        splits: OctreeSplits,
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

    fn from_leaves_unchecked(
        total_splits: u8,
        extent: OctreeExtent,
        child_extent: OctreeExtent,
        leaves: Self::LeavesBuilder,
        parent: Self::Parent,
    ) -> Self;

    fn from_nodes_unchecked(
        total_splits: u8,
        extent: OctreeExtent,
        child_extent: OctreeExtent,
        nodes: ArrayVec<Self, { OctreeSplits::MAX_VOLUME_USIZE }>,
        parent: Self::Parent,
    ) -> Self;

    /// Returns the extent of this [`OctreeNode`].
    fn extent(&self) -> OctreeExtent;

    /// Returns a reference to the underlying data of this [`OctreeNode`].
    ///
    /// - For leaf nodes: [`OctreeNode::LeafRef`]
    /// - For parent nodes: [`&OctreeNode::Parent`](OctreeNode::Parent), [`OctreeCache::Ref`]
    fn as_data(&self) -> NodeDataRef<Self>;

    fn into_leaf(self) -> Result<Self::Leaf, Self>;

    /// Fills the entire node with the given leaf while keeping the original extent.
    fn fill(&mut self, leaf: Self::Leaf) {
        *self = Self::new(self.extent(), leaf);
    }

    /// Returns an iterator that traverses the nodes in the octree depth-first.
    fn iter(&self) -> Iter<Self> {
        Iter::new(self)
    }

    /// Returns a reference to one of the children of this node.
    ///
    /// # Panics
    ///
    /// Panics if `index` is out of bounds or when called on a leaf node.
    fn get_child(&self, index: u8) -> NodeRef<Self>;

    fn debug(&self) -> DebugOctree<Self> {
        DebugOctree {
            node: self,
            split_list: self.extent().to_split_list(),
            split_index: 0,
        }
    }
}

pub struct DebugOctree<'a, T> {
    node: &'a T,
    split_list: OctreeSplitList,
    split_index: usize,
}

impl<'a, T: OctreeNode> std::fmt::Debug for DebugOctree<'a, T>
where
    T::LeafRef<'a>: std::fmt::Debug,
    T::Parent: std::fmt::Debug,
    CacheRef<'a, T>: std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut debug_struct = f.debug_struct("OctreeNode");
        debug_struct.field("extent", &self.node.extent());
        match self.node.as_data() {
            NodeDataRef::Leaf(leaf) => {
                debug_struct.field("leaf", &leaf);
            }
            NodeDataRef::Parent(parent, cache) => {
                debug_struct.field("parent", parent);
                debug_struct.field("cache", &cache);
                debug_struct.field(
                    "children",
                    &(DebugChildren {
                        node: self.node,
                        split_list: self.split_list,
                        split_index: self.split_index,
                    }),
                );
            }
        }
        debug_struct.finish()
    }
}

struct DebugChildren<'a, T> {
    node: &'a T,
    split_list: OctreeSplitList,
    split_index: usize,
}

impl<'a, T: OctreeNode> std::fmt::Debug for DebugChildren<'a, T>
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
                NodeRef::Node(node) => debug_list.entry(&DebugOctree {
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

type CacheRef<'a, T> =
    <<T as OctreeNode>::Cache<'a> as OctreeCache<'a, <T as OctreeNode>::LeafRef<'a>>>::Ref;

/// A reference to leaf, parent and cache data of a node.
#[derive_where(Clone, Copy)]
#[derive_where(Debug, Hash, PartialEq, Eq; T::LeafRef<'a>, T::Parent, CacheRef<'a, T>)]
pub enum NodeDataRef<'a, T: OctreeNode + 'a> {
    Leaf(T::LeafRef<'a>),
    Parent(&'a T::Parent, CacheRef<'a, T>),
}

impl<'a, T: OctreeNode> NodeDataRef<'a, T> {
    pub fn is_leaf(self) -> bool {
        matches!(self, Self::Leaf(..))
    }

    pub fn is_parent(self) -> bool {
        matches!(self, Self::Parent(..))
    }
}

/// A reference to a node within an octree.
///
/// [`Self::Leaf`] is used exclusively if a leaf is part of a leaves nodes rather than its own node.
#[derive_where(Clone, Copy)]
#[derive_where(Debug, Hash, PartialEq, Eq; T, T::LeafRef<'a>)]
pub enum NodeRef<'a, T: OctreeNode + 'a> {
    Node(&'a T),
    Leaf(T::LeafRef<'a>),
}

impl<'a, T: OctreeNode> NodeRef<'a, T> {
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

/// A reference to a node that is known to have children.
#[derive_where(Clone, Copy)]
#[derive(Debug, Hash, PartialEq, Eq)]
pub struct ParentNodeRef<'a, T>(&'a T);

impl<'a, T: OctreeNode> ParentNodeRef<'a, T> {
    pub fn new(node: &'a T) -> Option<Self> {
        node.as_data().is_parent().then_some(Self(node))
    }

    pub fn get(self) -> &'a T {
        self.0
    }
}

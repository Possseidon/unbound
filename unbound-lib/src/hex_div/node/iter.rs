//! Iteration of [`HexDivNode`] via a new set of [`Iterator`] traits.
//!
//! # The Basics
//!
//! Any [`HexDivNode`] can be iterated via [`HexDivNode::iter`]. By default, all nodes are visited:
//!
//! ```
//! # use unbound_lib::hex_div::{
//! #     extent::Extent,
//! #     node::{HexDivNode, Node},
//! # };
//! let root = Node::<u32>::with_default(Extent::ONE);
//!
//! // node types usually also implement IntoIterator
//! for (bounds, node) in root.iter() {
//!     // all parent and leaf nodes are yielded
//!     // in this example this is of course only the root leaf node itself
//! }
//! ```
//!
//! # A New Iterator Trait
//!
//! While this works fine for simple cases, iterating over each and every node of a [`HexDivNode`]
//! is far from optimal, especially considering that they are _intended_ to be efficient for both
//! storage and spatial queries.
//!
//! So, let me introduce you to a new trait: [`HexDivIterator`]
//!
//! It extends [`Iterator`] with some new and shiny functionality that is not only useful for
//! working with [`HexDivNode`]s, but also efficient in the way it operates, which would not be
//! possible with a plain [`Iterator`].
//!
//! While it has a _somewhat_ low-level API, let's first focus on the high-level goodies. Need to
//! filter out specific nodes purely based on their location? [`HexDivIterator::filter_bounds`] has
//! got your back:
//!
//! ```
//! # use unbound_lib::hex_div::{
//! #     extent::Extent,
//! #     node::{
//! #         iter::{filter::Filter, HexDivIterator},
//! #         HexDivNode, Node,
//! #     },
//! # };
//! # let root = Node::<u32>::with_default(Extent::ONE);
//! let filtered = root.iter().filter_bounds(|bounds| {
//!     // filter based on bounds; for simplicity this just skips everything
//!     Filter::SKIP
//! });
//!
//! for (bounds, node) in filtered {
//!     // nothing in our case, since the root node was skipped due to the filter
//!     unreachable!()
//! }
//! ```
//!
//! For the common case of entering everything within certain target [`Bounds`] there are helpers
//! [`filter::bounds::within`] and [`filter::bounds::until`] that return an appropriate closure.
//!
//! There are other filtering options, but let's take a look at some other traits.
//!
//! # Other Traits
//!
//! [`HexDivIterator`] on its own is already quite neat. Not only does it bring new functionality,
//! but it also puts and additional guarantee on the underlying [`Iterator`]: Nodes must be returned
//! in depth-first order.
//!
//! Note however, that [`HexDivIterator`] itself does not imply that _all_ nodes within a
//! [`HexDivNode`] will be returned. Only the order in which nodes are yielded is guaranteed. E.g.,
//! _if_ the root node is yielded, it is guaranteed to be the very first node.
//!
//! This brings us to a bunch of new traits that add additional guarantees:
//!
//! - [`HexDivIterator`] - the base; only requires order to be depth-first; any node may be omitted
//!   - [`HexDivLeafIterator`] - requires all leaf nodes to be yielded; no guarantee on parent nodes
//!     - [`HexDivNodeIterator`] - requires all parent nodes to be yielded as well
//!     - [`HexDivLeafExIterator`] - leaf nodes must be yielded _exclusively_; no parent nodes
//!
//! The phrase "all leaf nodes" might sound a bit confusing. What it means is that, if any single
//! leaf node in particular is yielded, _all_ of its direct neighbors must also be yielded. As a
//! consequence, a [`HexDivLeafIterator`]'s leaf nodes are guaranteed to cover the entire bounds
//! without any holes.
//!
//! There is no guarantee on which parent nodes will be yielded. This can make it inconvenient to
//! work with for certain scenarios. E.g. trying to build a new [`HexDivNode`] from an iterator
//! works fine if you _only_ have leaf nodes (parent data can just be [`Default`]ed) or if you have
//! _all_ parent nodes, which allows filling parent data manually. [`HexDivLeafExIterator`] and
//! [`HexDivNodeIterator`] bring those exact guarantees respectively.
//!
//! # The Nitty Gritty
//!
//! Being able to create iterator adapeters is a core part of Rust's iterator design.
//! [`HexDivIterator`] uses the very same concept. Creating new adapters by chaining together some
//! of the existing ones is already quite powerful, but just in case you need even more control,
//! let's go over the low-level API of [`HexDivIterator`] and how to make use of it.
//!
//! ## Peek and Traverse
//!
//! What makes [`HexDivIterator`] different from a plain [`Iterator`] is mainly the ability to peek
//! at the node that will be yielded next, followed by efficiently skipping over certain nodes based
//! on that information.
//!
//! ### Peek
//!
//! [`HexDivIterator::peek_bounds`] is your main entry point to take a look at the [`CachedBounds`]
//! of the node that will be yielded next. There is also [`HexDivPeekNodeIterator::peek_node`] to
//! peek at the node's value. It is in a separate trait, since some iterator adapters don't provide
//! this functionality.
//!
//! [`HexDivIterator::has_children`] is also very important, as it can tell you whether the iterator
//! will enter a child node once [`Iterator::next`] is called.
//!
//! ### Traverse
//!
//! So, we know where and what we are now; but how do we actually make use of that information?
//! There's quite a few ways:
//!
//! - [`HexDivIterator::skip_node`] to just skip the current node entirely without ever yielding it
//!   or any of its children
//! - [`HexDivIterator::skip_children`] to prevent the current node's children from being entered in
//!   any way; this also affects the previously mentioned [`HexDivIterator::has_children`], which
//!   now returns `false`
//! - [`HexDivIterator::enter`] to _try_ skipping ahead to the first child of the current node
//! - [`HexDivIterator::focus`] which is similar to [`HexDivIterator::enter`], but focuses on a
//!   specific child node without ever yielding any of its direct neighbors
//!
//! ### Entering Child Nodes
//!
//! Both [`HexDivIterator::enter`] and [`HexDivIterator::focus`] return a [`bool`] indicating
//! whether they were able to enter. They essentially return the same thing as
//! [`HexDivIterator::has_children`].
//!
//! Where it gets interesting is what happens when they return `false`: If entering a node fails,
//! the iterator remains at the same location. It does _not_ skip ahead to the next node like
//! [`HexDivIterator::skip_node`].
//!
//! There is a actually a very good and clever reason behind this behavior: Semantically speaking, a
//! leaf node is equivalent to a parent node where all of its children have the same value. It is
//! merely an optimized version of that to take up less space and be easier to work with.
//!
//! So, what happens if you try to enter a leaf node? You simply just get back that same value of
//! the leaf node itself, since all of its "virtual" child nodes hold that same leaf value. Keep in
//! mind though, that this doesn't actually advance the iterator to that "virtual" child node's
//! bounds; it really does just stay exactly where it was.
//!
//! ### Focus
//!
//! Let's talk a bit about [`HexDivIterator::focus`] and why it exists.
//!
//! In the same way, that [`HexDivIterator::skip_node`] is an optimization for [`Iterator::filter`]
//! (allowing skipping entire parent nodes), [`HexDivIterator::focus`] can be seen as an
//! optimization of combining [`HexDivIterator::enter`] with [`HexDivIterator::skip_node`].
//!
//! It doesn't provide any "new" functionality. All of its functionality can be achieved by simply
//! [`HexDivIterator::enter`]ing the node and then calling [`HexDivIterator::skip_node`] for all but
//! the focused node.
//!
//! The thing is, often times you have a large [`HexDivNode`] and are looking for a very small
//! region within it. While the above works, it requires lots and lots of skipping to ignore all of
//! the uninteresting neighbor nodes. [`HexDivIterator::focus`] can do all that skipping for you.
//!
//! You might be asking yourself:
//!
//! > Isn't that harder to use though? And is it actually faster? Surely this just does the whole
//! > [`HexDivIterator::skip_node`] dance under the hood, no?
//!
//! Hoo boy, do I have good news for you! As it turns out, you can do some very clever calculations
//! (making use of some neat bit fiddling magic) to find if and which single child node index you
//! need to enter to reach any given target bounds. If you're interested in how this works, check
//! out how [`Filter::focus`] is implemented. No loops, just raw bit math to skip over all of the
//! remaining 63 neighbors in one, very elegant, fell swoop.
//!
//! While this is incredibly useful, the current implementation has one small downside: If you need
//! to enter even just two children, you'll have to fall back to the manual approach of skipping the
//! remaining 62 neighbors.
//!
//! While it would of course be possible to implement focusing multiple children, it would require a
//! lot more state to keep track of, blowing up the iterator's size quite a bit. Doing it for only
//! single children is relatively easy to keep track of, since it only requires storing _which_
//! parents were focused instead of entered regularly. Those bits of information (literal bits, heh)
//! neatly fit in a single [`u16`], since there can only ever be a chain of 16 parent nodes.

pub mod filter;
pub mod map;
pub mod zip;

use std::iter::FusedIterator;

use arrayvec::ArrayVec;
use bitvec::array::BitArray;
use filter::{
    bounds::FilterBounds, children::FilterChildren, leaves::FilterLeaves, nodes::FilterNodes,
    Filter,
};
use glam::UVec3;
use map::nodes::MapNodes;
use zip::{leaves::ZipLeaves, nodes::ZipNodes};

use super::{builder::Builder, HexDivNode, IsParent, NodeRef, Parent};
use crate::hex_div::{
    bounds::{Bounds, CachedBounds},
    extent::{CachedExtent, HasCachedExtent, Splittable},
    node::builder::BuildAction,
    splits::SplitList,
};

/// An [`Iterator`] over [`CachedBounds`] and some arbitrary [`HexDivIterator::Node`].
///
/// Nodes are guaranteed to be yielded in depth-first order, but any of those nodes can be omitted
/// without violating this contract.
pub trait HexDivIterator: FusedIterator<Item = (CachedBounds, Self::Node)> {
    /// The value that is yielded alongside each [`Bounds`].
    type Node;

    /// The full [`CachedExtent`] of the root node.
    ///
    /// This is useful to check if two iterators have the same child layout, which makes them
    /// compatible for e.g. zipping via [`HexDivLeafExIterator::zip_leaves`].
    ///
    /// Technically it would be enough to check [`HexDivIterator::peek_bounds`] to ensure layout
    /// compatibility. However if one of the two iterators' [`HexDivIterator::root_extent`] is
    /// bigger, then it was likely just a coincidence that they happened to end up with the same
    /// child layout. It's better to catch likely future mismatches early, so checking
    /// [`HexDivIterator::root_extent`] is a more defensive approach.
    ///
    /// The returned value never changes throughout iteration, even if iteration is already over.
    ///
    /// If only the area that the iterator might still yield from this point forward is of interest,
    /// [`HexDivIterator::peek_remaining_bounds_hint`] can give a decent estimate for that.
    fn root_extent(&self) -> CachedExtent;

    /// Whether the iterator is still located at the origin.
    ///
    /// "Origin" does _not_ mean, "covering the entire [`HexDivIterator::root_extent`]". It only
    /// means, that the current node is located at [`UVec3::ZERO`].
    ///
    /// For a [`HexDivLeafIterator`] this implies, that the remaining nodes to be yielded will cover
    /// the entire [`HexDivIterator::root_extent`].
    ///
    /// Returns `false` if iteration is already over.
    fn peek_at_origin(&mut self) -> bool {
        self.peek_bounds()
            .is_some_and(|bounds| bounds.min() == UVec3::ZERO)
    }

    /// Returns the [`CachedBounds`] of the node that will be yielded next.
    ///
    /// Returns [`None`] if iteration is already over.
    fn peek_bounds(&mut self) -> Option<CachedBounds>;

    /// The area within which this [`HexDivNodeIterator`] may still yield nodes.
    ///
    /// This is only a hint; the actual remaining bounds might be smaller (e.g. due to filtering),
    /// but never bigger.
    ///
    /// This does some clever calculations. E.g., if a specific child node of the root node was
    /// [`HexDivIterator::focus`]ed, the remaining bounds are known to never exceed these bounds.
    /// These calculations are fairly cheap but not free, so avoid calling this more than necessary.
    ///
    /// Returns [`None`] if iteration is already over.
    fn peek_remaining_bounds_hint(&mut self) -> Option<CachedBounds>;

    /// Advances the iterator to the next neighbor, skipping over its children if there are any.
    ///
    /// Note to implementers: Make sure that [`HexDivIterator::skip_children`] loses its effect.
    /// E.g. [`Iter`] has a `skip_children` flag that must be cleared by this function.
    ///
    /// # Panics
    ///
    /// Panics if iteration is already over.
    fn skip_node(&mut self);

    /// Whether the node that will be yielded next has any children.
    ///
    /// Returns `false` if [`HexDivIterator::skip_children`] was called on the current node already.
    ///
    /// # Panics
    ///
    /// Panics if iteration is already over.
    fn has_children(&mut self) -> bool;

    /// Prevents the current node's children from being entered by the iterator in any way.
    ///
    /// This includes not only [`Iterator::next`], but also other means such as
    /// [`HexDivIterator::enter`] and [`HexDivIterator::focus`]. Similarly it also causes
    /// [`HexDivIterator::has_children`] to return `false`.
    ///
    /// Additional calls (before the iterator is advanced) do nothing.
    ///
    /// # Panics
    ///
    /// Panics if iteration is already over.
    fn skip_children(&mut self);

    /// Advances the iterator to the first child of the current node if any.
    ///
    /// Has no effect and returns `false` if the current node has no children.
    ///
    /// Returns `false` if [`HexDivIterator::skip_children`] was called on this node.
    ///
    /// # Panics
    ///
    /// Panics if iteration is already over.
    fn enter(&mut self) -> bool;

    /// Similar to [`HexDivIterator::enter`], but focuses iteration on a single `child` node.
    ///
    /// None of the `child`'s neighbors will be yielded.
    ///
    /// Has no effect and returns `false` if the current node has no children.
    ///
    /// Returns `false` if [`HexDivIterator::skip_children`] was called on this node.
    ///
    /// # Panics
    ///
    /// Panics if the current node has children but the `child` index is out of bounds.
    ///
    /// Panics if iteration is already over.
    fn focus(&mut self, child: u8) -> bool;

    /// Efficiently filters out nodes based on their [`CachedBounds`] using a closure.
    ///
    /// Similar to [`Iterator::filter`], but more flexible and performant, since [`Filter`] gives
    /// access to the various traversal functions that [`HexDivIterator`] is equipped with.
    ///
    /// Also see [`HexDivPeekNodeIterator::filter_nodes`] which takes a closure with additionally
    /// access to the associated [`HexDivIterator::Node`].
    fn filter_bounds<F>(self, filter: F) -> FilterBounds<Self, F>
    where
        Self: Sized,
        F: FnMut(CachedBounds) -> Filter,
    {
        FilterBounds::new(self, filter)
    }

    /// Filters out _all children_ of certain nodes based on their [`CachedBounds`] using a closure.
    ///
    /// If the closure returns `false`, all children of that node will be skipped via
    /// [`HexDivIterator::skip_children`].
    ///
    /// The closure is only called for nodes that [have children](HexDivIterator::has_children),
    /// which implies that the [`CachedBounds`] are also [`Splittable`].
    ///
    /// The resulting iterator still implements the exact same [`HexDivIterator`] specializations,
    /// since removing all children effectively just converts parent nodes into leaf nodes.
    ///
    /// There is no point in calling this function on a [`HexDivLeafExIterator`], since all nodes
    /// are leaf nodes.
    fn filter_children<F>(self, enter: F) -> FilterChildren<Self, F>
    where
        Self: Sized,
        F: FnMut(Splittable<CachedBounds>) -> bool,
    {
        FilterChildren::new(self, enter)
    }

    /// Removes any nodes that have children, leaving only leaf nodes.
    ///
    /// If the original iterator implements [`HexDivLeafIterator`], the resulting iterator satisfies
    /// [`HexDivLeafExIterator`].
    fn filter_leaves(self) -> FilterLeaves<Self>
    where
        Self: Sized,
    {
        FilterLeaves::new(self)
    }

    /// Takes a closure and creates an iterator which calls that closure on each node.
    ///
    /// Similar to [`Iterator::map`], but since the closure can only update [`HexDivIterator::Node`]
    /// but not the [`CachedBounds`], the resulting iterator still implements mostly the same
    /// [`HexDivIterator`] specializations as the original iterator. One exception is
    /// [`HexDivPeekNodeIterator`], which would need to cache the mapped node value to prevent the
    /// closure from being called on the same node unpredictably multiple times.
    fn map_nodes<B, F>(self, map: F) -> MapNodes<Self, F>
    where
        Self: Sized,
        F: FnMut(CachedBounds, Self::Node) -> B,
    {
        MapNodes::new(self, map)
    }
}

/// A [`HexDivIterator`] that guarantees, that all leaf nodes are yielded.
///
/// I.e., if a certain leaf node is yielded, all of its neighbors must also be yielded. This means,
/// that the union of _only_ all leaf nodes will cover the entire [`HexDivIterator::root_extent`].
///
/// This trait does not put any guarantee on which parent nodes are yielded. There are however two
/// specializations:
///
/// - [`HexDivNodeIterator`] guarantees that all parent nodes will be yielded
/// - [`HexDivLeafExIterator`] guarantees that leaf nodes will be yielded exclusively
pub trait HexDivLeafIterator: HexDivIterator {}

/// A [`HexDivLeafIterator`] that is guaranteed to return all parent nodes.
///
/// This still allows basic filtering, but only by skipping _all_ children of a parent, which
/// essentially turns that parent node into a leaf node as far as iteration is concerned.
///
/// Since skipping over nodes arbitrarily is no longer possible, the functions that required peeking
/// ahead can now directly return the current state. There exist new, immutable versions of these
/// functions without the `peek_` prefix.
pub trait HexDivNodeIterator: HexDivLeafIterator {
    /// Immutable version of [`HexDivIterator::peek_bounds`].
    fn bounds(&self) -> Option<CachedBounds>;

    /// Immutable version of [`HexDivIterator::peek_at_origin`].
    fn at_origin(&self) -> bool {
        self.bounds()
            .is_some_and(|bounds| bounds.min() == UVec3::ZERO)
    }

    /// Immutable version of [`HexDivIterator::peek_remaining_bounds_hint`].
    fn remaining_bounds_hint(&self) -> Option<CachedBounds>;

    /// 'Zips up' two iterators, yielding a pair of both their [`HexDivIterator::Node`]s.
    ///
    /// Similar to [`Iterator::zip`], but with spatial awareness.
    ///
    /// The two iterators must be located at the root node and also share the exact same
    /// [`HexDivIterator::root_extent`]. This ensures that the two iterators have consistent child
    /// layouts, simplifying traversal.
    ///
    /// Whenever only one of the two iterators is able to enter a parent node, the other iterator
    /// will continue yielding the same value, making use of [`HexDivPeekNodeIterator::peek_node`].
    ///
    /// # Panics
    ///
    /// Panics if either of the two iterators is not located at the root node or if the two
    /// iterators have different [`HexDivIterator::root_extent`]s.
    fn zip_nodes<U: IntoIterator>(self, other: U) -> ZipNodes<Self, U::IntoIter>
    where
        Self: HexDivPeekNodeIterator + Sized,
        U::IntoIter: HexDivNodeIterator + HexDivPeekNodeIterator,
    {
        ZipNodes::new(self, other.into_iter())
    }

    /// Builds a [`HexDivNode`] from a [`HexDivNodeIterator`] of [`BuildAction`].
    ///
    /// # Panics
    ///
    /// Panics if the iterator is not at the root node.
    ///
    /// Panics if a leaf node yields [`BuildAction::Split`] or if a parent node yields
    /// [`BuildAction::Fill`] or [`BuildAction::Node`].
    fn build<T: HexDivNode>(mut self) -> T
    where
        Self: Sized + HexDivNodeIterator<Node = BuildAction<T>>,
        T::Leaf: Clone + Eq,
    {
        let root_extent = self.root_extent();
        assert_eq!(
            self.bounds(),
            Some(CachedBounds::with_extent_at_origin(root_extent)),
            "iterator should be at the root node"
        );

        Builder::new(root_extent).build(|builder_bounds| {
            let (bounds, build_action) = self.next().expect("iterator should not yet be empty");

            let expect_split = bounds.extent() != builder_bounds.extent();
            if expect_split != matches!(build_action, BuildAction::Split(_)) {
                if expect_split {
                    panic!("build action should be a split");
                } else {
                    panic!("build action should be a node/fill");
                }
            }

            build_action
        })
    }
}

/// A [`HexDivLeafIterator`] that is guaranteed to yield leaf nodes exclusively.
pub trait HexDivLeafExIterator: HexDivLeafIterator {
    /// 'Zips up' two iterators, yielding a pair of both their [`HexDivIterator::Node`]s.
    ///
    /// Similar to [`Iterator::zip`], but with spatial awareness.
    ///
    /// The two iterators must be located at the [first leaf node] and also share the exact same
    /// [`HexDivIterator::root_extent`]. This ensures that the two iterators have consistent child
    /// layouts, simplifying traversal.
    ///
    /// # Panics
    ///
    /// Panics if either of the two iterators is not located at the [first leaf node] or if the two
    /// iterators have different [`HexDivIterator::root_extent`]s.
    ///
    /// [first leaf node]: HexDivIterator::peek_at_origin
    fn zip_leaves<U: IntoIterator>(self, other: U) -> ZipLeaves<Self, U::IntoIter>
    where
        Self: HexDivPeekNodeIterator + Sized,
        U::IntoIter: HexDivLeafExIterator + HexDivPeekNodeIterator,
    {
        ZipLeaves::new(self, other.into_iter())
    }

    /// Converts this iterator into an owned [`HexDivNode`].
    ///
    /// All [`HexDivNode::Parent`] values are filled using [`Default`].
    ///
    /// # Panics
    ///
    /// Panics if the iterator is not at the [first leaf node](HexDivIterator::peek_at_origin).
    fn build<T: HexDivNode<Leaf = Self::Node, Parent: Default>>(mut self) -> T
    where
        Self: Sized,
        Self::Node: Clone + Eq,
    {
        let mut bounds = self.peek_bounds().expect("iterator should be new");
        assert_eq!(bounds.min(), UVec3::ZERO, "iterator should be new");

        Builder::new(bounds.extent()).build(|builder_bounds| {
            if bounds.extent() == builder_bounds.extent() {
                let node;
                (bounds, node) = self.next().expect("iterator should not yet be empty");
                BuildAction::Fill(node)
            } else {
                BuildAction::Split(Default::default())
            }
        })
    }
}

/// A [`HexDivIterator`] that can peek at its current [`HexDivIterator::Node`].
///
/// Despite having `Node` in the name, this is only comes with the guarantees of [`HexDivIterator`];
/// not that of [`HexDivNodeIterator`]. The `Node` is merely to emphasize on what is being peeked.
pub trait HexDivPeekNodeIterator: HexDivIterator {
    /// Returns the [`HexDivIterator::Node`] that will be yielded next.
    fn peek_node(&mut self) -> Option<Self::Node>;

    /// [`HexDivIterator::filter_bounds`] with additional access to [`HexDivIterator::Node`].
    fn filter_nodes<F>(self, filter: F) -> FilterNodes<Self, F>
    where
        Self: Sized,
        F: FnMut(CachedBounds, Self::Node) -> Filter,
    {
        FilterNodes::new(self, filter)
    }
}

/// An iterator that iterates over the nodes of a [`HexDivNode`].
#[derive(Debug)]
pub struct Iter<'a, T> {
    /// Represents the iterator's current position within the [`HexDivNode`].
    ///
    /// [`None`] once iteration is over.
    bounds: Option<CachedBounds>,
    /// The root [`HexDivNode`] being iterated.
    ///
    /// If the root node is entered, it is copied into the first slot of [`Self::parents`].
    root: &'a T,
    /// Contains the full path of parent nodes describing the iterator's position.
    ///
    /// [`Self::bounds`] always references a child within the last entry in this list.
    parents: ArrayVec<Parent<&'a T>, { SplitList::MAX }>,
    /// Indicates which [`Self::parents`] were focused down a single child.
    focused: BitArray<u16>,
    /// Prevents the children of the current node to be entered.
    ///
    /// Reset back to `false` once the iterator has advanced over this node.
    skip_children: bool,
}

impl<'a, T: HexDivNode> Iter<'a, T> {
    pub(super) fn new(root: &'a T) -> Self {
        Self {
            bounds: Some(CachedBounds::with_extent_at_origin(root.cached_extent())),
            root,
            parents: ArrayVec::new(),
            focused: BitArray::default(),
            skip_children: false,
        }
    }

    /// Returns the current [`CachedBounds`] and [`NodeRef`].
    fn current_item(&self) -> Option<(CachedBounds, NodeRef<'a, T>)> {
        let bounds = self.bounds?;
        let node = if let Some(parent) = self.parents.last() {
            let extent = parent.extent();
            let index = Bounds::with_extent_at(extent.child_extent(), bounds.min())
                .child_index(extent.child_splits());
            parent.get_child(index)
        } else {
            NodeRef::Node(self.root)
        };
        Some((bounds, node))
    }

    fn enter_child(&mut self, focus: Option<u8>) -> bool {
        let (_, node) = self.current_item().expect("iteration already over");
        if let Some(parent) = node.as_parent() {
            self.enter_child_of_parent(parent, focus);
            true
        } else {
            false
        }
    }

    /// Enters a child node of `parent`.
    ///
    /// If `focus` is [`Some`], enters the child with that index instead of the first one and sets
    /// the [`Self::focused`] flag.
    ///
    /// # Panics
    ///
    /// Panics if iteration is already over.
    ///
    /// Panics if the `focus` child index is out of bounds.
    fn enter_child_of_parent(&mut self, parent: Parent<&'a T>, focus: Option<u8>) {
        let bounds = self.bounds.as_mut().expect("iteration already over");

        self.skip_children = false;

        self.parents.push(parent);
        *bounds = bounds.with_extent_unchecked(parent.get_child(0).cached_extent());

        if let Some(child) = focus {
            self.focused.set(self.parents.len() - 1, true);
            if child != 0 {
                let parent_splits = parent.cached_extent().child_splits();
                assert!(child < parent_splits.volume(), "child index out of bounds");
                *bounds = bounds.with_child_index(parent_splits, child);
            }
        }
    }
}

impl<T> Clone for Iter<'_, T> {
    fn clone(&self) -> Self {
        Self {
            bounds: self.bounds,
            root: self.root,
            parents: self.parents.clone(),
            focused: self.focused,
            skip_children: self.skip_children,
        }
    }
}

impl<'a, T: HexDivNode> Iterator for Iter<'a, T> {
    type Item = (CachedBounds, NodeRef<'a, T>);

    fn next(&mut self) -> Option<Self::Item> {
        let item @ (_, node) = self.current_item()?;

        if self.skip_children {
            self.skip_node();
        } else if let Some(parent) = node.as_parent() {
            self.enter_child_of_parent(parent, None);
        } else {
            self.skip_node();
        }

        Some(item)
    }
}

impl<T: HexDivNode> FusedIterator for Iter<'_, T> {}

impl<'a, T: HexDivNode> HexDivIterator for Iter<'a, T> {
    type Node = NodeRef<'a, T>;

    fn root_extent(&self) -> CachedExtent {
        self.root.cached_extent()
    }

    fn peek_bounds(&mut self) -> Option<CachedBounds> {
        self.bounds
    }

    fn peek_remaining_bounds_hint(&mut self) -> Option<CachedBounds> {
        self.remaining_bounds_hint()
    }

    fn has_children(&mut self) -> bool {
        !self.skip_children && {
            let (_, node) = self.current_item().expect("iteration already over");
            node.is_parent()
        }
    }

    fn skip_children(&mut self) {
        assert!(self.bounds.is_some(), "iteration already over");
        self.skip_children = true;
    }

    fn enter(&mut self) -> bool {
        !self.skip_children && self.enter_child(None)
    }

    fn skip_node(&mut self) {
        self.skip_children = false;

        let bounds = self.bounds.as_mut().expect("iteration already over");

        let mut parent_popped = false;
        loop {
            // pop any parents that were focused
            while !self.parents.is_empty() && self.focused[self.parents.len() - 1] {
                self.focused.set(self.parents.len() - 1, false);
                self.parents.pop();
                parent_popped = true;
            }

            let Some(parent) = self.parents.last().copied() else {
                // if no parent remains, iteration is over
                self.bounds = None;
                return;
            };

            // if a parent was popped the bounds no longer match
            if parent_popped {
                *bounds = bounds.resize(parent.get_child(0).cached_extent());
            }

            // advance the bounds to the next neighbor
            if let Some(new_bounds) =
                bounds.next_bounds_within(parent.cached_extent().child_splits())
            {
                *bounds = new_bounds;
                return;
            }

            // if there is no next neighbor, pop the parent that we knew was not focused
            self.parents.pop();
            parent_popped = true;
        }
    }

    fn focus(&mut self, child: u8) -> bool {
        !self.skip_children && self.enter_child(Some(child))
    }
}

impl<T: HexDivNode> HexDivLeafIterator for Iter<'_, T> {}

impl<T: HexDivNode> HexDivNodeIterator for Iter<'_, T> {
    fn bounds(&self) -> Option<CachedBounds> {
        self.bounds
    }

    fn remaining_bounds_hint(&self) -> Option<CachedBounds> {
        let bounds = self.bounds?;

        let mut index = 0;
        loop {
            index += self.focused[index..self.parents.len()].leading_ones();

            if let Some(first_non_focused) = self.parents.get(index) {
                let extent = first_non_focused.extent();
                let child_splits = extent.child_splits();
                let child_index = Bounds::with_extent_at(extent.child_extent(), bounds.min())
                    .child_index(child_splits);
                if child_index == child_splits.total() - 1 {
                    index += 1;
                    continue;
                }
            }

            break Some(if let Some(index) = index.checked_sub(1) {
                CachedBounds::with_extent_at(self.parents[index].cached_extent(), bounds.min())
            } else {
                CachedBounds::with_extent_at_origin(self.root.cached_extent())
            });
        }
    }
}

impl<T: HexDivNode> HexDivPeekNodeIterator for Iter<'_, T> {
    fn peek_node(&mut self) -> Option<Self::Node> {
        self.current_item().map(|(_, node)| node)
    }
}

#[cfg(test)]
mod tests {
    use glam::UVec3;
    use itertools::{assert_equal, Itertools};

    use super::*;
    use crate::hex_div::{
        extent::Extent,
        node::{
            bool::{BitNode, Count, NoBitCache},
            builder::{build_sphere_octant, BuildAction, Builder},
            NodeDataRef,
        },
    };

    #[test]
    fn iter_yields_all_nodes() {
        let root = build_sphere_octant::<BitNode>(6);

        let nodes = root.iter().counts_by(|(_, node)| node.data());

        assert_eq!(nodes[&NodeDataRef::Parent(&(), NoBitCache)], 483);
        assert_eq!(nodes[&NodeDataRef::Leaf(false)], 15_081);
        assert_eq!(nodes[&NodeDataRef::Leaf(true)], 15_349);
    }

    #[test]
    fn filter_focus_only_yields_specific_nodes() {
        let root = build_sphere_octant::<BitNode<Count>>(6);
        let bounds = CachedBounds::with_extent_at_origin(root.cached_extent());

        assert_equal(
            root.iter()
                .filter_bounds(|_| Filter::Focus { child: 0 })
                .map(|(bounds, node)| (bounds.strip_cache(), node)),
            [(bounds.first_child().unwrap(), root.get_child(0))],
        );

        assert_equal(
            root.iter()
                .filter_bounds(|_| Filter::Focus { child: 63 })
                .map(|(bounds, node)| (bounds.strip_cache(), node)),
            [(bounds.child(63).unwrap(), root.get_child(63))],
        );

        // child_chain is calculated via:
        // 64 / sqrt(3) ~= 36.95      ->  0b10__________01__________00
        // child index along diagonal -> [0b10_10_10, 0b01_01_01, 0b00_00_00]
        // convert to base 10         -> [        42,         21,          0]

        let mut filter_chain = [
            Filter::Focus { child: 42 },
            Filter::Focus { child: 21 },
            Filter::Focus { child: 0 },
            Filter::Yield {
                node: true,
                children: false,
            },
        ]
        .into_iter();

        assert_equal(
            root.iter()
                .filter_bounds(|_| {
                    filter_chain
                        .next()
                        .expect("filter_bounds should only call its closure four times")
                })
                .map(|(_, node)| node.data()),
            [
                NodeDataRef::Leaf(true), // true since we used 36; for 37 would be false
            ],
        );
        assert_equal(filter_chain, []); // filter_chain should be fully consumed
    }

    #[test]
    #[should_panic = "iteration already over"]
    fn skip_node_panics_if_iteration_is_already_over() {
        let root: BitNode = HexDivNode::with_default(Extent::ONE);
        let mut iter = root.iter();
        assert!(iter.next().is_some());

        iter.skip_node();
    }

    #[test]
    #[should_panic = "iteration already over"]
    fn focus_panics_if_iteration_is_already_over() {
        let root: BitNode = HexDivNode::with_default(Extent::ONE);
        let mut iter = root.iter();
        assert!(iter.next().is_some());

        iter.focus(0);
    }

    #[test]
    #[should_panic = "child index out of bounds"]
    fn focus_panics_if_the_index_is_out_of_bounds() {
        // 2x1x1 BitNode with [true, false]
        let root = Builder::<BitNode>::new(Extent::from_splits([1, 0, 0]).unwrap().compute_cache())
            .build(|bounds| match bounds.to_point() {
                Some(UVec3 { x: 0, y: 0, z: 0 }) => BuildAction::Fill(true),
                Some(UVec3 { x: 1, y: 0, z: 0 }) => BuildAction::Fill(false),
                Some(_) => unreachable!(),
                None => BuildAction::Split(()),
            });
        let mut iter = root.iter();

        iter.focus(2);
    }
}

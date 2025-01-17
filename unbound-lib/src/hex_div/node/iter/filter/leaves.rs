use std::iter::FusedIterator;

use crate::hex_div::{
    bounds::CachedBounds,
    extent::CachedExtent,
    node::iter::{
        HexDivIterator, HexDivLeafExIterator, HexDivLeafIterator, HexDivPeekNodeIterator,
    },
};

#[derive(Clone, Debug)]
pub struct FilterLeaves<I> {
    iter: I,
}

impl<I: HexDivIterator> FilterLeaves<I> {
    pub(in crate::hex_div::node::iter) fn new(iter: I) -> Self {
        Self { iter }
    }

    /// Ensures, that the iterator is at the next leaf node that is to be yielded.
    fn ensure_at_leaf(&mut self) {
        while self.iter.enter() {}
    }
}

impl<I: HexDivIterator> Iterator for FilterLeaves<I> {
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        self.ensure_at_leaf();
        self.iter.next()
    }
}

impl<I: HexDivIterator> FusedIterator for FilterLeaves<I> {}

impl<I: HexDivIterator> HexDivIterator for FilterLeaves<I> {
    type Node = I::Node;

    fn root_extent(&self) -> CachedExtent {
        self.iter.root_extent()
    }

    fn peek_bounds(&mut self) -> Option<CachedBounds> {
        self.ensure_at_leaf();
        self.iter.peek_bounds()
    }

    fn peek_remaining_bounds_hint(&mut self) -> Option<CachedBounds> {
        self.ensure_at_leaf();
        self.iter.peek_remaining_bounds_hint()
    }

    fn skip_node(&mut self) {
        self.ensure_at_leaf();
        self.iter.skip_node();
    }

    fn has_children(&mut self) -> bool {
        // the iterator only yields leaf nodes without children
        false
    }

    fn skip_children(&mut self) {
        // the iterator only yields leaf nodes without children
    }

    fn enter(&mut self) -> bool {
        // the iterator only yields leaf nodes that cannot be entered
        false
    }

    fn focus(&mut self, _child: u8) -> bool {
        // the iterator only yields leaf nodes that cannot be entered
        false
    }
}

impl<I: HexDivLeafIterator> HexDivLeafIterator for FilterLeaves<I> {}

/// Turns a [`HexDivLeafIterator`] into a [`HexDivLeafExIterator`] since parent nodes are skipped.
impl<I: HexDivLeafIterator> HexDivLeafExIterator for FilterLeaves<I> {}

impl<I: HexDivPeekNodeIterator> HexDivPeekNodeIterator for FilterLeaves<I> {
    fn peek_node(&mut self) -> Option<Self::Node> {
        self.ensure_at_leaf();
        self.iter.peek_node()
    }
}

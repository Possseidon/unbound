use std::iter::FusedIterator;

use super::Filter;
use crate::hex_div::{
    bounds::CachedBounds,
    extent::CachedExtent,
    node::iter::{HexDivIterator, HexDivPeekNodeIterator},
};

#[derive(Clone, Debug)]
pub struct FilterNodes<I, F> {
    iter: I,
    filter: F,
    filtered: bool,
}

impl<I, F> FilterNodes<I, F>
where
    I: HexDivPeekNodeIterator,
    F: FnMut(CachedBounds, I::Node) -> Filter,
{
    pub(in crate::hex_div::node::iter) fn new(iter: I, filter: F) -> Self {
        Self {
            iter,
            filter,
            filtered: false,
        }
    }

    fn ensure_filtered(&mut self) {
        if self.filtered {
            return;
        }

        self.filtered = true;
        loop {
            let Some(bounds) = self.iter.peek_bounds() else {
                break;
            };
            let node = self.iter.peek_node().expect("bounds should imply a node");
            if !(self.filter)(bounds, node).apply(&mut self.iter) {
                break;
            }
        }
    }
}

impl<I, F> Iterator for FilterNodes<I, F>
where
    I: HexDivPeekNodeIterator,
    F: FnMut(CachedBounds, I::Node) -> Filter,
{
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        self.ensure_filtered();
        self.filtered = false;
        self.iter.next()
    }
}

impl<I, F> FusedIterator for FilterNodes<I, F>
where
    I: HexDivPeekNodeIterator,
    F: FnMut(CachedBounds, I::Node) -> Filter,
{
}

impl<I, F> HexDivIterator for FilterNodes<I, F>
where
    I: HexDivPeekNodeIterator,
    F: FnMut(CachedBounds, I::Node) -> Filter,
{
    type Node = I::Node;

    fn root_extent(&self) -> CachedExtent {
        self.iter.root_extent()
    }

    fn peek_bounds(&mut self) -> Option<CachedBounds> {
        self.ensure_filtered();
        self.iter.peek_bounds()
    }

    fn peek_remaining_bounds_hint(&mut self) -> Option<CachedBounds> {
        self.ensure_filtered();
        self.iter.peek_remaining_bounds_hint()
    }

    fn skip_node(&mut self) {
        self.ensure_filtered();
        self.filtered = false;
        self.iter.skip_node();
    }

    fn has_children(&mut self) -> bool {
        self.ensure_filtered();
        self.iter.has_children()
    }

    fn skip_children(&mut self) {
        self.ensure_filtered();
        self.iter.skip_children();
    }

    fn enter(&mut self) -> bool {
        self.ensure_filtered();
        let entered = self.iter.enter();
        if entered {
            self.filtered = false;
        }
        entered
    }

    fn focus(&mut self, child: u8) -> bool {
        self.ensure_filtered();
        let focused = self.iter.focus(child);
        if focused {
            self.filtered = false;
        }
        focused
    }
}

// DO NOT implement HexDivLeafIterator; not all leaf nodes might be yielded!

impl<I, F> HexDivPeekNodeIterator for FilterNodes<I, F>
where
    I: HexDivPeekNodeIterator,
    F: FnMut(CachedBounds, I::Node) -> Filter,
{
    fn peek_node(&mut self) -> Option<Self::Node> {
        self.ensure_filtered();
        self.iter.peek_node()
    }
}

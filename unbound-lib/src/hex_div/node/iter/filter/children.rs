use std::iter::FusedIterator;

use crate::hex_div::{
    bounds::CachedBounds,
    extent::{CachedExtent, Splittable},
    node::iter::{
        HexDivIterator, HexDivLeafExIterator, HexDivLeafIterator, HexDivNodeIterator,
        HexDivPeekNodeIterator,
    },
};

#[derive(Clone, Debug)]
pub struct FilterChildren<I, F> {
    iter: I,
    enter: F,
}

impl<I, F> FilterChildren<I, F>
where
    I: HexDivIterator,
    F: FnMut(Splittable<CachedBounds>) -> bool,
{
    pub(in crate::hex_div::node::iter) fn new(iter: I, enter: F) -> Self {
        Self { iter, enter }
    }

    /// Ensures that children of the current node are skipped based on the closure.
    ///
    /// The closure will only be called once since [`HexDivIterator::skip_children`] causes
    /// [`HexDivIterator::has_children`] to return `false`.
    fn ensure_children_skipped(&mut self) {
        let Some(bounds) = self.iter.peek_bounds() else {
            // done first to prevent peek_has_children from panicking
            return;
        };

        if !self.iter.has_children() {
            return;
        }

        // has children, so it must be splittable
        if !(self.enter)(Splittable::new_unchecked(bounds)) {
            self.iter.skip_children();
        }
    }
}

impl<I, F> Iterator for FilterChildren<I, F>
where
    I: HexDivIterator,
    F: FnMut(Splittable<CachedBounds>) -> bool,
{
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        self.ensure_children_skipped();
        self.iter.next()
    }
}

impl<I, F> FusedIterator for FilterChildren<I, F>
where
    I: HexDivIterator,
    F: FnMut(Splittable<CachedBounds>) -> bool,
{
}

impl<I, F> HexDivIterator for FilterChildren<I, F>
where
    I: HexDivIterator,
    F: FnMut(Splittable<CachedBounds>) -> bool,
{
    type Node = I::Node;

    fn root_extent(&self) -> CachedExtent {
        self.iter.root_extent()
    }

    fn peek_bounds(&mut self) -> Option<CachedBounds> {
        self.iter.peek_bounds()
    }

    fn peek_remaining_bounds_hint(&mut self) -> Option<CachedBounds> {
        self.iter.peek_remaining_bounds_hint()
    }

    fn skip_node(&mut self) {
        // don't call the closure; children are skipped either way
        self.iter.skip_node();
    }

    fn has_children(&mut self) -> bool {
        self.ensure_children_skipped();
        self.iter.has_children()
    }

    fn skip_children(&mut self) {
        self.iter.skip_children();
    }

    fn enter(&mut self) -> bool {
        self.ensure_children_skipped();
        self.iter.enter()
    }

    fn focus(&mut self, child: u8) -> bool {
        self.ensure_children_skipped();
        self.iter.focus(child)
    }
}

impl<I, F> HexDivLeafIterator for FilterChildren<I, F>
where
    I: HexDivLeafIterator,
    F: FnMut(Splittable<CachedBounds>) -> bool,
{
}

impl<I, F> HexDivNodeIterator for FilterChildren<I, F>
where
    I: HexDivNodeIterator,
    F: FnMut(Splittable<CachedBounds>) -> bool,
{
    fn bounds(&self) -> Option<CachedBounds> {
        self.iter.bounds()
    }

    fn remaining_bounds_hint(&self) -> Option<CachedBounds> {
        self.iter.remaining_bounds_hint()
    }
}

impl<I, F> HexDivLeafExIterator for FilterChildren<I, F>
where
    I: HexDivLeafExIterator,
    F: FnMut(Splittable<CachedBounds>) -> bool,
{
}

impl<I, F> HexDivPeekNodeIterator for FilterChildren<I, F>
where
    I: HexDivPeekNodeIterator,
    F: FnMut(Splittable<CachedBounds>) -> bool,
{
    fn peek_node(&mut self) -> Option<Self::Node> {
        self.iter.peek_node()
    }
}

use std::iter::FusedIterator;

use crate::hex_div::{
    bounds::CachedBounds,
    extent::CachedExtent,
    node::iter::{HexDivIterator, HexDivLeafExIterator, HexDivLeafIterator, HexDivNodeIterator},
};

#[derive(Clone, Debug)]
pub struct MapNodes<I, F> {
    iter: I,
    map: F,
}

impl<I, F, B> MapNodes<I, F>
where
    I: HexDivIterator,
    F: FnMut(CachedBounds, I::Node) -> B,
{
    pub(in crate::hex_div::node::iter) fn new(iter: I, map: F) -> Self {
        Self { iter, map }
    }
}

impl<I, F, B> Iterator for MapNodes<I, F>
where
    I: HexDivIterator,
    F: FnMut(CachedBounds, I::Node) -> B,
{
    type Item = (CachedBounds, B);

    fn next(&mut self) -> Option<Self::Item> {
        self.iter
            .next()
            .map(|(bounds, node)| (bounds, (self.map)(bounds, node)))
    }
}

impl<I, F, B> FusedIterator for MapNodes<I, F>
where
    I: HexDivIterator,
    F: FnMut(CachedBounds, I::Node) -> B,
{
}

impl<I, F, B> HexDivIterator for MapNodes<I, F>
where
    I: HexDivIterator,
    F: FnMut(CachedBounds, I::Node) -> B,
{
    type Node = B;

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
        self.iter.skip_node();
    }

    fn has_children(&mut self) -> bool {
        self.iter.has_children()
    }

    fn skip_children(&mut self) {
        self.iter.skip_children();
    }

    fn enter(&mut self) -> bool {
        self.iter.enter()
    }

    fn focus(&mut self, child: u8) -> bool {
        self.iter.focus(child)
    }
}

impl<I, F, B> HexDivNodeIterator for MapNodes<I, F>
where
    I: HexDivNodeIterator,
    F: FnMut(CachedBounds, I::Node) -> B,
{
    fn bounds(&self) -> Option<CachedBounds> {
        self.iter.bounds()
    }

    fn remaining_bounds_hint(&self) -> Option<CachedBounds> {
        self.iter.remaining_bounds_hint()
    }
}

impl<I, F, B> HexDivLeafIterator for MapNodes<I, F>
where
    I: HexDivLeafIterator,
    F: FnMut(CachedBounds, I::Node) -> B,
{
}

impl<I, F, B> HexDivLeafExIterator for MapNodes<I, F>
where
    I: HexDivLeafExIterator,
    F: FnMut(CachedBounds, I::Node) -> B,
{
}

// DO NOT implement HexDivPeekNodeIterator; this would require caching the mapped peeked value

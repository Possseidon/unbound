use std::iter::FusedIterator;

use super::Advance;
use crate::hex_div::{
    bounds::CachedBounds,
    extent::CachedExtent,
    node::iter::{
        HexDivIterator, HexDivLeafExIterator, HexDivLeafIterator, HexDivPeekNodeIterator,
    },
};

#[derive(Clone, Debug)]
pub struct ZipLeaves<A, B> {
    a: A,
    b: B,
}

impl<A, B> ZipLeaves<A, B>
where
    A: HexDivLeafExIterator + HexDivPeekNodeIterator,
    B: HexDivLeafExIterator + HexDivPeekNodeIterator,
{
    pub(in crate::hex_div::node::iter) fn new(mut a: A, mut b: B) -> Self {
        assert_eq!(a.root_extent(), b.root_extent(), "incompatible iterators");

        assert!(a.peek_at_origin(), "`a` should be at the origin");
        assert!(b.peek_at_origin(), "`b` should be at the origin");

        Self { a, b }
    }

    /// Checks which iterator(s) will be advanced by the next call to [`Iterator::next`].
    fn check_advance(&mut self) -> Option<Advance> {
        let a_bounds = self.a.peek_bounds()?;
        let b_bounds = self.b.peek_bounds().expect("iterators should be in sync");

        Some(if a_bounds.extent() == b_bounds.extent() {
            // unlike ZipNodes, the iterators cannot have children since they are already leaves
            Advance::Both
        } else if a_bounds.extent().cmple(b_bounds.extent()).any() {
            Advance::A
        } else {
            Advance::B
        })
    }
}

impl<A, B> Iterator for ZipLeaves<A, B>
where
    A: HexDivLeafExIterator + HexDivPeekNodeIterator,
    B: HexDivLeafExIterator + HexDivPeekNodeIterator,
{
    type Item = (CachedBounds, <Self as HexDivIterator>::Node);

    fn next(&mut self) -> Option<Self::Item> {
        Some(self.check_advance()?.zipped_next(&mut self.a, &mut self.b))
    }
}

impl<A, B> FusedIterator for ZipLeaves<A, B>
where
    A: HexDivLeafExIterator + HexDivPeekNodeIterator,
    B: HexDivLeafExIterator + HexDivPeekNodeIterator,
{
}

impl<A, B> HexDivIterator for ZipLeaves<A, B>
where
    A: HexDivLeafExIterator + HexDivPeekNodeIterator,
    B: HexDivLeafExIterator + HexDivPeekNodeIterator,
{
    type Node = (A::Node, B::Node);

    fn root_extent(&self) -> CachedExtent {
        // both were checked to be identical
        self.a.root_extent()
    }

    fn peek_bounds(&mut self) -> Option<CachedBounds> {
        let a_bounds = self.a.peek_bounds()?;
        let b_bounds = self.b.peek_bounds().expect("iterators should be in sync");
        Some(if a_bounds.extent().cmple(b_bounds.extent()).any() {
            a_bounds
        } else {
            b_bounds
        })
    }

    fn peek_remaining_bounds_hint(&mut self) -> Option<CachedBounds> {
        // both still yield the same bounds; either should be valid
        self.a.peek_remaining_bounds_hint()
    }

    fn skip_node(&mut self) {
        self.check_advance()
            .expect("iteration already over")
            .zipped_skip_node(&mut self.a, &mut self.b);
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

impl<A, B> HexDivLeafIterator for ZipLeaves<A, B>
where
    A: HexDivLeafExIterator + HexDivPeekNodeIterator,
    B: HexDivLeafExIterator + HexDivPeekNodeIterator,
{
}

// DO NOT implement HexDivNodeIterator; not even a single parent node will be yielded

impl<A, B> HexDivLeafExIterator for ZipLeaves<A, B>
where
    A: HexDivLeafExIterator + HexDivPeekNodeIterator,
    B: HexDivLeafExIterator + HexDivPeekNodeIterator,
{
}

impl<A, B> HexDivPeekNodeIterator for ZipLeaves<A, B>
where
    A: HexDivLeafExIterator + HexDivPeekNodeIterator,
    B: HexDivLeafExIterator + HexDivPeekNodeIterator,
{
    fn peek_node(&mut self) -> Option<Self::Node> {
        Some((
            self.a.peek_node()?,
            self.b.peek_node().expect("iterators should be in sync"),
        ))
    }
}

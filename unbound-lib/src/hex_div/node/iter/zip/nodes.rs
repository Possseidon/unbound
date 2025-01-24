use std::{cmp::Ordering, iter::FusedIterator};

use super::Advance;
use crate::hex_div::{
    bounds::CachedBounds,
    extent::CachedExtent,
    node::iter::{HexDivIterator, HexDivLeafIterator, HexDivNodeIterator, HexDivPeekNodeIterator},
};

#[derive(Clone, Debug)]
pub struct ZipNodes<A, B> {
    a: A,
    b: B,
}

impl<A, B> ZipNodes<A, B>
where
    A: HexDivNodeIterator + HexDivPeekNodeIterator,
    B: HexDivNodeIterator + HexDivPeekNodeIterator,
{
    pub(in crate::hex_div::node::iter) fn new(a: A, b: B) -> Self {
        assert_eq!(a.root_extent(), b.root_extent(), "incompatible iterators");

        let root_bounds = Some(CachedBounds::with_extent_at_origin(a.root_extent()));
        assert_eq!(a.bounds(), root_bounds, "`a` should be at the root node");
        assert_eq!(b.bounds(), root_bounds, "`b` should be at the root node");

        Self { a, b }
    }

    /// Checks which iterator(s) will be advanced by the next call to [`Iterator::next`].
    fn check_advance(&mut self) -> Option<Advance> {
        let a_bounds = self.a.bounds()?;
        let b_bounds = self.b.bounds().expect("iterators should be in sync");

        Some(if a_bounds.extent() == b_bounds.extent() {
            match self.a.has_children().cmp(&self.b.has_children()) {
                Ordering::Less => Advance::B,
                Ordering::Equal => Advance::Both,
                Ordering::Greater => Advance::A,
            }
        } else if a_bounds.extent().cmple(b_bounds.extent()).any() {
            Advance::A
        } else {
            Advance::B
        })
    }
}

impl<A, B> Iterator for ZipNodes<A, B>
where
    A: HexDivNodeIterator + HexDivPeekNodeIterator,
    B: HexDivNodeIterator + HexDivPeekNodeIterator,
{
    type Item = (CachedBounds, <Self as HexDivIterator>::Node);

    fn next(&mut self) -> Option<Self::Item> {
        Some(self.check_advance()?.zipped_next(&mut self.a, &mut self.b))
    }
}

impl<A, B> FusedIterator for ZipNodes<A, B>
where
    A: HexDivNodeIterator + HexDivPeekNodeIterator,
    B: HexDivNodeIterator + HexDivPeekNodeIterator,
{
}

impl<A, B> HexDivIterator for ZipNodes<A, B>
where
    A: HexDivNodeIterator + HexDivPeekNodeIterator,
    B: HexDivNodeIterator + HexDivPeekNodeIterator,
{
    type Node = (A::Node, B::Node);

    fn root_extent(&self) -> CachedExtent {
        // both were checked to be identical
        self.a.root_extent()
    }

    fn peek_bounds(&mut self) -> Option<CachedBounds> {
        self.bounds()
    }

    fn peek_remaining_bounds_hint(&mut self) -> Option<CachedBounds> {
        self.remaining_bounds_hint()
    }

    fn skip_node(&mut self) {
        self.check_advance()
            .expect("iteration already over")
            .zipped_skip_node(&mut self.a, &mut self.b);
    }

    fn has_children(&mut self) -> bool {
        self.a.has_children() || self.b.has_children()
    }

    fn skip_children(&mut self) {
        match self.check_advance().expect("iteration already over") {
            Advance::A => self.a.skip_children(),
            Advance::B => self.b.skip_children(),
            Advance::Both => {
                self.a.skip_children();
                self.b.skip_children();
            }
        }
    }

    fn enter(&mut self) -> bool {
        // use `|` to ensure both are called
        self.a.enter() | self.b.enter()
    }

    fn focus(&mut self, child: u8) -> bool {
        // use `|` to ensure both are called
        self.a.focus(child) | self.b.focus(child)
    }
}

impl<A, B> HexDivLeafIterator for ZipNodes<A, B>
where
    A: HexDivNodeIterator + HexDivPeekNodeIterator + HexDivPeekNodeIterator,
    B: HexDivNodeIterator + HexDivPeekNodeIterator + HexDivPeekNodeIterator,
{
}

impl<A, B> HexDivNodeIterator for ZipNodes<A, B>
where
    A: HexDivNodeIterator + HexDivPeekNodeIterator,
    B: HexDivNodeIterator + HexDivPeekNodeIterator,
{
    fn bounds(&self) -> Option<CachedBounds> {
        let a_bounds = self.a.bounds()?;
        let b_bounds = self.b.bounds().expect("iterators should be in sync");
        Some(if a_bounds.extent().cmple(b_bounds.extent()).any() {
            a_bounds
        } else {
            b_bounds
        })
    }

    fn remaining_bounds_hint(&self) -> Option<CachedBounds> {
        // both still yield the same bounds; either should be valid
        self.a.remaining_bounds_hint()
    }
}

// DO NOT implement HexDivLeafExIterator; not just some but all parent nodes will be yielded

impl<A, B> HexDivPeekNodeIterator for ZipNodes<A, B>
where
    A: HexDivNodeIterator + HexDivPeekNodeIterator,
    B: HexDivNodeIterator + HexDivPeekNodeIterator,
{
    fn peek_node(&mut self) -> Option<Self::Node> {
        Some((
            self.a.peek_node()?,
            self.b.peek_node().expect("iterators should be in sync"),
        ))
    }
}

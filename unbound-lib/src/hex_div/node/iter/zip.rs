pub mod leaves;
pub mod nodes;

use super::{HexDivIterator, HexDivPeekNodeIterator};
use crate::hex_div::bounds::CachedBounds;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Advance {
    A,
    B,
    Both,
}

impl Advance {
    fn zipped_next<A, B>(self, a: &mut A, b: &mut B) -> (CachedBounds, (A::Node, B::Node))
    where
        A: HexDivIterator + HexDivPeekNodeIterator,
        B: HexDivIterator + HexDivPeekNodeIterator,
    {
        match self {
            Advance::A => zipped_advance(a, b),
            Advance::B => {
                let (bounds, (b, a)) = zipped_advance(b, a);
                (bounds, (a, b))
            }
            Advance::Both => {
                let (a_bounds, a_node) = a.next().expect("iteration should not be over");
                let (b_bounds, b_node) = b.next().expect("iteration should not be over");
                debug_assert_eq!(a_bounds, b_bounds, "zipped iterators should be in sync");
                (a_bounds, (a_node, b_node))
            }
        }
    }

    fn zipped_skip_node<A, B>(self, a: &mut A, b: &mut B)
    where
        A: HexDivIterator + HexDivPeekNodeIterator,
        B: HexDivIterator + HexDivPeekNodeIterator,
    {
        match self {
            Advance::A => {
                zipped_advance(a, b);
            }
            Advance::B => {
                zipped_advance(b, a);
            }
            Advance::Both => {
                a.skip_node();
                b.skip_node();
            }
        }
    }
}

/// `a` is always advanced, `b` is only advanced if if needs to catch up to the new bounds of `a`.
fn zipped_advance<A, B>(a: &mut A, b: &mut B) -> (CachedBounds, (A::Node, B::Node))
where
    A: HexDivIterator,
    B: HexDivIterator + HexDivPeekNodeIterator,
{
    let (a_bounds, a_node) = a.next().expect("iteration should not be over");
    let b_bounds = b.peek_bounds().expect("iteration should not be over");
    let b_node = b.peek_node().expect("iteration should not be over");

    if let Some(a_new_bounds) = a.peek_bounds() {
        // advance b if a is no longer one of its children
        if !b_bounds.contains(a_new_bounds.min()) {
            b.skip_node();
            debug_assert_eq!(
                b.peek_bounds(),
                Some(a_new_bounds),
                "iterators should be in sync"
            );
        } // else `a` is still within `b`'s current node
    } else {
        b.skip_node();
        debug_assert!(b.peek_bounds().is_none(), "iterators should be in sync");
    }

    (a_bounds, (a_node, b_node))
}

use super::{
    node::{
        bool::{BitNode, NoBitCache},
        NoCache, Node,
    },
    HexDivNode,
};
use crate::math::bounds::UBounds3;

pub struct HexDiv<N: HexDivNode> {
    /// The root node.
    root: N,
    /// The area within [`Self::hex_div`] that is not padding.
    ///
    /// Always guaranteed to lie within the bounds of [`Self::hex_div`].
    bounds: UBounds3,
}

impl<T: HexDivNode> HexDiv<T> {}

type A<T, P = (), C = NoCache> = HexDiv<Node<T, P, C>>;
type B<P = (), C = NoBitCache> = HexDiv<BitNode<P, C>>;

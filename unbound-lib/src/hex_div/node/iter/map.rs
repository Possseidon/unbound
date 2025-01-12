use super::HexDivNodeIterator;
use crate::hex_div::bounds::CachedBounds;

pub struct Map<I, F> {
    iter: I,
    f: F,
}

impl<I, F> Map<I, F> {
    pub(super) fn new(iter: I, f: F) -> Self {
        Self { iter, f }
    }
}

impl<B, I: HexDivNodeIterator, F> Iterator for Map<I, F>
where
    F: FnMut(CachedBounds, I::Node) -> B,
{
    type Item = (CachedBounds, B);

    fn next(&mut self) -> Option<Self::Item> {
        self.iter
            .next()
            .map(|(bounds, node)| (bounds, (self.f)(bounds, node)))
    }
}

impl<B, I: HexDivNodeIterator, F> HexDivNodeIterator for Map<I, F>
where
    F: FnMut(CachedBounds, I::Node) -> B,
{
    type Node = B;

    fn skip_children(&mut self) {
        self.iter.skip_children();
    }

    fn only_child(&mut self, index: u8) {
        self.iter.only_child(index);
    }
}

use super::{extent::Extent, HexDiv, NodeDataRef};

pub trait Cache<'a, T>: Sized {
    /// A reference to a cached value, usually `&Self`.
    ///
    /// Allows e.g. calculating the count of set [`bool`]s on the fly rather than storing it.
    type Ref: Copy;

    fn compute_cache(
        extent: Extent,
        inputs: impl IntoIterator<Item = CacheIn<'a, T, Self>>,
    ) -> Self;
}

pub enum CacheIn<'a, T, C: Cache<'a, T>> {
    Leaf(T),
    Cache(C::Ref),
}

impl<'a, T, C: Cache<'a, T>> CacheIn<'a, T, C> {
    pub fn from_node(node: &'a impl HexDiv<LeafRef<'a> = T, Cache<'a> = C>) -> Self {
        match node.as_data() {
            NodeDataRef::Leaf(leaf) => CacheIn::Leaf(leaf),
            NodeDataRef::Parent(_, cache) => CacheIn::Cache(cache),
        }
    }
}

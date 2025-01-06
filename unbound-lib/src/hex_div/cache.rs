use super::{extent::Extent, HexDivNode, NodeDataRef};

/// A value that is calculatable from a collection of `T` and already calculated [`Cache<T>`].
///
/// `T` is usually a reference to a leaf.
pub trait Cache<T>: Sized {
    /// A reference to a cached value, usually `&'a Self`.
    ///
    /// Allows e.g. calculating the count of set [`bool`]s on the fly rather than storing it.
    type Ref<'a>: Copy;

    fn compute_cache<'a>(
        extent: Extent,
        inputs: impl IntoIterator<Item = CacheIn<'a, T, Self>>,
    ) -> Self;
}

pub enum CacheIn<'a, T, C: Cache<T>> {
    Leaf(T),
    Cache(C::Ref<'a>),
}

impl<'a, T, C: Cache<T>> CacheIn<'a, T, C> {
    pub fn from_node(node: &'a impl HexDivNode<LeafRef<'a> = T, Cache<'a> = C>) -> Self {
        match node.as_data() {
            NodeDataRef::Leaf(leaf) => CacheIn::Leaf(leaf),
            NodeDataRef::Parent(_, cache) => CacheIn::Cache(cache),
        }
    }
}

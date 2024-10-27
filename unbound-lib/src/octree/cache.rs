use super::extent::OctreeExtent;

pub trait OctreeCache<'a, T>: Clone {
    /// A reference to a cached value, usually `&Self`.
    ///
    /// Allows e.g. calculating the count of set [`bool`]s on the fly rather than storing it.
    type Ref: Copy;

    fn compute_cache(
        extent: OctreeExtent,
        inputs: impl IntoIterator<Item = CacheInput<'a, T, Self>>,
    ) -> Self;
}

pub enum CacheInput<'a, T, C: OctreeCache<'a, T>> {
    Leaf(T),
    Cache(C::Ref),
}

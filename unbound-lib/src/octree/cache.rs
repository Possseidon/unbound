use super::extent::OctreeExtent;

pub trait OctreeCache<T>: Clone + Eq {
    fn compute_cache(
        extent: OctreeExtent,
        leaves: impl IntoIterator<Item = T>,
        caches: impl IntoIterator<Item = Self>,
    ) -> Self;
}

impl<T> OctreeCache<T> for () {
    fn compute_cache(
        _: OctreeExtent,
        _: impl IntoIterator<Item = T>,
        _: impl IntoIterator<Item = Self>,
    ) -> Self {
    }
}

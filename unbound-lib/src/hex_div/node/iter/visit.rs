use super::HexDivNodeIterator;
use crate::{
    hex_div::{
        bounds::{Bounds, CachedBounds},
        extent::{CachedExtent, Splittable},
        node::{IsParent, Parent},
    },
    math::bounds::UBounds3,
};

pub fn within<T>(
    target: Bounds,
    enter_target: bool,
) -> impl Fn(Splittable<CachedBounds>, Parent<T>) -> Option<Enter> {
    move |bounds, _| Enter::within_node(bounds, target, enter_target)
}

pub fn until<T>(target: Bounds) -> impl Fn(Splittable<CachedBounds>, Parent<T>) -> Option<Enter> {
    within(target, false)
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Enter {
    /// Yields only the child with the given index.
    Only { child: u8 },
    /// Yields all children.
    All,
}

impl Enter {
    /// Constructs an [`Enter`] that skips over nodes outside of `target`.
    ///
    /// - `bounds` is the bounds of the node that is to be entered
    /// - `target` is the targeted area to be entered within `bounds`
    /// - `enter_target` indicates whether `target` and its children should also be entered
    pub fn within_node(
        node_bounds: Splittable<CachedBounds>,
        target: Bounds,
        enter_target: bool,
    ) -> Option<Self> {
        ((enter_target || !target.encloses(node_bounds.strip_cache()))
            && !target.is_disjoint(node_bounds.strip_cache()))
        .then(|| Self::one_or_all(node_bounds.extent(), target.to_ubounds3()))
    }

    /// Constructs an [`Enter`] that skips over nodes outside of `target`.
    ///
    /// - `bounds` is the (possibly clamped) bounds of the node
    /// - `node_extent` is the _full_ [`Extent`] of the node
    /// - `splits` is the number of splits of the node
    /// - `target` is the targeted area to be entered within `bounds`
    /// - `enter_target` indicates whether `target` and its children should also be entered
    pub fn within(
        bounds: UBounds3,
        node_extent: Splittable<CachedExtent>,
        target: UBounds3,
        enter_target: bool,
    ) -> Option<Self> {
        ((enter_target || !target.encloses(bounds))
            && !target.is_empty()
            && !bounds.is_disjoint(target))
        .then(|| Self::one_or_all(node_extent, target))
    }

    fn one_or_all(node_extent: Splittable<CachedExtent>, target: UBounds3) -> Self {
        let child_extent = node_extent.child_extent();
        let child_bounds = Bounds::with_extent_at(child_extent, target.lower());
        let upper_bounds_min = Bounds::min_with_extent_at(child_extent, target.upper() - 1);
        if child_bounds.min() == upper_bounds_min {
            Self::Only {
                child: child_bounds.child_index(node_extent.child_splits()),
            }
        } else {
            Self::All
        }
    }
}

/// Guides which nodes a [`HexDivNodeIterator`] should [`Enter`] via a `visit` callback.
///
/// Note, that it intentionally does not implement [`HexDivNodeIterator`] itself. The `visit`
/// callback is meant to filter out some nodes, which means the [`Iterator`] no longer returns the
/// correct sequence of [`Bounds`] as is required by [`HexDivNodeIterator`].
pub struct Visit<I: HexDivNodeIterator, V> {
    iter: I,
    visit: V,
    prev_node: Option<(Splittable<CachedBounds>, Parent<I::Node>)>,
}

impl<I: HexDivNodeIterator, V> Visit<I, V> {
    pub(super) fn new(iter: I, visit: V) -> Self {
        Self {
            iter,
            visit,
            prev_node: None,
        }
    }
}

impl<
        I: HexDivNodeIterator<Node: IsParent + Clone>,
        V: FnMut(Splittable<CachedBounds>, Parent<I::Node>) -> Option<Enter>,
    > Iterator for Visit<I, V>
{
    type Item = (CachedBounds, I::Node);

    fn next(&mut self) -> Option<Self::Item> {
        if let Some((bounds, node)) = self.prev_node.take() {
            self.iter.enter((self.visit)(bounds, node));
        }
        self.iter.next().inspect(|(bounds, node)| {
            if let Some(node) = Parent::new(node) {
                self.prev_node = Some((
                    Splittable::new(*bounds).expect("bounds should be splittable if the node is"),
                    node.cloned(),
                ));
            }
        })
    }
}

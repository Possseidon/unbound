use array_init::array_init;
use itertools::Itertools;

use super::{
    bounds::OctreeBounds,
    visit::{
        ChangeTracker, OctreeVisitor, OctreeVisitorMut, VisitSplit, VisitSplitMut, VisitValue,
    },
};
use crate::octree::bounds::OctreeBoundsSplit;

/// A node within an octree, either holding a value or is split into more nodes.
///
/// Nodes can be split in half, quarters or octants. Halfs and quarters can be used to store
/// non-cube octrees efficiently.
///
/// There is a hierarchy of which splits are allowed:
///
/// - A series of halfs along the same axis
/// - A series of quarters along the same axis
/// - A series of octants
///
/// I.e. it is not possible to half an octant. If halfing (followed by quartering) is required to
/// reach a cube, those have to happen first.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub(crate) enum Node<T> {
    Value(T),
    Halfs(Box<[Node<T>; 2]>),
    Quarters(Box<[Node<T>; 4]>),
    Octants(Box<[Node<T>; 8]>),
}

impl<T> Node<T> {
    pub(crate) fn visit(&self, visitor: &mut impl OctreeVisitor<Value = T>, bounds: OctreeBounds) {
        match self {
            Self::Value(value) => visitor.visit_value(bounds, value),
            Self::Halfs(nodes) => {
                Self::visit_split(visitor, bounds, nodes, OctreeBounds::split_in_half)
            }
            Self::Quarters(nodes) => {
                Self::visit_split(visitor, bounds, nodes, OctreeBounds::split_into_quarters)
            }
            Self::Octants(nodes) => {
                Self::visit_split(visitor, bounds, nodes, OctreeBounds::split_into_octants)
            }
        }
    }

    fn visit_split<const N: usize>(
        visitor: &mut impl OctreeVisitor<Value = T>,
        bounds: OctreeBounds,
        nodes: &[Self; N],
        split: fn(OctreeBounds) -> Option<[OctreeBounds; N]>,
    ) {
        match visitor.visit_split(bounds) {
            VisitSplit::Skip => {}
            VisitSplit::Enter => {
                let split_bounds = split(bounds).expect("bounds should split");
                for (node, half) in nodes.as_slice().iter().zip_eq(split_bounds) {
                    node.visit(visitor, half);
                }
            }
        }
    }

    fn value_or_distinct(&self) -> Result<&T, Distinct> {
        if let Node::Value(value) = self {
            Ok(value)
        } else {
            Err(Distinct)
        }
    }

    fn into_value(self) -> T {
        if let Node::Value(value) = self {
            value
        } else {
            panic!("node should contain a value")
        }
    }

    fn replace_node(&mut self, node: SplitResult<T>) -> bool {
        match node {
            SplitResult::Keep { changed } => changed,
            SplitResult::Merge(value) => {
                *self = Self::Value(value);
                true
            }
        }
    }
}

impl<T: Clone + PartialEq> Node<T> {
    pub(crate) fn visit_mut(
        &mut self,
        visitor: &mut impl OctreeVisitorMut<Value = T>,
        bounds: OctreeBounds,
    ) -> bool {
        match self {
            Self::Value(value) => {
                let mut change_tracker = ChangeTracker::new(value);
                if let Some(point) = bounds.to_point() {
                    visitor.visit_single_value_mut(point, &mut change_tracker);
                    change_tracker.changed()
                } else {
                    let result = visitor.visit_value_mut(bounds, &mut change_tracker);
                    let changed = change_tracker.changed();
                    match result {
                        VisitValue::Next => changed,
                        VisitValue::Split => {
                            let node = Self::new_split(visitor, bounds, value);
                            // intentionally avoid short-circuiting
                            if let Some(node) = node {
                                *self = node;
                                true
                            } else {
                                changed
                            }
                        }
                    }
                }
            }
            Self::Halfs(nodes) => {
                let node =
                    Self::visit_split_mut(visitor, bounds, nodes, OctreeBounds::split_in_half);
                self.replace_node(node)
            }
            Self::Quarters(nodes) => {
                let node = Self::visit_split_mut(
                    visitor,
                    bounds,
                    nodes,
                    OctreeBounds::split_into_quarters,
                );
                self.replace_node(node)
            }
            Self::Octants(nodes) => {
                let node =
                    Self::visit_split_mut(visitor, bounds, nodes, OctreeBounds::split_into_octants);
                self.replace_node(node)
            }
        }
    }

    fn new_split(
        visitor: &mut impl OctreeVisitorMut<Value = T>,
        bounds: OctreeBounds,
        value: &T,
    ) -> Option<Self> {
        match bounds.split().expect("bounds should split") {
            OctreeBoundsSplit::Half(bounds) => {
                Self::new_split_for(visitor, bounds, value, Self::Halfs)
            }
            OctreeBoundsSplit::Quarter(bounds) => {
                Self::new_split_for(visitor, bounds, value, Self::Quarters)
            }
            OctreeBoundsSplit::Octant(bounds) => {
                Self::new_split_for(visitor, bounds, value, Self::Octants)
            }
        }
    }

    fn new_split_for<const N: usize>(
        visitor: &mut impl OctreeVisitorMut<Value = T>,
        bounds: [OctreeBounds; N],
        value: &T,
        wrap: fn(Box<[Self; N]>) -> Self,
    ) -> Option<Self> {
        let mut nodes = array_init(|_| Self::Value(value.clone()));
        nodes
            .iter_mut()
            .zip_eq(bounds)
            .fold(false, |current, (node, bounds)| {
                // intentionally avoid short-circuiting
                current | node.visit_mut(visitor, bounds)
            })
            .then(|| {
                if nodes.iter().map(Self::value_or_distinct).all_equal() {
                    Self::Value(
                        nodes
                            .into_iter()
                            .next()
                            .expect("nodes should not be empty")
                            .into_value(),
                    )
                } else {
                    wrap(Box::new(nodes))
                }
            })
    }

    fn visit_split_mut<const N: usize>(
        visitor: &mut impl OctreeVisitorMut<Value = T>,
        bounds: OctreeBounds,
        nodes: &mut [Self; N],
        split: fn(OctreeBounds) -> Option<[OctreeBounds; N]>,
    ) -> SplitResult<T> {
        match visitor.visit_split_mut(bounds) {
            VisitSplitMut::Skip => SplitResult::Keep { changed: false },
            VisitSplitMut::Enter => {
                let split_bounds = split(bounds).expect("bounds should split");
                let changed = nodes.as_mut_slice().iter_mut().zip_eq(split_bounds).fold(
                    false,
                    |current, (node, half)| {
                        // intentionally avoid short-circuiting
                        current | node.visit_mut(visitor, half)
                    },
                );

                if changed && nodes.iter().map(Self::value_or_distinct).all_equal() {
                    // this clone could in theory be avoided, but...
                    // that would require unwrapping the box upfront
                    // -> which would require unnecessary allocations when repacking
                    // or it would require some wrangling with the replace_with crate
                    // -> which would probably require a lot of function inlining and duplication
                    SplitResult::Merge(nodes[0].clone().into_value())
                } else {
                    SplitResult::Keep { changed }
                }
            }
            VisitSplitMut::Fill(value) => SplitResult::Merge(value),
        }
    }
}

impl<T: Default> Default for Node<T> {
    fn default() -> Self {
        Self::Value(Default::default())
    }
}

#[derive(Clone, Copy, Debug)]
struct Distinct;

impl PartialEq for Distinct {
    fn eq(&self, _: &Self) -> bool {
        false
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
enum SplitResult<T> {
    Keep { changed: bool },
    Merge(T),
}

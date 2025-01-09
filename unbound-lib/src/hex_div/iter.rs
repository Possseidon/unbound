use glam::UVec3;

use super::{
    node::{self, visit::Enter, HexDivNode, NodeRef},
    visit::VisitNode,
    HexDivNodeRef, HexDivRef, ParentHexDivRef,
};
use crate::math::bounds::UBounds3;

pub struct Iter<'a, T> {
    bounds: UBounds3,
    inner: node::iter::Iter<'a, T>,
}

impl<'a, T: HexDivNode> Iter<'a, T> {
    pub(super) fn new(bounds: UBounds3, node: &'a T) -> Self {
        Self {
            bounds,
            inner: node.iter(),
        }
    }

    pub fn visit<V: FnMut(VisitNode<'a, T>) -> Enter>(self, visit: V) -> Visit<'a, T, V> {
        Visit {
            iter: self,
            visit,
            prev_node: None,
        }
    }

    pub fn skip_children(&mut self) {
        self.inner.skip_children();
    }

    pub fn only_child(&mut self, index: u8) {
        self.inner.only_child(index);
    }
}

impl<'a, T: HexDivNode> Iterator for Iter<'a, T> {
    type Item = (UVec3, HexDivNodeRef<'a, T>);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let (bounds, node) = self.inner.next()?;
            let bounds = bounds.to_ubounds3();
            let clamped = bounds.clamp(self.bounds);
            if clamped.is_empty() {
                self.inner.skip_children();
            } else {
                break Some((
                    clamped.lower() - self.bounds.lower(),
                    match node {
                        NodeRef::Node(node) => HexDivNodeRef::Node(HexDivRef {
                            bounds: {
                                UBounds3::new(
                                    clamped.lower().saturating_sub(bounds.lower()),
                                    node.extent().size()
                                        - bounds.upper().saturating_sub(clamped.upper()),
                                )
                            },
                            root: node,
                        }),
                        NodeRef::Leaf(leaf) => HexDivNodeRef::Leaf(leaf),
                    },
                ));
            }
        }
    }
}

pub struct Visit<'a, T: HexDivNode, V> {
    iter: Iter<'a, T>,
    visit: V,
    prev_node: Option<VisitNode<'a, T>>,
}

impl<'a, T: HexDivNode, V: FnMut(VisitNode<'a, T>) -> Enter> Iterator for Visit<'a, T, V> {
    type Item = (UVec3, HexDivNodeRef<'a, T>);

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(node) = self.prev_node.take() {
            match (self.visit)(node) {
                Enter::None => self.iter.skip_children(),
                Enter::Only { child } => self.iter.only_child(child),
                Enter::All => {}
            }
        }
        self.iter.next().inspect(|&(lower, node)| {
            if let HexDivNodeRef::Node(node) = node {
                if let Some(node) = ParentHexDivRef::new(node) {
                    self.prev_node = Some(VisitNode { lower, node })
                }
            }
        })
    }
}

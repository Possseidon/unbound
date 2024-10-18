use std::marker::PhantomData;

use arrayvec::ArrayVec;

use super::{extent::OctreeSplitList, ParentNodeMut, ParentNodeRef};

/// A stack of [`ParentNodeMut`].
///
/// Multiple mutable references to the same original root `T` can be pushed onto the stack, but only
/// the top reference can be accessed at any given time.
pub(crate) struct ParentNodeMutStack<'a, T> {
    /// Contains all [`ParentNodeMut`] that have been entered, including the root node.
    ///
    /// Only the last entry can be accessed.
    ///
    /// Stored as raw pointers to avoid aliased mutable borrows.
    parents: ArrayVec<*mut T, { OctreeSplitList::MAX }>,
    _mut: PhantomData<&'a mut T>,
}

impl<'a, T> ParentNodeMutStack<'a, T> {
    /// Constructs a new [`MutStack`] for the given mutable root reference `T`.
    pub(crate) fn new() -> Self {
        Self {
            parents: ArrayVec::new(),
            _mut: PhantomData,
        }
    }

    /// Returns a reference to the top of the stack.
    ///
    /// Technically, the returned reference only requires lifetime `'a` instead of that of `&self`.
    /// However, this would allow keeping references while [`Self::push`]ing new ones, which this
    /// type must prevent.
    pub(crate) fn last(&self) -> Option<ParentNodeRef<T>> {
        self.top_ptr().map(|ptr| {
            // SAFETY: Only the top node can be accessed at any given time.
            ParentNodeRef(unsafe { &*ptr })
        })
    }

    /// Returns a mutable reference to the top of the stack.
    ///
    /// Technically, the returned reference only requires lifetime `'a` instead of that of `&mut
    /// self`. However, this would allow keeping references while [`Self::push`]ing new ones, which
    /// this type must prevent.
    pub(crate) fn last_mut(&mut self) -> Option<ParentNodeMut<T>> {
        self.top_ptr().map(|ptr| {
            // SAFETY: Only the top node can be accessed at any given time.
            ParentNodeMut(unsafe { &mut *ptr })
        })
    }

    /// Pushes a new mutable reference.
    ///
    /// This makes all previously pushed references inaccessible, since multiple mutable references
    /// to the same `T` are not allowed in Rust.
    ///
    /// # Panics
    ///
    /// Panics if the capacity is exceeded.
    pub(crate) fn push(&mut self, node: ParentNodeMut<'a, T>) {
        self.parents.push(node.0 as *mut _);
    }

    /// Pops the top of the stack, which makes the previous top of the stack accessible again.
    ///
    /// # Panics
    ///
    /// Panics if the stack is empty.
    pub(crate) fn pop(&mut self) {
        assert!(!self.parents.is_empty(), "stack should not be empty");
        self.parents.pop();
    }

    /// Returns the top of the stack as a pointer.
    fn top_ptr(&self) -> Option<*mut T> {
        self.parents.last().copied()
    }
}

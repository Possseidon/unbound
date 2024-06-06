use std::{mem::replace, ops::Deref};

/// Wraps a mutable reference with proper change tracking based on [`PartialEq`].
///
/// Unlike change tracking in e.g. bevy, [`Self::changed`] returns `true` if and only the value is
/// actually different from the original.
///
/// To avoid name-clashes, the methods of [`Mut`] itself are associated functions, called using
/// fully qualified snytax (similar to e.g. [`Arc`](std::sync::Arc)):
///
/// ```
/// # use unbound_lib::change_tracking::Mut;
/// let mut value = 0;
/// let mut change_tracker = Mut::new(&mut value);
/// Mut::set(&mut change_tracker, 42);
/// assert!(Mut::changed(&change_tracker));
/// ```
///
/// However, one caveat applies: If the original value was not equal to itself (such as
/// [`f32::NAN`]), only [`Self::reset`] will be able to reset the "changed" flag, since
/// [`Self::set`] will (rightfully) consider even the "same" value as different from the original:
///
/// ```
/// # use unbound_lib::change_tracking::Mut;
/// let mut value = f32::NAN;
/// let mut change_tracker = Mut::new(&mut value);
/// Mut::set(&mut change_tracker, 42.0);
/// Mut::set(&mut change_tracker, f32::NAN); // still considered "changed"
/// assert!(Mut::changed(&change_tracker));
/// Mut::reset(&mut change_tracker); // reset to the rescue!
/// assert!(!Mut::changed(&change_tracker));
/// ```
///
/// Implements [`Deref`] for easy access to the contained value, but updates have to be done through
/// [`Self::set`], which makes proper change tracking possible without ever having to [`Clone`] `T`.
#[derive(Debug)]
pub struct Mut<'a, T> {
    /// The mutable reference which should be tracked for changes.
    value: &'a mut T,
    /// Stores the original value if, and only if, the value was changed.
    original_value: Option<T>,
}

impl<'a, T> Mut<'a, T> {
    /// Constructs a new [`Mut`] from a mutable reference.
    pub fn new(value: &'a mut T) -> Self {
        Self {
            value,
            original_value: None,
        }
    }

    /// Updates the value that was passed to [`Self::new`] while keeping track of changes.
    ///
    /// This stores the original value internally, which allows for [`Self::set`] to be called
    /// multiple times while still knowing if the value is different from the original.
    pub fn set(this: &mut Self, value: T)
    where
        T: PartialEq,
    {
        if let Some(original_value) = &mut this.original_value {
            if &value == original_value {
                this.original_value = None;
            }
            *this.value = value;
        } else if &value != this.value {
            this.original_value = Some(replace(this.value, value));
        }
    }

    /// Resets the value back to what was originally passed to [`Self::new`].
    ///
    /// Unlike [`Self::set`], this can reset the changed state even if the original value wasn't
    /// equal to itself such as [`f32::NAN`].
    pub fn reset(this: &mut Self) {
        if let Some(original_value) = this.original_value.take() {
            *this.value = original_value;
        }
    }

    /// Whether the current value differs from the one that was originally passed to [`Self::new`].
    pub fn changed(this: &Self) -> bool {
        this.original_value.is_some()
    }
}

impl<T> Deref for Mut<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.value
    }
}

use std::cell::UnsafeCell;
use std::sync::atomic::{AtomicBool, Ordering};
use std::fmt::{Debug, Display, Formatter, Result as FmtResult};
use std::ops::{Deref, DerefMut};

pub trait BorrowChecker<'a, T: 'a>: Default + Clone {
    type ExclusiveBorrow: DerefMut<Target = T> + 'a;

    fn borrow(&self);

    fn unborrow(&self);
}

#[derive(Debug, Default, Clone)]
pub struct NoCheck;

impl<'a, T: 'a> BorrowChecker<'a, T> for NoCheck {
    type ExclusiveBorrow = &'a mut T;

    #[inline(always)]
    fn borrow(&self) {
        ()
    }

    #[inline(always)]
    fn unborrow(&self) {
        ()
    }
}

#[derive(Debug, Default)]
pub struct AlwaysCheck {
    check: AtomicBool,
}

impl<'a, T: 'a> BorrowChecker<'a, T> for AlwaysCheck {
    type ExclusiveBorrow = CheckBorrow<'a, T, Self>;

    #[inline(always)]
    fn borrow(&self) {
        let was_borrowed = self.check.swap(true, Ordering::Relaxed);
        assert!(!was_borrowed, "Tried to reborrow");
    }

    #[inline(always)]
    fn unborrow(&self) {
        self.check.store(false, Ordering::Relaxed);
    }
}

#[derive(Debug, Default, Clone)]
#[repr(transparent)]
pub struct DebugCheck {
    #[cfg(any(debug_assertions, feature = "check_cell"))]
    check: AlwaysCheck,

    #[cfg(not(any(debug_assertions, feature = "check_cell")))]
    check: NoCheck,
}

impl<'a, T: 'a> BorrowChecker<'a, T> for DebugCheck {
    #[cfg(any(debug_assertions, feature = "check_cell"))]
    type ExclusiveBorrow = CheckBorrow<'a, T, Self>;

    #[cfg(not(any(debug_assertions, feature = "check_cell")))]
    type ExclusiveBorrow = &'a mut T;

    #[inline(always)]
    fn borrow(&self) {
        BorrowChecker::<'a, T>::borrow(&self.check);
    }

    #[inline(always)]
    fn unborrow(&self) {
        BorrowChecker::<'a, T>::unborrow(&self.check);
    }
}

impl Clone for AlwaysCheck {
    fn clone(&self) -> Self {
        AlwaysCheck {
            check: self.check.load(Ordering::Relaxed).into(),
        }
    }
}

pub struct CheckBorrow<'a, T: 'a, U: BorrowChecker<'a, T>> {
    value: &'a mut T,
    checker: &'a U,
}

impl<'a, T: 'a, U: BorrowChecker<'a, T>> Drop for CheckBorrow<'a, T, U> {
    fn drop(&mut self) {
        self.checker.unborrow();
    }
}

impl<'a, T: 'a, U: BorrowChecker<'a, T>> Deref for CheckBorrow<'a, T, U> {
    type Target = T;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        self.value
    }
}

impl<'a, T: 'a, U: BorrowChecker<'a, T>> DerefMut for CheckBorrow<'a, T, U> {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.value
    }
}

pub struct DebugCell<T, U: for<'a> BorrowChecker<'a, T> = DebugCheck> {
    checker: U,
    value: UnsafeCell<T>,
}

impl<T, U: for<'a> BorrowChecker<'a, T>> DebugCell<T, U> {
    #[inline]
    pub fn new(value: T) -> Self {
        DebugCell {
            value: UnsafeCell::new(value),
            checker: Default::default(),
        }
    }

    #[inline]
    pub fn into_inner(self) -> T {
        self.value.into_inner()
    }

    #[inline]
    pub fn get_mut(&mut self) -> &mut T {
        self.value.get_mut()
    }

    #[inline]
    pub fn get<'a>(&'a self) -> CheckBorrow<'a, T, U> {
        self.checker.borrow();
        CheckBorrow {
            value: unsafe { &mut *self.value.get() },
            checker: &self.checker,
        }
    }

    #[inline]
    pub fn convert<T2>(self) -> DebugCell<T2, U>
    where
        T: Into<T2>,
        U: for<'a> BorrowChecker<'a, T2>,
    {
        DebugCell {
            checker: self.checker,
            value: UnsafeCell::new(self.value.into_inner().into()),
        }
    }
}

impl<T: Default, U: for<'a> BorrowChecker<'a, T>> Default for DebugCell<T, U> {
    #[inline]
    fn default() -> Self {
        DebugCell {
            checker: Default::default(),
            value: Default::default(),
        }
    }
}

impl<T, U: for<'a> BorrowChecker<'a, T>> From<T> for DebugCell<T, U> {
    #[inline]
    fn from(value: T) -> Self {
        DebugCell::new(value)
    }
}

unsafe impl<T: Send, U: Send + for<'a> BorrowChecker<'a, T>> Send for DebugCell<T, U> {}
unsafe impl<T: Sync, U: Sync + for<'a> BorrowChecker<'a, T>> Sync for DebugCell<T, U> {}

impl<T: Debug, U: for<'a> BorrowChecker<'a, T>> Debug for DebugCell<T, U> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        f
            .debug_struct("DebugCell")
            .field("value", &self.value)
            .finish_non_exhaustive()
    }
}

impl<T: Display, U: for<'a> BorrowChecker<'a, T>> Display for DebugCell<T, U> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        f
            .debug_struct("DebugCell")
            .field("value", &self.value)
            .finish_non_exhaustive()
    }
}

impl<T: Clone, U: Clone + for<'a> BorrowChecker<'a, T>> Clone for DebugCell<T, U> {
    fn clone(&self) -> Self {
        self.get().clone().into()
    }
}
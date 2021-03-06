use std::fmt::{Display, Debug, Formatter, Result as FmtResult};
use derive_more::Deref;
use arrayvec::ArrayVec;
use super::*;

const MAX_STRING_BYTES: usize = 1024;

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Default, Deref)]
pub struct YString {
    #[deref]
    pub(super) data: Box<ArrayVec<u8, MAX_STRING_BYTES>>,
}

impl YString {
    #[inline]
    #[allow(unused_must_use)]
    pub fn pre_inc(&mut self) {
        self.data.try_push(b' ');
    }

    #[inline]
    pub fn pre_dec(&mut self) -> ValueResult<()> {
        if self.data.pop().is_some() {
            Ok(())
        } else {
            Err(RuntimeErr::EmptyStr)
        }
    }

    #[inline]
    pub fn clear(&mut self) {
        self.data.clear();
    }

    #[inline]
    pub fn duplicate(&mut self) {
        self.data.extend(self.data.clone().into_iter().take(MAX_STRING_BYTES - self.data.len()));
    }
}

impl Clone for YString {
    fn clone(&self) -> Self {
        YString {
            data: self.data.clone(),
        }
    }

    fn clone_from(&mut self, source: &Self) {
        self.data.clone_from(&source.data);
    }
}

impl Display for YString {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "{}", String::from_utf8_lossy(self.data.as_slice()))
    }
}

impl<T: Into<String>> From<T> for YString {
    fn from(string: T) -> Self {
        YString {
            data: Box::new(string.into().bytes().collect()),
        }
    }
}

impl AddAssign<&'_ Self> for YString {
    fn add_assign(&mut self, rhs: &Self) {
        self.data
            .try_extend_from_slice(&rhs[0..rhs.len().min(MAX_STRING_BYTES - self.len())])
            .unwrap_or_else(|_| if cfg!(debug_assertions) {
                unreachable!()
            } else {
                unsafe { std::hint::unreachable_unchecked() }
            })
    }
}

impl SubAssign<&'_ Self> for YString {
    fn sub_assign(&mut self, rhs: &Self) {
        if self.is_empty() || rhs.is_empty() {
            return;
        }
        for (start, s) in self.data.windows(rhs.len()).enumerate().rev() {
            if s == rhs.data.as_slice() {
                self.data.drain(start..start + rhs.len());
                return;
            }
        }
    }
}

impl Debug for YString {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        Display::fmt(self, f)
    }
}

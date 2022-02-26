use super::*;

const MAX_LENGTH: u16 = 1024;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub(super) struct LengthInterval {
    pub start: u16,
    pub end: u16,
}

impl LengthInterval {
    fn truncate(&mut self) {
        self.end = self.end.min(MAX_LENGTH + 1);
        self.start = self.start.min(self.end);
    }

    fn swap_if_necessary(&mut self) {
        if self.start > self.end {
            std::mem::swap(&mut self.start, &mut self.end);
        }
        self.truncate();
    }

    fn from<R: RangeBounds<u16>>(range: R) -> Self {
        let start = match range.start_bound() {
            Bound::Included(&i) => i,
            Bound::Excluded(&i) => i.saturating_add(1),
            Bound::Unbounded => 0,
        };
        let end = match range.start_bound() {
            Bound::Included(&i) => i.saturating_add(1),
            Bound::Excluded(&i) => i,
            Bound::Unbounded => MAX_LENGTH,
        };
        let mut interval = LengthInterval {
            start,
            end,
        };
        interval.swap_if_necessary();
        interval
    }
}

impl From<u16> for LengthInterval {
    fn from(len: u16) -> Self {
        let mut interval = LengthInterval {
            start: len,
            end: len,
        };
        interval.truncate();
        interval
    }
}

impl<'a> Arbitrary<'a> for LengthInterval {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        let start = u.int_in_range(0..=MAX_LENGTH + 1)?;
        let end = u.int_in_range(start..=MAX_LENGTH + 1)?;
        Ok(LengthInterval {
            start,
            end,
        })
    }

    fn size_hint(_: usize) -> (usize, Option<usize>) {
        (0, Some(4))
    }
}

impl RangeBounds<u16> for LengthInterval {
    fn start_bound(&self) -> Bound<&u16> {
        Bound::Included(&self.start)
    }

    fn end_bound(&self) -> Bound<&u16> {
        Bound::Excluded(&self.end)
    }

    fn contains<U>(&self, item: &U) -> bool
    where
        u16: PartialOrd<U>,
        U: ?Sized + PartialOrd<u16>,
    {
        (self.start..self.end).contains(item)
    }
}

impl Display for LengthInterval {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        if self.start == self.end {
            write!(f, "∅")
        } else {
            write!(f, "[length: {}..{}]", self.start, self.end)
        }
    }
}

impl Add for LengthInterval {
    type Output = Self;

    fn add(mut self, rhs: Self) -> Self::Output {
        self.start += rhs.start;
        self.end += rhs.end;
        self.truncate();
        self
    }
}

impl Sub for LengthInterval {
    type Output = (Self, bool);

    fn sub(mut self, rhs: Self) -> Self::Output {
        let start = self.start.checked_sub(rhs.end);
        self.start = start.unwrap_or(0);
        self.end = self.end.saturating_sub(rhs.start);
        (self, start.is_none())
    }
}

#[derive(Debug, Clone, Eq, Serialize, Deserialize)]
pub struct StringInterval {
    pub(super) length: LengthInterval,
    pub runtime_error: bool,
}

impl StringInterval {
    pub fn everything() -> Self {
        StringInterval {
            length: LengthInterval::from(..),
            runtime_error: false,
        }
    }

    pub fn nothing() -> Self {
        StringInterval {
            length: LengthInterval::from(0..0),
            runtime_error: false,
        }
    }

    pub fn is_nothing(&self) -> bool {
        self.length.start == self.length.end
    }

    pub fn pre_inc(&mut self) {
        self.length = self.length + 1.into();
    }

    pub fn pre_dec(&mut self) {
        let (length, runtime_error) = self.length - 1.into();
        self.length = length;
        self.runtime_error |= runtime_error;
    }

    #[must_use]
    pub fn int_eq(&self, other: &Self) -> NumberIntervals {
        let mut intervals = NumberIntervals::nothing();
        intervals.intervals.push(Number::ZERO.into());
        let intersects = other.length.contains(&self.length.start)
            || other.length.contains(&self.length.end)
            || self.length.contains(&other.length.start)
            || self.length.contains(&other.length.end);
        if intersects {
            intervals.intervals.push(Number::ONE.into());
        }
        intervals
    }

    #[must_use]
    pub fn int_ne(&self, other: &Self) -> NumberIntervals {
        !self.int_eq(other)
    }

    fn true_or_false() -> NumberIntervals {
        let mut intervals = NumberIntervals::nothing();
        intervals.intervals.push(Number::ZERO.into());
        intervals.intervals.push(Number::ONE.into());
        intervals
    }

    #[must_use]
    pub fn int_le(&self, _other: &Self) -> NumberIntervals {
        Self::true_or_false()
    }

    #[must_use]
    pub fn int_lt(&self, _other: &Self) -> NumberIntervals {
        Self::true_or_false()
    }

    #[must_use]
    pub fn int_ge(&self, _other: &Self) -> NumberIntervals {
        Self::true_or_false()
    }

    #[must_use]
    pub fn int_gt(&self, _other: &Self) -> NumberIntervals {
        Self::true_or_false()
    }

    pub fn union(&mut self, other: &Self) {
        if self.is_nothing() {
            self.clone_from(other);
        } else if !other.is_nothing() {
            self.length.start = self.length.start.min(other.length.start);
            self.length.end = self.length.end.min(other.length.end);
        }
    }
}

impl PartialEq for StringInterval {
    fn eq(&self, other: &Self) -> bool {
        self.length == other.length
    }
}

impl Default for StringInterval {
    fn default() -> Self {
        Self::everything()
    }
}

impl Display for StringInterval {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        Display::fmt(&self.length, f)
    }
}

impl From<&str> for StringInterval {
    fn from(s: &str) -> Self {
        let len = s.len().try_into().unwrap_or(MAX_LENGTH);
        StringInterval {
            length: len.into(),
            runtime_error: false,
        }
    }
}

impl From<YString> for StringInterval {
    fn from(ys: YString) -> Self {
        ys.to_string().as_str().into()
    }
}

impl AddAssign<&StringInterval> for StringInterval {
    fn add_assign(&mut self, rhs: &StringInterval) {
        self.length = self.length + rhs.length;
        self.runtime_error |= rhs.runtime_error;
    }
}

impl Add<&StringInterval> for StringInterval {
    type Output = Self;

    fn add(mut self, rhs: &StringInterval) -> Self::Output {
        self += rhs;
        self
    }
}

impl SubAssign<&StringInterval> for StringInterval {
    fn sub_assign(&mut self, rhs: &StringInterval) {
        let (length, runtime_error) = self.length - LengthInterval {
            start: 0,
            ..rhs.length
        };
        self.length = length;
        self.runtime_error |= runtime_error;
    }
}

impl Sub<&StringInterval> for StringInterval {
    type Output = Self;

    fn sub(mut self, rhs: &StringInterval) -> Self::Output {
        self -= rhs;
        self
    }
}

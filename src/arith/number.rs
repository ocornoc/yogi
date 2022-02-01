use itertools::Itertools;
use rand::{prelude::*, distributions::{*, uniform::*}};
use super::*;

#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Default, Arbitrary)]
pub struct Number(pub i64);

impl Number {
    const SCALE: i64 = 1000;
    const SCALE_F32: f32 = 1000.0;
    const SCALE_F64: f64 = 1000.0;
    pub const MAX: Number = Number(i64::MAX);
    pub const MIN: Number = Number(i64::MIN);
    pub const ONE: Number = Number(1 * Self::SCALE);
    pub const ZERO: Number = Number(0);
    const MAX_VAL_F64: f64 = Self::MAX.0 as f64;
    const MIN_VAL_F64: f64 = Self::MIN.0 as f64;
    const MAX_VAL_F32: f32 = Self::MAX_VAL_F64 as f32;
    const MIN_VAL_F32: f32 = Self::MIN_VAL_F64 as f32;

    pub fn new(n: f64) -> Self {
        if n.is_finite() && n <= Self::MAX_VAL_F64 && n >= Self::MIN_VAL_F64 {
            Number((n * Number::SCALE_F64) as i64)
        } else {
            Number::MIN
        }
    }

    pub fn new_f32(n: f32) -> Self {
        if n.is_finite() && n <= Self::MAX_VAL_F32 && n >= Self::MIN_VAL_F32 {
            Number((n * Number::SCALE_F32) as i64)
        } else {
            Number::MIN
        }
    }

    pub fn as_f64(self) -> f64 {
        self.0 as f64 / Self::SCALE_F64
    }

    pub fn as_f32(self) -> f32 {
        self.0 as f32 / Self::SCALE_F32
    }

    pub fn as_bool(self) -> bool {
        self != Self::ZERO
    }

    pub fn stringify_with_buffer(&self, buffer: &mut YString) {
        let data = buffer.data.as_mut();
        let int = self.0 / Self::SCALE;
        let mut dec = (self.0 % Self::SCALE).unsigned_abs() as u32;
        let neg = int.is_negative();

        let mut int = int.unsigned_abs();
        let mut rem;
        loop {
            rem = (int % 10) as u32;
            int /= 10;
            unsafe {
                let c = std::char::from_digit(rem, 10)
                    .unwrap_or_else(|| std::hint::unreachable_unchecked());
                data.push_unchecked(c as u8);
            }
            if int == 0 {
                break;
            }
        }

        if neg {
            unsafe { data.push_unchecked(b'-'); }
        }

        data.reverse();
        if dec == 0 {
            return;
        }

        unsafe { data.push_unchecked(b'.'); }
        let old_len = data.len();

        for _ in 0..3 {
            rem = dec % 10;
            dec /= 10;
            unsafe {
                let c = std::char::from_digit(rem, 10)
                    .unwrap_or_else(|| std::hint::unreachable_unchecked());
                data.push_unchecked(c as u8);
            }
            if dec == 0 {
                break;
            }
        }

        let len = data.len();
        data[old_len..len].reverse();
    }

    pub fn stringify(&self) -> YString {
        let mut s = YString::default();
        self.stringify_with_buffer(&mut s);
        s
    }

    pub fn div_assign(&mut self, other: Self) -> ValueResult<()> {
        *self = (*self / other)?;
        Ok(())
    }

    fn round_to_new(mut v: f64) -> Self {
        v += 5e-5_f64.copysign(v);
        Self::new(v)
    }

    pub fn pow(self, other: Self) -> Self {
        let v = self.as_f64().powf(other.as_f64());
        Self::round_to_new(v)
    }

    pub fn pow_assign(&mut self, other: Self) {
        *self = self.pow(other);
    }

    pub fn rem_assign(&mut self, other: Self) -> ValueResult<()> {
        *self = (*self % other)?;
        Ok(())
    }

    pub fn pre_inc(&mut self) {
        *self += Number::ONE;
    }

    pub fn pre_dec(&mut self) {
        *self -= Number::ONE;
    }

    pub fn abs(self) -> Self {
        Number(self.0.overflowing_abs().0)
    }

    pub fn sqrt(self) -> Self {
        if self.0.is_negative() || self.0 >= 9223372036854775000 {
            Number::MIN
        } else {
            let v = self.as_f64().sqrt();
            Self::round_to_new(v)
        }
    }

    pub fn sin(self) -> Self {
        Self::new((self.as_f32().to_radians() as f64).sin())
    }

    pub fn cos(self) -> Self {
        Self::new((self.as_f32().to_radians() as f64).cos())
    }

    pub fn tan(self) -> Self {
        Self::new((self.as_f32().to_radians() as f64).tan())
    }

    pub fn asin(self) -> Self {
        Number::new_f32(self.as_f32().asin().to_degrees())
    }

    pub fn acos(self) -> Self {
        Number::new_f32(self.as_f32().acos().to_degrees())
    }

    pub fn atan(self) -> Self {
        let mut atan = self.as_f32().atan().to_degrees();
        if atan == -90.0 {
            atan = 90.0;
        }
        Number::new_f32(atan)
    }

    pub fn fact(self) -> Self {
        if self.0.is_negative() {
            Number::MIN
        } else {
            let mut v = self.0 / Number::SCALE;
            let mut i = 0;
            let mut result = 1_i64;
            while v.is_positive() {
                i += 1;
                v -= 1;
                result = result.wrapping_mul(i);
            }
            Number(result.wrapping_mul(Number::SCALE))
        }
    }

    /// Get the number after this, or `None` if there isn't one.
    pub fn next(self) -> Option<Self> {
        if self == Self::MAX {
            None
        } else {
            Some(Number(self.0 + 1))
        }
    }

    /// Get the number before this, or `None` if there isn't one.
    pub fn prev(self) -> Option<Self> {
        if self == Self::MIN {
            None
        } else {
            Some(Number(self.0 - 1))
        }
    }
}

impl SampleUniform for Number {
    type Sampler = Interval;
}

impl Distribution<Number> for Standard {
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> Number {
       Number(self.sample(rng))
    }
}

impl Distribution<Number> for Open01 {
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> Number {
        (Number(1)..Number(Number::SCALE)).sample_single(rng)
    }
}

impl Distribution<Number> for OpenClosed01 {
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> Number {
        (Number(1)..=Number(Number::SCALE)).sample_single(rng)
    }
}

impl From<bool> for Number {
    fn from(b: bool) -> Self {
        if b {
            Number::ONE
        } else {
            Number::ZERO
        }
    }
}

impl From<i64> for Number {
    fn from(n: i64) -> Self {
        Number(n * Self::SCALE)
    }
}

impl From<f64> for Number {
    fn from(n: f64) -> Self {
        Number::new(n)
    }
}

impl From<Number> for bool {
    fn from(n: Number) -> Self {
        n != Number::ZERO
    }
}

impl From<Number> for f64 {
    fn from(n: Number) -> Self {
        n.as_f64()
    }
}

impl Debug for Number {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        Display::fmt(&self, f)
    }
}

impl Display for Number {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "{}", self.stringify())
    }
}

#[derive(Debug, Error, Clone, Copy)]
pub enum NumberParseErr {
    #[error("Number can't fit in i64")]
    Overflow,
    #[error("Found unknown char '{0:}'")]
    UnknownChar(char),
}

impl FromStr for Number {
    type Err = NumberParseErr;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let neg = s.as_bytes()[0] == b'-';
        let (mut big, small) = if let Some((big, small)) = s.split_once('.') {
            (big.chars(), Some(small.chars()))
        } else {
            (s.chars(), None)
        };
        if neg {
            big.next();
        }

        let mut val: i64 = 0;
        let mut exp: i64 = Number::SCALE / 10;

        for c in big.rev() {
            if c.is_ascii_digit() {
                exp = exp
                    .checked_mul(10)
                    .ok_or(NumberParseErr::Overflow)?;
                let d = exp
                    .checked_mul(c as i64 - '0' as i64)
                    .ok_or(NumberParseErr::Overflow)?;
                val = val.checked_add(d).ok_or(NumberParseErr::Overflow)?;
            } else {
                return Err(NumberParseErr::UnknownChar(c));
            }
        }

        if neg {
            val = val.checked_neg().ok_or(NumberParseErr::Overflow)?;
        }

        if let Some(small) = small {
            exp = Number::SCALE;
            if neg {
                exp = -exp;
            }
            for c in small.take(3) {
                if c.is_ascii_digit() {
                    exp = exp
                        .checked_div(10)
                        .ok_or(NumberParseErr::Overflow)?;
                    let d = exp
                        .checked_mul(c as i64 - '0' as i64)
                        .ok_or(NumberParseErr::Overflow)?;
                    val = val.checked_add(d).ok_or(NumberParseErr::Overflow)?;
                } else {
                    return Err(NumberParseErr::UnknownChar(c));
                }
            }
        }

        Ok(Number(val))
    }
}

impl Add for Number {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Number(self.0.wrapping_add(rhs.0))
    }
}

impl AddAssign for Number {
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs;
    }
}

impl Sub for Number {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Number(self.0.wrapping_sub(rhs.0))
    }
}

impl SubAssign for Number {
    fn sub_assign(&mut self, rhs: Self) {
        *self = *self - rhs;
    }
}

impl Neg for Number {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Number(self.0.overflowing_neg().0)
    }
}

impl Mul for Number {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        Number(self.0.wrapping_mul(rhs.0) / Self::SCALE)
    }
}

impl MulAssign for Number {
    fn mul_assign(&mut self, rhs: Self) {
        *self = *self * rhs;
    }
}

impl Not for Number {
    type Output = Self;

    fn not(self) -> Self::Output {
        if self == Number::ZERO {
            Number::ONE
        } else {
            Number::ZERO
        }
    }
}

impl Div for Number {
    type Output = ValueResult<Number>;

    fn div(self, rhs: Self) -> Self::Output {
        if rhs.0 == 0 {
            Err(RuntimeErr::DivZero)
        } else {
            Ok(Number(self.0.wrapping_mul(Number::SCALE).wrapping_div(rhs.0)))
        }
    }
}

impl Rem for Number {
    type Output = ValueResult<Number>;

    fn rem(self, rhs: Self) -> Self::Output {
        if rhs.0 == 0 {
            Err(RuntimeErr::ModZero)
        } else {
            Ok(Number(self.0.wrapping_rem(rhs.0)))
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct Interval {
    pub start: Number,
    pub end: Number,
}

impl Interval {
    fn from<R: RangeBounds<Number>>(r: R) -> Self {
        let start = match r.start_bound() {
            Bound::Included(&start) => start,
            Bound::Excluded(start) => start.next().unwrap_or(Number::MAX),
            Bound::Unbounded => Number::MIN,
        };
        let end = match r.end_bound() {
            Bound::Included(&end) => end,
            Bound::Excluded(end) => end.prev().unwrap_or(Number::MIN),
            Bound::Unbounded => Number::MAX,
        };
        Interval {
            start,
            end,
        }
    }

    fn should_combine_intervals(&self, rt: &Interval) -> bool {
        self.interval_overlap(rt) || self.interval_adjacent(rt)
    }
    
    fn interval_overlap(&self, rt: &Interval) -> bool {
        // we know self <= rt
        let left_end_ge_right_start = self.end >= rt.start;
        let same_start = self.start == rt.start;
        left_end_ge_right_start || same_start
    }
    
    fn interval_adjacent(&self, rt: &Interval) -> bool {
        // we know self <= rt and there's no overlap
        if let Some(next) = self.end.next() {
            next == rt.start
        } else {
            false
        }
    }

    fn swap_if_necessary(&mut self) {
        if self.start > self.end {
            std::mem::swap(&mut self.start, &mut self.end);
        }
    }

    fn to_i128(self) -> (i128, i128) {
        (self.start.0 as i128, self.end.0 as i128)
    }

    fn split_overflow(mut self, start_of: bool, end_of: bool) -> (Self, Option<Self>) {
        let (start, end) = (self.start.0, self.end.0);
        let other = match (start_of, end_of) {
            (false, false) | (true, true) => {
                // if neither bound overflows, then we can just use it as-is.
                // if both bounds overflow, then we just need to swap.
                self.swap_if_necessary();
                None
            },
            (true, false) => {
                // if start overflowed and end didn't, then start wrapped around MIN
                self.start = Number::MIN;
                Some(Interval {
                    start: Number(start),
                    end: Number::MAX,
                })
            },
            (false, true) => {
                // if end overflowed and start didn't, then end wrapped around MAX
                self.end = Number::MAX;
                Some(Interval {
                    start: Number::MIN,
                    end: Number(end),
                })
            },
        };
        (self, other)
    }

    fn width(self) -> u64 {
        abs_diff_number(self.end, self.start)
    }

    pub fn new(start: Number, end: Number) -> Self {
        let mut interval = Interval {
            start,
            end,
        };
        interval.swap_if_necessary();
        interval
    }
}

impl<'a> Arbitrary<'a> for Interval {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(Interval::new(u.arbitrary()?, u.arbitrary()?))
    }

    fn size_hint(depth: usize) -> (usize, Option<usize>) {
        arbitrary::size_hint::and(i64::size_hint(depth), i64::size_hint(depth))
    }
}

impl UniformSampler for Interval {
    type X = Number;

    fn new<B1, B2>(low: B1, high: B2) -> Self
    where
        B1: SampleBorrow<Self::X> + Sized,
        B2: SampleBorrow<Self::X> + Sized,
    {
        let mut high = high.borrow().clone();
        high = high.prev().unwrap_or(high);
        Interval::new(low.borrow().clone(), high)
    }

    fn new_inclusive<B1, B2>(low: B1, high: B2) -> Self
    where
        B1: SampleBorrow<Self::X> + Sized,
        B2: SampleBorrow<Self::X> + Sized,
    {
        Interval::new(low.borrow().clone(), high.borrow().clone())
    }

    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> Self::X {
        (self.start..=self.end).sample_single(rng)
    }
}

impl Distribution<Number> for Interval {
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> Number {
        UniformSampler::sample(self, rng)
    }
}

impl RangeBounds<Number> for Interval {
    fn start_bound(&self) -> Bound<&Number> {
        Bound::Included(&self.start)
    }

    fn end_bound(&self) -> Bound<&Number> {
        Bound::Included(&self.end)
    }

    fn contains<U: ?Sized + PartialOrd<Number>>(&self, item: &U) -> bool {
        *item >= self.start && *item <= self.end
    }
}

impl IntoIterator for Interval {
    type Item = Number;
    type IntoIter = std::iter::Map<RangeInclusive<i64>, fn(i64) -> Number>;

    #[inline(always)]
    fn into_iter(self) -> Self::IntoIter {
        (self.start.0..=self.end.0).map(Number)
    }
}

impl Display for Interval {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        if self.start == self.end {
            write!(f, "{{{}}}", self.start)
        } else {
            write!(f, "[{}, {}]", self.start, self.end)
        }
    }
}

impl From<Number> for Interval {
    fn from(n: Number) -> Self {
        Interval {
            start: n,
            end: n,
        }
    }
}

impl Neg for Interval {
    type Output = (Self, Option<Self>);

    fn neg(mut self) -> Self::Output {
        let other = if self.start != self.end && self.start == Number::MIN {
            self = Interval {
                start: -self.end,
                end: Number::MAX,
            };
            Some(Number::MIN.into())
        } else {
            self = Interval {
                start: -self.end,
                end: -self.start,
            };
            None
        };
        (self, other)
    }
}

impl Add<Interval> for Interval {
    type Output = (Self, Option<Self>);

    fn add(mut self, rhs: Interval) -> Self::Output {
        let (mut start, mut end) = self.to_i128();
        // [a, b] + [c, d] = [a + c, b + d]
        start = start + rhs.start.0 as i128;
        end = end + rhs.end.0 as i128;
        // if the addition would've had a range at least u64::MAX, then we know that it would
        // completely fill the range of i64 thanks to modular arithmetic and the pigeonhole
        // principle.
        if abs_diff_i128(start, end) >= u64::MAX as u128 {
            return (Interval::from(..), None);
        }
        // otherwise, we now know the addition would not have filled the range, and thus we can
        // split the interval in the case of overflows
        let (start, start_of) = self.start.0.overflowing_add(rhs.start.0);
        let (end, end_of) = self.end.0.overflowing_add(rhs.end.0);
        self.start.0 = start;
        self.end.0 = end;
        // we've checked against a full range, so now we can split any overflows
        self.split_overflow(start_of, end_of)
    }
}

impl Sub<Interval> for Interval {
    type Output = (Self, Option<Self>);

    fn sub(mut self, rhs: Interval) -> Self::Output {
        let (mut start, mut end) = self.to_i128();
        // [a, b] - [c, d] = [a, b] + -[c, d] = [a, b] + [-d, -c] = [a - d, b - c]
        // we don't just do [a, b] + -[c, d] explicitly because of the edge case of c = Number::MIN,
        // because -Number::MIN = Number::MIN due to wrapping.
        start = start - rhs.end.0 as i128;
        end = end - rhs.start.0 as i128;
        // if the subtraction would've had a range at least u64::MAX, then we know that it would
        // completely fill the range of i64 thanks to modular arithmetic and the pigeonhole
        // principle.
        if abs_diff_i128(start, end) >= u64::MAX as u128 {
            return (Interval::from(..), None);
        }
        // otherwise, we now know the subtraction would not have filled the range, and thus we can
        // split the interval in the case of overflows
        let (start, start_of) = self.start.0.overflowing_sub(rhs.end.0);
        let (end, end_of) = self.end.0.overflowing_sub(rhs.start.0);
        self.start.0 = start;
        self.end.0 = end;
        // we've checked against a full range, so now we can split any overflows
        self.split_overflow(start_of, end_of)
    }
}

#[derive(PartialEq, Eq, Clone, Copy)]
struct WrapDir(Option<bool>);

impl PartialOrd for WrapDir {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for WrapDir {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        use std::cmp::Ordering::*;

        match (self.0, other.0) {
            (l, r) if l == r => Equal,
            (Some(false), _) | (_, Some(true)) => Less,
            (Some(true), _) | (_, Some(false)) => Greater,
            _ => unreachable!(),
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
struct MulInfo {
    wrap_dir: WrapDir,
    mul: Number,
    left: i64,
    right: i64,
}

impl MulInfo {
    const fn new(left: i64, right: i64) -> Self {
        let (mul, wrapped) = left.overflowing_mul(right);
        MulInfo {
            wrap_dir: WrapDir(if wrapped {
                Some(if left.signum() * right.signum() == 1 {
                    true
                } else {
                    false
                })
            } else {
                None
            }),
            mul: Number(mul),
            left,
            right,
        }
    }

    fn split_overflow(self, other: Self) -> (Interval, Option<Interval>) {
        // we know self <= other
        match (self.wrap_dir.0, other.wrap_dir.0) {
            (l, r) if l == r => (Interval {
                start: self.mul,
                end: other.mul,
            }, None),
            (Some(false), _) => (
                Interval {
                    start: self.mul,
                    end: Number::MAX,
                },
                Some(Interval {
                    start: Number::MIN,
                    end: other.mul,
                }),
            ),
            (_, Some(true)) => (
                Interval {
                    start: other.mul,
                    end: Number::MAX,
                },
                Some(Interval {
                    start: Number::MIN,
                    end: self.mul,
                }),
            ),
            (_, _) => unreachable!(),
        }
    }
}

impl Mul<Interval> for Interval {
    type Output = (Self, Option<Self>);

    fn mul(self, rhs: Interval) -> Self::Output {
        let (start0, end0) = self.to_i128();
        let (start1, end1) = rhs.to_i128();
        // [a, b] * [c, d] = [min(a*c, a*d, b*c, b*d), max(a*c, a*d, b*c, b*d)], except this gets
        // hairy when taking into account wrapping. to take into account wrapping, we first exclude
        // the obvious case where the bounds are at least 2^64 apart, because it would just cover
        // the entire i64 range underlying Yolol numbers.
        let startstart = start0 * start1;
        let startend = start0 * end1;
        let endstart = end0 * start1;
        let endend = end0 * end1;
        let start = startstart.min(startend).min(endstart).min(endend);
        let end = startstart.max(startend).max(endstart).max(endend);
        // if the multiplication would've had a range at least u64::MAX, then we know that it would
        // completely fill the range of i64 thanks to modular arithmetic and the pigeonhole
        // principle.
        if abs_diff_i128(start, end) >= u64::MAX as u128 {
            return (Interval::from(..), None);
        }
        // otherwise, we now know the multiplication would not have filled the range, and thus we
        // split the interval in the case of overflows
        let (start0, end0) = (self.start.0, self.end.0);
        let (start1, end1) = (rhs.start.0, rhs.end.0);
        let startstart = MulInfo::new(start0, start1);
        let startend = MulInfo::new(start0, end1);
        let endstart = MulInfo::new(end0, start1);
        let endend = MulInfo::new(end0, end1);
        let mut start = startstart.min(startend).min(endstart).min(endend);
        let mut end = startstart.max(startend).max(endstart).max(endend);
        // don't forget to get rid of the extra 1000 we multiplied by because of fixed point. we
        // do it here instead of in MulInfo::new to avoid 2 expensive, unnecessary integer divisions
        start.mul.0 /= Number::SCALE;
        end.mul.0 /= Number::SCALE;
        start.split_overflow(end)
    }
}

#[derive(Debug, Clone, AsRef, Eq)]
pub struct NumberIntervals {
    #[as_ref]
    intervals: Vec<Interval>,
    runtime_error: bool,
}

impl NumberIntervals {
    const DEFAULT_ALLOC: usize = 10;

    /// Combine intervals that overlap or are adjacent.
    ///
    /// Assumes that intervals is sorted.
    fn combine(&mut self) {
        if self.intervals.len() <= 1 {
            return;
        }

        let mut remove = Vec::with_capacity(self.intervals.len());
        let mut combined = true;
        while combined {
            let mut start = self.intervals.len();
            while let [.., ref mut lt, ref rt] = self.intervals[..start] {
                if lt.should_combine_intervals(rt) {
                    *lt = Interval {
                        start: lt.start.min(rt.start),
                        end: lt.end.max(rt.end),
                    };
                    remove.push(start - 1);
                }
                start -= 1;
            }

            for &i in remove.iter() {
                self.intervals.remove(i);
            }
            combined = !remove.is_empty();
            remove.clear();
        }
    }

    /// Does a complete rebuild of the intervals, including sorting and recombining.
    fn rebuild(&mut self) {
        self.intervals.sort_unstable();
        self.combine();
    }

    /// An interval containing every number.
    pub fn everything() -> Self {
        let mut intervals = Vec::with_capacity(Self::DEFAULT_ALLOC);
        intervals.push(Interval::from(..));
        NumberIntervals {
            intervals,
            runtime_error: false,
        }
    }

    pub fn is_everything(&self) -> bool {
        self.intervals.as_slice() == &[Interval::from(..)]
    }

    /// An interval containing no number.
    pub fn nothing() -> Self {
        NumberIntervals {
            intervals: Vec::with_capacity(Self::DEFAULT_ALLOC),
            runtime_error: false,
        }
    }

    /// Returns whether, in whatever operations have happened to this interval since the last reset,
    /// could this interval have caused a runtime error.
    pub const fn could_runtime_err(&self) -> bool {
        self.runtime_error
    }

    /// Resets the runtime error possibility to `false`.
    pub fn reset_runtime_err(&mut self) {
        self.runtime_error = false;
    }

    pub fn contains(&self, number: Number) -> bool {
        self.as_ref().iter().any(|i| i.contains(&number))
    }
}

impl PartialEq for NumberIntervals {
    fn eq(&self, other: &Self) -> bool {
        self.intervals == other.intervals
    }
}

impl Display for NumberIntervals {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        if let [before@.., end] = self.intervals.as_slice() {
            for interval in before {
                write!(f, "{} \u{222a} ", interval)?;
            }

            Display::fmt(end, f)
        } else {
            write!(f, "∅")
        }
    }
}

impl Distribution<Number> for NumberIntervals {
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> Number {
        if self.intervals.is_empty() {
            panic!("Can't sample an empty interval!")
        } else if let [interval] = self.as_ref().as_slice() {
            Distribution::sample(interval, rng)
        } else {
            let sampler = WeightedIndex::new(
                self.intervals
                .iter()
                .map(|i| i.width()),
            ).ok().unwrap();
            Distribution::sample(&self.intervals[sampler.sample(rng)], rng)
        }
    }
}

impl<'a> Arbitrary<'a> for NumberIntervals {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        let mut intervals = NumberIntervals {
            intervals: u.arbitrary_iter()?.try_collect()?,
            runtime_error: false,
        };
        intervals.rebuild();
        Ok(intervals)
    }
}

impl From<Number> for NumberIntervals {
    fn from(n: Number) -> Self {
        let mut intervals = NumberIntervals::nothing();
        intervals.intervals.push(n.into());
        intervals
    }
}

impl IntoIterator for NumberIntervals {
    type Item = Number;
    type IntoIter = std::iter::Flatten<std::vec::IntoIter<Interval>>;

    fn into_iter(self) -> Self::IntoIter {
        self.intervals.into_iter().flatten()
    }
}

impl Neg for &mut NumberIntervals {
    type Output = ();

    fn neg(self) -> Self::Output {
        let mut old_intervals = Vec::with_capacity(2 * self.intervals.len());
        std::mem::swap(&mut old_intervals, &mut self.intervals);

        for interval in old_intervals.into_iter() {
            let (i0, i1) = -interval;
            self.intervals.push(i0);
            if let Some(i1) = i1 {
                self.intervals.push(i1);
            }
        }

        self.rebuild();
    }
}

impl Neg for NumberIntervals {
    type Output = Self;

    fn neg(mut self) -> Self::Output {
        -(&mut self);
        self
    }
}

impl AddAssign<&NumberIntervals> for NumberIntervals {
    fn add_assign(&mut self, rhs: &NumberIntervals) {
        let mut old_intervals = Vec::with_capacity(2 * self.intervals.len() * rhs.intervals.len());
        std::mem::swap(&mut old_intervals, &mut self.intervals);

        for (l, &r) in old_intervals.into_iter().cartesian_product(rhs.intervals.iter()) {
            let (i0, i1) = l + r;
            self.intervals.push(i0);
            self.intervals.extend(i1);
        }

        self.rebuild();
    }
}

impl Add<&NumberIntervals> for NumberIntervals {
    type Output = Self;

    fn add(mut self, rhs: &NumberIntervals) -> Self::Output {
        self += rhs;
        self
    }
}

impl AddAssign<Number> for NumberIntervals {
    fn add_assign(&mut self, rhs: Number) {
        let intervals: NumberIntervals = rhs.into();
        *self += &intervals;
    }
}

impl Add<Number> for NumberIntervals {
    type Output = Self;

    fn add(mut self, rhs: Number) -> Self::Output {
        self += rhs;
        self
    }
}

impl SubAssign<&NumberIntervals> for NumberIntervals {
    fn sub_assign(&mut self, rhs: &NumberIntervals) {
        let mut old_intervals = Vec::with_capacity(2 * self.intervals.len() * rhs.intervals.len());
        std::mem::swap(&mut old_intervals, &mut self.intervals);

        for (l, &r) in old_intervals.into_iter().cartesian_product(rhs.intervals.iter()) {
            let (i0, i1) = l - r;
            self.intervals.push(i0);
            self.intervals.extend(i1);
        }

        self.rebuild();
    }
}

impl Sub<&NumberIntervals> for NumberIntervals {
    type Output = Self;

    fn sub(mut self, rhs: &NumberIntervals) -> Self::Output {
        self -= rhs;
        self
    }
}

impl SubAssign<Number> for NumberIntervals {
    fn sub_assign(&mut self, rhs: Number) {
        let intervals: NumberIntervals = rhs.into();
        *self -= &intervals;
    }
}

impl Sub<Number> for NumberIntervals {
    type Output = Self;

    fn sub(mut self, rhs: Number) -> Self::Output {
        self -= rhs;
        self
    }
}

impl MulAssign<&NumberIntervals> for NumberIntervals {
    fn mul_assign(&mut self, rhs: &NumberIntervals) {
        let mut old_intervals = Vec::with_capacity(2 * self.intervals.len() * rhs.intervals.len());
        std::mem::swap(&mut old_intervals, &mut self.intervals);

        for (l, &r) in old_intervals.into_iter().cartesian_product(rhs.intervals.iter()) {
            let (i0, i1) = l * r;
            self.intervals.push(i0);
            self.intervals.extend(i1);
        }

        self.rebuild();
    }
}

impl Mul<&NumberIntervals> for NumberIntervals {
    type Output = Self;

    fn mul(mut self, rhs: &NumberIntervals) -> Self::Output {
        self *= rhs;
        self
    }
}

impl MulAssign<Number> for NumberIntervals {
    fn mul_assign(&mut self, rhs: Number) {
        let intervals: NumberIntervals = rhs.into();
        *self *= &intervals;
    }
}

impl Mul<Number> for NumberIntervals {
    type Output = Self;

    fn mul(mut self, rhs: Number) -> Self::Output {
        self *= rhs;
        self
    }
}

const fn abs_diff_i128(n: i128, m: i128) -> u128 {
    if n < m {
        (m as u128).wrapping_sub(n as u128)
    } else {
        (n as u128).wrapping_sub(m as u128)
    }
}

const fn abs_diff_number(Number(n): Number, Number(m): Number) -> u64 {
    if n < m {
        (m as u64).wrapping_sub(n as u64)
    } else {
        (n as u64).wrapping_sub(m as u64)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn intervals_rebuild() {
        let mut expected_intervals = NumberIntervals {
            intervals: vec![
                Interval::from(Number::new(-7.0)..Number::new(1.0)),
                Number::new(5.0).into(),
                Interval::from(Number::new(7.0)..Number::new(99.0)),
            ],
            runtime_error: false,
        };
        let mut intervals = expected_intervals.clone();
        intervals.rebuild();
        assert_eq!(intervals.intervals, expected_intervals.intervals);
        intervals.intervals.push(Number::new(5.2).into());
        expected_intervals.intervals.insert(2, Number::new(5.2).into());
        intervals.rebuild();
        assert_eq!(intervals.intervals, expected_intervals.intervals);
        intervals.intervals.push(Number::new(5.2).into());
        intervals.rebuild();
        assert_eq!(intervals.intervals, expected_intervals.intervals);
        intervals.intervals.push(Interval::from(Number::new(2.0)..Number::new(6.0)));
        expected_intervals.intervals[1] = Interval::from(Number::new(2.0)..Number::new(6.0));
        expected_intervals.intervals.remove(2);
        intervals.rebuild();
        assert_eq!(intervals.intervals, expected_intervals.intervals);
        intervals.intervals.push(Number::new(6.0).into());
        expected_intervals.intervals[1] = Interval::from(Number::new(2.0)..=Number::new(6.0));
        intervals.rebuild();
        assert_eq!(intervals.intervals, expected_intervals.intervals);
        intervals.intervals.push(Interval::from(..=Number::new(-7.0)));
        expected_intervals.intervals[0] = Interval::from(..Number::new(1.0));
        intervals.rebuild();
        assert_eq!(intervals.intervals, expected_intervals.intervals);
    }

    #[test]
    fn intervals_negation() {
        let mut intervals = NumberIntervals {
            intervals: vec![Number::MAX.into()],
            runtime_error: false,
        };
        intervals.rebuild();
        intervals = -intervals;
        assert_eq!(intervals.intervals, [Number::MIN.next().unwrap().into()]);
        intervals = -intervals;
        assert_eq!(intervals.intervals, [Number::MAX.into()]);
        intervals.intervals = vec![Number::MIN.into()];
        intervals = -intervals;
        assert_eq!(intervals.intervals, [Number::MIN.into()]);
        intervals = NumberIntervals::everything();
        intervals = -intervals;
        assert_eq!(intervals.intervals, NumberIntervals::everything().intervals);
        intervals = NumberIntervals::nothing();
        intervals = -intervals;
        assert_eq!(intervals.intervals, NumberIntervals::nothing().intervals);
    }

    #[test]
    fn intervals_addition() {
        let orig_intervals = NumberIntervals {
            intervals: vec![
                Interval::from(Number::new(-7.0)..=Number::new(1.0)),
                Number::new(5.0).into(),
                Interval::from(Number::new(7.0)..Number::new(99.0)),
            ],
            runtime_error: false,
        };
        let mut intervals = orig_intervals.clone();
        intervals += &NumberIntervals::everything();
        assert_eq!(intervals.intervals, NumberIntervals::everything().intervals);
        intervals = orig_intervals.clone();
        intervals += &NumberIntervals::nothing();
        assert_eq!(intervals.intervals, NumberIntervals::nothing().intervals);
        intervals = orig_intervals.clone();
        intervals += Number::new(1.0);
        assert_eq!(intervals.intervals, [
            Interval::from(Number::new(-6.0)..=Number::new(2.0)),
            Number::new(6.0).into(),
            Interval::from(Number::new(8.0)..Number::new(100.0)),
        ]);
        intervals += Number::new(-1.0);
        assert_eq!(intervals.intervals, orig_intervals.intervals);
        intervals = orig_intervals.clone();
        intervals += &NumberIntervals{
            intervals: vec![Interval::from(Number::new(-2.0)..=Number::new(1.0))],
            runtime_error: false,
        };
        assert_eq!(intervals.intervals, [
            Interval::from(Number::new(-9.0)..=Number::new(2.0)),
            Interval::from(Number::new(3.0)..Number::new(100.0)),
        ]);
        intervals = orig_intervals.clone();
        intervals += Number::MAX;
        assert_eq!(intervals.intervals, [
            Interval::from(Number::MIN..=(Number::MAX + Number::new(1.0))),
            (Number::new(5.0) + Number::MAX).into(),
            Interval::from((Number::new(7.0) + Number::MAX)..(Number::new(99.0) + Number::MAX)),
            Interval::from((Number::MAX + Number::new(-7.0))..=Number::MAX),
        ]);
        intervals = orig_intervals.clone();
        intervals += Number::MIN;
        assert_eq!(intervals.intervals, [
            Interval::from(Number::MIN..=(Number::MAX + Number::new(1.0)).next().unwrap()),
            (Number::new(5.0) + Number::MAX).next().unwrap().into(),
            Interval::from((
                Number::new(7.0) + Number::MAX).next().unwrap()
                ..(Number::new(99.0) + Number::MAX).next().unwrap(),
            ),
            Interval::from((Number::MAX + Number::new(-7.0)).next().unwrap()..=Number::MAX),
        ]);
    }

    #[test]
    fn intervals_multiplication() {
        let mut intervals = NumberIntervals {
            intervals: vec![
                Interval::from(Number::new(-2.0)..=Number::new(-1.0)),
            ],
            runtime_error: false,
        };
        let mut intervals2 = NumberIntervals {
            intervals: vec![
                Interval::from(Number::new(1.0)..=Number::new(2.0)),
            ],
            runtime_error: false,
        };
        intervals *= &intervals2;
        assert_eq!(intervals.intervals, [Interval::from(Number::new(-4.0)..=Number::new(-1.0))]);
        intervals = Number::MAX.into();
        intervals *= &intervals2;
        assert_eq!(intervals.intervals, NumberIntervals::everything().intervals);
        intervals = Number::new(10.0).into();
        intervals *= &NumberIntervals::everything();
        assert_eq!(intervals.intervals, NumberIntervals::everything().intervals);
        intervals = Number::new(10.0).into();
        intervals *= &NumberIntervals::nothing();
        assert_eq!(intervals.intervals, NumberIntervals::nothing().intervals);
        intervals.intervals = vec![
            Interval::from(Number::new(2.0)..=Number::new(3.0)),
            Interval::from(Number::new(26.0)..=Number::new(28.0)),
        ];
        intervals2.intervals = vec![
            Interval::from(Number::new(1.5)..=Number::new(10.0)),
            Interval::from(Number::new(100.0)..=Number::new(101.0)),
        ];
        intervals *= &intervals2;
        assert_eq!(intervals.intervals, [
            Interval::from(Number::new(3.0)..=Number::new(30.0)),
            Interval::from(Number::new(39.0)..=Number::new(303.0)),
            Interval::from(Number::new(2600.0)..=Number::new(2828.0)),
        ]);
        intervals = Number::MAX.into();
        intervals2 = Number::new(1.0).into();
        intervals *= &intervals2;
        println!("{}", intervals);
    }
}
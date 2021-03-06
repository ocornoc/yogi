use itertools::Itertools;
use arrayvec::ArrayVec;
use super::*;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Serialize, Deserialize)]
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

    fn mul_no_div_aux_i128(self, rhs: Interval) -> (i128, i128) {
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
        (start, end)
    }

    fn mul_no_div_aux_info(self, rhs: Interval) -> (MulInfo, MulInfo) {
        let (start0, end0) = (self.start.0, self.end.0);
        let (start1, end1) = (rhs.start.0, rhs.end.0);
        let startstart = MulInfo::new(start0, start1);
        let startend = MulInfo::new(start0, end1);
        let endstart = MulInfo::new(end0, start1);
        let endend = MulInfo::new(end0, end1);
        let start = startstart.min(startend).min(endstart).min(endend);
        let end = startstart.max(startend).max(endstart).max(endend);
        (start, end)
    }

    fn mul_no_div<const DIST: u128>(self, rhs: Interval) -> (bool, MulInfo, MulInfo) {
        let (start, end) = self.mul_no_div_aux_i128(rhs);
        // if the multiplication would've had a range at least u64::MAX, then we know that it would
        // completely fill the range of i64 thanks to modular arithmetic and the pigeonhole
        // principle.
        let filled = abs_diff_i128(start, end) >= DIST;
        // otherwise, we now know the multiplication would not have filled the range, and thus we
        // split the interval in the case of overflows
        let (start, end) = self.mul_no_div_aux_info(rhs);
        (filled, start, end)
    }

    fn abs(mut self) -> (Self, Option<Self>) {
        let extra = if self.start == Number::MIN {
            self.start = -Number::MAX;
            Some(Number::MIN.into())
        } else {
            None
        };
        self.start = self.start.abs();
        self.end = self.end.abs();
        self.swap_if_necessary();
        (self, extra)
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

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct MulInfo {
    wrap_dir: WrapDir,
    mul: Number,
}

impl MulInfo {
    const fn new(left: i64, right: i64) -> Self {
        let (mul, wrapped) = left.overflowing_mul(right);
        MulInfo {
            wrap_dir: WrapDir(if wrapped {
                Some(left.signum() * right.signum() == 1)
            } else {
                None
            }),
            mul: Number(mul),
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
        if let (false, mut start, mut end) = self.mul_no_div::<{u64::MAX as u128}>(rhs) {
            // don't forget to get rid of the extra 1000 we multiplied by because of fixed point. we
            // do it here instead of in MulInfo::new to avoid 2 expensive, unnecessary integer
            // divisions.
            start.mul.0 /= Number::SCALE;
            end.mul.0 /= Number::SCALE;
            start.split_overflow(end)
        } else {
            (Interval::from(..), None)
        }
    }
}

fn int_div_no_wrap(array: &mut ArrayVec<Interval, 12>, mut lhs: Interval, rhs: Interval) {
    let startstart = lhs.start.0 / rhs.start.0;
    let startend = lhs.start.0 / rhs.end.0;
    let endstart = lhs.end.0 / rhs.start.0;
    let endend = lhs.end.0 / rhs.end.0;
    lhs.start.0 = startstart.min(startend).min(endstart).min(endend);
    lhs.end.0 = startstart.max(startend).max(endstart).max(endend);
    array.push(lhs);
}

fn int_split_at<const SPLIT: i64>(mut interval: Interval) -> (ArrayVec<Interval, 2>, bool) {
    let split = Number(SPLIT);
    let splitp1 = Number(SPLIT + 1);
    let mut array = ArrayVec::new();
    if interval.contains(&split) {
        match (interval.start == split, interval.end == split) {
            (true, true) => (),
            (true, false) => {
                interval.start = splitp1;
                array.push(interval);
            },
            (false, true) => {
                interval.end = Number(SPLIT - 1);
                array.push(interval);
            },
            (false, false) => {
                array.push(Interval {
                    end: Number(SPLIT - 1),
                    ..interval
                });
                array.push(Interval {
                    start: splitp1,
                    ..interval
                });
            },
        }
        (array, true)
    } else {
        array.push(interval);
        (array, false)
    }
}

fn int_div_no_zero(array: &mut ArrayVec<Interval, 12>, orig_lhs: Interval, orig_rhs: Interval) {
    let (lhs, lhs_split) = int_split_at::<{Number::MIN.0}>(orig_lhs);
    let (rhs, rhs_split) = int_split_at::<-1>(orig_rhs);
    let mut need_to_handle_wrap = true;
    need_to_handle_wrap &= lhs_split;  // if lhs doesn't include Number::MIN, wrap is impossible
    need_to_handle_wrap &= rhs_split;  // if rhs doesn't include Number(-1), wrap is impossible
    if need_to_handle_wrap {
        // orig_lhs includes Number::MIN and rhs includes Number(-1)
        // first, let's deal with the wrap
        array.push(Number::MIN.into());
        // next, we'll divide the split lhs intervals by the original rhs (can't wrap)
        for lhs in lhs {
            int_div_no_wrap(array, lhs, orig_rhs);
        }
        // finally, we'll divide the original lhs by the split rhs intervals (can't wrap)
        for rhs in rhs {
            int_div_no_wrap(array, orig_lhs, rhs);
        }
    } else {
        // no possibility of wrap
        int_div_no_wrap(array, orig_lhs, orig_rhs);
    }
}

impl Div<Interval> for Interval {
    type Output = (ArrayVec<Interval, 12>, bool);

    fn div(self, rhs: Interval) -> Self::Output {
        const DIST: u128 = (u64::MAX as u128) * (Number::SCALE as u128);
        let mut array = ArrayVec::new();
        // if the right hand side contains a zero, we need to splice it out in case of runtime error
        // we do this by splitting the interval into 0, 1, or 2 parts and merge the intervals from
        // each division
        if rhs.contains(&Number::ZERO) {
            let (rhs, _) = int_split_at::<0>(rhs);
            for rhs in rhs {
                let (intervals, _) = self / rhs;
                array.extend(intervals);
            }
            (array, true)
        } else if let (false, self0, self1) = self.mul_no_div::<DIST>(Number::ONE.into()) {
            // if we can multiply by 1 *excluding rescaling down* without filling the entire range,
            // then we can split the one or two resulting intervals to get the results. this is
            // essentially a multiplication by 1000, which is present in the real definition of
            // division.
            let (self0, self1) = self0.split_overflow(self1);
            // now we need to be careful about division by Number(-1). this is the only case where
            // division can wrap, and it wraps to Number::MIN. int_div_aux handles this.
            int_div_no_zero(&mut array, self0, rhs);
            if let Some(self1) = self1 {
                int_div_no_zero(&mut array, self1, rhs);
            }
            (array, false)
        } else {
            // we couldn't multiply by 1 without filling the entire range, so we know the result
            array.push(Interval::from(..));
            (array, false)
        }
    }
}

fn interval_largest_multiple(interval: Interval, m: Number) -> Option<Number> {
    let candidate = Number(interval.end.0 - interval.end.0 % m.0);
    if interval.contains(&candidate) {
        Some(candidate)
    } else {
        None
    }
}

fn interval_rem_pos_pos(array: &mut ArrayVec<Interval, 16>, lhs: Interval, rhs: Interval) {
    let lhs_width = abs_diff_number(lhs.start, lhs.end);
    // if the lhs would cover the rhs at least once, then we can just return the full range
    if rhs.end.0 as u64 <= lhs_width {
        array.push(Interval::from(Number::ZERO..rhs.end));
        return;
    } else if rhs.start == rhs.end {
        if let Some(largest) = interval_largest_multiple(lhs, rhs.start) {
            if largest != lhs.start {
                array.push(Interval {
                    start: Number(lhs.start.0 % rhs.start.0),
                    end: rhs.start.prev().unwrap(),
                });
            }
            array.push(Interval {
                start: Number::ZERO,
                end: Number(lhs.end.0 % rhs.start.0),
            });
        } else {
            array.push(Interval {
                start: Number(lhs.start.0 % rhs.start.0),
                end: Number(lhs.end.0 % rhs.start.0),
            });
        }
        return;
    }
    // cop-out: actually calculating it is hard!
    array.push(Interval::from(Number::ZERO..rhs.end));
}

fn interval_rem_min_pos(array: &mut ArrayVec<Interval, 16>, rhs: Interval) {
    // cop-out: actually calculating it is hard!
    array.push(Interval::from((-rhs.end).next().unwrap()..=Number::ZERO));
}

fn interval_rem_aux(
    mut orig_lhs: Interval,
    mut orig_rhs: Interval,
) -> (ArrayVec<Interval, 16>, bool) {
    let mut array = ArrayVec::new_const();
    // if the right hand side contains a zero, we need to splice it out in case of runtime error
    // we do this by splitting the interval into 0, 1, or 2 parts and merge the intervals from
    // each division
    if orig_rhs.contains(&Number::ZERO) {
        let (rhs, _) = int_split_at::<0>(orig_rhs);
        for rhs in rhs {
            let (intervals, _) = interval_rem_aux(orig_lhs, rhs);
            array.extend(intervals);
        }
        (array, true)
    // we've filtered out the zeros, and now we filter out the case of MIN % _
    } else if orig_lhs.contains(&Number::MIN) && orig_rhs.contains(&Number(-1)) {
        let (lhs, _) = int_split_at::<{Number::MIN.0}>(orig_lhs);
        let (rhs, _) = int_split_at::<-1>(orig_rhs);
        for lhs in lhs.clone() {
            let (intervals, _) = interval_rem_aux(lhs, orig_rhs);
            array.extend(intervals);
        }
        for rhs in rhs {
            let (intervals, _) = interval_rem_aux(orig_lhs, rhs);
            array.extend(intervals);
        }
        array.push(Number::ZERO.into());
        (array, false)
    // because there's no MIN in lhs, we can split at zero on lhs
    } else if orig_lhs.contains(&Number::ZERO) {
        let (lhs, _) = int_split_at::<0>(orig_lhs);
        for lhs in lhs.clone() {
            let (intervals, _) = interval_rem_aux(lhs, orig_rhs);
            array.extend(intervals);
        }
        array.push(Number::ZERO.into());
        (array, false)
    } else if orig_rhs.contains(&Number::MIN) {
        let (rhs, _) = int_split_at::<{Number::MIN.0}>(orig_rhs);
        for rhs in rhs {
            let (intervals, _) = interval_rem_aux(orig_lhs, rhs);
            array.extend(intervals);
        }
        let negate = orig_lhs.start.0.is_negative();
        orig_lhs.start = orig_lhs.start.abs();
        orig_lhs.end = orig_lhs.end.abs();
        orig_lhs.swap_if_necessary();
        let len = array.len();
        interval_rem_pos_pos(&mut array, orig_lhs, Number::MIN.into());
        if negate {
            for interval in array[len..].iter_mut() {
                interval.start = -interval.start;
                interval.end = -interval.end;
            }
        }
        (array, false)
    } else if orig_lhs.contains(&Number::MIN) {
        let (lhs, _) = int_split_at::<{Number::MIN.0}>(orig_lhs);
        for lhs in lhs {
            let (intervals, _) = interval_rem_aux(lhs, orig_rhs);
            array.extend(intervals);
        }
        orig_rhs.start = orig_rhs.start.abs();
        orig_rhs.end = orig_rhs.end.abs();
        orig_rhs.swap_if_necessary();
        interval_rem_min_pos(&mut array, orig_rhs);
        (array, false)
    } else {
        let negate = orig_lhs.start.0.is_negative();
        orig_lhs.start = orig_lhs.start.abs();
        orig_lhs.end = orig_lhs.end.abs();
        orig_lhs.swap_if_necessary();
        orig_rhs.start = orig_rhs.start.abs();
        orig_rhs.end = orig_rhs.end.abs();
        orig_rhs.swap_if_necessary();
        let len = array.len();
        interval_rem_pos_pos(&mut array, orig_lhs, orig_rhs);
        if negate {
            for interval in array[len..].iter_mut() {
                interval.start = -interval.start;
                interval.end = -interval.end;
            }
        }
        (array, false)
    }
}

impl Rem for Interval {
    type Output = (ArrayVec<Interval, 16>, bool);

    fn rem(self, rhs: Self) -> Self::Output {
        let (mut array, runtime_error) = interval_rem_aux(self, rhs);
        for interval in array.iter_mut() {
            interval.swap_if_necessary();
        }
        (array, runtime_error)
    }
}

#[derive(Debug, Clone, AsRef, Eq, Serialize, Deserialize)]
pub struct NumberIntervals {
    #[as_ref]
    pub(super) intervals: Vec<Interval>,
    pub runtime_error: bool,
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

    pub fn contains(&self, number: Number) -> bool {
        self.as_ref().iter().any(|i| i.contains(&number))
    }

    pub fn pre_inc(&mut self) {
        *self += Number::ONE;
    }

    pub fn pre_dec(&mut self) {
        *self -= Number::ONE;
    }

    pub fn abs(&mut self) {
        let mut old_intervals = Vec::with_capacity(2 * self.intervals.len());
        std::mem::swap(&mut old_intervals, &mut self.intervals);

        for l in old_intervals.into_iter() {
            let (i0, i1) = l.abs();
            self.intervals.push(i0);
            self.intervals.extend(i1);
        }

        self.rebuild();
    }

    pub fn sqrt(&mut self) {
        // TODO: Placeholder
        self.intervals.clear();
        self.intervals.push(Interval::from(..));
    }

    pub fn sin(&mut self) {
        // TODO: Placeholder
        self.intervals.clear();
        self.intervals.push(Interval::from(-Number::ONE..=Number::ONE));
    }

    pub fn cos(&mut self) {
        // TODO: Placeholder
        self.intervals.clear();
        self.intervals.push(Interval::from(-Number::ONE..=Number::ONE));
    }

    pub fn tan(&mut self) {
        // TODO: Placeholder
        self.intervals.clear();
        self.intervals.push(Interval::from(..));
    }

    pub fn asin(&mut self) {
        // TODO: Placeholder
        self.intervals.clear();
        self.intervals.push(Interval::from(..));
    }

    pub fn acos(&mut self) {
        // TODO: Placeholder
        self.intervals.clear();
        self.intervals.push(Interval::from(..));
    }

    pub fn atan(&mut self) {
        // TODO: Placeholder
        self.intervals.clear();
        self.intervals.push(Interval::from(..));
    }

    pub fn fact(&mut self) {
        // TODO: Placeholder
        self.intervals.clear();
        self.intervals.push(Interval::from(..));
    }

    pub fn pow_assign(&mut self, _other: &NumberIntervals) {
        // TODO: Placeholder
        self.intervals.clear();
        self.intervals.push(Interval::from(..));
    }

    #[must_use]
    pub fn intersect(&self, other: &NumberIntervals) -> Self {
        let mut intervals = Vec::with_capacity(self.as_ref().len().max(other.as_ref().len()));
        for (&lhs, &rhs) in self.intervals.iter().cartesian_product(other.intervals.iter()) {
            let interval = Interval {
                start: lhs.start.max(rhs.start),
                end: lhs.end.min(rhs.end),
            };
            if interval.start <= interval.end {
                intervals.push(interval);
            }
        }
        NumberIntervals {
            intervals,
            runtime_error: false,
        }
    }

    #[must_use]
    pub fn union(&self, other: &NumberIntervals) -> Self {
        let mut intervals = Vec::with_capacity(self.as_ref().len() + other.as_ref().len());
        intervals.extend(self.intervals.iter().copied());
        intervals.extend(other.intervals.iter().copied());
        let mut intervals = NumberIntervals {
            intervals,
            runtime_error: false,
        };
        intervals.rebuild();
        intervals
    }

    pub fn is_disjoint(&self, other: &NumberIntervals) -> bool {
        self.intersect(other).as_ref().is_empty()
    }

    pub fn is_constant(&self) -> Option<Number> {
        match self.intervals.as_slice() {
            [i] if i.start == i.end => Some(i.start),
            _ => None,
        }
    }

    #[must_use]
    pub fn int_ne(&self, other: &NumberIntervals) -> NumberIntervals {
        let mut possibilities = NumberIntervals::nothing();
        if self.is_disjoint(other) {
            possibilities.intervals.push(Number::ONE.into());
        } else {
            possibilities.intervals.push(Number::ZERO.into());
            match (self.is_constant(), other.is_constant()) {
                (Some(l), Some(r)) if l == r => (),
                _ => {
                    possibilities.intervals.push(Number::ONE.into());
                },
            }
        }
        possibilities
    }

    #[must_use]
    pub fn int_eq(&self, other: &NumberIntervals) -> NumberIntervals {
        !self.int_ne(other)
    }

    #[must_use]
    pub fn int_le(&self, other: &NumberIntervals) -> NumberIntervals {
        let mut le = false;
        let mut gt = false;
        for lhs in self.intervals.iter() {
            for rhs in other.intervals.iter() {
                le |= lhs.start <= rhs.end;
                gt |= lhs.end > rhs.start;
            }
        }
        let mut possibilities = NumberIntervals::nothing();
        if le {
            possibilities.intervals.push(Number::ONE.into());
        }
        if gt {
            possibilities.intervals.push(Number::ZERO.into());
        }
        possibilities
    }

    #[must_use]
    pub fn int_lt(&self, other: &NumberIntervals) -> NumberIntervals {
        let mut le = false;
        let mut gt = false;
        for lhs in self.intervals.iter() {
            for rhs in other.intervals.iter() {
                le |= lhs.start <= rhs.end;
                gt |= lhs.end > rhs.start;
            }
        }
        let mut possibilities = NumberIntervals::nothing();
        if le {
            possibilities.intervals.push(Number::ONE.into());
        }
        if gt {
            possibilities.intervals.push(Number::ZERO.into());
        }
        possibilities
    }

    #[must_use]
    pub fn int_ge(&self, other: &NumberIntervals) -> NumberIntervals {
        !self.int_lt(other)
    }

    #[must_use]
    pub fn int_gt(&self, other: &NumberIntervals) -> NumberIntervals {
        !self.int_le(other)
    }

    #[must_use]
    pub fn stringify(&self) -> StringInterval {
        if self.intervals.is_empty() {
            return StringInterval::nothing();
        }

        let start = 0;  // placeholder
        let end = Number::MIN.to_string().len() as u16;  // placeholder

        StringInterval {
            length: LengthInterval {
                start,
                end,
            },
            runtime_error: false,
        }
    }

    #[must_use]
    pub fn as_bool(&self) -> BoolInterval {
        let mut boolify = BoolInterval::nothing();
        if self.contains(Number::ZERO) {
            boolify.bfalse = true;
        }
        if !self.intervals.is_empty() && *self != Number::ZERO.into() {
            boolify.btrue = true;
        }
        boolify
    }

    pub fn set_to_bool(&mut self, bools: BoolInterval) {
        self.intervals.clear();
        if bools.bfalse {
            self.intervals.push(Number::ZERO.into());
        }
        if bools.btrue {
            self.intervals.push(Number::ONE.into());
        }
    }
}

impl PartialEq for NumberIntervals {
    fn eq(&self, other: &Self) -> bool {
        self.intervals == other.intervals
    }
}

impl From<BoolInterval> for NumberIntervals {
    fn from(bools: BoolInterval) -> Self {
        let mut intervals = NumberIntervals::nothing();
        intervals.set_to_bool(bools);
        intervals
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
            write!(f, "???")
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

impl DivAssign<&NumberIntervals> for NumberIntervals {
    fn div_assign(&mut self, rhs: &NumberIntervals) {
        let mut old_intervals = Vec::with_capacity(12 * self.intervals.len() * rhs.intervals.len());
        std::mem::swap(&mut old_intervals, &mut self.intervals);

        for (l, &r) in old_intervals.into_iter().cartesian_product(rhs.intervals.iter()) {
            let (intervals, runtime_error) = l / r;
            self.runtime_error |= runtime_error;
            self.intervals.extend(intervals);
        }

        self.rebuild();
    }
}

impl Div<&NumberIntervals> for NumberIntervals {
    type Output = Self;

    fn div(mut self, rhs: &NumberIntervals) -> Self::Output {
        self /= rhs;
        self
    }
}

impl DivAssign<Number> for NumberIntervals {
    fn div_assign(&mut self, rhs: Number) {
        let intervals: NumberIntervals = rhs.into();
        *self /= &intervals;
    }
}

impl Div<Number> for NumberIntervals {
    type Output = Self;

    fn div(mut self, rhs: Number) -> Self::Output {
        self /= rhs;
        self
    }
}

impl RemAssign<&NumberIntervals> for NumberIntervals {
    fn rem_assign(&mut self, rhs: &NumberIntervals) {
        let mut old_intervals = Vec::with_capacity(4 * self.intervals.len() * rhs.intervals.len());
        std::mem::swap(&mut old_intervals, &mut self.intervals);

        for (l, &r) in old_intervals.into_iter().cartesian_product(rhs.intervals.iter()) {
            let (intervals, runtime_error) = l % r;
            self.runtime_error |= runtime_error;
            self.intervals.extend(intervals);
        }

        self.rebuild();
    }
}

impl Rem<&NumberIntervals> for NumberIntervals {
    type Output = Self;

    fn rem(mut self, rhs: &NumberIntervals) -> Self::Output {
        self %= rhs;
        self
    }
}

impl RemAssign<Number> for NumberIntervals {
    fn rem_assign(&mut self, rhs: Number) {
        let intervals: NumberIntervals = rhs.into();
        *self %= &intervals;
    }
}

impl Rem<Number> for NumberIntervals {
    type Output = Self;

    fn rem(mut self, rhs: Number) -> Self::Output {
        self %= rhs;
        self
    }
}

impl Not for &mut NumberIntervals {
    type Output = ();

    fn not(self) -> Self::Output {
        self.set_to_bool(!self.as_bool());
    }
}

impl Not for NumberIntervals {
    type Output = Self;

    fn not(self) -> Self::Output {
        (!self.as_bool()).into()
    }
}

impl BitAndAssign<&NumberIntervals> for NumberIntervals {
    fn bitand_assign(&mut self, rhs: &NumberIntervals) {
        let bools = self.as_bool() & rhs.as_bool();
        self.set_to_bool(bools);
    }
}

impl BitAnd<&NumberIntervals> for NumberIntervals {
    type Output = Self;

    fn bitand(self, rhs: &NumberIntervals) -> Self::Output {
        (self.as_bool() & rhs.as_bool()).into()
    }
}

impl BitAndAssign<Number> for NumberIntervals {
    fn bitand_assign(&mut self, rhs: Number) {
        let bools = self.as_bool() & rhs.into();
        self.set_to_bool(bools);
    }
}

impl BitAnd<Number> for NumberIntervals {
    type Output = Self;

    fn bitand(self, rhs: Number) -> Self::Output {
        (self.as_bool() & rhs.into()).into()
    }
}

impl BitOrAssign<&NumberIntervals> for NumberIntervals {
    fn bitor_assign(&mut self, rhs: &NumberIntervals) {
        let bools = self.as_bool() | rhs.as_bool();
        self.set_to_bool(bools);
    }
}

impl BitOr<&NumberIntervals> for NumberIntervals {
    type Output = Self;

    fn bitor(self, rhs: &NumberIntervals) -> Self::Output {
        (self.as_bool() | rhs.as_bool()).into()
    }
}

impl BitOrAssign<Number> for NumberIntervals {
    fn bitor_assign(&mut self, rhs: Number) {
        let bools = self.as_bool() | rhs.into();
        self.set_to_bool(bools);
    }
}

impl BitOr<Number> for NumberIntervals {
    type Output = Self;

    fn bitor(self, rhs: Number) -> Self::Output {
        (self.as_bool() | rhs.into()).into()
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
        assert_eq!(intervals.intervals, [Number::new(-0.001).into()]);
    }

    #[test]
    fn interval_division() {
        let mut intervals = NumberIntervals::from(Number::new(12.0));
        let mut intervals2 = NumberIntervals::from(Number::new(3.0));
        intervals /= &intervals2;
        assert!(!intervals.runtime_error);
        assert_eq!(intervals.intervals, [Number::new(4.0).into()]);
        intervals = NumberIntervals {
            intervals: vec![
                Interval::from(Number::new(-2.0)..=Number::new(-1.0)),
            ],
            runtime_error: false,
        };
        intervals2 = NumberIntervals {
            intervals: vec![
                Interval::from(Number::new(1.0)..=Number::new(2.0)),
            ],
            runtime_error: false,
        };
        intervals /= &intervals2;
        assert!(!intervals.runtime_error);
        assert_eq!(intervals.intervals, [
            Interval::from(Number::new(-2.0)..=Number::new(-0.5)),
        ]);
        intervals = Number::MAX.into();
        intervals2 = Number::ONE.into();
        intervals /= &intervals2;
        assert!(!intervals.runtime_error);
        assert_eq!(intervals.intervals, [Number(-1).into()]);
        intervals = Number::MAX.into();
        intervals2 = Number::ONE.into();
        intervals /= &intervals2;
        assert!(!intervals.runtime_error);
        assert_eq!(intervals.intervals, [Number(-1).into()]);
        intervals = Number::MAX.into();
        intervals2 = (-Number::ONE).into();
        intervals /= &intervals2;
        assert!(!intervals.runtime_error);
        assert_eq!(intervals.intervals, [Number(1).into()]);
        intervals = Number::MIN.into();
        intervals2 = Number::ONE.into();
        intervals /= &intervals2;
        assert!(!intervals.runtime_error);
        assert_eq!(intervals.intervals, [Number::ZERO.into()]);
        intervals = Number::MIN.into();
        intervals2 = (-Number::ONE).into();
        intervals /= &intervals2;
        assert!(!intervals.runtime_error);
        assert_eq!(intervals.intervals, [Number::ZERO.into()]);
    }

    #[test]
    fn interval_rem() {
        let mut intervals = NumberIntervals::from(Number::new(12.0));
        let mut intervals2 = NumberIntervals::from(Number::new(3.0));
        intervals %= &intervals2;
        assert!(!intervals.runtime_error);
        assert_eq!(intervals.intervals, [Number::ZERO.into()]);
        intervals = Number::new(12.0).into();
        intervals2 = Number::new(5.0).into();
        intervals %= &intervals2;
        assert!(!intervals.runtime_error);
        assert_eq!(intervals.intervals, [Number::new(2.0).into()]);
        intervals = Number::new(-12.0).into();
        intervals2 = Number::new(3.0).into();
        intervals %= &intervals2;
        assert!(!intervals.runtime_error);
        assert_eq!(intervals.intervals, [Number::ZERO.into()]);
        intervals = Number::new(-12.0).into();
        intervals2 = Number::new(5.0).into();
        intervals %= &intervals2;
        assert!(!intervals.runtime_error);
        assert_eq!(intervals.intervals, [Number::new(-2.0).into()]);
        intervals = Number::new(27.0).into();
        intervals2.intervals = vec![Interval::from(Number::new(7.0)..=Number::new(8.0))];
        intervals %= &intervals2;
        assert!(!intervals.runtime_error);
        assert_eq!(intervals.intervals, [Interval::from(Number::ZERO..Number::new(8.0))]);
        intervals.intervals = vec![Number::new(5.0).into(), Number::new(6.0).into()];
        intervals2 = Number::new(3.0).into();
        intervals %= &intervals2;
        assert!(!intervals.runtime_error);
        assert_eq!(intervals.intervals, [Number::ZERO.into(), Number::new(2.0).into()]);
        intervals.intervals = vec![
            Interval::from(Number::new(71776119061217.279)..=Number::new(9187201950427381.888)),
            Number::new(9187201950435737.471).into(),
        ];
        intervals2 = Number::new(9187201950435737.471).into();
        intervals %= &intervals2;
        assert!(!intervals.runtime_error);
        assert_eq!(intervals.intervals, [
            Number::ZERO.into(),
            Interval::from(Number::new(71776119061217.279)..=Number::new(9187201950427381.888)),
        ]);
        intervals = Number::new(5.0).into();
        intervals2.intervals = vec![Interval::from(Number::new(2.0)..=Number::new(3.0))];
        intervals %= &intervals2;
        assert!(!intervals.runtime_error);
        assert_eq!(intervals.intervals, [Interval::from(Number::ZERO..Number::new(3.0))]);
        intervals.intervals = vec![
            Interval::from(Number::new(-0.129)..=Number::new(1099511627.775)),
        ];
        intervals2.intervals = vec![
            Interval::from(Number::new(-0.001)..=Number::new(16777.215)),
        ];
        intervals %= &intervals2;
        assert!(intervals.runtime_error);
        assert_eq!(intervals.intervals, [
            Interval::from(Number::new(-16777.214)..=Number::new(16777.214)),
        ]);
        intervals.runtime_error = false;
        intervals.intervals = vec![
            Interval::from(Number(-9222790894089797632)..=Number::new(262.148)),
        ];
        intervals2.intervals = vec![
            Interval::from(Number(-4971411038663605760)..=Number(68961369294110720)),
        ];
        intervals %= &intervals2;
        assert!(intervals.runtime_error);
        assert_eq!(intervals.intervals, [
            Interval::from(Number(-4971411038663605759)..=Number(4971411038663605759)),
        ]);
        intervals.runtime_error = false;
        intervals.intervals = vec![
            Interval::from(Number::new(-17592186044.417)..=Number::new(-0.129)),
            Interval::from(Number::new(-0.001)..=Number::new(4398046511.359)),
        ];
        intervals2.intervals = vec![
            Interval::from(Number::MIN..=Number::new(-0.001)),
        ];
        intervals %= &intervals2;
        assert!(!intervals.runtime_error);
        assert_eq!(intervals.intervals, [
            Interval::from(Number(-9223372036854775806)..Number::MAX),
        ]);
        intervals.runtime_error = false;
        intervals.intervals = vec![
            Number::MIN.into(),
            Interval::from(Number(-36170086435815553)..=Number::MAX),
        ];
        intervals2.intervals = vec![
            Number(-4629771061636907073).into(),
            Interval::from(Number(-4539628415531089920)..=Number(43910186561175552)),
        ];
        intervals %= &intervals2;
        assert!(intervals.runtime_error);
        assert_eq!(intervals.intervals, [
            Interval::from(Number(-4629771061636907072)..=Number(4629771061636907072)),
        ]);
    }
}
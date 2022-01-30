#[cfg(not(target_os = "linux"))]
compile_error!("Gotta compile on linux");

use honggfuzz::fuzz;
use yogi::arith::{NumberIntervals, Number, Interval};

fn get_min_max(intervals: &[Interval]) -> Option<(Number, Number)> {
    Some((intervals.first()?.start, intervals.last()?.end))
}

fn fuzz_mul(mut left: NumberIntervals, right: &NumberIntervals) {
    if left.as_ref().is_empty() || right.as_ref().is_empty() {
        left *= right;
        assert_eq!(left, NumberIntervals::nothing());
        return;
    } else if left == Number::ZERO.into() || *right == Number::ZERO.into() {
        left *= right;
        assert_eq!(left, Number::ZERO.into());
        return;
    } else if left.is_everything() || right.is_everything() {
        left *= right;
        assert_eq!(left, NumberIntervals::everything());
        return;
    }
    let (left_min, left_max) = get_min_max(left.as_ref()).expect("Failed to get left min/max");
    let (right_min, right_max) = get_min_max(right.as_ref()).expect("Failed to get right min/max");
    left *= right;
    assert!(left.contains(left_min * right_min));
    assert!(left.contains(left_min * right_max));
    assert!(left.contains(left_max * right_min));
    assert!(left.contains(left_max * right_max));
}

fn fuzz_inner(left: NumberIntervals, right: NumberIntervals) {
    fuzz_mul(left, &right);
}

fn main() {
    loop {
        fuzz!(|data: (NumberIntervals, NumberIntervals)| {
            fuzz_inner(data.0, data.1)
        });
    }
}
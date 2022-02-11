#[cfg(target_os = "linux")]
mod everything {
    use honggfuzz::fuzz;
    use yogi::arith::{NumberIntervals, NumberInterval, Number};

    fn get_min_max(intervals: &[NumberInterval]) -> Option<(Number, Number)> {
        Some((intervals.first()?.start, intervals.last()?.end))
    }
    
    fn fuzz_add(mut left: NumberIntervals, right: &NumberIntervals) {
        if left.as_ref().is_empty() || right.as_ref().is_empty() {
            left += right;
            assert_eq!(left, NumberIntervals::nothing());
            return;
        } else if left.is_everything() || right.is_everything() {
            left += right;
            assert_eq!(left, NumberIntervals::everything());
            return;
        }
        let (left_min, left_max) = get_min_max(left.as_ref()).expect("Failed to get left min/max");
        let (right_min, right_max) = get_min_max(right.as_ref()).expect("Failed to get right min/max");
        left += right;
        assert!(left.contains(left_min + right_min));
        assert!(left.contains(left_min + right_max));
        assert!(left.contains(left_max + right_min));
        assert!(left.contains(left_max + right_max));
    }

    fn fuzz_sub(mut left: NumberIntervals, right: &NumberIntervals) {
        if left.as_ref().is_empty() || right.as_ref().is_empty() {
            left -= right;
            assert_eq!(left, NumberIntervals::nothing());
            return;
        } else if left.is_everything() || right.is_everything() {
            left -= right;
            assert_eq!(left, NumberIntervals::everything());
            return;
        }
        let (left_min, left_max) = get_min_max(left.as_ref()).expect("Failed to get left min/max");
        let (right_min, right_max) = get_min_max(right.as_ref()).expect("Failed to get right min/max");
        left -= right;
        assert!(left.contains(left_min - right_min));
        assert!(left.contains(left_min - right_max));
        assert!(left.contains(left_max - right_min));
        assert!(left.contains(left_max - right_max));
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

    fn fuzz_div(mut left: NumberIntervals, right: &NumberIntervals) {
        if left.as_ref().is_empty() || right.as_ref().is_empty() {
            left /= right;
            assert_eq!(left, NumberIntervals::nothing());
            assert!(!left.could_runtime_err());
            return;
        }
        let (left_min, left_max) = get_min_max(left.as_ref()).expect("Failed to get left min/max");
        let (right_min, right_max) = get_min_max(right.as_ref()).expect("Failed to get right min/max");
        let could_err = right.contains(Number::ZERO);
        left /= right;
        if let Ok(n) = left_min / right_min {
            assert!(left.contains(n));
        }
        if let Ok(n) = left_min / right_max {
            assert!(left.contains(n));
        }
        if let Ok(n) = left_max / right_min {
            assert!(left.contains(n));
        }
        if let Ok(n) = left_max / right_max {
            assert!(left.contains(n));
        }
        assert_eq!(left.could_runtime_err(), could_err);
    }

    fn fuzz_rem(mut left: NumberIntervals, right: &NumberIntervals) {
        if left.as_ref().is_empty() || right.as_ref().is_empty() {
            left %= right;
            assert_eq!(left, NumberIntervals::nothing());
            assert!(!left.could_runtime_err());
            return;
        }
        let (left_min, left_max) = get_min_max(left.as_ref()).expect("Failed to get left min/max");
        let (right_min, right_max) = get_min_max(right.as_ref()).expect("Failed to get right min/max");
        let could_err = right.contains(Number::ZERO);
        left %= right;
        if let Ok(n) = left_min % right_min {
            assert!(left.contains(n));
        }
        if let Ok(n) = left_min % right_max {
            assert!(left.contains(n));
        }
        if let Ok(n) = left_max % right_min {
            assert!(left.contains(n));
        }
        if let Ok(n) = left_max % right_max {
            assert!(left.contains(n));
        }
        assert_eq!(left.could_runtime_err(), could_err);
    }
    
    fn fuzz_inner(left: NumberIntervals, right: NumberIntervals) {
        fuzz_add(left.clone(), &right);
        fuzz_sub(left.clone(), &right);
        fuzz_mul(left.clone(), &right);
        fuzz_div(left.clone(), &right);
        fuzz_rem(left, &right);
    }
    
    pub fn main() {
        loop {
            fuzz!(|data: (NumberIntervals, NumberIntervals)| {
                fuzz_inner(data.0, data.1)
            });
        }
    }
}

#[cfg(not(target_os = "linux"))]
mod everything {
    pub fn main() {
        panic!("Gotta compile this on Linux!");
    }
}

fn main() {
    everything::main();
}
#[cfg(target_os = "linux")]
mod everything {
    use honggfuzz::fuzz;
    use yogi::arith::{NumberIntervals, Number, Interval};

    fn get_min_max(intervals: &[Interval]) -> Option<(Number, Number)> {
        Some((intervals.first()?.start, intervals.last()?.end))
    }

    fn fuzz_mul(mut left: NumberIntervals, right: &NumberIntervals) {
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

    fn fuzz_inner(left: NumberIntervals, right: NumberIntervals) {
        fuzz_mul(left, &right);
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
use std::io::Read;
use std::path::PathBuf;
use std::fs::File;
use arbitrary::Unstructured;
use clap::Parser;
use itertools::Itertools;
use yogi::arith::{NumberIntervals, Interval, Number};

fn get_intervals(s: &[u8]) -> (NumberIntervals, NumberIntervals) {
    Unstructured::new(s).arbitrary().unwrap()
}

fn get_min_max(intervals: &[Interval]) -> Option<(Number, Number)> {
    Some((intervals.first()?.start, intervals.last()?.end))
}

#[derive(Parser)]
/// Parser and displayer for interval fuzzing .covs
#[clap(name = "interval_fuzz_parse_cov", version, author)]
struct Cli {
    /// Parse an addition cov
    #[clap(long)]
    add: bool,
    /// Parse a subtraction cov
    #[clap(long)]
    sub: bool,
    /// Parse a multiplication cov
    #[clap(long)]
    mul: bool,
    path: PathBuf,
}

fn main() {
    let Cli {
        add,
        sub,
        mul,
        path,
    } = Cli::parse();
    let bytes: Vec<_> = File::open(path).unwrap().bytes().try_collect().unwrap();
    let (mut left, right) = get_intervals(&bytes);
    println!("Left: {}\nRight: {}", left, right);
    let left_right_min_max = if let (
        Some((left_min, left_max)),
        Some((right_min, right_max)),
    ) = (get_min_max(left.as_ref()), get_min_max(right.as_ref())) {
        Some((left_min, left_max, right_min, right_max))
    } else {
        None
    };
    if add {
        left += &right;
        if let Some((left_min, left_max, right_min, right_max)) = left_right_min_max {
            let min_min = left_min + right_min;
            let min_max = left_min + right_max;
            let max_min = left_max + right_min;
            let max_max = left_max + right_max;
            println!("min + min: {}", min_min);
            if !left.contains(min_min) {
                println!("UNCONTAINED!");
            }
            println!("min + max: {}", min_max);
            if !left.contains(min_max) {
                println!("UNCONTAINED!");
            }
            println!("max + min: {}", max_min);
            if !left.contains(max_min) {
                println!("UNCONTAINED!");
            }
            println!("max + max: {}", max_max);
            if !left.contains(max_max) {
                println!("UNCONTAINED!");
            }
        }
    } else if sub {
        left -= &right;
        if let Some((left_min, left_max, right_min, right_max)) = left_right_min_max {
            let min_min = left_min - right_min;
            let min_max = left_min - right_max;
            let max_min = left_max - right_min;
            let max_max = left_max - right_max;
            println!("min - min: {}", min_min);
            if !left.contains(min_min) {
                println!("UNCONTAINED!");
            }
            println!("min - max: {}", min_max);
            if !left.contains(min_max) {
                println!("UNCONTAINED!");
            }
            println!("max - min: {}", max_min);
            if !left.contains(max_min) {
                println!("UNCONTAINED!");
            }
            println!("max - max: {}", max_max);
            if !left.contains(max_max) {
                println!("UNCONTAINED!");
            }
        }
    } else if mul {
        left *= &right;
        if let Some((left_min, left_max, right_min, right_max)) = left_right_min_max {
            let min_min = left_min * right_min;
            let min_max = left_min * right_max;
            let max_min = left_max * right_min;
            let max_max = left_max * right_max;
            println!("min * min: {}", min_min);
            if !left.contains(min_min) {
                println!("UNCONTAINED!");
            }
            println!("min * max: {}", min_max);
            if !left.contains(min_max) {
                println!("UNCONTAINED!");
            }
            println!("max * min: {}", max_min);
            if !left.contains(max_min) {
                println!("UNCONTAINED!");
            }
            println!("max * max: {}", max_max);
            if !left.contains(max_max) {
                println!("UNCONTAINED!");
            }
        }
    } else {
        println!("You forgot to select a mode.");
        return;
    }
    println!("Result: {}", left);
}
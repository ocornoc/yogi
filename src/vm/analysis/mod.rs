use std::fmt::{Display, Formatter, Result as FmtResult};
use std::ops::{Range, RangeBounds, Bound, Deref, DerefMut};
use super::*;
use petgraph::{prelude::*, visit::{Walker, DfsPostOrder}};
use nohash_hasher::IntSet;

pub type CodeLoc = usize;

pub mod cfg;
pub mod dfg;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CodeRange(Range<CodeLoc>);

impl CodeRange {
    fn merge(&mut self, other: Self) {
        debug_assert_eq!(self.0.end, other.0.start);
        self.0.end = other.0.end;
    }
}

impl Display for CodeRange {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "instructions {} .. {}", self.0.start, self.0.end)
    }
}

impl RangeBounds<CodeLoc> for CodeRange {
    fn start_bound(&self) -> Bound<&CodeLoc> {
        self.0.start_bound()
    }

    fn end_bound(&self) -> Bound<&CodeLoc> {
        self.0.end_bound()
    }

    fn contains<U>(&self, item: &U) -> bool
    where
        CodeLoc: PartialOrd<U>,
        U: ?Sized + PartialOrd<CodeLoc>,
    {
        self.0.contains(item)
    }
}
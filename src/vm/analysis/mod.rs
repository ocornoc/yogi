use std::fmt::{Display, Formatter, Result as FmtResult};
use std::ops::{Range, RangeBounds, Bound, Deref, DerefMut};
use super::*;
use petgraph::{prelude::*, visit::{Walker, DfsPostOrder}};
use nohash_hasher::IntSet;
pub use cfg::ControlFlowGraph;
pub use dfg::DataFlowGraph;
pub use domg::DomGraph;
pub use tfg::TypeFlowGraph;

pub type CodeLoc = usize;

pub mod cfg;
pub mod dfg;
pub mod domg;
pub mod tfg;

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

#[derive(Debug)]
pub struct Optimizer {
    pub vm: VMExec,
    pub cfg: ControlFlowGraph,
    pub dfg: DataFlowGraph,
    pub domg: DomGraph<cfg::NodeIndex>,
}

impl Optimizer {
    pub fn new(vm: VMExec) -> Self {
        let mut cfg = vm.control_flow_graph();
        cfg.clean_up();
        let dfg = cfg.dfg(&vm);
        let domg = cfg.domg();
        Optimizer { vm, cfg, dfg, domg }
    }

    fn refresh(&mut self) {
        self.cfg = self.vm.control_flow_graph();
        self.cfg.clean_up();
        self.dfg = self.cfg.dfg(&self.vm);
        self.domg = self.cfg.domg();
    }

    fn replace_reg(&mut self, old: AnyReg, new: AnyReg) {
        self.vm.replace_reg(old, new);

    }

    pub fn unify_constants(&mut self) {
        let constants = self.dfg.constants().collect::<Vec<_>>();
        
    }
}

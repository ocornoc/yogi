use super::*;
use cfg::{*, NodeIndex};

type Idx = u16;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Node {
    reg: AnyReg,
}

impl Display for Node {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self.reg {
            AnyReg::Number(reg) => write!(f, "Number #{}", reg.0),
            AnyReg::String(reg) => write!(f, "String #{}", reg.0),
            AnyReg::Value(reg) => write!(f, "Value #{}", reg.0),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum SourceModify {
    Read,
    ReadWrite,
}

impl Display for SourceModify {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        f.write_str(match self {
            SourceModify::Read => "R",
            SourceModify::ReadWrite => "RW",
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum TargetModify {
    Read,
    Write,
    ReadWrite,
}

impl Display for TargetModify {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        f.write_str(match self {
            TargetModify::Read => "R",
            TargetModify::Write => "W",
            TargetModify::ReadWrite => "RW",
        })
    }
}

#[derive(Debug, Clone)]
pub struct DataFlowInfo {
    pub cfg_node: NodeIndex,
    pub instr: CodeLoc,
    pub source: SourceModify,
    pub target: TargetModify,
}

impl Display for DataFlowInfo {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "Instr #{} ({} -> {})", self.instr, self.source, self.target)
    }
}

#[derive(Debug, Clone)]
pub struct DataFlowGraph {
    graph: StableDiGraph<Node, DataFlowInfo, Idx>,
    globals: Vec<NodeIndex>,
}

impl Display for DataFlowGraph {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        petgraph::dot::Dot::new(&self.graph).fmt(f)
    }
}

impl Deref for DataFlowGraph {
    type Target = StableDiGraph<Node, DataFlowInfo, Idx>;

    fn deref(&self) -> &Self::Target {
        &self.graph
    }
}

impl DerefMut for DataFlowGraph {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.graph
    }
}

impl ControlFlowGraph {
    pub fn dfg(&self, vm: &VMExec) -> DataFlowGraph {
        let mut dfg = DataFlowGraph {
            graph: StableGraph::with_capacity(
                vm.numbers.len() + vm.strings.len() + vm.values.len(),
                vm.code.len(),
            ),
            globals: Vec::with_capacity(vm.globals.capacity()),
        };
        let mut map = AHashMap::with_capacity(dfg.graph.capacity().0);
        for reg in 0..vm.numbers.len() as u16 {
            let reg = NumberReg(reg).into();
            map.insert(reg, dfg.add_node(Node { reg }));
        }
        for reg in 0..vm.strings.len() as u16 {
            let reg = StringReg(reg).into();
            map.insert(reg, dfg.add_node(Node { reg }));
        }
        for reg in 0..vm.values.len() as u16 {
            let reg = ValueReg(reg).into();
            map.insert(reg, dfg.add_node(Node { reg }));
        }

        for section in self.node_indices() {
            for loc in self[section].0.clone() {
                let i_arg1: AnyReg;
                let mut i_arg2: Option<AnyReg> = None;
                let mut i_out: Option<AnyReg> = None;
                match vm.code[loc] {
                    Instr::JumpRel { condition: Some(condition), .. } => {
                        i_arg1 = condition.into();
                    },
                    Instr::MoveSV { arg, out } => {
                        i_arg1 = arg.into();
                        i_out = Some(out.into());
                    },
                    Instr::MoveNV { arg, out } => {
                        i_arg1 = arg.into();
                        i_out = Some(out.into());
                    },
                    Instr::MoveVV { arg, out } | Instr::IncV { arg, out }
                    | Instr::DecV { arg, out } => {
                        i_arg1 = arg.into();
                        i_out = Some(out.into());
                    },
                    Instr::MoveVN { arg, out } | Instr::BoolV { arg, out } => {
                        i_arg1 = arg.into();
                        i_out = Some(out.into());
                    },
                    Instr::AddS { arg1, arg2, out } | Instr::SubS { arg1, arg2, out } => {
                        i_arg1 = arg1.into();
                        i_arg2 = Some(arg2.into());
                        i_out = Some(out.into());
                    },
                    Instr::SubV { arg1, arg2, out } | Instr::AddV { arg1, arg2, out } => {
                        i_arg1 = arg1.into();
                        i_arg2 = Some(arg2.into());
                        i_out = Some(out.into());
                    },
                    Instr::AddN { arg1, arg2, out } | Instr::SubN { arg1, arg2, out }
                    | Instr::Mul { arg1, arg2, out } | Instr::Div { arg1, arg2, out }
                    | Instr::Mod { arg1, arg2, out } | Instr::Pow { arg1, arg2, out }
                    | Instr::And { arg1, arg2, out } | Instr::Or { arg1, arg2, out } => {
                        i_arg1 = arg1.into();
                        i_arg2 = Some(arg2.into());
                        i_out = Some(out.into());
                    },
                    Instr::Eq { arg1, arg2, out } | Instr::Le { arg1, arg2, out }
                    | Instr::Lt { arg1, arg2, out } => {
                        i_arg1 = arg1.into();
                        i_arg2 = Some(arg2.into());
                        i_out = Some(out.into());
                    },
                    Instr::IncS { arg, out } | Instr::DecS { arg, out } => {
                        i_arg1 = arg.into();
                        i_out = Some(out.into());
                    },
                    Instr::IncN { arg, out } | Instr::DecN { arg, out }
                    | Instr::Abs { arg, out } | Instr::Fact { arg, out }
                    | Instr::Sqrt { arg, out } | Instr::Sin { arg, out }
                    | Instr::Tan { arg, out } | Instr::Asin { arg, out }
                    | Instr::Acos { arg, out } | Instr::Atan { arg, out }
                    | Instr::Neg { arg, out } | Instr::Not { arg, out }
                    | Instr::BoolN { arg, out } | Instr::Cos { arg, out } => {
                        i_arg1 = arg.into();
                        i_out = Some(out.into());
                    },
                    Instr::LineStart(_) | Instr::JumpErr | Instr::JumpLine(_)
                    | Instr::JumpRel { .. } => continue,
                }
                let mut flow_info_arg1 = DataFlowInfo {
                    cfg_node: section,
                    instr: loc,
                    source: SourceModify::Read,
                    target: TargetModify::Read,
                };
                let mut flow_info_arg2;
                match (i_arg2, i_out) {
                    (None, None) => {
                        let node = map[&i_arg1];
                        dfg.add_edge(node, node, flow_info_arg1);
                    },
                    (None, Some(out)) => {
                        let arg = map[&i_arg1];
                        if i_arg1 == out {
                            flow_info_arg1.source = SourceModify::ReadWrite;
                            flow_info_arg1.target = TargetModify::ReadWrite;
                            dfg.add_edge(arg, arg, flow_info_arg1);
                        } else {
                            let out = map[&out];
                            flow_info_arg1.target = TargetModify::Write;
                            dfg.add_edge(arg, out, flow_info_arg1);
                        }
                    },
                    (Some(arg2), Some(out)) => {
                        let arg1 = map[&i_arg1];
                        if i_arg1 == arg2 {
                            if i_arg1 == out {
                                flow_info_arg1.source = SourceModify::ReadWrite;
                                flow_info_arg1.target = TargetModify::ReadWrite;
                                dfg.add_edge(arg1, arg1, flow_info_arg1);
                            } else {
                                flow_info_arg1.source = SourceModify::Read;
                                flow_info_arg1.target = TargetModify::Write;
                                dfg.add_edge(arg1, map[&out], flow_info_arg1);
                            }
                        } else if i_arg1 == out {
                            flow_info_arg2 = flow_info_arg1.clone();
                            flow_info_arg1.source = SourceModify::ReadWrite;
                            flow_info_arg1.target = TargetModify::ReadWrite;
                            dfg.add_edge(arg1, arg1, flow_info_arg1);
                            flow_info_arg2.target = TargetModify::Write;
                            dfg.add_edge(map[&arg2], arg1, flow_info_arg2);
                        } else if arg2 == out {
                            let arg2 = map[&arg2];
                            flow_info_arg2 = flow_info_arg1.clone();
                            flow_info_arg1.source = SourceModify::ReadWrite;
                            flow_info_arg1.target = TargetModify::ReadWrite;
                            dfg.add_edge(arg2, arg2, flow_info_arg1);
                            flow_info_arg2.target = TargetModify::Write;
                            dfg.add_edge(arg1, arg2, flow_info_arg2);
                        } else {
                            let out = map[&out];
                            flow_info_arg2 = flow_info_arg1.clone();
                            flow_info_arg1.target = TargetModify::Write;
                            dfg.add_edge(arg1, out, flow_info_arg1);
                            flow_info_arg2.target = TargetModify::Write;
                            dfg.add_edge(map[&arg2], out, flow_info_arg2);
                        }
                    },
                    _ => unreachable!(),
                }
            }
        }
        dfg.globals.extend(vm.globals.values().map(|reg| map[reg]));
        dfg
    }
}
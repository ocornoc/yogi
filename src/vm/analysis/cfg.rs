use super::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum ReachReason {
    Continue,
    JumpRel,
    RuntimeError,
    JumpLine,
    LineEnd,
}

impl Display for ReachReason {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        f.write_str(match self {
            ReachReason::Continue => "continue",
            ReachReason::JumpRel => "relative jump",
            ReachReason::RuntimeError => "runtime error",
            ReachReason::JumpLine => "jump to line",
            ReachReason::LineEnd => "end of line",
        })
    }
}

type Idx = u16;
pub type NodeIndex = petgraph::stable_graph::NodeIndex<Idx>;

#[derive(Debug, Clone)]
pub struct ControlFlowGraph {
    graph: StableDiGraph<CodeRange, ReachReason, Idx>,
    root: NodeIndex,
}

impl ControlFlowGraph {
    fn section_containing(&self, loc: CodeLoc) -> Option<NodeIndex> {
        for node in self.graph.node_indices() {
            if self[node].contains(&loc) {
                return Some(node);
            }
        }

        None
    }

    fn merge_nodes(&mut self, left: NodeIndex, right: NodeIndex) {
        let edges_to = self
            .edges_directed(right, Direction::Incoming)
            .map(|e| (e.id(), e.source()))
            .filter(|&(_, source)| source != left)
            .collect::<Vec<_>>();
        let edges_from = self
            .edges_directed(right, Direction::Outgoing)
            .map(|e| (e.id(), e.target()))
            .collect::<Vec<_>>();
        for (e, other) in edges_to {
            let reason = self.remove_edge(e).unwrap();
            self.add_edge(other, left, reason);
        }
        for (e, other) in edges_from {
            let reason = self.remove_edge(e).unwrap();
            self.add_edge(left, other, reason);
        }
        let range = self.remove_node(right).unwrap();
        self[left].merge(range);
    }

    fn merge_adjacent_nodes(&mut self) -> bool {
        let mut did_anything = false;
        while let Some((left, right)) = self.node_indices().find_map(|n| {
            let mut iter = self.edges_directed(n, Outgoing);
            if let Some(first) = iter.next() {
                if iter.next().is_some() {
                    None
                } else {
                    let right = first.target();
                    if n == right || right == self.root || *first.weight() > ReachReason::JumpRel {
                        None
                    } else {
                        Some((n, right))
                    }
                }
            } else {
                None
            }
        }) {
            self.merge_nodes(left, right);
            did_anything = true;
        }
        did_anything
    }

    fn merge_edges(&mut self) {
        let nodes = self.node_indices().collect::<Vec<_>>();
        let mut edges = Vec::with_capacity(5);
        let mut to_delete = Vec::with_capacity(5);
        for node in nodes {
            for neighbor in self.neighbors_directed(node, Incoming) {
                edges.clear();
                edges.extend(self
                    .edges_directed(node, Incoming)
                    .filter(|edge| edge.source() == neighbor)
                    .map(|edge| edge.id())
                );
                if let Some(best_edge) = edges
                    .iter()
                    .copied()
                    .max_by(|&l, &r| self[l].cmp(&self[r]))
                {
                    to_delete.extend(edges.iter().copied().filter(|&edge| edge != best_edge));
                }
            }
        }
        for edge in to_delete {
            self.remove_edge(edge);
        }
    }

    fn trim_unreachable(&mut self) {
        let mut reachable = IntSet::with_capacity_and_hasher(self.node_count(), Default::default());
        let walker = DfsPostOrder::new(&self.graph, self.root);
        reachable.extend(walker.iter(&self.graph).map(|n| n.index()));
        for node in self.node_indices().collect::<Vec<_>>() {
            if !reachable.contains(&node.index()) {
                self.remove_node(node);
            }
        }
    }

    fn remove_zero_length_span(&mut self, node: NodeIndex) {
        let incoming = self.edges_directed(node, Incoming)
            .map(|edge| (edge.id(), edge.source()))
            .collect::<Vec<_>>();
        let outgoing = self.edges_directed(node, Outgoing)
            .map(|edge| (edge.id(), edge.target()))
            .collect::<Vec<_>>();
        for &(e0, e0s) in incoming.iter() {
            let e0reason = self[e0];
            for &(_, e1t) in outgoing.iter() {
                self.add_edge(e0s, e1t, e0reason);
            }
        }
        for &(e1, e1t) in outgoing.iter() {
            let e1reason = self[e1];
            for &(_, e0s) in incoming.iter() {
                self.add_edge(e0s, e1t, e1reason);
            }
        }
        self.remove_node(node);
    }

    fn remove_zero_length_spans(&mut self) {
        for node in self.node_indices().collect::<Vec<_>>() {
            if let Some(range) = self.node_weight(node).cloned() {
                if range.0.start == range.0.end {
                    self.remove_zero_length_span(node);
                }
            }
        }
    }

    pub fn clean_up(&mut self) {
        self.merge_edges();
        self.trim_unreachable();
        self.remove_zero_length_spans();
        self.merge_edges();
        while self.merge_adjacent_nodes() {
            self.merge_edges();
        }
    }
}

impl Display for ControlFlowGraph {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        petgraph::dot::Dot::new(&self.graph).fmt(f)
    }
}

impl Deref for ControlFlowGraph {
    type Target = StableDiGraph<CodeRange, ReachReason, Idx>;

    fn deref(&self) -> &Self::Target {
        &self.graph
    }
}

impl DerefMut for ControlFlowGraph {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.graph
    }
}

impl VMExec {
    fn next_line(&self, loc: CodeLoc) -> (Line, CodeLoc) {
        for (line, &start) in self.line_starts.iter().enumerate() {
            if loc < start {
                return (line as u8, self.line_starts[line]);
            }
        }

        (0, 0)
    }

    fn cfg_aux(&self, graph: &mut ControlFlowGraph) -> (
        Vec<(NodeIndex, CodeLoc, bool)>,
        Vec<CodeLoc>,
    ) {
        let mut last_node = None;
        let mut jumps = Vec::with_capacity(10);
        let mut runtime_errs = Vec::with_capacity(100);
        let mut code = self.code.iter().enumerate();
        if let Some((_, _)) = code.next() {
            let root = graph.add_node(CodeRange(0..1));
            graph.root = root;
            last_node = Some(root);
        }
        for (loc, instr) in code {
            let node = graph.add_node(CodeRange(loc..loc + 1));
            match instr {
                Instr { tag: InstrTag::LineStart, .. } => {
                    if let Some(last_node) = last_node {
                        graph.add_edge(last_node, node, ReachReason::LineEnd);
                    }
                    last_node = Some(node);
                },
                &Instr { tag: InstrTag::JumpRel, data, reg0,  .. } => {
                    let amount = unsafe { reg0.assume_init().ip_offset } as usize;
                    if data & 1 == 0 {
                        if let Some(last_node) = last_node {
                            graph.add_edge(last_node, node, ReachReason::Continue);
                        }
                        last_node = Some(node);
                    }

                    jumps.push((node, loc + amount + 1, true));
                },
                Instr { tag: InstrTag::JumpLine, .. } => {
                    if let Some(last_node) = last_node {
                        graph.add_edge(last_node, node, ReachReason::Continue);
                    }
                    for start in self.line_starts {
                        jumps.push((node, start, false));
                    }
                    last_node = None;
                },
                Instr { tag: InstrTag::JumpErr, .. } => {
                    if let Some(last_node) = last_node {
                        graph.add_edge(last_node, node, ReachReason::Continue);
                    }
                    runtime_errs.push(loc);
                    last_node = Some(node);
                },
                _ => {
                    if let Some(last_node) = last_node {
                        graph.add_edge(last_node, node, ReachReason::Continue);
                    }
                    last_node = Some(node);
                },
            }
        }
        if let Some(last_node) = last_node {
            let root = graph.root.clone();
            graph.add_edge(last_node, root, ReachReason::LineEnd);
        }
        (jumps, runtime_errs)
    }

    pub fn control_flow_graph(&self) -> ControlFlowGraph {
        let mut graph = ControlFlowGraph {
            graph: StableDiGraph::with_capacity(50, 50),
            root: NodeIndex::new(0),
        };
        let (jumps, runtime_errs) = self.cfg_aux(&mut graph);
        for (before, end, rel) in jumps {
            let node = graph.section_containing(self.next_line(end).1).unwrap();
            graph.add_edge(before, node, if rel {
                ReachReason::JumpRel
            } else {
                ReachReason::JumpLine
            });
        }
        for err_loc in runtime_errs {
            let err = graph.section_containing(err_loc).unwrap();
            let node = graph.section_containing(self.next_line(err_loc).1).unwrap();
            graph.add_edge(err, node, ReachReason::RuntimeError);
        }
        graph
    }
}

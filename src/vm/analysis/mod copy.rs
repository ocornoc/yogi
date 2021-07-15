use std::fmt::{Display, Formatter, Result as FmtResult};
use std::ops::{Range, RangeBounds, Bound, Deref, DerefMut};
use super::*;
use petgraph::{prelude::*, visit::{Walker, DfsPostOrder}};
use nohash_hasher::IntSet;

pub type CodeLoc = usize;

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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CodeRange(Range<CodeLoc>);

impl CodeRange {
    fn split_at(&self, i: CodeLoc) -> (CodeRange, CodeRange) {
        (CodeRange(self.0.start..i), CodeRange(i..self.0.end))
    }

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

#[derive(Debug, Clone)]
pub struct ControlFlowGraph {
    graph: StableDiGraph<CodeRange, ReachReason>,
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
                    if n == right || right == self.root {
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

    fn split_node(&mut self, node: NodeIndex, at: CodeLoc) -> NodeIndex {
        let (left, right) = self[node].split_at(at);
        let new_node = self.add_node(right);
        self[node] = left;
        let outgoing = self.edges_directed(node, Outgoing)
            .map(|edge| (edge.id(), edge.target()))
            .collect::<Vec<_>>();
        for (edge, target) in outgoing {
            let weight = self.remove_edge(edge).unwrap();
            self.add_edge(new_node, target, weight);
        }
        new_node
    }
}

impl Display for ControlFlowGraph {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        petgraph::dot::Dot::new(&self.graph).fmt(f)
    }
}

impl Deref for ControlFlowGraph {
    type Target = StableDiGraph<CodeRange, ReachReason>;

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
                return (line as u8 - 1, self.line_starts[line - 1]);
            }
        }

        (0, 0)
    }

    fn cfg_aux(&self, graph: &mut ControlFlowGraph) -> (
        Vec<(NodeIndex, CodeLoc, bool)>,
        Vec<CodeLoc>,
    ) {
        let mut root_made = false;
        let mut last_node = None;
        let mut leftover = CodeRange(0..self.code.len());
        let mut jumps = Vec::with_capacity(10);
        let mut runtime_errs = Vec::with_capacity(100);
        for (loc, instr) in self.code.iter().enumerate() {
            match instr {
                &Instr::LineStart(_) => {
                    let (before, now_and_after) = leftover.split_at(loc);
                    let node = graph.add_node(before);
                    if let Some(last_node) = last_node {
                        graph.add_edge(last_node, node, ReachReason::LineEnd);
                    }
                    last_node = Some(node);
                    leftover = now_and_after;
                },
                &Instr::JumpRel { amount, .. } => {
                    let (now_and_before, after) = leftover.split_at(loc + 1);
                    let node = graph.add_node(now_and_before);
                    if let Some(last_node) = last_node {
                        graph.add_edge(last_node, node, ReachReason::Continue);
                    }
                    jumps.push((node, loc + amount, true));
                    last_node = Some(node);
                    leftover = after;
                },
                Instr::JumpLine(_) => {
                    let (now_and_before, after) = leftover.split_at(loc + 1);
                    let node = graph.add_node(now_and_before);
                    for start in self.line_starts {
                        jumps.push((node, start, false));
                    }
                    last_node = None;
                    leftover = after;
                },
                Instr::MoveVS { .. } | Instr::MoveVN { .. } | Instr::SubS { .. }
                | Instr::SubV { .. } | Instr::DecS { .. } | Instr::DecV { .. } => {
                    let (now_and_before, after) = leftover.split_at(loc + 1);
                    let node = graph.add_node(now_and_before);
                    if let Some(last_node) = last_node {
                        graph.add_edge(last_node, node, ReachReason::Continue);
                    }
                    runtime_errs.push(loc);
                    last_node = Some(node);
                    leftover = after;
                },
                _ => continue,
            }
            if !root_made {
                graph.root = last_node.unwrap_or_else(|| graph.node_indices().next().unwrap());
                root_made = true;
            }
        }
        let node = graph.add_node(leftover);
        if let Some(last_node) = last_node {
            graph.add_edge(last_node, node, ReachReason::Continue);
        }
        let root = graph.root.clone();
        graph.add_edge(node, root, ReachReason::LineEnd);
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
            let new_node = graph.split_node(node, end);
            graph.add_edge(node, new_node, ReachReason::Continue);
            graph.add_edge(before, new_node, if rel {
                ReachReason::JumpRel
            } else {
                ReachReason::JumpLine
            });
        }
        for err_loc in runtime_errs {
            let node = graph.section_containing(self.next_line(err_loc).1).unwrap();
            let new_node = graph.split_node(node, err_loc);
            graph.add_edge(node, new_node, ReachReason::RuntimeError);
        }
        graph
    }
}

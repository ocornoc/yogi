use std::hash::Hash;
use std::fmt::Debug;
use petgraph::{visit::*, graphmap::NodeTrait};
use super::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Dominates;

impl Display for Dominates {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        f.write_str("dominates")
    }
}

#[derive(Debug, Clone)]
pub struct DomGraph<NodeId: NodeTrait> {
    pub graph: DiGraphMap<NodeId, Dominates>,
}

impl<NodeId: NodeTrait> DomGraph<NodeId> {
    pub fn new<'a, G>(graph: &'a G, root: NodeId) -> Self
    where
        &'a G: IntoNeighbors<NodeId=NodeId> + Visitable<NodeId=NodeId>
    {
        let doms = petgraph::algo::dominators::simple_fast(graph, root);
        let mut graph = GraphMap::default();
        let mut todo = Vec::with_capacity(10);
        todo.push(root);

        while let Some(node) = todo.pop() {
            graph.add_node(node);
            for idom in doms.immediately_dominated_by(node) {
                if node != idom && graph.add_edge(node, idom, Dominates).is_none() {
                    todo.push(idom);
                }
            }
        }

        DomGraph { graph }
    }
}

impl DomGraph<cfg::NodeIndex> {
    pub fn display<'a>(&'a self, cfg: &'a cfg::ControlFlowGraph) -> DomGraphDisplay<'a> {
        DomGraphDisplay {
            domg: self,
            cfg,
        }
    }
}

impl<NodeId: NodeTrait> Default for DomGraph<NodeId> {
    fn default() -> Self {
        DomGraph {
            graph: GraphMap::default(),
        }
    }
}

impl<NodeId: NodeTrait> Deref for DomGraph<NodeId> {
    type Target = DiGraphMap<NodeId, Dominates>;

    fn deref(&self) -> &Self::Target {
        &self.graph
    }
}

impl<NodeId: NodeTrait> DerefMut for DomGraph<NodeId> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.graph
    }
}

impl ControlFlowGraph {
    pub fn domg(&self) -> DomGraph<cfg::NodeIndex> {
        DomGraph::new(&self.graph, self.root)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct DomGraphDisplay<'a> {
    domg: &'a DomGraph<cfg::NodeIndex>,
    cfg: &'a ControlFlowGraph,
}

impl Display for DomGraphDisplay<'_> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        f.write_str("digraph {\n")?;
        for node in self.domg.nodes() {
            writeln!(f, "\t{} [ label = \"{}\" ]", node.index(), self.cfg[node])?;
        }
        for (source, target, _) in self.domg.all_edges() {
            writeln!(f, "\t{} -> {}", source.index(), target.index())?;
        }
        f.write_str("}")
    }
}

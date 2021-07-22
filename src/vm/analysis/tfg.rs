use super::*;
use cfg::NodeIndex;
use dfg::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TypeStatus {
    Unknown,
    Number, // todo: use union-find from petgraph to use refinement types
    String, // todo: use union-find from petgraph to use refinement types
    Value,  // todo: use union-find from petgraph to use refinement types
    Divergent,
}

#[derive(Debug, Clone)]
pub enum Edge {

}

#[derive(Debug, Clone)]
pub struct TypeFlowGraph {
    graph: StableDiGraph<Node, Edge>,
    pub(in crate::vm) globals: Vec<NodeIndex>,
}

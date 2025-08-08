use crate::NodeIndex;

/// Standard if statement with optional else and recursive else-if blocks.
#[derive(Debug)]
pub struct IfStatement {
    pub condition_expr: NodeIndex,
    pub then_block: NodeIndex,
    // Optionally, could be a `Block` to terminate the chain, or
    // another `IfStatement` chain.
    pub else_node: Option<NodeIndex>,
}

/// Assignment statement (i.e. lhs = rhs;)
#[derive(Debug)]
pub struct AssignStatement {
    pub lhs_expr: NodeIndex,
    pub rhs_expr: NodeIndex,
}

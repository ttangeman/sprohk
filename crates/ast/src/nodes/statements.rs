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

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

/// All loop statement and control flow representations:
/// `Unbounded` is an infinite loop that can only be terminated by `break`
/// `While` is a loop with a condition for termination
/// `For` is a loop over a range of elements
/// `Break` exits any loop control flow
/// `Continue` resets loop control flow
#[derive(Debug)]
pub enum LoopStatement {
    // `loop { ... }`
    Unbounded {
        block: NodeIndex,
    },
    // `while <condition> { ... }`
    While {
        condition_expr: NodeIndex,
        block: NodeIndex,
    },
    // `for <range> { ... }`
    //For {},
    Break,
    Continue,
}

/// Return statement for a semantic value block or function
#[derive(Debug)]
pub struct ReturnStatement {
    // Optional return expression (can have naked return
    // for early return in void functions)
    pub return_expr: Option<NodeIndex>,
}

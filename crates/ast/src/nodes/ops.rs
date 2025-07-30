use crate::nodes::NodeIndex;

pub enum Operator {
    Add,
    Sub,

    Mul,
    Div,
}

/// Simple binary operation (e.g., x + y)
pub struct BinaryOp {
    pub op: Operator,

    // Left and right-hand side expression node indices
    pub lhs: Option<NodeIndex>,
    pub rhs: Option<NodeIndex>,
}

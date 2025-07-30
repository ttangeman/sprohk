//! Note: operators are not considered separate nodes.
//! They are defined and used inside of the expression parsing.

use crate::nodes::NodeIndex;

#[derive(Debug, Copy, Clone)]
pub enum OpKind {
    Add,
    Sub,

    Mul,
    Div,
}

impl OpKind {
    pub fn as_str(self) -> &'static str {
        match self {
            OpKind::Add => "+",
            OpKind::Sub => "-",
            OpKind::Mul => "*",
            OpKind::Div => "/",
        }
    }
}

/// Simple binary operation (e.g., x + y)
#[derive(Debug)]
pub struct BinaryOp {
    pub kind: OpKind,

    // Left and right-hand side expression node indices
    pub lhs: NodeIndex,
    pub rhs: NodeIndex,
}

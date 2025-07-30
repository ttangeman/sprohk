//! Note: operators are not considered separate nodes.
//! They are defined and used inside of the expression parsing.

use sprohk_lexer::TokenKind;

use crate::nodes::NodeIndex;

#[derive(Debug, Copy, Clone)]
pub enum OpKind {
    Add,
    Sub,
    Mul,
    Div,

    Eq, // ==
    Lt, // <
    Gt, // >
}

impl OpKind {
    pub fn from_token_kind(kind: TokenKind) -> Option<Self> {
        match kind {
            TokenKind::Plus => Some(OpKind::Add),
            TokenKind::Minus => Some(OpKind::Sub),
            TokenKind::Star => Some(OpKind::Mul),
            TokenKind::Slash => Some(OpKind::Div),
            TokenKind::EqEq => Some(OpKind::Eq),
            TokenKind::Less => Some(OpKind::Lt),
            TokenKind::Greater => Some(OpKind::Gt),
            _ => None,
        }
    }

    pub fn as_str(self) -> &'static str {
        match self {
            OpKind::Add => "+",
            OpKind::Sub => "-",
            OpKind::Mul => "*",
            OpKind::Div => "/",
            OpKind::Eq => "==",
            OpKind::Lt => "<",
            OpKind::Gt => ">",
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

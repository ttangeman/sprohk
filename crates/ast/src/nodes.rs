use crate::ast::TokenIndex;
use crate::node_data::DataIndex;

use sprohk_lexer::TokenKind;

#[repr(u32)]
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum NodeKind {
    VarDecl,
    TypeExpr,
}

pub type NodeIndex = u32;

#[derive(Debug, Copy, Clone)]
pub struct Node {
    // The kind of the node, e.g., variable, function, etc.
    pub kind: NodeKind,
    // The index to the derived node data.
    pub data_index: DataIndex,
}

#[derive(Debug)]
pub struct VarDecl {
    // The specifier token, e.g., `let`, `const`, `var`.
    // This is used to determine the variable's scope and mutability.
    pub specifier: TokenKind,
    // The index of the variable name in the source code.
    pub name: TokenIndex,

    // The index of the type specification expr, if any.
    pub type_spec: Option<NodeIndex>,
    // The index of the initializer node, if any.
    pub initializer: Option<NodeIndex>,
}

#[derive(Debug)]
pub struct TypeExpr {
    // The index of the root token in the source code.
    // If this is a primitive type or identifier, it can be resolved to a string.
    // If this is a complex type, it may refer to a more complex structure.
    pub root: TokenIndex,
}

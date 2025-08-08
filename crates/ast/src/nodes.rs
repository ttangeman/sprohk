pub mod exprs;
pub mod ops;
pub mod statements;

pub use exprs::*;
pub use ops::*;
pub use statements::*;

use crate::ast::TokenIndex;
use crate::node_data::DataIndex;
use crate::{ParameterSpan, StatementSpan};

use sprohk_lexer::TokenKind;
use std::fmt::Display;

#[repr(u32)]
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum NodeKind {
    Block,

    VarDecl,
    ValueExpr,
    TypeExpr,

    Function,
    FnPrototype,
    FnParameter,

    IfStmt,
    AssignStmt,
}

#[derive(Clone, Copy, Debug)]
pub struct NodeIndex(pub u32);

impl NodeIndex {
    pub fn as_usize(self) -> usize {
        self.0 as usize
    }
}

impl Display for NodeIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl From<u32> for NodeIndex {
    fn from(value: u32) -> Self {
        NodeIndex(value)
    }
}

impl From<usize> for NodeIndex {
    fn from(value: usize) -> Self {
        NodeIndex(value as u32)
    }
}

/// Base node for all AST node types. Can be resolved through
/// the combination of the `data_index` and `kind`.
#[derive(Debug, Copy, Clone)]
pub struct Node {
    // The kind of the node, e.g., variable, function, etc.
    pub kind: NodeKind,
    // The index to the derived node data.
    pub data_index: DataIndex,
}

/// Generic node representing a block of code for a function,
/// statement, or creating a new scope. The block derives a list of
/// statements that can be provided semantic meaning through the node
/// that associates it as a child node.
#[derive(Debug)]
pub struct Block {
    // Span of statement indices in the block
    pub statements: StatementSpan,
}

/// Variable declaration is a statement with an optional type specifier
/// and assignment expression. Some examples:
///
/// `var x;`
/// `var y: i32;`
/// `var z: i32 = 42;`
#[derive(Debug)]
pub struct VarDecl {
    // The specifier token, e.g., `let`, `const`, `var`.
    // This is used to determine the variable's scope and mutability.
    pub specifier: TokenKind,
    // The index of the variable name in the source code.
    //
    // TODO: discriminated union with TokenIndex or interned string (i.e.,
    // the string is lazily interned upon use?)
    pub name: TokenIndex,

    // The index of the type specification expr, if any.
    pub type_expr: Option<NodeIndex>,
    // The index of the assignment expr, if any.
    pub assign_expr: Option<NodeIndex>,
}

/// Base function node which stores all information relating
/// to the function (i.e. prototype and block)
#[derive(Debug)]
pub struct Function {
    // Function prototype index
    pub prototype: NodeIndex,
    // Optional `Block` index
    pub block: Option<NodeIndex>,
}

/// Function prototype is all of the expressions and metadata associated
/// with the call definition for a function declaration.
#[derive(Debug)]
pub struct FnPrototype {
    // The index of the function name in the source code.
    pub name: TokenIndex,
    // Index to the optional return `TypeExpr`
    pub ret_type_expr: Option<NodeIndex>,

    // Indices to each `FunctionParameter` in the argument list.
    pub parameters: Option<ParameterSpan>,
}

/// Function parameter for a function prototype
#[derive(Debug)]
pub struct FnParameter {
    // The index of the variable name in the source code.
    pub name: TokenIndex,

    // Index to the non-optional type expr node
    pub type_expr: NodeIndex,
}

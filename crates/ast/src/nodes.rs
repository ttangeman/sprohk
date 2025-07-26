use crate::ast::TokenIndex;
use crate::node_data::DataIndex;

use sprohk_lexer::TokenKind;

#[repr(u32)]
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum NodeKind {
    VarDecl,
    TypeExpr,
    AssignExpr,
}

pub type NodeIndex = u32;

/// Base node for all AST node types. Can be resolved through
/// the combination of the `data_index` and `kind`.
#[derive(Debug, Copy, Clone)]
pub struct Node {
    // The kind of the node, e.g., variable, function, etc.
    pub kind: NodeKind,
    // The index to the derived node data.
    pub data_index: DataIndex,
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
    pub name: TokenIndex,

    // The index of the type specification expr, if any.
    pub type_expr: Option<NodeIndex>,
    // The index of the assignment expr, if any.
    pub assign_expr: Option<NodeIndex>,
}

/// Represents the possible type expressions linked to a variable declaration
/// or return statement.
#[derive(Debug)]
pub enum TypeExpr {
    /// A simple type expression that refers to a primitive type.
    Primitive(TokenIndex),
    /// A type expression that refers to a non-ambiguous (i.e. no compile
    /// time expression or procedure) type name.
    TypeName {
        name: TokenIndex,
        // TODO: specifier, references, etc.
    }
    // TODO: More complicated compile time expressions
}

/// Special case assignment expression for variable declarations.
#[derive(Debug)]
pub enum AssignExpr {
    Variable(TokenIndex),
    Literal(TokenIndex),
}

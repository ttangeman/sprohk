use crate::ast::TokenIndex;
use crate::node_data::DataIndex;

use smallvec::SmallVec;
use sprohk_lexer::TokenKind;

#[repr(u32)]
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum NodeKind {
    VarDecl,
    TypeExpr,
    AssignExpr,

    FnPrototype,
    FnParameter,
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
    //
    // TODO: discriminated union with TokenIndex or interned string (i.e.,
    // the string is lazily interned upon use?)
    pub name: TokenIndex,

    // The index of the type specification expr, if any.
    pub type_expr: Option<NodeIndex>,
    // The index of the assignment expr, if any.
    pub assign_expr: Option<NodeIndex>,
}

/// Represents a type expression for assigning a type, which is
/// either statically or dynamically resolved potentially, to a
/// typed declaration (e.g., a variable declaration, return type, or
/// function argument).
#[derive(Debug)]
pub enum TypeExpr {
    /// A simple type expression that refers to a primitive type.
    Primitive(TokenIndex),
    /// A type expression that refers to a non-ambiguous (i.e. no compile
    /// time expression or procedure) type name.
    TypeName {
        name: TokenIndex,
        // TODO: specifier, references, etc.
    }, // TODO: More complicated compile time expressions
}

/// Special case assignment expression for variable declarations.
#[derive(Debug)]
pub enum AssignExpr {
    // Simple variable assignment with token index to variable name
    Variable(TokenIndex),
    // Simple literal assignment with token index to literal value
    Literal(TokenIndex),
}

/// Function prototype is all of the expressions and metadata associated
/// with the call definition for a function declaration.
pub struct FnPrototype {
    // The index of the function name in the source code.
    pub name: TokenIndex,
    // Index to the optional return `TypeExpr`
    pub ret_type_expr: Option<NodeIndex>,

    // Indices to each `FunctionParameter` in the argument list.
    // Uses SBO for at least 8 parameters, as it is uncommon for
    // functions to exceed that.
    pub parameters: SmallVec<[NodeIndex; 8]>,
}

/// Function parameter for a function prototype
pub struct FnParameter {
    // The index of the variable name in the source code.
    pub name: TokenIndex,

    // Index to the non-optional type expr node
    pub type_expr: NodeIndex,
}

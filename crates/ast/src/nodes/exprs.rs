use crate::{BinaryOp, ParameterSpan, TokenIndex, UnaryOp};

/// Any expression that might yield a runtime value.
#[derive(Debug)]
pub enum ValueExpr {
    /// Simple literal value
    Literal(TokenIndex),
    /// Simple variable reference
    Variable(TokenIndex),
    /// Function call expr
    Function(FnCallExpr),

    /// Binary operator (infix)
    BinaryOp(BinaryOp),
    /// Unary operator (prefix)
    UnaryOp(UnaryOp),
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

/// Function call expression. Not directly stored on Ast; used as
/// part of the `ValueExpr` node type as it has value expression
/// semantics and can be parsed as part of a larger expression.
#[derive(Debug)]
pub struct FnCallExpr {
    /// Token index to function name invocation
    pub name: TokenIndex,
    /// Span of function parameter expression node indices.
    pub parameters: ParameterSpan,
}

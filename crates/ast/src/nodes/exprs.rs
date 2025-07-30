use crate::{BinaryOp, TokenIndex};

/// Any expression that might yield a runtime value.
#[derive(Debug)]
pub enum ValueExpr {
    /// Simple literal value
    Literal(TokenIndex),
    /// Simple variable reference
    Variable(TokenIndex),

    /// Binary operator
    BinaryOp(BinaryOp),
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

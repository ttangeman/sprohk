use crate::ast::TokenIndex;

use sprohk_lexer::TokenKind;

#[repr(u32)]
#[derive(Debug)]
pub enum NodeKind {
    VarDecl,
}

pub type DataIndex = u32;

#[derive(Debug)]
pub struct Node {
    // The kind of the node, e.g., variable, function, etc.
    pub kind: NodeKind,
    // The index to the node data.
    pub data_index: DataIndex,
}

#[derive(Debug)]
pub struct VarDecl {
    // The specifier token, e.g., `let`, `const`, `var`.
    // This is used to determine the variable's scope and mutability.
    pub specifier: TokenKind,
    // The index of the variable name in the token stream.
    pub name: TokenIndex,
    // The index of the type specification token, if any.
    pub type_spec: Option<TokenIndex>,
}

/// Stores metadata for AST nodes.
/// Note that it does not use the arena allocator like lexical tokens or
/// AST information, due to the higher density of data that is encoded.
#[derive(Debug)]
pub struct NodeData {
    var_decls: Vec<VarDecl>,
}

impl NodeData {
    pub fn new() -> NodeData {
        NodeData {
            var_decls: Vec::new(),
        }
    }

    pub fn add_variable(&mut self, name: String) {
        self.var_decls.push(VarDecl { name });
    }
}


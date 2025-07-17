use crate::ast::TokenIndex;

use sprohk_lexer::TokenKind;

#[repr(u32)]
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum NodeKind {
    VarDecl,
}

pub type DataIndex = u32;

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

// Note on getters: runtime checking of the `NodeKind` is maintained for the node data to
// avoid easy mistakes when accessing the data. Single accesses are considered the slow
// path by design, as the node data `Vec`s are intended to be used for bulk operations,
// so getters are mostly provided for convenience in tests or debugging.
impl NodeData {
    pub fn new() -> NodeData {
        NodeData {
            var_decls: Vec::new(),
        }
    }

    pub fn add_var_decl(&mut self, decl: VarDecl) -> DataIndex {
        let index = self.var_decls.len() as DataIndex;
        self.var_decls.push(decl);
        index
    }

    pub fn get_var_decl(&self, node: Node) -> &VarDecl {
        assert_eq!(node.kind, NodeKind::VarDecl);
        &self.var_decls[node.data_index as usize]
    }
}

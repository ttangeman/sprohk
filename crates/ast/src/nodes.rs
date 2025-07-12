#[repr(u32)]
pub enum NodeKind {
    Variable,
}

pub type DataIndex = u32;

pub struct Node {
    // The kind of the node, e.g., variable, function, etc.
    pub kind: NodeKind,
    // The index to the node data.
    pub data_index: DataIndex,
}

pub struct VarDecl {
    // TODO: interning
    pub name: String,
}

/// Stores metadata for AST nodes.
/// Note that it does not use the arena allocator like lexical tokens or
/// AST information, due to the higher density of data that is encoded.
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


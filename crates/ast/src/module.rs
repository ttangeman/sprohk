use crate::NodeIndex;

use sprohk_core::SourceFile;

/// The main compilation unit of a source file.
/// Maintains the list of root nodes for AST traversal.
pub struct Module {
    source: SourceFile,

    root_nodes: Vec<NodeIndex>,
}

impl Module {
    pub fn new(source: SourceFile) -> Self {
        Self {
            source,
            root_nodes: Vec::new(),
        }
    }

    pub fn source_text(&self) -> &str {
        &self.source.source()
    }

    pub fn add_root_node(&mut self, node_index: NodeIndex) {
        self.root_nodes.push(node_index);
    }
}

use crate::nodes::*;

pub type DataIndex = u32;

/// Stores metadata for AST nodes.
/// Note that it does not use the arena allocator like lexical tokens or
/// AST information, due to the higher density of data that is encoded.
#[derive(Debug)]
pub struct NodeData {
    var_decls: Vec<VarDecl>,
    type_exprs: Vec<TypeExpr>,
    assign_exprs: Vec<AssignExpr>,
}

// Note on getters: runtime checking of the `NodeKind` is maintained for the node data to
// avoid easy mistakes when accessing the data. Single accesses are considered the slow
// path by design, as the node data `Vec`s are intended to be used for bulk operations,
// so getters are mostly provided for convenience in tests or debugging.
impl NodeData {
    pub fn new() -> NodeData {
        NodeData {
            var_decls: Vec::new(),
            type_exprs: Vec::new(),
            assign_exprs: Vec::new(),
        }
    }

    pub fn add_var_decl(&mut self, decl: VarDecl) -> DataIndex {
        let index = self.var_decls.len() as DataIndex;
        self.var_decls.push(decl);
        index
    }

    pub fn add_type_expr(&mut self, type_expr: TypeExpr) -> DataIndex {
        let index = self.type_exprs.len() as DataIndex;
        self.type_exprs.push(type_expr);
        index
    }

    pub fn add_assign_expr(&mut self, assign_expr: AssignExpr) -> DataIndex {
        let index = self.assign_exprs.len() as DataIndex;
        self.assign_exprs.push(assign_expr);
        index
    }

    pub fn get_var_decl(&self, node: Node) -> &VarDecl {
        assert_eq!(node.kind, NodeKind::VarDecl);
        &self.var_decls[node.data_index as usize]
    }

    pub fn get_type_expr(&self, node: Node) -> &TypeExpr {
        assert_eq!(node.kind, NodeKind::TypeExpr);
        &self.type_exprs[node.data_index as usize]
    }

    pub fn get_assign_expr(&self, node: Node) -> &AssignExpr {
        assert_eq!(node.kind, NodeKind::AssignExpr);
        &self.assign_exprs[node.data_index as usize]
    }
}

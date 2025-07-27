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
    functions: Vec<Function>,
    fn_protos: Vec<FnPrototype>,
    fn_params: Vec<FnParameter>,
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
            functions: Vec::new(),
            fn_protos: Vec::new(),
            fn_params: Vec::new(),
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

    pub fn add_function(&mut self, func: Function) -> DataIndex {
        let index = self.functions.len() as DataIndex;
        self.functions.push(func);
        index
    }

    pub fn add_fn_prototype(&mut self, fn_proto: FnPrototype) -> DataIndex {
        let index = self.fn_protos.len() as DataIndex;
        self.fn_protos.push(fn_proto);
        index
    }

    pub fn add_fn_parameter(&mut self, fn_param: FnParameter) -> DataIndex {
        let index = self.fn_params.len() as DataIndex;
        self.fn_params.push(fn_param);
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

    pub fn get_function(&self, node: Node) -> &Function {
        assert_eq!(node.kind, NodeKind::Function);
        &self.functions[node.data_index as usize]
    }

    pub fn get_fn_prototype(&self, node: Node) -> &FnPrototype {
        assert_eq!(node.kind, NodeKind::FnPrototype);
        &self.fn_protos[node.data_index as usize]
    }

    pub fn get_fn_parameter(&self, node: Node) -> &FnParameter {
        assert_eq!(node.kind, NodeKind::FnParameter);
        &self.fn_params[node.data_index as usize]
    }
}

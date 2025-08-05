use crate::nodes::*;

use bumpalo::{Bump, collections::Vec as BumpVec};
use sprohk_core::Span;

pub type DataIndex = u32;

#[derive(Clone, Copy, Debug)]
pub struct StatementSpan(Span);

#[derive(Clone, Copy, Debug)]
pub struct ParameterSpan(Span);

/// Stores metadata for AST nodes as a structure of arrays (arenas).
/// This allows for efficient iteration of specfic node data types for
/// later compiler passes.
#[derive(Debug)]
pub struct NodeData<'arena> {
    blocks: BumpVec<'arena, Block>,
    var_decls: BumpVec<'arena, VarDecl>,

    value_exprs: BumpVec<'arena, ValueExpr>,
    type_exprs: BumpVec<'arena, TypeExpr>,

    functions: BumpVec<'arena, Function>,
    fn_protos: BumpVec<'arena, FnPrototype>,
    fn_params: BumpVec<'arena, FnParameter>,

    // Statement arena; used to store ranges of indices for blocks.
    statements: BumpVec<'arena, NodeIndex>,
    // Function parameters map into a span of node indices
    parameters: BumpVec<'arena, NodeIndex>,
}

// Note on getters: runtime checking of the `NodeKind` is maintained for the node data to
// avoid easy mistakes when accessing the data. Single accesses are considered the slow
// path by design, as the node data `Vec`s are intended to be used for bulk operations,
// so getters are mostly provided for convenience in tests or debugging.
impl<'arena> NodeData<'arena> {
    pub fn new(arena: &'arena Bump) -> Self {
        Self {
            blocks: BumpVec::new_in(arena),
            var_decls: BumpVec::new_in(arena),
            value_exprs: BumpVec::new_in(arena),
            type_exprs: BumpVec::new_in(arena),
            functions: BumpVec::new_in(arena),
            fn_protos: BumpVec::new_in(arena),
            fn_params: BumpVec::new_in(arena),
            statements: BumpVec::new_in(arena),
            parameters: BumpVec::new_in(arena),
        }
    }
    
    /// Reserves space in the node data arena using an estimate of the number
    /// of nodes to be populated in the AST.
    pub fn reserve_node_data(&mut self, nodes_estimate: usize) {
        // Rough guess of distribution of AST node types
        let expr_count = nodes_estimate / 2; // min 512
        let block_count = expr_count / 4; // min 128
        let function_count = block_count / 4; // min 32
        let var_count = block_count / 2; // min 64
        let param_count = function_count * 2; // min 64

        self.value_exprs.reserve(expr_count);
        self.blocks.reserve(block_count);
        self.functions.reserve(function_count);
        self.fn_protos.reserve(function_count);
        self.fn_params.reserve(param_count);
        self.var_decls.reserve(var_count);
        self.type_exprs.reserve(var_count / 2);

        self.statements.reserve(block_count);
        self.parameters.reserve(param_count)
    }

    pub fn add_block(&mut self, block: Block) -> DataIndex {
        let index = self.blocks.len() as DataIndex;
        self.blocks.push(block);
        index
    }

    pub fn add_var_decl(&mut self, decl: VarDecl) -> DataIndex {
        let index = self.var_decls.len() as DataIndex;
        self.var_decls.push(decl);
        index
    }

    pub fn add_value_expr(&mut self, expr: ValueExpr) -> DataIndex {
        let index = self.value_exprs.len() as DataIndex;
        self.value_exprs.push(expr);
        index
    }

    pub fn add_type_expr(&mut self, type_expr: TypeExpr) -> DataIndex {
        let index = self.type_exprs.len() as DataIndex;
        self.type_exprs.push(type_expr);
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

    /// Extends the global statement list with the incoming contiguous range of items
    pub fn push_statements(&mut self, stmts: impl IntoIterator<Item = NodeIndex>) -> StatementSpan {
        let start = self.statements.len();
        self.statements.extend(stmts);
        StatementSpan(Span {
            start,
            end: self.statements.len(),
        })
    }

    /// Extends the global parameter list with the incoming contiguous range of items
    pub fn push_parameters(&mut self, params: impl IntoIterator<Item = NodeIndex>) -> ParameterSpan {
        let start = self.parameters.len();
        self.parameters.extend(params);
        ParameterSpan(Span {
            start,
            end: self.parameters.len(),
        })
    }

    pub fn stmt_slice(&self, span: StatementSpan) -> &[NodeIndex] {
        &self.statements[span.0.start..span.0.end]
    }

    pub fn param_slice(&self, span: ParameterSpan) -> &[NodeIndex] {
        &&self.parameters[span.0.start..span.0.end]
    }

    pub fn get_block(&self, node: Node) -> &Block {
        assert_eq!(node.kind, NodeKind::Block);
        &self.blocks[node.data_index as usize]
    }

    pub fn get_var_decl(&self, node: Node) -> &VarDecl {
        assert_eq!(node.kind, NodeKind::VarDecl);
        &self.var_decls[node.data_index as usize]
    }

    pub fn get_value_expr(&self, node: Node) -> &ValueExpr {
        assert_eq!(node.kind, NodeKind::ValueExpr);
        &self.value_exprs[node.data_index as usize]
    }

    pub fn get_type_expr(&self, node: Node) -> &TypeExpr {
        assert_eq!(node.kind, NodeKind::TypeExpr);
        &self.type_exprs[node.data_index as usize]
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

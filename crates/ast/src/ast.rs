use crate::{module::Module, node_data::*, nodes::*};

use bumpalo::{Bump, collections::Vec as BumpVec};
use sprohk_core::{SourceFile, Span};
use sprohk_lexer::{Token, TokenKind};
use std::collections::HashMap;

/// Represents the index of a token in the AST.
/// Maps to primarily the token kind and its source location as
/// they use uniform memory layout.
#[derive(Clone, Copy, Debug)]
pub struct TokenIndex(pub usize);

/// Provides the AST (Abstract Syntax Tree) for the input source code.
/// Uses arena allocation for efficient memory management of AST nodes and data.
pub struct Ast<'a> {
    // The arena used for allocating memory for the AST.
    arena: &'a Bump,

    // Modules for the program; mapped to the `SourceFile::file_hash()`
    modules: HashMap<u64, Module>,

    // Token list for all modules
    tokens: BumpVec<'a, Token>,

    // The kind of each node in the AST.
    nodes: BumpVec<'a, Node>,
    // Manages the metadata for each node in the AST.
    node_data: NodeData<'a>,
    // The data associated with each node, such as variable indices or other metadata.
    // Span of tokens the node covers (indices).
    // Note that the end of the range is exclusive.
    node_spans: BumpVec<'a, Span>,
}

impl<'a> Ast<'a> {
    /// Creates a new instance of the AST with the arena
    pub fn new(arena: &'a Bump) -> Self {
        Ast {
            arena,
            modules: HashMap::new(),
            tokens: BumpVec::new_in(arena),
            nodes: BumpVec::new_in(arena),
            node_data: NodeData::new(arena),
            node_spans: BumpVec::new_in(arena),
        }
    }

    /// Adds a module given its source file
    pub fn add_module(&mut self, source: SourceFile) {
        self.modules.insert(source.file_hash(), Module::new(source));
    }

    /// Adds a node to the module root list for traversal
    pub fn add_to_module_root(&mut self, token: Token, node_index: NodeIndex) {
        let module = self.modules.get_mut(&token.loc.source_hash).unwrap();
        module.add_root_node(node_index);
    }

    /// Adds lexical tokens to the module. Note that this
    /// does not perform any semantic analysis or validation.
    pub fn add_token(&mut self, token: Token) {
        self.tokens.push(token);
    }

    /// Reserves space for the specified number of tokens in the AST.
    pub fn reserve_tokens(&mut self, count: usize) {
        // Reserve space for the tokens and their locations.
        self.tokens.reserve(count);
    }

    /// Adds a node to the AST with the specified kind and span.
    /// The `add_data` function is used to add data to the node's metadata.
    pub fn add_node_with_data<F>(&mut self, kind: NodeKind, span: Span, add_data: F) -> NodeIndex
    where
        F: FnOnce(&mut NodeData) -> DataIndex,
    {
        let data_index = add_data(&mut self.node_data);
        let node_index = NodeIndex::from(self.nodes.len());

        self.nodes.push(Node { kind, data_index });
        self.node_spans.push(span);

        node_index
    }

    /// Pushes a contiguous range of parameters to the global parameter list
    pub fn push_parameters(
        &mut self,
        params: impl IntoIterator<Item = NodeIndex>,
    ) -> ParameterSpan {
        self.node_data.push_parameters(params)
    }

    /// Pushes a contiguous range of statements to the global statement list
    pub fn push_statements(&mut self, stmts: impl IntoIterator<Item = NodeIndex>) -> StatementSpan {
        self.node_data.push_statements(stmts)
    }

    /// Uses the allocated token count to reserve space for nodes.
    /// Typically called before adding nodes to the AST and after
    /// adding all tokens.
    pub fn reserve_nodes(&mut self) {
        // Guess that each node corresponds to, on average, 2 tokens,
        // and allocating a minimum of 1024 nodes.
        let node_count = std::cmp::max(self.tokens.len() / 2, 1024);

        self.nodes.reserve(node_count);
        self.node_spans.reserve(node_count);
        self.node_data.reserve_node_data(node_count);
    }

    pub fn arena(&self) -> &'a Bump {
        self.arena
    }

    pub fn nodes(&self) -> &[Node] {
        &self.nodes
    }

    pub fn node_data(&self) -> &NodeData {
        &self.node_data
    }

    pub fn tokens(&self) -> &[Token] {
        &self.tokens
    }

    /// Retrieves the token
    pub fn get_token(&self, index: TokenIndex) -> Option<Token> {
        self.tokens.get(index.0).cloned()
    }

    /// Retrieves the token kind
    pub fn get_token_kind(&self, index: TokenIndex) -> Option<TokenKind> {
        self.get_token(index).map(|t| t.kind)
    }

    /// Lookups the module from a token's source location
    pub fn get_module(&self, token: &Token) -> Option<&Module> {
        self.modules.get(&token.loc.source_hash)
    }

    /// Retrieves the slice of source code for the given token index.
    pub fn get_src(&self, index: TokenIndex) -> Option<&str> {
        if let Some(src) = self.tokens.get(index.0).and_then(|tok| {
            let module = self.get_module(tok).unwrap();
            module.source_text().get(tok.loc.start..tok.loc.end)
        }) {
            Some(src)
        } else {
            None
        }
    }

    /// Outputs the AST nodes in a human-readable format.
    /// This is primarily used for debugging and snapshot testing.
    pub fn render_ast(&self) -> String {
        let mut out = String::new();
        for (i, node) in self.nodes.iter().enumerate() {
            let kind = format!("{:?}", node.kind);
            let span = self.node_spans.get(i);
            let token_indices = match span {
                Some(s) => format!("[{}..{}]", s.start, s.end),
                None => "[?]".to_string(),
            };
            out.push_str(&format!("node {} {{\n", i));
            out.push_str(&format!("  kind: {}\n", kind));
            out.push_str(&format!("  tokens: {}\n", token_indices));
            match node.kind {
                NodeKind::Block => {
                    let block = self.node_data.get_block(*node);
                    out.push_str("  data: {\n");

                    let statements = self.node_data.stmt_slice(block.statements);
                    if !statements.is_empty() {
                        out.push_str("    statements: [\n");
                        for (i, stmt_index) in statements.iter().enumerate() {
                            out.push_str(&format!("      {}", stmt_index));
                            if i < statements.len() - 1 {
                                out.push_str(&format!(",\n"));
                            }
                        }
                        out.push_str("\n    ]\n");
                    }

                    out.push_str("  }\n");
                }
                NodeKind::VarDecl => {
                    let var_decl = self.node_data.get_var_decl(*node);
                    let name_str = self.get_src(var_decl.name).unwrap_or("");
                    out.push_str("  data: {\n");
                    out.push_str(&format!("    specifier: {:?}\n", var_decl.specifier));
                    out.push_str(&format!("    name: '{}'\n", name_str));
                    if let Some(type_index) = var_decl.type_expr {
                        out.push_str(&format!("    type_expr: {}\n", type_index));
                    }
                    if let Some(assign_index) = var_decl.assign_expr {
                        out.push_str(&format!("    assign_expr: {}\n", assign_index));
                    }
                    out.push_str("  }\n");
                }
                NodeKind::ValueExpr => {
                    let expr = self.node_data.get_value_expr(*node);
                    out.push_str("  data: {\n");
                    match expr {
                        ValueExpr::Variable(index) => {
                            let var_str = self.get_src(*index).unwrap_or("");
                            out.push_str(&format!("    variable: '{}'\n", var_str));
                        }
                        ValueExpr::Literal(index) => {
                            let lit_str = self.get_src(*index).unwrap_or("");
                            out.push_str(&format!("    literal: '{}'\n", lit_str));
                        }
                        ValueExpr::Function(fn_call) => {
                            let name_str = self.get_src(fn_call.name).unwrap_or("");
                            out.push_str(&format!("    function: '{}'\n", name_str));

                            let params = self.node_data.param_slice(fn_call.parameters);
                            if !params.is_empty() {
                                out.push_str(&format!("    params: [\n"));
                                for (i, param_index) in params.iter().enumerate() {
                                    out.push_str(&format!("      {}", param_index));
                                    if i < params.len() - 1 {
                                        out.push_str(&format!(",\n"));
                                    }
                                }
                                out.push_str(&format!("\n    ]\n"));
                            }
                        }
                        ValueExpr::BinaryOp(op) => {
                            let op_str = op.kind.as_str();
                            out.push_str(&format!("    op: '{}'\n", op_str));
                            out.push_str(&format!("    lhs: {}\n", op.lhs));
                            out.push_str(&format!("    rhs: {}\n", op.rhs));
                        }
                        ValueExpr::UnaryOp(op) => {
                            let op_str = op.kind.as_str();
                            out.push_str(&format!("    op: '{}'\n", op_str));
                            out.push_str(&format!("    rhs: {}\n", op.rhs));
                        }
                    }
                    out.push_str("  }\n");
                }
                NodeKind::TypeExpr => {
                    let type_expr = self.node_data.get_type_expr(*node);
                    match *type_expr {
                        TypeExpr::Primitive(index) | TypeExpr::TypeName { name: index } => {
                            let root_str = self.get_src(index).unwrap_or("");
                            out.push_str("  data: {\n");
                            out.push_str(&format!("    root: '{}'\n", root_str));
                            out.push_str("  }\n");
                        }
                    }
                }
                NodeKind::Function => {
                    let func = self.node_data.get_function(*node);
                    out.push_str("  data: {\n");
                    out.push_str(&format!("    prototype: {}\n", func.prototype));
                    if let Some(block_index) = func.block {
                        out.push_str(&format!("    block: {}\n", block_index));
                    }
                    out.push_str("  }\n");
                }
                NodeKind::FnPrototype => {
                    let fn_proto = self.node_data.get_fn_prototype(*node);
                    let name_str = self.get_src(fn_proto.name).unwrap_or("");
                    out.push_str("  data: {\n");
                    out.push_str(&format!("    name: {}\n", name_str));
                    if let Some(type_index) = fn_proto.ret_type_expr {
                        out.push_str(&format!("    ret_type_expr: {}\n", type_index));
                    }

                    if let Some(span) = fn_proto.parameters {
                        let params = self.node_data.param_slice(span);
                        out.push_str(&format!("    params: [\n"));

                        for (i, param_index) in params.iter().enumerate() {
                            out.push_str(&format!("      {}", *param_index));
                            if i < params.len() - 1 {
                                out.push_str(&format!(",\n"));
                            }
                        }
                        out.push_str(&format!("\n    ]\n"));
                    }
                    out.push_str("  }\n");
                }
                NodeKind::FnParameter => {
                    let fn_param = self.node_data.get_fn_parameter(*node);
                    let name_str = self.get_src(fn_param.name).unwrap_or("");
                    let type_index = fn_param.type_expr;
                    out.push_str("  data: {\n");
                    out.push_str(&format!("    name: {}\n", name_str));
                    out.push_str(&format!("    type_expr: {}\n", type_index));
                    out.push_str("  }\n");
                }
                NodeKind::IfStmt => {
                    let if_stmt = self.node_data.get_if_stmt(*node);
                    out.push_str("  data: {\n");
                    out.push_str(&format!("    cond_expr: {}\n", if_stmt.condition_expr));
                    out.push_str(&format!("    then_block: {}\n", if_stmt.then_block));
                    if let Some(else_index) = if_stmt.else_node {
                        if self.nodes[else_index.as_usize()].kind == NodeKind::IfStmt {
                            out.push_str(&format!("    else_if: {}\n", else_index));
                        } else {
                            out.push_str(&format!("    else_block: {}\n", else_index));
                        }
                    }
                    out.push_str("  }\n");
                }
                NodeKind::AssignStmt => {
                    let assign_stmt = self.node_data.get_assign_stmt(*node);
                    out.push_str("  data: {\n");
                    out.push_str(&format!("    lhs: {}\n", assign_stmt.lhs_expr));
                    out.push_str(&format!("    rhs: {}\n", assign_stmt.rhs_expr));
                    out.push_str("  }\n");
                }

                #[allow(unreachable_patterns)]
                _ => {
                    out.push_str("  data: ?\n");
                }
            }
            out.push_str("}\n");
        }
        out
    }
}

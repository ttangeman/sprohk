use crate::nodes::*;

use bumpalo::{Bump, collections::Vec as BumpVec};
use sprohk_core::{SourceLocation, Span};
use sprohk_lexer::{Token, TokenKind};
use std::rc::Rc;

/// Represents the index of a token in the AST.
/// Maps to primarily the token kind and its source location as
/// they use uniform memory layout.
pub type TokenIndex = usize;

/// Provides the AST (Abstract Syntax Tree) for the input source code.
/// Uses arena allocation for efficient memory management of AST nodes and data.
pub struct Ast<'a> {
    // The arena used for allocating memory for the AST.
    arena: &'a Bump,
    // The original source code. Primarily used for backing out
    // identifiers, literals, etc. from the `SourceLocation`.
    source: Rc<String>,

    // The deconstructed lexical tokens of the source code.
    tokens: BumpVec<'a, TokenKind>,
    token_locs: BumpVec<'a, SourceLocation>,

    // The kind of each node in the AST.
    nodes: BumpVec<'a, Node>,
    // Manages the metadata for each node in the AST.
    node_data: NodeData,
    // The data associated with each node, such as variable indices or other metadata.
    // Span of tokens the node covers (indices).
    node_spans: BumpVec<'a, Span>,
}

impl<'a> Ast<'a> {
    /// Creates a new instance of the AST with the arena
    pub fn new(arena: &'a Bump, source: Rc<String>) -> Self {
        Ast {
            arena,
            source,
            tokens: BumpVec::new_in(arena),
            token_locs: BumpVec::new_in(arena),
            nodes: BumpVec::new_in(arena),
            node_data: NodeData::new(),
            node_spans: BumpVec::new_in(arena),
        }
    }

    /// Adds lexical tokens to the AST. Note that this
    /// does not perform any semantic analysis or validation.
    pub fn add_token(&mut self, token: Token) {
        self.tokens.push(token.kind);
        self.token_locs.push(token.loc);
    }

    /// Adds a node to the AST with the specified kind and span.
    /// The `add_data` function is used to add data to the node's metadata.
    pub fn add_node_with_data<F>(&mut self, kind: NodeKind, span: Span, add_data: F)
    where
        F: FnOnce(&mut NodeData) -> DataIndex,
    {
        let data_index = add_data(&mut self.node_data);

        self.nodes.push(Node { kind, data_index });
        self.node_spans.push(span);
    }

    /// Reserves space for the specified number of tokens in the AST.
    pub fn reserve_tokens(&mut self, count: usize) {
        // Reserve space for the tokens and their locations.
        self.tokens.reserve(count);
        self.token_locs.reserve(count);
    }

    /// Uses the allocated token count to reserve space for nodes.
    /// Typically called before adding nodes to the AST and after
    /// adding all tokens.
    pub fn reserve_nodes(&mut self) {
        // Guess that each node corresponds to, on average, 2 tokens.
        let count = std::cmp::max(self.tokens.len() / 2, 8);

        self.nodes.reserve(count);
        self.node_spans.reserve(count);
    }

    pub fn source(&self) -> &str {
        &self.source
    }

    pub fn tokens(&self) -> &[TokenKind] {
        &self.tokens
    }

    pub fn nodes(&self) -> &[Node] {
        &self.nodes
    }

    pub fn node_data(&self) -> &NodeData {
        &self.node_data
    }

    /// Retrieves the span of the node at the given index.
    pub fn get_token_kind(&self, index: usize) -> Option<TokenKind> {
        self.tokens.get(index).cloned()
    }

    /// Retrieves the source location of the token at the given index.
    pub fn get_src_loc(&self, index: usize) -> Option<SourceLocation> {
        self.token_locs.get(index).cloned()
    }

    /// Retrieves the slice of source code for the given token index.
    pub fn get_src(&self, index: usize) -> Option<&str> {
        if let Some(src) = self
            .token_locs
            .get(index)
            .and_then(|loc| self.source.get(loc.start..loc.end))
        {
            Some(src)
        } else {
            None
        }
    }
}

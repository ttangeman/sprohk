use crate::nodes::*;

use bumpalo::{Bump, collections::Vec as BumpVec};

use sprohk_core::{SourceLocation, Span};
use sprohk_lexer::{Token, TokenKind};

/// Represents the index of a token in the AST.
/// Maps to primarily the token kind and its source location as
/// they use uniform memory layout.
pub type TokenIndex = usize;

/// Provides the AST (Abstract Syntax Tree) for the input source code.
/// Uses arena allocation for efficient memory management of AST nodes and data.
pub struct Ast<'a> {
    // The arena used for allocating memory for the AST.
    arena: &'a Bump,

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
    pub fn new(arena: &'a Bump) -> Self {
        Ast {
            arena,
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

    pub fn reserve_tokens(&mut self, count: usize) {
        // Reserve space for the tokens and their locations.
        self.tokens.reserve(count);
        self.token_locs.reserve(count);
    }

    /// Returns the token kind at the specified index.
    pub fn get_token_kind(&self, index: usize) -> Option<TokenKind> {
        self.tokens.get(index).cloned()
    }

    pub fn get_src_loc(&self, index: usize) -> Option<SourceLocation> {
        self.token_locs.get(index).cloned()
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
}

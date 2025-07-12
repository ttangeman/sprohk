use crate::nodes::*;

use bumpalo::{Bump, collections::Vec as BumpVec};

use sprohk_core::{SourceLocation, Span};
use sprohk_lexer::{Token, TokenKind};

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
            tokens: BumpVec::with_capacity_in(4096, arena),
            token_locs: BumpVec::with_capacity_in(4096, arena),
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

    /// Uses the allocated token count to reserve space for nodes.
    /// Typically called before adding nodes to the AST and after
    /// adding all tokens.
    pub fn reserve_nodes(&mut self) {
        // Guess that each node corresponds to, on average, 8 tokens.
        // TODO: better guess
        let count = std::cmp::max(self.tokens.len() / 8, 8);

        self.nodes.reserve(count);
        self.node_spans.reserve(count);
    }
}

use bumpalo::{Bump, collections::Vec as BumpVec};

use sprohk_core::SourceLocation;
use sprohk_lexer::{Token, TokenKind};

/// Provides the AST (Abstract Syntax Tree) for the input source code.
/// Uses arena allocation for efficient memory management of AST nodes and data.
pub struct Ast<'a> {
    arena: &'a Bump,
    tokens: BumpVec<'a, TokenKind>,
    locs: BumpVec<'a, SourceLocation>,
}

impl<'a> Ast<'a> {
    /// Creates a new instance of the AST with the arena
    pub fn new(arena: &'a Bump) -> Self {
        Ast {
            arena,
            tokens: BumpVec::with_capacity_in(4096, arena),
            locs: BumpVec::with_capacity_in(4096, arena),
        }
    }

    /// Adds lexical tokens to the AST. Note that this
    /// does not perform any semantic analysis or validation.
    pub fn add_token(&mut self, token: Token) {
        self.tokens.push(token.kind);
        self.locs.push(token.loc);
    }
}

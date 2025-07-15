use bumpalo::Bump;

use sprohk_ast::{Ast, NodeKind, VarDecl};
use sprohk_core::Span;
use sprohk_lexer::{TokenKind, Tokenizer};

/// State machine for the parser:
/// Tracks the current position in the token stream and provides methods
/// for parsing different constructs and populating the AST.
pub struct Parser {
    cursor: usize,
}

#[derive(Debug)]
pub enum ParserError {
    ExpectedToken(TokenKind),
    UnexpectedToken(TokenKind),
    InvalidSyntax(String),
}

impl Parser {
    pub fn new() -> Self {
        Parser { cursor: 0 }
    }

    /// Returns the current position of the cursor in the token stream.
    pub fn at(&self) -> usize {
        self.cursor
    }

    /// Advances the cursor to the next token in the stream.
    pub fn advance(&mut self) {
        self.cursor += 1;
    }

    /// Checks if the current token matches the expected token kind.
    /// If it matches, it advances the cursor and returns the current index.
    pub fn expect(&mut self, ast: &Ast, expected: TokenKind) -> Result<usize, ParserError> {
        let current = self.at();
        if let Some(token) = ast.get_token_kind(current) {
            if token == expected {
                self.advance();
                Ok(current)
            } else {
                Err(ParserError::UnexpectedToken(token))
            }
        } else {
            Err(ParserError::ExpectedToken(expected))
        }
    }

    pub fn expect_any<F>(
        &mut self,
        ast: &Ast,
        expected: F,
    ) -> Result<usize, ParserError>
    where
        F: Fn(TokenKind) -> bool,
    {
        let current = self.at();
        if let Some(token) = ast.get_token_kind(current) {
            if expected(token) {
                self.advance();
                Ok(current)
            } else {
                Err(ParserError::UnexpectedToken(token))
            }
        } else {
            Err(ParserError::InvalidSyntax(format!(
                "Expected token at index {}, but found end of stream",
                current
            )))
        }
    }

    /// Conditionally accepts the current token if it matches the specified kind.
    pub fn accept(&mut self, ast: &Ast, kind: TokenKind) -> Option<usize> {
        let current = self.at();
        if let Some(token) = ast.get_token_kind(current) {
            if token == kind {
                self.advance();
                return Some(current);
            }
        }
        None
    }

    /// Parses a variable declaration from the current position in the token stream.
    /// Must start with a specifier token (e.g., `var`, `let`, `const`).
    pub fn parse_var_decl(
        &mut self,
        ast: &mut Ast,
        specifier: TokenKind,
    ) -> Result<(VarDecl, Span), ParserError> {
        assert!(matches!(
            specifier,
            TokenKind::Var | TokenKind::Let | TokenKind::Const
        ));

        let start = self.at();
        // Move past the specifier token
        self.advance();

        // Parse the identifier token
        let name_index = self.expect(ast, TokenKind::Identifier)?;
        let type_spec_index = if self.accept(ast, TokenKind::Colon).is_some() {
            // TODO: Can be a compile-time expression
            let index = self.expect_any(ast, |kind| {
                match kind {
                    TokenKind::Identifier => true,
                    k if k.is_primitive_type() => true,
                    _ => false,
                }
            })?;
            Some(index)
        } else {
            None
        };

        Ok((
            VarDecl {
                specifier,
                name: name_index,
                type_spec: type_spec_index,
            },
            Span {
                start,
                end: self.at(),
            },
        ))
    }
}

pub fn parse_ast<'a>(arena: &'a Bump, source: &str) -> Result<Ast<'a>, ParserError> {
    let mut ast = Ast::new(arena);
    // Reserve space for tokens based on the source length (best guess).
    ast.reserve_tokens(source.len() / 8);

    let mut tokenizer = Tokenizer::new(source);

    // Tokenize the source code and add tokens to the AST.
    while let Some(token) = tokenizer.next() {
        if token.kind == TokenKind::Invalid {
            return Err(ParserError::InvalidSyntax(format!(
                "Invalid token {:?} ({}:{})",
                // TODO: Compute column instead of using start
                token.kind,
                token.loc.line,
                token.loc.start
            )));
        }
        ast.add_token(token);
    }

    // Reserve space for nodes based on the number of tokens (best guess)
    ast.reserve_nodes();

    // Begin parsing the tokens into AST nodes.
    let mut parser = Parser::new();
    while let Some(token) = ast.get_token_kind(parser.at()) {
        match token {
            TokenKind::Var | TokenKind::Let | TokenKind::Const => {
                // Parse a variable declaration
                let (var_decl, span) = parser.parse_var_decl(&mut ast, token)?;
                // Add the variable declaration node to the AST
                ast.add_node_with_data(NodeKind::VarDecl, span, |node_data| {
                    node_data.add_var_decl(var_decl)
                });
            }
            _ => return Err(ParserError::UnexpectedToken(token)),
        }
    }

    Ok(ast)
}

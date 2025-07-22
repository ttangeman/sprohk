use sprohk_ast::{Ast, NodeIndex, NodeKind, TokenIndex, TypeExpr, VarDecl};
use sprohk_core::Span;
use sprohk_lexer::TokenKind;

#[derive(Debug)]
pub enum ParserError {
    ExpectedToken(TokenKind),
    UnexpectedToken(TokenKind),
    InvalidSyntax(String),
}

/// State machine for the parser:
/// Tracks the current position in the token stream and provides methods
/// for parsing different constructs and populating the AST.
pub struct Parser {
    cursor: TokenIndex,
}

impl Parser {
    pub fn new() -> Self {
        Parser { cursor: 0 }
    }

    /// Returns the current position of the cursor in the token stream.
    pub fn at(&self) -> TokenIndex {
        self.cursor
    }

    /// Advances the cursor to the next token in the stream.
    pub fn advance(&mut self) {
        self.cursor += 1;
    }

    pub fn span_from(&self, start: TokenIndex) -> Span {
        let end = self.at();
        Span { start, end }
    }

    /// Checks if the current token matches the expected token kind.
    /// If it matches, it advances the cursor and returns the current index.
    pub fn expect(&mut self, ast: &Ast, expected: TokenKind) -> Result<TokenIndex, ParserError> {
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

    pub fn expect_any<F>(&mut self, ast: &Ast, expected: F) -> Result<TokenIndex, ParserError>
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
    pub fn accept(&mut self, ast: &Ast, kind: TokenKind) -> Option<TokenIndex> {
        let current = self.at();
        if let Some(token) = ast.get_token_kind(current) {
            if token == kind {
                self.advance();
                return Some(current);
            }
        }
        None
    }

    /// Parses a type expression from the current position in the token stream.
    pub fn parse_type_expr(&mut self, ast: &mut Ast) -> Result<NodeIndex, ParserError> {
        let type_start = self.at();
        let type_index = self.expect_any(ast, |kind| {
            match kind {
                // Possible type name
                TokenKind::Identifier => true,
                // Primitive type
                k if k.is_primitive_type() => true,
                _ => false,
            }
        })?;

        Ok(ast.add_node_with_data(
            NodeKind::TypeExpr,
            self.span_from(type_start),
            |node_data| {
                let type_expr = TypeExpr { root: type_index };
                node_data.add_type_expr(type_expr)
            },
        ))
    }

    /// Parses a variable declaration from the current position in the token stream.
    /// Must start with a specifier token (e.g., `var`, `let`, `const`).
    pub fn parse_var_decl(
        &mut self,
        ast: &mut Ast,
        specifier: TokenKind,
    ) -> Result<NodeIndex, ParserError> {
        debug_assert!(matches!(
            specifier,
            TokenKind::Var | TokenKind::Let | TokenKind::Const
        ));

        let start = self.at();
        // Move past the specifier token
        self.advance();

        // Parse the identifier token
        let name = self.expect(ast, TokenKind::Identifier)?;
        // Parse the optional type specification
        let type_spec = if self.accept(ast, TokenKind::Colon).is_some() {
            Some(self.parse_type_expr(ast)?)
        } else {
            None
        };

        // TODO: Handle initializer expressions
        let initializer = None;

        Ok(
            ast.add_node_with_data(NodeKind::VarDecl, self.span_from(start), |node_data| {
                let var_decl = VarDecl {
                    specifier,
                    name,
                    type_spec,
                    initializer,
                };
                node_data.add_var_decl(var_decl)
            }),
        )
    }
}

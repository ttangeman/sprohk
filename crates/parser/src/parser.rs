use sprohk_ast::{AssignExpr, Ast, NodeIndex, NodeKind, TokenIndex, TypeExpr, VarDecl};
use sprohk_core::Span;
use sprohk_lexer::TokenKind;

#[derive(Debug)]
pub enum ParserError {
    ExpectedToken(TokenKind),
    UnexpectedToken(TokenKind),
    InvalidSyntax(String),
    UnexpectedEof,
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

    /// Returns the span given the start and current position of the cursor.
    /// The end of the span is exclusive.
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
            Err(ParserError::UnexpectedEof)
        }
    }

    /// Expects any token that matches the provided predicate.
    /// If it matches, it advances the cursor and returns the current index.
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
            Err(ParserError::UnexpectedEof)
        }
    }

    /// Expects any token that matches the provided predicate, allowing state mutation.
    /// If it matches, it advances the cursor and returns the current index.
    pub fn expect_any_mut<F>(
        &mut self,
        ast: &Ast,
        expected: &mut F,
    ) -> Result<TokenIndex, ParserError>
    where
        F: FnMut(TokenKind) -> bool,
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
            Err(ParserError::UnexpectedEof)
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
        let mut is_primitive = false;
        let type_index = self.expect_any_mut(ast, &mut |kind| {
            match kind {
                // Possible type name
                TokenKind::Identifier => true,
                // Primitive type
                k if k.is_primitive_type() => {
                    is_primitive = true;
                    true
                }
                _ => false,
            }
        })?;

        Ok(ast.add_node_with_data(
            NodeKind::TypeExpr,
            self.span_from(type_start),
            |node_data| {
                let type_expr = if is_primitive {
                    TypeExpr::Primitive(type_index)
                } else {
                    TypeExpr::TypeName {
                        name: type_index
                    }
                };
                node_data.add_type_expr(type_expr)
            },
        ))
    }

    /// Parses an expression in the context of an assignment operation.
    /// It is assumed that the equal sign (`=`) has already been consumed.
    pub fn parse_assign_expr(&mut self, ast: &mut Ast) -> Result<Option<NodeIndex>, ParserError> {
        let start = self.at();
        let mut root_node: Option<NodeIndex> = None;

        while let Some(token) = ast.get_token_kind(self.at()) {
            match token {
                // Parse the identifier as a variable name or function name.
                TokenKind::Identifier => {
                    let name_index = self.at();
                    self.advance();

                    if root_node.is_none() {
                        root_node = Some(ast.add_node_with_data(
                            NodeKind::AssignExpr,
                            self.span_from(start),
                            |node_data| {
                                let assign_expr = AssignExpr::Variable(name_index);
                                node_data.add_assign_expr(assign_expr)
                            },
                        ));
                    } else {
                        return Err(ParserError::InvalidSyntax(
                            "Unexpected identifier: multiple identifiers in assignment expression"
                                .to_string(),
                        ));
                    }

                    // TODO: accept function calls
                }

                // Parse a literal value (e.g., number, string).
                _ if token.is_literal() => {
                    let literal_index = self.at();
                    self.advance();

                    if root_node.is_none() {
                        root_node = Some(ast.add_node_with_data(
                            NodeKind::AssignExpr,
                            self.span_from(start),
                            |node_data| {
                                let assign_expr = AssignExpr::Literal(literal_index);
                                node_data.add_assign_expr(assign_expr)
                            },
                        ));
                    } else {
                        return Err(ParserError::InvalidSyntax(
                            "Unexpected literal: multiple literals in assignment expression"
                                .to_string(),
                        ));
                    }
                }

                // Terminates expression parsing.
                TokenKind::Semicolon => {
                    self.advance();

                    return root_node
                        .is_some()
                        .then(|| {
                            // If we have a root node, return it.
                            root_node
                        })
                        .ok_or(ParserError::InvalidSyntax(
                            "Expected an expression before semicolon".to_string(),
                        ));
                }

                TokenKind::Eof => return Err(ParserError::UnexpectedEof),
                _ => return Err(ParserError::UnexpectedToken(token)),
            }
        }

        Err(ParserError::UnexpectedEof)
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

        // Mark var decl span
        let span = self.span_from(start);

        // Parse the optional type specification
        let type_expr = if self.accept(ast, TokenKind::Colon).is_some() {
            Some(self.parse_type_expr(ast)?)
        } else {
            None
        };

        // Parse the optional initializer
        let assign_expr = if self.accept(ast, TokenKind::Eq).is_some() {
            self.parse_assign_expr(ast)?
        } else {
            // Expect terminating semicolon if no assignment.
            self.expect(ast, TokenKind::Semicolon)?;
            None
        };

        Ok(
            ast.add_node_with_data(NodeKind::VarDecl, span, |node_data| {
                let var_decl = VarDecl {
                    specifier,
                    name,
                    type_expr,
                    assign_expr,
                };
                node_data.add_var_decl(var_decl)
            }),
        )
    }
}

mod parser;

pub use parser::{Parser, ParserError};

use bumpalo::Bump;
use sprohk_ast::{Ast, NodeKind};
use sprohk_lexer::{TokenKind, Tokenizer};
use std::rc::Rc;

pub fn parse_ast<'a>(arena: &'a Bump, source: Rc<String>) -> Result<Ast<'a>, ParserError> {
    let mut ast = Ast::new(arena, source.clone());
    // Reserve space for tokens based on the source length (best guess).
    ast.reserve_tokens(ast.source().len() / 8);

    let mut tokenizer = Tokenizer::new(&source);

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

            TokenKind::Semicolon => {
                // Just consume the semicolon, no action needed for now
                parser.advance()
            }

            _ => return Err(ParserError::UnexpectedToken(token)),
        }
    }

    Ok(ast)
}

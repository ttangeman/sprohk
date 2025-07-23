mod parser;

pub use parser::{Parser, ParserError};

use bumpalo::Bump;
use sprohk_ast::Ast;
use sprohk_lexer::{TokenKind, Tokenizer};
use std::rc::Rc;

pub fn parse_ast<'a>(arena: &'a Bump, source: Rc<String>) -> Result<Ast<'a>, ParserError> {
    let mut ast = Ast::new(arena, source.clone());
    // Reserve space for tokens based on the source length (best guess).
    ast.reserve_tokens(ast.source().len() / 8);

    let mut tokenizer = Tokenizer::new(&source);

    // Tokenize the source code and add tokens to the AST.
    loop {
        let token = tokenizer.next();
        match token.kind {
            TokenKind::Invalid => {
                return Err(ParserError::InvalidSyntax(format!(
                    "Invalid token {:?} at {}:{}",
                    token.kind, token.loc.line, token.loc.start
                )));
            }
            TokenKind::Eof => {
                ast.add_token(token);
                break;
            }
            _ => ast.add_token(token),
        }
    }

    // Reserve space for nodes based on the number of tokens (best guess)
    ast.reserve_nodes();

    // Begin parsing the tokens into AST nodes.
    let mut parser = Parser::new();
    while let Some(token) = ast.get_token_kind(parser.at()) {
        match token {
            TokenKind::Var | TokenKind::Let | TokenKind::Const => {
                parser.parse_var_decl(&mut ast, token)?;
            }

            TokenKind::Eof => break,
            _ => return Err(ParserError::UnexpectedToken(token)),
        }
    }

    Ok(ast)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn check_parse_var_decl_errors() {
        let arena = Bump::new();
        let sources = vec![
            "var x = ;",
            "var x: i32",
            "var x: i32 = 42 42;",
            "var x: i32 = a b;",
        ];

        for source in sources {
            let result = parse_ast(&arena, source.to_string().into());

            assert!(result.is_err());
            assert!(matches!(
                result.err().unwrap(),
                ParserError::InvalidSyntax(_) | ParserError::UnexpectedToken(_)
            ));
        }
    }
}

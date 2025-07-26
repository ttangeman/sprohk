mod parser;

pub use parser::{Parser, ParserError};

use bumpalo::Bump;
use sprohk_ast::Ast;
use sprohk_core::SourceFile;
use sprohk_lexer::{TokenKind, Tokenizer};

pub fn parse_ast<'a>(arena: &'a Bump, sources: Vec<SourceFile>) -> Result<Ast<'a>, ParserError> {
    let mut ast = Ast::new(arena);
    let source_len: usize = sources.iter().map(|s| s.source().len()).sum();

    // Reserve space for tokens based on the source length (best guess).
    ast.reserve_tokens(source_len / 8);

    for source in sources {
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

        ast.add_module(source);
    }

    // Reserve space for nodes based on the number of tokens (best guess)
    ast.reserve_nodes();

    // Begin parsing the tokens into AST nodes from the root set of nodes.
    let mut parser = Parser::new();
    while let Some(token) = ast.get_token(parser.at()) {
        match token.kind {
            // Global variable
            TokenKind::Var | TokenKind::Let | TokenKind::Const => {
                let node_index = parser.parse_var_decl(&mut ast, token.kind)?;
                ast.add_to_module_root(token, node_index);
            }

            // Top-level function
            TokenKind::Fn => {
                let node_index = parser.parse_function(&mut ast)?;
                ast.add_to_module_root(token, node_index);
            }

            TokenKind::Eof => break,
            _ => return Err(ParserError::UnexpectedToken(token.kind)),
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
        ]
        .into_iter()
        .map(|s| SourceFile::from_raw_source(s.to_string()))
        .collect();

        let result = parse_ast(&arena, sources);

        assert!(result.is_err());
        assert!(matches!(
            result.err().unwrap(),
            ParserError::InvalidSyntax(_) | ParserError::UnexpectedToken(_)
        ));
    }
}

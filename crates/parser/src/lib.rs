use bumpalo::Bump;

use sprohk_ast::Ast;
use sprohk_lexer::{TokenKind, Tokenizer};

pub fn parse_ast<'a>(arena: &'a Bump, source: &str) -> Result<Ast<'a>, String> {
    let mut ast = Ast::new(arena);
    let mut tokenizer = Tokenizer::new(source);

    while let Some(token) = tokenizer.next() {
        if token.kind == TokenKind::Invalid {
            // TODO: better error
            return Err(format!(
                "Error at line {} ({})",
                token.loc.line, token.loc.start
            ));
        }
        ast.add_token(token);
    }

    Ok(ast)
}

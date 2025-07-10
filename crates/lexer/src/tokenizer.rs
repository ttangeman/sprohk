use phf::phf_map;
use sprohk_core::SourceLocation;

/// List of keywords for the tokenizer.
/// Uses a compile time perfect hash table for fast lookup.
pub const KEYWORDS: phf::Map<&'static [u8], TokenKind> = phf_map! {
    b"const" => TokenKind::Const,
    b"let" => TokenKind::Let,
    b"var" => TokenKind::Var,
    b"fn" => TokenKind::Fn,
    b"return" => TokenKind::Return,
    b"if" => TokenKind::If,
    b"else" => TokenKind::Else,
    b"and" => TokenKind::LogicalAnd,
    b"or" => TokenKind::LogicalOr,
    b"null" => TokenKind::Null,
    b"true" => TokenKind::True,
    b"false" => TokenKind::False,
    b"i8" => TokenKind::I8,
    b"i16" => TokenKind::I16,
    b"i32" => TokenKind::I32,
    b"i64" => TokenKind::I64,
    b"u8" => TokenKind::U8,
    b"u16" => TokenKind::U16,
    b"u32" => TokenKind::U32,
    b"u64" => TokenKind::U64,
    b"f32" => TokenKind::F32,
    b"f64" => TokenKind::F64,
    b"str" => TokenKind::Str,
    b"char" => TokenKind::Char,
};

/// Represents the kind of token that can be produced by the tokenizer.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum TokenKind {
    Invalid,
    Identifier,
    NumberLiteral,
    StringLiteral,
    CharLiteral,

    // Symbols and operators
    LBrace,     // {
    RBrace,     // }
    LParen,     // (
    RParen,     // )
    LBracket,   // [
    RBracket,   // ]
    Semicolon,  // ;
    Comma,      // ,
    Not,        // !
    Less,       // <
    Greater,    // >
    BitwiseAnd, // &
    BitwiseOr,  // |
    Dot,        // .
    Colon,      // :
    Question,   // ?
    Plus,       // +
    PlusEq,     // +=
    Star,       // *
    StarEq,     // *=
    Eq,         // =
    EqEq,       // ==
    Minus,      // -
    MinusEq,    // -=
    Arrow,      // ->
    Slash,      // /
    SlashEq,    // /=

    // Reserved keywords
    Const,
    Let,
    Var,
    Fn,
    Return,
    If,
    Else,
    LogicalAnd,
    LogicalOr,
    Null,
    True,
    False,
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
    Str,
    Char,
}

/// Represents a token with its kind and source location.
#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub loc: SourceLocation,
}

/// Tokenizes an arbitrary string into a sequence of tokens.
pub struct Tokenizer<'a> {
    buf: &'a [u8],
    pos: usize,
    line: usize,
}

impl<'a> Tokenizer<'a> {
    pub fn new(buf: &'a str) -> Self {
        Tokenizer {
            buf: buf.as_bytes(),
            pos: 0,
            line: 1,
        }
    }

    fn parse_identifier_or_keyword(&mut self) -> Token {
        let start = self.pos;
        let line = self.line;
        while let Some(&byte) = self.buf.get(self.pos) {
            if byte.is_ascii_alphanumeric() || byte == b'_' {
                self.pos += 1;
            } else {
                break;
            }
        }

        // Check if the character sequence is a keyword, otherwise return an identifier token.
        let identifier = &self.buf[start..self.pos];
        if let Some(keyword) = KEYWORDS.get(identifier) {
            Token {
                kind: *keyword,
                loc: SourceLocation::new(line, start, self.pos - start),
            }
        } else {
            Token {
                kind: TokenKind::Identifier,
                loc: SourceLocation::new(line, start, self.pos - start),
            }
        }
    }

    fn parse_number_literal(&mut self) -> Token {
        let start = self.pos;
        let line = self.line;

        'outer: while let Some(&byte) = self.buf.get(self.pos) {
            match byte {
                b'0'..=b'9' => {
                    self.pos += 1;
                }
                // Handle decimal point
                b'.' => {
                    self.pos += 1;
                    while let Some(&byte) = self.buf.get(self.pos) {
                        match byte {
                            b'0'..=b'9' => {
                                self.pos += 1;
                            }
                            b'e' | b'E' => continue 'outer,
                            b'.' => {
                                self.pos += 1;
                                // If we encounter another decimal point, it's invalid
                                return Token {
                                    kind: TokenKind::Invalid,
                                    loc: SourceLocation::new(line, start, self.pos - start),
                                };
                            }
                            _ => break 'outer,
                        }
                    }
                }
                // Handle scientific notation
                b'e' | b'E' => {
                    self.pos += 1;

                    if let Some(&byte) = self.buf.get(self.pos) {
                        if byte == b'-' || byte == b'+' {
                            self.pos += 1; // Skip the sign
                        }
                    }
                    while let Some(&byte) = self.buf.get(self.pos) {
                        match byte {
                            b'0'..=b'9' => {
                                self.pos += 1;
                            }
                            _ => break 'outer,
                        }
                    }
                }
                _ => break 'outer,
            }
        }

        Token {
            kind: TokenKind::NumberLiteral,
            loc: SourceLocation::new(line, start, self.pos - start),
        }
    }

    fn parse_string_literal(&mut self) -> Token {
        let start = self.pos;
        let line = self.line;
        self.pos += 1; // Skip the opening quote

        while let Some(&byte) = self.buf.get(self.pos) {
            match byte {
                b'"' => {
                    self.pos += 1; // Skip the closing quote
                    return Token {
                        kind: TokenKind::StringLiteral,
                        loc: SourceLocation::new(line, start, self.pos - start),
                    };
                }
                _ => {
                    self.pos += 1;
                }
            }
        }

        Token {
            kind: TokenKind::Invalid,
            loc: SourceLocation::new(line, start, self.pos - start),
        }
    }

    fn parse_char_literal(&mut self) -> Token {
        let start: usize = self.pos;
        let line = self.line;
        self.pos += 1; // Skip the opening quote

        match self.buf.get(self.pos) {
            Some(b'\'') => {
                self.pos += 1; // Skip the closing quote
                return Token {
                    kind: TokenKind::CharLiteral,
                    loc: SourceLocation::new(line, start, self.pos - start),
                };
            }
            Some(_) => {
                self.pos += 1;
                match self.buf.get(self.pos) {
                    Some(b'\'') => {
                        self.pos += 1; // Skip the closing quote
                        return Token {
                            kind: TokenKind::CharLiteral,
                            loc: SourceLocation::new(line, start, self.pos - start),
                        };
                    }
                    _ => {
                        // If we reach here, it means we didn't find a valid character literal
                        return Token {
                            kind: TokenKind::Invalid,
                            loc: SourceLocation::new(line, start, self.pos - start),
                        };
                    }
                }
            }
            None => {
                // If we reach the end of the buffer without finding a closing quote
                // treat it as an invalid character literal.
                return Token {
                    kind: TokenKind::Invalid,
                    loc: SourceLocation::new(line, start, self.pos - start),
                };
            }
        }
    }
    pub fn next(&mut self) -> Option<Token> {
        while let Some(&byte) = self.buf.get(self.pos) {
            match byte {
                b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                    return Some(self.parse_identifier_or_keyword());
                }
                b'0'..=b'9' => {
                    return Some(self.parse_number_literal());
                }
                b'"' => {
                    return Some(self.parse_string_literal());
                }
                b'\'' => {
                    return Some(self.parse_char_literal());
                }
                b'{' => {
                    let start = self.pos;
                    let line = self.line;
                    self.pos += 1;
                    return Some(Token {
                        kind: TokenKind::LBrace,
                        loc: SourceLocation::new(line, start, 1),
                    });
                }
                b'}' => {
                    let start = self.pos;
                    let line = self.line;
                    self.pos += 1;
                    return Some(Token {
                        kind: TokenKind::RBrace,
                        loc: SourceLocation::new(line, start, 1),
                    });
                }
                b'(' => {
                    let start = self.pos;
                    let line = self.line;
                    self.pos += 1;
                    return Some(Token {
                        kind: TokenKind::LParen,
                        loc: SourceLocation::new(line, start, 1),
                    });
                }
                b')' => {
                    let start = self.pos;
                    let line = self.line;
                    self.pos += 1;
                    return Some(Token {
                        kind: TokenKind::RParen,
                        loc: SourceLocation::new(line, start, 1),
                    });
                }
                b'[' => {
                    let start = self.pos;
                    let line = self.line;
                    self.pos += 1;
                    return Some(Token {
                        kind: TokenKind::LBracket,
                        loc: SourceLocation::new(line, start, 1),
                    });
                }
                b']' => {
                    let start = self.pos;
                    let line = self.line;
                    self.pos += 1;
                    return Some(Token {
                        kind: TokenKind::RBracket,
                        loc: SourceLocation::new(line, start, 1),
                    });
                }
                b';' => {
                    let start = self.pos;
                    let line = self.line;
                    self.pos += 1;
                    return Some(Token {
                        kind: TokenKind::Semicolon,
                        loc: SourceLocation::new(line, start, 1),
                    });
                }
                b',' => {
                    let start = self.pos;
                    let line = self.line;
                    self.pos += 1;
                    return Some(Token {
                        kind: TokenKind::Comma,
                        loc: SourceLocation::new(line, start, 1),
                    });
                }
                b'!' => {
                    let start = self.pos;
                    let line = self.line;
                    self.pos += 1;
                    return Some(Token {
                        kind: TokenKind::Not,
                        loc: SourceLocation::new(line, start, 1),
                    });
                }
                b'<' => {
                    let start = self.pos;
                    let line = self.line;
                    self.pos += 1;
                    return Some(Token {
                        kind: TokenKind::Less,
                        loc: SourceLocation::new(line, start, 1),
                    });
                }
                b'>' => {
                    let start = self.pos;
                    let line = self.line;
                    self.pos += 1;
                    return Some(Token {
                        kind: TokenKind::Greater,
                        loc: SourceLocation::new(line, start, 1),
                    });
                }
                b'&' => {
                    let start = self.pos;
                    let line = self.line;
                    self.pos += 1;
                    return Some(Token {
                        kind: TokenKind::BitwiseAnd,
                        loc: SourceLocation::new(line, start, 1),
                    });
                }
                b'|' => {
                    let start = self.pos;
                    let line = self.line;
                    self.pos += 1;
                    return Some(Token {
                        kind: TokenKind::BitwiseOr,
                        loc: SourceLocation::new(line, start, 1),
                    });
                }
                b'.' => {
                    let start = self.pos;
                    let line = self.line;
                    self.pos += 1;
                    return Some(Token {
                        kind: TokenKind::Dot,
                        loc: SourceLocation::new(line, start, 1),
                    });
                }
                b':' => {
                    let start = self.pos;
                    let line = self.line;
                    self.pos += 1;
                    return Some(Token {
                        kind: TokenKind::Colon,
                        loc: SourceLocation::new(line, start, 1),
                    });
                }
                b'?' => {
                    let start = self.pos;
                    let line = self.line;
                    self.pos += 1;
                    return Some(Token {
                        kind: TokenKind::Question,
                        loc: SourceLocation::new(line, start, 1),
                    });
                }
                b'+' => {
                    let start = self.pos;
                    let line = self.line;
                    self.pos += 1;
                    if let Some(b'=') = self.buf.get(self.pos) {
                        self.pos += 1;
                        return Some(Token {
                            kind: TokenKind::PlusEq,
                            loc: SourceLocation::new(line, start, 2),
                        });
                    }
                    return Some(Token {
                        kind: TokenKind::Plus,
                        loc: SourceLocation::new(line, start, 1),
                    });
                }
                b'*' => {
                    let start = self.pos;
                    let line = self.line;
                    self.pos += 1;
                    if let Some(b'=') = self.buf.get(self.pos) {
                        self.pos += 1;
                        return Some(Token {
                            kind: TokenKind::StarEq,
                            loc: SourceLocation::new(line, start, 2),
                        });
                    }
                    return Some(Token {
                        kind: TokenKind::Star,
                        loc: SourceLocation::new(line, start, 1),
                    });
                }
                b'=' => {
                    let start = self.pos;
                    let line = self.line;
                    self.pos += 1;
                    if let Some(b'=') = self.buf.get(self.pos) {
                        self.pos += 1;
                        return Some(Token {
                            kind: TokenKind::EqEq,
                            loc: SourceLocation::new(line, start, 2),
                        });
                    }
                    return Some(Token {
                        kind: TokenKind::Eq,
                        loc: SourceLocation::new(line, start, 1),
                    });
                }
                b'-' => {
                    let start = self.pos;
                    let line = self.line;
                    self.pos += 1;

                    match self.buf.get(self.pos) {
                        // Check for minus equals
                        Some(b'=') => {
                            self.pos += 1;
                            return Some(Token {
                                kind: TokenKind::MinusEq,
                                loc: SourceLocation::new(line, start, 2),
                            });
                        }
                        // Check for arrow
                        Some(b'>') => {
                            self.pos += 1;
                            return Some(Token {
                                kind: TokenKind::Arrow,
                                loc: SourceLocation::new(line, start, 2),
                            });
                        }
                        _ => {
                            return Some(Token {
                                kind: TokenKind::Minus,
                                loc: SourceLocation::new(line, start, 1),
                            });
                        }
                    }
                }
                b'/' => {
                    let start = self.pos;
                    let line = self.line;
                    self.pos += 1;

                    match self.buf.get(self.pos) {
                        // Single-line comment
                        Some(b'/') => {
                            self.pos += 1;
                            while let Some(&byte) = self.buf.get(self.pos) {
                                if byte == b'\n' {
                                    break;
                                }
                                self.pos += 1;
                            }
                            // Skip comment, continue lexing
                        }
                        // Digraph
                        Some(b'=') => {
                            self.pos += 1;
                            return Some(Token {
                                kind: TokenKind::SlashEq,
                                loc: SourceLocation::new(line, start, 2),
                            });
                        }
                        // Not a comment, treat as a symbol
                        _ => {
                            return Some(Token {
                                kind: TokenKind::Slash,
                                loc: SourceLocation::new(line, start, 1),
                            });
                        }
                    }
                }
                // Whitespace
                b'\n' => {
                    self.line += 1;
                    self.pos += 1;
                }
                b' ' | b'\t' | b'\r' => {
                    self.pos += 1;
                }
                _ => {
                    let start = self.pos;
                    let line = self.line;
                    self.pos += 1;

                    return Some(Token {
                        kind: TokenKind::Invalid,
                        loc: SourceLocation::new(line, start, 1),
                    });
                }
            }
        }
        None
    }
}

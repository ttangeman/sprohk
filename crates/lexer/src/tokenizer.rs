use phf::phf_map;
use sprohk_core::{SourceFile, SourceLocation};

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

    Eof,
}

impl TokenKind {
    #[inline]
    pub fn is_primitive_type(self) -> bool {
        matches!(
            self,
            TokenKind::I8
                | TokenKind::I16
                | TokenKind::I32
                | TokenKind::I64
                | TokenKind::U8
                | TokenKind::U16
                | TokenKind::U32
                | TokenKind::U64
                | TokenKind::F32
                | TokenKind::F64
                | TokenKind::Str
                | TokenKind::Char
        )
    }

    #[inline]
    pub fn is_literal(self) -> bool {
        matches!(
            self,
            TokenKind::NumberLiteral
                | TokenKind::StringLiteral
                | TokenKind::CharLiteral
                | TokenKind::True
                | TokenKind::False
                | TokenKind::Null
        )
    }

    #[inline]
    pub fn is_operator(self) -> bool {
        matches!(
            self,
            TokenKind::Plus
                | TokenKind::Minus
                | TokenKind::Star
                | TokenKind::Slash
                | TokenKind::EqEq
                | TokenKind::Less
                | TokenKind::Greater
        )
    }
}

/// Represents a token with its kind and source location.
#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub loc: SourceLocation,
}

/// Tokenizes an arbitrary string into a sequence of tokens.
pub struct Tokenizer<'a> {
    source_file: &'a SourceFile,
    buf: &'a [u8],
    pos: usize,
    line: usize,
}

impl<'a> Tokenizer<'a> {
    pub fn new(source_file: &'a SourceFile) -> Self {
        Tokenizer {
            source_file,
            buf: source_file.source().as_bytes(),
            pos: 0,
            line: 1,
        }
    }

    fn loc(&self, line: usize, start: usize, length: usize) -> SourceLocation {
        SourceLocation::from_source(self.source_file, line, start, length)
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
                loc: self.loc(line, start, self.pos - start),
            }
        } else {
            Token {
                kind: TokenKind::Identifier,
                loc: self.loc(line, start, self.pos - start),
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
                                    loc: self.loc(line, start, self.pos - start),
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
            loc: self.loc(line, start, self.pos - start),
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
                        loc: self.loc(line, start, self.pos - start),
                    };
                }
                _ => {
                    self.pos += 1;
                }
            }
        }

        Token {
            kind: TokenKind::Invalid,
            loc: self.loc(line, start, self.pos - start),
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
                    loc: self.loc(line, start, self.pos - start),
                };
            }
            Some(_) => {
                self.pos += 1;
                match self.buf.get(self.pos) {
                    Some(b'\'') => {
                        self.pos += 1; // Skip the closing quote
                        return Token {
                            kind: TokenKind::CharLiteral,
                            loc: self.loc(line, start, self.pos - start),
                        };
                    }
                    _ => {
                        // If we reach here, it means we didn't find a valid character literal
                        return Token {
                            kind: TokenKind::Invalid,
                            loc: self.loc(line, start, self.pos - start),
                        };
                    }
                }
            }
            None => {
                // If we reach the end of the buffer without finding a closing quote
                // treat it as an invalid character literal.
                return Token {
                    kind: TokenKind::Invalid,
                    loc: self.loc(line, start, self.pos - start),
                };
            }
        }
    }

    /// Returns the next token from the input buffer.
    /// Parser is terminated when it reaches the end of the buffer,
    /// yielding a token with kind `TokenKind::Eof`.
    pub fn next(&mut self) -> Token {
        while let Some(&byte) = self.buf.get(self.pos) {
            match byte {
                b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                    return self.parse_identifier_or_keyword();
                }
                b'0'..=b'9' => {
                    return self.parse_number_literal();
                }
                b'"' => {
                    return self.parse_string_literal();
                }
                b'\'' => {
                    return self.parse_char_literal();
                }
                b'{' => {
                    let start = self.pos;
                    let line = self.line;
                    self.pos += 1;
                    return Token {
                        kind: TokenKind::LBrace,
                        loc: self.loc(line, start, 1),
                    };
                }
                b'}' => {
                    let start = self.pos;
                    let line = self.line;
                    self.pos += 1;
                    return Token {
                        kind: TokenKind::RBrace,
                        loc: self.loc(line, start, 1),
                    };
                }
                b'(' => {
                    let start = self.pos;
                    let line = self.line;
                    self.pos += 1;
                    return Token {
                        kind: TokenKind::LParen,
                        loc: self.loc(line, start, 1),
                    };
                }
                b')' => {
                    let start = self.pos;
                    let line = self.line;
                    self.pos += 1;
                    return Token {
                        kind: TokenKind::RParen,
                        loc: self.loc(line, start, 1),
                    };
                }
                b'[' => {
                    let start = self.pos;
                    let line = self.line;
                    self.pos += 1;
                    return Token {
                        kind: TokenKind::LBracket,
                        loc: self.loc(line, start, 1),
                    };
                }
                b']' => {
                    let start = self.pos;
                    let line = self.line;
                    self.pos += 1;
                    return Token {
                        kind: TokenKind::RBracket,
                        loc: self.loc(line, start, 1),
                    };
                }
                b';' => {
                    let start = self.pos;
                    let line = self.line;
                    self.pos += 1;
                    return Token {
                        kind: TokenKind::Semicolon,
                        loc: self.loc(line, start, 1),
                    };
                }
                b',' => {
                    let start = self.pos;
                    let line = self.line;
                    self.pos += 1;
                    return Token {
                        kind: TokenKind::Comma,
                        loc: self.loc(line, start, 1),
                    };
                }
                b'!' => {
                    let start = self.pos;
                    let line = self.line;
                    self.pos += 1;
                    return Token {
                        kind: TokenKind::Not,
                        loc: self.loc(line, start, 1),
                    };
                }
                b'<' => {
                    let start = self.pos;
                    let line = self.line;
                    self.pos += 1;
                    return Token {
                        kind: TokenKind::Less,
                        loc: self.loc(line, start, 1),
                    };
                }
                b'>' => {
                    let start = self.pos;
                    let line = self.line;
                    self.pos += 1;
                    return Token {
                        kind: TokenKind::Greater,
                        loc: self.loc(line, start, 1),
                    };
                }
                b'&' => {
                    let start = self.pos;
                    let line = self.line;
                    self.pos += 1;
                    return Token {
                        kind: TokenKind::BitwiseAnd,
                        loc: self.loc(line, start, 1),
                    };
                }
                b'|' => {
                    let start = self.pos;
                    let line = self.line;
                    self.pos += 1;
                    return Token {
                        kind: TokenKind::BitwiseOr,
                        loc: self.loc(line, start, 1),
                    };
                }
                b'.' => {
                    let start = self.pos;
                    let line = self.line;
                    self.pos += 1;
                    return Token {
                        kind: TokenKind::Dot,
                        loc: self.loc(line, start, 1),
                    };
                }
                b':' => {
                    let start = self.pos;
                    let line = self.line;
                    self.pos += 1;
                    return Token {
                        kind: TokenKind::Colon,
                        loc: self.loc(line, start, 1),
                    };
                }
                b'?' => {
                    let start = self.pos;
                    let line = self.line;
                    self.pos += 1;
                    return Token {
                        kind: TokenKind::Question,
                        loc: self.loc(line, start, 1),
                    };
                }
                b'+' => {
                    let start = self.pos;
                    let line = self.line;
                    self.pos += 1;
                    if let Some(b'=') = self.buf.get(self.pos) {
                        self.pos += 1;
                        return Token {
                            kind: TokenKind::PlusEq,
                            loc: self.loc(line, start, 2),
                        };
                    }
                    return Token {
                        kind: TokenKind::Plus,
                        loc: self.loc(line, start, 1),
                    };
                }
                b'*' => {
                    let start = self.pos;
                    let line = self.line;
                    self.pos += 1;
                    if let Some(b'=') = self.buf.get(self.pos) {
                        self.pos += 1;
                        return Token {
                            kind: TokenKind::StarEq,
                            loc: self.loc(line, start, 2),
                        };
                    }
                    return Token {
                        kind: TokenKind::Star,
                        loc: self.loc(line, start, 1),
                    };
                }
                b'=' => {
                    let start = self.pos;
                    let line = self.line;
                    self.pos += 1;
                    if let Some(b'=') = self.buf.get(self.pos) {
                        self.pos += 1;
                        return Token {
                            kind: TokenKind::EqEq,
                            loc: self.loc(line, start, 2),
                        };
                    }
                    return Token {
                        kind: TokenKind::Eq,
                        loc: self.loc(line, start, 1),
                    };
                }
                b'-' => {
                    let start = self.pos;
                    let line = self.line;
                    self.pos += 1;

                    match self.buf.get(self.pos) {
                        // Check for minus equals
                        Some(b'=') => {
                            self.pos += 1;
                            return Token {
                                kind: TokenKind::MinusEq,
                                loc: self.loc(line, start, 2),
                            };
                        }
                        // Check for arrow
                        Some(b'>') => {
                            self.pos += 1;
                            return Token {
                                kind: TokenKind::Arrow,
                                loc: self.loc(line, start, 2),
                            };
                        }
                        _ => {
                            return Token {
                                kind: TokenKind::Minus,
                                loc: self.loc(line, start, 1),
                            };
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
                            return Token {
                                kind: TokenKind::SlashEq,
                                loc: self.loc(line, start, 2),
                            };
                        }
                        // Not a comment, treat as a symbol
                        _ => {
                            return Token {
                                kind: TokenKind::Slash,
                                loc: self.loc(line, start, 1),
                            };
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

                    return Token {
                        kind: TokenKind::Invalid,
                        loc: self.loc(line, start, 1),
                    };
                }
            }
        }
        return Token {
            kind: TokenKind::Eof,
            loc: self.loc(self.line, self.pos, 0),
        };
    }
}

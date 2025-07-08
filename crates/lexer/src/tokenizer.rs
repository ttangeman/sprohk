use phf::phf_set;
use sprohk_core::SourceLocation;

// List of keywords for the tokenizer.
// Uses a compile time hash set for fast lookup.
const KEYWORDS: phf::Set<&'static [u8]> = phf_set! {
    b"const", b"let", b"var", // decltypes
    b"fn", b"return", // function related keywords
    b"if", b"else", // control flow keywords
    b"and", b"or", // logical operators
    b"null", b"true", b"false", // literal keywords
    b"i8", b"i16", b"i32", b"i64", // signed integer types
    b"u8", b"u16", b"u32", b"u64", // unsigned integer types
    b"f32", b"f64", // float types
    b"str", b"char", // string and character types
};

#[derive(Debug)]
enum TokenKind {
    Invalid,
    Identifier,
    NumberLiteral,
    StringLiteral,
    CharLiteral,
    Symbol,
    Keyword,
}

#[derive(Debug)]
pub struct Token {
    kind: TokenKind,
    loc: SourceLocation,
}

// Tokenizes an arbitrary string into a sequence of tokens.
pub struct Tokenizer<'a> {
    buf: &'a [u8],
    pos: usize,
}

impl<'a> Tokenizer<'a> {
    pub fn new(buf: &'a str) -> Self {
        Tokenizer {
            buf: buf.as_bytes(),
            pos: 0,
        }
    }

    fn parse_identifier_or_keyword(&mut self) -> Token {
        let start = self.pos;
        while let Some(&byte) = self.buf.get(self.pos) {
            if byte.is_ascii_alphanumeric() || byte == b'_' {
                self.pos += 1;
            } else {
                break;
            }
        }

        // Check if the character sequence is a keyword, otherwise return an identifier token.
        let identifier = &self.buf[start..self.pos];
        if KEYWORDS.contains(identifier) {
            Token {
                kind: TokenKind::Keyword,
                loc: SourceLocation::new(start, self.pos - start),
            }
        } else {
            Token {
                kind: TokenKind::Identifier,
                loc: SourceLocation::new(start, self.pos - start),
            }
        }
    }

    fn parse_number_literal(&mut self) -> Token {
        let start = self.pos;

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
                                    loc: SourceLocation::new(start, self.pos - start),
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
            loc: SourceLocation::new(start, self.pos - start),
        }
    }

    fn parse_string_literal(&mut self) -> Token {
        let start = self.pos;
        self.pos += 1; // Skip the opening quote

        while let Some(&byte) = self.buf.get(self.pos) {
            match byte {
                b'"' => {
                    self.pos += 1; // Skip the closing quote
                    return Token {
                        kind: TokenKind::StringLiteral,
                        loc: SourceLocation::new(start, self.pos - start),
                    };
                }
                _ => {
                    self.pos += 1;
                }
            }
        }

        Token {
            kind: TokenKind::Invalid,
            loc: SourceLocation::new(start, self.pos - start),
        }
    }

    fn parse_char_literal(&mut self) -> Token {
        let start: usize = self.pos;
        self.pos += 1; // Skip the opening quote

        match self.buf.get(self.pos) {
            Some(b'\'') => {
                self.pos += 1; // Skip the closing quote
                return Token {
                    kind: TokenKind::CharLiteral,
                    loc: SourceLocation::new(start, self.pos - start),
                };
            }
            Some(_) => {
                self.pos += 1;
                match self.buf.get(self.pos) {
                    Some(b'\'') => {
                        self.pos += 1; // Skip the closing quote
                        return Token {
                            kind: TokenKind::CharLiteral,
                            loc: SourceLocation::new(start, self.pos - start),
                        };
                    }
                    _ => {
                        // If we reach here, it means we didn't find a valid character literal
                        return Token {
                            kind: TokenKind::Invalid,
                            loc: SourceLocation::new(start, self.pos - start),
                        };
                    }
                }
            }
            None => {
                // If we reach the end of the buffer without finding a closing quote
                // treat it as an invalid character literal.
                return Token {
                    kind: TokenKind::Invalid,
                    loc: SourceLocation::new(start, self.pos - start),
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
                b'{' | b'}' | b'(' | b')' | b'[' | b']' | b';' | b',' | b'!' | b'<' | b'>'
                | b'&' | b'|' | b'.' | b':' | b'?' => {
                    let start = self.pos;
                    self.pos += 1;

                    // TODO: Convert to specific token kind based on the symbol?
                    return Some(Token {
                        kind: TokenKind::Symbol,
                        loc: SourceLocation::new(start, 1),
                    });
                }
                b'+' | b'*' | b'=' => {
                    // Parse digraphs like `+=`, `*=`, `==`
                    self.pos += 1;
                    match self.buf.get(self.pos) {
                        Some(b'=') => {
                            self.pos += 1;
                            return Some(Token {
                                kind: TokenKind::Symbol,
                                loc: SourceLocation::new(self.pos - 2, 2),
                            });
                        }
                        _ => {
                            return Some(Token {
                                kind: TokenKind::Symbol,
                                loc: SourceLocation::new(self.pos - 1, 1),
                            });
                        }
                    }
                }
                b'-' => {
                    // Parse digraphs like `->`, `-=`
                    self.pos += 1;
                    match self.buf.get(self.pos) {
                        Some(b'=') => {
                            self.pos += 1;
                            return Some(Token {
                                kind: TokenKind::Symbol,
                                loc: SourceLocation::new(self.pos - 2, 2),
                            });
                        }
                        Some(b'>') => {
                            self.pos += 1;
                            return Some(Token {
                                kind: TokenKind::Symbol,
                                loc: SourceLocation::new(self.pos - 2, 2),
                            });
                        }
                        _ => {
                            return Some(Token {
                                kind: TokenKind::Symbol,
                                loc: SourceLocation::new(self.pos - 1, 1),
                            });
                        }
                    }
                }
                b'/' => {
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
                        }
                        // Digraph
                        Some(b'=') => {
                            self.pos += 1;
                            return Some(Token {
                                kind: TokenKind::Symbol,
                                loc: SourceLocation::new(self.pos - 2, 2),
                            });
                        }
                        // Not a comment, treat as a symbol
                        _ => {
                            return Some(Token {
                                kind: TokenKind::Symbol,
                                loc: SourceLocation::new(self.pos - 1, 1),
                            });
                        }
                    }
                }
                // Whitespace
                b' ' | b'\t' | b'\n' | b'\r' => {
                    self.pos += 1;
                }
                _ => {
                    self.pos += 1;

                    return Some(Token {
                        kind: TokenKind::Invalid,
                        loc: SourceLocation::new(self.pos - 1, 1),
                    });
                }
            }
        }

        None
    }
}

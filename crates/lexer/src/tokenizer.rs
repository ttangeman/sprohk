use sprohk_core::SourceLocation;

#[derive(Debug)]
enum TokenKind {
    Invalid,
    Identifier,
    NumberLiteral,
    StringLiteral,
    CharLiteral,
    Symbol,
}

#[derive(Debug)]
pub struct Token {
    kind: TokenKind,
    loc: SourceLocation,
}

pub struct Tokenizer<'a> {
    // Store the input buffer as bytes for efficient parsing.
    // This allows us to handle both ASCII and UTF-8 characters
    // without needing to convert to a string for each token.
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

    fn parse_identifier(&mut self) -> Token {
        let start = self.pos;
        while let Some(&byte) = self.buf.get(self.pos) {
            if byte.is_ascii_alphanumeric() || byte == b'_' {
                self.pos += 1;
            } else {
                break;
            }
        }
        Token {
            kind: TokenKind::Identifier,
            loc: SourceLocation::new(start, self.pos - start),
        }
    }

    fn parse_number_literal(&mut self) -> Token {
        let start = self.pos;
        while let Some(&byte) = self.buf.get(self.pos) {
            if byte.is_ascii_digit() {
                self.pos += 1;
            } else {
                break;
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

        // Character literal length checks get resolved later, so
        // we simply read until we find the escaping quote.
        while let Some(&byte) = self.buf.get(self.pos) {
            match byte {
                b'\'' => {
                    self.pos += 1; // Skip the closing quote
                    return Token {
                        kind: TokenKind::CharLiteral,
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

    pub fn next(&mut self) -> Option<Token> {
        while let Some(&byte) = self.buf.get(self.pos) {
            match byte {
                b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                    return Some(self.parse_identifier());
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
                b'+' | b'-' | b'*' | b'/' | b'=' | b'!' | b'<' | b'>' | b'&' | b'|' => {
                    let start = self.pos;
                    self.pos += 1;

                    return Some(Token {
                        kind: TokenKind::Symbol,
                        loc: SourceLocation::new(start, 1),
                    });
                }
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

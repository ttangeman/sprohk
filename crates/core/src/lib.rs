/// Source location for a token in the source code.
#[derive(Copy, Clone, Debug)]
pub struct SourceLocation {
    pub line: usize,
    pub start: usize,
    pub end: usize,
}

impl SourceLocation {
    pub fn new(line: usize, start: usize, length: usize) -> Self {
        SourceLocation {
            line,
            start,
            end: start + length,
        }
    }
}

/// A generic span -- may be a range of token indices or even
/// characters in the source code.
#[derive(Copy, Clone, Debug)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

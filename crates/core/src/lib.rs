#[derive(Debug)]
pub struct SourceLocation {
    pub line: usize,
    pub start: usize,
    pub end: usize,
}

impl SourceLocation {
    pub fn new(start: usize, length: usize) -> Self {
        SourceLocation {
            line: 1,
            start,
            end: start + length,
        }
    }
}

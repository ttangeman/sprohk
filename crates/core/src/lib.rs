#[derive(Debug)]
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

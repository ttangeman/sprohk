use std::{collections::hash_map::DefaultHasher, fmt, hash::Hasher, io};

/// A source code file. Contains file information and
/// the entire file contents.
pub struct SourceFile {
    file_path: Option<String>,
    file_hash: u64,

    contents: String,
}

impl SourceFile {
    pub fn new(file_path: String) -> io::Result<Self> {
        let contents = std::fs::read_to_string(&file_path)?;

        let mut result = SourceFile {
            file_path: Some(file_path),
            file_hash: 0,
            contents,
        };

        result.hash_file_path();
        Ok(result)
    }

    // Should only be used for testing
    pub fn from_raw_source(source: String) -> Self {
        SourceFile {
            file_path: None,
            file_hash: 0,
            contents: source,
        }
    }

    fn hash_file_path(&mut self) {
        let mut s = DefaultHasher::new();
        s.write(self.file_path.as_ref().unwrap().as_bytes());
        self.file_hash = s.finish();
    }

    pub fn source(&self) -> &str {
        &self.contents
    }

    pub fn path(&self) -> &str {
        &self.file_path.as_ref().unwrap()
    }

    pub fn file_hash(&self) -> u64 {
        self.file_hash
    }
}

/// Source location corresponding to some part of the contents
/// of a `SourceFile`. Tracks which `SourceFile` it belongs to through
/// the `source_hash`.
#[derive(Copy, Clone)]
pub struct SourceLocation {
    /// Optionally can associate a source index for bookkeeping
    pub source_hash: u64,

    pub line: usize,
    pub start: usize,
    pub end: usize,
}

impl SourceLocation {
    pub fn from_source(source_file: &SourceFile, line: usize, start: usize, length: usize) -> Self {
        SourceLocation {
            source_hash: source_file.file_hash(),
            line,
            start,
            end: start + length,
        }
    }
}

impl fmt::Debug for SourceLocation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Excludes source hash
        f.debug_struct("SourceLocation")
            .field("line", &self.line)
            .field("start", &self.start)
            .field("end", &self.end)
            .finish()
    }
}

/// A generic span -- may be a range of token indices or even
/// characters in the source code.
#[derive(Copy, Clone, Debug)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

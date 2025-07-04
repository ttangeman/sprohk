pub mod tokenizer;

pub use tokenizer::Token;
pub use tokenizer::Tokenizer;

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use std::path::Path;

    #[test]
    fn lexer_golden_tests() {
        let crate_root = env!("CARGO_MANIFEST_DIR");
        let tests_dir = Path::new(crate_root).join("tests");

        for entry in fs::read_dir(&tests_dir).expect("Failed to read tests directory") {
            let entry = entry.expect("Failed to read entry");
            let path = entry.path();
            if path.extension().and_then(|s| s.to_str()) == Some("spk") {
                // Read and tokenize the source file
                let source =
                    fs::read_to_string(&path).expect(&format!("Failed to read {}", path.display()));

                let mut tokenizer = Tokenizer::new(&source);
                let mut tokens = Vec::new();
                while let Some(token) = tokenizer.next() {
                    tokens.push(format!("{:?}", token));
                }

                // Read the expected tokens from the golden file
                let golden_dir = path.parent().unwrap().join("snapshots");
                let golden_path = golden_dir.join(
                    path.file_name().unwrap()
                ).with_extension("golden");

                let expected = fs::read_to_string(&golden_path)
                    .expect(&format!("Failed to read {}", golden_path.display()));

                let actual = tokens.join("\n");
                assert_eq!(
                    actual,
                    expected,
                    "Lexer output did not match golden file for {}",
                    path.display()
                );
            }
        }
    }
}

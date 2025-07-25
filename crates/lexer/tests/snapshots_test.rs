use pretty_assertions::assert_eq;
use sprohk_core::SourceFile;
use sprohk_lexer::{TokenKind, Tokenizer};
use std::{fs, str::FromStr};

#[test]
fn lexer_snapshot_tests() {
    let cwd = std::env::current_dir().expect("Failed to get current directory");
    let test_dir = cwd.join("tests");

    for entry in fs::read_dir(&test_dir).expect("Failed to read tests directory") {
        let entry = entry.expect("Failed to read entry");
        let path = entry.path();
        if path.extension().and_then(|s| s.to_str()) == Some("spk") {
            // Read and tokenize the source file
            let source = SourceFile::new(
                String::from_str(path.to_str().unwrap()).expect("failed str convert"),
            )
            .expect("failed to read file");

            let mut tokenizer = Tokenizer::new(&source);
            let mut tokens = Vec::new();
            loop {
                let token = tokenizer.next();
                tokens.push(format!("{:?}", token));

                if token.kind == TokenKind::Eof {
                    break; // End of file reached
                }
            }

            // Read the expected tokens from the golden file
            let golden_dir = path.parent().unwrap().join("snapshots");
            let golden_path = golden_dir
                .join(path.file_name().unwrap())
                .with_extension("golden");

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

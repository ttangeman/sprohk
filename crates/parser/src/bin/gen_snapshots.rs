use sprohk_core::SourceFile;
use sprohk_parser::parse_ast;

use std::fs;
use std::path::Path;
use std::str::FromStr;

fn main() {
    let crate_root = env!("CARGO_MANIFEST_DIR");
    let tests_dir = Path::new(crate_root).join("tests");

    let arena = bumpalo::Bump::new();

    // Iterate over all .spk files in the tests directory
    for entry in fs::read_dir(&tests_dir).expect("Failed to read tests directory") {
        let entry = entry.expect("Failed to read entry");
        let path = entry.path();
        if path.extension().and_then(|s| s.to_str()) == Some("spk") {
            // Read and tokenize the source file
            let source = vec![
                SourceFile::new(
                    String::from_str(path.to_str().unwrap()).expect("failed str convert"),
                )
                .expect("failed to read file"),
            ];

            let ast = parse_ast(&arena, source).expect("Failed to parse AST");

            // Construct the golden file path and write the snapshot
            // Place the golden file in a "snapshots" subdirectory next to the .spk file
            let golden_dir = path.parent().unwrap().join("snapshots");
            _ = fs::create_dir_all(&golden_dir);
            let golden_path = golden_dir
                .join(path.file_name().unwrap())
                .with_extension("golden");

            fs::write(&golden_path, ast.render_ast())
                .expect(&format!("Failed to write {}", golden_path.display()));

            println!("Generated golden file at: {}", golden_path.display());
        }
    }
}

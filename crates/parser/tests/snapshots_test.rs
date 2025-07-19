use pretty_assertions::assert_eq;
use sprohk_parser::parse_ast;
use std::fs;

#[test]
fn parser_snapshot_tests() {
    let cwd = std::env::current_dir().expect("Failed to get current directory");
    let test_dir = cwd.join("tests");

    let arena = bumpalo::Bump::new();

    for entry in fs::read_dir(&test_dir).expect("Failed to read tests directory") {
        let entry = entry.expect("Failed to read entry");
        let path = entry.path();
        if path.extension().and_then(|s| s.to_str()) == Some("spk") {
            // Read and tokenize the source file
            let source =
                fs::read_to_string(&path).expect(&format!("Failed to read {}", path.display()));

            let ast = parse_ast(&arena, source.into()).expect("Failed to parse AST");
            let nodes = ast
                .nodes()
                .iter()
                .map(|node| format!("{:?}", node))
                .collect::<Vec<_>>();

            // Read the expected tokens from the golden file
            let golden_dir = path.parent().unwrap().join("snapshots");
            let golden_path = golden_dir
                .join(path.file_name().unwrap())
                .with_extension("golden");
            let golden_data_path = golden_dir
                .join(path.file_name().unwrap())
                .with_extension("data");

            let expected = fs::read_to_string(&golden_path)
                .expect(&format!("Failed to read {}", golden_path.display()));
            let actual = nodes.join("\n");

            assert_eq!(
                actual,
                expected,
                "Parser output did not match golden file for {}",
                path.display()
            );

            let expected = fs::read_to_string(&golden_data_path)
                .expect(&format!("Failed to read {}", golden_data_path.display()));
            let actual = format!("{:#?}", ast.node_data());

            assert_eq!(
                actual,
                expected,
                "Node data did not match golden file for {}",
                path.display()
            );
        }
    }
}

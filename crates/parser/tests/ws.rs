use beacon_core::fixtures::python_files;
use beacon_parser::PythonParser;

#[test]
fn python_files_parse_to_ast() {
    let files = python_files().expect("workspace fixture should exist");
    assert!(!files.is_empty(), "workspace fixture should contain Python files");

    for file in files {
        let source =
            std::fs::read_to_string(&file).unwrap_or_else(|err| panic!("failed to read {}: {err}", file.display()));
        let mut parser = PythonParser::new().expect("parser should initialize");
        let parsed = parser
            .parse(&source)
            .unwrap_or_else(|err| panic!("failed to parse {}: {err}", file.display()));

        assert!(
            !parsed.tree.root_node().has_error(),
            "tree-sitter reported syntax errors for {}",
            file.display()
        );

        parser
            .to_ast(&parsed)
            .unwrap_or_else(|err| panic!("failed to build AST for {}: {err}", file.display()));
    }
}

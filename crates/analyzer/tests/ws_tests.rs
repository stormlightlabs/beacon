use beacon_analyzer::Linter;
use beacon_core::fixtures::python_files;
use beacon_parser::PythonParser;

#[test]
fn workspace_fixture_runs_analyzer_lint_smoke() {
    let files = python_files().expect("workspace fixture should exist");
    assert!(!files.is_empty(), "workspace fixture should contain Python files");

    let mut analyzed = 0;

    for file in files {
        if file
            .extension()
            .and_then(|ext| ext.to_str())
            .is_some_and(|ext| ext == "pyi")
        {
            continue;
        }

        let source =
            std::fs::read_to_string(&file).unwrap_or_else(|err| panic!("failed to read {}: {err}", file.display()));
        let mut parser = PythonParser::new().expect("parser should initialize");
        let (ast, symbol_table) = parser
            .parse_and_resolve(&source)
            .unwrap_or_else(|err| panic!("failed to parse and resolve {}: {err}", file.display()));

        let mut linter = Linter::new(&symbol_table, file.display().to_string(), &source);
        let _diagnostics = linter.analyze(&ast);
        analyzed += 1;
    }

    assert!(analyzed > 0, "expected at least one Python source file");
}

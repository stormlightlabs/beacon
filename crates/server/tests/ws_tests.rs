use beacon_core::fixtures::{
    ExpectedDiagnostic, assert_type_display_contains, file, python_files, range, workspace as workspace_fixture,
};
use beacon_lsp::{
    analysis::Analyzer, config::Config, document::DocumentManager, features::diagnostics::DiagnosticProvider,
    workspace::Workspace,
};
use lsp_types::{DiagnosticSeverity, Position};
use std::sync::Arc;
use tokio::sync::RwLock;
use url::Url;

#[tokio::test]
async fn workspace_fixture_lsp_diagnostics_smoke() {
    let documents = DocumentManager::new().expect("document manager should initialize");
    let config = Config::discover_and_load(&workspace_fixture()).expect("fixture config should load");
    let root_uri = Url::from_directory_path(workspace_fixture()).expect("workspace path should become file URI");
    let mut workspace = Workspace::new(Some(root_uri), config.clone(), documents.clone());

    let files = python_files().expect("workspace fixture should exist");
    assert!(!files.is_empty(), "workspace fixture should contain Python files");

    for file in files.iter().filter(|path| {
        path.extension()
            .and_then(|ext| ext.to_str())
            .is_some_and(|ext| ext == "py")
    }) {
        let source =
            std::fs::read_to_string(file).unwrap_or_else(|err| panic!("failed to read {}: {err}", file.display()));
        let uri = Url::from_file_path(file).expect("fixture file should become file URI");
        documents
            .open_document(uri.clone(), 1, source)
            .unwrap_or_else(|err| panic!("failed to open {}: {err}", file.display()));
        workspace.update_dependencies(&uri);
    }

    let workspace = Arc::new(RwLock::new(workspace));
    let diagnostic_provider = DiagnosticProvider::new(documents.clone(), workspace);
    let mut analyzer = Analyzer::new(config, documents);
    let broken_uri = Url::from_file_path(file("app/broken.py")).expect("broken file URI");

    let diagnostics = diagnostic_provider.generate_diagnostics(&broken_uri, &mut analyzer);

    ExpectedDiagnostic {
        code: "HM001",
        severity: DiagnosticSeverity::ERROR,
        message_fragment: "cannot unify str with int",
        range: range(9, 4, 9, 23),
        source_file: Some("app/broken.py"),
        tags: Some(&[]),
    }
    .assert_present_for_file(file("app/broken.py"), &diagnostics);

    ExpectedDiagnostic {
        code: "HM007",
        severity: DiagnosticSeverity::ERROR,
        message_fragment: "Attribute 'owner' not found",
        range: range(17, 11, 17, 20),
        source_file: Some("app/broken.py"),
        tags: Some(&[]),
    }
    .assert_present_for_file(file("app/broken.py"), &diagnostics);
}

#[tokio::test]
async fn workspace_fixture_typing_breadth_lsp_type_positions() {
    let documents = DocumentManager::new().expect("document manager should initialize");
    let config = Config::discover_and_load(&workspace_fixture()).expect("fixture config should load");
    let mut analyzer = Analyzer::new(config, documents.clone());
    let source = std::fs::read_to_string(file("cases/typing_breadth.py")).expect("typing breadth fixture should read");
    let uri = Url::from_file_path(file("cases/typing_breadth.py")).expect("fixture file URI");

    documents
        .open_document(uri.clone(), 1, source)
        .expect("typing breadth fixture should open");

    let identity_ty = analyzer
        .type_at_position(&uri, Position::new(64, 4))
        .expect("type lookup should succeed")
        .expect("identity function type should be recorded");
    assert_type_display_contains(identity_ty, "(value: T) -> T");

    let containers_ty = analyzer
        .type_at_position(&uri, Position::new(103, 4))
        .expect("type lookup should succeed")
        .expect("containers function type should be recorded");
    assert_type_display_contains(containers_ty, "tuple[list[str], dict[str, int], set[Status]]");

    let operator_results_ty = analyzer
        .type_at_position(&uri, Position::new(139, 4))
        .expect("type lookup should succeed")
        .expect("operator_results function type should be recorded");
    assert_type_display_contains(operator_results_ty, "tuple[bool, str, int]");
}

use beacon_core::fixtures::{ExpectedDiagnostic, file, python_files, range, workspace as workspace_fixture};
use beacon_lsp::{
    analysis::Analyzer, config::Config, document::DocumentManager, features::diagnostics::DiagnosticProvider,
    workspace::Workspace,
};
use lsp_types::DiagnosticSeverity;
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

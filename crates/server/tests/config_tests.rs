use beacon_core::fixtures::workspace as workspace_fixture;
use beacon_lsp::{
    analysis::Analyzer,
    config::{Config, DiagnosticSeverity as ConfigSeverity, PythonVersion, TypeCheckingMode},
    document::DocumentManager,
    features::diagnostics::DiagnosticProvider,
    workspace::Workspace,
};
use lsp_types::{Diagnostic, DiagnosticSeverity, NumberOrString};
use serde_json::json;
use std::sync::Arc;
use tempfile::TempDir;
use tokio::sync::RwLock;
use url::Url;

fn diagnostic_code(diagnostic: &Diagnostic) -> Option<&str> {
    match diagnostic.code.as_ref()? {
        NumberOrString::String(code) => Some(code.as_str()),
        NumberOrString::Number(_) => None,
    }
}

fn diagnostic_severity_for_source(config: Config, source: &str, code: &str) -> Option<DiagnosticSeverity> {
    let documents = DocumentManager::new().expect("document manager");
    let uri = Url::parse("file:///workspace/config.py").expect("test URI");
    documents.open_document(uri.clone(), 1, source).expect("open document");

    let mut workspace = Workspace::new(None, config.clone(), documents.clone());
    workspace.update_dependencies(&uri);
    let workspace = Arc::new(RwLock::new(workspace));
    let provider = DiagnosticProvider::new(documents.clone(), workspace);
    let mut analyzer = Analyzer::new(config, documents);

    provider
        .generate_diagnostics(&uri, &mut analyzer)
        .into_iter()
        .find(|diagnostic| diagnostic_code(diagnostic) == Some(code))
        .and_then(|diagnostic| diagnostic.severity)
}

#[test]
fn workspace_beacon_toml_is_loaded() {
    let config = Config::discover_and_load(&workspace_fixture()).expect("fixture config should load");

    assert_eq!(config.type_checking.mode, TypeCheckingMode::Balanced);
    assert_eq!(config.python_version, PythonVersion::Py312);
    assert_eq!(config.stub_paths, [std::path::PathBuf::from("stubs")]);
    assert_eq!(config.source_roots, [std::path::PathBuf::from(".")]);
    assert!(config.inlay_hints.enable);
    assert!(config.inlay_hints.variable_types);
    assert!(config.inlay_hints.function_return_types);
    assert!(config.inlay_hints.parameter_names);
}

#[test]
fn pyproject_beacon_is_loaded_when_beacon_toml_is_absent() {
    let temp_dir = TempDir::new().expect("temp dir");
    std::fs::write(
        temp_dir.path().join("pyproject.toml"),
        r#"[project]
name = "beacon-workspace-fixture"
version = "0.0.0"

[tool.beacon]
python_version = "3.12"
stub_paths = ["stubs"]

[tool.beacon.type_checking]
mode = "strict"
"#,
    )
    .expect("write pyproject");

    let config = Config::discover_and_load(temp_dir.path()).expect("pyproject config should load");
    assert_eq!(config.type_checking.mode, TypeCheckingMode::Strict);
    assert_eq!(config.python_version, PythonVersion::Py312);
    assert_eq!(config.stub_paths, [std::path::PathBuf::from("stubs")]);
}

#[test]
fn editor_settings_override_loaded_config() {
    let mut config = Config::discover_and_load(&workspace_fixture()).expect("fixture config should load");
    let settings = json!({
        "beacon": {
            "typeChecking": { "mode": "relaxed" },
            "unresolvedImportSeverity": "error",
            "maxAnyDepth": 9
        }
    });

    config.update_from_value(settings["beacon"].clone());

    assert_eq!(config.type_checking.mode, TypeCheckingMode::Relaxed);
    assert_eq!(config.unresolved_import_severity, ConfigSeverity::Error);
    assert_eq!(config.max_any_depth, 9);
}

#[test]
fn file_mode_directive_overrides_workspace_mode() {
    let config = Config {
        type_checking: beacon_lsp::config::TypeCheckingConfig { mode: TypeCheckingMode::Relaxed },
        ..Default::default()
    };
    let source = "# beacon: mode=strict\n\ndef sample() -> int:\n    value = 1\n    return value\n";

    assert_eq!(
        diagnostic_severity_for_source(config, source, "ANN002"),
        Some(DiagnosticSeverity::ERROR)
    );
}

#[test]
fn modes_change_missing_annotation_severity_predictably() {
    let source = "def sample() -> int:\n    value = 1\n    return value\n";

    for (mode, expected) in [
        (TypeCheckingMode::Strict, Some(DiagnosticSeverity::ERROR)),
        (TypeCheckingMode::Balanced, Some(DiagnosticSeverity::WARNING)),
        (TypeCheckingMode::Relaxed, None),
    ] {
        let config = Config { type_checking: beacon_lsp::config::TypeCheckingConfig { mode }, ..Default::default() };
        assert_eq!(
            diagnostic_severity_for_source(config, source, "ANN002"),
            expected,
            "{mode:?} should map ANN002 to {expected:?}"
        );
    }
}

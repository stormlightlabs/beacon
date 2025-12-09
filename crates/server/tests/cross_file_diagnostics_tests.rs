//! Integration tests for cross-file import/export diagnostics

use beacon_lsp::{
    analysis::Analyzer, config::Config, document::DocumentManager, features::diagnostics::DiagnosticProvider,
    workspace::Workspace,
};
use std::sync::Arc;
use tokio::sync::RwLock;
use url::Url;

/// Helper to create file URIs from simple paths relative to workspace root
fn file_uri(workspace_root: &str, path: &str) -> Url {
    Url::parse(&format!("{workspace_root}/{path}")).unwrap()
}

#[tokio::test]
async fn test_invalid_symbol_import_from_valid_module() {
    let documents = DocumentManager::new().unwrap();
    let config = Config::default();
    let workspace_root = Url::parse("file:///workspace").unwrap();
    let mut workspace = Workspace::new(Some(workspace_root.clone()), config.clone(), documents.clone());

    let module_a_uri = file_uri(workspace_root.as_str(), "module_a.py");
    let module_a_content = r#"
def valid_function():
    pass

class ValidClass:
    pass

VALID_CONSTANT = 42
"#;
    documents
        .open_document(module_a_uri.clone(), 0, module_a_content.to_string())
        .unwrap();
    workspace.update_dependencies(&module_a_uri);

    let module_b_uri = file_uri(workspace_root.as_str(), "module_b.py");
    let module_b_content = r#"
from module_a import valid_function  # OK
from module_a import nonexistent_function  # ERROR: symbol doesn't exist
from module_a import ValidClass  # OK
from module_a import NonexistentClass  # ERROR: symbol doesn't exist
"#;
    documents
        .open_document(module_b_uri.clone(), 0, module_b_content.to_string())
        .unwrap();
    workspace.update_dependencies(&module_b_uri);

    let workspace_arc = Arc::new(RwLock::new(workspace));
    let mut analyzer = Analyzer::new(config, documents.clone());
    let diagnostic_provider = DiagnosticProvider::new(documents.clone(), workspace_arc);
    let diagnostics = diagnostic_provider.generate_diagnostics(&module_b_uri, &mut analyzer);

    let invalid_import_diagnostics: Vec<_> = diagnostics
        .iter()
        .filter(|d| {
            d.code.as_ref().map_or(false, |c| match c {
                lsp_types::NumberOrString::String(s) => s == "invalid-import",
                _ => false,
            })
        })
        .collect();

    assert_eq!(
        invalid_import_diagnostics.len(),
        2,
        "Expected 2 invalid-import diagnostics, found {}. Messages: {:?}",
        invalid_import_diagnostics.len(),
        invalid_import_diagnostics
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );

    let messages: Vec<String> = invalid_import_diagnostics.iter().map(|d| d.message.clone()).collect();
    assert!(
        messages.iter().any(|m| m.contains("nonexistent_function")),
        "Expected error for nonexistent_function"
    );
    assert!(
        messages.iter().any(|m| m.contains("NonexistentClass")),
        "Expected error for NonexistentClass"
    );
}

#[tokio::test]
async fn test_private_symbol_import_warnings() {
    let documents = DocumentManager::new().unwrap();
    let config = Config::default();
    let workspace_root = Url::parse("file:///workspace").unwrap();
    let mut workspace = Workspace::new(Some(workspace_root.clone()), config.clone(), documents.clone());

    let module_a_uri = file_uri(workspace_root.as_str(), "module_a.py");
    let module_a_content = r#"
def public_function():
    pass

def _private_function():
    pass

class PublicClass:
    pass

class _PrivateClass:
    pass

_PRIVATE_CONSTANT = 42
"#;
    documents
        .open_document(module_a_uri.clone(), 0, module_a_content.to_string())
        .unwrap();
    workspace.update_dependencies(&module_a_uri);

    let module_b_uri = file_uri(workspace_root.as_str(), "module_b.py");
    let module_b_content = r#"
from module_a import public_function  # OK
from module_a import _private_function  # WARNING: private symbol
from module_a import PublicClass  # OK
from module_a import _PrivateClass  # WARNING: private symbol
from module_a import _PRIVATE_CONSTANT  # WARNING: private symbol
"#;
    documents
        .open_document(module_b_uri.clone(), 0, module_b_content.to_string())
        .unwrap();
    workspace.update_dependencies(&module_b_uri);

    let workspace_arc = Arc::new(RwLock::new(workspace));
    let mut analyzer = Analyzer::new(config, documents.clone());
    let diagnostic_provider = DiagnosticProvider::new(documents.clone(), workspace_arc);
    let diagnostics = diagnostic_provider.generate_diagnostics(&module_b_uri, &mut analyzer);

    let private_import_diagnostics: Vec<_> = diagnostics
        .iter()
        .filter(|d| {
            d.code.as_ref().map_or(false, |c| match c {
                lsp_types::NumberOrString::String(s) => s == "private-import",
                _ => false,
            })
        })
        .collect();

    assert_eq!(
        private_import_diagnostics.len(),
        3,
        "Expected 3 private-import warnings, found {}. Messages: {:?}",
        private_import_diagnostics.len(),
        private_import_diagnostics
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );

    for diag in &private_import_diagnostics {
        assert_eq!(diag.severity, Some(lsp_types::DiagnosticSeverity::WARNING));
    }

    let messages: Vec<String> = private_import_diagnostics.iter().map(|d| d.message.clone()).collect();
    assert!(messages.iter().any(|m| m.contains("_private_function")));
    assert!(messages.iter().any(|m| m.contains("_PrivateClass")));
    assert!(messages.iter().any(|m| m.contains("_PRIVATE_CONSTANT")));
}

#[tokio::test]
async fn test_reexport_chain_validation() {
    let documents = DocumentManager::new().unwrap();
    let config = Config::default();
    let workspace_root = Url::parse("file:///workspace").unwrap();
    let mut workspace = Workspace::new(Some(workspace_root.clone()), config.clone(), documents.clone());

    let source_uri = file_uri(workspace_root.as_str(), "source.py");
    let source_module_content = r#"
def existing_function():
    pass

class ExistingClass:
    pass
"#;
    documents
        .open_document(source_uri.clone(), 0, source_module_content.to_string())
        .unwrap();
    workspace.update_dependencies(&source_uri);

    let reexport_uri = file_uri(workspace_root.as_str(), "reexport.py");
    let reexport_module_content = r#"
from source import existing_function
from source import nonexistent_function

__all__ = [
    "existing_function",  # OK: imported from source and exists
    "nonexistent_function",  # ERROR: imported but doesn't exist in source
]
"#;
    documents
        .open_document(reexport_uri.clone(), 0, reexport_module_content.to_string())
        .unwrap();
    workspace.update_dependencies(&reexport_uri);

    let workspace_arc = Arc::new(RwLock::new(workspace));
    let mut analyzer = Analyzer::new(config, documents.clone());
    let diagnostic_provider = DiagnosticProvider::new(documents.clone(), workspace_arc);
    let diagnostics = diagnostic_provider.generate_diagnostics(&reexport_uri, &mut analyzer);

    let reexport_diagnostics: Vec<_> = diagnostics
        .iter()
        .filter(|d| {
            d.code.as_ref().map_or(false, |c| match c {
                lsp_types::NumberOrString::String(s) => s == "broken-reexport",
                _ => false,
            })
        })
        .collect();

    assert_eq!(
        reexport_diagnostics.len(),
        1,
        "Expected 1 broken-reexport diagnostic, found {}. Messages: {:?}",
        reexport_diagnostics.len(),
        reexport_diagnostics.iter().map(|d| &d.message).collect::<Vec<_>>()
    );

    let message = &reexport_diagnostics[0].message;
    assert!(message.contains("nonexistent_function"));
    assert!(message.contains("source"));
}

#[tokio::test]
async fn test_no_false_positives_for_valid_imports() {
    let documents = DocumentManager::new().unwrap();
    let config = Config::default();
    let workspace_root = Url::parse("file:///workspace").unwrap();
    let mut workspace = Workspace::new(Some(workspace_root.clone()), config.clone(), documents.clone());

    let module_a_uri = file_uri(workspace_root.as_str(), "module_a.py");
    let module_a_content = r#"
def function_one():
    pass

def function_two():
    pass

class ClassA:
    pass

CONSTANT = 42

__all__ = ["function_one", "ClassA"]
"#;
    documents
        .open_document(module_a_uri.clone(), 0, module_a_content.to_string())
        .unwrap();
    workspace.update_dependencies(&module_a_uri);

    let module_b_uri = file_uri(workspace_root.as_str(), "module_b.py");
    let module_b_content = r#"
from module_a import function_one
from module_a import function_two
from module_a import ClassA
from module_a import CONSTANT
"#;
    documents
        .open_document(module_b_uri.clone(), 0, module_b_content.to_string())
        .unwrap();
    workspace.update_dependencies(&module_b_uri);

    let workspace_arc = Arc::new(RwLock::new(workspace));
    let mut analyzer = Analyzer::new(config, documents.clone());
    let diagnostic_provider = DiagnosticProvider::new(documents.clone(), workspace_arc);
    let diagnostics = diagnostic_provider.generate_diagnostics(&module_b_uri, &mut analyzer);

    let invalid_diagnostics: Vec<_> = diagnostics
        .iter()
        .filter(|d| {
            d.code.as_ref().map_or(false, |c| match c {
                lsp_types::NumberOrString::String(s) => s == "invalid-import" || s == "broken-reexport",
                _ => false,
            })
        })
        .collect();

    assert_eq!(
        invalid_diagnostics.len(),
        0,
        "Expected no invalid-import diagnostics for valid imports, found {}. Messages: {:?}",
        invalid_diagnostics.len(),
        invalid_diagnostics.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
}

#[tokio::test]
async fn test_wildcard_imports_not_flagged() {
    let documents = DocumentManager::new().unwrap();
    let config = Config::default();
    let workspace_root = Url::parse("file:///workspace").unwrap();
    let mut workspace = Workspace::new(Some(workspace_root.clone()), config.clone(), documents.clone());

    let module_a_uri = file_uri(workspace_root.as_str(), "module_a.py");
    let module_a_content = r#"
def function():
    pass
"#;
    documents
        .open_document(module_a_uri.clone(), 0, module_a_content.to_string())
        .unwrap();
    workspace.update_dependencies(&module_a_uri);

    let module_b_uri = file_uri(workspace_root.as_str(), "module_b.py");
    let module_b_content = r#"
from module_a import *
"#;
    documents
        .open_document(module_b_uri.clone(), 0, module_b_content.to_string())
        .unwrap();
    workspace.update_dependencies(&module_b_uri);

    let workspace_arc = Arc::new(RwLock::new(workspace));
    let mut analyzer = Analyzer::new(config, documents.clone());
    let diagnostic_provider = DiagnosticProvider::new(documents.clone(), workspace_arc);
    let diagnostics = diagnostic_provider.generate_diagnostics(&module_b_uri, &mut analyzer);

    let invalid_diagnostics: Vec<_> = diagnostics
        .iter()
        .filter(|d| {
            d.code.as_ref().map_or(false, |c| match c {
                lsp_types::NumberOrString::String(s) => s == "invalid-import",
                _ => false,
            })
        })
        .collect();

    assert_eq!(invalid_diagnostics.len(), 0);
}

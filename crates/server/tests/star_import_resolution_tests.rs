//! Integration tests for star import resolution
//!
//! Verifies that `from module import *` correctly expands to concrete symbols based on __all__ or all public symbols.

use beacon_lsp::Config;
use beacon_lsp::document::DocumentManager;
use beacon_lsp::workspace::Workspace;
use url::Url;

fn file_uri(path: &str) -> Url {
    Url::parse(&format!("file:///{path}")).unwrap()
}

#[test]
fn test_resolve_star_import_with_all() {
    let documents = DocumentManager::new().unwrap();
    let config = Config::default();
    let mut workspace = Workspace::new(None, config, documents.clone());

    let module_uri = file_uri("src/module.py");
    let source = r#"
__all__ = ["public_func", "PublicClass"]

def public_func():
    pass

def _private_func():
    pass

class PublicClass:
    pass

class PrivateClass:
    pass

PUBLIC_VAR = 42
_PRIVATE_VAR = 99
"#;

    documents
        .open_document(module_uri.clone(), 1, source.to_string())
        .unwrap();
    workspace.update_dependencies(&module_uri);

    let symbols = workspace.resolve_star_import(&module_uri);

    assert_eq!(symbols.len(), 2, "Should only include symbols in __all__");
    assert!(symbols.contains(&"public_func".to_string()));
    assert!(symbols.contains(&"PublicClass".to_string()));
    assert!(!symbols.contains(&"_private_func".to_string()));
    assert!(!symbols.contains(&"PrivateClass".to_string()));
    assert!(!symbols.contains(&"PUBLIC_VAR".to_string()), "__all__ takes precedence");
}

#[test]
fn test_resolve_star_import_without_all() {
    let documents = DocumentManager::new().unwrap();
    let config = Config::default();
    let mut workspace = Workspace::new(None, config, documents.clone());

    let module_uri = file_uri("src/module.py");
    let source = r#"
def public_func():
    pass

def _private_func():
    pass

class PublicClass:
    pass

class _PrivateClass:
    pass

PUBLIC_VAR = 42
_PRIVATE_VAR = 99
"#;

    documents
        .open_document(module_uri.clone(), 1, source.to_string())
        .unwrap();
    workspace.update_dependencies(&module_uri);

    let symbols = workspace.resolve_star_import(&module_uri);

    assert!(symbols.contains(&"public_func".to_string()));
    assert!(symbols.contains(&"PublicClass".to_string()));
    assert!(symbols.contains(&"PUBLIC_VAR".to_string()));

    assert!(!symbols.contains(&"_private_func".to_string()));
    assert!(!symbols.contains(&"_PrivateClass".to_string()));
    assert!(!symbols.contains(&"_PRIVATE_VAR".to_string()));
}

#[test]
fn test_resolve_star_import_empty_all() {
    let documents = DocumentManager::new().unwrap();
    let config = Config::default();
    let mut workspace = Workspace::new(None, config, documents.clone());

    let module_uri = file_uri("src/module.py");
    let source = r#"
__all__ = []

def public_func():
    pass

class PublicClass:
    pass
"#;

    documents
        .open_document(module_uri.clone(), 1, source.to_string())
        .unwrap();
    workspace.update_dependencies(&module_uri);

    let symbols = workspace.resolve_star_import(&module_uri);
    assert_eq!(symbols.len(), 0);
}

#[test]
fn test_resolve_star_import_nonexistent_module() {
    let documents = DocumentManager::new().unwrap();
    let config = Config::default();
    let workspace = Workspace::new(None, config, documents.clone());
    let module_uri = file_uri("src/nonexistent.py");
    let symbols = workspace.resolve_star_import(&module_uri);
    assert_eq!(symbols.len(), 0);
}

#[test]
fn test_resolve_star_import_all_with_imported_symbols() {
    let documents = DocumentManager::new().unwrap();
    let config = Config::default();
    let mut workspace = Workspace::new(None, config, documents.clone());

    let module_uri = file_uri("src/module.py");
    let source = r#"
from other import external_func

__all__ = ["public_func", "external_func"]

def public_func():
    pass
"#;

    documents
        .open_document(module_uri.clone(), 1, source.to_string())
        .unwrap();
    workspace.update_dependencies(&module_uri);

    let symbols = workspace.resolve_star_import(&module_uri);

    assert_eq!(symbols.len(), 2);
    assert!(symbols.contains(&"public_func".to_string()));
    assert!(
        symbols.contains(&"external_func".to_string()),
        "Should include re-exported symbols"
    );
}

#[test]
fn test_resolve_star_import_only_imports() {
    let documents = DocumentManager::new().unwrap();
    let config = Config::default();
    let mut workspace = Workspace::new(None, config, documents.clone());

    let module_uri = file_uri("src/module.py");
    let source = r#"
from os import path
from sys import argv
"#;

    documents
        .open_document(module_uri.clone(), 1, source.to_string())
        .unwrap();
    workspace.update_dependencies(&module_uri);

    let symbols = workspace.resolve_star_import(&module_uri);

    assert!(symbols.contains(&"path".to_string()));
    assert!(symbols.contains(&"argv".to_string()));
}

#[test]
fn test_resolve_star_import_mixed_definitions() {
    let documents = DocumentManager::new().unwrap();
    let config = Config::default();
    let mut workspace = Workspace::new(None, config, documents.clone());

    let module_uri = file_uri("src/module.py");
    let source = r#"
"""Module with mixed definitions."""

from typing import List

def helper():
    pass

class MyClass:
    pass

my_var: int = 42

def _internal():
    pass
"#;

    documents
        .open_document(module_uri.clone(), 1, source.to_string())
        .unwrap();
    workspace.update_dependencies(&module_uri);

    let symbols = workspace.resolve_star_import(&module_uri);

    assert!(
        symbols.contains(&"List".to_string()),
        "Imported types should be included"
    );
    assert!(symbols.contains(&"helper".to_string()));
    assert!(symbols.contains(&"MyClass".to_string()));
    assert!(symbols.contains(&"my_var".to_string()));
    assert!(!symbols.contains(&"_internal".to_string()));
}

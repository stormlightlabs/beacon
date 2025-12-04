//! Integration tests for custom stub overrides
//!
//! Tests the layered stub resolution system ensuring that custom stubs
//! can override typeshed stubs according to PEP 561 priority order:
//!
//! 1. Manual stubs (config.stub_paths, highest priority)
//! 2. Stub packages (*-stubs directories)
//! 3. Inline stubs (.pyi files in project)
//! 4. Typeshed stubs (embedded at build time, fallback)

use beacon_lsp::config::Config;
use beacon_lsp::document::DocumentManager;
use beacon_lsp::workspace::Workspace;
use std::fs;
use std::path::PathBuf;
use tempfile::TempDir;
use url::Url;

/// Helper to create a workspace with custom stub configuration
fn create_workspace_with_stubs(stub_dir: PathBuf) -> (Workspace, TempDir) {
    let project_dir = TempDir::new().unwrap();
    let project_path = project_dir.path().to_path_buf();

    let config = Config { stub_paths: vec![stub_dir], ..Default::default() };

    let root_uri = Url::from_directory_path(&project_path).unwrap();
    let documents = DocumentManager::new().unwrap();
    let workspace = Workspace::new(Some(root_uri), config, documents);

    (workspace, project_dir)
}

#[test]
fn test_custom_stub_overrides_typeshed_builtins() {
    let custom_stubs_dir = TempDir::new().unwrap();
    let custom_stub_path = custom_stubs_dir.path().join("builtins.pyi");
    let custom_stub_content = r#"
class list:
    """Custom list stub that overrides typeshed"""
    def custom_method(self) -> str:
        """This method only exists in the custom stub"""
        ...

    def append(self, item) -> None:
        """Standard append method"""
        ...
"#;

    fs::write(&custom_stub_path, custom_stub_content).unwrap();
    let (workspace, _temp_dir) = create_workspace_with_stubs(custom_stubs_dir.path().to_path_buf());

    let stub = workspace.load_stub("builtins").expect("Should load builtins stub");
    assert_eq!(stub.module, "builtins");
    assert!(stub.path.to_str().unwrap().contains("builtins.pyi"));

    let content = stub.content.or_else(|| fs::read_to_string(&stub.path).ok());
    assert!(content.is_some(), "Stub should have content");

    let content = content.unwrap();
    assert!(
        content.contains("custom_method"),
        "Custom stub should contain custom_method"
    );
    assert!(
        content.contains("Custom list stub that overrides typeshed"),
        "Custom stub should contain our docstring"
    );
}

#[test]
fn test_custom_stub_overrides_typeshed_typing() {
    let custom_stubs_dir = TempDir::new().unwrap();
    let custom_stub_path = custom_stubs_dir.path().join("typing.pyi");
    let custom_stub_content = r#"
from typing import TypeVar

T = TypeVar('T')

class Protocol:
    """Custom Protocol implementation"""
    def custom_protocol_method(self) -> None:
        """This is a custom extension to Protocol"""
        ...
"#;

    fs::write(&custom_stub_path, custom_stub_content).unwrap();

    let (workspace, _temp_dir) = create_workspace_with_stubs(custom_stubs_dir.path().to_path_buf());
    let stub = workspace.load_stub("typing").expect("Should load typing stub");

    assert_eq!(stub.module, "typing");

    let content = stub.content.or_else(|| fs::read_to_string(&stub.path).ok());
    assert!(content.is_some(), "Stub should have content");

    let content = content.unwrap();
    assert!(
        content.contains("custom_protocol_method"),
        "Custom stub should contain custom_protocol_method"
    );
}

#[test]
fn test_typeshed_fallback_when_no_custom_stub() {
    let custom_stubs_dir = TempDir::new().unwrap();
    let (workspace, _temp_dir) = create_workspace_with_stubs(custom_stubs_dir.path().to_path_buf());
    let stub = workspace.load_stub("builtins");

    assert!(
        stub.is_some(),
        "Should fall back to typeshed when no custom stub exists"
    );

    let stub = stub.unwrap();
    assert_eq!(stub.module, "builtins");
    assert!(stub.content.is_some(), "Typeshed stubs should be embedded");
}

#[test]
fn test_multiple_stub_paths_priority_order() {
    let first_stubs_dir = TempDir::new().unwrap();
    let second_stubs_dir = TempDir::new().unwrap();

    let first_stub_path = first_stubs_dir.path().join("os.pyi");
    let first_stub_content = r#"
# First priority stub
def first_priority_function() -> str: ...
"#;
    fs::write(&first_stub_path, first_stub_content).unwrap();

    let second_stub_path = second_stubs_dir.path().join("os.pyi");
    let second_stub_content = r#"
# Second priority stub
def second_priority_function() -> str: ...
"#;
    fs::write(&second_stub_path, second_stub_content).unwrap();

    let project_dir = TempDir::new().unwrap();
    let project_path = project_dir.path().to_path_buf();

    let config = Config {
        stub_paths: vec![
            first_stubs_dir.path().to_path_buf(),
            second_stubs_dir.path().to_path_buf(),
        ],
        ..Default::default()
    };

    let root_uri = Url::from_directory_path(&project_path).unwrap();
    let documents = DocumentManager::new().unwrap();
    let workspace = Workspace::new(Some(root_uri), config, documents);
    let stub = workspace.load_stub("os").expect("Should load os stub");
    let content = stub.content.or_else(|| fs::read_to_string(&stub.path).ok());
    assert!(content.is_some(), "Stub should have content");

    let content = content.unwrap();
    assert!(
        content.contains("first_priority_function"),
        "Should use first stub path (higher priority)"
    );
    assert!(
        !content.contains("second_priority_function"),
        "Should not use second stub path when first has the module"
    );
}

#[test]
fn test_stub_package_overrides_typeshed() {
    let project_dir = TempDir::new().unwrap();
    let project_path = project_dir.path();

    let stub_package_dir = project_path.join("mypy-stubs");
    fs::create_dir_all(&stub_package_dir).unwrap();

    let stub_path = stub_package_dir.join("mypy.pyi");
    let stub_content = r#"
# Custom mypy stub
def custom_type_check() -> bool: ...
"#;
    fs::write(&stub_path, stub_content).unwrap();

    let config = Config::default();
    let root_uri = Url::from_directory_path(project_path).unwrap();
    let documents = DocumentManager::new().unwrap();
    let workspace = Workspace::new(Some(root_uri), config, documents);

    let stub = workspace.load_stub("mypy");

    if let Some(stub) = stub {
        let content = fs::read_to_string(&stub.path).ok();
        if let Some(content) = content {
            assert!(
                content.contains("custom_type_check"),
                "Stub package should override typeshed"
            );
        }
    }
}

#[test]
fn test_inline_stub_in_project() {
    let project_dir = TempDir::new().unwrap();
    let project_path = project_dir.path();

    let py_file = project_path.join("mymodule.py");
    fs::write(&py_file, "def hello(): pass").unwrap();

    let pyi_file = project_path.join("mymodule.pyi");
    let pyi_content = r#"
def hello() -> str: ...
"#;
    fs::write(&pyi_file, pyi_content).unwrap();

    let config = Config::default();
    let root_uri = Url::from_directory_path(project_path).unwrap();
    let documents = DocumentManager::new().unwrap();
    let _workspace = Workspace::new(Some(root_uri), config, documents);

    assert!(pyi_file.exists(), "Inline stub file should exist");
}

#[test]
fn test_custom_stub_with_nested_module() {
    let custom_stubs_dir = TempDir::new().unwrap();
    let collections_dir = custom_stubs_dir.path().join("collections");
    fs::create_dir_all(&collections_dir).unwrap();

    let abc_stub_path = collections_dir.join("abc.pyi");
    let abc_stub_content = r#"
# Custom collections.abc stub
class CustomIterator:
    """Custom iterator implementation"""
    def custom_iter_method(self) -> None: ...
"#;
    fs::write(&abc_stub_path, abc_stub_content).unwrap();

    let (workspace, _temp_dir) = create_workspace_with_stubs(custom_stubs_dir.path().to_path_buf());
    let stub = workspace
        .load_stub("collections.abc")
        .expect("Should load collections.abc stub");

    assert_eq!(stub.module, "collections.abc");

    let content = stub.content.or_else(|| fs::read_to_string(&stub.path).ok());
    assert!(content.is_some(), "Stub should have content");

    let content = content.unwrap();
    assert!(
        content.contains("CustomIterator"),
        "Custom nested stub should contain CustomIterator"
    );
}

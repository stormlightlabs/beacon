//! Integration tests for cross-module type mismatch diagnostics

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
async fn test_user_defined_function_wrong_argument_count() {
    let documents = DocumentManager::new().unwrap();
    let config = Config::default();
    let workspace_root = Url::parse("file:///workspace").unwrap();
    let mut workspace = Workspace::new(Some(workspace_root.clone()), config.clone(), documents.clone());

    let utils_uri = file_uri(workspace_root.as_str(), "utils.py");
    let utils_content = r#"
def greet(name: str) -> str:
    return f"Hello, {name}!"
"#;
    documents
        .open_document(utils_uri.clone(), 0, utils_content.to_string())
        .unwrap();
    workspace.update_dependencies(&utils_uri);

    let main_uri = file_uri(workspace_root.as_str(), "main.py");
    let main_content = r#"
from utils import greet

# This should trigger an argument count mismatch diagnostic
result = greet("Alice", "Bob")  # ERROR: expects 1 argument, got 2
"#;
    documents
        .open_document(main_uri.clone(), 0, main_content.to_string())
        .unwrap();
    workspace.update_dependencies(&main_uri);

    let workspace_arc = Arc::new(RwLock::new(workspace));
    let mut analyzer = Analyzer::new(config, documents.clone());
    let diagnostic_provider = DiagnosticProvider::new(documents.clone(), workspace_arc);
    let diagnostics = diagnostic_provider.generate_diagnostics(&main_uri, &mut analyzer);

    let mismatch_diagnostics: Vec<_> = diagnostics
        .iter()
        .filter(|d| {
            d.code.as_ref().map_or(false, |c| match c {
                lsp_types::NumberOrString::String(s) => s == "argument-count-mismatch",
                _ => false,
            })
        })
        .collect();

    assert_eq!(
        mismatch_diagnostics.len(),
        1,
        "Expected 1 argument count mismatch diagnostic. Found: {:?}",
        mismatch_diagnostics.iter().map(|d| &d.message).collect::<Vec<_>>()
    );

    let diagnostic = mismatch_diagnostics[0];
    assert!(
        diagnostic.message.contains("greet"),
        "Diagnostic should mention function name 'greet'"
    );
    assert!(
        diagnostic.message.contains("1") && diagnostic.message.contains("2"),
        "Diagnostic should mention expected (1) and actual (2) argument counts"
    );
}

#[tokio::test]
async fn test_no_false_positives_for_correct_types() {
    let documents = DocumentManager::new().unwrap();
    let config = Config::default();
    let workspace_root = Url::parse("file:///workspace").unwrap();
    let mut workspace = Workspace::new(Some(workspace_root.clone()), config.clone(), documents.clone());

    let module_a_uri = file_uri(workspace_root.as_str(), "utils.py");
    let module_a_content = r#"
def greet(name: str) -> str:
    return f"Hello, {name}!"

def count(items: list) -> int:
    return len(items)
"#;
    documents
        .open_document(module_a_uri.clone(), 0, module_a_content.to_string())
        .unwrap();
    workspace.update_dependencies(&module_a_uri);

    let module_b_uri = file_uri(workspace_root.as_str(), "app.py");
    let module_b_content = r#"
from utils import greet, count

message = greet("Alice")  # OK: str argument
total = count([1, 2, 3])  # OK: list argument
"#;
    documents
        .open_document(module_b_uri.clone(), 0, module_b_content.to_string())
        .unwrap();
    workspace.update_dependencies(&module_b_uri);

    let workspace_arc = Arc::new(RwLock::new(workspace));
    let mut analyzer = Analyzer::new(config, documents.clone());
    let diagnostic_provider = DiagnosticProvider::new(documents.clone(), workspace_arc);
    let diagnostics = diagnostic_provider.generate_diagnostics(&module_b_uri, &mut analyzer);

    let mismatch_diagnostics: Vec<_> = diagnostics
        .iter()
        .filter(|d| {
            d.code.as_ref().map_or(false, |c| match c {
                lsp_types::NumberOrString::String(s) => s == "argument-count-mismatch",
                _ => false,
            })
        })
        .collect();

    assert_eq!(
        mismatch_diagnostics.len(),
        0,
        "Expected no argument count mismatch diagnostics for correct usage. Found: {:?}",
        mismatch_diagnostics.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
}

#[tokio::test]
async fn test_user_defined_function_with_type_mismatch() {
    let documents = DocumentManager::new().unwrap();
    let config = Config::default();
    let workspace_root = Url::parse("file:///workspace").unwrap();
    let mut workspace = Workspace::new(Some(workspace_root.clone()), config.clone(), documents.clone());

    let math_ops_uri = file_uri(workspace_root.as_str(), "math_ops.py");
    let math_ops_content = r#"
def add(a: int, b: int) -> int:
    return a + b
"#;
    documents
        .open_document(math_ops_uri.clone(), 0, math_ops_content.to_string())
        .unwrap();
    workspace.update_dependencies(&math_ops_uri);

    let main_uri = file_uri(workspace_root.as_str(), "main.py");
    let main_content = r#"
from math_ops import add

# This should trigger type mismatch diagnostics
result = add("hello", 42)  # ERROR: first arg should be int, got str
"#;
    documents
        .open_document(main_uri.clone(), 0, main_content.to_string())
        .unwrap();
    workspace.update_dependencies(&main_uri);

    let workspace_arc = Arc::new(RwLock::new(workspace));
    let mut analyzer = Analyzer::new(config, documents.clone());
    let diagnostic_provider = DiagnosticProvider::new(documents.clone(), workspace_arc);
    let diagnostics = diagnostic_provider.generate_diagnostics(&main_uri, &mut analyzer);

    let type_mismatch_diagnostics: Vec<_> = diagnostics
        .iter()
        .filter(|d| {
            d.code.as_ref().map_or(false, |c| match c {
                lsp_types::NumberOrString::String(s) => s == "type-mismatch-argument",
                _ => false,
            })
        })
        .collect();

    assert_eq!(
        type_mismatch_diagnostics.len(),
        1,
        "Expected 1 type mismatch diagnostic. Found: {:?}",
        type_mismatch_diagnostics.iter().map(|d| &d.message).collect::<Vec<_>>()
    );

    let diagnostic = type_mismatch_diagnostics[0];
    assert!(
        diagnostic.message.contains("add"),
        "Diagnostic should mention function name 'add'"
    );
    assert!(
        diagnostic.message.contains("str") && diagnostic.message.contains("int"),
        "Diagnostic should mention both actual (str) and expected (int) types"
    );
}

#[tokio::test]
async fn test_any_type_compatibility() {
    let documents = DocumentManager::new().unwrap();
    let config = Config::default();
    let workspace_root = Url::parse("file:///workspace").unwrap();
    let mut workspace = Workspace::new(Some(workspace_root.clone()), config.clone(), documents.clone());

    let module_a_uri = file_uri(workspace_root.as_str(), "flexible.py");
    let module_a_content = r#"
from typing import Any

def process(data: Any) -> str:
    return str(data)
"#;
    documents
        .open_document(module_a_uri.clone(), 0, module_a_content.to_string())
        .unwrap();
    workspace.update_dependencies(&module_a_uri);

    let module_b_uri = file_uri(workspace_root.as_str(), "caller.py");
    let module_b_content = r#"
from flexible import process

result1 = process(42)  # OK: Any accepts int
result2 = process("hello")  # OK: Any accepts str
result3 = process([1, 2, 3])  # OK: Any accepts list
"#;
    documents
        .open_document(module_b_uri.clone(), 0, module_b_content.to_string())
        .unwrap();
    workspace.update_dependencies(&module_b_uri);

    let workspace_arc = Arc::new(RwLock::new(workspace));
    let mut analyzer = Analyzer::new(config, documents.clone());
    let diagnostic_provider = DiagnosticProvider::new(documents.clone(), workspace_arc);
    let diagnostics = diagnostic_provider.generate_diagnostics(&module_b_uri, &mut analyzer);

    let mismatch_diagnostics: Vec<_> = diagnostics
        .iter()
        .filter(|d| {
            d.code.as_ref().map_or(false, |c| match c {
                lsp_types::NumberOrString::String(s) => s == "argument-count-mismatch",
                _ => false,
            })
        })
        .collect();

    assert_eq!(
        mismatch_diagnostics.len(),
        0,
        "Expected no argument count mismatch diagnostics when using Any type. Found: {:?}",
        mismatch_diagnostics.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
}

#[tokio::test]
async fn test_untyped_imports_no_false_positives() {
    let documents = DocumentManager::new().unwrap();
    let config = Config::default();
    let workspace_root = Url::parse("file:///workspace").unwrap();
    let mut workspace = Workspace::new(Some(workspace_root.clone()), config.clone(), documents.clone());

    let module_a_uri = file_uri(workspace_root.as_str(), "untyped.py");
    let module_a_content = r#"
def do_something(x, y):
    return x + y
"#;
    documents
        .open_document(module_a_uri.clone(), 0, module_a_content.to_string())
        .unwrap();
    workspace.update_dependencies(&module_a_uri);

    let module_b_uri = file_uri(workspace_root.as_str(), "usage.py");
    let module_b_content = r#"
from untyped import do_something

result = do_something("a", "b")  # Should not error: no type info available
"#;
    documents
        .open_document(module_b_uri.clone(), 0, module_b_content.to_string())
        .unwrap();
    workspace.update_dependencies(&module_b_uri);

    let workspace_arc = Arc::new(RwLock::new(workspace));
    let mut analyzer = Analyzer::new(config, documents.clone());
    let diagnostic_provider = DiagnosticProvider::new(documents.clone(), workspace_arc);
    let diagnostics = diagnostic_provider.generate_diagnostics(&module_b_uri, &mut analyzer);

    let mismatch_diagnostics: Vec<_> = diagnostics
        .iter()
        .filter(|d| {
            d.code.as_ref().map_or(false, |c| match c {
                lsp_types::NumberOrString::String(s) => s == "argument-count-mismatch",
                _ => false,
            })
        })
        .collect();

    assert_eq!(
        mismatch_diagnostics.len(),
        0,
        "Expected no argument count mismatch diagnostics when type info unavailable. Found: {:?}",
        mismatch_diagnostics.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
}

#[tokio::test]
async fn test_complex_type_mismatch() {
    let documents = DocumentManager::new().unwrap();
    let config = Config::default();
    let workspace_root = Url::parse("file:///workspace").unwrap();
    let mut workspace = Workspace::new(Some(workspace_root.clone()), config.clone(), documents.clone());

    let module_a_uri = file_uri(workspace_root.as_str(), "collections.py");
    let module_a_content = r#"
from typing import List, Dict

def process_items(items: List[int]) -> int:
    return sum(items)

def merge_dicts(d1: Dict[str, int], d2: Dict[str, int]) -> Dict[str, int]:
    return {**d1, **d2}
"#;
    documents
        .open_document(module_a_uri.clone(), 0, module_a_content.to_string())
        .unwrap();
    workspace.update_dependencies(&module_a_uri);

    let module_b_uri = file_uri(workspace_root.as_str(), "app.py");
    let module_b_content = r#"
from collections import process_items, merge_dicts

total = process_items([1, 2, 3])  # OK
# Note: We might not detect list element type mismatches yet
# result = process_items(["a", "b"])  # Potential ERROR: List[str] instead of List[int]
"#;
    documents
        .open_document(module_b_uri.clone(), 0, module_b_content.to_string())
        .unwrap();
    workspace.update_dependencies(&module_b_uri);

    let workspace_arc = Arc::new(RwLock::new(workspace));
    let mut analyzer = Analyzer::new(config, documents.clone());
    let diagnostic_provider = DiagnosticProvider::new(documents.clone(), workspace_arc);
    let _diagnostics = diagnostic_provider.generate_diagnostics(&module_b_uri, &mut analyzer);
}

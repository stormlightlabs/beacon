//! Integration tests for magic method validation diagnostics

use beacon_lsp::{
    analysis::Analyzer, config::Config, document::DocumentManager, features::diagnostics::DiagnosticProvider,
    workspace::Workspace,
};
use std::sync::Arc;
use tokio::sync::RwLock;
use url::Url;

#[tokio::test]
async fn test_magic_method_with_correct_signature() {
    let documents = DocumentManager::new().unwrap();
    let config = Config::default();
    let workspace_root = Url::parse("file:///workspace").unwrap();
    let workspace = Workspace::new(Some(workspace_root.clone()), config.clone(), documents.clone());

    let test_uri = Url::parse("file:///workspace/test.py").unwrap();
    let content = r#"
class MyClass:
    def __init__(self, x):
        self.x = x

    def __str__(self):
        return str(self.x)

    def __eq__(self, other):
        return self.x == other.x
"#;

    documents
        .open_document(test_uri.clone(), 0, content.to_string())
        .unwrap();
    let workspace_arc = Arc::new(RwLock::new(workspace));
    let provider = DiagnosticProvider::new(documents.clone(), workspace_arc);
    let mut analyzer = Analyzer::new(config, documents.clone());

    let diagnostics = provider.generate_diagnostics(&test_uri, &mut analyzer);

    let magic_method_diagnostics: Vec<_> = diagnostics
        .iter()
        .filter(|d| {
            d.code.as_ref().map_or(false, |c| match c {
                lsp_types::NumberOrString::String(s) => s == "DUNDER002",
                _ => false,
            })
        })
        .collect();

    assert_eq!(
        magic_method_diagnostics.len(),
        0,
        "No magic method diagnostics should be generated for correct signatures"
    );
}

#[tokio::test]
async fn test_magic_method_with_wrong_parameter_count() {
    let documents = DocumentManager::new().unwrap();
    let config = Config::default();
    let workspace_root = Url::parse("file:///workspace").unwrap();
    let workspace = Workspace::new(Some(workspace_root.clone()), config.clone(), documents.clone());

    let test_uri = Url::parse("file:///workspace/test.py").unwrap();
    let content = r#"
class MyClass:
    def __str__(self, extra_param):
        return str(self.x)

    def __eq__(self):
        return True
"#;

    documents
        .open_document(test_uri.clone(), 0, content.to_string())
        .unwrap();
    let workspace_arc = Arc::new(RwLock::new(workspace));
    let provider = DiagnosticProvider::new(documents.clone(), workspace_arc);
    let mut analyzer = Analyzer::new(config, documents.clone());

    let diagnostics = provider.generate_diagnostics(&test_uri, &mut analyzer);

    let magic_method_diagnostics: Vec<_> = diagnostics
        .iter()
        .filter(|d| {
            d.code.as_ref().map_or(false, |c| match c {
                lsp_types::NumberOrString::String(s) => s == "DUNDER002",
                _ => false,
            })
        })
        .collect();

    assert_eq!(
        magic_method_diagnostics.len(),
        2,
        "Should find 2 magic method parameter count issues"
    );

    let str_diagnostic = magic_method_diagnostics
        .iter()
        .find(|d| d.message.contains("__str__"))
        .unwrap();
    assert!(str_diagnostic.message.contains("has 2 parameter"));
    assert!(str_diagnostic.message.contains("expected 1"));

    let eq_diagnostic = magic_method_diagnostics
        .iter()
        .find(|d| d.message.contains("__eq__"))
        .unwrap();
    assert!(eq_diagnostic.message.contains("has 1 parameter"));
    assert!(eq_diagnostic.message.contains("expected 2"));
}

#[tokio::test]
async fn test_magic_method_with_wrong_first_parameter() {
    let documents = DocumentManager::new().unwrap();
    let config = Config::default();
    let workspace_root = Url::parse("file:///workspace").unwrap();
    let workspace = Workspace::new(Some(workspace_root.clone()), config.clone(), documents.clone());

    let test_uri = Url::parse("file:///workspace/test.py").unwrap();
    let content = r#"
class MyClass:
    def __init__(this, x):
        this.x = x

    def __new__(self):
        return object.__new__(self)
"#;

    documents
        .open_document(test_uri.clone(), 0, content.to_string())
        .unwrap();
    let workspace_arc = Arc::new(RwLock::new(workspace));
    let provider = DiagnosticProvider::new(documents.clone(), workspace_arc);
    let mut analyzer = Analyzer::new(config, documents.clone());

    let diagnostics = provider.generate_diagnostics(&test_uri, &mut analyzer);

    let magic_method_diagnostics: Vec<_> = diagnostics
        .iter()
        .filter(|d| {
            d.code.as_ref().map_or(false, |c| match c {
                lsp_types::NumberOrString::String(s) => s == "DUNDER002",
                _ => false,
            })
        })
        .collect();

    assert_eq!(
        magic_method_diagnostics.len(),
        2,
        "Should find 2 magic method first parameter issues"
    );

    let init_diagnostic = magic_method_diagnostics
        .iter()
        .find(|d| d.message.contains("__init__"))
        .unwrap();
    assert!(init_diagnostic.message.contains("should be 'self'"));
    assert!(init_diagnostic.message.contains("not 'this'"));

    let new_diagnostic = magic_method_diagnostics
        .iter()
        .find(|d| d.message.contains("__new__"))
        .unwrap();
    assert!(new_diagnostic.message.contains("should be 'cls'"));
}

#[tokio::test]
async fn test_magic_method_outside_class() {
    let documents = DocumentManager::new().unwrap();
    let config = Config::default();
    let workspace_root = Url::parse("file:///workspace").unwrap();
    let workspace = Workspace::new(Some(workspace_root.clone()), config.clone(), documents.clone());

    let test_uri = Url::parse("file:///workspace/test.py").unwrap();
    let content = r#"
def __init__(self):
    pass

def __str__(self):
    return "test"
"#;

    documents
        .open_document(test_uri.clone(), 0, content.to_string())
        .unwrap();
    let workspace_arc = Arc::new(RwLock::new(workspace));
    let provider = DiagnosticProvider::new(documents.clone(), workspace_arc);
    let mut analyzer = Analyzer::new(config, documents.clone());

    let diagnostics = provider.generate_diagnostics(&test_uri, &mut analyzer);

    let dunder_diagnostics: Vec<_> = diagnostics
        .iter()
        .filter(|d| {
            d.code.as_ref().map_or(false, |c| match c {
                lsp_types::NumberOrString::String(s) => s == "DUNDER001",
                _ => false,
            })
        })
        .collect();

    assert_eq!(
        dunder_diagnostics.len(),
        2,
        "Should find 2 magic methods defined outside class"
    );

    for diag in &dunder_diagnostics {
        assert!(diag.message.contains("defined outside of a class"));
    }
}

#[tokio::test]
async fn test_variadic_magic_methods() {
    let documents = DocumentManager::new().unwrap();
    let config = Config::default();
    let workspace_root = Url::parse("file:///workspace").unwrap();
    let workspace = Workspace::new(Some(workspace_root.clone()), config.clone(), documents.clone());

    let test_uri = Url::parse("file:///workspace/test.py").unwrap();
    let content = r#"
class MyClass:
    def __init__(self):
        pass

    def __init__(self, x):
        self.x = x

    def __init__(self, x, y, z):
        self.x = x
        self.y = y
        self.z = z

    def __call__(self):
        pass

    def __call__(self, x, y):
        pass
"#;

    documents
        .open_document(test_uri.clone(), 0, content.to_string())
        .unwrap();
    let workspace_arc = Arc::new(RwLock::new(workspace));
    let provider = DiagnosticProvider::new(documents.clone(), workspace_arc);
    let mut analyzer = Analyzer::new(config, documents.clone());

    let diagnostics = provider.generate_diagnostics(&test_uri, &mut analyzer);

    let magic_method_diagnostics: Vec<_> = diagnostics
        .iter()
        .filter(|d| {
            d.code.as_ref().map_or(false, |c| match c {
                lsp_types::NumberOrString::String(s) => s == "DUNDER002",
                _ => false,
            })
        })
        .collect();

    assert_eq!(
        magic_method_diagnostics.len(),
        0,
        "Variadic magic methods (__init__, __call__) should accept any number of parameters >= minimum"
    );
}

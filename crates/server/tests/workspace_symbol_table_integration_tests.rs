//! Integration tests for symbol definition tracking
//!
//! Verifies that symbol definitions are correctly extracted during analysis
//! and can be queried through the cache manager.

use beacon_lsp::analysis::Analyzer;
use beacon_lsp::cache::{CacheManager, WorkspaceSymbolTable};
use beacon_lsp::config::Config;
use beacon_lsp::document::DocumentManager;
use std::sync::Arc;
use url::Url;

fn file_uri(path: &str) -> Url {
    Url::parse(&format!("file:///{path}")).unwrap()
}

#[test]
fn test_symbol_definitions_extracted_during_analysis() {
    let documents = DocumentManager::new().unwrap();
    let config = Config::default();
    let mut analyzer = Analyzer::new(config, documents.clone());

    let uri = file_uri("test.py");
    let source = r#"
class MyClass:
    pass

def my_function():
    pass

my_variable = 42
"#;

    documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

    let _result = analyzer.analyze(&uri);

    let symbols = analyzer.cache().get_file_symbols(&uri);
    assert!(!symbols.is_empty(), "Should have extracted symbol definitions");

    let symbol_names: Vec<String> = symbols.iter().map(|s| s.name.clone()).collect();
    assert!(symbol_names.contains(&"MyClass".to_string()), "Should contain MyClass");
    assert!(
        symbol_names.contains(&"my_function".to_string()),
        "Should contain my_function"
    );
    assert!(
        symbol_names.contains(&"my_variable".to_string()),
        "Should contain my_variable"
    );
}

#[test]
fn test_symbol_definitions_from_multiple_files() {
    let documents = DocumentManager::new().unwrap();
    let config = Config::default();
    let mut analyzer = Analyzer::new(config, documents.clone());

    let uri1 = file_uri("module_a.py");
    let uri2 = file_uri("module_b.py");

    let source1 = r#"
def func_a():
    return 42

class ClassA:
    pass
"#;

    let source2 = r#"
def func_b():
    return "hello"

class ClassB:
    pass
"#;

    documents.open_document(uri1.clone(), 1, source1.to_string()).unwrap();
    documents.open_document(uri2.clone(), 1, source2.to_string()).unwrap();

    let _ = analyzer.analyze(&uri1);
    let _ = analyzer.analyze(&uri2);

    let symbols_a = analyzer.cache().get_file_symbols(&uri1);
    assert!(symbols_a.len() >= 2, "Should have at least func_a and ClassA");

    let symbols_b = analyzer.cache().get_file_symbols(&uri2);
    assert!(symbols_b.len() >= 2, "Should have at least func_b and ClassB");

    let all_symbols = analyzer.cache().all_symbol_definitions();
    assert!(all_symbols.len() >= 4, "Should have symbols from both files");
}

#[test]
fn test_symbol_definition_details() {
    let documents = DocumentManager::new().unwrap();
    let config = Config::default();
    let mut analyzer = Analyzer::new(config, documents.clone());

    let uri = file_uri("test.py");
    let source = r#"
class MyClass:
    pass

def my_function():
    pass
"#;

    documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

    let _result = analyzer.analyze(&uri);

    let class_def = analyzer.cache().get_symbol_definition(&uri, "MyClass");
    assert!(class_def.is_some());
    let class_def = class_def.unwrap();
    assert_eq!(class_def.name, "MyClass");
    assert_eq!(class_def.kind, beacon_parser::SymbolKind::Class);
    assert_eq!(class_def.line, 2);

    let func_def = analyzer.cache().get_symbol_definition(&uri, "my_function");
    assert!(func_def.is_some());
    let func_def = func_def.unwrap();
    assert_eq!(func_def.name, "my_function");
    assert_eq!(func_def.kind, beacon_parser::SymbolKind::Function);
    assert_eq!(func_def.line, 5);
}

#[test]
fn test_symbol_definitions_cleared_on_invalidation() {
    let documents = DocumentManager::new().unwrap();
    let config = Config::default();
    let mut analyzer = Analyzer::new(config, documents.clone());

    let uri = file_uri("test.py");
    let source = r#"
def old_function():
    pass
"#;

    documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

    let _ = analyzer.analyze(&uri);

    assert!(analyzer.cache().get_symbol_definition(&uri, "old_function").is_some());

    analyzer.cache().invalidate_document(&uri);

    assert_eq!(
        analyzer.cache().get_file_symbols(&uri).len(),
        0,
        "Symbols should be cleared after invalidation"
    );
}

#[test]
fn test_nested_class_methods_tracked() {
    let documents = DocumentManager::new().unwrap();
    let config = Config::default();
    let mut analyzer = Analyzer::new(config, documents.clone());

    let uri = file_uri("test.py");
    let source = r#"
class MyClass:
    def method1(self):
        pass

    def method2(self):
        pass
"#;

    documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

    let _ = analyzer.analyze(&uri);

    let symbols = analyzer.cache().get_file_symbols(&uri);
    let symbol_names: Vec<String> = symbols.iter().map(|s| s.name.clone()).collect();

    assert!(symbol_names.contains(&"MyClass".to_string()));
    assert!(symbol_names.contains(&"method1".to_string()));
    assert!(symbol_names.contains(&"method2".to_string()));
}

#[test]
fn test_workspace_symbol_table_standalone() {
    let cache = Arc::new(CacheManager::new());
    let symbol_table = WorkspaceSymbolTable::new(Arc::clone(&cache));

    let uri1 = file_uri("module_a.py");
    let uri2 = file_uri("module_b.py");

    symbol_table.register_module("module_a".to_string(), uri1.clone());
    symbol_table.register_module("module_b".to_string(), uri2.clone());

    let scope_id = beacon_parser::ScopeId::from_raw(0);

    cache.add_symbol_definition(beacon_lsp::cache::SymbolDefinition::new(
        uri1.clone(),
        "func_a".to_string(),
        beacon_parser::SymbolKind::Function,
        10,
        0,
        scope_id,
    ));

    cache.add_symbol_definition(beacon_lsp::cache::SymbolDefinition::new(
        uri2.clone(),
        "ClassB".to_string(),
        beacon_parser::SymbolKind::Class,
        5,
        0,
        scope_id,
    ));

    let resolved_a = symbol_table.resolve_symbol("module_a", "func_a");
    assert!(resolved_a.is_some(), "Should resolve func_a from module_a");
    assert_eq!(resolved_a.unwrap().name, "func_a");

    let resolved_b = symbol_table.resolve_symbol("module_b", "ClassB");
    assert!(resolved_b.is_some(), "Should resolve ClassB from module_b");
    assert_eq!(resolved_b.unwrap().name, "ClassB");

    let results = symbol_table.search_symbols("func");
    assert_eq!(results.len(), 1);
    assert_eq!(results[0].name, "func_a");
}

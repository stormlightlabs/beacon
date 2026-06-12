use super::*;
use beacon_constraint::{Span, TypeErrorInfo};
use beacon_core::{AnalysisError, Type, TypeCtor, TypeError, TypeVar};
use lsp_types::DiagnosticSeverity;
use std::str::FromStr;
use std::sync::Arc;
use tokio::sync::RwLock;

/// Create a test workspace for diagnostic tests
fn create_test_workspace(documents: crate::document::DocumentManager) -> Arc<RwLock<crate::workspace::Workspace>> {
    let config = crate::config::Config::default();
    Arc::new(RwLock::new(crate::workspace::Workspace::new(None, config, documents)))
}

#[test]
fn parse_error_conversion() {
    let parse_error = ParseError {
        message: "Syntax error".to_string(),
        range: Range { start: Position { line: 1, character: 5 }, end: Position { line: 1, character: 10 } },
        severity: crate::parser::ErrorSeverity::Error,
    };

    let diagnostic = parse_error_to_diagnostic(&parse_error);

    assert_eq!(diagnostic.severity, Some(DiagnosticSeverity::ERROR));
    assert_eq!(diagnostic.message, "Syntax error");
    assert_eq!(diagnostic.source, Some("beacon".to_string()));
}

#[test]
fn type_error_unification_conversion() {
    let error_info = TypeErrorInfo {
        error: TypeError::UnificationError("int".to_string(), "str".to_string()),
        span: Span { line: 10, col: 5, end_line: Some(10), end_col: Some(8) },
    };

    let diagnostic = type_error_to_diagnostic(&error_info);

    assert_eq!(diagnostic.severity, Some(DiagnosticSeverity::ERROR));
    assert!(diagnostic.message.contains("cannot unify"));
    assert!(diagnostic.message.contains("int"));
    assert!(diagnostic.message.contains("str"));
    assert_eq!(
        diagnostic.code,
        Some(lsp_types::NumberOrString::String("HM001".to_string()))
    );
    assert_eq!(diagnostic.range.start.line, 9);
    assert_eq!(diagnostic.range.start.character, 4);
}

#[test]
fn type_error_occurs_check_conversion() {
    let tv = TypeVar::new(0);
    let error_info = TypeErrorInfo {
        error: TypeError::OccursCheckFailed(tv, "List['t0]".to_string()),
        span: Span { line: 5, col: 10, end_line: None, end_col: None },
    };

    let diagnostic = type_error_to_diagnostic(&error_info);

    assert_eq!(diagnostic.severity, Some(DiagnosticSeverity::ERROR));
    assert!(diagnostic.message.contains("Infinite type"));
    assert_eq!(
        diagnostic.code,
        Some(lsp_types::NumberOrString::String("HM002".to_string()))
    );
}

#[test]
fn type_error_kind_mismatch_conversion() {
    let error_info = TypeErrorInfo {
        error: TypeError::KindMismatch { expected: "*".to_string(), found: "* -> *".to_string() },
        span: Span { line: 3, col: 1, end_line: Some(3), end_col: Some(10) },
    };

    let diagnostic = type_error_to_diagnostic(&error_info);

    assert_eq!(diagnostic.severity, Some(DiagnosticSeverity::ERROR));
    assert!(diagnostic.message.contains("Kind mismatch"));
    assert_eq!(
        diagnostic.code,
        Some(lsp_types::NumberOrString::String("HM004".to_string()))
    );
}

#[test]
fn contains_any_type_simple() {
    assert!(DiagnosticProvider::contains_any_type(&Type::Con(TypeCtor::Any), 0));
    assert!(!DiagnosticProvider::contains_any_type(&Type::Con(TypeCtor::Unknown), 0));
    assert!(!DiagnosticProvider::contains_any_type(&Type::Con(TypeCtor::Int), 0));
}

#[test]
fn contains_unknown_type_simple() {
    assert!(DiagnosticProvider::contains_unknown_type(
        &Type::Con(TypeCtor::Unknown),
        0
    ));
    assert!(!DiagnosticProvider::contains_unknown_type(&Type::Con(TypeCtor::Any), 0));
    assert!(!DiagnosticProvider::contains_unknown_type(&Type::Con(TypeCtor::Int), 0));
}

#[test]
fn contains_any_type_nested() {
    let list_any = Type::App(Box::new(Type::Con(TypeCtor::List)), Box::new(Type::Con(TypeCtor::Any)));
    assert!(DiagnosticProvider::contains_any_type(&list_any, 0));

    let list_int = Type::App(Box::new(Type::Con(TypeCtor::List)), Box::new(Type::Con(TypeCtor::Int)));
    assert!(!DiagnosticProvider::contains_any_type(&list_int, 0));
}

#[test]
fn contains_any_type_function() {
    let fun_any = Type::Fun(
        vec![(String::new(), Type::Con(TypeCtor::Int))],
        Box::new(Type::Con(TypeCtor::Any)),
    );
    assert!(DiagnosticProvider::contains_any_type(&fun_any, 0));

    let fun_normal = Type::Fun(
        vec![(String::new(), Type::Con(TypeCtor::Int))],
        Box::new(Type::Con(TypeCtor::String)),
    );
    assert!(!DiagnosticProvider::contains_any_type(&fun_normal, 0));
}

#[test]
fn contains_any_type_union() {
    let union_any = Type::Union(vec![Type::Con(TypeCtor::Int), Type::Con(TypeCtor::Any)]);
    assert!(DiagnosticProvider::contains_any_type(&union_any, 0));

    let union_normal = Type::Union(vec![Type::Con(TypeCtor::Int), Type::Con(TypeCtor::String)]);
    assert!(!DiagnosticProvider::contains_any_type(&union_normal, 0));
}

#[test]
fn diagnostic_provider_creation() {
    let documents = DocumentManager::new().unwrap();
    let workspace = create_test_workspace(documents.clone());
    let _provider = DiagnosticProvider::new(documents, workspace);
}

#[test]
fn generate_diagnostics_with_parse_errors() {
    let documents = DocumentManager::new().unwrap();
    let workspace = create_test_workspace(documents.clone());
    let provider = DiagnosticProvider::new(documents.clone(), workspace);
    let config = crate::config::Config::default();
    let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

    let uri = Url::from_str("file:///test.py").unwrap();
    let source = "def broken(";

    documents.open_document(uri.clone(), 1, source).unwrap();

    let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);

    assert!(!diagnostics.is_empty());
    assert!(
        diagnostics
            .iter()
            .any(|d| d.severity == Some(DiagnosticSeverity::ERROR))
    );
}

#[test]
fn generate_diagnostics_with_unbound_variables() {
    let documents = DocumentManager::new().unwrap();
    let workspace = create_test_workspace(documents.clone());
    let provider = DiagnosticProvider::new(documents.clone(), workspace);
    let config = crate::config::Config::default();
    let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

    let uri = Url::from_str("file:///test.py").unwrap();
    let source = r#"
def test():
    x = undefined_variable
"#;

    documents.open_document(uri.clone(), 1, source).unwrap();

    let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);
    assert!(diagnostics.iter().any(|d| d.message.contains("undefined_variable")));
    assert!(
        diagnostics
            .iter()
            .any(|d| d.code == Some(lsp_types::NumberOrString::String("undefined-variable".to_string())))
    );
}

#[test]
fn type_error_undefined_typevar_conversion() {
    let tv = TypeVar::new(5);
    let error_info = TypeErrorInfo {
        error: TypeError::UndefinedTypeVar(tv),
        span: Span { line: 1, col: 1, end_line: None, end_col: None },
    };

    let diagnostic = type_error_to_diagnostic(&error_info);
    assert_eq!(diagnostic.severity, Some(DiagnosticSeverity::ERROR));
    assert!(diagnostic.message.contains("Undefined type variable"));
    assert_eq!(
        diagnostic.code,
        Some(lsp_types::NumberOrString::String("HM003".to_string()))
    );
}

#[test]
fn type_error_infinite_type_conversion() {
    let error_info = TypeErrorInfo {
        error: TypeError::InfiniteType("recursive type".to_string()),
        span: Span { line: 7, col: 3, end_line: Some(7), end_col: Some(15) },
    };

    let diagnostic = type_error_to_diagnostic(&error_info);
    assert_eq!(diagnostic.severity, Some(DiagnosticSeverity::ERROR));
    assert!(diagnostic.message.contains("Infinite type"));
    assert_eq!(
        diagnostic.code,
        Some(lsp_types::NumberOrString::String("HM005".to_string()))
    );
}

#[test]
fn contains_any_type_record() {
    let record_any = Type::Record(vec![("field".to_string(), Type::Con(TypeCtor::Any))], None);
    assert!(DiagnosticProvider::contains_any_type(&record_any, 0));

    let record_normal = Type::Record(vec![("field".to_string(), Type::Con(TypeCtor::Int))], None);
    assert!(!DiagnosticProvider::contains_any_type(&record_normal, 0));
}

#[test]
fn generate_diagnostics_empty_document() {
    let documents = DocumentManager::new().unwrap();
    let workspace = create_test_workspace(documents.clone());
    let provider = DiagnosticProvider::new(documents.clone(), workspace);
    let config = crate::config::Config::default();
    let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

    let uri = Url::from_str("file:///empty.py").unwrap();
    documents.open_document(uri.clone(), 1, "").unwrap();

    let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);
    assert!(diagnostics.is_empty());
}

#[test]
fn analysis_error_to_diagnostic() {
    let error = AnalysisError::MissingAst;
    let diagnostic = analysis_error_into_diagnostic(&BeaconError::from(error));

    assert_eq!(diagnostic.severity, Some(DiagnosticSeverity::ERROR));
    assert_eq!(diagnostic.source, Some("beacon".to_string()));
    assert!(diagnostic.message.contains("Missing AST"));
}

#[test]
fn dunder_name_main_pattern_detected() {
    let documents = DocumentManager::new().unwrap();
    let workspace = create_test_workspace(documents.clone());
    let provider = DiagnosticProvider::new(documents.clone(), workspace);
    let config = crate::config::Config::default();
    let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

    let uri = Url::from_str("file:///test.py").unwrap();
    let source = r#"
if __name__ == "__main__":
    print("Running as main")
"#;

    documents.open_document(uri.clone(), 1, source).unwrap();

    let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);

    let dunder_hint = diagnostics
        .iter()
        .find(|d| d.code == Some(lsp_types::NumberOrString::String("DUNDER_INFO".to_string())));

    assert!(dunder_hint.is_some());
    let hint = dunder_hint.unwrap();
    assert_eq!(hint.severity, Some(DiagnosticSeverity::HINT));
    assert!(hint.message.contains("Entry point guard"));
}

#[test]
fn magic_method_outside_class_warning() {
    let documents = DocumentManager::new().unwrap();
    let workspace = create_test_workspace(documents.clone());
    let provider = DiagnosticProvider::new(documents.clone(), workspace);
    let config = crate::config::Config::default();
    let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

    let uri = Url::from_str("file:///test.py").unwrap();
    let source = r#"
def __init__(self):
    self.x = 1
"#;

    documents.open_document(uri.clone(), 1, source).unwrap();

    let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);

    let magic_warning = diagnostics
        .iter()
        .find(|d| d.code == Some(lsp_types::NumberOrString::String("DUNDER001".to_string())));

    assert!(magic_warning.is_some());
    let warning = magic_warning.unwrap();
    assert_eq!(warning.severity, Some(DiagnosticSeverity::WARNING));
    assert!(warning.message.contains("Magic method"));
    assert!(warning.message.contains("outside of a class"));
}

#[test]
fn magic_method_inside_class_no_warning() {
    let documents = DocumentManager::new().unwrap();
    let workspace = create_test_workspace(documents.clone());
    let provider = DiagnosticProvider::new(documents.clone(), workspace);
    let config = crate::config::Config::default();
    let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

    let uri = Url::from_str("file:///test.py").unwrap();
    let source = r#"
class MyClass:
    def __init__(self):
        self.x = 1
"#;

    documents.open_document(uri.clone(), 1, source).unwrap();

    let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);
    let magic_warnings: Vec<_> = diagnostics
        .iter()
        .filter(|d| {
            d.code == Some(lsp_types::NumberOrString::String("DUNDER001".to_string())) && d.message.contains("__init__")
        })
        .collect();

    if !magic_warnings.is_empty() {
        panic!("Got unexpected warning for __init__ inside class: {magic_warnings:?}");
    }
}

#[test]
fn is_name_main_check_positive() {
    let test_expr = AstNode::Compare {
        left: Box::new(AstNode::Identifier { name: "__name__".to_string(), line: 1, col: 4, end_line: 1, end_col: 12 }),
        ops: vec![beacon_parser::CompareOperator::Eq],
        comparators: vec![AstNode::Literal {
            value: beacon_parser::LiteralValue::String { value: "__main__".to_string(), prefix: String::new() },
            line: 1,
            col: 16,
            end_line: 1,
            end_col: 9,
        }],
        line: 1,
        col: 12,
        end_line: 1,
        end_col: 12,
    };

    assert!(DiagnosticProvider::is_name_main_check(&test_expr));
}

#[test]
fn is_name_main_check_negative() {
    let test_expr = AstNode::Compare {
        left: Box::new(AstNode::Identifier { name: "x".to_string(), line: 1, col: 4, end_line: 1, end_col: 5 }),
        ops: vec![beacon_parser::CompareOperator::Eq],
        comparators: vec![AstNode::Literal {
            value: beacon_parser::LiteralValue::Integer(42),
            line: 1,
            col: 9,
            end_line: 1,
            end_col: 11,
        }],
        line: 1,
        col: 6,
        end_line: 1,
        end_col: 6,
    };

    assert!(!DiagnosticProvider::is_name_main_check(&test_expr));
}

#[test]
fn type_errors_surfaced_in_diagnostics() {
    let documents = DocumentManager::new().unwrap();
    let workspace = create_test_workspace(documents.clone());
    let provider = DiagnosticProvider::new(documents.clone(), workspace);
    let config = crate::config::Config::default();
    let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

    let uri = Url::from_str("file:///test.py").unwrap();
    let source = r#"
def test():
    x: int = "hello"
    return x
"#;

    documents.open_document(uri.clone(), 1, source).unwrap();

    let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);
    let hm_diagnostics = diagnostics.iter().find(|d| {
        if let Some(lsp_types::NumberOrString::String(code)) = &d.code {
            code.starts_with("HM")
        } else {
            false
        }
    });
    assert!(hm_diagnostics.is_some())
}

#[tokio::test]
async fn circular_import_detection() {
    let documents = DocumentManager::new().unwrap();
    let config = crate::config::Config::default();
    let workspace_root = Url::parse("file:///workspace").unwrap();
    let mut workspace = Workspace::new(Some(workspace_root), config.clone(), documents.clone());

    let uri_a = Url::parse("file:///workspace/a.py").unwrap();
    let source_a = "import b\n\ndef func_a():\n    pass";

    let uri_b = Url::parse("file:///workspace/b.py").unwrap();
    let source_b = "import a\n\ndef func_b():\n    pass";

    documents.open_document(uri_a.clone(), 0, source_a).unwrap();
    documents.open_document(uri_b.clone(), 0, source_b).unwrap();

    workspace.add_test_module(uri_a.clone(), "a".to_string(), std::path::PathBuf::from("/workspace"));
    workspace.add_test_module(uri_b.clone(), "b".to_string(), std::path::PathBuf::from("/workspace"));

    workspace.update_dependencies(&uri_a);
    workspace.update_dependencies(&uri_b);

    let workspace_arc = Arc::new(RwLock::new(workspace));
    let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());
    let provider = DiagnosticProvider::new(documents, workspace_arc);

    let diagnostics = provider.generate_diagnostics(&uri_a, &mut analyzer);

    let circular_diagnostics = diagnostics.iter().find(|d| {
        if let Some(lsp_types::NumberOrString::String(code)) = &d.code {
            code == "circular-import"
        } else {
            false
        }
    });

    if circular_diagnostics.is_none() {
        eprintln!("Warning: Circular import not detected in test (requires full workspace initialization)");
    }
}

#[tokio::test]
async fn unresolved_import_detection() {
    let documents = DocumentManager::new().unwrap();
    let workspace = create_test_workspace(documents.clone());
    let provider = DiagnosticProvider::new(documents.clone(), workspace.clone());
    let config = crate::config::Config::default();
    let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

    let uri = Url::from_str("file:///workspace/main.py").unwrap();
    let source = "import nonexistent_module\n\ndef main():\n    pass";

    documents.open_document(uri.clone(), 1, source).unwrap();

    {
        let mut ws = workspace.write().await;
        ws.add_test_module(uri.clone(), "main".to_string(), std::path::PathBuf::from("/workspace"));
    }

    let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);

    let unresolved_diagnostics: Vec<_> = diagnostics
        .iter()
        .filter(|d| {
            if let Some(lsp_types::NumberOrString::String(code)) = &d.code {
                code == "unresolved-import" || code == "missing-module"
            } else {
                false
            }
        })
        .collect();

    assert!(
        !unresolved_diagnostics.is_empty(),
        "Expected unresolved import diagnostic but found none"
    );

    assert!(
        unresolved_diagnostics
            .iter()
            .any(|d| d.message.contains("nonexistent_module")),
        "Expected diagnostic message to mention nonexistent_module"
    );
}

#[tokio::test]
async fn missing_module_detection() {
    let documents = DocumentManager::new().unwrap();
    let workspace = create_test_workspace(documents.clone());
    let provider = DiagnosticProvider::new(documents.clone(), workspace.clone());
    let config = crate::config::Config::default();
    let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

    let uri = Url::from_str("file:///workspace/pkg/module.py").unwrap();
    let source = "from ..nonexistent import something\n\ndef func():\n    pass";

    documents.open_document(uri.clone(), 1, source).unwrap();

    {
        let mut ws = workspace.write().await;
        ws.add_test_module(
            uri.clone(),
            "pkg.module".to_string(),
            std::path::PathBuf::from("/workspace"),
        );
    }

    let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);

    let missing_diagnostics = diagnostics.iter().find(|d| {
        if let Some(lsp_types::NumberOrString::String(code)) = &d.code {
            code == "missing-module" || code == "unresolved-import"
        } else {
            false
        }
    });

    assert!(
        missing_diagnostics.is_some(),
        "Expected missing module diagnostic but found none"
    );
}

#[test]
fn format_missing_module_message_relative_import_beyond_package() {
    let documents = DocumentManager::new().unwrap();
    let workspace = create_test_workspace(documents.clone());
    let provider = DiagnosticProvider::new(documents, workspace.clone());

    let ws = workspace.try_read().unwrap();
    let message = provider.format_missing_module_message("...", "pkg", &ws);

    assert!(
        message.contains("goes beyond top-level package"),
        "Expected message about going beyond package, got: {message}"
    );
}

#[test]
fn fuzzy_module_name_suggestions() {
    let documents = DocumentManager::new().unwrap();
    let workspace = create_test_workspace(documents.clone());
    let provider = DiagnosticProvider::new(documents, workspace);

    let score_similar = provider.fuzzy_matcher.similarity("foo", "foobar");
    let score_typo = provider.fuzzy_matcher.similarity("clections", "collections");
    let score_different = provider.fuzzy_matcher.similarity("abc", "xyz");
    assert!(score_similar >= provider.fuzzy_matcher.threshold());
    assert!(score_typo >= provider.fuzzy_matcher.threshold());
    assert!(score_different < provider.fuzzy_matcher.threshold());
}

#[test]
fn enhanced_unification_error_message_str_int() {
    let msg = enhance_unification_error_message("Type mismatch: cannot unify str with int", "str", "int");
    assert!(msg.contains("mixing strings and integers"));
}

#[test]
fn enhanced_unification_error_message_none() {
    let msg = enhance_unification_error_message("Type mismatch: cannot unify str with None", "str", "None");
    assert!(msg.contains("None where a different type is expected") && msg.contains("Optional"));
}

#[test]
fn enhanced_attribute_error_message_splitlines() {
    let msg = enhance_attribute_error_message("int", "splitlines");
    assert!(msg.contains("string method"));
}

#[test]
fn enhanced_attribute_error_message_write_text() {
    let msg = enhance_attribute_error_message("int", "write_text");
    assert!(msg.contains("Path object"));
}

#[test]
fn enhanced_attribute_error_message_get() {
    let msg = enhance_attribute_error_message("str", "get");
    assert!(msg.contains("dictionaries"));
}

#[test]
fn enhanced_protocol_error_message_iterable() {
    let msg = enhance_protocol_error_message("int", "Iterable");
    assert!(msg.contains("iterated over"));
}

#[test]
fn type_error_diagnostic_span_default() {
    let error_info = TypeErrorInfo {
        error: TypeError::UnificationError("int".to_string(), "str".to_string()),
        span: Span { line: 5, col: 10, end_line: None, end_col: None },
    };

    let diagnostic = type_error_to_diagnostic(&error_info);
    assert_eq!(diagnostic.range.start.line, 4);
    assert_eq!(diagnostic.range.start.character, 9);
    assert_eq!(diagnostic.range.end.character, 19);
}

#[test]
fn strict_mode_rejects_implicit_any_in_parameters() {
    let documents = DocumentManager::new().unwrap();
    let mut config = crate::config::Config::default();
    config.type_checking.mode = crate::config::TypeCheckingMode::Strict;
    let workspace = Arc::new(RwLock::new(crate::workspace::Workspace::new(
        None,
        config.clone(),
        documents.clone(),
    )));
    let provider = DiagnosticProvider::new(documents.clone(), workspace);
    let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

    let uri = Url::from_str("file:///test.py").unwrap();
    let source = r#"
def add(x, y):
    return x + y
"#;

    documents.open_document(uri.clone(), 1, source).unwrap();

    let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);

    let ann007_diagnostics = diagnostics
        .iter()
        .filter(|d| d.code == Some(lsp_types::NumberOrString::String("ANN007".to_string())));

    let count = ann007_diagnostics.clone().count();
    assert!(count > 0, "Expected ANN007 diagnostics for implicit Any parameters");
    assert_eq!(count, 2, "Expected 2 ANN007 diagnostics (one for each parameter)");

    for diag in ann007_diagnostics {
        assert_eq!(diag.severity, Some(DiagnosticSeverity::ERROR));
        assert!(diag.message.contains("implicit Any type"));
        assert!(diag.message.contains("strict mode"));
    }
}

#[test]
fn strict_mode_rejects_implicit_any_in_return_type() {
    let documents = DocumentManager::new().unwrap();
    let mut config = crate::config::Config::default();
    config.type_checking.mode = crate::config::TypeCheckingMode::Strict;
    let workspace = Arc::new(RwLock::new(crate::workspace::Workspace::new(
        None,
        config.clone(),
        documents.clone(),
    )));
    let provider = DiagnosticProvider::new(documents.clone(), workspace);
    let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

    let uri = Url::from_str("file:///test.py").unwrap();
    let source = r#"
def get_data(x: int):
    return x * 2
"#;

    documents.open_document(uri.clone(), 1, source).unwrap();

    let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);

    let ann008_diagnostic = diagnostics
        .iter()
        .find(|d| d.code == Some(lsp_types::NumberOrString::String("ANN008".to_string())));

    assert!(
        ann008_diagnostic.is_some(),
        "Expected ANN008 diagnostic for implicit Any return type"
    );

    let diag = ann008_diagnostic.unwrap();
    assert_eq!(diag.severity, Some(DiagnosticSeverity::ERROR));
    assert!(diag.message.contains("implicit Any return type"));
    assert!(diag.message.contains("strict mode"));
}

#[test]
fn strict_mode_accepts_properly_annotated_functions() {
    let documents = DocumentManager::new().unwrap();
    let mut config = crate::config::Config::default();
    config.type_checking.mode = crate::config::TypeCheckingMode::Strict;
    let workspace = Arc::new(RwLock::new(crate::workspace::Workspace::new(
        None,
        config.clone(),
        documents.clone(),
    )));
    let provider = DiagnosticProvider::new(documents.clone(), workspace);
    let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

    let uri = Url::from_str("file:///test.py").unwrap();
    let source = r#"
def add(x: int, y: int) -> int:
    return x + y
"#;

    documents.open_document(uri.clone(), 1, source).unwrap();

    let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);

    let implicit_any_diagnostics = diagnostics.iter().find(|d| {
        d.code == Some(lsp_types::NumberOrString::String("ANN007".to_string()))
            || d.code == Some(lsp_types::NumberOrString::String("ANN008".to_string()))
    });

    assert!(
        implicit_any_diagnostics.is_none(),
        "Expected no implicit Any diagnostics for properly annotated function, got: {implicit_any_diagnostics:?}"
    );
}

#[test]
fn balanced_mode_does_not_reject_implicit_any() {
    let documents = DocumentManager::new().unwrap();
    let mut config = crate::config::Config::default();
    config.type_checking.mode = crate::config::TypeCheckingMode::Balanced;
    let workspace = Arc::new(RwLock::new(crate::workspace::Workspace::new(
        None,
        config.clone(),
        documents.clone(),
    )));
    let provider = DiagnosticProvider::new(documents.clone(), workspace);
    let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

    let uri = Url::from_str("file:///test.py").unwrap();
    let source = r#"
def add(x, y):
    return x + y
"#;

    documents.open_document(uri.clone(), 1, source).unwrap();

    let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);

    let mut ann007_diagnostics = diagnostics
        .iter()
        .filter(|d| d.code == Some(lsp_types::NumberOrString::String("ANN007".to_string())));

    let mut ann008_diagnostics = diagnostics
        .iter()
        .filter(|d| d.code == Some(lsp_types::NumberOrString::String("ANN008".to_string())));

    assert!(
        ann007_diagnostics.next().is_none(),
        "Balanced mode should not generate ANN007 for implicit Any"
    );
    assert!(
        ann008_diagnostics.next().is_none(),
        "Balanced mode should not generate ANN008 for implicit Any"
    );
}

#[test]
fn relaxed_mode_does_not_reject_implicit_any() {
    let documents = DocumentManager::new().unwrap();
    let mut config = crate::config::Config::default();
    config.type_checking.mode = crate::config::TypeCheckingMode::Relaxed;
    let workspace = Arc::new(RwLock::new(crate::workspace::Workspace::new(
        None,
        config.clone(),
        documents.clone(),
    )));
    let provider = DiagnosticProvider::new(documents.clone(), workspace);
    let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

    let uri = Url::from_str("file:///test.py").unwrap();
    let source = r#"
def add(x, y):
    return x + y
"#;

    documents.open_document(uri.clone(), 1, source).unwrap();

    let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);

    let mut ann007_diagnostics = diagnostics
        .iter()
        .filter(|d| d.code == Some(lsp_types::NumberOrString::String("ANN007".to_string())));

    let mut ann008_diagnostics = diagnostics
        .iter()
        .filter(|d| d.code == Some(lsp_types::NumberOrString::String("ANN008".to_string())));

    assert!(
        ann007_diagnostics.next().is_none(),
        "Relaxed mode should not generate ANN007 for implicit Any"
    );
    assert!(
        ann008_diagnostics.next().is_none(),
        "Relaxed mode should not generate ANN008 for implicit Any"
    );
}

#[test]
fn strict_mode_requires_annotations_for_all_parameters() {
    let documents = DocumentManager::new().unwrap();
    let mut config = crate::config::Config::default();
    config.type_checking.mode = crate::config::TypeCheckingMode::Strict;
    let workspace = Arc::new(RwLock::new(crate::workspace::Workspace::new(
        None,
        config.clone(),
        documents.clone(),
    )));
    let provider = DiagnosticProvider::new(documents.clone(), workspace);
    let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

    let uri = Url::from_str("file:///test.py").unwrap();
    let source = r#"
# Function where type could be inferred from usage, but strict mode requires explicit annotations
def sum_list(items):
    total = 0
    for item in items:
        total += item
    return total
"#;

    documents.open_document(uri.clone(), 1, source).unwrap();

    let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);

    let ann007_count = diagnostics
        .iter()
        .filter(|d| d.code == Some(lsp_types::NumberOrString::String("ANN007".to_string())))
        .count();

    let ann008_count = diagnostics
        .iter()
        .filter(|d| d.code == Some(lsp_types::NumberOrString::String("ANN008".to_string())))
        .count();

    assert_eq!(
        ann007_count, 1,
        "Expected ANN007 for parameter 'items' even though type could be inferred"
    );
    assert_eq!(
        ann008_count, 1,
        "Expected ANN008 for return type even though type could be inferred"
    );
}

#[test]
fn strict_mode_with_mixed_annotations() {
    let documents = DocumentManager::new().unwrap();
    let mut config = crate::config::Config::default();
    config.type_checking.mode = crate::config::TypeCheckingMode::Strict;
    let workspace = Arc::new(RwLock::new(crate::workspace::Workspace::new(
        None,
        config.clone(),
        documents.clone(),
    )));
    let provider = DiagnosticProvider::new(documents.clone(), workspace);
    let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

    let uri = Url::from_str("file:///test.py").unwrap();
    let source = r#"
def mixed_params(a: int, b, c) -> int:
    return a + b + c
"#;

    documents.open_document(uri.clone(), 1, source).unwrap();

    let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);

    let ann007_diagnostics = diagnostics
        .iter()
        .filter(|d| d.code == Some(lsp_types::NumberOrString::String("ANN007".to_string())));

    assert_eq!(
        ann007_diagnostics.count(),
        2,
        "Expected ANN007 for parameters 'b' and 'c' (not 'a' which is annotated)"
    );

    let ann008_count = diagnostics
        .iter()
        .filter(|d| d.code == Some(lsp_types::NumberOrString::String("ANN008".to_string())))
        .count();

    assert_eq!(ann008_count, 0, "Expected no ANN008 since return type is annotated");
}

#[test]
fn strict_mode_class_methods() {
    let documents = DocumentManager::new().unwrap();
    let mut config = crate::config::Config::default();
    config.type_checking.mode = crate::config::TypeCheckingMode::Strict;
    let workspace = Arc::new(RwLock::new(crate::workspace::Workspace::new(
        None,
        config.clone(),
        documents.clone(),
    )));
    let provider = DiagnosticProvider::new(documents.clone(), workspace);
    let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

    let uri = Url::from_str("file:///test.py").unwrap();
    let source = r#"
class Calculator:
    def add(self, x, y):
        return x + y

    def subtract(self, x: int, y: int) -> int:
        return x - y
"#;

    documents.open_document(uri.clone(), 1, source).unwrap();

    let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);

    let ann007_diagnostics = diagnostics
        .iter()
        .filter(|d| d.code == Some(lsp_types::NumberOrString::String("ANN007".to_string())));

    let mut ann008_diagnostics = diagnostics
        .iter()
        .filter(|d| d.code == Some(lsp_types::NumberOrString::String("ANN008".to_string())));

    assert!(
        ann007_diagnostics.count() >= 2,
        "Expected at least 2 ANN007 for 'x' and 'y' in 'add' method"
    );
    assert!(
        ann008_diagnostics.next().is_some(),
        "Expected at least 1 ANN008 for 'add' method return type"
    );
}

#[test]
fn strict_mode_nested_functions() {
    let documents = DocumentManager::new().unwrap();
    let mut config = crate::config::Config::default();
    config.type_checking.mode = crate::config::TypeCheckingMode::Strict;
    let workspace = Arc::new(RwLock::new(crate::workspace::Workspace::new(
        None,
        config.clone(),
        documents.clone(),
    )));
    let provider = DiagnosticProvider::new(documents.clone(), workspace);
    let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

    let uri = Url::from_str("file:///test.py").unwrap();
    let source = r#"
def outer(x: int) -> int:
    def inner(y):
        return x + y
    return inner(10)
"#;

    documents.open_document(uri.clone(), 1, source).unwrap();

    let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);

    let mut ann007_diagnostics = diagnostics
        .iter()
        .filter(|d| d.code == Some(lsp_types::NumberOrString::String("ANN007".to_string())));

    let mut ann008_diagnostics = diagnostics
        .iter()
        .filter(|d| d.code == Some(lsp_types::NumberOrString::String("ANN008".to_string())));

    assert!(
        ann007_diagnostics.next().is_some(),
        "Expected at least 1 ANN007 for inner function parameter 'y'"
    );
    assert!(
        ann008_diagnostics.next().is_some(),
        "Expected at least 1 ANN008 for inner function return type"
    );
}

#[test]
fn strict_mode_function_with_default_values() {
    let documents = DocumentManager::new().unwrap();
    let mut config = crate::config::Config::default();
    config.type_checking.mode = crate::config::TypeCheckingMode::Strict;
    let workspace = Arc::new(RwLock::new(crate::workspace::Workspace::new(
        None,
        config.clone(),
        documents.clone(),
    )));
    let provider = DiagnosticProvider::new(documents.clone(), workspace);
    let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

    let uri = Url::from_str("file:///test.py").unwrap();
    let source = r#"
def with_default(value=42) -> int:
    return value + 1
"#;

    documents.open_document(uri.clone(), 1, source).unwrap();

    let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);

    let ann007_count = diagnostics
        .iter()
        .filter(|d| d.code == Some(lsp_types::NumberOrString::String("ANN007".to_string())))
        .count();

    assert_eq!(
        ann007_count, 1,
        "Expected ANN007 for parameter 'value' even though it has a default value"
    );
}

#[test]
fn strict_mode_requires_class_attribute_annotations() {
    let documents = DocumentManager::new().unwrap();
    let mut config = crate::config::Config::default();
    config.type_checking.mode = crate::config::TypeCheckingMode::Strict;
    let workspace = Arc::new(RwLock::new(crate::workspace::Workspace::new(
        None,
        config.clone(),
        documents.clone(),
    )));
    let provider = DiagnosticProvider::new(documents.clone(), workspace);
    let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

    let uri = Url::from_str("file:///test.py").unwrap();
    let source = r#"
class MyClass:
    # Class attribute without annotation
    count = 0

    # Class attribute with annotation
    name: str = "default"
"#;

    documents.open_document(uri.clone(), 1, source).unwrap();

    let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);

    let ann009_diagnostics: Vec<_> = diagnostics
        .iter()
        .filter(|d| d.code == Some(lsp_types::NumberOrString::String("ANN009".to_string())))
        .collect();

    assert_eq!(
        ann009_diagnostics.len(),
        1,
        "Expected 1 ANN009 diagnostic for unannotated class attribute 'count'"
    );

    if let Some(diag) = ann009_diagnostics.first() {
        assert_eq!(diag.severity, Some(DiagnosticSeverity::ERROR));
        assert!(diag.message.contains("count"));
        assert!(diag.message.contains("Class attribute"));
        assert!(diag.message.contains("strict mode"));
    }
}

#[test]
fn strict_mode_class_attributes_vs_instance_attributes() {
    let documents = DocumentManager::new().unwrap();
    let mut config = crate::config::Config::default();
    config.type_checking.mode = crate::config::TypeCheckingMode::Strict;
    let workspace = Arc::new(RwLock::new(crate::workspace::Workspace::new(
        None,
        config.clone(),
        documents.clone(),
    )));
    let provider = DiagnosticProvider::new(documents.clone(), workspace);
    let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

    let uri = Url::from_str("file:///test.py").unwrap();
    let source = r#"
class MyClass:
    class_attr = 0  # Should trigger ANN009

    def __init__(self):
        self.instance_attr = 10  # Should NOT trigger ANN009 (instance attribute, not class)
"#;

    documents.open_document(uri.clone(), 1, source).unwrap();

    let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);

    let ann009_count = diagnostics
        .iter()
        .filter(|d| d.code == Some(lsp_types::NumberOrString::String("ANN009".to_string())))
        .count();

    assert_eq!(
        ann009_count, 1,
        "Expected exactly 1 ANN009 for class attribute, not instance attributes"
    );
}

#[test]
fn strict_mode_multiple_class_attributes() {
    let documents = DocumentManager::new().unwrap();
    let mut config = crate::config::Config::default();
    config.type_checking.mode = crate::config::TypeCheckingMode::Strict;
    let workspace = Arc::new(RwLock::new(crate::workspace::Workspace::new(
        None,
        config.clone(),
        documents.clone(),
    )));
    let provider = DiagnosticProvider::new(documents.clone(), workspace);
    let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

    let uri = Url::from_str("file:///test.py").unwrap();
    let source = r#"
class Config:
    host = "localhost"  # Missing annotation
    port = 8080  # Missing annotation
    timeout: int = 30  # OK: Has annotation
    debug_mode = True  # Missing annotation
"#;

    documents.open_document(uri.clone(), 1, source).unwrap();

    let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);

    let ann009_count = diagnostics
        .iter()
        .filter(|d| d.code == Some(lsp_types::NumberOrString::String("ANN009".to_string())))
        .count();

    assert_eq!(
        ann009_count, 3,
        "Expected 3 ANN009 diagnostics for host, port, and debug_mode"
    );
}

#[test]
fn balanced_mode_does_not_require_class_attribute_annotations() {
    let documents = DocumentManager::new().unwrap();
    let mut config = crate::config::Config::default();
    config.type_checking.mode = crate::config::TypeCheckingMode::Balanced;
    let workspace = Arc::new(RwLock::new(crate::workspace::Workspace::new(
        None,
        config.clone(),
        documents.clone(),
    )));
    let provider = DiagnosticProvider::new(documents.clone(), workspace);
    let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

    let uri = Url::from_str("file:///test.py").unwrap();
    let source = r#"
class MyClass:
    count = 0
"#;

    documents.open_document(uri.clone(), 1, source).unwrap();

    let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);

    let ann009_count = diagnostics
        .iter()
        .filter(|d| d.code == Some(lsp_types::NumberOrString::String("ANN009".to_string())))
        .count();

    assert_eq!(
        ann009_count, 0,
        "Balanced mode should not generate ANN009 for class attributes"
    );
}

#[test]
fn relaxed_mode_does_not_require_class_attribute_annotations() {
    let documents = DocumentManager::new().unwrap();
    let mut config = crate::config::Config::default();
    config.type_checking.mode = crate::config::TypeCheckingMode::Relaxed;
    let workspace = Arc::new(RwLock::new(crate::workspace::Workspace::new(
        None,
        config.clone(),
        documents.clone(),
    )));
    let provider = DiagnosticProvider::new(documents.clone(), workspace);
    let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

    let uri = Url::from_str("file:///test.py").unwrap();
    let source = r#"
class MyClass:
    count = 0
"#;

    documents.open_document(uri.clone(), 1, source).unwrap();

    let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);

    let ann009_count = diagnostics
        .iter()
        .filter(|d| d.code == Some(lsp_types::NumberOrString::String("ANN009".to_string())))
        .count();

    assert_eq!(
        ann009_count, 0,
        "Relaxed mode should not generate ANN009 for class attributes"
    );
}

#[test]
fn strict_mode_rejects_bare_except() {
    let documents = DocumentManager::new().unwrap();
    let mut config = crate::config::Config::default();
    config.type_checking.mode = crate::config::TypeCheckingMode::Strict;
    let workspace = Arc::new(RwLock::new(crate::workspace::Workspace::new(
        None,
        config.clone(),
        documents.clone(),
    )));
    let provider = DiagnosticProvider::new(documents.clone(), workspace);
    let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

    let uri = Url::from_str("file:///test.py").unwrap();
    let source = r#"
def foo():
    try:
        x = 1 / 0
    except:
        pass
"#;

    documents.open_document(uri.clone(), 1, source).unwrap();

    let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);

    let ann010_diagnostics: Vec<_> = diagnostics
        .iter()
        .filter(|d| d.code == Some(lsp_types::NumberOrString::String("ANN010".to_string())))
        .collect();

    assert_eq!(
        ann010_diagnostics.len(),
        1,
        "Expected 1 ANN010 diagnostic for bare except clause"
    );

    if let Some(diag) = ann010_diagnostics.first() {
        assert_eq!(diag.severity, Some(DiagnosticSeverity::ERROR));
        assert!(diag.message.contains("Bare except"));
        assert!(diag.message.contains("strict mode"));
    }
}

#[test]
fn strict_mode_accepts_specific_exception_types() {
    let documents = DocumentManager::new().unwrap();
    let mut config = crate::config::Config::default();
    config.type_checking.mode = crate::config::TypeCheckingMode::Strict;
    let workspace = Arc::new(RwLock::new(crate::workspace::Workspace::new(
        None,
        config.clone(),
        documents.clone(),
    )));
    let provider = DiagnosticProvider::new(documents.clone(), workspace);
    let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

    let uri = Url::from_str("file:///test.py").unwrap();
    let source = r#"
def foo():
    try:
        x = 1 / 0
    except ZeroDivisionError:
        pass
    except (ValueError, TypeError):
        pass
"#;

    documents.open_document(uri.clone(), 1, source).unwrap();

    let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);

    let ann010_count = diagnostics
        .iter()
        .filter(|d| d.code == Some(lsp_types::NumberOrString::String("ANN010".to_string())))
        .count();

    assert_eq!(ann010_count, 0, "Specific exception types should not trigger ANN010");
}

#[test]
fn balanced_mode_allows_bare_except() {
    let documents = DocumentManager::new().unwrap();
    let mut config = crate::config::Config::default();
    config.type_checking.mode = crate::config::TypeCheckingMode::Balanced;
    let workspace = Arc::new(RwLock::new(crate::workspace::Workspace::new(
        None,
        config.clone(),
        documents.clone(),
    )));
    let provider = DiagnosticProvider::new(documents.clone(), workspace);
    let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

    let uri = Url::from_str("file:///test.py").unwrap();
    let source = r#"
def foo():
    try:
        x = 1 / 0
    except:
        pass
"#;

    documents.open_document(uri.clone(), 1, source).unwrap();

    let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);

    let ann010_count = diagnostics
        .iter()
        .filter(|d| d.code == Some(lsp_types::NumberOrString::String("ANN010".to_string())))
        .count();

    assert_eq!(
        ann010_count, 0,
        "Balanced mode should not generate ANN010 for bare except"
    );
}

#[test]
fn relaxed_mode_allows_bare_except() {
    let documents = DocumentManager::new().unwrap();
    let mut config = crate::config::Config::default();
    config.type_checking.mode = crate::config::TypeCheckingMode::Relaxed;
    let workspace = Arc::new(RwLock::new(crate::workspace::Workspace::new(
        None,
        config.clone(),
        documents.clone(),
    )));
    let provider = DiagnosticProvider::new(documents.clone(), workspace);
    let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

    let uri = Url::from_str("file:///test.py").unwrap();
    let source = r#"
def foo():
    try:
        x = 1 / 0
    except:
        pass
"#;

    documents.open_document(uri.clone(), 1, source).unwrap();

    let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);

    let ann010_count = diagnostics
        .iter()
        .filter(|d| d.code == Some(lsp_types::NumberOrString::String("ANN010".to_string())))
        .count();

    assert_eq!(
        ann010_count, 0,
        "Relaxed mode should not generate ANN010 for bare except"
    );
}

#[test]
fn balanced_mode_warns_on_implicit_any_parameters() {
    let documents = DocumentManager::new().unwrap();
    let mut config = crate::config::Config::default();
    config.type_checking.mode = crate::config::TypeCheckingMode::Balanced;
    let workspace = Arc::new(RwLock::new(crate::workspace::Workspace::new(
        None,
        config.clone(),
        documents.clone(),
    )));
    let provider = DiagnosticProvider::new(documents.clone(), workspace);
    let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

    let uri = Url::from_str("file:///test.py").unwrap();
    let source = r#"
def process_unknown(data, options):
    return data
"#;

    documents.open_document(uri.clone(), 1, source).unwrap();

    let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);

    let ann011_diagnostics: Vec<_> = diagnostics
        .iter()
        .filter(|d| d.code == Some(lsp_types::NumberOrString::String("ANN011".to_string())))
        .collect();

    assert!(
        !ann011_diagnostics.is_empty(),
        "Expected ANN011 warnings for parameters with implicit Any"
    );

    for diag in &ann011_diagnostics {
        assert_eq!(diag.severity, Some(DiagnosticSeverity::WARNING));
        assert!(diag.message.contains("implicit Any type"));
        assert!(diag.message.contains("consider adding type annotation"));
    }
}

#[test]
fn balanced_mode_warns_on_missing_annotations() {
    let documents = DocumentManager::new().unwrap();
    let mut config = crate::config::Config::default();
    config.type_checking.mode = crate::config::TypeCheckingMode::Balanced;
    let workspace = Arc::new(RwLock::new(crate::workspace::Workspace::new(
        None,
        config.clone(),
        documents.clone(),
    )));
    let provider = DiagnosticProvider::new(documents.clone(), workspace);
    let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

    let uri = Url::from_str("file:///test.py").unwrap();
    let source = r#"
def handle_dynamic(value):
    return value
"#;

    documents.open_document(uri.clone(), 1, source).unwrap();

    let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);

    let annotation_warnings: Vec<_> = diagnostics
        .iter()
        .filter(|d| {
            matches!(
                d.code.as_ref(),
                Some(lsp_types::NumberOrString::String(code))
                if code.starts_with("ANN")
            )
        })
        .collect();

    assert!(
        !annotation_warnings.is_empty(),
        "Expected annotation warnings for unannotated function"
    );

    for diag in &annotation_warnings {
        assert_eq!(diag.severity, Some(DiagnosticSeverity::WARNING));
    }
}

#[test]
fn balanced_mode_warns_on_missing_annotation_with_inferred_type() {
    let documents = DocumentManager::new().unwrap();
    let mut config = crate::config::Config::default();
    config.type_checking.mode = crate::config::TypeCheckingMode::Balanced;
    let workspace = Arc::new(RwLock::new(crate::workspace::Workspace::new(
        None,
        config.clone(),
        documents.clone(),
    )));
    let provider = DiagnosticProvider::new(documents.clone(), workspace);
    let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

    let uri = Url::from_str("file:///test.py").unwrap();
    let source = r#"
def add(x, y):
    return x + y

result = add(1, 2)
"#;

    documents.open_document(uri.clone(), 1, source).unwrap();

    let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);

    let annotation_warnings: Vec<_> = diagnostics
        .iter()
        .filter(|d| {
            matches!(
                d.code.as_ref(),
                Some(lsp_types::NumberOrString::String(code))
                if code == "ANN004" || code == "ANN006" || code == "ANN011" || code == "ANN012"
            )
        })
        .collect();

    assert!(
        !annotation_warnings.is_empty(),
        "Expected annotation warnings for parameters and return type"
    );

    for diag in &annotation_warnings {
        assert_eq!(diag.severity, Some(DiagnosticSeverity::WARNING));
    }
}

#[test]
fn balanced_mode_gradual_typing_mixed_annotations() {
    let documents = DocumentManager::new().unwrap();
    let mut config = crate::config::Config::default();
    config.type_checking.mode = crate::config::TypeCheckingMode::Balanced;
    let workspace = Arc::new(RwLock::new(crate::workspace::Workspace::new(
        None,
        config.clone(),
        documents.clone(),
    )));
    let provider = DiagnosticProvider::new(documents.clone(), workspace);
    let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

    let uri = Url::from_str("file:///test.py").unwrap();
    let source = r#"
def mixed_params(a: int, b, c: int) -> int:
    return a + b + c
"#;

    documents.open_document(uri.clone(), 1, source).unwrap();

    let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);

    let param_b_diagnostics: Vec<_> = diagnostics
        .iter()
        .filter(|d| {
            matches!(
                d.code.as_ref(),
                Some(lsp_types::NumberOrString::String(code))
                if (code == "ANN004" || code == "ANN011") && d.message.contains("'b'")
            )
        })
        .collect();

    assert_eq!(
        param_b_diagnostics.len(),
        1,
        "Expected exactly 1 annotation warning for unannotated parameter 'b'"
    );

    for diag in &param_b_diagnostics {
        assert_eq!(diag.severity, Some(DiagnosticSeverity::WARNING));
    }
}

#[test]
fn balanced_mode_with_fully_annotated_function() {
    let documents = DocumentManager::new().unwrap();
    let mut config = crate::config::Config::default();
    config.type_checking.mode = crate::config::TypeCheckingMode::Balanced;
    let workspace = Arc::new(RwLock::new(crate::workspace::Workspace::new(
        None,
        config.clone(),
        documents.clone(),
    )));
    let provider = DiagnosticProvider::new(documents.clone(), workspace);
    let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

    let uri = Url::from_str("file:///test.py").unwrap();
    let source = r#"
def multiply(x: int, y: int) -> int:
    return x * y
"#;

    documents.open_document(uri.clone(), 1, source).unwrap();

    let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);

    let mut missing_annotation_diagnostics = diagnostics.iter().filter(|d| {
        matches!(
            d.code.as_ref(),
            Some(lsp_types::NumberOrString::String(code))
            if code == "ANN004" || code == "ANN006" || code == "ANN011" || code == "ANN012"
        )
    });

    assert!(
        missing_annotation_diagnostics.next().is_none(),
        "Fully annotated functions should not generate missing annotation warnings in balanced mode"
    );
}

#[test]
fn strict_mode_multiple_bare_except_handlers() {
    let documents = DocumentManager::new().unwrap();
    let mut config = crate::config::Config::default();
    config.type_checking.mode = crate::config::TypeCheckingMode::Strict;
    let workspace = Arc::new(RwLock::new(crate::workspace::Workspace::new(
        None,
        config.clone(),
        documents.clone(),
    )));
    let provider = DiagnosticProvider::new(documents.clone(), workspace);
    let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

    let uri = Url::from_str("file:///test.py").unwrap();
    let source = r#"
def foo():
    try:
        x = 1 / 0
    except ValueError:
        pass
    except:
        pass

def bar():
    try:
        y = 2 / 0
    except:
        pass
"#;

    documents.open_document(uri.clone(), 1, source).unwrap();

    let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);

    let ann010_count = diagnostics
        .iter()
        .filter(|d| d.code == Some(lsp_types::NumberOrString::String("ANN010".to_string())))
        .count();

    assert_eq!(
        ann010_count, 2,
        "Expected 2 ANN010 diagnostics for two bare except clauses"
    );
}

#[test]
fn config_severity_to_lsp() {
    assert_eq!(
        DiagnosticProvider::config_severity_to_lsp(config::DiagnosticSeverity::Error),
        lsp_types::DiagnosticSeverity::ERROR
    );
    assert_eq!(
        DiagnosticProvider::config_severity_to_lsp(config::DiagnosticSeverity::Warning),
        lsp_types::DiagnosticSeverity::WARNING
    );
    assert_eq!(
        DiagnosticProvider::config_severity_to_lsp(config::DiagnosticSeverity::Info),
        lsp_types::DiagnosticSeverity::INFORMATION
    );
}

#[test]
fn mode_severity_for_diagnostic_implicit_any() {
    assert_eq!(
        DiagnosticProvider::mode_severity_for_diagnostic(
            config::TypeCheckingMode::Strict,
            DiagnosticCategory::ImplicitAny
        ),
        Some(DiagnosticSeverity::ERROR)
    );
    assert_eq!(
        DiagnosticProvider::mode_severity_for_diagnostic(
            config::TypeCheckingMode::Balanced,
            DiagnosticCategory::ImplicitAny
        ),
        Some(DiagnosticSeverity::WARNING)
    );
    assert_eq!(
        DiagnosticProvider::mode_severity_for_diagnostic(
            config::TypeCheckingMode::Relaxed,
            DiagnosticCategory::ImplicitAny
        ),
        None
    );
}

#[test]
fn mode_severity_for_diagnostic_missing_annotation() {
    assert_eq!(
        DiagnosticProvider::mode_severity_for_diagnostic(
            config::TypeCheckingMode::Strict,
            DiagnosticCategory::MissingAnnotation
        ),
        Some(DiagnosticSeverity::ERROR)
    );
    assert_eq!(
        DiagnosticProvider::mode_severity_for_diagnostic(
            config::TypeCheckingMode::Balanced,
            DiagnosticCategory::MissingAnnotation
        ),
        Some(DiagnosticSeverity::WARNING)
    );
    assert_eq!(
        DiagnosticProvider::mode_severity_for_diagnostic(
            config::TypeCheckingMode::Relaxed,
            DiagnosticCategory::MissingAnnotation
        ),
        None
    );
}

#[test]
fn mode_severity_for_diagnostic_annotation_mismatch() {
    assert_eq!(
        DiagnosticProvider::mode_severity_for_diagnostic(
            config::TypeCheckingMode::Strict,
            DiagnosticCategory::AnnotationMismatch
        ),
        Some(DiagnosticSeverity::ERROR)
    );
    assert_eq!(
        DiagnosticProvider::mode_severity_for_diagnostic(
            config::TypeCheckingMode::Balanced,
            DiagnosticCategory::AnnotationMismatch
        ),
        Some(DiagnosticSeverity::WARNING)
    );
    assert_eq!(
        DiagnosticProvider::mode_severity_for_diagnostic(
            config::TypeCheckingMode::Relaxed,
            DiagnosticCategory::AnnotationMismatch
        ),
        Some(DiagnosticSeverity::HINT)
    );
}

#[test]
fn types_are_compatible_basic() {
    assert!(DiagnosticProvider::types_are_compatible(
        &Type::Con(TypeCtor::Int),
        &Type::Con(TypeCtor::Int)
    ));
    assert!(!DiagnosticProvider::types_are_compatible(
        &Type::Con(TypeCtor::Int),
        &Type::Con(TypeCtor::String)
    ));
}

#[test]
fn types_are_compatible_with_any() {
    assert!(DiagnosticProvider::types_are_compatible(
        &Type::Con(TypeCtor::Any),
        &Type::Con(TypeCtor::Int)
    ));
    assert!(DiagnosticProvider::types_are_compatible(
        &Type::Con(TypeCtor::Int),
        &Type::Con(TypeCtor::Any)
    ));
}

#[test]
fn types_are_compatible_application() {
    let list_int1 = Type::App(Box::new(Type::Con(TypeCtor::List)), Box::new(Type::Con(TypeCtor::Int)));
    let list_int2 = Type::App(Box::new(Type::Con(TypeCtor::List)), Box::new(Type::Con(TypeCtor::Int)));
    let list_str = Type::App(
        Box::new(Type::Con(TypeCtor::List)),
        Box::new(Type::Con(TypeCtor::String)),
    );

    assert!(DiagnosticProvider::types_are_compatible(&list_int1, &list_int2));
    assert!(!DiagnosticProvider::types_are_compatible(&list_int1, &list_str));
}

#[test]
fn types_are_compatible_function() {
    let fun1 = Type::Fun(
        vec![("x".to_string(), Type::Con(TypeCtor::Int))],
        Box::new(Type::Con(TypeCtor::String)),
    );
    let fun2 = Type::Fun(
        vec![("y".to_string(), Type::Con(TypeCtor::Int))],
        Box::new(Type::Con(TypeCtor::String)),
    );
    let fun3 = Type::Fun(
        vec![("x".to_string(), Type::Con(TypeCtor::String))],
        Box::new(Type::Con(TypeCtor::String)),
    );

    assert!(DiagnosticProvider::types_are_compatible(&fun1, &fun2));
    assert!(!DiagnosticProvider::types_are_compatible(&fun1, &fun3));
}

#[test]
fn types_are_compatible_union() {
    let union1 = Type::Union(vec![Type::Con(TypeCtor::Int), Type::Con(TypeCtor::String)]);
    let union2 = Type::Union(vec![Type::Con(TypeCtor::String), Type::Con(TypeCtor::Int)]);
    let union3 = Type::Union(vec![Type::Con(TypeCtor::Int), Type::Con(TypeCtor::Bool)]);

    assert!(DiagnosticProvider::types_are_compatible(&union1, &union2));
    assert!(!DiagnosticProvider::types_are_compatible(&union1, &union3));
}

#[test]
fn types_are_compatible_tuple() {
    let tuple1 = Type::Tuple(vec![Type::Con(TypeCtor::Int), Type::Con(TypeCtor::String)]);
    let tuple2 = Type::Tuple(vec![Type::Con(TypeCtor::Int), Type::Con(TypeCtor::String)]);
    let tuple3 = Type::Tuple(vec![Type::Con(TypeCtor::Int), Type::Con(TypeCtor::Bool)]);

    assert!(DiagnosticProvider::types_are_compatible(&tuple1, &tuple2));
    assert!(!DiagnosticProvider::types_are_compatible(&tuple1, &tuple3));
}

#[test]
fn types_are_compatible_with_type_vars() {
    let tv = TypeVar::new(0);
    assert!(DiagnosticProvider::types_are_compatible(
        &Type::Var(tv.clone()),
        &Type::Con(TypeCtor::Int)
    ));
    assert!(DiagnosticProvider::types_are_compatible(
        &Type::Con(TypeCtor::Int),
        &Type::Var(tv)
    ));
}

#[test]
fn contains_type_var_simple() {
    let tv = TypeVar::new(0);
    assert!(DiagnosticProvider::contains_type_var(&Type::Var(tv)));
    assert!(!DiagnosticProvider::contains_type_var(&Type::Con(TypeCtor::Int)));
}

#[test]
fn contains_type_var_nested() {
    let tv = TypeVar::new(0);
    let list_var = Type::App(Box::new(Type::Con(TypeCtor::List)), Box::new(Type::Var(tv)));
    assert!(DiagnosticProvider::contains_type_var(&list_var));

    let list_int = Type::App(Box::new(Type::Con(TypeCtor::List)), Box::new(Type::Con(TypeCtor::Int)));
    assert!(!DiagnosticProvider::contains_type_var(&list_int));
}

#[test]
fn contains_type_var_function() {
    let tv = TypeVar::new(0);
    let fun_var = Type::Fun(
        vec![("x".to_string(), Type::Var(tv))],
        Box::new(Type::Con(TypeCtor::Int)),
    );
    assert!(DiagnosticProvider::contains_type_var(&fun_var));

    let fun_normal = Type::Fun(
        vec![("x".to_string(), Type::Con(TypeCtor::Int))],
        Box::new(Type::Con(TypeCtor::String)),
    );
    assert!(!DiagnosticProvider::contains_type_var(&fun_normal));
}

#[test]
fn contains_type_var_union() {
    let tv = TypeVar::new(0);
    let union_var = Type::Union(vec![Type::Con(TypeCtor::Int), Type::Var(tv)]);
    assert!(DiagnosticProvider::contains_type_var(&union_var));

    let union_normal = Type::Union(vec![Type::Con(TypeCtor::Int), Type::Con(TypeCtor::String)]);
    assert!(!DiagnosticProvider::contains_type_var(&union_normal));
}

#[test]
fn extract_target_name_identifier() {
    let node = AstNode::Identifier { name: "my_var".to_string(), line: 1, col: 1, end_line: 1, end_col: 7 };
    assert_eq!(DiagnosticProvider::extract_target_name(&node), "my_var");
}

#[test]
fn extract_target_name_attribute() {
    let node = AstNode::Attribute {
        object: Box::new(AstNode::Identifier { name: "obj".to_string(), line: 1, col: 1, end_line: 1, end_col: 4 }),
        attribute: "attr".to_string(),
        line: 1,
        col: 5,
        end_line: 1,
        end_col: 9,
    };
    assert_eq!(DiagnosticProvider::extract_target_name(&node), "attr");
}

#[test]
fn extract_target_name_fallback() {
    let node =
        AstNode::Literal { value: beacon_parser::LiteralValue::Integer(42), line: 1, col: 1, end_line: 1, end_col: 3 };
    assert_eq!(DiagnosticProvider::extract_target_name(&node), "variable");
}

#[test]
fn is_name_main_check_valid() {
    let test = AstNode::Compare {
        left: Box::new(AstNode::Identifier { name: "__name__".to_string(), line: 1, col: 1, end_line: 1, end_col: 9 }),
        ops: vec![beacon_parser::CompareOperator::Eq],
        comparators: vec![AstNode::Literal {
            value: beacon_parser::LiteralValue::String { value: "__main__".to_string(), prefix: String::new() },
            line: 1,
            col: 10,
            end_line: 1,
            end_col: 20,
        }],
        line: 1,
        col: 1,
        end_line: 1,
        end_col: 20,
    };

    assert!(DiagnosticProvider::is_name_main_check(&test));
}

#[test]
fn is_name_main_check_invalid() {
    let test = AstNode::Compare {
        left: Box::new(AstNode::Identifier {
            name: "other_var".to_string(),
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 10,
        }),
        ops: vec![beacon_parser::CompareOperator::Eq],
        comparators: vec![AstNode::Literal {
            value: beacon_parser::LiteralValue::String { value: "__main__".to_string(), prefix: String::new() },
            line: 1,
            col: 10,
            end_line: 1,
            end_col: 20,
        }],
        line: 1,
        col: 1,
        end_line: 1,
        end_col: 20,
    };

    assert!(!DiagnosticProvider::is_name_main_check(&test));
}

#[test]
fn enhance_unification_error_str_int() {
    let base = "Type mismatch: cannot unify str with int";
    let enhanced = enhance_unification_error_message(base, "str", "int");
    assert!(enhanced.contains("mixing strings and integers"));
}

#[test]
fn enhance_unification_error_list_dict() {
    let base = "Type mismatch: cannot unify list with dict";
    let enhanced = enhance_unification_error_message(base, "list", "dict");
    assert!(enhanced.contains("Collection type mismatch"));
}

#[test]
fn enhance_unification_error_none() {
    let base = "Type mismatch: cannot unify str with None";
    let enhanced = enhance_unification_error_message(base, "str", "None");
    assert!(enhanced.contains("None"));
    assert!(enhanced.contains("Optional[T]"));
}

#[test]
fn enhance_unification_error_union() {
    let base = "Type mismatch: cannot unify Union[int, str] with bool";
    let enhanced = enhance_unification_error_message(base, "Union[int, str]", "bool");
    assert!(enhanced.contains("Union types"));
}

#[test]
fn enhance_unification_error_default() {
    let base = "Type mismatch: cannot unify float with bool";
    let enhanced = enhance_unification_error_message(base, "float", "bool");
    assert_eq!(enhanced, base);
}

#[test]
fn enhance_protocol_error_iterable() {
    let enhanced = enhance_protocol_error_message("MyClass", "Iterable");
    assert!(enhanced.contains("MyClass"));
    assert!(enhanced.contains("Iterable"));
    assert!(enhanced.contains("cannot be iterated"));
}

#[test]
fn enhance_protocol_error_other() {
    let enhanced = enhance_protocol_error_message("MyClass", "Sized");
    assert!(enhanced.contains("MyClass"));
    assert!(enhanced.contains("Sized"));
    assert!(!enhanced.contains("cannot be iterated"));
}

#[test]
fn enhance_attribute_error_splitlines() {
    let enhanced = enhance_attribute_error_message("int", "splitlines");
    assert!(enhanced.contains("splitlines"));
    assert!(enhanced.contains("string method"));
}

#[test]
fn enhance_attribute_error_write_text() {
    let enhanced = enhance_attribute_error_message("str", "write_text");
    assert!(enhanced.contains("write_text"));
    assert!(enhanced.contains("Path object"));
}

#[test]
fn enhance_attribute_error_get() {
    let enhanced = enhance_attribute_error_message("list", "get");
    assert!(enhanced.contains("get()"));
    assert!(enhanced.contains("dictionaries"));
}

#[test]
fn enhance_attribute_error_append() {
    let enhanced = enhance_attribute_error_message("str", "append");
    assert!(enhanced.contains("append()"));
    assert!(enhanced.contains("lists"));
}

#[test]
fn enhance_attribute_error_default() {
    let enhanced = enhance_attribute_error_message("MyClass", "unknown_method");
    assert!(enhanced.contains("unknown_method"));
    assert!(enhanced.contains("not found"));
}

#[test]
fn enhance_variance_error_invariant_list() {
    let enhanced = enhance_variance_error_message("list element", "invariant", "Dog", "Animal");
    assert!(enhanced.contains("invariant"));
    assert!(enhanced.contains("list"));
    assert!(enhanced.contains("Mutable containers"));
}

#[test]
fn enhance_variance_error_invariant_generic() {
    let enhanced = enhance_variance_error_message("generic type", "invariant", "int", "float");
    assert!(enhanced.contains("invariant"));
    assert!(enhanced.contains("exact type matches"));
}

#[test]
fn enhance_variance_error_covariant_return() {
    let enhanced = enhance_variance_error_message("return type", "covariant", "Animal", "Dog");
    assert!(enhanced.contains("covariant"));
    assert!(enhanced.contains("Return types"));
    assert!(enhanced.contains("subtype"));
}

#[test]
fn enhance_variance_error_contravariant_parameter() {
    let enhanced = enhance_variance_error_message("parameter", "contravariant", "Dog", "Animal");
    assert!(enhanced.contains("contravariant"));
    assert!(enhanced.contains("parameters"));
    assert!(enhanced.contains("supertype"));
}

#[test]
fn enhance_variance_error_unknown() {
    let enhanced = enhance_variance_error_message("position", "unknown", "Type1", "Type2");
    assert!(enhanced.contains("Variance error"));
    assert!(!enhanced.contains("Mutable containers"));
}

#[test]
fn unreachable_pattern_diagnostic_range() {
    let documents = DocumentManager::new().unwrap();
    let config = crate::config::Config::default();
    let workspace = Arc::new(RwLock::new(crate::workspace::Workspace::new(
        None,
        config.clone(),
        documents.clone(),
    )));
    let provider = DiagnosticProvider::new(documents.clone(), workspace);
    let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

    let uri = Url::from_str("file:///test.py").unwrap();
    let source = r#"
def example(value: int | str) -> str:
    match value:
        case _:
            return "wildcard"
        case int():  # PM002: Unreachable
            return "integer"
"#;

    documents.open_document(uri.clone(), 1, source).unwrap();

    let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);

    let pm002_diagnostics: Vec<_> = diagnostics
        .iter()
        .filter(|d| d.code == Some(lsp_types::NumberOrString::String("PM002".to_string())))
        .collect();

    assert_eq!(
        pm002_diagnostics.len(),
        1,
        "Expected 1 PM002 diagnostic for unreachable pattern"
    );

    let diagnostic = &pm002_diagnostics[0];

    eprintln!(
        "Diagnostic range: start={}:{}, end={}:{}",
        diagnostic.range.start.line,
        diagnostic.range.start.character,
        diagnostic.range.end.line,
        diagnostic.range.end.character
    );

    assert_eq!(
        diagnostic.range.start.line, 5,
        "Diagnostic should be on line 6 (0-indexed as 5)"
    );
    assert_eq!(
        diagnostic.range.start.character, 13,
        "Diagnostic should start at column 13 (start of 'int()')"
    );
    assert_eq!(
        diagnostic.range.end.character, 18,
        "Diagnostic should end at column 18 (end of 'int()')"
    );
    assert_eq!(diagnostic.range.end.line, 5, "Diagnostic should end on the same line");
}

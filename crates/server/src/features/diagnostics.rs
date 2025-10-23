//! Diagnostic generation and publishing
//!
//! Converts type errors, parse errors, and other analysis results into LSP diagnostics for display in the editor.

use beacon_core::BeaconError;
use lsp_types::{Diagnostic, DiagnosticSeverity, Position, Range};
use url::Url;

use crate::analysis::Analyzer;
use crate::document::DocumentManager;
use crate::parser::ParseError;

pub struct DiagnosticProvider {
    documents: DocumentManager,
}

impl DiagnosticProvider {
    pub fn new(documents: DocumentManager) -> Self {
        Self { documents }
    }

    /// Generate diagnostics for a document
    ///
    /// Combines syntax errors, type errors, and other analysis issues.
    pub fn generate_diagnostics(&self, uri: &Url, analyzer: &mut Analyzer) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();
        self.add_parse_errors(uri, &mut diagnostics);
        self.add_unbound_variable_errors(uri, analyzer, &mut diagnostics);
        self.add_type_errors(uri, analyzer, &mut diagnostics);
        self.add_unsafe_any_warnings(uri, analyzer, &mut diagnostics);
        self.add_annotation_mismatch_warnings(uri, analyzer, &mut diagnostics);

        diagnostics
    }

    /// Add parse errors as diagnostics
    fn add_parse_errors(&self, uri: &Url, diagnostics: &mut Vec<Diagnostic>) {
        self.documents.get_document(uri, |doc| {
            if let Some(parse_result) = &doc.parse_result {
                for error in &parse_result.errors {
                    diagnostics.push(parse_error_to_diagnostic(error));
                }
            }
        });
    }

    /// Add type errors as diagnostics
    fn add_type_errors(&self, uri: &Url, analyzer: &mut Analyzer, diagnostics: &mut Vec<Diagnostic>) {
        match analyzer.analyze(uri) {
            Ok(result) => {
                // Convert type errors from analysis result
                for type_error_info in &result.type_errors {
                    diagnostics.push(type_error_to_diagnostic(type_error_info));
                }
            }
            Err(e) => diagnostics.push(analysis_error_to_diagnostic(&e)),
        }
    }

    /// Add unbound variable errors as diagnostics
    fn add_unbound_variable_errors(&self, uri: &Url, analyzer: &Analyzer, diagnostics: &mut Vec<Diagnostic>) {
        let unbound = analyzer.find_unbound_variables(uri);

        for (name, line, col) in unbound {
            let position =
                Position { line: (line.saturating_sub(1)) as u32, character: (col.saturating_sub(1)) as u32 };

            let range = Range {
                start: position,
                end: Position { line: position.line, character: position.character + name.len() as u32 },
            };

            diagnostics.push(Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::ERROR),
                code: Some(lsp_types::NumberOrString::String("undefined-variable".to_string())),
                source: Some("beacon".to_string()),
                message: format!("Undefined variable '{}'", name),
                related_information: None,
                tags: None,
                data: None,
                code_description: None,
            });
        }
    }

    /// Check for unsafe Any type usage that exceeds configured depth
    fn add_unsafe_any_warnings(&self, uri: &Url, analyzer: &mut Analyzer, diagnostics: &mut Vec<Diagnostic>) {
        let result = match analyzer.analyze(uri) {
            Ok(r) => r,
            Err(_) => return,
        };

        // TODO: Track Any propagation depth through the type map
        // TODO: flow tracking is implemented
        // TODO: Get actual position from node_id
        for (_node_id, ty) in &result.type_map {
            if self.contains_any_type(ty, 0) {
                let diagnostic = Diagnostic {
                    range: Range { start: Position { line: 0, character: 0 }, end: Position { line: 0, character: 1 } },
                    severity: Some(DiagnosticSeverity::WARNING),
                    code: Some(lsp_types::NumberOrString::String("ANY001".to_string())),
                    source: Some("beacon".to_string()),
                    message: "Type 'Any' detected - this reduces type safety".to_string(),
                    related_information: None,
                    tags: None,
                    data: None,
                    code_description: None,
                };
                diagnostics.push(diagnostic);
                break;
            }
        }
    }

    fn contains_any_type(&self, ty: &beacon_core::Type, _depth: u32) -> bool {
        use beacon_core::{Type, TypeCtor};

        match ty {
            Type::Con(TypeCtor::Any) => true,
            Type::App(t1, t2) => self.contains_any_type(t1, _depth + 1) || self.contains_any_type(t2, _depth + 1),
            Type::Fun(args, ret) => {
                args.iter().any(|arg| self.contains_any_type(arg, _depth + 1))
                    || self.contains_any_type(ret, _depth + 1)
            }
            Type::Union(types) => types.iter().any(|t| self.contains_any_type(t, _depth + 1)),
            Type::Record(fields, _) => fields.iter().any(|(_, t)| self.contains_any_type(t, _depth + 1)),
            _ => false,
        }
    }

    /// Check annotation mismatches based on config mode
    fn add_annotation_mismatch_warnings(&self, uri: &Url, analyzer: &mut Analyzer, _diagnostics: &mut Vec<Diagnostic>) {
        let result = match analyzer.analyze(uri) {
            Ok(r) => r,
            Err(_) => return,
        };

        // TODO: Get config mode from analyzer or pass as parameter
        // For now, use Balanced mode as default
        let mode = crate::config::TypeCheckingMode::Balanced;

        // TODO: Extract annotations from AST and compare with inferred types
        // This requires:
        // 1. Walking the AST to find annotated assignments/parameters
        // 2. Looking up the inferred type from result.type_map
        // 3. Comparing annotation with inference
        // 4. Generating appropriate diagnostic based on mode

        let _type_map = &result.type_map;
        let _ = mode;

        // TODO: Implement annotation checking when AST includes annotation info
    }
}

/// Convert a parse error to an LSP diagnostic
fn parse_error_to_diagnostic(error: &ParseError) -> Diagnostic {
    Diagnostic {
        range: error.range,
        severity: Some(match error.severity {
            crate::parser::ErrorSeverity::Error => DiagnosticSeverity::ERROR,
            crate::parser::ErrorSeverity::Warning => DiagnosticSeverity::WARNING,
            crate::parser::ErrorSeverity::Hint => DiagnosticSeverity::HINT,
        }),
        code: None,
        code_description: None,
        source: Some("beacon".to_string()),
        message: error.message.clone(),
        related_information: None,
        tags: None,
        data: None,
    }
}

/// Convert an analysis error to an LSP diagnostic
fn analysis_error_to_diagnostic(error: &BeaconError) -> Diagnostic {
    Diagnostic {
        range: Range { start: Position { line: 0, character: 0 }, end: Position { line: 0, character: 0 } },
        severity: Some(DiagnosticSeverity::ERROR),
        code: None,
        code_description: None,
        source: Some("beacon".to_string()),
        message: error.to_string(),
        related_information: None,
        tags: None,
        data: None,
    }
}

/// Convert a type error with location info to an LSP diagnostic
fn type_error_to_diagnostic(error_info: &crate::analysis::TypeErrorInfo) -> Diagnostic {
    use beacon_core::TypeError;

    let start_pos = Position {
        line: (error_info.line.saturating_sub(1)) as u32,
        character: (error_info.col.saturating_sub(1)) as u32,
    };

    let end_pos = match (error_info.end_line, error_info.end_col) {
        (Some(end_line), Some(end_col)) => {
            Position { line: (end_line.saturating_sub(1)) as u32, character: (end_col.saturating_sub(1)) as u32 }
        }
        _ => Position { line: start_pos.line, character: start_pos.character + 1 },
    };

    let range = Range { start: start_pos, end: end_pos };

    let (code, message) = match &error_info.error {
        TypeError::UnificationError(t1, t2) => ("HM001", format!("Type mismatch: cannot unify {} with {}", t1, t2)),
        TypeError::OccursCheckFailed(tv, ty) => {
            ("HM002", format!("Infinite type: type variable {} occurs in {}", tv, ty))
        }
        TypeError::UndefinedTypeVar(tv) => ("HM003", format!("Undefined type variable: {}", tv)),
        TypeError::KindMismatch { expected, found } => (
            "HM004",
            format!("Kind mismatch: expected {}, found {}", expected, found),
        ),
        TypeError::InfiniteType(msg) => ("HM005", format!("Infinite type: {}", msg)),
    };

    Diagnostic {
        range,
        severity: Some(DiagnosticSeverity::ERROR),
        code: Some(lsp_types::NumberOrString::String(code.to_string())),
        source: Some("beacon".to_string()),
        message,
        related_information: None,
        tags: None,
        data: None,
        code_description: None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use beacon_core::{AnalysisError, Type, TypeCtor, TypeError, TypeVar};
    use std::str::FromStr;

    #[test]
    fn test_parse_error_conversion() {
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
    fn test_type_error_unification_conversion() {
        let error_info = crate::analysis::TypeErrorInfo {
            error: TypeError::UnificationError("int".to_string(), "str".to_string()),
            line: 10,
            col: 5,
            end_line: Some(10),
            end_col: Some(8),
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
        assert_eq!(diagnostic.range.start.line, 9); // 0-indexed
        assert_eq!(diagnostic.range.start.character, 4); // 0-indexed
    }

    #[test]
    fn test_type_error_occurs_check_conversion() {
        let tv = TypeVar::new(0);
        let error_info = crate::analysis::TypeErrorInfo {
            error: TypeError::OccursCheckFailed(tv.clone(), "List['t0]".to_string()),
            line: 5,
            col: 10,
            end_line: None,
            end_col: None,
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
    fn test_type_error_kind_mismatch_conversion() {
        let error_info = crate::analysis::TypeErrorInfo {
            error: TypeError::KindMismatch { expected: "*".to_string(), found: "* -> *".to_string() },
            line: 3,
            col: 1,
            end_line: Some(3),
            end_col: Some(10),
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
    fn test_contains_any_type_simple() {
        let documents = DocumentManager::new().unwrap();
        let provider = DiagnosticProvider::new(documents);

        assert!(provider.contains_any_type(&Type::Con(TypeCtor::Any), 0));
        assert!(!provider.contains_any_type(&Type::Con(TypeCtor::Int), 0));
    }

    #[test]
    fn test_contains_any_type_nested() {
        let documents = DocumentManager::new().unwrap();
        let provider = DiagnosticProvider::new(documents);

        // List[Any]
        let list_any = Type::App(Box::new(Type::Con(TypeCtor::List)), Box::new(Type::Con(TypeCtor::Any)));
        assert!(provider.contains_any_type(&list_any, 0));

        // List[int]
        let list_int = Type::App(Box::new(Type::Con(TypeCtor::List)), Box::new(Type::Con(TypeCtor::Int)));
        assert!(!provider.contains_any_type(&list_int, 0));
    }

    #[test]
    fn test_contains_any_type_function() {
        let documents = DocumentManager::new().unwrap();
        let provider = DiagnosticProvider::new(documents);

        // int -> Any
        let fun_any = Type::Fun(vec![Type::Con(TypeCtor::Int)], Box::new(Type::Con(TypeCtor::Any)));
        assert!(provider.contains_any_type(&fun_any, 0));

        // int -> str
        let fun_normal = Type::Fun(vec![Type::Con(TypeCtor::Int)], Box::new(Type::Con(TypeCtor::String)));
        assert!(!provider.contains_any_type(&fun_normal, 0));
    }

    #[test]
    fn test_contains_any_type_union() {
        let documents = DocumentManager::new().unwrap();
        let provider = DiagnosticProvider::new(documents);

        // int | Any
        let union_any = Type::Union(vec![Type::Con(TypeCtor::Int), Type::Con(TypeCtor::Any)]);
        assert!(provider.contains_any_type(&union_any, 0));

        // int | str
        let union_normal = Type::Union(vec![Type::Con(TypeCtor::Int), Type::Con(TypeCtor::String)]);
        assert!(!provider.contains_any_type(&union_normal, 0));
    }

    #[test]
    fn test_diagnostic_provider_creation() {
        let documents = DocumentManager::new().unwrap();
        let _provider = DiagnosticProvider::new(documents);
    }

    #[test]
    fn test_generate_diagnostics_with_parse_errors() {
        use std::str::FromStr;

        let documents = DocumentManager::new().unwrap();
        let provider = DiagnosticProvider::new(documents.clone());
        let config = crate::config::Config::default();
        let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = "def broken("; // Syntax error

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);

        assert!(!diagnostics.is_empty());
        assert!(
            diagnostics
                .iter()
                .any(|d| d.severity == Some(DiagnosticSeverity::ERROR))
        );
    }

    #[test]
    fn test_generate_diagnostics_with_unbound_variables() {
        use std::str::FromStr;

        let documents = DocumentManager::new().unwrap();
        let provider = DiagnosticProvider::new(documents.clone());
        let config = crate::config::Config::default();
        let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
def test():
    x = undefined_variable
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);
        assert!(diagnostics.iter().any(|d| d.message.contains("undefined_variable")));
        assert!(
            diagnostics
                .iter()
                .any(|d| d.code == Some(lsp_types::NumberOrString::String("undefined-variable".to_string())))
        );
    }

    #[test]
    fn test_type_error_undefined_typevar_conversion() {
        let tv = TypeVar::new(5);
        let error_info = crate::analysis::TypeErrorInfo {
            error: TypeError::UndefinedTypeVar(tv.clone()),
            line: 1,
            col: 1,
            end_line: None,
            end_col: None,
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
    fn test_type_error_infinite_type_conversion() {
        let error_info = crate::analysis::TypeErrorInfo {
            error: TypeError::InfiniteType("recursive type".to_string()),
            line: 7,
            col: 3,
            end_line: Some(7),
            end_col: Some(15),
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
    fn test_contains_any_type_record() {
        let documents = DocumentManager::new().unwrap();
        let provider = DiagnosticProvider::new(documents);

        let record_any = Type::Record(vec![("field".to_string(), Type::Con(TypeCtor::Any))], None);
        assert!(provider.contains_any_type(&record_any, 0));

        let record_normal = Type::Record(vec![("field".to_string(), Type::Con(TypeCtor::Int))], None);
        assert!(!provider.contains_any_type(&record_normal, 0));
    }

    #[test]
    fn test_generate_diagnostics_empty_document() {
        let documents = DocumentManager::new().unwrap();
        let provider = DiagnosticProvider::new(documents.clone());
        let config = crate::config::Config::default();
        let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

        let uri = Url::from_str("file:///empty.py").unwrap();
        documents.open_document(uri.clone(), 1, "".to_string()).unwrap();

        let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);
        assert!(diagnostics.is_empty());
    }

    #[test]
    fn test_analysis_error_to_diagnostic() {
        let error = AnalysisError::MissingAst;
        let diagnostic = analysis_error_to_diagnostic(&BeaconError::from(error));

        assert_eq!(diagnostic.severity, Some(DiagnosticSeverity::ERROR));
        assert_eq!(diagnostic.source, Some("beacon".to_string()));
        assert!(diagnostic.message.contains("Missing AST"));
    }
}

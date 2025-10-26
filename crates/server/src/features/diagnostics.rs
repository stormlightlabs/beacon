//! Diagnostic generation and publishing
//!
//! Converts type errors, parse errors, and other analysis results into LSP diagnostics for display in the editor.

use beacon_core::BeaconError;
use beacon_parser::{AstNode, MAGIC_METHODS};
use lsp_types::{Diagnostic, DiagnosticSeverity, Position, Range};
use url::Url;

use crate::analysis::Analyzer;
use crate::config;
use crate::document::DocumentManager;
use crate::parser::{self, ParseError};

pub struct DiagnosticProvider {
    documents: DocumentManager,
}

impl DiagnosticProvider {
    pub fn new(documents: DocumentManager) -> Self {
        Self { documents }
    }

    /// Generate diagnostics for a document by combining syntax errors, type errors, and other analysis issues.
    pub fn generate_diagnostics(&self, uri: &Url, analyzer: &mut Analyzer) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();
        self.add_parse_errors(uri, &mut diagnostics);
        self.add_unbound_variable_errors(uri, analyzer, &mut diagnostics);
        self.add_type_errors(uri, analyzer, &mut diagnostics);
        self.add_unsafe_any_warnings(uri, analyzer, &mut diagnostics);
        self.add_annotation_mismatch_warnings(uri, analyzer, &mut diagnostics);
        self.add_dunder_diagnostics(uri, &mut diagnostics);
        self.add_static_analysis_diagnostics(uri, analyzer, &mut diagnostics);

        // TODO: Add cross-file diagnostics
        // This would include:
        // - Circular import detection
        // - Missing module errors
        // - Inconsistent symbol exports (__all__ mismatches)
        // - Conflicting stub definitions
        // Requires integration with workspace dependency graph

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
                message: format!("Undefined variable '{name}'"),
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
        for ty in result.type_map.values() {
            if Self::contains_any_type(ty, 0) {
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

    fn contains_any_type(ty: &beacon_core::Type, _depth: u32) -> bool {
        use beacon_core::{Type, TypeCtor};

        match ty {
            Type::Con(TypeCtor::Any) => true,
            Type::App(t1, t2) => Self::contains_any_type(t1, _depth + 1) || Self::contains_any_type(t2, _depth + 1),
            Type::Fun(args, ret) => {
                args.iter().any(|arg| Self::contains_any_type(arg, _depth + 1))
                    || Self::contains_any_type(ret, _depth + 1)
            }
            Type::Union(types) => types.iter().any(|t| Self::contains_any_type(t, _depth + 1)),
            Type::Record(fields, _) => fields.iter().any(|(_, t)| Self::contains_any_type(t, _depth + 1)),
            _ => false,
        }
    }

    /// Check annotation mismatches based on config mode
    fn add_annotation_mismatch_warnings(&self, uri: &Url, analyzer: &mut Analyzer, _diagnostics: &mut [Diagnostic]) {
        let result = match analyzer.analyze(uri) {
            Ok(r) => r,
            Err(_) => return,
        };

        // TODO: Get config mode from analyzer or pass as parameter
        let mode = config::TypeCheckingMode::Balanced;

        // TODO: Annotation coverage and mismatch detection (deferred to Parking Lot)
        // This requires:
        // 1. Walking the AST to find annotated assignments/parameters
        // 2. Looking up the inferred type from result.type_map (needs type_map implementation)
        // 3. Comparing annotation with inference
        // 4. Generating appropriate diagnostic based on mode
        // 5. Detecting missing/partial annotations for annotation coverage warnings

        let _type_map = &result.type_map;
        let _ = mode;
    }

    /// Add dunder-specific diagnostics
    fn add_dunder_diagnostics(&self, uri: &Url, diagnostics: &mut Vec<Diagnostic>) {
        self.documents.get_document(uri, |doc| {
            if let Some(ast) = doc.ast() {
                Self::check_dunder_patterns(ast, diagnostics);

                if let Some(symbol_table) = doc.symbol_table() {
                    let source = doc.text();
                    self.check_magic_methods_in_scope(ast, symbol_table, diagnostics, &source);
                }
            }
        });
    }

    /// Check for common dunder patterns like if __name__ == "__main__"
    fn check_dunder_patterns(node: &AstNode, diagnostics: &mut Vec<Diagnostic>) {
        match node {
            AstNode::If { test, body, line, col, .. } => {
                if Self::is_name_main_check(test) {
                    let position = Position { line: (*line - 1) as u32, character: (*col - 1) as u32 };
                    diagnostics.push(Diagnostic {
                        range: Range {
                            start: position,
                            end: Position { line: position.line, character: position.character + 10 },
                        },
                        severity: Some(DiagnosticSeverity::HINT),
                        code: Some(lsp_types::NumberOrString::String("DUNDER_INFO".to_string())),
                        source: Some("beacon".to_string()),
                        message: "Entry point guard: This code runs only when the script is executed directly"
                            .to_string(),
                        related_information: None,
                        tags: None,
                        data: None,
                        code_description: None,
                    });
                }

                for stmt in body {
                    Self::check_dunder_patterns(stmt, diagnostics);
                }
            }
            AstNode::Module { body, .. } | AstNode::FunctionDef { body, .. } | AstNode::ClassDef { body, .. } => {
                for stmt in body {
                    Self::check_dunder_patterns(stmt, diagnostics);
                }
            }
            _ => {}
        }
    }

    /// Check if a test expression is: __name__ == "__main__"
    fn is_name_main_check(test: &AstNode) -> bool {
        match test {
            AstNode::Compare { left, ops, comparators, .. } => {
                let is_name = matches!(**left, AstNode::Identifier { ref name, .. } if name == "__name__");

                let is_eq_main = ops.iter().any(|op| matches!(op, beacon_parser::CompareOperator::Eq))
                    && comparators.iter().any(|comp| {
                        matches!(comp, AstNode::Literal { value: beacon_parser::LiteralValue::String(s), .. } if s == "__main__")
                    });

                is_name && is_eq_main
            }
            _ => false,
        }
    }

    /// Check for magic methods defined outside class scope
    fn check_magic_methods_in_scope(
        &self, node: &AstNode, symbol_table: &beacon_parser::SymbolTable, diagnostics: &mut Vec<Diagnostic>,
        source: &str,
    ) {
        match node {
            AstNode::FunctionDef { name, line, col, body, .. } => {
                if MAGIC_METHODS.contains(&name.as_str()) {
                    let byte_offset = Self::line_col_to_byte_offset_from_source(source, line, col);
                    let scope_id = symbol_table.find_scope_at_position(byte_offset);

                    if !symbol_table.is_in_class_scope(scope_id) {
                        let position = Position { line: (*line - 1) as u32, character: (*col - 1) as u32 };
                        diagnostics.push(Diagnostic {
                            range: Range {
                                start: position,
                                end: Position {
                                    line: position.line,
                                    character: position.character + name.len() as u32,
                                },
                            },
                            severity: Some(DiagnosticSeverity::WARNING),
                            code: Some(lsp_types::NumberOrString::String("DUNDER001".to_string())),
                            source: Some("beacon".to_string()),
                            message: format!("Magic method '{name}' defined outside of a class"),
                            related_information: None,
                            tags: None,
                            data: None,
                            code_description: None,
                        });
                    } else {
                        self.validate_magic_method_signature(name, line, col, diagnostics);
                    }
                }

                for stmt in body {
                    self.check_magic_methods_in_scope(stmt, symbol_table, diagnostics, source);
                }
            }
            AstNode::ClassDef { body, .. } => {
                for stmt in body {
                    self.check_magic_methods_in_scope(stmt, symbol_table, diagnostics, source);
                }
            }
            AstNode::Module { body, .. } => {
                for stmt in body {
                    self.check_magic_methods_in_scope(stmt, symbol_table, diagnostics, source);
                }
            }
            _ => {}
        }
    }

    /// Basic validation for magic method signatures
    /// TODO: Implement more comprehensive signature validation
    fn validate_magic_method_signature(
        &self, name: &str, line: &usize, col: &usize, diagnostics: &mut Vec<Diagnostic>,
    ) {
        let _ = (name, line, col, diagnostics);
    }

    /// Convert line/col to byte offset using the actual source
    fn line_col_to_byte_offset_from_source(source: &str, line: &usize, col: &usize) -> usize {
        let mut byte_offset = 0;
        let mut current_line = 1;
        let mut current_col = 1;

        for ch in source.chars() {
            if current_line == *line && current_col == *col {
                return byte_offset;
            }
            if ch == '\n' {
                current_line += 1;
                current_col = 1;
            } else {
                current_col += 1;
            }
            byte_offset += ch.len_utf8();
        }

        byte_offset
    }

    /// Add static analysis diagnostics (use-before-def, unreachable code, unused variables, shadowing)
    fn add_static_analysis_diagnostics(&self, uri: &Url, analyzer: &mut Analyzer, diagnostics: &mut Vec<Diagnostic>) {
        let result = match analyzer.analyze(uri) {
            Ok(r) => r,
            Err(_) => return,
        };

        let Some(static_analysis) = result.static_analysis else {
            return;
        };

        for use_before_def in &static_analysis.use_before_def {
            let position = Position {
                line: (use_before_def.line.saturating_sub(1)) as u32,
                character: (use_before_def.col.saturating_sub(1)) as u32,
            };

            let range = Range {
                start: position,
                end: Position {
                    line: position.line,
                    character: position.character + use_before_def.var_name.len() as u32,
                },
            };

            diagnostics.push(Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::ERROR),
                code: Some(lsp_types::NumberOrString::String("use-before-def".to_string())),
                source: Some("beacon".to_string()),
                message: format!("Variable '{}' used before assignment", use_before_def.var_name),
                related_information: None,
                tags: None,
                data: None,
                code_description: None,
            });
        }

        for unreachable in &static_analysis.unreachable_code {
            let position = Position {
                line: (unreachable.line.saturating_sub(1)) as u32,
                character: (unreachable.col.saturating_sub(1)) as u32,
            };

            let range =
                Range { start: position, end: Position { line: position.line, character: position.character + 10 } };

            diagnostics.push(Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::WARNING),
                code: Some(lsp_types::NumberOrString::String("unreachable-code".to_string())),
                source: Some("beacon".to_string()),
                message: "Unreachable code detected".to_string(),
                related_information: None,
                tags: Some(vec![lsp_types::DiagnosticTag::UNNECESSARY]),
                data: None,
                code_description: None,
            });
        }

        for unused in &static_analysis.unused_variables {
            let position = Position {
                line: (unused.line.saturating_sub(1)) as u32,
                character: (unused.col.saturating_sub(1)) as u32,
            };

            let range = Range {
                start: position,
                end: Position { line: position.line, character: position.character + unused.var_name.len() as u32 },
            };

            diagnostics.push(Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::HINT),
                code: Some(lsp_types::NumberOrString::String("unused-variable".to_string())),
                source: Some("beacon".to_string()),
                message: format!("Variable '{}' is assigned but never used", unused.var_name),
                related_information: None,
                tags: Some(vec![lsp_types::DiagnosticTag::UNNECESSARY]),
                data: None,
                code_description: None,
            });
        }

        self.documents.get_document(uri, |doc| {
            if let Some(symbol_table) = doc.symbol_table() {
                let shadowed = symbol_table.find_shadowed_symbols();
                for (child_symbol, parent_symbol) in shadowed {
                    let position = Position {
                        line: (child_symbol.line.saturating_sub(1)) as u32,
                        character: (child_symbol.col.saturating_sub(1)) as u32,
                    };

                    let range = Range {
                        start: position,
                        end: Position {
                            line: position.line,
                            character: position.character + child_symbol.name.len() as u32,
                        },
                    };

                    diagnostics.push(Diagnostic {
                        range,
                        severity: Some(DiagnosticSeverity::WARNING),
                        code: Some(lsp_types::NumberOrString::String("shadowed-variable".to_string())),
                        source: Some("beacon".to_string()),
                        message: format!(
                            "Variable '{}' shadows variable from outer scope (line {})",
                            child_symbol.name, parent_symbol.line
                        ),
                        related_information: None,
                        tags: None,
                        data: None,
                        code_description: None,
                    });
                }
            }
        });
    }
}

/// Convert a parse error to an LSP diagnostic
fn parse_error_to_diagnostic(error: &ParseError) -> Diagnostic {
    Diagnostic {
        range: error.range,
        severity: Some(match error.severity {
            parser::ErrorSeverity::Error => DiagnosticSeverity::ERROR,
            parser::ErrorSeverity::Warning => DiagnosticSeverity::WARNING,
            parser::ErrorSeverity::Hint => DiagnosticSeverity::HINT,
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
        line: (error_info.line().saturating_sub(1)) as u32,
        character: (error_info.col().saturating_sub(1)) as u32,
    };

    let end_pos = match (error_info.end_line(), error_info.end_col()) {
        (Some(end_line), Some(end_col)) => {
            Position { line: (end_line.saturating_sub(1)) as u32, character: (end_col.saturating_sub(1)) as u32 }
        }
        _ => Position { line: start_pos.line, character: start_pos.character + 1 },
    };

    let range = Range { start: start_pos, end: end_pos };

    let (code, message) = match &error_info.error {
        TypeError::UnificationError(t1, t2) => ("HM001", format!("Type mismatch: cannot unify {t1} with {t2}")),
        TypeError::OccursCheckFailed(tv, ty) => ("HM002", format!("Infinite type: type variable {tv} occurs in {ty}")),
        TypeError::UndefinedTypeVar(tv) => ("HM003", format!("Undefined type variable: {tv}")),
        TypeError::KindMismatch { expected, found } => {
            ("HM004", format!("Kind mismatch: expected {expected}, found {found}"))
        }
        TypeError::InfiniteType(msg) => ("HM005", format!("Infinite type: {msg}")),
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
    use crate::analysis::{Span, TypeErrorInfo};
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
        assert_eq!(diagnostic.range.start.line, 9); // 0-indexed
        assert_eq!(diagnostic.range.start.character, 4); // 0-indexed
    }

    #[test]
    fn test_type_error_occurs_check_conversion() {
        let tv = TypeVar::new(0);
        let error_info = TypeErrorInfo {
            error: TypeError::OccursCheckFailed(tv.clone(), "List['t0]".to_string()),
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
    fn test_type_error_kind_mismatch_conversion() {
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
    fn test_contains_any_type_simple() {
        assert!(DiagnosticProvider::contains_any_type(&Type::Con(TypeCtor::Any), 0));
        assert!(!DiagnosticProvider::contains_any_type(&Type::Con(TypeCtor::Int), 0));
    }

    #[test]
    fn test_contains_any_type_nested() {
        let list_any = Type::App(Box::new(Type::Con(TypeCtor::List)), Box::new(Type::Con(TypeCtor::Any)));
        assert!(DiagnosticProvider::contains_any_type(&list_any, 0));

        let list_int = Type::App(Box::new(Type::Con(TypeCtor::List)), Box::new(Type::Con(TypeCtor::Int)));
        assert!(!DiagnosticProvider::contains_any_type(&list_int, 0));
    }

    #[test]
    fn test_contains_any_type_function() {
        let fun_any = Type::Fun(vec![Type::Con(TypeCtor::Int)], Box::new(Type::Con(TypeCtor::Any)));
        assert!(DiagnosticProvider::contains_any_type(&fun_any, 0));

        let fun_normal = Type::Fun(vec![Type::Con(TypeCtor::Int)], Box::new(Type::Con(TypeCtor::String)));
        assert!(!DiagnosticProvider::contains_any_type(&fun_normal, 0));
    }

    #[test]
    fn test_contains_any_type_union() {
        let union_any = Type::Union(vec![Type::Con(TypeCtor::Int), Type::Con(TypeCtor::Any)]);
        assert!(DiagnosticProvider::contains_any_type(&union_any, 0));

        let union_normal = Type::Union(vec![Type::Con(TypeCtor::Int), Type::Con(TypeCtor::String)]);
        assert!(!DiagnosticProvider::contains_any_type(&union_normal, 0));
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
        let source = "def broken(";

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
        let error_info = TypeErrorInfo {
            error: TypeError::UndefinedTypeVar(tv.clone()),
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
    fn test_type_error_infinite_type_conversion() {
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
    fn test_contains_any_type_record() {
        let record_any = Type::Record(vec![("field".to_string(), Type::Con(TypeCtor::Any))], None);
        assert!(DiagnosticProvider::contains_any_type(&record_any, 0));

        let record_normal = Type::Record(vec![("field".to_string(), Type::Con(TypeCtor::Int))], None);
        assert!(!DiagnosticProvider::contains_any_type(&record_normal, 0));
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

    #[test]
    fn test_dunder_name_main_pattern_detected() {
        let documents = DocumentManager::new().unwrap();
        let provider = DiagnosticProvider::new(documents.clone());
        let config = crate::config::Config::default();
        let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
if __name__ == "__main__":
    print("Running as main")
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

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
    fn test_magic_method_outside_class_warning() {
        let documents = DocumentManager::new().unwrap();
        let provider = DiagnosticProvider::new(documents.clone());
        let config = crate::config::Config::default();
        let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
def __init__(self):
    self.x = 1
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

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
    fn test_magic_method_inside_class_no_warning() {
        let documents = DocumentManager::new().unwrap();
        let provider = DiagnosticProvider::new(documents.clone());
        let config = crate::config::Config::default();
        let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
class MyClass:
    def __init__(self):
        self.x = 1
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);
        let magic_warnings: Vec<_> = diagnostics
            .iter()
            .filter(|d| {
                d.code == Some(lsp_types::NumberOrString::String("DUNDER001".to_string()))
                    && d.message.contains("__init__")
            })
            .collect();

        if !magic_warnings.is_empty() {
            panic!("Got unexpected warning for __init__ inside class: {magic_warnings:?}");
        }
    }

    #[test]
    fn test_is_name_main_check_positive() {
        let test_expr = AstNode::Compare {
            left: Box::new(AstNode::Identifier { name: "__name__".to_string(), line: 1, col: 4 }),
            ops: vec![beacon_parser::CompareOperator::Eq],
            comparators: vec![AstNode::Literal {
                value: beacon_parser::LiteralValue::String("__main__".to_string()),
                line: 1,
                col: 16,
            }],
            line: 1,
            col: 12,
        };

        assert!(DiagnosticProvider::is_name_main_check(&test_expr));
    }

    #[test]
    fn test_is_name_main_check_negative() {
        let test_expr = AstNode::Compare {
            left: Box::new(AstNode::Identifier { name: "x".to_string(), line: 1, col: 4 }),
            ops: vec![beacon_parser::CompareOperator::Eq],
            comparators: vec![AstNode::Literal { value: beacon_parser::LiteralValue::Integer(42), line: 1, col: 9 }],
            line: 1,
            col: 6,
        };

        assert!(!DiagnosticProvider::is_name_main_check(&test_expr));
    }

    #[test]
    fn test_type_errors_surfaced_in_diagnostics() {
        use std::str::FromStr;

        let documents = DocumentManager::new().unwrap();
        let provider = DiagnosticProvider::new(documents.clone());
        let config = crate::config::Config::default();
        let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
def test():
    x: int = "hello"
    return x
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);
        let type_errors: Vec<_> = diagnostics
            .iter()
            .filter(|d| {
                if let Some(lsp_types::NumberOrString::String(code)) = &d.code {
                    code.starts_with("HM")
                } else {
                    false
                }
            })
            .collect();

        assert!(!type_errors.is_empty())
    }
}

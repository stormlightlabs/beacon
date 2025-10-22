//! Diagnostic generation and publishing
//!
//! Converts type errors, parse errors, and other analysis results into
//! LSP diagnostics for display in the editor.

use beacon_core::BeaconError;
use lsp_types::{Diagnostic, DiagnosticSeverity, Position, Range};
use url::Url;

use crate::analysis::Analyzer;
use crate::document::DocumentManager;
use crate::parser::ParseError;

/// Diagnostic provider for generating LSP diagnostics
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

        // Add parse/syntax errors
        self.add_parse_errors(uri, &mut diagnostics);

        // Add type errors
        self.add_type_errors(uri, analyzer, &mut diagnostics);

        // TODO: Add name resolution errors (unbound variables)
        // TODO: Add unsafe Any flow warnings
        // TODO: Add annotation mismatch warnings

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
    ///
    /// TODO: Implement after type inference is complete
    fn add_type_errors(&self, uri: &Url, analyzer: &mut Analyzer, diagnostics: &mut Vec<Diagnostic>) {
        // Attempt analysis
        match analyzer.analyze(uri) {
            Ok(_result) => {
                // TODO: Extract type errors from result
                // TODO: Convert to LSP diagnostics
            }
            Err(e) => {
                // Analysis failed, add error diagnostic
                diagnostics.push(analysis_error_to_diagnostic(&e));
            }
        }
    }

    /// TODO: Check for unbound names using symbol table
    pub fn _check_unbound_names(&self, _uri: &Url) -> Vec<Diagnostic> {
        Vec::new()
    }

    /// TODO: Check for unsafe Any type usage
    pub fn _check_unsafe_any(&self, _uri: &Url) -> Vec<Diagnostic> {
        Vec::new()
    }

    /// TODO: Check annotation mismatches based on config mode
    pub fn _check_annotation_mismatches(&self, _uri: &Url) -> Vec<Diagnostic> {
        Vec::new()
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

/// TODO: Convert type error to diagnostic with proper range and message
pub fn _type_error_to_diagnostic(_uri: &Url) -> Diagnostic {
    Diagnostic {
        range: Range::default(),
        severity: Some(DiagnosticSeverity::ERROR),
        code: Some(lsp_types::NumberOrString::String("HM001".to_string())),
        source: Some("beacon".to_string()),
        message: "Type mismatch".to_string(),
        related_information: None,
        tags: None,
        data: None,
        code_description: None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
}

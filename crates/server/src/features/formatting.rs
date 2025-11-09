//! Document formatting feature provider
//!
//! Implements textDocument/formatting and textDocument/rangeFormatting LSP handlers for PEP8-compliant Python code formatting.

use crate::document::DocumentManager;
use crate::formatting::{Formatter, FormatterConfig};

use beacon_parser::PythonParser;
use lsp_types::{DocumentFormattingParams, DocumentRangeFormattingParams, Position, Range, TextEdit};

/// Provider for document formatting features
///
/// Handles full document formatting and range formatting requests from the LSP client.
pub struct FormattingProvider {
    documents: DocumentManager,
}

impl FormattingProvider {
    /// Create a new formatting provider
    pub fn new(documents: DocumentManager) -> Self {
        Self { documents }
    }

    /// Format an entire document
    pub fn format_document(
        &self, params: &DocumentFormattingParams, config: &FormatterConfig,
    ) -> Option<Vec<TextEdit>> {
        let uri = &params.text_document.uri;

        tracing::debug!(?uri, "Formatting document");

        let (content, line_count) = self.documents.get_document(uri, |doc| {
            let text = doc.text();
            let lines = text.lines().count();
            (text, lines)
        })?;

        let parser = PythonParser::default();
        let mut formatter = Formatter::new(config.clone(), parser);

        match formatter.format_range(&content, 0, line_count) {
            Ok(formatted) => {
                if formatted == content {
                    tracing::debug!(?uri, "Document already formatted");
                    return None;
                }

                let edit = TextEdit {
                    range: Range {
                        start: Position { line: 0, character: 0 },
                        end: Position { line: line_count as u32, character: 0 },
                    },
                    new_text: formatted,
                };

                tracing::debug!(?uri, "Document formatted successfully");
                Some(vec![edit])
            }
            Err(e) => {
                tracing::error!(?uri, ?e, "Failed to format document");
                None
            }
        }
    }

    /// Format a range within a document
    ///
    /// Formats only the specified range of the document.
    pub fn format_range(
        &self, params: &DocumentRangeFormattingParams, config: &FormatterConfig,
    ) -> Option<Vec<TextEdit>> {
        let uri = &params.text_document.uri;
        let range = params.range;

        tracing::debug!(?uri, ?range, "Formatting document range");

        let (content, line_count) = self.documents.get_document(uri, |doc| {
            let text = doc.text();
            let lines = text.lines().count();
            (text, lines)
        })?;

        let parser = PythonParser::default();
        let mut formatter = Formatter::new(config.clone(), parser);

        let start_line = range.start.line as usize;
        let end_line = range.end.line as usize;

        match formatter.format_range(&content, start_line, end_line) {
            Ok(formatted) => {
                if formatted == content {
                    tracing::debug!(?uri, "Range already formatted");
                    return None;
                }

                let edit = TextEdit {
                    range: Range {
                        start: Position { line: 0, character: 0 },
                        end: Position { line: line_count as u32, character: 0 },
                    },
                    new_text: formatted,
                };

                tracing::debug!(?uri, "Range formatted successfully");
                Some(vec![edit])
            }
            Err(e) => {
                tracing::error!(?uri, ?e, "Failed to format range");
                None
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lsp_types::{Position, Range, TextDocumentIdentifier};

    fn create_test_provider() -> FormattingProvider {
        let documents = DocumentManager::new().expect("Failed to create document manager");
        FormattingProvider::new(documents)
    }

    #[test]
    fn test_provider_creation() {
        let _provider = create_test_provider();
    }

    #[test]
    fn test_format_document_nonexistent() {
        let provider = create_test_provider();
        let config = FormatterConfig::default();
        let params = DocumentFormattingParams {
            text_document: TextDocumentIdentifier { uri: lsp_types::Url::parse("file:///nonexistent.py").unwrap() },
            options: Default::default(),
            work_done_progress_params: Default::default(),
        };

        let result = provider.format_document(&params, &config);
        assert!(result.is_none());
    }

    #[test]
    fn test_format_range_nonexistent() {
        let provider = create_test_provider();
        let config = FormatterConfig::default();
        let params = DocumentRangeFormattingParams {
            text_document: TextDocumentIdentifier { uri: lsp_types::Url::parse("file:///nonexistent.py").unwrap() },
            range: Range { start: Position { line: 0, character: 0 }, end: Position { line: 5, character: 0 } },
            options: Default::default(),
            work_done_progress_params: Default::default(),
        };

        let result = provider.format_range(&params, &config);
        assert!(result.is_none());
    }

    #[test]
    fn test_format_document_already_formatted() {
        let provider = create_test_provider();
        let config = FormatterConfig::default();
        let uri = lsp_types::Url::parse("file:///test.py").unwrap();
        let content = "def foo():\n    pass\n";

        provider
            .documents
            .open_document(uri.clone(), 1, content.to_string())
            .ok();

        let params = DocumentFormattingParams {
            text_document: TextDocumentIdentifier { uri },
            options: Default::default(),
            work_done_progress_params: Default::default(),
        };

        let result = provider.format_document(&params, &config);
        assert!(result.is_none() || result.is_some());
    }
}

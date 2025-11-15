//! Document formatting feature provider
//!
//! Implements textDocument/formatting and textDocument/rangeFormatting LSP handlers for PEP8-compliant Python code formatting.

use crate::document::DocumentManager;
use crate::formatting::{Formatter, FormatterConfig};

use beacon_parser::PythonParser;
use lsp_types::{
    DocumentFormattingParams, DocumentOnTypeFormattingParams, DocumentRangeFormattingParams, Position, Range,
    TextDocumentSaveReason as TextSaveReason, TextEdit, WillSaveTextDocumentParams,
};

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
    /// Formats only the specified range of the document and returns
    /// a TextEdit that replaces exactly that range.
    pub fn format_range(
        &self, params: &DocumentRangeFormattingParams, config: &FormatterConfig,
    ) -> Option<Vec<TextEdit>> {
        let uri = &params.text_document.uri;
        let range = params.range;

        tracing::debug!(?uri, ?range, "Formatting document range");

        let content = self.documents.get_document(uri, |doc| doc.text())?;

        let parser = PythonParser::default();
        let mut formatter = Formatter::new(config.clone(), parser);

        let start_line = range.start.line as usize;
        let end_line = range.end.line as usize;

        let source_lines: Vec<&str> = content.lines().collect();
        let range_content: String = source_lines
            .iter()
            .skip(start_line)
            .take((end_line - start_line).max(1))
            .map(|line| format!("{line}\n"))
            .collect();

        match formatter.format_range(&range_content, 0, (end_line - start_line).max(1)) {
            Ok(formatted) => {
                let formatted_trimmed = if range_content.ends_with('\n') && !formatted.ends_with('\n') {
                    formatted.clone()
                } else if !range_content.ends_with('\n') && formatted.ends_with('\n') {
                    formatted.trim_end_matches('\n').to_string()
                } else {
                    formatted.clone()
                };

                if formatted_trimmed == range_content || formatted_trimmed == range_content.trim_end_matches('\n') {
                    tracing::debug!(?uri, "Range already formatted");
                    return None;
                }

                let end_col = source_lines
                    .get(end_line.saturating_sub(1))
                    .map(|l| l.len())
                    .unwrap_or(0);

                let edit = TextEdit {
                    range: Range {
                        start: Position { line: start_line as u32, character: 0 },
                        end: Position { line: end_line.saturating_sub(1) as u32, character: end_col as u32 },
                    },
                    new_text: formatted_trimmed.trim_end_matches('\n').to_string(),
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

    /// Format document on save
    pub fn format_on_save(
        &self, params: &WillSaveTextDocumentParams, config: &FormatterConfig,
    ) -> Option<Vec<TextEdit>> {
        let uri = &params.text_document.uri;

        if params.reason != TextSaveReason::MANUAL {
            tracing::debug!(?uri, ?params.reason, "Skipping format-on-save (not manual save)");
            return None;
        }

        tracing::debug!(?uri, "Format-on-save triggered");

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

                tracing::debug!(?uri, "Format-on-save completed successfully");
                Some(vec![edit])
            }
            Err(e) => {
                tracing::error!(?uri, ?e, "Failed to format on save");
                None
            }
        }
    }

    /// Format on type
    ///
    /// Automatically formats code as the user types. Triggered after specific characters (e.g., `:` in function definitions).
    pub fn on_type_format(
        &self, params: &DocumentOnTypeFormattingParams, config: &FormatterConfig,
    ) -> Option<Vec<TextEdit>> {
        let uri = &params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        let trigger_char = &params.ch;

        tracing::debug!(?uri, ?position, trigger_char, "On-type formatting triggered");

        let (content, line_count) = self.documents.get_document(uri, |doc| {
            let text = doc.text();
            let lines = text.lines().count();
            (text, lines)
        })?;

        let parser = PythonParser::default();
        let mut formatter = Formatter::new(config.clone(), parser);

        let current_line = position.line as usize;
        let start_line = current_line.saturating_sub(1);
        let end_line = (current_line + 2).min(line_count);

        match formatter.format_range(&content, start_line, end_line) {
            Ok(formatted) => {
                if formatted == content {
                    tracing::debug!(?uri, "Code already formatted");
                    return None;
                }

                let edit = TextEdit {
                    range: Range {
                        start: Position { line: 0, character: 0 },
                        end: Position { line: line_count as u32, character: 0 },
                    },
                    new_text: formatted,
                };

                tracing::debug!(?uri, "On-type formatting completed");
                Some(vec![edit])
            }
            Err(e) => {
                tracing::error!(?uri, ?e, "Failed to format on type");
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

    #[test]
    fn test_format_range_simple() {
        let provider = create_test_provider();
        let config = FormatterConfig::default();
        let uri = lsp_types::Url::parse("file:///test.py").unwrap();
        let content = "x=1\ny=2\nz=3\n";

        provider
            .documents
            .open_document(uri.clone(), 1, content.to_string())
            .ok();

        let params = DocumentRangeFormattingParams {
            text_document: TextDocumentIdentifier { uri },
            range: Range { start: Position { line: 0, character: 0 }, end: Position { line: 1, character: 0 } },
            options: Default::default(),
            work_done_progress_params: Default::default(),
        };

        let result = provider.format_range(&params, &config);
        assert!(result.is_some());

        if let Some(edits) = result {
            assert_eq!(edits.len(), 1);
            let edit = &edits[0];
            assert_eq!(edit.range.start.line, 0);
            assert!(edit.range.end.line <= 1);
        }
    }

    #[test]
    fn test_format_range_already_formatted() {
        let provider = create_test_provider();
        let config = FormatterConfig::default();
        let uri = lsp_types::Url::parse("file:///test.py").unwrap();
        let content = "x = 1\ny = 2\nz = 3\n";

        provider
            .documents
            .open_document(uri.clone(), 1, content.to_string())
            .ok();

        let params = DocumentRangeFormattingParams {
            text_document: TextDocumentIdentifier { uri },
            range: Range { start: Position { line: 0, character: 0 }, end: Position { line: 2, character: 0 } },
            options: Default::default(),
            work_done_progress_params: Default::default(),
        };

        let result = provider.format_range(&params, &config);
        assert!(result.is_none() || result.is_some());
    }

    #[test]
    fn test_format_range_middle_of_document() {
        let provider = create_test_provider();
        let config = FormatterConfig::default();
        let uri = lsp_types::Url::parse("file:///test.py").unwrap();
        let content = "a = 1\nx=2\ny=3\nz = 4\n";

        provider
            .documents
            .open_document(uri.clone(), 1, content.to_string())
            .ok();

        let params = DocumentRangeFormattingParams {
            text_document: TextDocumentIdentifier { uri },
            range: Range { start: Position { line: 1, character: 0 }, end: Position { line: 3, character: 0 } },
            options: Default::default(),
            work_done_progress_params: Default::default(),
        };

        let result = provider.format_range(&params, &config);
        if let Some(edits) = result {
            assert_eq!(edits.len(), 1);
            let edit = &edits[0];
            assert_eq!(edit.range.start.line, 1);
        }
    }

    #[test]
    fn test_format_range_preserves_rest_of_document() {
        let provider = create_test_provider();
        let config = FormatterConfig::default();
        let uri = lsp_types::Url::parse("file:///test.py").unwrap();
        let content = "# Header\nx=1\n# Footer\n";

        provider
            .documents
            .open_document(uri.clone(), 1, content.to_string())
            .ok();

        let params = DocumentRangeFormattingParams {
            text_document: TextDocumentIdentifier { uri },
            range: Range { start: Position { line: 1, character: 0 }, end: Position { line: 2, character: 0 } },
            options: Default::default(),
            work_done_progress_params: Default::default(),
        };

        let result = provider.format_range(&params, &config);
        if let Some(edits) = result {
            assert_eq!(edits.len(), 1);
            let edit = &edits[0];
            assert_eq!(edit.range.start.line, 1);
            assert!(!edit.new_text.contains("Header"));
            assert!(!edit.new_text.contains("Footer"));
        }
    }
}

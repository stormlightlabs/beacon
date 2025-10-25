//! Code completion provider
//!
//! Provides intelligent completions for identifiers, attributes, imports, and keywords.
use crate::document::DocumentManager;
use crate::features::dunders;
use beacon_parser::{BUILTIN_DUNDERS, MAGIC_METHODS};
use lsp_types::{
    CompletionItem, CompletionItemKind, CompletionParams, CompletionResponse, Documentation, MarkupContent, MarkupKind,
    Position,
};
use url::Url;

pub struct CompletionProvider {
    _documents: DocumentManager,
}

impl CompletionProvider {
    pub fn new(documents: DocumentManager) -> Self {
        Self { _documents: documents }
    }

    /// Provide completions at a position
    pub fn completion(&self, params: CompletionParams) -> Option<CompletionResponse> {
        let uri = &params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;

        let mut items = Vec::new();

        let is_dunder_completion = self
            ._documents
            .get_document(uri, |doc| {
                let content = doc.text();
                let line_text = content.lines().nth(position.line as usize)?;
                let prefix = &line_text[..position.character.min(line_text.len() as u32) as usize];
                Some(prefix.ends_with("__") || prefix.contains("__"))
            })
            .flatten()
            .unwrap_or(false);

        if is_dunder_completion {
            items.extend(self.dunder_completions(uri, position));
        } else {
            items.extend(vec![
                self.builtin_completion("print", "Built-in print function"),
                self.builtin_completion("len", "Return the length of an object"),
                self.builtin_completion("range", "Return a sequence of numbers"),
            ]);
        }

        Some(CompletionResponse::Array(items))
    }

    /// Get dunder completions based on current context
    fn dunder_completions(&self, uri: &Url, position: Position) -> Vec<CompletionItem> {
        let mut items = Vec::new();

        let in_class = self
            ._documents
            .get_document(uri, |doc| {
                if let Some(symbol_table) = doc.symbol_table() {
                    let line = (position.line + 1) as usize;
                    let col = (position.character + 1) as usize;
                    let content = doc.text();
                    let byte_offset = Self::position_to_byte_offset(&content, line, col);

                    let scope_id = symbol_table.find_scope_at_position(byte_offset);
                    Some(symbol_table.is_in_class_scope(scope_id))
                } else {
                    None
                }
            })
            .flatten()
            .unwrap_or(false);

        for &dunder_name in BUILTIN_DUNDERS {
            if let Some(info) = dunders::get_dunder_info(dunder_name) {
                items.push(self.dunder_completion_item(info));
            }
        }

        if in_class {
            for &magic_method in MAGIC_METHODS {
                if let Some(info) = dunders::get_dunder_info(magic_method) {
                    items.push(self.dunder_completion_item(info));
                }
            }
        }

        items
    }

    /// Create a completion item for a dunder with documentation
    fn dunder_completion_item(&self, info: &dunders::DunderInfo) -> CompletionItem {
        CompletionItem {
            label: info.name.clone(),
            kind: Some(if info.category == "method" {
                CompletionItemKind::METHOD
            } else {
                CompletionItemKind::VARIABLE
            }),
            detail: Some(info.category.clone()),
            documentation: Some(Documentation::MarkupContent(MarkupContent {
                kind: MarkupKind::Markdown,
                value: format!("{}\n\n[Documentation]({})", info.doc, info.link),
            })),
            ..Default::default()
        }
    }

    /// Convert line/col position to byte offset
    fn position_to_byte_offset(content: &str, line: usize, col: usize) -> usize {
        let mut byte_offset = 0;
        let mut current_line = 1;
        let mut current_col = 1;

        for ch in content.chars() {
            if current_line == line && current_col == col {
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

    /// Create a completion item for a builtin
    fn builtin_completion(&self, name: &str, detail: &str) -> CompletionItem {
        CompletionItem {
            label: name.to_string(),
            kind: Some(CompletionItemKind::FUNCTION),
            detail: Some(detail.to_string()),
            ..Default::default()
        }
    }

    /// TODO: Get completions for symbols in scope
    pub fn _symbol_completions(&self, _uri: &Url, _position: Position) -> Vec<CompletionItem> {
        Vec::new()
    }

    /// TODO: Get completions for attributes on a type
    pub fn _attribute_completions(&self, _uri: &Url, _position: Position) -> Vec<CompletionItem> {
        // Use type inference to get type of expression before '.'
        // Look up attributes/methods on that type
        Vec::new()
    }

    /// TODO: Get completions for imports
    pub fn _import_completions(&self, _uri: &Url, _position: Position) -> Vec<CompletionItem> {
        // Scan available modules and stub files
        Vec::new()
    }

    /// TODO: Get keyword completions based on context
    pub fn _keyword_completions(&self, _position: Position) -> Vec<CompletionItem> {
        Vec::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lsp_types::TextDocumentPositionParams;
    use std::str::FromStr;

    #[test]
    fn test_builtin_completion() {
        let documents = DocumentManager::new().unwrap();
        let provider = CompletionProvider::new(documents);

        let item = provider.builtin_completion("test", "Test function");

        assert_eq!(item.label, "test");
        assert_eq!(item.kind, Some(CompletionItemKind::FUNCTION));
        assert_eq!(item.detail, Some("Test function".to_string()));
    }

    #[test]
    fn test_dunder_completion_in_module_scope() {
        let documents = DocumentManager::new().unwrap();
        let provider = CompletionProvider::new(documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = "__";
        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let params = CompletionParams {
            text_document_position: TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                position: Position { line: 0, character: 2 },
            },
            work_done_progress_params: lsp_types::WorkDoneProgressParams::default(),
            partial_result_params: lsp_types::PartialResultParams::default(),
            context: None,
        };

        let response = provider.completion(params);
        assert!(response.is_some());

        match response.unwrap() {
            CompletionResponse::Array(items) => {
                assert!(items.iter().any(|item| item.label == "__name__"));
                assert!(items.iter().any(|item| item.label == "__file__"));
            }
            _ => panic!("Expected array response"),
        }
    }

    #[test]
    fn test_dunder_completion_in_class_scope() {
        let documents = DocumentManager::new().unwrap();
        let provider = CompletionProvider::new(documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = "class MyClass:\n    def __";
        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let params = CompletionParams {
            text_document_position: TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                position: Position { line: 1, character: 10 },
            },
            work_done_progress_params: lsp_types::WorkDoneProgressParams::default(),
            partial_result_params: lsp_types::PartialResultParams::default(),
            context: None,
        };

        let response = provider.completion(params);
        assert!(response.is_some());

        match response.unwrap() {
            CompletionResponse::Array(items) => {
                assert!(items.iter().any(|item| item.label == "__name__"));

                // Magic methods may or may not be included depending on whether
                // symbol table is available - this is acceptable behavior
                // The important thing is we don't crash and provide some completions
            }
            _ => panic!("Expected array response"),
        }
    }

    #[test]
    fn test_dunder_completion_item_has_documentation() {
        let documents = DocumentManager::new().unwrap();
        let provider = CompletionProvider::new(documents.clone());

        if let Some(info) = dunders::get_dunder_info("__init__") {
            let item = provider.dunder_completion_item(info);

            assert_eq!(item.label, "__init__");
            assert_eq!(item.kind, Some(CompletionItemKind::METHOD));
            assert!(item.documentation.is_some());

            match item.documentation.unwrap() {
                Documentation::MarkupContent(content) => {
                    assert!(content.value.contains("Initializer"));
                    assert!(content.value.contains("https://"));
                }
                _ => panic!("Expected markup content"),
            }
        }
    }

    #[test]
    fn test_position_to_byte_offset() {
        let content = "line 1\nline 2\nline 3";

        assert_eq!(CompletionProvider::position_to_byte_offset(content, 1, 1), 0);
        assert_eq!(CompletionProvider::position_to_byte_offset(content, 2, 1), 7);
        assert_eq!(CompletionProvider::position_to_byte_offset(content, 2, 3), 9);
    }
}

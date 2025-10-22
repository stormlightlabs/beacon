//! Code completion provider
//!
//! Provides intelligent completions for identifiers, attributes, imports, and keywords.
use crate::document::DocumentManager;
use lsp_types::{CompletionItem, CompletionItemKind, CompletionParams, CompletionResponse, Position};
use url::Url;

pub struct CompletionProvider {
    _documents: DocumentManager,
}

impl CompletionProvider {
    pub fn new(documents: DocumentManager) -> Self {
        Self { _documents: documents }
    }

    /// Provide completions at a position
    ///
    /// TODO: Implement context-aware completions
    /// TODO: Determine completion context (in expression, import, attribute access, etc.)
    /// TODO: Use symbol table for available names in scope
    /// TODO: Use type information for attribute completions
    /// TODO: Filter based on what's been typed
    pub fn completion(&self, _params: CompletionParams) -> Option<CompletionResponse> {
        let items = vec![
            self.builtin_completion("print", "Built-in print function"),
            self.builtin_completion("len", "Return the length of an object"),
            self.builtin_completion("range", "Return a sequence of numbers"),
        ];

        Some(CompletionResponse::Array(items))
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

    #[test]
    fn test_builtin_completion() {
        let documents = DocumentManager::new().unwrap();
        let provider = CompletionProvider::new(documents);

        let item = provider.builtin_completion("test", "Test function");

        assert_eq!(item.label, "test");
        assert_eq!(item.kind, Some(CompletionItemKind::FUNCTION));
        assert_eq!(item.detail, Some("Test function".to_string()));
    }
}

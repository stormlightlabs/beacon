//! Inlay hints provider
//!
//! Displays inline type annotations, parameter names, and other hints.

use lsp_types::{InlayHint, InlayHintKind, InlayHintLabel, InlayHintParams, Position, Range};
use url::Url;

use crate::analysis::Analyzer;
use crate::document::DocumentManager;

/// Inlay hints provider
pub struct InlayHintsProvider {
    documents: DocumentManager,
}

impl InlayHintsProvider {
    pub fn new(documents: DocumentManager) -> Self {
        Self { documents }
    }

    /// Provide inlay hints for a range in a document
    ///
    /// TODO: Implement inlay hints for:
    /// - Inferred variable types
    /// - Function parameter names
    /// - Function return types
    /// - Type arguments for generic calls
    pub fn inlay_hints(
        &self,
        params: InlayHintParams,
        _analyzer: &mut Analyzer,
    ) -> Vec<InlayHint> {
        let uri = params.text_document.uri;
        let range = params.range;

        let mut hints = Vec::new();

        // Get hints for variables in range
        self.add_variable_type_hints(&uri, range, &mut hints);

        // TODO: Add parameter name hints
        // TODO: Add return type hints

        hints
    }

    /// Add type hints for variable assignments
    ///
    /// TODO: Implement using type inference results
    fn add_variable_type_hints(&self, uri: &Url, _range: Range, hints: &mut Vec<InlayHint>) {
        self.documents.get_document(uri, |doc| {
            // TODO: Walk AST to find assignments in range
            // TODO: Get inferred type for each assignment
            // TODO: Create InlayHint for each

            let _ast = doc.ast()?;

            // Placeholder
            hints.push(InlayHint {
                position: Position { line: 0, character: 0 },
                label: InlayHintLabel::String(": int".to_string()),
                kind: Some(InlayHintKind::TYPE),
                text_edits: None,
                tooltip: None,
                padding_left: None,
                padding_right: None,
                data: None,
            });

            Some(())
        });
    }

    /// TODO: Add hints for parameter names in function calls
    pub fn _add_parameter_hints(&self, _uri: &Url, _range: Range) -> Vec<InlayHint> {
        // For calls like `func(1, 2)`, show `func(a: 1, b: 2)`
        Vec::new()
    }

    /// TODO: Add hints for inferred return types
    pub fn _add_return_type_hints(&self, _uri: &Url, _range: Range) -> Vec<InlayHint> {
        // For functions without return type annotation, show inferred type
        Vec::new()
    }

    /// TODO: Add hints for type parameters in generic calls
    pub fn _add_type_parameter_hints(&self, _uri: &Url, _range: Range) -> Vec<InlayHint> {
        // For calls like `list()`, show `list[T]()`
        Vec::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_provider_creation() {
        let documents = DocumentManager::new().unwrap();
        let _provider = InlayHintsProvider::new(documents);
    }
}

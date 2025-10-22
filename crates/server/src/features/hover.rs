//! Hover information provider
//!
//! Displays type information, signatures, and documentation when hovering
//! over identifiers and expressions.

use lsp_types::{Hover, HoverContents, HoverParams, MarkupContent, MarkupKind, Position};
use url::Url;

use crate::analysis::Analyzer;
use crate::document::DocumentManager;

/// Hover information provider
pub struct HoverProvider {
    _documents: DocumentManager,
}

impl HoverProvider {
    pub fn new(documents: DocumentManager) -> Self {
        Self { _documents: documents }
    }

    /// Provide hover information at a position
    ///
    /// TODO: Implement hover based on type inference results
    pub fn hover(&self, params: HoverParams, analyzer: &mut Analyzer) -> Option<Hover> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        // Get inferred type at position
        let ty = analyzer.type_at_position(&uri, position).ok()??;

        // Format type as hover content
        let content = self.format_type_hover(&ty);

        Some(Hover {
            contents: HoverContents::Markup(content),
            range: None, // TODO: Include range of the hovered identifier
        })
    }

    /// Format a type for display in hover
    ///
    /// TODO: Implement rich formatting with:
    /// - Principal types vs instantiated types
    /// - Type variable names
    /// - Function signatures
    /// - Documentation comments
    fn format_type_hover(&self, ty: &beacon_core::Type) -> MarkupContent {
        MarkupContent { kind: MarkupKind::Markdown, value: format!("```python\n{}\n```", self.format_type(ty)) }
    }

    /// Format a type as a string
    ///
    /// TODO: Use pretty-printing from beacon-core
    fn format_type(&self, ty: &beacon_core::Type) -> String {
        format!("{:?}", ty) // Placeholder
    }

    /// TODO: Get hover for a function showing signature and docstring
    pub fn _hover_function(&self, _uri: &Url, _position: Position) -> Option<Hover> {
        None
    }

    /// TODO: Get hover for a class showing definition and docstring
    pub fn _hover_class(&self, _uri: &Url, _position: Position) -> Option<Hover> {
        None
    }

    /// TODO: Get hover for a variable showing inferred type
    pub fn _hover_variable(&self, _uri: &Url, _position: Position) -> Option<Hover> {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use beacon_core::{Type, TypeCtor};

    #[test]
    fn test_format_type_hover() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents);

        let ty = Type::Con(TypeCtor::Int);
        let hover = provider.format_type_hover(&ty);

        assert_eq!(hover.kind, MarkupKind::Markdown);
        assert!(hover.value.contains("```python"));
    }
}

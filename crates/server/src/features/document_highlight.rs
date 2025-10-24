//! Document highlight provider
//!
//! Highlights all occurrences of the symbol under the cursor within the current document.
//! This provides visual feedback when the user's cursor is on a symbol.

use crate::document::DocumentManager;
use beacon_parser::{AstNode, SymbolTable};
use lsp_types::{DocumentHighlight, DocumentHighlightKind, DocumentHighlightParams, Position, Range};
use url::Url;

/// Document highlight provider
///
/// Finds all occurrences of a symbol in the current document and returns their locations.
/// Used for highlighting all uses of a variable, function, class, etc.
pub struct DocumentHighlightProvider {
    documents: DocumentManager,
}

impl DocumentHighlightProvider {
    pub fn new(documents: DocumentManager) -> Self {
        Self { documents }
    }

    /// Find all highlights for the symbol at the given position
    ///
    /// Returns highlights with appropriate kinds (Read, Write, Text) for each occurrence.
    pub fn document_highlight(&self, params: DocumentHighlightParams) -> Option<Vec<DocumentHighlight>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        self.documents.get_document(&uri, |doc| {
            let tree = doc.tree()?;
            let text = doc.text();
            let symbol_table = doc.symbol_table()?;
            let parser = crate::parser::LspParser::new().ok()?;
            let node = parser.node_at_position(tree, &text, position)?;

            match node.kind() {
                "identifier" => {
                    let identifier_text = node.utf8_text(text.as_bytes()).ok()?;
                    self.find_symbol_highlights(&uri, identifier_text, symbol_table, &text)
                }
                _ => None,
            }
        })?
    }

    /// Find all highlights for a symbol name in the document
    ///
    /// Walks the AST to find all occurrences of the symbol and classifies them as Read or Write based on context.
    fn find_symbol_highlights(
        &self, uri: &Url, symbol_name: &str, symbol_table: &SymbolTable, text: &str,
    ) -> Option<Vec<DocumentHighlight>> {
        let _symbol = symbol_table.lookup_symbol(symbol_name, symbol_table.root_scope)?;
        let mut highlights = Vec::new();

        self.documents.get_document(uri, |doc| {
            let ast = doc.ast()?;
            self.collect_highlights(ast, symbol_name, &mut highlights, text);
            Some(())
        })?;

        if highlights.is_empty() { None } else { Some(highlights) }
    }

    /// Recursively collect highlights from the AST
    ///
    /// Classifies each occurrence as Read or Write based on context.
    fn collect_highlights(
        &self, node: &AstNode, symbol_name: &str, highlights: &mut Vec<DocumentHighlight>, text: &str,
    ) {
        match node {
            AstNode::Identifier { name, line, col } if name == symbol_name => {
                let position =
                    Position { line: (*line as u32).saturating_sub(1), character: (*col as u32).saturating_sub(1) };
                let end_position =
                    Position { line: position.line, character: position.character + symbol_name.len() as u32 };

                highlights.push(DocumentHighlight {
                    range: Range { start: position, end: end_position },
                    kind: Some(DocumentHighlightKind::READ),
                });
            }
            AstNode::Assignment { target, value, line, col } => {
                if target == symbol_name {
                    let position =
                        Position { line: (*line as u32).saturating_sub(1), character: (*col as u32).saturating_sub(1) };
                    let end_position =
                        Position { line: position.line, character: position.character + symbol_name.len() as u32 };

                    highlights.push(DocumentHighlight {
                        range: Range { start: position, end: end_position },
                        kind: Some(DocumentHighlightKind::WRITE),
                    });
                }
                self.collect_highlights(value, symbol_name, highlights, text);
            }
            AstNode::FunctionDef { name, body, args, .. } => {
                if name == symbol_name {
                    // We don't have precise position for function name in current AST
                    // Skip for now - will be fixed when we improve AST
                }

                for param in args {
                    if param.name == *symbol_name {
                        // Parameter definition - we now have position info
                        // TODO: Add highlight for parameter definition
                    }
                }

                for stmt in body {
                    self.collect_highlights(stmt, symbol_name, highlights, text);
                }
            }
            AstNode::ClassDef { body, .. } | AstNode::Module { body, .. } => {
                for stmt in body {
                    self.collect_highlights(stmt, symbol_name, highlights, text);
                }
            }
            AstNode::Call { args, .. } => {
                for arg in args {
                    self.collect_highlights(arg, symbol_name, highlights, text);
                }
            }
            AstNode::Return { value, .. } => {
                if let Some(val) = value {
                    self.collect_highlights(val, symbol_name, highlights, text);
                }
            }
            AstNode::Literal { .. } => {}
            _ => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::str::FromStr;

    #[test]
    fn test_provider_creation() {
        let documents = DocumentManager::new().unwrap();
        let _provider = DocumentHighlightProvider::new(documents);
    }

    #[test]
    fn test_document_highlight_variable() {
        let documents = DocumentManager::new().unwrap();
        let provider = DocumentHighlightProvider::new(documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"x = 42
y = x + 1
print(x)"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let params = DocumentHighlightParams {
            text_document_position_params: lsp_types::TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri },
                position: Position { line: 0, character: 0 }, // On 'x' in first line
            },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        };

        let highlights = provider.document_highlight(params);
        assert!(highlights.is_some());
        let highlights = highlights.unwrap();

        // Should find at least the definition
        assert!(!highlights.is_empty());
    }

    #[test]
    fn test_document_highlight_function() {
        let documents = DocumentManager::new().unwrap();
        let provider = DocumentHighlightProvider::new(documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"def hello():
    return 42

result = hello()"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let params = DocumentHighlightParams {
            text_document_position_params: lsp_types::TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri },
                position: Position { line: 0, character: 4 }, // On 'hello' in definition
            },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        };

        let _highlights = provider.document_highlight(params);
        // Currently returns None because function names don't have position info in AST
        // This will be fixed when we improve AST structure
    }

    #[test]
    fn test_document_highlight_no_symbol() {
        let documents = DocumentManager::new().unwrap();
        let provider = DocumentHighlightProvider::new(documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = "x = 42";

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let params = DocumentHighlightParams {
            text_document_position_params: lsp_types::TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri },
                position: Position { line: 0, character: 2 }, // On '=' sign
            },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        };

        let highlights = provider.document_highlight(params);
        assert!(highlights.is_none());
    }

    #[test]
    fn test_collect_highlights_assignment() {
        let documents = DocumentManager::new().unwrap();
        let provider = DocumentHighlightProvider::new(documents);

        let ast = AstNode::Assignment {
            target: "x".to_string(),
            value: Box::new(AstNode::Identifier { name: "x".to_string(), line: 1, col: 5 }),
            line: 1,
            col: 1,
        };

        let mut highlights = Vec::new();
        provider.collect_highlights(&ast, "x", &mut highlights, "x = x");

        // Should find both the write (target) and read (value)
        assert_eq!(highlights.len(), 2);
        assert_eq!(highlights[0].kind, Some(DocumentHighlightKind::WRITE));
        assert_eq!(highlights[1].kind, Some(DocumentHighlightKind::READ));
    }

    #[test]
    fn test_collect_highlights_in_function() {
        let documents = DocumentManager::new().unwrap();
        let provider = DocumentHighlightProvider::new(documents);

        let ast = AstNode::FunctionDef {
            name: "test".to_string(),
            args: vec![],
            body: vec![AstNode::Return {
                value: Some(Box::new(AstNode::Identifier {
                    name: "x".to_string(),
                    line: 2,
                    col: 12,
                })),
                line: 2,
                col: 5,
            }],
            line: 1,
            col: 1,
            docstring: None,
            return_type: None,
            decorators: Vec::new(),
        };

        let mut highlights = Vec::new();
        provider.collect_highlights(&ast, "x", &mut highlights, "def test():\n    return x");

        assert_eq!(highlights.len(), 1);
        assert_eq!(highlights[0].kind, Some(DocumentHighlightKind::READ));
    }

    #[test]
    fn test_collect_highlights_in_call() {
        let documents = DocumentManager::new().unwrap();
        let provider = DocumentHighlightProvider::new(documents);

        let ast = AstNode::Call {
            function: "print".to_string(),
            args: vec![AstNode::Identifier { name: "x".to_string(), line: 1, col: 7 }],
            line: 1,
            col: 1,
        };

        let mut highlights = Vec::new();
        provider.collect_highlights(&ast, "x", &mut highlights, "print(x)");

        assert_eq!(highlights.len(), 1);
        assert_eq!(highlights[0].kind, Some(DocumentHighlightKind::READ));
    }

    #[test]
    fn test_highlight_position_calculation() {
        let documents = DocumentManager::new().unwrap();
        let provider = DocumentHighlightProvider::new(documents);

        let ast = AstNode::Identifier { name: "variable_name".to_string(), line: 5, col: 10 };

        let mut highlights = Vec::new();
        provider.collect_highlights(&ast, "variable_name", &mut highlights, "");

        assert_eq!(highlights.len(), 1);
        assert_eq!(highlights[0].range.start.line, 4); // 0-indexed
        assert_eq!(highlights[0].range.start.character, 9); // 0-indexed
        assert_eq!(highlights[0].range.end.character, 9 + "variable_name".len() as u32);
    }
}

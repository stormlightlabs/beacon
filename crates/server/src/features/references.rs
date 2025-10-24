//! Find references provider
//!
//! Locates all references to a symbol across the workspace with scope-aware matching.

use crate::document::DocumentManager;
use crate::utils;
use beacon_parser::{Symbol, SymbolTable};
use lsp_types::{Location, Position, Range, ReferenceParams};
use url::Url;

pub struct ReferencesProvider {
    documents: DocumentManager,
}

impl ReferencesProvider {
    pub fn new(documents: DocumentManager) -> Self {
        Self { documents }
    }

    /// Find all references to the symbol at a position
    ///
    /// Uses scope-aware matching to ensure shadowed variables are handled correctly.
    pub fn find_references(&self, params: ReferenceParams) -> Vec<Location> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        let include_declaration = params.context.include_declaration;

        let (target_symbol, symbol_name) = match self.resolve_symbol_at_position(&uri, position) {
            Some(result) => result,
            None => return Vec::new(),
        };

        let mut locations = self.find_references_in_document(&uri, &symbol_name, &target_symbol, include_declaration);

        for document_uri in self.documents.all_documents() {
            if document_uri != uri {
                let other_refs = self.find_references_in_document(&document_uri, &symbol_name, &target_symbol, false);
                locations.extend(other_refs);
            }
        }

        locations
    }

    /// Resolve the symbol at the cursor position
    ///
    /// Returns the symbol and its name for reference matching.
    fn resolve_symbol_at_position(&self, uri: &Url, position: Position) -> Option<(Symbol, String)> {
        self.documents.get_document(uri, |doc| {
            let tree = doc.tree()?;
            let text = doc.text();
            let symbol_table = doc.symbol_table()?;
            let parser = crate::parser::LspParser::new().ok()?;
            let node = parser.node_at_position(tree, &text, position)?;

            if node.kind() != "identifier" {
                return None;
            }

            let identifier_text = node.utf8_text(text.as_bytes()).ok()?;
            let byte_offset = utils::position_to_byte_offset(&text, position);
            let scope = symbol_table.find_scope_at_position(byte_offset);
            let symbol = symbol_table.lookup_symbol(identifier_text, scope)?;

            Some((symbol.clone(), identifier_text.to_string()))
        })?
    }

    /// Find references within a single document
    ///
    /// Uses scope-aware matching to verify each identifier resolves to the target symbol.
    /// Traverses the tree-sitter CST directly to find all identifier nodes.
    fn find_references_in_document(
        &self, uri: &Url, symbol_name: &str, target_symbol: &Symbol, include_declaration: bool,
    ) -> Vec<Location> {
        self.documents
            .get_document(uri, |doc| {
                let mut locations = Vec::new();

                if let Some(tree) = doc.tree() {
                    if let Some(symbol_table) = doc.symbol_table() {
                        let text = doc.text();

                        self.collect_references_from_tree(
                            tree.root_node(),
                            symbol_name,
                            target_symbol,
                            symbol_table,
                            &text,
                            uri,
                            &mut locations,
                        );

                        if include_declaration {
                            if let Some(def_range) = self.symbol_to_range(&text, target_symbol, symbol_name) {
                                locations.push(Location { uri: uri.clone(), range: def_range });
                            }
                        }
                    }
                }
                locations
            })
            .unwrap_or_default()
    }

    /// Recursively collect references from the tree-sitter CST
    ///
    /// Traverses all nodes to find identifier nodes matching the symbol name.
    /// For each match, verifies it resolves to the target symbol using scope-aware lookup.
    /// Excludes the definition itself (only references/uses are collected).
    fn collect_references_from_tree(
        &self, node: tree_sitter::Node, symbol_name: &str, target_symbol: &Symbol, symbol_table: &SymbolTable,
        text: &str, uri: &Url, locations: &mut Vec<Location>,
    ) {
        if node.kind() == "identifier" {
            if let Ok(node_text) = node.utf8_text(text.as_bytes()) {
                if node_text == symbol_name {
                    let byte_offset = node.start_byte();
                    let scope = symbol_table.find_scope_at_position(byte_offset);

                    if let Some(resolved_symbol) = symbol_table.lookup_symbol(symbol_name, scope) {
                        if resolved_symbol.scope_id == target_symbol.scope_id
                            && resolved_symbol.line == target_symbol.line
                            && resolved_symbol.col == target_symbol.col
                        {
                            let node_line = node.start_position().row + 1;
                            let node_col = node.start_position().column + 1;

                            if node_line != target_symbol.line || node_col != target_symbol.col {
                                let range = utils::tree_sitter_range_to_lsp_range(text, node.range());
                                locations.push(Location { uri: uri.clone(), range });
                            }
                        }
                    }
                }
            }
        }

        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            self.collect_references_from_tree(child, symbol_name, target_symbol, symbol_table, text, uri, locations);
        }
    }

    /// Convert symbol definition to LSP Range
    fn symbol_to_range(&self, _text: &str, symbol: &Symbol, symbol_name: &str) -> Option<Range> {
        let position =
            Position { line: (symbol.line as u32).saturating_sub(1), character: (symbol.col as u32).saturating_sub(1) };
        let end_position = Position { line: position.line, character: position.character + symbol_name.len() as u32 };

        Some(Range { start: position, end: end_position })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lsp_types::ReferenceContext;
    use std::str::FromStr;

    fn create_test_provider() -> (ReferencesProvider, Url) {
        let documents = DocumentManager::new().unwrap();
        let provider = ReferencesProvider::new(documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        (provider, uri)
    }

    fn open_test_document(provider: &ReferencesProvider, uri: &Url, source: &str) {
        provider
            .documents
            .open_document(uri.clone(), 1, source.to_string())
            .unwrap();
    }

    #[test]
    fn test_provider_creation() {
        let documents = DocumentManager::new().unwrap();
        let _provider = ReferencesProvider::new(documents);
    }

    #[test]
    fn test_find_variable_references() {
        let (provider, uri) = create_test_provider();
        let source = r#"x = 42
y = x + 1
print(x)"#;
        open_test_document(&provider, &uri, source);

        let params = ReferenceParams {
            text_document_position: lsp_types::TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                position: Position { line: 0, character: 0 },
            },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
            context: ReferenceContext { include_declaration: false },
        };

        let locations = provider.find_references(params);

        assert_eq!(locations.len(), 2, "Expected 2 references to 'x'");
        assert_eq!(locations[0].range.start.line, 1);
        assert_eq!(locations[0].range.start.character, 4);
        assert_eq!(locations[1].range.start.line, 2);
        assert_eq!(locations[1].range.start.character, 6);
    }

    #[test]
    fn test_find_references_with_declaration() {
        let (provider, uri) = create_test_provider();
        let source = r#"x = 42
y = x + 1"#;
        open_test_document(&provider, &uri, source);

        let params = ReferenceParams {
            text_document_position: lsp_types::TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                position: Position { line: 0, character: 0 },
            },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
            context: ReferenceContext { include_declaration: true },
        };

        let locations = provider.find_references(params);

        assert_eq!(locations.len(), 2, "Expected declaration + 1 reference");
        assert!(locations.iter().any(|loc| loc.range.start.line == 0));
        assert!(locations.iter().any(|loc| loc.range.start.line == 1));
    }

    #[test]
    fn test_find_function_references() {
        let (provider, uri) = create_test_provider();
        let source = r#"def hello():
    pass

result = hello()
hello()"#;
        open_test_document(&provider, &uri, source);

        let params = ReferenceParams {
            text_document_position: lsp_types::TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                position: Position { line: 3, character: 9 },
            },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
            context: ReferenceContext { include_declaration: false },
        };

        let locations = provider.find_references(params);

        assert!(!locations.is_empty(), "Should find function call references");
    }

    #[test]
    fn test_shadowing_different_scopes() {
        let (provider, uri) = create_test_provider();
        let source = r#"x = 10

def foo():
    x = 20
    print(x)

print(x)"#;
        open_test_document(&provider, &uri, source);

        let params = ReferenceParams {
            text_document_position: lsp_types::TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                position: Position { line: 0, character: 0 },
            },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
            context: ReferenceContext { include_declaration: false },
        };

        let locations = provider.find_references(params);

        assert_eq!(locations.len(), 1, "Should only find outer scope references");
        assert_eq!(locations[0].range.start.line, 6);
    }

    #[test]
    fn test_nested_scope_references() {
        let (provider, uri) = create_test_provider();
        let source = r#"def outer():
    y = 5
    def inner():
        z = y + 1
        return y
    return inner()"#;
        open_test_document(&provider, &uri, source);

        let params = ReferenceParams {
            text_document_position: lsp_types::TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                position: Position { line: 1, character: 4 },
            },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
            context: ReferenceContext { include_declaration: false },
        };

        let locations = provider.find_references(params);

        assert_eq!(locations.len(), 2, "Should find references in nested scope");
    }

    #[test]
    fn test_no_references_found() {
        let (provider, uri) = create_test_provider();
        let source = r#"x = 42
y = 10"#;
        open_test_document(&provider, &uri, source);

        let params = ReferenceParams {
            text_document_position: lsp_types::TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                position: Position { line: 0, character: 0 },
            },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
            context: ReferenceContext { include_declaration: false },
        };

        let locations = provider.find_references(params);

        assert_eq!(locations.len(), 0, "Should find no references");
    }

    #[test]
    fn test_not_on_identifier() {
        let (provider, uri) = create_test_provider();
        let source = "x = 42";
        open_test_document(&provider, &uri, source);

        let params = ReferenceParams {
            text_document_position: lsp_types::TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri },
                position: Position { line: 0, character: 2 },
            },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
            context: ReferenceContext { include_declaration: false },
        };

        let locations = provider.find_references(params);

        assert_eq!(locations.len(), 0, "Should return empty when not on identifier");
    }

    #[test]
    fn test_undefined_symbol() {
        let (provider, uri) = create_test_provider();
        let source = "x = undefined_var";
        open_test_document(&provider, &uri, source);

        let params = ReferenceParams {
            text_document_position: lsp_types::TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri },
                position: Position { line: 0, character: 4 },
            },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
            context: ReferenceContext { include_declaration: false },
        };

        let locations = provider.find_references(params);

        assert_eq!(locations.len(), 0, "Should return empty for undefined symbol");
    }

    #[test]
    fn test_reference_in_function_call_arguments() {
        let (provider, uri) = create_test_provider();
        let source = r#"data = [1, 2, 3]
result = len(data)
print(data)"#;
        open_test_document(&provider, &uri, source);

        let params = ReferenceParams {
            text_document_position: lsp_types::TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                position: Position { line: 0, character: 0 },
            },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
            context: ReferenceContext { include_declaration: false },
        };

        let locations = provider.find_references(params);

        assert_eq!(locations.len(), 2, "Should find references in function arguments");
        assert_eq!(locations[0].range.start.line, 1);
        assert_eq!(locations[1].range.start.line, 2);
    }

    #[test]
    fn test_reference_in_assignment_value() {
        let (provider, uri) = create_test_provider();
        let source = r#"x = 10
y = x
z = x + y"#;
        open_test_document(&provider, &uri, source);

        let params = ReferenceParams {
            text_document_position: lsp_types::TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                position: Position { line: 0, character: 0 },
            },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
            context: ReferenceContext { include_declaration: false },
        };

        let locations = provider.find_references(params);

        assert_eq!(locations.len(), 2, "Should find all references in assignments");
    }
}

//! Go to definition provider
//!
//! Navigates to the definition of symbols (variables, functions, classes, imports).

use crate::document::DocumentManager;
use crate::workspace::Workspace;
use crate::{parser, utils};
use lsp_types::{GotoDefinitionParams, GotoDefinitionResponse, Location, Position};
use ropey::Rope;
use std::sync::Arc;
use tokio::sync::RwLock;
use tree_sitter::Node;
use url::Url;

pub struct GotoDefinitionProvider {
    documents: DocumentManager,
    workspace: Arc<RwLock<Workspace>>,
}

impl GotoDefinitionProvider {
    pub fn new(documents: DocumentManager, workspace: Arc<RwLock<Workspace>>) -> Self {
        Self { documents, workspace }
    }

    /// Find the definition of the symbol at a position
    ///
    /// Uses symbol table and name resolution with proper scope tracking
    pub fn goto_definition(&self, params: GotoDefinitionParams) -> Option<GotoDefinitionResponse> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        if let Some(location) = self.find_definition(&uri, position) {
            return Some(GotoDefinitionResponse::Scalar(location));
        }

        let is_import = self.documents.get_document(&uri, |doc| {
            let tree = doc.tree()?;
            let text = doc.text();
            let p = parser::LspParser::new().ok()?;
            let node = p.node_at_position(tree, &text, position)?;

            let mut current = node;
            while let Some(parent) = current.parent() {
                if matches!(parent.kind(), "import_statement" | "import_from_statement") {
                    return Some(true);
                }
                current = parent;
            }

            Some(false)
        });

        if is_import == Some(Some(true)) {
            let cross_file_location = tokio::task::block_in_place(|| {
                tokio::runtime::Handle::current()
                    .block_on(async { self.find_cross_file_definition(&uri, position).await })
            });

            return cross_file_location.map(GotoDefinitionResponse::Scalar);
        }

        None
    }

    /// Find the definition location for a symbol at a position by looking up the identifier at the
    /// given position in the symbol table and returns its definition location.
    ///
    /// Uses position-based scope resolution to find symbols in the correct lexical scope.
    fn find_definition(&self, uri: &Url, position: Position) -> Option<Location> {
        self.documents.get_document(uri, |doc| {
            let tree = doc.tree()?;
            let text = doc.text();
            let symbol_table = doc.symbol_table()?;
            let p = parser::LspParser::new().ok()?;
            let node = p.node_at_position(tree, &text, position)?;

            if node.kind() != "identifier" {
                None
            } else {
                let identifier_text = node.utf8_text(text.as_bytes()).ok()?;
                let byte_offset = utils::position_to_byte_offset(&text, position);
                let scope = symbol_table.find_scope_at_position(byte_offset);
                let symbol = symbol_table.lookup_symbol(identifier_text, scope)?;
                let range = self.find_identifier_range_at_definition(tree, &text, symbol, identifier_text)?;
                Some(Location { uri: uri.clone(), range })
            }
        })?
    }

    /// Find the range of an identifier at its definition location
    ///
    /// Symbols store the position of the entire statement (e.g., "def" for functions), but we want to
    /// return the range of the actual identifier name.
    fn find_identifier_range_at_definition(
        &self, tree: &tree_sitter::Tree, text: &str, symbol: &beacon_parser::Symbol, identifier_name: &str,
    ) -> Option<lsp_types::Range> {
        let line_idx = symbol.line.saturating_sub(1);
        let byte_col = symbol.col.saturating_sub(1);
        let rope = Rope::from_str(text);

        if line_idx >= rope.len_lines() {
            None
        } else {
            let symbol_byte_offset = rope.line_to_byte(line_idx) + byte_col;
            let node = tree
                .root_node()
                .descendant_for_byte_range(symbol_byte_offset, symbol_byte_offset)?;

            let identifier_node = Self::find_identifier_node(text, node, identifier_name)?;
            Some(utils::tree_sitter_range_to_lsp_range(text, identifier_node.range()))
        }
    }

    /// Traverses up the tree to find a node matching `identifier_name`.
    fn find_identifier_node<'a>(text: &str, mut node: Node<'a>, identifier_name: &str) -> Option<Node<'a>> {
        loop {
            if let Some(name_node) = node.child_by_field_name("name") {
                if name_node.kind() == "identifier" {
                    return Some(name_node);
                }
            }

            if Self::node_is_identifier_with_name(node, text, identifier_name) {
                return Some(node);
            }

            if let Some(left_node) = node.child_by_field_name("left") {
                if Self::node_is_identifier_with_name(left_node, text, identifier_name) {
                    return Some(left_node);
                }
            }

            if Self::is_import_node(node) {
                if let Some(found) = Self::find_in_imports(node, text, identifier_name) {
                    return Some(found);
                }
            }

            node = node.parent()?;
        }
    }

    fn node_is_identifier_with_name(node: Node, text: &str, name: &str) -> bool {
        node.kind() == "identifier" && node.utf8_text(text.as_bytes()).map(|t| t == name).unwrap_or(false)
    }

    fn is_import_node(node: Node) -> bool {
        matches!(node.kind(), "import_statement" | "import_from_statement")
    }

    fn find_in_imports<'a>(node: Node<'a>, text: &str, identifier_name: &str) -> Option<Node<'a>> {
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            match child.kind() {
                "dotted_name" | "identifier" => {
                    if let Ok(child_text) = child.utf8_text(text.as_bytes()) {
                        if child_text == identifier_name {
                            return Some(child);
                        }
                    }
                }
                "aliased_import" => {
                    for field in ["alias", "name"] {
                        if let Some(alias_or_name) = child.child_by_field_name(field) {
                            if Self::node_is_identifier_with_name(alias_or_name, text, identifier_name) {
                                return Some(alias_or_name);
                            }
                        }
                    }
                }
                _ => {}
            }
        }
        None
    }

    /// Find cross-file definitions by following imports
    ///
    /// Detects import statements and resolves them to their target files.
    /// Handles both `import foo` and `from foo import bar` cases.
    pub async fn find_cross_file_definition(&self, uri: &Url, position: Position) -> Option<Location> {
        let (module_name, is_relative) = self.documents.get_document(uri, |doc| {
            let tree = doc.tree()?;
            let text = doc.text();
            let p = parser::LspParser::new().ok()?;
            let node = p.node_at_position(tree, &text, position)?;

            if node.kind() != "identifier" && node.kind() != "dotted_name" {
                return None;
            }

            let mut current = node;
            while let Some(parent) = current.parent() {
                if parent.kind() == "import_statement" {
                    let name = parent
                        .child_by_field_name("name")
                        .and_then(|n| n.utf8_text(text.as_bytes()).ok())?
                        .to_string();
                    let is_rel = name.starts_with('.');
                    return Some((name, is_rel));
                } else if parent.kind() == "import_from_statement" {
                    let name = parent
                        .child_by_field_name("module_name")
                        .and_then(|n| n.utf8_text(text.as_bytes()).ok())?
                        .to_string();
                    let is_rel = name.starts_with('.');
                    return Some((name, is_rel));
                }
                current = parent;
            }

            None
        })??;

        let workspace = self.workspace.read().await;

        let target_uri = if is_relative {
            let from_module = workspace.uri_to_module_name(uri)?;
            let leading_dots = module_name.chars().take_while(|&c| c == '.').count();
            let rest = &module_name[leading_dots..];
            workspace.resolve_relative_import(&from_module, rest, leading_dots)?
        } else {
            workspace.resolve_import(&module_name)?
        };

        let range = if self.documents.has_document(&target_uri) {
            self.documents.get_document(&target_uri, |doc| {
                let tree = doc.tree()?;
                let text = doc.text();
                Some(utils::tree_sitter_range_to_lsp_range(&text, tree.root_node().range()))
            })?
        } else {
            let parse_result = workspace.load_workspace_file(&target_uri)?;
            let root_text = parse_result.rope.to_string();
            Some(utils::tree_sitter_range_to_lsp_range(
                &root_text,
                parse_result.tree.root_node().range(),
            ))
        }?;

        Some(Location { uri: target_uri, range })
    }

    /// TODO: Implement type definition lookup
    /// Use type inference to get type of expression
    /// Navigate to the definition of that type (class, protocol, etc.)
    pub fn _goto_type_definition(&self, _uri: &Url, _position: Position) -> Option<Location> {
        None
    }

    /// TODO: Navigate to implementation (for protocols/abstract methods)
    pub fn _goto_implementation(&self, _uri: &Url, _position: Position) -> Vec<Location> {
        Vec::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::Config;
    use std::str::FromStr;

    fn create_test_provider() -> (GotoDefinitionProvider, Url) {
        let documents = DocumentManager::new().unwrap();
        let config = Config::default();
        let workspace = Arc::new(RwLock::new(Workspace::new(None, config, documents.clone())));
        let provider = GotoDefinitionProvider::new(documents.clone(), workspace);
        let uri = Url::from_str("file:///test.py").unwrap();
        (provider, uri)
    }

    fn open_test_document(documents: &DocumentManager, uri: &Url, source: &str) {
        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();
    }

    #[test]
    fn test_provider_creation() {
        let documents = DocumentManager::new().unwrap();
        let config = Config::default();
        let workspace = Arc::new(RwLock::new(Workspace::new(None, config, documents.clone())));
        let _provider = GotoDefinitionProvider::new(documents, workspace);
    }

    #[test]
    fn test_goto_function_definition() {
        let (provider, uri) = create_test_provider();
        let source = r#"def hello():
    pass

hello()"#;
        open_test_document(&provider.documents, &uri, source);

        let params = GotoDefinitionParams {
            text_document_position_params: lsp_types::TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                position: Position { line: 3, character: 0 },
            },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        };

        let result = provider.goto_definition(params);
        assert!(result.is_some());

        match result.unwrap() {
            GotoDefinitionResponse::Scalar(location) => {
                assert_eq!(location.uri, uri);
                assert_eq!(location.range.start.line, 0);
                assert_eq!(location.range.start.character, 4);
                assert_eq!(location.range.end.line, 0);
                assert_eq!(location.range.end.character, 9);
            }
            _ => panic!("Expected scalar location"),
        }
    }

    #[test]
    fn test_goto_variable_definition() {
        let (provider, uri) = create_test_provider();
        let source = r#"x = 42
y = x + 10"#;
        open_test_document(&provider.documents, &uri, source);

        let params = GotoDefinitionParams {
            text_document_position_params: lsp_types::TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                position: Position { line: 1, character: 4 },
            },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        };

        let result = provider.goto_definition(params);
        assert!(result.is_some());

        match result.unwrap() {
            GotoDefinitionResponse::Scalar(location) => {
                assert_eq!(location.uri, uri);
                assert_eq!(location.range.start.line, 0);
                assert_eq!(location.range.start.character, 0);
                assert_eq!(location.range.end.character, 1);
            }
            _ => panic!("Expected scalar location"),
        }
    }

    #[test]
    fn test_goto_class_definition() {
        let (provider, uri) = create_test_provider();
        let source = r#"class MyClass:
    pass

obj = MyClass()"#;
        open_test_document(&provider.documents, &uri, source);

        let params = GotoDefinitionParams {
            text_document_position_params: lsp_types::TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                position: Position { line: 3, character: 6 },
            },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        };

        let result = provider.goto_definition(params);
        assert!(result.is_some());

        match result.unwrap() {
            GotoDefinitionResponse::Scalar(location) => {
                assert_eq!(location.uri, uri);
                assert_eq!(location.range.start.line, 0);
                assert_eq!(location.range.start.character, 6);
                assert_eq!(location.range.end.character, 13);
            }
            _ => panic!("Expected scalar location"),
        }
    }

    #[test]
    fn test_goto_definition_not_identifier() {
        let (provider, uri) = create_test_provider();
        let source = "x = 42";
        open_test_document(&provider.documents, &uri, source);

        let params = GotoDefinitionParams {
            text_document_position_params: lsp_types::TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri },
                position: Position { line: 0, character: 2 },
            },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        };

        let result = provider.goto_definition(params);
        assert!(result.is_none());
    }

    #[test]
    fn test_goto_definition_undefined_symbol() {
        let (provider, uri) = create_test_provider();
        let source = "x = undefined_var";
        open_test_document(&provider.documents, &uri, source);

        let params = GotoDefinitionParams {
            text_document_position_params: lsp_types::TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri },
                position: Position { line: 0, character: 4 },
            },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        };

        let result = provider.goto_definition(params);
        assert!(result.is_none());
    }

    #[test]
    fn test_goto_definition_multi_line_function() {
        let (provider, uri) = create_test_provider();
        let source = r#"def long_function_name(
    param1,
    param2
):
    pass

long_function_name(1, 2)"#;
        open_test_document(&provider.documents, &uri, source);

        let params = GotoDefinitionParams {
            text_document_position_params: lsp_types::TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                position: Position { line: 6, character: 0 },
            },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        };

        let result = provider.goto_definition(params);
        assert!(result.is_some());

        match result.unwrap() {
            GotoDefinitionResponse::Scalar(location) => {
                assert_eq!(location.uri, uri);
                assert_eq!(location.range.start.line, 0);
                assert_eq!(location.range.start.character, 4);
            }
            _ => panic!("Expected scalar location"),
        }
    }

    #[test]
    fn test_goto_import_definition() {
        let (provider, uri) = create_test_provider();
        let source = r#"import os
x = os"#;
        open_test_document(&provider.documents, &uri, source);

        let params = GotoDefinitionParams {
            text_document_position_params: lsp_types::TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                position: Position { line: 1, character: 4 },
            },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        };

        let result = provider.goto_definition(params);
        assert!(result.is_some());

        match result.unwrap() {
            GotoDefinitionResponse::Scalar(location) => {
                assert_eq!(location.uri, uri);
                assert_eq!(location.range.start.line, 0);
            }
            _ => panic!("Expected scalar location"),
        }
    }

    #[test]
    fn test_goto_definition_with_utf8_chars() {
        let (provider, uri) = create_test_provider();
        let source = "café = '☕'\nx = café";
        open_test_document(&provider.documents, &uri, source);

        let params = GotoDefinitionParams {
            text_document_position_params: lsp_types::TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                position: Position { line: 1, character: 4 },
            },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        };

        let result = provider.goto_definition(params);
        assert!(result.is_some());

        match result.unwrap() {
            GotoDefinitionResponse::Scalar(location) => {
                assert_eq!(location.uri, uri);
                assert_eq!(location.range.start.line, 0);
                assert_eq!(location.range.start.character, 0);
                assert_eq!(location.range.end.character, 4); // "café" in UTF-16 is 4 code units
            }
            _ => panic!("Expected scalar location"),
        }
    }

    #[test]
    fn test_goto_definition_local_variable() {
        let (provider, uri) = create_test_provider();
        let source = r#"def foo():
    test_data = [1, 2, 3]
    result = test_data
    return result"#;
        open_test_document(&provider.documents, &uri, source);

        let params = GotoDefinitionParams {
            text_document_position_params: lsp_types::TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                position: Position { line: 2, character: 13 },
            },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        };

        let result = provider.goto_definition(params);
        assert!(result.is_some());

        match result.unwrap() {
            GotoDefinitionResponse::Scalar(location) => {
                assert_eq!(location.uri, uri);
                assert_eq!(location.range.start.line, 1);
                assert_eq!(location.range.start.character, 4);
            }
            _ => panic!("Expected scalar location"),
        }
    }

    #[test]
    fn test_goto_definition_nested_function_variable() {
        let (provider, uri) = create_test_provider();
        let source = r#"def outer():
    x = 10
    def inner():
        y = x + 5
        return y
    return inner()"#;
        open_test_document(&provider.documents, &uri, source);

        let params = GotoDefinitionParams {
            text_document_position_params: lsp_types::TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                position: Position { line: 4, character: 15 },
            },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        };

        let result = provider.goto_definition(params);
        assert!(result.is_some());

        match result.unwrap() {
            GotoDefinitionResponse::Scalar(location) => {
                assert_eq!(location.uri, uri);
                assert_eq!(location.range.start.line, 3); // Definition on line 3
                assert_eq!(location.range.start.character, 8); // Start of 'y'
            }
            _ => panic!("Expected scalar location"),
        }
    }

    #[test]
    fn test_goto_definition_document_not_found() {
        let (provider, _) = create_test_provider();
        let nonexistent_uri = Url::from_str("file:///nonexistent.py").unwrap();

        let params = GotoDefinitionParams {
            text_document_position_params: lsp_types::TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: nonexistent_uri },
                position: Position { line: 0, character: 0 },
            },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        };

        let result = provider.goto_definition(params);
        assert!(result.is_none());
    }
}

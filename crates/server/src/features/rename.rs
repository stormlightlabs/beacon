//! Rename provider
//!
//! Renames a symbol across all references in the workspace.
//! Validates the new name and creates workspace edits for all occurrences.

use crate::{document::DocumentManager, parser, utils, workspace::Workspace};

use beacon_parser::{Symbol, SymbolTable};
use lsp_types::{Position, RenameParams, TextEdit, Url, WorkspaceEdit};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;

/// Rename provider
///
/// Handles symbol renaming across the workspace with validation by finding all references and creates appropriate [`TextEdit`].
pub struct RenameProvider {
    documents: DocumentManager,
    workspace: Arc<RwLock<Workspace>>,
}

impl RenameProvider {
    pub fn new(documents: DocumentManager, workspace: Arc<RwLock<Workspace>>) -> Self {
        Self { documents, workspace }
    }

    /// Rename a symbol at the given position
    pub async fn rename(&self, params: RenameParams) -> Option<WorkspaceEdit> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        let new_name = params.new_name;

        if !Self::is_valid_identifier(&new_name) {
            return None;
        }

        let (symbol_name, target_symbol) = self.resolve_symbol_at_position(&uri, position)?;

        let edits = self
            .find_all_references(&uri, &symbol_name, &new_name, &target_symbol)
            .await?;

        if edits.is_empty() {
            return None;
        }

        Some(WorkspaceEdit { changes: Some(edits), document_changes: None, change_annotations: None })
    }

    /// Resolve the symbol at the cursor position
    fn resolve_symbol_at_position(&self, uri: &Url, position: Position) -> Option<(String, Symbol)> {
        self.documents.get_document(uri, |doc| {
            let tree = doc.tree()?;
            let text = doc.text();
            let symbol_table = doc.symbol_table()?;
            let p = parser::LspParser::new().ok()?;
            let node = p.node_at_position(tree, &text, position)?;

            if node.kind() != "identifier" {
                return None;
            }

            let identifier_text = node.utf8_text(text.as_bytes()).ok()?;
            let byte_offset = utils::position_to_byte_offset(&text, position);
            let scope = symbol_table.find_scope_at_position(byte_offset);
            let symbol = symbol_table.lookup_symbol(identifier_text, scope)?;

            Some((identifier_text.to_string(), symbol.clone()))
        })?
    }

    /// Find all references to a symbol and create text edits
    async fn find_all_references(
        &self, uri: &Url, symbol_name: &str, new_name: &str, target_symbol: &Symbol,
    ) -> Option<HashMap<Url, Vec<TextEdit>>> {
        let mut changes = HashMap::new();

        if let Some(edits) = self.find_renames_in_document(uri, symbol_name, new_name, target_symbol) {
            if !edits.is_empty() {
                changes.insert(uri.clone(), edits);
            }
        }

        for document_uri in self.documents.all_documents() {
            if document_uri != *uri {
                if let Some(edits) = self.find_renames_in_document(&document_uri, symbol_name, new_name, target_symbol)
                {
                    if !edits.is_empty() {
                        changes.insert(document_uri, edits);
                    }
                }
            }
        }

        let workspace = self.workspace.read().await;
        let dependents = workspace.get_dependents(uri);
        for dependent_uri in dependents {
            if !self.documents.has_document(&dependent_uri) {
                if let Some(edits) = self.find_renames_in_workspace_file(
                    &dependent_uri,
                    symbol_name,
                    new_name,
                    target_symbol,
                    &workspace,
                ) {
                    if !edits.is_empty() {
                        changes.insert(dependent_uri, edits);
                    }
                }
            }
        }

        Some(changes)
    }

    /// Find renames in an open document using scope-aware matching
    ///
    /// Uses symbol table to verify each identifier resolves to the target symbol.
    fn find_renames_in_document(
        &self, uri: &Url, symbol_name: &str, new_name: &str, target_symbol: &Symbol,
    ) -> Option<Vec<TextEdit>> {
        self.documents
            .get_document(uri, |doc| {
                let tree = doc.tree()?;
                let symbol_table = doc.symbol_table()?;
                let text = doc.text();

                let mut edits = Vec::new();
                Self::collect_renames_from_tree(
                    tree.root_node(),
                    symbol_name,
                    new_name,
                    target_symbol,
                    symbol_table,
                    &text,
                    &mut edits,
                );

                Some(edits)
            })
            .unwrap_or(None)
    }

    /// Find renames in a workspace file that is not currently open
    ///
    /// Loads the file, parses it, and searches for references using scope-aware matching.
    fn find_renames_in_workspace_file(
        &self, uri: &Url, symbol_name: &str, new_name: &str, target_symbol: &Symbol, workspace: &Workspace,
    ) -> Option<Vec<TextEdit>> {
        let parse_result = workspace.load_workspace_file(uri)?;
        let text = parse_result.rope.to_string();
        let mut edits = Vec::new();

        Self::collect_renames_from_tree(
            parse_result.tree.root_node(),
            symbol_name,
            new_name,
            target_symbol,
            &parse_result.symbol_table,
            &text,
            &mut edits,
        );

        Some(edits)
    }

    /// Recursively collect renames from the tree-sitter CST using scope-aware matching
    ///
    /// Traverses all nodes to find identifier nodes matching the symbol name.
    /// For each match, verifies it resolves to the target symbol using scope-aware lookup.
    fn collect_renames_from_tree(
        node: tree_sitter::Node, symbol_name: &str, new_name: &str, target_symbol: &Symbol, symbol_table: &SymbolTable,
        text: &str, edits: &mut Vec<TextEdit>,
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
                            let range = utils::tree_sitter_range_to_lsp_range(text, node.range());
                            edits.push(TextEdit { range, new_text: new_name.to_string() });
                        }
                    }
                }
            }
        }

        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            Self::collect_renames_from_tree(child, symbol_name, new_name, target_symbol, symbol_table, text, edits);
        }
    }

    /// Validate that a string is a valid Python identifier
    ///
    /// Must start with letter or underscore, followed by alphanumeric or underscore.
    fn is_valid_identifier(name: &str) -> bool {
        if name.is_empty() {
            return false;
        }

        let mut chars = name.chars();
        let first = chars.next().unwrap();

        if !first.is_alphabetic() && first != '_' {
            return false;
        }

        chars.all(|c| c.is_alphanumeric() || c == '_')
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::Config;
    use std::str::FromStr;

    fn create_test_provider() -> RenameProvider {
        let documents = DocumentManager::new().unwrap();
        let config = Config::default();
        let workspace = Arc::new(RwLock::new(Workspace::new(None, config, documents.clone())));
        RenameProvider::new(documents, workspace)
    }

    #[test]
    fn test_provider_creation() {
        let _ = create_test_provider();
    }

    #[test]
    fn test_is_valid_identifier() {
        assert!(RenameProvider::is_valid_identifier("valid_name"));
        assert!(RenameProvider::is_valid_identifier("_private"));
        assert!(RenameProvider::is_valid_identifier("name123"));
        assert!(RenameProvider::is_valid_identifier("CamelCase"));

        assert!(!RenameProvider::is_valid_identifier(""));
        assert!(!RenameProvider::is_valid_identifier("123invalid"));
        assert!(!RenameProvider::is_valid_identifier("has-dash"));
        assert!(!RenameProvider::is_valid_identifier("has space"));
        assert!(!RenameProvider::is_valid_identifier("has.dot"));
    }

    #[tokio::test]
    async fn test_rename_variable() {
        let documents = DocumentManager::new().unwrap();
        let config = Config::default();
        let workspace = Arc::new(RwLock::new(Workspace::new(None, config, documents.clone())));
        let provider = RenameProvider::new(documents.clone(), workspace);
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"x = 42
y = x + 1
print(x)"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let params = RenameParams {
            text_document_position: lsp_types::TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                position: Position { line: 0, character: 0 },
            },
            new_name: "renamed_var".to_string(),
            work_done_progress_params: Default::default(),
        };

        let result = provider.rename(params).await;
        assert!(result.is_some());

        let workspace_edit = result.unwrap();
        assert!(workspace_edit.changes.is_some());

        let changes = workspace_edit.changes.unwrap();
        assert!(changes.contains_key(&uri));

        let edits = &changes[&uri];
        assert!(!edits.is_empty());

        for edit in edits {
            assert_eq!(edit.new_text, "renamed_var");
        }
    }

    #[tokio::test]
    async fn test_rename_invalid_name() {
        let documents = DocumentManager::new().unwrap();
        let config = Config::default();
        let workspace = Arc::new(RwLock::new(Workspace::new(None, config, documents.clone())));
        let provider = RenameProvider::new(documents.clone(), workspace);
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = "x = 42";

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let params = RenameParams {
            text_document_position: lsp_types::TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri },
                position: Position { line: 0, character: 0 },
            },
            new_name: "123invalid".to_string(),
            work_done_progress_params: Default::default(),
        };

        let result = provider.rename(params).await;
        assert!(result.is_none());
    }

    #[tokio::test]
    async fn test_rename_not_on_identifier() {
        let documents = DocumentManager::new().unwrap();
        let config = Config::default();
        let workspace = Arc::new(RwLock::new(Workspace::new(None, config, documents.clone())));
        let provider = RenameProvider::new(documents.clone(), workspace);
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = "x = 42";

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let params = RenameParams {
            text_document_position: lsp_types::TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri },
                position: Position { line: 0, character: 2 },
            },
            new_name: "new_name".to_string(),
            work_done_progress_params: Default::default(),
        };

        let result = provider.rename(params).await;
        assert!(result.is_none());
    }

    #[tokio::test]
    async fn test_rename_with_multiple_occurrences() {
        let documents = DocumentManager::new().unwrap();
        let config = Config::default();
        let workspace = Arc::new(RwLock::new(Workspace::new(None, config, documents.clone())));
        let provider = RenameProvider::new(documents.clone(), workspace);

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"def calculate(x):
    result = x * 2
    return result"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let params = RenameParams {
            text_document_position: lsp_types::TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                position: Position { line: 1, character: 13 },
            },
            new_name: "value".to_string(),
            work_done_progress_params: Default::default(),
        };

        let result = provider.rename(params).await;
        assert!(result.is_some());

        let workspace_edit = result.unwrap();
        let changes = workspace_edit.changes.unwrap();
        let edits = &changes[&uri];

        assert!(!edits.is_empty());
    }

    #[test]
    fn test_is_valid_identifier_unicode() {
        assert!(RenameProvider::is_valid_identifier("café"));
        assert!(RenameProvider::is_valid_identifier("π"));
        assert!(RenameProvider::is_valid_identifier("变量"));
    }

    #[test]
    fn test_is_valid_identifier_keywords() {
        assert!(RenameProvider::is_valid_identifier("if"));
        assert!(RenameProvider::is_valid_identifier("for"));
        assert!(RenameProvider::is_valid_identifier("class"));
    }

    #[tokio::test]
    async fn test_cross_file_rename() {
        let documents = DocumentManager::new().unwrap();
        let config = Config::default();
        let workspace = Arc::new(RwLock::new(Workspace::new(None, config, documents.clone())));
        let provider = RenameProvider::new(documents.clone(), workspace);
        let module_uri = Url::from_str("file:///module.py").unwrap();
        let module_source = r#"def helper_function():
    return 42"#;

        documents
            .open_document(module_uri.clone(), 1, module_source.to_string())
            .unwrap();

        let main_uri = Url::from_str("file:///main.py").unwrap();
        let main_source = r#"from module import helper_function

result = helper_function()"#;

        documents
            .open_document(main_uri.clone(), 1, main_source.to_string())
            .unwrap();

        let params = RenameParams {
            text_document_position: lsp_types::TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: module_uri.clone() },
                position: Position { line: 0, character: 4 },
            },
            new_name: "new_helper".to_string(),
            work_done_progress_params: Default::default(),
        };

        let result = provider.rename(params).await;
        assert!(result.is_some(), "Rename should return a WorkspaceEdit");

        let workspace_edit = result.unwrap();
        assert!(workspace_edit.changes.is_some());

        let changes = workspace_edit.changes.unwrap();

        assert!(changes.contains_key(&module_uri), "Should have edits in module.py");

        for (_, edits) in changes.iter() {
            for edit in edits {
                assert_eq!(edit.new_text, "new_helper");
            }
        }
    }
}

//! Multi-file refactoring infrastructure
//!
//! Provides generic utilities and traits for implementing refactoring operations that work across multiple files in the workspace.

use crate::{document::DocumentManager, parser, utils, workspace::Workspace};
use beacon_parser::{Symbol, SymbolTable};
use lsp_types::{Position, TextEdit, Url, WorkspaceEdit};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;

/// Context for refactoring operations providing access to workspace resources
#[derive(Clone)]
pub struct RefactoringContext {
    /// Document manager for accessing open files
    pub documents: DocumentManager,
    /// Workspace for dependency tracking and loading workspace files
    pub workspace: Arc<RwLock<Workspace>>,
}

impl RefactoringContext {
    /// Create a new refactoring context
    pub fn new(documents: DocumentManager, workspace: Arc<RwLock<Workspace>>) -> Self {
        Self { documents, workspace }
    }

    /// Resolve the symbol at a cursor position
    pub fn resolve_symbol_at_position(&self, uri: &Url, position: Position) -> Option<(String, Symbol)> {
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

    /// Get the tree-sitter tree and text for a document
    pub fn get_tree_and_text(&self, uri: &Url) -> Option<(tree_sitter::Tree, String)> {
        self.documents.get_document(uri, |doc| {
            let tree = doc.tree()?.clone();
            let text = doc.text();
            Some((tree, text))
        })?
    }

    /// Get symbol table for a document
    pub fn get_symbol_table(&self, uri: &Url) -> Option<SymbolTable> {
        self.documents.get_document(uri, |doc| doc.symbol_table().cloned())?
    }

    /// Iterate over all files relevant to a refactoring starting from a source URI
    ///
    /// Yields files in this order:
    /// 1. The source file itself
    /// 2. Other open documents
    /// 3. Workspace files that depend on the source (not already open)
    pub async fn iter_relevant_files(&self, source_uri: &Url) -> Vec<FileContext> {
        let mut files = Vec::new();

        if let Some(ctx) = self.get_file_context(source_uri) {
            files.push(ctx);
        }

        for uri in self.documents.all_documents() {
            if uri != *source_uri {
                if let Some(ctx) = self.get_file_context(&uri) {
                    files.push(ctx);
                }
            }
        }

        let workspace = self.workspace.read().await;
        for dependent_uri in workspace.get_dependents(source_uri) {
            if !self.documents.has_document(&dependent_uri) {
                if let Some(ctx) = self.get_workspace_file_context(&dependent_uri, &workspace) {
                    files.push(ctx);
                }
            }
        }

        files
    }

    /// Get file context for an open document
    fn get_file_context(&self, uri: &Url) -> Option<FileContext> {
        self.documents.get_document(uri, |doc| {
            let tree = doc.tree()?.clone();
            let symbol_table = doc.symbol_table()?.clone();
            let text = doc.text();

            Some(FileContext { uri: uri.clone(), tree, symbol_table, text })
        })?
    }

    /// Get file context for a workspace file (not currently open)
    fn get_workspace_file_context(&self, uri: &Url, workspace: &Workspace) -> Option<FileContext> {
        let parse_result = workspace.load_workspace_file(uri)?;
        let text = parse_result.rope.to_string();

        Some(FileContext { uri: uri.clone(), tree: parse_result.tree, symbol_table: parse_result.symbol_table, text })
    }
}

/// Context for a single file including parse tree and symbol table
pub struct FileContext {
    pub uri: Url,
    pub tree: tree_sitter::Tree,
    pub symbol_table: SymbolTable,
    pub text: String,
}

/// Collector for building workspace edits across multiple files
pub struct EditCollector {
    changes: HashMap<Url, Vec<TextEdit>>,
}

impl EditCollector {
    /// Create a new edit collector
    pub fn new() -> Self {
        Self { changes: HashMap::new() }
    }
    /// Add a text edit for a file
    pub fn add_edit(&mut self, uri: Url, edit: TextEdit) {
        self.changes.entry(uri).or_default().push(edit);
    }
    /// Add multiple edits for a file
    pub fn add_edits(&mut self, uri: Url, edits: Vec<TextEdit>) {
        if !edits.is_empty() {
            self.changes.entry(uri).or_default().extend(edits);
        }
    }
    /// Check if any edits have been collected
    pub fn is_empty(&self) -> bool {
        self.changes.is_empty()
    }
    /// Convert collected edits into a WorkspaceEdit
    pub fn into_workspace_edit(self) -> Option<WorkspaceEdit> {
        if self.changes.is_empty() {
            return None;
        }

        Some(WorkspaceEdit { changes: Some(self.changes), document_changes: None, change_annotations: None })
    }
}

impl Default for EditCollector {
    fn default() -> Self {
        Self::new()
    }
}

/// Utility for traversing tree-sitter nodes and collecting matches
pub struct NodeTraverser;

impl NodeTraverser {
    /// Traverse tree and collect nodes matching a predicate
    ///
    /// The predicate receives the node, text, and symbol table, and should return
    /// Some(T) for nodes to collect, None otherwise.
    pub fn collect_nodes<T, F>(
        node: tree_sitter::Node, text: &str, symbol_table: &SymbolTable, predicate: &F, results: &mut Vec<T>,
    ) where
        F: Fn(tree_sitter::Node, &str, &SymbolTable) -> Option<T>,
    {
        if let Some(result) = predicate(node, text, symbol_table) {
            results.push(result);
        }

        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            Self::collect_nodes(child, text, symbol_table, predicate, results);
        }
    }

    /// Find all identifier nodes matching a name with scope-aware verification
    pub fn find_matching_identifiers(
        node: tree_sitter::Node, symbol_name: &str, target_symbol: &Symbol, symbol_table: &SymbolTable, text: &str,
        new_text: &str,
    ) -> Vec<TextEdit> {
        let mut edits = Vec::new();
        Self::collect_matching_identifiers(
            node,
            symbol_name,
            target_symbol,
            symbol_table,
            text,
            new_text,
            &mut edits,
        );
        edits
    }

    /// Recursive helper for collecting matching identifiers
    fn collect_matching_identifiers(
        node: tree_sitter::Node, symbol_name: &str, target_symbol: &Symbol, symbol_table: &SymbolTable, text: &str,
        new_text: &str, edits: &mut Vec<TextEdit>,
    ) {
        if node.kind() == "identifier" {
            if let Ok(node_text) = node.utf8_text(text.as_bytes()) {
                if node_text == symbol_name {
                    let byte_offset = node.start_byte();
                    let scope = symbol_table.find_scope_at_position(byte_offset);

                    if let Some(resolved_symbol) = symbol_table.lookup_symbol(symbol_name, scope) {
                        if Self::symbols_match(resolved_symbol, target_symbol) {
                            let range = utils::tree_sitter_range_to_lsp_range(text, node.range());
                            edits.push(TextEdit { range, new_text: new_text.to_string() });
                        }
                    }
                }
            }
        }

        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            Self::collect_matching_identifiers(child, symbol_name, target_symbol, symbol_table, text, new_text, edits);
        }
    }

    /// Check if two symbols represent the same definition
    fn symbols_match(a: &Symbol, b: &Symbol) -> bool {
        a.scope_id == b.scope_id && a.line == b.line && a.col == b.col
    }
}

/// Validator utilities for refactoring operations
pub struct RefactoringValidator;

impl RefactoringValidator {
    /// Validate that a string is a valid Python identifier
    pub fn is_valid_identifier(name: &str) -> bool {
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

    /// Check if a name conflicts with Python keywords
    pub fn is_python_keyword(name: &str) -> bool {
        matches!(
            name,
            "False"
                | "None"
                | "True"
                | "and"
                | "as"
                | "assert"
                | "async"
                | "await"
                | "break"
                | "class"
                | "continue"
                | "def"
                | "del"
                | "elif"
                | "else"
                | "except"
                | "finally"
                | "for"
                | "from"
                | "global"
                | "if"
                | "import"
                | "in"
                | "is"
                | "lambda"
                | "nonlocal"
                | "not"
                | "or"
                | "pass"
                | "raise"
                | "return"
                | "try"
                | "while"
                | "with"
                | "yield"
        )
    }

    /// Validate identifier is valid and not a keyword
    pub fn validate_identifier(name: &str) -> Result<(), String> {
        if !Self::is_valid_identifier(name) {
            return Err(format!("'{name}' is not a valid Python identifier"));
        }

        if Self::is_python_keyword(name) {
            return Err(format!("'{name}' is a Python keyword"));
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_edit_collector() {
        let mut collector = EditCollector::new();
        assert!(collector.is_empty());

        let uri = Url::parse("file:///test.py").unwrap();
        let edit = TextEdit {
            range: lsp_types::Range {
                start: Position { line: 0, character: 0 },
                end: Position { line: 0, character: 1 },
            },
            new_text: "new".to_string(),
        };

        collector.add_edit(uri.clone(), edit);
        assert!(!collector.is_empty());

        let workspace_edit = collector.into_workspace_edit();
        assert!(workspace_edit.is_some());
    }

    #[test]
    fn test_validator_identifier() {
        assert!(RefactoringValidator::is_valid_identifier("valid_name"));
        assert!(RefactoringValidator::is_valid_identifier("_private"));
        assert!(RefactoringValidator::is_valid_identifier("name123"));

        assert!(!RefactoringValidator::is_valid_identifier(""));
        assert!(!RefactoringValidator::is_valid_identifier("123invalid"));
        assert!(!RefactoringValidator::is_valid_identifier("has-dash"));
    }

    #[test]
    fn test_validator_keywords() {
        assert!(RefactoringValidator::is_python_keyword("if"));
        assert!(RefactoringValidator::is_python_keyword("for"));
        assert!(RefactoringValidator::is_python_keyword("class"));
        assert!(!RefactoringValidator::is_python_keyword("valid_name"));
    }

    #[test]
    fn test_validate_identifier() {
        assert!(RefactoringValidator::validate_identifier("valid_name").is_ok());
        assert!(RefactoringValidator::validate_identifier("if").is_err());
        assert!(RefactoringValidator::validate_identifier("123invalid").is_err());
    }
}

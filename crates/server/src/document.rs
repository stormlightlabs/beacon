//! Document state management for LSP
//!
//! Tracks open documents with their parse trees, ASTs, symbol tables, and versions.
//! Handles incremental updates and maintains derived analysis artifacts.

use crate::{
    parser::{LspParser, ParseResult},
    utils,
};
use beacon_core::{DocumentError, Result};
use lsp_types::{TextDocumentContentChangeEvent, VersionedTextDocumentIdentifier};
use ropey::Rope;
use rustc_hash::FxHashMap;
use std::sync::{Arc, RwLock};
use url::Url;

/// Manages the state of a single document
///
/// Contains the source text, parse tree, AST, symbol table, and version.
/// All derived artifacts are kept in sync with the document content.
pub struct Document {
    /// Document URI
    pub uri: Url,

    /// Current version number (from LSP)
    pub version: i32,

    /// Document text as a Rope for efficient edits
    pub rope: Arc<Rope>,

    /// Parse result including tree, AST, and symbol table
    pub parse_result: Option<ParseResult>,

    /// Per-file type checking mode override (from `# beacon: mode=strict` directive)
    pub mode_override: Option<crate::config::TypeCheckingMode>,
}

impl Document {
    /// Create a new document from initial text
    pub fn new(uri: Url, version: i32, text: String) -> Self {
        let rope = Arc::new(Rope::from_str(&text));

        Self { uri, version, rope, parse_result: None, mode_override: None }
    }

    /// Get the full text of the document as a String
    pub fn text(&self) -> String {
        self.rope.to_string()
    }

    /// Apply content changes from LSP didChange events
    ///
    /// Handles both full document sync and incremental sync.
    pub fn apply_changes(&mut self, changes: Vec<TextDocumentContentChangeEvent>) {
        for change in changes {
            match change.range {
                Some(range) => {
                    let start_offset = utils::position_to_byte_offset(&self.text(), range.start);
                    let end_offset = utils::position_to_byte_offset(&self.text(), range.end);
                    let rope_mut = Arc::make_mut(&mut self.rope);
                    rope_mut.remove(start_offset..end_offset);
                    rope_mut.insert(start_offset, &change.text);
                }
                None => self.rope = Arc::new(Rope::from_str(&change.text)),
            }
        }
    }

    /// Reparse the document after changes and extract per-file mode directives from comments.
    pub fn reparse(&mut self, parser: &mut LspParser) -> Result<()> {
        let text = self.text();
        let result = parser.parse(&text)?;

        self.parse_result = Some(result);
        self.mode_override = crate::config::parse_mode_directive(&text);

        if let Some(mode) = self.mode_override {
            tracing::debug!("Found per-file mode directive: {} for {}", mode.as_str(), self.uri);
        }

        Ok(())
    }

    /// Get the parse tree
    pub fn tree(&self) -> Option<&tree_sitter::Tree> {
        self.parse_result.as_ref().map(|r| &r.tree)
    }

    /// Get the AST
    pub fn ast(&self) -> Option<&beacon_parser::AstNode> {
        self.parse_result.as_ref().map(|r| &r.ast)
    }

    /// Get the symbol table
    pub fn symbol_table(&self) -> Option<&beacon_parser::SymbolTable> {
        self.parse_result.as_ref().map(|r| &r.symbol_table)
    }

    /// Get the effective type checking mode for this document
    ///
    /// Returns the per-file mode override if present, otherwise falls back to the workspace mode.
    pub fn effective_mode(&self, workspace_mode: crate::config::TypeCheckingMode) -> crate::config::TypeCheckingMode {
        self.mode_override.unwrap_or(workspace_mode)
    }
}

/// Manages all open documents in the workspace
///
/// Thread-safe collection of documents with efficient lookup by URI.
#[derive(Clone)]
pub struct DocumentManager {
    documents: Arc<RwLock<FxHashMap<Url, Document>>>,
    parser: Arc<RwLock<LspParser>>,
}

impl DocumentManager {
    /// Create a new document manager
    pub fn new() -> Result<Self> {
        let parser = LspParser::new()?;
        Ok(Self { documents: Arc::new(RwLock::new(FxHashMap::default())), parser: Arc::new(RwLock::new(parser)) })
    }

    /// Open a new document
    ///
    /// Called when the client sends textDocument/didOpen.
    pub fn open_document(&self, uri: Url, version: i32, text: String) -> Result<()> {
        let mut document = Document::new(uri.clone(), version, text);
        let mut parser = self.parser.write().unwrap();
        document.reparse(&mut parser)?;

        let mut documents = self.documents.write().unwrap();
        documents.insert(uri, document);

        Ok(())
    }

    /// Close a document
    ///
    /// Called when the client sends textDocument/didClose.
    pub fn close_document(&self, uri: &Url) {
        let mut documents = self.documents.write().unwrap();
        documents.remove(uri);
    }

    /// Update a document with changes
    ///
    /// Called when the client sends textDocument/didChange.
    pub fn update_document(
        &self, params: VersionedTextDocumentIdentifier, changes: Vec<TextDocumentContentChangeEvent>,
    ) -> Result<()> {
        let mut documents = self.documents.write().unwrap();

        let document = documents
            .get_mut(&params.uri)
            .ok_or_else(|| DocumentError::DocumentNotFound(params.uri.clone()))?;

        document.version = params.version;
        document.apply_changes(changes);

        let mut parser = self.parser.write().unwrap();
        document.reparse(&mut parser)?;

        Ok(())
    }

    /// Get a document by URI with read access
    ///
    /// Returns a guard that allows reading the document.
    /// TODO: Consider returning a snapshot instead of holding the lock
    pub fn get_document<F, R>(&self, uri: &Url, f: F) -> Option<R>
    where
        F: FnOnce(&Document) -> R,
    {
        let documents = self.documents.read().unwrap();
        documents.get(uri).map(f)
    }

    /// Get a mutable reference to a document
    ///
    /// TODO: Consider a better pattern that doesn't expose mutable state
    pub fn get_document_mut<F, R>(&self, uri: &Url, f: F) -> Option<R>
    where
        F: FnOnce(&mut Document) -> R,
    {
        let mut documents = self.documents.write().unwrap();
        documents.get_mut(uri).map(f)
    }

    /// Check if a document is open
    pub fn has_document(&self, uri: &Url) -> bool {
        let documents = self.documents.read().unwrap();
        documents.contains_key(uri)
    }

    /// Get all open document URIs
    pub fn all_documents(&self) -> Vec<Url> {
        let documents = self.documents.read().unwrap();
        documents.keys().cloned().collect()
    }

    /// Force reparse of a document
    ///
    /// Useful when configuration changes or external factors require reparsing.
    pub fn force_reparse(&self, uri: &Url) -> Result<()> {
        let mut documents = self.documents.write().unwrap();
        let document = documents
            .get_mut(uri)
            .ok_or_else(|| DocumentError::DocumentNotFound(uri.clone()))?;

        let mut parser = self.parser.write().unwrap();
        document.reparse(&mut parser)?;

        Ok(())
    }

    /// Reparse all open documents
    ///
    /// TODO: Parallelize for large workspaces
    pub fn reparse_all(&self) -> Result<()> {
        for uri in self.all_documents() {
            self.force_reparse(&uri)?;
        }

        Ok(())
    }
}

impl Default for DocumentManager {
    fn default() -> Self {
        Self::new().expect("Failed to create document manager")
    }
}

#[cfg(test)]
mod tests {

    use std::str::FromStr;

    use super::*;

    #[test]
    fn test_document_creation() {
        let uri = Url::from_str("file:///test.py").unwrap();
        let doc = Document::new(uri.clone(), 1, "x = 42".to_string());

        assert_eq!(doc.uri, uri);
        assert_eq!(doc.version, 1);
        assert_eq!(doc.text(), "x = 42");
    }

    #[test]
    fn test_document_apply_changes_full() {
        let uri = Url::from_str("file:///test.py").unwrap();
        let mut doc = Document::new(uri, 1, "old text".to_string());

        let change = TextDocumentContentChangeEvent { range: None, range_length: None, text: "new text".to_string() };

        doc.apply_changes(vec![change]);
        assert_eq!(doc.text(), "new text");
    }

    #[test]
    fn test_document_manager_open_close() {
        let manager = DocumentManager::new().unwrap();
        let uri = Url::from_str("file:///test.py").unwrap();

        manager.open_document(uri.clone(), 1, "x = 42".to_string()).unwrap();

        assert!(manager.has_document(&uri));

        manager.close_document(&uri);
        assert!(!manager.has_document(&uri));
    }

    #[test]
    fn test_document_manager_get() {
        let manager = DocumentManager::new().unwrap();
        let uri = Url::from_str("file:///test.py").unwrap();

        manager.open_document(uri.clone(), 1, "x = 42".to_string()).unwrap();

        let text = manager.get_document(&uri, |doc| doc.text());
        assert_eq!(text, Some("x = 42".to_string()));
    }

    #[test]
    fn test_document_reparse() {
        let uri = Url::from_str("file:///test.py").unwrap();
        let mut doc = Document::new(uri, 1, "def hello():\n    pass".to_string());

        let mut parser = LspParser::new().unwrap();
        doc.reparse(&mut parser).unwrap();

        assert!(doc.parse_result.is_some());
        assert!(doc.ast().is_some());
        assert!(doc.symbol_table().is_some());
    }

    #[test]
    fn test_document_mode_override_extraction() {
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"# beacon: mode=strict
def foo():
    pass
"#;
        let mut doc = Document::new(uri, 1, source.to_string());

        let mut parser = LspParser::new().unwrap();
        doc.reparse(&mut parser).unwrap();

        assert_eq!(doc.mode_override, Some(crate::config::TypeCheckingMode::Strict));
    }

    #[test]
    fn test_document_no_mode_override() {
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"def foo():
    pass
"#;
        let mut doc = Document::new(uri, 1, source.to_string());

        let mut parser = LspParser::new().unwrap();
        doc.reparse(&mut parser).unwrap();

        assert_eq!(doc.mode_override, None);
    }

    #[test]
    fn test_document_effective_mode_with_override() {
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"# beacon: mode=loose
def foo():
    pass
"#;
        let mut doc = Document::new(uri, 1, source.to_string());

        let mut parser = LspParser::new().unwrap();
        doc.reparse(&mut parser).unwrap();

        let workspace_mode = crate::config::TypeCheckingMode::Strict;
        assert_eq!(
            doc.effective_mode(workspace_mode),
            crate::config::TypeCheckingMode::Loose
        );
    }

    #[test]
    fn test_document_effective_mode_without_override() {
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"def foo():
    pass
"#;
        let mut doc = Document::new(uri, 1, source.to_string());

        let mut parser = LspParser::new().unwrap();
        doc.reparse(&mut parser).unwrap();

        let workspace_mode = crate::config::TypeCheckingMode::Strict;
        assert_eq!(
            doc.effective_mode(workspace_mode),
            crate::config::TypeCheckingMode::Strict
        );
    }

    #[test]
    fn test_document_mode_override_case_insensitive() {
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"# BEACON: MODE=BALANCED
def foo():
    pass
"#;
        let mut doc = Document::new(uri, 1, source.to_string());

        let mut parser = LspParser::new().unwrap();
        doc.reparse(&mut parser).unwrap();

        assert_eq!(doc.mode_override, Some(crate::config::TypeCheckingMode::Balanced));
    }

    #[test]
    fn test_document_mode_override_after_shebang() {
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"#!/usr/bin/env python3
# beacon: mode=strict
def foo():
    pass
"#;
        let mut doc = Document::new(uri, 1, source.to_string());

        let mut parser = LspParser::new().unwrap();
        doc.reparse(&mut parser).unwrap();

        assert_eq!(doc.mode_override, Some(crate::config::TypeCheckingMode::Strict));
    }
}

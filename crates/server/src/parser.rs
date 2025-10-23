//! LSP-specific Python parser with incremental update support
//!
//! This module wraps the beacon-parser crate with LSP-specific functionality:
//! - Incremental parsing using tree-sitter edit sessions
//! - Position tracking and conversions
//! - Parse tree caching
//! - Error recovery and diagnostics

use crate::utils;
use beacon_core::{Result, errors};
use beacon_parser::{AstNode, ParsedFile, PythonParser, SymbolTable};
use lsp_types::{Position, Range};
use ropey::Rope;
use std::sync::Arc;
use tree_sitter::{InputEdit, Parser, Point, Tree};

/// LSP-specific parser that manages incremental updates
///
/// Maintains parse state for a single document and supports efficient reparsing when document content changes.
pub struct LspParser {
    parser: PythonParser,
    ts_parser: Parser,
}

/// Result of parsing a document including all derived artifacts
pub struct ParseResult {
    /// The tree-sitter concrete syntax tree
    pub tree: Tree,
    /// Our typed AST representation
    pub ast: AstNode,
    /// Symbol table from name resolution
    pub symbol_table: SymbolTable,
    /// Document text as a Rope for efficient operations
    pub rope: Arc<Rope>,
    /// Parse errors (syntax errors from tree-sitter)
    pub errors: Vec<ParseError>,
}

/// Parse error with LSP-compatible position information
#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: String,
    pub range: Range,
    pub severity: ErrorSeverity,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorSeverity {
    Error,
    Warning,
    Hint,
}

impl LspParser {
    pub fn new() -> Result<Self> {
        let parser = PythonParser::new()?;

        let language = tree_sitter_python::LANGUAGE;
        let mut ts_parser = Parser::new();
        ts_parser
            .set_language(&language.into())
            .map_err(|e| errors::ParseError::TreeSitterError(e.to_string()))?;

        Ok(Self { parser, ts_parser })
    }

    /// Parse a document from scratch
    ///
    /// This performs a full parse of the document and generates the AST and symbol table.
    pub fn parse(&mut self, text: &str) -> Result<ParseResult> {
        let parsed = self.parser.parse(text)?;
        let ast = self.parser.to_ast(&parsed)?;
        let symbol_table = self.parser.resolve_names(&ast)?;
        let rope = Arc::new(Rope::from_str(text));
        let errors = self.collect_errors(&parsed.tree, text);

        Ok(ParseResult { tree: parsed.tree, ast, symbol_table, rope, errors })
    }

    /// Incrementally reparse a document after an edit
    ///
    /// Uses tree-sitter's incremental parsing to efficiently update the parse tree.
    ///
    /// TODO: Implement using InputEdit for efficiency
    pub fn reparse(
        &mut self, old_tree: &Tree, old_text: &str, new_text: &str, edits: &[TextEdit],
    ) -> Result<ParseResult> {
        let input_edits = self.convert_edits(old_text, new_text, edits);

        let mut tree = old_tree.clone();
        for edit in input_edits {
            tree.edit(&edit);
        }

        let new_tree = self
            .ts_parser
            .parse(new_text, Some(&tree))
            .ok_or_else(|| errors::ParseError::TreeSitterError("Failed to reparse".to_string()))?;

        let parsed = ParsedFile { tree: new_tree.clone(), source: new_text.to_string() };
        let ast = self.parser.to_ast(&parsed)?;
        let symbol_table = self.parser.resolve_names(&ast)?;
        let rope = Arc::new(Rope::from_str(new_text));
        let errors = self.collect_errors(&new_tree, new_text);
        Ok(ParseResult { tree: new_tree, ast, symbol_table, rope, errors })
    }

    /// Convert LSP text edits to tree-sitter InputEdits
    fn convert_edits(&self, old_text: &str, _new_text: &str, edits: &[TextEdit]) -> Vec<InputEdit> {
        let mut input_edits = Vec::new();

        for edit in edits {
            let start_byte = utils::position_to_byte_offset(old_text, edit.range.start);
            let old_end_byte = utils::position_to_byte_offset(old_text, edit.range.end);
            let new_end_byte = start_byte + edit.new_text.len();

            let start_point =
                Point { row: edit.range.start.line as usize, column: edit.range.start.character as usize };

            let old_end_point = Point { row: edit.range.end.line as usize, column: edit.range.end.character as usize };
            let new_end_point = self.calculate_new_end_point(start_point, &edit.new_text);

            input_edits.push(InputEdit {
                start_byte,
                old_end_byte,
                new_end_byte,
                start_position: start_point,
                old_end_position: old_end_point,
                new_end_position: new_end_point,
            });
        }

        input_edits
    }

    /// Calculate the end position after inserting text
    fn calculate_new_end_point(&self, start: Point, inserted_text: &str) -> Point {
        let newline_count = inserted_text.matches('\n').count();

        if newline_count == 0 {
            Point { row: start.row, column: start.column + inserted_text.len() }
        } else {
            let last_line = inserted_text.split('\n').last().unwrap_or("");
            Point { row: start.row + newline_count, column: last_line.len() }
        }
    }

    /// Collect syntax errors from the parse tree
    ///
    /// Tree-sitter marks nodes with errors; we extract these into diagnostics.
    fn collect_errors(&self, tree: &Tree, text: &str) -> Vec<ParseError> {
        let mut errors = Vec::new();
        let root = tree.root_node();

        self.collect_errors_recursive(root, text, &mut errors);

        errors
    }

    /// Recursively traverse the tree to find error nodes
    fn collect_errors_recursive(&self, node: tree_sitter::Node, text: &str, errors: &mut Vec<ParseError>) {
        if node.is_error() || node.is_missing() {
            let range = Range {
                start: utils::tree_sitter_point_to_position(text, node.start_position()),
                end: utils::tree_sitter_point_to_position(text, node.end_position()),
            };

            errors.push(ParseError {
                message: if node.is_missing() {
                    format!("Missing {}", node.kind())
                } else {
                    format!("Syntax error: unexpected {}", node.kind())
                },
                range,
                severity: ErrorSeverity::Error,
            });
        }

        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            self.collect_errors_recursive(child, text, errors);
        }
    }

    /// Find the AST node at a given position
    ///
    /// Returns the most specific (deepest) node at the given position.
    pub fn node_at_position<'a>(
        &self, tree: &'a Tree, text: &str, position: Position,
    ) -> Option<tree_sitter::Node<'a>> {
        let byte_offset = utils::position_to_byte_offset(text, position);
        tree.root_node().descendant_for_byte_range(byte_offset, byte_offset)
    }

    /// Get the range of a tree-sitter node as an LSP range
    pub fn node_range(&self, node: tree_sitter::Node, text: &str) -> Range {
        utils::tree_sitter_range_to_lsp_range(text, node.range())
    }
}

impl Default for LspParser {
    fn default() -> Self {
        Self::new().expect("Failed to create LSP parser")
    }
}

/// Represents a text edit operation
///
/// Similar to [`lsp_types::TextEdit`] but used internally
#[derive(Debug, Clone)]
pub struct TextEdit {
    pub range: Range,
    pub new_text: String,
}

impl From<lsp_types::TextEdit> for TextEdit {
    fn from(edit: lsp_types::TextEdit) -> Self {
        Self { range: edit.range, new_text: edit.new_text }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parser_creation() {
        let parser = LspParser::new();
        assert!(parser.is_ok());
    }

    #[test]
    fn test_simple_parse() {
        let mut parser = LspParser::new().unwrap();
        let source = "def hello(name):\n    return f'Hello {name}'";
        let result = parser.parse(source).unwrap();
        assert!(!result.tree.root_node().has_error());
        assert!(result.errors.is_empty());
    }

    #[test]
    fn test_parse_with_errors() {
        let mut parser = LspParser::new().unwrap();
        let source = "def incomplete(";
        let result = parser.parse(source).unwrap();
        assert!(result.tree.root_node().child_count() > 0);
    }

    #[test]
    fn test_symbol_table_creation() {
        let mut parser = LspParser::new().unwrap();
        let source = r#"
def greet(name):
    message = f"Hello {name}"
    return message

result = greet("World")
"#;

        let result = parser.parse(source).unwrap();
        let root_scope = result.symbol_table.root_scope;
        let greet = result.symbol_table.lookup_symbol("greet", root_scope);
        assert!(greet.is_some());

        let result_var = result.symbol_table.lookup_symbol("result", root_scope);
        assert!(result_var.is_some());
    }

    #[test]
    fn test_calculate_new_end_point() {
        let parser = LspParser::new().unwrap();
        let start = Point { row: 0, column: 5 };
        let end = parser.calculate_new_end_point(start, "hello");
        assert_eq!(end.row, 0);
        assert_eq!(end.column, 10);

        let end = parser.calculate_new_end_point(start, "hello\nworld");
        assert_eq!(end.row, 1);
        assert_eq!(end.column, 5);
    }

    #[test]
    fn test_node_at_position() {
        let mut parser = LspParser::new().unwrap();
        let source = "def hello():\n    return 42";

        let result = parser.parse(source).unwrap();
        let pos = Position { line: 0, character: 4 };
        let node = parser.node_at_position(&result.tree, source, pos);
        assert!(node.is_some());

        let pos = Position { line: 1, character: 11 };
        let node = parser.node_at_position(&result.tree, source, pos);
        assert!(node.is_some());
        let node = node.unwrap();
        assert_eq!(node.kind(), "integer");
    }
}

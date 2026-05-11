//! Shared tree-sitter node helpers.

use beacon_core::{ParseError, Result};
use tree_sitter::Node;

pub(crate) fn node_span(node: &Node) -> (usize, usize, usize, usize) {
    let start_position = node.start_position();
    let end_position = node.end_position();
    (
        start_position.row + 1,
        start_position.column + 1,
        end_position.row + 1,
        end_position.column + 1,
    )
}

pub(crate) fn node_text(node: &Node, source: &str) -> Result<String> {
    Ok(node
        .utf8_text(source.as_bytes())
        .map_err(|_| ParseError::InvalidUtf8)?
        .to_string())
}

pub(crate) fn field_text(node: &Node, source: &str, field: &str) -> Result<String> {
    let child = node
        .child_by_field_name(field)
        .ok_or_else(|| ParseError::TreeSitterError(format!("Missing {field} field")))?;
    node_text(&child, source)
}

pub(crate) fn is_list_delimiter(kind: &str) -> bool {
    matches!(kind, "(" | ")" | "[" | "]" | "{" | "}" | ",")
}

impl crate::PythonParser {
    pub(crate) fn debug_node(node: tree_sitter::Node, source: &str, depth: usize) -> String {
        let indent = "  ".repeat(depth);
        let text = node.utf8_text(source.as_bytes()).unwrap_or("<invalid>");
        let mut result = format!(
            "{}{}({}): '{}'\n",
            indent,
            node.kind(),
            node.id(),
            text.replace('\n', "\\n")
        );

        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            result.push_str(&Self::debug_node(child, source, depth + 1));
        }
        result
    }
}

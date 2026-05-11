use beacon_core::{ParseError, Result};
use tree_sitter::Node;

use crate::literal_parse::{parse_python_float, parse_python_integer};
use crate::{AstNode, LiteralValue, PythonParser};

use super::helpers::is_list_delimiter;

impl PythonParser {
    pub(crate) fn extract_literal_value(&self, node: &Node, source: &str) -> Result<LiteralValue> {
        let text = node.utf8_text(source.as_bytes()).map_err(|_| ParseError::InvalidUtf8)?;

        match node.kind() {
            "string" => {
                let mut cursor = node.walk();
                let mut content = String::new();
                let mut prefix = String::new();

                for child in node.children(&mut cursor) {
                    if child.kind() == "string_start" {
                        if let Ok(start_text) = child.utf8_text(source.as_bytes())
                            && let Some(pos) = start_text.find(|c: char| ['\'', '"'].contains(&c))
                        {
                            prefix = start_text[..pos].to_string();
                        }
                    } else if (child.kind() == "string_content" || child.kind() == "interpolation")
                        && let Ok(text) = child.utf8_text(source.as_bytes())
                    {
                        content.push_str(text);
                    }
                }
                Ok(LiteralValue::String { value: content, prefix })
            }
            "integer" => {
                let value = parse_python_integer(text);
                Ok(LiteralValue::Integer(value))
            }
            "float" => {
                let value = parse_python_float(text)?;
                Ok(LiteralValue::Float(value))
            }
            "true" => Ok(LiteralValue::Boolean(true)),
            "false" => Ok(LiteralValue::Boolean(false)),
            "none" => Ok(LiteralValue::None),
            _ => Err(ParseError::TreeSitterError(format!("Unknown literal type: {}", node.kind())).into()),
        }
    }

    pub(crate) fn extract_tuple_elements(&self, node: &Node, source: &str) -> Result<Vec<AstNode>> {
        let mut elements = Vec::new();
        let mut cursor = node.walk();

        for child in node.children(&mut cursor) {
            if !child.is_extra() && !is_list_delimiter(child.kind()) {
                elements.push(self.node_to_ast(child, source)?);
            }
        }

        Ok(elements)
    }

    pub(crate) fn extract_list_elements(&self, node: &Node, source: &str) -> Result<Vec<AstNode>> {
        let mut elements = Vec::new();
        let mut cursor = node.walk();

        for child in node.children(&mut cursor) {
            if !child.is_extra() && !is_list_delimiter(child.kind()) {
                elements.push(self.node_to_ast(child, source)?);
            }
        }

        Ok(elements)
    }

    pub(crate) fn extract_dict_pairs(&self, node: &Node, source: &str) -> Result<(Vec<AstNode>, Vec<AstNode>)> {
        let mut keys = Vec::new();
        let mut values = Vec::new();
        let mut cursor = node.walk();

        for child in node.children(&mut cursor) {
            if !child.is_extra() && child.kind() == "pair" {
                if let Some(key_node) = child.child_by_field_name("key") {
                    keys.push(self.node_to_ast(key_node, source)?);
                }
                if let Some(value_node) = child.child_by_field_name("value") {
                    values.push(self.node_to_ast(value_node, source)?);
                }
            }
        }

        Ok((keys, values))
    }

    pub(crate) fn extract_set_elements(&self, node: &Node, source: &str) -> Result<Vec<AstNode>> {
        let mut elements = Vec::new();
        let mut cursor = node.walk();

        for child in node.children(&mut cursor) {
            if !child.is_extra() && !is_list_delimiter(child.kind()) {
                elements.push(self.node_to_ast(child, source)?);
            }
        }

        Ok(elements)
    }
}

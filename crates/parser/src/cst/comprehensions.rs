use beacon_core::{ParseError, Result};
use tree_sitter::Node;

use crate::{AstNode, Comprehension, PythonParser};

impl PythonParser {
    pub(crate) fn extract_list_comp_info(&self, node: &Node, source: &str) -> Result<(AstNode, Vec<Comprehension>)> {
        let element = node
            .child_by_field_name("body")
            .ok_or_else(|| ParseError::TreeSitterError("Missing list comprehension body".to_string()))?;
        let element_ast = self.node_to_ast(element, source)?;
        let generators = self.extract_comprehension_clauses(node, source)?;

        Ok((element_ast, generators))
    }

    pub(crate) fn extract_dict_comp_info(
        &self, node: &Node, source: &str,
    ) -> Result<(AstNode, AstNode, Vec<Comprehension>)> {
        let mut key = None;
        let mut value = None;
        let mut cursor = node.walk();

        for child in node.children(&mut cursor) {
            if child.kind() == "pair" {
                if let Some(key_node) = child.child_by_field_name("key") {
                    key = Some(self.node_to_ast(key_node, source)?);
                }
                if let Some(value_node) = child.child_by_field_name("value") {
                    value = Some(self.node_to_ast(value_node, source)?);
                }
            }
        }

        let key = key.ok_or_else(|| ParseError::TreeSitterError("Missing dict comprehension key".to_string()))?;
        let value = value.ok_or_else(|| ParseError::TreeSitterError("Missing dict comprehension value".to_string()))?;
        let generators = self.extract_comprehension_clauses(node, source)?;

        Ok((key, value, generators))
    }

    pub(crate) fn extract_set_comp_info(&self, node: &Node, source: &str) -> Result<(AstNode, Vec<Comprehension>)> {
        let element = node
            .child_by_field_name("body")
            .ok_or_else(|| ParseError::TreeSitterError("Missing set comprehension body".to_string()))?;
        let element_ast = self.node_to_ast(element, source)?;
        let generators = self.extract_comprehension_clauses(node, source)?;

        Ok((element_ast, generators))
    }

    pub(crate) fn extract_generator_exp_info(
        &self, node: &Node, source: &str,
    ) -> Result<(AstNode, Vec<Comprehension>)> {
        let element = node
            .child_by_field_name("body")
            .ok_or_else(|| ParseError::TreeSitterError("Missing generator expression body".to_string()))?;
        let element_ast = self.node_to_ast(element, source)?;
        let generators = self.extract_comprehension_clauses(node, source)?;

        Ok((element_ast, generators))
    }

    pub(crate) fn extract_comprehension_clauses(&self, node: &Node, source: &str) -> Result<Vec<Comprehension>> {
        let mut generators = Vec::new();
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if child.kind() == "for_in_clause" {
                let left = child
                    .child_by_field_name("left")
                    .ok_or_else(|| ParseError::TreeSitterError("Missing comprehension target".to_string()))?;
                let target = left
                    .utf8_text(source.as_bytes())
                    .map_err(|_| ParseError::InvalidUtf8)?
                    .to_string();

                let right = child
                    .child_by_field_name("right")
                    .ok_or_else(|| ParseError::TreeSitterError("Missing comprehension iterator".to_string()))?;
                let iter = self.node_to_ast(right, source)?;

                let mut ifs = Vec::new();
                let mut sibling_cursor = child.walk();
                if let Some(parent) = child.parent() {
                    for sibling in parent.children(&mut sibling_cursor) {
                        if sibling.kind() == "if_clause"
                            && sibling.start_byte() > child.end_byte()
                            && let Some(cond) = sibling.named_child(0)
                        {
                            ifs.push(self.node_to_ast(cond, source)?);
                        }
                    }
                }

                generators.push(Comprehension { target, iter, ifs });
            }
        }

        Ok(generators)
    }
}

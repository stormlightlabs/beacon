use beacon_core::{ParseError, Result};
use tree_sitter::Node;

use super::types::{InfoFor, InfoIf, InfoTry};
use crate::{AstNode, BinaryOperator, ExceptHandler, PythonParser, WithItem};

impl PythonParser {
    pub(crate) fn extract_assignment_target(&self, node: &Node, source: &str) -> Result<AstNode> {
        let left_node = node
            .child_by_field_name("left")
            .ok_or_else(|| ParseError::TreeSitterError("Missing assignment target".to_string()))?;

        self.node_to_ast(left_node, source)
    }

    pub(crate) fn extract_assignment_value(&self, node: &Node, source: &str) -> Result<AstNode> {
        let right_node = node
            .child_by_field_name("right")
            .ok_or_else(|| ParseError::TreeSitterError("Missing assignment value".to_string()))?;

        self.node_to_ast(right_node, source)
    }

    pub(crate) fn extract_augmented_assignment_operator(&self, node: &Node, source: &str) -> Result<BinaryOperator> {
        if let Some(op_node) = node.child_by_field_name("operator") {
            let op = op_node
                .utf8_text(source.as_bytes())
                .map_err(|_| ParseError::InvalidUtf8)?;
            return self.parse_binary_operator(op.trim_end_matches('='));
        }

        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if child.is_named() {
                continue;
            }

            if let Ok(text) = child.utf8_text(source.as_bytes())
                && text.ends_with('=')
                && text != "="
            {
                return self.parse_binary_operator(text.trim_end_matches('='));
            }
        }

        Err(ParseError::TreeSitterError("Missing augmented assignment operator".to_string()).into())
    }

    pub(crate) fn extract_return_value(&self, node: &Node, source: &str) -> Result<Option<AstNode>> {
        match node.named_child(0) {
            Some(value_node) => Ok(Some(self.node_to_ast(value_node, source)?)),
            None => Ok(None),
        }
    }

    pub(crate) fn extract_raise_value(&self, node: &Node, source: &str) -> Result<Option<AstNode>> {
        match node.named_child(0) {
            Some(value_node) => Ok(Some(self.node_to_ast(value_node, source)?)),
            None => Ok(None),
        }
    }

    pub(crate) fn extract_global_or_nonlocal_names(&self, node: &Node, source: &str) -> Result<Vec<String>> {
        let mut names = Vec::new();
        let mut cursor = node.walk();

        for child in node.children(&mut cursor) {
            if child.kind() == "identifier"
                && let Ok(name) = child.utf8_text(source.as_bytes())
            {
                names.push(name.to_string());
            }
        }

        Ok(names)
    }

    pub(crate) fn extract_if_info(&self, node: &Node, source: &str) -> Result<InfoIf> {
        let condition = node
            .child_by_field_name("condition")
            .ok_or_else(|| ParseError::TreeSitterError("Missing if condition".to_string()))?;
        let test = self.node_to_ast(condition, source)?;
        let consequence = node
            .child_by_field_name("consequence")
            .ok_or_else(|| ParseError::TreeSitterError("Missing if body".to_string()))?;
        let body = self.extract_body(&consequence, source)?;

        let mut elif_parts = Vec::new();
        let mut else_body = None;

        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            match child.kind() {
                "elif_clause" => {
                    if let Some(elif_cond) = child.child_by_field_name("condition") {
                        let elif_test = self.node_to_ast(elif_cond, source)?;
                        if let Some(elif_cons) = child.child_by_field_name("consequence") {
                            let elif_body = self.extract_body(&elif_cons, source)?;
                            elif_parts.push((elif_test, elif_body));
                        }
                    }
                }
                "else_clause" => {
                    if let Some(else_block) = child.child_by_field_name("body") {
                        else_body = Some(self.extract_body(&else_block, source)?);
                    }
                }
                _ => {}
            }
        }

        Ok(InfoIf(test, body, elif_parts, else_body))
    }

    pub(crate) fn extract_for_info(&self, node: &Node, source: &str) -> Result<InfoFor> {
        let left = node
            .child_by_field_name("left")
            .ok_or_else(|| ParseError::TreeSitterError("Missing for target".to_string()))?;
        let target = self.node_to_ast(left, source)?;

        let right = node
            .child_by_field_name("right")
            .ok_or_else(|| ParseError::TreeSitterError("Missing for iterator".to_string()))?;
        let iter = self.node_to_ast(right, source)?;

        let body_node = node
            .child_by_field_name("body")
            .ok_or_else(|| ParseError::TreeSitterError("Missing for body".to_string()))?;
        let body = self.extract_body(&body_node, source)?;

        let else_body = node
            .child_by_field_name("alternative")
            .and_then(
                |alt| {
                    if alt.kind() == "else_clause" { alt.child_by_field_name("body") } else { Some(alt) }
                },
            )
            .map(|body_node| self.extract_body(&body_node, source))
            .transpose()?;

        let is_async = {
            let mut cursor = node.walk();
            node.children(&mut cursor).any(|child| child.kind() == "async")
        };

        Ok(InfoFor(target, iter, body, else_body, is_async))
    }

    pub(crate) fn extract_while_info(
        &self, node: &Node, source: &str,
    ) -> Result<(AstNode, Vec<AstNode>, Option<Vec<AstNode>>)> {
        let condition = node
            .child_by_field_name("condition")
            .ok_or_else(|| ParseError::TreeSitterError("Missing while condition".to_string()))?;
        let test = self.node_to_ast(condition, source)?;

        let body_node = node
            .child_by_field_name("body")
            .ok_or_else(|| ParseError::TreeSitterError("Missing while body".to_string()))?;
        let body = self.extract_body(&body_node, source)?;

        let else_body = node
            .child_by_field_name("alternative")
            .and_then(
                |alt| {
                    if alt.kind() == "else_clause" { alt.child_by_field_name("body") } else { Some(alt) }
                },
            )
            .map(|body_node| self.extract_body(&body_node, source))
            .transpose()?;

        Ok((test, body, else_body))
    }

    pub(crate) fn extract_try_info(&self, node: &Node, source: &str) -> Result<InfoTry> {
        let body_node = node
            .child_by_field_name("body")
            .ok_or_else(|| ParseError::TreeSitterError("Missing try body".to_string()))?;
        let body = self.extract_body(&body_node, source)?;
        let mut handlers = Vec::new();
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if child.kind() == "except_clause" {
                let handler = self.extract_except_handler(&child, source)?;
                handlers.push(handler);
            }
        }

        let else_body = self.extract_clause_body(node, "else_clause", source)?;
        let finally_body = self.extract_clause_body(node, "finally_clause", source)?;

        Ok(InfoTry(body, handlers, else_body, finally_body))
    }

    pub(crate) fn extract_clause_body(
        &self, node: &Node, clause_kind: &str, source: &str,
    ) -> Result<Option<Vec<AstNode>>> {
        let mut cursor = node.walk();
        if let Some(clause) = node.children(&mut cursor).find(|n| n.kind() == clause_kind) {
            if let Some(body) = clause.child_by_field_name("body") {
                return self.extract_body(&body, source).map(Some);
            }

            let mut clause_cursor = clause.walk();
            if let Some(block) = clause.children(&mut clause_cursor).find(|n| n.kind() == "block") {
                return self.extract_body(&block, source).map(Some);
            }
        }

        Ok(None)
    }

    pub(crate) fn extract_except_handler(&self, node: &Node, source: &str) -> Result<ExceptHandler> {
        let start_position = node.start_position();
        let end_position = node.end_position();
        let line = start_position.row + 1;
        let col = start_position.column + 1;
        let end_line = end_position.row + 1;
        let end_col = end_position.column + 1;
        let mut exception_type = node
            .child_by_field_name("type")
            .and_then(|n| n.utf8_text(source.as_bytes()).ok())
            .map(|s| s.to_string())
            .or_else(|| {
                node.children(&mut node.walk())
                    .find(|n| {
                        n.kind() == "dotted_name"
                            || n.kind() == "identifier"
                            || n.kind() == "tuple"
                            || n.kind() == "attribute"
                    })
                    .and_then(|n| n.utf8_text(source.as_bytes()).ok())
                    .map(|s| s.to_string())
            });

        let mut pattern_alias = None;
        if let Some(pattern) = node.children(&mut node.walk()).find(|n| n.kind() == "as_pattern") {
            let mut cursor = pattern.walk();
            for child in pattern.children(&mut cursor) {
                match child.kind() {
                    "dotted_name" | "identifier" => {
                        if exception_type.is_none() {
                            exception_type = child.utf8_text(source.as_bytes()).ok().map(|s| s.to_string());
                        }
                    }
                    "as_pattern_target" => {
                        pattern_alias = child.utf8_text(source.as_bytes()).ok().map(|s| s.to_string());
                    }
                    _ => {}
                }
            }
        }

        let name = node
            .child_by_field_name("name")
            .and_then(|n| n.utf8_text(source.as_bytes()).ok())
            .map(|s| s.to_string())
            .or(pattern_alias);

        let body_node = node
            .children(&mut node.walk())
            .find(|n| n.kind() == "block")
            .ok_or_else(|| ParseError::TreeSitterError("Missing except body".to_string()))?;
        let body = self.extract_body(&body_node, source)?;

        Ok(ExceptHandler { exception_type, name, body, line, col, end_line, end_col })
    }

    pub(crate) fn extract_with_info(&self, node: &Node, source: &str) -> Result<(Vec<WithItem>, Vec<AstNode>, bool)> {
        let mut items = Vec::new();
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if child.kind() == "with_clause" {
                let mut clause_cursor = child.walk();
                for with_item in child.children(&mut clause_cursor) {
                    if with_item.kind() == "with_item" {
                        let first_child = with_item.named_child(0).ok_or_else(|| {
                            ParseError::TreeSitterError("Missing context expression in with item".to_string())
                        })?;

                        let (context_expr, optional_vars) = if first_child.kind() == "as_pattern" {
                            let ctx_expr_node = first_child.named_child(0).ok_or_else(|| {
                                ParseError::TreeSitterError("Missing context in as_pattern".to_string())
                            })?;
                            let context_expr = self.node_to_ast(ctx_expr_node, source)?;

                            let alias = first_child
                                .child_by_field_name("alias")
                                .and_then(|n| n.utf8_text(source.as_bytes()).ok())
                                .map(|s| s.to_string());

                            (context_expr, alias)
                        } else {
                            (self.node_to_ast(first_child, source)?, None)
                        };

                        items.push(WithItem { context_expr, optional_vars });
                    }
                }
            }
        }

        let body_node = node
            .child_by_field_name("body")
            .ok_or_else(|| ParseError::TreeSitterError("Missing with body".to_string()))?;
        let body = self.extract_body(&body_node, source)?;

        let is_async = {
            let mut cursor = node.walk();
            node.children(&mut cursor).any(|child| child.kind() == "async")
        };

        Ok((items, body, is_async))
    }

    pub(crate) fn extract_body(&self, node: &Node, source: &str) -> Result<Vec<AstNode>> {
        let mut body = Vec::new();
        let mut cursor = node.walk();

        for child in node.children(&mut cursor) {
            if !child.is_extra() {
                body.push(self.node_to_ast(child, source)?);
            }
        }

        Ok(body)
    }
}

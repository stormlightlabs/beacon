use beacon_core::{ParseError, Result};
use tree_sitter::Node;

use crate::{AstNode, MatchCase, Pattern, PythonParser};

impl PythonParser {
    pub(crate) fn extract_match_info(&self, node: &Node, source: &str) -> Result<(AstNode, Vec<MatchCase>)> {
        let subject_node = node
            .child_by_field_name("subject")
            .ok_or_else(|| ParseError::TreeSitterError("Missing match subject".to_string()))?;
        let subject = self.node_to_ast(subject_node, source)?;

        let mut cases = Vec::new();
        let mut cursor = node.walk();

        for child in node.children(&mut cursor) {
            if child.kind() == "block" {
                let mut block_cursor = child.walk();
                for case_node in child.children(&mut block_cursor) {
                    if case_node.kind() == "case_clause" {
                        let case = self.extract_match_case(&case_node, source)?;
                        cases.push(case);
                    }
                }
            }
        }

        Ok((subject, cases))
    }

    pub(crate) fn extract_match_case(&self, node: &Node, source: &str) -> Result<MatchCase> {
        let mut cursor = node.walk();
        let pattern_node = node
            .children(&mut cursor)
            .find(|child| child.kind() == "case_pattern")
            .ok_or_else(|| ParseError::TreeSitterError("Missing case pattern".to_string()))?;
        let pattern_start = pattern_node.start_position();
        let pattern_end = pattern_node.end_position();
        let pattern_line = pattern_start.row + 1;
        let pattern_col = pattern_start.column + 1;
        let pattern_end_line = pattern_end.row + 1;
        let pattern_end_col = pattern_end.column + 1;
        let pattern = self.extract_pattern(&pattern_node, source)?;

        let body_node = node
            .child_by_field_name("consequence")
            .ok_or_else(|| ParseError::TreeSitterError("Missing case body".to_string()))?;
        let body = self.extract_body(&body_node, source)?;

        let guard = node
            .child_by_field_name("guard")
            .and_then(|if_clause| if_clause.named_child(0))
            .map(|cond| self.node_to_ast(cond, source))
            .transpose()?;

        let start_position = node.start_position();
        let end_position = node.end_position();
        let line = start_position.row + 1;
        let col = start_position.column + 1;
        let end_line = end_position.row + 1;
        let end_col = end_position.column + 1;

        Ok(MatchCase {
            pattern,
            guard,
            body,
            pattern_line,
            pattern_col,
            pattern_end_line,
            pattern_end_col,
            line,
            col,
            end_line,
            end_col,
        })
    }

    pub(crate) fn extract_pattern(&self, node: &Node, source: &str) -> Result<Pattern> {
        match node.kind() {
            "case_pattern" => {
                if let Some(child) = node.named_child(0) {
                    self.extract_pattern(&child, source)
                } else {
                    let pattern_text = node
                        .utf8_text(source.as_bytes())
                        .map_err(|e| ParseError::TreeSitterError(format!("Failed to extract pattern text: {e}")))?;
                    if pattern_text.trim() == "_" {
                        Ok(Pattern::MatchAs { pattern: None, name: None })
                    } else {
                        Ok(Pattern::MatchValue(self.node_to_ast(*node, source)?))
                    }
                }
            }
            "as_pattern" => {
                let pattern = node
                    .named_child(0)
                    .map(|p| self.extract_pattern(&p, source))
                    .transpose()?
                    .map(Box::new);

                let name = node
                    .child_by_field_name("alias")
                    .or_else(|| node.named_child(1))
                    .and_then(|n| n.utf8_text(source.as_bytes()).ok())
                    .map(|s| s.to_string());

                Ok(Pattern::MatchAs { pattern, name })
            }
            "list_pattern" | "tuple_pattern" => {
                let mut patterns = Vec::new();
                let mut cursor = node.walk();

                for child in node.children(&mut cursor) {
                    if !child.is_extra()
                        && child.kind() != "["
                        && child.kind() != "]"
                        && child.kind() != "("
                        && child.kind() != ")"
                        && child.kind() != ","
                    {
                        patterns.push(self.extract_pattern(&child, source)?);
                    }
                }

                Ok(Pattern::MatchSequence(patterns))
            }
            "dict_pattern" => {
                let mut keys = Vec::new();
                let mut patterns = Vec::new();
                let mut cursor = node.walk();
                let mut pending_key: Option<AstNode> = None;

                for child in node.children(&mut cursor) {
                    if child.is_extra() {
                        continue;
                    }

                    match child.kind() {
                        "{" | "}" | "," | ":" => continue,
                        "case_pattern" => {
                            let pattern = self.extract_pattern(&child, source)?;
                            if let Some(key) = pending_key.take() {
                                keys.push(key);
                                patterns.push(pattern);
                            } else {
                                patterns.push(pattern);
                            }
                        }
                        _ => {
                            let key = self.node_to_ast(child, source)?;
                            pending_key = Some(key);
                        }
                    }
                }

                Ok(Pattern::MatchMapping { keys, patterns })
            }
            "class_pattern" => {
                let mut cursor = node.walk();
                let cls = node
                    .children(&mut cursor)
                    .find(|n| n.kind() == "dotted_name")
                    .and_then(|n| n.utf8_text(source.as_bytes()).ok())
                    .ok_or_else(|| ParseError::TreeSitterError("Missing dotted_name in class pattern".to_string()))?
                    .to_string();

                let mut patterns = Vec::new();
                let mut cursor = node.walk();

                for child in node.children(&mut cursor) {
                    if child.kind() == "case_pattern" {
                        patterns.push(self.extract_pattern(&child, source)?);
                    }
                }

                Ok(Pattern::MatchClass { cls, patterns })
            }
            "or_pattern" | "union_pattern" => {
                let mut patterns = Vec::new();
                let mut cursor = node.walk();

                for child in node.children(&mut cursor) {
                    if !child.is_extra() && child.kind() != "|" {
                        patterns.push(self.extract_pattern(&child, source)?);
                    }
                }

                Ok(Pattern::MatchOr(patterns))
            }
            "identifier" => {
                let text = node
                    .utf8_text(source.as_bytes())
                    .map_err(|e| ParseError::TreeSitterError(format!("Failed to extract identifier text: {e}")))?;

                if text == "_" {
                    Ok(Pattern::MatchAs { pattern: None, name: None })
                } else {
                    Ok(Pattern::MatchAs { pattern: None, name: Some(text.to_string()) })
                }
            }
            "dotted_name" => {
                let text = node
                    .utf8_text(source.as_bytes())
                    .map_err(|e| ParseError::TreeSitterError(format!("Failed to extract dotted name text: {e}")))?;

                if text.contains('.') {
                    Ok(Pattern::MatchValue(self.node_to_ast(*node, source)?))
                } else if text == "_" {
                    Ok(Pattern::MatchAs { pattern: None, name: None })
                } else {
                    Ok(Pattern::MatchAs { pattern: None, name: Some(text.to_string()) })
                }
            }
            "_" => Ok(Pattern::MatchAs { pattern: None, name: None }),
            _ => Ok(Pattern::MatchValue(self.node_to_ast(*node, source)?)),
        }
    }
}

use beacon_core::{ParseError, Result};
use tree_sitter::Node;

use crate::{ImportName, PythonParser};

impl PythonParser {
    pub(crate) fn extract_import_info(&self, node: &Node, source: &str) -> Result<Vec<(String, Option<String>)>> {
        let mut modules = Vec::new();

        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            match child.kind() {
                "dotted_name" | "identifier" => {
                    if let Ok(name) = child.utf8_text(source.as_bytes())
                        && name != "import"
                    {
                        modules.push((name.to_string(), None));
                    }
                }
                "aliased_import" => {
                    if let Some(name_node) = child.child_by_field_name("name") {
                        let module = name_node
                            .utf8_text(source.as_bytes())
                            .map_err(|_| ParseError::InvalidUtf8)?
                            .to_string();
                        let alias = child
                            .child_by_field_name("alias")
                            .and_then(|alias_node| alias_node.utf8_text(source.as_bytes()).ok())
                            .map(|s| s.to_string());
                        modules.push((module, alias));
                    }
                }
                _ => {}
            }
        }

        modules.retain(|(name, _)| !name.is_empty());

        if modules.is_empty() {
            Err(ParseError::TreeSitterError("Missing import name".to_string()).into())
        } else {
            Ok(modules)
        }
    }

    pub(crate) fn extract_import_from_info(&self, node: &Node, source: &str) -> Result<(String, Vec<ImportName>)> {
        let module_node = node
            .child_by_field_name("module_name")
            .ok_or_else(|| ParseError::TreeSitterError("Missing module name in import from".to_string()))?;

        let module = module_node
            .utf8_text(source.as_bytes())
            .map_err(|_| ParseError::InvalidUtf8)?
            .to_string();

        let mut names = Vec::new();
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            match child.kind() {
                "dotted_name" | "identifier" => {
                    if child.id() != module_node.id()
                        && let Ok(name) = child.utf8_text(source.as_bytes())
                        && name != "from"
                        && name != "import"
                    {
                        let start_pos = child.start_position();
                        let end_pos = child.end_position();
                        names.push(ImportName {
                            name: name.to_string(),
                            line: start_pos.row + 1,
                            col: start_pos.column + 1,
                            end_line: end_pos.row + 1,
                            end_col: end_pos.column + 1,
                        });
                    }
                }
                "aliased_import" => {
                    if let Some(name_node) = child.child_by_field_name("name")
                        && let Ok(name) = name_node.utf8_text(source.as_bytes())
                    {
                        let start_pos = name_node.start_position();
                        let end_pos = name_node.end_position();
                        names.push(ImportName {
                            name: name.to_string(),
                            line: start_pos.row + 1,
                            col: start_pos.column + 1,
                            end_line: end_pos.row + 1,
                            end_col: end_pos.column + 1,
                        });
                    }
                }
                "wildcard_import" => {
                    let start_pos = child.start_position();
                    let end_pos = child.end_position();
                    names.push(ImportName {
                        name: "*".to_string(),
                        line: start_pos.row + 1,
                        col: start_pos.column + 1,
                        end_line: end_pos.row + 1,
                        end_col: end_pos.column + 1,
                    });
                }
                _ => {}
            }
        }

        Ok((module, names))
    }

    pub(crate) fn extract_future_import_names(&self, node: &Node, source: &str) -> Vec<ImportName> {
        let mut names = Vec::new();
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            match child.kind() {
                "dotted_name" | "identifier" => {
                    if let Ok(text) = child.utf8_text(source.as_bytes())
                        && text != "__future__"
                        && text != "from"
                        && text != "import"
                    {
                        let start_pos = child.start_position();
                        let end_pos = child.end_position();
                        names.push(ImportName {
                            name: text.to_string(),
                            line: start_pos.row + 1,
                            col: start_pos.column + 1,
                            end_line: end_pos.row + 1,
                            end_col: end_pos.column + 1,
                        });
                    }
                }
                _ => {}
            }
        }
        names
    }
}

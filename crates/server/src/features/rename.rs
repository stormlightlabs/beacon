//! Rename provider
//!
//! Renames a symbol across all references in the workspace.
//! Validates the new name and creates workspace edits for all occurrences.

use crate::{document::DocumentManager, parser};
use beacon_parser::{AstNode, SymbolTable};
use lsp_types::{Position, Range, RenameParams, TextEdit, Url, WorkspaceEdit};
use std::collections::{HashMap, HashSet};

/// Rename provider
///
/// Handles symbol renaming across the workspace with validation by finding all references and creates appropriate [`TextEdit`].
pub struct RenameProvider {
    documents: DocumentManager,
}

impl RenameProvider {
    pub fn new(documents: DocumentManager) -> Self {
        Self { documents }
    }

    /// Rename a symbol at the given position
    ///
    /// Validates the new name and finds all references to create a WorkspaceEdit.
    pub fn rename(&self, params: RenameParams) -> Option<WorkspaceEdit> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        let new_name = params.new_name;

        if !Self::is_valid_identifier(&new_name) {
            return None;
        }

        let symbol_name = self.documents.get_document(&uri, |doc| {
            let tree = doc.tree()?;
            let text = doc.text();
            let p = parser::LspParser::new().ok()?;
            let node = p.node_at_position(tree, &text, position)?;

            match node.kind() {
                "identifier" => {
                    let identifier_text = node.utf8_text(text.as_bytes()).ok()?;
                    Some(identifier_text.to_string())
                }
                _ => None,
            }
        })??;

        let edits = self.find_all_references(&uri, &symbol_name, &new_name)?;

        if edits.is_empty() {
            return None;
        }

        Some(WorkspaceEdit { changes: Some(edits), document_changes: None, change_annotations: None })
    }

    /// Find all references to a symbol and create text edits
    ///
    /// Currently searches only the current document.
    /// TODO: Extend to workspace-wide search
    fn find_all_references(&self, uri: &Url, symbol_name: &str, new_name: &str) -> Option<HashMap<Url, Vec<TextEdit>>> {
        let mut changes = HashMap::new();

        self.documents.get_document(uri, |doc| {
            let tree = doc.tree()?;
            let symbol_table = doc.symbol_table()?;
            let ast = doc.ast()?;
            let text = doc.text();

            if !Self::symbol_exists(symbol_table, symbol_name) {
                return None;
            }

            let mut edits = Vec::new();
            Self::collect_renames(ast, symbol_name, new_name, &mut edits, &text);

            let tree_edits = Self::collect_tree_identifier_edits(tree, &text, symbol_name, new_name);
            let edits = Self::merge_edits(edits, tree_edits);

            if !edits.is_empty() {
                changes.insert(uri.clone(), edits);
            }

            Some(())
        })?;

        Some(changes)
    }

    /// Recursively collect rename edits from the AST
    ///
    /// Finds all occurrences of the symbol and creates text edits to replace them.
    fn collect_renames(node: &AstNode, symbol_name: &str, new_name: &str, edits: &mut Vec<TextEdit>, _text: &str) {
        match node {
            AstNode::Identifier { name, line, col } if name == symbol_name => {
                let position =
                    Position { line: (*line as u32).saturating_sub(1), character: (*col as u32).saturating_sub(1) };
                let end_position =
                    Position { line: position.line, character: position.character + symbol_name.len() as u32 };

                edits.push(TextEdit {
                    range: Range { start: position, end: end_position },
                    new_text: new_name.to_string(),
                });
            }
            AstNode::Assignment { target, value, line, col } => {
                if target == symbol_name {
                    let position =
                        Position { line: (*line as u32).saturating_sub(1), character: (*col as u32).saturating_sub(1) };
                    let end_position =
                        Position { line: position.line, character: position.character + symbol_name.len() as u32 };

                    edits.push(TextEdit {
                        range: Range { start: position, end: end_position },
                        new_text: new_name.to_string(),
                    });
                }
                let value_is_same_identifier =
                    matches!(value.as_ref(), AstNode::Identifier { name, .. } if name == symbol_name);

                if !(target == symbol_name && value_is_same_identifier) {
                    Self::collect_renames(value, symbol_name, new_name, edits, _text);
                }
            }
            AstNode::FunctionDef { body, .. } => {
                for stmt in body {
                    Self::collect_renames(stmt, symbol_name, new_name, edits, _text);
                }
            }
            AstNode::ClassDef { body, .. } | AstNode::Module { body, .. } => {
                for stmt in body {
                    Self::collect_renames(stmt, symbol_name, new_name, edits, _text);
                }
            }
            AstNode::Call { args, .. } => {
                for arg in args {
                    Self::collect_renames(arg, symbol_name, new_name, edits, _text);
                }
            }
            AstNode::Return { value: Some(val), .. } => {
                Self::collect_renames(val, symbol_name, new_name, edits, _text);
            }
            AstNode::Literal { .. } => {}
            _ => {}
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

impl RenameProvider {
    fn merge_edits(edits: Vec<TextEdit>, additional: Vec<TextEdit>) -> Vec<TextEdit> {
        let mut seen = HashSet::new();
        let mut merged = Vec::new();

        for edit in edits.into_iter().chain(additional.into_iter()) {
            let key = (
                edit.range.start.line,
                edit.range.start.character,
                edit.range.end.line,
                edit.range.end.character,
            );

            if seen.insert(key) {
                merged.push(edit);
            }
        }

        merged
    }

    fn collect_tree_identifier_edits(
        tree: &tree_sitter::Tree, text: &str, symbol_name: &str, new_name: &str,
    ) -> Vec<TextEdit> {
        fn visit(node: tree_sitter::Node, text: &str, symbol_name: &str, new_name: &str, edits: &mut Vec<TextEdit>) {
            if node.kind() == "identifier" {
                if let Ok(node_text) = node.utf8_text(text.as_bytes()) {
                    if node_text == symbol_name {
                        let start = node.start_position();
                        let end = node.end_position();
                        edits.push(TextEdit {
                            range: Range {
                                start: Position { line: start.row as u32, character: start.column as u32 },
                                end: Position { line: end.row as u32, character: end.column as u32 },
                            },
                            new_text: new_name.to_string(),
                        });
                    }
                }
            }

            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                visit(child, text, symbol_name, new_name, edits);
            }
        }

        let mut edits = Vec::new();
        visit(tree.root_node(), text, symbol_name, new_name, &mut edits);
        edits
    }

    fn symbol_exists(symbol_table: &SymbolTable, name: &str) -> bool {
        symbol_table
            .scopes
            .values()
            .any(|scope| scope.symbols.contains_key(name))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::str::FromStr;

    #[test]
    fn test_provider_creation() {
        let documents = DocumentManager::new().unwrap();
        let _ = RenameProvider::new(documents);
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

    #[test]
    fn test_rename_variable() {
        let documents = DocumentManager::new().unwrap();
        let provider = RenameProvider::new(documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"x = 42
y = x + 1
print(x)"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let params = RenameParams {
            text_document_position: lsp_types::TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                position: Position { line: 0, character: 0 }, // On 'x'
            },
            new_name: "renamed_var".to_string(),
            work_done_progress_params: Default::default(),
        };

        let result = provider.rename(params);
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

    #[test]
    fn test_rename_invalid_name() {
        let documents = DocumentManager::new().unwrap();
        let provider = RenameProvider::new(documents.clone());
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

        let result = provider.rename(params);
        assert!(result.is_none());
    }

    #[test]
    fn test_rename_not_on_identifier() {
        let documents = DocumentManager::new().unwrap();
        let provider = RenameProvider::new(documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = "x = 42";

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let params = RenameParams {
            text_document_position: lsp_types::TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri },
                position: Position { line: 0, character: 2 }, // On '='
            },
            new_name: "new_name".to_string(),
            work_done_progress_params: Default::default(),
        };

        let result = provider.rename(params);
        assert!(result.is_none());
    }

    #[test]
    fn test_collect_renames_simple() {
        let ast = AstNode::Identifier { name: "old_name".to_string(), line: 1, col: 1 };
        let mut edits = Vec::new();

        RenameProvider::collect_renames(&ast, "old_name", "new_name", &mut edits, "old_name");

        assert_eq!(edits.len(), 1);
        assert_eq!(edits[0].new_text, "new_name");
        assert_eq!(edits[0].range.start.line, 0);
        assert_eq!(edits[0].range.start.character, 0);
    }

    #[test]
    fn test_collect_renames_in_assignment() {
        let ast = AstNode::Assignment {
            target: "x".to_string(),
            value: Box::new(AstNode::Identifier { name: "x".to_string(), line: 1, col: 5 }),
            line: 1,
            col: 1,
        };

        let mut edits = Vec::new();
        RenameProvider::collect_renames(&ast, "x", "y", &mut edits, "x = x");

        assert_eq!(edits.len(), 1);
        assert_eq!(edits[0].new_text, "y");
    }

    #[test]
    fn test_collect_renames_in_function() {
        let ast = AstNode::FunctionDef {
            name: "test".to_string(),
            args: vec![],
            body: vec![
                AstNode::Assignment {
                    target: "x".to_string(),
                    value: Box::new(AstNode::Literal {
                        value: beacon_parser::LiteralValue::Integer(42),
                        line: 2,
                        col: 9,
                    }),
                    line: 2,
                    col: 5,
                },
                AstNode::Return {
                    value: Some(Box::new(AstNode::Identifier {
                        name: "x".to_string(),
                        line: 3,
                        col: 12,
                    })),
                    line: 3,
                    col: 5,
                },
            ],
            line: 1,
            col: 1,
            docstring: None,
            return_type: None,
            decorators: Vec::new(),
        };

        let mut edits = Vec::new();
        RenameProvider::collect_renames(&ast, "x", "result", &mut edits, "");

        assert_eq!(edits.len(), 2);
        for edit in &edits {
            assert_eq!(edit.new_text, "result");
        }
    }

    #[test]
    fn test_collect_renames_in_call() {
        let ast = AstNode::Call {
            function: "print".to_string(),
            args: vec![AstNode::Identifier { name: "x".to_string(), line: 1, col: 7 }],
            line: 1,
            col: 1,
        };

        let mut edits = Vec::new();
        RenameProvider::collect_renames(&ast, "x", "value", &mut edits, "");

        assert_eq!(edits.len(), 1);
        assert_eq!(edits[0].new_text, "value");
    }

    #[test]
    fn test_collect_renames_no_match() {
        let ast = AstNode::Identifier { name: "x".to_string(), line: 1, col: 1 };
        let mut edits = Vec::new();

        RenameProvider::collect_renames(&ast, "y", "z", &mut edits, "");

        assert_eq!(edits.len(), 0);
    }

    #[test]
    fn test_collect_renames_nested() {
        let ast = AstNode::Module {
            body: vec![AstNode::ClassDef {
                name: "MyClass".to_string(),
                metaclass: None,
                bases: Vec::new(),
                body: vec![AstNode::FunctionDef {
                    name: "method".to_string(),
                    args: vec![],
                    body: vec![AstNode::Return {
                        value: Some(Box::new(AstNode::Identifier {
                            name: "x".to_string(),
                            line: 3,
                            col: 16,
                        })),
                        line: 3,
                        col: 9,
                    }],
                    line: 2,
                    col: 5,
                    docstring: None,
                    return_type: None,
                    decorators: Vec::new(),
                }],
                line: 1,
                col: 1,
                docstring: None,
                decorators: Vec::new(),
            }],
            docstring: None,
        };

        let mut edits = Vec::new();
        RenameProvider::collect_renames(&ast, "x", "new_x", &mut edits, "");

        assert_eq!(edits.len(), 1);
        assert_eq!(edits[0].new_text, "new_x");
    }

    #[test]
    fn test_rename_position_calculation() {
        let ast = AstNode::Identifier { name: "long_variable_name".to_string(), line: 5, col: 10 };
        let mut edits = Vec::new();

        RenameProvider::collect_renames(&ast, "long_variable_name", "short", &mut edits, "");

        assert_eq!(edits.len(), 1);
        assert_eq!(edits[0].range.start.line, 4); // 0-indexed
        assert_eq!(edits[0].range.start.character, 9); // 0-indexed
        assert_eq!(edits[0].range.end.character, 9 + "long_variable_name".len() as u32);
        assert_eq!(edits[0].new_text, "short");
    }

    #[test]
    fn test_rename_with_multiple_occurrences() {
        let documents = DocumentManager::new().unwrap();
        let provider = RenameProvider::new(documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"def calculate(x):
    result = x * 2
    return result"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let params = RenameParams {
            text_document_position: lsp_types::TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                position: Position { line: 1, character: 13 }, // On 'x'
            },
            new_name: "value".to_string(),
            work_done_progress_params: Default::default(),
        };

        let result = provider.rename(params);
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

    #[test]
    fn test_collect_renames_module() {
        let ast = AstNode::Module {
            body: vec![
                AstNode::Assignment {
                    target: "x".to_string(),
                    value: Box::new(AstNode::Literal {
                        value: beacon_parser::LiteralValue::Integer(1),
                        line: 1,
                        col: 5,
                    }),
                    line: 1,
                    col: 1,
                },
                AstNode::Assignment {
                    target: "y".to_string(),
                    value: Box::new(AstNode::Identifier { name: "x".to_string(), line: 2, col: 5 }),
                    line: 2,
                    col: 1,
                },
            ],
            docstring: None,
        };

        let mut edits = Vec::new();
        RenameProvider::collect_renames(&ast, "x", "renamed", &mut edits, "");

        assert_eq!(edits.len(), 2);
    }
}

//! Move Symbol refactoring
//!
//! Moves a symbol (function, class, variable) from one file to another, updating all imports across the workspace.

use super::refactoring::{EditCollector, RefactoringContext};

use beacon_parser::{Symbol, SymbolTable};
use lsp_types::{Position, Range, TextEdit, Url, WorkspaceEdit};
use std::path::Path;

/// Parameters for move symbol refactoring
pub struct MoveSymbolParams {
    /// Source file containing the symbol
    pub source_uri: Url,
    /// Position of the symbol to move
    pub position: Position,
    /// Target file to move the symbol to
    pub target_uri: Url,
}

/// Move symbol refactoring provider
pub struct MoveSymbolProvider {
    context: RefactoringContext,
}

/// Context for checking symbol usage recursively
struct SymbolUsageChecker<'a> {
    text: &'a str,
    symbol_table: &'a SymbolTable,
    symbol_name: &'a str,
    target_symbol: &'a Symbol,
    def_start: usize,
    def_end: usize,
}

impl<'a> SymbolUsageChecker<'a> {
    /// Recursively check for symbol usage
    fn check_recursive(&self, node: tree_sitter::Node, found: &mut bool) {
        if *found {
            return;
        }

        let node_start = node.start_byte();
        let node_end = node.end_byte();

        if node_start >= self.def_start && node_end <= self.def_end {
            return;
        }

        if node.kind() == "identifier"
            && let Ok(node_text) = node.utf8_text(self.text.as_bytes())
                && node_text == self.symbol_name {
                    let byte_offset = node.start_byte();
                    let scope = self.symbol_table.find_scope_at_position(byte_offset);

                    if let Some(resolved) = self.symbol_table.lookup_symbol(self.symbol_name, scope)
                        && resolved.scope_id == self.target_symbol.scope_id
                            && resolved.line == self.target_symbol.line
                            && resolved.col == self.target_symbol.col
                        {
                            *found = true;
                            return;
                        }
                }

        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            self.check_recursive(child, found);
        }
    }
}

impl MoveSymbolProvider {
    /// Create a new move symbol provider
    pub fn new(context: RefactoringContext) -> Self {
        Self { context }
    }

    /// Execute the move symbol refactoring
    pub async fn execute(&self, params: MoveSymbolParams) -> Option<WorkspaceEdit> {
        let (symbol_name, target_symbol) = self
            .context
            .resolve_symbol_at_position(&params.source_uri, params.position)?;

        let (source_tree, source_text) = self.context.get_tree_and_text(&params.source_uri)?;
        let source_symbol_table = self.context.get_symbol_table(&params.source_uri)?;
        let definition_node = Self::find_definition_node(&source_tree, &source_text, &target_symbol)?;

        let symbol_definition =
            Self::extract_node_text(&source_text, definition_node.start_byte(), definition_node.end_byte());

        let (target_tree, target_text) = self.context.get_tree_and_text(&params.target_uri)?;
        let target_insertion = Self::find_module_level_insertion_point(&target_tree, &target_text);

        let mut collector = EditCollector::new();

        let definition_range = crate::utils::tree_sitter_range_to_lsp_range(&source_text, definition_node.range());
        collector.add_edit(
            params.source_uri.clone(),
            TextEdit { range: definition_range, new_text: String::new() },
        );

        collector.add_edit(
            params.target_uri.clone(),
            TextEdit {
                range: Range { start: target_insertion, end: target_insertion },
                new_text: format!("{symbol_definition}\n\n"),
            },
        );

        let source_still_uses = Self::symbol_still_used_in_file(
            &source_tree,
            &source_text,
            &source_symbol_table,
            &symbol_name,
            &target_symbol,
            definition_node.start_byte(),
            definition_node.end_byte(),
        );

        if source_still_uses {
            let import_statement = Self::generate_import_statement(&params.target_uri, &symbol_name);
            let import_insertion = Self::find_import_insertion_point(&source_tree, &source_text);

            collector.add_edit(
                params.source_uri.clone(),
                TextEdit {
                    range: Range { start: import_insertion, end: import_insertion },
                    new_text: format!("{import_statement}\n"),
                },
            );
        }

        let _dependents = self
            .update_dependent_imports(&params.source_uri, &params.target_uri, &symbol_name, &mut collector)
            .await;

        collector.into_workspace_edit()
    }

    fn find_definition_node<'a>(
        tree: &'a tree_sitter::Tree, text: &str, symbol: &Symbol,
    ) -> Option<tree_sitter::Node<'a>> {
        let root = tree.root_node();

        Self::find_node_at_line_col(root, text, symbol.line, symbol.col)
    }

    /// Find a node at a specific line and column (1-indexed)
    fn find_node_at_line_col<'a>(
        node: tree_sitter::Node<'a>, _text: &str, line: usize, col: usize,
    ) -> Option<tree_sitter::Node<'a>> {
        let target_line = line.saturating_sub(1);
        let target_col = col.saturating_sub(1);

        let start_point = node.start_position();

        if start_point.row == target_line && start_point.column == target_col {
            return Self::find_parent_definition(node);
        }

        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if let Some(found) = Self::find_node_at_line_col(child, _text, line, col) {
                return Some(found);
            }
        }

        None
    }

    /// Walk up the tree to find the definition node (function_definition, class_definition, etc.)
    fn find_parent_definition(mut node: tree_sitter::Node) -> Option<tree_sitter::Node> {
        loop {
            match node.kind() {
                "function_definition" | "class_definition" | "decorated_definition" => {
                    return Some(node);
                }
                _ => {
                    if let Some(parent) = node.parent() {
                        node = parent;
                    } else {
                        return None;
                    }
                }
            }
        }
    }

    /// Extract text from source by byte offsets
    fn extract_node_text(text: &str, start_byte: usize, end_byte: usize) -> String {
        text[start_byte..end_byte].to_string()
    }

    /// Find where to insert at module level in target file
    ///
    /// Inserts after imports and module docstring
    fn find_module_level_insertion_point(tree: &tree_sitter::Tree, _text: &str) -> Position {
        let root = tree.root_node();
        let mut last_import_line = 0;
        let mut has_docstring = false;

        let mut cursor = root.walk();
        for child in root.children(&mut cursor) {
            match child.kind() {
                "import_statement" | "import_from_statement" => {
                    last_import_line = child.end_position().row;
                }
                "expression_statement" => {
                    if !has_docstring && child.start_position().row <= 1
                        && let Some(string_node) = child.child(0)
                            && string_node.kind() == "string" {
                                has_docstring = true;
                                last_import_line = child.end_position().row;
                            }
                }
                _ => {}
            }
        }

        Position { line: (last_import_line + 2) as u32, character: 0 }
    }

    /// Check if symbol is still used in the file (excluding its definition)
    fn symbol_still_used_in_file(
        tree: &tree_sitter::Tree, text: &str, symbol_table: &SymbolTable, symbol_name: &str, target_symbol: &Symbol,
        def_start: usize, def_end: usize,
    ) -> bool {
        let checker = SymbolUsageChecker { text, symbol_table, symbol_name, target_symbol, def_start, def_end };

        let mut found = false;
        checker.check_recursive(tree.root_node(), &mut found);
        found
    }

    /// Generate import statement for moved symbol
    ///
    /// Creates an absolute import statement for the symbol.
    /// The module path is computed by walking up the directory tree to find package boundaries.
    fn generate_import_statement(target_uri: &Url, symbol_name: &str) -> String {
        let module_name = Self::uri_to_module_name(target_uri);
        format!("from {module_name} import {symbol_name}")
    }

    /// Convert URI to Python module name
    fn uri_to_module_name(uri: &Url) -> String {
        let path = Path::new(uri.path());

        let filename = path.file_stem().and_then(|s| s.to_str()).unwrap_or("module");

        if filename == "__init__"
            && let Some(parent) = path.parent()
                && let Some(parent_name) = parent.file_name().and_then(|s| s.to_str()) {
                    return parent_name.to_string();
                }

        let mut components = Vec::new();
        components.push(filename.to_string());

        let mut current = path.parent();
        while let Some(parent) = current {
            let init_file = parent.join("__init__.py");
            if init_file.exists() {
                if let Some(parent_name) = parent.file_name().and_then(|s| s.to_str()) {
                    components.push(parent_name.to_string());
                    current = parent.parent();
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        components.reverse();
        components.join(".")
    }

    /// Find where to insert import statements
    ///
    /// Inserts after existing imports, or after module docstring if no imports exist.
    fn find_import_insertion_point(tree: &tree_sitter::Tree, _text: &str) -> Position {
        let root = tree.root_node();
        let mut last_import_line = 0;
        let mut has_docstring = false;

        let mut cursor = root.walk();
        for child in root.children(&mut cursor) {
            match child.kind() {
                "import_statement" | "import_from_statement" => last_import_line = child.end_position().row,
                "expression_statement" if !has_docstring && child.start_position().row <= 1 => {
                    if let Some(string_node) = child.child(0) {
                        if string_node.kind() != "string" {
                            continue;
                        }

                        has_docstring = true;
                        if last_import_line == 0 {
                            last_import_line = child.end_position().row
                        }
                    };
                }

                _ => {}
            }
        }

        Position::new((last_import_line + 1) as u32, 0)
    }

    /// Update imports in files that depend on the source
    ///
    /// Finds all imports of the moved symbol from source and updates them to import from target.
    async fn update_dependent_imports(
        &self, source_uri: &Url, target_uri: &Url, symbol_name: &str, collector: &mut EditCollector,
    ) -> Vec<Url> {
        let workspace = self.context.workspace.read().await;
        let dependents = workspace.get_dependents(source_uri);
        let source_module = Self::uri_to_module_name(source_uri);
        let target_module = Self::uri_to_module_name(target_uri);

        for dependent_uri in &dependents {
            if *dependent_uri == *source_uri || *dependent_uri == *target_uri {
                continue;
            }

            let file_ctx = if self.context.documents.has_document(dependent_uri) {
                self.context.get_tree_and_text(dependent_uri)
            } else {
                workspace
                    .load_workspace_file(dependent_uri)
                    .map(|pr| (pr.tree, pr.rope.to_string()))
            };

            if let Some((tree, text)) = file_ctx {
                let edits = Self::find_and_update_imports(&tree, &text, symbol_name, &source_module, &target_module);

                if !edits.is_empty() {
                    collector.add_edits(dependent_uri.clone(), edits);
                }
            }
        }

        dependents
    }

    /// Find import statements and generate edits to update them
    fn find_and_update_imports(
        tree: &tree_sitter::Tree, text: &str, symbol_name: &str, source_module: &str, target_module: &str,
    ) -> Vec<TextEdit> {
        let mut edits = Vec::new();
        let root = tree.root_node();
        let mut cursor = root.walk();

        for child in root.children(&mut cursor) {
            if child.kind() != "import_from_statement" {
                continue;
            }

            let Some(module_node) = child.child_by_field_name("module_name") else {
                continue;
            };

            let Ok(module_text) = module_node.utf8_text(text.as_bytes()) else {
                continue;
            };

            if module_text != source_module {
                continue;
            }

            if !Self::imports_symbol(child, text, symbol_name) {
                continue;
            }

            let range = crate::utils::tree_sitter_range_to_lsp_range(text, module_node.range());

            edits.push(TextEdit { range, new_text: target_module.to_string() });
        }

        edits
    }

    /// Check if an import statement imports a specific symbol
    fn imports_symbol(import_node: tree_sitter::Node, text: &str, symbol_name: &str) -> bool {
        let mut cursor = import_node.walk();
        for child in import_node.children(&mut cursor) {
            if (child.kind() == "dotted_name" || child.kind() == "identifier")
                && let Ok(name) = child.utf8_text(text.as_bytes())
                    && name == symbol_name {
                        return true;
                    }

            if child.kind() == "aliased_import"
                && let Some(name_node) = child.child_by_field_name("name")
                    && let Ok(name) = name_node.utf8_text(text.as_bytes())
                        && name == symbol_name {
                            return true;
                        }
        }
        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extract_node_text() {
        let text = "def foo():\n    pass";
        let result = MoveSymbolProvider::extract_node_text(text, 0, 9);
        assert_eq!(result, "def foo()");
    }

    #[test]
    fn test_generate_import_statement() {
        let uri = Url::parse("file:///path/to/target.py").unwrap();
        let result = MoveSymbolProvider::generate_import_statement(&uri, "my_function");
        assert_eq!(result, "from target import my_function");
    }

    #[test]
    fn test_uri_to_module_name() {
        let uri = Url::parse("file:///path/to/my_module.py").unwrap();
        let result = MoveSymbolProvider::uri_to_module_name(&uri);
        assert_eq!(result, "my_module");
    }

    #[test]
    fn test_uri_to_module_name_with_underscores() {
        let uri = Url::parse("file:///path/to/my_long_module_name.py").unwrap();
        let result = MoveSymbolProvider::uri_to_module_name(&uri);
        assert_eq!(result, "my_long_module_name");
    }

    #[test]
    fn test_uri_to_module_name_init_file() {
        let uri = Url::parse("file:///path/to/package/__init__.py").unwrap();
        let result = MoveSymbolProvider::uri_to_module_name(&uri);
        assert_eq!(result, "package");
    }

    #[test]
    fn test_generate_import_statement_simple() {
        let uri = Url::parse("file:///target.py").unwrap();
        let result = MoveSymbolProvider::generate_import_statement(&uri, "MyClass");
        assert_eq!(result, "from target import MyClass");
    }

    #[test]
    fn test_generate_import_statement_with_path() {
        let uri = Url::parse("file:///src/utils/helpers.py").unwrap();
        let result = MoveSymbolProvider::generate_import_statement(&uri, "helper_func");
        assert!(result.contains("helpers"));
        assert!(result.contains("helper_func"));
    }

    #[test]
    fn test_extract_node_text_edge_case_empty() {
        let text = "def foo():\n    pass";
        let result = MoveSymbolProvider::extract_node_text(text, 0, 0);
        assert_eq!(result, "");
    }

    #[test]
    fn test_extract_node_text_full_text() {
        let text = "def foo():\n    pass";
        let result = MoveSymbolProvider::extract_node_text(text, 0, text.len());
        assert_eq!(result, text);
    }

    #[test]
    fn test_find_module_level_insertion_point_empty_file() {
        let text = "";
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&tree_sitter_python::LANGUAGE.into()).unwrap();
        let tree = parser.parse(text, None).unwrap();

        let position = MoveSymbolProvider::find_module_level_insertion_point(&tree, text);
        assert_eq!(position.line, 2);
        assert_eq!(position.character, 0);
    }

    #[test]
    fn test_find_module_level_insertion_point_only_imports() {
        let text = "import os\nimport sys\nfrom typing import List";
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&tree_sitter_python::LANGUAGE.into()).unwrap();
        let tree = parser.parse(text, None).unwrap();

        let position = MoveSymbolProvider::find_module_level_insertion_point(&tree, text);
        assert_eq!(position.line, 4);
        assert_eq!(position.character, 0);
    }

    #[test]
    fn test_find_module_level_insertion_point_with_docstring() {
        let text = "\"\"\"Module docstring.\"\"\"\n\ndef foo():\n    pass";
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&tree_sitter_python::LANGUAGE.into()).unwrap();
        let tree = parser.parse(text, None).unwrap();

        let position = MoveSymbolProvider::find_module_level_insertion_point(&tree, text);
        assert_eq!(position.line, 2);
        assert_eq!(position.character, 0);
    }

    #[test]
    fn test_find_module_level_insertion_point_docstring_and_imports() {
        let text = "\"\"\"Module docstring.\"\"\"\nimport os\nimport sys";
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&tree_sitter_python::LANGUAGE.into()).unwrap();
        let tree = parser.parse(text, None).unwrap();

        let position = MoveSymbolProvider::find_module_level_insertion_point(&tree, text);
        assert_eq!(position.line, 4);
        assert_eq!(position.character, 0);
    }

    #[test]
    fn test_find_module_level_insertion_point_with_code() {
        let text = "import os\n\ndef foo():\n    pass\n\nclass Bar:\n    pass";
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&tree_sitter_python::LANGUAGE.into()).unwrap();
        let tree = parser.parse(text, None).unwrap();

        let position = MoveSymbolProvider::find_module_level_insertion_point(&tree, text);
        assert_eq!(position.line, 2);
        assert_eq!(position.character, 0);
    }
}

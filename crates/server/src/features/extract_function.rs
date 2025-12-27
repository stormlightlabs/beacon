//! Extract Function refactoring
//!
//! Extracts a selected code range into a new function, determining parameters

use super::refactoring::{EditCollector, RefactoringContext, RefactoringValidator};

use lsp_types::{Position, Range, TextEdit, Url, WorkspaceEdit};
use std::collections::{HashMap, HashSet};

/// Parameters for extract function refactoring
pub struct ExtractFunctionParams {
    /// The file containing the code to extract
    pub uri: Url,
    /// The range of code to extract
    pub range: Range,
    /// The name for the new function
    pub function_name: String,
}

/// Extract function refactoring provider
pub struct ExtractFunctionProvider {
    context: RefactoringContext,
}

impl ExtractFunctionProvider {
    /// Create a new extract function provider
    pub fn new(context: RefactoringContext) -> Self {
        Self { context }
    }

    /// Execute the extract function refactoring
    pub async fn execute(
        &self, params: ExtractFunctionParams, mut analyzer: Option<&mut crate::analysis::Analyzer>,
    ) -> Option<WorkspaceEdit> {
        if RefactoringValidator::validate_identifier(&params.function_name).is_err() {
            return None;
        }

        let (tree, text) = self.context.get_tree_and_text(&params.uri)?;
        let symbol_table = self.context.get_symbol_table(&params.uri)?;

        let selected_code = Self::extract_range_text(&text, params.range)?;

        let analysis = Self::analyze_variables(&tree, &text, &symbol_table, params.range)?;

        let type_map = if let Some(ref mut a) = analyzer {
            let start_byte = Self::position_to_byte_offset(&text, params.range.start);
            let scope_id = symbol_table.find_scope_at_position(start_byte);

            let mut all_vars = analysis.parameters.clone();
            all_vars.extend(analysis.returns.clone());
            Self::build_variable_type_map(a, &params.uri, &all_vars, &symbol_table, &text, scope_id)
        } else {
            std::collections::HashMap::new()
        };

        let function_def = Self::generate_function_definition(
            &params.function_name,
            &selected_code,
            &analysis.parameters,
            &analysis.returns,
            &type_map,
        );

        let function_call =
            Self::generate_function_call(&params.function_name, &analysis.parameters, &analysis.returns);

        let insertion_point = Self::find_function_insertion_point(&tree, &text, params.range)?;

        let mut collector = EditCollector::new();

        collector.add_edit(
            params.uri.clone(),
            TextEdit { range: params.range, new_text: function_call },
        );

        collector.add_edit(
            params.uri.clone(),
            TextEdit {
                range: Range { start: insertion_point, end: insertion_point },
                new_text: format!("{function_def}\n\n"),
            },
        );

        collector.into_workspace_edit()
    }

    /// Extract text from a range in the source
    fn extract_range_text(text: &str, range: Range) -> Option<String> {
        let lines: Vec<&str> = text.lines().collect();

        if range.start.line as usize >= lines.len() || range.end.line as usize >= lines.len() {
            return None;
        }

        if range.start.line == range.end.line {
            let line = lines[range.start.line as usize];
            let start = range.start.character as usize;
            let end = range.end.character as usize;

            if start > line.len() || end > line.len() {
                return None;
            }

            return Some(line[start..end].to_string());
        }

        let mut result = String::new();

        let first_line = lines[range.start.line as usize];
        if range.start.character as usize <= first_line.len() {
            result.push_str(&first_line[range.start.character as usize..]);
            result.push('\n');
        }

        for line_idx in (range.start.line + 1)..(range.end.line) {
            if let Some(line) = lines.get(line_idx as usize) {
                result.push_str(line);
                result.push('\n');
            }
        }

        let last_line = lines[range.end.line as usize];
        if range.end.character as usize <= last_line.len() {
            result.push_str(&last_line[..range.end.character as usize]);
        }

        Some(result)
    }

    /// Analyze variables in the selected range
    ///
    /// Determines which variables need to be parameters (used but not defined) and which need to be returned (defined and used after selection).
    fn analyze_variables(
        tree: &tree_sitter::Tree, text: &str, _symbol_table: &beacon_parser::SymbolTable, range: Range,
    ) -> Option<VariableAnalysis> {
        let start_byte = Self::position_to_byte_offset(text, range.start);
        let end_byte = Self::position_to_byte_offset(text, range.end);

        let mut defined_vars = HashSet::new();
        let mut used_vars = HashSet::new();
        let mut all_module_vars = HashSet::new();

        Self::collect_all_identifiers(tree.root_node(), text, 0, start_byte, &mut all_module_vars);

        Self::find_variables_in_range(
            tree.root_node(),
            text,
            start_byte,
            end_byte,
            &mut defined_vars,
            &mut used_vars,
        );

        let parameters: Vec<String> = used_vars
            .iter()
            .filter(|v| !defined_vars.contains(*v) && all_module_vars.contains(*v))
            .cloned()
            .collect();

        let returns: Vec<String> = defined_vars.iter().cloned().collect();

        Some(VariableAnalysis { parameters, returns })
    }

    /// Build a map of variable names to their inferred types
    ///
    /// Looks up each variable in the symbol table and uses the analyzer to get its type.
    fn build_variable_type_map(
        analyzer: &mut crate::analysis::Analyzer, uri: &Url, variables: &[String],
        symbol_table: &beacon_parser::SymbolTable, _text: &str, scope_id: beacon_parser::ScopeId,
    ) -> HashMap<String, String> {
        let mut type_map = HashMap::new();

        for var_name in variables {
            if let Some(symbol) = symbol_table.lookup_symbol(var_name, scope_id) {
                let position = lsp_types::Position {
                    line: (symbol.line.saturating_sub(1)) as u32,
                    character: (symbol.col.saturating_sub(1)) as u32,
                };

                if let Ok(Some(ty)) = analyzer.type_at_position(uri, position) {
                    let type_str = ty.to_string();
                    if !type_str.contains('\'') && type_str != "Any" {
                        type_map.insert(var_name.clone(), type_str);
                    }
                }
            }
        }

        type_map
    }

    /// Convert LSP position to byte offset
    fn position_to_byte_offset(text: &str, position: Position) -> usize {
        let mut offset = 0;
        let mut line = 0;

        for ch in text.chars() {
            if line == position.line as usize {
                break;
            }
            if ch == '\n' {
                line += 1;
            }
            offset += ch.len_utf8();
        }

        offset + position.character as usize
    }

    /// Collect all identifiers in a range
    fn collect_all_identifiers(
        node: tree_sitter::Node, text: &str, start: usize, end: usize, vars: &mut std::collections::HashSet<String>,
    ) {
        let node_start = node.start_byte();
        let node_end = node.end_byte();

        if node_start >= end || node_end <= start {
            return;
        }

        if node.kind() == "identifier"
            && let Ok(name) = node.utf8_text(text.as_bytes())
        {
            vars.insert(name.to_string());
        }

        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            Self::collect_all_identifiers(child, text, start, end, vars);
        }
    }

    /// Find variables defined and used in a range
    fn find_variables_in_range(
        node: tree_sitter::Node, text: &str, start: usize, end: usize, defined: &mut std::collections::HashSet<String>,
        used: &mut std::collections::HashSet<String>,
    ) {
        let node_start = node.start_byte();
        let node_end = node.end_byte();

        if node_start >= end || node_end <= start {
            return;
        }

        if node.kind() == "assignment" {
            if let Some(left) = node.child_by_field_name("left") {
                Self::collect_assigned_vars(left, text, defined);
            }
            if let Some(right) = node.child_by_field_name("right") {
                Self::collect_used_vars(right, text, used);
            }
            return;
        }

        if node.kind() == "identifier"
            && node_start >= start
            && node_end <= end
            && let Ok(name) = node.utf8_text(text.as_bytes())
        {
            used.insert(name.to_string());
        }

        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            Self::find_variables_in_range(child, text, start, end, defined, used);
        }
    }

    /// Collect variables being assigned
    fn collect_assigned_vars(node: tree_sitter::Node, text: &str, vars: &mut std::collections::HashSet<String>) {
        if node.kind() == "identifier"
            && let Ok(name) = node.utf8_text(text.as_bytes())
        {
            vars.insert(name.to_string());
        }

        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            Self::collect_assigned_vars(child, text, vars);
        }
    }

    /// Collect variables being used
    fn collect_used_vars(node: tree_sitter::Node, text: &str, vars: &mut std::collections::HashSet<String>) {
        if node.kind() == "identifier"
            && let Ok(name) = node.utf8_text(text.as_bytes())
        {
            vars.insert(name.to_string());
        }

        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            Self::collect_used_vars(child, text, vars);
        }
    }

    /// Generate the function definition
    fn generate_function_definition(
        name: &str, body: &str, parameters: &[String], returns: &[String], type_map: &HashMap<String, String>,
    ) -> String {
        let params_str = parameters.join(", ");

        let return_annotation = if returns.is_empty() {
            String::new()
        } else if returns.len() == 1 {
            format!(" -> {}", Self::infer_type_annotation(&returns[0], type_map))
        } else {
            let type_annotations: Vec<String> = returns
                .iter()
                .map(|r| Self::infer_type_annotation(r, type_map))
                .collect();
            format!(" -> tuple[{}]", type_annotations.join(", "))
        };

        let indented_body = Self::indent_code(body, 4);

        let return_statement =
            if !returns.is_empty() { format!("    return {}\n", returns.join(", ")) } else { String::new() };

        format!("def {name}({params_str}){return_annotation}:\n{indented_body}{return_statement}")
    }

    /// Generate the function call to replace selected code
    fn generate_function_call(name: &str, parameters: &[String], returns: &[String]) -> String {
        let args = parameters.join(", ");
        let call = format!("{name}({args})");

        if returns.is_empty() {
            call
        } else if returns.len() == 1 {
            format!("{} = {}", returns[0], call)
        } else {
            format!("{} = {}", returns.join(", "), call)
        }
    }

    /// Find where to insert the new function definition
    ///
    /// Inserts before the containing function if extracting from within a function, or at module level after imports/docstrings if already at module level.
    fn find_function_insertion_point(tree: &tree_sitter::Tree, text: &str, range: Range) -> Option<Position> {
        let start_byte = Self::position_to_byte_offset(text, range.start);
        let root = tree.root_node();

        if let Some(containing_def) = Self::find_containing_definition(root, start_byte) {
            let start_point = containing_def.start_position();
            return Some(Position { line: start_point.row as u32, character: 0 });
        }

        Self::find_module_level_insertion(root, text)
    }

    /// Find the containing function or class definition
    fn find_containing_definition(node: tree_sitter::Node, byte_offset: usize) -> Option<tree_sitter::Node> {
        if matches!(node.kind(), "function_definition" | "class_definition")
            && node.start_byte() < byte_offset
            && byte_offset < node.end_byte()
        {
            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                if let Some(inner) = Self::find_containing_definition(child, byte_offset) {
                    return Some(inner);
                }
            }

            return Some(node);
        }

        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if let Some(found) = Self::find_containing_definition(child, byte_offset) {
                return Some(found);
            }
        }

        None
    }

    /// Find insertion point at module level (after imports and docstrings)
    fn find_module_level_insertion(root: tree_sitter::Node, _text: &str) -> Option<Position> {
        let mut last_import_line = 0;
        let mut has_docstring = false;

        let mut cursor = root.walk();
        for child in root.children(&mut cursor) {
            match child.kind() {
                "import_statement" | "import_from_statement" => {
                    last_import_line = child.end_position().row;
                }
                "expression_statement" => {
                    if !has_docstring
                        && child.start_position().row <= 1
                        && let Some(string_node) = child.child(0)
                        && string_node.kind() == "string"
                    {
                        has_docstring = true;
                        last_import_line = child.end_position().row;
                    }
                }
                _ => {}
            }
        }

        Some(Position { line: (last_import_line + 2) as u32, character: 0 })
    }

    /// Indent code by a number of spaces
    fn indent_code(code: &str, spaces: usize) -> String {
        let indent = " ".repeat(spaces);
        code.lines().map(|line| format!("{indent}{line}\n")).collect()
    }

    /// Infer type annotation for a variable
    ///
    /// Uses the provided type map from the analyzer, falling back to "Any" if not available.
    fn infer_type_annotation(var_name: &str, type_map: &HashMap<String, String>) -> String {
        type_map.get(var_name).cloned().unwrap_or_else(|| "Any".to_string())
    }
}

/// Result of variable analysis for extract function
struct VariableAnalysis {
    /// Variables that need to be passed as parameters
    parameters: Vec<String>,
    /// Variables that need to be returned
    returns: Vec<String>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extract_range_text_single_line() {
        let text = "x = 1 + 2 + 3";
        let range = Range { start: Position { line: 0, character: 4 }, end: Position { line: 0, character: 13 } };

        let result = ExtractFunctionProvider::extract_range_text(text, range);
        assert_eq!(result, Some("1 + 2 + 3".to_string()));
    }

    #[test]
    fn test_extract_range_text_multi_line() {
        let text = "x = 1\ny = 2\nz = 3";
        let range = Range { start: Position { line: 0, character: 4 }, end: Position { line: 2, character: 5 } };

        let result = ExtractFunctionProvider::extract_range_text(text, range);
        assert!(result.is_some());
    }

    #[test]
    fn test_generate_function_definition_no_params_no_returns() {
        let type_map = HashMap::new();
        let result =
            ExtractFunctionProvider::generate_function_definition("foo", "print('hello')", &[], &[], &type_map);

        assert!(result.contains("def foo():"));
        assert!(result.contains("print('hello')"));
    }

    #[test]
    fn test_generate_function_definition_with_params() {
        let type_map = HashMap::new();
        let result = ExtractFunctionProvider::generate_function_definition(
            "add",
            "result = a + b",
            &["a".to_string(), "b".to_string()],
            &["result".to_string()],
            &type_map,
        );

        assert!(result.contains("def add(a, b)"));
        assert!(result.contains("return result"));
    }

    #[test]
    fn test_generate_function_call_no_params_no_returns() {
        let result = ExtractFunctionProvider::generate_function_call("foo", &[], &[]);
        assert_eq!(result, "foo()");
    }

    #[test]
    fn test_generate_function_call_with_params_and_return() {
        let result = ExtractFunctionProvider::generate_function_call(
            "add",
            &["a".to_string(), "b".to_string()],
            &["result".to_string()],
        );
        assert_eq!(result, "result = add(a, b)");
    }

    #[test]
    fn test_indent_code() {
        let code = "x = 1\ny = 2";
        let result = ExtractFunctionProvider::indent_code(code, 4);
        assert!(result.contains("    x = 1"));
        assert!(result.contains("    y = 2"));
    }

    #[test]
    fn test_extract_range_text_edge_case_empty_selection() {
        let text = "x = 1";
        let range = Range { start: Position { line: 0, character: 2 }, end: Position { line: 0, character: 2 } };

        let result = ExtractFunctionProvider::extract_range_text(text, range);
        assert_eq!(result, Some(String::new()));
    }

    #[test]
    fn test_extract_range_text_edge_case_out_of_bounds() {
        let text = "x = 1";
        let range = Range { start: Position { line: 10, character: 0 }, end: Position { line: 10, character: 5 } };

        let result = ExtractFunctionProvider::extract_range_text(text, range);
        assert!(result.is_none());
    }

    #[test]
    fn test_position_to_byte_offset_first_line() {
        let text = "hello\nworld";
        let pos = Position { line: 0, character: 2 };
        let offset = ExtractFunctionProvider::position_to_byte_offset(text, pos);
        assert_eq!(offset, 2);
    }

    #[test]
    fn test_position_to_byte_offset_second_line() {
        let text = "hello\nworld";
        let pos = Position { line: 1, character: 2 };
        let offset = ExtractFunctionProvider::position_to_byte_offset(text, pos);
        assert_eq!(offset, 8);
    }

    #[test]
    fn test_generate_function_call_multiple_returns() {
        let result = ExtractFunctionProvider::generate_function_call(
            "compute",
            &["a".to_string(), "b".to_string()],
            &["x".to_string(), "y".to_string()],
        );
        assert_eq!(result, "x, y = compute(a, b)");
    }

    #[test]
    fn test_generate_function_definition_multiple_returns() {
        let type_map = HashMap::new();
        let result = ExtractFunctionProvider::generate_function_definition(
            "compute",
            "x = a + b\ny = a - b",
            &["a".to_string(), "b".to_string()],
            &["x".to_string(), "y".to_string()],
            &type_map,
        );

        assert!(result.contains("def compute(a, b) -> tuple[Any, Any]:"));
        assert!(result.contains("return x, y"));
    }

    #[test]
    fn test_infer_type_annotation_with_known_type() {
        let mut type_map = HashMap::new();
        type_map.insert("x".to_string(), "int".to_string());

        let result = ExtractFunctionProvider::infer_type_annotation("x", &type_map);
        assert_eq!(result, "int");
    }

    #[test]
    fn test_infer_type_annotation_with_unknown_type() {
        let type_map = HashMap::new();
        let result = ExtractFunctionProvider::infer_type_annotation("unknown_var", &type_map);
        assert_eq!(result, "Any");
    }

    #[test]
    fn test_generate_function_definition_with_inferred_types() {
        let mut type_map = HashMap::new();
        type_map.insert("result".to_string(), "int".to_string());

        let result = ExtractFunctionProvider::generate_function_definition(
            "add",
            "result = a + b",
            &["a".to_string(), "b".to_string()],
            &["result".to_string()],
            &type_map,
        );

        assert!(result.contains("def add(a, b) -> int:"));
        assert!(result.contains("return result"));
    }

    #[test]
    fn test_generate_function_definition_with_multiple_inferred_types() {
        let mut type_map = HashMap::new();
        type_map.insert("x".to_string(), "int".to_string());
        type_map.insert("y".to_string(), "str".to_string());

        let result = ExtractFunctionProvider::generate_function_definition(
            "compute",
            "x = a + b\ny = str(a)",
            &["a".to_string(), "b".to_string()],
            &["x".to_string(), "y".to_string()],
            &type_map,
        );

        assert!(result.contains("def compute(a, b) -> tuple[int, str]:"));
        assert!(result.contains("return x, y"));
    }

    #[test]
    fn test_generate_function_definition_with_partial_type_info() {
        let mut type_map = HashMap::new();
        type_map.insert("x".to_string(), "int".to_string());

        let result = ExtractFunctionProvider::generate_function_definition(
            "compute",
            "x = a + b\ny = unknown",
            &["a".to_string(), "b".to_string()],
            &["x".to_string(), "y".to_string()],
            &type_map,
        );

        assert!(result.contains("def compute(a, b) -> tuple[int, Any]:"));
        assert!(result.contains("return x, y"));
    }
}

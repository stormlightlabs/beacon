//! Extract Variable refactoring
//!
//! Extracts a selected expression into a new variable, with optional replacement of all occurrences

use super::refactoring::{EditCollector, RefactoringContext, RefactoringValidator};

use lsp_types::{Position, Range, TextEdit, Url, WorkspaceEdit};

/// Parameters for extract variable refactoring
pub struct ExtractVariableParams {
    /// The file containing the expression
    pub uri: Url,
    /// The range of the expression to extract
    pub range: Range,
    /// The name for the new variable
    pub variable_name: String,
    /// Whether to replace all occurrences or just the selected one
    pub replace_all: bool,
}

/// Extract variable refactoring provider
pub struct ExtractVariableProvider {
    context: RefactoringContext,
}

impl ExtractVariableProvider {
    /// Create a new extract variable provider
    pub fn new(context: RefactoringContext) -> Self {
        Self { context }
    }

    /// Execute the extract variable refactoring
    pub async fn execute(
        &self, params: ExtractVariableParams, analyzer: Option<&mut crate::analysis::Analyzer>,
    ) -> Option<WorkspaceEdit> {
        if RefactoringValidator::validate_identifier(&params.variable_name).is_err() {
            return None;
        }

        let (tree, text) = self.context.get_tree_and_text(&params.uri)?;

        let expression_text = Self::extract_range_text(&text, params.range)?;

        let expression_node = Self::find_expression_node(&tree, &text, params.range)?;

        let type_annotation = if let Some(a) = analyzer {
            Self::infer_type(a, &params.uri, params.range.start)
        } else {
            None
        };

        let mut occurrences = vec![params.range];

        if params.replace_all {
            let duplicates = Self::find_duplicate_expressions(&tree, &text, expression_node, &expression_text);
            occurrences.extend(duplicates);
        }

        let insertion_point = Self::find_insertion_point(&text, &occurrences)?;

        let assignment = Self::generate_assignment(&params.variable_name, &expression_text, type_annotation.as_deref());

        let mut collector = EditCollector::new();

        for occurrence in occurrences.iter().rev() {
            collector.add_edit(
                params.uri.clone(),
                TextEdit { range: *occurrence, new_text: params.variable_name.clone() },
            );
        }

        collector.add_edit(
            params.uri.clone(),
            TextEdit {
                range: Range { start: insertion_point, end: insertion_point },
                new_text: format!("{assignment}\n"),
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

    /// Find the expression node at the given range
    fn find_expression_node<'a>(
        tree: &'a tree_sitter::Tree, text: &str, range: Range,
    ) -> Option<tree_sitter::Node<'a>> {
        let start_byte = Self::position_to_byte_offset(text, range.start);
        let end_byte = Self::position_to_byte_offset(text, range.end);

        Self::find_node_at_range(tree.root_node(), start_byte, end_byte)
    }

    /// Find a node that matches the given byte range
    fn find_node_at_range<'a>(node: tree_sitter::Node<'a>, start: usize, end: usize) -> Option<tree_sitter::Node<'a>> {
        if node.start_byte() == start && node.end_byte() == end {
            return Some(node);
        }

        if node.start_byte() > start || node.end_byte() < end {
            return None;
        }

        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if let Some(found) = Self::find_node_at_range(child, start, end) {
                return Some(found);
            }
        }

        if node.start_byte() <= start && node.end_byte() >= end {
            return Some(node);
        }

        None
    }

    /// Find duplicate expressions in the tree that match the given expression
    fn find_duplicate_expressions<'a>(
        tree: &'a tree_sitter::Tree, text: &str, expression_node: tree_sitter::Node<'a>, expression_text: &str,
    ) -> Vec<Range> {
        let mut duplicates = Vec::new();
        let original_start = expression_node.start_byte();
        let original_end = expression_node.end_byte();

        Self::collect_matching_expressions(
            tree.root_node(),
            text,
            expression_text,
            original_start,
            original_end,
            &mut duplicates,
        );

        duplicates
    }

    /// Recursively collect expressions that match the given text
    fn collect_matching_expressions<'a>(
        node: tree_sitter::Node<'a>, text: &str, target_text: &str, original_start: usize, original_end: usize,
        duplicates: &mut Vec<Range>,
    ) {
        if node.start_byte() == original_start && node.end_byte() == original_end {
            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                Self::collect_matching_expressions(child, text, target_text, original_start, original_end, duplicates);
            }
            return;
        }

        if Self::is_expression_node(node.kind()) {
            if let Ok(node_text) = node.utf8_text(text.as_bytes()) {
                if node_text.trim() == target_text.trim() {
                    let start_pos = Self::byte_offset_to_position(text, node.start_byte());
                    let end_pos = Self::byte_offset_to_position(text, node.end_byte());
                    duplicates.push(Range { start: start_pos, end: end_pos });
                    return;
                }
            }
        }

        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            Self::collect_matching_expressions(child, text, target_text, original_start, original_end, duplicates);
        }
    }

    /// Check if a node kind represents an expression
    fn is_expression_node(kind: &str) -> bool {
        matches!(
            kind,
            "binary_operator"
                | "unary_operator"
                | "comparison_operator"
                | "boolean_operator"
                | "call"
                | "attribute"
                | "subscript"
                | "list"
                | "tuple"
                | "dictionary"
                | "set"
                | "list_comprehension"
                | "dictionary_comprehension"
                | "set_comprehension"
                | "generator_expression"
                | "lambda"
                | "conditional_expression"
                | "string"
                | "integer"
                | "float"
                | "true"
                | "false"
                | "none"
                | "identifier"
                | "parenthesized_expression"
        )
    }

    /// Find the insertion point for the variable assignment
    fn find_insertion_point(text: &str, occurrences: &[Range]) -> Option<Position> {
        let first_occurrence = occurrences.iter().min_by_key(|r| (r.start.line, r.start.character))?;

        let line = first_occurrence.start.line;

        let lines: Vec<&str> = text.lines().collect();
        if line as usize >= lines.len() {
            return None;
        }

        let current_line = lines[line as usize];
        let indent = Self::calculate_indent(current_line);

        Some(Position { line, character: indent as u32 })
    }

    /// Calculate the indentation of a line
    fn calculate_indent(line: &str) -> usize {
        line.chars().take_while(|c| c.is_whitespace()).count()
    }

    /// Infer the type of the expression at the given position
    fn infer_type(analyzer: &mut crate::analysis::Analyzer, uri: &Url, position: Position) -> Option<String> {
        if let Ok(Some(ty)) = analyzer.type_at_position(uri, position) {
            let type_str = ty.to_string();
            if !type_str.contains('\'') && type_str != "Any" {
                return Some(type_str);
            }
        }
        None
    }

    /// Generate the variable assignment statement
    fn generate_assignment(variable_name: &str, expression: &str, type_annotation: Option<&str>) -> String {
        if let Some(ty) = type_annotation {
            format!("{variable_name}: {ty} = {expression}")
        } else {
            format!("{variable_name} = {expression}")
        }
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

    /// Convert byte offset to LSP position
    fn byte_offset_to_position(text: &str, offset: usize) -> Position {
        let mut line = 0;
        let mut col = 0;
        let mut current_offset = 0;

        for ch in text.chars() {
            if current_offset >= offset {
                break;
            }
            if ch == '\n' {
                line += 1;
                col = 0;
            } else {
                col += 1;
            }
            current_offset += ch.len_utf8();
        }

        Position { line: line as u32, character: col as u32 }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extract_range_text_single_line() {
        let text = "x = 1 + 2 + 3";
        let range = Range { start: Position { line: 0, character: 4 }, end: Position { line: 0, character: 13 } };

        let result = ExtractVariableProvider::extract_range_text(text, range);
        assert_eq!(result, Some("1 + 2 + 3".to_string()));
    }

    #[test]
    fn test_extract_range_text_multi_line() {
        let text = "result = (\n    1 + 2\n    + 3\n)";
        let range = Range { start: Position { line: 0, character: 9 }, end: Position { line: 3, character: 1 } };

        let result = ExtractVariableProvider::extract_range_text(text, range);
        assert!(result.is_some());
        let text_content = result.unwrap();
        assert!(text_content.contains("1 + 2"));
        assert!(text_content.contains("+ 3"));
    }

    #[test]
    fn test_generate_assignment_without_type() {
        let result = ExtractVariableProvider::generate_assignment("temp", "1 + 2", None);
        assert_eq!(result, "temp = 1 + 2");
    }

    #[test]
    fn test_generate_assignment_with_type() {
        let result = ExtractVariableProvider::generate_assignment("temp", "1 + 2", Some("int"));
        assert_eq!(result, "temp: int = 1 + 2");
    }

    #[test]
    fn test_calculate_indent_no_indent() {
        let line = "x = 1";
        assert_eq!(ExtractVariableProvider::calculate_indent(line), 0);
    }

    #[test]
    fn test_calculate_indent_with_spaces() {
        let line = "    x = 1";
        assert_eq!(ExtractVariableProvider::calculate_indent(line), 4);
    }

    #[test]
    fn test_calculate_indent_with_tabs() {
        let line = "\t\tx = 1";
        assert_eq!(ExtractVariableProvider::calculate_indent(line), 2);
    }

    #[test]
    fn test_is_expression_node_binary_operator() {
        assert!(ExtractVariableProvider::is_expression_node("binary_operator"));
    }

    #[test]
    fn test_is_expression_node_call() {
        assert!(ExtractVariableProvider::is_expression_node("call"));
    }

    #[test]
    fn test_is_expression_node_identifier() {
        assert!(ExtractVariableProvider::is_expression_node("identifier"));
    }

    #[test]
    fn test_is_expression_node_not_expression() {
        assert!(!ExtractVariableProvider::is_expression_node("assignment"));
        assert!(!ExtractVariableProvider::is_expression_node("function_definition"));
        assert!(!ExtractVariableProvider::is_expression_node("module"));
    }

    #[test]
    fn test_position_to_byte_offset_first_line() {
        let text = "hello world";
        let position = Position { line: 0, character: 6 };
        assert_eq!(ExtractVariableProvider::position_to_byte_offset(text, position), 6);
    }

    #[test]
    fn test_position_to_byte_offset_second_line() {
        let text = "hello\nworld";
        let position = Position { line: 1, character: 3 };
        assert_eq!(ExtractVariableProvider::position_to_byte_offset(text, position), 9);
    }

    #[test]
    fn test_byte_offset_to_position_first_line() {
        let text = "hello world";
        let position = ExtractVariableProvider::byte_offset_to_position(text, 6);
        assert_eq!(position, Position { line: 0, character: 6 });
    }

    #[test]
    fn test_byte_offset_to_position_second_line() {
        let text = "hello\nworld";
        let position = ExtractVariableProvider::byte_offset_to_position(text, 9);
        assert_eq!(position, Position { line: 1, character: 3 });
    }

    #[test]
    fn test_find_insertion_point_single_occurrence() {
        let text = "    x = 1 + 2\n    y = x * 2";
        let occurrences =
            vec![Range { start: Position { line: 0, character: 8 }, end: Position { line: 0, character: 13 } }];

        let result = ExtractVariableProvider::find_insertion_point(text, &occurrences);
        assert_eq!(result, Some(Position { line: 0, character: 4 }));
    }

    #[test]
    fn test_find_insertion_point_multiple_occurrences() {
        let text = "    x = 1 + 2\n    y = 1 + 2";
        let occurrences = vec![
            Range { start: Position { line: 0, character: 8 }, end: Position { line: 0, character: 13 } },
            Range { start: Position { line: 1, character: 8 }, end: Position { line: 1, character: 13 } },
        ];

        let result = ExtractVariableProvider::find_insertion_point(text, &occurrences);
        assert_eq!(result, Some(Position { line: 0, character: 4 }));
    }

    #[test]
    fn test_extract_range_text_edge_case_empty_selection() {
        let text = "x = 1";
        let range = Range { start: Position { line: 0, character: 2 }, end: Position { line: 0, character: 2 } };

        let result = ExtractVariableProvider::extract_range_text(text, range);
        assert_eq!(result, Some("".to_string()));
    }

    #[test]
    fn test_extract_range_text_out_of_bounds() {
        let text = "x = 1";
        let range = Range { start: Position { line: 0, character: 0 }, end: Position { line: 5, character: 0 } };

        let result = ExtractVariableProvider::extract_range_text(text, range);
        assert_eq!(result, None);
    }

    #[test]
    fn test_extract_range_text_multiline_list() {
        let text = "result = [\n    1,\n    2,\n    3\n]";
        let range = Range { start: Position { line: 0, character: 9 }, end: Position { line: 4, character: 1 } };

        let result = ExtractVariableProvider::extract_range_text(text, range);
        assert!(result.is_some());
        let content = result.unwrap();
        assert!(content.contains("1,"));
        assert!(content.contains("2,"));
        assert!(content.contains("3"));
    }

    #[test]
    fn test_extract_range_text_multiline_dict() {
        let text = "config = {\n    'key1': 'value1',\n    'key2': 'value2'\n}";
        let range = Range { start: Position { line: 0, character: 9 }, end: Position { line: 3, character: 1 } };

        let result = ExtractVariableProvider::extract_range_text(text, range);
        assert!(result.is_some());
        let content = result.unwrap();
        assert!(content.contains("'key1': 'value1'"));
        assert!(content.contains("'key2': 'value2'"));
    }

    #[test]
    fn test_extract_range_text_multiline_string() {
        let text = "text = '''\nHello\nWorld\n'''";
        let range = Range { start: Position { line: 0, character: 7 }, end: Position { line: 3, character: 3 } };

        let result = ExtractVariableProvider::extract_range_text(text, range);
        assert!(result.is_some());
        let content = result.unwrap();
        assert!(content.contains("'''"));
        assert!(content.contains("Hello"));
        assert!(content.contains("World"));
    }

    #[test]
    fn test_extract_range_text_nested_structure() {
        let text = "data = [\n    {'a': 1},\n    {'b': 2}\n]";
        let range = Range { start: Position { line: 0, character: 7 }, end: Position { line: 3, character: 1 } };

        let result = ExtractVariableProvider::extract_range_text(text, range);
        assert!(result.is_some());
        let content = result.unwrap();
        assert!(content.contains("{'a': 1}"));
        assert!(content.contains("{'b': 2}"));
    }

    #[test]
    fn test_extract_range_text_multiline_function_call() {
        let text = "result = some_function(\n    arg1,\n    arg2\n)";
        let range = Range { start: Position { line: 0, character: 9 }, end: Position { line: 3, character: 1 } };

        let result = ExtractVariableProvider::extract_range_text(text, range);
        assert!(result.is_some());
        let content = result.unwrap();
        assert!(content.contains("some_function("));
        assert!(content.contains("arg1,"));
        assert!(content.contains("arg2"));
    }

    #[test]
    fn test_find_insertion_point_nested_scope() {
        let text = "def func():\n    x = 1 + 2\n    y = x * 2";
        let occurrences =
            vec![Range { start: Position { line: 1, character: 8 }, end: Position { line: 1, character: 13 } }];

        let result = ExtractVariableProvider::find_insertion_point(text, &occurrences);
        assert_eq!(result, Some(Position { line: 1, character: 4 }));
    }

    #[test]
    fn test_find_insertion_point_deeply_nested() {
        let text = "def func():\n    if True:\n        x = 1 + 2\n        y = x * 2";
        let occurrences =
            vec![Range { start: Position { line: 2, character: 12 }, end: Position { line: 2, character: 17 } }];

        let result = ExtractVariableProvider::find_insertion_point(text, &occurrences);
        assert_eq!(result, Some(Position { line: 2, character: 8 }));
    }

    #[test]
    fn test_generate_assignment_complex_expression() {
        let expression = "{\n    'key': value,\n    'other': data\n}";
        let result = ExtractVariableProvider::generate_assignment("config", expression, Some("dict[str, Any]"));

        assert!(result.starts_with("config: dict[str, Any] = "));
        assert!(result.contains("'key': value"));
    }

    #[test]
    fn test_is_expression_node_list_comprehension() {
        assert!(ExtractVariableProvider::is_expression_node("list_comprehension"));
    }

    #[test]
    fn test_is_expression_node_dict_comprehension() {
        assert!(ExtractVariableProvider::is_expression_node("dictionary_comprehension"));
    }

    #[test]
    fn test_is_expression_node_lambda() {
        assert!(ExtractVariableProvider::is_expression_node("lambda"));
    }
}

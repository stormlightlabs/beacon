//! Inline Function refactoring
//!
//! Replaces function call sites with the function body, substituting parameters with arguments.
//!
//! # Algorithm
//!
//! 1. Find the function definition at the cursor position
//! 2. Extract function parameters and body
//! 3. Find all call sites across the workspace
//! 4. For each call site:
//!    - Replace the call with the function body
//!    - Substitute parameters with arguments
//!    - Handle return statements
//!    - Adjust variable scoping
//! 5. Remove the function definition if no longer used (or if inline_all is true)
//!
//! # Notes
//!
//! - Handling return statements (early returns, multiple returns)
//! - Variable name conflicts in target scope
//! - Side effects and evaluation order
//! - Control flow (break, continue, return in inlined code)

use super::refactoring::{EditCollector, RefactoringContext};
use crate::utils;

use lsp_types::{Position, Range, TextEdit, Url, WorkspaceEdit};
use std::collections::{HashMap, HashSet};

/// Parameters for inline function refactoring
pub struct InlineFunctionParams {
    /// The file containing the function to inline
    pub uri: Url,
    /// Position of the function definition or call site
    pub position: Position,
    /// Whether to inline all calls or just one
    pub inline_all: bool,
}

/// Inline function refactoring provider
pub struct InlineFunctionProvider {
    context: RefactoringContext,
}

impl InlineFunctionProvider {
    /// Create a new inline function provider
    pub fn new(context: RefactoringContext) -> Self {
        Self { context }
    }

    /// Execute the inline function refactoring
    pub async fn execute(&self, params: InlineFunctionParams) -> Option<WorkspaceEdit> {
        let (tree, text) = self.context.get_tree_and_text(&params.uri)?;
        let _symbol_table = self.context.get_symbol_table(&params.uri)?;

        let function_def = Self::find_function_definition(&tree, &text, params.position)?;
        let function_info = Self::extract_function_info(function_def, &text)?;

        let mut call_sites = Vec::new();

        if params.inline_all {
            for file_ctx in self.context.iter_relevant_files(&params.uri).await {
                Self::find_call_sites(
                    file_ctx.tree.root_node(),
                    &file_ctx.text,
                    &function_info.name,
                    &file_ctx.uri,
                    &mut call_sites,
                );
            }
        } else {
            if let Some(call_site) = Self::find_call_at_position(&tree, &text, params.position) {
                if let Some(call_name) = Self::get_call_name(call_site, &text)
                    && call_name == function_info.name
                {
                    call_sites.push(CallSite {
                        uri: params.uri.clone(),
                        range: utils::tree_sitter_range_to_lsp_range(&text, call_site.range()),
                    });
                }
            } else {
                for file_ctx in self.context.iter_relevant_files(&params.uri).await {
                    Self::find_call_sites(
                        file_ctx.tree.root_node(),
                        &file_ctx.text,
                        &function_info.name,
                        &file_ctx.uri,
                        &mut call_sites,
                    );
                }
            }
        }

        if call_sites.is_empty() {
            return None;
        }

        let mut collector = EditCollector::new();

        for call_site in &call_sites {
            let (call_tree, call_text) = self.context.get_tree_and_text(&call_site.uri)?;

            let byte_offset = Self::position_to_byte_offset(&call_text, call_site.range.start);
            let call_node = Self::find_call_at_offset(call_tree.root_node(), byte_offset)?;

            let arguments = Self::extract_call_arguments(call_node, &call_text)?;

            let inlined_body =
                Self::inline_function_body(&function_info, &arguments, &call_text, call_site.range.start)?;

            collector.add_edit(
                call_site.uri.clone(),
                TextEdit { range: call_site.range, new_text: inlined_body },
            );
        }

        if params.inline_all {
            let func_range = utils::tree_sitter_range_to_lsp_range(&text, function_def.range());

            let removal_range = Self::expand_range_for_removal(&text, func_range);

            collector.add_edit(
                params.uri.clone(),
                TextEdit { range: removal_range, new_text: String::new() },
            );
        }

        collector.into_workspace_edit()
    }

    /// Find function definition at or near a position
    fn find_function_definition<'a>(
        tree: &'a tree_sitter::Tree, text: &str, position: Position,
    ) -> Option<tree_sitter::Node<'a>> {
        let byte_offset = Self::position_to_byte_offset(text, position);
        let root = tree.root_node();

        Self::find_function_at_offset(root, byte_offset)
    }

    /// Recursively find function definition containing the byte offset
    fn find_function_at_offset(node: tree_sitter::Node, byte_offset: usize) -> Option<tree_sitter::Node> {
        if node.kind() == "function_definition" {
            if let Some(name_node) = node.child_by_field_name("name")
                && name_node.start_byte() <= byte_offset
                && byte_offset <= name_node.end_byte()
            {
                return Some(node);
            }

            if node.start_byte() <= byte_offset && byte_offset <= node.end_byte() {
                if let Some(body) = node.child_by_field_name("body")
                    && byte_offset >= body.start_byte()
                {
                    return Self::find_function_at_offset(body, byte_offset);
                }
                return Some(node);
            }
        }

        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if let Some(func) = Self::find_function_at_offset(child, byte_offset) {
                return Some(func);
            }
        }

        None
    }

    /// Find a call expression at a specific position
    fn find_call_at_position<'a>(
        tree: &'a tree_sitter::Tree, text: &str, position: Position,
    ) -> Option<tree_sitter::Node<'a>> {
        let byte_offset = Self::position_to_byte_offset(text, position);
        let root = tree.root_node();

        Self::find_call_at_offset(root, byte_offset)
    }

    /// Recursively find call expression at byte offset
    fn find_call_at_offset(node: tree_sitter::Node, byte_offset: usize) -> Option<tree_sitter::Node> {
        if node.kind() == "call" && node.start_byte() <= byte_offset && byte_offset <= node.end_byte() {
            return Some(node);
        }

        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if let Some(call) = Self::find_call_at_offset(child, byte_offset) {
                return Some(call);
            }
        }

        None
    }

    /// Extract function name, parameters, and body
    fn extract_function_info(node: tree_sitter::Node, text: &str) -> Option<FunctionInfo> {
        if node.kind() != "function_definition" {
            return None;
        }

        let name_node = node.child_by_field_name("name")?;
        let name = name_node.utf8_text(text.as_bytes()).ok()?.to_string();

        let params_node = node.child_by_field_name("parameters")?;
        let parameters = Self::extract_parameter_names(params_node, text)?;

        let body_node = node.child_by_field_name("body")?;
        let body = Self::extract_function_body(body_node, text)?;

        let return_type = Self::analyze_return_type(body_node, text);

        Some(FunctionInfo { name, parameters, body, return_type })
    }

    /// Extract parameter names from function parameters
    fn extract_parameter_names(params_node: tree_sitter::Node, text: &str) -> Option<Vec<String>> {
        let mut parameters = Vec::new();
        let mut cursor = params_node.walk();

        for child in params_node.children(&mut cursor) {
            match child.kind() {
                "identifier" => {
                    if let Ok(param_name) = child.utf8_text(text.as_bytes()) {
                        parameters.push(param_name.to_string());
                    }
                }
                "typed_parameter" | "default_parameter" => {
                    if let Some(name_node) = child.child_by_field_name("name")
                        && let Ok(param_name) = name_node.utf8_text(text.as_bytes())
                    {
                        parameters.push(param_name.to_string());
                    }
                }
                _ => {}
            }
        }

        Some(parameters)
    }

    /// Extract function body text (without the def line and indentation)
    fn extract_function_body(body_node: tree_sitter::Node, text: &str) -> Option<String> {
        let body_text = body_node.utf8_text(text.as_bytes()).ok()?.to_string();
        let dedented = Self::dedent_code(&body_text);
        Some(dedented)
    }

    /// Analyze what the function returns
    fn analyze_return_type(body_node: tree_sitter::Node, text: &str) -> ReturnType {
        let mut has_return = false;
        let mut return_values = HashSet::new();

        Self::collect_return_statements(body_node, text, &mut has_return, &mut return_values);

        if !has_return {
            ReturnType::None
        } else if return_values.is_empty() {
            ReturnType::None
        } else if return_values.len() == 1 {
            ReturnType::Single(return_values.into_iter().next().unwrap())
        } else {
            ReturnType::Multiple(return_values.into_iter().collect())
        }
    }

    /// Collect return statements from function body
    fn collect_return_statements(
        node: tree_sitter::Node, text: &str, has_return: &mut bool, return_values: &mut HashSet<String>,
    ) {
        if node.kind() == "return_statement" {
            *has_return = true;

            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                if child.kind() != "return"
                    && let Ok(value) = child.utf8_text(text.as_bytes())
                {
                    return_values.insert(value.to_string());
                }
            }
        }

        if node.kind() == "function_definition" {
            return;
        }

        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            Self::collect_return_statements(child, text, has_return, return_values);
        }
    }

    /// Find all call sites for a function
    fn find_call_sites(
        node: tree_sitter::Node, text: &str, function_name: &str, uri: &Url, call_sites: &mut Vec<CallSite>,
    ) {
        if node.kind() == "call"
            && let Some(func_node) = node.child_by_field_name("function")
            && func_node.kind() == "identifier"
            && let Ok(name) = func_node.utf8_text(text.as_bytes())
            && name == function_name
        {
            call_sites
                .push(CallSite { uri: uri.clone(), range: utils::tree_sitter_range_to_lsp_range(text, node.range()) });
        }

        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            Self::find_call_sites(child, text, function_name, uri, call_sites);
        }
    }

    /// Get the function name from a call expression
    fn get_call_name(call_node: tree_sitter::Node, text: &str) -> Option<String> {
        let func_node = call_node.child_by_field_name("function")?;
        if func_node.kind() == "identifier" {
            Some(func_node.utf8_text(text.as_bytes()).ok()?.to_string())
        } else {
            None
        }
    }

    /// Extract arguments from a call expression
    fn extract_call_arguments(call_node: tree_sitter::Node, text: &str) -> Option<Vec<String>> {
        let args_node = call_node.child_by_field_name("arguments")?;
        let mut arguments = Vec::new();
        let mut cursor = args_node.walk();

        for child in args_node.children(&mut cursor) {
            match child.kind() {
                "(" | ")" | "," => {}
                _ => {
                    if let Ok(arg_text) = child.utf8_text(text.as_bytes()) {
                        arguments.push(arg_text.to_string());
                    }
                }
            }
        }

        Some(arguments)
    }

    /// Inline the function body with parameter substitution
    fn inline_function_body(
        func_info: &FunctionInfo, arguments: &[String], _text: &str, indent_pos: Position,
    ) -> Option<String> {
        let mut substitutions = HashMap::new();
        for (param, arg) in func_info.parameters.iter().zip(arguments.iter()) {
            substitutions.insert(param.clone(), arg.clone());
        }

        let mut inlined = func_info.body.clone();
        for (param, arg) in &substitutions {
            inlined = Self::substitute_parameter(&inlined, param, arg);
        }

        inlined = match &func_info.return_type {
            ReturnType::None => Self::remove_return_statements(&inlined),
            ReturnType::Single(_) => Self::transform_single_return(&inlined),
            // TODO: Complex case: might need temporary variables
            ReturnType::Multiple(_) => Self::transform_multiple_returns(&inlined),
        };

        let indented = Self::indent_to_position(&inlined, indent_pos.character as usize);

        Some(indented.trim_end().to_string())
    }

    /// Substitute a parameter with its argument value
    fn substitute_parameter(body: &str, param: &str, arg: &str) -> String {
        // Simple word boundary replacement
        // TODO: can be improved with AST-based substitution
        let pattern = format!(r"\b{}\b", regex::escape(param));
        let re = regex::Regex::new(&pattern).unwrap();
        re.replace_all(body, arg).to_string()
    }

    /// Remove return statements from body
    fn remove_return_statements(body: &str) -> String {
        let lines: Vec<&str> = body.lines().collect();
        let mut result = Vec::new();

        for line in lines {
            let trimmed = line.trim_start();
            if trimmed.starts_with("return") && trimmed.trim_end() == "return" {
                continue;
            }
            result.push(line);
        }

        result.join("\n")
    }

    /// Transform return statements for single return value
    fn transform_single_return(body: &str) -> String {
        let lines: Vec<&str> = body.lines().collect();

        if lines.len() == 1 {
            let line = lines[0].trim();
            if let Some(value) = line.strip_prefix("return ") {
                return value.to_string();
            }
        }

        // For multi-line bodies, keep as-is for now (complex case)
        // TODO: Handle early returns with control flow
        body.to_string()
    }

    /// Transform return statements for multiple return values
    fn transform_multiple_returns(body: &str) -> String {
        // Complex case: needs control flow analysis
        // For now, keep as-is
        // TODO: Convert to if/else chain or use temporary variables
        body.to_string()
    }

    /// Dedent code by removing common leading whitespace
    fn dedent_code(code: &str) -> String {
        let lines: Vec<&str> = code.lines().collect();
        if lines.is_empty() {
            return String::new();
        }

        let min_indent = lines
            .iter()
            .filter(|line| !line.trim().is_empty())
            .map(|line| line.len() - line.trim_start().len())
            .min()
            .unwrap_or(0);

        lines
            .iter()
            .map(|line| if line.len() >= min_indent { &line[min_indent..] } else { line })
            .collect::<Vec<_>>()
            .join("\n")
    }

    /// Indent code to match a specific column position
    fn indent_to_position(code: &str, column: usize) -> String {
        let indent = " ".repeat(column);
        code.lines()
            .map(
                |line| {
                    if line.trim().is_empty() { String::new() } else { format!("{}{}", indent, line) }
                },
            )
            .collect::<Vec<_>>()
            .join("\n")
    }

    /// Expand range to include leading/trailing whitespace for clean removal
    fn expand_range_for_removal(text: &str, range: Range) -> Range {
        let lines: Vec<&str> = text.lines().collect();
        let start = Position { line: range.start.line, character: 0 };
        let end_line = if range.end.line + 1 < lines.len() as u32 { range.end.line + 1 } else { range.end.line };
        let end = Position { line: end_line, character: 0 };
        Range { start, end }
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
}

/// Information about a function to be inlined
struct FunctionInfo {
    name: String,
    parameters: Vec<String>,
    body: String,
    return_type: ReturnType,
}

/// Type of return value(s) from a function
enum ReturnType {
    None,
    Single(String),
    Multiple(Vec<String>),
}

/// A call site to be inlined
struct CallSite {
    uri: Url,
    range: Range,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_dedent_code() {
        let code = "    x = 1\n    y = 2\n    return x + y";
        let result = InlineFunctionProvider::dedent_code(code);
        assert_eq!(result, "x = 1\ny = 2\nreturn x + y");
    }

    #[test]
    fn test_dedent_code_mixed_indentation() {
        let code = "    x = 1\n        y = 2\n    return x + y";
        let result = InlineFunctionProvider::dedent_code(code);
        assert_eq!(result, "x = 1\n    y = 2\nreturn x + y");
    }

    #[test]
    fn test_substitute_parameter() {
        let body = "result = x + y\nreturn result";
        let result = InlineFunctionProvider::substitute_parameter(body, "x", "a");
        assert_eq!(result, "result = a + y\nreturn result");
    }

    #[test]
    fn test_substitute_parameter_word_boundary() {
        let body = "x = x + 1";
        let result = InlineFunctionProvider::substitute_parameter(body, "x", "value");
        assert_eq!(result, "value = value + 1");
    }

    #[test]
    fn test_remove_return_statements() {
        let body = "x = 1\ny = 2\nreturn";
        let result = InlineFunctionProvider::remove_return_statements(body);
        assert_eq!(result, "x = 1\ny = 2");
    }

    #[test]
    fn test_transform_single_return() {
        let body = "return x + y";
        let result = InlineFunctionProvider::transform_single_return(body);
        assert_eq!(result, "x + y");
    }

    #[test]
    fn test_indent_to_position() {
        let code = "x = 1\ny = 2";
        let result = InlineFunctionProvider::indent_to_position(code, 4);
        assert_eq!(result, "    x = 1\n    y = 2");
    }

    #[test]
    fn test_expand_range_for_removal() {
        let text = "def foo():\n    pass\nx = 1";
        let range = Range { start: Position { line: 0, character: 0 }, end: Position { line: 1, character: 8 } };
        let result = InlineFunctionProvider::expand_range_for_removal(text, range);
        assert_eq!(result.start.line, 0);
        assert_eq!(result.start.character, 0);
        assert_eq!(result.end.line, 2);
    }
}

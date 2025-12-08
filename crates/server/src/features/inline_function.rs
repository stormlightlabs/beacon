//! Inline Function refactoring
//!
//! Replaces function call sites with the function body, substituting parameters with arguments.
//! Supports both statement and expression contexts, with handling of side effects.

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
        } else if let Some(call_site) = Self::find_call_at_position(&tree, &text, params.position) {
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

        if call_sites.is_empty() {
            return None;
        }

        let mut collector = EditCollector::new();

        for call_site in &call_sites {
            let (call_tree, call_text) = self.context.get_tree_and_text(&call_site.uri)?;

            let byte_offset = Self::position_to_byte_offset(&call_text, call_site.range.start);
            let call_node = Self::find_call_at_offset(call_tree.root_node(), byte_offset)?;

            let arguments = Self::extract_call_arguments(call_node, &call_text)?;
            let is_expr_context = Self::is_expression_context(call_node);

            let inline_result = Self::inline_function_body(
                &function_info,
                &arguments,
                &call_text,
                call_node,
                is_expr_context,
                call_site.range.start,
            )?;

            match inline_result {
                InlineResult::Simple { replacement } => {
                    collector.add_edit(
                        call_site.uri.clone(),
                        TextEdit { range: call_site.range, new_text: replacement },
                    );
                }
                InlineResult::WithPrelude { prelude, replacement, insertion_point } => {
                    collector.add_edit(
                        call_site.uri.clone(),
                        TextEdit { range: insertion_point, new_text: prelude },
                    );
                    collector.add_edit(
                        call_site.uri.clone(),
                        TextEdit { range: call_site.range, new_text: replacement },
                    );
                }
            }
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

        if return_values.is_empty() {
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

    /// Check if a call node is in an expression context (not a statement)
    ///
    /// A call is in an expression context if it's part of:
    /// - An assignment (right side)
    /// - A return statement
    /// - Another expression (binary op, function argument, list element, etc.)
    fn is_expression_context(call_node: tree_sitter::Node) -> bool {
        let Some(parent) = call_node.parent() else {
            return false;
        };

        match parent.kind() {
            "assignment"
            | "augmented_assignment"
            | "return_statement"
            | "binary_operator"
            | "unary_operator"
            | "comparison_operator"
            | "boolean_operator"
            | "argument_list"
            | "list"
            | "tuple"
            | "dictionary"
            | "set"
            | "if_statement"
            | "elif_clause"
            | "while_statement"
            | "for_statement"
            | "subscript"
            | "attribute"
            | "conditional_expression" => true,
            "expression_statement" => false,
            _ => Self::is_expression_context(parent),
        }
    }

    /// Check if an expression has potential side effects
    ///
    /// An expression has side effects if it contains:
    /// - Function calls
    /// - Attribute access (might invoke __getattr__)
    /// - Subscript operations (might invoke __getitem__)
    fn has_side_effects(expr: &str) -> bool {
        let mut parser = tree_sitter::Parser::new();
        if parser.set_language(&tree_sitter_python::LANGUAGE.into()).is_err() {
            return true;
        }

        let Some(tree) = parser.parse(expr, None) else {
            return true;
        };

        Self::node_has_side_effects(tree.root_node())
    }

    /// Check if a tree-sitter node contains side-effecting operations
    fn node_has_side_effects(node: tree_sitter::Node) -> bool {
        match node.kind() {
            "call" | "attribute" | "subscript" => true,
            "identifier" | "integer" | "float" | "string" | "true" | "false" | "none" => false,
            _ => {
                let mut cursor = node.walk();
                for child in node.children(&mut cursor) {
                    if Self::node_has_side_effects(child) {
                        return true;
                    }
                }
                false
            }
        }
    }

    /// Inline the function body with parameter substitution and conflict resolution
    ///
    /// Handles both expression and statement contexts, with proper side effect management.
    fn inline_function_body(
        func_info: &FunctionInfo, arguments: &[String], call_text: &str, call_node: tree_sitter::Node,
        is_expr_context: bool, indent_pos: Position,
    ) -> Option<InlineResult> {
        let args_with_side_effects: Vec<bool> = arguments.iter().map(|arg| Self::has_side_effects(arg)).collect();
        let has_any_side_effects = args_with_side_effects.iter().any(|&x| x);

        let is_complex_body = Self::is_complex_body(&func_info.body);

        let mut substitutions = HashMap::new();
        let mut prelude_lines = Vec::new();

        if has_any_side_effects {
            let call_scope_vars = Self::collect_identifiers(call_text);
            let mut used_temp_names = call_scope_vars.clone();

            for (i, (param, arg)) in func_info.parameters.iter().zip(arguments.iter()).enumerate() {
                if args_with_side_effects[i] {
                    let temp_name =
                        Self::generate_unique_name(&format!("_arg_{}", i), &used_temp_names, &HashSet::new());
                    used_temp_names.insert(temp_name.clone());
                    prelude_lines.push(format!("{} = {}", temp_name, arg));
                    substitutions.insert(param.clone(), temp_name);
                } else {
                    substitutions.insert(param.clone(), arg.clone());
                }
            }
        } else {
            for (param, arg) in func_info.parameters.iter().zip(arguments.iter()) {
                substitutions.insert(param.clone(), arg.clone());
            }
        }

        let body_vars = Self::collect_identifiers(&func_info.body);
        let call_scope_vars = Self::collect_identifiers(call_text);

        let mut inlined = func_info.body.clone();
        let mut renamed_vars = HashMap::new();

        for var in &body_vars {
            if !substitutions.contains_key(var) && call_scope_vars.contains(var) {
                let new_name = Self::generate_unique_name(var, &call_scope_vars, &body_vars);
                renamed_vars.insert(var.clone(), new_name);
            }
        }

        for (old_name, new_name) in &renamed_vars {
            inlined = Self::substitute_parameter(&inlined, old_name, new_name);
        }

        for (param, arg) in &substitutions {
            inlined = Self::substitute_parameter(&inlined, param, arg);
        }

        inlined = match &func_info.return_type {
            ReturnType::None => Self::remove_return_statements(&inlined),
            ReturnType::Single(_) if !is_expr_context || !is_complex_body => Self::transform_single_return(&inlined),
            ReturnType::Single(_) => Self::transform_multiple_returns(&inlined),
            ReturnType::Multiple(_) => Self::transform_multiple_returns(&inlined),
        };

        if is_expr_context && is_complex_body {
            let lines: Vec<&str> = inlined.lines().collect();
            if lines.len() > 1 {
                let setup_lines = &lines[..lines.len() - 1];
                let final_expr = lines.last().unwrap_or(&"");

                for line in setup_lines {
                    prelude_lines.push(line.to_string());
                }

                let replacement = final_expr.trim().to_string();

                if !prelude_lines.is_empty() {
                    let insertion_point = Self::find_statement_insertion_point(call_node, call_text);
                    let base_indent = insertion_point.start.character as usize;
                    let prelude = prelude_lines
                        .iter()
                        .map(|line| format!("{}{}\n", " ".repeat(base_indent), line))
                        .collect::<String>();

                    return Some(InlineResult::WithPrelude { prelude, replacement, insertion_point });
                } else {
                    return Some(InlineResult::Simple { replacement });
                }
            }
        }

        let indented = Self::indent_to_position(&inlined, indent_pos.character as usize);
        let replacement = indented.trim_end().to_string();

        if !prelude_lines.is_empty() {
            let insertion_point = Self::find_statement_insertion_point(call_node, call_text);
            let base_indent = insertion_point.start.character as usize;
            let prelude = prelude_lines
                .iter()
                .map(|line| format!("{}{}\n", " ".repeat(base_indent), line))
                .collect::<String>();

            Some(InlineResult::WithPrelude { prelude, replacement, insertion_point })
        } else {
            Some(InlineResult::Simple { replacement })
        }
    }

    /// Check if function body is complex (has multiple statements)
    ///
    /// A body is complex if it contains:
    /// - Multiple statements
    /// - Control flow (if/while/for)
    /// - Try/except blocks
    fn is_complex_body(body: &str) -> bool {
        let trimmed = body.trim();

        if !trimmed.contains('\n') {
            return false;
        }

        let mut parser = tree_sitter::Parser::new();
        if parser.set_language(&tree_sitter_python::LANGUAGE.into()).is_err() {
            return true;
        }

        let Some(tree) = parser.parse(body, None) else {
            return true;
        };

        let root = tree.root_node();

        if Self::has_control_flow(root) {
            return true;
        }

        Self::count_statements(root) > 1
    }

    /// Check if a node contains control flow structures
    fn has_control_flow(node: tree_sitter::Node) -> bool {
        match node.kind() {
            "if_statement" | "while_statement" | "for_statement" | "try_statement" | "with_statement" => true,
            _ => {
                let mut cursor = node.walk();
                for child in node.children(&mut cursor) {
                    if Self::has_control_flow(child) {
                        return true;
                    }
                }
                false
            }
        }
    }

    /// Count the number of statements in a node
    fn count_statements(node: tree_sitter::Node) -> usize {
        match node.kind() {
            "module" | "block" => {
                let mut count = 0;
                let mut cursor = node.walk();
                for child in node.children(&mut cursor) {
                    if !matches!(child.kind(), "comment" | "\n") {
                        count += 1;
                    }
                }
                count
            }
            _ => {
                if node.child_count() == 0 {
                    return 0;
                }
                let mut cursor = node.walk();
                let mut total = 0;
                for child in node.children(&mut cursor) {
                    total += Self::count_statements(child);
                }
                total.max(1)
            }
        }
    }

    /// Find the insertion point for prelude code before the call
    fn find_statement_insertion_point(call_node: tree_sitter::Node, text: &str) -> Range {
        let mut node = call_node;

        while let Some(parent) = node.parent() {
            if Self::is_statement_node(parent) {
                let start = utils::byte_offset_to_position(text, parent.start_byte());
                return Range { start, end: start };
            }
            node = parent;
        }

        let pos = utils::byte_offset_to_position(text, call_node.start_byte());
        Range { start: pos, end: pos }
    }

    /// Check if a node represents a statement
    fn is_statement_node(node: tree_sitter::Node) -> bool {
        matches!(
            node.kind(),
            "expression_statement"
                | "assignment"
                | "augmented_assignment"
                | "return_statement"
                | "if_statement"
                | "while_statement"
                | "for_statement"
                | "with_statement"
                | "try_statement"
                | "raise_statement"
                | "assert_statement"
                | "delete_statement"
                | "pass_statement"
                | "break_statement"
                | "continue_statement"
                | "import_statement"
                | "import_from_statement"
                | "global_statement"
                | "nonlocal_statement"
        )
    }

    /// Collect all identifier names used in code
    fn collect_identifiers(code: &str) -> HashSet<String> {
        let mut identifiers = HashSet::new();

        let mut parser = tree_sitter::Parser::new();
        if parser.set_language(&tree_sitter_python::LANGUAGE.into()).is_err() {
            return identifiers;
        }

        let Some(tree) = parser.parse(code, None) else {
            return identifiers;
        };

        Self::collect_identifiers_from_node(tree.root_node(), code, &mut identifiers);
        identifiers
    }

    /// Recursively collect identifiers from AST nodes
    fn collect_identifiers_from_node(node: tree_sitter::Node, text: &str, identifiers: &mut HashSet<String>) {
        if node.kind() == "identifier"
            && let Ok(name) = node.utf8_text(text.as_bytes())
        {
            identifiers.insert(name.to_string());
        }

        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            Self::collect_identifiers_from_node(child, text, identifiers);
        }
    }

    /// Generate a unique variable name to avoid conflicts
    fn generate_unique_name(base: &str, call_scope: &HashSet<String>, body_scope: &HashSet<String>) -> String {
        let mut counter = 1;
        loop {
            let candidate = format!("{}_{}", base, counter);
            if !call_scope.contains(&candidate) && !body_scope.contains(&candidate) {
                return candidate;
            }
            counter += 1;
        }
    }

    /// Substitute a parameter with its argument value using AST-based replacement
    ///
    /// Uses tree-sitter to parse the body and only substitute identifier nodes
    /// that match the parameter name, avoiding replacements in strings, comments, etc.
    fn substitute_parameter(body: &str, param: &str, arg: &str) -> String {
        if let Some(result) = Self::substitute_parameter_ast(body, param, arg) {
            return result;
        }

        let pattern = format!(r"\b{}\b", regex::escape(param));
        let re = regex::Regex::new(&pattern).unwrap();
        re.replace_all(body, arg).to_string()
    }

    /// AST-based parameter substitution using tree-sitter
    fn substitute_parameter_ast(body: &str, param: &str, arg: &str) -> Option<String> {
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&tree_sitter_python::LANGUAGE.into()).ok()?;

        let tree = parser.parse(body, None)?;
        let root = tree.root_node();

        let mut replacements = Vec::new();
        Self::collect_identifier_replacements(root, body, param, &mut replacements);

        if replacements.is_empty() {
            return Some(body.to_string());
        }

        replacements.sort_by_key(|r| std::cmp::Reverse(r.0));

        let mut result = body.to_string();
        for (start, end, _) in replacements {
            result.replace_range(start..end, arg);
        }

        Some(result)
    }

    /// Collect identifier nodes that should be replaced
    fn collect_identifier_replacements(
        node: tree_sitter::Node, text: &str, param: &str, replacements: &mut Vec<(usize, usize, String)>,
    ) {
        if matches!(node.kind(), "string" | "comment") {
            return;
        }

        if node.kind() == "identifier"
            && let Ok(node_text) = node.utf8_text(text.as_bytes())
            && node_text == param
        {
            replacements.push((node.start_byte(), node.end_byte(), param.to_string()));
        }

        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            Self::collect_identifier_replacements(child, text, param, replacements);
        }
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

        if Self::has_only_trailing_return(body) {
            return Self::replace_trailing_return(body);
        }

        Self::transform_multiple_returns(body)
    }

    /// Check if function body has only a single return statement at the end
    fn has_only_trailing_return(body: &str) -> bool {
        let mut parser = tree_sitter::Parser::new();
        if parser.set_language(&tree_sitter_python::LANGUAGE.into()).is_err() {
            return false;
        }

        let Some(tree) = parser.parse(body, None) else {
            return false;
        };

        let root = tree.root_node();
        let return_nodes = Self::count_return_statements(root, body);

        if return_nodes != 1 {
            return false;
        }

        let lines: Vec<&str> = body.lines().collect();
        for line in lines.iter().rev() {
            let trimmed = line.trim();
            if trimmed.is_empty() {
                continue;
            }
            return trimmed.starts_with("return ");
        }

        false
    }

    /// Count return statements in AST
    fn count_return_statements(node: tree_sitter::Node, _text: &str) -> usize {
        let mut count = 0;

        if node.kind() == "return_statement" {
            count += 1;
        }

        if node.kind() == "function_definition" {
            return count;
        }

        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            count += Self::count_return_statements(child, _text);
        }

        count
    }

    /// Replace trailing return statement with its value
    fn replace_trailing_return(body: &str) -> String {
        let lines: Vec<&str> = body.lines().collect();
        let mut result = Vec::new();

        for (i, line) in lines.iter().enumerate() {
            let trimmed = line.trim_start();
            if i == lines.len() - 1 && trimmed.starts_with("return ") {
                if let Some(value) = trimmed.strip_prefix("return ") {
                    let indent = &line[..line.len() - trimmed.len()];
                    result.push(format!("{}{}", indent, value));
                }
            } else {
                result.push(line.to_string());
            }
        }

        result.join("\n")
    }

    /// Transform return statements for multiple return values using temporary variables
    fn transform_multiple_returns(body: &str) -> String {
        let mut parser = tree_sitter::Parser::new();
        if parser.set_language(&tree_sitter_python::LANGUAGE.into()).is_err() {
            return body.to_string();
        }

        let Some(tree) = parser.parse(body, None) else {
            return body.to_string();
        };

        let root = tree.root_node();

        let mut return_replacements = Vec::new();
        Self::collect_return_replacements(root, body, &mut return_replacements);

        if return_replacements.is_empty() {
            return body.to_string();
        }

        return_replacements.sort_by_key(|r| std::cmp::Reverse(r.0));

        let mut result = body.to_string();
        for (start, end, value) in return_replacements {
            if value.is_empty() {
                result.replace_range(start..end, "_inline_result = None")
            } else {
                result.replace_range(start..end, &format!("_inline_result = {}", value))
            }
        }

        format!("{}\n_inline_result", result)
    }

    /// Collect return statement replacements (start_byte, end_byte, return_value)
    fn collect_return_replacements(
        node: tree_sitter::Node, text: &str, replacements: &mut Vec<(usize, usize, String)>,
    ) {
        if node.kind() == "return_statement" {
            let mut return_value = String::new();
            let mut cursor = node.walk();

            for child in node.children(&mut cursor) {
                if child.kind() != "return"
                    && let Ok(value) = child.utf8_text(text.as_bytes())
                {
                    return_value = value.to_string();
                }
            }

            replacements.push((node.start_byte(), node.end_byte(), return_value));
            return;
        }

        if node.kind() == "function_definition" {
            return;
        }

        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            Self::collect_return_replacements(child, text, replacements);
        }
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
#[derive(Debug)]
enum ReturnType {
    None,
    #[allow(dead_code)]
    Single(String),
    #[allow(dead_code)]
    Multiple(Vec<String>),
}

/// A call site to be inlined
struct CallSite {
    uri: Url,
    range: Range,
}

/// Result of inlining a function
enum InlineResult {
    /// Simple replacement without additional code
    Simple { replacement: String },
    /// Replacement requiring prelude code (for side effects or complex bodies)
    WithPrelude {
        prelude: String,
        replacement: String,
        insertion_point: Range,
    },
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

    #[test]
    fn test_substitute_parameter_avoids_strings() {
        let body = "msg = \"x is the value\"\nreturn x + 1";
        let result = InlineFunctionProvider::substitute_parameter(body, "x", "value");
        assert!(result.contains("\"x is the value\""));
        assert!(result.contains("value + 1"));
    }

    #[test]
    fn test_substitute_parameter_avoids_comments() {
        let body = "# x is the parameter\nreturn x + 1";
        let result = InlineFunctionProvider::substitute_parameter(body, "x", "value");
        assert!(result.contains("# x is the parameter"));
        assert!(result.contains("value + 1"));
    }

    #[test]
    fn test_has_only_trailing_return() {
        let body = "x = a + b\nreturn x";
        assert!(InlineFunctionProvider::has_only_trailing_return(body));

        let body = "if a:\n    return 1\nreturn 2";
        assert!(!InlineFunctionProvider::has_only_trailing_return(body));

        let body = "print('hello')";
        assert!(!InlineFunctionProvider::has_only_trailing_return(body));
    }

    #[test]
    fn test_replace_trailing_return() {
        let body = "x = a + b\ny = x * 2\nreturn y";
        let result = InlineFunctionProvider::replace_trailing_return(body);
        assert_eq!(result, "x = a + b\ny = x * 2\ny");
    }

    #[test]
    fn test_replace_trailing_return_with_indentation() {
        let body = "    x = a + b\n    return x";
        let result = InlineFunctionProvider::replace_trailing_return(body);
        assert_eq!(result, "    x = a + b\n    x");
    }

    #[test]
    fn test_collect_identifiers() {
        let code = "x = 1\ny = x + 2\nz = foo(y)";
        let identifiers = InlineFunctionProvider::collect_identifiers(code);
        assert!(identifiers.contains("x"));
        assert!(identifiers.contains("y"));
        assert!(identifiers.contains("z"));
        assert!(identifiers.contains("foo"));
    }

    #[test]
    fn test_generate_unique_name() {
        let call_scope = ["x", "x_1", "y"].iter().map(|s| s.to_string()).collect();
        let body_scope = ["z"].iter().map(|s| s.to_string()).collect();
        let result = InlineFunctionProvider::generate_unique_name("x", &call_scope, &body_scope);
        assert_eq!(result, "x_2");
    }

    #[test]
    fn test_transform_multiple_returns_with_early_return() {
        let body = "if condition:\n    return a\nelse:\n    return b";
        let result = InlineFunctionProvider::transform_multiple_returns(body);
        assert!(result.contains("_inline_result = a"));
        assert!(result.contains("_inline_result = b"));
        assert!(result.ends_with("_inline_result"));
    }

    #[test]
    fn test_transform_multiple_returns_complex_control_flow() {
        let body = "if x > 0:\n    return x\nelif x < 0:\n    return -x\nelse:\n    return 0";
        let result = InlineFunctionProvider::transform_multiple_returns(body);
        assert!(result.contains("_inline_result"));
        assert!(result.ends_with("_inline_result"));
    }

    #[test]
    fn test_transform_single_return_with_early_returns() {
        let body = "if error:\n    return None\nreturn result";
        let result = InlineFunctionProvider::transform_single_return(body);
        assert!(result.contains("_inline_result"));
    }

    #[test]
    fn test_collect_return_replacements() {
        let body = "if x:\n    return 1\nreturn 2";
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&tree_sitter_python::LANGUAGE.into()).unwrap();
        let tree = parser.parse(body, None).unwrap();

        let mut replacements = Vec::new();
        InlineFunctionProvider::collect_return_replacements(tree.root_node(), body, &mut replacements);
        assert_eq!(replacements.len(), 2);
    }

    #[test]
    fn test_collect_return_replacements_ignores_nested_functions() {
        let body = "def inner():\n    return 1\nreturn 2";
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&tree_sitter_python::LANGUAGE.into()).unwrap();
        let tree = parser.parse(body, None).unwrap();

        let mut replacements = Vec::new();
        InlineFunctionProvider::collect_return_replacements(tree.root_node(), body, &mut replacements);
        assert_eq!(replacements.len(), 1);
    }

    #[test]
    fn test_variable_conflict_resolution() {
        let body = "temp = x + 1\nreturn temp";
        let call_text = "temp = 5\nresult = foo(10)";

        let body_vars = InlineFunctionProvider::collect_identifiers(body);
        let call_vars = InlineFunctionProvider::collect_identifiers(call_text);

        assert!(body_vars.contains("temp"));
        assert!(call_vars.contains("temp"));

        let unique = InlineFunctionProvider::generate_unique_name("temp", &call_vars, &body_vars);
        assert_ne!(unique, "temp");
        assert!(!call_vars.contains(&unique));
    }

    #[test]
    fn test_transform_multiple_returns_with_empty_return() {
        let body = "if error:\n    return\nreturn result";
        let result = InlineFunctionProvider::transform_multiple_returns(body);
        assert!(result.contains("_inline_result = None"));
        assert!(result.contains("_inline_result = result"));
    }

    #[test]
    fn test_is_expression_context_assignment() {
        let code = "result = foo(x)";
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&tree_sitter_python::LANGUAGE.into()).unwrap();
        let tree = parser.parse(code, None).unwrap();

        let call_node = find_call_in_tree(tree.root_node());
        assert!(call_node.is_some());
        assert!(InlineFunctionProvider::is_expression_context(call_node.unwrap()));
    }

    #[test]
    fn test_is_expression_context_return() {
        let code = "return foo(x)";
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&tree_sitter_python::LANGUAGE.into()).unwrap();
        let tree = parser.parse(code, None).unwrap();

        let call_node = find_call_in_tree(tree.root_node());
        assert!(call_node.is_some());
        assert!(InlineFunctionProvider::is_expression_context(call_node.unwrap()));
    }

    #[test]
    fn test_is_expression_context_statement() {
        let code = "foo(x)";
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&tree_sitter_python::LANGUAGE.into()).unwrap();
        let tree = parser.parse(code, None).unwrap();

        let call_node = find_call_in_tree(tree.root_node());
        assert!(call_node.is_some());
        assert!(!InlineFunctionProvider::is_expression_context(call_node.unwrap()));
    }

    #[test]
    fn test_is_expression_context_binary_op() {
        let code = "x = foo(a) + bar(b)";
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&tree_sitter_python::LANGUAGE.into()).unwrap();
        let tree = parser.parse(code, None).unwrap();

        let call_node = find_call_in_tree(tree.root_node());
        assert!(call_node.is_some());
        assert!(InlineFunctionProvider::is_expression_context(call_node.unwrap()));
    }

    #[test]
    fn test_has_side_effects_simple() {
        assert!(!InlineFunctionProvider::has_side_effects("42"));
        assert!(!InlineFunctionProvider::has_side_effects("x"));
        assert!(!InlineFunctionProvider::has_side_effects("\"string\""));
        assert!(!InlineFunctionProvider::has_side_effects("True"));
    }

    #[test]
    fn test_has_side_effects_function_call() {
        assert!(InlineFunctionProvider::has_side_effects("foo()"));
        assert!(InlineFunctionProvider::has_side_effects("bar(x)"));
        assert!(InlineFunctionProvider::has_side_effects("get_value()"));
    }

    #[test]
    fn test_has_side_effects_attribute_access() {
        assert!(InlineFunctionProvider::has_side_effects("obj.attr"));
        assert!(InlineFunctionProvider::has_side_effects("x.y.z"));
    }

    #[test]
    fn test_has_side_effects_subscript() {
        assert!(InlineFunctionProvider::has_side_effects("arr[0]"));
        assert!(InlineFunctionProvider::has_side_effects("dict[key]"));
    }

    #[test]
    fn test_is_complex_body_simple() {
        let body = "return x + y";
        assert!(!InlineFunctionProvider::is_complex_body(body));
    }

    #[test]
    fn test_is_complex_body_multiple_statements() {
        let body = "x = a + b\nreturn x";
        assert!(InlineFunctionProvider::is_complex_body(body));
    }

    #[test]
    fn test_is_complex_body_with_control_flow() {
        let body = "if condition:\n    return a\nelse:\n    return b";
        assert!(InlineFunctionProvider::is_complex_body(body));
    }

    #[test]
    fn test_count_statements() {
        let code = "x = 1\ny = 2\nreturn x + y";
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&tree_sitter_python::LANGUAGE.into()).unwrap();
        let tree = parser.parse(code, None).unwrap();

        let count = InlineFunctionProvider::count_statements(tree.root_node());
        assert_eq!(count, 3);
    }

    #[test]
    fn test_count_statements_single() {
        let code = "return 42";
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&tree_sitter_python::LANGUAGE.into()).unwrap();
        let tree = parser.parse(code, None).unwrap();

        let count = InlineFunctionProvider::count_statements(tree.root_node());
        assert_eq!(count, 1);
    }

    #[test]
    fn test_is_statement_node() {
        let code = "x = 1\ny = 2";
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&tree_sitter_python::LANGUAGE.into()).unwrap();
        let tree = parser.parse(code, None).unwrap();

        let mut found_assignment = false;
        walk_tree(tree.root_node(), &mut |node| {
            if node.kind() == "expression_statement" {
                found_assignment = true;
                assert!(InlineFunctionProvider::is_statement_node(node));
            }
        });
        assert!(found_assignment);
    }

    fn find_call_in_tree(node: tree_sitter::Node) -> Option<tree_sitter::Node> {
        if node.kind() == "call" {
            return Some(node);
        }

        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if let Some(call) = find_call_in_tree(child) {
                return Some(call);
            }
        }

        None
    }

    fn walk_tree<F>(node: tree_sitter::Node, callback: &mut F)
    where
        F: FnMut(tree_sitter::Node),
    {
        callback(node);
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            walk_tree(child, callback);
        }
    }
}

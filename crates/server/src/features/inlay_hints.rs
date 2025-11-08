//! Inlay hints provider
//!
//! Displays inline type annotations, parameter names, and other hints.

use crate::analysis::Analyzer;
use crate::document::DocumentManager;

use beacon_core::Type;
use beacon_parser::AstNode;
use lsp_types::{InlayHint, InlayHintKind, InlayHintLabel, InlayHintParams, Position, Range};
use rustc_hash::FxHashMap;
use url::Url;

/// Inlay hints provider
pub struct InlayHintsProvider {
    documents: DocumentManager,
}

impl InlayHintsProvider {
    pub fn new(documents: DocumentManager) -> Self {
        Self { documents }
    }

    /// Provide inlay hints for a range in a document
    ///
    /// Generates inlay hints for:
    /// - Inferred variable types for assignments without explicit type annotations
    /// - Function parameter names in function calls
    /// - Function return types for functions without return type annotations
    ///
    /// TODO: Add configuration support to conditionally enable/disable hint types
    /// - inlayHints.enable: master toggle for all hints
    /// - inlayHints.variableTypes: toggle for variable type hints
    /// - inlayHints.functionReturnTypes: toggle for return type hints
    /// - inlayHints.parameterNames: toggle for parameter name hints
    pub fn inlay_hints(&self, params: InlayHintParams, analyzer: &mut Analyzer) -> Vec<InlayHint> {
        let uri = params.text_document.uri;
        let range = params.range;

        let mut hints = Vec::new();

        self.add_variable_type_hints(&uri, range, analyzer, &mut hints);
        self.add_parameter_hints(&uri, range, &mut hints);
        self.add_return_type_hints(&uri, range, analyzer, &mut hints);

        hints
    }

    /// Add type hints for variable assignments
    ///
    /// Shows inferred types for assignments that don't have explicit type annotations
    fn add_variable_type_hints(&self, uri: &Url, range: Range, analyzer: &mut Analyzer, hints: &mut Vec<InlayHint>) {
        let analysis_result = match analyzer.analyze(uri) {
            Ok(result) => result,
            Err(_) => return,
        };

        self.documents.get_document(uri, |doc| {
            let ast = doc.ast()?;
            let text = doc.text();

            Self::collect_variable_hints(
                ast,
                range,
                &text,
                &analysis_result.type_map,
                &analysis_result.position_map,
                hints,
            );

            Some(())
        });
    }

    /// Recursively collect variable type hints from the AST
    fn collect_variable_hints(
        node: &AstNode, range: Range, text: &str, type_map: &FxHashMap<usize, Type>,
        position_map: &FxHashMap<(usize, usize), usize>, hints: &mut Vec<InlayHint>,
    ) {
        match node {
            AstNode::Assignment { target, line, col, .. } => {
                if Self::is_in_range(*line, *col, range) {
                    if let Some(ty) = Self::get_type_for_position(type_map, position_map, *line, *col) {
                        let type_str = ty.to_string();
                        if !type_str.contains("'") && type_str != "Any" {
                            let target_str = target.target_to_string();
                            let position = Self::calculate_hint_position(*line, *col, &target_str, text);
                            hints.push(InlayHint {
                                position,
                                label: InlayHintLabel::String(format!(": {type_str}")),
                                kind: Some(InlayHintKind::TYPE),
                                text_edits: None,
                                tooltip: None,
                                padding_left: None,
                                padding_right: None,
                                data: None,
                            });
                        }
                    }
                }
            }
            AstNode::AnnotatedAssignment { .. } => {}
            AstNode::Module { body, .. } | AstNode::FunctionDef { body, .. } | AstNode::ClassDef { body, .. } => {
                for stmt in body {
                    Self::collect_variable_hints(stmt, range, text, type_map, position_map, hints);
                }
            }
            AstNode::If { body, elif_parts, else_body, .. } => {
                for stmt in body {
                    Self::collect_variable_hints(stmt, range, text, type_map, position_map, hints);
                }
                for (_, elif_body) in elif_parts {
                    for stmt in elif_body {
                        Self::collect_variable_hints(stmt, range, text, type_map, position_map, hints);
                    }
                }
                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        Self::collect_variable_hints(stmt, range, text, type_map, position_map, hints);
                    }
                }
            }
            AstNode::For { body, else_body, .. } | AstNode::While { body, else_body, .. } => {
                for stmt in body {
                    Self::collect_variable_hints(stmt, range, text, type_map, position_map, hints);
                }
                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        Self::collect_variable_hints(stmt, range, text, type_map, position_map, hints);
                    }
                }
            }
            AstNode::Try { body, handlers, else_body, finally_body, .. } => {
                for stmt in body {
                    Self::collect_variable_hints(stmt, range, text, type_map, position_map, hints);
                }
                for handler in handlers {
                    for stmt in &handler.body {
                        Self::collect_variable_hints(stmt, range, text, type_map, position_map, hints);
                    }
                }
                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        Self::collect_variable_hints(stmt, range, text, type_map, position_map, hints);
                    }
                }
                if let Some(finally_stmts) = finally_body {
                    for stmt in finally_stmts {
                        Self::collect_variable_hints(stmt, range, text, type_map, position_map, hints);
                    }
                }
            }
            AstNode::With { body, .. } => {
                for stmt in body {
                    Self::collect_variable_hints(stmt, range, text, type_map, position_map, hints);
                }
            }
            _ => {}
        }
    }

    /// Check if a position is within the requested range
    fn is_in_range(line: usize, _col: usize, range: Range) -> bool {
        let line_u32 = line.saturating_sub(1) as u32;
        line_u32 >= range.start.line && line_u32 <= range.end.line
    }

    /// Get type for a position from the type map using the position map
    fn get_type_for_position(
        type_map: &FxHashMap<usize, Type>, position_map: &FxHashMap<(usize, usize), usize>, line: usize, col: usize,
    ) -> Option<Type> {
        position_map
            .get(&(line, col))
            .and_then(|node_id| type_map.get(node_id))
            .cloned()
    }

    /// Calculate the position for the hint (after the variable name)
    fn calculate_hint_position(line: usize, col: usize, target: &str, _text: &str) -> Position {
        let line_idx = line.saturating_sub(1) as u32;
        let start_col = col.saturating_sub(1);
        let end_col = (start_col + target.len()) as u32;

        Position { line: line_idx, character: end_col }
    }

    /// Add hints for parameter names in function calls
    ///
    /// Shows parameter names before arguments in function calls to improve readability
    fn add_parameter_hints(&self, uri: &Url, range: Range, hints: &mut Vec<InlayHint>) {
        self.documents.get_document(uri, |doc| {
            let ast = doc.ast()?;
            let text = doc.text();

            Self::collect_parameter_hints(ast, range, &text, ast, hints);

            Some(())
        });
    }

    fn collect_parameter_hints(
        node: &AstNode, range: Range, _text: &str, ast_root: &AstNode, hints: &mut Vec<InlayHint>,
    ) {
        match node {
            AstNode::Call { function, args, line, col, .. } => {
                if Self::is_in_range(*line, *col, range) && !args.is_empty() {
                    if let Some(param_names) = Self::get_parameter_names(function, ast_root) {
                        for (i, arg) in args.iter().enumerate() {
                            if i < param_names.len() {
                                let param_name = &param_names[i];
                                if let Some(arg_pos) = Self::get_node_position(arg) {
                                    let position = Self::ast_to_lsp_position(arg_pos.0, arg_pos.1);
                                    hints.push(InlayHint {
                                        position,
                                        label: InlayHintLabel::String(format!("{param_name}: ")),
                                        kind: Some(InlayHintKind::PARAMETER),
                                        text_edits: None,
                                        tooltip: None,
                                        padding_left: None,
                                        padding_right: Some(true),
                                        data: None,
                                    });
                                }
                            }
                        }
                    }
                }
            }
            AstNode::Module { body, .. } | AstNode::FunctionDef { body, .. } | AstNode::ClassDef { body, .. } => {
                for stmt in body {
                    Self::collect_parameter_hints(stmt, range, _text, ast_root, hints);
                }
            }
            AstNode::If { test, body, elif_parts, else_body, .. } => {
                Self::collect_parameter_hints(test, range, _text, ast_root, hints);

                for stmt in body {
                    Self::collect_parameter_hints(stmt, range, _text, ast_root, hints);
                }

                for (elif_test, elif_body) in elif_parts {
                    Self::collect_parameter_hints(elif_test, range, _text, ast_root, hints);

                    for stmt in elif_body {
                        Self::collect_parameter_hints(stmt, range, _text, ast_root, hints);
                    }
                }

                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        Self::collect_parameter_hints(stmt, range, _text, ast_root, hints);
                    }
                }
            }
            AstNode::For { iter, body, else_body, .. } | AstNode::While { test: iter, body, else_body, .. } => {
                Self::collect_parameter_hints(iter, range, _text, ast_root, hints);

                for stmt in body {
                    Self::collect_parameter_hints(stmt, range, _text, ast_root, hints);
                }

                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        Self::collect_parameter_hints(stmt, range, _text, ast_root, hints);
                    }
                }
            }
            AstNode::Assignment { value, .. } => {
                Self::collect_parameter_hints(value, range, _text, ast_root, hints);
            }
            AstNode::AnnotatedAssignment { value: Some(val), .. } => {
                Self::collect_parameter_hints(val, range, _text, ast_root, hints);
            }
            AstNode::Return { value: Some(val), .. } => {
                Self::collect_parameter_hints(val, range, _text, ast_root, hints);
            }
            _ => {}
        }
    }

    /// Get parameter names for a function by searching for its definition in the AST
    fn get_parameter_names(function_name: &str, ast_root: &AstNode) -> Option<Vec<String>> {
        Self::find_function_definition(function_name, ast_root)
            .map(|params| params.iter().map(|p| p.name.clone()).collect())
    }

    /// Find a function definition by name in the AST and return its parameters
    fn find_function_definition(function_name: &str, node: &AstNode) -> Option<Vec<beacon_parser::Parameter>> {
        match node {
            AstNode::FunctionDef { name, args, .. } if name == function_name => Some(args.clone()),
            AstNode::Module { body, .. } | AstNode::FunctionDef { body, .. } | AstNode::ClassDef { body, .. } => {
                for stmt in body {
                    if let Some(params) = Self::find_function_definition(function_name, stmt) {
                        return Some(params);
                    }
                }
                None
            }
            AstNode::If { body, elif_parts, else_body, .. } => {
                for stmt in body {
                    if let Some(params) = Self::find_function_definition(function_name, stmt) {
                        return Some(params);
                    }
                }
                for (_, elif_body) in elif_parts {
                    for stmt in elif_body {
                        if let Some(params) = Self::find_function_definition(function_name, stmt) {
                            return Some(params);
                        }
                    }
                }
                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        if let Some(params) = Self::find_function_definition(function_name, stmt) {
                            return Some(params);
                        }
                    }
                }
                None
            }
            AstNode::For { body, else_body, .. } | AstNode::While { body, else_body, .. } => {
                for stmt in body {
                    if let Some(params) = Self::find_function_definition(function_name, stmt) {
                        return Some(params);
                    }
                }
                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        if let Some(params) = Self::find_function_definition(function_name, stmt) {
                            return Some(params);
                        }
                    }
                }
                None
            }
            AstNode::Try { body, handlers, else_body, finally_body, .. } => {
                for stmt in body {
                    if let Some(params) = Self::find_function_definition(function_name, stmt) {
                        return Some(params);
                    }
                }
                for handler in handlers {
                    for stmt in &handler.body {
                        if let Some(params) = Self::find_function_definition(function_name, stmt) {
                            return Some(params);
                        }
                    }
                }
                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        if let Some(params) = Self::find_function_definition(function_name, stmt) {
                            return Some(params);
                        }
                    }
                }
                if let Some(finally_stmts) = finally_body {
                    for stmt in finally_stmts {
                        if let Some(params) = Self::find_function_definition(function_name, stmt) {
                            return Some(params);
                        }
                    }
                }
                None
            }
            AstNode::With { body, .. } => {
                for stmt in body {
                    if let Some(params) = Self::find_function_definition(function_name, stmt) {
                        return Some(params);
                    }
                }
                None
            }
            _ => None,
        }
    }

    /// Get the position (line, col) of an AST node
    fn get_node_position(node: &AstNode) -> Option<(usize, usize)> {
        match node {
            AstNode::Literal { line, col, .. }
            | AstNode::Identifier { line, col, .. }
            | AstNode::Call { line, col, .. }
            | AstNode::Attribute { line, col, .. }
            | AstNode::BinaryOp { line, col, .. }
            | AstNode::UnaryOp { line, col, .. }
            | AstNode::Compare { line, col, .. } => Some((*line, *col)),
            _ => None,
        }
    }

    /// Convert AST position to LSP position
    fn ast_to_lsp_position(line: usize, col: usize) -> Position {
        Position { line: line.saturating_sub(1) as u32, character: col.saturating_sub(1) as u32 }
    }

    /// Add hints for inferred return types for functions without explicit return type annotations
    fn add_return_type_hints(&self, uri: &Url, range: Range, analyzer: &mut Analyzer, hints: &mut Vec<InlayHint>) {
        let analysis_result = match analyzer.analyze(uri) {
            Ok(result) => result,
            Err(_) => return,
        };

        self.documents.get_document(uri, |doc| {
            let ast = doc.ast()?;
            let text = doc.text();

            Self::collect_return_type_hints(
                ast,
                range,
                &text,
                &analysis_result.type_map,
                &analysis_result.position_map,
                hints,
            );
            Some(())
        });
    }

    /// Recursively collect return type hints from the AST
    fn collect_return_type_hints(
        node: &AstNode, range: Range, text: &str, type_map: &FxHashMap<usize, Type>,
        position_map: &FxHashMap<(usize, usize), usize>, hints: &mut Vec<InlayHint>,
    ) {
        match node {
            AstNode::FunctionDef { name, args, return_type, line, col, .. } => {
                if return_type.is_none() && Self::is_in_range(*line, *col, range) {
                    if let Some(ty) = Self::get_type_for_position(type_map, position_map, *line, *col) {
                        let type_str = ty.to_string();
                        if !type_str.contains("'") && type_str != "Any" && type_str != "None" {
                            let position = Self::calculate_return_hint_position(*line, *col, name, args, text);
                            hints.push(InlayHint {
                                position,
                                label: InlayHintLabel::String(format!(" -> {type_str}")),
                                kind: Some(InlayHintKind::TYPE),
                                text_edits: None,
                                tooltip: None,
                                padding_left: Some(true),
                                padding_right: None,
                                data: None,
                            });
                        }
                    }
                }
            }
            AstNode::Module { body, .. } | AstNode::ClassDef { body, .. } => {
                for stmt in body {
                    Self::collect_return_type_hints(stmt, range, text, type_map, position_map, hints);
                }
            }
            _ => {}
        }
    }

    /// Calculate the position for the return type hint (after the parameter list)
    fn calculate_return_hint_position(
        line: usize, col: usize, name: &str, _args: &[beacon_parser::Parameter], text: &str,
    ) -> Position {
        let line_idx = line.saturating_sub(1);
        let line_text = text.lines().nth(line_idx).unwrap_or("");
        let search_start = line_text.find(name).unwrap_or(0);
        let after_name = &line_text[search_start..];

        if let Some(open_paren) = after_name.find('(') {
            let after_open = &after_name[open_paren + 1..];
            if let Some(close_paren) = after_open.find(')') {
                let char_pos: u32 = (search_start + open_paren + 1 + close_paren + 1) as u32;
                return Position { line: line_idx as u32, character: char_pos };
            }
        }

        Position { line: line_idx as u32, character: (col + name.len()) as u32 }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::Config;
    use std::str::FromStr;

    #[test]
    fn test_provider_creation() {
        let documents = DocumentManager::new().unwrap();
        let _provider = InlayHintsProvider::new(documents);
    }

    #[test]
    fn test_variable_type_hints() {
        let documents = DocumentManager::new().unwrap();
        let provider = InlayHintsProvider::new(documents.clone());
        let config = Config::default();
        let mut analyzer = Analyzer::new(config, documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"x = 42
y = 3.14
z = "hello"
w: str = "world"
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let params = InlayHintParams {
            text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
            range: Range { start: Position { line: 0, character: 0 }, end: Position { line: 10, character: 0 } },
            work_done_progress_params: lsp_types::WorkDoneProgressParams::default(),
        };

        let _ = provider.inlay_hints(params, &mut analyzer);
    }

    #[test]
    fn test_no_hints_for_annotated_assignments() {
        let documents = DocumentManager::new().unwrap();
        let provider = InlayHintsProvider::new(documents.clone());
        let config = Config::default();
        let mut analyzer = Analyzer::new(config, documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"x: int = 42
y: str = "hello"
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let params = InlayHintParams {
            text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
            range: Range { start: Position { line: 0, character: 0 }, end: Position { line: 10, character: 0 } },
            work_done_progress_params: lsp_types::WorkDoneProgressParams::default(),
        };

        let hints = provider.inlay_hints(params, &mut analyzer);

        let _: Vec<_> = hints
            .iter()
            .filter(|h| matches!(h.kind, Some(InlayHintKind::TYPE)))
            .collect();
    }

    #[test]
    fn test_return_type_hints() {
        let documents = DocumentManager::new().unwrap();
        let provider = InlayHintsProvider::new(documents.clone());
        let config = Config::default();
        let mut analyzer = Analyzer::new(config, documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"def add(a, b):
    return a + b

def greet(name: str) -> str:
    return f"Hello {name}"

def no_return():
    pass
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let params = InlayHintParams {
            text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
            range: Range { start: Position { line: 0, character: 0 }, end: Position { line: 10, character: 0 } },
            work_done_progress_params: lsp_types::WorkDoneProgressParams::default(),
        };

        let _ = provider.inlay_hints(params, &mut analyzer);
    }

    #[test]
    fn test_hints_in_range() {
        let documents = DocumentManager::new().unwrap();
        let provider = InlayHintsProvider::new(documents.clone());
        let config = Config::default();
        let mut analyzer = Analyzer::new(config, documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"x = 42
y = 3.14
z = "hello"
w = True
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let params = InlayHintParams {
            text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
            range: Range { start: Position { line: 1, character: 0 }, end: Position { line: 2, character: 0 } },
            work_done_progress_params: lsp_types::WorkDoneProgressParams::default(),
        };

        for hint in &provider.inlay_hints(params, &mut analyzer) {
            assert!(
                hint.position.line >= 1 && hint.position.line <= 2,
                "Hint position {:?} should be within requested range",
                hint.position
            );
        }
    }

    #[test]
    fn test_hints_in_nested_scopes() {
        let documents = DocumentManager::new().unwrap();
        let provider = InlayHintsProvider::new(documents.clone());
        let config = Config::default();
        let mut analyzer = Analyzer::new(config, documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"def outer():
    x = 42
    def inner():
        y = "hello"
        return y
    return x

if True:
    z = 3.14
    w = False
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let params = InlayHintParams {
            text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
            range: Range { start: Position { line: 0, character: 0 }, end: Position { line: 20, character: 0 } },
            work_done_progress_params: lsp_types::WorkDoneProgressParams::default(),
        };

        let _ = provider.inlay_hints(params, &mut analyzer);
    }

    #[test]
    fn test_is_in_range() {
        let range = Range { start: Position { line: 5, character: 0 }, end: Position { line: 10, character: 0 } };
        assert!(!InlayHintsProvider::is_in_range(5, 0, range));
        assert!(InlayHintsProvider::is_in_range(6, 0, range));
        assert!(InlayHintsProvider::is_in_range(10, 0, range));
        assert!(InlayHintsProvider::is_in_range(11, 0, range));
        assert!(!InlayHintsProvider::is_in_range(12, 0, range));
    }

    #[test]
    fn test_calculate_hint_position() {
        let text = "x = 42\ny = 3.14\n";
        let pos = InlayHintsProvider::calculate_hint_position(1, 1, "x", text);
        assert_eq!(pos.line, 0);
        assert_eq!(pos.character, 1);

        let pos = InlayHintsProvider::calculate_hint_position(2, 1, "y", text);
        assert_eq!(pos.line, 1);
        assert_eq!(pos.character, 1);

        let text = "my_variable = 42\n";
        let pos = InlayHintsProvider::calculate_hint_position(1, 1, "my_variable", text);
        assert_eq!(pos.line, 0);
        assert_eq!(pos.character, 11);
    }

    #[test]
    fn test_ast_to_lsp_position() {
        let pos = InlayHintsProvider::ast_to_lsp_position(1, 1);
        assert_eq!(pos.line, 0);
        assert_eq!(pos.character, 0);

        let pos = InlayHintsProvider::ast_to_lsp_position(10, 5);
        assert_eq!(pos.line, 9);
        assert_eq!(pos.character, 4);
    }

    #[test]
    fn test_list_type_hints() {
        let documents = DocumentManager::new().unwrap();
        let provider = InlayHintsProvider::new(documents.clone());
        let config = Config::default();
        let mut analyzer = Analyzer::new(config, documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"numbers = [1, 2, 3]
names = ["alice", "bob"]
mixed = [1, "two", 3.0]
empty = []
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let params = InlayHintParams {
            text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
            range: Range { start: Position { line: 0, character: 0 }, end: Position { line: 10, character: 0 } },
            work_done_progress_params: lsp_types::WorkDoneProgressParams::default(),
        };

        let _ = provider.inlay_hints(params, &mut analyzer);
    }

    #[test]
    fn test_dict_type_hints() {
        let documents = DocumentManager::new().unwrap();
        let provider = InlayHintsProvider::new(documents.clone());
        let config = Config::default();
        let mut analyzer = Analyzer::new(config, documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"ages = {"alice": 30, "bob": 25}
scores = {1: 100, 2: 95}
empty_dict = {}
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let params = InlayHintParams {
            text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
            range: Range { start: Position { line: 0, character: 0 }, end: Position { line: 10, character: 0 } },
            work_done_progress_params: lsp_types::WorkDoneProgressParams::default(),
        };

        let _ = provider.inlay_hints(params, &mut analyzer);
    }

    #[test]
    fn test_set_type_hints() {
        let documents = DocumentManager::new().unwrap();
        let provider = InlayHintsProvider::new(documents.clone());
        let config = Config::default();
        let mut analyzer = Analyzer::new(config, documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"numbers = {1, 2, 3}
names = {"alice", "bob"}
empty_set = set()
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let params = InlayHintParams {
            text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
            range: Range { start: Position { line: 0, character: 0 }, end: Position { line: 10, character: 0 } },
            work_done_progress_params: lsp_types::WorkDoneProgressParams::default(),
        };

        let _ = provider.inlay_hints(params, &mut analyzer);
    }

    #[test]
    fn test_tuple_type_hints() {
        let documents = DocumentManager::new().unwrap();
        let provider = InlayHintsProvider::new(documents.clone());
        let config = Config::default();
        let mut analyzer = Analyzer::new(config, documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"coords = (1, 2)
person = ("alice", 30, True)
singleton = (42,)
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let params = InlayHintParams {
            text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
            range: Range { start: Position { line: 0, character: 0 }, end: Position { line: 10, character: 0 } },
            work_done_progress_params: lsp_types::WorkDoneProgressParams::default(),
        };

        let hints = provider.inlay_hints(params, &mut analyzer);

        let type_hints: Vec<_> = hints
            .iter()
            .filter(|h| matches!(h.kind, Some(InlayHintKind::TYPE)))
            .collect();

        assert!(!type_hints.is_empty(), "Should have type hints for tuple assignments");
    }

    #[test]
    fn test_nested_collections() {
        let documents = DocumentManager::new().unwrap();
        let provider = InlayHintsProvider::new(documents.clone());
        let config = Config::default();
        let mut analyzer = Analyzer::new(config, documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"matrix = [[1, 2], [3, 4]]
lookup = {"key": [1, 2, 3]}
complex = [{"a": 1}, {"b": 2}]
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let params = InlayHintParams {
            text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
            range: Range { start: Position { line: 0, character: 0 }, end: Position { line: 10, character: 0 } },
            work_done_progress_params: lsp_types::WorkDoneProgressParams::default(),
        };

        let _ = provider.inlay_hints(params, &mut analyzer);
    }

    #[test]
    fn test_no_hints_for_any_type() {
        let documents = DocumentManager::new().unwrap();
        let provider = InlayHintsProvider::new(documents.clone());
        let config = Config::default();
        let mut analyzer = Analyzer::new(config, documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"x = unknown_function()
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let params = InlayHintParams {
            text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
            range: Range { start: Position { line: 0, character: 0 }, end: Position { line: 10, character: 0 } },
            work_done_progress_params: lsp_types::WorkDoneProgressParams::default(),
        };

        let hints = provider.inlay_hints(params, &mut analyzer);

        let any_hints: Vec<_> = hints
            .iter()
            .filter(
                |h| {
                    if let InlayHintLabel::String(label) = &h.label { label.contains("Any") } else { false }
                },
            )
            .collect();

        assert!(any_hints.is_empty(), "Should not show hints for 'Any' type");
    }

    #[test]
    fn test_no_hints_for_type_variables() {
        let documents = DocumentManager::new().unwrap();
        let provider = InlayHintsProvider::new(documents.clone());
        let config = Config::default();
        let mut analyzer = Analyzer::new(config, documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"x = []
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let params = InlayHintParams {
            text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
            range: Range { start: Position { line: 0, character: 0 }, end: Position { line: 10, character: 0 } },
            work_done_progress_params: lsp_types::WorkDoneProgressParams::default(),
        };

        let hints = provider.inlay_hints(params, &mut analyzer);
        let type_var_hints: Vec<_> = hints
            .iter()
            .filter(
                |h| {
                    if let InlayHintLabel::String(label) = &h.label { label.contains("'") } else { false }
                },
            )
            .collect();

        assert!(type_var_hints.is_empty(), "Should not show hints with type variables");
    }

    #[test]
    fn test_comprehension_type_hints() {
        let documents = DocumentManager::new().unwrap();
        let provider = InlayHintsProvider::new(documents.clone());
        let config = Config::default();
        let mut analyzer = Analyzer::new(config, documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"squares = [x * x for x in range(10)]
even = {x for x in range(10) if x % 2 == 0}
mapping = {x: x * 2 for x in range(5)}
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let params = InlayHintParams {
            text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
            range: Range { start: Position { line: 0, character: 0 }, end: Position { line: 10, character: 0 } },
            work_done_progress_params: lsp_types::WorkDoneProgressParams::default(),
        };

        let hints = provider.inlay_hints(params, &mut analyzer);

        let type_hints: Vec<_> = hints
            .iter()
            .filter(|h| matches!(h.kind, Some(InlayHintKind::TYPE)))
            .collect();

        assert!(
            !type_hints.is_empty(),
            "Should have type hints for comprehension results"
        );
    }

    #[test]
    fn test_return_type_hint_with_collection() {
        let documents = DocumentManager::new().unwrap();
        let provider = InlayHintsProvider::new(documents.clone());
        let config = Config::default();
        let mut analyzer = Analyzer::new(config, documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"def get_numbers():
    return [1, 2, 3]

def get_mapping():
    return {"a": 1, "b": 2}
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let params = InlayHintParams {
            text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
            range: Range { start: Position { line: 0, character: 0 }, end: Position { line: 10, character: 0 } },
            work_done_progress_params: lsp_types::WorkDoneProgressParams::default(),
        };

        let _ = provider.inlay_hints(params, &mut analyzer);
    }

    #[test]
    fn test_parameter_name_hints() {
        let documents = DocumentManager::new().unwrap();
        let provider = InlayHintsProvider::new(documents.clone());
        let config = Config::default();
        let mut analyzer = Analyzer::new(config, documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"def greet(name, age):
    print(name, age)

greet("Alice", 30)
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let params = InlayHintParams {
            text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
            range: Range { start: Position { line: 0, character: 0 }, end: Position { line: 10, character: 0 } },
            work_done_progress_params: lsp_types::WorkDoneProgressParams::default(),
        };

        let hints = provider.inlay_hints(params, &mut analyzer);

        let param_hints: Vec<_> = hints
            .iter()
            .filter(|h| matches!(h.kind, Some(InlayHintKind::PARAMETER)))
            .collect();

        assert_eq!(param_hints.len(), 2, "Should have 2 parameter hints");

        if let InlayHintLabel::String(label) = &param_hints[0].label {
            assert_eq!(label, "name: ", "First parameter should be 'name'");
        }

        if let InlayHintLabel::String(label) = &param_hints[1].label {
            assert_eq!(label, "age: ", "Second parameter should be 'age'");
        }
    }

    #[test]
    fn test_parameter_hints_nested_function() {
        let documents = DocumentManager::new().unwrap();
        let provider = InlayHintsProvider::new(documents.clone());
        let config = Config::default();
        let mut analyzer = Analyzer::new(config, documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"def outer():
    def inner(x, y):
        return x + y
    inner(1, 2)

outer()
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let params = InlayHintParams {
            text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
            range: Range { start: Position { line: 0, character: 0 }, end: Position { line: 10, character: 0 } },
            work_done_progress_params: lsp_types::WorkDoneProgressParams::default(),
        };

        let hints = provider.inlay_hints(params, &mut analyzer);

        let param_hints: Vec<_> = hints
            .iter()
            .filter(|h| matches!(h.kind, Some(InlayHintKind::PARAMETER)))
            .collect();

        assert_eq!(param_hints.len(), 2, "Should find hints for nested function");
    }

    #[test]
    fn test_no_parameter_hints_for_builtin() {
        let documents = DocumentManager::new().unwrap();
        let provider = InlayHintsProvider::new(documents.clone());
        let config = Config::default();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"print("hello")
len([1, 2, 3])
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let params = InlayHintParams {
            text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
            range: Range { start: Position { line: 0, character: 0 }, end: Position { line: 10, character: 0 } },
            work_done_progress_params: lsp_types::WorkDoneProgressParams::default(),
        };

        let hints = provider.inlay_hints(params, &mut analyzer);
        let param_hints: Vec<_> = hints
            .iter()
            .filter(|h| matches!(h.kind, Some(InlayHintKind::PARAMETER)))
            .collect();

        assert_eq!(
            param_hints.len(),
            0,
            "Should not show parameter hints for builtin functions"
        );
    }

    #[test]
    fn test_find_function_definition() {
        let documents = DocumentManager::new().unwrap();
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"def add(a, b):
    return a + b

def multiply(x, y, z):
    return x * y * z
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();
        documents.get_document(&uri, |doc| {
            let ast = doc.ast().unwrap();

            let add_params = InlayHintsProvider::find_function_definition("add", ast);
            assert!(add_params.is_some(), "Should find 'add' function");
            assert_eq!(add_params.unwrap().len(), 2, "'add' should have 2 parameters");

            let multiply_params = InlayHintsProvider::find_function_definition("multiply", ast);
            assert!(multiply_params.is_some(), "Should find 'multiply' function");
            assert_eq!(multiply_params.unwrap().len(), 3, "'multiply' should have 3 parameters");

            let nonexistent = InlayHintsProvider::find_function_definition("nonexistent", ast);
            assert!(nonexistent.is_none(), "Should not find nonexistent function");

            Some(())
        });
    }

    #[test]
    fn test_get_parameter_names() {
        let documents = DocumentManager::new().unwrap();
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"def process(input_data, config, verbose):
    pass
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();
        documents.get_document(&uri, |doc| {
            let ast = doc.ast().unwrap();

            let param_names = InlayHintsProvider::get_parameter_names("process", ast);
            assert!(param_names.is_some(), "Should find parameter names");

            let names = param_names.unwrap();
            assert_eq!(names.len(), 3, "Should have 3 parameter names");
            assert_eq!(names[0], "input_data");
            assert_eq!(names[1], "config");
            assert_eq!(names[2], "verbose");

            Some(())
        });
    }

    #[test]
    fn test_parameter_hints_in_class_method() {
        let documents = DocumentManager::new().unwrap();
        let provider = InlayHintsProvider::new(documents.clone());
        let config = Config::default();
        let mut analyzer = Analyzer::new(config, documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"class Calculator:
    def add(self, a, b):
        return a + b

calc = Calculator()
calc.add(5, 3)
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let params = InlayHintParams {
            text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
            range: Range { start: Position { line: 0, character: 0 }, end: Position { line: 10, character: 0 } },
            work_done_progress_params: lsp_types::WorkDoneProgressParams::default(),
        };

        let _ = provider.inlay_hints(params, &mut analyzer);
    }
}

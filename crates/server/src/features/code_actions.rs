//! Code actions provider
//!
//! Provides quick fixes and refactorings:
//! - Insert type annotations
//! - Convert None to Optional[T]
//! - Add missing imports
//! - Fix type errors where possible

use crate::document::DocumentManager;
use lsp_types::{
    CodeAction, CodeActionKind, CodeActionOrCommand, CodeActionParams, NumberOrString, Position, Range, TextEdit,
    WorkspaceEdit,
};
use std::collections::HashMap;
use url::Url;

trait CodeAsStr {
    fn as_str(&self) -> Option<&str>;
}

impl CodeAsStr for NumberOrString {
    fn as_str(&self) -> Option<&str> {
        match self {
            NumberOrString::String(s) => Some(s.as_str()),
            NumberOrString::Number(_) => None,
        }
    }
}

pub struct CodeActionsProvider {
    documents: DocumentManager,
}

impl CodeActionsProvider {
    pub fn new(documents: DocumentManager) -> Self {
        Self { documents }
    }

    /// Provide code actions for a range
    ///
    /// TODO: Add refactoring actions
    /// TODO: Add insert type annotation actions
    /// TODO: Add convert to Optional actions
    pub fn code_actions(&self, params: CodeActionParams) -> Vec<CodeActionOrCommand> {
        let mut actions = Vec::new();

        for diagnostic in &params.context.diagnostics {
            if let Some(action) = self.quick_fix_for_diagnostic(&params.text_document.uri, diagnostic) {
                actions.push(CodeActionOrCommand::CodeAction(action));
            }
        }

        actions
    }

    /// Generate a quick fix for a diagnostic
    fn quick_fix_for_diagnostic(&self, uri: &Url, diagnostic: &lsp_types::Diagnostic) -> Option<CodeAction> {
        let code = diagnostic.code.as_ref()?.as_str()?;

        match code {
            "unused-variable" | "unused-import" => self.remove_unused_line(uri, diagnostic),
            "HM001" if diagnostic.message.contains("None") => self.suggest_optional(uri, diagnostic),
            "PM001" => self.add_missing_patterns(uri, diagnostic),
            "PM002" => self.remove_unreachable_pattern(uri, diagnostic),
            _ => None,
        }
    }

    /// Quick fix: Remove unused variable or import line
    fn remove_unused_line(&self, uri: &Url, diagnostic: &lsp_types::Diagnostic) -> Option<CodeAction> {
        let line_start = Position { line: diagnostic.range.start.line, character: 0 };
        let line_end = Position { line: diagnostic.range.start.line + 1, character: 0 };
        let edit = TextEdit { range: Range { start: line_start, end: line_end }, new_text: String::new() };
        let mut changes = HashMap::default();

        changes.insert(uri.clone(), vec![edit]);

        Some(CodeAction {
            title: "Remove unused definition".to_string(),
            kind: Some(CodeActionKind::QUICKFIX),
            diagnostics: Some(vec![diagnostic.clone()]),
            edit: Some(WorkspaceEdit { changes: Some(changes), document_changes: None, change_annotations: None }),
            command: None,
            is_preferred: Some(true),
            disabled: None,
            data: None,
        })
    }

    /// Quick fix: Add Optional type annotation for None type errors
    ///
    /// Handles both function return types and variable annotations:
    /// - `def f() -> str:` with `return None` → `def f() -> Optional[str]:`
    /// - `x: str = ...` with `x = None` → `x: Optional[str] = ...`
    fn suggest_optional(&self, uri: &Url, diagnostic: &lsp_types::Diagnostic) -> Option<CodeAction> {
        self.documents.get_document(uri, |document| {
            let source = document.text();

            let line = diagnostic.range.start.line as usize;
            let lines: Vec<&str> = source.lines().collect();

            if line >= lines.len() {
                return None;
            }

            let current_line = lines[line];
            let mut edits = Vec::new();

            if let Some(return_type_edit) = self.wrap_return_type_with_optional(current_line, line) {
                edits.push(return_type_edit);
            } else if let Some(var_type_edit) = self.wrap_variable_type_with_optional(current_line, line) {
                edits.push(var_type_edit);
            } else {
                return Some(CodeAction {
                    title: "Add Optional type annotation".to_string(),
                    kind: Some(CodeActionKind::QUICKFIX),
                    diagnostics: Some(vec![diagnostic.clone()]),
                    edit: None,
                    command: None,
                    is_preferred: Some(false),
                    disabled: Some(lsp_types::CodeActionDisabled {
                        reason: "Could not locate type annotation to wrap - add Optional[T] manually".to_string(),
                    }),
                    data: None,
                });
            }

            if !source.contains("from typing import") || !source.contains("Optional") {
                if let Some(import_edit) = self.add_optional_import(&source) {
                    edits.insert(0, import_edit);
                }
            }

            let mut changes = HashMap::default();
            changes.insert(uri.clone(), edits);

            Some(CodeAction {
                title: "Wrap type with Optional".to_string(),
                kind: Some(CodeActionKind::QUICKFIX),
                diagnostics: Some(vec![diagnostic.clone()]),
                edit: Some(WorkspaceEdit { changes: Some(changes), document_changes: None, change_annotations: None }),
                command: None,
                is_preferred: Some(true),
                disabled: None,
                data: None,
            })
        })?
    }

    /// Find and wrap return type annotation with Optional
    /// Converts `-> Type:` to `-> Optional[Type]:`
    /// Match pattern: `-> TypeName:` where TypeName doesn't already contain Optional
    fn wrap_return_type_with_optional(&self, line: &str, line_num: usize) -> Option<TextEdit> {
        let pattern = r"->\s*([A-Za-z_][A-Za-z0-9_\[\],\s|]*?)\s*:";
        let re = regex::Regex::new(pattern).ok()?;

        let captures = re.captures(line)?;
        let type_name = captures.get(1)?.as_str().trim();

        if type_name.starts_with("Optional") {
            return None;
        }

        let full_match = captures.get(0)?;
        let end_char = full_match.end();

        let arrow_end = line[..end_char].rfind("->")? + 2;
        let colon_start = line[..end_char].rfind(':')?;

        let start = Position { line: line_num as u32, character: arrow_end as u32 };
        let end = Position { line: line_num as u32, character: colon_start as u32 };

        Some(TextEdit { range: Range { start, end }, new_text: format!(" Optional[{type_name}]") })
    }

    /// Find and wrap variable type annotation with Optional
    /// Converts `x: Type` to `x: Optional[Type]`
    /// Match pattern: `identifier: TypeName` where TypeName doesn't already contain Optional
    fn wrap_variable_type_with_optional(&self, line: &str, line_num: usize) -> Option<TextEdit> {
        let pattern = r"([A-Za-z_][A-Za-z0-9_]*)\s*:\s*([A-Za-z_][A-Za-z0-9_\[\],\s|]*?)(?:\s*[=\n]|$)";
        let re = regex::Regex::new(pattern).ok()?;
        let captures = re.captures(line)?;
        let type_name = captures.get(2)?.as_str().trim();

        if type_name.starts_with("Optional") {
            return None;
        }

        let colon_pos = line.find(':')?;
        let type_start = colon_pos + 1;
        let type_end = type_start + line[type_start..].find(['=', '\n']).unwrap_or(line[type_start..].len());

        let start = Position { line: line_num as u32, character: type_start as u32 };
        let end = Position { line: line_num as u32, character: type_end as u32 };

        Some(TextEdit { range: Range { start, end }, new_text: format!(" Optional[{type_name}]") })
    }

    /// Add `from typing import Optional` at the top of the file
    /// Strategy: after any existing imports, or at the top of the file
    fn add_optional_import(&self, source: &str) -> Option<TextEdit> {
        let lines: Vec<&str> = source.lines().collect();
        let mut insert_line = 0;

        for (i, line) in lines.iter().enumerate() {
            if line.trim_start().starts_with("import ") || line.trim_start().starts_with("from ") {
                insert_line = i + 1;
            }
        }

        Some(TextEdit {
            range: Range {
                start: Position { line: insert_line as u32, character: 0 },
                end: Position { line: insert_line as u32, character: 0 },
            },
            new_text: "from typing import Optional\n".to_string(),
        })
    }

    /// TODO: Insert type annotation from inference
    /// Get inferred type at position
    /// Generate edit to insert type annotation
    /// Example: `x = 42` -> `x: int = 42`
    pub fn _insert_type_annotation(&self, _uri: &Url, _range: Range) -> Option<CodeAction> {
        None
    }

    /// TODO: Convert None to Optional
    /// Find assignments/returns with None
    /// Add Optional[T] annotation
    /// Example: `def f() -> str:` with `return None` -> `def f() -> Optional[str]:`
    pub fn _convert_to_optional(&self, _uri: &Url, _range: Range) -> Option<CodeAction> {
        None
    }

    /// TODO: Add missing import
    /// Determine appropriate module for symbol
    /// Generate import statement edit
    pub fn _add_missing_import(&self, _uri: &Url, _symbol: &str) -> Option<CodeAction> {
        None
    }

    /// TODO: Implement missing protocol methods
    /// For classes implementing protocols
    /// Generate stubs for missing methods
    pub fn _implement_protocol(&self, _uri: &Url, _range: Range) -> Option<CodeAction> {
        None
    }

    /// TODO: Extract to function/method
    /// Refactoring: extract selected code to new function
    pub fn _extract_function(&self, _uri: &Url, _range: Range) -> Option<CodeAction> {
        None
    }

    /// TODO: Inline variable
    /// Replace variable uses with its value
    pub fn _inline_variable(&self, _uri: &Url, _range: Range) -> Option<CodeAction> {
        None
    }

    /// Quick fix: Remove unreachable pattern (PM002)
    ///
    /// Removes the entire case block that contains an unreachable pattern.
    /// The diagnostic range points to the pattern, we need to extend it to include the entire case.
    fn remove_unreachable_pattern(&self, uri: &Url, diagnostic: &lsp_types::Diagnostic) -> Option<CodeAction> {
        self.documents.get_document(uri, |document| {
            let source = document.text();
            let lines: Vec<&str> = source.lines().collect();
            let start_line = diagnostic.range.start.line as usize;
            if start_line >= lines.len() {
                return None;
            }

            let case_start_line = (0..=start_line)
                .rev()
                .find(|&i| i < lines.len() && lines[i].trim_start().starts_with("case "))?;

            let case_end_line = ((case_start_line + 1)..lines.len())
                .find(|&i| {
                    let trimmed = lines[i].trim_start();
                    trimmed.starts_with("case ")
                        || (!trimmed.is_empty() && !trimmed.starts_with(' ') && !trimmed.starts_with('\t'))
                })
                .unwrap_or(lines.len());

            let start_pos = Position { line: case_start_line as u32, character: 0 };
            let end_pos = Position { line: case_end_line as u32, character: 0 };

            let edit = TextEdit { range: Range { start: start_pos, end: end_pos }, new_text: String::new() };

            let mut changes = HashMap::default();
            changes.insert(uri.clone(), vec![edit]);

            Some(CodeAction {
                title: "Remove unreachable pattern".to_string(),
                kind: Some(CodeActionKind::QUICKFIX),
                diagnostics: Some(vec![diagnostic.clone()]),
                edit: Some(WorkspaceEdit { changes: Some(changes), document_changes: None, change_annotations: None }),
                command: None,
                is_preferred: Some(true),
                disabled: None,
                data: None,
            })
        })?
    }

    /// Quick fix: Add missing pattern cases (PM001)
    ///
    /// Parses the diagnostic message to extract uncovered types and generates case statements for each.
    /// Inserts the new cases before the end of the match statement.
    fn add_missing_patterns(&self, uri: &Url, diagnostic: &lsp_types::Diagnostic) -> Option<CodeAction> {
        self.documents.get_document(uri, |document| {
            let source = document.text();
            let lines: Vec<&str> = source.lines().collect();

            let uncovered_types = self.extract_uncovered_types(&diagnostic.message)?;

            let match_start_line = diagnostic.range.start.line as usize;
            let indent = self.detect_case_indentation(&lines, match_start_line)?;

            let insert_line = self.find_last_case_end(&lines, match_start_line)?;

            let mut new_cases = String::new();
            for uncovered_type in &uncovered_types {
                let pattern = self.generate_pattern_for_type(uncovered_type);
                new_cases.push_str(&format!("{indent}case {pattern}:\n"));
                new_cases.push_str(&format!("{indent}    pass  # TODO: Handle {uncovered_type} case\n"));
            }

            let edit = TextEdit {
                range: Range {
                    start: Position { line: insert_line as u32, character: 0 },
                    end: Position { line: insert_line as u32, character: 0 },
                },
                new_text: new_cases,
            };

            let mut changes = HashMap::default();
            changes.insert(uri.clone(), vec![edit]);

            Some(CodeAction {
                title: format!(
                    "Add missing pattern case{}",
                    if uncovered_types.len() > 1 { "s" } else { "" }
                ),
                kind: Some(CodeActionKind::QUICKFIX),
                diagnostics: Some(vec![diagnostic.clone()]),
                edit: Some(WorkspaceEdit { changes: Some(changes), document_changes: None, change_annotations: None }),
                command: None,
                is_preferred: Some(true),
                disabled: None,
                data: None,
            })
        })?
    }

    /// Extract uncovered types from PM001 diagnostic message
    fn extract_uncovered_types(&self, message: &str) -> Option<Vec<String>> {
        let prefix = "Missing coverage for: ";
        let types_str = message.split(prefix).nth(1)?;

        Some(types_str.split(", ").map(|s| s.trim().to_string()).collect())
    }

    /// Detect the indentation level of case statements in the match block
    fn detect_case_indentation(&self, lines: &[&str], start_line: usize) -> Option<String> {
        for line in lines.iter().skip(start_line) {
            if line.trim_start().starts_with("case ") {
                let indent_count = line.len() - line.trim_start().len();
                return Some(" ".repeat(indent_count));
            }
        }
        Some("    ".to_string())
    }

    /// Find the line after the last case block where we should insert new cases
    fn find_last_case_end(&self, lines: &[&str], start_line: usize) -> Option<usize> {
        let mut last_case_end = start_line;
        let mut in_case = false;

        for (i, line) in lines.iter().enumerate().skip(start_line) {
            let trimmed = line.trim_start();

            if trimmed.starts_with("case ") {
                in_case = true;
                last_case_end = i + 1;
            } else if in_case && !trimmed.is_empty() && !trimmed.starts_with(' ') && !trimmed.starts_with('\t') {
                return Some(last_case_end);
            } else if in_case && !trimmed.is_empty() {
                last_case_end = i + 1;
            }
        }

        Some(last_case_end)
    }

    /// Generate a simple pattern for a given type name
    fn generate_pattern_for_type(&self, type_name: &str) -> String {
        if type_name.chars().next().is_some_and(|c| c.is_uppercase()) {
            format!("{type_name}()")
        } else {
            format!("_ if isinstance(_, {type_name})")
        }
    }

    /// Create a workspace edit for a single file
    fn _create_edit(&self, uri: &Url, edits: Vec<TextEdit>) -> WorkspaceEdit {
        let mut changes = HashMap::default();
        changes.insert(uri.clone(), edits);
        WorkspaceEdit { changes: Some(changes), document_changes: None, change_annotations: None }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_provider_creation() {
        let documents = DocumentManager::new().unwrap();
        let _ = CodeActionsProvider::new(documents);
    }
}

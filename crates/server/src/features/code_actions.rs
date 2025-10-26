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

/// Helper trait to extract string from NumberOrString
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
    /// TODO: Implement various code action types
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
    ///
    /// Matches on diagnostic code to provide appropriate fixes
    fn quick_fix_for_diagnostic(&self, uri: &Url, diagnostic: &lsp_types::Diagnostic) -> Option<CodeAction> {
        let code = diagnostic.code.as_ref()?.as_str()?;

        match code {
            "unused-variable" | "unused-import" => self.remove_unused_line(uri, diagnostic),
            "HM001" if diagnostic.message.contains("None") => self.suggest_optional(uri, diagnostic),
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

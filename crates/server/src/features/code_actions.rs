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
    _documents: DocumentManager,
}

impl CodeActionsProvider {
    pub fn new(documents: DocumentManager) -> Self {
        Self { _documents: documents }
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

    /// Quick fix: Suggest adding Optional for None type errors
    /// For now, just suggest the action without implementing the edit
    /// Full implementation would parse the annotation and add Optional[...]
    /// TODO: Implement edit generation
    fn suggest_optional(&self, _uri: &Url, diagnostic: &lsp_types::Diagnostic) -> Option<CodeAction> {
        Some(CodeAction {
            title: "Add Optional type annotation".to_string(),
            kind: Some(CodeActionKind::QUICKFIX),
            diagnostics: Some(vec![diagnostic.clone()]),
            edit: None,
            command: None,
            is_preferred: Some(false),
            disabled: Some(lsp_types::CodeActionDisabled {
                reason: "Not yet implemented - add Optional[T] manually".to_string(),
            }),
            data: None,
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

//! Code actions provider
//!
//! Provides quick fixes and refactorings:
//! - Insert type annotations
//! - Convert None to Optional[T]
//! - Add missing imports
//! - Fix type errors where possible

use crate::{analysis::Analyzer, document::DocumentManager};

use beacon_core::{Type, TypeCtor};
use lsp_types::{
    CodeAction, CodeActionKind, CodeActionOrCommand, CodeActionParams, NumberOrString, Position, Range, TextEdit,
    WorkspaceEdit,
};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
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
    analyzer: Arc<RwLock<Analyzer>>,
}

impl CodeActionsProvider {
    pub fn new(documents: DocumentManager, analyzer: Arc<RwLock<Analyzer>>) -> Self {
        Self { documents, analyzer }
    }

    /// Provide code actions for a range
    ///
    /// Provides both diagnostic-based quick fixes and refactoring actions
    pub async fn code_actions(&self, params: CodeActionParams) -> Vec<CodeActionOrCommand> {
        let mut actions = Vec::new();

        for diagnostic in &params.context.diagnostics {
            let fixes = self.quick_fix_for_diagnostic(&params.text_document.uri, diagnostic);
            for action in fixes {
                actions.push(CodeActionOrCommand::CodeAction(action));
            }
        }

        if let Some(action) = self
            .insert_type_annotation(&params.text_document.uri, params.range.start)
            .await
        {
            actions.push(CodeActionOrCommand::CodeAction(action));
        }

        actions
    }

    /// Generate quick fixes for a diagnostic
    ///
    /// For some diagnostics (like PM002), multiple actions may be available.
    fn quick_fix_for_diagnostic(&self, uri: &Url, diagnostic: &lsp_types::Diagnostic) -> Vec<CodeAction> {
        let code = diagnostic.code.as_ref().and_then(|c| c.as_str());

        match code {
            Some("unused-variable" | "unused-import") => self.remove_unused_line(uri, diagnostic).into_iter().collect(),
            Some("HM001") if diagnostic.message.contains("None") => {
                self.suggest_optional(uri, diagnostic).into_iter().collect()
            }
            Some("HM006") => self.implement_protocol_methods(uri, diagnostic).into_iter().collect(),
            Some("PM001") => self.add_missing_patterns(uri, diagnostic).into_iter().collect(),
            Some("PM002") => {
                let mut actions = Vec::new();
                if let Some(move_action) = self.move_pattern_before_previous(uri, diagnostic) {
                    actions.push(move_action);
                }
                if let Some(remove_action) = self.remove_unreachable_pattern(uri, diagnostic) {
                    actions.push(remove_action);
                }
                actions
            }
            _ => Vec::new(),
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

    /// Insert type annotation from inference
    ///
    /// Analyzes variable assignments and offers to add type annotations based on inferred types.
    /// Example: `x = 42` -> `x: int = 42`
    pub async fn insert_type_annotation(&self, uri: &Url, position: Position) -> Option<CodeAction> {
        let mut analyzer = self.analyzer.write().await;
        let inferred_type = analyzer.type_at_position(uri, position).ok()??;

        let (line_text, line_num) = self.documents.get_document(uri, |doc| {
            let text = doc.text();
            let lines: Vec<&str> = text.lines().collect();
            let line_idx = position.line as usize;
            if line_idx < lines.len() { Some((lines[line_idx].to_string(), line_idx)) } else { None }
        })??;

        let assignment_pattern = regex::Regex::new(r"^\s*([A-Za-z_][A-Za-z0-9_]*)\s*=\s*").ok()?;

        if let Some(captures) = assignment_pattern.captures(&line_text) {
            if line_text.contains(':') && line_text.find(':')? < line_text.find('=')? {
                return None;
            }

            let var_name = captures.get(1)?.as_str();
            let var_end = captures.get(1)?.end();
            let type_str = Self::format_type_for_annotation(&inferred_type);

            let edit = TextEdit {
                range: Range {
                    start: Position { line: line_num as u32, character: var_end as u32 },
                    end: Position { line: line_num as u32, character: var_end as u32 },
                },
                new_text: format!(": {type_str}"),
            };

            let mut changes = HashMap::default();
            changes.insert(uri.clone(), vec![edit]);

            return Some(CodeAction {
                title: format!("Add type annotation: {var_name}: {type_str}"),
                kind: Some(CodeActionKind::REFACTOR),
                diagnostics: None,
                edit: Some(WorkspaceEdit { changes: Some(changes), document_changes: None, change_annotations: None }),
                command: None,
                is_preferred: Some(false),
                disabled: None,
                data: None,
            });
        }

        None
    }

    /// Format a [Type] for use in a type annotation, i.e. converts internal Type representation to Python syntax
    fn format_type_for_annotation(ty: &Type) -> String {
        match ty {
            Type::Con(TypeCtor::Int) => "int".to_string(),
            Type::Con(TypeCtor::Float) => "float".to_string(),
            Type::Con(TypeCtor::String) => "str".to_string(),
            Type::Con(TypeCtor::Bool) => "bool".to_string(),
            Type::Con(TypeCtor::NoneType) => "None".to_string(),
            Type::Con(TypeCtor::Class(name)) => name.clone(),
            Type::Con(TypeCtor::Protocol(Some(name), _)) => name.clone(),
            Type::Con(TypeCtor::Protocol(None, _)) => "Protocol".to_string(),
            Type::Con(TypeCtor::TypeVariable(name)) => name.clone(),
            Type::Con(TypeCtor::Any) => "Any".to_string(),
            Type::Var(tv) => format!("TypeVar('{tv}')"),
            Type::App(ctor, elem_ty) if matches!(**ctor, Type::Con(TypeCtor::List)) => {
                format!("list[{}]", Self::format_type_for_annotation(elem_ty))
            }
            Type::App(ctor, elem_ty) if matches!(**ctor, Type::Con(TypeCtor::Set)) => {
                format!("set[{}]", Self::format_type_for_annotation(elem_ty))
            }
            Type::App(app_ctor, val_ty) => {
                if let Type::App(ctor, key_ty) = &**app_ctor {
                    if matches!(**ctor, Type::Con(TypeCtor::Dict)) {
                        return format!(
                            "dict[{}, {}]",
                            Self::format_type_for_annotation(key_ty),
                            Self::format_type_for_annotation(val_ty)
                        );
                    }
                }
                format!(
                    "{}[{}]",
                    Self::format_type_for_annotation(app_ctor),
                    Self::format_type_for_annotation(val_ty)
                )
            }
            Type::Tuple(elem_types) => {
                let types = elem_types
                    .iter()
                    .map(Self::format_type_for_annotation)
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("tuple[{types}]")
            }
            Type::Fun(_, _) => "Callable".to_string(),
            Type::Union(types) => {
                if types.len() == 2 && types.iter().any(|t| matches!(t, Type::Con(TypeCtor::NoneType))) {
                    let inner = types
                        .iter()
                        .find(|t| !matches!(t, Type::Con(TypeCtor::NoneType)))
                        .unwrap();
                    return format!("Optional[{}]", Self::format_type_for_annotation(inner));
                }

                types
                    .iter()
                    .map(Self::format_type_for_annotation)
                    .collect::<Vec<_>>()
                    .join(" | ")
            }
            _ => "Any".to_string(),
        }
    }

    /// Quick fix: Move unreachable pattern before the subsuming pattern (PM002)
    ///
    /// Moves the unreachable pattern case block before the previous case block (likely the subsuming one).
    /// This allows the pattern to be checked before the more general pattern that subsumes it.
    fn move_pattern_before_previous(&self, uri: &Url, diagnostic: &lsp_types::Diagnostic) -> Option<CodeAction> {
        self.documents.get_document(uri, |document| {
            let source = document.text();
            let lines: Vec<&str> = source.lines().collect();
            let diag_line = diagnostic.range.start.line as usize;

            if diag_line >= lines.len() {
                return None;
            }

            let unreachable_case_start = (0..=diag_line)
                .rev()
                .find(|&i| i < lines.len() && lines[i].trim_start().starts_with("case "))?;

            let unreachable_case_end = ((unreachable_case_start + 1)..lines.len())
                .find(|&i| {
                    let trimmed = lines[i].trim_start();
                    trimmed.starts_with("case ")
                        || (!trimmed.is_empty() && !trimmed.starts_with(' ') && !trimmed.starts_with('\t'))
                })
                .unwrap_or(lines.len());

            let previous_case_start = (0..unreachable_case_start)
                .rev()
                .find(|&i| lines[i].trim_start().starts_with("case "))?;

            let unreachable_case_text: String = lines[unreachable_case_start..unreachable_case_end]
                .iter()
                .map(|line| format!("{line}\n"))
                .collect();

            let edits = vec![
                TextEdit {
                    range: Range {
                        start: Position { line: previous_case_start as u32, character: 0 },
                        end: Position { line: previous_case_start as u32, character: 0 },
                    },
                    new_text: unreachable_case_text,
                },
                TextEdit {
                    range: Range {
                        start: Position { line: unreachable_case_start as u32, character: 0 },
                        end: Position { line: unreachable_case_end as u32, character: 0 },
                    },
                    new_text: String::new(),
                },
            ];

            let mut changes = HashMap::default();
            changes.insert(uri.clone(), edits);

            Some(CodeAction {
                title: "Move pattern before subsuming pattern".to_string(),
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

    /// Quick fix: Remove unreachable pattern (PM002)
    ///
    /// Removes the entire case block that contains an unreachable pattern.
    /// The diagnostic range points to the case pattern.
    fn remove_unreachable_pattern(&self, uri: &Url, diagnostic: &lsp_types::Diagnostic) -> Option<CodeAction> {
        self.documents.get_document(uri, |document| {
            let source = document.text();
            let lines: Vec<&str> = source.lines().collect();
            let diag_line = diagnostic.range.start.line as usize;

            if diag_line >= lines.len() {
                return None;
            }

            let case_start_line = (0..=diag_line)
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
                is_preferred: Some(false),
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

    /// Quick fix: Implement missing protocol methods (HM006)
    ///
    /// Parses the diagnostic message to extract the class and protocol, then
    /// generates stub implementations for protocols that a class doesn't satisfy.
    fn implement_protocol_methods(&self, uri: &Url, diagnostic: &lsp_types::Diagnostic) -> Option<CodeAction> {
        let message = &diagnostic.message;
        let (class_name, protocol_name) = self.parse_protocol_error(message)?;

        self.documents.get_document(uri, |document| {
            let source = document.text();
            let lines: Vec<&str> = source.lines().collect();
            let (_, class_end, class_indent) = self.find_class_definition(&lines, &class_name)?;
            let method_stubs = self.generate_protocol_stubs(&protocol_name, &class_indent)?;
            let insert_line = class_end;

            let edit = TextEdit {
                range: Range {
                    start: Position { line: insert_line as u32, character: 0 },
                    end: Position { line: insert_line as u32, character: 0 },
                },
                new_text: method_stubs,
            };

            let mut changes = HashMap::default();
            changes.insert(uri.clone(), vec![edit]);

            Some(CodeAction {
                title: format!("Implement {protocol_name} protocol methods"),
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

    /// Parse protocol error message to extract class and protocol names
    fn parse_protocol_error(&self, message: &str) -> Option<(String, String)> {
        let pattern = regex::Regex::new(r"Type (\w+) does not satisfy protocol (\w+)").ok()?;
        let captures = pattern.captures(message)?;
        let class_name = captures.get(1)?.as_str().to_string();
        let protocol_name = captures.get(2)?.as_str().to_string();
        Some((class_name, protocol_name))
    }

    /// Find a class definition in source lines
    fn find_class_definition(&self, lines: &[&str], class_name: &str) -> Option<(usize, usize, String)> {
        let class_pattern = regex::Regex::new(&format!(r"^(\s*)class\s+{class_name}\s*[\(:]")).ok()?;

        let mut class_start = None;
        let mut class_indent = String::new();

        for (i, line) in lines.iter().enumerate() {
            if let Some(captures) = class_pattern.captures(line) {
                class_start = Some(i);
                class_indent = captures.get(1)?.as_str().to_string();
                break;
            }
        }

        let start = class_start?;

        let mut end = start + 1;
        for (i, line) in lines.iter().enumerate().skip(start + 1) {
            if line.trim().is_empty() {
                continue;
            }

            let line_indent = line.len() - line.trim_start().len();
            let class_indent_len = class_indent.len();

            if line_indent <= class_indent_len {
                end = i;
                break;
            }
            end = i + 1;
        }

        Some((start, end, class_indent))
    }

    /// Generate method stubs for built-in protocols like Iterable, Iterator, Sized, etc.
    fn generate_protocol_stubs(&self, protocol_name: &str, base_indent: &str) -> Option<String> {
        let method_indent = format!("{base_indent}    ");
        let body_indent = format!("{method_indent}    ");

        let stubs = match protocol_name {
            "Iterable" => {
                format!("\n{method_indent}def __iter__(self):\n{body_indent}raise NotImplementedError\n")
            }
            "Iterator" => {
                format!(
                    "\n{method_indent}def __iter__(self):\n{body_indent}return self\n\n{method_indent}def __next__(self):\n{body_indent}raise NotImplementedError\n"
                )
            }
            "Sized" => {
                format!("\n{method_indent}def __len__(self) -> int:\n{body_indent}raise NotImplementedError\n")
            }
            "Callable" => {
                format!(
                    "\n{method_indent}def __call__(self, *args, **kwargs):\n{body_indent}raise NotImplementedError\n"
                )
            }
            "Sequence" => {
                format!(
                    "\n{method_indent}def __getitem__(self, index):\n{body_indent}raise NotImplementedError\n\n{method_indent}def __len__(self) -> int:\n{body_indent}raise NotImplementedError\n"
                )
            }
            "Mapping" => {
                format!(
                    "\n{method_indent}def __getitem__(self, key):\n{body_indent}raise NotImplementedError\n\n{method_indent}def __len__(self) -> int:\n{body_indent}raise NotImplementedError\n\n{method_indent}def __iter__(self):\n{body_indent}raise NotImplementedError\n"
                )
            }
            _ => return None,
        };

        Some(stubs)
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
    use crate::config::Config;
    use beacon_core::Type;
    use std::str::FromStr;
    use std::sync::Arc;
    use tokio::sync::RwLock;
    use url::Url;

    async fn create_test_provider() -> (CodeActionsProvider, Url) {
        let documents = DocumentManager::new().unwrap();
        let config = Config::default();
        let analyzer = Arc::new(RwLock::new(Analyzer::new(config, documents.clone())));
        let provider = CodeActionsProvider::new(documents.clone(), analyzer);

        let uri = Url::from_str("file:///test.py").unwrap();
        (provider, uri)
    }

    #[tokio::test]
    async fn test_provider_creation() {
        let _ = create_test_provider().await;
    }

    #[tokio::test]
    async fn test_parse_protocol_error() {
        let (provider, _) = create_test_provider().await;
        let message = "Type MyClass does not satisfy protocol Iterable";
        let result = provider.parse_protocol_error(message);

        assert!(result.is_some());
        let (class_name, protocol_name) = result.unwrap();
        assert_eq!(class_name, "MyClass");
        assert_eq!(protocol_name, "Iterable");
    }

    #[tokio::test]
    async fn test_find_class_definition() {
        let (provider, _) = create_test_provider().await;
        let source = vec![
            "# comment",
            "class MyClass:",
            "    def method(self):",
            "        pass",
            "",
            "def function():",
            "    pass",
        ];

        let result = provider.find_class_definition(&source, "MyClass");
        assert!(result.is_some());

        let (start, end, indent) = result.unwrap();
        assert_eq!(start, 1);
        assert_eq!(end, 5);
        assert_eq!(indent, "");
    }

    #[tokio::test]
    async fn test_find_indented_class() {
        let (provider, _) = create_test_provider().await;

        let source = vec![
            "if True:",
            "    class IndentedClass:",
            "        x: int = 5",
            "        ",
            "    y = 10",
        ];

        let result = provider.find_class_definition(&source, "IndentedClass");
        assert!(result.is_some());

        let (start, end, indent) = result.unwrap();
        assert_eq!(start, 1);
        assert_eq!(end, 4);
        assert_eq!(indent, "    ");
    }

    #[tokio::test]
    async fn test_generate_protocol_stubs_iterable() {
        let (provider, _) = create_test_provider().await;
        let stubs = provider.generate_protocol_stubs("Iterable", "");
        assert!(stubs.is_some());

        let stubs_str = stubs.unwrap();
        assert!(stubs_str.contains("def __iter__(self):"));
        assert!(stubs_str.contains("raise NotImplementedError"));
    }

    #[tokio::test]
    async fn test_generate_protocol_stubs_iterator() {
        let (provider, _) = create_test_provider().await;
        let stubs = provider.generate_protocol_stubs("Iterator", "");
        assert!(stubs.is_some());

        let stubs_str = stubs.unwrap();
        assert!(stubs_str.contains("def __iter__(self):"));
        assert!(stubs_str.contains("def __next__(self):"));
        assert!(stubs_str.contains("return self"));
    }

    #[tokio::test]
    async fn test_generate_protocol_stubs_sized() {
        let (provider, _) = create_test_provider().await;

        let stubs = provider.generate_protocol_stubs("Sized", "");
        assert!(stubs.is_some());

        let stubs_str = stubs.unwrap();
        assert!(stubs_str.contains("def __len__(self) -> int:"));
    }

    #[tokio::test]
    async fn test_generate_protocol_stubs_with_indent() {
        let (provider, _) = create_test_provider().await;

        let stubs = provider.generate_protocol_stubs("Iterable", "    ");
        assert!(stubs.is_some());

        let stubs_str = stubs.unwrap();
        assert!(stubs_str.contains("    def __iter__(self):"));
    }

    #[tokio::test]
    async fn test_format_type_for_annotation_primitives() {
        assert_eq!(CodeActionsProvider::format_type_for_annotation(&Type::int()), "int");
        assert_eq!(CodeActionsProvider::format_type_for_annotation(&Type::string()), "str");
        assert_eq!(CodeActionsProvider::format_type_for_annotation(&Type::bool()), "bool");
        assert_eq!(CodeActionsProvider::format_type_for_annotation(&Type::float()), "float");
    }

    #[tokio::test]
    async fn test_format_type_for_annotation_collections() {
        let list_type = Type::list(Type::int());
        assert_eq!(CodeActionsProvider::format_type_for_annotation(&list_type), "list[int]");

        let dict_type = Type::dict(Type::string(), Type::int());
        assert_eq!(
            CodeActionsProvider::format_type_for_annotation(&dict_type),
            "dict[str, int]"
        );

        let set_type = Type::set(Type::string());
        assert_eq!(CodeActionsProvider::format_type_for_annotation(&set_type), "set[str]");
    }

    #[tokio::test]
    async fn test_format_type_for_annotation_optional() {
        let optional_type = Type::optional(Type::int());
        assert_eq!(
            CodeActionsProvider::format_type_for_annotation(&optional_type),
            "Optional[int]"
        );
    }

    #[tokio::test]
    async fn test_format_type_for_annotation_union() {
        let union_type = Type::union(vec![Type::int(), Type::string()]);
        let result = CodeActionsProvider::format_type_for_annotation(&union_type);
        assert!(result.contains("int"));
        assert!(result.contains("str"));
        assert!(result.contains("|"));
    }

    #[tokio::test]
    async fn test_move_pattern_before_previous() {
        let (provider, uri) = create_test_provider().await;
        let documents = provider.documents.clone();

        let source = "def foo(x):\n    match x:\n        case int():\n            return 1\n        case _:\n            return 0\n";

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let diagnostic = lsp_types::Diagnostic {
            range: Range { start: Position { line: 4, character: 13 }, end: Position { line: 4, character: 14 } },
            severity: Some(lsp_types::DiagnosticSeverity::WARNING),
            code: Some(NumberOrString::String("PM002".to_string())),
            message: "This pattern is unreachable (subsumed by an earlier pattern)".to_string(),
            source: Some("beacon".to_string()),
            ..Default::default()
        };

        let result = provider.move_pattern_before_previous(&uri, &diagnostic);
        assert!(result.is_some());

        let action = result.unwrap();
        assert_eq!(action.title, "Move pattern before subsuming pattern");
        assert_eq!(action.is_preferred, Some(true));
    }

    #[tokio::test]
    async fn test_pm002_provides_both_actions() {
        let (provider, uri) = create_test_provider().await;
        let documents = provider.documents.clone();

        let source = "def foo(x):\n    match x:\n        case int():\n            return 1\n        case _:\n            return 0\n";

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let diagnostic = lsp_types::Diagnostic {
            range: Range { start: Position { line: 4, character: 13 }, end: Position { line: 4, character: 14 } },
            severity: Some(lsp_types::DiagnosticSeverity::WARNING),
            code: Some(NumberOrString::String("PM002".to_string())),
            message: "This pattern is unreachable (subsumed by an earlier pattern)".to_string(),
            source: Some("beacon".to_string()),
            ..Default::default()
        };

        let actions = provider.quick_fix_for_diagnostic(&uri, &diagnostic);
        assert_eq!(actions.len(), 2);

        assert!(actions[0].title.contains("Move"));
        assert_eq!(actions[0].is_preferred, Some(true));

        assert!(actions[1].title.contains("Remove"));
        assert_eq!(actions[1].is_preferred, Some(false));
    }

    #[tokio::test]
    async fn test_pm002_code_actions_integration() {
        let (provider, uri) = create_test_provider().await;
        let documents = provider.documents.clone();

        let source = "def foo(x):\n    match x:\n        case int():\n            return 1\n        case _:\n            return 0\n";

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let diagnostic = lsp_types::Diagnostic {
            range: Range { start: Position { line: 4, character: 13 }, end: Position { line: 4, character: 14 } },
            severity: Some(lsp_types::DiagnosticSeverity::WARNING),
            code: Some(NumberOrString::String("PM002".to_string())),
            message: "This pattern is unreachable (subsumed by an earlier pattern)".to_string(),
            source: Some("beacon".to_string()),
            ..Default::default()
        };

        let params = CodeActionParams {
            text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
            range: diagnostic.range,
            context: lsp_types::CodeActionContext { diagnostics: vec![diagnostic], only: None, trigger_kind: None },
            work_done_progress_params: lsp_types::WorkDoneProgressParams::default(),
            partial_result_params: lsp_types::PartialResultParams::default(),
        };

        let actions = provider.code_actions(params).await;
        assert!(actions.len() >= 2, "Expected at least 2 actions, got {}", actions.len());

        let move_action = actions.iter().find(|a| {
            if let CodeActionOrCommand::CodeAction(ca) = a { ca.title.contains("Move") } else { false }
        });

        let remove_action = actions.iter().find(|a| {
            if let CodeActionOrCommand::CodeAction(ca) = a { ca.title.contains("Remove") } else { false }
        });

        assert!(move_action.is_some(), "Move action not found");
        assert!(remove_action.is_some(), "Remove action not found");
    }

    #[tokio::test]
    async fn test_remove_unused_line() {
        let (provider, uri) = create_test_provider().await;
        let documents = provider.documents.clone();

        let source = "x = 42\ny = 10\nz = 5\n";
        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let diagnostic = lsp_types::Diagnostic {
            range: Range { start: Position { line: 1, character: 0 }, end: Position { line: 1, character: 1 } },
            severity: Some(lsp_types::DiagnosticSeverity::WARNING),
            code: Some(NumberOrString::String("unused-variable".to_string())),
            message: "Unused variable 'y'".to_string(),
            source: Some("beacon".to_string()),
            ..Default::default()
        };

        let result = provider.remove_unused_line(&uri, &diagnostic);
        assert!(result.is_some());

        let action = result.unwrap();
        assert_eq!(action.title, "Remove unused definition");
        assert_eq!(action.kind, Some(CodeActionKind::QUICKFIX));
        assert_eq!(action.is_preferred, Some(true));

        let edit = action.edit.unwrap();
        let changes = edit.changes.unwrap();
        let edits = &changes[&uri];
        assert_eq!(edits.len(), 1);

        assert_eq!(edits[0].range.start.line, 1);
        assert_eq!(edits[0].range.start.character, 0);
        assert_eq!(edits[0].range.end.line, 2);
        assert_eq!(edits[0].range.end.character, 0);
        assert_eq!(edits[0].new_text, "");
    }

    #[tokio::test]
    async fn test_suggest_optional_return_type() {
        let (provider, uri) = create_test_provider().await;
        let documents = provider.documents.clone();

        let source = "def foo() -> str:\n    return None\n";
        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let diagnostic = lsp_types::Diagnostic {
            range: Range { start: Position { line: 0, character: 13 }, end: Position { line: 0, character: 16 } },
            severity: Some(lsp_types::DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::String("HM001".to_string())),
            message: "Type mismatch: expected str, got None".to_string(),
            source: Some("beacon".to_string()),
            ..Default::default()
        };

        let result = provider.suggest_optional(&uri, &diagnostic);
        assert!(result.is_some());

        let action = result.unwrap();
        assert_eq!(action.title, "Wrap type with Optional");
        assert_eq!(action.kind, Some(CodeActionKind::QUICKFIX));
        assert_eq!(action.is_preferred, Some(true));

        let edit = action.edit.unwrap();
        let changes = edit.changes.unwrap();
        let edits = &changes[&uri];

        assert!(edits.iter().any(|e| e.new_text.contains("Optional[str]")));
        assert!(edits.iter().any(|e| e.new_text.contains("from typing import Optional")));
    }

    #[tokio::test]
    async fn test_suggest_optional_variable_type() {
        let (provider, uri) = create_test_provider().await;
        let documents = provider.documents.clone();

        let source = "x: str = \"hello\"\nx = None\n";
        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let diagnostic = lsp_types::Diagnostic {
            range: Range { start: Position { line: 0, character: 3 }, end: Position { line: 0, character: 6 } },
            severity: Some(lsp_types::DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::String("HM001".to_string())),
            message: "Type mismatch: expected str, got None".to_string(),
            source: Some("beacon".to_string()),
            ..Default::default()
        };

        let result = provider.suggest_optional(&uri, &diagnostic);
        assert!(result.is_some());

        let action = result.unwrap();
        assert_eq!(action.title, "Wrap type with Optional");

        let edit = action.edit.unwrap();
        let changes = edit.changes.unwrap();
        let edits = &changes[&uri];
        assert!(edits.iter().any(|e| e.new_text.contains("Optional[str]")));
    }

    #[tokio::test]
    async fn test_suggest_optional_when_optional_exists() {
        let (provider, uri) = create_test_provider().await;
        let documents = provider.documents.clone();

        let source = "from typing import Optional\nx: str = \"hello\"\n";
        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let diagnostic = lsp_types::Diagnostic {
            range: Range { start: Position { line: 1, character: 3 }, end: Position { line: 1, character: 6 } },
            severity: Some(lsp_types::DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::String("HM001".to_string())),
            message: "Type mismatch: expected str, got None".to_string(),
            source: Some("beacon".to_string()),
            ..Default::default()
        };

        let result = provider.suggest_optional(&uri, &diagnostic);
        assert!(result.is_some());

        let action = result.unwrap();
        let edit = action.edit.unwrap();
        let changes = edit.changes.unwrap();
        let edits = &changes[&uri];

        let import_edits: Vec<_> = edits
            .iter()
            .filter(|e| e.new_text.contains("from typing import"))
            .collect();
        assert_eq!(import_edits.len(), 0, "Should not add duplicate import");
    }

    #[tokio::test]
    async fn test_wrap_return_type_with_optional() {
        let (provider, _) = create_test_provider().await;

        let line = "def foo() -> str:";
        let result = provider.wrap_return_type_with_optional(line, 0);

        assert!(result.is_some());
        let edit = result.unwrap();
        assert_eq!(edit.new_text, " Optional[str]");
        assert_eq!(edit.range.start.line, 0);
    }

    #[tokio::test]
    async fn test_wrap_return_type_already_optional() {
        let (provider, _) = create_test_provider().await;

        let line = "def foo() -> Optional[str]:";
        let result = provider.wrap_return_type_with_optional(line, 0);

        assert!(result.is_none());
    }

    #[tokio::test]
    async fn test_wrap_return_type_complex() {
        let (provider, _) = create_test_provider().await;

        let line = "def foo() -> list[int]:";
        let result = provider.wrap_return_type_with_optional(line, 0);

        assert!(result.is_some());
        let edit = result.unwrap();
        assert_eq!(edit.new_text, " Optional[list[int]]");
    }

    #[tokio::test]
    async fn test_wrap_variable_type_with_optional() {
        let (provider, _) = create_test_provider().await;

        let line = "x: str = \"hello\"";
        let result = provider.wrap_variable_type_with_optional(line, 0);

        assert!(result.is_some());
        let edit = result.unwrap();
        assert_eq!(edit.new_text, " Optional[str]");
        assert_eq!(edit.range.start.line, 0);
    }

    #[tokio::test]
    async fn test_wrap_variable_type_already_optional() {
        let (provider, _) = create_test_provider().await;

        let line = "x: Optional[str] = None";
        let result = provider.wrap_variable_type_with_optional(line, 0);

        assert!(result.is_none());
    }

    #[tokio::test]
    async fn test_wrap_variable_type_no_equals() {
        let (provider, _) = create_test_provider().await;

        let line = "x: str";
        let result = provider.wrap_variable_type_with_optional(line, 0);

        assert!(result.is_some());
        let edit = result.unwrap();
        assert_eq!(edit.new_text, " Optional[str]");
    }

    #[tokio::test]
    async fn test_add_optional_import_no_imports() {
        let (provider, _) = create_test_provider().await;

        let source = "x = 42\ny = 10\n";
        let result = provider.add_optional_import(source);

        assert!(result.is_some());
        let edit = result.unwrap();
        assert_eq!(edit.new_text, "from typing import Optional\n");
        assert_eq!(edit.range.start.line, 0);
        assert_eq!(edit.range.start.character, 0);
    }

    #[tokio::test]
    async fn test_add_optional_import_after_existing_imports() {
        let (provider, _) = create_test_provider().await;

        let source = "import os\nimport sys\nfrom pathlib import Path\n\nx = 42\n";
        let result = provider.add_optional_import(source);

        assert!(result.is_some());
        let edit = result.unwrap();
        assert_eq!(edit.new_text, "from typing import Optional\n");
        assert_eq!(edit.range.start.line, 3);
    }

    #[tokio::test]
    async fn test_add_missing_patterns() {
        let (provider, uri) = create_test_provider().await;
        let documents = provider.documents.clone();

        let source = "def foo(x):\n    match x:\n        case int():\n            return 1\n";
        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let diagnostic = lsp_types::Diagnostic {
            range: Range { start: Position { line: 1, character: 4 }, end: Position { line: 1, character: 9 } },
            severity: Some(lsp_types::DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::String("PM001".to_string())),
            message: "Pattern match not exhaustive. Missing coverage for: str, bool".to_string(),
            source: Some("beacon".to_string()),
            ..Default::default()
        };

        let result = provider.add_missing_patterns(&uri, &diagnostic);
        assert!(result.is_some());

        let action = result.unwrap();
        assert_eq!(action.title, "Add missing pattern cases");
        assert_eq!(action.kind, Some(CodeActionKind::QUICKFIX));
        assert_eq!(action.is_preferred, Some(true));

        let edit = action.edit.unwrap();
        let changes = edit.changes.unwrap();
        let edits = &changes[&uri];
        assert_eq!(edits.len(), 1);

        assert!(edits[0].new_text.contains("case _ if isinstance(_, str):"));
        assert!(edits[0].new_text.contains("case _ if isinstance(_, bool):"));
        assert!(edits[0].new_text.contains("pass  # TODO: Handle"));
    }

    #[tokio::test]
    async fn test_add_missing_patterns_single_type() {
        let (provider, uri) = create_test_provider().await;
        let documents = provider.documents.clone();

        let source = "def foo(x):\n    match x:\n        case int():\n            return 1\n";
        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let diagnostic = lsp_types::Diagnostic {
            range: Range { start: Position { line: 1, character: 4 }, end: Position { line: 1, character: 9 } },
            severity: Some(lsp_types::DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::String("PM001".to_string())),
            message: "Pattern match not exhaustive. Missing coverage for: str".to_string(),
            source: Some("beacon".to_string()),
            ..Default::default()
        };

        let result = provider.add_missing_patterns(&uri, &diagnostic);
        assert!(result.is_some());

        let action = result.unwrap();
        assert_eq!(action.title, "Add missing pattern case");
    }

    #[tokio::test]
    async fn test_generate_pattern_for_type_uppercase() {
        let (provider, _) = create_test_provider().await;

        let pattern = provider.generate_pattern_for_type("MyClass");
        assert_eq!(pattern, "MyClass()");
    }

    #[tokio::test]
    async fn test_generate_pattern_for_type_lowercase() {
        let (provider, _) = create_test_provider().await;

        let pattern = provider.generate_pattern_for_type("str");
        assert_eq!(pattern, "_ if isinstance(_, str)");
    }

    #[tokio::test]
    async fn test_generate_pattern_for_type_builtin() {
        let (provider, _) = create_test_provider().await;

        assert_eq!(provider.generate_pattern_for_type("int"), "_ if isinstance(_, int)");
        assert_eq!(provider.generate_pattern_for_type("float"), "_ if isinstance(_, float)");
        assert_eq!(provider.generate_pattern_for_type("bool"), "_ if isinstance(_, bool)");
    }

    #[tokio::test]
    async fn test_implement_protocol_methods() {
        let (provider, uri) = create_test_provider().await;
        let documents = provider.documents.clone();

        let source = "class MyClass:\n    pass\n\ndef other():\n    pass\n";
        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let diagnostic = lsp_types::Diagnostic {
            range: Range { start: Position { line: 0, character: 6 }, end: Position { line: 0, character: 13 } },
            severity: Some(lsp_types::DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::String("HM006".to_string())),
            message: "Type MyClass does not satisfy protocol Iterable".to_string(),
            source: Some("beacon".to_string()),
            ..Default::default()
        };

        let result = provider.implement_protocol_methods(&uri, &diagnostic);
        assert!(result.is_some());

        let action = result.unwrap();
        assert_eq!(action.title, "Implement Iterable protocol methods");
        assert_eq!(action.kind, Some(CodeActionKind::QUICKFIX));
        assert_eq!(action.is_preferred, Some(true));

        let edit = action.edit.unwrap();
        let changes = edit.changes.unwrap();
        let edits = &changes[&uri];
        assert_eq!(edits.len(), 1);

        assert!(edits[0].new_text.contains("def __iter__(self):"));
        assert!(edits[0].new_text.contains("raise NotImplementedError"));
        assert_eq!(edits[0].range.start.line, 3);
    }

    #[tokio::test]
    async fn test_implement_protocol_methods_iterator() {
        let (provider, uri) = create_test_provider().await;
        let documents = provider.documents.clone();

        let source = "class MyIterator:\n    def __init__(self):\n        pass\n";
        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let diagnostic = lsp_types::Diagnostic {
            range: Range { start: Position { line: 0, character: 6 }, end: Position { line: 0, character: 16 } },
            severity: Some(lsp_types::DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::String("HM006".to_string())),
            message: "Type MyIterator does not satisfy protocol Iterator".to_string(),
            source: Some("beacon".to_string()),
            ..Default::default()
        };

        let result = provider.implement_protocol_methods(&uri, &diagnostic);
        assert!(result.is_some());

        let action = result.unwrap();
        let edit = action.edit.unwrap();
        let changes = edit.changes.unwrap();
        let edits = &changes[&uri];

        assert!(edits[0].new_text.contains("def __iter__(self):"));
        assert!(edits[0].new_text.contains("return self"));
        assert!(edits[0].new_text.contains("def __next__(self):"));
    }

    #[tokio::test]
    async fn test_implement_protocol_methods_sized() {
        let (provider, uri) = create_test_provider().await;
        let documents = provider.documents.clone();

        let source = "class MyContainer:\n    pass\n";
        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let diagnostic = lsp_types::Diagnostic {
            range: Range { start: Position { line: 0, character: 6 }, end: Position { line: 0, character: 17 } },
            severity: Some(lsp_types::DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::String("HM006".to_string())),
            message: "Type MyContainer does not satisfy protocol Sized".to_string(),
            source: Some("beacon".to_string()),
            ..Default::default()
        };

        let result = provider.implement_protocol_methods(&uri, &diagnostic);
        assert!(result.is_some());

        let action = result.unwrap();
        let edit = action.edit.unwrap();
        let changes = edit.changes.unwrap();
        let edits = &changes[&uri];

        assert!(edits[0].new_text.contains("def __len__(self) -> int:"));
    }
}

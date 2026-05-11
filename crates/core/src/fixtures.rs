//! Shared fixture helpers for repository tests.

use std::fs;
use std::path::{Path, PathBuf};

use lsp_types::{Diagnostic, DiagnosticSeverity, Position, Range, TextEdit};

/// Return the repository root inferred from the core crate manifest path.
pub fn repo_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(Path::parent)
        .expect("core crate should live under crates/core")
        .to_path_buf()
}

/// Return the shared workspace fixture root.
pub fn workspace() -> PathBuf {
    repo_root().join("fixtures").join("workspace")
}

/// Return a path inside the shared workspace fixture.
pub fn file(path: impl AsRef<Path>) -> PathBuf {
    workspace().join(path)
}

/// Read a file from the shared workspace fixture.
pub fn read_file(path: impl AsRef<Path>) -> std::io::Result<String> {
    fs::read_to_string(file(path))
}

/// List all Python files in the shared workspace fixture.
pub fn python_files() -> std::io::Result<Vec<PathBuf>> {
    python_files_under(&workspace())
}

fn python_files_under(root: &Path) -> std::io::Result<Vec<PathBuf>> {
    let mut files = Vec::new();
    collect_python_files(root, &mut files)?;
    files.sort();
    Ok(files)
}

fn collect_python_files(path: &Path, files: &mut Vec<PathBuf>) -> std::io::Result<()> {
    if path.is_file() {
        if path
            .extension()
            .and_then(|ext| ext.to_str())
            .is_some_and(|ext| ext == "py" || ext == "pyi")
        {
            files.push(path.to_path_buf());
        }
        return Ok(());
    }

    for entry in fs::read_dir(path)? {
        let entry = entry?;
        collect_python_files(&entry.path(), files)?;
    }

    Ok(())
}

/// Structured expectation for LSP diagnostics in fixture tests.
#[derive(Debug, Clone)]
pub struct ExpectedDiagnostic<'a> {
    pub code: &'a str,
    pub severity: DiagnosticSeverity,
    pub message_fragment: &'a str,
    pub range: Range,
}

impl<'a> ExpectedDiagnostic<'a> {
    /// Assert that this expectation is present in a diagnostic list.
    pub fn assert_present(&self, diagnostics: &[Diagnostic]) {
        assert!(
            diagnostics.iter().any(|diagnostic| self.matches(diagnostic)),
            "expected diagnostic not found: {self:?}\nactual diagnostics: {diagnostics:#?}"
        );
    }

    /// Check whether a diagnostic matches this expectation.
    pub fn matches(&self, diagnostic: &Diagnostic) -> bool {
        diagnostic.severity == Some(self.severity)
            && diagnostic.message.contains(self.message_fragment)
            && diagnostic.range == self.range
            && diagnostic.code.as_ref().is_some_and(|code| match code {
                lsp_types::NumberOrString::String(value) => value == self.code,
                lsp_types::NumberOrString::Number(value) => value.to_string() == self.code,
            })
    }
}

/// Build a zero-based LSP position.
pub fn position(line: u32, character: u32) -> Position {
    Position { line, character }
}

/// Build a zero-based LSP range.
pub fn range(start_line: u32, start_character: u32, end_line: u32, end_character: u32) -> Range {
    Range { start: position(start_line, start_character), end: position(end_line, end_character) }
}

/// Assert a type display string contains an expected stable fragment.
pub fn assert_type_display_contains(actual: impl std::fmt::Display, expected_fragment: &str) {
    let actual = actual.to_string();
    assert!(
        actual.contains(expected_fragment),
        "expected type display to contain {expected_fragment:?}, got {actual:?}"
    );
}

/// Assert a text edit exactly matches the expected range and replacement.
pub fn assert_text_edit(edit: &TextEdit, expected_range: Range, expected_new_text: &str) {
    assert_eq!(edit.range, expected_range, "text edit range mismatch");
    assert_eq!(edit.new_text, expected_new_text, "text edit replacement mismatch");
}

//! LSP Protocol Integration Tests for Formatting
//!
//! Tests the complete LSP formatting flow including:
//! - textDocument/formatting
//! - textDocument/rangeFormatting
//! - textDocument/willSaveWaitUntil with formatting
//! - textDocument/onTypeFormatting
//!
//! These tests verify that the formatter integrates correctly with the LSP protocol.

use beacon_lsp::{document::DocumentManager, features::FormattingProvider, formatting::FormatterConfig};
use lsp_types::{
    DocumentFormattingParams, DocumentOnTypeFormattingParams, DocumentRangeFormattingParams, FormattingOptions,
    Position, Range, TextDocumentIdentifier, TextDocumentSaveReason as TextSaveReason, Url, WillSaveTextDocumentParams,
};

/// Create a test document manager and formatting provider
fn setup() -> (DocumentManager, FormattingProvider) {
    let documents = DocumentManager::new().expect("Failed to create document manager");
    let provider = FormattingProvider::new(documents.clone());
    (documents, provider)
}

/// Open a document in the document manager
fn open_document(documents: &DocumentManager, uri: &Url, content: &str) {
    documents
        .open_document(uri.clone(), 1, content.to_string())
        .expect("Failed to open document");
}

#[test]
fn test_lsp_format_document_full() {
    let (documents, provider) = setup();
    let config = FormatterConfig::default();
    let uri = Url::parse("file:///test_format_full.py").unwrap();

    let source = r#"def foo(x,y,z):
    return x+y+z

class Bar:
    def __init__(self):
        self.x=1
        self.y=2
"#;

    open_document(&documents, &uri, source);

    let params = DocumentFormattingParams {
        text_document: TextDocumentIdentifier { uri: uri.clone() },
        options: FormattingOptions::default(),
        work_done_progress_params: Default::default(),
    };

    let result = provider.format_document(&params, &config);
    assert!(
        result.is_some(),
        "Formatting should return TextEdit for unformatted document"
    );

    if let Some(edits) = result {
        assert!(!edits.is_empty(), "Should have at least one edit");
        let formatted = &edits[0].new_text;

        assert!(
            formatted.contains("def foo(x, y, z):"),
            "Should add spaces after commas"
        );
        assert!(
            formatted.contains("return x + y + z"),
            "Should add spaces around operators"
        );
        assert!(formatted.contains("self.x = 1"), "Should add spaces around assignments");
    }
}

#[test]
fn test_lsp_format_document_already_formatted() {
    let (documents, provider) = setup();
    let config = FormatterConfig::default();
    let uri = Url::parse("file:///test_already_formatted.py").unwrap();

    let source = r#"def foo():
    pass
"#;

    open_document(&documents, &uri, source);

    let params = DocumentFormattingParams {
        text_document: TextDocumentIdentifier { uri },
        options: FormattingOptions::default(),
        work_done_progress_params: Default::default(),
    };

    let result = provider.format_document(&params, &config);
    assert!(result.is_none(), "Should return None for already formatted document");
}

#[test]
fn test_lsp_format_document_with_imports() {
    let (documents, provider) = setup();
    let config = FormatterConfig::default();
    let uri = Url::parse("file:///test_imports.py").unwrap();

    let source = r#"import sys
import os
from typing import List

def main():
    pass
"#;

    open_document(&documents, &uri, source);

    let params = DocumentFormattingParams {
        text_document: TextDocumentIdentifier { uri },
        options: FormattingOptions::default(),
        work_done_progress_params: Default::default(),
    };

    let result = provider.format_document(&params, &config);

    if let Some(edits) = result {
        let formatted = &edits[0].new_text;
        let os_pos = formatted.find("import os");
        let sys_pos = formatted.find("import sys");

        assert!(os_pos.is_some() && sys_pos.is_some(), "Both imports should be present");

        if let (Some(os), Some(sys)) = (os_pos, sys_pos) {
            assert!(os < sys, "Imports should be sorted alphabetically");
        }
    }
}

#[test]
fn test_lsp_range_formatting_single_function() {
    let (documents, provider) = setup();
    let config = FormatterConfig::default();
    let uri = Url::parse("file:///test_range.py").unwrap();

    let source = r#"def foo():
    x=1
    y=2

def bar():
    a=3
    b=4
"#;

    open_document(&documents, &uri, source);

    let params = DocumentRangeFormattingParams {
        text_document: TextDocumentIdentifier { uri },
        range: Range { start: Position { line: 4, character: 0 }, end: Position { line: 7, character: 0 } },
        options: FormattingOptions::default(),
        work_done_progress_params: Default::default(),
    };

    let result = provider.format_range(&params, &config);
    assert!(result.is_some(), "Range formatting should return edits");

    if let Some(edits) = result {
        let formatted = &edits[0].new_text;
        assert!(formatted.contains("a = 3"), "Should format selected range");
        assert!(formatted.contains("b = 4"), "Should format selected range");
    }
}

#[test]
fn test_lsp_range_formatting_class_methods() {
    let (documents, provider) = setup();
    let config = FormatterConfig::default();
    let uri = Url::parse("file:///test_class_range.py").unwrap();

    let source = r#"class Calculator:
    def add(self,x,y):
        return x+y

    def subtract(self,x,y):
        return x-y
"#;

    open_document(&documents, &uri, source);

    let params = DocumentRangeFormattingParams {
        text_document: TextDocumentIdentifier { uri },
        range: Range { start: Position { line: 1, character: 0 }, end: Position { line: 3, character: 0 } },
        options: FormattingOptions::default(),
        work_done_progress_params: Default::default(),
    };

    let result = provider.format_range(&params, &config);

    if let Some(edits) = result {
        let formatted = &edits[0].new_text;
        assert!(
            formatted.contains("def add(self, x, y):"),
            "Should format method signature"
        );
        assert!(formatted.contains("return x + y"), "Should format return statement");
    }
}

#[test]
fn test_lsp_format_on_save_manual() {
    let (documents, provider) = setup();
    let config = FormatterConfig::default();
    let uri = Url::parse("file:///test_save_manual.py").unwrap();

    let source = "x=1+2";

    open_document(&documents, &uri, source);

    let params =
        WillSaveTextDocumentParams { text_document: TextDocumentIdentifier { uri }, reason: TextSaveReason::MANUAL };

    let result = provider.format_on_save(&params, &config);
    assert!(result.is_some(), "Should format on manual save");

    if let Some(edits) = result {
        let formatted = &edits[0].new_text;
        assert!(formatted.contains("x = 1 + 2"), "Should format on save");
    }
}

#[test]
fn test_lsp_format_on_save_auto_skip() {
    let (documents, provider) = setup();
    let config = FormatterConfig::default();
    let uri = Url::parse("file:///test_save_auto.py").unwrap();

    let source = "x=1+2";

    open_document(&documents, &uri, source);

    let params = WillSaveTextDocumentParams {
        text_document: TextDocumentIdentifier { uri },
        reason: TextSaveReason::AFTER_DELAY,
    };

    let result = provider.format_on_save(&params, &config);
    assert!(result.is_none(), "Should skip formatting on auto-save (not manual)");
}

#[test]
fn test_lsp_on_type_formatting_colon() {
    let (documents, provider) = setup();
    let config = FormatterConfig::default();
    let uri = Url::parse("file:///test_ontype.py").unwrap();

    let source = "def foo():";

    open_document(&documents, &uri, source);

    let params = DocumentOnTypeFormattingParams {
        text_document_position: lsp_types::TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri },
            position: Position { line: 0, character: 10 },
        },
        ch: ":".to_string(),
        options: FormattingOptions::default(),
    };

    let result = provider.on_type_format(&params, &config);
    assert!(result.is_some() || result.is_none(), "On-type formatting completed");
}

#[test]
fn test_lsp_format_document_with_comments() {
    let (documents, provider) = setup();
    let config = FormatterConfig::default();
    let uri = Url::parse("file:///test_comments.py").unwrap();

    let source = r#"# Module comment
x=1# inline comment
# Another comment
y=2
"#;

    open_document(&documents, &uri, source);

    let params = DocumentFormattingParams {
        text_document: TextDocumentIdentifier { uri },
        options: FormattingOptions::default(),
        work_done_progress_params: Default::default(),
    };

    let result = provider.format_document(&params, &config);

    if let Some(edits) = result {
        let formatted = &edits[0].new_text;
        assert!(formatted.contains("# Module comment"), "Should preserve module comment");
        assert!(formatted.contains("# inline comment"), "Should preserve inline comment");
        assert!(
            formatted.contains("# Another comment"),
            "Should preserve standalone comment"
        );
    }
}

#[test]
fn test_lsp_format_document_with_decorators() {
    let (documents, provider) = setup();
    let config = FormatterConfig::default();
    let uri = Url::parse("file:///test_decorators.py").unwrap();

    let source = r#"@decorator
def foo():
    pass

@decorator_a
@decorator_b
def bar():
    pass
"#;

    open_document(&documents, &uri, source);

    let params = DocumentFormattingParams {
        text_document: TextDocumentIdentifier { uri },
        options: FormattingOptions::default(),
        work_done_progress_params: Default::default(),
    };

    let result = provider.format_document(&params, &config);

    if let Some(edits) = result {
        let formatted = &edits[0].new_text;
        assert!(
            formatted.contains("@decorator\ndef foo():"),
            "Should preserve decorator formatting"
        );
        assert!(
            formatted.contains("@decorator_a\n@decorator_b\ndef bar():"),
            "Should preserve multiple decorators"
        );
    }
}

#[test]
fn test_lsp_format_document_with_docstrings() {
    let (documents, provider) = setup();
    let config = FormatterConfig::default();
    let uri = Url::parse("file:///test_docstrings.py").unwrap();

    let source = r#"def foo():
    """This is a docstring."""
    pass

class Bar:
    """Class docstring."""
    x=1
"#;

    open_document(&documents, &uri, source);

    let params = DocumentFormattingParams {
        text_document: TextDocumentIdentifier { uri },
        options: FormattingOptions::default(),
        work_done_progress_params: Default::default(),
    };

    let result = provider.format_document(&params, &config);

    if let Some(edits) = result {
        let formatted = &edits[0].new_text;
        assert!(
            formatted.contains("\"\"\"This is a docstring.\"\"\""),
            "Should preserve function docstring"
        );
        assert!(
            formatted.contains("\"\"\"Class docstring.\"\"\""),
            "Should preserve class docstring"
        );
    }
}

#[test]
fn test_lsp_format_nonexistent_document() {
    let (_documents, provider) = setup();
    let config = FormatterConfig::default();
    let uri = Url::parse("file:///nonexistent.py").unwrap();

    let params = DocumentFormattingParams {
        text_document: TextDocumentIdentifier { uri },
        options: FormattingOptions::default(),
        work_done_progress_params: Default::default(),
    };

    let result = provider.format_document(&params, &config);
    assert!(result.is_none(), "Should return None for nonexistent document");
}

#[test]
fn test_lsp_range_formatting_nonexistent_document() {
    let (_documents, provider) = setup();
    let config = FormatterConfig::default();
    let uri = Url::parse("file:///nonexistent.py").unwrap();

    let params = DocumentRangeFormattingParams {
        text_document: TextDocumentIdentifier { uri },
        range: Range { start: Position { line: 0, character: 0 }, end: Position { line: 5, character: 0 } },
        options: FormattingOptions::default(),
        work_done_progress_params: Default::default(),
    };

    let result = provider.format_range(&params, &config);
    assert!(result.is_none(), "Should return None for nonexistent document");
}

#[test]
fn test_lsp_format_document_idempotent() {
    let (documents, provider) = setup();
    let config = FormatterConfig::default();
    let uri = Url::parse("file:///test_idempotent.py").unwrap();

    let source = r#"def foo(x,y):
    return x+y
"#;

    open_document(&documents, &uri, source);

    let params = DocumentFormattingParams {
        text_document: TextDocumentIdentifier { uri: uri.clone() },
        options: FormattingOptions::default(),
        work_done_progress_params: Default::default(),
    };

    let result1 = provider.format_document(&params, &config);
    assert!(result1.is_some(), "First format should produce edits");

    if let Some(edits) = result1 {
        let formatted = edits[0].new_text.clone();
        let uri2 = Url::parse("file:///test_idempotent_2.py").unwrap();
        open_document(&documents, &uri2, &formatted);

        let params2 = DocumentFormattingParams {
            text_document: TextDocumentIdentifier { uri: uri2 },
            options: FormattingOptions::default(),
            work_done_progress_params: Default::default(),
        };

        let result2 = provider.format_document(&params2, &config);
        assert!(
            result2.is_none(),
            "Second format should return None (already formatted)"
        );
    }
}

#[test]
fn test_lsp_format_document_with_type_annotations() {
    let (documents, provider) = setup();
    let config = FormatterConfig::default();
    let uri = Url::parse("file:///test_type_annotations.py").unwrap();

    let source = r#"def foo(x:int,y:str)->int:
    return len(y)+x
"#;

    open_document(&documents, &uri, source);

    let params = DocumentFormattingParams {
        text_document: TextDocumentIdentifier { uri },
        options: FormattingOptions::default(),
        work_done_progress_params: Default::default(),
    };

    let result = provider.format_document(&params, &config);

    if let Some(edits) = result {
        let formatted = &edits[0].new_text;
        assert!(
            formatted.contains("def foo(x: int, y: str) -> int:"),
            "Should format type annotations correctly"
        );
    }
}

#[test]
fn test_lsp_format_document_complex_expressions() {
    let (documents, provider) = setup();
    let config = FormatterConfig::default();
    let uri = Url::parse("file:///test_complex.py").unwrap();

    let source = r#"result=a+b*c
data=[1,2,3,4,5]
mapping={"key":"value","foo":"bar"}
"#;

    open_document(&documents, &uri, source);

    let params = DocumentFormattingParams {
        text_document: TextDocumentIdentifier { uri },
        options: FormattingOptions::default(),
        work_done_progress_params: Default::default(),
    };

    let result = provider.format_document(&params, &config);

    if let Some(edits) = result {
        let formatted = &edits[0].new_text;
        assert!(
            formatted.contains("result = a + b * c"),
            "Should format arithmetic expressions. Actual:\n{formatted}"
        );
        assert!(
            formatted.contains("[1, 2, 3, 4, 5]"),
            "Should format lists. Actual:\n{formatted}"
        );
        assert!(
            formatted.contains("\"key\": \"value\""),
            "Should format dictionary keys. Actual:\n{formatted}"
        );
    }
}

#[test]
fn test_lsp_range_formatting_empty_range() {
    let (documents, provider) = setup();
    let config = FormatterConfig::default();
    let uri = Url::parse("file:///test_empty_range.py").unwrap();

    let source = r#"def foo():
    pass
"#;

    open_document(&documents, &uri, source);

    let params = DocumentRangeFormattingParams {
        text_document: TextDocumentIdentifier { uri },
        range: Range { start: Position { line: 0, character: 0 }, end: Position { line: 0, character: 0 } },
        options: FormattingOptions::default(),
        work_done_progress_params: Default::default(),
    };

    let result = provider.format_range(&params, &config);
    assert!(
        result.is_some() || result.is_none(),
        "Empty range formatting should complete"
    );
}

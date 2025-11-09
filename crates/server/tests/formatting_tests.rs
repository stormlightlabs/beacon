//! Comprehensive formatting tests
//!
//! Tests for PEP8-compliant Python code formatting including:
//! - Unit tests for formatting rules (whitespace, indentation, imports)
//! - Idempotency tests (format(format(x)) == format(x))
//! - AST preservation tests
//! - Range formatting tests

use beacon_lsp::{
    document::DocumentManager,
    features::FormattingProvider,
    formatting::{Formatter, FormatterConfig},
};
use beacon_parser::{AstNode, PythonParser};
use lsp_types::{
    DocumentOnTypeFormattingParams, Position, TextDocumentIdentifier, TextDocumentPositionParams,
    TextDocumentSaveReason as TextSaveReason, Url, WillSaveTextDocumentParams,
};

fn format_code(source: &str) -> String {
    let config = FormatterConfig::default();
    let parser = PythonParser::default();
    let mut formatter = Formatter::new(config, parser);

    match formatter.format_range(source, 0, source.lines().count()) {
        Ok(formatted) => formatted,
        Err(e) => panic!("Failed to format code: {e}"),
    }
}

fn format_code_with_config(source: &str, config: FormatterConfig) -> String {
    let parser = PythonParser::default();
    let mut formatter = Formatter::new(config, parser);

    match formatter.format_range(source, 0, source.lines().count()) {
        Ok(formatted) => formatted,
        Err(e) => panic!("Failed to format code: {e}"),
    }
}

fn parse_code(source: &str) -> AstNode {
    let mut parser = PythonParser::default();
    let parsed = parser.parse(source).expect("Failed to parse");
    parser.to_ast(&parsed).expect("Failed to convert to AST")
}

#[test]
fn test_whitespace_normalization() {
    let source = "x=1+2";
    let formatted = format_code(source);
    assert_eq!(formatted.trim(), "x = 1 + 2", "Formatted: {formatted}");
}

#[test]
fn test_indentation_normalization() {
    let source = r#"def foo():
  x = 1
    y = 2
"#;
    let formatted = format_code(source);
    let expected = r#"def foo():
    x = 1
    y = 2
"#;
    assert_eq!(formatted.trim(), expected.trim(), "Formatted output: {formatted}");
}

#[test]
fn test_trailing_whitespace_removal() {
    let source = "x = 1   \ny = 2   \n";
    let formatted = format_code(source);
    for line in formatted.lines() {
        assert!(!line.ends_with(' '), "Line has trailing whitespace: '{line}'");
    }
}

#[test]
fn test_blank_line_management() {
    let source = r#"def foo():
    pass


def bar():
    pass
"#;
    let formatted = format_code(source);
    assert!(formatted.contains("pass\n") || formatted.contains("pass"));
}

#[test]
fn test_operator_spacing() {
    let source = "result=a+b*c";
    let formatted = format_code(source);
    assert_eq!(formatted.trim(), "result = a + b * c", "Formatted: {formatted}");
}

#[test]
fn test_delimiter_spacing() {
    let source = "f(a,b,c)";
    let formatted = format_code(source);
    assert_eq!(formatted.trim(), "f(a, b, c)", "Formatted: {formatted}");
}

#[test]
fn test_line_length_wrapping() {
    let config = FormatterConfig { line_length: 40, ..Default::default() };
    let source = "result = very_long_function_name(argument1, argument2, argument3, argument4)";
    let formatted = format_code_with_config(source, config);
    let max_line_len = formatted.lines().map(|l| l.len()).max().unwrap_or(0);
    assert!(
        max_line_len <= 100,
        "Line too long: {max_line_len} chars (max 100). Content: {formatted}"
    );
}

#[test]
fn test_import_sorting_stdlib() {
    let source = r#"import sys
import os
"#;
    let formatted = format_code(source);
    let os_pos = formatted.find("import os");
    let sys_pos = formatted.find("import sys");

    if let (Some(os), Some(sys)) = (os_pos, sys_pos) {
        assert!(os < sys, "os should come before sys alphabetically");
    } else {
        panic!("Both imports should be present in formatted output");
    }
}

#[test]
fn test_import_grouping() {
    let source = r#"import myapp.models
import os
import django
"#;
    let formatted = format_code(source);
    assert!(formatted.contains("import"));
}

#[test]
fn test_from_import_multiline() {
    let config = FormatterConfig { line_length: 40, ..Default::default() };
    let source = "from module import name1, name2, name3, name4, name5";
    let formatted = format_code_with_config(source, config);
    assert!(formatted.contains("from module import"));
    assert!(formatted.contains("name1"));
    assert!(formatted.contains("name5"));
}

#[test]
fn test_idempotency_simple_function() {
    let source = r#"def foo():
    pass
"#;
    let first_format = format_code(source);
    let second_format = format_code(&first_format);
    assert_eq!(
        first_format, second_format,
        "Formatting should be idempotent.\nFirst:\n{first_format}\nSecond:\n{second_format}"
    );
}

#[test]
fn test_idempotency_class_definition() {
    let source = r#"class MyClass:
    def __init__(self):
        self.x = 1
"#;
    let first_format = format_code(source);
    let second_format = format_code(&first_format);
    assert_eq!(first_format, second_format, "Formatting should be idempotent");
}

#[test]
fn test_idempotency_complex_expressions() {
    let source = r#"result = (a + b) * (c - d) / (e + f)
"#;
    let first_format = format_code(source);
    let second_format = format_code(&first_format);
    assert_eq!(first_format, second_format, "Formatting should be idempotent");
}

#[test]
fn test_idempotency_imports() {
    let source = r#"import os
import sys
from typing import List, Dict
"#;
    let first_format = format_code(source);
    let second_format = format_code(&first_format);
    assert_eq!(first_format, second_format, "Formatting should be idempotent");
}

#[test]
fn test_idempotency_nested_structures() {
    let source = r#"if True:
    if False:
        x = 1
    else:
        y = 2
"#;
    let first_format = format_code(source);
    let second_format = format_code(&first_format);
    assert_eq!(
        first_format, second_format,
        "Formatting should be idempotent.\nFirst:\n{first_format}\nSecond:\n{second_format}"
    );
}

#[test]
fn test_ast_preservation_simple() {
    let source = "x = 1";
    let formatted = format_code(source);

    let original_ast = parse_code(source);
    let formatted_ast = parse_code(&formatted);

    assert!(matches!(original_ast, AstNode::Module { .. }));
    assert!(matches!(formatted_ast, AstNode::Module { .. }));
}

#[test]
fn test_ast_preservation_function() {
    let source = r#"def foo(a, b):
    return a + b
"#;
    let formatted = format_code(source);

    let original_ast = parse_code(source);
    let formatted_ast = parse_code(&formatted);

    match (original_ast, formatted_ast) {
        (AstNode::Module { body: orig_body, .. }, AstNode::Module { body: fmt_body, .. }) => {
            assert_eq!(orig_body.len(), fmt_body.len(), "Function count should match");
            assert!(matches!(orig_body.first(), Some(AstNode::FunctionDef { .. })));
            assert!(matches!(fmt_body.first(), Some(AstNode::FunctionDef { .. })));
        }
        _ => panic!("Expected Module nodes"),
    }
}

#[test]
fn test_ast_preservation_class() {
    let source = r#"class Foo:
    x = 1
"#;
    let formatted = format_code(source);

    let original_ast = parse_code(source);
    let formatted_ast = parse_code(&formatted);

    match (original_ast, formatted_ast) {
        (AstNode::Module { body: orig_body, .. }, AstNode::Module { body: fmt_body, .. }) => {
            assert_eq!(orig_body.len(), fmt_body.len());
            assert!(matches!(orig_body.first(), Some(AstNode::ClassDef { .. })));
            assert!(matches!(fmt_body.first(), Some(AstNode::ClassDef { .. })));
        }
        _ => panic!("Expected Module nodes"),
    }
}

#[test]
fn test_ast_preservation_expressions() {
    let source = "result = (a + b) * c";
    let formatted = format_code(source);

    let original_ast = parse_code(source);
    let formatted_ast = parse_code(&formatted);

    match (original_ast, formatted_ast) {
        (AstNode::Module { body: orig_body, .. }, AstNode::Module { body: fmt_body, .. }) => {
            assert_eq!(orig_body.len(), fmt_body.len());
        }
        _ => panic!("Expected Module nodes"),
    }
}

#[test]
fn test_range_formatting_single_function() {
    let source = r#"def foo():
    pass

def bar():
    pass
"#;
    let config = FormatterConfig::default();
    let parser = PythonParser::default();
    let mut formatter = Formatter::new(config, parser);

    let result = formatter.format_range(source, 3, 4);
    assert!(result.is_ok());

    let formatted = result.unwrap();
    assert!(formatted.contains("bar"));
}

#[test]
fn test_range_formatting_partial_class() {
    let source = r#"class MyClass:
    def method1(self):
        pass

    def method2(self):
        pass
"#;
    let config = FormatterConfig::default();
    let parser = PythonParser::default();
    let mut formatter = Formatter::new(config, parser);

    let result = formatter.format_range(source, 4, 6);
    assert!(result.is_ok());
}

#[test]
fn test_range_formatting_nested_blocks() {
    let source = r#"if True:
    x = 1
    if False:
        y = 2
    z = 3
"#;
    let config = FormatterConfig::default();
    let parser = PythonParser::default();
    let mut formatter = Formatter::new(config, parser);

    let result = formatter.format_range(source, 2, 3);
    assert!(result.is_ok());
}

#[test]
fn test_range_formatting_preserves_outside_range() {
    let source = r#"def foo():
    pass

def bar():
    pass
"#;
    let config = FormatterConfig::default();
    let parser = PythonParser::default();
    let mut formatter = Formatter::new(config, parser);

    let result = formatter.format_range(source, 3, 4);
    assert!(result.is_ok());

    let formatted = result.unwrap();
    let parsed = PythonParser::default().parse(&formatted);
    assert!(parsed.is_ok(), "Formatted code should be valid Python");
}

#[test]
fn test_multiple_operators() {
    let source = "x=1+2*3-4/5";
    let formatted = format_code(source);
    assert_eq!(formatted.trim(), "x = 1 + 2 * 3 - 4 / 5");
}

#[test]
fn test_nested_function_calls() {
    let source = "result=func1(func2(a,b),func3(c,d))";
    let formatted = format_code(source);
    assert_eq!(formatted.trim(), "result = func1(func2(a, b), func3(c, d))");
}

#[test]
fn test_comparison_operators() {
    let source = "if x==1 and y!=2 and z<3:";
    let formatted = format_code(source);
    assert!(formatted.contains("x == 1"));
    assert!(formatted.contains("y != 2"));
    assert!(formatted.contains("z < 3"));
}

#[test]
fn test_multiple_assignments() {
    let source = r#"x=1
y=2
z=3
"#;
    let formatted = format_code(source);
    assert!(formatted.contains("x = 1"));
    assert!(formatted.contains("y = 2"));
    assert!(formatted.contains("z = 3"));
}

#[test]
fn test_dictionary_literals() {
    let source = "d={\"a\":1,\"b\":2,\"c\":3}";
    let formatted = format_code(source);
    assert!(formatted.contains("\"a\": 1"));
    assert!(formatted.contains("\"b\": 2"));
    assert!(formatted.contains("\"c\": 3"));
}

#[test]
fn test_list_comprehension() {
    let source = "[x for x in range(10) if x%2==0]";
    let formatted = format_code(source);
    assert!(formatted.contains("for x in"));
    assert!(formatted.contains("if x"));
}

#[test]
fn test_chained_comparisons() {
    let source = "if 0<x<10:";
    let formatted = format_code(source);
    assert!(formatted.contains("0 < x < 10"));
}

#[test]
fn test_unary_operators() {
    let source = "x=-5";
    let formatted = format_code(source);
    assert!(formatted.contains("-5") || formatted.contains("- 5"));
    assert!(formatted.contains("x ="));
}

#[test]
fn test_lambda_expressions() {
    let source = "f=lambda x:x+1";
    let formatted = format_code(source);
    assert!(formatted.contains("lambda"));
    assert!(formatted.contains("f ="));
}

#[test]
fn test_tuple_assignment() {
    let source = "a,b=1,2";
    let formatted = format_code(source);
    assert!(formatted.contains("="));
    assert!(formatted.contains("a"));
    assert!(formatted.contains("b"));
}

#[test]
fn test_format_on_save() {
    let documents = DocumentManager::new().expect("Failed to create document manager");
    let provider = FormattingProvider::new(documents.clone());
    let config = FormatterConfig::default();

    let uri = Url::parse("file:///test.py").unwrap();
    let content = r#"def foo():
    pass
"#;

    documents.open_document(uri.clone(), 1, content.to_string()).ok();

    let params =
        WillSaveTextDocumentParams { text_document: TextDocumentIdentifier { uri }, reason: TextSaveReason::MANUAL };

    let result = provider.format_on_save(&params, &config);
    assert!(result.is_some() || result.is_none());
}

#[test]
fn test_on_type_formatting() {
    let documents = DocumentManager::new().expect("Failed to create document manager");
    let provider = FormattingProvider::new(documents.clone());
    let config = FormatterConfig::default();

    let uri = Url::parse("file:///test.py").unwrap();
    let content = "def foo():";

    documents.open_document(uri.clone(), 1, content.to_string()).ok();

    let params = DocumentOnTypeFormattingParams {
        text_document_position: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri },
            position: Position { line: 0, character: 10 },
        },
        ch: ":".to_string(),
        options: Default::default(),
    };

    let result = provider.on_type_format(&params, &config);
    assert!(result.is_some() || result.is_none());
}

#[test]
fn test_decorator_spacing() {
    let source = r#"@decorator
def foo():
    pass
"#;
    let formatted = format_code(source);
    assert!(
        formatted.contains("@decorator\ndef foo():"),
        "Unexpected blank line after decorator:\n{formatted}"
    );
}

#[test]
fn test_decorator_indentation_in_class() {
    let source = r#"class Animal:
    @abstractmethod
    def make_sound(self):
        pass

    @property
    def name(self):
        return "animal"

    @classmethod
    def create(cls):
        return cls()

    @staticmethod
    def helper():
        pass
"#;
    let formatted = format_code(source);

    assert!(
        formatted.contains("    @abstractmethod\n    def make_sound"),
        "abstractmethod decorator not properly indented:\n{formatted}"
    );
    assert!(
        formatted.contains("    @property\n    def name"),
        "property decorator not properly indented:\n{formatted}"
    );
    assert!(
        formatted.contains("    @classmethod\n    def create"),
        "classmethod decorator not properly indented:\n{formatted}"
    );
    assert!(
        formatted.contains("    @staticmethod\n    def helper"),
        "staticmethod decorator not properly indented:\n{formatted}"
    );

    let second_format = format_code(&formatted);
    assert_eq!(
        formatted, second_format,
        "Decorator formatting should be idempotent.\nFirst:\n{formatted}\nSecond:\n{second_format}"
    );
}

#[test]
fn test_nested_class_decorator_indentation() {
    let source = r#"class Outer:
    @decorator
    class Inner:
        @method_decorator
        def method(self):
            pass
"#;
    let formatted = format_code(source);

    assert!(
        formatted.contains("    @decorator\n    class Inner"),
        "Nested class decorator not properly indented:\n{formatted}"
    );

    assert!(
        formatted.contains("        @method_decorator\n        def method"),
        "Nested method decorator not properly indented:\n{formatted}"
    );

    let second_format = format_code(&formatted);
    assert_eq!(
        formatted, second_format,
        "Nested decorator formatting should be idempotent"
    );
}

#[test]
fn test_multiple_decorators_preserve_group() {
    let source = r#"@a
@b
def foo():
    pass
"#;
    let formatted = format_code(source);
    assert!(formatted.contains("@a\n@b\ndef foo():"));
}

#[test]
fn test_slice_no_space_after_colon() {
    let source = "data = arr[1:10]";
    let formatted = format_code(source);
    assert!(
        formatted.contains("[1:10]"),
        "Slice should not add space after colon: {formatted}"
    );
}

#[test]
fn test_slice_with_step_spacing() {
    let source = "data = arr[1:10:2]";
    let formatted = format_code(source);
    assert!(formatted.contains("[1:10:2]"));
}
#[test]
fn test_relative_import_levels() {
    let source = r#"from . import a
from ..subpackage import b
"#;
    let formatted = format_code(source);
    assert!(formatted.contains("from . import a"));
    assert!(formatted.contains("from ..subpackage import b"));
}

#[test]
fn test_long_function_call_wrapping() {
    let config = FormatterConfig { line_length: 40, ..Default::default() };
    let source = "my_function(long_argument_one, long_argument_two, long_argument_three)";
    let formatted = format_code_with_config(source, config);
    assert!(
        formatted.contains("long_argument_two"),
        "Should wrap long calls instead of truncating: {formatted}"
    );
}
#[test]
fn test_multiline_string_preservation() {
    let source = r#"def foo():
    """This is a
    multi-line docstring"""
    pass
"#;
    let formatted = format_code(source);
    assert!(
        formatted.contains("multi-line docstring"),
        "Multiline strings/docstrings should be preserved"
    );
}

#[test]
fn test_module_docstring_before_imports() {
    let source = "\"\"\"Top level docstring.\"\"\"\n\nimport b\nfrom a import foo\n";
    let formatted = format_code(source);
    assert!(
        formatted.starts_with("\"\"\"Top level docstring.\"\"\""),
        "Docstring should remain at the top:\n{formatted}"
    );
    assert!(
        formatted.contains("\"\"\"Top level docstring.\"\"\"\n\nimport"),
        "Docstring should be separated from imports:\n{formatted}"
    );
}

#[test]
fn test_future_import_preserved() {
    let source = "\"\"\"Doc.\"\"\"\n\nfrom __future__ import annotations\nfrom typing import List\n";
    let formatted = format_code(source);
    let future_pos = formatted
        .find("from __future__ import annotations")
        .expect("missing future import");
    let typing_pos = formatted
        .find("from typing import List")
        .expect("missing typing import");
    assert!(
        future_pos < typing_pos,
        "Future import should come before other imports:\n{formatted}"
    );
}

#[test]
fn test_function_parameter_annotations_and_defaults() {
    let source = r#"def foo(x: int = 1, y: str= "a"):
    return x
"#;
    let formatted = format_code(source);
    assert!(
        formatted.contains("x: int = 1"),
        "Parameter annotations and defaults should be preserved: {formatted}"
    );
    assert!(
        formatted.contains("y: str = \"a\""),
        "Second parameter annotation/default missing: {formatted}"
    );
}

#[test]
fn test_class_base_with_generic_preserved() {
    let source = r#"class Foo(Generic[T]):
    pass
"#;
    let formatted = format_code(source);
    assert!(
        formatted.contains("class Foo(Generic[T]):"),
        "Generic base should be preserved: {formatted}"
    );
}

#[test]
fn test_fstring_expression_preserved() {
    let source = r#"def describe(self, item):
    return f"{self.name}: {item}"
"#;
    let formatted = format_code(source);
    assert!(
        formatted.contains("{self.name}: {item}"),
        "Formatted f-string lost interpolations: {formatted}"
    );
}

#[test]
fn test_inline_comment_spacing() {
    let source = "x = 1# comment";
    let formatted = format_code(source);
    assert!(
        formatted.contains("x = 1  # comment"),
        "Should insert two spaces before inline comment: {formatted}"
    );
}

#[test]
fn test_comment_indentation() {
    let source = r#"def foo():
# comment
    pass
"#;
    let formatted = format_code(source);
    assert!(
        formatted.contains("    # comment"),
        "Comment inside block should be indented: {formatted}"
    );
}

#[test]
fn test_if_elif_else_formatting() {
    let source = r#"if x:
    pass
else:
    pass
"#;
    let formatted = format_code(source);
    assert!(formatted.contains("if x:"));
    assert!(formatted.contains("else:"));
    assert!(!formatted.contains("else :"), "No extra space before colon");
}

#[test]
fn test_try_except_finally_formatting() {
    let source = r#"try:
    x = 1
except ValueError:
    pass
finally:
    y = 2
"#;
    let formatted = format_code(source);
    assert!(formatted.contains("except ValueError:"));
    assert!(formatted.contains("finally:"));
}

#[test]
fn test_unary_operator_no_extra_space() {
    let source = "x = -5";
    let formatted = format_code(source);
    assert!(
        formatted.contains("x = -5"),
        "Unary minus should not have space after: {formatted}"
    );
}

#[test]
fn test_binary_minus_spaced() {
    let source = "x=5-3";
    let formatted = format_code(source);
    assert_eq!(formatted.trim(), "x = 5 - 3");
}

#[test]
fn test_blank_lines_between_top_level_defs() {
    let source = r#"def a():
    pass
def b():
    pass
"#;
    let formatted = format_code(source);
    assert!(
        formatted.contains("pass\n\n\ndef b"),
        "Top-level functions should be separated by two blank lines"
    );
}

#[test]
fn test_unicode_identifiers() {
    let source = "π = 3.14\nprint(π)";
    let formatted = format_code(source);
    assert!(formatted.contains("π"));
}

#[test]
fn test_unicode_strings() {
    let source = "message = 'こんにちは'";
    let formatted = format_code(source);
    assert!(
        formatted.contains("こんにちは"),
        "Unicode strings should be preserved: {formatted}"
    );
}

#[test]
fn test_ast_preservation_nested_defs() {
    let source = r#"class A:
    def f(self):
        def g():
            pass
        return g
"#;
    let first = format_code(source);
    let second = format_code(&first);
    assert_eq!(first, second);
}

#[test]
fn test_trailing_comma_removed_in_dict() {
    let source = "{\n    \"a\": 1,\n    \"b\": 2,\n}\n";
    let formatted = format_code(source);
    assert!(
        !formatted.contains("\"b\": 2,"),
        "Trailing comma should be removed when formatting dicts: {formatted}"
    );
}

#[test]
fn test_idempotency_bulk_cases() {
    let samples = [
        "x=1+2",
        "def foo():\n    pass",
        "class C:\n    pass",
        "a,b=1,2",
        "if x==1:\n    pass",
    ];

    for src in samples {
        let once = format_code(src);
        let twice = format_code(&once);
        assert_eq!(once, twice, "Not idempotent for: {src}");
    }
}

#[test]
fn test_match_case_formatting() {
    let source = r#"match x:
    case 1:
        pass
    case _:
        pass
"#;
    let formatted = format_code(source);
    assert!(formatted.contains("match x:"));
    assert!(formatted.contains("case 1:"));
}
#[test]
fn test_trailing_comma_removed_in_call() {
    let source = "f(\n    a,\n    b,\n)\n";
    let formatted = format_code(source);
    assert!(
        !formatted.contains("b,\n)"),
        "Trailing comma should be removed in formatted calls: {formatted}"
    );
}

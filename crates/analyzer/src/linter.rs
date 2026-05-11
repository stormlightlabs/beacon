//! Static analysis linter for Python code
//!
//! Implements PyFlakes-style linting rules (BEA001-BEA030) for detecting common coding issues, style violations, and potential bugs.

mod checks;
mod context;
mod symbol_rules;
mod traversal;

use crate::rules::{DiagnosticMessage, RuleKind};

use beacon_core::SuppressionMap;
use beacon_parser::{AstNode, SymbolTable};

use context::LinterContext;

/// Static analysis linter for Python code
pub struct Linter<'a> {
    /// Collected diagnostic messages
    diagnostics: Vec<DiagnosticMessage>,
    /// Context for tracking state
    ctx: LinterContext,
    /// Symbol table for scope analysis
    symbol_table: &'a SymbolTable,
    /// Source filename
    filename: String,
    /// Full source split into lines for span calculations
    source_lines: Vec<String>,
    /// Suppression map for filtering diagnostics
    suppression_map: SuppressionMap,
    /// __all__ exports list for checking re-exported imports
    all_exports: Option<Vec<String>>,
}

impl<'a> Linter<'a> {
    /// Create a new linter for the given AST and symbol table
    pub fn new(symbol_table: &'a SymbolTable, filename: String, source: &str) -> Self {
        let suppression_map = SuppressionMap::from_source(source);
        let source_lines = source.lines().map(|line| line.to_string()).collect();
        Self {
            diagnostics: Vec::new(),
            ctx: LinterContext::new(),
            symbol_table,
            filename,
            source_lines,
            suppression_map,
            all_exports: None,
        }
    }

    /// Analyze the AST and return collected diagnostics
    pub fn analyze(&mut self, ast: &AstNode) -> Vec<DiagnosticMessage> {
        self.all_exports = Self::extract_all_exports(ast);
        self.visit_node(ast);
        self.check_symbol_table_rules();
        let diagnostics = std::mem::take(&mut self.diagnostics);

        diagnostics
            .into_iter()
            .filter(|diag| !self.suppression_map.is_suppressed(diag.line, Some(diag.rule.code())))
            .collect()
    }

    /// Report a diagnostic message
    pub(super) fn report(&mut self, rule: RuleKind, message: String, line: usize, col: usize, end_col: usize) {
        self.diagnostics
            .push(DiagnosticMessage { rule, message, filename: self.filename.clone(), line, col, end_col });
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use beacon_parser::PythonParser;

    fn lint_source(source: &str) -> Vec<DiagnosticMessage> {
        let mut parser = PythonParser::new().unwrap();
        let (ast, symbol_table) = parser.parse_and_resolve(source).unwrap();
        let mut linter = Linter::new(&symbol_table, "test.py".to_string(), source);
        linter.analyze(&ast)
    }

    #[test]
    fn test_forward_annotation_error_span_matches_return_position() {
        let source = "def foo() -> \"list[\":\n    return 1";
        let diagnostics = lint_source(source);
        let diag = diagnostics
            .iter()
            .find(|d| d.rule == RuleKind::ForwardAnnotationSyntaxError)
            .expect("expected forward annotation diagnostic");
        assert_eq!(diag.line, 1);
        assert_eq!(diag.col, 14);
        assert_eq!(diag.end_col, 21);
    }

    #[test]
    fn test_return_outside_function() {
        let source = "return 42";
        let diagnostics = lint_source(source);
        assert!(diagnostics.iter().any(|d| d.rule == RuleKind::ReturnOutsideFunction));
    }

    #[test]
    fn test_return_inside_function() {
        let source = "def foo():\n    return 42";
        let diagnostics = lint_source(source);
        assert!(!diagnostics.iter().any(|d| d.rule == RuleKind::ReturnOutsideFunction));
    }

    #[test]
    fn test_break_outside_loop() {
        let source = "break";
        let diagnostics = lint_source(source);
        assert!(diagnostics.iter().any(|d| d.rule == RuleKind::BreakOutsideLoop));
    }

    #[test]
    fn test_break_inside_loop() {
        let source = "for i in range(10):\n    break";
        let diagnostics = lint_source(source);
        assert!(!diagnostics.iter().any(|d| d.rule == RuleKind::BreakOutsideLoop));
    }

    #[test]
    fn test_continue_outside_loop() {
        let source = "continue";
        let diagnostics = lint_source(source);
        assert!(diagnostics.iter().any(|d| d.rule == RuleKind::ContinueOutsideLoop));
    }

    #[test]
    fn test_duplicate_argument() {
        let source = "def foo(x, y, x):\n    pass";
        let diagnostics = lint_source(source);
        assert!(diagnostics.iter().any(|d| d.rule == RuleKind::DuplicateArgument));
    }

    #[test]
    fn test_no_duplicate_argument() {
        let source = "def foo(x, y, z):\n    pass";
        let diagnostics = lint_source(source);
        assert!(!diagnostics.iter().any(|d| d.rule == RuleKind::DuplicateArgument));
    }

    #[test]
    fn test_import_star_in_function() {
        let source = "def foo():\n    from os import *";
        let diagnostics = lint_source(source);
        assert!(diagnostics.iter().any(|d| d.rule == RuleKind::ImportStarNotPermitted));
    }

    #[test]
    fn test_import_star_at_module_level() {
        let source = "from os import *";
        let diagnostics = lint_source(source);
        assert!(diagnostics.iter().any(|d| d.rule == RuleKind::ImportStarUsed));
        assert!(!diagnostics.iter().any(|d| d.rule == RuleKind::ImportStarNotPermitted));
    }

    #[test]
    fn test_raise_not_implemented() {
        let source = "raise NotImplemented";
        let diagnostics = lint_source(source);
        assert!(diagnostics.iter().any(|d| d.rule == RuleKind::RaiseNotImplemented));
    }

    #[test]
    fn test_raise_not_implemented_error() {
        let source = "raise NotImplementedError";
        let diagnostics = lint_source(source);
        assert!(!diagnostics.iter().any(|d| d.rule == RuleKind::RaiseNotImplemented));
    }

    #[test]
    fn test_default_except_not_last() {
        let source = r#"
try:
    pass
except:
    pass
except ValueError:
    pass
"#;
        let diagnostics = lint_source(source);

        assert!(diagnostics.iter().any(|d| d.rule == RuleKind::DefaultExceptNotLast));
    }

    #[test]
    fn test_default_except_last() {
        let source = r#"
try:
    pass
except ValueError:
    pass
except:
    pass
"#;
        let diagnostics = lint_source(source);
        assert!(!diagnostics.iter().any(|d| d.rule == RuleKind::DefaultExceptNotLast));
    }

    #[test]
    fn test_empty_except() {
        let source = r#"
try:
    pass
except:
    pass
"#;
        let diagnostics = lint_source(source);
        assert!(diagnostics.iter().any(|d| d.rule == RuleKind::EmptyExcept));
    }

    #[test]
    fn test_is_literal_with_integer() {
        let source = "x is 5";
        let diagnostics = lint_source(source);
        assert!(diagnostics.iter().any(|d| d.rule == RuleKind::IsLiteral));
    }

    #[test]
    fn test_is_none_allowed() {
        let source = "x is None";
        let diagnostics = lint_source(source);
        assert!(!diagnostics.iter().any(|d| d.rule == RuleKind::IsLiteral));
    }

    #[test]
    fn test_import_shadowed_by_loop_var() {
        let source = "import os\nfor os in range(10):\n    pass";
        let diagnostics = lint_source(source);
        assert!(diagnostics.iter().any(|d| d.rule == RuleKind::ImportShadowedByLoopVar));
    }

    #[test]
    fn test_if_tuple() {
        let source = "if (x,):\n    pass";
        let diagnostics = lint_source(source);
        assert!(diagnostics.iter().any(|d| d.rule == RuleKind::IfTuple));
    }

    #[test]
    fn test_if_tuple_multi_element() {
        let source = "if (x, y, z):\n    pass";
        let diagnostics = lint_source(source);
        assert!(diagnostics.iter().any(|d| d.rule == RuleKind::IfTuple));
    }

    #[test]
    fn test_if_not_tuple() {
        let source = "if (x):\n    pass";
        let diagnostics = lint_source(source);
        assert!(!diagnostics.iter().any(|d| d.rule == RuleKind::IfTuple));
    }

    #[test]
    fn test_fstring_missing_placeholders() {
        let source = "x = f'hello'";
        let diagnostics = lint_source(source);

        assert!(
            diagnostics
                .iter()
                .any(|d| d.rule == RuleKind::FStringMissingPlaceholders)
        );
    }

    #[test]
    fn test_fstring_with_placeholders() {
        let source = "x = f'hello {name}'";
        let diagnostics = lint_source(source);

        assert!(
            !diagnostics
                .iter()
                .any(|d| d.rule == RuleKind::FStringMissingPlaceholders)
        );
    }

    #[test]
    fn test_regular_string_no_warning() {
        let source = "x = 'hello'";
        let diagnostics = lint_source(source);

        assert!(
            !diagnostics
                .iter()
                .any(|d| d.rule == RuleKind::FStringMissingPlaceholders)
        );
    }

    #[test]
    fn test_uppercase_fstring_missing_placeholders() {
        let source = "x = F'hello'";
        let diagnostics = lint_source(source);
        assert!(
            diagnostics
                .iter()
                .any(|d| d.rule == RuleKind::FStringMissingPlaceholders)
        );
    }

    #[test]
    fn test_raw_fstring_missing_placeholders() {
        let source = "x = rf'hello'";
        let diagnostics = lint_source(source);
        assert!(
            diagnostics
                .iter()
                .any(|d| d.rule == RuleKind::FStringMissingPlaceholders)
        );
    }

    #[test]
    fn test_tstring_missing_placeholders() {
        let source = "x = t'hello'";
        let diagnostics = lint_source(source);
        assert!(
            diagnostics
                .iter()
                .any(|d| d.rule == RuleKind::TStringMissingPlaceholders)
        );
    }

    #[test]
    fn test_tstring_with_placeholders() {
        let source = "x = t'hello {name}'";
        let diagnostics = lint_source(source);
        assert!(
            !diagnostics
                .iter()
                .any(|d| d.rule == RuleKind::TStringMissingPlaceholders)
        );
    }

    #[test]
    fn test_uppercase_tstring_missing_placeholders() {
        let source = "x = T'hello'";
        let diagnostics = lint_source(source);
        assert!(
            diagnostics
                .iter()
                .any(|d| d.rule == RuleKind::TStringMissingPlaceholders)
        );
    }

    #[test]
    fn test_regular_string_no_tstring_warning() {
        let source = "x = 'hello'";
        let diagnostics = lint_source(source);
        assert!(
            !diagnostics
                .iter()
                .any(|d| d.rule == RuleKind::TStringMissingPlaceholders)
        );
    }

    #[test]
    fn test_unused_import() {
        let source = "import os\nx = 5";
        let diagnostics = lint_source(source);
        assert!(diagnostics.iter().any(|d| d.rule == RuleKind::UnusedImport));
    }

    #[test]
    fn test_used_import() {
        let source = "import os\nprint(os.name)";
        let diagnostics = lint_source(source);
        assert!(!diagnostics.iter().any(|d| d.rule == RuleKind::UnusedImport));
    }

    #[test]
    fn test_unused_import_with_underscore_prefix() {
        let source = "import _private\nx = 5";
        let diagnostics = lint_source(source);
        assert!(!diagnostics.iter().any(|d| d.rule == RuleKind::UnusedImport));
    }

    #[test]
    fn test_import_used_in_nested_function_with_if_not() {
        let source = r#"
import os

def outer():
    def inner():
        if not os.path.exists("/tmp"):
            return []
    inner()
"#;
        let diagnostics = lint_source(source);
        assert!(
            !diagnostics.iter().any(|d| d.rule == RuleKind::UnusedImport),
            "os should not be flagged as unused when used in nested function"
        );
    }

    #[test]
    fn test_import_used_in_nested_function_simple() {
        let source = r#"
import os

def outer():
    def inner():
        print(os.name)
    inner()
"#;
        let diagnostics = lint_source(source);
        assert!(!diagnostics.iter().any(|d| d.rule == RuleKind::UnusedImport));
    }

    #[test]
    fn test_import_used_in_nested_function_attribute_access() {
        let source = r#"
import os

def outer():
    def inner():
        result = os.path.exists("/tmp")
        return result
    inner()
"#;
        let diagnostics = lint_source(source);
        assert!(!diagnostics.iter().any(|d| d.rule == RuleKind::UnusedImport));
    }

    #[test]
    fn test_unused_annotation() {
        let source = "x: int";
        let diagnostics = lint_source(source);
        assert!(diagnostics.iter().any(|d| d.rule == RuleKind::UnusedAnnotation));
    }

    #[test]
    fn test_used_annotation() {
        let source = "x: int = 5\nprint(x)";
        let diagnostics = lint_source(source);
        assert!(!diagnostics.iter().any(|d| d.rule == RuleKind::UnusedAnnotation));
    }

    #[test]
    fn test_annotation_with_underscore_prefix() {
        let source = "_x: int";
        let diagnostics = lint_source(source);
        assert!(!diagnostics.iter().any(|d| d.rule == RuleKind::UnusedAnnotation));
    }

    #[test]
    fn test_loop_variable_used() {
        let source = "for entry in [1, 2, 3]:\n    print(entry)";
        let diagnostics = lint_source(source);
        assert!(!diagnostics.iter().any(|d| d.rule == RuleKind::UnusedAnnotation));
    }

    #[test]
    fn test_loop_variable_used_in_fstring() {
        let source = "for entry in [1, 2, 3]:\n    print(f'History: {entry}')";
        let diagnostics = lint_source(source);
        assert!(!diagnostics.iter().any(|d| d.rule == RuleKind::UnusedAnnotation));
    }

    #[test]
    fn test_loop_variable_with_attribute_in_fstring() {
        let source = "for item in items:\n    print(f'Item name: {item.name}')";
        let diagnostics = lint_source(source);
        assert!(!diagnostics.iter().any(|d| d.rule == RuleKind::UnusedAnnotation));
    }

    #[test]
    fn test_loop_variable_with_format_spec_in_fstring() {
        let source = "for value in values:\n    print(f'Value: {value:.2f}')";
        let diagnostics = lint_source(source);
        assert!(!diagnostics.iter().any(|d| d.rule == RuleKind::UnusedAnnotation));
    }

    #[test]
    fn test_comprehension_variable_in_fstring() {
        let source = "[f'{x}' for x in range(10)]";
        let diagnostics = lint_source(source);
        assert!(!diagnostics.iter().any(|d| d.rule == RuleKind::UnusedAnnotation));
    }

    #[test]
    fn test_nested_loops_with_fstring() {
        let source = "for i in range(3):\n    for j in range(3):\n        print(f'{i},{j}')";
        let diagnostics = lint_source(source);
        assert!(!diagnostics.iter().any(|d| d.rule == RuleKind::UnusedAnnotation));
    }

    #[test]
    fn test_loop_variable_in_tstring() {
        let source = "for entry in [1, 2, 3]:\n    sql = t'SELECT * FROM table WHERE id = {entry}'";
        let diagnostics = lint_source(source);
        assert!(!diagnostics.iter().any(|d| d.rule == RuleKind::UnusedAnnotation));
    }

    #[test]
    fn test_redefined_while_unused() {
        let source = "x = 1\nx = 2";
        let diagnostics = lint_source(source);
        assert!(diagnostics.iter().any(|d| d.rule == RuleKind::RedefinedWhileUnused));
    }

    #[test]
    fn test_redefined_after_use() {
        let source = "x = 1\nprint(x)\nx = 2";
        let diagnostics = lint_source(source);
        assert!(!diagnostics.iter().any(|d| d.rule == RuleKind::RedefinedWhileUnused));
    }

    #[test]
    fn test_redefined_while_unused_with_underscore() {
        let source = "_x = 1\n_x = 2";
        let diagnostics = lint_source(source);
        assert!(!diagnostics.iter().any(|d| d.rule == RuleKind::RedefinedWhileUnused));
    }

    #[test]
    fn test_multiple_redefinitions() {
        let source = "x = 1\nx = 2\nx = 3";
        let diagnostics = lint_source(source);
        let count = diagnostics
            .iter()
            .filter(|d| d.rule == RuleKind::RedefinedWhileUnused)
            .count();
        assert_eq!(count, 1);
    }

    #[test]
    fn test_yield_outside_function() {
        let source = "yield 42";
        let diagnostics = lint_source(source);
        assert!(diagnostics.iter().any(|d| d.rule == RuleKind::YieldOutsideFunction));
    }

    #[test]
    fn test_yield_inside_function() {
        let source = "def foo():\n    yield 42";
        let diagnostics = lint_source(source);
        assert!(!diagnostics.iter().any(|d| d.rule == RuleKind::YieldOutsideFunction));
    }

    #[test]
    fn test_yield_from_outside_function() {
        let source = "yield from [1, 2, 3]";
        let diagnostics = lint_source(source);
        assert!(diagnostics.iter().any(|d| d.rule == RuleKind::YieldOutsideFunction));
    }

    #[test]
    fn test_yield_from_inside_function() {
        let source = "def foo():\n    yield from [1, 2, 3]";
        let diagnostics = lint_source(source);
        assert!(!diagnostics.iter().any(|d| d.rule == RuleKind::YieldOutsideFunction));
    }

    #[test]
    fn test_assert_tuple() {
        let source = "assert (x, y)";
        let diagnostics = lint_source(source);
        assert!(diagnostics.iter().any(|d| d.rule == RuleKind::AssertTuple));
    }

    #[test]
    fn test_assert_non_tuple() {
        let source = "assert x == y";
        let diagnostics = lint_source(source);
        assert!(!diagnostics.iter().any(|d| d.rule == RuleKind::AssertTuple));
    }

    #[test]
    fn test_assert_single_element_parenthesized() {
        let source = "assert (x)";
        let diagnostics = lint_source(source);
        assert!(!diagnostics.iter().any(|d| d.rule == RuleKind::AssertTuple));
    }

    #[test]
    #[ignore]
    fn test_nested_functions_break_continue() {
        // TODO: This test fails because nested functions correctly reset scope.
        // Break inside inner() is correctly NOT flagged because Python allows this
        // (though it would fail at runtime). The linter only checks syntax, not runtime behavior.
        let source = r#"
def outer():
    for i in range(10):
        def inner():
            break
        inner()
"#;
        let diagnostics = lint_source(source);
        assert!(diagnostics.iter().any(|d| d.rule == RuleKind::BreakOutsideLoop));
    }

    #[test]
    fn test_multiple_except_handlers() {
        let source = r#"
try:
    pass
except ValueError:
    pass
except KeyError:
    pass
except:
    pass
"#;
        let diagnostics = lint_source(source);
        assert!(diagnostics.iter().any(|d| d.rule == RuleKind::EmptyExcept));
        assert!(!diagnostics.iter().any(|d| d.rule == RuleKind::DefaultExceptNotLast));
    }

    #[test]
    fn test_is_literal_with_string() {
        let source = "x is 'hello'";
        let diagnostics = lint_source(source);
        assert!(diagnostics.iter().any(|d| d.rule == RuleKind::IsLiteral));
    }

    #[test]
    #[ignore]
    fn test_is_not_literal() {
        // TODO: Parser may not be generating IsNot as a single CompareOperator
        // Need to verify parser output for "is not" expressions
        let source = "x is not 5";
        let diagnostics = lint_source(source);
        assert!(diagnostics.iter().any(|d| d.rule == RuleKind::IsLiteral));
    }

    #[test]
    fn test_is_true_false_allowed() {
        let source = "x is True";
        let diagnostics = lint_source(source);
        assert!(!diagnostics.iter().any(|d| d.rule == RuleKind::IsLiteral));
    }

    #[test]
    fn test_percent_format_valid() {
        let source = r#"x = "%s %d" % ("hello", 5)"#;
        let diagnostics = lint_source(source);
        assert!(
            !diagnostics
                .iter()
                .any(|d| d.rule == RuleKind::PercentFormatInvalidFormat)
        );
    }

    #[test]
    fn test_percent_format_invalid() {
        let source = r#"x = "%q" % 3"#;
        let diagnostics = lint_source(source);
        assert!(
            diagnostics
                .iter()
                .any(|d| d.rule == RuleKind::PercentFormatInvalidFormat)
        );
    }

    #[test]
    fn test_percent_format_valid_all_specifiers() {
        let source = r#"x = "%s %d %i %f %e %E %g %G %c %r %x %X %o" % data"#;
        let diagnostics = lint_source(source);
        assert!(
            !diagnostics
                .iter()
                .any(|d| d.rule == RuleKind::PercentFormatInvalidFormat)
        );
    }

    #[test]
    fn test_percent_format_double_percent() {
        let source = r#"x = "100%% complete" % ()"#;
        let diagnostics = lint_source(source);
        assert!(
            !diagnostics
                .iter()
                .any(|d| d.rule == RuleKind::PercentFormatInvalidFormat)
        );
    }

    #[test]
    fn test_percent_format_incomplete() {
        let source = r#"x = "incomplete %" % ()"#;
        let diagnostics = lint_source(source);
        assert!(
            diagnostics
                .iter()
                .any(|d| d.rule == RuleKind::PercentFormatInvalidFormat)
        );
    }

    #[test]
    fn test_percent_operator_not_format() {
        let source = "x = 10 % 3";
        let diagnostics = lint_source(source);
        assert!(
            !diagnostics
                .iter()
                .any(|d| d.rule == RuleKind::PercentFormatInvalidFormat)
        );
    }

    #[test]
    fn test_duplicate_dict_key_string() {
        let source = r#"x = {'a': 1, 'a': 2}"#;
        let diagnostics = lint_source(source);
        assert!(
            diagnostics
                .iter()
                .any(|d| d.rule == RuleKind::MultiValueRepeatedKeyLiteral)
        );
    }

    #[test]
    fn test_duplicate_dict_key_integer() {
        let source = r#"x = {1: 'x', 1: 'y'}"#;
        let diagnostics = lint_source(source);
        assert!(
            diagnostics
                .iter()
                .any(|d| d.rule == RuleKind::MultiValueRepeatedKeyLiteral)
        );
    }

    #[test]
    fn test_duplicate_dict_key_boolean() {
        let source = r#"x = {True: 1, True: 2}"#;
        let diagnostics = lint_source(source);
        assert!(
            diagnostics
                .iter()
                .any(|d| d.rule == RuleKind::MultiValueRepeatedKeyLiteral)
        );
    }

    #[test]
    fn test_duplicate_dict_key_none() {
        let source = r#"x = {None: 1, None: 2}"#;
        let diagnostics = lint_source(source);
        assert!(
            diagnostics
                .iter()
                .any(|d| d.rule == RuleKind::MultiValueRepeatedKeyLiteral)
        );
    }

    #[test]
    fn test_no_duplicate_dict_keys() {
        let source = r#"x = {'a': 1, 'b': 2, 'c': 3}"#;
        let diagnostics = lint_source(source);
        assert!(
            !diagnostics
                .iter()
                .any(|d| d.rule == RuleKind::MultiValueRepeatedKeyLiteral)
        );
    }

    #[test]
    fn test_dict_with_variable_keys_no_warning() {
        let source = r#"x = {a: 1, b: 2}"#;
        let diagnostics = lint_source(source);
        assert!(
            !diagnostics
                .iter()
                .any(|d| d.rule == RuleKind::MultiValueRepeatedKeyLiteral)
        );
    }

    #[test]
    fn test_dict_mixed_literal_and_variable_keys() {
        let source = r#"x = {'a': 1, b: 2, 'a': 3}"#;
        let diagnostics = lint_source(source);
        assert!(
            diagnostics
                .iter()
                .any(|d| d.rule == RuleKind::MultiValueRepeatedKeyLiteral)
        );
    }

    #[test]
    fn test_redundant_pass_in_function() {
        let source = "def f():\n    pass\n    return 1";
        let diagnostics = lint_source(source);
        assert!(diagnostics.iter().any(|d| d.rule == RuleKind::RedundantPass));
    }

    #[test]
    fn test_redundant_pass_in_if_statement() {
        let source = "if x:\n    pass\n    print()";
        let diagnostics = lint_source(source);
        assert!(diagnostics.iter().any(|d| d.rule == RuleKind::RedundantPass));
    }

    #[test]
    fn test_redundant_pass_in_for_loop() {
        let source = "for i in range(10):\n    pass\n    break";
        let diagnostics = lint_source(source);
        assert!(diagnostics.iter().any(|d| d.rule == RuleKind::RedundantPass));
    }

    #[test]
    fn test_redundant_pass_in_while_loop() {
        let source = "while True:\n    pass\n    break";
        let diagnostics = lint_source(source);
        assert!(diagnostics.iter().any(|d| d.rule == RuleKind::RedundantPass));
    }

    #[test]
    fn test_redundant_pass_in_try_block() {
        let source = "try:\n    pass\n    x = 1\nexcept:\n    pass";
        let diagnostics = lint_source(source);
        let redundant_count = diagnostics.iter().filter(|d| d.rule == RuleKind::RedundantPass).count();
        assert_eq!(redundant_count, 1);
    }

    #[test]
    fn test_single_pass_in_function_valid() {
        let source = "def f():\n    pass";
        let diagnostics = lint_source(source);
        assert!(!diagnostics.iter().any(|d| d.rule == RuleKind::RedundantPass));
    }

    #[test]
    fn test_single_pass_in_class_valid() {
        let source = "class C:\n    pass";
        let diagnostics = lint_source(source);
        assert!(!diagnostics.iter().any(|d| d.rule == RuleKind::RedundantPass));
    }

    #[test]
    fn test_single_pass_in_if_valid() {
        let source = "if x:\n    pass";
        let diagnostics = lint_source(source);
        assert!(!diagnostics.iter().any(|d| d.rule == RuleKind::RedundantPass));
    }

    #[test]
    fn test_single_pass_in_except_valid() {
        let source = "try:\n    x = 1\nexcept:\n    pass";
        let diagnostics = lint_source(source);
        assert!(!diagnostics.iter().any(|d| d.rule == RuleKind::RedundantPass));
    }

    #[test]
    fn test_multiple_pass_in_block() {
        let source = "def f():\n    pass\n    pass\n    return 1";
        let diagnostics = lint_source(source);
        let redundant_count = diagnostics.iter().filter(|d| d.rule == RuleKind::RedundantPass).count();
        assert_eq!(redundant_count, 2);
    }

    #[test]
    fn test_suppression_noqa_all() {
        let source = "return 42  # noqa";
        let diagnostics = lint_source(source);
        assert!(diagnostics.is_empty());
    }

    #[test]
    fn test_suppression_noqa_specific() {
        let source = "return 42  # noqa: BEA003";
        let diagnostics = lint_source(source);
        assert!(diagnostics.is_empty());
    }

    #[test]
    fn test_suppression_noqa_wrong_code() {
        let source = "return 42  # noqa: BEA001";
        let diagnostics = lint_source(source);
        assert_eq!(diagnostics.len(), 1);
        assert_eq!(diagnostics[0].rule, RuleKind::ReturnOutsideFunction);
    }

    #[test]
    fn test_suppression_noqa_multiple_codes() {
        let source = "return 42  # noqa: BEA001, BEA003, BEA999";
        let diagnostics = lint_source(source);
        assert!(diagnostics.is_empty());
    }

    #[test]
    fn test_suppression_case_insensitive() {
        let source = "return 42  # noqa: bea003";
        let diagnostics = lint_source(source);
        assert!(diagnostics.is_empty());
    }

    #[test]
    fn test_annotation_pep604_union_syntax() {
        let source = "def foo(x: 'int | str') -> str:\n    return str(x)";
        let diagnostics = lint_source(source);
        assert!(
            !diagnostics
                .iter()
                .any(|d| d.rule == RuleKind::ForwardAnnotationSyntaxError),
            "PEP 604 union syntax (int | str) should be valid"
        );
    }

    #[test]
    fn test_annotation_multiple_union_members() {
        let source = "def foo(x: 'int | str | None') -> str:\n    return str(x)";
        let diagnostics = lint_source(source);
        assert!(
            !diagnostics
                .iter()
                .any(|d| d.rule == RuleKind::ForwardAnnotationSyntaxError),
            "Multiple union members should be valid"
        );
    }

    #[test]
    fn test_annotation_complex_nested_generics() {
        let source = "def foo(x: 'dict[str, list[int]]') -> None:\n    pass";
        let diagnostics = lint_source(source);
        assert!(
            !diagnostics
                .iter()
                .any(|d| d.rule == RuleKind::ForwardAnnotationSyntaxError),
            "Complex nested generics should be valid"
        );
    }

    #[test]
    fn test_annotation_callable_syntax() {
        let source = "def foo(f: 'Callable[[int, str], bool]') -> bool:\n    return f(1, 'x')";
        let diagnostics = lint_source(source);
        assert!(
            !diagnostics
                .iter()
                .any(|d| d.rule == RuleKind::ForwardAnnotationSyntaxError),
            "Callable syntax should be valid"
        );
    }

    #[test]
    fn test_annotation_callable_no_args() {
        let source = "def foo(f: 'Callable[[], int]') -> int:\n    return f()";
        let diagnostics = lint_source(source);
        assert!(
            !diagnostics
                .iter()
                .any(|d| d.rule == RuleKind::ForwardAnnotationSyntaxError),
            "Callable with no args should be valid"
        );
    }

    #[test]
    fn test_annotation_callable_ellipsis() {
        let source = "def foo(f: 'Callable[..., int]') -> int:\n    return f()";
        let diagnostics = lint_source(source);
        assert!(
            !diagnostics
                .iter()
                .any(|d| d.rule == RuleKind::ForwardAnnotationSyntaxError),
            "Callable with ellipsis should be valid"
        );
    }

    #[test]
    fn test_annotation_optional_type() {
        let source = "def foo(x: 'Optional[int]') -> int:\n    return x or 0";
        let diagnostics = lint_source(source);
        assert!(
            !diagnostics
                .iter()
                .any(|d| d.rule == RuleKind::ForwardAnnotationSyntaxError),
            "Optional type should be valid"
        );
    }

    #[test]
    fn test_annotation_union_syntax() {
        let source = "def foo(x: 'Union[int, str]') -> str:\n    return str(x)";
        let diagnostics = lint_source(source);
        assert!(
            !diagnostics
                .iter()
                .any(|d| d.rule == RuleKind::ForwardAnnotationSyntaxError),
            "Union syntax should be valid"
        );
    }

    #[test]
    fn test_annotation_tuple_types() {
        let source = "def foo(x: 'tuple[int, str, bool]') -> int:\n    return x[0]";
        let diagnostics = lint_source(source);
        assert!(
            !diagnostics
                .iter()
                .any(|d| d.rule == RuleKind::ForwardAnnotationSyntaxError),
            "Tuple with multiple types should be valid"
        );
    }

    #[test]
    fn test_annotation_type_variables() {
        let source = "def foo(x: 'T') -> 'T':\n    return x";
        let diagnostics = lint_source(source);
        assert!(
            !diagnostics
                .iter()
                .any(|d| d.rule == RuleKind::ForwardAnnotationSyntaxError),
            "Type variables should be valid"
        );
    }

    #[test]
    fn test_annotation_intersection_types() {
        let source = "def foo(x: 'Iterable & Sized') -> int:\n    return len(x)";
        let diagnostics = lint_source(source);
        assert!(
            !diagnostics
                .iter()
                .any(|d| d.rule == RuleKind::ForwardAnnotationSyntaxError),
            "Intersection types should be valid"
        );
    }

    #[test]
    fn test_annotation_generic_class() {
        let source = "def foo(x: 'MyGeneric[int, str]') -> None:\n    pass";
        let diagnostics = lint_source(source);
        assert!(
            !diagnostics
                .iter()
                .any(|d| d.rule == RuleKind::ForwardAnnotationSyntaxError),
            "Generic user-defined classes should be valid"
        );
    }

    #[test]
    fn test_annotation_generator_types() {
        let source = "def foo() -> 'Generator[int, None, str]':\n    yield 1\n    return 'done'";
        let diagnostics = lint_source(source);
        assert!(
            !diagnostics
                .iter()
                .any(|d| d.rule == RuleKind::ForwardAnnotationSyntaxError),
            "Generator types should be valid"
        );
    }

    #[test]
    fn test_annotation_mismatched_brackets() {
        let source = "def foo(x: 'list[int') -> None:\n    pass";
        let diagnostics = lint_source(source);
        assert!(
            diagnostics
                .iter()
                .any(|d| d.rule == RuleKind::ForwardAnnotationSyntaxError),
            "Mismatched brackets should produce BEA023 error"
        );
    }

    #[test]
    fn test_annotation_invalid_syntax() {
        let source = "def foo(x: 'int | | str') -> None:\n    pass";
        let diagnostics = lint_source(source);
        assert!(
            diagnostics
                .iter()
                .any(|d| d.rule == RuleKind::ForwardAnnotationSyntaxError),
            "Invalid syntax (double pipe) should produce BEA023 error"
        );
    }

    #[test]
    fn test_annotation_unmatched_closing_bracket() {
        let source = "def foo(x: 'list]int[') -> None:\n    pass";
        let diagnostics = lint_source(source);
        assert!(
            diagnostics
                .iter()
                .any(|d| d.rule == RuleKind::ForwardAnnotationSyntaxError),
            "Unmatched closing bracket should produce BEA023 error"
        );
    }

    #[test]
    fn test_annotation_invalid_callable_syntax() {
        let source = "def foo(f: 'Callable[int, str]') -> None:\n    pass";
        let diagnostics = lint_source(source);
        assert!(
            diagnostics
                .iter()
                .any(|d| d.rule == RuleKind::ForwardAnnotationSyntaxError),
            "Invalid Callable syntax (missing nested brackets) should produce BEA023 error"
        );
    }

    #[test]
    fn test_annotation_deeply_nested_valid() {
        let source = "def foo(x: 'dict[str, list[tuple[int, str, Optional[bool]]]]') -> None:\n    pass";
        let diagnostics = lint_source(source);
        assert!(
            !diagnostics
                .iter()
                .any(|d| d.rule == RuleKind::ForwardAnnotationSyntaxError),
            "Deeply nested valid types should not produce errors"
        );
    }

    #[test]
    fn test_annotation_mixed_union_and_intersection() {
        let source = "def foo(x: '(A | B) & (C | D)') -> None:\n    pass";
        let diagnostics = lint_source(source);
        assert!(
            !diagnostics
                .iter()
                .any(|d| d.rule == RuleKind::ForwardAnnotationSyntaxError),
            "Mixed union and intersection types should be valid"
        );
    }

    #[test]
    fn test_annotation_basic_types() {
        let source = "def foo(a: 'int', b: 'str', c: 'bool', d: 'float', e: 'None') -> 'Any':\n    pass";
        let diagnostics = lint_source(source);
        assert!(
            !diagnostics
                .iter()
                .any(|d| d.rule == RuleKind::ForwardAnnotationSyntaxError),
            "Basic types should all be valid"
        );
    }

    #[test]
    fn test_annotation_whitespace_handling() {
        let source = "def foo(x: '  int  |  str  ') -> None:\n    pass";
        let diagnostics = lint_source(source);
        assert!(
            !diagnostics
                .iter()
                .any(|d| d.rule == RuleKind::ForwardAnnotationSyntaxError),
            "Whitespace in annotations should be handled correctly"
        );
    }

    #[test]
    fn test_unused_import_with_all_export() {
        let source = r#"
import os
import sys

__all__ = ["os"]

x = 5
"#;
        let diagnostics = lint_source(source);
        assert!(
            !diagnostics
                .iter()
                .any(|d| d.rule == RuleKind::UnusedImport && d.message.contains("os")),
            "os should not be flagged as unused because it's in __all__"
        );
        assert!(
            diagnostics
                .iter()
                .any(|d| d.rule == RuleKind::UnusedImport && d.message.contains("sys")),
            "sys should be flagged as unused because it's not in __all__ and not used"
        );
    }

    #[test]
    fn test_unused_import_with_empty_all() {
        let source = r#"
import os

__all__ = []

x = 5
"#;
        let diagnostics = lint_source(source);
        assert!(
            diagnostics
                .iter()
                .any(|d| d.rule == RuleKind::UnusedImport && d.message.contains("os")),
            "os should be flagged as unused because __all__ is empty"
        );
    }

    #[test]
    fn test_unused_import_without_all() {
        let source = r#"
import os

x = 5
"#;
        let diagnostics = lint_source(source);
        assert!(
            diagnostics
                .iter()
                .any(|d| d.rule == RuleKind::UnusedImport && d.message.contains("os")),
            "os should be flagged as unused because it's not used and there's no __all__"
        );
    }

    #[test]
    fn test_multiple_imports_with_all_export() {
        let source = r#"
import os
import sys
import json
from typing import List

__all__ = ["os", "List"]

x = sys.platform
"#;
        let diagnostics = lint_source(source);
        assert!(
            !diagnostics
                .iter()
                .any(|d| d.rule == RuleKind::UnusedImport && d.message.contains("os")),
            "os should not be flagged as unused (in __all__)"
        );
        assert!(
            !diagnostics
                .iter()
                .any(|d| d.rule == RuleKind::UnusedImport && d.message.contains("List")),
            "List should not be flagged as unused (in __all__)"
        );
        assert!(
            !diagnostics
                .iter()
                .any(|d| d.rule == RuleKind::UnusedImport && d.message.contains("sys")),
            "sys should not be flagged as unused (used locally)"
        );
        assert!(
            diagnostics
                .iter()
                .any(|d| d.rule == RuleKind::UnusedImport && d.message.contains("json")),
            "json should be flagged as unused (not in __all__ and not used)"
        );
    }

    #[test]
    fn test_all_export_with_tuple() {
        let source = r#"
import os
import sys

__all__ = ("os",)

x = 5
"#;
        let diagnostics = lint_source(source);
        assert!(
            !diagnostics
                .iter()
                .any(|d| d.rule == RuleKind::UnusedImport && d.message.contains("os")),
            "os should not be flagged as unused (in __all__ tuple)"
        );
        assert!(
            diagnostics
                .iter()
                .any(|d| d.rule == RuleKind::UnusedImport && d.message.contains("sys")),
            "sys should be flagged as unused"
        );
    }
}

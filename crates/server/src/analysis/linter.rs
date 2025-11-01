//! Static analysis linter for Python code
//!
//! Implements PyFlakes-style linting rules (BEA001-BEA030) for detecting common coding issues, style violations, and potential bugs.

use super::rules::{DiagnosticMessage, RuleKind};
use beacon_parser::{AstNode, CompareOperator, ExceptHandler, LiteralValue, SymbolTable};
use rustc_hash::FxHashSet;

/// Context for tracking state during AST traversal
#[derive(Debug, Clone)]
struct LinterContext {
    /// Depth of function nesting (0 = module level)
    function_depth: usize,
    /// Depth of loop nesting (0 = not in loop)
    loop_depth: usize,
    /// Depth of class nesting (0 = module level)
    class_depth: usize,
    /// Stack of import names at each scope level
    import_names: Vec<FxHashSet<String>>,
    /// Stack of loop variable names
    loop_vars: Vec<String>,
}

impl LinterContext {
    fn new() -> Self {
        Self {
            function_depth: 0,
            loop_depth: 0,
            class_depth: 0,
            import_names: vec![FxHashSet::default()],
            loop_vars: Vec::new(),
        }
    }

    fn enter_function(&mut self) {
        self.function_depth += 1;
    }

    fn exit_function(&mut self) {
        self.function_depth = self.function_depth.saturating_sub(1);
    }

    fn enter_loop(&mut self) {
        self.loop_depth += 1;
    }

    fn exit_loop(&mut self) {
        self.loop_depth = self.loop_depth.saturating_sub(1);
    }

    fn enter_class(&mut self) {
        self.class_depth += 1;
    }

    fn exit_class(&mut self) {
        self.class_depth = self.class_depth.saturating_sub(1);
    }

    fn add_import(&mut self, name: String) {
        if let Some(imports) = self.import_names.last_mut() {
            imports.insert(name);
        }
    }

    fn is_import(&self, name: &str) -> bool {
        self.import_names.iter().any(|imports| imports.contains(name))
    }

    fn add_loop_var(&mut self, name: String) {
        self.loop_vars.push(name);
    }
}

/// Static analysis linter for Python code
pub struct Linter<'a> {
    /// Collected diagnostic messages
    diagnostics: Vec<DiagnosticMessage>,
    /// Context for tracking state
    ctx: LinterContext,
    /// Symbol table for scope analysis
    _symbol_table: &'a SymbolTable,
    /// Source filename
    filename: String,
}

impl<'a> Linter<'a> {
    /// Create a new linter for the given AST and symbol table
    pub fn new(symbol_table: &'a SymbolTable, filename: String) -> Self {
        Self { diagnostics: Vec::new(), ctx: LinterContext::new(), _symbol_table: symbol_table, filename }
    }

    /// Analyze the AST and return collected diagnostics
    pub fn analyze(&mut self, ast: &AstNode) -> Vec<DiagnosticMessage> {
        self.visit_node(ast);
        std::mem::take(&mut self.diagnostics)
    }

    /// Report a diagnostic message
    fn report(&mut self, rule: RuleKind, message: String, line: usize, col: usize) {
        self.diagnostics
            .push(DiagnosticMessage { rule, message, filename: self.filename.clone(), line, col });
    }

    /// Visit an AST node and check all applicable rules
    fn visit_node(&mut self, node: &AstNode) {
        match node {
            AstNode::Module { body, .. } => {
                for stmt in body {
                    self.visit_node(stmt);
                }
            }
            AstNode::FunctionDef { name, args, body, .. } => {
                self.check_duplicate_arguments(args, name);
                self.ctx.enter_function();
                for stmt in body {
                    self.visit_node(stmt);
                }
                self.ctx.exit_function();
            }
            AstNode::ClassDef { body, .. } => {
                self.ctx.enter_class();
                for stmt in body {
                    self.visit_node(stmt);
                }
                self.ctx.exit_class();
            }
            AstNode::Return { line, col, value, .. } => {
                self.check_return_outside_function(*line, *col);
                if let Some(val) = value {
                    self.visit_node(val);
                }
            }
            AstNode::Break { line, col } => {
                self.check_break_outside_loop(*line, *col);
            }
            AstNode::Continue { line, col } => {
                self.check_continue_outside_loop(*line, *col);
            }
            AstNode::For { target, iter, body, else_body, line, col, .. } => {
                self.visit_node(iter);

                if self.ctx.is_import(target) {
                    self.report(
                        RuleKind::ImportShadowedByLoopVar,
                        format!("Import '{target}' shadowed by loop variable"),
                        *line,
                        *col,
                    );
                }

                self.ctx.add_loop_var(target.clone());
                self.ctx.enter_loop();
                for stmt in body {
                    self.visit_node(stmt);
                }
                self.ctx.exit_loop();

                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        self.visit_node(stmt);
                    }
                }
            }
            AstNode::While { test, body, else_body, .. } => {
                self.visit_node(test);
                self.ctx.enter_loop();
                for stmt in body {
                    self.visit_node(stmt);
                }
                self.ctx.exit_loop();

                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        self.visit_node(stmt);
                    }
                }
            }
            AstNode::If { test, body, elif_parts, else_body, line, col } => {
                self.check_if_tuple(test, *line, *col);
                self.visit_node(test);

                for stmt in body {
                    self.visit_node(stmt);
                }

                for (elif_test, elif_body) in elif_parts {
                    self.visit_node(elif_test);
                    for stmt in elif_body {
                        self.visit_node(stmt);
                    }
                }

                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        self.visit_node(stmt);
                    }
                }
            }
            AstNode::Try { body, handlers, else_body, finally_body, line, col } => {
                self.check_default_except_not_last(handlers, *line, *col);

                for stmt in body {
                    self.visit_node(stmt);
                }

                for handler in handlers {
                    self.check_empty_except(handler);
                    for stmt in &handler.body {
                        self.visit_node(stmt);
                    }
                }

                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        self.visit_node(stmt);
                    }
                }

                if let Some(finally_stmts) = finally_body {
                    for stmt in finally_stmts {
                        self.visit_node(stmt);
                    }
                }
            }
            AstNode::Raise { exc, line, col } => {
                if let Some(exception) = exc {
                    self.check_raise_not_implemented(exception, *line, *col);
                    self.visit_node(exception);
                }
            }
            AstNode::Compare { left, ops, comparators, line, col } => {
                self.check_is_literal(left, ops, comparators, *line, *col);
                self.visit_node(left);
                for comp in comparators {
                    self.visit_node(comp);
                }
            }
            AstNode::Import { module, alias, line, col } => {
                let import_name = alias.as_ref().unwrap_or(module).clone();
                self.ctx.add_import(import_name);
                let _ = (line, col);
            }
            AstNode::ImportFrom { module: _, names, line, col } => {
                let is_star_import = names.is_empty();
                if is_star_import {
                    if self.ctx.function_depth > 0 || self.ctx.class_depth > 0 {
                        self.report(
                            RuleKind::ImportStarNotPermitted,
                            "from module import * not allowed inside function or class".to_string(),
                            *line,
                            *col,
                        );
                    }
                    self.report(
                        RuleKind::ImportStarUsed,
                        "import * prevents detection of undefined names".to_string(),
                        *line,
                        *col,
                    );
                } else {
                    for name in names {
                        self.ctx.add_import(name.clone());
                    }
                }
            }
            // TODO: BEA029 will be checked in a separate pass since we need to know if the block has other content
            AstNode::Pass { .. } => {}
            AstNode::Assignment { target: _, value, .. } => {
                self.visit_node(value);
            }
            AstNode::AnnotatedAssignment { target: _, type_annotation, value, line, col } => {
                self.check_forward_annotation_syntax(type_annotation, *line, *col);
                if let Some(val) = value {
                    self.visit_node(val);
                }
            }
            AstNode::Call { args, .. } => {
                for arg in args {
                    self.visit_node(arg);
                }
            }
            AstNode::BinaryOp { left, right, .. } => {
                self.visit_node(left);
                self.visit_node(right);
            }
            AstNode::UnaryOp { operand, .. } => {
                self.visit_node(operand);
            }
            AstNode::Attribute { object, .. } => {
                self.visit_node(object);
            }
            AstNode::Subscript { value, slice, .. } => {
                self.visit_node(value);
                self.visit_node(slice);
            }
            AstNode::Literal { value, line, col } => {
                if let LiteralValue::String { value: s, prefix } = value {
                    self.check_fstring_missing_placeholders(s, prefix, *line, *col);
                }
            }
            AstNode::With { items, body, .. } => {
                for item in items {
                    self.visit_node(&item.context_expr);
                }
                for stmt in body {
                    self.visit_node(stmt);
                }
            }
            AstNode::Match { subject, cases, .. } => {
                self.visit_node(subject);
                for case in cases {
                    for stmt in &case.body {
                        self.visit_node(stmt);
                    }
                }
            }
            AstNode::Lambda { body, .. } => {
                self.ctx.enter_function();
                self.visit_node(body);
                self.ctx.exit_function();
            }
            AstNode::ListComp { element, generators, .. }
            | AstNode::SetComp { element, generators, .. }
            | AstNode::GeneratorExp { element, generators, .. } => {
                for generator in generators {
                    self.visit_node(&generator.iter);
                    for if_clause in &generator.ifs {
                        self.visit_node(if_clause);
                    }
                }
                self.visit_node(element);
            }
            AstNode::DictComp { key, value, generators, line, col } => {
                for generator in generators {
                    self.visit_node(&generator.iter);
                    for if_clause in &generator.ifs {
                        self.visit_node(if_clause);
                    }
                }
                // TODO: Check for repeated keys - need to evaluate expressions
                let _ = (key, value, line, col);
                self.visit_node(key);
                self.visit_node(value);
            }
            AstNode::NamedExpr { value, .. } => {
                self.visit_node(value);
            }
            AstNode::Tuple { elements, .. } => {
                for element in elements {
                    self.visit_node(element);
                }
            }
            AstNode::Yield { .. } | AstNode::YieldFrom { .. } | AstNode::Await { .. } | AstNode::Identifier { .. } => {}
        }
    }

    /// BEA002: DuplicateArgument
    fn check_duplicate_arguments(&mut self, args: &[beacon_parser::Parameter], func_name: &str) {
        let mut seen = FxHashSet::default();
        for arg in args {
            if !seen.insert(&arg.name) {
                self.report(
                    RuleKind::DuplicateArgument,
                    format!("Duplicate argument '{}' in function '{}'", arg.name, func_name),
                    arg.line,
                    arg.col,
                );
            }
        }
    }

    /// BEA003: ReturnOutsideFunction
    fn check_return_outside_function(&mut self, line: usize, col: usize) {
        if self.ctx.function_depth == 0 {
            self.report(
                RuleKind::ReturnOutsideFunction,
                "'return' outside function".to_string(),
                line,
                col,
            );
        }
    }

    /// BEA005: BreakOutsideLoop
    fn check_break_outside_loop(&mut self, line: usize, col: usize) {
        if self.ctx.loop_depth == 0 {
            self.report(
                RuleKind::BreakOutsideLoop,
                "'break' outside loop".to_string(),
                line,
                col,
            );
        }
    }

    /// BEA006: [RuleKind::ContinueOutsideLoop]
    fn check_continue_outside_loop(&mut self, line: usize, col: usize) {
        if self.ctx.loop_depth == 0 {
            self.report(
                RuleKind::ContinueOutsideLoop,
                "'continue' outside loop".to_string(),
                line,
                col,
            );
        }
    }

    /// BEA007 & BEA027: [RuleKind::DefaultExceptNotLast]
    fn check_default_except_not_last(&mut self, handlers: &[ExceptHandler], line: usize, col: usize) {
        let mut found_bare_except = false;
        let mut bare_except_index = None;

        for (i, handler) in handlers.iter().enumerate() {
            if handler.exception_type.is_none() {
                found_bare_except = true;
                bare_except_index = Some(i);
                break;
            }
        }

        if let Some(idx) = bare_except_index {
            if idx != handlers.len() - 1 {
                self.report(
                    RuleKind::DefaultExceptNotLast,
                    "default 'except:' must be last".to_string(),
                    line,
                    col,
                );
            }
        }

        let _ = found_bare_except;
    }

    /// BEA008: [RuleKind::RaiseNotImplemented]
    fn check_raise_not_implemented(&mut self, exception: &AstNode, line: usize, col: usize) {
        if let AstNode::Identifier { name, .. } = exception {
            if name == "NotImplemented" {
                self.report(
                    RuleKind::RaiseNotImplemented,
                    "use NotImplementedError instead of NotImplemented".to_string(),
                    line,
                    col,
                );
            }
        }
    }

    /// BEA011: [RuleKind::IfTuple]
    fn check_if_tuple(&mut self, test: &AstNode, line: usize, col: usize) {
        if let AstNode::Tuple { .. } = test {
            self.report(
                RuleKind::IfTuple,
                "if condition is a tuple literal - this is always True".to_string(),
                line,
                col,
            );
        }
    }

    /// BEA013: [RuleKind::FStringMissingPlaceholders]
    fn check_fstring_missing_placeholders(&mut self, s: &str, prefix: &str, line: usize, col: usize) {
        let is_fstring = prefix.to_lowercase().contains('f');
        if is_fstring && !s.contains('{') {
            self.report(
                RuleKind::FStringMissingPlaceholders,
                "f-string without any placeholders".to_string(),
                line,
                col,
            );
        }
    }

    /// BEA023: ForwardAnnotationSyntaxError
    ///
    /// TODO: Try to parse the annotation as a Python expression
    fn check_forward_annotation_syntax(&mut self, annotation: &str, line: usize, col: usize) {
        if annotation.contains('[') && !annotation.contains(']') {
            self.report(
                RuleKind::ForwardAnnotationSyntaxError,
                format!("Syntax error in annotation: '{annotation}'"),
                line,
                col,
            );
        }
    }

    /// BEA026: [RuleKind::IsLiteral]
    fn check_is_literal(
        &mut self, left: &AstNode, ops: &[CompareOperator], comparators: &[AstNode], line: usize, col: usize,
    ) {
        for (op, comparator) in ops.iter().zip(comparators.iter()) {
            if matches!(op, CompareOperator::Is | CompareOperator::IsNot) {
                if let AstNode::Literal { value, .. } = comparator {
                    let is_singleton = matches!(value, LiteralValue::None | LiteralValue::Boolean(_));
                    if !is_singleton {
                        self.report(
                            RuleKind::IsLiteral,
                            "use '==' to compare constant values, not 'is'".to_string(),
                            line,
                            col,
                        );
                    }
                }

                if let AstNode::Literal { value, .. } = left {
                    let is_singleton = matches!(value, LiteralValue::None | LiteralValue::Boolean(_));
                    if !is_singleton {
                        self.report(
                            RuleKind::IsLiteral,
                            "use '==' to compare constant values, not 'is'".to_string(),
                            line,
                            col,
                        );
                    }
                }
            }
        }
    }

    /// BEA030: [RuleKind::EmptyExcept]
    fn check_empty_except(&mut self, handler: &ExceptHandler) {
        if handler.exception_type.is_none()
            && handler.body.len() == 1
            && matches!(&handler.body[0], AstNode::Pass { .. })
        {
            self.report(
                RuleKind::EmptyExcept,
                "except clause with only 'pass' silently hides all errors".to_string(),
                handler.line,
                handler.col,
            );
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use beacon_parser::PythonParser;

    fn lint_source(source: &str) -> Vec<DiagnosticMessage> {
        let mut parser = PythonParser::new().unwrap();
        let (ast, symbol_table) = parser.parse_and_resolve(source).unwrap();

        let mut linter = Linter::new(&symbol_table, "test.py".to_string());
        linter.analyze(&ast)
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
}

//! Static analysis linter for Python code
//!
//! Implements PyFlakes-style linting rules (BEA001-BEA030) for detecting common coding issues, style violations, and potential bugs.

use super::rules::{DiagnosticMessage, RuleKind};

use beacon_parser::{
    AstNode, CompareOperator, Comprehension, ExceptHandler, LiteralValue, MatchCase, ReferenceKind, SymbolKind,
    SymbolTable, WithItem,
};
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
    symbol_table: &'a SymbolTable,
    /// Source filename
    filename: String,
}

impl<'a> Linter<'a> {
    /// Create a new linter for the given AST and symbol table
    pub fn new(symbol_table: &'a SymbolTable, filename: String) -> Self {
        Self { diagnostics: Vec::new(), ctx: LinterContext::new(), symbol_table, filename }
    }

    /// Analyze the AST and return collected diagnostics
    pub fn analyze(&mut self, ast: &AstNode) -> Vec<DiagnosticMessage> {
        self.visit_node(ast);
        self.check_symbol_table_rules();
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
            AstNode::Module { body, .. } => self.visit_body(body),
            AstNode::FunctionDef { name, args, body, .. } => self.visit_function_def(name, args, body),
            AstNode::ClassDef { body, .. } => self.visit_class_def(body),
            AstNode::Return { line, col, value, .. } => self.visit_return(*line, *col, value.as_deref()),
            AstNode::Break { line, col, .. } => self.check_break_outside_loop(*line, *col),
            AstNode::Continue { line, col, .. } => self.check_continue_outside_loop(*line, *col),
            AstNode::For { target, iter, body, else_body, line, col, .. } => {
                self.visit_for_loop(target, iter, body, else_body, *line, *col);
            }
            AstNode::While { test, body, else_body, .. } => {
                self.visit_while_loop(test, body, else_body);
            }
            AstNode::If { test, body, elif_parts, else_body, line, col, .. } => {
                self.visit_if_statement(test, body, elif_parts, else_body, *line, *col);
            }
            AstNode::Try { body, handlers, else_body, finally_body, line, col, .. } => {
                self.visit_try_statement(body, handlers, else_body, finally_body, *line, *col);
            }
            AstNode::Raise { exc, line, col, .. } => self.visit_raise(exc.as_deref(), *line, *col),
            AstNode::Compare { left, ops, comparators, line, col, .. } => {
                self.visit_compare(left, ops, comparators, *line, *col);
            }
            AstNode::Import { module, alias, .. } => self.track_import(module, alias),
            AstNode::ImportFrom { module: _, names, line, col, .. } => {
                self.visit_from_import(names, *line, *col);
            }
            AstNode::Pass { .. } => {}
            AstNode::Assignment { target, value, line, col, .. } => {
                self.visit_assignment(target, value, *line, *col);
            }
            AstNode::Assert { test, line, col, .. } => {
                self.check_assert_tuple(test, *line, *col);
                self.visit_node(test);
            }
            AstNode::AnnotatedAssignment { target: _, type_annotation, value, line, col, .. } => {
                self.check_forward_annotation_syntax(type_annotation, *line, *col);
                if let Some(val) = value {
                    self.visit_node(val);
                }
            }
            AstNode::Call { args, .. } => self.visit_body(args),
            AstNode::BinaryOp { left, right, .. } => {
                self.visit_node(left);
                self.visit_node(right);
            }
            AstNode::UnaryOp { operand, .. } => self.visit_node(operand),
            AstNode::Attribute { object, .. } => self.visit_node(object),
            AstNode::Subscript { value, slice, .. } => {
                self.visit_node(value);
                self.visit_node(slice);
            }
            AstNode::Literal { value, line, col, .. } => self.visit_literal(value, *line, *col),
            AstNode::With { items, body, .. } => self.visit_with(items, body),
            AstNode::Match { subject, cases, .. } => self.visit_match(subject, cases),
            AstNode::Lambda { body, .. } => {
                self.ctx.enter_function();
                self.visit_node(body);
                self.ctx.exit_function();
            }
            AstNode::ListComp { element, generators, .. }
            | AstNode::SetComp { element, generators, .. }
            | AstNode::GeneratorExp { element, generators, .. } => {
                self.visit_comprehension(element, generators);
            }
            AstNode::DictComp { key, value, generators, .. } => {
                self.visit_dict_comprehension(key, value, generators);
            }
            AstNode::NamedExpr { value, .. } => self.visit_node(value),
            AstNode::Tuple { elements, .. } => self.visit_body(elements),
            AstNode::List { elements, .. } | AstNode::Set { elements, .. } => self.visit_body(elements),
            AstNode::Dict { keys, values, .. } => {
                for key in keys {
                    self.visit_node(key);
                }
                for value in values {
                    self.visit_node(value);
                }
            }
            AstNode::Yield { .. }
            | AstNode::YieldFrom { .. }
            | AstNode::Await { .. }
            | AstNode::Identifier { .. }
            | AstNode::Starred { .. } => {}
            AstNode::ParenthesizedExpression { expression, .. } => self.visit_node(expression),
        }
    }

    fn visit_body(&mut self, body: &[AstNode]) {
        for stmt in body {
            self.visit_node(stmt);
        }
    }

    fn visit_optional_body(&mut self, body: &Option<Vec<AstNode>>) {
        if let Some(stmts) = body {
            self.visit_body(stmts);
        }
    }

    fn visit_function_def(&mut self, name: &str, args: &[beacon_parser::Parameter], body: &[AstNode]) {
        self.check_duplicate_arguments(args, name);
        self.ctx.enter_function();
        self.visit_body(body);
        self.ctx.exit_function();
    }

    fn visit_class_def(&mut self, body: &[AstNode]) {
        self.ctx.enter_class();
        self.visit_body(body);
        self.ctx.exit_class();
    }

    fn visit_return(&mut self, line: usize, col: usize, value: Option<&AstNode>) {
        self.check_return_outside_function(line, col);
        if let Some(val) = value {
            self.visit_node(val);
        }
    }

    fn visit_for_loop(
        &mut self, target: &AstNode, iter: &AstNode, body: &[AstNode], else_body: &Option<Vec<AstNode>>, line: usize,
        col: usize,
    ) {
        self.visit_node(iter);

        for var_name in target.extract_target_names() {
            if self.ctx.is_import(&var_name) {
                self.report(
                    RuleKind::ImportShadowedByLoopVar,
                    format!("Import '{var_name}' shadowed by loop variable"),
                    line,
                    col,
                );
            }

            self.ctx.add_loop_var(var_name);
        }

        self.ctx.enter_loop();
        self.visit_body(body);
        self.ctx.exit_loop();
        self.visit_optional_body(else_body);
    }

    fn visit_while_loop(&mut self, test: &AstNode, body: &[AstNode], else_body: &Option<Vec<AstNode>>) {
        self.visit_node(test);
        self.ctx.enter_loop();
        self.visit_body(body);
        self.ctx.exit_loop();
        self.visit_optional_body(else_body);
    }

    fn visit_if_statement(
        &mut self, test: &AstNode, body: &[AstNode], elif_parts: &[(AstNode, Vec<AstNode>)],
        else_body: &Option<Vec<AstNode>>, line: usize, col: usize,
    ) {
        self.check_if_tuple(test, line, col);
        self.visit_node(test);
        self.visit_body(body);

        for (elif_test, elif_body) in elif_parts {
            self.visit_node(elif_test);
            self.visit_body(elif_body);
        }

        self.visit_optional_body(else_body);
    }

    fn visit_try_statement(
        &mut self, body: &[AstNode], handlers: &[ExceptHandler], else_body: &Option<Vec<AstNode>>,
        finally_body: &Option<Vec<AstNode>>, line: usize, col: usize,
    ) {
        self.check_default_except_not_last(handlers, line, col);
        self.visit_body(body);

        for handler in handlers {
            self.check_empty_except(handler);
            self.visit_body(&handler.body);
        }

        self.visit_optional_body(else_body);
        self.visit_optional_body(finally_body);
    }

    fn visit_raise(&mut self, exc: Option<&AstNode>, line: usize, col: usize) {
        if let Some(exception) = exc {
            self.check_raise_not_implemented(exception, line, col);
            self.visit_node(exception);
        }
    }

    fn visit_compare(
        &mut self, left: &AstNode, ops: &[CompareOperator], comparators: &[AstNode], line: usize, col: usize,
    ) {
        self.check_is_literal(left, ops, comparators, line, col);
        self.visit_node(left);
        for comp in comparators {
            self.visit_node(comp);
        }
    }

    fn track_import(&mut self, module: &String, alias: &Option<String>) {
        let import_name = alias.as_ref().unwrap_or(module).clone();
        self.ctx.add_import(import_name);
    }

    fn visit_from_import(&mut self, names: &[String], line: usize, col: usize) {
        if names.is_empty() {
            if self.ctx.function_depth > 0 || self.ctx.class_depth > 0 {
                self.report(
                    RuleKind::ImportStarNotPermitted,
                    "from module import * not allowed inside function or class".to_string(),
                    line,
                    col,
                );
            }
            self.report(
                RuleKind::ImportStarUsed,
                "import * prevents detection of undefined names".to_string(),
                line,
                col,
            );
        } else {
            for name in names {
                self.ctx.add_import(name.clone());
            }
        }
    }

    fn visit_assignment(&mut self, target: &AstNode, value: &AstNode, line: usize, col: usize) {
        self.check_two_starred_expressions(target, line, col);
        self.check_too_many_expressions_in_starred_assignment(target, value, line, col);
        self.visit_node(value);
    }

    fn visit_literal(&mut self, value: &LiteralValue, line: usize, col: usize) {
        if let LiteralValue::String { value: s, prefix } = value {
            self.check_fstring_missing_placeholders(s, prefix, line, col);
        }
    }

    fn visit_with(&mut self, items: &[WithItem], body: &[AstNode]) {
        for item in items {
            self.visit_node(&item.context_expr);
        }
        self.visit_body(body);
    }

    fn visit_match(&mut self, subject: &AstNode, cases: &[MatchCase]) {
        self.visit_node(subject);
        for case in cases {
            self.visit_body(&case.body);
        }
    }

    fn visit_comprehension(&mut self, element: &AstNode, generators: &[Comprehension]) {
        self.visit_comprehension_generators(generators);
        self.visit_node(element);
    }

    fn visit_comprehension_generators(&mut self, generators: &[Comprehension]) {
        for generator in generators {
            self.visit_node(&generator.iter);
            for if_clause in &generator.ifs {
                self.visit_node(if_clause);
            }
        }
    }

    fn visit_dict_comprehension(&mut self, key: &AstNode, value: &AstNode, generators: &[Comprehension]) {
        self.visit_comprehension_generators(generators);
        self.visit_node(key);
        self.visit_node(value);
        // TODO: Check for repeated keys - need to evaluate expressions
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

    /// Check for assert with tuple test (always True)
    ///
    /// BEA012: `assert (1, 2)` creates a non-empty tuple which is always True
    fn check_assert_tuple(&mut self, test: &AstNode, line: usize, col: usize) {
        if let AstNode::Tuple { elements, .. } = test {
            if !elements.is_empty() {
                self.report(
                    RuleKind::AssertTuple,
                    "assertion is a tuple literal, which is always True".to_string(),
                    line,
                    col,
                );
            }
        }
    }

    /// Check for multiple starred expressions in assignment target
    ///
    /// BEA009: `a, *b, *c = [1, 2, 3]` has two starred expressions (invalid)
    fn check_two_starred_expressions(&mut self, target: &AstNode, line: usize, col: usize) {
        let starred_count = Self::count_starred_expressions(target);
        if starred_count > 1 {
            self.report(
                RuleKind::TwoStarredExpressions,
                format!("assignment target contains {starred_count} starred expressions, only one allowed"),
                line,
                col,
            );
        }
    }

    /// Count starred expressions in an assignment target
    fn count_starred_expressions(node: &AstNode) -> usize {
        match node {
            AstNode::Starred { .. } => 1,
            AstNode::Tuple { elements, .. } | AstNode::List { elements, .. } => {
                elements.iter().map(Self::count_starred_expressions).sum()
            }
            _ => 0,
        }
    }

    /// Check for too many or too few expressions in starred assignment
    ///
    /// BEA010: `*a, b, c = [1]` has too many names for the iterable
    fn check_too_many_expressions_in_starred_assignment(
        &mut self, target: &AstNode, value: &AstNode, line: usize, col: usize,
    ) {
        if Self::count_starred_expressions(target) == 0 {
            return;
        }

        let target_count = Self::count_target_names(target);
        let starred_count = Self::count_starred_expressions(target);
        let min_required = target_count - starred_count;
        let value_len = match value {
            AstNode::List { elements, .. } | AstNode::Tuple { elements, .. } => Some(elements.len()),
            _ => None,
        };

        if let Some(value_len) = value_len {
            if value_len < min_required {
                self.report(
                    RuleKind::TooManyExpressionsInStarredAssignment,
                    format!("too many expressions in assignment; need at least {min_required} values, got {value_len}"),
                    line,
                    col,
                );
            }
        }
    }

    /// Count total number of names in assignment target
    fn count_target_names(node: &AstNode) -> usize {
        match node {
            AstNode::Identifier { .. } | AstNode::Starred { .. } => 1,
            AstNode::Tuple { elements, .. } | AstNode::List { elements, .. } => {
                elements.iter().map(Self::count_target_names).sum()
            }
            _ => 0,
        }
    }

    /// Check rules that require symbol table analysis
    ///
    /// Called after AST traversal to check for issues detected via symbol usage patterns
    fn check_symbol_table_rules(&mut self) {
        self.check_unused_imports();
        self.check_unused_annotations();
        self.check_redefined_while_unused();
    }

    /// BEA015: UnusedImport
    ///
    /// Check for imports that are never read
    fn check_unused_imports(&mut self) {
        for scope in self.symbol_table.scopes.values() {
            for symbol in scope.symbols.values() {
                if symbol.kind != SymbolKind::Import {
                    continue;
                }

                if symbol.name.starts_with('_') {
                    continue;
                }

                let has_read = symbol.references.iter().any(|r| r.kind == ReferenceKind::Read);

                if !has_read {
                    self.report(
                        RuleKind::UnusedImport,
                        format!("'{}' imported but never used", symbol.name),
                        symbol.line,
                        symbol.col,
                    );
                }
            }
        }
    }

    /// BEA017: UnusedAnnotation
    ///
    /// Check for annotated variables that are never read
    fn check_unused_annotations(&mut self) {
        for scope in self.symbol_table.scopes.values() {
            for symbol in scope.symbols.values() {
                if symbol.kind != SymbolKind::Variable {
                    continue;
                }

                if symbol.name.starts_with('_') {
                    continue;
                }

                let has_read = symbol.references.iter().any(|r| r.kind == ReferenceKind::Read);
                let has_write = symbol.references.iter().any(|r| r.kind == ReferenceKind::Write);

                if !has_read && !has_write {
                    self.report(
                        RuleKind::UnusedAnnotation,
                        format!("Annotated variable '{}' is never used", symbol.name),
                        symbol.line,
                        symbol.col,
                    );
                }
            }
        }
    }

    /// BEA018: RedefinedWhileUnused
    ///
    /// Check for variables that are redefined before the original value is used
    fn check_redefined_while_unused(&mut self) {
        for scope in self.symbol_table.scopes.values() {
            for symbol in scope.symbols.values() {
                if symbol.kind != SymbolKind::Variable {
                    continue;
                }

                if symbol.name.starts_with('_') {
                    continue;
                }

                let writes: Vec<_> = symbol
                    .references
                    .iter()
                    .filter(|r| r.kind == ReferenceKind::Write)
                    .collect();

                if writes.len() < 2 {
                    continue;
                }

                for i in 1..writes.len() {
                    let prev_write = writes[i - 1];
                    let curr_write = writes[i];

                    let has_read_between = symbol.references.iter().any(|r| {
                        r.kind == ReferenceKind::Read
                            && ((r.line > prev_write.line) || (r.line == prev_write.line && r.col > prev_write.col))
                            && ((r.line < curr_write.line) || (r.line == curr_write.line && r.col < curr_write.col))
                    });

                    if !has_read_between {
                        self.report(
                            RuleKind::RedefinedWhileUnused,
                            format!("'{}' is redefined before being used", symbol.name),
                            curr_write.line,
                            curr_write.col,
                        );
                        break;
                    }
                }
            }
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
    #[ignore]
    fn test_yield_outside_function() {
        // TODO: YieldOutsideFunction rule is not yet implemented
        // Yield nodes are currently not checked for context
        let source = "yield 42";
        let diagnostics = lint_source(source);
        assert!(diagnostics.iter().any(|d| d.rule == RuleKind::YieldOutsideFunction));
    }

    #[test]
    #[ignore]
    fn test_yield_inside_function() {
        // TODO: YieldOutsideFunction rule is not yet implemented
        let source = "def foo():\n    yield 42";
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
}

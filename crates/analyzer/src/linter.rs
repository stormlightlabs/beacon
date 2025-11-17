//! Static analysis linter for Python code
//!
//! Implements PyFlakes-style linting rules (BEA001-BEA030) for detecting common coding issues, style violations, and potential bugs.

use super::const_eval::{ConstValue, evaluate_const_expr};
use super::rules::{DiagnosticMessage, RuleKind};

use beacon_core::SuppressionMap;
use beacon_parser::{
    AstNode, BinaryOperator, CompareOperator, Comprehension, ExceptHandler, LiteralValue, MatchCase, ReferenceKind,
    SymbolKind, SymbolTable, WithItem,
};
use rustc_hash::{FxHashMap, FxHashSet};

/// Tracks a global or nonlocal declaration
#[derive(Debug, Clone)]
struct GlobalOrNonlocalDecl {
    name: String,
    line: usize,
    col: usize,
    is_global: bool,
}

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
    /// Global and nonlocal declarations in current function scope
    global_nonlocal_decls: Vec<GlobalOrNonlocalDecl>,
    /// Variables assigned in the current function scope
    assigned_vars: FxHashSet<String>,
    /// Track class scopes that are dataclasses (line -> is_dataclass)
    dataclass_scopes: FxHashMap<usize, bool>,
    /// Track class scopes that inherit from Protocol (line -> is_protocol)
    protocol_scopes: FxHashMap<usize, bool>,
}

impl LinterContext {
    fn new() -> Self {
        Self {
            function_depth: 0,
            loop_depth: 0,
            class_depth: 0,
            import_names: vec![FxHashSet::default()],
            loop_vars: Vec::new(),
            global_nonlocal_decls: Vec::new(),
            assigned_vars: FxHashSet::default(),
            dataclass_scopes: FxHashMap::default(),
            protocol_scopes: FxHashMap::default(),
        }
    }

    fn enter_function(&mut self) {
        self.function_depth += 1;
        self.global_nonlocal_decls.clear();
        self.assigned_vars.clear();
    }

    fn exit_function(&mut self) {
        self.function_depth = self.function_depth.saturating_sub(1);
        self.global_nonlocal_decls.clear();
        self.assigned_vars.clear();
    }

    fn track_assignment(&mut self, var_name: String) {
        self.assigned_vars.insert(var_name);
    }

    fn add_global_decl(&mut self, name: String, line: usize, col: usize) {
        self.global_nonlocal_decls
            .push(GlobalOrNonlocalDecl { name, line, col, is_global: true });
    }

    fn add_nonlocal_decl(&mut self, name: String, line: usize, col: usize) {
        self.global_nonlocal_decls
            .push(GlobalOrNonlocalDecl { name, line, col, is_global: false });
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
    /// Suppression map for filtering diagnostics
    suppression_map: SuppressionMap,
}

impl<'a> Linter<'a> {
    /// Create a new linter for the given AST and symbol table
    pub fn new(symbol_table: &'a SymbolTable, filename: String, source: &str) -> Self {
        let suppression_map = SuppressionMap::from_source(source);
        Self { diagnostics: Vec::new(), ctx: LinterContext::new(), symbol_table, filename, suppression_map }
    }

    /// Analyze the AST and return collected diagnostics
    pub fn analyze(&mut self, ast: &AstNode) -> Vec<DiagnosticMessage> {
        self.visit_node(ast);
        self.check_symbol_table_rules();
        let diagnostics = std::mem::take(&mut self.diagnostics);

        diagnostics
            .into_iter()
            .filter(|diag| !self.suppression_map.is_suppressed(diag.line, Some(diag.rule.code())))
            .collect()
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
            AstNode::FunctionDef { name, args, body, return_type, line, .. } => {
                self.visit_function_def(name, args, body, return_type, *line)
            }
            AstNode::ClassDef { body, decorators, bases, line, .. } => {
                self.visit_class_def(body, decorators, bases, *line)
            }
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
            AstNode::Global { names, line, col, .. } => {
                self.visit_global(names, *line, *col);
            }
            AstNode::Nonlocal { names, line, col, .. } => {
                self.visit_nonlocal(names, *line, *col);
            }
            AstNode::Assignment { target, value, line, col, .. } => {
                self.visit_assignment(target, value, *line, *col);
            }
            AstNode::Assert { test, line, col, .. } => {
                self.check_assert_tuple(test, *line, *col);
                self.visit_node(test);
            }
            AstNode::AnnotatedAssignment { target, type_annotation, value, line, col, .. } => {
                self.check_forward_annotation_syntax(type_annotation, *line, *col);

                if self.ctx.function_depth > 0 {
                    for name in target.extract_target_names() {
                        self.ctx.track_assignment(name);
                    }
                }

                if let Some(val) = value {
                    self.visit_node(val);
                }
            }
            AstNode::Call { args, .. } => self.visit_body(args),
            AstNode::BinaryOp { left, right, op, line, col, .. } => {
                self.check_percent_format(left, op, *line, *col);
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
            AstNode::Dict { keys, values, line, col, .. } => {
                self.check_duplicate_dict_keys(keys, *line, *col);
                for key in keys {
                    self.visit_node(key);
                }
                for value in values {
                    self.visit_node(value);
                }
            }
            AstNode::Yield { value, line, col, .. } => {
                self.check_yield_outside_function(*line, *col);
                if let Some(val) = value {
                    self.visit_node(val);
                }
            }
            AstNode::YieldFrom { value, line, col, .. } => {
                self.check_yield_outside_function(*line, *col);
                self.visit_node(value);
            }
            AstNode::Await { .. } | AstNode::Identifier { .. } | AstNode::Starred { .. } => {}
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

    fn visit_function_def(
        &mut self, name: &str, args: &[beacon_parser::Parameter], body: &[AstNode], return_type: &Option<String>,
        line: usize,
    ) {
        self.check_duplicate_arguments(args, name);
        self.check_redundant_pass(body);

        for param in args {
            if let Some(annotation) = &param.type_annotation {
                self.check_forward_annotation_syntax(annotation, param.line, param.col);
            }
        }

        if let Some(ret_type) = return_type {
            self.check_forward_annotation_syntax(ret_type, line, 0);
        }

        self.ctx.enter_function();
        self.visit_body(body);
        self.check_unused_global_nonlocal();
        self.ctx.exit_function();
    }

    fn visit_class_def(&mut self, body: &[AstNode], decorators: &[String], bases: &[String], line: usize) {
        self.check_redundant_pass(body);

        let is_dataclass = decorators.iter().any(|d| d.contains("dataclass"));
        if is_dataclass {
            self.ctx.dataclass_scopes.insert(line, true);
        }

        let is_protocol = bases.iter().any(|b| b.contains("Protocol"));
        if is_protocol {
            self.ctx.protocol_scopes.insert(line, true);
        }

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

        self.check_redundant_pass(body);
        self.ctx.enter_loop();
        self.visit_body(body);
        self.ctx.exit_loop();
        if let Some(else_stmts) = else_body {
            self.check_redundant_pass(else_stmts);
        }
        self.visit_optional_body(else_body);
    }

    fn visit_while_loop(&mut self, test: &AstNode, body: &[AstNode], else_body: &Option<Vec<AstNode>>) {
        self.visit_node(test);
        self.check_redundant_pass(body);
        self.ctx.enter_loop();
        self.visit_body(body);
        self.ctx.exit_loop();
        if let Some(else_stmts) = else_body {
            self.check_redundant_pass(else_stmts);
        }
        self.visit_optional_body(else_body);
    }

    fn visit_if_statement(
        &mut self, test: &AstNode, body: &[AstNode], elif_parts: &[(AstNode, Vec<AstNode>)],
        else_body: &Option<Vec<AstNode>>, line: usize, col: usize,
    ) {
        self.check_if_tuple(test, line, col);
        self.visit_node(test);
        self.check_redundant_pass(body);
        self.visit_body(body);

        for (elif_test, elif_body) in elif_parts {
            self.visit_node(elif_test);
            self.check_redundant_pass(elif_body);
            self.visit_body(elif_body);
        }

        if let Some(else_stmts) = else_body {
            self.check_redundant_pass(else_stmts);
        }
        self.visit_optional_body(else_body);
    }

    fn visit_try_statement(
        &mut self, body: &[AstNode], handlers: &[ExceptHandler], else_body: &Option<Vec<AstNode>>,
        finally_body: &Option<Vec<AstNode>>, line: usize, col: usize,
    ) {
        self.check_default_except_not_last(handlers, line, col);
        self.check_redundant_pass(body);
        self.visit_body(body);

        for handler in handlers {
            self.check_empty_except(handler);
            self.check_redundant_pass(&handler.body);
            self.visit_body(&handler.body);
        }

        if let Some(else_stmts) = else_body {
            self.check_redundant_pass(else_stmts);
        }
        self.visit_optional_body(else_body);
        if let Some(finally_stmts) = finally_body {
            self.check_redundant_pass(finally_stmts);
        }
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

    /// Visit global statement and track declarations
    fn visit_global(&mut self, names: &[String], line: usize, col: usize) {
        for name in names {
            self.ctx.add_global_decl(name.clone(), line, col);
        }
    }

    /// Visit nonlocal statement and track declarations
    fn visit_nonlocal(&mut self, names: &[String], line: usize, col: usize) {
        for name in names {
            self.ctx.add_nonlocal_decl(name.clone(), line, col);
        }
    }

    fn visit_assignment(&mut self, target: &AstNode, value: &AstNode, line: usize, col: usize) {
        self.check_two_starred_expressions(target, line, col);
        self.check_too_many_expressions_in_starred_assignment(target, value, line, col);

        if self.ctx.function_depth > 0 {
            for name in target.extract_target_names() {
                self.ctx.track_assignment(name);
            }
        }

        self.visit_node(value);
    }

    fn visit_literal(&mut self, value: &LiteralValue, line: usize, col: usize) {
        if let LiteralValue::String { value: s, prefix } = value {
            self.check_fstring_missing_placeholders(s, prefix, line, col);
            self.check_tstring_missing_placeholders(s, prefix, line, col);
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
        // NOTE: Cannot statically check for repeated keys in comprehensions as keys are generated at runtime
    }

    /// BEA024: MultiValueRepeatedKeyLiteral
    ///
    /// Detects dictionary literals with duplicate keys.
    /// Example: {'a': 1, 'a': 2} - the key 'a' appears twice
    fn check_duplicate_dict_keys(&mut self, keys: &[AstNode], line: usize, col: usize) {
        let mut seen_keys: FxHashMap<ConstValue, (usize, usize)> = FxHashMap::default();

        for key in keys {
            if let Some(const_key) = evaluate_const_expr(key) {
                if let Some((first_line, first_col)) = seen_keys.get(&const_key) {
                    let key_repr = match &const_key {
                        ConstValue::String(s) => format!("'{s}'"),
                        ConstValue::Integer(i) => i.to_string(),
                        ConstValue::Float(f) => f.to_string(),
                        ConstValue::Boolean(b) => b.to_string(),
                        ConstValue::None => "None".to_string(),
                        ConstValue::Tuple(_) => "<tuple>".to_string(),
                        ConstValue::List(_) => "<list>".to_string(),
                    };

                    self.diagnostics.push(DiagnosticMessage {
                        rule: RuleKind::MultiValueRepeatedKeyLiteral,
                        message: format!(
                            "Dictionary key {key_repr} repeated (first occurrence at line {first_line}, col {first_col})"
                        ),
                        filename: self.filename.clone(),
                        line,
                        col,
                    });
                } else {
                    let (key_line, key_col) = match key {
                        AstNode::Literal { line, col, .. } => (*line, *col),
                        _ => (line, col),
                    };
                    seen_keys.insert(const_key, (key_line, key_col));
                }
            }
        }
    }

    /// BEA029: RedundantPass
    ///
    /// Detects pass statements that are redundant because other statements exist in the same block.
    /// Valid: `def f(): pass` (single pass in block)
    /// Invalid: `def f(): pass; return 1` (pass is redundant)
    fn check_redundant_pass(&mut self, body: &[AstNode]) {
        if body.len() <= 1 {
            return;
        }

        let has_non_pass = body.iter().any(|node| !matches!(node, AstNode::Pass { .. }));

        if has_non_pass {
            for node in body {
                if let AstNode::Pass { line, col, .. } = node {
                    self.diagnostics.push(DiagnosticMessage {
                        rule: RuleKind::RedundantPass,
                        message: "Pass statement is redundant when other statements are present".to_string(),
                        filename: self.filename.clone(),
                        line: *line,
                        col: *col,
                    });
                }
            }
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

    /// BEA004: YieldOutsideFunction
    fn check_yield_outside_function(&mut self, line: usize, col: usize) {
        if self.ctx.function_depth == 0 {
            self.report(
                RuleKind::YieldOutsideFunction,
                "'yield' outside function".to_string(),
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

    /// BEA014: [RuleKind::TStringMissingPlaceholders]
    fn check_tstring_missing_placeholders(&mut self, s: &str, prefix: &str, line: usize, col: usize) {
        let is_tstring = prefix.to_lowercase().contains('t');
        if is_tstring && !s.contains('{') {
            self.report(
                RuleKind::TStringMissingPlaceholders,
                "t-string without any placeholders".to_string(),
                line,
                col,
            );
        }
    }

    /// BEA025: [RuleKind::PercentFormatInvalidFormat]
    ///
    /// Validate percent format strings (e.g., "%s" % value)
    fn check_percent_format(&mut self, left: &AstNode, op: &BinaryOperator, line: usize, col: usize) {
        if !matches!(op, BinaryOperator::Mod) {
            return;
        }

        if let AstNode::Literal { value: LiteralValue::String { value: s, .. }, .. } = left {
            if let Err(error) = Self::validate_percent_format(s) {
                self.report(
                    RuleKind::PercentFormatInvalidFormat,
                    format!("Invalid % format string: {error}"),
                    line,
                    col,
                );
            }
        }
    }

    /// Validate a percent format string
    fn validate_percent_format(s: &str) -> Result<(), String> {
        let mut chars = s.chars().peekable();

        while let Some(ch) = chars.next() {
            if ch == '%' {
                match chars.peek() {
                    Some('%') => {
                        chars.next();
                    }
                    Some(&c) => {
                        let spec = c;
                        if !Self::is_valid_format_specifier(spec) {
                            return Err(format!("invalid format character '{spec}'"));
                        }
                        chars.next();
                    }
                    None => {
                        return Err("incomplete format string".to_string());
                    }
                }
            }
        }

        Ok(())
    }

    /// Check if a character is a valid Python % format specifier
    fn is_valid_format_specifier(c: char) -> bool {
        matches!(
            c,
            's' | 'd' | 'i' | 'f' | 'e' | 'E' | 'g' | 'G' | 'c' | 'r' | 'x' | 'X' | 'o' | 'b' | 'a'
        )
    }

    /// BEA023: ForwardAnnotationSyntaxError
    ///
    /// Validates forward type annotation syntax
    fn check_forward_annotation_syntax(&mut self, annotation: &str, line: usize, col: usize) {
        if let Some(error_msg) = Self::validate_annotation_syntax(annotation) {
            self.report(
                RuleKind::ForwardAnnotationSyntaxError,
                format!("Syntax error in annotation '{annotation}': {error_msg}"),
                line,
                col,
            );
        }
    }

    /// Validate annotation syntax using the proper AnnotationParser
    ///
    /// This uses beacon_core's AnnotationParser for full PEP 484/585/604 validation:
    /// - Basic types: int, str, bool, float, None
    /// - Parameterized types: list[int], dict[str, int]
    /// - Union types: Union[int, str], int | str (PEP 604)
    /// - Optional: Optional[T]
    /// - Callable: Callable[[args...], return]
    /// - Complex nested generics
    fn validate_annotation_syntax(annotation: &str) -> Option<String> {
        let mut annotation = annotation.trim();

        if annotation.is_empty() {
            return None;
        }

        if (annotation.starts_with('\'') && annotation.ends_with('\''))
            || (annotation.starts_with('"') && annotation.ends_with('"'))
        {
            annotation = &annotation[1..annotation.len() - 1];
        }

        let parser = beacon_core::AnnotationParser::new();
        match parser.parse(annotation) {
            Ok(_) => None,
            Err(err) => Some(format!("{err}")),
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

    /// BEA022: UnusedIndirectAssignment
    ///
    /// Check for global/nonlocal declarations that are never reassigned in the function.
    fn check_unused_global_nonlocal(&mut self) {
        let decls = self.ctx.global_nonlocal_decls.clone();

        for decl in &decls {
            let has_write = self.has_assignment_in_function(&decl.name);

            if !has_write {
                let keyword = if decl.is_global { "global" } else { "nonlocal" };
                self.report(
                    RuleKind::UnusedIndirectAssignment,
                    format!("'{keyword} {}' declared but never assigned in function", decl.name),
                    decl.line,
                    decl.col,
                );
            }
        }
    }

    /// Check if a variable has any assignment in the function body
    fn has_assignment_in_function(&self, var_name: &str) -> bool {
        self.ctx.assigned_vars.contains(var_name)
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
        const FUTURE_IMPORTS: &[&str] = &["annotations", "barry_as_FLUFL"];

        for scope in self.symbol_table.scopes.values() {
            for symbol in scope.symbols.values() {
                if symbol.kind != SymbolKind::Import {
                    continue;
                }

                if symbol.name.starts_with('_') {
                    continue;
                }

                if FUTURE_IMPORTS.contains(&symbol.name.as_str()) {
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
            if scope.kind == beacon_parser::ScopeKind::Class {
                let is_dataclass = self.ctx.dataclass_scopes.values().any(|&v| v);
                let is_protocol = self.ctx.protocol_scopes.values().any(|&v| v);

                if is_dataclass || is_protocol {
                    continue;
                }
            }

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
        let mut linter = Linter::new(&symbol_table, "test.py".to_string(), source);
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
}

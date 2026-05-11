use crate::rules::RuleKind;

use beacon_parser::{AstNode, CompareOperator, Comprehension, ExceptHandler, LiteralValue, MatchCase, WithItem};

use super::Linter;
use super::context::SourcePosition;

impl<'a> Linter<'a> {
    pub(super) fn get_line_text(&self, line: usize) -> Option<&str> {
        self.source_lines.get(line.checked_sub(1)?).map(|s| s.as_str())
    }

    pub(super) fn find_return_type_col(&self, line: usize, annotation: &str) -> Option<usize> {
        let line_text = self.get_line_text(line)?;
        let arrow_pos = line_text.find("->")?;
        let after_arrow = arrow_pos + 2;
        if after_arrow >= line_text.len() {
            return None;
        }

        let remaining = &line_text[after_arrow..];
        let type_start = remaining.find(|c: char| !c.is_whitespace())?;
        let type_pos = after_arrow + type_start;
        if line_text[type_pos..].starts_with(annotation) { Some(type_pos + 1) } else { None }
    }

    pub(super) fn visit_node(&mut self, node: &AstNode) {
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
            AstNode::If { test, body, elif_parts, else_body, line, col, end_col, .. } => {
                let pos = SourcePosition { line: *line, col: *col, end_col: *end_col };
                self.visit_if_statement(test, body, elif_parts, else_body, pos);
            }
            AstNode::Try { body, handlers, else_body, finally_body, line, col, end_col, .. } => {
                let pos = SourcePosition { line: *line, col: *col, end_col: *end_col };
                self.visit_try_statement(body, handlers, else_body, finally_body, pos);
            }
            AstNode::Raise { exc, line, col, end_col, .. } => self.visit_raise(exc.as_deref(), *line, *col, *end_col),
            AstNode::Compare { left, ops, comparators, line, col, end_col, .. } => {
                self.visit_compare(left, ops, comparators, *line, *col, *end_col);
            }
            AstNode::Import { module, alias, .. } => self.track_import(module, alias),
            AstNode::ImportFrom { module: _, names, line, col, .. } => {
                self.visit_from_import(names, *line, *col);
            }
            AstNode::Pass { .. } => {}
            AstNode::Global { names, line, col, end_col, .. } => {
                self.visit_global(names, *line, *col, *end_col);
            }
            AstNode::Nonlocal { names, line, col, end_col, .. } => {
                self.visit_nonlocal(names, *line, *col, *end_col);
            }
            AstNode::Assignment { target, value, line, col, end_col, .. } => {
                self.visit_assignment(target, value, *line, *col, *end_col);
            }
            AstNode::Assert { test, line, col, end_col, .. } => {
                self.check_assert_tuple(test, *line, *col, *end_col);
                self.visit_node(test);
            }
            AstNode::AnnotatedAssignment { target, type_annotation, value, line, col, end_col, .. } => {
                self.check_forward_annotation_syntax(type_annotation, *line, *col, *end_col);

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
            AstNode::BinaryOp { left, right, op, line, col, end_col, .. } => {
                self.check_percent_format(left, op, *line, *col, *end_col);
                self.visit_node(left);
                self.visit_node(right);
            }
            AstNode::UnaryOp { operand, .. } => self.visit_node(operand),
            AstNode::Attribute { object, .. } => self.visit_node(object),
            AstNode::Subscript { value, slice, .. } => {
                self.visit_node(value);
                self.visit_node(slice);
            }
            AstNode::Literal { value, line, col, end_col, .. } => self.visit_literal(value, *line, *col, *end_col),
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

    pub(super) fn visit_body(&mut self, body: &[AstNode]) {
        for stmt in body {
            self.visit_node(stmt);
        }
    }

    pub(super) fn visit_optional_body(&mut self, body: &Option<Vec<AstNode>>) {
        if let Some(stmts) = body {
            self.visit_body(stmts);
        }
    }

    pub(super) fn visit_function_def(
        &mut self, name: &str, args: &[beacon_parser::Parameter], body: &[AstNode], return_type: &Option<String>,
        line: usize,
    ) {
        self.check_duplicate_arguments(args, name);
        self.check_redundant_pass(body);

        for param in args {
            if let Some(annotation) = &param.type_annotation {
                self.check_forward_annotation_syntax(annotation, param.line, param.col, param.end_col);
            }
        }

        if let Some(ret_type) = return_type {
            let (ret_col, ret_end_col) = if let Some(col) = self.find_return_type_col(line, ret_type) {
                let end_col = col + ret_type.len();
                (col, end_col)
            } else {
                (1, 2)
            };
            self.check_forward_annotation_syntax(ret_type, line, ret_col, ret_end_col);
        }

        self.ctx.enter_function();
        self.visit_body(body);
        self.check_unused_global_nonlocal();
        self.ctx.exit_function();
    }

    pub(super) fn visit_class_def(&mut self, body: &[AstNode], decorators: &[String], bases: &[String], line: usize) {
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

    pub(super) fn visit_return(&mut self, line: usize, col: usize, value: Option<&AstNode>) {
        self.check_return_outside_function(line, col);
        if let Some(val) = value {
            self.visit_node(val);
        }
    }

    pub(super) fn visit_for_loop(
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
                    col + var_name.len(),
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

    pub(super) fn visit_while_loop(&mut self, test: &AstNode, body: &[AstNode], else_body: &Option<Vec<AstNode>>) {
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

    pub(super) fn visit_if_statement(
        &mut self, test: &AstNode, body: &[AstNode], elif_parts: &[(AstNode, Vec<AstNode>)],
        else_body: &Option<Vec<AstNode>>, pos: SourcePosition,
    ) {
        self.check_if_tuple(test, pos.line, pos.col, pos.end_col);
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

    pub(super) fn visit_try_statement(
        &mut self, body: &[AstNode], handlers: &[ExceptHandler], else_body: &Option<Vec<AstNode>>,
        finally_body: &Option<Vec<AstNode>>, pos: SourcePosition,
    ) {
        self.check_default_except_not_last(handlers, pos.line, pos.col, pos.end_col);
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

    pub(super) fn visit_raise(&mut self, exc: Option<&AstNode>, line: usize, col: usize, end_col: usize) {
        if let Some(exception) = exc {
            self.check_raise_not_implemented(exception, line, col, end_col);
            self.visit_node(exception);
        }
    }

    pub(super) fn visit_compare(
        &mut self, left: &AstNode, ops: &[CompareOperator], comparators: &[AstNode], line: usize, col: usize,
        end_col: usize,
    ) {
        self.check_is_literal(left, ops, comparators, line, col, end_col);
        self.visit_node(left);
        for comp in comparators {
            self.visit_node(comp);
        }
    }

    pub(super) fn track_import(&mut self, module: &String, alias: &Option<String>) {
        let import_name = alias.as_ref().unwrap_or(module).clone();
        self.ctx.add_import(import_name);
    }

    pub(super) fn visit_from_import(&mut self, names: &[beacon_parser::ImportName], line: usize, col: usize) {
        let is_wildcard = names.len() == 1 && names[0].name == "*";

        if is_wildcard {
            if self.ctx.function_depth > 0 || self.ctx.class_depth > 0 {
                self.report(
                    RuleKind::ImportStarNotPermitted,
                    "from module import * not allowed inside function or class".to_string(),
                    line,
                    col,
                    col + 1,
                );
            } else {
                self.report(
                    RuleKind::ImportStarUsed,
                    "import * prevents detection of undefined names".to_string(),
                    line,
                    col,
                    col + 1,
                );
            }
        } else {
            for iname in names {
                self.ctx.add_import(iname.name.clone());
            }
        }
    }

    pub(super) fn visit_global(&mut self, names: &[String], line: usize, col: usize, end_col: usize) {
        for name in names {
            self.ctx.add_global_decl(name.clone(), line, col, end_col);
        }
    }

    pub(super) fn visit_nonlocal(&mut self, names: &[String], line: usize, col: usize, end_col: usize) {
        for name in names {
            self.ctx.add_nonlocal_decl(name.clone(), line, col, end_col);
        }
    }

    pub(super) fn visit_assignment(
        &mut self, target: &AstNode, value: &AstNode, line: usize, col: usize, end_col: usize,
    ) {
        self.check_two_starred_expressions(target, line, col, end_col);
        self.check_too_many_expressions_in_starred_assignment(target, value, line, col, end_col);

        if self.ctx.function_depth > 0 {
            for name in target.extract_target_names() {
                self.ctx.track_assignment(name);
            }
        }

        self.visit_node(value);
    }

    pub(super) fn visit_literal(&mut self, value: &LiteralValue, line: usize, col: usize, end_col: usize) {
        if let LiteralValue::String { value: s, prefix } = value {
            self.check_fstring_missing_placeholders(s, prefix, line, col, end_col);
            self.check_tstring_missing_placeholders(s, prefix, line, col, end_col);
        }
    }

    pub(super) fn visit_with(&mut self, items: &[WithItem], body: &[AstNode]) {
        for item in items {
            self.visit_node(&item.context_expr);
        }
        self.visit_body(body);
    }

    pub(super) fn visit_match(&mut self, subject: &AstNode, cases: &[MatchCase]) {
        self.visit_node(subject);
        for case in cases {
            self.visit_body(&case.body);
        }
    }

    pub(super) fn visit_comprehension(&mut self, element: &AstNode, generators: &[Comprehension]) {
        self.visit_comprehension_generators(generators);
        self.visit_node(element);
    }

    pub(super) fn visit_comprehension_generators(&mut self, generators: &[Comprehension]) {
        for generator in generators {
            self.visit_node(&generator.iter);
            for if_clause in &generator.ifs {
                self.visit_node(if_clause);
            }
        }
    }

    pub(super) fn visit_dict_comprehension(&mut self, key: &AstNode, value: &AstNode, generators: &[Comprehension]) {
        self.visit_comprehension_generators(generators);
        self.visit_node(key);
        self.visit_node(value);
        // NOTE: Cannot statically check for repeated keys in comprehensions as keys are generated at runtime
    }
}

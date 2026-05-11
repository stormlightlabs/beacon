use crate::const_eval::{ConstValue, evaluate_const_expr};
use crate::rules::{DiagnosticMessage, RuleKind};

use beacon_parser::{AstNode, BinaryOperator, CompareOperator, ExceptHandler, LiteralValue};
use rustc_hash::{FxHashMap, FxHashSet};

use super::Linter;

impl<'a> Linter<'a> {
    pub(super) fn check_duplicate_dict_keys(&mut self, keys: &[AstNode], line: usize, col: usize) {
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
                        ConstValue::Dict(_) => "<dict>".to_string(),
                        ConstValue::Set(_) => "<set>".to_string(),
                    };

                    self.diagnostics.push(DiagnosticMessage {
                        rule: RuleKind::MultiValueRepeatedKeyLiteral,
                        message: format!(
                            "Dictionary key {key_repr} repeated (first occurrence at line {first_line}, col {first_col})"
                        ),
                        filename: self.filename.clone(),
                        line,
                        col,
                        end_col: col + 1,
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

    pub(super) fn check_redundant_pass(&mut self, body: &[AstNode]) {
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
                        end_col: *col + 4,
                    });
                }
            }
        }
    }

    pub(super) fn check_duplicate_arguments(&mut self, args: &[beacon_parser::Parameter], func_name: &str) {
        let mut seen = FxHashSet::default();
        for arg in args {
            if !seen.insert(&arg.name) {
                self.report(
                    RuleKind::DuplicateArgument,
                    format!("Duplicate argument '{}' in function '{}'", arg.name, func_name),
                    arg.line,
                    arg.col,
                    arg.end_col,
                );
            }
        }
    }

    pub(super) fn check_return_outside_function(&mut self, line: usize, col: usize) {
        if self.ctx.function_depth == 0 {
            self.report(
                RuleKind::ReturnOutsideFunction,
                "'return' outside function".to_string(),
                line,
                col,
                col + 6,
            );
        }
    }

    pub(super) fn check_yield_outside_function(&mut self, line: usize, col: usize) {
        if self.ctx.function_depth == 0 {
            self.report(
                RuleKind::YieldOutsideFunction,
                "'yield' outside function".to_string(),
                line,
                col,
                col + 5,
            );
        }
    }

    pub(super) fn check_break_outside_loop(&mut self, line: usize, col: usize) {
        if self.ctx.loop_depth == 0 {
            self.report(
                RuleKind::BreakOutsideLoop,
                "'break' outside loop".to_string(),
                line,
                col,
                col + 5,
            );
        }
    }

    pub(super) fn check_continue_outside_loop(&mut self, line: usize, col: usize) {
        if self.ctx.loop_depth == 0 {
            self.report(
                RuleKind::ContinueOutsideLoop,
                "'continue' outside loop".to_string(),
                line,
                col,
                col + 8,
            );
        }
    }

    pub(super) fn check_default_except_not_last(
        &mut self, handlers: &[ExceptHandler], line: usize, col: usize, end_col: usize,
    ) {
        let mut found_bare_except = false;
        let mut bare_except_index = None;

        for (i, handler) in handlers.iter().enumerate() {
            if handler.exception_type.is_none() {
                found_bare_except = true;
                bare_except_index = Some(i);
                break;
            }
        }

        if let Some(idx) = bare_except_index
            && idx != handlers.len() - 1
        {
            self.report(
                RuleKind::DefaultExceptNotLast,
                "default 'except:' must be last".to_string(),
                line,
                col,
                end_col,
            );
        }

        let _ = found_bare_except;
    }

    pub(super) fn check_raise_not_implemented(&mut self, exception: &AstNode, line: usize, col: usize, end_col: usize) {
        if let AstNode::Identifier { name, .. } = exception
            && name == "NotImplemented"
        {
            self.report(
                RuleKind::RaiseNotImplemented,
                "use NotImplementedError instead of NotImplemented".to_string(),
                line,
                col,
                end_col,
            );
        }
    }

    pub(super) fn check_if_tuple(&mut self, test: &AstNode, line: usize, col: usize, end_col: usize) {
        if let AstNode::Tuple { .. } = test {
            self.report(
                RuleKind::IfTuple,
                "if condition is a tuple literal - this is always True".to_string(),
                line,
                col,
                end_col,
            );
        }
    }

    pub(super) fn check_fstring_missing_placeholders(
        &mut self, s: &str, prefix: &str, line: usize, col: usize, end_col: usize,
    ) {
        let is_fstring = prefix.to_lowercase().contains('f');
        if is_fstring && !s.contains('{') {
            self.report(
                RuleKind::FStringMissingPlaceholders,
                "f-string without any placeholders".to_string(),
                line,
                col,
                end_col,
            );
        }
    }

    pub(super) fn check_tstring_missing_placeholders(
        &mut self, s: &str, prefix: &str, line: usize, col: usize, end_col: usize,
    ) {
        let is_tstring = prefix.to_lowercase().contains('t');
        if is_tstring && !s.contains('{') {
            self.report(
                RuleKind::TStringMissingPlaceholders,
                "t-string without any placeholders".to_string(),
                line,
                col,
                end_col,
            );
        }
    }

    pub(super) fn check_percent_format(
        &mut self, left: &AstNode, op: &BinaryOperator, line: usize, col: usize, end_col: usize,
    ) {
        if !matches!(op, BinaryOperator::Mod) {
            return;
        }

        if let AstNode::Literal { value: LiteralValue::String { value: s, .. }, .. } = left
            && let Err(error) = Self::validate_percent_format(s)
        {
            self.report(
                RuleKind::PercentFormatInvalidFormat,
                format!("Invalid % format string: {error}"),
                line,
                col,
                end_col,
            );
        }
    }

    pub(super) fn validate_percent_format(s: &str) -> Result<(), String> {
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

    pub(super) fn is_valid_format_specifier(c: char) -> bool {
        matches!(
            c,
            's' | 'd' | 'i' | 'f' | 'e' | 'E' | 'g' | 'G' | 'c' | 'r' | 'x' | 'X' | 'o' | 'b' | 'a'
        )
    }

    pub(super) fn check_forward_annotation_syntax(
        &mut self, annotation: &str, line: usize, col: usize, end_col: usize,
    ) {
        if let Some(error_msg) = Self::validate_annotation_syntax(annotation) {
            self.report(
                RuleKind::ForwardAnnotationSyntaxError,
                format!("Syntax error in annotation '{annotation}': {error_msg}"),
                line,
                col,
                end_col,
            );
        }
    }

    pub(super) fn validate_annotation_syntax(annotation: &str) -> Option<String> {
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

    pub(super) fn check_is_literal(
        &mut self, left: &AstNode, ops: &[CompareOperator], comparators: &[AstNode], line: usize, col: usize,
        end_col: usize,
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
                            end_col,
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
                            end_col,
                        );
                    }
                }
            }
        }
    }

    pub(super) fn check_empty_except(&mut self, handler: &ExceptHandler) {
        if handler.exception_type.is_none()
            && handler.body.len() == 1
            && matches!(&handler.body[0], AstNode::Pass { .. })
        {
            self.report(
                RuleKind::EmptyExcept,
                "except clause with only 'pass' silently hides all errors".to_string(),
                handler.line,
                handler.col,
                handler.end_col,
            );
        }
    }

    pub(super) fn check_assert_tuple(&mut self, test: &AstNode, line: usize, col: usize, end_col: usize) {
        if let AstNode::Tuple { elements, .. } = test
            && !elements.is_empty()
        {
            self.report(
                RuleKind::AssertTuple,
                "assertion is a tuple literal, which is always True".to_string(),
                line,
                col,
                end_col,
            );
        }
    }

    pub(super) fn check_two_starred_expressions(&mut self, target: &AstNode, line: usize, col: usize, end_col: usize) {
        let starred_count = Self::count_starred_expressions(target);
        if starred_count > 1 {
            self.report(
                RuleKind::TwoStarredExpressions,
                format!("assignment target contains {starred_count} starred expressions, only one allowed"),
                line,
                col,
                end_col,
            );
        }
    }

    pub(super) fn count_starred_expressions(node: &AstNode) -> usize {
        match node {
            AstNode::Starred { .. } => 1,
            AstNode::Tuple { elements, .. } | AstNode::List { elements, .. } => {
                elements.iter().map(Self::count_starred_expressions).sum()
            }
            _ => 0,
        }
    }

    pub(super) fn check_too_many_expressions_in_starred_assignment(
        &mut self, target: &AstNode, value: &AstNode, line: usize, col: usize, end_col: usize,
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

        if let Some(value_len) = value_len
            && value_len < min_required
        {
            self.report(
                RuleKind::TooManyExpressionsInStarredAssignment,
                format!("too many expressions in assignment; need at least {min_required} values, got {value_len}"),
                line,
                col,
                end_col,
            );
        }
    }

    pub(super) fn count_target_names(node: &AstNode) -> usize {
        match node {
            AstNode::Identifier { .. } | AstNode::Starred { .. } => 1,
            AstNode::Tuple { elements, .. } | AstNode::List { elements, .. } => {
                elements.iter().map(Self::count_target_names).sum()
            }
            _ => 0,
        }
    }

    pub(super) fn check_unused_global_nonlocal(&mut self) {
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
                    decl.end_col,
                );
            }
        }
    }

    pub(super) fn has_assignment_in_function(&self, var_name: &str) -> bool {
        self.ctx.assigned_vars.contains(var_name)
    }
}

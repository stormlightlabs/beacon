use beacon_core::Result;

use crate::{AstNode, LiteralValue};

use super::{NameResolver, ReferenceKind};

impl NameResolver {
    pub(crate) fn track_references(&mut self, node: &AstNode) -> Result<()> {
        match node {
            AstNode::Module { body, .. } => {
                for stmt in body {
                    self.track_references(stmt)?;
                }
            }
            AstNode::FunctionDef { args, return_type, body, decorators, line, col, .. } => {
                let decorator_spans = self.compute_decorator_spans(*line, decorators);
                for (decorator, span) in decorators.iter().zip(decorator_spans.iter()) {
                    if let Some((dec_line, dec_col, _)) = span {
                        self.track_type_annotation_references(decorator, *dec_line, *dec_col)?;
                    } else {
                        self.track_type_annotation_references(decorator, *line, *col)?;
                    }
                }

                for param in args {
                    if let Some(type_ann) = &param.type_annotation {
                        self.track_type_annotation_references(type_ann, param.line, param.col)?;
                    }
                }

                if let Some(ret_type) = return_type {
                    // TODO: capture precise ranges for return annotations when parser retains that metadata.
                    self.track_type_annotation_references(ret_type, *line, *col)?;
                }

                for stmt in body {
                    self.track_references(stmt)?;
                }
            }
            AstNode::ClassDef { body, decorators, bases, line, col, .. } => {
                let decorator_spans = self.compute_decorator_spans(*line, decorators);
                for (decorator, span) in decorators.iter().zip(decorator_spans.iter()) {
                    if let Some((dec_line, dec_col, _)) = span {
                        self.track_type_annotation_references(decorator, *dec_line, *dec_col)?;
                    } else {
                        self.track_type_annotation_references(decorator, *line, *col)?;
                    }
                }

                for base in bases {
                    // TODO: capture precise spans for base expressions instead of defaulting to the class location.
                    self.track_type_annotation_references(base, *line, *col)?;
                }

                for stmt in body {
                    self.track_references(stmt)?;
                }
            }
            AstNode::Assignment { value, .. } => self.track_references(value)?,
            AstNode::AnnotatedAssignment { type_annotation, value, line, col, .. } => {
                self.track_type_annotation_references(type_annotation, *line, *col)?;
                if let Some(val) = value {
                    self.track_references(val)?;
                }
            }
            AstNode::Call { function, args, keywords, .. } => {
                // Track references in the function node (handles nested calls/attributes)
                self.track_references(function)?;

                for arg in args {
                    self.track_references(arg)?;
                }

                for (_name, value) in keywords {
                    self.track_references(value)?;
                }
            }
            AstNode::Identifier { name, line, col, end_col, .. } => {
                let byte_offset = self.line_col_to_byte_offset(*line, *col);
                let scope = self.symbol_table.find_scope_at_position(byte_offset);
                self.symbol_table
                    .add_reference(name, scope, *line, *col, *end_col, ReferenceKind::Read);
            }
            AstNode::Return { value, .. } => {
                if let Some(val) = value {
                    self.track_references(val)?;
                }
            }
            AstNode::Attribute { object, .. } => {
                self.track_references(object)?;
            }
            AstNode::If { test, body, elif_parts, else_body, .. } => {
                self.track_references(test)?;
                for stmt in body {
                    self.track_references(stmt)?;
                }
                for (elif_test, elif_body) in elif_parts {
                    self.track_references(elif_test)?;
                    for stmt in elif_body {
                        self.track_references(stmt)?;
                    }
                }
                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        self.track_references(stmt)?;
                    }
                }
            }
            AstNode::For { iter, body, else_body, .. } => {
                self.track_references(iter)?;
                for stmt in body {
                    self.track_references(stmt)?;
                }
                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        self.track_references(stmt)?;
                    }
                }
            }
            AstNode::While { test, body, else_body, .. } => {
                self.track_references(test)?;
                for stmt in body {
                    self.track_references(stmt)?;
                }
                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        self.track_references(stmt)?;
                    }
                }
            }
            AstNode::Try { body, handlers, else_body, finally_body, .. } => {
                for stmt in body {
                    self.track_references(stmt)?;
                }
                for handler in handlers {
                    for stmt in &handler.body {
                        self.track_references(stmt)?;
                    }
                }
                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        self.track_references(stmt)?;
                    }
                }
                if let Some(finally_stmts) = finally_body {
                    for stmt in finally_stmts {
                        self.track_references(stmt)?;
                    }
                }
            }
            AstNode::With { items, body, .. } => {
                for item in items {
                    self.track_references(&item.context_expr)?;
                }
                for stmt in body {
                    self.track_references(stmt)?;
                }
            }
            AstNode::ListComp { element, generators, .. }
            | AstNode::SetComp { element, generators, .. }
            | AstNode::GeneratorExp { element, generators, .. } => {
                self.track_references(element)?;
                for generator in generators {
                    self.track_references(&generator.iter)?;
                    for if_clause in &generator.ifs {
                        self.track_references(if_clause)?;
                    }
                }
            }
            AstNode::DictComp { key, value, generators, .. } => {
                self.track_references(key)?;
                self.track_references(value)?;
                for generator in generators {
                    self.track_references(&generator.iter)?;
                    for if_clause in &generator.ifs {
                        self.track_references(if_clause)?;
                    }
                }
            }
            AstNode::NamedExpr { value, .. } => self.track_references(value)?,
            AstNode::BinaryOp { left, right, .. } => {
                self.track_references(left)?;
                self.track_references(right)?;
            }
            AstNode::UnaryOp { operand, .. } => {
                self.track_references(operand)?;
            }
            AstNode::Compare { left, comparators, .. } => {
                self.track_references(left)?;
                for comp in comparators {
                    self.track_references(comp)?;
                }
            }
            AstNode::Lambda { args, body, .. } => {
                for param in args {
                    if let Some(type_ann) = &param.type_annotation {
                        self.track_type_annotation_references(type_ann, param.line, param.col)?;
                    }
                }
                self.track_references(body)?;
            }
            AstNode::Subscript { value, slice, .. } => {
                self.track_references(value)?;
                self.track_references(slice)?;
            }
            AstNode::Match { subject, cases, .. } => {
                self.track_references(subject)?;
                for case in cases {
                    if let Some(guard) = &case.guard {
                        self.track_references(guard)?;
                    }
                    for stmt in &case.body {
                        self.track_references(stmt)?;
                    }
                }
            }
            AstNode::Raise { exc, .. } => {
                if let Some(exception) = exc {
                    self.track_references(exception)?;
                }
            }
            AstNode::List { elements, .. } | AstNode::Set { elements, .. } => {
                for elem in elements {
                    self.track_references(elem)?;
                }
            }
            AstNode::Dict { keys, values, .. } => {
                for key in keys {
                    self.track_references(key)?;
                }
                for value in values {
                    self.track_references(value)?;
                }
            }
            AstNode::Tuple { elements, .. } => {
                for elem in elements {
                    self.track_references(elem)?;
                }
            }
            AstNode::Literal { value, line, col, .. } => {
                if let LiteralValue::String { value: string_value, prefix } = value {
                    let prefix_lower = prefix.to_lowercase();
                    if prefix_lower.contains('f') || prefix_lower.contains('t') {
                        self.track_fstring_references(string_value, *line, *col)?;
                    }
                }
            }
            AstNode::Yield { .. }
            | AstNode::YieldFrom { .. }
            | AstNode::Await { .. }
            | AstNode::Pass { .. }
            | AstNode::Break { .. }
            | AstNode::Continue { .. }
            | AstNode::Import { .. }
            | AstNode::ImportFrom { .. }
            | AstNode::Assert { .. }
            | AstNode::Starred { .. }
            | AstNode::Global { .. }
            | AstNode::Nonlocal { .. } => {}
            AstNode::ParenthesizedExpression { expression, .. } => {
                self.track_references(expression)?;
            }
        }

        Ok(())
    }
}

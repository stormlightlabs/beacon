use super::{AstNode, NameResolver, ReferenceKind, ScopeKind, Symbol, SymbolKind, SymbolReference};
use beacon_core::Result;

impl NameResolver {
    pub(crate) fn get_node_end_byte(&self, node: &AstNode) -> usize {
        match node {
            AstNode::Module { body, .. } => {
                if let Some(last) = body.last() {
                    self.get_node_end_byte(last)
                } else {
                    self.source.len()
                }
            }
            AstNode::FunctionDef { body, line, col, .. } | AstNode::ClassDef { body, line, col, .. } => {
                if let Some(last) = body.last() {
                    self.get_node_end_byte(last)
                } else {
                    self.line_col_to_byte_offset(*line, *col) + 50
                }
            }
            AstNode::If { body, else_body, .. } => {
                if let Some(else_stmts) = else_body
                    && let Some(last) = else_stmts.last()
                {
                    return self.get_node_end_byte(last);
                }
                if let Some(last) = body.last() { self.get_node_end_byte(last) } else { self.source.len() }
            }
            AstNode::For { body, else_body, .. } | AstNode::While { body, else_body, .. } => {
                if let Some(else_stmts) = else_body
                    && let Some(last) = else_stmts.last()
                {
                    return self.get_node_end_byte(last);
                }
                if let Some(last) = body.last() { self.get_node_end_byte(last) } else { self.source.len() }
            }
            AstNode::Try { body, else_body, finally_body, .. } => {
                if let Some(finally_stmts) = finally_body
                    && let Some(last) = finally_stmts.last()
                {
                    return self.get_node_end_byte(last);
                }
                if let Some(else_stmts) = else_body
                    && let Some(last) = else_stmts.last()
                {
                    return self.get_node_end_byte(last);
                }
                if let Some(last) = body.last() { self.get_node_end_byte(last) } else { self.source.len() }
            }
            AstNode::With { body, .. } => {
                if let Some(last) = body.last() {
                    self.get_node_end_byte(last)
                } else {
                    self.source.len()
                }
            }
            AstNode::Match { cases, .. } => {
                if let Some(last_case) = cases.last()
                    && let Some(last_stmt) = last_case.body.last()
                {
                    return self.get_node_end_byte(last_stmt);
                }
                self.source.len()
            }
            AstNode::Yield { line, col, .. }
            | AstNode::YieldFrom { line, col, .. }
            | AstNode::Await { line, col, .. }
            | AstNode::Tuple { line, col, .. }
            | AstNode::List { line, col, .. }
            | AstNode::Dict { line, col, .. }
            | AstNode::Set { line, col, .. }
            | AstNode::Assignment { line, col, .. }
            | AstNode::AnnotatedAssignment { line, col, .. }
            | AstNode::Call { line, col, .. }
            | AstNode::Identifier { line, col, .. }
            | AstNode::Literal { line, col, .. }
            | AstNode::Return { line, col, .. }
            | AstNode::Import { line, col, .. }
            | AstNode::ImportFrom { line, col, .. }
            | AstNode::Attribute { line, col, .. }
            | AstNode::ListComp { line, col, .. }
            | AstNode::DictComp { line, col, .. }
            | AstNode::SetComp { line, col, .. }
            | AstNode::GeneratorExp { line, col, .. }
            | AstNode::NamedExpr { line, col, .. }
            | AstNode::BinaryOp { line, col, .. }
            | AstNode::UnaryOp { line, col, .. }
            | AstNode::Compare { line, col, .. }
            | AstNode::Lambda { line, col, .. }
            | AstNode::Subscript { line, col, .. }
            | AstNode::Pass { line, col, .. }
            | AstNode::Break { line, col, .. }
            | AstNode::Continue { line, col, .. }
            | AstNode::Raise { line, col, .. }
            | AstNode::Global { line, col, .. }
            | AstNode::Nonlocal { line, col, .. }
            | AstNode::Assert { line, col, .. }
            | AstNode::Starred { line, col, .. }
            | AstNode::ParenthesizedExpression { line, col, .. } => {
                let start_byte = self.line_col_to_byte_offset(*line, *col);
                let mut end_byte = start_byte;
                for (i, ch) in self.source[start_byte..].chars().enumerate() {
                    if ch == '\n' {
                        break;
                    }
                    end_byte = start_byte + i + ch.len_utf8();
                }
                end_byte
            }
        }
    }

    pub(crate) fn visit_node(&mut self, node: &AstNode) -> Result<()> {
        match node {
            AstNode::Module { body, .. } => {
                for stmt in body {
                    self.visit_node(stmt)?;
                }
            }
            AstNode::FunctionDef { name, args, body, docstring, line, col, .. } => {
                let symbol = Symbol {
                    name: name.clone(),
                    kind: SymbolKind::Function,
                    line: *line,
                    col: *col,
                    end_col: *col + name.len(),
                    scope_id: self.current_scope,
                    docstring: docstring.clone(),
                    references: Vec::new(),
                };
                self.symbol_table.add_symbol(self.current_scope, symbol);

                let start_byte = self.line_col_to_byte_offset(*line, *col);
                let end_byte = if let Some(last_stmt) = body.last() {
                    self.get_node_end_byte(last_stmt)
                } else {
                    start_byte + name.len()
                };

                let func_scope =
                    self.symbol_table
                        .create_scope(ScopeKind::Function, self.current_scope, start_byte, end_byte);
                let prev_scope = self.current_scope;
                self.current_scope = func_scope;

                for param in args.iter() {
                    let param_symbol = Symbol {
                        name: param.name.clone(),
                        kind: SymbolKind::Parameter,
                        line: param.line,
                        col: param.col,
                        end_col: param.end_col,
                        scope_id: func_scope,
                        docstring: None,
                        references: Vec::new(),
                    };
                    self.symbol_table.add_symbol(func_scope, param_symbol);
                }
                for stmt in body {
                    self.visit_node(stmt)?;
                }

                self.current_scope = prev_scope;
            }
            AstNode::ClassDef { name, body, docstring, line, col, .. } => {
                let symbol = Symbol {
                    name: name.clone(),
                    kind: SymbolKind::Class,
                    line: *line,
                    col: *col,
                    end_col: *col + name.len(),
                    scope_id: self.current_scope,
                    docstring: docstring.clone(),
                    references: Vec::new(),
                };
                self.symbol_table.add_symbol(self.current_scope, symbol);

                let start_byte = self.line_col_to_byte_offset(*line, *col);
                let end_byte = if let Some(last_stmt) = body.last() {
                    self.get_node_end_byte(last_stmt)
                } else {
                    start_byte + name.len()
                };

                let class_scope =
                    self.symbol_table
                        .create_scope(ScopeKind::Class, self.current_scope, start_byte, end_byte);
                let prev_scope = self.current_scope;
                self.current_scope = class_scope;

                for stmt in body {
                    self.visit_node(stmt)?;
                }

                self.current_scope = prev_scope;
            }
            AstNode::Assignment { target, value, line, col, .. } => {
                self.visit_node(value)?;

                for name in target.binding_names() {
                    if name == "__all__"
                        && let AstNode::List { elements, .. } = value.as_ref()
                    {
                        for elem in elements {
                            if let AstNode::Literal {
                                value: crate::LiteralValue::String { value: ref_name, .. }, ..
                            } = elem
                            {
                                self.symbol_table.add_reference(
                                    ref_name,
                                    self.current_scope,
                                    *line,
                                    *col,
                                    *col,
                                    ReferenceKind::Read,
                                );
                            }
                        }
                    }

                    if let Some(scope) = self.symbol_table.scopes.get(&self.current_scope) {
                        if scope.symbols.contains_key(&name) {
                            let end_col = *col + name.len();
                            self.symbol_table.add_reference(
                                &name,
                                self.current_scope,
                                *line,
                                *col,
                                end_col,
                                ReferenceKind::Write,
                            );
                        } else {
                            let end_col = *col + name.len();
                            let mut symbol = Symbol {
                                name: name.clone(),
                                kind: SymbolKind::Variable,
                                line: *line,
                                col: *col,
                                end_col,
                                scope_id: self.current_scope,
                                docstring: None,
                                references: Vec::new(),
                            };
                            symbol.references.push(SymbolReference {
                                line: *line,
                                col: *col,
                                end_col,
                                kind: ReferenceKind::Write,
                            });
                            self.symbol_table.add_symbol(self.current_scope, symbol);
                        }
                    }
                }
            }
            AstNode::AnnotatedAssignment { target, value, line, col, .. } => {
                for name in target.binding_names() {
                    if let Some(scope) = self.symbol_table.scopes.get(&self.current_scope) {
                        if scope.symbols.contains_key(&name) {
                            let end_col = *col + name.len();
                            self.symbol_table.add_reference(
                                &name,
                                self.current_scope,
                                *line,
                                *col,
                                end_col,
                                ReferenceKind::Write,
                            );
                        } else {
                            let end_col = *col + name.len();
                            let mut symbol = Symbol {
                                name: name.clone(),
                                kind: SymbolKind::Variable,
                                line: *line,
                                col: *col,
                                end_col,
                                scope_id: self.current_scope,
                                docstring: None,
                                references: Vec::new(),
                            };
                            if value.is_some() {
                                symbol.references.push(SymbolReference {
                                    line: *line,
                                    col: *col,
                                    end_col,
                                    kind: ReferenceKind::Write,
                                });
                            }
                            self.symbol_table.add_symbol(self.current_scope, symbol);
                        }
                    }
                }

                if let Some(val) = value {
                    self.visit_node(val)?;
                }
            }
            AstNode::Call { args, .. } => {
                for arg in args {
                    self.visit_node(arg)?;
                }
            }
            AstNode::Return { value, .. } => {
                if let Some(val) = value {
                    self.visit_node(val)?;
                }
            }
            AstNode::Import { module, alias, line, col, .. } => {
                let name = alias.as_ref().unwrap_or(module);
                self.symbol_table.add_symbol(
                    self.current_scope,
                    Symbol {
                        name: name.clone(),
                        kind: SymbolKind::Import,
                        line: *line,
                        col: *col,
                        end_col: *col + name.len(),
                        scope_id: self.current_scope,
                        docstring: None,
                        references: Vec::new(),
                    },
                )
            }
            AstNode::ImportFrom { module, names, .. } => {
                if module != "__future__" {
                    for import_name in names {
                        self.symbol_table.add_symbol(
                            self.current_scope,
                            Symbol {
                                name: import_name.name.clone(),
                                kind: SymbolKind::Import,
                                line: import_name.line,
                                col: import_name.col,
                                end_col: import_name.end_col,
                                scope_id: self.current_scope,
                                docstring: None,
                                references: Vec::new(),
                            },
                        );
                    }
                }
            }
            AstNode::Attribute { object, .. } => self.visit_node(object)?,
            AstNode::If { test, body, elif_parts, else_body, .. } => {
                self.visit_node(test)?;
                for stmt in body {
                    self.visit_node(stmt)?;
                }
                for (elif_test, elif_body) in elif_parts {
                    self.visit_node(elif_test)?;
                    for stmt in elif_body {
                        self.visit_node(stmt)?;
                    }
                }
                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        self.visit_node(stmt)?;
                    }
                }
            }
            AstNode::For { target, iter, body, else_body, line, col, .. } => {
                self.visit_node(iter)?;

                for var_name in target.binding_names() {
                    let symbol = Symbol {
                        name: var_name.clone(),
                        kind: SymbolKind::Variable,
                        line: *line,
                        col: *col,
                        end_col: *col + var_name.len(),
                        scope_id: self.current_scope,
                        docstring: None,
                        references: Vec::new(),
                    };
                    self.symbol_table.add_symbol(self.current_scope, symbol);
                }

                for stmt in body {
                    self.visit_node(stmt)?;
                }
                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        self.visit_node(stmt)?;
                    }
                }
            }
            AstNode::While { test, body, else_body, .. } => {
                self.visit_node(test)?;
                for stmt in body {
                    self.visit_node(stmt)?;
                }
                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        self.visit_node(stmt)?;
                    }
                }
            }
            AstNode::Try { body, handlers, else_body, finally_body, .. } => {
                for stmt in body {
                    self.visit_node(stmt)?;
                }
                for handler in handlers {
                    if let Some(name) = &handler.name {
                        let symbol = Symbol {
                            name: name.clone(),
                            kind: SymbolKind::Variable,
                            line: handler.line,
                            col: handler.col,
                            end_col: handler.col + name.len(),
                            scope_id: self.current_scope,
                            docstring: None,
                            references: Vec::new(),
                        };
                        self.symbol_table.add_symbol(self.current_scope, symbol);
                    }
                    for stmt in &handler.body {
                        self.visit_node(stmt)?;
                    }
                }
                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        self.visit_node(stmt)?;
                    }
                }
                if let Some(finally_stmts) = finally_body {
                    for stmt in finally_stmts {
                        self.visit_node(stmt)?;
                    }
                }
            }
            AstNode::With { items, body, .. } => {
                for item in items {
                    self.visit_node(&item.context_expr)?;
                    if let Some(var_name) = &item.optional_vars {
                        // TODO: extract position
                        let symbol = Symbol {
                            name: var_name.clone(),
                            kind: SymbolKind::Variable,
                            line: 0,
                            col: 0,
                            end_col: var_name.len(),
                            scope_id: self.current_scope,
                            docstring: None,
                            references: Vec::new(),
                        };
                        self.symbol_table.add_symbol(self.current_scope, symbol);
                    }
                }
                for stmt in body {
                    self.visit_node(stmt)?;
                }
            }
            AstNode::ListComp { element, generators, line, col, .. }
            | AstNode::SetComp { element, generators, line, col, .. }
            | AstNode::GeneratorExp { element, generators, line, col, .. } => {
                for generator in generators {
                    self.visit_node(&generator.iter)?;

                    for var_name in generator.target_names() {
                        let symbol = Symbol {
                            name: var_name.clone(),
                            kind: SymbolKind::Variable,
                            line: *line,
                            col: *col,
                            end_col: *col + var_name.len(),
                            scope_id: self.current_scope,
                            docstring: None,
                            references: Vec::new(),
                        };
                        self.symbol_table.add_symbol(self.current_scope, symbol);
                    }

                    for if_clause in &generator.ifs {
                        self.visit_node(if_clause)?;
                    }
                }
                self.visit_node(element)?;
            }
            AstNode::DictComp { key, value, generators, line, col, .. } => {
                for generator in generators {
                    self.visit_node(&generator.iter)?;

                    for var_name in generator.target_names() {
                        let symbol = Symbol {
                            name: var_name.clone(),
                            kind: SymbolKind::Variable,
                            line: *line,
                            col: *col,
                            end_col: *col + var_name.len(),
                            scope_id: self.current_scope,
                            docstring: None,
                            references: Vec::new(),
                        };

                        self.symbol_table.add_symbol(self.current_scope, symbol);
                    }

                    for if_clause in &generator.ifs {
                        self.visit_node(if_clause)?;
                    }
                }
                self.visit_node(key)?;
                self.visit_node(value)?;
            }
            AstNode::NamedExpr { target, value, line, col, .. } => {
                self.visit_node(value)?;
                let symbol = Symbol {
                    name: target.clone(),
                    kind: SymbolKind::Variable,
                    line: *line,
                    col: *col,
                    end_col: *col + target.len(),
                    scope_id: self.current_scope,
                    docstring: None,
                    references: Vec::new(),
                };
                self.symbol_table.add_symbol(self.current_scope, symbol);
            }
            AstNode::BinaryOp { left, right, .. } => {
                self.visit_node(left)?;
                self.visit_node(right)?;
            }
            AstNode::UnaryOp { operand, .. } => self.visit_node(operand)?,
            AstNode::Compare { left, comparators, .. } => {
                self.visit_node(left)?;
                for comp in comparators {
                    self.visit_node(comp)?;
                }
            }
            AstNode::Lambda { args, body, line, col, .. } => {
                let start_byte = self.line_col_to_byte_offset(*line, *col);
                let end_byte = self.get_node_end_byte(node);

                let lambda_scope =
                    self.symbol_table
                        .create_scope(ScopeKind::Function, self.current_scope, start_byte, end_byte);
                let prev_scope = self.current_scope;

                self.current_scope = lambda_scope;

                for param in args {
                    let param_symbol = Symbol {
                        name: param.name.clone(),
                        kind: SymbolKind::Parameter,
                        line: param.line,
                        col: param.col,
                        end_col: param.end_col,
                        scope_id: lambda_scope,
                        docstring: None,
                        references: Vec::new(),
                    };
                    self.symbol_table.add_symbol(lambda_scope, param_symbol);
                }

                self.visit_node(body)?;
                self.current_scope = prev_scope;
            }
            AstNode::Subscript { value, slice, .. } => {
                self.visit_node(value)?;
                self.visit_node(slice)?;
            }
            AstNode::Match { subject, cases, .. } => {
                self.visit_node(subject)?;
                for case in cases {
                    if let Some(guard) = &case.guard {
                        self.visit_node(guard)?;
                    }
                    for stmt in &case.body {
                        self.visit_node(stmt)?;
                    }
                }
            }
            AstNode::Raise { exc, .. } => {
                if let Some(exception) = exc {
                    self.visit_node(exception)?;
                }
            }
            AstNode::List { elements, .. } | AstNode::Set { elements, .. } => {
                for elem in elements {
                    self.visit_node(elem)?;
                }
            }
            AstNode::Dict { keys, values, .. } => {
                for key in keys {
                    self.visit_node(key)?;
                }
                for value in values {
                    self.visit_node(value)?;
                }
            }
            AstNode::Tuple { elements, .. } => {
                for elem in elements {
                    self.visit_node(elem)?;
                }
            }
            AstNode::Yield { .. }
            | AstNode::YieldFrom { .. }
            | AstNode::Await { .. }
            | AstNode::Identifier { .. }
            | AstNode::Literal { .. }
            | AstNode::Pass { .. }
            | AstNode::Break { .. }
            | AstNode::Continue { .. }
            | AstNode::Global { .. }
            | AstNode::Nonlocal { .. }
            | AstNode::Assert { .. }
            | AstNode::Starred { .. } => {}
            AstNode::ParenthesizedExpression { expression, .. } => {
                self.visit_node(expression)?;
            }
        }

        Ok(())
    }
}

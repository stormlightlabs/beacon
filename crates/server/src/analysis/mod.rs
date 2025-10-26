//! Type analysis orchestration
//!
//! Coordinates parsing, constraint generation, type inference, and caching.
//! This is the bridge between the parser, type system, and LSP features.
//!
//! [`Analyzer`] manages the flow from source code to inferred types:
//!  1. Parsing -> AST
//!  2. Name resolution -> Symbol table
//!  3. Constraint generation -> Constraint set
//!  4. Unification/solving -> Type substitution
//!  5. Caching -> Type cache

pub mod cfg;
pub mod data_flow;
pub mod type_env;

use crate::cache::CacheManager;
use crate::config::Config;
use crate::document::DocumentManager;
use crate::utils;
use beacon_core::{
    Subst, Type, TypeScheme, TypeVar, TypeVarGen, Unifier,
    errors::{AnalysisError, Result},
};
use beacon_parser::{AstNode, LiteralValue, SymbolTable};
use lsp_types::Position;
use rustc_hash::FxHashMap;
use type_env::TypeEnvironment;
use url::Url;

/// Orchestrates type analysis for documents
pub struct Analyzer {
    _config: Config,
    cache: CacheManager,
    _type_var_gen: TypeVarGen,
    documents: DocumentManager,
}

impl Analyzer {
    /// Create a new analyzer with the given configuration
    pub fn new(config: Config, documents: DocumentManager) -> Self {
        Self { _config: config, cache: CacheManager::new(), _type_var_gen: TypeVarGen::new(), documents }
    }

    /// Analyze a document and return inferred types
    pub fn analyze(&mut self, uri: &Url) -> Result<AnalysisResult> {
        let result = self.documents.get_document(uri, |doc| {
            let ast = doc.ast().ok_or(AnalysisError::MissingAst)?.clone();
            let symbol_table = doc.symbol_table().ok_or(AnalysisError::MissingSymbolTable)?.clone();

            Ok::<_, AnalysisError>((ast, symbol_table, doc.version))
        });

        let (ast, symbol_table, version) = match result {
            Some(Ok(data)) => data,
            Some(Err(e)) => return Err(e.into()),
            None => return Err(AnalysisError::DocumentNotFound(uri.clone()).into()),
        };

        let constraints = self.generate_constraints(&ast, &symbol_table)?;

        let (_substitution, type_errors) = self.solve_constraints(constraints)?;

        // TODO: Build type_map by walking AST after solving
        // This map is needed for hover information and type-at-position queries.
        // For each AST node, apply the final substitution to get the concrete type.
        // Track node IDs during constraint generation to enable lookup.
        let type_map = FxHashMap::default();

        let static_analysis = self.perform_static_analysis(&ast, &symbol_table);
        Ok(AnalysisResult { uri: uri.clone(), version, type_map, type_errors, static_analysis })
    }

    /// Get the inferred type at a specific position
    ///
    /// TODO: Implement position-to-node lookup and type retrieval
    /// TODO: Convert position to node_id and check cache
    /// TODO: Look up type for node at position
    pub fn type_at_position(&mut self, uri: &Url, _position: Position) -> Result<Option<Type>> {
        let _result = self.analyze(uri)?;
        Ok(None)
    }

    /// Invalidate cached analysis for a document
    pub fn invalidate(&mut self, uri: &Url) {
        self.cache.invalidate_document(uri);
    }

    /// Generate constraints from an AST
    ///
    /// Implements Algorithm W constraint generation with type environment threading
    fn generate_constraints(&mut self, ast: &AstNode, symbol_table: &SymbolTable) -> Result<ConstraintSet> {
        let mut constraints = Vec::new();
        let mut env = TypeEnvironment::from_symbol_table(symbol_table, ast);

        Self::visit_node_with_env(ast, &mut env, &mut constraints)?;

        Ok(ConstraintSet { constraints })
    }

    /// Visit an AST node and generate constraints with type environment
    ///
    /// Implements constraint generation for core Python constructs
    fn visit_node_with_env(
        node: &AstNode, env: &mut TypeEnvironment, constraints: &mut Vec<Constraint>,
    ) -> Result<Type> {
        match node {
            AstNode::Module { body, .. } => {
                for stmt in body {
                    Self::visit_node_with_env(stmt, env, constraints)?;
                }
                Ok(Type::Con(beacon_core::TypeCtor::Module("".into())))
            }
            AstNode::FunctionDef { name, args, return_type, body, line, col, .. } => {
                let param_types: Vec<Type> = args
                    .iter()
                    .map(|param| {
                        param
                            .type_annotation
                            .as_ref()
                            .map(|ann| env.parse_annotation_or_any(ann))
                            .unwrap_or_else(|| Type::Var(env.fresh_var()))
                    })
                    .collect();

                let ret_type = return_type
                    .as_ref()
                    .map(|ann| env.parse_annotation_or_any(ann))
                    .unwrap_or_else(|| Type::Var(env.fresh_var()));

                let fn_type = Type::fun(param_types.clone(), ret_type.clone());

                let mut body_env = env.clone();
                for (param, param_type) in args.iter().zip(param_types.iter()) {
                    body_env.bind(param.name.clone(), TypeScheme::mono(param_type.clone()));
                }

                let mut body_ty = Type::none();
                for stmt in body {
                    body_ty = Self::visit_node_with_env(stmt, &mut body_env, constraints)?;
                }

                let span = Span::new(*line, *col);
                constraints.push(Constraint::Equal(body_ty, ret_type, span));
                env.bind(name.clone(), TypeScheme::mono(fn_type.clone()));

                Ok(fn_type)
            }

            AstNode::ClassDef { name, body, .. } => {
                let class_type = Type::Con(beacon_core::TypeCtor::Class(name.clone()));
                env.bind(name.clone(), TypeScheme::mono(class_type.clone()));

                for stmt in body {
                    Self::visit_node_with_env(stmt, env, constraints)?;
                }

                Ok(class_type)
            }

            AstNode::Assignment { target, value, .. } => {
                let value_ty = Self::visit_node_with_env(value, env, constraints)?;
                // TODO: Determine if value is non-expansive for generalization
                env.bind(target.clone(), TypeScheme::mono(value_ty.clone()));
                Ok(value_ty)
            }

            AstNode::AnnotatedAssignment { target, type_annotation, value, line, col } => {
                let annotated_ty = env.parse_annotation_or_any(type_annotation);
                if let Some(val) = value {
                    let value_ty = Self::visit_node_with_env(val, env, constraints)?;
                    let span = Span::new(*line, *col);
                    constraints.push(Constraint::Equal(value_ty, annotated_ty.clone(), span));
                }
                env.bind(target.clone(), TypeScheme::mono(annotated_ty.clone()));
                Ok(annotated_ty)
            }

            AstNode::Call { function, args, line, col } => {
                // TODO: Handle complex call expressions
                let func_ty = env.lookup(function).unwrap_or_else(|| Type::Var(env.fresh_var()));
                let mut arg_types = Vec::new();
                for arg in args {
                    arg_types.push(Self::visit_node_with_env(arg, env, constraints)?);
                }
                let ret_ty = Type::Var(env.fresh_var());
                let span = Span::new(*line, *col);
                constraints.push(Constraint::Call(func_ty, arg_types, ret_ty.clone(), span));
                Ok(ret_ty)
            }
            AstNode::Identifier { name, .. } => Ok(env.lookup(name).unwrap_or_else(|| Type::Var(env.fresh_var()))),
            AstNode::Literal { value, .. } => Ok(match value {
                LiteralValue::Integer(_) => Type::int(),
                LiteralValue::Float(_) => Type::float(),
                LiteralValue::String(_) => Type::string(),
                LiteralValue::Boolean(_) => Type::bool(),
                LiteralValue::None => Type::none(),
            }),
            AstNode::Return { value, .. } => {
                if let Some(val) = value {
                    Self::visit_node_with_env(val, env, constraints)
                } else {
                    Ok(Type::none())
                }
            }
            AstNode::Attribute { object, attribute, line, col } => {
                let obj_ty = Self::visit_node_with_env(object, env, constraints)?;
                let attr_ty = Type::Var(env.fresh_var());
                let span = Span::new(*line, *col);
                constraints.push(Constraint::HasAttr(obj_ty, attribute.clone(), attr_ty.clone(), span));
                Ok(attr_ty)
            }
            AstNode::BinaryOp { left, right, line, col, .. } => {
                let left_ty = Self::visit_node_with_env(left, env, constraints)?;
                let right_ty = Self::visit_node_with_env(right, env, constraints)?;
                let span = Span::new(*line, *col);
                constraints.push(Constraint::Equal(left_ty.clone(), right_ty, span));
                Ok(left_ty)
            }
            AstNode::UnaryOp { operand, .. } => Self::visit_node_with_env(operand, env, constraints),
            AstNode::If { test, body, elif_parts, else_body, .. } => {
                Self::visit_node_with_env(test, env, constraints)?;

                let (narrowed_var, narrowed_type) = Self::detect_type_guard(test);

                let mut true_env = env.clone();
                if let (Some(var_name), Some(refined_ty)) = (narrowed_var.as_ref(), narrowed_type.as_ref()) {
                    true_env.bind(var_name.clone(), TypeScheme::mono(refined_ty.clone()));
                }

                for stmt in body {
                    Self::visit_node_with_env(stmt, &mut true_env, constraints)?;
                }

                for (elif_test, elif_body) in elif_parts {
                    Self::visit_node_with_env(elif_test, env, constraints)?;
                    for stmt in elif_body {
                        Self::visit_node_with_env(stmt, env, constraints)?;
                    }
                }
                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        Self::visit_node_with_env(stmt, env, constraints)?;
                    }
                }
                Ok(Type::none())
            }
            AstNode::Import { .. } | AstNode::ImportFrom { .. } => {
                Ok(Type::Con(beacon_core::TypeCtor::Module("".into())))
            }

            _ => Ok(Type::Var(env.fresh_var())),
        }
    }

    #[deprecated]
    fn _visit_node(&mut self, node: &AstNode, _constraints: &mut [Constraint]) -> Result<Type> {
        let mut env = TypeEnvironment::new();
        let mut constraints = Vec::new();
        Self::visit_node_with_env(node, &mut env, &mut constraints)
    }

    /// Solve a set of constraints using beacon-core's unification algorithm
    ///
    /// Returns a substitution and a list of type errors encountered during solving.
    /// Errors are accumulated rather than failing fast to provide comprehensive feedback.
    fn solve_constraints(&mut self, constraint_set: ConstraintSet) -> Result<(Substitution, Vec<TypeErrorInfo>)> {
        let mut subst = Subst::empty();
        let mut type_errors = Vec::new();

        for constraint in constraint_set.constraints {
            match constraint {
                Constraint::Equal(t1, t2, span) => match Unifier::unify(&subst.apply(&t1), &subst.apply(&t2)) {
                    Ok(s) => {
                        subst = s.compose(subst);
                    }
                    Err(beacon_core::BeaconError::TypeError(type_err)) => {
                        type_errors.push(TypeErrorInfo::new(type_err, span));
                    }
                    Err(_) => {}
                },

                Constraint::Call(func_ty, arg_types, ret_ty, span) => {
                    let expected_fn_ty = Type::fun(arg_types, ret_ty);
                    match Unifier::unify(&subst.apply(&func_ty), &subst.apply(&expected_fn_ty)) {
                        Ok(s) => {
                            subst = s.compose(subst);
                        }
                        Err(beacon_core::BeaconError::TypeError(type_err)) => {
                            type_errors.push(TypeErrorInfo::new(type_err, span));
                        }
                        Err(_) => {}
                    }
                }

                // HasAttr constraint: object must have attribute
                // TODO: Implement proper record/structural typing
                // TODO: use row-polymorphic records
                Constraint::HasAttr(_obj_ty, _attr_name, _attr_ty, _span) => {}
            }
        }

        Ok((
            Substitution { _map: subst.iter().map(|(k, v)| (k.clone(), v.clone())).collect() },
            type_errors,
        ))
    }

    /// Perform static analysis (CFG + data flow) on the AST
    ///
    /// Builds control flow graphs for each function and runs data flow analyses to detect use-before-def, unreachable code, and unused variables.
    fn perform_static_analysis(&self, ast: &AstNode, symbol_table: &SymbolTable) -> Option<data_flow::DataFlowResult> {
        // TODO: Extend to module-level analysis
        match ast {
            AstNode::Module { body, .. } => {
                for stmt in body {
                    if let AstNode::FunctionDef { body: func_body, .. } = stmt {
                        let mut builder = cfg::CfgBuilder::new();
                        builder.build_function(func_body);
                        let cfg = builder.build();
                        let analyzer = data_flow::DataFlowAnalyzer::new(&cfg, func_body, symbol_table);
                        return Some(analyzer.analyze());
                    }
                }
                None
            }
            AstNode::FunctionDef { body, .. } => {
                let mut builder = cfg::CfgBuilder::new();
                builder.build_function(body);
                let cfg = builder.build();
                let analyzer = data_flow::DataFlowAnalyzer::new(&cfg, body, symbol_table);
                Some(analyzer.analyze())
            }
            _ => None,
        }
    }

    /// Find unbound variables in the AST
    pub fn find_unbound_variables(&self, uri: &Url) -> Vec<(String, usize, usize)> {
        let result = self.documents.get_document(uri, |doc| {
            let ast = doc.ast()?;
            let symbol_table = doc.symbol_table()?;
            let text = doc.text();
            Some((ast.clone(), symbol_table.clone(), text))
        });

        let (ast, symbol_table, text) = match result {
            Some(Some(data)) => data,
            _ => return Vec::new(),
        };

        let mut unbound = Vec::new();
        Self::collect_unbound_in_node(&ast, &symbol_table, &text, &mut unbound);

        unbound
    }

    /// Recursively collect unbound variables in an AST node using position-based scope resolution
    fn collect_unbound_in_node(
        node: &AstNode, symbol_table: &SymbolTable, text: &str, unbound: &mut Vec<(String, usize, usize)>,
    ) {
        match node {
            AstNode::Identifier { name, line, col } => {
                if !Self::is_valid_identifier(name) {
                    return;
                }
                let ln = (*line as u32).saturating_sub(1);
                let ch = (*col as u32).saturating_sub(1);
                let byte_offset = utils::position_to_byte_offset(text, Position { line: ln, character: ch });
                let scope = symbol_table.find_scope_at_position(byte_offset);

                if symbol_table.lookup_symbol(name, scope).is_none() && !Self::is_builtin(name) {
                    unbound.push((name.clone(), *line, *col));
                }
            }
            AstNode::FunctionDef { body, .. } | AstNode::ClassDef { body, .. } | AstNode::Module { body, .. } => {
                for stmt in body {
                    Self::collect_unbound_in_node(stmt, symbol_table, text, unbound);
                }
            }
            AstNode::Call { args, .. } => {
                for arg in args {
                    Self::collect_unbound_in_node(arg, symbol_table, text, unbound);
                }
            }
            AstNode::Assignment { value, .. } => Self::collect_unbound_in_node(value, symbol_table, text, unbound),
            AstNode::AnnotatedAssignment { value, .. } => {
                if let Some(val) = value {
                    Self::collect_unbound_in_node(val, symbol_table, text, unbound);
                }
            }
            AstNode::Return { value, .. } => {
                if let Some(val) = value {
                    Self::collect_unbound_in_node(val, symbol_table, text, unbound);
                }
            }
            AstNode::Attribute { object, .. } => Self::collect_unbound_in_node(object, symbol_table, text, unbound),
            AstNode::Import { .. } | AstNode::ImportFrom { .. } | AstNode::Literal { .. } => {}
            AstNode::If { test, body, elif_parts, else_body, .. } => {
                Self::collect_unbound_in_node(test, symbol_table, text, unbound);
                for stmt in body {
                    Self::collect_unbound_in_node(stmt, symbol_table, text, unbound);
                }
                for (elif_test, elif_body) in elif_parts {
                    Self::collect_unbound_in_node(elif_test, symbol_table, text, unbound);
                    for stmt in elif_body {
                        Self::collect_unbound_in_node(stmt, symbol_table, text, unbound);
                    }
                }
                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        Self::collect_unbound_in_node(stmt, symbol_table, text, unbound);
                    }
                }
            }
            AstNode::For { iter, body, else_body, .. } => {
                Self::collect_unbound_in_node(iter, symbol_table, text, unbound);
                for stmt in body {
                    Self::collect_unbound_in_node(stmt, symbol_table, text, unbound);
                }
                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        Self::collect_unbound_in_node(stmt, symbol_table, text, unbound);
                    }
                }
            }
            AstNode::While { test, body, else_body, .. } => {
                Self::collect_unbound_in_node(test, symbol_table, text, unbound);
                for stmt in body {
                    Self::collect_unbound_in_node(stmt, symbol_table, text, unbound);
                }
                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        Self::collect_unbound_in_node(stmt, symbol_table, text, unbound);
                    }
                }
            }
            AstNode::Try { body, handlers, else_body, finally_body, .. } => {
                for stmt in body {
                    Self::collect_unbound_in_node(stmt, symbol_table, text, unbound);
                }
                for handler in handlers {
                    for stmt in &handler.body {
                        Self::collect_unbound_in_node(stmt, symbol_table, text, unbound);
                    }
                }
                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        Self::collect_unbound_in_node(stmt, symbol_table, text, unbound);
                    }
                }
                if let Some(finally_stmts) = finally_body {
                    for stmt in finally_stmts {
                        Self::collect_unbound_in_node(stmt, symbol_table, text, unbound);
                    }
                }
            }
            AstNode::With { items, body, .. } => {
                for item in items {
                    Self::collect_unbound_in_node(&item.context_expr, symbol_table, text, unbound);
                }
                for stmt in body {
                    Self::collect_unbound_in_node(stmt, symbol_table, text, unbound);
                }
            }
            AstNode::ListComp { element, generators, .. }
            | AstNode::SetComp { element, generators, .. }
            | AstNode::GeneratorExp { element, generators, .. } => {
                Self::collect_unbound_in_node(element, symbol_table, text, unbound);
                for generator in generators {
                    Self::collect_unbound_in_node(&generator.iter, symbol_table, text, unbound);
                    for if_clause in &generator.ifs {
                        Self::collect_unbound_in_node(if_clause, symbol_table, text, unbound);
                    }
                }
            }
            AstNode::DictComp { key, value, generators, .. } => {
                Self::collect_unbound_in_node(key, symbol_table, text, unbound);
                Self::collect_unbound_in_node(value, symbol_table, text, unbound);
                for generator in generators {
                    Self::collect_unbound_in_node(&generator.iter, symbol_table, text, unbound);
                    for if_clause in &generator.ifs {
                        Self::collect_unbound_in_node(if_clause, symbol_table, text, unbound);
                    }
                }
            }
            AstNode::NamedExpr { value, .. } => {
                Self::collect_unbound_in_node(value, symbol_table, text, unbound);
            }
            AstNode::BinaryOp { left, right, .. } => {
                Self::collect_unbound_in_node(left, symbol_table, text, unbound);
                Self::collect_unbound_in_node(right, symbol_table, text, unbound);
            }
            AstNode::UnaryOp { operand, .. } => {
                Self::collect_unbound_in_node(operand, symbol_table, text, unbound);
            }
            AstNode::Compare { left, comparators, .. } => {
                Self::collect_unbound_in_node(left, symbol_table, text, unbound);
                for comp in comparators {
                    Self::collect_unbound_in_node(comp, symbol_table, text, unbound);
                }
            }
            AstNode::Lambda { body, .. } => {
                Self::collect_unbound_in_node(body, symbol_table, text, unbound);
            }
            AstNode::Subscript { value, slice, .. } => {
                Self::collect_unbound_in_node(value, symbol_table, text, unbound);
                Self::collect_unbound_in_node(slice, symbol_table, text, unbound);
            }
            AstNode::Match { subject, cases, .. } => {
                Self::collect_unbound_in_node(subject, symbol_table, text, unbound);
                for case in cases {
                    if let Some(guard) = &case.guard {
                        Self::collect_unbound_in_node(guard, symbol_table, text, unbound);
                    }
                    for stmt in &case.body {
                        Self::collect_unbound_in_node(stmt, symbol_table, text, unbound);
                    }
                }
            }
            AstNode::Raise { exc, .. } => {
                if let Some(exception) = exc {
                    Self::collect_unbound_in_node(exception, symbol_table, text, unbound);
                }
            }
            AstNode::Pass { .. } | AstNode::Break { .. } | AstNode::Continue { .. } => {}
        }
    }

    /// Check if a name is a Python builtin
    fn is_builtin(name: &str) -> bool {
        matches!(
            name,
            "print"
                | "len"
                | "range"
                | "str"
                | "int"
                | "float"
                | "bool"
                | "list"
                | "dict"
                | "set"
                | "tuple"
                | "abs"
                | "all"
                | "any"
                | "ascii"
                | "bin"
                | "callable"
                | "chr"
                | "compile"
                | "complex"
                | "delattr"
                | "dir"
                | "divmod"
                | "enumerate"
                | "eval"
                | "exec"
                | "filter"
                | "format"
                | "frozenset"
                | "getattr"
                | "globals"
                | "hasattr"
                | "hash"
                | "help"
                | "hex"
                | "id"
                | "input"
                | "isinstance"
                | "issubclass"
                | "iter"
                | "locals"
                | "map"
                | "max"
                | "min"
                | "next"
                | "object"
                | "oct"
                | "open"
                | "ord"
                | "pow"
                | "property"
                | "repr"
                | "reversed"
                | "round"
                | "setattr"
                | "slice"
                | "sorted"
                | "staticmethod"
                | "sum"
                | "super"
                | "type"
                | "vars"
                | "zip"
                | "__import__"
                | "True"
                | "False"
                | "None"
                | "NotImplemented"
                | "Ellipsis"
                | "__debug__"
                | "quit"
                | "exit"
                | "copyright"
                | "credits"
                | "license"
        )
    }

    /// Check if a string is a valid Python identifier ([a-zA-Z_][a-zA-Z0-9_]*), filtering out cases
    /// where the parser created an Identifier node for other syntax.
    fn is_valid_identifier(name: &str) -> bool {
        if name.is_empty() {
            return false;
        }

        let mut chars = name.chars();
        let first = chars.next().unwrap();
        if !first.is_alphabetic() && first != '_' {
            return false;
        }

        chars.all(|c| c.is_alphanumeric() || c == '_')
    }

    /// Detect type guard patterns for flow-sensitive type narrowing
    ///
    /// Returns (variable_name, refined_type) if a type guard is detected.
    ///
    /// Supported patterns:
    /// - `isinstance(x, int)` -> (x, int)
    /// - `isinstance(x, str)` -> (x, str)
    /// - `x is None` -> (x, None)
    /// - `x is not None` -> (x, not None) - TODO: implement Union narrowing
    fn detect_type_guard(test: &AstNode) -> (Option<String>, Option<Type>) {
        if let AstNode::Call { function, args, .. } = test {
            if function == "isinstance" && args.len() == 2 {
                if let AstNode::Identifier { name: var_name, .. } = &args[0] {
                    if let AstNode::Identifier { name: type_name, .. } = &args[1] {
                        let refined_type = Self::type_name_to_type(type_name);
                        return (Some(var_name.clone()), Some(refined_type));
                    }
                }
            }
        }

        if let AstNode::Compare { left, ops, comparators, .. } = test {
            if ops.len() == 1 && comparators.len() == 1 {
                if let AstNode::Identifier { name: var_name, .. } = left.as_ref() {
                    if let AstNode::Literal { value: LiteralValue::None, .. } = &comparators[0] {
                        match &ops[0] {
                            beacon_parser::CompareOperator::Is => {
                                return (Some(var_name.clone()), Some(Type::none()));
                            }
                            beacon_parser::CompareOperator::IsNot => {
                                // TODO: For "x is not None", we should narrow to non-None type.
                                // This requires implementing Union type narrowing:
                                // - If x: Union[T, None], narrow to T in the true branch
                                // - If x: T (not a Union), keep as T
                                // This will be implemented when we add full Union type support.
                                return (None, None);
                            }
                            _ => {}
                        }
                    }
                }
            }
        }

        (None, None)
    }

    /// Convert a type name string to a Type
    fn type_name_to_type(name: &str) -> Type {
        match name {
            "int" => Type::int(),
            "str" => Type::string(),
            "float" => Type::float(),
            "bool" => Type::bool(),
            "list" => Type::Con(beacon_core::TypeCtor::List),
            "dict" => Type::Con(beacon_core::TypeCtor::Dict),
            "set" => Type::Con(beacon_core::TypeCtor::Set),
            "tuple" => Type::Con(beacon_core::TypeCtor::Tuple),
            _ => Type::Var(TypeVarGen::new().fresh()),
        }
    }
}

/// Source code span for tracking positions
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    /// Line number (1-indexed)
    pub line: usize,
    /// Column number (1-indexed)
    pub col: usize,
    /// Optional end position for range
    pub end_line: Option<usize>,
    pub end_col: Option<usize>,
}

impl Span {
    /// Create a new span from a line and column
    pub fn new(line: usize, col: usize) -> Self {
        Self { line, col, end_line: None, end_col: None }
    }

    /// Create a new span with an end position
    pub fn with_end(line: usize, col: usize, end_line: usize, end_col: usize) -> Self {
        Self { line, col, end_line: Some(end_line), end_col: Some(end_col) }
    }
}

/// Type error with location information
#[derive(Debug, Clone)]
pub struct TypeErrorInfo {
    pub error: beacon_core::TypeError,
    /// Span where the error occurred
    pub span: Span,
}

impl TypeErrorInfo {
    /// Create a new type error with location
    pub fn new(error: beacon_core::TypeError, span: Span) -> Self {
        Self { error, span }
    }

    pub fn line(&self) -> usize {
        self.span.line
    }

    pub fn col(&self) -> usize {
        self.span.col
    }

    pub fn end_line(&self) -> Option<usize> {
        self.span.end_line
    }

    pub fn end_col(&self) -> Option<usize> {
        self.span.end_col
    }
}

/// Result of analyzing a document
pub struct AnalysisResult {
    /// Document URI
    pub uri: Url,
    /// Document version
    pub version: i32,
    /// Map from AST node IDs to inferred types
    pub type_map: FxHashMap<usize, Type>,
    /// Type errors encountered during analysis
    pub type_errors: Vec<TypeErrorInfo>,
    /// Static analysis results (data flow analysis)
    pub static_analysis: Option<data_flow::DataFlowResult>,
}

/// Set of type constraints
///
/// TODO: Use [`beacon_core`] constraint types when available
pub struct ConstraintSet {
    pub constraints: Vec<Constraint>,
}

/// Type constraint with source position tracking
///
/// Each constraint variant includes a Span to track where the constraint
/// originated in the source code, enabling precise error reporting.
#[derive(Debug, Clone)]
pub enum Constraint {
    /// Type equality constraint: t1 ~ t2
    Equal(Type, Type, Span),

    /// HasAttr constraint: τ has attribute "name" : τ'
    HasAttr(Type, String, Type, Span),

    /// Call constraint: f(args) -> ret
    Call(Type, Vec<Type>, Type, Span),
}

/// Type substitution
///
/// TODO: Use [`beacon_core::Subst`] when ready
pub struct Substitution {
    _map: FxHashMap<TypeVar, Type>,
}

impl Substitution {
    pub fn empty() -> Self {
        Self { _map: FxHashMap::default() }
    }

    /// TODO: Implement substitution application (apply substitution to a type)
    pub fn _apply(&self, _ty: &Type) -> Type {
        todo!()
    }

    /// TODO: Implement substitution composition
    pub fn _compose(&self, _other: &Substitution) -> Substitution {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::str::FromStr;

    #[test]
    fn test_analyzer_creation() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let _analyzer = Analyzer::new(config, documents);
    }

    #[test]
    fn test_constraint_generation_literal() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents);

        let lit = AstNode::Literal { value: beacon_parser::LiteralValue::Integer(42), line: 1, col: 1 };

        let mut constraints = Vec::new();
        #[allow(deprecated)]
        let ty = analyzer._visit_node(&lit, &mut constraints).unwrap();

        assert!(matches!(ty, Type::Con(beacon_core::TypeCtor::Int)));
    }

    #[test]
    fn test_substitution_empty() {
        let subst = Substitution::empty();
        assert_eq!(subst._map.len(), 0);
    }

    #[test]
    fn test_find_unbound_variables() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
def hello():
    x = 42
    return undefined_var
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let unbound = analyzer.find_unbound_variables(&uri);

        assert!(!unbound.is_empty());
        assert!(unbound.iter().any(|(name, _, _)| name == "undefined_var"));
    }

    #[test]
    fn test_find_unbound_variables_with_builtins() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
x = len([1, 2, 3])
print(x)
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let unbound = analyzer.find_unbound_variables(&uri);

        for (name, line, col) in &unbound {
            eprintln!("Found unbound: {name} at {line}:{col}");
        }

        let non_builtins: Vec<_> = unbound
            .iter()
            .filter(|(name, _, _)| !Analyzer::is_builtin(name))
            .collect();

        assert!(
            non_builtins.is_empty(),
            "Found unexpected unbound variables: {non_builtins:?}"
        );
    }

    #[test]
    fn test_local_variables_not_unbound() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"def main():
    test_data = [1, 2, 3]
    result = test_data
    return result"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let unbound = analyzer.find_unbound_variables(&uri);
        for (name, line, col) in &unbound {
            eprintln!("Found unbound: {name} at {line}:{col}");
        }

        assert!(
            !unbound.iter().any(|(name, _, _)| name == "test_data"),
            "test_data should not be marked as unbound"
        );
        assert!(
            !unbound.iter().any(|(name, _, _)| name == "result"),
            "result should not be marked as unbound"
        );
    }

    #[test]
    fn test_nested_function_local_variables() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"def outer():
    x = 10
    def inner():
        y = x + 5
        return y
    return inner()"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let unbound = analyzer.find_unbound_variables(&uri);
        for (name, line, col) in &unbound {
            eprintln!("Found unbound: {name} at {line}:{col}");
        }

        assert!(
            !unbound.iter().any(|(name, _, _)| name == "x"),
            "x should not be marked as unbound"
        );
        assert!(
            !unbound.iter().any(|(name, _, _)| name == "y"),
            "y should not be marked as unbound"
        );
    }

    #[test]
    fn test_flow_sensitive_isinstance_narrowing() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
def process(x):
    if isinstance(x, int):
        # In this branch, x should be narrowed to int
        return x + 1
    else:
        # In this branch, x keeps its original type
        return x
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = analyzer.analyze(&uri);
        assert!(result.is_ok(), "Analysis should succeed with isinstance narrowing");
    }

    #[test]
    fn test_flow_sensitive_none_check() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
def process(x):
    if x is None:
        # In this branch, x is narrowed to None type
        return None
    else:
        # In this branch, x is not None
        return x
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = analyzer.analyze(&uri);
        assert!(result.is_ok(), "Analysis should succeed with None check narrowing");
    }
}

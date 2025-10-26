//! Type analysis orchestration
//!
//! Coordinates parsing, constraint generation, type inference, and caching.
//! This is the bridge between the parser, type system, and LSP features.
//!
//! [Analyzer] manages the flow from source code to inferred types:
//!  1. Parsing -> AST
//!  2. Name resolution -> Symbol table
//!  3. Constraint generation -> Constraint set
//!  4. Unification/solving -> Type substitution
//!  5. Caching -> Type cache
//!
//! ## TODO Protocol Constraints
//!
//! - Add `Protocol` constraint variant alongside `Equal`, `HasAttr`, and `Call`
//! - Model Python protocols: Iterator, Iterable, ContextManager, Sized, etc.
//! - Support structural subtyping: check if a type satisfies required methods/attributes
//! - Enable protocol definitions via `typing.Protocol` or `.pyi` stubs
//!
//! 1. Extend `Constraint` enum with `Protocol(Type, ProtocolName, Span)`
//! 2. Define protocol specifications (method signatures, attribute types)
//! 3. Implement protocol checking in constraint solver
//! 4. Handle variance in protocol method signatures
//! 5. Support protocol intersection and union
//!
//! **Affected Constructs:**
//! - Iterator protocol: `for` loops, comprehensions, `iter()`, `next()`
//! - Context manager protocol: `with` statements (`__enter__`, `__exit__`)
//! - Subscript protocol: indexing (`__getitem__`, `__setitem__`)
//! - Callable protocol: function calls, decorators
//! - Sequence/Mapping protocols: collection operations

pub mod cfg;
pub mod constraint_gen;
pub mod data_flow;
pub mod type_env;
use crate::cache::CacheManager;
use crate::config::Config;
use crate::document::DocumentManager;
use crate::utils;
use beacon_core::{
    Subst, Type, TypeScheme, TypeVarGen, Unifier,
    errors::{AnalysisError, Result},
};
use beacon_parser::{AstNode, LiteralValue, SymbolTable};
use constraint_gen::{Constraint, ConstraintGenContext, ConstraintResult, ConstraintSet, Span};
use lsp_types::Position;
use rustc_hash::FxHashMap;
use type_env::TypeEnvironment;
use url::Url;

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

/// Orchestrates type analysis for documents
pub struct Analyzer {
    _config: Config,
    cache: CacheManager,
    _type_var_gen: TypeVarGen,
    documents: DocumentManager,
    /// Map from document URI to position maps for type-at-position queries
    position_maps: FxHashMap<Url, FxHashMap<(usize, usize), usize>>,
}

impl Analyzer {
    /// Create a new analyzer with the given configuration
    pub fn new(config: Config, documents: DocumentManager) -> Self {
        Self {
            _config: config,
            cache: CacheManager::new(),
            _type_var_gen: TypeVarGen::new(),
            documents,
            position_maps: FxHashMap::default(),
        }
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

        let ConstraintResult(constraints, mut type_map, position_map) =
            Self::generate_constraints(&ast, &symbol_table)?;

        let (substitution, type_errors) = self.solve_constraints(constraints)?;

        for (_node_id, ty) in type_map.iter_mut() {
            *ty = substitution.apply(ty);
        }

        self.position_maps.insert(uri.clone(), position_map);

        let static_analysis = self.perform_static_analysis(&ast, &symbol_table);
        Ok(AnalysisResult { uri: uri.clone(), version, type_map, type_errors, static_analysis })
    }

    /// Get the inferred type at a specific position
    ///
    /// Analyzes the document and looks up the type at the specified position.
    /// Returns None if no type information is available at that position.
    pub fn type_at_position(&mut self, uri: &Url, position: Position) -> Result<Option<Type>> {
        let result = self.analyze(uri)?;
        let line = (position.line + 1) as usize;
        let col = (position.character + 1) as usize;

        let position_map = self.position_maps.get(uri);
        if let Some(pos_map) = position_map {
            if let Some(node_id) = pos_map.get(&(line, col)) {
                return Ok(result.type_map.get(node_id).cloned());
            }
        }

        Ok(None)
    }

    /// Invalidate cached analysis for a document
    pub fn invalidate(&mut self, uri: &Url) {
        self.cache.invalidate_document(uri);
        self.position_maps.remove(uri);
    }

    /// Generate constraints from an AST
    ///
    /// Implements Algorithm W constraint generation with type environment threading.
    /// Returns constraints, type_map, and position_map for type-at-position queries.
    fn generate_constraints(ast: &AstNode, symbol_table: &SymbolTable) -> Result<ConstraintResult> {
        let mut ctx = ConstraintGenContext::new();
        let mut env = TypeEnvironment::from_symbol_table(symbol_table, ast);

        Self::visit_node_with_env(ast, &mut env, &mut ctx)?;

        Ok(ConstraintResult(
            ConstraintSet { constraints: ctx.constraints },
            ctx.type_map,
            ctx.position_map,
        ))
    }

    /// Visit an AST node and generate constraints with type environment
    ///
    /// Implements constraint generation for core Python constructs.
    /// Records type information in the context for type-at-position queries.
    fn visit_node_with_env(node: &AstNode, env: &mut TypeEnvironment, ctx: &mut ConstraintGenContext) -> Result<Type> {
        match node {
            AstNode::Module { body, .. } => {
                for stmt in body {
                    Self::visit_node_with_env(stmt, env, ctx)?;
                }
                Ok(Type::Con(beacon_core::TypeCtor::Module("".into())))
            }
            AstNode::FunctionDef { name, args, return_type, body, decorators, line, col, .. } => {
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
                    body_ty = Self::visit_node_with_env(stmt, &mut body_env, ctx)?;
                }

                let span = Span::new(*line, *col);
                ctx.constraints.push(Constraint::Equal(body_ty, ret_type, span));

                let mut decorated_type = fn_type.clone();
                for decorator in decorators.iter().rev() {
                    let decorator_ty = env.lookup(decorator).unwrap_or_else(|| Type::Var(env.fresh_var()));
                    let result_ty = Type::Var(env.fresh_var());

                    let span = Span::new(*line, *col);
                    ctx.constraints.push(Constraint::Call(
                        decorator_ty,
                        vec![decorated_type],
                        result_ty.clone(),
                        span,
                    ));

                    decorated_type = result_ty;
                }

                env.bind(name.clone(), TypeScheme::mono(decorated_type.clone()));

                ctx.record_type(*line, *col, decorated_type.clone());
                Ok(decorated_type)
            }

            AstNode::ClassDef { name, body, decorators, line, col, .. } => {
                let class_type = Type::Con(beacon_core::TypeCtor::Class(name.clone()));

                for stmt in body {
                    Self::visit_node_with_env(stmt, env, ctx)?;
                }

                let mut decorated_type = class_type.clone();
                for decorator in decorators.iter().rev() {
                    let decorator_ty = env.lookup(decorator).unwrap_or_else(|| Type::Var(env.fresh_var()));
                    let result_ty = Type::Var(env.fresh_var());
                    let span = Span::new(*line, *col);

                    ctx.constraints.push(Constraint::Call(
                        decorator_ty,
                        vec![decorated_type],
                        result_ty.clone(),
                        span,
                    ));

                    decorated_type = result_ty;
                }

                env.bind(name.clone(), TypeScheme::mono(decorated_type.clone()));

                ctx.record_type(*line, *col, decorated_type.clone());
                Ok(decorated_type)
            }
            // TODO: Determine if value is non-expansive for generalization
            AstNode::Assignment { target, value, line, col } => {
                let value_ty = Self::visit_node_with_env(value, env, ctx)?;
                env.bind(target.clone(), TypeScheme::mono(value_ty.clone()));
                ctx.record_type(*line, *col, value_ty.clone());
                Ok(value_ty)
            }
            AstNode::AnnotatedAssignment { target, type_annotation, value, line, col } => {
                let annotated_ty = env.parse_annotation_or_any(type_annotation);
                if let Some(val) = value {
                    let value_ty = Self::visit_node_with_env(val, env, ctx)?;
                    let span = Span::new(*line, *col);
                    ctx.constraints
                        .push(Constraint::Equal(value_ty, annotated_ty.clone(), span));
                }
                env.bind(target.clone(), TypeScheme::mono(annotated_ty.clone()));
                ctx.record_type(*line, *col, annotated_ty.clone());
                Ok(annotated_ty)
            }
            // TODO: Handle complex call expressions
            AstNode::Call { function, args, line, col } => {
                let func_ty = env.lookup(function).unwrap_or_else(|| Type::Var(env.fresh_var()));
                let mut arg_types = Vec::new();
                for arg in args {
                    arg_types.push(Self::visit_node_with_env(arg, env, ctx)?);
                }
                let ret_ty = Type::Var(env.fresh_var());
                let span = Span::new(*line, *col);
                ctx.constraints
                    .push(Constraint::Call(func_ty, arg_types, ret_ty.clone(), span));
                ctx.record_type(*line, *col, ret_ty.clone());
                Ok(ret_ty)
            }
            AstNode::Identifier { name, line, col } => {
                let ty = env.lookup(name).unwrap_or_else(|| Type::Var(env.fresh_var()));
                ctx.record_type(*line, *col, ty.clone());
                Ok(ty)
            }
            AstNode::Literal { value, line, col } => {
                let ty = match value {
                    LiteralValue::Integer(_) => Type::int(),
                    LiteralValue::Float(_) => Type::float(),
                    LiteralValue::String(_) => Type::string(),
                    LiteralValue::Boolean(_) => Type::bool(),
                    LiteralValue::None => Type::none(),
                };
                ctx.record_type(*line, *col, ty.clone());
                Ok(ty)
            }
            AstNode::Return { value, line, col } => {
                let ty = if let Some(val) = value { Self::visit_node_with_env(val, env, ctx)? } else { Type::none() };
                ctx.record_type(*line, *col, ty.clone());
                Ok(ty)
            }
            AstNode::Attribute { object, attribute, line, col } => {
                let obj_ty = Self::visit_node_with_env(object, env, ctx)?;
                let attr_ty = Type::Var(env.fresh_var());
                let span = Span::new(*line, *col);
                ctx.constraints
                    .push(Constraint::HasAttr(obj_ty, attribute.clone(), attr_ty.clone(), span));
                ctx.record_type(*line, *col, attr_ty.clone());
                Ok(attr_ty)
            }
            AstNode::BinaryOp { left, right, line, col, .. } => {
                let left_ty = Self::visit_node_with_env(left, env, ctx)?;
                let right_ty = Self::visit_node_with_env(right, env, ctx)?;
                let span = Span::new(*line, *col);
                ctx.constraints.push(Constraint::Equal(left_ty.clone(), right_ty, span));
                ctx.record_type(*line, *col, left_ty.clone());
                Ok(left_ty)
            }
            AstNode::UnaryOp { operand, line, col, .. } => {
                let ty = Self::visit_node_with_env(operand, env, ctx)?;
                ctx.record_type(*line, *col, ty.clone());
                Ok(ty)
            }
            AstNode::If { test, body, elif_parts, else_body, .. } => {
                Self::visit_node_with_env(test, env, ctx)?;

                let (narrowed_var, narrowed_type) = Self::detect_type_guard(test, &mut env.clone());

                let mut true_env = env.clone();
                if let (Some(var_name), Some(refined_ty)) = (narrowed_var.as_ref(), narrowed_type.as_ref()) {
                    true_env.bind(var_name.clone(), TypeScheme::mono(refined_ty.clone()));
                }

                for stmt in body {
                    Self::visit_node_with_env(stmt, &mut true_env, ctx)?;
                }

                for (elif_test, elif_body) in elif_parts {
                    Self::visit_node_with_env(elif_test, env, ctx)?;
                    for stmt in elif_body {
                        Self::visit_node_with_env(stmt, env, ctx)?;
                    }
                }
                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        Self::visit_node_with_env(stmt, env, ctx)?;
                    }
                }
                Ok(Type::none())
            }
            AstNode::Import { .. } | AstNode::ImportFrom { .. } => {
                Ok(Type::Con(beacon_core::TypeCtor::Module("".into())))
            }
            // TODO: Replace with Protocol constraint when protocol system is implemented
            AstNode::For { target, iter, body, else_body, line, col } => {
                let iter_ty = Self::visit_node_with_env(iter, env, ctx)?;

                let iter_return_ty = Type::Var(env.fresh_var());
                let span = Span::new(*line, *col);
                ctx.constraints.push(Constraint::HasAttr(
                    iter_ty,
                    "__iter__".to_string(),
                    iter_return_ty.clone(),
                    span,
                ));

                let element_ty = Type::Var(env.fresh_var());
                env.bind(target.clone(), TypeScheme::mono(element_ty));

                for stmt in body {
                    Self::visit_node_with_env(stmt, env, ctx)?;
                }

                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        Self::visit_node_with_env(stmt, env, ctx)?;
                    }
                }

                ctx.record_type(*line, *col, Type::none());
                Ok(Type::none())
            }

            AstNode::While { test, body, else_body, line, col } => {
                Self::visit_node_with_env(test, env, ctx)?;

                for stmt in body {
                    Self::visit_node_with_env(stmt, env, ctx)?;
                }

                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        Self::visit_node_with_env(stmt, env, ctx)?;
                    }
                }

                ctx.record_type(*line, *col, Type::none());
                Ok(Type::none())
            }

            AstNode::Try { body, handlers, else_body, finally_body, line, col } => {
                for stmt in body {
                    Self::visit_node_with_env(stmt, env, ctx)?;
                }

                for handler in handlers {
                    let mut handler_env = env.clone();

                    if let Some(ref name) = handler.name {
                        // TODO: Use proper exception type hierarchy when available
                        let exc_ty = Type::Var(env.fresh_var());
                        handler_env.bind(name.clone(), TypeScheme::mono(exc_ty));
                    }

                    for stmt in &handler.body {
                        Self::visit_node_with_env(stmt, &mut handler_env, ctx)?;
                    }
                }

                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        Self::visit_node_with_env(stmt, env, ctx)?;
                    }
                }

                if let Some(finally_stmts) = finally_body {
                    for stmt in finally_stmts {
                        Self::visit_node_with_env(stmt, env, ctx)?;
                    }
                }

                ctx.record_type(*line, *col, Type::none());
                Ok(Type::none())
            }

            AstNode::Match { subject, cases, line, col } => {
                Self::visit_node_with_env(subject, env, ctx)?;

                // TODO: Implement pattern matching type narrowing when pattern system is ready
                for case in cases {
                    let mut case_env = env.clone();

                    if let Some(ref guard) = case.guard {
                        Self::visit_node_with_env(guard, &mut case_env, ctx)?;
                    }

                    for stmt in &case.body {
                        Self::visit_node_with_env(stmt, &mut case_env, ctx)?;
                    }
                }

                ctx.record_type(*line, *col, Type::none());
                Ok(Type::none())
            }
            AstNode::Raise { exc, line, col } => {
                if let Some(exception) = exc {
                    Self::visit_node_with_env(exception, env, ctx)?;
                }

                ctx.record_type(*line, *col, Type::never());
                Ok(Type::never())
            }

            AstNode::With { items, body, line, col } => {
                for item in items {
                    let context_ty = Self::visit_node_with_env(&item.context_expr, env, ctx)?;

                    // TODO: Replace with Protocol constraint for context manager protocol
                    let enter_ty = Type::Var(env.fresh_var());
                    let span = Span::new(*line, *col);
                    ctx.constraints.push(Constraint::HasAttr(
                        context_ty.clone(),
                        "__enter__".to_string(),
                        enter_ty.clone(),
                        span,
                    ));

                    let exit_ty = Type::Var(env.fresh_var());
                    ctx.constraints
                        .push(Constraint::HasAttr(context_ty, "__exit__".to_string(), exit_ty, span));

                    if let Some(ref target) = item.optional_vars {
                        env.bind(target.clone(), TypeScheme::mono(enter_ty));
                    }
                }

                for stmt in body {
                    Self::visit_node_with_env(stmt, env, ctx)?;
                }

                ctx.record_type(*line, *col, Type::none());
                Ok(Type::none())
            }

            AstNode::Lambda { args, body, line, col } => {
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

                let mut lambda_env = env.clone();
                for (param, param_type) in args.iter().zip(param_types.iter()) {
                    lambda_env.bind(param.name.clone(), TypeScheme::mono(param_type.clone()));
                }

                let body_ty = Self::visit_node_with_env(body, &mut lambda_env, ctx)?;
                let lambda_ty = Type::fun(param_types, body_ty);

                ctx.record_type(*line, *col, lambda_ty.clone());
                Ok(lambda_ty)
            }

            AstNode::Compare { left, comparators, line, col, .. } => {
                Self::visit_node_with_env(left, env, ctx)?;

                for comp in comparators {
                    Self::visit_node_with_env(comp, env, ctx)?;
                }

                ctx.record_type(*line, *col, Type::bool());
                Ok(Type::bool())
            }
            // TODO: Replace with Protocol constraint when subscript protocol is implemented
            AstNode::Subscript { value, slice, line, col } => {
                let value_ty = Self::visit_node_with_env(value, env, ctx)?;
                Self::visit_node_with_env(slice, env, ctx)?;

                let item_ty = Type::Var(env.fresh_var());
                let span = Span::new(*line, *col);
                ctx.constraints.push(Constraint::HasAttr(
                    value_ty,
                    "__getitem__".to_string(),
                    item_ty.clone(),
                    span,
                ));

                ctx.record_type(*line, *col, item_ty.clone());
                Ok(item_ty)
            }
            AstNode::NamedExpr { target, value, line, col } => {
                let value_ty = Self::visit_node_with_env(value, env, ctx)?;
                env.bind(target.clone(), TypeScheme::mono(value_ty.clone()));
                ctx.record_type(*line, *col, value_ty.clone());
                Ok(value_ty)
            }
            // TODO: Replace with Protocol constraint for iterator protocol
            AstNode::ListComp { element, generators, line, col } => {
                let mut comp_env = env.clone();

                for generator in generators {
                    let iter_ty = Self::visit_node_with_env(&generator.iter, &mut comp_env, ctx)?;

                    let iter_return_ty = Type::Var(comp_env.fresh_var());
                    let span = Span::new(*line, *col);
                    ctx.constraints.push(Constraint::HasAttr(
                        iter_ty,
                        "__iter__".to_string(),
                        iter_return_ty,
                        span,
                    ));

                    let element_ty = Type::Var(comp_env.fresh_var());
                    comp_env.bind(generator.target.clone(), TypeScheme::mono(element_ty));

                    for if_clause in &generator.ifs {
                        Self::visit_node_with_env(if_clause, &mut comp_env, ctx)?;
                    }
                }

                let elem_ty = Self::visit_node_with_env(element, &mut comp_env, ctx)?;
                let list_ty = Type::list(elem_ty);

                ctx.record_type(*line, *col, list_ty.clone());
                Ok(list_ty)
            }

            AstNode::SetComp { element, generators, line, col } => {
                let mut comp_env = env.clone();

                for generator in generators {
                    let iter_ty = Self::visit_node_with_env(&generator.iter, &mut comp_env, ctx)?;

                    let iter_return_ty = Type::Var(comp_env.fresh_var());
                    let span = Span::new(*line, *col);
                    ctx.constraints.push(Constraint::HasAttr(
                        iter_ty,
                        "__iter__".to_string(),
                        iter_return_ty,
                        span,
                    ));

                    let element_ty = Type::Var(comp_env.fresh_var());
                    comp_env.bind(generator.target.clone(), TypeScheme::mono(element_ty));

                    for if_clause in &generator.ifs {
                        Self::visit_node_with_env(if_clause, &mut comp_env, ctx)?;
                    }
                }

                let elem_ty = Self::visit_node_with_env(element, &mut comp_env, ctx)?;
                let set_ty = Type::App(Box::new(Type::Con(beacon_core::TypeCtor::Set)), Box::new(elem_ty));

                ctx.record_type(*line, *col, set_ty.clone());
                Ok(set_ty)
            }

            AstNode::DictComp { key, value, generators, line, col } => {
                let mut comp_env = env.clone();

                for generator in generators {
                    let iter_ty = Self::visit_node_with_env(&generator.iter, &mut comp_env, ctx)?;

                    let iter_return_ty = Type::Var(comp_env.fresh_var());
                    let span = Span::new(*line, *col);
                    ctx.constraints.push(Constraint::HasAttr(
                        iter_ty,
                        "__iter__".to_string(),
                        iter_return_ty,
                        span,
                    ));

                    let element_ty = Type::Var(comp_env.fresh_var());
                    comp_env.bind(generator.target.clone(), TypeScheme::mono(element_ty));

                    for if_clause in &generator.ifs {
                        Self::visit_node_with_env(if_clause, &mut comp_env, ctx)?;
                    }
                }

                let key_ty = Self::visit_node_with_env(key, &mut comp_env, ctx)?;
                let val_ty = Self::visit_node_with_env(value, &mut comp_env, ctx)?;
                let dict_ty = Type::dict(key_ty, val_ty);

                ctx.record_type(*line, *col, dict_ty.clone());
                Ok(dict_ty)
            }

            // NOTE: Approximated as iterable[T] rather than proper Generator[T, None, None]
            // TODO: See ROADMAP.md for Generator[YieldType, SendType, ReturnType] modeling
            AstNode::GeneratorExp { element, generators, line, col } => {
                let mut comp_env = env.clone();

                for generator in generators {
                    let iter_ty = Self::visit_node_with_env(&generator.iter, &mut comp_env, ctx)?;

                    let iter_return_ty = Type::Var(comp_env.fresh_var());
                    let span = Span::new(*line, *col);
                    ctx.constraints.push(Constraint::HasAttr(
                        iter_ty,
                        "__iter__".to_string(),
                        iter_return_ty,
                        span,
                    ));

                    let element_ty = Type::Var(comp_env.fresh_var());
                    comp_env.bind(generator.target.clone(), TypeScheme::mono(element_ty));

                    for if_clause in &generator.ifs {
                        Self::visit_node_with_env(if_clause, &mut comp_env, ctx)?;
                    }
                }

                let elem_ty = Self::visit_node_with_env(element, &mut comp_env, ctx)?;
                let generator_ty = Type::list(elem_ty);

                ctx.record_type(*line, *col, generator_ty.clone());
                Ok(generator_ty)
            }
            AstNode::Pass { .. } | AstNode::Break { .. } | AstNode::Continue { .. } => Ok(Type::none()),
        }
    }

    #[deprecated]
    fn _visit_node(&mut self, node: &AstNode, _constraints: &mut [Constraint]) -> Result<Type> {
        let mut env = TypeEnvironment::new();
        let mut ctx = ConstraintGenContext::new();
        Self::visit_node_with_env(node, &mut env, &mut ctx)
    }

    /// Solve a set of constraints using beacon-core's unification algorithm
    ///
    /// Returns a substitution and a list of type errors encountered during solving.
    /// Errors are accumulated rather than failing fast to provide comprehensive feedback.
    fn solve_constraints(&mut self, constraint_set: ConstraintSet) -> Result<(Subst, Vec<TypeErrorInfo>)> {
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

                // TODO: Implement record/structural typing
                // TODO: use row-polymorphic records
                Constraint::HasAttr(_obj_ty, _attr_name, _attr_ty, _span) => {}
            }
        }

        Ok((subst, type_errors))
    }

    /// Perform static analysis (CFG + data flow) on the AST
    ///
    /// Builds control flow graphs for each function and runs data flow analyses to detect use-before-def, unreachable code, and unused variables.
    /// TODO: Extend to module-level analysis
    fn perform_static_analysis(&self, ast: &AstNode, symbol_table: &SymbolTable) -> Option<data_flow::DataFlowResult> {
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

    /// Check if a string is a valid Python identifier ([a-zA-Z_][a-zA-Z0-9_]*),
    /// filtering out cases where the parser created an Identifier node for other syntax.
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
    /// - `x is not None` -> (x, T) where x: Optional[T]
    /// - `x == None` -> (x, None)
    /// - `x != None` -> (x, T) where x: Optional[T]
    /// - `if x:` -> (x, T) where x: Optional[T] (truthiness narrows out None)
    fn detect_type_guard(test: &AstNode, env: &mut TypeEnvironment) -> (Option<String>, Option<Type>) {
        if let AstNode::Identifier { name: var_name, .. } = test {
            if let Some(current_type) = env.lookup(var_name) {
                if current_type.is_optional() || matches!(current_type, Type::Union(_)) {
                    let narrowed = current_type.remove_from_union(&Type::none());
                    return (Some(var_name.clone()), Some(narrowed));
                }
            }
            return (None, None);
        }

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
                            beacon_parser::CompareOperator::Is | beacon_parser::CompareOperator::Eq => {
                                return (Some(var_name.clone()), Some(Type::none()));
                            }
                            beacon_parser::CompareOperator::IsNot | beacon_parser::CompareOperator::NotEq => {
                                if let Some(current_type) = env.lookup(var_name) {
                                    let narrowed = current_type.remove_from_union(&Type::none());
                                    return (Some(var_name.clone()), Some(narrowed));
                                }
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

    #[test]
    fn test_flow_sensitive_is_not_none() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
def process(x: int | None):
    if x is not None:
        # In this branch, x is narrowed from Optional[int] to int
        return x + 1
    else:
        return 0
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = analyzer.analyze(&uri);
        assert!(result.is_ok(), "Analysis should succeed with 'is not None' narrowing");
    }

    #[test]
    fn test_flow_sensitive_not_equal_none() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
def process(x: str | None):
    if x != None:
        # In this branch, x is narrowed from Optional[str] to str
        return x.upper()
    else:
        return ""
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = analyzer.analyze(&uri);
        assert!(result.is_ok(), "Analysis should succeed with '!= None' narrowing");
    }

    #[test]
    fn test_flow_sensitive_equal_none() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
def process(x: int | None):
    if x == None:
        # In this branch, x is narrowed to None
        return 0
    else:
        # In this branch, x is int
        return x + 1
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = analyzer.analyze(&uri);
        assert!(result.is_ok(), "Analysis should succeed with '== None' narrowing");
    }

    #[test]
    fn test_flow_sensitive_truthiness() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
def process(x: str | None):
    if x:
        # In this branch, x is narrowed from Optional[str] to str
        # because None is falsy
        return x.upper()
    else:
        return ""
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = analyzer.analyze(&uri);
        assert!(result.is_ok(), "Analysis should succeed with truthiness narrowing");
    }

    #[test]
    fn test_type_at_position_simple() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
x = 42
y = "hello"
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let pos = Position { line: 1, character: 4 };
        let result = analyzer.type_at_position(&uri, pos);

        assert!(result.is_ok(), "type_at_position should succeed");
    }

    #[test]
    fn test_type_at_position_identifier() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
x = 42
print(x)
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let pos = Position { line: 1, character: 0 };
        let result = analyzer.type_at_position(&uri, pos);

        assert!(result.is_ok(), "type_at_position should succeed");
    }

    #[test]
    fn test_type_map_populated() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
x = 42
y = 3.14
z = "hello"
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = analyzer.analyze(&uri).unwrap();

        assert!(!result.type_map.is_empty(), "Type map should be populated");

        let has_int = result
            .type_map
            .values()
            .any(|t| matches!(t, Type::Con(beacon_core::TypeCtor::Int)));
        let has_float = result
            .type_map
            .values()
            .any(|t| matches!(t, Type::Con(beacon_core::TypeCtor::Float)));
        let has_string = result
            .type_map
            .values()
            .any(|t| matches!(t, Type::Con(beacon_core::TypeCtor::String)));

        assert!(has_int, "Type map should contain int type");
        assert!(has_float, "Type map should contain float type");
        assert!(has_string, "Type map should contain string type");
    }

    #[test]
    fn test_position_map_invalidation() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = "x = 42";

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let _ = analyzer.analyze(&uri);
        assert!(
            analyzer.position_maps.contains_key(&uri),
            "Position map should be stored"
        );

        analyzer.invalidate(&uri);
        assert!(
            !analyzer.position_maps.contains_key(&uri),
            "Position map should be removed"
        );
    }

    #[test]
    fn test_for_loop_constraint_generation() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
for item in [1, 2, 3]:
    print(item)
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = analyzer.analyze(&uri);
        assert!(result.is_ok(), "For loop analysis should succeed");
    }

    #[test]
    fn test_while_loop_constraint_generation() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
x = 0
while x < 10:
    x = x + 1
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = analyzer.analyze(&uri);
        assert!(result.is_ok(), "While loop analysis should succeed");
    }

    #[test]
    fn test_try_except_constraint_generation() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
try:
    x = 1 / 0
except Exception as e:
    print(e)
finally:
    print("done")
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = analyzer.analyze(&uri);
        assert!(result.is_ok(), "Try/except analysis should succeed");
    }

    #[test]
    fn test_with_statement_constraint_generation() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
with open("file.txt") as f:
    content = f.read()
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = analyzer.analyze(&uri);
        assert!(result.is_ok(), "With statement analysis should succeed");
    }

    #[test]
    fn test_lambda_constraint_generation() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
f = lambda x: x + 1
result = f(42)
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = analyzer.analyze(&uri);
        assert!(result.is_ok(), "Lambda analysis should succeed");
    }

    #[test]
    fn test_list_comprehension_constraint_generation() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
numbers = [1, 2, 3, 4, 5]
squares = [x * x for x in numbers]
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = analyzer.analyze(&uri);
        assert!(result.is_ok(), "List comprehension analysis should succeed");
    }

    #[test]
    fn test_dict_comprehension_constraint_generation() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
numbers = [1, 2, 3]
mapping = {x: x * 2 for x in numbers}
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = analyzer.analyze(&uri);
        assert!(result.is_ok(), "Dict comprehension analysis should succeed");
    }

    #[test]
    fn test_set_comprehension_constraint_generation() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
numbers = [1, 2, 2, 3, 3, 3]
unique = {x for x in numbers}
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = analyzer.analyze(&uri);
        assert!(result.is_ok(), "Set comprehension analysis should succeed");
    }

    #[test]
    fn test_generator_expression_constraint_generation() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
numbers = [1, 2, 3]
gen = (x * 2 for x in numbers)
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = analyzer.analyze(&uri);
        assert!(result.is_ok(), "Generator expression analysis should succeed");
    }

    #[test]
    fn test_compare_expression_constraint_generation() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
x = 5
result = x > 3 and x < 10
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = analyzer.analyze(&uri);
        assert!(result.is_ok(), "Compare expression analysis should succeed");
    }

    #[test]
    fn test_subscript_constraint_generation() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
items = [1, 2, 3]
first = items[0]
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = analyzer.analyze(&uri);
        assert!(result.is_ok(), "Subscript analysis should succeed");
    }

    #[test]
    fn test_walrus_operator_constraint_generation() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
if (n := len([1, 2, 3])) > 2:
    print(n)
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = analyzer.analyze(&uri);
        assert!(result.is_ok(), "Walrus operator analysis should succeed");
    }

    #[test]
    fn test_raise_statement_returns_never() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
def fail():
    raise ValueError("error")
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = analyzer.analyze(&uri);
        assert!(result.is_ok(), "Raise statement analysis should succeed");
    }

    #[test]
    fn test_decorator_constraint_generation() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
def decorator(func):
    return func

@decorator
def greet(name):
    return "Hello " + name
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = analyzer.analyze(&uri);
        assert!(result.is_ok(), "Decorator analysis should succeed");
    }

    #[test]
    fn test_match_statement_constraint_generation() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
def describe(x):
    match x:
        case 0:
            return "zero"
        case _:
            return "other"
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = analyzer.analyze(&uri);
        assert!(result.is_ok(), "Match statement analysis should succeed");
    }

    #[test]
    fn test_pass_break_continue_statements() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
for i in range(10):
    if i == 0:
        continue
    elif i == 5:
        break
    else:
        pass
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = analyzer.analyze(&uri);
        assert!(result.is_ok(), "Pass/break/continue analysis should succeed");
    }

    #[test]
    fn test_constraint_gen_context() {
        let mut ctx = ConstraintGenContext::new();

        assert_eq!(ctx.node_counter, 0);
        assert!(ctx.type_map.is_empty());
        assert!(ctx.position_map.is_empty());

        let node_id = ctx.record_type(1, 5, Type::int());
        assert_eq!(node_id, 0);
        assert_eq!(ctx.node_counter, 1);
        assert_eq!(ctx.type_map.len(), 1);
        assert_eq!(ctx.position_map.len(), 1);
        assert_eq!(ctx.position_map.get(&(1, 5)), Some(&0));

        let node_id2 = ctx.record_type(2, 3, Type::string());
        assert_eq!(node_id2, 1);
        assert_eq!(ctx.node_counter, 2);
        assert_eq!(ctx.type_map.len(), 2);
        assert_eq!(ctx.position_map.len(), 2);
    }
}

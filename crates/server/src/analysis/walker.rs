use super::loader;
use super::pattern::extract_pattern_bindings;
use super::type_env::TypeEnvironment;

use beacon_constraint::{
    Constraint, ConstraintGenContext, ConstraintResult, ConstraintSet, Span, TypeGuardInfo, TypeGuardKind,
    TypePredicate,
};
use beacon_core::{ClassMetadata, Type, TypeScheme, errors::Result};
use beacon_core::{TypeCtor, TypeVarGen};
use beacon_parser::{AstNode, LiteralValue, SymbolTable};
use std::sync::Arc;

type TStubCache = Arc<std::sync::RwLock<crate::workspace::StubCache>>;

/// Expression context for constraint generation
///
/// Tracks whether an expression's result is used (value context) or discarded (void context) to avoid generating spurious constraints.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ExprContext {
    /// Value context: expression result is used (e.g., in assignment, return, argument)
    Value,
    /// Void context: expression result is discarded (e.g., statement position)
    Void,
}

/// Represents the kind of function based on yield/await detection
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum FunctionKind {
    /// Regular synchronous function
    Regular,
    /// Generator function (contains yield or yield from)
    Generator,
    /// Async generator function (async def with yield)
    AsyncGenerator,
    /// Coroutine function (async def without yield)
    Coroutine,
}

/// Detect what kind of function this is based on its body and async flag
///
/// Traverses the function body AST to detect yield, yield from, and await expressions.
/// Note: This is a simplified implementation that detects presence but doesn't validate
/// that yields/awaits are at the correct scope level (e.g., not inside nested functions).
fn detect_function_kind(body: &[AstNode], is_async: bool) -> FunctionKind {
    let has_yield = contains_yield(body);

    match (is_async, has_yield) {
        (true, true) => FunctionKind::AsyncGenerator,
        (true, false) => FunctionKind::Coroutine,
        (false, true) => FunctionKind::Generator,
        (false, false) => FunctionKind::Regular,
    }
}

/// Check if a statement is a docstring (string literal expression)
///
/// Note that docstrings are standalone string literals that appear as expression statements.
/// They are metadata and should not generate type constraints.
fn is_docstring(node: &AstNode) -> bool {
    matches!(node, AstNode::Literal { value: LiteralValue::String { .. }, .. })
}

/// Get the line and column position from any AstNode.
///
/// Extracts the source position from the node for error reporting and span tracking.
fn get_node_position(node: &AstNode) -> (usize, usize, usize, usize) {
    match node {
        AstNode::Module { .. } => (1, 1, 1, 1),
        AstNode::FunctionDef { line, col, end_line, end_col, .. }
        | AstNode::ClassDef { line, col, end_line, end_col, .. }
        | AstNode::If { line, col, end_line, end_col, .. }
        | AstNode::For { line, col, end_line, end_col, .. }
        | AstNode::While { line, col, end_line, end_col, .. }
        | AstNode::Try { line, col, end_line, end_col, .. }
        | AstNode::With { line, col, end_line, end_col, .. }
        | AstNode::Match { line, col, end_line, end_col, .. }
        | AstNode::Assignment { line, col, end_line, end_col, .. }
        | AstNode::AnnotatedAssignment { line, col, end_line, end_col, .. }
        | AstNode::Call { line, col, end_line, end_col, .. }
        | AstNode::Identifier { line, col, end_line, end_col, .. }
        | AstNode::Literal { line, col, end_line, end_col, .. }
        | AstNode::Return { line, col, end_line, end_col, .. }
        | AstNode::BinaryOp { line, col, end_line, end_col, .. }
        | AstNode::UnaryOp { line, col, end_line, end_col, .. }
        | AstNode::Compare { line, col, end_line, end_col, .. }
        | AstNode::Attribute { line, col, end_line, end_col, .. }
        | AstNode::Subscript { line, col, end_line, end_col, .. }
        | AstNode::List { line, col, end_line, end_col, .. }
        | AstNode::Tuple { line, col, end_line, end_col, .. }
        | AstNode::Set { line, col, end_line, end_col, .. }
        | AstNode::Dict { line, col, end_line, end_col, .. }
        | AstNode::ListComp { line, col, end_line, end_col, .. }
        | AstNode::DictComp { line, col, end_line, end_col, .. }
        | AstNode::SetComp { line, col, end_line, end_col, .. }
        | AstNode::GeneratorExp { line, col, end_line, end_col, .. }
        | AstNode::NamedExpr { line, col, end_line, end_col, .. }
        | AstNode::Lambda { line, col, end_line, end_col, .. }
        | AstNode::Yield { line, col, end_line, end_col, .. }
        | AstNode::YieldFrom { line, col, end_line, end_col, .. }
        | AstNode::Await { line, col, end_line, end_col, .. }
        | AstNode::Import { line, col, end_line, end_col, .. }
        | AstNode::ImportFrom { line, col, end_line, end_col, .. }
        | AstNode::Raise { line, col, end_line, end_col, .. }
        | AstNode::Pass { line, col, end_line, end_col, .. }
        | AstNode::Break { line, col, end_line, end_col, .. }
        | AstNode::Continue { line, col, end_line, end_col, .. }
        | AstNode::Assert { line, col, end_line, end_col, .. }
        | AstNode::Starred { line, col, end_line, end_col, .. }
        | AstNode::ParenthesizedExpression { line, col, end_line, end_col, .. } => (*line, *col, *end_line, *end_col),
    }
}

/// Check if the AST nodes contain yield or yield from expressions in the current scope.
///
/// This correctly excludes yields that appear in nested function definitions, lambdas,
/// or comprehensions (which have their own scope).
fn contains_yield(nodes: &[AstNode]) -> bool {
    for node in nodes {
        if check_node_for_yield(node) {
            return true;
        }
    }
    false
}

/// Recursively check a single node for yield/yield from in the current scope
fn check_node_for_yield(node: &AstNode) -> bool {
    match node {
        AstNode::Yield { .. } | AstNode::YieldFrom { .. } => true,
        AstNode::FunctionDef { .. } | AstNode::ClassDef { .. } => false,
        AstNode::If { test, body, elif_parts, else_body, .. } => {
            check_node_for_yield(test)
                || contains_yield(body)
                || elif_parts
                    .iter()
                    .any(|(test, body)| check_node_for_yield(test) || contains_yield(body))
                || else_body.as_ref().is_some_and(|b| contains_yield(b))
        }
        AstNode::For { iter, body, else_body, .. } => {
            check_node_for_yield(iter) || contains_yield(body) || else_body.as_ref().is_some_and(|b| contains_yield(b))
        }
        AstNode::While { test, body, else_body, .. } => {
            check_node_for_yield(test) || contains_yield(body) || else_body.as_ref().is_some_and(|b| contains_yield(b))
        }
        AstNode::Try { body, handlers, else_body, finally_body, .. } => {
            contains_yield(body)
                || handlers.iter().any(|h| contains_yield(&h.body))
                || else_body.as_ref().is_some_and(|b| contains_yield(b))
                || finally_body.as_ref().is_some_and(|b| contains_yield(b))
        }
        AstNode::With { body, .. } => contains_yield(body),
        AstNode::Match { subject, cases, .. } => {
            check_node_for_yield(subject) || cases.iter().any(|c| contains_yield(&c.body))
        }
        AstNode::ListComp { .. } => false,
        AstNode::DictComp { .. } => false,
        AstNode::SetComp { .. } => false,
        AstNode::GeneratorExp { .. } => false,
        AstNode::Call { args, keywords, .. } => {
            args.iter().any(check_node_for_yield) || keywords.iter().any(|(_, v)| check_node_for_yield(v))
        }
        AstNode::BinaryOp { left, right, .. } => check_node_for_yield(left) || check_node_for_yield(right),
        AstNode::UnaryOp { operand, .. } => check_node_for_yield(operand),
        AstNode::Compare { left, comparators, .. } => {
            check_node_for_yield(left) || comparators.iter().any(check_node_for_yield)
        }
        AstNode::Lambda { .. } => false,
        AstNode::Assignment { value, .. } => check_node_for_yield(value),
        AstNode::AnnotatedAssignment { value, .. } => value.as_ref().is_some_and(|v| check_node_for_yield(v)),
        AstNode::NamedExpr { value, .. } => check_node_for_yield(value),
        AstNode::Return { value, .. } => value.as_ref().is_some_and(|v| check_node_for_yield(v)),
        AstNode::Raise { exc, .. } => exc.as_ref().is_some_and(|e| check_node_for_yield(e)),
        AstNode::Attribute { object, .. } => check_node_for_yield(object),
        AstNode::Subscript { value, slice, .. } => check_node_for_yield(value) || check_node_for_yield(slice),
        AstNode::Tuple { elements, .. } => elements.iter().any(check_node_for_yield),
        AstNode::List { elements, .. } => elements.iter().any(check_node_for_yield),
        AstNode::Dict { keys, values, .. } => {
            keys.iter().any(check_node_for_yield) || values.iter().any(check_node_for_yield)
        }
        AstNode::Set { elements, .. } => elements.iter().any(check_node_for_yield),
        AstNode::Await { value, .. } => check_node_for_yield(value),
        AstNode::Identifier { .. }
        | AstNode::Literal { .. }
        | AstNode::Pass { .. }
        | AstNode::Break { .. }
        | AstNode::Continue { .. }
        | AstNode::Import { .. }
        | AstNode::ImportFrom { .. } => false,
        AstNode::Module { .. } => false,
        AstNode::Assert { .. } | AstNode::Starred { .. } | AstNode::ParenthesizedExpression { .. } => false,
    }
}

/// Return path analysis result
///
/// Tracks what kinds of returns exist in a function to infer the correct return type.
#[derive(Debug, Clone)]
struct ReturnPathAnalysis {
    /// Whether function has explicit `return value` statements
    has_value_returns: bool,
    /// Whether function has explicit `return` (no value) statements
    has_none_returns: bool,
    /// Whether function can fall through (implicit return None)
    has_implicit_return: bool,
}

impl ReturnPathAnalysis {
    /// Determine if this function should infer Optional[T] return type
    ///
    /// A function gets Optional[T] when it has both:
    /// - Explicit returns with values, AND
    /// - Either explicit `return` without value OR implicit returns (fall through)
    fn should_infer_optional(&self) -> bool {
        self.has_value_returns && (self.has_none_returns || self.has_implicit_return)
    }

    /// Determine if this function should infer None return type
    ///
    /// A function gets None return type when it has NO value returns, only explicit `return` or implicit returns.
    fn should_infer_none(&self) -> bool {
        !self.has_value_returns && (self.has_none_returns || self.has_implicit_return)
    }
}

/// Analyze return paths in a function body
///
/// Examines all paths through the function to determine what types of returns exist.
/// This is used to infer Optional[T] for mixed returns and None for implicit returns.
fn analyze_return_paths(body: &[AstNode]) -> ReturnPathAnalysis {
    let mut analysis =
        ReturnPathAnalysis { has_value_returns: false, has_none_returns: false, has_implicit_return: true };

    for stmt in body {
        collect_returns(stmt, &mut analysis);
    }

    analysis.has_implicit_return = !all_paths_exit(body);

    analysis
}

/// Recursively collect return statements from a node
fn collect_returns(node: &AstNode, analysis: &mut ReturnPathAnalysis) {
    match node {
        AstNode::Return { value, .. } => {
            if value.is_some() {
                analysis.has_value_returns = true;
            } else {
                analysis.has_none_returns = true;
            }
        }
        AstNode::If { body, elif_parts, else_body, .. } => {
            for stmt in body {
                collect_returns(stmt, analysis);
            }
            for (_, elif_body) in elif_parts {
                for stmt in elif_body {
                    collect_returns(stmt, analysis);
                }
            }
            if let Some(else_stmts) = else_body {
                for stmt in else_stmts {
                    collect_returns(stmt, analysis);
                }
            }
        }
        AstNode::For { body, else_body, .. } | AstNode::While { body, else_body, .. } => {
            for stmt in body {
                collect_returns(stmt, analysis);
            }
            if let Some(else_stmts) = else_body {
                for stmt in else_stmts {
                    collect_returns(stmt, analysis);
                }
            }
        }
        AstNode::Try { body, handlers, else_body, finally_body, .. } => {
            for stmt in body {
                collect_returns(stmt, analysis);
            }
            for handler in handlers {
                for stmt in &handler.body {
                    collect_returns(stmt, analysis);
                }
            }
            if let Some(else_stmts) = else_body {
                for stmt in else_stmts {
                    collect_returns(stmt, analysis);
                }
            }
            if let Some(finally_stmts) = finally_body {
                for stmt in finally_stmts {
                    collect_returns(stmt, analysis);
                }
            }
        }
        AstNode::With { body, .. } => {
            for stmt in body {
                collect_returns(stmt, analysis);
            }
        }
        AstNode::Match { cases, .. } => {
            for case in cases {
                for stmt in &case.body {
                    collect_returns(stmt, analysis);
                }
            }
        }
        AstNode::FunctionDef { .. } | AstNode::ClassDef { .. } | AstNode::Lambda { .. } => {}
        _ => {}
    }
}

/// Check if all paths through the statements exit (return or raise)
fn all_paths_exit(stmts: &[AstNode]) -> bool {
    if stmts.is_empty() {
        return false;
    }

    for stmt in stmts {
        if stmt_always_exits(stmt) {
            return true;
        }
    }

    false
}

/// Check if a statement always exits (never falls through)
fn stmt_always_exits(stmt: &AstNode) -> bool {
    match stmt {
        AstNode::Return { .. } | AstNode::Raise { .. } => true,
        AstNode::If { body, elif_parts, else_body, .. } => {
            if else_body.is_none() {
                return false;
            }
            all_paths_exit(body)
                && elif_parts.iter().all(|(_, elif_body)| all_paths_exit(elif_body))
                && else_body.as_ref().is_some_and(|e| all_paths_exit(e))
        }
        _ => false,
    }
}

pub fn generate_constraints(
    stub_cache: &Option<TStubCache>, ast: &AstNode, symbol_table: &SymbolTable,
) -> Result<ConstraintResult> {
    let mut ctx = ConstraintGenContext::new();

    if let Some(stub_cache) = &stub_cache {
        if let Ok(cache) = stub_cache.try_read() {
            if let Some(builtins) = cache.get("builtins") {
                loader::load_stub_into_registry(builtins, &mut ctx.class_registry)?;
            }
        }
    }

    let mut env = TypeEnvironment::from_symbol_table(symbol_table, ast);

    visit_node_with_env(ast, &mut env, &mut ctx, stub_cache.as_ref())?;

    Ok(ConstraintResult(
        ConstraintSet { constraints: ctx.constraints },
        ctx.type_map,
        ctx.position_map,
        ctx.class_registry,
    ))
}

/// Visit an AST node and generate constraints with type environment
///
/// Implements constraint generation for core Python constructs and records type information in the context for type-at-position queries.
pub fn visit_node_with_env(
    node: &AstNode, env: &mut TypeEnvironment, ctx: &mut ConstraintGenContext, stub_cache: Option<&TStubCache>,
) -> Result<Type> {
    visit_node_with_context(node, env, ctx, stub_cache, ExprContext::Value)
}

/// Internal visitor with expression context tracking
///
/// The `expr_ctx` parameter determines whether to generate unification constraints for expression results.
/// In void contexts (statement position), we skip generating Equal constraints since the result is discarded.
fn visit_node_with_context(
    node: &AstNode, env: &mut TypeEnvironment, ctx: &mut ConstraintGenContext, stub_cache: Option<&TStubCache>,
    expr_ctx: ExprContext,
) -> Result<Type> {
    match node {
        AstNode::Module { body, .. } => {
            for (i, stmt) in body.iter().enumerate() {
                if i == 0 && is_docstring(stmt) {
                    continue;
                }
                visit_node_with_context(stmt, env, ctx, stub_cache, ExprContext::Void)?;
            }
            Ok(Type::Con(TypeCtor::Module("".into())))
        }
        AstNode::FunctionDef {
            name,
            args,
            return_type,
            body,
            decorators,
            is_async,
            line,
            col,
            end_line,
            end_col,
            ..
        } => {
            let params: Vec<(String, Type)> = args
                .iter()
                .map(|param| {
                    let param_type = param
                        .type_annotation
                        .as_ref()
                        .map(|ann| env.parse_annotation_or_any(ann))
                        .unwrap_or_else(|| Type::Var(env.fresh_var()));
                    (param.name.clone(), param_type)
                })
                .collect();

            let function_kind = detect_function_kind(body, *is_async);
            let return_analysis = analyze_return_paths(body);

            let type_guard_info = if let Some(ann) = return_type { extract_type_guard_info(ann, args) } else { None };

            if let Some(guard_info) = &type_guard_info {
                env.register_type_guard(name.clone(), guard_info.clone());
            }

            let (ret_type, gen_params) = if matches!(function_kind, FunctionKind::Regular) {
                let ret = if let Some(ann) = return_type {
                    let parsed = env.parse_annotation_or_any(ann);
                    if type_guard_info.is_some() { Type::bool() } else { parsed }
                } else if return_analysis.should_infer_optional() {
                    let inner_ty = Type::Var(env.fresh_var());
                    Type::Union(vec![inner_ty, Type::none()])
                } else if return_analysis.should_infer_none() {
                    Type::none()
                } else {
                    Type::Var(env.fresh_var())
                };
                (ret, None)
            } else {
                let annotated_ret = return_type
                    .as_ref()
                    .map(|ann| env.parse_annotation_or_any(ann))
                    .unwrap_or_else(|| Type::Var(env.fresh_var()));

                let yield_var = Type::Var(env.fresh_var());
                let send_var = Type::Var(env.fresh_var());

                let (ret, gen_params) = match function_kind {
                    FunctionKind::Generator => {
                        let ret = Type::generator(yield_var.clone(), send_var.clone(), annotated_ret.clone());
                        (ret, Some((yield_var, send_var, annotated_ret)))
                    }
                    FunctionKind::AsyncGenerator => {
                        let ret = Type::async_generator(yield_var.clone(), send_var.clone());
                        (ret, Some((yield_var, send_var, Type::none())))
                    }
                    FunctionKind::Coroutine => {
                        let ret = Type::coroutine(Type::none(), Type::none(), annotated_ret.clone());
                        (ret, Some((Type::none(), Type::none(), annotated_ret)))
                    }
                    FunctionKind::Regular => unreachable!(),
                };
                (ret, gen_params)
            };

            let fn_type = Type::fun(params.clone(), ret_type.clone());

            let mut body_env = env.clone();
            for (param_name, param_type) in &params {
                body_env.bind(param_name.clone(), TypeScheme::mono(param_type.clone()));
            }

            if let Some((y, s, r)) = gen_params {
                body_env.set_generator_params(y, s, r);
            }

            body_env.set_expected_return_type(ret_type.clone());

            for (i, stmt) in body.iter().enumerate() {
                if i == 0 && is_docstring(stmt) {
                    continue;
                }
                visit_node_with_context(stmt, &mut body_env, ctx, stub_cache, ExprContext::Void)?;
            }

            let mut decorated_type = fn_type.clone();
            for decorator in decorators.iter().rev() {
                let decorator_ty = env.lookup(decorator).unwrap_or_else(|| Type::Var(env.fresh_var()));
                let result_ty = Type::Var(env.fresh_var());

                let span = Span::with_end(*line, *col, *end_line, *end_col);
                ctx.constraints.push(Constraint::Call(
                    decorator_ty,
                    vec![(decorated_type, span)],
                    vec![],
                    result_ty.clone(),
                    span,
                ));

                decorated_type = result_ty;
            }

            env.bind(name.clone(), TypeScheme::mono(decorated_type.clone()));

            ctx.record_type(*line, *col, decorated_type.clone());
            Ok(decorated_type)
        }

        AstNode::ClassDef { name, body, decorators, bases, line, col, end_line, end_col, .. } => {
            let class_type = Type::Con(TypeCtor::Class(name.clone()));
            let mut metadata = extract_class_metadata(name, body, env);

            for base in bases {
                metadata.add_base_class(base.clone());
            }

            let has_dataclass = is_dataclass_decorator(decorators);
            let has_enum_base = has_enum_base(bases);

            if has_dataclass {
                synthesize_dataclass_init(&mut metadata, env);
            }

            if has_enum_base {
                extract_enum_members(&mut metadata, body, name, env);
            }

            ctx.class_registry.register_class(name.clone(), metadata);

            for (i, stmt) in body.iter().enumerate() {
                if i == 0 && is_docstring(stmt) {
                    continue;
                }
                if !matches!(stmt, AstNode::FunctionDef { .. }) {
                    visit_node_with_env(stmt, env, ctx, stub_cache)?;
                }
            }

            let type_transforming_decorators: Vec<&String> =
                decorators.iter().filter(|d| !is_special_class_decorator(d)).collect();

            let mut decorated_type = class_type.clone();
            for decorator in type_transforming_decorators.iter().rev() {
                let decorator_ty = env.lookup(decorator).unwrap_or_else(|| Type::Var(env.fresh_var()));
                let result_ty = Type::Var(env.fresh_var());
                let span = Span::with_end(*line, *col, *end_line, *end_col);

                ctx.constraints.push(Constraint::Call(
                    decorator_ty,
                    vec![(decorated_type, span)],
                    vec![],
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
        AstNode::Assignment { target, value, line, col, .. } => {
            let value_ty = visit_node_with_env(value, env, ctx, stub_cache)?;
            for name in target.extract_target_names() {
                env.bind(name, TypeScheme::mono(value_ty.clone()));
            }
            ctx.record_type(*line, *col, value_ty.clone());
            Ok(value_ty)
        }
        AstNode::AnnotatedAssignment { target, type_annotation, value, line, col, end_line, end_col, .. } => {
            let annotated_ty = env.parse_annotation_or_any(type_annotation);
            if let Some(val) = value {
                let value_ty = visit_node_with_env(val, env, ctx, stub_cache)?;
                let span = Span::with_end(*line, *col, *end_line, *end_col);
                ctx.constraints
                    .push(Constraint::Equal(value_ty, annotated_ty.clone(), span));
            }
            for name in target.extract_target_names() {
                env.bind(name, TypeScheme::mono(annotated_ty.clone()));
            }
            ctx.record_type(*line, *col, annotated_ty.clone());
            Ok(annotated_ty)
        }
        AstNode::Call { function, args, keywords, line, col, end_line, end_col, .. } => {
            let func_ty = if function.contains('.') {
                if let Some(last_dot_idx) = function.rfind('.') {
                    let (object_part, method_part) = function.split_at(last_dot_idx);
                    let method_name = &method_part[1..];
                    let obj_ty = env.lookup(object_part).unwrap_or_else(|| Type::Var(env.fresh_var()));
                    let method_ty = Type::Var(env.fresh_var());
                    let span = Span::with_end(*line, *col, *end_line, *end_col);

                    ctx.constraints.push(Constraint::HasAttr(
                        obj_ty,
                        method_name.to_string(),
                        method_ty.clone(),
                        span,
                    ));

                    method_ty
                } else {
                    env.lookup(function).unwrap_or_else(|| Type::Var(env.fresh_var()))
                }
            } else {
                env.lookup(function).unwrap_or_else(|| Type::Var(env.fresh_var()))
            };

            let mut positional_arg_types = Vec::new();
            for arg in args {
                let arg_ty = visit_node_with_env(arg, env, ctx, stub_cache)?;
                let (arg_line, arg_col, arg_end_line, arg_end_col) = get_node_position(arg);
                let arg_span = Span::with_end(arg_line, arg_col, arg_end_line, arg_end_col);
                positional_arg_types.push((arg_ty, arg_span));
            }

            let mut keyword_arg_types = Vec::new();
            for (name, value) in keywords {
                let kw_ty = visit_node_with_env(value, env, ctx, stub_cache)?;
                let (kw_line, kw_col, kw_end_line, kw_end_col) = get_node_position(value);
                let kw_span = Span::with_end(kw_line, kw_col, kw_end_line, kw_end_col);
                keyword_arg_types.push((name.clone(), kw_ty, kw_span));
            }

            let ret_ty = Type::Var(env.fresh_var());
            let span = Span::with_end(*line, *col, *end_line, *end_col);
            ctx.constraints.push(Constraint::Call(
                func_ty,
                positional_arg_types,
                keyword_arg_types,
                ret_ty.clone(),
                span,
            ));

            ctx.record_type(*line, *col, ret_ty.clone());
            Ok(ret_ty)
        }
        AstNode::Identifier { name, line, col, .. } => {
            let ty = env.lookup(name).unwrap_or_else(|| Type::Var(env.fresh_var()));
            ctx.record_type(*line, *col, ty.clone());
            Ok(ty)
        }
        AstNode::Literal { value, line, col, .. } => {
            let ty = match value {
                LiteralValue::Integer(_) => Type::int(),
                LiteralValue::Float(_) => Type::float(),
                LiteralValue::String { .. } => Type::string(),
                LiteralValue::Boolean(_) => Type::bool(),
                LiteralValue::None => Type::none(),
            };
            ctx.record_type(*line, *col, ty.clone());
            Ok(ty)
        }
        AstNode::Return { value, line, col, end_line, end_col, .. } => {
            let ty = if let Some(val) = value {
                visit_node_with_context(val, env, ctx, stub_cache, ExprContext::Value)?
            } else {
                Type::none()
            };

            if let Some(expected_ret_ty) = env.get_expected_return_type() {
                let span = Span::with_end(*line, *col, *end_line, *end_col);
                ctx.constraints
                    .push(Constraint::Equal(ty.clone(), expected_ret_ty.clone(), span));
            }

            ctx.record_type(*line, *col, ty.clone());
            Ok(ty)
        }
        AstNode::Yield { value, line, col, end_line, end_col, .. } => {
            let yielded_ty =
                if let Some(val) = value { visit_node_with_env(val, env, ctx, stub_cache)? } else { Type::none() };

            let result_ty = if let Some((yield_var, send_var, _return_var)) = env.get_generator_params() {
                let span = Span::with_end(*line, *col, *end_line, *end_col);
                ctx.constraints
                    .push(Constraint::Equal(yielded_ty.clone(), yield_var.clone(), span));

                send_var.clone()
            } else {
                yielded_ty.clone()
            };

            ctx.record_type(*line, *col, result_ty.clone());
            Ok(result_ty)
        }
        AstNode::YieldFrom { value, line, col, end_line, end_col, .. } => {
            let subgen_ty = visit_node_with_env(value, env, ctx, stub_cache)?;
            let span = Span::with_end(*line, *col, *end_line, *end_col);

            let result_ty = if let Some((sub_yield, sub_send, sub_return)) = subgen_ty.extract_generator_params() {
                if let Some((yield_var, send_var, _return_var)) = env.get_generator_params() {
                    ctx.constraints
                        .push(Constraint::Equal(yield_var.clone(), sub_yield.clone(), span));
                    ctx.constraints
                        .push(Constraint::Equal(send_var.clone(), sub_send.clone(), span));
                }

                sub_return.clone()
            } else if let Some(elem_ty) = subgen_ty.extract_iterator_elem() {
                if let Some((yield_var, _send_var, _return_var)) = env.get_generator_params() {
                    ctx.constraints
                        .push(Constraint::Equal(yield_var.clone(), elem_ty.clone(), span));
                }

                Type::none()
            } else {
                Type::Var(env.fresh_var())
            };

            ctx.record_type(*line, *col, result_ty.clone());
            Ok(result_ty)
        }
        AstNode::Await { value, line, col, end_col, end_line } => {
            let awaitable_ty = visit_node_with_env(value, env, ctx, stub_cache)?;
            let span = Span::with_end(*line, *col, *end_line, *end_col);

            let result_ty = if let Some((_y, _s, r)) = awaitable_ty.extract_coroutine_params() {
                r.clone()
            } else if matches!(awaitable_ty, Type::Var(_)) {
                let result_var = Type::Var(env.fresh_var());
                let y_var = Type::Var(env.fresh_var());
                let s_var = Type::Var(env.fresh_var());
                let expected_coro_ty = Type::coroutine(y_var, s_var, result_var.clone());
                ctx.constraints
                    .push(Constraint::Equal(awaitable_ty.clone(), expected_coro_ty, span));
                result_var
            } else {
                let result_var = Type::Var(env.fresh_var());
                ctx.constraints.push(Constraint::Protocol(
                    awaitable_ty.clone(),
                    beacon_core::ProtocolName::Awaitable,
                    result_var.clone(),
                    span,
                ));
                result_var
            };

            ctx.record_type(*line, *col, result_ty.clone());
            Ok(result_ty)
        }
        AstNode::Attribute { object, attribute, line, col, end_line, end_col, .. } => {
            let obj_ty = visit_node_with_env(object, env, ctx, stub_cache)?;
            let attr_ty = Type::Var(env.fresh_var());
            let span = Span::with_end(*line, *col, *end_line, *end_col);
            ctx.constraints
                .push(Constraint::HasAttr(obj_ty, attribute.clone(), attr_ty.clone(), span));
            ctx.record_type(*line, *col, attr_ty.clone());
            Ok(attr_ty)
        }
        AstNode::BinaryOp { left, right, line, col, end_col, end_line, .. } => {
            let left_ty = visit_node_with_env(left, env, ctx, stub_cache)?;
            let right_ty = visit_node_with_env(right, env, ctx, stub_cache)?;
            let span = Span::with_end(*line, *col, *end_line, *end_col);
            ctx.constraints.push(Constraint::Equal(left_ty.clone(), right_ty, span));
            ctx.record_type(*line, *col, left_ty.clone());
            Ok(left_ty)
        }
        AstNode::UnaryOp { operand, line, col, .. } => {
            let ty = visit_node_with_env(operand, env, ctx, stub_cache)?;
            ctx.record_type(*line, *col, ty.clone());
            Ok(ty)
        }
        AstNode::Subscript { value, slice, line, col, end_line, end_col, .. } => {
            let value_ty = visit_node_with_env(value, env, ctx, stub_cache)?;
            let slice_ty = visit_node_with_env(slice, env, ctx, stub_cache)?;
            let span = Span::with_end(*line, *col, *end_line, *end_col);

            let getitem_method_ty = Type::Var(env.fresh_var());
            ctx.constraints.push(Constraint::HasAttr(
                value_ty,
                "__getitem__".to_string(),
                getitem_method_ty.clone(),
                span,
            ));

            let result_ty = Type::Var(env.fresh_var());
            ctx.constraints.push(Constraint::Call(
                getitem_method_ty,
                vec![(slice_ty, span)],
                vec![],
                result_ty.clone(),
                span,
            ));

            ctx.record_type(*line, *col, result_ty.clone());
            Ok(result_ty)
        }
        AstNode::If { test, body, elif_parts, else_body, line, col, end_col, end_line } => {
            visit_node_with_context(test, env, ctx, stub_cache, ExprContext::Value)?;

            let is_main = is_main_guard(test);
            let body_context = if is_main { ExprContext::Void } else { expr_ctx };

            let predicate = extract_type_predicate(test, env);
            let inverse_predicate = predicate.as_ref().map(|p| p.negate());

            let (narrowed_var, narrowed_type) = detect_type_guard(test, &mut env.clone());
            let mut true_env = env.clone();

            if let (Some(var_name), Some(_)) = (narrowed_var.as_ref(), narrowed_type.as_ref()) {
                if let Some(original_type) = env.lookup(var_name) {
                    ctx.control_flow.start_tracking(var_name.clone(), original_type);
                }
            }

            if let (Some(var_name), Some(refined_ty)) = (narrowed_var.as_ref(), narrowed_type.as_ref()) {
                true_env.bind(var_name.clone(), TypeScheme::mono(refined_ty.clone()));

                if let Some(pred) = &predicate {
                    let span = Span::with_end(*line, *col, *end_line, *end_col);
                    ctx.constraints.push(Constraint::Narrowing(
                        var_name.clone(),
                        pred.clone(),
                        refined_ty.clone(),
                        span,
                    ));

                    if let Some(original_type) = env.lookup(var_name) {
                        let eliminated = pred.eliminated_types(&original_type);
                        for elim_ty in eliminated {
                            ctx.control_flow.eliminate_type(var_name, elim_ty);
                        }
                    }
                }
            }

            for stmt in body {
                visit_node_with_context(stmt, &mut true_env, ctx, stub_cache, body_context)?;
            }

            let (inverse_var, inverse_type) = detect_inverse_type_guard(test, &mut env.clone());
            let mut elif_env = env.clone();

            if let Some(var_name) = inverse_var {
                let fallback = ctx.control_flow.get_remaining_types(&var_name);
                if let Some(refined_ty) = inverse_type.clone().or(fallback) {
                    elif_env.bind(var_name.clone(), TypeScheme::mono(refined_ty.clone()));

                    if let Some(pred) = &predicate {
                        if pred.has_simple_negation() {
                            if let Some(inv_pred) = &inverse_predicate {
                                let span = Span::with_end(*line, *col, *end_line, *end_col);
                                ctx.constraints.push(Constraint::Narrowing(
                                    var_name.clone(),
                                    inv_pred.clone(),
                                    refined_ty.clone(),
                                    span,
                                ));

                                if let Some(original_type) = env.lookup(&var_name) {
                                    let eliminated = inv_pred.eliminated_types(&original_type);
                                    for elim_ty in eliminated {
                                        ctx.control_flow.eliminate_type(&var_name, elim_ty);
                                    }
                                }
                            }
                        }
                    }
                }
            }

            for (elif_test, elif_body) in elif_parts {
                visit_node_with_context(elif_test, &mut elif_env, ctx, stub_cache, ExprContext::Value)?;

                let elif_predicate = extract_type_predicate(elif_test, env);
                let (elif_narrowed_var, elif_narrowed_type) = detect_type_guard(elif_test, &mut elif_env.clone());
                let mut elif_true_env = elif_env.clone();

                if let (Some(var_name), Some(refined_ty)) = (elif_narrowed_var.as_ref(), elif_narrowed_type.as_ref()) {
                    elif_true_env.bind(var_name.clone(), TypeScheme::mono(refined_ty.clone()));

                    if let Some(pred) = &elif_predicate {
                        if let Some(original_type) = env.lookup(var_name) {
                            let eliminated = pred.eliminated_types(&original_type);
                            for elim_ty in eliminated {
                                ctx.control_flow.eliminate_type(var_name, elim_ty);
                            }
                        }
                    }
                }

                for stmt in elif_body {
                    visit_node_with_context(stmt, &mut elif_true_env, ctx, stub_cache, expr_ctx)?;
                }

                let (elif_inverse_var, elif_inverse_type) = detect_inverse_type_guard(elif_test, &mut elif_env.clone());
                if let Some(var_name) = elif_inverse_var {
                    let fallback = ctx.control_flow.get_remaining_types(&var_name);
                    if let Some(refined_ty) = elif_inverse_type.or(fallback) {
                        elif_env.bind(var_name, TypeScheme::mono(refined_ty));
                    }
                }
            }

            if let Some(else_stmts) = else_body {
                for stmt in else_stmts {
                    visit_node_with_context(stmt, &mut elif_env, ctx, stub_cache, expr_ctx)?;
                }
            }

            env.merge_from_conditional(&true_env, &elif_env);

            if let Some(var_name) = narrowed_var.as_ref() {
                if let Some(existing_scheme) = env.get_scheme(var_name) {
                    if let Some(refined_ty) = ctx.control_flow.apply_remaining_types(var_name, &existing_scheme.ty) {
                        env.bind(var_name.clone(), TypeScheme::mono(refined_ty));
                    }
                }
                ctx.control_flow.stop_tracking(var_name);
            }

            Ok(Type::none())
        }
        AstNode::Import { module, alias, line, col, .. } => {
            let module_name = alias.as_ref().unwrap_or(module);
            let module_type = Type::Con(TypeCtor::Module(module.clone()));
            if let Some(cache_arc) = stub_cache {
                if let Ok(cache) = cache_arc.read() {
                    if let Some(stub) = cache.get(module) {
                        if !ctx.loaded_stub_modules.contains(module) {
                            loader::load_stub_into_registry(stub, &mut ctx.class_registry)?;
                            ctx.loaded_stub_modules.insert(module.clone());
                        }
                    }
                }
            }

            env.bind(module_name.clone(), TypeScheme::mono(module_type.clone()));
            ctx.record_type(*line, *col, module_type.clone());
            Ok(module_type)
        }
        AstNode::ImportFrom { module, names, line, col, .. } => {
            if let Some(cache_arc) = stub_cache {
                if let Ok(cache) = cache_arc.read() {
                    if let Some(stub) = cache.get(module) {
                        if !ctx.loaded_stub_modules.contains(module) {
                            loader::load_stub_into_registry(stub, &mut ctx.class_registry)?;
                            ctx.loaded_stub_modules.insert(module.clone());
                        }
                    }
                    for name in names {
                        let ty = cache
                            .get(module)
                            .and_then(|stub| stub.exports.get(name).cloned())
                            .unwrap_or_else(|| Type::Var(env.fresh_var()));
                        env.bind(name.clone(), TypeScheme::mono(ty));
                    }
                } else {
                    for name in names {
                        let ty = Type::Var(env.fresh_var());
                        env.bind(name.clone(), TypeScheme::mono(ty));
                    }
                }
            } else {
                for name in names {
                    let ty = Type::Var(env.fresh_var());
                    env.bind(name.clone(), TypeScheme::mono(ty));
                }
            }
            let module_type = Type::Con(TypeCtor::Module(module.clone()));
            ctx.record_type(*line, *col, module_type.clone());
            Ok(module_type)
        }
        AstNode::For { target, iter, body, else_body, is_async, line, col, end_col, end_line } => {
            let iter_ty = visit_node_with_env(iter, env, ctx, stub_cache)?;
            let element_ty = Type::Var(env.fresh_var());
            let span = Span::with_end(*line, *col, *end_line, *end_col);

            let protocol = if *is_async {
                beacon_core::ProtocolName::AsyncIterable
            } else {
                beacon_core::ProtocolName::Iterable
            };

            ctx.constraints
                .push(Constraint::Protocol(iter_ty, protocol, element_ty.clone(), span));

            env.bind(target.clone(), TypeScheme::mono(element_ty));

            for stmt in body {
                visit_node_with_env(stmt, env, ctx, stub_cache)?;
            }

            if let Some(else_stmts) = else_body {
                for stmt in else_stmts {
                    visit_node_with_env(stmt, env, ctx, stub_cache)?;
                }
            }

            ctx.record_type(*line, *col, Type::none());
            Ok(Type::none())
        }

        AstNode::While { test, body, else_body, line, col, end_col, end_line } => {
            visit_node_with_context(test, env, ctx, stub_cache, ExprContext::Value)?;

            let predicate = extract_type_predicate(test, env);
            let inverse_predicate = predicate.as_ref().map(|p| p.negate());

            let (narrowed_var, narrowed_type) = detect_type_guard(test, &mut env.clone());
            let mut loop_env = env.clone();

            if let (Some(var_name), Some(refined_ty)) = (narrowed_var.as_ref(), narrowed_type.as_ref()) {
                loop_env.bind(var_name.clone(), TypeScheme::mono(refined_ty.clone()));

                if let Some(pred) = &predicate {
                    let span = Span::with_end(*line, *col, *end_line, *end_col);
                    ctx.constraints.push(Constraint::Narrowing(
                        var_name.clone(),
                        pred.clone(),
                        refined_ty.clone(),
                        span,
                    ));
                }
            }

            for stmt in body {
                visit_node_with_context(stmt, &mut loop_env, ctx, stub_cache, ExprContext::Void)?;
            }

            if let Some(else_stmts) = else_body {
                let (inverse_var, inverse_type) = detect_inverse_type_guard(test, &mut env.clone());
                let mut else_env = env.clone();

                if let (Some(var_name), Some(refined_ty)) = (inverse_var.as_ref(), inverse_type.as_ref()) {
                    else_env.bind(var_name.clone(), TypeScheme::mono(refined_ty.clone()));

                    if let Some(pred) = &predicate {
                        if pred.has_simple_negation() {
                            if let Some(inv_pred) = &inverse_predicate {
                                let span = Span::with_end(*line, *col, *end_line, *end_col);
                                ctx.constraints.push(Constraint::Narrowing(
                                    var_name.clone(),
                                    inv_pred.clone(),
                                    refined_ty.clone(),
                                    span,
                                ));
                            }
                        }
                    }
                }

                for stmt in else_stmts {
                    visit_node_with_context(stmt, &mut else_env, ctx, stub_cache, ExprContext::Void)?;
                }
            }

            ctx.record_type(*line, *col, Type::none());
            Ok(Type::none())
        }

        AstNode::Try { body, handlers, else_body, finally_body, line, col, end_col, end_line } => {
            for stmt in body {
                visit_node_with_context(stmt, env, ctx, stub_cache, ExprContext::Void)?;
            }

            for handler in handlers {
                let mut handler_env = env.clone();
                if let Some(ref name) = handler.name {
                    let exc_ty = if let Some(ref exc_type_name) = handler.exception_type {
                        type_name_to_type(exc_type_name)
                    } else {
                        Type::Var(env.fresh_var())
                    };
                    handler_env.bind(name.clone(), TypeScheme::mono(exc_ty.clone()));

                    let span = Span::with_end(*line, *col, *end_line, *end_col);
                    ctx.constraints.push(Constraint::Narrowing(
                        name.clone(),
                        TypePredicate::IsInstance(exc_ty.clone()),
                        exc_ty,
                        span,
                    ));
                }

                for stmt in &handler.body {
                    visit_node_with_context(stmt, &mut handler_env, ctx, stub_cache, ExprContext::Void)?;
                }
            }

            if let Some(else_stmts) = else_body {
                for stmt in else_stmts {
                    visit_node_with_context(stmt, env, ctx, stub_cache, ExprContext::Void)?;
                }
            }

            if let Some(finally_stmts) = finally_body {
                for stmt in finally_stmts {
                    visit_node_with_context(stmt, env, ctx, stub_cache, ExprContext::Void)?;
                }
            }

            ctx.record_type(*line, *col, Type::none());
            Ok(Type::none())
        }

        AstNode::Match { subject, cases, line, col, end_line, end_col, .. } => {
            let subject_ty = visit_node_with_env(subject, env, ctx, stub_cache)?;
            let all_patterns: Vec<beacon_parser::Pattern> = cases.iter().map(|c| c.pattern.clone()).collect();
            let span = Span::with_end(*line, *col, *end_line, *end_col);

            ctx.constraints.push(Constraint::PatternExhaustive(
                subject_ty.clone(),
                all_patterns.clone(),
                span,
            ));

            let subject_var =
                if let AstNode::Identifier { name, .. } = subject.as_ref() { Some(name.clone()) } else { None };

            let mut previous_patterns = Vec::new();
            for case in cases {
                ctx.constraints.push(Constraint::PatternReachable(
                    case.pattern.clone(),
                    previous_patterns.clone(),
                    span,
                ));

                ctx.constraints.push(Constraint::PatternTypeCompatible(
                    case.pattern.clone(),
                    subject_ty.clone(),
                    span,
                ));

                ctx.constraints.push(Constraint::PatternStructureValid(
                    case.pattern.clone(),
                    subject_ty.clone(),
                    span,
                ));

                let mut case_env = env.clone();
                let bindings =
                    extract_pattern_bindings(&case.pattern, &subject_ty, &mut case_env, &ctx.class_registry)?;

                ctx.constraints.push(Constraint::MatchPattern(
                    subject_ty.clone(),
                    case.pattern.clone(),
                    bindings.clone(),
                    span,
                ));

                let pattern_predicate = TypePredicate::MatchesPattern(case.pattern.clone());
                ctx.control_flow.push_scope(Some(pattern_predicate.clone()));

                if let Some(ref var_name) = subject_var {
                    let narrowed_type = pattern_predicate.apply(&subject_ty);

                    ctx.constraints.push(Constraint::Narrowing(
                        var_name.clone(),
                        pattern_predicate.clone(),
                        narrowed_type.clone(),
                        span,
                    ));

                    ctx.control_flow.narrow(var_name.clone(), narrowed_type.clone());
                    case_env.bind(var_name.clone(), beacon_core::TypeScheme::mono(narrowed_type));
                }

                for (var_name, var_type) in bindings {
                    case_env.bind(var_name.clone(), beacon_core::TypeScheme::mono(var_type.clone()));

                    ctx.control_flow.narrow(var_name, var_type);
                }

                if let Some(ref guard) = case.guard {
                    visit_node_with_env(guard, &mut case_env, ctx, stub_cache)?;
                }

                for stmt in &case.body {
                    visit_node_with_env(stmt, &mut case_env, ctx, stub_cache)?;
                }

                ctx.control_flow.pop_scope();

                previous_patterns.push(case.pattern.clone());
            }

            ctx.record_type(*line, *col, Type::none());
            Ok(Type::none())
        }
        AstNode::Raise { exc, line, col, .. } => {
            if let Some(exception) = exc {
                visit_node_with_env(exception, env, ctx, stub_cache)?;
            }

            ctx.record_type(*line, *col, Type::never());
            Ok(Type::never())
        }

        AstNode::With { items, body, is_async, line, col, end_col, end_line } => {
            let (enter_method, exit_method) =
                if *is_async { ("__aenter__", "__aexit__") } else { ("__enter__", "__exit__") };

            for item in items {
                let context_ty = visit_node_with_context(&item.context_expr, env, ctx, stub_cache, ExprContext::Value)?;

                let enter_ty = Type::Var(env.fresh_var());
                let span = Span::with_end(*line, *col, *end_line, *end_col);
                ctx.constraints.push(Constraint::HasAttr(
                    context_ty.clone(),
                    enter_method.to_string(),
                    enter_ty.clone(),
                    span,
                ));

                let exit_ty = Type::Var(env.fresh_var());
                ctx.constraints
                    .push(Constraint::HasAttr(context_ty, exit_method.to_string(), exit_ty, span));

                if let Some(ref target) = item.optional_vars {
                    env.bind(target.clone(), TypeScheme::mono(enter_ty.clone()));

                    ctx.constraints.push(Constraint::Narrowing(
                        target.clone(),
                        TypePredicate::IsInstance(enter_ty.clone()),
                        enter_ty,
                        span,
                    ));
                }
            }

            for stmt in body {
                visit_node_with_context(stmt, env, ctx, stub_cache, ExprContext::Void)?;
            }

            ctx.record_type(*line, *col, Type::none());
            Ok(Type::none())
        }
        AstNode::Lambda { args, body, line, col, .. } => {
            let params: Vec<(String, Type)> = args
                .iter()
                .enumerate()
                .map(|(i, param)| {
                    let param_type = param
                        .type_annotation
                        .as_ref()
                        .map(|ann| env.parse_annotation_or_any(ann))
                        .unwrap_or_else(|| Type::Var(env.fresh_var()));
                    let param_name = if param.name.is_empty() { format!("_{i}") } else { param.name.clone() };
                    (param_name, param_type)
                })
                .collect();

            let mut lambda_env = env.clone();
            for (param_name, param_type) in &params {
                lambda_env.bind(param_name.clone(), TypeScheme::mono(param_type.clone()));
            }

            let body_ty = visit_node_with_env(body, &mut lambda_env, ctx, stub_cache)?;
            let lambda_ty = Type::fun(params, body_ty);

            ctx.record_type(*line, *col, lambda_ty.clone());
            Ok(lambda_ty)
        }
        AstNode::Compare { left, comparators, line, col, .. } => {
            visit_node_with_env(left, env, ctx, stub_cache)?;

            for comp in comparators {
                visit_node_with_env(comp, env, ctx, stub_cache)?;
            }

            ctx.record_type(*line, *col, Type::bool());
            Ok(Type::bool())
        }
        AstNode::NamedExpr { target, value, line, col, .. } => {
            let value_ty = visit_node_with_env(value, env, ctx, stub_cache)?;
            env.bind(target.clone(), TypeScheme::mono(value_ty.clone()));
            ctx.record_type(*line, *col, value_ty.clone());
            Ok(value_ty)
        }
        AstNode::ListComp { element, generators, line, col, end_col, end_line } => {
            let mut comp_env = env.clone();

            for generator in generators {
                let iter_ty = visit_node_with_env(&generator.iter, &mut comp_env, ctx, stub_cache)?;

                let element_ty = Type::Var(comp_env.fresh_var());
                let span = Span::with_end(*line, *col, *end_line, *end_col);
                ctx.constraints.push(Constraint::Protocol(
                    iter_ty,
                    beacon_core::ProtocolName::Iterable,
                    element_ty.clone(),
                    span,
                ));

                comp_env.bind(generator.target.clone(), TypeScheme::mono(element_ty));

                for if_clause in &generator.ifs {
                    visit_node_with_env(if_clause, &mut comp_env, ctx, stub_cache)?;
                }
            }

            let elem_ty = visit_node_with_env(element, &mut comp_env, ctx, stub_cache)?;
            let list_ty = Type::list(elem_ty);

            ctx.record_type(*line, *col, list_ty.clone());
            Ok(list_ty)
        }
        AstNode::SetComp { element, generators, line, col, end_col, end_line } => {
            let mut comp_env = env.clone();

            for generator in generators {
                let iter_ty = visit_node_with_env(&generator.iter, &mut comp_env, ctx, stub_cache)?;

                let element_ty = Type::Var(comp_env.fresh_var());
                let span = Span::with_end(*line, *col, *end_line, *end_col);
                ctx.constraints.push(Constraint::Protocol(
                    iter_ty,
                    beacon_core::ProtocolName::Iterable,
                    element_ty.clone(),
                    span,
                ));

                comp_env.bind(generator.target.clone(), TypeScheme::mono(element_ty));

                for if_clause in &generator.ifs {
                    visit_node_with_env(if_clause, &mut comp_env, ctx, stub_cache)?;
                }
            }

            let elem_ty = visit_node_with_env(element, &mut comp_env, ctx, stub_cache)?;
            let set_ty = Type::App(Box::new(Type::Con(TypeCtor::Set)), Box::new(elem_ty));

            ctx.record_type(*line, *col, set_ty.clone());
            Ok(set_ty)
        }
        AstNode::DictComp { key, value, generators, line, col, end_col, end_line } => {
            let mut comp_env = env.clone();

            for generator in generators {
                let iter_ty = visit_node_with_env(&generator.iter, &mut comp_env, ctx, stub_cache)?;

                let element_ty = Type::Var(comp_env.fresh_var());
                let span = Span::with_end(*line, *col, *end_line, *end_col);
                ctx.constraints.push(Constraint::Protocol(
                    iter_ty,
                    beacon_core::ProtocolName::Iterable,
                    element_ty.clone(),
                    span,
                ));

                comp_env.bind(generator.target.clone(), TypeScheme::mono(element_ty));

                for if_clause in &generator.ifs {
                    visit_node_with_env(if_clause, &mut comp_env, ctx, stub_cache)?;
                }
            }

            let key_ty = visit_node_with_env(key, &mut comp_env, ctx, stub_cache)?;
            let val_ty = visit_node_with_env(value, &mut comp_env, ctx, stub_cache)?;
            let dict_ty = Type::dict(key_ty, val_ty);

            ctx.record_type(*line, *col, dict_ty.clone());
            Ok(dict_ty)
        }
        AstNode::GeneratorExp { element, generators, line, col, end_col, end_line } => {
            let mut comp_env = env.clone();

            for generator in generators {
                let iter_ty = visit_node_with_env(&generator.iter, &mut comp_env, ctx, stub_cache)?;

                let element_ty = Type::Var(comp_env.fresh_var());
                let span = Span::with_end(*line, *col, *end_line, *end_col);
                ctx.constraints.push(Constraint::Protocol(
                    iter_ty,
                    beacon_core::ProtocolName::Iterable,
                    element_ty.clone(),
                    span,
                ));

                comp_env.bind(generator.target.clone(), TypeScheme::mono(element_ty));

                for if_clause in &generator.ifs {
                    visit_node_with_env(if_clause, &mut comp_env, ctx, stub_cache)?;
                }
            }

            let elem_ty = visit_node_with_env(element, &mut comp_env, ctx, stub_cache)?;
            let generator_ty = Type::generator(elem_ty, Type::none(), Type::none());

            ctx.record_type(*line, *col, generator_ty.clone());
            Ok(generator_ty)
        }
        AstNode::Tuple { elements, line, col, .. } => {
            if elements.is_empty() {
                let elem_ty = Type::Var(env.fresh_var());
                let tuple_ty = Type::tuple(elem_ty);
                ctx.record_type(*line, *col, tuple_ty.clone());
                Ok(tuple_ty)
            } else {
                let element_types = elements
                    .iter()
                    .map(|elem| visit_node_with_env(elem, env, ctx, stub_cache))
                    .collect::<std::result::Result<Vec<Type>, _>>()?;

                let tuple_ty = if element_types.len() == 1 {
                    Type::tuple(element_types[0].clone())
                } else {
                    Type::tuple_heterogeneous(element_types)
                };

                ctx.record_type(*line, *col, tuple_ty.clone());
                Ok(tuple_ty)
            }
        }
        AstNode::List { elements, line, col, end_line, end_col, .. } => {
            let span = Span::with_end(*line, *col, *end_line, *end_col);

            if elements.is_empty() {
                let elem_ty = Type::Var(env.fresh_var());
                let list_ty = Type::list(elem_ty);
                ctx.record_type(*line, *col, list_ty.clone());
                Ok(list_ty)
            } else {
                let element_types = elements
                    .iter()
                    .map(|elem| visit_node_with_env(elem, env, ctx, stub_cache))
                    .collect::<std::result::Result<Vec<Type>, _>>()?;

                let unified_ty = Type::Var(env.fresh_var());
                for elem_ty in &element_types {
                    ctx.constraints
                        .push(Constraint::Equal(elem_ty.clone(), unified_ty.clone(), span));
                }

                let list_ty = Type::list(unified_ty);
                ctx.record_type(*line, *col, list_ty.clone());
                Ok(list_ty)
            }
        }
        AstNode::Dict { keys, values, line, col, end_line, end_col, .. } => {
            let span = Span::with_end(*line, *col, *end_line, *end_col);

            if keys.is_empty() {
                let key_ty = Type::Var(env.fresh_var());
                let value_ty = Type::Var(env.fresh_var());
                let dict_ty = Type::dict(key_ty, value_ty);
                ctx.record_type(*line, *col, dict_ty.clone());
                Ok(dict_ty)
            } else {
                let key_types = keys
                    .iter()
                    .map(|key| visit_node_with_env(key, env, ctx, stub_cache))
                    .collect::<std::result::Result<Vec<Type>, _>>()?;

                let value_types = values
                    .iter()
                    .map(|value| visit_node_with_env(value, env, ctx, stub_cache))
                    .collect::<std::result::Result<Vec<Type>, _>>()?;

                let unified_key_ty = Type::Var(env.fresh_var());
                let unified_value_ty = Type::Var(env.fresh_var());

                for key_ty in &key_types {
                    ctx.constraints
                        .push(Constraint::Equal(key_ty.clone(), unified_key_ty.clone(), span));
                }
                for value_ty in &value_types {
                    ctx.constraints
                        .push(Constraint::Equal(value_ty.clone(), unified_value_ty.clone(), span));
                }

                let dict_ty = Type::dict(unified_key_ty, unified_value_ty);
                ctx.record_type(*line, *col, dict_ty.clone());
                Ok(dict_ty)
            }
        }
        AstNode::Set { elements, line, col, end_col, end_line } => {
            let span = Span::with_end(*line, *col, *end_line, *end_col);

            if elements.is_empty() {
                let elem_ty = Type::Var(env.fresh_var());
                let set_ty = Type::set(elem_ty);
                ctx.record_type(*line, *col, set_ty.clone());
                Ok(set_ty)
            } else {
                let element_types = elements
                    .iter()
                    .map(|elem| visit_node_with_env(elem, env, ctx, stub_cache))
                    .collect::<std::result::Result<Vec<Type>, _>>()?;

                let unified_ty = Type::Var(env.fresh_var());
                for elem_ty in &element_types {
                    ctx.constraints
                        .push(Constraint::Equal(elem_ty.clone(), unified_ty.clone(), span));
                }

                let set_ty = Type::set(unified_ty);
                ctx.record_type(*line, *col, set_ty.clone());
                Ok(set_ty)
            }
        }
        AstNode::Pass { .. } | AstNode::Break { .. } | AstNode::Continue { .. } => Ok(Type::none()),
        AstNode::Assert { .. } | AstNode::Starred { .. } => Ok(Type::none()),
        AstNode::ParenthesizedExpression { expression, .. } => {
            visit_node_with_context(expression, env, ctx, stub_cache, expr_ctx)
        }
    }
}

/// Extract class metadata from a ClassDef node
///
/// Scans the class body for class-level fields and methods to build [ClassMetadata].
/// This metadata is used during [Constraint::HasAttr] constraint solving to resolve attribute types.
///
/// ## Field Extraction Strategy
///
/// This function extracts fields from two sources:
///
/// 1. **Class-level fields**: Annotations and assignments at class scope
///    - `x: int` or `x: int = 0` become class attributes
///    - In Python these are shared across instances, but for type checking we treat them as
///      "objects of this class have attribute x of type int"
///
/// 2. **Instance fields**: Assignments in `__init__` method
///    - `self.field = value` patterns found by recursively scanning `__init__` body
///    - These represent per-instance attributes
///
/// Both types are registered using `add_field` since the distinction matters for Python runtime
/// semantics (class variables are shared) but not for structural type checking (both are attributes
/// the object provides). If a name appears in both places, both get registered and the constraint
/// solver handles shadowing semantics.
///
/// TODO: Classes with multiple methods (beyond just __init__) experience type unification errors during construction.
///
/// The issue appears to be related to how [Type::fun()] constructs function types when processing multiple methods with a shared environment.
/// Type variables or parameter lists may be getting confused between methods.
///
/// TODO: Consider cloning env for each method to isolate type variable generation
fn extract_class_metadata(name: &str, body: &[AstNode], env: &mut TypeEnvironment) -> ClassMetadata {
    let mut metadata = ClassMetadata::new(name.to_string());

    for stmt in body {
        match stmt {
            AstNode::AnnotatedAssignment { target, type_annotation, .. } => {
                let field_type = env.parse_annotation_or_any(type_annotation);
                metadata.add_field(target.target_to_string(), field_type);
            }
            AstNode::Assignment { target, .. } => {
                let field_type = Type::Var(env.fresh_var());
                metadata.add_field(target.target_to_string(), field_type);
            }
            _ => {}
        }
    }

    for stmt in body {
        if let AstNode::FunctionDef { name: method_name, args, return_type, body: method_body, decorators, .. } = stmt {
            let params: Vec<(String, Type)> = args
                .iter()
                .map(|param| {
                    let param_type = param
                        .type_annotation
                        .as_ref()
                        .map(|ann| env.parse_annotation_or_any(ann))
                        .unwrap_or_else(|| Type::Var(env.fresh_var()));
                    (param.name.clone(), param_type)
                })
                .collect();

            let ret_type = return_type
                .as_ref()
                .map(|ann| env.parse_annotation_or_any(ann))
                .unwrap_or_else(|| Type::Var(env.fresh_var()));

            let has_property = decorators.iter().any(|d| d == "property");
            let has_staticmethod = decorators.iter().any(|d| d == "staticmethod");
            let has_classmethod = decorators.iter().any(|d| d == "classmethod");

            if method_name == "__init__" {
                let method_type = Type::fun(params.clone(), ret_type);
                metadata.set_init_type(method_type);

                for body_stmt in method_body {
                    extract_field_assignments(body_stmt, &mut metadata, env);
                }
            } else if method_name == "__new__" {
                metadata.set_new_type(Type::fun(params.clone(), ret_type));
            } else if has_property {
                metadata.add_property(method_name.clone(), ret_type);
            } else if has_staticmethod {
                let static_params: Vec<(String, Type)> =
                    if !params.is_empty() { params.iter().skip(1).cloned().collect() } else { params.clone() };
                metadata.add_staticmethod(method_name.clone(), Type::fun(static_params, ret_type));
            } else if has_classmethod {
                metadata.add_classmethod(method_name.clone(), Type::fun(params.clone(), ret_type));
            } else {
                metadata.add_method(method_name.clone(), Type::fun(params.clone(), ret_type));
            }
        }
    }

    metadata
}

/// Recursively extract field assignments from statement nodes by looking for patterns like
/// `self.field = value` or `self.field: Type = value` and registers the field in [ClassMetadata].
fn extract_field_assignments(stmt: &AstNode, metadata: &mut ClassMetadata, env: &mut TypeEnvironment) {
    match stmt {
        AstNode::Assignment { target, .. } => {
            if let Some(field_name) = target.target_to_string().strip_prefix("self.") {
                let field_type = Type::Var(env.fresh_var());
                metadata.add_field(field_name.to_string(), field_type);
            }
        }
        AstNode::AnnotatedAssignment { target, type_annotation, .. } => {
            if let Some(field_name) = target.target_to_string().strip_prefix("self.") {
                let field_type = env.parse_annotation_or_any(type_annotation);
                metadata.add_field(field_name.to_string(), field_type);
            }
        }
        AstNode::If { body, elif_parts, else_body, .. } => {
            for body_stmt in body {
                extract_field_assignments(body_stmt, metadata, env);
            }
            for (_, elif_body) in elif_parts {
                for body_stmt in elif_body {
                    extract_field_assignments(body_stmt, metadata, env);
                }
            }
            if let Some(else_stmts) = else_body {
                for body_stmt in else_stmts {
                    extract_field_assignments(body_stmt, metadata, env);
                }
            }
        }
        AstNode::For { body, .. } | AstNode::While { body, .. } => {
            for body_stmt in body {
                extract_field_assignments(body_stmt, metadata, env);
            }
        }
        AstNode::Try { body, handlers, else_body, finally_body, .. } => {
            for body_stmt in body {
                extract_field_assignments(body_stmt, metadata, env);
            }
            for handler in handlers {
                for body_stmt in &handler.body {
                    extract_field_assignments(body_stmt, metadata, env);
                }
            }
            if let Some(else_stmts) = else_body {
                for body_stmt in else_stmts {
                    extract_field_assignments(body_stmt, metadata, env);
                }
            }
            if let Some(finally_stmts) = finally_body {
                for body_stmt in finally_stmts {
                    extract_field_assignments(body_stmt, metadata, env);
                }
            }
        }
        AstNode::With { body, .. } => {
            for body_stmt in body {
                extract_field_assignments(body_stmt, metadata, env);
            }
        }
        _ => {}
    }
}

/// Check if a decorator is @dataclass or a variant (e.g., dataclasses.dataclass)
fn is_dataclass_decorator(decorators: &[String]) -> bool {
    decorators.iter().any(|d| d == "dataclass" || d.ends_with(".dataclass"))
}

/// Check if any base class is Enum or a variant (e.g., enum.Enum, IntEnum, StrEnum)
fn has_enum_base(bases: &[String]) -> bool {
    bases.iter().any(|b| {
        b == "Enum"
            || b.ends_with(".Enum")
            || b == "IntEnum"
            || b.ends_with(".IntEnum")
            || b == "StrEnum"
            || b.ends_with(".StrEnum")
            || b == "Flag"
            || b.ends_with(".Flag")
            || b == "IntFlag"
            || b.ends_with(".IntFlag")
    })
}

/// Check if a decorator is a special class decorator that doesn't transform the type
///
/// These decorators modify class behavior but return the same class type:
/// - @dataclass: adds __init__, __repr__, etc., but type remains the same class
/// - @unique: enum decorator that enforces unique values
/// - Other non-type-transforming decorators can be added here
fn is_special_class_decorator(decorator: &str) -> bool {
    decorator == "dataclass"
        || decorator.ends_with(".dataclass")
        || decorator == "unique"
        || decorator.ends_with(".unique")
}

/// Synthesize __init__ method for a @dataclass from class-level fields
///
/// For a dataclass with fields `x: int` and `y: str`, this generates `__init__(self, x: int, y: str) -> None`
fn synthesize_dataclass_init(metadata: &mut ClassMetadata, env: &mut TypeEnvironment) {
    let _ = env;

    if metadata.init_type.is_some() {
        return;
    }

    let mut params = vec![("self".to_string(), Type::any())];

    for (field_name, field_type) in &metadata.fields {
        params.push((field_name.clone(), field_type.clone()));
    }

    let init_type = Type::fun(params, Type::none());
    metadata.set_init_type(init_type);
}

/// Extract enum members from class body and register them as class attributes
///
/// For an Enum class, class-level assignments like `RED = 1` or `BLUE = auto()`
/// are enum members, not regular class fields. Each member has the type of the enum class itself.
///
/// Example:
/// ```python
/// class Color(Enum):
///     RED = 1
///     GREEN = 2
///     BLUE = auto()
/// ```
/// This creates attributes: Color.RED, Color.GREEN, Color.BLUE all of type Color
fn extract_enum_members(metadata: &mut ClassMetadata, body: &[AstNode], class_name: &str, env: &mut TypeEnvironment) {
    let _ = env;
    let enum_type = Type::Con(TypeCtor::Class(class_name.to_string()));

    for stmt in body {
        match stmt {
            AstNode::Assignment { target, .. } => {
                let target_str = target.target_to_string();
                if !target_str.contains('.') && !target_str.starts_with('_') {
                    metadata.add_field(target_str, enum_type.clone());
                }
            }
            AstNode::AnnotatedAssignment { target, .. } => {
                let target_str = target.target_to_string();
                if !target_str.contains('.') && !target_str.starts_with('_') {
                    metadata.add_field(target_str, enum_type.clone());
                }
            }
            _ => {}
        }
    }
}

/// Detect type guard patterns for flow-sensitive type narrowing
///
/// Supported patterns:
/// - `isinstance(x, int)` -> (x, int)
/// - `isinstance(x, str)` -> (x, str)
/// - `isinstance(x, (int, str))` -> (x, Union[int, str])
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

    if let AstNode::Call { function, args, keywords, .. } = test {
        if function == "isinstance" && args.len() == 2 && keywords.is_empty() {
            if let AstNode::Identifier { name: var_name, .. } = &args[0] {
                if let AstNode::Identifier { name: type_name, .. } = &args[1] {
                    let refined_type = type_name_to_type(type_name);
                    return (Some(var_name.clone()), Some(refined_type));
                }

                if let AstNode::Tuple { elements, .. } = &args[1] {
                    let types: Vec<Type> = elements
                        .iter()
                        .filter_map(|elem| {
                            if let AstNode::Identifier { name: type_name, .. } = elem {
                                Some(type_name_to_type(type_name))
                            } else {
                                None
                            }
                        })
                        .collect();
                    if !types.is_empty() {
                        let refined_type =
                            if types.len() == 1 { types.into_iter().next().unwrap() } else { Type::union(types) };
                        return (Some(var_name.clone()), Some(refined_type));
                    }
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

/// Detect inverse type guard for else branches
///
/// Returns the narrowed type for the "false" branch of a conditional.
/// For example:
/// - `if x is not None:` -> else branch has type None
/// - `if isinstance(x, int):` -> else branch removes int from union
/// - `if x:` -> else branch narrows to None (for Optional types)
fn detect_inverse_type_guard(test: &AstNode, env: &mut TypeEnvironment) -> (Option<String>, Option<Type>) {
    if let AstNode::Identifier { name: var_name, .. } = test {
        if let Some(current_type) = env.lookup(var_name) {
            if current_type.is_optional() || matches!(current_type, Type::Union(_)) {
                return (Some(var_name.clone()), Some(Type::none()));
            }
        }
        return (None, None);
    }

    if let AstNode::Call { function, args, keywords, .. } = test {
        if function == "isinstance" && args.len() == 2 && keywords.is_empty() {
            if let AstNode::Identifier { name: var_name, .. } = &args[0] {
                if let Some(current_type) = env.lookup(var_name) {
                    if let AstNode::Identifier { name: type_name, .. } = &args[1] {
                        let checked_type = type_name_to_type(type_name);
                        let narrowed = current_type.remove_from_union(&checked_type);
                        return (Some(var_name.clone()), Some(narrowed));
                    }

                    if let AstNode::Tuple { elements, .. } = &args[1] {
                        let mut result_type = current_type.clone();
                        for elem in elements {
                            if let AstNode::Identifier { name: type_name, .. } = elem {
                                let checked_type = type_name_to_type(type_name);
                                result_type = result_type.remove_from_union(&checked_type);
                            }
                        }
                        return (Some(var_name.clone()), Some(result_type));
                    }
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
                            if let Some(current_type) = env.lookup(var_name) {
                                let narrowed = current_type.remove_from_union(&Type::none());
                                return (Some(var_name.clone()), Some(narrowed));
                            }
                            return (None, None);
                        }

                        beacon_parser::CompareOperator::IsNot | beacon_parser::CompareOperator::NotEq => {
                            return (Some(var_name.clone()), Some(Type::none()));
                        }
                        _ => {}
                    }
                }
            }
        }
    }

    (None, None)
}

/// Extract type guard information from a function's return annotation
///
/// Detects `TypeGuard[T]` or `TypeIs[T]` annotations and extracts the guarded type.
fn extract_type_guard_info(
    return_annotation: &str, _params: &[beacon_parser::Parameter],
) -> Option<beacon_constraint::TypeGuardInfo> {
    let trimmed = return_annotation.trim();

    if trimmed.starts_with("TypeGuard[") || trimmed.starts_with("TypeIs[") {
        let kind = if trimmed.starts_with("TypeGuard[") { TypeGuardKind::TypeGuard } else { TypeGuardKind::TypeIs };
        let prefix = if kind == TypeGuardKind::TypeGuard { "TypeGuard[" } else { "TypeIs[" };

        if let Some(inner) = trimmed.strip_prefix(prefix) {
            if let Some(type_str) = inner.strip_suffix(']') {
                let parser = beacon_core::AnnotationParser::new();
                if let Ok(guarded_type) = parser.parse(type_str) {
                    // TODO: Support guarding specific parameters
                    let param_index = 0;
                    return Some(TypeGuardInfo::new(param_index, guarded_type, kind));
                }
            }
        }
    }

    None
}

/// Extract a type predicate from a test expression for flow-sensitive narrowing
///
/// It supports:
/// - None checks: `x is not None`, `x is None`
/// - isinstance checks: `isinstance(x, Type)`, `isinstance(x, (Type1, Type2))`
/// - Truthiness checks: `if x:`
/// - Negation: `if not x:`
/// - User-defined type guards: `is_str(x)` where `is_str` returns `TypeGuard[str]`
fn extract_type_predicate(test: &AstNode, env: &TypeEnvironment) -> Option<TypePredicate> {
    match test {
        AstNode::Compare { left: _, ops, comparators, .. } if ops.len() == 1 && comparators.len() == 1 => {
            if let AstNode::Literal { value: LiteralValue::None, .. } = &comparators[0] {
                match &ops[0] {
                    beacon_parser::CompareOperator::IsNot | beacon_parser::CompareOperator::NotEq => {
                        return Some(TypePredicate::IsNotNone);
                    }
                    beacon_parser::CompareOperator::Is | beacon_parser::CompareOperator::Eq => {
                        return Some(TypePredicate::IsNone);
                    }
                    _ => {}
                }
            }
            None
        }
        AstNode::Call { function, args, keywords, .. }
            if function == "isinstance" && args.len() == 2 && keywords.is_empty() =>
        {
            let target_type = match &args[1] {
                AstNode::Identifier { name: type_name, .. } => Some(type_name_to_type(type_name)),
                AstNode::Tuple { elements, .. } => {
                    let types: Vec<Type> = elements
                        .iter()
                        .filter_map(|elem| {
                            if let AstNode::Identifier { name: type_name, .. } = elem {
                                Some(type_name_to_type(type_name))
                            } else {
                                None
                            }
                        })
                        .collect();

                    if types.is_empty() {
                        None
                    } else if types.len() == 1 {
                        Some(types.into_iter().next().unwrap())
                    } else {
                        Some(Type::union(types))
                    }
                }
                _ => None,
            }?;

            Some(TypePredicate::IsInstance(target_type))
        }
        AstNode::Call { function, args, .. } if !args.is_empty() => {
            if let Some(guard_info) = env.get_type_guard(function) {
                if args.len() > guard_info.param_index {
                    return Some(TypePredicate::UserDefinedGuard(guard_info.guarded_type.clone()));
                }
            }
            None
        }
        AstNode::Identifier { .. } => Some(TypePredicate::IsTruthy),
        AstNode::UnaryOp { op: beacon_parser::UnaryOperator::Not, operand, .. } => {
            let inner_pred = extract_type_predicate(operand, env)?;
            Some(TypePredicate::Not(Box::new(inner_pred)))
        }
        _ => None,
    }
}

/// Extract the variable being guarded by a test expression (TODO)
#[allow(dead_code)]
fn extract_guarded_variable(test: &AstNode, env: &mut TypeEnvironment) -> Option<(String, Type)> {
    let var_name = match test {
        AstNode::Compare { left, .. } => {
            if let AstNode::Identifier { name, .. } = left.as_ref() {
                Some(name.clone())
            } else {
                None
            }
        }
        AstNode::Identifier { name, .. } => Some(name.clone()),
        _ => None,
    }?;

    let original_type = env.lookup(&var_name)?;
    Some((var_name, original_type))
}

/// Convert a type name string to a Type
fn type_name_to_type(name: &str) -> Type {
    match name {
        "int" => Type::int(),
        "str" => Type::string(),
        "float" => Type::float(),
        "bool" => Type::bool(),
        "list" => Type::Con(TypeCtor::List),
        "dict" => Type::Con(TypeCtor::Dict),
        "set" => Type::Con(TypeCtor::Set),
        "tuple" => Type::Con(TypeCtor::Tuple),
        _ => Type::Var(TypeVarGen::new().fresh()),
    }
}

/// Detect if this is the `if __name__ == "__main__":` idiom
///
/// This Python idiom checks if the script is being run directly.
/// The body should always be treated as void context (like module-level code).
fn is_main_guard(test: &AstNode) -> bool {
    if let AstNode::Compare { left, ops, comparators, .. } = test {
        if ops.len() == 1 && comparators.len() == 1 {
            let is_name_left = matches!(
                left.as_ref(),
                AstNode::Identifier { name, .. } if name == "__name__"
            );
            let is_main_right = matches!(
                &comparators[0],
                AstNode::Literal { value: LiteralValue::String { value, .. }, .. } if value == "__main__"
            );

            let is_main_left = matches!(
                left.as_ref(),
                AstNode::Literal { value: LiteralValue::String { value, .. }, .. } if value == "__main__"
            );
            let is_name_right = matches!(
                &comparators[0],
                AstNode::Identifier { name, .. } if name == "__name__"
            );

            let is_eq = matches!(ops[0], beacon_parser::CompareOperator::Eq);

            is_eq && ((is_name_left && is_main_right) || (is_main_left && is_name_right))
        } else {
            false
        }
    } else {
        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use beacon_parser::{AstNode, SymbolTable};

    fn make_yield(line: usize, col: usize) -> AstNode {
        AstNode::Yield { value: None, end_line: line, line, col, end_col: col + 5 }
    }

    #[test]
    fn test_contains_yield_simple() {
        let nodes = vec![make_yield(1, 1)];
        assert!(contains_yield(&nodes));
    }

    #[test]
    fn test_contains_yield_nested_function() {
        let nodes = vec![AstNode::FunctionDef {
            name: "inner".to_string(),
            args: vec![],
            body: vec![make_yield(2, 5)],
            docstring: None,
            return_type: None,
            decorators: vec![],
            is_async: false,
            line: 2,
            end_line: 2,
            col: 1,
            end_col: 1,
        }];
        assert!(
            !contains_yield(&nodes),
            "Yields in nested functions should not be detected"
        );
    }

    #[test]
    fn test_contains_yield_in_lambda() {
        let nodes = vec![AstNode::Assignment {
            target: Box::new(AstNode::Identifier { name: "x".to_string(), line: 1, col: 1, end_line: 1, end_col: 2 }),
            value: Box::new(AstNode::Lambda {
                args: vec![],
                body: Box::new(make_yield(1, 20)),
                line: 1,
                col: 5,
                end_line: 1,
                end_col: 5,
            }),
            line: 1,
            end_line: 1,
            col: 1,
            end_col: 1,
        }];
        assert!(
            !contains_yield(&nodes),
            "Yields in lambda functions should not be detected"
        );
    }

    #[test]
    fn test_contains_yield_in_list_comp() {
        let nodes = vec![AstNode::ListComp {
            element: Box::new(make_yield(1, 10)),
            generators: vec![],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 1,
        }];
        assert!(
            !contains_yield(&nodes),
            "Yields in list comprehensions should not be detected"
        );
    }

    #[test]
    fn test_contains_yield_in_generator_exp() {
        let nodes = vec![AstNode::GeneratorExp {
            element: Box::new(make_yield(1, 10)),
            generators: vec![],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 1,
        }];
        assert!(
            !contains_yield(&nodes),
            "Yields in generator expressions should not be detected"
        );
    }

    #[test]
    fn test_contains_yield_in_dict_comp() {
        let nodes = vec![AstNode::DictComp {
            key: Box::new(make_yield(1, 10)),
            value: Box::new(AstNode::Identifier { name: "v".to_string(), line: 1, col: 15, end_line: 1, end_col: 16 }),
            generators: vec![],
            line: 1,
            end_line: 1,
            col: 1,
            end_col: 1,
        }];
        assert!(
            !contains_yield(&nodes),
            "Yields in dict comprehensions should not be detected"
        );
    }

    #[test]
    fn test_contains_yield_in_set_comp() {
        let nodes = vec![AstNode::SetComp {
            element: Box::new(make_yield(1, 10)),
            generators: vec![],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 1,
        }];
        assert!(
            !contains_yield(&nodes),
            "Yields in set comprehensions should not be detected"
        );
    }

    #[test]
    fn test_contains_yield_in_if_statement() {
        let nodes = vec![AstNode::If {
            test: Box::new(AstNode::Identifier {
                name: "condition".to_string(),
                line: 1,
                col: 4,
                end_line: 1,
                end_col: 13,
            }),
            body: vec![make_yield(2, 5)],
            elif_parts: vec![],
            else_body: None,
            line: 1,
            end_line: 1,
            col: 1,
            end_col: 1,
        }];
        assert!(
            contains_yield(&nodes),
            "Yields in if statement bodies should be detected"
        );
    }

    #[test]
    fn test_detect_function_kind_regular() {
        let body = vec![AstNode::Return {
            value: Some(Box::new(AstNode::Literal {
                value: LiteralValue::Integer(42),
                line: 1,
                end_line: 1,
                col: 8,
                end_col: 8,
            })),
            line: 1,
            end_line: 1,
            col: 1,
            end_col: 1,
        }];
        assert_eq!(detect_function_kind(&body, false), FunctionKind::Regular);
    }

    #[test]
    fn test_detect_function_kind_generator() {
        let body = vec![AstNode::Yield {
            value: Some(Box::new(AstNode::Literal {
                value: LiteralValue::Integer(1),
                line: 1,
                end_line: 1,
                col: 11,
                end_col: 11,
            })),
            line: 1,
            end_line: 1,
            col: 5,
            end_col: 5,
        }];
        assert_eq!(detect_function_kind(&body, false), FunctionKind::Generator);
    }

    #[test]
    fn test_detect_function_kind_async_generator() {
        let body = vec![AstNode::Yield {
            value: Some(Box::new(AstNode::Literal {
                value: LiteralValue::Integer(1),
                line: 1,
                end_line: 1,
                col: 11,
                end_col: 11,
            })),
            line: 1,
            end_line: 1,
            col: 5,
            end_col: 5,
        }];
        assert_eq!(detect_function_kind(&body, true), FunctionKind::AsyncGenerator);
    }

    #[test]
    fn test_detect_function_kind_coroutine() {
        let body = vec![AstNode::Await {
            value: Box::new(AstNode::Identifier {
                name: "something".to_string(),
                line: 1,
                col: 11,
                end_line: 1,
                end_col: 20,
            }),
            line: 1,
            end_line: 1,
            col: 5,
            end_col: 5,
        }];
        assert_eq!(detect_function_kind(&body, true), FunctionKind::Coroutine);
    }

    #[test]
    fn test_conditional_assignment_merges_branch_types() {
        let mut ctx = ConstraintGenContext::new();
        let mut env = super::super::type_env::TypeEnvironment::new();

        env.bind("provider".to_string(), TypeScheme::mono(Type::optional(Type::string())));

        let condition = AstNode::Compare {
            left: Box::new(AstNode::Identifier {
                name: "provider".to_string(),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 9,
            }),
            ops: vec![beacon_parser::CompareOperator::Is],
            comparators: vec![AstNode::Literal {
                value: LiteralValue::None,
                line: 1,
                col: 15,
                end_col: 19,
                end_line: 1,
            }],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 1,
        };

        let reassignment = AstNode::Assignment {
            target: Box::new(AstNode::Identifier {
                name: "provider".to_string(),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 9,
            }),
            value: Box::new(AstNode::Literal {
                value: LiteralValue::String { value: "fallback".to_string(), prefix: String::new() },
                line: 2,
                end_line: 2,
                col: 4,
                end_col: 12,
            }),
            line: 2,
            col: 4,
            end_line: 2,
            end_col: 4,
        };

        let if_node = AstNode::If {
            test: Box::new(condition),
            body: vec![reassignment],
            elif_parts: vec![],
            else_body: None,
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 1,
        };

        visit_node_with_context(&if_node, &mut env, &mut ctx, None, ExprContext::Void).unwrap();

        let scheme = env.get_scheme("provider").expect("provider binding");
        assert_eq!(scheme.ty, Type::string());
    }

    #[test]
    fn test_await_coroutine_type() {
        let mut ctx = ConstraintGenContext::new();
        let symbol_table = SymbolTable::new();
        let mut env = super::super::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let async_fn = AstNode::FunctionDef {
            name: "async_fn".to_string(),
            args: vec![],
            body: vec![AstNode::Return {
                value: Some(Box::new(AstNode::Literal {
                    value: beacon_parser::LiteralValue::Integer(42),
                    line: 2,
                    end_line: 2,
                    col: 12,
                    end_col: 12,
                })),
                line: 2,
                end_line: 2,
                col: 5,
                end_col: 5,
            }],
            docstring: None,
            return_type: Some("int".to_string()),
            decorators: vec![],
            is_async: true,
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 1,
        };

        let fn_ty = visit_node_with_env(&async_fn, &mut env, &mut ctx, None).unwrap();

        assert!(matches!(fn_ty, Type::Fun(_, _)));
    }

    #[test]
    fn test_await_generates_awaitable_constraint() {
        let mut ctx = beacon_constraint::ConstraintGenContext::new();
        let symbol_table = SymbolTable::new();
        let mut env = super::super::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let await_expr = AstNode::Await {
            value: Box::new(AstNode::Identifier {
                name: "unknown_awaitable".to_string(),
                line: 1,
                col: 7,
                end_line: 1,
                end_col: 24,
            }),
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 1,
        };

        let result_ty = visit_node_with_env(&await_expr, &mut env, &mut ctx, None).unwrap();

        assert!(matches!(result_ty, Type::Var(_)));
        assert!(!ctx.constraints.is_empty());
    }

    #[test]
    fn test_void_context_module_level_call() {
        let mut ctx = beacon_constraint::ConstraintGenContext::new();
        let symbol_table = SymbolTable::new();
        let mut env = super::super::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let func_def = AstNode::FunctionDef {
            name: "create_calculator".to_string(),
            args: vec![],
            body: vec![],
            docstring: None,
            return_type: Some("Calculator".to_string()),
            decorators: vec![],
            is_async: false,
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 1,
        };
        visit_node_with_env(&func_def, &mut env, &mut ctx, None).unwrap();

        let module = AstNode::Module {
            body: vec![AstNode::Call {
                function: "create_calculator".to_string(),
                args: vec![],
                keywords: vec![],
                line: 2,
                col: 1,
                end_line: 2,
                end_col: 1,
            }],
            docstring: None,
        };

        visit_node_with_env(&module, &mut env, &mut ctx, None).unwrap();

        let has_none_calculator_equal = ctx.constraints.iter().any(|c| {
            matches!(
                c,
                Constraint::Equal(Type::Con(TypeCtor::NoneType), Type::Con(_), _)
                    | Constraint::Equal(Type::Con(_), Type::Con(TypeCtor::NoneType), _)
            )
        });

        assert!(
            !has_none_calculator_equal,
            "Should not generate Equal constraints involving None for expression statements in void context"
        );

        let has_call_constraint = ctx
            .constraints
            .iter()
            .any(|c| matches!(c, Constraint::Call(_, _, _, _, _)));

        assert!(
            has_call_constraint,
            "Should still generate Call constraint to type-check the function call"
        );
    }

    #[test]
    fn test_if_main_guard_void_context() {
        let mut ctx = beacon_constraint::ConstraintGenContext::new();
        let symbol_table = SymbolTable::new();
        let mut env = super::super::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let main_func = AstNode::FunctionDef {
            name: "main".to_string(),
            args: vec![],
            body: vec![],
            docstring: None,
            return_type: Some("Calculator".to_string()),
            decorators: vec![],
            is_async: false,
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 1,
        };
        visit_node_with_env(&main_func, &mut env, &mut ctx, None).unwrap();

        let if_main = AstNode::If {
            test: Box::new(AstNode::Compare {
                left: Box::new(AstNode::Identifier {
                    name: "__name__".to_string(),
                    line: 3,
                    col: 4,
                    end_line: 3,
                    end_col: 12,
                }),
                ops: vec![beacon_parser::CompareOperator::Eq],
                comparators: vec![AstNode::Literal {
                    value: LiteralValue::String { value: "__main__".to_string(), prefix: String::new() },
                    line: 3,
                    end_line: 3,
                    col: 15,
                    end_col: 15,
                }],
                line: 3,
                end_line: 3,
                col: 4,
                end_col: 4,
            }),
            body: vec![AstNode::Call {
                function: "main".to_string(),
                args: vec![],
                keywords: vec![],
                line: 4,
                col: 5,
                end_line: 4,
                end_col: 8,
            }],
            elif_parts: vec![],
            else_body: None,
            line: 3,
            col: 1,
            end_line: 3,
            end_col: 1,
        };

        visit_node_with_env(&if_main, &mut env, &mut ctx, None).unwrap();

        let has_none_calculator_equal = ctx.constraints.iter().any(|c| {
            matches!(
                c,
                Constraint::Equal(Type::Con(TypeCtor::NoneType), Type::Con(_), _)
                    | Constraint::Equal(Type::Con(_), Type::Con(TypeCtor::NoneType), _)
            )
        });

        assert!(
            !has_none_calculator_equal,
            "if __name__ == \"__main__\" body should be void context, not generating Equal constraints with None"
        );

        let has_call_constraint = ctx
            .constraints
            .iter()
            .any(|c| matches!(c, Constraint::Call(_, _, _, _, _)));

        assert!(has_call_constraint, "Should still type-check the main() call");
    }

    #[test]
    fn test_if_main_guard_detection() {
        let test1 = AstNode::Compare {
            left: Box::new(AstNode::Identifier {
                name: "__name__".to_string(),
                line: 1,
                col: 4,
                end_line: 1,
                end_col: 12,
            }),
            ops: vec![beacon_parser::CompareOperator::Eq],
            comparators: vec![AstNode::Literal {
                value: LiteralValue::String { value: "__main__".to_string(), prefix: String::new() },
                line: 1,
                end_line: 1,
                col: 15,
                end_col: 15,
            }],
            line: 1,
            col: 4,
            end_line: 1,
            end_col: 4,
        };
        assert!(is_main_guard(&test1), "Should detect __name__ == \"__main__\"");

        let test2 = AstNode::Compare {
            left: Box::new(AstNode::Literal {
                value: LiteralValue::String { value: "__main__".to_string(), prefix: String::new() },
                line: 1,
                end_line: 1,
                col: 4,
                end_col: 4,
            }),
            ops: vec![beacon_parser::CompareOperator::Eq],
            comparators: vec![AstNode::Identifier {
                name: "__name__".to_string(),
                line: 1,
                col: 18,
                end_line: 1,
                end_col: 26,
            }],
            line: 1,
            col: 4,
            end_line: 1,
            end_col: 4,
        };
        assert!(is_main_guard(&test2), "Should detect \"__main__\" == __name__");

        let test3 = AstNode::Compare {
            left: Box::new(AstNode::Identifier { name: "x".to_string(), line: 1, col: 4, end_line: 1, end_col: 5 }),
            ops: vec![beacon_parser::CompareOperator::Eq],
            comparators: vec![AstNode::Literal {
                value: LiteralValue::Integer(42),
                line: 1,
                col: 9,
                end_line: 1,
                end_col: 11,
            }],
            line: 1,
            col: 4,
            end_line: 1,
            end_col: 4,
        };
        assert!(!is_main_guard(&test3), "Should not detect regular comparison");
    }

    #[test]
    fn test_return_path_analysis_implicit_none() {
        let body = vec![AstNode::Assignment {
            target: Box::new(AstNode::Identifier { name: "x".to_string(), line: 1, col: 1, end_line: 1, end_col: 2 }),
            value: Box::new(AstNode::Literal {
                value: LiteralValue::Integer(42),
                line: 1,
                col: 5,
                end_line: 1,
                end_col: 7,
            }),
            line: 1,
            end_line: 1,
            col: 1,
            end_col: 2,
        }];

        let analysis = analyze_return_paths(&body);
        assert!(!analysis.has_value_returns, "Should have no value returns");
        assert!(!analysis.has_none_returns, "Should have no explicit None returns");
        assert!(analysis.has_implicit_return, "Should have implicit return");
        assert!(analysis.should_infer_none(), "Should infer None return type");
    }

    #[test]
    fn test_return_path_analysis_explicit_value() {
        let body = vec![AstNode::Return {
            value: Some(Box::new(AstNode::Literal {
                value: LiteralValue::Integer(42),
                line: 1,
                end_line: 1,
                col: 12,
                end_col: 12,
            })),
            line: 1,
            end_line: 1,
            col: 5,
            end_col: 5,
        }];

        let analysis = analyze_return_paths(&body);
        assert!(analysis.has_value_returns, "Should have value returns");
        assert!(!analysis.has_none_returns, "Should have no explicit None returns");
        assert!(
            !analysis.has_implicit_return,
            "Should not have implicit return (always exits)"
        );
        assert!(!analysis.should_infer_none(), "Should not infer None");
    }

    #[test]
    fn test_return_path_analysis_mixed_returns() {
        let body = vec![AstNode::If {
            test: Box::new(AstNode::Identifier {
                name: "condition".to_string(),
                line: 1,
                col: 4,
                end_line: 1,
                end_col: 13,
            }),
            body: vec![AstNode::Return {
                value: Some(Box::new(AstNode::Literal {
                    value: LiteralValue::Integer(42),
                    line: 2,
                    end_line: 2,
                    col: 16,
                    end_col: 16,
                })),
                line: 2,
                end_line: 2,
                col: 9,
                end_col: 9,
            }],
            elif_parts: vec![],
            else_body: None,
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 1,
        }];

        let analysis = analyze_return_paths(&body);
        assert!(analysis.has_value_returns, "Should have value returns");
        assert!(!analysis.has_none_returns, "Should have no explicit None returns");
        assert!(
            analysis.has_implicit_return,
            "Should have implicit return (if might not execute)"
        );
        assert!(
            analysis.should_infer_optional(),
            "Should infer Optional[T] for mixed returns"
        );
    }

    #[test]
    fn test_return_path_analysis_all_paths_return() {
        let body = vec![AstNode::If {
            test: Box::new(AstNode::Identifier {
                name: "condition".to_string(),
                line: 1,
                col: 4,
                end_line: 1,
                end_col: 13,
            }),
            body: vec![AstNode::Return {
                value: Some(Box::new(AstNode::Literal {
                    value: LiteralValue::Integer(1),
                    line: 2,
                    end_line: 2,
                    col: 16,
                    end_col: 16,
                })),
                line: 2,
                end_line: 2,
                col: 9,
                end_col: 9,
            }],
            elif_parts: vec![],
            else_body: Some(vec![AstNode::Return {
                value: Some(Box::new(AstNode::Literal {
                    value: LiteralValue::Integer(2),
                    line: 4,
                    end_line: 4,
                    col: 16,
                    end_col: 16,
                })),
                line: 4,
                end_line: 4,
                col: 9,
                end_col: 9,
            }]),
            line: 1,
            end_line: 1,
            col: 1,
            end_col: 1,
        }];

        let analysis = analyze_return_paths(&body);
        assert!(analysis.has_value_returns, "Should have value returns");
        assert!(!analysis.has_none_returns, "Should have no explicit None returns");
        assert!(
            !analysis.has_implicit_return,
            "Should not have implicit return (all paths exit)"
        );
        assert!(
            !analysis.should_infer_optional(),
            "Should not infer Optional when all paths return values"
        );
    }

    #[test]
    fn test_function_with_implicit_none_return() {
        let mut ctx = beacon_constraint::ConstraintGenContext::new();
        let symbol_table = SymbolTable::new();
        let mut env = super::super::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let func_def = AstNode::FunctionDef {
            name: "do_nothing".to_string(),
            args: vec![],
            body: vec![AstNode::Pass { line: 2, col: 5, end_line: 2, end_col: 9 }],
            docstring: None,
            return_type: None,
            decorators: vec![],
            is_async: false,
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 1,
        };

        let fn_ty = visit_node_with_env(&func_def, &mut env, &mut ctx, None).unwrap();

        if let Type::Fun(params, ret_ty) = fn_ty {
            assert!(params.is_empty(), "Should have no parameters");
            assert!(
                matches!(*ret_ty, Type::Con(TypeCtor::NoneType)),
                "Should infer None return type"
            );
        } else {
            panic!("Expected function type");
        }
    }

    #[test]
    fn test_function_with_explicit_return_constrains_type() {
        let mut ctx = beacon_constraint::ConstraintGenContext::new();
        let symbol_table = SymbolTable::new();
        let mut env = super::super::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let func_def = AstNode::FunctionDef {
            name: "get_number".to_string(),
            args: vec![],
            body: vec![AstNode::Return {
                value: Some(Box::new(AstNode::Literal {
                    value: LiteralValue::Integer(42),
                    line: 2,
                    end_line: 2,
                    col: 12,
                    end_col: 12,
                })),
                line: 2,
                end_line: 2,
                col: 5,
                end_col: 5,
            }],
            docstring: None,
            return_type: Some("int".to_string()),
            decorators: vec![],
            is_async: false,
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 1,
        };

        visit_node_with_env(&func_def, &mut env, &mut ctx, None).unwrap();

        let has_return_constraint = ctx.constraints.iter().any(|c| {
            matches!(
                c,
                Constraint::Equal(Type::Con(TypeCtor::Int), Type::Con(TypeCtor::Int), _)
            )
        });

        assert!(
            has_return_constraint || !ctx.constraints.is_empty(),
            "Should generate constraint for return value"
        );
    }

    #[test]
    fn test_function_with_mixed_returns_infers_optional() {
        let mut ctx = beacon_constraint::ConstraintGenContext::new();
        let symbol_table = SymbolTable::new();
        let mut env = super::super::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let func_def = AstNode::FunctionDef {
            name: "maybe_number".to_string(),
            args: vec![],
            body: vec![AstNode::If {
                test: Box::new(AstNode::Identifier {
                    name: "condition".to_string(),
                    line: 2,
                    col: 8,
                    end_line: 2,
                    end_col: 17,
                }),
                body: vec![AstNode::Return {
                    value: Some(Box::new(AstNode::Literal {
                        value: LiteralValue::Integer(42),
                        line: 3,
                        end_line: 3,
                        col: 16,
                        end_col: 16,
                    })),
                    line: 3,
                    end_line: 3,
                    col: 9,
                    end_col: 9,
                }],
                elif_parts: vec![],
                else_body: None,
                line: 2,
                col: 5,
                end_line: 2,
                end_col: 5,
            }],
            docstring: None,
            return_type: None,
            decorators: vec![],
            is_async: false,
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 1,
        };

        let fn_ty = visit_node_with_env(&func_def, &mut env, &mut ctx, None).unwrap();

        if let Type::Fun(params, ret_ty) = fn_ty {
            assert!(params.is_empty(), "Should have no parameters");
            match ret_ty.as_ref() {
                Type::Union(members) => {
                    assert_eq!(members.len(), 2, "Optional should be a union of 2 types");
                    assert!(
                        members.iter().any(|t| matches!(t, Type::Con(TypeCtor::NoneType))),
                        "Union should contain None type"
                    );
                    assert!(
                        members.iter().any(|t| matches!(t, Type::Var(_))),
                        "Union should contain a type variable for the inner type"
                    );
                }
                _ => panic!("Expected Union type for Optional, got {ret_ty:?}"),
            }
        } else {
            panic!("Expected function type");
        }
    }

    #[test]
    fn test_function_with_explicit_none_return_infers_optional() {
        let mut ctx = beacon_constraint::ConstraintGenContext::new();
        let symbol_table = SymbolTable::new();
        let mut env = super::super::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let func_def = AstNode::FunctionDef {
            name: "maybe_string".to_string(),
            args: vec![],
            body: vec![AstNode::If {
                test: Box::new(AstNode::Identifier {
                    name: "condition".to_string(),
                    line: 2,
                    col: 8,
                    end_line: 2,
                    end_col: 17,
                }),
                body: vec![AstNode::Return {
                    value: Some(Box::new(AstNode::Literal {
                        value: LiteralValue::String { value: "hello".to_string(), prefix: String::new() },
                        line: 3,
                        end_line: 3,
                        col: 16,
                        end_col: 16,
                    })),
                    line: 3,
                    col: 9,
                    end_line: 3,
                    end_col: 9,
                }],
                elif_parts: vec![],
                else_body: Some(vec![AstNode::Return {
                    value: None,
                    line: 5,
                    col: 9,
                    end_line: 5,
                    end_col: 9,
                }]),
                line: 2,
                end_line: 2,
                col: 5,
                end_col: 5,
            }],
            docstring: None,
            return_type: None,
            decorators: vec![],
            is_async: false,
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 1,
        };

        let fn_ty = visit_node_with_env(&func_def, &mut env, &mut ctx, None).unwrap();

        if let Type::Fun(params, ret_ty) = fn_ty {
            assert!(params.is_empty(), "Should have no parameters");
            match ret_ty.as_ref() {
                Type::Union(members) => {
                    assert_eq!(members.len(), 2, "Optional should be a union of 2 types");
                    assert!(
                        members.iter().any(|t| matches!(t, Type::Con(TypeCtor::NoneType))),
                        "Union should contain None type"
                    );
                }
                _ => panic!("Expected Union type for Optional, got {ret_ty:?}"),
            }
        } else {
            panic!("Expected function type");
        }
    }

    #[test]
    fn test_function_all_paths_return_value_not_optional() {
        let mut ctx = beacon_constraint::ConstraintGenContext::new();
        let symbol_table = SymbolTable::new();
        let mut env = super::super::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let func_def = AstNode::FunctionDef {
            name: "always_number".to_string(),
            args: vec![],
            body: vec![AstNode::If {
                test: Box::new(AstNode::Identifier {
                    name: "condition".to_string(),
                    line: 2,
                    col: 8,
                    end_line: 2,
                    end_col: 17,
                }),
                body: vec![AstNode::Return {
                    value: Some(Box::new(AstNode::Literal {
                        value: LiteralValue::Integer(1),
                        line: 3,
                        end_line: 3,
                        col: 16,
                        end_col: 16,
                    })),
                    line: 3,
                    end_line: 3,
                    col: 9,
                    end_col: 9,
                }],
                elif_parts: vec![],
                else_body: Some(vec![AstNode::Return {
                    value: Some(Box::new(AstNode::Literal {
                        value: LiteralValue::Integer(2),
                        line: 5,
                        end_line: 5,
                        col: 16,
                        end_col: 16,
                    })),
                    line: 5,
                    end_line: 5,
                    col: 9,
                    end_col: 9,
                }]),
                line: 2,
                end_line: 2,
                col: 5,
                end_col: 5,
            }],
            docstring: None,
            return_type: None,
            decorators: vec![],
            is_async: false,
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 1,
        };

        let fn_ty = visit_node_with_env(&func_def, &mut env, &mut ctx, None).unwrap();

        if let Type::Fun(params, ret_ty) = fn_ty {
            assert!(params.is_empty(), "Should have no parameters");
            match ret_ty.as_ref() {
                Type::Var(_) => {}
                Type::Union(_) => {
                    panic!("Should not infer Optional when all paths return values");
                }
                _ => {}
            }
        } else {
            panic!("Expected function type");
        }
    }

    #[test]
    fn test_function_only_implicit_none_not_optional() {
        let mut ctx = beacon_constraint::ConstraintGenContext::new();
        let symbol_table = SymbolTable::new();
        let mut env = super::super::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let func_def = AstNode::FunctionDef {
            name: "side_effect".to_string(),
            args: vec![],
            body: vec![AstNode::Assignment {
                target: Box::new(AstNode::Identifier {
                    name: "x".to_string(),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 2,
                }),
                value: Box::new(AstNode::Literal {
                    value: LiteralValue::Integer(42),
                    line: 2,
                    col: 9,
                    end_line: 2,
                    end_col: 9,
                }),
                line: 2,
                col: 5,
                end_line: 2,
                end_col: 5,
            }],
            docstring: None,
            return_type: None,
            decorators: vec![],
            is_async: false,
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 1,
        };

        let fn_ty = visit_node_with_env(&func_def, &mut env, &mut ctx, None).unwrap();

        if let Type::Fun(params, ret_ty) = fn_ty {
            assert!(params.is_empty(), "Should have no parameters");
            assert!(
                matches!(ret_ty.as_ref(), Type::Con(TypeCtor::NoneType)),
                "Should infer None type for functions with only implicit returns, got {ret_ty:?}"
            );
        } else {
            panic!("Expected function type");
        }
    }

    #[test]
    fn test_main_guard_with_implicit_none_return() {
        let mut ctx = beacon_constraint::ConstraintGenContext::new();
        let symbol_table = SymbolTable::new();
        let mut env = super::super::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let main_func = AstNode::FunctionDef {
            name: "main".to_string(),
            args: vec![],
            body: vec![AstNode::Assignment {
                target: Box::new(AstNode::Identifier {
                    name: "x".to_string(),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 2,
                }),
                value: Box::new(AstNode::Literal {
                    value: LiteralValue::Integer(42),
                    line: 2,
                    col: 9,
                    end_line: 2,
                    end_col: 11,
                }),
                line: 2,
                col: 5,
                end_line: 2,
                end_col: 6,
            }],
            docstring: None,
            return_type: None,
            decorators: vec![],
            is_async: false,
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 1,
        };
        let main_ty = visit_node_with_env(&main_func, &mut env, &mut ctx, None).unwrap();

        if let Type::Fun(params, ret_ty) = &main_ty {
            assert!(params.is_empty());
            assert!(
                matches!(ret_ty.as_ref(), Type::Con(TypeCtor::NoneType)),
                "main should be inferred as returning None, got {ret_ty:?}"
            );
        } else {
            panic!("Expected function type for main");
        }

        let if_main = AstNode::If {
            test: Box::new(AstNode::Compare {
                left: Box::new(AstNode::Identifier {
                    name: "__name__".to_string(),
                    line: 5,
                    col: 4,
                    end_line: 5,
                    end_col: 12,
                }),
                ops: vec![beacon_parser::CompareOperator::Eq],
                comparators: vec![AstNode::Literal {
                    value: LiteralValue::String { value: "__main__".to_string(), prefix: String::new() },
                    line: 5,
                    col: 15,
                    end_line: 5,
                    end_col: 15,
                }],
                line: 5,
                end_line: 5,
                col: 4,
                end_col: 4,
            }),
            body: vec![AstNode::Call {
                function: "main".to_string(),
                args: vec![],
                keywords: vec![],
                line: 6,
                col: 5,
                end_line: 6,
                end_col: 5,
            }],
            elif_parts: vec![],
            else_body: None,
            line: 5,
            col: 1,
            end_line: 5,
            end_col: 1,
        };

        visit_node_with_env(&if_main, &mut env, &mut ctx, None).unwrap();

        let has_calculator_none_constraint = ctx.constraints.iter().any(|c| {
            matches!(
                c,
                Constraint::Equal(Type::Con(TypeCtor::Class(name)), Type::Con(TypeCtor::NoneType), _)
                    | Constraint::Equal(Type::Con(TypeCtor::NoneType), Type::Con(TypeCtor::Class(name)), _)
                if name.contains("Calculator") || name.contains("Processor")
            )
        });

        assert!(
            !has_calculator_none_constraint,
            "Should NOT generate Equal constraints between class types and None for main() call in void context"
        );

        let has_call_to_main = ctx.constraints.iter().any(|c| {
            if let Constraint::Call(func_ty, args, _, _, _) = c {
                args.is_empty() && matches!(func_ty, Type::Fun(_, _))
            } else {
                false
            }
        });

        assert!(
            has_call_to_main,
            "Should still generate Call constraint for main() to verify it's called correctly"
        );
    }

    #[test]
    fn test_list_literal_empty() {
        let mut ctx = beacon_constraint::ConstraintGenContext::new();
        let symbol_table = SymbolTable::new();
        let mut env = super::super::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let list_node = AstNode::List { elements: vec![], line: 1, col: 5, end_line: 1, end_col: 7 };
        let list_ty = visit_node_with_env(&list_node, &mut env, &mut ctx, None).unwrap();

        assert!(
            matches!(&list_ty, Type::App(inner, _) if matches!(**inner, Type::Con(TypeCtor::List))),
            "Empty list should create list type, got {list_ty:?}"
        );
    }

    #[test]
    fn test_list_literal_with_elements() {
        let mut ctx = beacon_constraint::ConstraintGenContext::new();
        let symbol_table = SymbolTable::new();
        let mut env = super::super::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let list_node = AstNode::List {
            elements: vec![
                AstNode::Literal { value: LiteralValue::Integer(1), line: 1, col: 6, end_line: 1, end_col: 6 },
                AstNode::Literal { value: LiteralValue::Integer(2), line: 1, col: 9, end_line: 1, end_col: 9 },
                AstNode::Literal { value: LiteralValue::Integer(3), line: 1, col: 12, end_line: 1, end_col: 12 },
            ],
            line: 1,
            col: 5,
            end_line: 1,
            end_col: 5,
        };

        let list_ty = visit_node_with_env(&list_node, &mut env, &mut ctx, None).unwrap();
        assert!(
            matches!(&list_ty, Type::App(inner, _) if matches!(**inner, Type::Con(TypeCtor::List))),
            "List literal should create list type, got {list_ty:?}"
        );

        let equal_constraints = ctx
            .constraints
            .iter()
            .filter(|c| matches!(c, Constraint::Equal(_, _, _)))
            .count();
        assert!(
            equal_constraints >= 3,
            "Should have equality constraints for unifying element types"
        );
    }

    #[test]
    fn test_dict_literal_empty() {
        let mut ctx = beacon_constraint::ConstraintGenContext::new();
        let symbol_table = SymbolTable::new();
        let mut env = super::super::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let dict_node = AstNode::Dict { keys: vec![], values: vec![], line: 1, col: 5, end_line: 1, end_col: 5 };
        let dict_ty = visit_node_with_env(&dict_node, &mut env, &mut ctx, None).unwrap();

        assert!(
            matches!(&dict_ty, Type::App(inner, _) if matches!(inner.as_ref(), Type::App(dict_inner, _) if matches!(dict_inner.as_ref(), Type::Con(TypeCtor::Dict)))),
            "Empty dict should create dict type, got {dict_ty:?}"
        );
    }

    #[test]
    fn test_dict_literal_with_pairs() {
        let mut ctx = beacon_constraint::ConstraintGenContext::new();
        let symbol_table = SymbolTable::new();
        let mut env = super::super::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let dict_node = AstNode::Dict {
            keys: vec![
                AstNode::Literal {
                    value: LiteralValue::String { value: "a".to_string(), prefix: "".to_string() },
                    line: 1,
                    col: 6,
                    end_line: 1,
                    end_col: 6,
                },
                AstNode::Literal {
                    value: LiteralValue::String { value: "b".to_string(), prefix: "".to_string() },
                    line: 1,
                    col: 15,
                    end_line: 1,
                    end_col: 15,
                },
            ],
            values: vec![
                AstNode::Literal { value: LiteralValue::Integer(1), line: 1, col: 11, end_line: 1, end_col: 11 },
                AstNode::Literal { value: LiteralValue::Integer(2), line: 1, col: 20, end_line: 1, end_col: 20 },
            ],
            line: 1,
            col: 5,
            end_line: 1,
            end_col: 5,
        };
        let dict_ty = visit_node_with_env(&dict_node, &mut env, &mut ctx, None).unwrap();

        assert!(
            matches!(&dict_ty, Type::App(inner, _) if matches!(inner.as_ref(), Type::App(dict_inner, _) if matches!(dict_inner.as_ref(), Type::Con(TypeCtor::Dict)))),
            "Dict literal should create dict type, got {dict_ty:?}"
        );

        let equal_constraints = ctx
            .constraints
            .iter()
            .filter(|c| matches!(c, Constraint::Equal(_, _, _)))
            .count();
        assert!(
            equal_constraints >= 4,
            "Should have equality constraints for unifying key and value types"
        );
    }

    #[test]
    fn test_set_literal() {
        let mut ctx = beacon_constraint::ConstraintGenContext::new();
        let symbol_table = SymbolTable::new();
        let mut env = super::super::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let set_node = AstNode::Set {
            elements: vec![
                AstNode::Literal { value: LiteralValue::Integer(1), line: 1, col: 6, end_line: 1, end_col: 6 },
                AstNode::Literal { value: LiteralValue::Integer(2), line: 1, col: 9, end_line: 1, end_col: 9 },
                AstNode::Literal { value: LiteralValue::Integer(3), line: 1, col: 12, end_line: 1, end_col: 12 },
            ],
            line: 1,
            col: 5,
            end_line: 1,
            end_col: 5,
        };
        let set_ty = visit_node_with_env(&set_node, &mut env, &mut ctx, None).unwrap();

        assert!(
            matches!(&set_ty, Type::App(inner, _) if matches!(**inner, Type::Con(TypeCtor::Set))),
            "Set literal should create set type, got {set_ty:?}",
        );

        let equal_constraints = ctx
            .constraints
            .iter()
            .filter(|c| matches!(c, Constraint::Equal(_, _, _)))
            .count();
        assert!(
            equal_constraints >= 3,
            "Should have equality constraints for unifying element types"
        );
    }

    #[test]
    fn test_tuple_literal() {
        let mut ctx = beacon_constraint::ConstraintGenContext::new();
        let symbol_table = SymbolTable::new();
        let mut env = super::super::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let tuple_node = AstNode::Tuple {
            elements: vec![
                AstNode::Literal { value: LiteralValue::Integer(1), line: 1, col: 6, end_line: 1, end_col: 6 },
                AstNode::Literal {
                    value: LiteralValue::String { value: "a".to_string(), prefix: "".to_string() },
                    line: 1,
                    col: 9,
                    end_line: 1,
                    end_col: 9,
                },
                AstNode::Literal { value: LiteralValue::Boolean(true), line: 1, col: 14, end_line: 1, end_col: 14 },
            ],
            is_parenthesized: true,
            line: 1,
            col: 5,
            end_line: 1,
            end_col: 5,
        };
        let tuple_ty = visit_node_with_env(&tuple_node, &mut env, &mut ctx, None).unwrap();

        match &tuple_ty {
            Type::Tuple(types) => {
                assert_eq!(types.len(), 3, "Tuple should have 3 elements");
                assert!(
                    matches!(types[0], Type::Con(TypeCtor::Int)),
                    "First element should be int, got {:?}",
                    types[0]
                );
                assert!(
                    matches!(types[1], Type::Con(TypeCtor::String)),
                    "Second element should be str, got {:?}",
                    types[1]
                );
                assert!(
                    matches!(types[2], Type::Con(TypeCtor::Bool)),
                    "Third element should be bool, got {:?}",
                    types[2]
                );
            }
            _ => panic!("Expected heterogeneous tuple type, got {tuple_ty:?}"),
        }
    }

    #[test]
    fn test_subscript_generates_call_constraint() {
        let mut ctx = beacon_constraint::ConstraintGenContext::new();
        let symbol_table = SymbolTable::new();
        let mut env = super::super::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let list_node = AstNode::List {
            elements: vec![
                AstNode::Literal { value: LiteralValue::Integer(1), line: 1, col: 6, end_line: 1, end_col: 6 },
                AstNode::Literal { value: LiteralValue::Integer(2), line: 1, col: 9, end_line: 1, end_col: 9 },
                AstNode::Literal { value: LiteralValue::Integer(3), line: 1, col: 12, end_line: 1, end_col: 12 },
            ],
            line: 1,
            col: 5,
            end_line: 1,
            end_col: 5,
        };

        let subscript_node = AstNode::Subscript {
            value: Box::new(list_node),
            slice: Box::new(AstNode::Literal {
                value: LiteralValue::Integer(0),
                line: 1,
                col: 16,
                end_line: 1,
                end_col: 16,
            }),
            line: 1,
            col: 5,
            end_line: 1,
            end_col: 5,
        };

        let _result_ty = visit_node_with_env(&subscript_node, &mut env, &mut ctx, None).unwrap();

        let has_attr_count = ctx
            .constraints
            .iter()
            .filter(|c| matches!(c, Constraint::HasAttr(_, name, _, _) if name == "__getitem__"))
            .count();
        assert_eq!(
            has_attr_count, 1,
            "Should generate one HasAttr constraint for __getitem__"
        );

        let call_count = ctx
            .constraints
            .iter()
            .filter(|c| matches!(c, Constraint::Call(_, args, _, _, _) if args.len() == 1))
            .count();
        assert!(call_count >= 1, "Should generate Call constraint to invoke __getitem__");
    }

    #[test]
    fn test_subscript_result_type() {
        let mut ctx = beacon_constraint::ConstraintGenContext::new();
        let symbol_table = SymbolTable::new();
        let mut env = super::super::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let assignment = AstNode::Assignment {
            target: Box::new(AstNode::Identifier { name: "x".to_string(), line: 1, col: 1, end_line: 1, end_col: 2 }),
            value: Box::new(AstNode::Subscript {
                value: Box::new(AstNode::List {
                    elements: vec![AstNode::Literal {
                        value: LiteralValue::Integer(1),
                        line: 1,
                        col: 10,
                        end_line: 1,
                        end_col: 10,
                    }],
                    line: 1,
                    col: 9,
                    end_line: 1,
                    end_col: 9,
                }),
                slice: Box::new(AstNode::Literal {
                    value: LiteralValue::Integer(0),
                    line: 1,
                    col: 12,
                    end_line: 1,
                    end_col: 12,
                }),
                line: 1,
                col: 9,
                end_line: 1,
                end_col: 9,
            }),
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 1,
        };

        visit_node_with_env(&assignment, &mut env, &mut ctx, None).unwrap();

        let has_bound_method = ctx.type_map.values().any(|t| matches!(t, Type::BoundMethod(_, _, _)));
        assert!(!has_bound_method, "Subscript result should not be a BoundMethod type");
    }

    #[test]
    fn test_class_with_only_class_level_annotations() {
        let symbol_table = SymbolTable::new();
        let mut env = super::super::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let class_body = vec![
            AstNode::AnnotatedAssignment {
                target: Box::new(AstNode::Identifier {
                    name: "x".to_string(),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 2,
                }),
                type_annotation: "int".to_string(),
                value: None,
                line: 2,
                col: 5,
                end_line: 2,
                end_col: 5,
            },
            AstNode::AnnotatedAssignment {
                target: Box::new(AstNode::Identifier {
                    name: "y".to_string(),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 2,
                }),
                type_annotation: "str".to_string(),
                value: Some(Box::new(AstNode::Literal {
                    value: LiteralValue::String { value: "default".to_string(), prefix: "".to_string() },
                    line: 3,
                    end_line: 3,
                    col: 13,
                    end_col: 13,
                })),
                line: 3,
                col: 5,
                end_line: 3,
                end_col: 5,
            },
        ];

        let metadata = extract_class_metadata("Point", &class_body, &mut env);

        assert!(metadata.fields.contains_key("x"), "Should extract class-level field x");
        assert!(metadata.fields.contains_key("y"), "Should extract class-level field y");

        if let Some(x_type) = metadata.fields.get("x") {
            assert!(
                matches!(x_type, Type::Con(TypeCtor::Int)),
                "Field x should have type int, got {x_type:?}"
            );
        }

        if let Some(y_type) = metadata.fields.get("y") {
            assert!(
                matches!(y_type, Type::Con(TypeCtor::String)),
                "Field y should have type str, got {y_type:?}"
            );
        }
    }

    #[test]
    fn test_class_with_both_class_and_instance_fields() {
        let symbol_table = SymbolTable::new();
        let mut env = super::super::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let class_body = vec![
            AstNode::AnnotatedAssignment {
                target: Box::new(AstNode::Identifier {
                    name: "class_var".to_string(),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 10,
                }),
                type_annotation: "int".to_string(),
                value: Some(Box::new(AstNode::Literal {
                    value: LiteralValue::Integer(0),
                    line: 2,
                    end_line: 2,
                    col: 21,
                    end_col: 21,
                })),
                line: 2,
                col: 5,
                end_line: 2,
                end_col: 5,
            },
            AstNode::FunctionDef {
                name: "__init__".to_string(),
                args: vec![beacon_parser::Parameter {
                    name: "self".to_string(),
                    line: 4,
                    col: 14,
                    end_line: 4,
                    end_col: 18,
                    type_annotation: None,
                    default_value: None,
                }],
                body: vec![AstNode::AnnotatedAssignment {
                    target: Box::new(AstNode::Identifier {
                        name: "self.instance_var".to_string(),
                        line: 1,
                        col: 1,
                        end_line: 1,
                        end_col: 18,
                    }),
                    type_annotation: "str".to_string(),
                    value: Some(Box::new(AstNode::Literal {
                        value: LiteralValue::String { value: "hello".to_string(), prefix: "".to_string() },
                        line: 5,
                        end_line: 5,
                        col: 33,
                        end_col: 33,
                    })),
                    line: 5,
                    end_line: 5,
                    col: 9,
                    end_col: 9,
                }],
                docstring: None,
                return_type: None,
                decorators: vec![],
                is_async: false,
                line: 4,
                col: 5,
                end_line: 4,
                end_col: 5,
            },
        ];

        let metadata = extract_class_metadata("MyClass", &class_body, &mut env);

        assert!(
            metadata.fields.contains_key("class_var"),
            "Should extract class-level field class_var"
        );
        assert!(
            metadata.fields.contains_key("instance_var"),
            "Should extract instance field instance_var from __init__"
        );

        if let Some(class_var_type) = metadata.fields.get("class_var") {
            assert!(
                matches!(class_var_type, Type::Con(TypeCtor::Int)),
                "class_var should have type int, got {class_var_type:?}"
            );
        }

        if let Some(instance_var_type) = metadata.fields.get("instance_var") {
            assert!(
                matches!(instance_var_type, Type::Con(TypeCtor::String)),
                "instance_var should have type str, got {instance_var_type:?}"
            );
        }
    }

    #[test]
    fn test_class_with_non_annotated_class_assignments() {
        let symbol_table = SymbolTable::new();
        let mut env = super::super::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let class_body = vec![
            AstNode::Assignment {
                target: Box::new(AstNode::Identifier {
                    name: "counter".to_string(),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 8,
                }),
                value: Box::new(AstNode::Literal {
                    value: LiteralValue::Integer(0),
                    line: 2,
                    col: 17,
                    end_line: 2,
                    end_col: 18,
                }),
                line: 2,
                col: 5,
                end_line: 2,
                end_col: 5,
            },
            AstNode::Assignment {
                target: Box::new(AstNode::Identifier {
                    name: "name".to_string(),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 5,
                }),
                value: Box::new(AstNode::Literal {
                    value: LiteralValue::String { value: "default".to_string(), prefix: "".to_string() },
                    line: 3,
                    end_line: 3,
                    col: 14,
                    end_col: 14,
                }),
                line: 3,
                col: 5,
                end_line: 3,
                end_col: 5,
            },
        ];

        let metadata = extract_class_metadata("Config", &class_body, &mut env);

        assert!(
            metadata.fields.contains_key("counter"),
            "Should extract non-annotated class-level field counter"
        );
        assert!(
            metadata.fields.contains_key("name"),
            "Should extract non-annotated class-level field name"
        );

        if let Some(counter_type) = metadata.fields.get("counter") {
            assert!(
                matches!(counter_type, Type::Var(_)),
                "Non-annotated field should get fresh type variable, got {counter_type:?}"
            );
        }
    }

    #[test]
    fn test_class_with_mixed_annotated_and_non_annotated() {
        let symbol_table = SymbolTable::new();
        let mut env = super::super::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let class_body = vec![
            AstNode::AnnotatedAssignment {
                target: Box::new(AstNode::Identifier {
                    name: "typed_field".to_string(),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 12,
                }),
                type_annotation: "int".to_string(),
                value: None,
                line: 2,
                col: 5,
                end_line: 2,
                end_col: 5,
            },
            AstNode::Assignment {
                target: Box::new(AstNode::Identifier {
                    name: "untyped_field".to_string(),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 14,
                }),
                value: Box::new(AstNode::Literal {
                    value: LiteralValue::Integer(42),
                    line: 3,
                    col: 23,
                    end_line: 3,
                    end_col: 25,
                }),
                line: 3,
                col: 5,
                end_line: 3,
                end_col: 5,
            },
            AstNode::AnnotatedAssignment {
                target: Box::new(AstNode::Identifier {
                    name: "another_typed".to_string(),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 14,
                }),
                type_annotation: "str".to_string(),
                value: Some(Box::new(AstNode::Literal {
                    value: LiteralValue::String { value: "test".to_string(), prefix: "".to_string() },
                    line: 4,
                    end_line: 4,
                    col: 25,
                    end_col: 25,
                })),
                line: 4,
                col: 5,
                end_line: 4,
                end_col: 5,
            },
        ];

        let metadata = extract_class_metadata("MixedClass", &class_body, &mut env);

        assert!(
            metadata.fields.contains_key("typed_field"),
            "Should extract typed_field"
        );
        assert!(
            metadata.fields.contains_key("untyped_field"),
            "Should extract untyped_field"
        );
        assert!(
            metadata.fields.contains_key("another_typed"),
            "Should extract another_typed"
        );

        if let Some(typed_field_type) = metadata.fields.get("typed_field") {
            assert!(
                matches!(typed_field_type, Type::Con(TypeCtor::Int)),
                "typed_field should have type int"
            );
        }

        if let Some(untyped_field_type) = metadata.fields.get("untyped_field") {
            assert!(
                matches!(untyped_field_type, Type::Var(_)),
                "untyped_field should have fresh type variable"
            );
        }

        if let Some(another_typed_type) = metadata.fields.get("another_typed") {
            assert!(
                matches!(another_typed_type, Type::Con(TypeCtor::String)),
                "another_typed should have type str"
            );
        }
    }

    #[test]
    fn test_class_level_fields_dont_conflict_with_nested_classes() {
        let symbol_table = SymbolTable::new();
        let mut env = super::super::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let class_body = vec![
            AstNode::AnnotatedAssignment {
                target: Box::new(AstNode::Identifier {
                    name: "outer_field".to_string(),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 12,
                }),
                type_annotation: "int".to_string(),
                value: None,
                line: 2,
                col: 5,
                end_line: 2,
                end_col: 5,
            },
            AstNode::ClassDef {
                name: "Inner".to_string(),
                bases: vec![],
                metaclass: None,
                body: vec![AstNode::Assignment {
                    target: Box::new(AstNode::Identifier {
                        name: "inner_field".to_string(),
                        line: 1,
                        col: 1,
                        end_line: 1,
                        end_col: 12,
                    }),
                    value: Box::new(AstNode::Literal {
                        value: LiteralValue::Integer(1),
                        line: 4,
                        col: 21,
                        end_line: 4,
                        end_col: 22,
                    }),
                    line: 4,
                    end_line: 4,
                    col: 9,
                    end_col: 9,
                }],
                docstring: None,
                decorators: vec![],
                line: 3,
                col: 5,
                end_line: 3,
                end_col: 5,
            },
        ];

        let metadata = extract_class_metadata("Outer", &class_body, &mut env);

        assert!(
            metadata.fields.contains_key("outer_field"),
            "Should extract outer_field from outer class"
        );
        assert!(
            !metadata.fields.contains_key("inner_field"),
            "Should NOT extract inner_field from nested class"
        );
    }

    #[test]
    fn test_is_dataclass_decorator() {
        assert!(is_dataclass_decorator(&["dataclass".to_string()]));
        assert!(is_dataclass_decorator(&["dataclasses.dataclass".to_string()]));
        assert!(is_dataclass_decorator(&["other".to_string(), "dataclass".to_string()]));
        assert!(!is_dataclass_decorator(&["property".to_string()]));
        assert!(!is_dataclass_decorator(&[]));
    }

    #[test]
    fn test_has_enum_base() {
        assert!(has_enum_base(&["Enum".to_string()]));
        assert!(has_enum_base(&["enum.Enum".to_string()]));
        assert!(has_enum_base(&["IntEnum".to_string()]));
        assert!(has_enum_base(&["enum.IntEnum".to_string()]));
        assert!(has_enum_base(&["StrEnum".to_string()]));
        assert!(has_enum_base(&["Flag".to_string()]));
        assert!(has_enum_base(&["IntFlag".to_string()]));
        assert!(has_enum_base(&["object".to_string(), "Enum".to_string()]));
        assert!(!has_enum_base(&["object".to_string()]));
        assert!(!has_enum_base(&[]));
    }

    #[test]
    fn test_is_special_class_decorator() {
        assert!(is_special_class_decorator("dataclass"));
        assert!(is_special_class_decorator("dataclasses.dataclass"));
        assert!(is_special_class_decorator("unique"));
        assert!(is_special_class_decorator("enum.unique"));
        assert!(!is_special_class_decorator("property"));
        assert!(!is_special_class_decorator("classmethod"));
    }

    #[test]
    fn test_synthesize_dataclass_init() {
        let symbol_table = SymbolTable::new();
        let mut env = super::super::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let class_body = vec![
            AstNode::AnnotatedAssignment {
                target: Box::new(AstNode::Identifier {
                    name: "x".to_string(),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 2,
                }),
                type_annotation: "int".to_string(),
                value: None,
                line: 2,
                col: 5,
                end_line: 2,
                end_col: 5,
            },
            AstNode::AnnotatedAssignment {
                target: Box::new(AstNode::Identifier {
                    name: "y".to_string(),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 2,
                }),
                type_annotation: "str".to_string(),
                value: None,
                line: 3,
                col: 5,
                end_line: 3,
                end_col: 5,
            },
        ];

        let mut metadata = extract_class_metadata("Point", &class_body, &mut env);

        assert!(metadata.init_type.is_none());

        synthesize_dataclass_init(&mut metadata, &mut env);

        assert!(
            metadata.init_type.is_some(),
            "Dataclass should have synthesized __init__"
        );

        if let Some(Type::Fun(params, ret_ty)) = &metadata.init_type {
            assert!(!params.is_empty(), "__init__ should have at least self parameter");
            assert!(
                matches!(ret_ty.as_ref(), Type::Con(TypeCtor::NoneType)),
                "__init__ should return None"
            );
        } else {
            panic!("Expected function type for __init__");
        }
    }

    #[test]
    fn test_synthesize_dataclass_init_respects_explicit_init() {
        let symbol_table = SymbolTable::new();
        let mut env = super::super::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let class_body = vec![
            AstNode::AnnotatedAssignment {
                target: Box::new(AstNode::Identifier {
                    name: "x".to_string(),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 2,
                }),
                type_annotation: "int".to_string(),
                value: None,
                line: 2,
                col: 5,
                end_line: 2,
                end_col: 5,
            },
            AstNode::FunctionDef {
                name: "__init__".to_string(),
                args: vec![],
                body: vec![],
                docstring: None,
                return_type: None,
                decorators: vec![],
                is_async: false,
                line: 3,
                col: 5,
                end_line: 3,
                end_col: 5,
            },
        ];

        let mut metadata = extract_class_metadata("CustomPoint", &class_body, &mut env);

        assert!(metadata.init_type.is_some());

        let original_init = metadata.init_type.clone();
        synthesize_dataclass_init(&mut metadata, &mut env);
        assert_eq!(
            metadata.init_type, original_init,
            "Should not override explicit __init__"
        );
    }

    #[test]
    fn test_extract_enum_members() {
        let symbol_table = SymbolTable::new();
        let mut env = super::super::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let class_body = vec![
            AstNode::Assignment {
                target: Box::new(AstNode::Identifier {
                    name: "RED".to_string(),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 4,
                }),
                value: Box::new(AstNode::Literal {
                    value: LiteralValue::Integer(1),
                    line: 2,
                    col: 11,
                    end_line: 2,
                    end_col: 12,
                }),
                line: 2,
                col: 5,
                end_line: 2,
                end_col: 5,
            },
            AstNode::Assignment {
                target: Box::new(AstNode::Identifier {
                    name: "GREEN".to_string(),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 6,
                }),
                value: Box::new(AstNode::Literal {
                    value: LiteralValue::Integer(2),
                    line: 3,
                    col: 13,
                    end_line: 3,
                    end_col: 14,
                }),
                line: 3,
                col: 5,
                end_line: 3,
                end_col: 5,
            },
            AstNode::Assignment {
                target: Box::new(AstNode::Identifier {
                    name: "BLUE".to_string(),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 5,
                }),
                value: Box::new(AstNode::Literal {
                    value: LiteralValue::Integer(3),
                    line: 4,
                    col: 12,
                    end_line: 4,
                    end_col: 13,
                }),
                line: 4,
                col: 5,
                end_line: 4,
                end_col: 5,
            },
        ];

        let mut metadata = beacon_core::ClassMetadata::new("Color".to_string());
        extract_enum_members(&mut metadata, &class_body, "Color", &mut env);

        assert!(metadata.fields.contains_key("RED"), "Should extract RED as enum member");
        assert!(
            metadata.fields.contains_key("GREEN"),
            "Should extract GREEN as enum member"
        );
        assert!(
            metadata.fields.contains_key("BLUE"),
            "Should extract BLUE as enum member"
        );

        if let Some(red_type) = metadata.fields.get("RED") {
            match red_type {
                Type::Con(TypeCtor::Class(name)) => {
                    assert_eq!(name, "Color", "Enum member should have enum class type");
                }
                _ => panic!("Expected class type for enum member"),
            }
        }
    }

    #[test]
    fn test_extract_enum_members_ignores_methods() {
        let symbol_table = SymbolTable::new();
        let mut env = super::super::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let class_body = vec![
            AstNode::Assignment {
                target: Box::new(AstNode::Identifier {
                    name: "MEMBER".to_string(),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 7,
                }),
                value: Box::new(AstNode::Literal {
                    value: LiteralValue::Integer(1),
                    line: 2,
                    col: 14,
                    end_line: 2,
                    end_col: 14,
                }),
                line: 2,
                col: 5,
                end_line: 2,
                end_col: 5,
            },
            AstNode::FunctionDef {
                name: "some_method".to_string(),
                args: vec![],
                body: vec![],
                docstring: None,
                return_type: None,
                decorators: vec![],
                is_async: false,
                line: 3,
                col: 5,
                end_line: 3,
                end_col: 5,
            },
        ];

        let mut metadata = beacon_core::ClassMetadata::new("MyEnum".to_string());
        extract_enum_members(&mut metadata, &class_body, "MyEnum", &mut env);

        assert!(
            metadata.fields.contains_key("MEMBER"),
            "Should extract MEMBER as enum member"
        );
        assert_eq!(metadata.fields.len(), 1, "Should only extract assignment, not method");
    }

    #[test]
    fn test_extract_enum_members_ignores_private() {
        let symbol_table = SymbolTable::new();
        let mut env = super::super::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let class_body = vec![
            AstNode::Assignment {
                target: Box::new(AstNode::Identifier {
                    name: "PUBLIC".to_string(),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 7,
                }),
                value: Box::new(AstNode::Literal {
                    value: LiteralValue::Integer(1),
                    line: 2,
                    col: 14,
                    end_line: 2,
                    end_col: 15,
                }),
                line: 2,
                col: 5,
                end_line: 2,
                end_col: 5,
            },
            AstNode::Assignment {
                target: Box::new(AstNode::Identifier {
                    name: "_private".to_string(),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 9,
                }),
                value: Box::new(AstNode::Literal {
                    value: LiteralValue::Integer(2),
                    line: 3,
                    col: 16,
                    end_line: 3,
                    end_col: 17,
                }),
                line: 3,
                col: 5,
                end_line: 3,
                end_col: 5,
            },
        ];

        let mut metadata = beacon_core::ClassMetadata::new("MyEnum".to_string());
        extract_enum_members(&mut metadata, &class_body, "MyEnum", &mut env);

        assert!(metadata.fields.contains_key("PUBLIC"), "Should extract public member");
        assert!(
            !metadata.fields.contains_key("_private"),
            "Should ignore private members"
        );
    }

    #[test]
    fn test_docstring_filtering() {
        let mut ctx = beacon_constraint::ConstraintGenContext::new();
        let symbol_table = SymbolTable::new();
        let mut env = super::super::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let module_with_docstring = AstNode::Module {
            body: vec![
                AstNode::Literal {
                    value: LiteralValue::String { value: "Module docstring".to_string(), prefix: "".to_string() },
                    line: 1,
                    end_line: 1,
                    col: 1,
                    end_col: 1,
                },
                AstNode::Assignment {
                    target: Box::new(AstNode::Identifier {
                        name: "x".to_string(),
                        line: 1,
                        col: 1,
                        end_line: 1,
                        end_col: 2,
                    }),
                    value: Box::new(AstNode::Literal {
                        value: LiteralValue::Integer(42),
                        line: 2,
                        col: 5,
                        end_line: 2,
                        end_col: 7,
                    }),
                    line: 2,
                    end_line: 2,
                    col: 1,
                    end_col: 1,
                },
            ],
            docstring: Some("Module docstring".to_string()),
        };

        visit_node_with_env(&module_with_docstring, &mut env, &mut ctx, None).unwrap();

        let string_type_count = ctx
            .type_map
            .values()
            .filter(|t| matches!(t, Type::Con(TypeCtor::String)))
            .count();

        assert_eq!(
            string_type_count, 0,
            "Docstring should not generate string type in type_map"
        );

        let has_string_constraints = ctx.constraints.iter().any(|c| match c {
            beacon_constraint::Constraint::Equal(t1, t2, _) => {
                matches!(t1, Type::Con(TypeCtor::String)) || matches!(t2, Type::Con(TypeCtor::String))
            }
            _ => false,
        });

        assert!(
            !has_string_constraints,
            "Docstring should not generate constraints involving string type"
        );
    }

    #[test]
    fn test_extract_type_guard_info_with_typeguard() {
        let params = vec![];
        let result = extract_type_guard_info("TypeGuard[str]", &params);
        assert!(result.is_some(), "Should extract TypeGuard info");

        let guard_info = result.unwrap();
        assert_eq!(guard_info.kind, beacon_constraint::TypeGuardKind::TypeGuard);
    }

    #[test]
    fn test_extract_type_guard_info_with_typeis() {
        let params = vec![];
        let result = extract_type_guard_info("TypeIs[int]", &params);
        assert!(result.is_some(), "Should extract TypeIs info");

        let guard_info = result.unwrap();
        assert_eq!(guard_info.kind, beacon_constraint::TypeGuardKind::TypeIs);
    }

    #[test]
    fn test_extract_type_guard_info_with_whitespace() {
        let params = vec![];
        let result = extract_type_guard_info("  TypeGuard[str]  ", &params);
        assert!(result.is_some(), "Should handle whitespace around TypeGuard");
    }

    #[test]
    fn test_extract_type_guard_info_with_complex_type() {
        let params = vec![];
        let result = extract_type_guard_info("TypeGuard[list[str]]", &params);
        assert!(result.is_some(), "Should handle complex type parameters");
    }

    #[test]
    fn test_extract_type_guard_info_without_type_guard() {
        let params = vec![];
        let result = extract_type_guard_info("bool", &params);
        assert!(result.is_none(), "Should return None for non-TypeGuard annotations");
    }

    #[test]
    fn test_extract_type_guard_info_with_invalid_syntax() {
        let params = vec![];
        let result = extract_type_guard_info("TypeGuard[", &params);
        assert!(result.is_none(), "Should return None for invalid syntax");
    }
}

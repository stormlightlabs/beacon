use super::class::{
    extract_class_metadata, extract_enum_members, has_enum_base, is_dataclass_decorator, is_special_class_decorator,
    synthesize_dataclass_init,
};
use super::function::{FunctionKind, analyze_return_paths, detect_function_kind};
use super::guards::{detect_inverse_type_guard, detect_type_guard, extract_type_guard_info, extract_type_predicate};
use super::loader::{self, StubCache};
use super::utils::{get_node_position, is_docstring, is_main_guard, type_name_to_type};

use crate::pattern::extract_pattern_bindings;
use crate::type_env::TypeEnvironment;
use crate::walker::{ExprContext, visit_node_with_context, visit_node_with_env};

use beacon_constraint::{Constraint, ConstraintGenContext, Span, TypePredicate};
use beacon_core::{AnalysisError, BeaconError, TypeCtor, TypeVar, Variance};
use beacon_core::{Type, TypeScheme, errors::Result};
use beacon_parser::AstNode;
use std::sync::Arc;

pub type TStubCache = Arc<std::sync::RwLock<StubCache>>;

/// Extract variable names from a guard expression
///
/// Recursively walks the AST to find all identifiers that could be narrowed by the guard condition.
/// This includes the primary subject of type checks as well as variables in compound expressions.
fn extract_guard_variables(guard: &AstNode) -> Vec<String> {
    let mut vars = Vec::new();
    extract_guard_variables_recursive(guard, &mut vars);
    vars.sort();
    vars.dedup();
    vars
}

/// Recursive helper for extracting variables from guard expressions
fn extract_guard_variables_recursive(node: &AstNode, vars: &mut Vec<String>) {
    match node {
        AstNode::Identifier { name, .. } => {
            vars.push(name.clone());
        }
        AstNode::Compare { left, comparators, .. } => {
            extract_guard_variables_recursive(left, vars);
            for comp in comparators {
                extract_guard_variables_recursive(comp, vars);
            }
        }
        AstNode::Call { function, args, .. } => {
            extract_guard_variables_recursive(function, vars);
            for arg in args {
                extract_guard_variables_recursive(arg, vars);
            }
        }
        AstNode::UnaryOp { operand, .. } => {
            extract_guard_variables_recursive(operand, vars);
        }
        AstNode::BinaryOp { left, right, .. } => {
            extract_guard_variables_recursive(left, vars);
            extract_guard_variables_recursive(right, vars);
        }
        AstNode::Attribute { object, .. } => {
            extract_guard_variables_recursive(object, vars);
        }
        AstNode::Subscript { value, slice, .. } => {
            extract_guard_variables_recursive(value, vars);
            extract_guard_variables_recursive(slice, vars);
        }
        _ => {}
    }
}

/// Extract type parameters from Generic[T1, T2, ...] or Protocol[T1, T2, ...] base class notation by
/// parsing strings like "Generic[T_co]", "Protocol[T_co]", "Generic[T_co, T_contra]" and looks up the
/// [TypeVar] instances in the environment to get their variance information.
fn extract_generic_type_params(base: &str, env: &mut TypeEnvironment) -> Option<Vec<TypeVar>> {
    let (is_generic, is_protocol) = (base.starts_with("Generic["), base.starts_with("Protocol["));

    if !is_generic && !is_protocol {
        return None;
    }

    if !base.ends_with(']') {
        return None;
    }

    let prefix_len = if is_generic { 8 } else { 9 };
    let content = &base[prefix_len..base.len() - 1];
    let type_param_names: Vec<&str> = content.split(',').map(|s| s.trim()).filter(|s| !s.is_empty()).collect();

    let mut type_params = Vec::new();
    for param_name in type_param_names {
        if let Some(Type::Var(tv)) = env.lookup(param_name) {
            type_params.push(tv);
        }
    }

    if type_params.is_empty() { None } else { Some(type_params) }
}

/// Metadata extracted from a TypeVar call
#[derive(Debug)]
struct TypeVarMetadata {
    variance: Variance,
    bound: Option<Type>,
    constraints: Vec<Type>,
}

/// Extract variance, bounds, and constraints from TypeVar calls.
///
/// Parses:
/// - Variance: `covariant=True` and `contravariant=True`
/// - Bound: `bound=Animal` keyword argument
/// - Constraints: Positional arguments after the name: `TypeVar('T', int, str)`
///
/// Examples:
/// - `TypeVar('T')` → Variance::Invariant, no bound, no constraints
/// - `TypeVar('T_Co', covariant=True)` → Variance::Covariant
/// - `TypeVar('T', bound=Animal)` → bound=Animal
/// - `TypeVar('T', int, str)` → constraints=[int, str]
fn extract_typevar_metadata(
    args: &[AstNode], keywords: &[(String, AstNode)], env: &mut TypeEnvironment, ctx: &mut ConstraintGenContext,
    stub_cache: Option<&TStubCache>,
) -> Result<TypeVarMetadata> {
    let mut covariant = false;
    let mut contravariant = false;
    let mut bound = None;

    for (key, value) in keywords {
        match key.as_str() {
            "covariant" => {
                covariant = is_true_literal(value);
            }
            "contravariant" => {
                contravariant = is_true_literal(value);
            }
            "bound" => {
                let bound_ty = visit_node_with_env(value, env, ctx, stub_cache)?;
                bound = Some(bound_ty);
            }
            _ => {}
        }
    }

    let variance = match (covariant, contravariant) {
        (true, false) => Variance::Covariant,
        (false, true) => Variance::Contravariant,
        _ => Variance::Invariant,
    };

    let mut constraints = Vec::new();
    if args.len() > 1 {
        for constraint_node in &args[1..] {
            let constraint_ty = visit_node_with_env(constraint_node, env, ctx, stub_cache)?;
            constraints.push(constraint_ty);
        }
    }

    Ok(TypeVarMetadata { variance, bound, constraints })
}

/// Check if an AST node is a True literal
fn is_true_literal(node: &AstNode) -> bool {
    matches!(
        node,
        AstNode::Literal { value: beacon_parser::LiteralValue::Boolean(true), .. }
    )
}

pub fn visit_class_def(
    class_def: &AstNode, env: &mut TypeEnvironment, ctx: &mut ConstraintGenContext, stub_cache: Option<&TStubCache>,
) -> Result<Type> {
    match class_def {
        AstNode::ClassDef { name, body, decorators, bases, line, col, end_line, end_col, .. } => {
            let is_protocol = bases.iter().any(|base| {
                base == "Protocol"
                    || base.starts_with("Protocol[")
                    || base.contains(".Protocol")
                    || base == "typing.Protocol"
                    || base.starts_with("typing.Protocol[")
            });

            let class_type = if is_protocol {
                Type::Con(TypeCtor::Protocol(Some(name.clone()), vec![]))
            } else {
                Type::Con(TypeCtor::Class(name.clone()))
            };

            let mut metadata = extract_class_metadata(name, body, env);

            for base in bases {
                metadata.add_base_class(base.clone());

                if let Some(type_params) = extract_generic_type_params(base, env) {
                    metadata.type_param_vars.extend(type_params.clone());
                    metadata
                        .type_params
                        .extend(type_params.iter().filter_map(|tv| tv.hint.clone()));
                }
            }

            if is_protocol {
                metadata.set_protocol(true);
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

            if let Some(scope_id) = ctx.find_scope_at_position(*line, *col) {
                ctx.push_scope(scope_id);
            }

            for (i, stmt) in body.iter().enumerate() {
                if i == 0 && is_docstring(stmt) {
                    continue;
                }
                if !matches!(stmt, AstNode::FunctionDef { .. }) {
                    visit_node_with_env(stmt, env, ctx, stub_cache)?;
                }
            }

            ctx.pop_scope();

            let type_transforming_decorators: Vec<&String> =
                decorators.iter().filter(|d| !is_special_class_decorator(d)).collect();

            let mut decorated_type = class_type;
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
        _ => {
            todo!()
        }
    }
}

pub fn visit_function(
    fn_def: &AstNode, env: &mut TypeEnvironment, ctx: &mut ConstraintGenContext, stub_cache: Option<&TStubCache>,
) -> Result<Type> {
    match fn_def {
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

                let (ret, gen_params) = match function_kind {
                    FunctionKind::Generator => {
                        if let Some((yield_ty, send_ty, return_ty)) = annotated_ret.extract_generator_params() {
                            (
                                annotated_ret.clone(),
                                Some((yield_ty.clone(), send_ty.clone(), return_ty.clone())),
                            )
                        } else {
                            let yield_var = Type::Var(env.fresh_var());
                            let send_var = Type::Var(env.fresh_var());
                            let ret = Type::generator(yield_var.clone(), send_var.clone(), annotated_ret.clone());
                            (ret, Some((yield_var, send_var, annotated_ret)))
                        }
                    }
                    FunctionKind::AsyncGenerator => {
                        if let Some((yield_ty, send_ty)) = annotated_ret.extract_async_generator_params() {
                            (
                                annotated_ret.clone(),
                                Some((yield_ty.clone(), send_ty.clone(), Type::none())),
                            )
                        } else {
                            let yield_var = Type::Var(env.fresh_var());
                            let send_var = Type::Var(env.fresh_var());
                            let ret = Type::async_generator(yield_var.clone(), send_var.clone());
                            (ret, Some((yield_var, send_var, Type::none())))
                        }
                    }
                    FunctionKind::Coroutine => {
                        if let Some((yield_ty, send_ty, return_ty)) = annotated_ret.extract_coroutine_params() {
                            (
                                annotated_ret.clone(),
                                Some((yield_ty.clone(), send_ty.clone(), return_ty.clone())),
                            )
                        } else {
                            let yield_var = Type::Var(env.fresh_var());
                            let send_var = Type::Var(env.fresh_var());
                            let ret = Type::coroutine(yield_var.clone(), send_var.clone(), annotated_ret.clone());
                            (ret, Some((yield_var, send_var, annotated_ret)))
                        }
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
                body_env.set_generator_params(y, s, r.clone());
                body_env.set_expected_return_type(r);
            } else {
                body_env.set_expected_return_type(ret_type);
            }

            if let Some(scope_id) = ctx.find_scope_at_position(*line, *col) {
                ctx.push_scope(scope_id);
            }

            for (i, stmt) in body.iter().enumerate() {
                if i == 0 && is_docstring(stmt) {
                    continue;
                }
                visit_node_with_context(stmt, &mut body_env, ctx, stub_cache, ExprContext::Void)?;
            }

            ctx.pop_scope();

            let mut decorated_type = fn_type;
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
        _ => Err(BeaconError::from(AnalysisError::NotImplemented)),
    }
}

pub fn visit_call(
    node: &AstNode, env: &mut TypeEnvironment, ctx: &mut ConstraintGenContext, stub_cache: Option<&TStubCache>,
) -> Result<Type> {
    match node {
        AstNode::Call { function, args, keywords, line, col, end_line, end_col, .. } => {
            let function_name = function.function_to_string();
            let func_ty = if function_name.contains('.') {
                if let Some(last_dot_idx) = function_name.rfind('.') {
                    let (object_part, method_part) = function_name.split_at(last_dot_idx);
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
                    env.lookup(&function_name).unwrap_or_else(|| Type::Var(env.fresh_var()))
                }
            } else {
                env.lookup(&function_name).unwrap_or_else(|| Type::Var(env.fresh_var()))
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
        _ => Err(BeaconError::from(AnalysisError::NotImplemented)),
    }
}

pub fn visit_match(
    node: &AstNode, env: &mut TypeEnvironment, ctx: &mut ConstraintGenContext, stub_cache: Option<&TStubCache>,
) -> Result<Type> {
    match node {
        AstNode::Match { subject, cases, line, col, end_line, end_col, .. } => {
            let subject_ty = visit_node_with_env(subject, env, ctx, stub_cache)?;
            let all_patterns: Vec<(beacon_parser::Pattern, bool)> =
                cases.iter().map(|c| (c.pattern.clone(), c.guard.is_some())).collect();
            let span = Span::with_end(*line, *col, *end_line, *end_col);

            ctx.constraints
                .push(Constraint::PatternExhaustive(subject_ty.clone(), all_patterns, span));

            let subject_var =
                if let AstNode::Identifier { name, .. } = subject.as_ref() { Some(name.clone()) } else { None };

            let mut previous_patterns = Vec::new();
            for case in cases {
                let case_span = Span::with_end(case.line, case.col, case.end_line, case.end_col);
                ctx.constraints.push(Constraint::PatternReachable(
                    case.pattern.clone(),
                    previous_patterns.clone(),
                    case_span,
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

                    if let Some(guard_predicate) = extract_type_predicate(guard, &case_env) {
                        let guard_vars = extract_guard_variables(guard);

                        for var_name in guard_vars {
                            if let Some(current_type) = case_env.lookup(&var_name) {
                                let narrowed_type = guard_predicate.apply(&current_type);

                                if narrowed_type != current_type {
                                    ctx.constraints.push(Constraint::Narrowing(
                                        var_name.clone(),
                                        guard_predicate.clone(),
                                        narrowed_type.clone(),
                                        case_span,
                                    ));

                                    ctx.control_flow.narrow(var_name.clone(), narrowed_type.clone());
                                    case_env.bind(var_name, beacon_core::TypeScheme::mono(narrowed_type));
                                }
                            }
                        }
                    }
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
        _ => Err(BeaconError::from(AnalysisError::NotImplemented)),
    }
}

pub fn visit_try_raise(
    node: &AstNode, env: &mut TypeEnvironment, ctx: &mut ConstraintGenContext, stub_cache: Option<&TStubCache>,
) -> Result<Type> {
    match node {
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
        AstNode::Raise { exc, line, col, .. } => {
            if let Some(exception) = exc {
                visit_node_with_env(exception, env, ctx, stub_cache)?;
            }

            ctx.record_type(*line, *col, Type::never());
            Ok(Type::never())
        }
        _ => Err(BeaconError::from(AnalysisError::NotImplemented)),
    }
}

pub fn visit_while(
    node: &AstNode, env: &mut TypeEnvironment, ctx: &mut ConstraintGenContext, stub_cache: Option<&TStubCache>,
) -> Result<Type> {
    match node {
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
        _ => Err(BeaconError::from(AnalysisError::NotImplemented)),
    }
}

pub fn visit_for(
    node: &AstNode, env: &mut TypeEnvironment, ctx: &mut ConstraintGenContext, stub_cache: Option<&TStubCache>,
) -> Result<Type> {
    match node {
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

            for var_name in target.extract_target_names() {
                env.bind(var_name, TypeScheme::mono(element_ty.clone()));
            }

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
        _ => Err(BeaconError::from(AnalysisError::NotImplemented)),
    }
}

pub fn visit_await(
    node: &AstNode, env: &mut TypeEnvironment, ctx: &mut ConstraintGenContext, stub_cache: Option<&TStubCache>,
) -> Result<Type> {
    match node {
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
                    .push(Constraint::Equal(awaitable_ty, expected_coro_ty, span));
                result_var
            } else {
                let result_var = Type::Var(env.fresh_var());
                ctx.constraints.push(Constraint::Protocol(
                    awaitable_ty,
                    beacon_core::ProtocolName::Awaitable,
                    result_var.clone(),
                    span,
                ));
                result_var
            };

            ctx.record_type(*line, *col, result_ty.clone());
            Ok(result_ty)
        }
        _ => Err(BeaconError::from(AnalysisError::NotImplemented)),
    }
}

pub fn visit_imports(
    node: &AstNode, env: &mut TypeEnvironment, ctx: &mut ConstraintGenContext, stub_cache: Option<&TStubCache>,
) -> Result<Type> {
    match node {
        AstNode::Import { module, alias, line, col, .. } => {
            let module_name = alias.as_ref().unwrap_or(module);
            let module_type = Type::Con(TypeCtor::Module(module.clone()));
            if let Some(cache_arc) = stub_cache {
                if let Ok(cache) = cache_arc.read() {
                    if let Some(stub) = cache.get(module) {
                        if !ctx.loaded_stub_modules.contains(module) {
                            loader::load_stub_into_registry(stub, &mut ctx.class_registry, &mut ctx.typevar_registry)?;
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
                            loader::load_stub_into_registry(stub, &mut ctx.class_registry, &mut ctx.typevar_registry)?;
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
        _ => Err(BeaconError::from(AnalysisError::NotImplemented)),
    }
}

pub fn visit_module(
    node: &AstNode, env: &mut TypeEnvironment, ctx: &mut ConstraintGenContext, stub_cache: Option<&TStubCache>,
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
        _ => Err(BeaconError::from(AnalysisError::NotImplemented)),
    }
}

pub fn visit_assignments(
    node: &AstNode, env: &mut TypeEnvironment, ctx: &mut ConstraintGenContext, stub_cache: Option<&TStubCache>,
) -> Result<Type> {
    match node {
        AstNode::Assignment { target, value, line, col, .. } => {
            if let AstNode::Call { function, args, keywords, .. } = value.as_ref() {
                let function_name = function.function_to_string();
                if function_name == "TypeVar" || function_name.ends_with(".TypeVar") {
                    let metadata = extract_typevar_metadata(args, keywords, env, ctx, stub_cache)?;

                    let type_var =
                        TypeVar::with_variance(env.fresh_var().id, Some(target.target_to_string()), metadata.variance);
                    let type_var_ty = Type::Var(type_var.clone());

                    if let Some(bound) = metadata.bound {
                        ctx.typevar_registry.set_bound(type_var.id, bound);
                    }
                    if !metadata.constraints.is_empty() {
                        ctx.typevar_registry.set_constraints(type_var.id, metadata.constraints);
                    }

                    for name in target.extract_target_names() {
                        env.bind(name, TypeScheme::mono(type_var_ty.clone()));
                    }
                    ctx.record_type(*line, *col, type_var_ty.clone());
                    return Ok(type_var_ty);
                }
            }

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
        _ => Err(BeaconError::from(AnalysisError::NotImplemented)),
    }
}

pub fn visit_yield(
    node: &AstNode, env: &mut TypeEnvironment, ctx: &mut ConstraintGenContext, stub_cache: Option<&TStubCache>,
) -> Result<Type> {
    match node {
        AstNode::Yield { value, line, col, end_line, end_col, .. } => {
            let yielded_ty =
                if let Some(val) = value { visit_node_with_env(val, env, ctx, stub_cache)? } else { Type::none() };

            let result_ty = if let Some((yield_var, send_var, _return_var)) = env.get_generator_params() {
                let span = Span::with_end(*line, *col, *end_line, *end_col);
                ctx.constraints
                    .push(Constraint::Equal(yielded_ty, yield_var.clone(), span));

                send_var.clone()
            } else {
                yielded_ty
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
        _ => Err(BeaconError::from(AnalysisError::NotImplemented)),
    }
}

pub fn visit_comprehension(
    node: &AstNode, env: &mut TypeEnvironment, ctx: &mut ConstraintGenContext, stub_cache: Option<&TStubCache>,
) -> Result<Type> {
    match node {
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
        _ => Err(BeaconError::from(AnalysisError::NotImplemented)),
    }
}

pub fn visit_ops(
    node: &AstNode, env: &mut TypeEnvironment, ctx: &mut ConstraintGenContext, stub_cache: Option<&TStubCache>,
) -> Result<Type> {
    match node {
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
        _ => Err(BeaconError::from(AnalysisError::NotImplemented)),
    }
}

pub fn visit_with(
    node: &AstNode, env: &mut TypeEnvironment, ctx: &mut ConstraintGenContext, stub_cache: Option<&TStubCache>,
) -> Result<Type> {
    match node {
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
        _ => Err(BeaconError::from(AnalysisError::NotImplemented)),
    }
}

pub fn visit_lambda(
    node: &AstNode, env: &mut TypeEnvironment, ctx: &mut ConstraintGenContext, stub_cache: Option<&TStubCache>,
) -> Result<Type> {
    match node {
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
        _ => Err(BeaconError::from(AnalysisError::NotImplemented)),
    }
}

pub fn visit_if(
    node: &AstNode, env: &mut TypeEnvironment, ctx: &mut ConstraintGenContext, stub_cache: Option<&TStubCache>,
    expr_ctx: ExprContext,
) -> Result<Type> {
    match node {
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
                if let Some(refined_ty) = inverse_type.or(fallback) {
                    elif_env.bind(var_name.clone(), TypeScheme::mono(refined_ty.clone()));

                    if let Some(pred) = &predicate {
                        if pred.has_simple_negation() {
                            if let Some(inv_pred) = &inverse_predicate {
                                let span = Span::with_end(*line, *col, *end_line, *end_col);
                                ctx.constraints.push(Constraint::Narrowing(
                                    var_name.clone(),
                                    inv_pred.clone(),
                                    refined_ty,
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

            for elif_part in elif_parts {
                visit_elif_part(env, expr_ctx, elif_part, &mut elif_env, ctx, stub_cache)?;
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
        _ => Err(BeaconError::from(AnalysisError::NotImplemented)),
    }
}

fn visit_elif_part(
    env: &mut TypeEnvironment, expr_ctx: ExprContext, elif_part: &(AstNode, Vec<AstNode>),
    elif_env: &mut TypeEnvironment, ctx: &mut ConstraintGenContext, stub_cache: Option<&TStubCache>,
) -> Result<()> {
    let (elif_test, elif_body) = elif_part;

    visit_node_with_context(elif_test, elif_env, ctx, stub_cache, ExprContext::Value)?;

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

    Ok(())
}

pub fn visit_collections(
    node: &AstNode, env: &mut TypeEnvironment, ctx: &mut ConstraintGenContext, stub_cache: Option<&TStubCache>,
) -> Result<Type> {
    match node {
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
        _ => Err(BeaconError::from(AnalysisError::NotImplemented)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use beacon_parser::LiteralValue;

    #[test]
    fn test_extract_guard_variables_identifier() {
        let guard = AstNode::Identifier { name: "x".to_string(), line: 1, col: 0, end_line: 1, end_col: 1 };

        let vars = extract_guard_variables(&guard);
        assert_eq!(vars, vec!["x"]);
    }

    #[test]
    fn test_extract_guard_variables_compare() {
        let guard = AstNode::Compare {
            left: Box::new(AstNode::Identifier { name: "x".to_string(), line: 1, col: 0, end_line: 1, end_col: 1 }),
            ops: vec![beacon_parser::CompareOperator::IsNot],
            comparators: vec![AstNode::Literal { value: LiteralValue::None, line: 1, col: 5, end_line: 1, end_col: 9 }],
            line: 1,
            col: 0,
            end_line: 1,
            end_col: 9,
        };

        let vars = extract_guard_variables(&guard);
        assert_eq!(vars, vec!["x"]);
    }

    #[test]
    fn test_extract_guard_variables_isinstance() {
        let guard = AstNode::Call {
            function: Box::new(AstNode::Identifier {
                name: "isinstance".to_string(),
                line: 1,
                col: 0,
                end_line: 1,
                end_col: 10,
            }),
            args: vec![
                AstNode::Identifier { name: "x".to_string(), line: 1, col: 11, end_line: 1, end_col: 12 },
                AstNode::Identifier { name: "int".to_string(), line: 1, col: 14, end_line: 1, end_col: 17 },
            ],
            keywords: vec![],
            line: 1,
            col: 0,
            end_line: 1,
            end_col: 18,
        };

        let vars = extract_guard_variables(&guard);
        assert!(vars.contains(&"x".to_string()));
    }

    #[test]
    fn test_extract_guard_variables_binary_op() {
        let guard = AstNode::BinaryOp {
            left: Box::new(AstNode::Identifier { name: "x".to_string(), line: 1, col: 0, end_line: 1, end_col: 1 }),
            op: beacon_parser::BinaryOperator::Add,
            right: Box::new(AstNode::Identifier { name: "y".to_string(), line: 1, col: 4, end_line: 1, end_col: 5 }),
            line: 1,
            col: 0,
            end_line: 1,
            end_col: 5,
        };

        let vars = extract_guard_variables(&guard);
        assert_eq!(vars, vec!["x", "y"]);
    }

    #[test]
    fn test_extract_guard_variables_attribute() {
        let guard = AstNode::Attribute {
            object: Box::new(AstNode::Identifier { name: "obj".to_string(), line: 1, col: 0, end_line: 1, end_col: 3 }),
            attribute: "field".to_string(),
            line: 1,
            col: 0,
            end_line: 1,
            end_col: 9,
        };

        let vars = extract_guard_variables(&guard);
        assert_eq!(vars, vec!["obj"]);
    }

    #[test]
    fn test_extract_guard_variables_unary_op() {
        let guard = AstNode::UnaryOp {
            op: beacon_parser::UnaryOperator::Not,
            operand: Box::new(AstNode::Identifier { name: "x".to_string(), line: 1, col: 4, end_line: 1, end_col: 5 }),
            line: 1,
            col: 0,
            end_line: 1,
            end_col: 5,
        };

        let vars = extract_guard_variables(&guard);
        assert_eq!(vars, vec!["x"]);
    }

    #[test]
    fn test_extract_guard_variables_deduplication() {
        let guard = AstNode::BinaryOp {
            left: Box::new(AstNode::Identifier { name: "x".to_string(), line: 1, col: 0, end_line: 1, end_col: 1 }),
            op: beacon_parser::BinaryOperator::Add,
            right: Box::new(AstNode::Identifier { name: "x".to_string(), line: 1, col: 4, end_line: 1, end_col: 5 }),
            line: 1,
            col: 0,
            end_line: 1,
            end_col: 5,
        };

        let vars = extract_guard_variables(&guard);
        assert_eq!(vars, vec!["x"]);
    }

    #[test]
    fn test_extract_guard_variables_subscript() {
        let guard = AstNode::Subscript {
            value: Box::new(AstNode::Identifier { name: "arr".to_string(), line: 1, col: 0, end_line: 1, end_col: 3 }),
            slice: Box::new(AstNode::Identifier { name: "idx".to_string(), line: 1, col: 4, end_line: 1, end_col: 7 }),
            line: 1,
            col: 0,
            end_line: 1,
            end_col: 8,
        };

        let vars = extract_guard_variables(&guard);
        assert_eq!(vars, vec!["arr", "idx"]);
    }

    #[test]
    fn test_extract_guard_variables_literal_no_vars() {
        let guard = AstNode::Literal { value: LiteralValue::Integer(42), line: 1, col: 0, end_line: 1, end_col: 2 };

        let vars = extract_guard_variables(&guard);
        assert!(vars.is_empty());
    }
}

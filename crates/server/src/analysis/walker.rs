use super::constraint_gen::{Constraint, ConstraintGenContext, ConstraintResult, ConstraintSet, Span};
use super::pattern::extract_pattern_bindings;
use super::type_env::TypeEnvironment;
use super::{class_metadata::ClassMetadata, loader};

use beacon_core::{Type, TypeScheme, errors::Result};
use beacon_core::{TypeCtor, TypeVarGen};
use beacon_parser::{AstNode, LiteralValue, SymbolTable};
use std::sync::Arc;

type TStubCache = Arc<std::sync::RwLock<crate::workspace::StubCache>>;

pub fn generate_constraints(
    stub_cache: &Option<TStubCache>, ast: &AstNode, symbol_table: &SymbolTable,
) -> Result<ConstraintResult> {
    let mut ctx = ConstraintGenContext::new();

    if let Some(stub_cache) = &stub_cache {
        if let Ok(cache) = stub_cache.try_read() {
            if let Some(builtins) = cache.get("builtins") {
                loader::load_builtins_into_registry(&builtins.path, &mut ctx.class_registry)?;
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
    match node {
        AstNode::Module { body, .. } => {
            for stmt in body {
                visit_node_with_env(stmt, env, ctx, stub_cache)?;
            }
            Ok(Type::Con(TypeCtor::Module("".into())))
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
                body_ty = visit_node_with_env(stmt, &mut body_env, ctx, stub_cache)?;
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
            let class_type = Type::Con(TypeCtor::Class(name.clone()));
            let metadata = extract_class_metadata(name, body, env);
            ctx.class_registry.register_class(name.clone(), metadata);

            for stmt in body {
                if !matches!(stmt, AstNode::FunctionDef { .. }) {
                    visit_node_with_env(stmt, env, ctx, stub_cache)?;
                }
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
            let value_ty = visit_node_with_env(value, env, ctx, stub_cache)?;
            env.bind(target.clone(), TypeScheme::mono(value_ty.clone()));
            ctx.record_type(*line, *col, value_ty.clone());
            Ok(value_ty)
        }
        AstNode::AnnotatedAssignment { target, type_annotation, value, line, col } => {
            let annotated_ty = env.parse_annotation_or_any(type_annotation);
            if let Some(val) = value {
                let value_ty = visit_node_with_env(val, env, ctx, stub_cache)?;
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
                arg_types.push(visit_node_with_env(arg, env, ctx, stub_cache)?);
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
                LiteralValue::String { .. } => Type::string(),
                LiteralValue::Boolean(_) => Type::bool(),
                LiteralValue::None => Type::none(),
            };
            ctx.record_type(*line, *col, ty.clone());
            Ok(ty)
        }
        AstNode::Return { value, line, col } => {
            let ty = if let Some(val) = value { visit_node_with_env(val, env, ctx, stub_cache)? } else { Type::none() };
            ctx.record_type(*line, *col, ty.clone());
            Ok(ty)
        }
        AstNode::Attribute { object, attribute, line, col } => {
            let obj_ty = visit_node_with_env(object, env, ctx, stub_cache)?;
            let attr_ty = Type::Var(env.fresh_var());
            let span = Span::new(*line, *col);
            ctx.constraints
                .push(Constraint::HasAttr(obj_ty, attribute.clone(), attr_ty.clone(), span));
            ctx.record_type(*line, *col, attr_ty.clone());
            Ok(attr_ty)
        }
        AstNode::BinaryOp { left, right, line, col, .. } => {
            let left_ty = visit_node_with_env(left, env, ctx, stub_cache)?;
            let right_ty = visit_node_with_env(right, env, ctx, stub_cache)?;
            let span = Span::new(*line, *col);
            ctx.constraints.push(Constraint::Equal(left_ty.clone(), right_ty, span));
            ctx.record_type(*line, *col, left_ty.clone());
            Ok(left_ty)
        }
        AstNode::UnaryOp { operand, line, col, .. } => {
            let ty = visit_node_with_env(operand, env, ctx, stub_cache)?;
            ctx.record_type(*line, *col, ty.clone());
            Ok(ty)
        }
        AstNode::Subscript { value, slice, line, col } => {
            let value_ty = visit_node_with_env(value, env, ctx, stub_cache)?;

            visit_node_with_env(slice, env, ctx, stub_cache)?;

            let result_ty = Type::Var(env.fresh_var());
            let span = Span::new(*line, *col);

            ctx.constraints.push(Constraint::HasAttr(
                value_ty,
                "__getitem__".to_string(),
                result_ty.clone(),
                span,
            ));

            ctx.record_type(*line, *col, result_ty.clone());
            Ok(result_ty)
        }
        AstNode::If { test, body, elif_parts, else_body, .. } => {
            visit_node_with_env(test, env, ctx, stub_cache)?;

            let (narrowed_var, narrowed_type) = detect_type_guard(test, &mut env.clone());

            let mut true_env = env.clone();
            if let (Some(var_name), Some(refined_ty)) = (narrowed_var.as_ref(), narrowed_type.as_ref()) {
                true_env.bind(var_name.clone(), TypeScheme::mono(refined_ty.clone()));
            }

            for stmt in body {
                visit_node_with_env(stmt, &mut true_env, ctx, stub_cache)?;
            }

            for (elif_test, elif_body) in elif_parts {
                visit_node_with_env(elif_test, env, ctx, stub_cache)?;
                for stmt in elif_body {
                    visit_node_with_env(stmt, env, ctx, stub_cache)?;
                }
            }
            if let Some(else_stmts) = else_body {
                for stmt in else_stmts {
                    visit_node_with_env(stmt, env, ctx, stub_cache)?;
                }
            }
            Ok(Type::none())
        }
        AstNode::Import { module, alias, line, col } => {
            let module_name = alias.as_ref().unwrap_or(module);
            let module_type = Type::Con(TypeCtor::Module(module.clone()));
            env.bind(module_name.clone(), TypeScheme::mono(module_type.clone()));
            ctx.record_type(*line, *col, module_type.clone());
            Ok(module_type)
        }
        AstNode::ImportFrom { module, names, line, col } => {
            if let Some(cache_arc) = stub_cache {
                if let Ok(cache) = cache_arc.read() {
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
        AstNode::For { target, iter, body, else_body, line, col } => {
            let iter_ty = visit_node_with_env(iter, env, ctx, stub_cache)?;

            let element_ty = Type::Var(env.fresh_var());
            let span = Span::new(*line, *col);
            ctx.constraints.push(Constraint::Protocol(
                iter_ty,
                beacon_core::ProtocolName::Iterable,
                element_ty.clone(),
                span,
            ));

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

        AstNode::While { test, body, else_body, line, col } => {
            visit_node_with_env(test, env, ctx, stub_cache)?;

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

        AstNode::Try { body, handlers, else_body, finally_body, line, col } => {
            for stmt in body {
                visit_node_with_env(stmt, env, ctx, stub_cache)?;
            }

            for handler in handlers {
                let mut handler_env = env.clone();
                if let Some(ref name) = handler.name {
                    // TODO: Use proper exception type hierarchy when available
                    let exc_ty = Type::Var(env.fresh_var());
                    handler_env.bind(name.clone(), TypeScheme::mono(exc_ty));
                }

                for stmt in &handler.body {
                    visit_node_with_env(stmt, &mut handler_env, ctx, stub_cache)?;
                }
            }

            if let Some(else_stmts) = else_body {
                for stmt in else_stmts {
                    visit_node_with_env(stmt, env, ctx, stub_cache)?;
                }
            }

            if let Some(finally_stmts) = finally_body {
                for stmt in finally_stmts {
                    visit_node_with_env(stmt, env, ctx, stub_cache)?;
                }
            }

            ctx.record_type(*line, *col, Type::none());
            Ok(Type::none())
        }

        AstNode::Match { subject, cases, line, col } => {
            let subject_ty = visit_node_with_env(subject, env, ctx, stub_cache)?;
            let all_patterns: Vec<beacon_parser::Pattern> = cases.iter().map(|c| c.pattern.clone()).collect();
            let span = Span::new(*line, *col);

            ctx.constraints.push(Constraint::PatternExhaustive(
                subject_ty.clone(),
                all_patterns.clone(),
                span,
            ));

            let mut previous_patterns = Vec::new();
            for case in cases {
                ctx.constraints.push(Constraint::PatternReachable(
                    case.pattern.clone(),
                    previous_patterns.clone(),
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

                for (var_name, var_type) in bindings {
                    case_env.bind(var_name, beacon_core::TypeScheme::mono(var_type));
                }

                if let Some(ref guard) = case.guard {
                    visit_node_with_env(guard, &mut case_env, ctx, stub_cache)?;
                }

                for stmt in &case.body {
                    visit_node_with_env(stmt, &mut case_env, ctx, stub_cache)?;
                }

                previous_patterns.push(case.pattern.clone());
            }

            ctx.record_type(*line, *col, Type::none());
            Ok(Type::none())
        }
        AstNode::Raise { exc, line, col } => {
            if let Some(exception) = exc {
                visit_node_with_env(exception, env, ctx, stub_cache)?;
            }

            ctx.record_type(*line, *col, Type::never());
            Ok(Type::never())
        }

        AstNode::With { items, body, line, col } => {
            for item in items {
                let context_ty = visit_node_with_env(&item.context_expr, env, ctx, stub_cache)?;

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
                visit_node_with_env(stmt, env, ctx, stub_cache)?;
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

            let body_ty = visit_node_with_env(body, &mut lambda_env, ctx, stub_cache)?;
            let lambda_ty = Type::fun(param_types, body_ty);

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
        AstNode::NamedExpr { target, value, line, col } => {
            let value_ty = visit_node_with_env(value, env, ctx, stub_cache)?;
            env.bind(target.clone(), TypeScheme::mono(value_ty.clone()));
            ctx.record_type(*line, *col, value_ty.clone());
            Ok(value_ty)
        }
        AstNode::ListComp { element, generators, line, col } => {
            let mut comp_env = env.clone();

            for generator in generators {
                let iter_ty = visit_node_with_env(&generator.iter, &mut comp_env, ctx, stub_cache)?;

                let element_ty = Type::Var(comp_env.fresh_var());
                let span = Span::new(*line, *col);
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

        AstNode::SetComp { element, generators, line, col } => {
            let mut comp_env = env.clone();

            for generator in generators {
                let iter_ty = visit_node_with_env(&generator.iter, &mut comp_env, ctx, stub_cache)?;

                let element_ty = Type::Var(comp_env.fresh_var());
                let span = Span::new(*line, *col);
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

        AstNode::DictComp { key, value, generators, line, col } => {
            let mut comp_env = env.clone();

            for generator in generators {
                let iter_ty = visit_node_with_env(&generator.iter, &mut comp_env, ctx, stub_cache)?;

                let element_ty = Type::Var(comp_env.fresh_var());
                let span = Span::new(*line, *col);
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
        // NOTE: Approximated as iterable[T] rather than proper Generator[T, None, None]
        // TODO: See ROADMAP.md for Generator[YieldType, SendType, ReturnType] modeling
        AstNode::GeneratorExp { element, generators, line, col } => {
            let mut comp_env = env.clone();

            for generator in generators {
                let iter_ty = visit_node_with_env(&generator.iter, &mut comp_env, ctx, stub_cache)?;

                let element_ty = Type::Var(comp_env.fresh_var());
                let span = Span::new(*line, *col);
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
            let generator_ty = Type::list(elem_ty);

            ctx.record_type(*line, *col, generator_ty.clone());
            Ok(generator_ty)
        }
        AstNode::Tuple { .. } | AstNode::Pass { .. } | AstNode::Break { .. } | AstNode::Continue { .. } => {
            Ok(Type::none())
        }
    }
}

/// Extract class metadata from a ClassDef node
///
/// Scans the class body for __init__ method and field assignments to build [ClassMetadata].
/// This metadata is used during [Constraint::HasAttr] constraint solving to resolve attribute types.
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
        if let AstNode::FunctionDef { name: method_name, args, return_type, body: method_body, decorators, .. } = stmt {
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

            let has_property = decorators.iter().any(|d| d == "property");
            let has_staticmethod = decorators.iter().any(|d| d == "staticmethod");
            let has_classmethod = decorators.iter().any(|d| d == "classmethod");

            if method_name == "__init__" {
                let method_type = Type::fun(param_types.clone(), ret_type);
                metadata.set_init_type(method_type);

                for body_stmt in method_body {
                    extract_field_assignments(body_stmt, &mut metadata, env);
                }
            } else if method_name == "__new__" {
                metadata.set_new_type(Type::fun(param_types.clone(), ret_type));
            } else if has_property {
                metadata.add_property(method_name.clone(), ret_type);
            } else if has_staticmethod {
                let static_param_types: Vec<Type> = if !param_types.is_empty() {
                    param_types.iter().skip(1).cloned().collect()
                } else {
                    param_types.clone()
                };
                metadata.add_staticmethod(method_name.clone(), Type::fun(static_param_types, ret_type));
            } else if has_classmethod {
                metadata.add_classmethod(method_name.clone(), Type::fun(param_types.clone(), ret_type));
            } else {
                metadata.add_method(method_name.clone(), Type::fun(param_types.clone(), ret_type));
            }
        }
    }

    metadata
}

/// Recursively extract field assignments from statement nodes
///
/// Looks for patterns like `self.field = value` or `self.field: Type = value` and registers the field in [ClassMetadata].
fn extract_field_assignments(stmt: &AstNode, metadata: &mut ClassMetadata, env: &mut TypeEnvironment) {
    match stmt {
        AstNode::Assignment { target, .. } => {
            if let Some(field_name) = target.strip_prefix("self.") {
                let field_type = Type::Var(env.fresh_var());
                metadata.add_field(field_name.to_string(), field_type);
            }
        }
        AstNode::AnnotatedAssignment { target, type_annotation, .. } => {
            if let Some(field_name) = target.strip_prefix("self.") {
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

/// Detect type guard patterns for flow-sensitive type narrowing
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
                    let refined_type = type_name_to_type(type_name);
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
        "list" => Type::Con(TypeCtor::List),
        "dict" => Type::Con(TypeCtor::Dict),
        "set" => Type::Con(TypeCtor::Set),
        "tuple" => Type::Con(TypeCtor::Tuple),
        _ => Type::Var(TypeVarGen::new().fresh()),
    }
}

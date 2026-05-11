use super::{
    compat::{function_compatible, generator_iterable_subst, types_compatible},
    state::SolveState,
};
use crate::{Span, TypeErrorInfo};

use beacon_core::{BeaconError, ClassMetadata, ClassRegistry, Subst, Type, TypeCtor, TypeError, TypeVar, Unifier};
use std::{collections::HashSet, result};

pub(super) struct CallContext<'a> {
    pub(super) subst: &'a mut Subst,
    pub(super) type_errors: &'a mut Vec<TypeErrorInfo>,
    pub(super) class_registry: &'a ClassRegistry,
    pub(super) typevar_registry: &'a beacon_core::TypeVarConstraintRegistry,
    pub(super) span: Span,
}

pub(super) fn merge_and_validate_args(
    pos_args: &[(Type, Span)], kw_args: &[(String, Type, Span)], params: &[(String, Type)], skip_first: bool,
) -> result::Result<Vec<(Type, Span, String)>, String> {
    let effective_params: Vec<_> =
        if skip_first && !params.is_empty() { params[1..].to_vec() } else { params.to_vec() };

    let mut seen_keywords = HashSet::new();
    for (kw_name, _, _) in kw_args {
        if !seen_keywords.insert(kw_name) {
            return Err(format!("duplicate keyword argument: '{kw_name}'"));
        }
    }

    let param_names: HashSet<_> = effective_params.iter().map(|(n, _)| n.as_str()).collect();
    for (kw_name, _, _) in kw_args {
        if !param_names.contains(kw_name.as_str()) {
            return Err(format!("unexpected keyword argument: '{kw_name}'"));
        }
    }

    for (i, (param_name, _)) in effective_params.iter().enumerate() {
        if i < pos_args.len() {
            for (kw_name, _, _) in kw_args {
                if kw_name == param_name {
                    return Err(format!(
                        "argument '{param_name}' specified both positionally and as keyword"
                    ));
                }
            }
        }
    }

    if pos_args.len() > effective_params.len() {
        return Err(format!(
            "too many positional arguments: expected at most {}, got {}",
            effective_params.len(),
            pos_args.len()
        ));
    }

    let mut merged = Vec::new();
    for (i, (param_name, _param_ty)) in effective_params.iter().enumerate() {
        if i < pos_args.len() {
            let (arg_ty, arg_span) = &pos_args[i];
            merged.push((arg_ty.clone(), *arg_span, param_name.clone()));
        } else if let Some((_, kw_ty, kw_span)) = kw_args.iter().find(|(name, _, _)| name == param_name) {
            merged.push((kw_ty.clone(), *kw_span, param_name.clone()));
        } else {
            break;
        }
    }

    Ok(merged)
}

pub(super) fn instantiate_class_type(metadata: &ClassMetadata, class_name: &str, subst: &Subst) -> Type {
    let mut class_ty = Type::Con(TypeCtor::Class(class_name.to_string()));
    if !metadata.type_param_vars.is_empty() {
        for tv in &metadata.type_param_vars {
            let arg = subst.apply(&Type::Var(tv.clone()));
            class_ty = Type::App(Box::new(class_ty), Box::new(arg));
        }
    }
    class_ty
}

/// Simplify all types in a substitution by applying normalization rules
///
/// This performs post-processing after constraint solving to simplify union types that may have been created during unification.
pub(super) fn simplify_substitution(subst: Subst) -> Subst {
    let simplified_pairs: Vec<(TypeVar, Type)> = subst
        .iter()
        .map(|(tv, ty)| (tv.clone(), ty.clone().simplify()))
        .collect();

    let mut result = Subst::empty();
    for (tv, simplified_ty) in simplified_pairs {
        result.insert(tv, simplified_ty);
    }
    result
}

pub(super) fn handle_call_args(
    ctx: &mut CallContext<'_>, pos_args: &[(Type, Span)], kw_args: &[(String, Type, Span)], params: &[(String, Type)],
    has_bound_receiver: bool,
) {
    match merge_and_validate_args(pos_args, kw_args, params, has_bound_receiver) {
        Ok(merged_args) => {
            let expected_params: Vec<(String, Type)> =
                if has_bound_receiver { params.iter().skip(1).cloned().collect() } else { params.to_vec() };

            if merged_args.len() <= expected_params.len() {
                for ((provided_ty, arg_span, param_name), (_pname, expected_param_ty)) in
                    merged_args.iter().zip(expected_params.iter())
                {
                    let provided_ty = ctx.subst.apply(provided_ty);
                    let expected_ty = ctx.subst.apply(expected_param_ty);

                    if function_compatible(&provided_ty, &expected_ty, ctx.class_registry, ctx.typevar_registry) {
                        continue;
                    }

                    match Unifier::unify_with_class_registry(
                        &provided_ty,
                        &expected_ty,
                        ctx.typevar_registry,
                        ctx.class_registry,
                    ) {
                        Ok(s) => {
                            *ctx.subst = s.compose(ctx.subst.clone());
                        }
                        Err(BeaconError::TypeError(_)) => {
                            if let Some(s) = generator_iterable_subst(&provided_ty, &expected_ty, ctx.typevar_registry)
                            {
                                *ctx.subst = s.compose(ctx.subst.clone());
                            } else if !types_compatible(
                                &provided_ty,
                                &expected_ty,
                                ctx.class_registry,
                                ctx.typevar_registry,
                            ) {
                                ctx.type_errors.push(TypeErrorInfo::new(
                                    TypeError::ArgumentTypeMismatch {
                                        param_name: param_name.clone(),
                                        expected: expected_ty.display_for_diagnostics(),
                                        found: provided_ty.display_for_diagnostics(),
                                    },
                                    *arg_span,
                                ));
                            }
                        }
                        Err(_) => {}
                    }
                }
            } else {
                ctx.type_errors.push(TypeErrorInfo::new(
                    TypeError::ArgumentCountMismatch { expected: expected_params.len(), found: merged_args.len() },
                    ctx.span,
                ));
            }
        }
        Err(error_msg) => {
            ctx.type_errors
                .push(TypeErrorInfo::new(TypeError::KeywordArgumentError(error_msg), ctx.span));
        }
    }
}

pub(super) fn unify_return_type(ctx: &mut CallContext<'_>, call_ret_ty: &Type, target_ty: &Type) {
    match Unifier::unify_with_class_registry(
        &ctx.subst.apply(call_ret_ty),
        &ctx.subst.apply(target_ty),
        ctx.typevar_registry,
        ctx.class_registry,
    ) {
        Ok(s) => *ctx.subst = s.compose(ctx.subst.clone()),
        Err(BeaconError::TypeError(type_err)) => ctx.type_errors.push(TypeErrorInfo::new(type_err, ctx.span)),
        Err(_) => (),
    }
}

pub(super) fn unify_with_adhoc_fun(
    ctx: &mut CallContext<'_>, callable: &Type, pos_args: &[(Type, Span)], kw_args: &[(String, Type, Span)],
    ret_ty: &Type,
) {
    let all_args: Vec<Type> = pos_args
        .iter()
        .map(|(ty, _)| ty.clone())
        .chain(kw_args.iter().map(|(_, ty, _)| ty.clone()))
        .collect();

    let expected_fn_ty = Type::fun_unnamed(all_args, ret_ty.clone());

    match Unifier::unify_with_class_registry(
        &ctx.subst.apply(callable),
        &ctx.subst.apply(&expected_fn_ty),
        ctx.typevar_registry,
        ctx.class_registry,
    ) {
        Ok(s) => {
            *ctx.subst = s.compose(ctx.subst.clone());
        }
        Err(BeaconError::TypeError(type_err)) => {
            ctx.type_errors.push(TypeErrorInfo::new(type_err, ctx.span));
        }
        Err(_) => {}
    }
}

/// Re-resolve a bound method type against receiver metadata.
/// Falls back to the original method if we can't do better.
///
/// For generic classes and protocols with type parameters, the method type
/// should already have substitution applied when the BoundMethod was created,
/// so we should just use that directly.
pub(super) fn resolve_bound_method_type<'a>(
    rec: Box<Type>, name: &str, method: &'a Type, pos_args: &[(Type, Span)], subst: &Subst, reg: &'a ClassRegistry,
) -> &'a Type {
    if rec.unapply_protocol().is_some() || rec.unapply_class().is_some() {
        return method;
    }

    if let Type::Con(TypeCtor::Class(class_name)) = rec.as_ref()
        && let Some(metadata) = reg.get_class(class_name)
        && let Some(method_type) = metadata.lookup_method_type(name)
    {
        let applied_args: Vec<Type> = pos_args.iter().map(|(arg_ty, _)| subst.apply(arg_ty)).collect();
        return method_type.resolve_for_args(&applied_args).unwrap_or(method);
    }
    method
}

pub(super) fn solve_call_constraint(
    func_ty: Type, pos_args: Vec<(Type, Span)>, kw_args: Vec<(String, Type, Span)>, ret_ty: Type, span: Span,
    state: &mut SolveState<'_>,
) {
    let applied_func = state.subst.apply(&func_ty);

    if let Type::Con(TypeCtor::Class(class_name)) = &applied_func {
        if let Some(metadata) = state.class_registry.get_class(class_name) {
            if let Some(Type::Fun(params, _)) = metadata.new_type.as_ref().or(metadata.init_type.as_ref()) {
                let mut ctx = CallContext {
                    subst: &mut *state.subst,
                    type_errors: state.type_errors,
                    class_registry: state.class_registry,
                    typevar_registry: state.typevar_registry,
                    span,
                };
                handle_call_args(&mut ctx, &pos_args, &kw_args, params, true);
            }

            let class_result_ty = instantiate_class_type(metadata, class_name, state.subst);
            let mut ctx = CallContext {
                subst: &mut *state.subst,
                type_errors: state.type_errors,
                class_registry: state.class_registry,
                typevar_registry: state.typevar_registry,
                span,
            };
            unify_return_type(&mut ctx, &ret_ty, &class_result_ty);
        }
    } else if let Type::BoundMethod(receiver, method_name, method) = &applied_func {
        let resolved_method = resolve_bound_method_type(
            receiver.clone(),
            method_name,
            method,
            &pos_args,
            state.subst,
            state.class_registry,
        );

        if let Type::Fun(params, method_ret) = resolved_method {
            let mut ctx = CallContext {
                subst: &mut *state.subst,
                type_errors: state.type_errors,
                class_registry: state.class_registry,
                typevar_registry: state.typevar_registry,
                span,
            };

            handle_call_args(&mut ctx, &pos_args, &kw_args, params, true);
            unify_return_type(&mut ctx, &ret_ty, method_ret);
        } else {
            let mut ctx = CallContext {
                subst: &mut *state.subst,
                type_errors: state.type_errors,
                class_registry: state.class_registry,
                typevar_registry: state.typevar_registry,
                span,
            };
            unify_with_adhoc_fun(&mut ctx, resolved_method, &pos_args, &kw_args, &ret_ty);
        }
    } else {
        let applied_func = state.subst.apply(&func_ty);
        if let Type::Fun(params, fn_ret) = &applied_func {
            let mut ctx = CallContext {
                subst: &mut *state.subst,
                type_errors: state.type_errors,
                class_registry: state.class_registry,
                typevar_registry: state.typevar_registry,
                span,
            };
            handle_call_args(&mut ctx, &pos_args, &kw_args, params, false);
            unify_return_type(&mut ctx, &ret_ty, fn_ret);
        } else {
            let mut ctx = CallContext {
                subst: &mut *state.subst,
                type_errors: state.type_errors,
                class_registry: state.class_registry,
                typevar_registry: state.typevar_registry,
                span,
            };
            unify_with_adhoc_fun(&mut ctx, &applied_func, &pos_args, &kw_args, &ret_ty);
        }
    }
}

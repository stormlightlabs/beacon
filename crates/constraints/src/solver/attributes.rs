use super::compat::{check_has_attribute, extract_base_constructor, get_attribute_type};
use super::state::SolveState;
use crate::{Span, TypeErrorInfo};

use beacon_core::{BeaconError, Type, TypeCtor, TypeError, TypeVar, Unifier};

pub(super) fn solve_attribute_constraint(
    obj_ty: Type, attr_name: String, attr_ty: Type, span: Span, state: &mut SolveState<'_>,
) {
    let applied_obj = state.subst.apply(&obj_ty);
    if let Type::Union(variants) = &applied_obj {
        let mut all_have_attr = true;
        let mut attr_types = Vec::new();

        for variant in variants {
            let has_attr = check_has_attribute(variant, &attr_name, state.class_registry);
            if !has_attr {
                all_have_attr = false;
                break;
            }

            if let Some(resolved_attr_ty) = get_attribute_type(variant, &attr_name, state.class_registry) {
                attr_types.push(resolved_attr_ty);
            }
        }

        if !all_have_attr {
            state.type_errors.push(TypeErrorInfo::new(
                beacon_core::TypeError::AttributeNotFound(applied_obj.display_for_diagnostics(), attr_name.clone()),
                span,
            ));
        } else if !attr_types.is_empty() {
            let attr_union =
                if attr_types.len() == 1 { attr_types.into_iter().next().unwrap() } else { Type::union(attr_types) };

            match Unifier::unify_with_class_registry(
                &state.subst.apply(&attr_ty),
                &attr_union,
                state.typevar_registry,
                state.class_registry,
            ) {
                Ok(s) => {
                    *state.subst = s.compose(state.subst.clone());
                }
                Err(BeaconError::TypeError(type_err)) => {
                    state.type_errors.push(TypeErrorInfo::new(type_err, span));
                }
                Err(_) => {}
            }
        }
        return;
    }

    match &applied_obj {
        Type::Con(TypeCtor::Protocol(Some(protocol_name), variances)) if attr_name == "__getitem__" => {
            let type_param = Type::Var(TypeVar::new(999999));
            let result_ty = Type::App(
                Box::new(Type::Con(TypeCtor::Protocol(
                    Some(protocol_name.clone()),
                    variances.clone(),
                ))),
                Box::new(type_param.clone()),
            );
            let getitem_ty = Type::Fun(vec![("item".to_string(), type_param)], Box::new(result_ty));

            match Unifier::unify_with_class_registry(
                &state.subst.apply(&attr_ty),
                &getitem_ty,
                state.typevar_registry,
                state.class_registry,
            ) {
                Ok(s) => {
                    *state.subst = s.compose(state.subst.clone());
                }
                Err(BeaconError::TypeError(type_err)) => {
                    state.type_errors.push(TypeErrorInfo::new(type_err, span));
                }
                Err(_) => {}
            }
        }
        Type::Con(TypeCtor::Class(class_name)) => {
            if let Some(resolved_attr_ty) = state
                .class_registry
                .lookup_attribute_with_inheritance(class_name, &attr_name)
            {
                let final_type = if state.class_registry.is_method(class_name, &attr_name) {
                    Type::BoundMethod(
                        Box::new(applied_obj.clone()),
                        attr_name.clone(),
                        Box::new(resolved_attr_ty.clone()),
                    )
                } else {
                    resolved_attr_ty.clone()
                };

                match Unifier::unify_with_class_registry(
                    &state.subst.apply(&attr_ty),
                    &final_type,
                    state.typevar_registry,
                    state.class_registry,
                ) {
                    Ok(s) => {
                        *state.subst = s.compose(state.subst.clone());
                    }
                    Err(BeaconError::TypeError(type_err)) => {
                        state.type_errors.push(TypeErrorInfo::new(type_err, span));
                    }
                    Err(_) => {}
                }
            } else {
                state.type_errors.push(TypeErrorInfo::new(
                    beacon_core::TypeError::AttributeNotFound(applied_obj.to_string(), attr_name.clone()),
                    span,
                ));
            }
        }
        Type::Con(type_ctor) => {
            let class_name = match type_ctor {
                TypeCtor::String => Some("str"),
                TypeCtor::Int => Some("int"),
                TypeCtor::Float => Some("float"),
                TypeCtor::Bool => Some("bool"),
                TypeCtor::List => Some("list"),
                TypeCtor::Dict => Some("dict"),
                TypeCtor::Set => Some("set"),
                TypeCtor::Tuple => Some("tuple"),
                TypeCtor::Any => {
                    if let Ok(s) = Unifier::unify_with_class_registry(
                        &state.subst.apply(&attr_ty),
                        &Type::any(),
                        state.typevar_registry,
                        state.class_registry,
                    ) {
                        *state.subst = s.compose(state.subst.clone());
                    }
                    None
                }
                _ => None,
            };

            if let Some(class_name) = class_name {
                if let Some(resolved_attr_ty) = state
                    .class_registry
                    .lookup_attribute_with_inheritance(class_name, &attr_name)
                {
                    let final_type = if state.class_registry.is_method(class_name, &attr_name) {
                        Type::BoundMethod(
                            Box::new(applied_obj.clone()),
                            attr_name.clone(),
                            Box::new(resolved_attr_ty.clone()),
                        )
                    } else {
                        resolved_attr_ty.clone()
                    };

                    match Unifier::unify_with_class_registry(
                        &state.subst.apply(&attr_ty),
                        &final_type,
                        state.typevar_registry,
                        state.class_registry,
                    ) {
                        Ok(s) => {
                            *state.subst = s.compose(state.subst.clone());
                        }
                        Err(BeaconError::TypeError(type_err)) => {
                            state.type_errors.push(TypeErrorInfo::new(type_err, span));
                        }
                        Err(_) => {}
                    }
                } else {
                    state.type_errors.push(TypeErrorInfo::new(
                        TypeError::AttributeNotFound(applied_obj.to_string(), attr_name.clone()),
                        span,
                    ));
                }
            }
        }
        Type::App(_, _) => {
            let (resolved_attr_ty, is_method, _class_name_opt) =
                if let Some((protocol_name, type_args)) = applied_obj.unapply_protocol() {
                    if let Some(attr_type) = state
                        .class_registry
                        .lookup_attribute_with_inheritance(protocol_name, &attr_name)
                    {
                        let substituted_ty =
                            if let Some(protocol_metadata) = state.class_registry.get_class(protocol_name) {
                                if !protocol_metadata.type_params.is_empty() {
                                    let subst_map = protocol_metadata.create_type_substitution(&type_args);
                                    beacon_core::ClassMetadata::substitute_type_params(&attr_type, &subst_map)
                                } else {
                                    attr_type.clone()
                                }
                            } else {
                                attr_type.clone()
                            };
                        let is_method = state.class_registry.is_method(protocol_name, &attr_name);
                        (Some(substituted_ty), is_method, Some(protocol_name.to_string()))
                    } else {
                        (None, false, None)
                    }
                } else if let Some((class_name, type_args)) = applied_obj.unapply_class() {
                    if let Some(attr_type) = state
                        .class_registry
                        .lookup_attribute_with_inheritance(class_name, &attr_name)
                    {
                        let substituted_ty = if let Some(class_metadata) = state.class_registry.get_class(class_name) {
                            if !class_metadata.type_params.is_empty() {
                                let subst_map = class_metadata.create_type_substitution(&type_args);
                                beacon_core::ClassMetadata::substitute_type_params(&attr_type, &subst_map)
                            } else {
                                attr_type.clone()
                            }
                        } else {
                            attr_type.clone()
                        };
                        let is_method = state.class_registry.is_method(class_name, &attr_name);
                        (Some(substituted_ty), is_method, Some(class_name.to_string()))
                    } else {
                        (None, false, None)
                    }
                } else if let Some(resolved) = get_attribute_type(&applied_obj, &attr_name, state.class_registry) {
                    let base_ctor = extract_base_constructor(&applied_obj);
                    let is_method = base_ctor
                        .map(|name| state.class_registry.is_method(name, &attr_name))
                        .unwrap_or(false);
                    (Some(resolved), is_method, base_ctor.map(String::from))
                } else {
                    (None, false, None)
                };

            if let Some(resolved_ty) = resolved_attr_ty {
                let final_type = if is_method {
                    Type::BoundMethod(
                        Box::new(applied_obj.clone()),
                        attr_name.clone(),
                        Box::new(resolved_ty.clone()),
                    )
                } else {
                    resolved_ty.clone()
                };

                match Unifier::unify_with_class_registry(
                    &state.subst.apply(&attr_ty),
                    &final_type,
                    state.typevar_registry,
                    state.class_registry,
                ) {
                    Ok(s) => {
                        *state.subst = s.compose(state.subst.clone());
                    }
                    Err(BeaconError::TypeError(type_err)) => {
                        state.type_errors.push(TypeErrorInfo::new(type_err, span));
                    }
                    Err(_) => {}
                }
            } else {
                state.type_errors.push(TypeErrorInfo::new(
                    beacon_core::TypeError::AttributeNotFound(applied_obj.to_string(), attr_name.clone()),
                    span,
                ));
            }
        }
        Type::Var(_) => {}
        _ => {
            state.type_errors.push(TypeErrorInfo::new(
                beacon_core::TypeError::AttributeNotFound(applied_obj.display_for_diagnostics(), attr_name.clone()),
                span,
            ));
        }
    }
}

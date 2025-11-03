use crate::{
    Constraint, ConstraintSet, TypeErrorInfo,
    exhaustiveness::{ExhaustivenessResult, ReachabilityResult, check_exhaustiveness, check_reachability},
};

use beacon_core::{
    ClassRegistry, MethodSignature, ProtocolChecker, ProtocolName, Result, Subst, Type, TypeCtor, TypeError, TypeVar,
    Unifier,
};

/// Extract the base type constructor from a type application
///
/// For example, `dict[str, int]` is represented as `App(App(Dict, str), int)`, and this function extracts "dict".
fn extract_base_constructor(ty: &Type) -> Option<&str> {
    match ty {
        Type::Con(TypeCtor::String) => Some("str"),
        Type::Con(TypeCtor::Int) => Some("int"),
        Type::Con(TypeCtor::Float) => Some("float"),
        Type::Con(TypeCtor::Bool) => Some("bool"),
        Type::Con(TypeCtor::List) => Some("list"),
        Type::Con(TypeCtor::Dict) => Some("dict"),
        Type::Con(TypeCtor::Set) => Some("set"),
        Type::Con(TypeCtor::Tuple) => Some("tuple"),
        Type::App(base, _) => extract_base_constructor(base),
        _ => None,
    }
}

/// Convert a [Type::Fun] to a [MethodSignature] by extracting parameters and return types from a function type.
fn type_to_method_signature(name: &str, ty: &Type) -> Option<MethodSignature> {
    match ty {
        Type::Fun(params, ret) => {
            Some(MethodSignature { name: name.to_string(), params: params.clone(), return_type: ret.as_ref().clone() })
        }
        _ => None,
    }
}

/// Check if a type satisfies a user-defined protocol
///
/// This method checks structural conformance by verifying that the type has all required methods with compatible signatures.
/// Uses full variance checking: contravariant parameters, covariant returns.
fn check_user_defined_protocol(ty: &Type, protocol_name: &str, class_registry: &ClassRegistry) -> bool {
    let protocol_meta = match class_registry.get_class(protocol_name) {
        Some(meta) if meta.is_protocol => meta,
        _ => return false,
    };

    let required_methods = protocol_meta.get_protocol_methods();
    if required_methods.is_empty() {
        return true;
    }

    match ty {
        Type::Con(TypeCtor::Class(class_name)) => {
            if let Some(class_meta) = class_registry.get_class(class_name) {
                let mut required_sigs = Vec::new();
                for (method_name, method_type) in required_methods {
                    if let Some(sig) = type_to_method_signature(method_name, method_type) {
                        required_sigs.push(sig);
                    }
                }

                let mut available_sigs = Vec::new();
                for (method_name, method_type) in class_meta.methods.iter() {
                    if let Some(ty) = method_type.primary_type() {
                        if let Some(sig) = type_to_method_signature(method_name, ty) {
                            available_sigs.push((method_name.clone(), sig));
                        }
                    }
                }

                let protocol_def = beacon_core::protocols::ProtocolDef {
                    name: beacon_core::protocols::ProtocolName::UserDefined(protocol_name.to_string()),
                    required_methods: required_sigs,
                };

                protocol_def.check_method_signatures(&available_sigs).is_ok()
            } else {
                false
            }
        }
        Type::Con(TypeCtor::Any) => true,
        _ => false,
    }
}

/// Check if a class type satisfies a builtin protocol by checking for dunder methods
///
/// This function checks if a class has the appropriate dunder methods to satisfy builtin protocols like Iterable, Sized, etc.
fn check_builtin_protocol_on_class(ty: &Type, protocol: &ProtocolName, class_registry: &ClassRegistry) -> bool {
    let class_name = match ty {
        Type::Con(TypeCtor::Class(name)) => name,
        Type::App(ctor, _) => {
            if let Type::Con(TypeCtor::Class(name)) = ctor.as_ref() {
                name
            } else {
                return false;
            }
        }
        _ => return false,
    };

    match protocol {
        ProtocolName::Iterable => class_registry.lookup_attribute(class_name, "__iter__").is_some(),
        ProtocolName::Iterator => {
            class_registry.lookup_attribute(class_name, "__iter__").is_some()
                && class_registry.lookup_attribute(class_name, "__next__").is_some()
        }
        ProtocolName::Sized => class_registry.lookup_attribute(class_name, "__len__").is_some(),
        ProtocolName::Sequence => {
            class_registry.lookup_attribute(class_name, "__len__").is_some()
                && class_registry.lookup_attribute(class_name, "__getitem__").is_some()
        }
        ProtocolName::Mapping => {
            class_registry.lookup_attribute(class_name, "__len__").is_some()
                && class_registry.lookup_attribute(class_name, "__getitem__").is_some()
                && class_registry.lookup_attribute(class_name, "__iter__").is_some()
        }
        ProtocolName::AsyncIterable => class_registry.lookup_attribute(class_name, "__aiter__").is_some(),
        ProtocolName::AsyncIterator => {
            class_registry.lookup_attribute(class_name, "__aiter__").is_some()
                && class_registry.lookup_attribute(class_name, "__anext__").is_some()
        }
        ProtocolName::Awaitable => class_registry.lookup_attribute(class_name, "__await__").is_some(),
        ProtocolName::ContextManager => {
            class_registry.lookup_attribute(class_name, "__enter__").is_some()
                && class_registry.lookup_attribute(class_name, "__exit__").is_some()
        }
        ProtocolName::AsyncContextManager => {
            class_registry.lookup_attribute(class_name, "__aenter__").is_some()
                && class_registry.lookup_attribute(class_name, "__aexit__").is_some()
        }
        ProtocolName::Callable => class_registry.lookup_attribute(class_name, "__call__").is_some(),
        ProtocolName::UserDefined(_) => false,
    }
}

/// Check if a type has a specific attribute
fn check_has_attribute(ty: &Type, attr_name: &str, class_registry: &ClassRegistry) -> bool {
    match ty {
        Type::Con(TypeCtor::Class(class_name)) => class_registry
            .lookup_attribute_with_inheritance(class_name, attr_name)
            .is_some(),
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
                TypeCtor::Any => return true,
                _ => None,
            };
            class_name.is_some_and(|name| class_registry.lookup_attribute(name, attr_name).is_some())
        }
        Type::App(base, _) => {
            let base_ctor = extract_base_constructor(base);
            base_ctor.is_some_and(|name| class_registry.lookup_attribute(name, attr_name).is_some())
        }
        Type::Var(_) => true,
        _ => false,
    }
}

/// Get the type of an attribute from a type
fn get_attribute_type(ty: &Type, attr_name: &str, class_registry: &ClassRegistry) -> Option<Type> {
    match ty {
        Type::Con(TypeCtor::Class(class_name)) => {
            class_registry.lookup_attribute_with_inheritance(class_name, attr_name)
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
                TypeCtor::Any => return Some(Type::any()),
                _ => None,
            };
            class_name.and_then(|name| class_registry.lookup_attribute(name, attr_name).cloned())
        }
        Type::App(_, _) => {
            if let Some((type_ctor, type_args)) = ty.unapply() {
                let class_name = match type_ctor {
                    TypeCtor::List => Some("list"),
                    TypeCtor::Dict => Some("dict"),
                    TypeCtor::Set => Some("set"),
                    TypeCtor::Tuple => Some("tuple"),
                    _ => None,
                };

                if let Some(name) = class_name {
                    if let Some(class_metadata) = class_registry.get_class(name) {
                        if let Some(attr_type) = class_metadata.lookup_attribute(attr_name) {
                            if !class_metadata.type_params.is_empty() {
                                let subst = class_metadata.create_type_substitution(&type_args);
                                return Some(beacon_core::ClassMetadata::substitute_type_params(attr_type, &subst));
                            }
                            return Some(attr_type.clone());
                        }
                    }
                }
            }

            if let Some((class_name, type_args)) = ty.unapply_class() {
                if let Some(class_metadata) = class_registry.get_class(class_name) {
                    if let Some(attr_type) = class_metadata.lookup_attribute(attr_name) {
                        if !class_metadata.type_params.is_empty() {
                            let subst = class_metadata.create_type_substitution(&type_args);
                            return Some(beacon_core::ClassMetadata::substitute_type_params(attr_type, &subst));
                        }
                        return Some(attr_type.clone());
                    }
                }
            }

            let base_ctor = extract_base_constructor(ty);
            base_ctor.and_then(|name| class_registry.lookup_attribute(name, attr_name).cloned())
        }
        _ => None,
    }
}

/// Solve a set of constraints using beacon-core's unification algorithm
///
/// Errors are accumulated rather than failing fast to provide comprehensive feedback.
pub fn solve_constraints(
    constraint_set: ConstraintSet, class_registry: &ClassRegistry,
) -> Result<(Subst, Vec<TypeErrorInfo>)> {
    let mut subst = Subst::empty();
    let mut type_errors = Vec::new();
    for constraint in constraint_set.constraints {
        match constraint {
            Constraint::Equal(t1, t2, span) => {
                let applied_t1 = subst.apply(&t1);
                let applied_t2 = subst.apply(&t2);

                let involves_union = matches!(applied_t1, Type::Union(_)) || matches!(applied_t2, Type::Union(_));

                if !(involves_union && (applied_t1.is_subtype_of(&applied_t2) || applied_t2.is_subtype_of(&applied_t1)))
                {
                    match Unifier::unify(&applied_t1, &applied_t2) {
                        Ok(s) => {
                            subst = s.compose(subst);
                        }
                        Err(beacon_core::BeaconError::TypeError(type_err)) => {
                            type_errors.push(TypeErrorInfo::new(type_err, span));
                        }
                        Err(_) => {}
                    }
                }
            }

            Constraint::Call(func_ty, arg_types, ret_ty, span) => {
                let applied_func = subst.apply(&func_ty);

                if let Type::Con(TypeCtor::Class(class_name)) = &applied_func {
                    if let Some(metadata) = class_registry.get_class(class_name) {
                        if let Some(ctor_ty) = metadata.new_type.as_ref().or(metadata.init_type.as_ref()) {
                            if let Type::Fun(params, _) = ctor_ty {
                                let ctor_params: Vec<Type> = params.iter().skip(1).cloned().collect();
                                if arg_types.len() <= ctor_params.len() {
                                    for (provided_arg, expected_param) in arg_types.iter().zip(ctor_params.iter()) {
                                        match Unifier::unify(&subst.apply(provided_arg), &subst.apply(expected_param)) {
                                            Ok(s) => {
                                                subst = s.compose(subst);
                                            }
                                            Err(beacon_core::BeaconError::TypeError(type_err)) => {
                                                type_errors.push(TypeErrorInfo::new(type_err, span));
                                            }
                                            Err(_) => {}
                                        }
                                    }
                                } else {
                                    type_errors.push(TypeErrorInfo::new(
                                        TypeError::ArgumentCountMismatch {
                                            expected: ctor_params.len(),
                                            found: arg_types.len(),
                                        },
                                        span,
                                    ));
                                }
                            }

                            match Unifier::unify(&subst.apply(&ret_ty), &applied_func) {
                                Ok(s) => {
                                    subst = s.compose(subst);
                                }
                                Err(beacon_core::BeaconError::TypeError(type_err)) => {
                                    type_errors.push(TypeErrorInfo::new(type_err, span));
                                }
                                Err(_) => {}
                            }
                        } else {
                            match Unifier::unify(&subst.apply(&ret_ty), &applied_func) {
                                Ok(s) => {
                                    subst = s.compose(subst);
                                }
                                Err(beacon_core::BeaconError::TypeError(type_err)) => {
                                    type_errors.push(TypeErrorInfo::new(type_err, span));
                                }
                                Err(_) => {}
                            }
                        }
                    }
                } else if let Type::BoundMethod(receiver, method_name, method) = &applied_func {
                    let resolved_method = if let Type::Con(TypeCtor::Class(class_name)) = receiver.as_ref() {
                        if let Some(metadata) = class_registry.get_class(class_name) {
                            if let Some(method_type) = metadata.lookup_method_type(method_name) {
                                let applied_args: Vec<Type> = arg_types.iter().map(|arg| subst.apply(arg)).collect();
                                method_type.resolve_for_args(&applied_args).unwrap_or(method.as_ref())
                            } else {
                                method.as_ref()
                            }
                        } else {
                            method.as_ref()
                        }
                    } else {
                        method.as_ref()
                    };

                    if let Type::Fun(params, method_ret) = resolved_method {
                        let bound_params: Vec<Type> = params.iter().skip(1).cloned().collect();
                        if arg_types.len() <= bound_params.len() {
                            for (provided_arg, expected_param) in arg_types.iter().zip(bound_params.iter()) {
                                match Unifier::unify(&subst.apply(provided_arg), &subst.apply(expected_param)) {
                                    Ok(s) => {
                                        subst = s.compose(subst);
                                    }
                                    Err(beacon_core::BeaconError::TypeError(type_err)) => {
                                        type_errors.push(TypeErrorInfo::new(type_err, span));
                                    }
                                    Err(_) => {}
                                }
                            }

                            match Unifier::unify(&subst.apply(&ret_ty), &subst.apply(method_ret)) {
                                Ok(s) => {
                                    subst = s.compose(subst);
                                }
                                Err(beacon_core::BeaconError::TypeError(type_err)) => {
                                    type_errors.push(TypeErrorInfo::new(type_err, span));
                                }
                                Err(_) => {}
                            }
                        } else {
                            type_errors.push(TypeErrorInfo::new(
                                TypeError::ArgumentCountMismatch {
                                    expected: bound_params.len(),
                                    found: arg_types.len(),
                                },
                                span,
                            ));
                        }
                    } else {
                        let expected_fn_ty = Type::fun(arg_types, ret_ty);
                        let resolved_method_ty = subst.apply(&resolved_method.clone());
                        match Unifier::unify(&resolved_method_ty, &subst.apply(&expected_fn_ty)) {
                            Ok(s) => {
                                subst = s.compose(subst);
                            }
                            Err(beacon_core::BeaconError::TypeError(type_err)) => {
                                type_errors.push(TypeErrorInfo::new(type_err, span));
                            }
                            Err(_) => {}
                        }
                    }
                } else {
                    let applied_func = subst.apply(&func_ty);
                    if let Type::Fun(params, fn_ret) = &applied_func {
                        if arg_types.len() <= params.len() {
                            for (provided_arg, expected_param) in arg_types.iter().zip(params.iter()) {
                                match Unifier::unify(&subst.apply(provided_arg), &subst.apply(expected_param)) {
                                    Ok(s) => {
                                        subst = s.compose(subst);
                                    }
                                    Err(beacon_core::BeaconError::TypeError(type_err)) => {
                                        type_errors.push(TypeErrorInfo::new(type_err, span));
                                    }
                                    Err(_) => {}
                                }
                            }

                            match Unifier::unify(&subst.apply(&ret_ty), &subst.apply(fn_ret)) {
                                Ok(s) => {
                                    subst = s.compose(subst);
                                }
                                Err(beacon_core::BeaconError::TypeError(type_err)) => {
                                    type_errors.push(TypeErrorInfo::new(type_err, span));
                                }
                                Err(_) => {}
                            }
                        } else {
                            type_errors.push(TypeErrorInfo::new(
                                TypeError::ArgumentCountMismatch { expected: params.len(), found: arg_types.len() },
                                span,
                            ));
                        }
                    } else {
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
                }
            }
            Constraint::HasAttr(obj_ty, attr_name, attr_ty, span) => {
                let applied_obj = subst.apply(&obj_ty);

                if let Type::Union(variants) = &applied_obj {
                    let mut all_have_attr = true;
                    let mut attr_types = Vec::new();

                    for variant in variants {
                        let has_attr = check_has_attribute(variant, &attr_name, class_registry);
                        if !has_attr {
                            all_have_attr = false;
                            break;
                        }

                        if let Some(resolved_attr_ty) = get_attribute_type(variant, &attr_name, class_registry) {
                            attr_types.push(resolved_attr_ty);
                        }
                    }

                    if !all_have_attr {
                        type_errors.push(TypeErrorInfo::new(
                            beacon_core::TypeError::AttributeNotFound(applied_obj.to_string(), attr_name.clone()),
                            span,
                        ));
                    } else if !attr_types.is_empty() {
                        let attr_union = if attr_types.len() == 1 {
                            attr_types.into_iter().next().unwrap()
                        } else {
                            Type::union(attr_types)
                        };

                        match Unifier::unify(&subst.apply(&attr_ty), &attr_union) {
                            Ok(s) => {
                                subst = s.compose(subst);
                            }
                            Err(beacon_core::BeaconError::TypeError(type_err)) => {
                                type_errors.push(TypeErrorInfo::new(type_err, span));
                            }
                            Err(_) => {}
                        }
                    }
                    continue;
                }

                match &applied_obj {
                    Type::Con(TypeCtor::Class(class_name)) => {
                        if let Some(resolved_attr_ty) =
                            class_registry.lookup_attribute_with_inheritance(class_name, &attr_name)
                        {
                            let final_type = if class_registry.is_method(class_name, &attr_name) {
                                Type::BoundMethod(
                                    Box::new(applied_obj.clone()),
                                    attr_name.clone(),
                                    Box::new(resolved_attr_ty.clone()),
                                )
                            } else {
                                resolved_attr_ty.clone()
                            };

                            match Unifier::unify(&subst.apply(&attr_ty), &final_type) {
                                Ok(s) => {
                                    subst = s.compose(subst);
                                }
                                Err(beacon_core::BeaconError::TypeError(type_err)) => {
                                    type_errors.push(TypeErrorInfo::new(type_err, span));
                                }
                                Err(_) => {}
                            }
                        } else {
                            type_errors.push(TypeErrorInfo::new(
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
                                if let Ok(s) = Unifier::unify(&subst.apply(&attr_ty), &Type::any()) {
                                    subst = s.compose(subst);
                                }
                                None
                            }
                            _ => None,
                        };

                        if let Some(class_name) = class_name {
                            if let Some(resolved_attr_ty) = class_registry.lookup_attribute(class_name, &attr_name) {
                                let final_type = if class_registry.is_method(class_name, &attr_name) {
                                    Type::BoundMethod(
                                        Box::new(applied_obj.clone()),
                                        attr_name.clone(),
                                        Box::new(resolved_attr_ty.clone()),
                                    )
                                } else {
                                    resolved_attr_ty.clone()
                                };

                                match Unifier::unify(&subst.apply(&attr_ty), &final_type) {
                                    Ok(s) => {
                                        subst = s.compose(subst);
                                    }
                                    Err(beacon_core::BeaconError::TypeError(type_err)) => {
                                        type_errors.push(TypeErrorInfo::new(type_err, span));
                                    }
                                    Err(_) => {}
                                }
                            } else {
                                type_errors.push(TypeErrorInfo::new(
                                    TypeError::AttributeNotFound(applied_obj.to_string(), attr_name.clone()),
                                    span,
                                ));
                            }
                        }
                    }
                    Type::App(_, _) => {
                        if let Some(resolved_attr_ty) = get_attribute_type(&applied_obj, &attr_name, class_registry) {
                            let base_ctor = extract_base_constructor(&applied_obj);
                            let final_type = if let Some(class_name) = base_ctor {
                                if class_registry.is_method(class_name, &attr_name) {
                                    Type::BoundMethod(
                                        Box::new(applied_obj.clone()),
                                        attr_name.clone(),
                                        Box::new(resolved_attr_ty.clone()),
                                    )
                                } else {
                                    resolved_attr_ty.clone()
                                }
                            } else {
                                resolved_attr_ty.clone()
                            };

                            match Unifier::unify(&subst.apply(&attr_ty), &final_type) {
                                Ok(s) => {
                                    subst = s.compose(subst);
                                }
                                Err(beacon_core::BeaconError::TypeError(type_err)) => {
                                    type_errors.push(TypeErrorInfo::new(type_err, span));
                                }
                                Err(_) => {}
                            }
                        } else {
                            type_errors.push(TypeErrorInfo::new(
                                beacon_core::TypeError::AttributeNotFound(applied_obj.to_string(), attr_name.clone()),
                                span,
                            ));
                        }
                    }
                    Type::Var(_) => {}
                    _ => {
                        type_errors.push(TypeErrorInfo::new(
                            beacon_core::TypeError::AttributeNotFound(applied_obj.to_string(), attr_name.clone()),
                            span,
                        ));
                    }
                }
            }

            Constraint::Protocol(obj_ty, protocol_name, elem_ty, span) => {
                let applied_obj = subst.apply(&obj_ty);
                if matches!(applied_obj, Type::Var(_)) {
                    continue;
                }

                let satisfies = match &protocol_name {
                    ProtocolName::UserDefined(proto_name) => {
                        check_user_defined_protocol(&applied_obj, proto_name, class_registry)
                    }
                    _ => {
                        check_builtin_protocol_on_class(&applied_obj, &protocol_name, class_registry)
                            || ProtocolChecker::satisfies(&applied_obj, &protocol_name)
                    }
                };

                if satisfies {
                    let extracted_elem = match protocol_name {
                        ProtocolName::Iterable | ProtocolName::Iterator | ProtocolName::Sequence => {
                            ProtocolChecker::extract_iterable_element(&applied_obj)
                        }
                        ProtocolName::AsyncIterable | ProtocolName::AsyncIterator => {
                            ProtocolChecker::extract_async_iterable_element(&applied_obj)
                        }
                        ProtocolName::Awaitable => ProtocolChecker::extract_awaitable_result(&applied_obj),
                        _ => Type::any(),
                    };

                    let applied_extracted = subst.apply(&extracted_elem);
                    match Unifier::unify(&subst.apply(&elem_ty), &applied_extracted) {
                        Ok(s) => {
                            subst = s.compose(subst);
                        }
                        Err(beacon_core::BeaconError::TypeError(type_err)) => {
                            type_errors.push(TypeErrorInfo::new(type_err, span));
                        }
                        Err(_) => {}
                    }
                } else {
                    type_errors.push(TypeErrorInfo::new(
                        beacon_core::TypeError::ProtocolNotSatisfied(
                            applied_obj.to_string(),
                            protocol_name.to_string(),
                        ),
                        span,
                    ));
                }
            }

            Constraint::MatchPattern(_subject_ty, _pattern, bindings, _span) => {
                for (_var_name, binding_ty) in bindings {
                    let applied_binding = subst.apply(&binding_ty);
                    let _ = applied_binding;
                }
            }

            Constraint::PatternExhaustive(subject_ty, patterns, span) => {
                let result = check_exhaustiveness(&subject_ty, &patterns);
                match result {
                    ExhaustivenessResult::Exhaustive => {}
                    ExhaustivenessResult::NonExhaustive { uncovered } => {
                        let uncovered_str = uncovered.iter().map(|ty| ty.to_string()).collect::<Vec<_>>().join(", ");
                        type_errors.push(TypeErrorInfo::new(TypeError::PatternNonExhaustive(uncovered_str), span));
                    }
                }
            }
            Constraint::PatternReachable(pattern, previous_patterns, span) => {
                let result = check_reachability(&pattern, &previous_patterns);
                match result {
                    ReachabilityResult::Reachable => {}
                    ReachabilityResult::Unreachable { subsumed_by: _ } => {
                        type_errors.push(TypeErrorInfo::new(TypeError::PatternUnreachable, span));
                    }
                }
            }
            Constraint::Narrowing(_var, _predicate, _narrowed_type, _span) => {}
            Constraint::Join(_var, incoming_types, result_type, span) => {
                let union_type = if incoming_types.is_empty() {
                    Type::Con(TypeCtor::NoneType)
                } else if incoming_types.len() == 1 {
                    incoming_types[0].clone()
                } else {
                    Type::union(incoming_types.clone())
                };

                match Unifier::unify(&subst.apply(&result_type), &subst.apply(&union_type)) {
                    Ok(s) => {
                        subst = s.compose(subst);
                    }
                    Err(beacon_core::BeaconError::TypeError(type_err)) => {
                        type_errors.push(TypeErrorInfo::new(type_err, span));
                    }
                    Err(_) => {}
                }
            }
        }
    }

    let simplified_subst = simplify_substitution(subst);

    Ok((simplified_subst, type_errors))
}

/// Simplify all types in a substitution by applying normalization rules
///
/// This performs post-processing after constraint solving to simplify union types that may have been created during unification.
fn simplify_substitution(subst: Subst) -> Subst {
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Span;
    use crate::predicate::TypePredicate;
    use beacon_core::{ClassMetadata, MethodType, TypeVar};
    use beacon_parser::{AstNode, LiteralValue, Pattern};

    /// Helper to create a test span
    fn test_span() -> Span {
        Span::new(1, 1)
    }

    /// Helper to create a type variable
    fn tvar(id: u32) -> Type {
        Type::Var(TypeVar { id, hint: None })
    }

    #[test]
    fn test_type_to_method_signature_success() {
        let func_ty = Type::fun(vec![Type::int(), Type::string()], Type::bool());
        let sig = type_to_method_signature("test_method", &func_ty);
        assert!(sig.is_some());

        let sig = sig.unwrap();
        assert_eq!(sig.name, "test_method");
        assert_eq!(sig.params.len(), 2);
        assert_eq!(sig.return_type, Type::bool());
    }

    #[test]
    fn test_type_to_method_signature_non_function() {
        let non_func = Type::int();
        let sig = type_to_method_signature("test", &non_func);
        assert!(sig.is_none());
    }

    #[test]
    fn test_check_user_defined_protocol_no_methods() {
        let mut registry = ClassRegistry::new();
        let mut protocol_meta = ClassMetadata::new("EmptyProtocol".to_string());
        protocol_meta.is_protocol = true;
        registry.register_class("EmptyProtocol".to_string(), protocol_meta);

        let result = check_user_defined_protocol(&Type::int(), "EmptyProtocol", &registry);
        assert!(
            result,
            "Protocol with no required methods should be satisfied by any type"
        );
    }

    #[test]
    fn test_check_user_defined_protocol_any_satisfies() {
        let mut registry = ClassRegistry::new();
        let mut protocol_meta = ClassMetadata::new("TestProtocol".to_string());
        protocol_meta.is_protocol = true;
        protocol_meta.methods.insert(
            "test".to_string(),
            MethodType::Single(Type::fun(vec![Type::any()], Type::any())),
        );
        registry.register_class("TestProtocol".to_string(), protocol_meta);

        let result = check_user_defined_protocol(&Type::any(), "TestProtocol", &registry);
        assert!(result, "Any type should satisfy any protocol");
    }

    #[test]
    fn test_solve_equal_constraint_success() {
        let constraints = ConstraintSet { constraints: vec![Constraint::Equal(Type::int(), Type::int(), test_span())] };
        let registry = ClassRegistry::new();
        let result = solve_constraints(constraints, &registry);
        assert!(result.is_ok());

        let (_, errors) = result.unwrap();
        assert!(errors.is_empty());
    }

    #[test]
    fn test_solve_equal_constraint_unification_error() {
        let constraints =
            ConstraintSet { constraints: vec![Constraint::Equal(Type::int(), Type::string(), test_span())] };

        let registry = ClassRegistry::new();
        let result = solve_constraints(constraints, &registry);
        assert!(result.is_ok());

        let (_, errors) = result.unwrap();
        assert!(!errors.is_empty(), "Unifying int and string should produce an error");
    }

    #[test]
    fn test_solve_equal_constraint_with_type_vars() {
        let t1 = tvar(0);
        let t2 = Type::int();
        let constraints = ConstraintSet { constraints: vec![Constraint::Equal(t1, t2, test_span())] };
        let registry = ClassRegistry::new();
        let result = solve_constraints(constraints, &registry);
        assert!(result.is_ok());

        let (subst, errors) = result.unwrap();
        assert!(errors.is_empty());
        assert!(!subst.is_empty());
    }

    #[test]
    fn test_solve_multiple_equal_constraints() {
        let t1 = tvar(0);
        let t2 = tvar(1);
        let constraints = ConstraintSet {
            constraints: vec![
                Constraint::Equal(t1.clone(), Type::int(), test_span()),
                Constraint::Equal(t2, Type::string(), test_span()),
            ],
        };

        let registry = ClassRegistry::new();
        let result = solve_constraints(constraints, &registry);
        assert!(result.is_ok());

        let (subst, errors) = result.unwrap();
        assert!(errors.is_empty());
        assert!(!subst.is_empty());
    }

    #[test]
    fn test_solve_call_constraint_simple_function() {
        let func_ty = Type::fun(vec![Type::int()], Type::string());
        let arg_types = vec![Type::int()];
        let ret_ty = tvar(0);
        let constraints =
            ConstraintSet { constraints: vec![Constraint::Call(func_ty, arg_types, ret_ty, test_span())] };

        let registry = ClassRegistry::new();
        let result = solve_constraints(constraints, &registry);
        assert!(result.is_ok());

        let (subst, errors) = result.unwrap();
        assert!(errors.is_empty());
        assert!(!subst.is_empty());
    }

    #[test]
    fn test_solve_call_constraint_wrong_arg_count() {
        let func_ty = Type::fun(vec![Type::int()], Type::string());
        let arg_types = vec![Type::int(), Type::int()]; // Too many args
        let ret_ty = tvar(0);
        let constraints =
            ConstraintSet { constraints: vec![Constraint::Call(func_ty, arg_types, ret_ty, test_span())] };

        let registry = ClassRegistry::new();
        let result = solve_constraints(constraints, &registry);
        assert!(result.is_ok());

        let (_, errors) = result.unwrap();
        assert!(!errors.is_empty(), "Wrong argument count should produce an error");
    }

    #[test]
    fn test_solve_call_constraint_class_constructor() {
        let mut registry = ClassRegistry::new();
        let mut class_meta = ClassMetadata::new("TestClass".to_string());
        class_meta.init_type = Some(Type::fun(vec![Type::any(), Type::int()], Type::any()));
        registry.register_class("TestClass".to_string(), class_meta);

        let class_ty = Type::Con(TypeCtor::Class("TestClass".to_string()));
        let arg_types = vec![Type::int()];
        let ret_ty = tvar(0);
        let constraints =
            ConstraintSet { constraints: vec![Constraint::Call(class_ty, arg_types, ret_ty, test_span())] };

        let result = solve_constraints(constraints, &registry);
        assert!(result.is_ok());

        let (_, errors) = result.unwrap();
        assert!(errors.is_empty(), "Class constructor call should succeed");
    }

    #[test]
    fn test_solve_call_constraint_bound_method() {
        let mut registry = ClassRegistry::new();
        let mut class_meta = ClassMetadata::new("TestClass".to_string());
        class_meta.methods.insert(
            "method".to_string(),
            MethodType::Single(Type::fun(vec![Type::any(), Type::int()], Type::string())),
        );
        registry.register_class("TestClass".to_string(), class_meta);

        let receiver = Type::Con(TypeCtor::Class("TestClass".to_string()));
        let method_ty = Type::fun(vec![Type::any(), Type::int()], Type::string());
        let bound_method = Type::BoundMethod(Box::new(receiver), "method".to_string(), Box::new(method_ty));
        let arg_types = vec![Type::int()];
        let ret_ty = tvar(0);

        let constraints =
            ConstraintSet { constraints: vec![Constraint::Call(bound_method, arg_types, ret_ty, test_span())] };

        let result = solve_constraints(constraints, &registry);
        assert!(result.is_ok());

        let (_subst, errors) = result.unwrap();
        assert!(errors.is_empty(), "Bound method call should succeed");
    }

    #[test]
    fn test_solve_has_attr_on_class() {
        let mut registry = ClassRegistry::new();
        let mut class_meta = ClassMetadata::new("TestClass".to_string());
        class_meta.fields.insert("attr".to_string(), Type::int());
        registry.register_class("TestClass".to_string(), class_meta);

        let obj_ty = Type::Con(TypeCtor::Class("TestClass".to_string()));
        let attr_ty = tvar(0);
        let constraints =
            ConstraintSet { constraints: vec![Constraint::HasAttr(obj_ty, "attr".to_string(), attr_ty, test_span())] };

        let result = solve_constraints(constraints, &registry);
        assert!(result.is_ok());

        let (_, errors) = result.unwrap();
        assert!(errors.is_empty(), "Attribute access should succeed");
    }

    #[test]
    fn test_solve_has_attr_missing() {
        let mut registry = ClassRegistry::new();
        let class_meta = ClassMetadata::new("TestClass".to_string());
        registry.register_class("TestClass".to_string(), class_meta);

        let obj_ty = Type::Con(TypeCtor::Class("TestClass".to_string()));
        let attr_ty = tvar(0);
        let constraints = ConstraintSet {
            constraints: vec![Constraint::HasAttr(
                obj_ty,
                "nonexistent".to_string(),
                attr_ty,
                test_span(),
            )],
        };

        let result = solve_constraints(constraints, &registry);
        assert!(result.is_ok());

        let (_, errors) = result.unwrap();
        assert!(!errors.is_empty(), "Missing attribute should produce an error");
        assert!(
            errors
                .iter()
                .any(|e| matches!(e.error, TypeError::AttributeNotFound(_, _)))
        );
    }

    #[test]
    fn test_solve_has_attr_on_builtin_str() {
        let mut registry = ClassRegistry::new();
        let mut str_meta = ClassMetadata::new("str".to_string());
        str_meta.methods.insert(
            "upper".to_string(),
            MethodType::Single(Type::fun(vec![Type::any()], Type::string())),
        );
        registry.register_class("str".to_string(), str_meta);

        let obj_ty = Type::string();
        let attr_ty = tvar(0);
        let constraints =
            ConstraintSet { constraints: vec![Constraint::HasAttr(obj_ty, "upper".to_string(), attr_ty, test_span())] };

        let result = solve_constraints(constraints, &registry);
        assert!(result.is_ok());

        let (_, errors) = result.unwrap();
        assert!(errors.is_empty(), "Builtin string method access should succeed");
    }

    #[test]
    fn test_solve_has_attr_on_any() {
        let registry = ClassRegistry::new();
        let obj_ty = Type::any();
        let attr_ty = tvar(0);
        let constraints = ConstraintSet {
            constraints: vec![Constraint::HasAttr(
                obj_ty,
                "anything".to_string(),
                attr_ty,
                test_span(),
            )],
        };

        let result = solve_constraints(constraints, &registry);
        assert!(result.is_ok());

        let (_, errors) = result.unwrap();
        assert!(errors.is_empty(), "Any type should allow any attribute access");
    }

    #[test]
    fn test_solve_protocol_iterable() {
        let obj_ty = Type::list(Type::int());
        let elem_ty = tvar(0);
        let constraints = ConstraintSet {
            constraints: vec![Constraint::Protocol(
                obj_ty,
                ProtocolName::Iterable,
                elem_ty,
                test_span(),
            )],
        };

        let registry = ClassRegistry::new();
        let result = solve_constraints(constraints, &registry);
        assert!(result.is_ok());

        let (_, errors) = result.unwrap();
        assert!(errors.is_empty(), "List should satisfy Iterable protocol");
    }

    #[test]
    fn test_solve_protocol_not_satisfied() {
        let obj_ty = Type::int();
        let elem_ty = tvar(0);
        let constraints = ConstraintSet {
            constraints: vec![Constraint::Protocol(
                obj_ty,
                ProtocolName::Iterable,
                elem_ty,
                test_span(),
            )],
        };

        let registry = ClassRegistry::new();
        let result = solve_constraints(constraints, &registry);
        assert!(result.is_ok());

        let (_, errors) = result.unwrap();
        assert!(!errors.is_empty(), "Int should not satisfy Iterable protocol");
        assert!(
            errors
                .iter()
                .any(|e| matches!(e.error, TypeError::ProtocolNotSatisfied(_, _)))
        );
    }

    #[test]
    fn test_solve_protocol_async_iterable() {
        let obj_ty = Type::async_generator(Type::int(), Type::none());
        let elem_ty = tvar(0);
        let constraints = ConstraintSet {
            constraints: vec![Constraint::Protocol(
                obj_ty,
                ProtocolName::AsyncIterable,
                elem_ty,
                test_span(),
            )],
        };
        let registry = ClassRegistry::new();
        let result = solve_constraints(constraints, &registry);
        assert!(result.is_ok());

        let (_, errors) = result.unwrap();
        assert!(
            errors.is_empty(),
            "AsyncGenerator should satisfy AsyncIterable protocol"
        );
    }

    #[test]
    fn test_solve_protocol_awaitable() {
        let obj_ty = Type::coroutine(Type::none(), Type::none(), Type::int());
        let result_ty = tvar(0);
        let constraints = ConstraintSet {
            constraints: vec![Constraint::Protocol(
                obj_ty,
                ProtocolName::Awaitable,
                result_ty,
                test_span(),
            )],
        };
        let registry = ClassRegistry::new();
        let result = solve_constraints(constraints, &registry);
        assert!(result.is_ok());

        let (_, errors) = result.unwrap();
        assert!(errors.is_empty(), "Coroutine should satisfy Awaitable protocol");
    }

    #[test]
    fn test_solve_protocol_with_type_variable() {
        let obj_ty = tvar(12);
        let elem_ty = tvar(13);
        let constraints = ConstraintSet {
            constraints: vec![Constraint::Protocol(
                obj_ty,
                ProtocolName::Iterable,
                elem_ty,
                test_span(),
            )],
        };
        let registry = ClassRegistry::new();
        let result = solve_constraints(constraints, &registry);
        assert!(result.is_ok());

        let (_, errors) = result.unwrap();
        assert!(
            errors.is_empty(),
            "Protocol constraint on unresolved type variable should be skipped, but got errors: {errors:?}"
        );
    }

    #[test]
    fn test_solve_match_pattern_constraint() {
        let subject_ty = Type::int();
        let pattern = Pattern::MatchAs { pattern: None, name: Some("x".to_string()) };
        let bindings = vec![("x".to_string(), Type::int())];
        let constraints =
            ConstraintSet { constraints: vec![Constraint::MatchPattern(subject_ty, pattern, bindings, test_span())] };
        let registry = ClassRegistry::new();

        let result = solve_constraints(constraints, &registry);
        assert!(result.is_ok());

        let (_, errors) = result.unwrap();
        assert!(errors.is_empty(), "MatchPattern constraint should succeed");
    }

    #[test]
    fn test_solve_pattern_exhaustive_complete() {
        let subject_ty = Type::bool();
        let patterns = vec![
            Pattern::MatchValue(AstNode::Literal { value: LiteralValue::Boolean(true), line: 1, col: 1 }),
            Pattern::MatchValue(AstNode::Literal { value: LiteralValue::Boolean(false), line: 1, col: 1 }),
        ];
        let constraints =
            ConstraintSet { constraints: vec![Constraint::PatternExhaustive(subject_ty, patterns, test_span())] };

        let registry = ClassRegistry::new();
        let result = solve_constraints(constraints, &registry);
        assert!(result.is_ok());

        let (_, errors) = result.unwrap();
        assert!(errors.is_empty(), "Exhaustive patterns should not produce errors");
    }

    #[test]
    fn test_solve_pattern_exhaustive_incomplete() {
        let subject_ty = Type::bool();
        let patterns = vec![Pattern::MatchValue(AstNode::Literal {
            value: LiteralValue::Boolean(true),
            line: 1,
            col: 1,
        })];

        let constraints =
            ConstraintSet { constraints: vec![Constraint::PatternExhaustive(subject_ty, patterns, test_span())] };

        let registry = ClassRegistry::new();
        let result = solve_constraints(constraints, &registry);
        assert!(result.is_ok());

        let (_, errors) = result.unwrap();
        assert!(!errors.is_empty(), "Non-exhaustive patterns should produce an error");
        assert!(
            errors
                .iter()
                .any(|e| matches!(e.error, TypeError::PatternNonExhaustive(_)))
        );
    }

    #[test]
    fn test_solve_pattern_reachable() {
        let pattern = Pattern::MatchValue(AstNode::Literal { value: LiteralValue::Integer(42), line: 1, col: 1 });
        let previous = vec![Pattern::MatchValue(AstNode::Literal {
            value: LiteralValue::Integer(43),
            line: 1,
            col: 1,
        })];

        let constraints =
            ConstraintSet { constraints: vec![Constraint::PatternReachable(pattern, previous, test_span())] };

        let registry = ClassRegistry::new();
        let result = solve_constraints(constraints, &registry);
        assert!(result.is_ok());

        let (_, errors) = result.unwrap();
        assert!(errors.is_empty(), "Different patterns should be reachable");
    }

    #[test]
    fn test_solve_pattern_unreachable() {
        let catch_all = Pattern::MatchAs { pattern: None, name: Some("_".to_string()) };
        let pattern = Pattern::MatchValue(AstNode::Literal { value: LiteralValue::Integer(42), line: 1, col: 1 });
        let previous = vec![catch_all];
        let constraints =
            ConstraintSet { constraints: vec![Constraint::PatternReachable(pattern, previous, test_span())] };

        let registry = ClassRegistry::new();
        let result = solve_constraints(constraints, &registry);
        assert!(result.is_ok());

        let (_, errors) = result.unwrap();
        assert!(!errors.is_empty(), "Unreachable pattern should produce an error");
        assert!(errors.iter().any(|e| matches!(e.error, TypeError::PatternUnreachable)));
    }

    #[test]
    fn test_error_accumulation() {
        let constraints = ConstraintSet {
            constraints: vec![
                Constraint::Equal(Type::int(), Type::string(), test_span()),
                Constraint::Equal(Type::bool(), Type::float(), test_span()),
            ],
        };

        let registry = ClassRegistry::new();
        let result = solve_constraints(constraints, &registry);
        assert!(result.is_ok());
        let (_, errors) = result.unwrap();
        assert_eq!(errors.len(), 2, "Both errors should be accumulated");
    }

    #[test]
    fn test_substitution_composition() {
        let t1 = tvar(0);
        let t2 = tvar(1);
        let constraints = ConstraintSet {
            constraints: vec![
                Constraint::Equal(t1.clone(), Type::int(), test_span()),
                Constraint::Equal(t2.clone(), t1, test_span()),
            ],
        };

        let registry = ClassRegistry::new();
        let result = solve_constraints(constraints, &registry);
        assert!(result.is_ok());

        let (subst, errors) = result.unwrap();
        assert!(errors.is_empty());

        let resolved_t2 = subst.apply(&t2);
        assert_eq!(resolved_t2, Type::int(), "Substitution should compose correctly");
    }

    #[test]
    fn test_has_attr_on_type_var() {
        let obj_ty = tvar(0);
        let attr_ty = tvar(1);
        let constraints =
            ConstraintSet { constraints: vec![Constraint::HasAttr(obj_ty, "attr".to_string(), attr_ty, test_span())] };

        let registry = ClassRegistry::new();
        let result = solve_constraints(constraints, &registry);
        assert!(result.is_ok());

        let (_subst, errors) = result.unwrap();
        assert!(errors.is_empty() || errors.len() <= 1);
    }

    #[test]
    fn test_solve_call_constraint_wrong_arg_type() {
        let func_ty = Type::fun(vec![Type::int()], Type::string());
        let arg_types = vec![Type::string()];
        let ret_ty = tvar(0);
        let constraints =
            ConstraintSet { constraints: vec![Constraint::Call(func_ty, arg_types, ret_ty, test_span())] };

        let registry = ClassRegistry::new();
        let result = solve_constraints(constraints, &registry);
        assert!(result.is_ok());

        let (_subst, errors) = result.unwrap();
        assert!(!errors.is_empty(), "Wrong argument type should produce an error");
    }

    #[test]
    fn test_solve_has_attr_method_becomes_bound_method() {
        let mut registry = ClassRegistry::new();
        let mut class_meta = ClassMetadata::new("TestClass".to_string());
        class_meta.methods.insert(
            "method".to_string(),
            MethodType::Single(Type::fun(vec![Type::any()], Type::string())),
        );
        registry.register_class("TestClass".to_string(), class_meta);

        let obj_ty = Type::Con(TypeCtor::Class("TestClass".to_string()));
        let attr_ty = tvar(0);
        let constraints = ConstraintSet {
            constraints: vec![Constraint::HasAttr(
                obj_ty,
                "method".to_string(),
                attr_ty.clone(),
                test_span(),
            )],
        };

        let result = solve_constraints(constraints, &registry);
        assert!(result.is_ok());

        let (subst, errors) = result.unwrap();
        assert!(errors.is_empty(), "Method access should succeed");

        let resolved = subst.apply(&attr_ty);
        assert!(
            matches!(resolved, Type::BoundMethod(_, _, _)),
            "Method should become BoundMethod type"
        );
    }

    #[test]
    fn test_empty_constraint_set() {
        let constraints = ConstraintSet { constraints: vec![] };
        let registry = ClassRegistry::new();
        let result = solve_constraints(constraints, &registry);
        assert!(result.is_ok());

        let (subst, errors) = result.unwrap();
        assert!(errors.is_empty());
        assert!(subst.is_empty());
    }

    #[test]
    fn test_call_with_fewer_args_than_params() {
        let func_ty = Type::fun(vec![Type::string(), Type::string()], Type::string());
        let arg_types = vec![Type::string()]; // Only provide first arg
        let ret_ty = tvar(0);
        let constraints =
            ConstraintSet { constraints: vec![Constraint::Call(func_ty, arg_types, ret_ty.clone(), test_span())] };

        let registry = ClassRegistry::new();
        let result = solve_constraints(constraints, &registry);
        assert!(result.is_ok());

        let (subst, errors) = result.unwrap();
        assert!(
            errors.is_empty(),
            "Calling function with fewer args (assuming defaults) should succeed, but got errors: {errors:?}"
        );

        let resolved_ret = subst.apply(&ret_ty);
        assert_eq!(resolved_ret, Type::string());
    }

    #[test]
    fn test_call_with_zero_args_all_defaults() {
        let func_ty = Type::fun(
            vec![Type::string()],
            Type::Con(TypeCtor::Class("Processor".to_string())),
        );
        let arg_types = vec![];
        let ret_ty = tvar(0);
        let constraints =
            ConstraintSet { constraints: vec![Constraint::Call(func_ty, arg_types, ret_ty.clone(), test_span())] };

        let registry = ClassRegistry::new();
        let result = solve_constraints(constraints, &registry);
        assert!(result.is_ok());

        let (subst, errors) = result.unwrap();
        assert!(
            errors.is_empty(),
            "Calling function with no args (all have defaults) should succeed, but got errors: {errors:?}"
        );

        let resolved_ret = subst.apply(&ret_ty);
        assert_eq!(resolved_ret, Type::Con(TypeCtor::Class("Processor".to_string())));
    }

    #[test]
    fn test_call_with_too_many_args() {
        let func_ty = Type::fun(vec![Type::int()], Type::string());
        let arg_types = vec![Type::int(), Type::int()]; // Too many args
        let ret_ty = tvar(0);
        let constraints =
            ConstraintSet { constraints: vec![Constraint::Call(func_ty, arg_types, ret_ty, test_span())] };

        let registry = ClassRegistry::new();
        let result = solve_constraints(constraints, &registry);
        assert!(result.is_ok());

        let (_, errors) = result.unwrap();
        assert!(
            !errors.is_empty(),
            "Providing too many arguments should still produce an error"
        );
    }

    #[test]
    fn test_bound_method_call_with_fewer_args() {
        let mut registry = ClassRegistry::new();
        let mut class_meta = ClassMetadata::new("TestClass".to_string());
        class_meta.methods.insert(
            "method".to_string(),
            MethodType::Single(Type::fun(vec![Type::any(), Type::int(), Type::string()], Type::bool())),
        );
        registry.register_class("TestClass".to_string(), class_meta);

        let receiver = Type::Con(TypeCtor::Class("TestClass".to_string()));
        let method_ty = Type::fun(vec![Type::any(), Type::int(), Type::string()], Type::bool());
        let bound_method = Type::BoundMethod(Box::new(receiver), "method".to_string(), Box::new(method_ty));
        let arg_types = vec![Type::int()]; // Only provide first arg (second has default)
        let ret_ty = tvar(0);

        let constraints =
            ConstraintSet { constraints: vec![Constraint::Call(bound_method, arg_types, ret_ty.clone(), test_span())] };

        let result = solve_constraints(constraints, &registry);
        assert!(result.is_ok());

        let (subst, errors) = result.unwrap();
        assert!(
            errors.is_empty(),
            "Bound method call with fewer args (assuming defaults) should succeed, but got errors: {errors:?}"
        );

        let resolved_ret = subst.apply(&ret_ty);
        assert_eq!(resolved_ret, Type::bool());
    }

    #[test]
    fn test_class_constructor_with_fewer_args() {
        let mut registry = ClassRegistry::new();
        let mut class_meta = ClassMetadata::new("TestClass".to_string());
        class_meta.init_type = Some(Type::fun(vec![Type::any(), Type::int(), Type::string()], Type::any()));
        registry.register_class("TestClass".to_string(), class_meta);

        let class_ty = Type::Con(TypeCtor::Class("TestClass".to_string()));
        let arg_types = vec![Type::int()]; // Only provide x, y has default
        let ret_ty = tvar(0);
        let constraints = ConstraintSet {
            constraints: vec![Constraint::Call(
                class_ty.clone(),
                arg_types,
                ret_ty.clone(),
                test_span(),
            )],
        };

        let result = solve_constraints(constraints, &registry);
        assert!(result.is_ok());

        let (subst, errors) = result.unwrap();
        assert!(
            errors.is_empty(),
            "Class constructor call with fewer args should succeed, but got errors: {errors:?}"
        );

        let resolved_ret = subst.apply(&ret_ty);
        assert_eq!(resolved_ret, class_ty);
    }

    #[test]
    fn test_simplify_substitution_with_union_any() {
        let tv = tvar(0);
        let union_with_any = Type::union(vec![Type::int(), Type::any(), Type::string()]);
        let mut subst = Subst::empty();
        subst.insert(TypeVar::new(0), union_with_any);

        let simplified = simplify_substitution(subst);
        let result = simplified.apply(&tv);
        assert_eq!(result, Type::any(), "Union with Any should simplify to Any");
    }

    #[test]
    fn test_simplify_substitution_preserves_normal_types() {
        let tv1 = tvar(0);
        let tv2 = tvar(1);

        let mut subst = Subst::empty();
        subst.insert(TypeVar::new(0), Type::int());
        subst.insert(TypeVar::new(1), Type::union(vec![Type::string(), Type::bool()]));

        let simplified = simplify_substitution(subst);

        assert_eq!(simplified.apply(&tv1), Type::int());
        match simplified.apply(&tv2) {
            Type::Union(types) => {
                assert_eq!(types.len(), 2);
                assert!(types.contains(&Type::string()));
                assert!(types.contains(&Type::bool()));
            }
            _ => panic!("Expected union type"),
        }
    }

    #[test]
    fn test_has_attr_on_union_all_branches_have_attr() {
        let mut registry = ClassRegistry::new();
        let mut class1 = ClassMetadata::new("Class1".to_string());
        class1.fields.insert("value".to_string(), Type::int());
        registry.register_class("Class1".to_string(), class1);

        let mut class2 = ClassMetadata::new("Class2".to_string());
        class2.fields.insert("value".to_string(), Type::string());
        registry.register_class("Class2".to_string(), class2);

        let obj_ty = Type::union(vec![
            Type::Con(TypeCtor::Class("Class1".to_string())),
            Type::Con(TypeCtor::Class("Class2".to_string())),
        ]);
        let attr_ty = tvar(0);

        let constraints = ConstraintSet {
            constraints: vec![Constraint::HasAttr(
                obj_ty,
                "value".to_string(),
                attr_ty.clone(),
                test_span(),
            )],
        };

        let result = solve_constraints(constraints, &registry);
        assert!(result.is_ok());

        let (subst, errors) = result.unwrap();
        assert!(
            errors.is_empty(),
            "Both branches have 'value', should succeed. Got errors: {errors:?}"
        );

        let resolved_attr = subst.apply(&attr_ty);
        match resolved_attr {
            Type::Union(types) => {
                assert_eq!(types.len(), 2);
                assert!(types.contains(&Type::int()));
                assert!(types.contains(&Type::string()));
            }
            _ => panic!("Expected union type for attribute, got: {resolved_attr}"),
        }
    }

    #[test]
    fn test_has_attr_on_union_missing_attr_in_one_branch() {
        let mut registry = ClassRegistry::new();
        let mut class1 = ClassMetadata::new("Class1".to_string());
        class1.fields.insert("value".to_string(), Type::int());
        registry.register_class("Class1".to_string(), class1);

        let class2 = ClassMetadata::new("Class2".to_string());
        registry.register_class("Class2".to_string(), class2);

        let obj_ty = Type::union(vec![
            Type::Con(TypeCtor::Class("Class1".to_string())),
            Type::Con(TypeCtor::Class("Class2".to_string())),
        ]);
        let attr_ty = tvar(0);

        let constraints =
            ConstraintSet { constraints: vec![Constraint::HasAttr(obj_ty, "value".to_string(), attr_ty, test_span())] };

        let result = solve_constraints(constraints, &registry);
        assert!(result.is_ok());

        let (_, errors) = result.unwrap();
        assert!(
            !errors.is_empty(),
            "Class2 doesn't have 'value', should produce an error"
        );
        assert!(
            errors
                .iter()
                .any(|e| matches!(e.error, TypeError::AttributeNotFound(_, _))),
            "Expected AttributeNotFound error"
        );
    }

    #[test]
    fn test_has_attr_on_optional_none_missing_attr() {
        let mut registry = ClassRegistry::new();
        let mut calc_class = ClassMetadata::new("Calculator".to_string());
        calc_class.methods.insert(
            "add".to_string(),
            MethodType::Single(Type::fun(vec![Type::any(), Type::int(), Type::int()], Type::int())),
        );
        registry.register_class("Calculator".to_string(), calc_class);

        let obj_ty = Type::optional(Type::Con(TypeCtor::Class("Calculator".to_string())));
        let attr_ty = tvar(0);
        let constraints =
            ConstraintSet { constraints: vec![Constraint::HasAttr(obj_ty, "add".to_string(), attr_ty, test_span())] };

        let result = solve_constraints(constraints, &registry);
        assert!(result.is_ok());

        let (_, errors) = result.unwrap();
        assert!(
            !errors.is_empty(),
            "None doesn't have 'add' method, should produce an error"
        );
        assert!(
            errors
                .iter()
                .any(|e| matches!(e.error, TypeError::AttributeNotFound(_, _))),
            "Expected AttributeNotFound error"
        );
    }

    #[test]
    fn test_generic_type_parameter_instantiation_list() {
        let mut registry = ClassRegistry::new();
        let mut list_meta = ClassMetadata::new("list".to_string());

        list_meta.set_type_params(vec!["_T".to_string()]);
        list_meta.methods.insert(
            "__getitem__".to_string(),
            MethodType::Single(Type::fun(
                vec![Type::any(), Type::int()],
                Type::Con(TypeCtor::TypeVariable("_T".to_string())),
            )),
        );

        registry.register_class("list".to_string(), list_meta);

        let list_int = Type::list(Type::int());
        let attr_ty = tvar(0);
        let constraints = ConstraintSet {
            constraints: vec![Constraint::HasAttr(
                list_int,
                "__getitem__".to_string(),
                attr_ty.clone(),
                test_span(),
            )],
        };

        let result = solve_constraints(constraints, &registry);
        assert!(result.is_ok());

        let (subst, errors) = result.unwrap();
        assert!(errors.is_empty(), "Should not have errors");

        let resolved = subst.apply(&attr_ty);
        match resolved {
            Type::BoundMethod(_, _, method) => match *method {
                Type::Fun(params, ret) => {
                    assert_eq!(params.len(), 2); // self + index
                    assert_eq!(*ret, Type::int(), "Should return int, not _T");
                }
                _ => panic!("Expected Fun inside BoundMethod"),
            },
            _ => panic!("Expected BoundMethod for __getitem__, got {resolved:?}"),
        }
    }

    #[test]
    fn test_generic_type_parameter_instantiation_dict() {
        let mut registry = ClassRegistry::new();
        let mut dict_meta = ClassMetadata::new("dict".to_string());

        dict_meta.set_type_params(vec!["_KT".to_string(), "_VT".to_string()]);
        dict_meta.methods.insert(
            "get".to_string(),
            MethodType::Single(Type::fun(
                vec![Type::any(), Type::Con(TypeCtor::TypeVariable("_KT".to_string()))],
                Type::optional(Type::Con(TypeCtor::TypeVariable("_VT".to_string()))),
            )),
        );

        registry.register_class("dict".to_string(), dict_meta);

        let dict_str_int = Type::App(
            Box::new(Type::App(Box::new(Type::Con(TypeCtor::Dict)), Box::new(Type::string()))),
            Box::new(Type::int()),
        );

        let attr_ty = tvar(0);
        let constraints = ConstraintSet {
            constraints: vec![Constraint::HasAttr(
                dict_str_int,
                "get".to_string(),
                attr_ty.clone(),
                test_span(),
            )],
        };

        let result = solve_constraints(constraints, &registry);
        assert!(result.is_ok());

        let (subst, errors) = result.unwrap();
        assert!(errors.is_empty(), "Should not have errors");

        let resolved = subst.apply(&attr_ty);
        match resolved {
            Type::BoundMethod(_, _, method) => match *method {
                Type::Fun(params, ret) => {
                    assert_eq!(params.len(), 2); // self + key
                    assert_eq!(params[1], Type::string(), "Key param should be str");
                    assert_eq!(*ret, Type::optional(Type::int()), "Should return int | None");
                }
                _ => panic!("Expected Fun inside BoundMethod"),
            },
            _ => panic!("Expected BoundMethod for get, got {resolved:?}"),
        }
    }

    #[test]
    fn test_generic_type_no_instantiation_when_no_params() {
        let mut registry = ClassRegistry::new();
        let mut list_meta = ClassMetadata::new("list".to_string());

        list_meta.methods.insert(
            "__len__".to_string(),
            MethodType::Single(Type::fun(vec![Type::any()], Type::int())),
        );

        registry.register_class("list".to_string(), list_meta);

        let list_int = Type::list(Type::int());
        let attr_ty = tvar(0);
        let constraints = ConstraintSet {
            constraints: vec![Constraint::HasAttr(
                list_int,
                "__len__".to_string(),
                attr_ty.clone(),
                test_span(),
            )],
        };

        let result = solve_constraints(constraints, &registry);
        assert!(result.is_ok());

        let (subst, errors) = result.unwrap();
        assert!(errors.is_empty(), "Should not have errors");

        let resolved = subst.apply(&attr_ty);
        match resolved {
            Type::BoundMethod(_, _, method) => match *method {
                Type::Fun(_, ret) => {
                    assert_eq!(*ret, Type::int());
                }
                _ => panic!("Expected Fun inside BoundMethod"),
            },
            _ => panic!("Expected BoundMethod, got {resolved:?}"),
        }
    }

    #[test]
    fn test_type_predicate_is_not_none_apply() {
        let optional_int = Type::optional(Type::int());
        let pred = TypePredicate::IsNotNone;

        let narrowed = pred.apply(&optional_int);
        assert_eq!(narrowed, Type::int(), "IsNotNone should remove None from union");
    }

    #[test]
    fn test_type_predicate_is_none_apply() {
        let optional_int = Type::optional(Type::int());
        let pred = TypePredicate::IsNone;

        let narrowed = pred.apply(&optional_int);
        assert_eq!(narrowed, Type::none(), "IsNone should narrow to None");
    }

    #[test]
    fn test_type_predicate_negate_is_not_none() {
        let pred = TypePredicate::IsNotNone;
        let negated = pred.negate();

        assert_eq!(negated, TypePredicate::IsNone, "Negation of IsNotNone should be IsNone");
    }

    #[test]
    fn test_type_predicate_negate_is_none() {
        let pred = TypePredicate::IsNone;
        let negated = pred.negate();

        assert_eq!(
            negated,
            TypePredicate::IsNotNone,
            "Negation of IsNone should be IsNotNone"
        );
    }

    #[test]
    fn test_solve_narrowing_constraint() {
        let var_name = "x".to_string();
        let pred = TypePredicate::IsNotNone;
        let narrowed_type = Type::int();

        let constraints =
            ConstraintSet { constraints: vec![Constraint::Narrowing(var_name, pred, narrowed_type, test_span())] };

        let registry = ClassRegistry::new();
        let result = solve_constraints(constraints, &registry);
        assert!(result.is_ok());

        let (_, errors) = result.unwrap();
        assert!(errors.is_empty(), "Narrowing constraint should not produce errors");
    }

    #[test]
    fn test_solve_join_constraint_single_type() {
        let var_name = "x".to_string();
        let incoming_types = vec![Type::int()];
        let result_type = tvar(0);

        let constraints = ConstraintSet {
            constraints: vec![Constraint::Join(
                var_name,
                incoming_types,
                result_type.clone(),
                test_span(),
            )],
        };

        let registry = ClassRegistry::new();
        let result = solve_constraints(constraints, &registry);
        assert!(result.is_ok());

        let (subst, errors) = result.unwrap();
        assert!(errors.is_empty(), "Join with single type should not produce errors");

        let resolved = subst.apply(&result_type);
        assert_eq!(resolved, Type::int(), "Join of single type should be that type");
    }

    #[test]
    fn test_solve_join_constraint_multiple_types() {
        let var_name = "x".to_string();
        let incoming_types = vec![Type::int(), Type::string()];
        let result_type = tvar(0);

        let constraints = ConstraintSet {
            constraints: vec![Constraint::Join(
                var_name,
                incoming_types.clone(),
                result_type.clone(),
                test_span(),
            )],
        };

        let registry = ClassRegistry::new();
        let result = solve_constraints(constraints, &registry);
        assert!(result.is_ok());

        let (subst, errors) = result.unwrap();
        assert!(errors.is_empty(), "Join with multiple types should not produce errors");

        let resolved = subst.apply(&result_type);
        match resolved {
            Type::Union(types) => {
                assert_eq!(types.len(), 2, "Join should create union of both types");
                assert!(types.contains(&Type::int()));
                assert!(types.contains(&Type::string()));
            }
            _ => panic!("Expected union type, got {resolved:?}"),
        }
    }

    #[test]
    fn test_solve_join_constraint_empty() {
        let var_name = "x".to_string();
        let incoming_types = vec![];
        let result_type = tvar(0);

        let constraints = ConstraintSet {
            constraints: vec![Constraint::Join(
                var_name,
                incoming_types,
                result_type.clone(),
                test_span(),
            )],
        };

        let registry = ClassRegistry::new();
        let result = solve_constraints(constraints, &registry);
        assert!(result.is_ok());

        let (subst, errors) = result.unwrap();
        assert!(errors.is_empty(), "Join with empty types should not produce errors");

        let resolved = subst.apply(&result_type);
        assert_eq!(resolved, Type::none(), "Join of no types should be None");
    }

    #[test]
    fn test_narrowing_with_union_type() {
        let union_type = Type::union(vec![Type::int(), Type::string(), Type::none()]);
        let pred = TypePredicate::IsNotNone;
        let narrowed = pred.apply(&union_type);

        match narrowed {
            Type::Union(types) => {
                assert_eq!(types.len(), 2, "Should have 2 types after removing None");
                assert!(types.contains(&Type::int()));
                assert!(types.contains(&Type::string()));
                assert!(!types.iter().any(|t| matches!(t, Type::Con(TypeCtor::NoneType))));
            }
            _ => panic!("Expected union type after narrowing, got {narrowed:?}"),
        }
    }

    #[test]
    fn test_narrowing_already_non_optional() {
        let int_type = Type::int();
        let pred = TypePredicate::IsNotNone;
        let narrowed = pred.apply(&int_type);
        assert_eq!(narrowed, Type::int(), "Narrowing non-optional type should be no-op");
    }

    #[test]
    fn test_type_predicate_isinstance_single_type() {
        let union_type = Type::union(vec![Type::int(), Type::string()]);
        let pred = TypePredicate::IsInstance(Type::int());
        let narrowed = pred.apply(&union_type);
        assert_eq!(narrowed, Type::int(), "isinstance(x, int) should narrow to int");
    }

    #[test]
    fn test_type_predicate_isinstance_union_target() {
        let union_type = Type::union(vec![Type::int(), Type::string(), Type::bool()]);
        let target = Type::union(vec![Type::int(), Type::string()]);
        let pred = TypePredicate::IsInstance(target.clone());
        let narrowed = pred.apply(&union_type);
        match narrowed {
            Type::Union(types) => {
                assert_eq!(types.len(), 2, "Should narrow to union of int and str");
                assert!(types.contains(&Type::int()));
                assert!(types.contains(&Type::string()));
                assert!(!types.contains(&Type::bool()));
            }
            _ => panic!("Expected union type after narrowing, got {narrowed:?}"),
        }
    }

    #[test]
    fn test_type_predicate_isinstance_non_union() {
        let int_type = Type::int();
        let pred = TypePredicate::IsInstance(Type::string());
        let narrowed = pred.apply(&int_type);
        assert_eq!(narrowed, Type::string(), "isinstance narrows to target type");
    }

    #[test]
    fn test_type_predicate_isinstance_target_in_union() {
        let union_type = Type::union(vec![Type::int(), Type::string(), Type::none()]);
        let pred = TypePredicate::IsInstance(Type::string());
        let narrowed = pred.apply(&union_type);
        assert_eq!(narrowed, Type::string(), "Should narrow to exact type in union");
    }

    #[test]
    fn test_type_predicate_isinstance_target_not_in_union() {
        let union_type = Type::union(vec![Type::int(), Type::string()]);
        let pred = TypePredicate::IsInstance(Type::bool());
        let narrowed = pred.apply(&union_type);
        assert_eq!(narrowed, Type::bool(), "Should narrow to target even if not in union");
    }

    #[test]
    fn test_type_predicate_isinstance_has_no_simple_negation() {
        let pred = TypePredicate::IsInstance(Type::int());
        assert!(
            !pred.has_simple_negation(),
            "isinstance should not have simple negation"
        );
    }

    #[test]
    fn test_solve_narrowing_isinstance_constraint() {
        let var_name = "x".to_string();
        let pred = TypePredicate::IsInstance(Type::int());
        let narrowed_type = Type::int();
        let constraints =
            ConstraintSet { constraints: vec![Constraint::Narrowing(var_name, pred, narrowed_type, test_span())] };

        let registry = ClassRegistry::new();
        let result = solve_constraints(constraints, &registry);
        assert!(result.is_ok());

        let (_, errors) = result.unwrap();
        assert!(
            errors.is_empty(),
            "isinstance narrowing constraint should not produce errors"
        );
    }

    #[test]
    fn test_isinstance_with_multiple_types() {
        let original = Type::union(vec![Type::int(), Type::string(), Type::bool(), Type::none()]);
        let target = Type::union(vec![Type::int(), Type::string()]);
        let pred = TypePredicate::IsInstance(target);
        let narrowed = pred.apply(&original);
        match narrowed {
            Type::Union(types) => {
                assert_eq!(types.len(), 2);
                assert!(types.contains(&Type::int()));
                assert!(types.contains(&Type::string()));
            }
            _ => panic!("Expected union, got {narrowed:?}"),
        }
    }

    #[test]
    fn test_isinstance_empty_intersection() {
        let original = Type::union(vec![Type::int(), Type::string()]);
        let target = Type::union(vec![Type::bool(), Type::float()]);
        let pred = TypePredicate::IsInstance(target.clone());
        let narrowed = pred.apply(&original);
        assert_eq!(narrowed, target);
    }

    #[test]
    fn test_type_predicate_is_truthy() {
        let optional_int = Type::optional(Type::int());
        let pred = TypePredicate::IsTruthy;
        let narrowed = pred.apply(&optional_int);
        assert_eq!(narrowed, Type::int(), "IsTruthy should remove None from union");
    }

    #[test]
    fn test_type_predicate_is_truthy_non_optional() {
        let int_type = Type::int();
        let pred = TypePredicate::IsTruthy;
        let narrowed = pred.apply(&int_type);
        assert_eq!(narrowed, Type::int(), "IsTruthy on non-optional should be no-op");
    }

    #[test]
    fn test_type_predicate_is_falsy() {
        let optional_int = Type::optional(Type::int());
        let pred = TypePredicate::IsFalsy;
        let narrowed = pred.apply(&optional_int);
        assert_eq!(narrowed, Type::none(), "IsFalsy should narrow Optional to None");
    }

    #[test]
    fn test_type_predicate_is_falsy_on_union() {
        let union_type = Type::union(vec![Type::int(), Type::string(), Type::none()]);
        let pred = TypePredicate::IsFalsy;
        let narrowed = pred.apply(&union_type);
        assert_eq!(narrowed, Type::none(), "IsFalsy should extract only None from union");
    }

    #[test]
    fn test_type_predicate_is_falsy_on_non_optional() {
        let int_type = Type::int();
        let pred = TypePredicate::IsFalsy;
        let narrowed = pred.apply(&int_type);
        assert_eq!(narrowed, Type::none(), "IsFalsy on non-optional type narrows to None");
    }

    #[test]
    fn test_type_predicate_truthiness_negate() {
        let truthy = TypePredicate::IsTruthy;
        let falsy = TypePredicate::IsFalsy;

        assert_eq!(truthy.negate(), falsy, "Negation of IsTruthy should be IsFalsy");
        assert_eq!(falsy.negate(), truthy, "Negation of IsFalsy should be IsTruthy");
    }

    #[test]
    fn test_type_predicate_truthiness_has_simple_negation() {
        let truthy = TypePredicate::IsTruthy;
        assert!(truthy.has_simple_negation(), "IsTruthy should have simple negation");
    }

    #[test]
    fn test_type_predicate_and() {
        let union_type = Type::union(vec![Type::int(), Type::string(), Type::none()]);
        let pred1 = TypePredicate::IsNotNone;
        let pred2 = TypePredicate::IsInstance(Type::int());
        let and_pred = TypePredicate::And(Box::new(pred1), Box::new(pred2));
        let narrowed = and_pred.apply(&union_type);
        assert_eq!(narrowed, Type::int(), "And should apply both predicates sequentially");
    }

    #[test]
    fn test_type_predicate_or() {
        let union_type = Type::union(vec![Type::int(), Type::string(), Type::bool()]);
        let pred1 = TypePredicate::IsInstance(Type::int());
        let pred2 = TypePredicate::IsInstance(Type::string());
        let or_pred = TypePredicate::Or(Box::new(pred1), Box::new(pred2));

        let narrowed = or_pred.apply(&union_type);
        match narrowed {
            Type::Union(types) => {
                assert_eq!(types.len(), 2);
                assert!(types.contains(&Type::int()));
                assert!(types.contains(&Type::string()));
            }
            _ => panic!("Expected union, got {narrowed:?}"),
        }
    }

    #[test]
    fn test_type_predicate_not() {
        let optional_int = Type::optional(Type::int());
        let truthy = TypePredicate::IsTruthy;
        let not_truthy = TypePredicate::Not(Box::new(truthy));
        let narrowed = not_truthy.apply(&optional_int);
        assert_eq!(narrowed, Type::none(), "Not(IsTruthy) should narrow to None");
    }

    #[test]
    fn test_type_predicate_not_isinstance() {
        let union_type = Type::union(vec![Type::int(), Type::string()]);
        let isinstance_pred = TypePredicate::IsInstance(Type::int());
        let not_isinstance = TypePredicate::Not(Box::new(isinstance_pred));
        let narrowed = not_isinstance.apply(&union_type);
        match narrowed {
            Type::Union(types) => {
                assert_eq!(types.len(), 2);
                assert!(types.contains(&Type::int()));
                assert!(types.contains(&Type::string()));
            }
            _ => panic!("Expected union type, got {narrowed:?}"),
        }
    }

    #[test]
    fn test_type_predicate_de_morgan_and() {
        let pred1 = TypePredicate::IsTruthy;
        let pred2 = TypePredicate::IsNotNone;
        let and_pred = TypePredicate::And(Box::new(pred1), Box::new(pred2));
        let negated = and_pred.negate();
        match negated {
            TypePredicate::Or(p1, p2) => {
                assert_eq!(*p1, TypePredicate::IsFalsy);
                assert_eq!(*p2, TypePredicate::IsNone);
            }
            _ => panic!("Expected Or predicate from De Morgan's law"),
        }
    }

    #[test]
    fn test_type_predicate_de_morgan_or() {
        let pred1 = TypePredicate::IsTruthy;
        let pred2 = TypePredicate::IsNotNone;
        let or_pred = TypePredicate::Or(Box::new(pred1), Box::new(pred2));
        let negated = or_pred.negate();
        match negated {
            TypePredicate::And(p1, p2) => {
                assert_eq!(*p1, TypePredicate::IsFalsy);
                assert_eq!(*p2, TypePredicate::IsNone);
            }
            _ => panic!("Expected And predicate from De Morgan's law"),
        }
    }

    #[test]
    fn test_type_predicate_double_negation() {
        let truthy = TypePredicate::IsTruthy;
        let not_not_truthy = TypePredicate::Not(Box::new(TypePredicate::Not(Box::new(truthy.clone()))));
        let result = not_not_truthy.negate();
        match result {
            TypePredicate::Not(inner) => match *inner {
                TypePredicate::IsTruthy => {}
                _ => panic!("Expected IsTruthy"),
            },
            _ => panic!("Expected Not predicate"),
        }
    }

    #[test]
    fn test_complex_predicate_combination() {
        let optional_union = Type::union(vec![Type::int(), Type::string(), Type::none()]);
        let truthy = TypePredicate::IsTruthy;
        let isinstance = TypePredicate::IsInstance(Type::int());
        let combined = TypePredicate::And(Box::new(truthy), Box::new(isinstance));
        let narrowed = combined.apply(&optional_union);
        assert_eq!(narrowed, Type::int(), "Complex predicate should narrow to int");
    }

    #[test]
    fn test_solve_narrowing_truthiness_constraint() {
        let var_name = "x".to_string();
        let pred = TypePredicate::IsTruthy;
        let narrowed_type = Type::int();
        let constraints =
            ConstraintSet { constraints: vec![Constraint::Narrowing(var_name, pred, narrowed_type, test_span())] };

        let registry = ClassRegistry::new();
        let result = solve_constraints(constraints, &registry);
        assert!(result.is_ok());

        let (_, errors) = result.unwrap();
        assert!(
            errors.is_empty(),
            "Truthiness narrowing constraint should not produce errors"
        );
    }

    #[test]
    fn test_solve_narrowing_and_constraint() {
        let var_name = "x".to_string();
        let pred = TypePredicate::And(
            Box::new(TypePredicate::IsNotNone),
            Box::new(TypePredicate::IsInstance(Type::int())),
        );
        let narrowed_type = Type::int();

        let constraints =
            ConstraintSet { constraints: vec![Constraint::Narrowing(var_name, pred, narrowed_type, test_span())] };

        let registry = ClassRegistry::new();
        let result = solve_constraints(constraints, &registry);
        assert!(result.is_ok());

        let (_, errors) = result.unwrap();
        assert!(
            errors.is_empty(),
            "And predicate narrowing constraint should not produce errors"
        );
    }

    #[test]
    fn test_while_loop_narrowing_constraint() {
        let var_name = "x".to_string();
        let pred = TypePredicate::IsNotNone;
        let narrowed_type = Type::int();

        let constraints =
            ConstraintSet { constraints: vec![Constraint::Narrowing(var_name, pred, narrowed_type, test_span())] };

        let registry = ClassRegistry::new();
        let result = solve_constraints(constraints, &registry);
        assert!(result.is_ok());

        let (_, errors) = result.unwrap();
        assert!(errors.is_empty(), "While loop narrowing should work like If narrowing");
    }

    #[test]
    fn test_try_except_exception_narrowing() {
        let exc_var = "e".to_string();
        let exception_type = Type::Var(TypeVar::new(0));
        let pred = TypePredicate::IsInstance(exception_type.clone());

        let constraints =
            ConstraintSet { constraints: vec![Constraint::Narrowing(exc_var, pred, exception_type, test_span())] };

        let registry = ClassRegistry::new();
        let result = solve_constraints(constraints, &registry);
        assert!(result.is_ok());

        let (_, errors) = result.unwrap();
        assert!(
            errors.is_empty(),
            "Exception variable narrowing should not produce errors"
        );
    }

    #[test]
    fn test_with_statement_context_manager_narrowing() {
        let target_var = "f".to_string();
        let file_type = Type::Var(TypeVar::new(0));
        let pred = TypePredicate::IsInstance(file_type.clone());

        let constraints =
            ConstraintSet { constraints: vec![Constraint::Narrowing(target_var, pred, file_type, test_span())] };

        let registry = ClassRegistry::new();
        let result = solve_constraints(constraints, &registry);
        assert!(result.is_ok());

        let (_, errors) = result.unwrap();
        assert!(errors.is_empty(), "Context manager narrowing should not produce errors");
    }

    #[test]
    fn test_while_loop_with_truthiness() {
        let optional_int = Type::optional(Type::int());
        let pred = TypePredicate::IsTruthy;

        let narrowed = pred.apply(&optional_int);
        assert_eq!(
            narrowed,
            Type::int(),
            "While loop with truthiness should narrow Optional to non-None"
        );
    }

    #[test]
    fn test_while_loop_with_isinstance() {
        let union_type = Type::union(vec![Type::int(), Type::string(), Type::none()]);
        let pred = TypePredicate::And(
            Box::new(TypePredicate::IsNotNone),
            Box::new(TypePredicate::IsInstance(Type::int())),
        );

        let narrowed = pred.apply(&union_type);
        assert_eq!(
            narrowed,
            Type::int(),
            "While loop with complex guard should narrow correctly"
        );
    }

    #[test]
    fn test_multiple_narrowing_constraints() {
        let var1 = "x".to_string();
        let var2 = "y".to_string();

        let constraints = ConstraintSet {
            constraints: vec![
                Constraint::Narrowing(var1, TypePredicate::IsNotNone, Type::int(), test_span()),
                Constraint::Narrowing(
                    var2,
                    TypePredicate::IsInstance(Type::string()),
                    Type::string(),
                    test_span(),
                ),
            ],
        };

        let registry = ClassRegistry::new();
        let result = solve_constraints(constraints, &registry);
        assert!(result.is_ok());

        let (_, errors) = result.unwrap();
        assert!(errors.is_empty(), "Multiple narrowing constraints should work together");
    }

    #[test]
    fn test_protocol_extract_list_element() {
        let list_int = Type::list(Type::int());
        let elem_ty = tvar(0);
        let constraints = ConstraintSet {
            constraints: vec![Constraint::Protocol(
                list_int,
                beacon_core::ProtocolName::Iterable,
                elem_ty.clone(),
                test_span(),
            )],
        };

        let registry = ClassRegistry::new();
        let result = solve_constraints(constraints, &registry);
        assert!(result.is_ok());

        let (subst, errors) = result.unwrap();
        assert!(errors.is_empty(), "list[int] should satisfy Iterable protocol");

        let resolved_elem = subst.apply(&elem_ty);
        assert_eq!(resolved_elem, Type::int(), "Element type should be extracted as int");
    }

    #[test]
    fn test_protocol_extract_dict_key() {
        let dict_str_int = Type::dict(Type::string(), Type::int());
        let elem_ty = tvar(0);
        let constraints = ConstraintSet {
            constraints: vec![Constraint::Protocol(
                dict_str_int,
                beacon_core::ProtocolName::Iterable,
                elem_ty.clone(),
                test_span(),
            )],
        };

        let registry = ClassRegistry::new();
        let result = solve_constraints(constraints, &registry);
        assert!(result.is_ok());

        let (subst, errors) = result.unwrap();
        assert!(errors.is_empty(), "dict should satisfy Iterable protocol");

        let resolved_elem = subst.apply(&elem_ty);
        assert_eq!(resolved_elem, Type::string(), "Iterating dict should yield keys (str)");
    }

    #[test]
    fn test_protocol_extract_set_element() {
        let set_float = Type::App(Box::new(Type::Con(TypeCtor::Set)), Box::new(Type::float()));
        let elem_ty = tvar(0);
        let constraints = ConstraintSet {
            constraints: vec![Constraint::Protocol(
                set_float,
                beacon_core::ProtocolName::Iterable,
                elem_ty.clone(),
                test_span(),
            )],
        };

        let registry = ClassRegistry::new();
        let result = solve_constraints(constraints, &registry);
        assert!(result.is_ok());

        let (subst, errors) = result.unwrap();
        assert!(errors.is_empty(), "set[float] should satisfy Iterable protocol");

        let resolved_elem = subst.apply(&elem_ty);
        assert_eq!(
            resolved_elem,
            Type::float(),
            "Element type should be extracted as float"
        );
    }

    #[test]
    fn test_protocol_extract_tuple_element() {
        let tuple_bool = Type::App(Box::new(Type::Con(TypeCtor::Tuple)), Box::new(Type::bool()));
        let elem_ty = tvar(0);
        let constraints = ConstraintSet {
            constraints: vec![Constraint::Protocol(
                tuple_bool,
                beacon_core::ProtocolName::Iterable,
                elem_ty.clone(),
                test_span(),
            )],
        };

        let registry = ClassRegistry::new();
        let result = solve_constraints(constraints, &registry);
        assert!(result.is_ok());

        let (subst, errors) = result.unwrap();
        assert!(errors.is_empty(), "tuple[bool] should satisfy Iterable protocol");

        let resolved_elem = subst.apply(&elem_ty);
        assert_eq!(resolved_elem, Type::bool(), "Element type should be extracted as bool");
    }

    #[test]
    fn test_protocol_non_iterable_type() {
        let int_ty = Type::int();
        let elem_ty = tvar(0);
        let constraints = ConstraintSet {
            constraints: vec![Constraint::Protocol(
                int_ty,
                beacon_core::ProtocolName::Iterable,
                elem_ty,
                test_span(),
            )],
        };

        let registry = ClassRegistry::new();
        let result = solve_constraints(constraints, &registry);
        assert!(result.is_ok());

        let (_, errors) = result.unwrap();
        assert!(!errors.is_empty(), "int should not satisfy Iterable protocol");
        assert!(
            errors[0].error.to_string().contains("does not satisfy protocol"),
            "Error should mention protocol not satisfied"
        );
    }

    #[test]
    fn test_bound_method_call_with_args() {
        let mut registry = ClassRegistry::new();
        let mut class_meta = ClassMetadata::new("MyClass".to_string());
        class_meta.methods.insert(
            "process".to_string(),
            MethodType::Single(Type::fun(vec![Type::any(), Type::int()], Type::string())),
        );
        registry.register_class("MyClass".to_string(), class_meta);

        let obj_ty = Type::Con(TypeCtor::Class("MyClass".to_string()));
        let method_ty = Type::BoundMethod(
            Box::new(obj_ty),
            "process".to_string(),
            Box::new(Type::fun(vec![Type::any(), Type::int()], Type::string())),
        );

        let ret_ty = tvar(0);
        let constraints = ConstraintSet {
            constraints: vec![Constraint::Call(
                method_ty,
                vec![Type::int()],
                ret_ty.clone(),
                test_span(),
            )],
        };

        let result = solve_constraints(constraints, &registry);
        assert!(result.is_ok());

        let (subst, errors) = result.unwrap();
        assert!(errors.is_empty(), "BoundMethod call should succeed");

        let resolved_ret = subst.apply(&ret_ty);
        assert_eq!(resolved_ret, Type::string(), "Return type should be string");
    }

    #[test]
    fn test_bound_method_call_wrong_arg_count() {
        let obj_ty = Type::Con(TypeCtor::Class("MyClass".to_string()));
        let method_ty = Type::BoundMethod(
            Box::new(obj_ty),
            "process".to_string(),
            Box::new(Type::fun(vec![Type::any(), Type::int()], Type::string())),
        );

        let ret_ty = tvar(0);
        let constraints = ConstraintSet {
            constraints: vec![Constraint::Call(
                method_ty,
                vec![Type::int(), Type::bool()],
                ret_ty,
                test_span(),
            )],
        };

        let registry = ClassRegistry::new();
        let result = solve_constraints(constraints, &registry);
        assert!(result.is_ok());

        let (_, errors) = result.unwrap();
        assert!(
            !errors.is_empty(),
            "BoundMethod call with wrong arg count should produce error"
        );
    }

    #[test]
    fn test_bound_method_call_wrong_arg_type() {
        let obj_ty = Type::Con(TypeCtor::Class("MyClass".to_string()));
        let method_ty = Type::BoundMethod(
            Box::new(obj_ty),
            "process".to_string(),
            Box::new(Type::fun(vec![Type::any(), Type::int()], Type::string())),
        );

        let ret_ty = tvar(0);
        let constraints =
            ConstraintSet { constraints: vec![Constraint::Call(method_ty, vec![Type::string()], ret_ty, test_span())] };

        let registry = ClassRegistry::new();
        let result = solve_constraints(constraints, &registry);
        assert!(result.is_ok());

        let (_, errors) = result.unwrap();
        assert!(
            !errors.is_empty(),
            "BoundMethod call with wrong arg type should produce error"
        );
    }

    #[test]
    fn test_protocol_with_type_variables() {
        let elem_inner = tvar(0);
        let list_ty = Type::list(elem_inner.clone());
        let elem_outer = tvar(1);

        let mut subst = beacon_core::Subst::empty();
        subst.insert(TypeVar { id: 0, hint: None }, Type::string());

        let constraints = ConstraintSet {
            constraints: vec![Constraint::Protocol(
                list_ty,
                beacon_core::ProtocolName::Iterable,
                elem_outer.clone(),
                test_span(),
            )],
        };

        let registry = ClassRegistry::new();
        let result = solve_constraints(constraints, &registry);
        assert!(result.is_ok());

        let (final_subst, errors) = result.unwrap();
        assert!(errors.is_empty(), "Protocol with type variables should work");

        let resolved_outer = final_subst.apply(&elem_outer);
        assert!(
            matches!(resolved_outer, Type::Var(_)) || resolved_outer == Type::string(),
            "Element type should be resolved correctly"
        );
    }

    #[test]
    fn test_optional_unifies_with_none_via_subtyping() {
        let optional_int = Type::optional(Type::int());
        let none_ty = Type::none();
        let constraints = ConstraintSet { constraints: vec![Constraint::Equal(optional_int, none_ty, test_span())] };
        let registry = ClassRegistry::new();
        let result = solve_constraints(constraints, &registry);
        assert!(result.is_ok());

        let (_, errors) = result.unwrap();
        assert!(
            errors.is_empty(),
            "Optional[int] should unify with None via subtyping, got {} errors",
            errors.len()
        );
    }

    #[test]
    fn test_union_unifies_with_member_via_subtyping() {
        let union_ty = Type::union(vec![Type::int(), Type::string(), Type::none()]);
        let str_ty = Type::string();

        let constraints = ConstraintSet { constraints: vec![Constraint::Equal(union_ty, str_ty, test_span())] };

        let registry = ClassRegistry::new();
        let result = solve_constraints(constraints, &registry);
        assert!(result.is_ok());

        let (_, errors) = result.unwrap();
        assert!(
            errors.is_empty(),
            "Union[int, str, None] should unify with str via subtyping, got {} errors",
            errors.len()
        );
    }

    #[test]
    fn test_none_unifies_with_optional_via_subtyping() {
        let none_ty = Type::none();
        let optional_str = Type::optional(Type::string());
        let constraints = ConstraintSet { constraints: vec![Constraint::Equal(none_ty, optional_str, test_span())] };
        let registry = ClassRegistry::new();
        let result = solve_constraints(constraints, &registry);
        assert!(result.is_ok());

        let (_, errors) = result.unwrap();
        assert!(
            errors.is_empty(),
            "None should unify with Optional[str] via subtyping, got {} errors",
            errors.len()
        );
    }

    #[test]
    fn test_union_member_unifies_with_union_via_subtyping() {
        let int_ty = Type::int();
        let union_ty = Type::union(vec![Type::int(), Type::string()]);
        let constraints = ConstraintSet { constraints: vec![Constraint::Equal(int_ty, union_ty, test_span())] };
        let registry = ClassRegistry::new();
        let result = solve_constraints(constraints, &registry);
        assert!(result.is_ok());

        let (_, errors) = result.unwrap();
        assert!(
            errors.is_empty(),
            "int should unify with Union[int, str] via subtyping, got {} errors",
            errors.len()
        );
    }

    #[test]
    fn test_non_union_types_still_use_unification() {
        let int_ty = Type::int();
        let str_ty = Type::string();
        let constraints = ConstraintSet { constraints: vec![Constraint::Equal(int_ty, str_ty, test_span())] };
        let registry = ClassRegistry::new();
        let result = solve_constraints(constraints, &registry);
        assert!(result.is_ok());

        let (_, errors) = result.unwrap();
        assert!(
            !errors.is_empty(),
            "Non-union types should still fail unification when incompatible"
        );
    }

    #[test]
    fn test_union_fails_when_not_subtype() {
        let union_ty = Type::union(vec![Type::int(), Type::string()]);
        let float_ty = Type::float();
        let constraints = ConstraintSet { constraints: vec![Constraint::Equal(union_ty, float_ty, test_span())] };
        let registry = ClassRegistry::new();
        let result = solve_constraints(constraints, &registry);
        assert!(result.is_ok());

        let (_, errors) = result.unwrap();
        assert!(
            !errors.is_empty(),
            "Union[int, str] should not unify with float when not a subtype"
        );
    }
}

use beacon_core::{
    ClassRegistry, MethodSignature, ProtocolName, Subst, Type, TypeCtor, TypeVarConstraintRegistry, Unifier,
};
use std::collections::HashSet;

/// Extract the base type constructor from a type application
///
/// For example, `dict[str, int]` is represented as `App(App(Dict, str), int)`, and this function extracts "dict".
pub(super) fn extract_base_constructor(ty: &Type) -> Option<&str> {
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
pub(super) fn type_to_method_signature(name: &str, ty: &Type) -> Option<MethodSignature> {
    match ty {
        Type::Fun(params, ret) => {
            let param_types: Vec<Type> = params.iter().map(|(_, ty)| ty.clone()).collect();
            Some(MethodSignature { name: name.to_string(), params: param_types, return_type: ret.as_ref().clone() })
        }
        _ => None,
    }
}

/// Check if a type satisfies a user-defined protocol
///
/// This method checks structural conformance by verifying that the type has all required methods with compatible signatures.
/// Uses full variance checking: contravariant parameters, covariant returns.
pub(super) fn check_user_defined_protocol(
    ty: &Type, protocol_name: &str, class_registry: &ClassRegistry,
    typevar_registry: &beacon_core::TypeVarConstraintRegistry,
) -> bool {
    check_user_defined_protocol_impl(ty, protocol_name, class_registry, typevar_registry, &mut HashSet::new())
}

/// Internal implementation of protocol checking with cycle detection
fn check_user_defined_protocol_impl(
    ty: &Type, protocol_name: &str, class_registry: &ClassRegistry,
    typevar_registry: &beacon_core::TypeVarConstraintRegistry, visited: &mut HashSet<String>,
) -> bool {
    if !visited.insert(protocol_name.to_string()) {
        return true;
    }

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
                    if let Some(ty) = method_type.primary_type()
                        && let Some(sig) = type_to_method_signature(method_name, ty)
                    {
                        available_sigs.push((method_name.clone(), sig));
                    }
                }

                let mut all_methods_ok = true;
                for required in &required_sigs {
                    let provided = available_sigs.iter().find(|(name, _)| name == &required.name);
                    let Some((_, provided_sig)) = provided else {
                        all_methods_ok = false;
                        break;
                    };

                    if provided_sig.params.len() != required.params.len() {
                        all_methods_ok = false;
                        break;
                    }

                    for (req_param, prov_param) in required.params.iter().zip(&provided_sig.params) {
                        let contravariant_ok = req_param.is_subtype_of(prov_param)
                            || types_compatible(prov_param, req_param, class_registry, typevar_registry);
                        if !contravariant_ok {
                            all_methods_ok = false;
                            break;
                        }
                    }

                    if !all_methods_ok {
                        break;
                    }

                    let covariant_ok = provided_sig.return_type.is_subtype_of(&required.return_type)
                        || types_compatible(
                            &provided_sig.return_type,
                            &required.return_type,
                            class_registry,
                            typevar_registry,
                        );
                    if !covariant_ok {
                        all_methods_ok = false;
                        break;
                    }
                }

                if all_methods_ok {
                    for base_name in &protocol_meta.base_classes {
                        if let Some(base_meta) = class_registry.get_class(base_name)
                            && base_meta.is_protocol
                            && !check_user_defined_protocol_impl(
                                ty,
                                base_name,
                                class_registry,
                                typevar_registry,
                                visited,
                            )
                        {
                            all_methods_ok = false;
                            break;
                        }
                    }
                }

                all_methods_ok
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
pub(super) fn check_builtin_protocol_on_class(
    ty: &Type, protocol: &ProtocolName, class_registry: &ClassRegistry,
) -> bool {
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
        ProtocolName::Iterable => class_registry
            .lookup_attribute_with_inheritance(class_name, "__iter__")
            .is_some(),
        ProtocolName::Iterator => {
            class_registry
                .lookup_attribute_with_inheritance(class_name, "__iter__")
                .is_some()
                && class_registry
                    .lookup_attribute_with_inheritance(class_name, "__next__")
                    .is_some()
        }
        ProtocolName::Sized => class_registry
            .lookup_attribute_with_inheritance(class_name, "__len__")
            .is_some(),
        ProtocolName::Sequence => {
            class_registry
                .lookup_attribute_with_inheritance(class_name, "__len__")
                .is_some()
                && class_registry
                    .lookup_attribute_with_inheritance(class_name, "__getitem__")
                    .is_some()
        }
        ProtocolName::Mapping => {
            class_registry
                .lookup_attribute_with_inheritance(class_name, "__len__")
                .is_some()
                && class_registry
                    .lookup_attribute_with_inheritance(class_name, "__getitem__")
                    .is_some()
                && class_registry
                    .lookup_attribute_with_inheritance(class_name, "__iter__")
                    .is_some()
        }
        ProtocolName::AsyncIterable => class_registry
            .lookup_attribute_with_inheritance(class_name, "__aiter__")
            .is_some(),
        ProtocolName::AsyncIterator => {
            class_registry
                .lookup_attribute_with_inheritance(class_name, "__aiter__")
                .is_some()
                && class_registry
                    .lookup_attribute_with_inheritance(class_name, "__anext__")
                    .is_some()
        }
        ProtocolName::Awaitable => class_registry
            .lookup_attribute_with_inheritance(class_name, "__await__")
            .is_some(),
        ProtocolName::ContextManager => {
            class_registry
                .lookup_attribute_with_inheritance(class_name, "__enter__")
                .is_some()
                && class_registry
                    .lookup_attribute_with_inheritance(class_name, "__exit__")
                    .is_some()
        }
        ProtocolName::AsyncContextManager => {
            class_registry
                .lookup_attribute_with_inheritance(class_name, "__aenter__")
                .is_some()
                && class_registry
                    .lookup_attribute_with_inheritance(class_name, "__aexit__")
                    .is_some()
        }
        ProtocolName::Callable => class_registry
            .lookup_attribute_with_inheritance(class_name, "__call__")
            .is_some(),
        ProtocolName::UserDefined(_) => false,
    }
}

/// Check if a type has a specific attribute
pub(super) fn check_has_attribute(ty: &Type, attr_name: &str, class_registry: &ClassRegistry) -> bool {
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
            class_name.is_some_and(|name| {
                class_registry
                    .lookup_attribute_with_inheritance(name, attr_name)
                    .is_some()
            })
        }
        Type::App(base, _) => {
            let base_ctor = extract_base_constructor(base);
            base_ctor.is_some_and(|name| {
                class_registry
                    .lookup_attribute_with_inheritance(name, attr_name)
                    .is_some()
            })
        }
        Type::Var(_) => true,
        _ => false,
    }
}

/// Get the type of an attribute from a type
pub(super) fn get_attribute_type(ty: &Type, attr_name: &str, class_registry: &ClassRegistry) -> Option<Type> {
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
            class_name.and_then(|name| class_registry.lookup_attribute_with_inheritance(name, attr_name))
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

                if let Some(name) = class_name
                    && let Some(attr_type) = class_registry.lookup_attribute_with_inheritance(name, attr_name)
                {
                    if let Some(class_metadata) = class_registry.get_class(name)
                        && !class_metadata.type_params.is_empty()
                    {
                        let subst = class_metadata.create_type_substitution(&type_args);
                        return Some(beacon_core::ClassMetadata::substitute_type_params(&attr_type, &subst));
                    }
                    return Some(attr_type);
                }
            }

            if let Some((class_name, type_args)) = ty.unapply_class()
                && let Some(attr_type) = class_registry.lookup_attribute_with_inheritance(class_name, attr_name)
            {
                if let Some(class_metadata) = class_registry.get_class(class_name)
                    && !class_metadata.type_params.is_empty()
                {
                    let subst = class_metadata.create_type_substitution(&type_args);
                    return Some(beacon_core::ClassMetadata::substitute_type_params(&attr_type, &subst));
                }
                return Some(attr_type);
            }

            let base_ctor = extract_base_constructor(ty);
            base_ctor.and_then(|name| class_registry.lookup_attribute_with_inheritance(name, attr_name))
        }
        _ => None,
    }
}

pub(super) fn classes_compatible(
    actual: &Type, expected: &Type, class_registry: &ClassRegistry,
    typevar_registry: &beacon_core::TypeVarConstraintRegistry,
) -> bool {
    if matches!(expected, Type::Con(TypeCtor::Any)) || matches!(actual, Type::Con(TypeCtor::Any)) {
        return true;
    }

    if let Some((protocol_name, expected_args)) = expected.unapply_protocol() {
        if !expected_args.is_empty() {
            let result =
                check_protocol_with_variance(actual, protocol_name, &expected_args, class_registry, typevar_registry);
            return result;
        } else if let Some(metadata) = class_registry.get_class(protocol_name)
            && metadata.is_protocol
        {
            return check_user_defined_protocol(actual, protocol_name, class_registry, typevar_registry);
        }
    }

    if let Some((expected_name, expected_args)) = class_info(expected) {
        if let Some((actual_name, actual_args)) = class_info(actual)
            && class_registry.is_subclass_of(&actual_name, &expected_name)
        {
            if !expected_args.is_empty() && actual_args.len() == expected_args.len() {
                let variance_info =
                    class_registry
                        .get_class(&actual_name)
                        .or_else(|| class_registry.get_class(&expected_name))
                        .and_then(|metadata| {
                            if metadata.type_param_vars.is_empty() { None } else { Some(&metadata.type_param_vars) }
                        });

                for (i, (actual_arg, expected_arg)) in actual_args.iter().zip(expected_args.iter()).enumerate() {
                    let variance = variance_info
                        .and_then(|vars| vars.get(i))
                        .map(|tv| tv.variance)
                        .unwrap_or(beacon_core::Variance::Invariant);

                    let compatible = match variance {
                        beacon_core::Variance::Covariant => {
                            types_compatible(actual_arg, expected_arg, class_registry, typevar_registry)
                        }
                        beacon_core::Variance::Contravariant => {
                            types_compatible(expected_arg, actual_arg, class_registry, typevar_registry)
                        }
                        beacon_core::Variance::Invariant => {
                            actual_arg == expected_arg
                                || Unifier::unify_with_class_registry(
                                    actual_arg,
                                    expected_arg,
                                    typevar_registry,
                                    class_registry,
                                )
                                .is_ok()
                        }
                    };

                    if !compatible {
                        return false;
                    }
                }
            }
            return true;
        }

        if let Some(metadata) = class_registry.get_class(&expected_name)
            && metadata.is_protocol
        {
            if !expected_args.is_empty() {
                return check_protocol_with_variance(
                    actual,
                    &expected_name,
                    &expected_args,
                    class_registry,
                    typevar_registry,
                );
            } else {
                return check_user_defined_protocol(actual, &expected_name, class_registry, typevar_registry);
            }
        }
    }

    false
}

pub(super) fn class_info(ty: &Type) -> Option<(String, Vec<Type>)> {
    match ty {
        Type::Con(TypeCtor::Class(name)) => Some((name.clone(), Vec::new())),
        _ => ty.unapply_class().map(|(name, args)| (name.to_string(), args)),
    }
}

pub(super) fn iterable_compatible(
    actual: &Type, expected: &Type, class_registry: &ClassRegistry,
    typevar_registry: &beacon_core::TypeVarConstraintRegistry,
) -> bool {
    if let Some((yield_ty, _send_ty, _return_ty)) = actual.extract_generator_params()
        && let Some((expected_ctor, expected_args)) = expected.unapply()
    {
        let expects_iterable = matches!(expected_ctor, TypeCtor::Iterable)
            || matches!(expected_ctor, TypeCtor::Class(name) if name == "Iterable");

        if expects_iterable {
            if let Some(elem_ty) = expected_args.first() {
                return types_compatible(yield_ty, elem_ty, class_registry, typevar_registry);
            }
            return true;
        }
    }

    if let Some((actual_ctor, actual_args)) = actual.unapply() {
        if let Some((expected_ctor, expected_args)) = expected.unapply() {
            if actual_args.len() != expected_args.len() {
                return false;
            }

            let bases_match = match (actual_ctor, expected_ctor) {
                (TypeCtor::List, TypeCtor::Iterable) => true,
                (TypeCtor::List, TypeCtor::Class(name)) if name == "Iterable" => true,
                (TypeCtor::Class(name), TypeCtor::Iterable) if name == "list" => true,
                (TypeCtor::Class(name), TypeCtor::Class(exp_name)) if name == "list" && exp_name == "Iterable" => true,
                _ => false,
            };

            bases_match
                && actual_args
                    .iter()
                    .zip(expected_args.iter())
                    .all(|(a, e)| types_compatible(a, e, class_registry, typevar_registry))
        } else {
            false
        }
    } else {
        false
    }
}

/// Check if a function type is compatible with another, respecting variance.
///
/// For function subtyping: Callable[P1, R1] <: Callable[P2, R2] requires:
/// - Parameters are contravariant: P2 <: P1 (note the flip!)
/// - Return type is covariant: R1 <: R2
pub(super) fn function_compatible(
    actual: &Type, expected: &Type, class_registry: &ClassRegistry,
    typevar_registry: &beacon_core::TypeVarConstraintRegistry,
) -> bool {
    match (actual, expected) {
        (Type::Fun(params_a, ret_a), Type::Fun(params_e, ret_e)) => {
            if params_a.len() != params_e.len() {
                return false;
            }

            let params_ok = params_a
                .iter()
                .zip(params_e.iter())
                .all(|((_, a), (_, e))| types_compatible(e, a, class_registry, typevar_registry));

            let ret_ok = types_compatible(ret_a, ret_e, class_registry, typevar_registry);

            params_ok && ret_ok
        }
        _ => false,
    }
}

/// Check if a concrete type satisfies a protocol with specific type arguments, respecting variance.
pub(super) fn check_protocol_with_variance(
    actual: &Type, protocol_name: &str, expected_type_args: &[Type], class_registry: &ClassRegistry,
    typevar_registry: &beacon_core::TypeVarConstraintRegistry,
) -> bool {
    let protocol_meta = match class_registry.get_class(protocol_name) {
        Some(meta) if meta.is_protocol => meta,
        _ => return false,
    };

    let (actual_class_name, actual_type_args) = match actual {
        Type::Con(TypeCtor::Class(name)) => (name.clone(), Vec::new()),
        Type::App(_, _) => {
            if let Some((name, args)) = actual.unapply_class() {
                (name.to_string(), args)
            } else {
                return false;
            }
        }
        _ => {
            return false;
        }
    };

    let actual_class_meta = match class_registry.get_class(&actual_class_name) {
        Some(meta) => meta,
        None => {
            return false;
        }
    };

    let protocol_methods = protocol_meta.get_protocol_methods();
    let mut inferred_type_args = Vec::new();

    for param_index in 0..protocol_meta.type_param_vars.len() {
        let type_var = &protocol_meta.type_param_vars[param_index];
        let default_name = format!("T{param_index}");
        let type_var_name = type_var.hint.as_ref().unwrap_or(&default_name);

        let mut inferred_type = None;
        for (method_name, protocol_method_ty) in &protocol_methods {
            if let Some(actual_method) = actual_class_meta.lookup_method(method_name) {
                let actual_method_resolved = if !actual_type_args.is_empty()
                    && !actual_class_meta.type_param_vars.is_empty()
                {
                    let mut subst = beacon_core::Subst::empty();
                    for (class_tv, class_ty) in actual_class_meta.type_param_vars.iter().zip(actual_type_args.iter()) {
                        subst.insert(class_tv.clone(), class_ty.clone());
                    }
                    subst.apply(actual_method)
                } else {
                    actual_method.clone()
                };

                if let Some(inferred) =
                    infer_type_param_from_methods(protocol_method_ty, &actual_method_resolved, type_var_name)
                {
                    inferred_type = Some(inferred);
                    break;
                }
            }
        }

        let inferred = inferred_type.unwrap_or_else(Type::any);
        inferred_type_args.push(inferred);
    }

    if inferred_type_args.len() != expected_type_args.len() {
        return false;
    }

    let mut protocol_subst_map = std::collections::HashMap::new();
    for (param, inferred_arg) in protocol_meta.type_params.iter().zip(inferred_type_args.iter()) {
        protocol_subst_map.insert(param.clone(), inferred_arg.clone());
    }

    for (method_name, protocol_method_ty) in &protocol_methods {
        let actual_method = match actual_class_meta.lookup_method(method_name) {
            Some(m) => m,
            None => {
                return false;
            }
        };

        let expected_method =
            beacon_core::ClassMetadata::substitute_type_params(protocol_method_ty, &protocol_subst_map);

        let actual_method_resolved = if !actual_type_args.is_empty() && !actual_class_meta.type_param_vars.is_empty() {
            let mut actual_subst = beacon_core::Subst::empty();
            for (class_tv, class_ty) in actual_class_meta.type_param_vars.iter().zip(actual_type_args.iter()) {
                actual_subst.insert(class_tv.clone(), class_ty.clone());
            }
            actual_subst.apply(actual_method)
        } else {
            actual_method.clone()
        };

        let sig_compatible = method_signatures_compatible(
            &expected_method,
            &actual_method_resolved,
            class_registry,
            typevar_registry,
        );
        if !sig_compatible {
            return false;
        }
    }

    for (i, (inferred_arg, expected_arg)) in inferred_type_args.iter().zip(expected_type_args.iter()).enumerate() {
        let variance = protocol_meta
            .type_param_vars
            .get(i)
            .map(|tv| tv.variance)
            .unwrap_or(beacon_core::Variance::Invariant);

        let compatible = match variance {
            beacon_core::Variance::Covariant => {
                let types_compat = types_compatible(inferred_arg, expected_arg, class_registry, typevar_registry);
                let is_subtype = inferred_arg.is_subtype_of(expected_arg);
                let builtin_to_object = matches!(expected_arg, Type::Con(TypeCtor::Class(name)) if name == "object")
                    && matches!(
                        inferred_arg,
                        Type::Con(
                            TypeCtor::Int
                                | TypeCtor::String
                                | TypeCtor::Float
                                | TypeCtor::Bool
                                | TypeCtor::List
                                | TypeCtor::Dict
                                | TypeCtor::Set
                                | TypeCtor::Tuple
                        )
                    );
                types_compat || is_subtype || builtin_to_object
            }
            beacon_core::Variance::Contravariant => {
                let types_compat = types_compatible(expected_arg, inferred_arg, class_registry, typevar_registry);
                let is_subtype = expected_arg.is_subtype_of(inferred_arg);
                let object_is_supertype = matches!(inferred_arg, Type::Con(TypeCtor::Class(name)) if name == "object")
                    && matches!(
                        expected_arg,
                        Type::Con(
                            TypeCtor::Int
                                | TypeCtor::String
                                | TypeCtor::Float
                                | TypeCtor::Bool
                                | TypeCtor::List
                                | TypeCtor::Dict
                                | TypeCtor::Set
                                | TypeCtor::Tuple
                        )
                    );
                types_compat || is_subtype || object_is_supertype
            }
            beacon_core::Variance::Invariant => {
                inferred_arg == expected_arg
                    || beacon_core::Unifier::unify_with_class_registry(
                        inferred_arg,
                        expected_arg,
                        typevar_registry,
                        class_registry,
                    )
                    .is_ok()
            }
        };

        if !compatible {
            return false;
        }
    }

    true
}

/// Check if two method signatures are compatible
pub(super) fn method_signatures_compatible(
    expected: &Type, actual: &Type, class_registry: &ClassRegistry,
    typevar_registry: &beacon_core::TypeVarConstraintRegistry,
) -> bool {
    match (expected, actual) {
        (Type::Fun(expected_params, expected_ret), Type::Fun(actual_params, actual_ret)) => {
            if expected_params.len() != actual_params.len() {
                return false;
            }

            for (exp_param, act_param) in expected_params.iter().skip(1).zip(actual_params.iter().skip(1)) {
                let param_ok = act_param.1.is_subtype_of(&exp_param.1)
                    || types_compatible(&exp_param.1, &act_param.1, class_registry, typevar_registry);
                if !param_ok {
                    return false;
                }
            }

            actual_ret.is_subtype_of(expected_ret)
                || types_compatible(actual_ret, expected_ret, class_registry, typevar_registry)
        }
        _ => false,
    }
}

/// Check if a concrete type satisfies a protocol instantiation with variance.
///
/// This function:
/// 1. Checks that actual structurally satisfies the protocol
/// 2. Infers the type parameter from actual's methods
/// 3. Checks if the inferred protocol type is compatible with expected using variance
pub(super) fn protocol_instantiation_compatible(
    actual: &Type, expected: &Type, cls_registry: &ClassRegistry, tv_registry: &TypeVarConstraintRegistry,
) -> bool {
    let (protocol_name, expected_type_args) = match expected {
        Type::Con(TypeCtor::Protocol(Some(name), _)) => (name, vec![]),

        _ => {
            let (protocol_ctor, args) = match expected.unapply() {
                Some((ctor, args)) => (ctor, args),
                None => return false,
            };
            match protocol_ctor {
                TypeCtor::Protocol(Some(name), _) => (name, args),
                _ => return false,
            }
        }
    };

    let protocol_meta = match cls_registry.get_class(protocol_name) {
        Some(meta) if meta.is_protocol => meta,
        _ => return false,
    };

    let actual_class_name = match actual {
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

    if !check_user_defined_protocol(actual, protocol_name, cls_registry, tv_registry) {
        return false;
    }

    if expected_type_args.is_empty() {
        return true;
    }

    let actual_class_meta = match cls_registry.get_class(actual_class_name) {
        Some(meta) => meta,
        None => return false,
    };

    let protocol_methods = protocol_meta.get_protocol_methods();
    let mut inferred_type_args = Vec::new();

    for param_index in 0..protocol_meta.type_param_vars.len() {
        let type_var = &protocol_meta.type_param_vars[param_index];
        let default_name = format!("T{param_index}");
        let type_var_name = type_var.hint.as_ref().unwrap_or(&default_name);

        let mut inferred_type = None;
        for (method_name, protocol_method_ty) in &protocol_methods {
            if let Some(actual_method) = actual_class_meta.lookup_method(method_name)
                && let Some(inferred) = infer_type_param_from_methods(protocol_method_ty, actual_method, type_var_name)
            {
                inferred_type = Some(inferred);
                break;
            }
        }

        inferred_type_args.push(inferred_type.unwrap_or_else(Type::any));
    }

    if inferred_type_args.len() != expected_type_args.len() {
        return false;
    }

    let enriched_protocol = expected.clone().enrich_protocol_variance(cls_registry);

    let mut inferred_protocol_ty = Type::Con(TypeCtor::Protocol(
        Some(protocol_name.clone()),
        protocol_meta.type_param_vars.iter().map(|tv| tv.variance).collect(),
    ));
    for arg in inferred_type_args {
        inferred_protocol_ty = Type::App(Box::new(inferred_protocol_ty), Box::new(arg));
    }

    inferred_protocol_ty.is_subtype_of(&enriched_protocol)
}

/// Infer a type parameter from protocol and actual method signatures.
///
/// Given a protocol method signature containing a type variable (e.g., `T`) and a method signature, try to infer what `T` should be.
pub(super) fn infer_type_param_from_methods(
    protocol_method: &Type, actual_method: &Type, type_var_name: &str,
) -> Option<Type> {
    match (protocol_method, actual_method) {
        (Type::Fun(protocol_params, protocol_ret), Type::Fun(actual_params, actual_ret)) => {
            if contains_type_var(protocol_ret, type_var_name) {
                return Some(actual_ret.as_ref().clone());
            }

            for (protocol_param, actual_param) in protocol_params.iter().zip(actual_params.iter()) {
                if contains_type_var(&protocol_param.1, type_var_name) {
                    return Some(actual_param.1.clone());
                }
            }

            None
        }
        _ => None,
    }
}

/// Check if a type contains a type variable with the given name.
pub(super) fn contains_type_var(ty: &Type, var_name: &str) -> bool {
    match ty {
        Type::Con(TypeCtor::TypeVariable(name)) => name == var_name,
        Type::App(ctor, arg) => contains_type_var(ctor, var_name) || contains_type_var(arg, var_name),
        Type::Fun(params, ret) => {
            params.iter().any(|(_, ty)| contains_type_var(ty, var_name)) || contains_type_var(ret, var_name)
        }
        Type::Union(types) | Type::Intersection(types) | Type::Tuple(types) => {
            types.iter().any(|t| contains_type_var(t, var_name))
        }
        _ => false,
    }
}

pub(super) fn types_compatible(
    actual: &Type, expected: &Type, class_registry: &ClassRegistry,
    typevar_registry: &beacon_core::TypeVarConstraintRegistry,
) -> bool {
    if actual == expected {
        return true;
    }

    if matches!(expected, Type::Con(TypeCtor::Any)) || matches!(actual, Type::Con(TypeCtor::Any)) {
        return true;
    }

    if matches!(expected, Type::Con(TypeCtor::TypeVariable(_)))
        || matches!(actual, Type::Con(TypeCtor::TypeVariable(_)))
    {
        return true;
    }

    if function_compatible(actual, expected, class_registry, typevar_registry) {
        return true;
    }

    if classes_compatible(actual, expected, class_registry, typevar_registry) {
        return true;
    }

    if iterable_compatible(actual, expected, class_registry, typevar_registry) {
        return true;
    }

    if protocol_instantiation_compatible(actual, expected, class_registry, typevar_registry) {
        return true;
    }

    if generator_compatible(actual, expected, class_registry) {
        return true;
    }

    false
}

/// Check if actual Generator/AsyncGenerator/Coroutine type is compatible with expected type considering mixed variance (covariant yield/return, contravariant send)
pub(super) fn generator_compatible(actual: &Type, expected: &Type, class_registry: &ClassRegistry) -> bool {
    let typevar_registry = &beacon_core::TypeVarConstraintRegistry::new();

    if let (Some((actual_y, actual_s, actual_r)), Some((expected_y, expected_s, expected_r))) =
        (actual.extract_generator_params(), expected.extract_generator_params())
    {
        let y_compatible = types_compatible(actual_y, expected_y, class_registry, typevar_registry);
        let s_compatible = types_compatible(expected_s, actual_s, class_registry, typevar_registry);
        let r_compatible = types_compatible(actual_r, expected_r, class_registry, typevar_registry);
        return y_compatible && s_compatible && r_compatible;
    }

    if let (Some((actual_y, actual_s)), Some((expected_y, expected_s))) = (
        actual.extract_async_generator_params(),
        expected.extract_async_generator_params(),
    ) {
        let y_compatible = types_compatible(actual_y, expected_y, class_registry, typevar_registry);
        let s_compatible = types_compatible(expected_s, actual_s, class_registry, typevar_registry);
        return y_compatible && s_compatible;
    }

    if let (Some((actual_y, actual_s, actual_r)), Some((expected_y, expected_s, expected_r))) =
        (actual.extract_coroutine_params(), expected.extract_coroutine_params())
    {
        let y_compatible = types_compatible(actual_y, expected_y, class_registry, typevar_registry);
        let s_compatible = types_compatible(expected_s, actual_s, class_registry, typevar_registry);
        let r_compatible = types_compatible(actual_r, expected_r, class_registry, typevar_registry);
        return y_compatible && s_compatible && r_compatible;
    }

    false
}

pub(super) fn generator_iterable_subst(
    actual: &Type, expected: &Type, typevar_registry: &beacon_core::TypeVarConstraintRegistry,
) -> Option<Subst> {
    if let (Type::Fun(actual_params, actual_ret), Type::Fun(expected_params, expected_ret)) = (actual, expected) {
        if actual_params.len() == expected_params.len() {
            let mut combined = Subst::empty();
            for (param_actual, param_expected) in actual_params.iter().zip(expected_params.iter()) {
                if let Some(sub) = generator_iterable_subst(&param_actual.1, &param_expected.1, typevar_registry) {
                    combined = sub.compose(combined);
                }
            }

            if let Some(ret_subst) = generator_iterable_subst(actual_ret, expected_ret, typevar_registry) {
                return Some(ret_subst.compose(combined));
            }

            return if combined.is_empty() { None } else { Some(combined) };
        }

        return generator_iterable_subst(actual_ret, expected_ret, typevar_registry);
    }

    if let Type::Union(variants) = actual {
        for variant in variants {
            if let Some(sub) = generator_iterable_subst(variant, expected, typevar_registry) {
                return Some(sub);
            }
        }
        return None;
    }

    if let Type::Union(variants) = expected {
        for variant in variants {
            if let Some(sub) = generator_iterable_subst(actual, variant, typevar_registry) {
                return Some(sub);
            }
        }
        return None;
    }

    let (yield_ty, _send_ty, _ret_ty) = actual.extract_generator_params()?;
    let (expected_ctor, expected_args) = expected.unapply()?;
    let expects_iterable = matches!(expected_ctor, TypeCtor::Iterable)
        || matches!(expected_ctor, TypeCtor::Class(name) if name == "Iterable");
    if !expects_iterable {
        return None;
    }
    let elem_ty = expected_args.first()?;
    Unifier::unify(yield_ty, elem_ty, typevar_registry).ok()
}

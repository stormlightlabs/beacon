use crate::{
    Constraint, ConstraintSet, Span, TypeErrorInfo,
    exhaustiveness::{ExhaustivenessResult, ReachabilityResult, check_exhaustiveness, check_reachability},
    pattern_validation::{validate_pattern_structure, validate_pattern_type_compatibility},
};

use beacon_core::{
    BeaconError, ClassMetadata, ClassRegistry, MethodSignature, ProtocolChecker, ProtocolName, Result, Subst, Type,
    TypeCtor, TypeError, TypeVar, TypeVarConstraintRegistry, Unifier,
};
use std::{collections::HashSet, result};

struct CallContext<'a> {
    subst: &'a mut Subst,
    type_errors: &'a mut Vec<TypeErrorInfo>,
    class_registry: &'a ClassRegistry,
    typevar_registry: &'a beacon_core::TypeVarConstraintRegistry,
    span: Span,
}

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
fn check_user_defined_protocol(
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
                    if let Some(ty) = method_type.primary_type() {
                        if let Some(sig) = type_to_method_signature(method_name, ty) {
                            available_sigs.push((method_name.clone(), sig));
                        }
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
                        if let Some(base_meta) = class_registry.get_class(base_name) {
                            if base_meta.is_protocol
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
        ProtocolName::Iterable => class_registry.lookup_attribute_with_inheritance(class_name, "__iter__").is_some(),
        ProtocolName::Iterator => {
            class_registry.lookup_attribute_with_inheritance(class_name, "__iter__").is_some()
                && class_registry.lookup_attribute_with_inheritance(class_name, "__next__").is_some()
        }
        ProtocolName::Sized => class_registry.lookup_attribute_with_inheritance(class_name, "__len__").is_some(),
        ProtocolName::Sequence => {
            class_registry.lookup_attribute_with_inheritance(class_name, "__len__").is_some()
                && class_registry.lookup_attribute_with_inheritance(class_name, "__getitem__").is_some()
        }
        ProtocolName::Mapping => {
            class_registry.lookup_attribute_with_inheritance(class_name, "__len__").is_some()
                && class_registry.lookup_attribute_with_inheritance(class_name, "__getitem__").is_some()
                && class_registry.lookup_attribute_with_inheritance(class_name, "__iter__").is_some()
        }
        ProtocolName::AsyncIterable => class_registry.lookup_attribute_with_inheritance(class_name, "__aiter__").is_some(),
        ProtocolName::AsyncIterator => {
            class_registry.lookup_attribute_with_inheritance(class_name, "__aiter__").is_some()
                && class_registry.lookup_attribute_with_inheritance(class_name, "__anext__").is_some()
        }
        ProtocolName::Awaitable => class_registry.lookup_attribute_with_inheritance(class_name, "__await__").is_some(),
        ProtocolName::ContextManager => {
            class_registry.lookup_attribute_with_inheritance(class_name, "__enter__").is_some()
                && class_registry.lookup_attribute_with_inheritance(class_name, "__exit__").is_some()
        }
        ProtocolName::AsyncContextManager => {
            class_registry.lookup_attribute_with_inheritance(class_name, "__aenter__").is_some()
                && class_registry.lookup_attribute_with_inheritance(class_name, "__aexit__").is_some()
        }
        ProtocolName::Callable => class_registry.lookup_attribute_with_inheritance(class_name, "__call__").is_some(),
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
            class_name.is_some_and(|name| class_registry.lookup_attribute_with_inheritance(name, attr_name).is_some())
        }
        Type::App(base, _) => {
            let base_ctor = extract_base_constructor(base);
            base_ctor.is_some_and(|name| class_registry.lookup_attribute_with_inheritance(name, attr_name).is_some())
        }
        Type::Var(_) => true,
        _ => false,
    }
}

/// Get the type of an attribute from a type
///
/// TODO: Some builtin type methods (e.g., str.upper, str.lower, dict.get) may not be found
/// depending on stub loading timing and class registration order. This suggests edge cases
/// in stub parsing or class registry population that need investigation.
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

                if let Some(name) = class_name {
                    if let Some(attr_type) = class_registry.lookup_attribute_with_inheritance(name, attr_name) {
                        if let Some(class_metadata) = class_registry.get_class(name) {
                            if !class_metadata.type_params.is_empty() {
                                let subst = class_metadata.create_type_substitution(&type_args);
                                return Some(beacon_core::ClassMetadata::substitute_type_params(&attr_type, &subst));
                            }
                        }
                        return Some(attr_type);
                    }
                }
            }

            if let Some((class_name, type_args)) = ty.unapply_class() {
                if let Some(attr_type) = class_registry.lookup_attribute_with_inheritance(class_name, attr_name) {
                    if let Some(class_metadata) = class_registry.get_class(class_name) {
                        if !class_metadata.type_params.is_empty() {
                            let subst = class_metadata.create_type_substitution(&type_args);
                            return Some(beacon_core::ClassMetadata::substitute_type_params(&attr_type, &subst));
                        }
                    }
                    return Some(attr_type);
                }
            }

            let base_ctor = extract_base_constructor(ty);
            base_ctor.and_then(|name| class_registry.lookup_attribute_with_inheritance(name, attr_name))
        }
        _ => None,
    }
}

fn classes_compatible(
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
        } else if let Some(metadata) = class_registry.get_class(protocol_name) {
            if metadata.is_protocol {
                return check_user_defined_protocol(actual, protocol_name, class_registry, typevar_registry);
            }
        }
    }

    if let Some((expected_name, expected_args)) = class_info(expected) {
        if let Some((actual_name, actual_args)) = class_info(actual) {
            if class_registry.is_subclass_of(&actual_name, &expected_name) {
                if !expected_args.is_empty() && actual_args.len() == expected_args.len() {
                    let variance_info = class_registry
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
                                    || Unifier::unify(actual_arg, expected_arg, typevar_registry).is_ok()
                            }
                        };

                        if !compatible {
                            return false;
                        }
                    }
                }
                return true;
            }
        }

        if let Some(metadata) = class_registry.get_class(&expected_name) {
            if metadata.is_protocol {
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
    }

    false
}

fn class_info(ty: &Type) -> Option<(String, Vec<Type>)> {
    match ty {
        Type::Con(TypeCtor::Class(name)) => Some((name.clone(), Vec::new())),
        _ => ty.unapply_class().map(|(name, args)| (name.to_string(), args)),
    }
}

fn iterable_compatible(
    actual: &Type, expected: &Type, class_registry: &ClassRegistry,
    typevar_registry: &beacon_core::TypeVarConstraintRegistry,
) -> bool {
    if let Some((yield_ty, _send_ty, _return_ty)) = actual.extract_generator_params() {
        if let Some((expected_ctor, expected_args)) = expected.unapply() {
            let expects_iterable = matches!(expected_ctor, TypeCtor::Iterable)
                || matches!(expected_ctor, TypeCtor::Class(name) if name == "Iterable");

            if expects_iterable {
                if let Some(elem_ty) = expected_args.first() {
                    return types_compatible(yield_ty, elem_ty, class_registry, typevar_registry);
                }
                return true;
            }
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
fn function_compatible(
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
fn check_protocol_with_variance(
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
                    || beacon_core::Unifier::unify(inferred_arg, expected_arg, typevar_registry).is_ok()
            }
        };

        if !compatible {
            return false;
        }
    }

    true
}

/// Check if two method signatures are compatible
fn method_signatures_compatible(
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
fn protocol_instantiation_compatible(
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
            if let Some(actual_method) = actual_class_meta.lookup_method(method_name) {
                if let Some(inferred) = infer_type_param_from_methods(protocol_method_ty, actual_method, type_var_name)
                {
                    inferred_type = Some(inferred);
                    break;
                }
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
fn infer_type_param_from_methods(protocol_method: &Type, actual_method: &Type, type_var_name: &str) -> Option<Type> {
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
fn contains_type_var(ty: &Type, var_name: &str) -> bool {
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

fn types_compatible(
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
fn generator_compatible(actual: &Type, expected: &Type, class_registry: &ClassRegistry) -> bool {
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

fn generator_iterable_subst(
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

/// Merge positional and keyword arguments according to function parameters
///
/// Takes positional arguments, keyword arguments, and function parameters, then:
/// 1. Validates that no keyword argument is duplicated
/// 2. Validates that all keyword arguments match parameter names
/// 3. Merges them into a single vector aligned with the function's parameters
fn merge_and_validate_args(
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

fn instantiate_class_type(metadata: &ClassMetadata, class_name: &str, subst: &Subst) -> Type {
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

fn handle_call_args(
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

                    match Unifier::unify(&provided_ty, &expected_ty, ctx.typevar_registry) {
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

fn unify_return_type(ctx: &mut CallContext<'_>, call_ret_ty: &Type, target_ty: &Type) {
    match Unifier::unify(
        &ctx.subst.apply(call_ret_ty),
        &ctx.subst.apply(target_ty),
        ctx.typevar_registry,
    ) {
        Ok(s) => *ctx.subst = s.compose(ctx.subst.clone()),
        Err(BeaconError::TypeError(type_err)) => ctx.type_errors.push(TypeErrorInfo::new(type_err, ctx.span)),
        Err(_) => (),
    }
}

fn unify_with_adhoc_fun(
    ctx: &mut CallContext<'_>, callable: &Type, pos_args: &[(Type, Span)], kw_args: &[(String, Type, Span)],
    ret_ty: &Type,
) {
    let all_args: Vec<Type> = pos_args
        .iter()
        .map(|(ty, _)| ty.clone())
        .chain(kw_args.iter().map(|(_, ty, _)| ty.clone()))
        .collect();

    let expected_fn_ty = Type::fun_unnamed(all_args, ret_ty.clone());

    match Unifier::unify(
        &ctx.subst.apply(callable),
        &ctx.subst.apply(&expected_fn_ty),
        ctx.typevar_registry,
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
fn resolve_bound_method_type<'a>(
    rec: Box<Type>, name: &str, method: &'a Type, pos_args: &[(Type, Span)], subst: &Subst, reg: &'a ClassRegistry,
) -> &'a Type {
    if rec.unapply_protocol().is_some() || rec.unapply_class().is_some() {
        return method;
    }

    if let Type::Con(TypeCtor::Class(class_name)) = rec.as_ref() {
        if let Some(metadata) = reg.get_class(class_name) {
            if let Some(method_type) = metadata.lookup_method_type(name) {
                let applied_args: Vec<Type> = pos_args.iter().map(|(arg_ty, _)| subst.apply(arg_ty)).collect();
                return method_type.resolve_for_args(&applied_args).unwrap_or(method);
            }
        }
    }
    method
}

/// Provide a contextual fallback type when unification fails
///
/// Instead of always falling back to `Any`, use context to provide a more specific type:
/// - If one type is concrete and the other is a type variable, prefer the concrete type
/// - For container types, preserve the container structure with Any for unknown elements
/// - This allows partial type information to be preserved even when full inference fails
fn contextual_fallback_type(t1: &Type, t2: &Type) -> Type {
    match (t1, t2) {
        (Type::Con(TypeCtor::Any), other) | (other, Type::Con(TypeCtor::Any)) => other.clone(),
        (Type::Var(_), concrete) | (concrete, Type::Var(_)) if !matches!(concrete, Type::Var(_)) => concrete.clone(),
        (Type::App(f1, _), Type::App(f2, _)) if extract_base_constructor(f1) == extract_base_constructor(f2) => {
            t1.clone()
        }
        (Type::Union(types1), Type::Union(types2)) => {
            let common: Vec<Type> = types1
                .iter()
                .filter(|t1| types2.iter().any(|t2| *t1 == t2))
                .cloned()
                .collect();

            if !common.is_empty() { Type::union(common) } else { Type::any() }
        }
        _ => {
            tracing::debug!(
                "Type inference failed, falling back to Any for types: {} and {}",
                t1.display_for_diagnostics(),
                t2.display_for_diagnostics()
            );
            Type::any()
        }
    }
}

/// Try to apply a partial substitution even when unification fails
///
/// When unification fails, we might still have learned something useful
/// from the successful parts of the unification attempt. This function
/// attempts to extract and apply any partial progress.
fn try_partial_unify(
    t1: &Type, t2: &Type, _typevar_registry: &beacon_core::TypeVarConstraintRegistry,
) -> Option<Subst> {
    match (t1, t2) {
        (Type::Var(tv), other) | (other, Type::Var(tv)) if !matches!(other, Type::Var(_)) => {
            let fallback = contextual_fallback_type(t1, t2);
            Some(Subst::singleton(tv.clone(), fallback))
        }
        _ => None,
    }
}

/// Solve a set of constraints using beacon-core's unification algorithm
///
/// Errors are accumulated rather than failing fast to provide comprehensive feedback.
/// This implementation includes improved error recovery:
/// - Contextual fallback types when unification fails
/// - Partial substitution application to preserve successful inference
/// - Enhanced tracing for debugging partial failures
pub fn solve_constraints(
    constraint_set: ConstraintSet, class_registry: &ClassRegistry,
    typevar_registry: &beacon_core::TypeVarConstraintRegistry,
) -> Result<(Subst, Vec<TypeErrorInfo>)> {
    tracing::debug!("Solving {} constraints", constraint_set.constraints.len());
    let mut subst = Subst::empty();
    let mut type_errors = Vec::new();
    let total_constraints = constraint_set.constraints.len();

    for (idx, constraint) in constraint_set.constraints.into_iter().enumerate() {
        tracing::trace!("Processing constraint {}/{}", idx + 1, total_constraints);

        match constraint {
            Constraint::Equal(t1, t2, span) => {
                let applied_t1 = subst.apply(&t1);
                let applied_t2 = subst.apply(&t2);

                let needs_variance_check = if let (Some((name1, args1)), Some((name2, args2))) =
                    (class_info(&applied_t1), class_info(&applied_t2))
                {
                    name1 == name2
                        && !args1.is_empty()
                        && !args2.is_empty()
                        && class_registry
                            .get_class(&name1)
                            .is_some_and(|m| !m.type_param_vars.is_empty())
                } else {
                    false
                };

                if needs_variance_check {
                    if !types_compatible(&applied_t1, &applied_t2, class_registry, typevar_registry) {
                        let (class_name, _) = class_info(&applied_t1).unwrap();
                        type_errors.push(TypeErrorInfo::new(
                            TypeError::VarianceError {
                                position: format!("{class_name} type argument"),
                                expected_variance: "user-defined".to_string(),
                                got_type: applied_t1.display_for_diagnostics(),
                                expected_type: applied_t2.display_for_diagnostics(),
                            },
                            span,
                        ));
                    }
                } else {
                    let involves_union = matches!(applied_t1, Type::Union(_)) || matches!(applied_t2, Type::Union(_));

                    if !(involves_union
                        && (applied_t1.is_subtype_of(&applied_t2) || applied_t2.is_subtype_of(&applied_t1)))
                    {
                        match Unifier::unify(&applied_t1, &applied_t2, typevar_registry) {
                            Ok(s) => {
                                subst = s.compose(subst);
                            }
                            Err(BeaconError::TypeError(type_err)) => {
                                let are_compatible =
                                    types_compatible(&applied_t1, &applied_t2, class_registry, typevar_registry)
                                        || types_compatible(&applied_t2, &applied_t1, class_registry, typevar_registry);

                                if !are_compatible {
                                    type_errors.push(TypeErrorInfo::new(type_err, span));

                                    if let Some(partial_subst) =
                                        try_partial_unify(&applied_t1, &applied_t2, typevar_registry)
                                    {
                                        tracing::debug!(
                                            "Applied partial substitution after unification failure: {} = {}",
                                            applied_t1.display_for_diagnostics(),
                                            applied_t2.display_for_diagnostics()
                                        );
                                        subst = partial_subst.compose(subst);
                                    }
                                }
                            }
                            Err(_) => {}
                        }
                    }
                }
            }
            Constraint::Call(func_ty, pos_args, kw_args, ret_ty, span) => {
                let applied_func = subst.apply(&func_ty);

                if let Type::Con(TypeCtor::Class(class_name)) = &applied_func {
                    if let Some(metadata) = class_registry.get_class(class_name) {
                        if let Some(Type::Fun(params, _)) = metadata.new_type.as_ref().or(metadata.init_type.as_ref()) {
                            let mut ctx = CallContext {
                                subst: &mut subst,
                                type_errors: &mut type_errors,
                                class_registry,
                                typevar_registry,
                                span,
                            };
                            handle_call_args(&mut ctx, &pos_args, &kw_args, params, true);
                        }

                        let class_result_ty = instantiate_class_type(metadata, class_name, &subst);
                        let mut ctx = CallContext {
                            subst: &mut subst,
                            type_errors: &mut type_errors,
                            class_registry,
                            typevar_registry,
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
                        &subst,
                        class_registry,
                    );

                    if let Type::Fun(params, method_ret) = resolved_method {
                        let mut ctx = CallContext {
                            subst: &mut subst,
                            type_errors: &mut type_errors,
                            class_registry,
                            typevar_registry,
                            span,
                        };

                        handle_call_args(&mut ctx, &pos_args, &kw_args, params, true);
                        unify_return_type(&mut ctx, &ret_ty, method_ret);
                    } else {
                        let mut ctx = CallContext {
                            subst: &mut subst,
                            type_errors: &mut type_errors,
                            class_registry,
                            typevar_registry,
                            span,
                        };
                        unify_with_adhoc_fun(&mut ctx, resolved_method, &pos_args, &kw_args, &ret_ty);
                    }
                } else {
                    let applied_func = subst.apply(&func_ty);
                    if let Type::Fun(params, fn_ret) = &applied_func {
                        let mut ctx = CallContext {
                            subst: &mut subst,
                            type_errors: &mut type_errors,
                            class_registry,
                            typevar_registry,
                            span,
                        };
                        handle_call_args(&mut ctx, &pos_args, &kw_args, params, false);
                        unify_return_type(&mut ctx, &ret_ty, fn_ret);
                    } else {
                        let mut ctx = CallContext {
                            subst: &mut subst,
                            type_errors: &mut type_errors,
                            class_registry,
                            typevar_registry,
                            span,
                        };
                        unify_with_adhoc_fun(&mut ctx, &applied_func, &pos_args, &kw_args, &ret_ty);
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
                            beacon_core::TypeError::AttributeNotFound(
                                applied_obj.display_for_diagnostics(),
                                attr_name.clone(),
                            ),
                            span,
                        ));
                    } else if !attr_types.is_empty() {
                        let attr_union = if attr_types.len() == 1 {
                            attr_types.into_iter().next().unwrap()
                        } else {
                            Type::union(attr_types)
                        };

                        match Unifier::unify(&subst.apply(&attr_ty), &attr_union, typevar_registry) {
                            Ok(s) => {
                                subst = s.compose(subst);
                            }
                            Err(BeaconError::TypeError(type_err)) => {
                                type_errors.push(TypeErrorInfo::new(type_err, span));
                            }
                            Err(_) => {}
                        }
                    }
                    continue;
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

                        match Unifier::unify(&subst.apply(&attr_ty), &getitem_ty, typevar_registry) {
                            Ok(s) => {
                                subst = s.compose(subst);
                            }
                            Err(BeaconError::TypeError(type_err)) => {
                                type_errors.push(TypeErrorInfo::new(type_err, span));
                            }
                            Err(_) => {}
                        }
                    }
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

                            match Unifier::unify(&subst.apply(&attr_ty), &final_type, typevar_registry) {
                                Ok(s) => {
                                    subst = s.compose(subst);
                                }
                                Err(BeaconError::TypeError(type_err)) => {
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
                                if let Ok(s) = Unifier::unify(&subst.apply(&attr_ty), &Type::any(), typevar_registry) {
                                    subst = s.compose(subst);
                                }
                                None
                            }
                            _ => None,
                        };

                        if let Some(class_name) = class_name {
                            if let Some(resolved_attr_ty) = class_registry.lookup_attribute_with_inheritance(class_name, &attr_name) {
                                let final_type = if class_registry.is_method(class_name, &attr_name) {
                                    Type::BoundMethod(
                                        Box::new(applied_obj.clone()),
                                        attr_name.clone(),
                                        Box::new(resolved_attr_ty.clone()),
                                    )
                                } else {
                                    resolved_attr_ty.clone()
                                };

                                match Unifier::unify(&subst.apply(&attr_ty), &final_type, typevar_registry) {
                                    Ok(s) => {
                                        subst = s.compose(subst);
                                    }
                                    Err(BeaconError::TypeError(type_err)) => {
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
                        let (resolved_attr_ty, is_method, _class_name_opt) = if let Some((protocol_name, type_args)) =
                            applied_obj.unapply_protocol()
                        {
                            if let Some(attr_type) = class_registry.lookup_attribute_with_inheritance(protocol_name, &attr_name) {
                                let substituted_ty = if let Some(protocol_metadata) = class_registry.get_class(protocol_name) {
                                    if !protocol_metadata.type_params.is_empty() {
                                        let subst_map = protocol_metadata.create_type_substitution(&type_args);
                                        beacon_core::ClassMetadata::substitute_type_params(&attr_type, &subst_map)
                                    } else {
                                        attr_type.clone()
                                    }
                                } else {
                                    attr_type.clone()
                                };
                                let is_method = class_registry.is_method(protocol_name, &attr_name);
                                (Some(substituted_ty), is_method, Some(protocol_name.to_string()))
                            } else {
                                (None, false, None)
                            }
                        } else if let Some((class_name, type_args)) = applied_obj.unapply_class() {
                            if let Some(attr_type) = class_registry.lookup_attribute_with_inheritance(class_name, &attr_name) {
                                let substituted_ty = if let Some(class_metadata) = class_registry.get_class(class_name) {
                                    if !class_metadata.type_params.is_empty() {
                                        let subst_map = class_metadata.create_type_substitution(&type_args);
                                        beacon_core::ClassMetadata::substitute_type_params(&attr_type, &subst_map)
                                    } else {
                                        attr_type.clone()
                                    }
                                } else {
                                    attr_type.clone()
                                };
                                let is_method = class_registry.is_method(class_name, &attr_name);
                                (Some(substituted_ty), is_method, Some(class_name.to_string()))
                            } else {
                                (None, false, None)
                            }
                        } else if let Some(resolved) = get_attribute_type(&applied_obj, &attr_name, class_registry) {
                            let base_ctor = extract_base_constructor(&applied_obj);
                            let is_method = base_ctor
                                .map(|name| class_registry.is_method(name, &attr_name))
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

                            match Unifier::unify(&subst.apply(&attr_ty), &final_type, typevar_registry) {
                                Ok(s) => {
                                    subst = s.compose(subst);
                                }
                                Err(BeaconError::TypeError(type_err)) => {
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
                            beacon_core::TypeError::AttributeNotFound(
                                applied_obj.display_for_diagnostics(),
                                attr_name.clone(),
                            ),
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
                        check_user_defined_protocol(&applied_obj, proto_name, class_registry, typevar_registry)
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
                    match Unifier::unify(&subst.apply(&elem_ty), &applied_extracted, typevar_registry) {
                        Ok(s) => {
                            subst = s.compose(subst);
                        }
                        Err(BeaconError::TypeError(type_err)) => {
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
                let result = check_exhaustiveness(&subject_ty, &patterns, class_registry);
                match result {
                    ExhaustivenessResult::Exhaustive => {}
                    ExhaustivenessResult::NonExhaustive { uncovered } => {
                        let uncovered_str = uncovered
                            .iter()
                            .map(|ty| ty.display_for_diagnostics())
                            .collect::<Vec<_>>()
                            .join(", ");
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
            Constraint::PatternTypeCompatible(pattern, subject_ty, span) => {
                if let Err(type_err) = validate_pattern_type_compatibility(&pattern, &subject_ty, class_registry) {
                    type_errors.push(TypeErrorInfo::new(type_err, span));
                }
            }
            Constraint::PatternStructureValid(pattern, subject_ty, span) => {
                if let Err(type_err) = validate_pattern_structure(&pattern, &subject_ty) {
                    type_errors.push(TypeErrorInfo::new(type_err, span));
                }
            }
            Constraint::Narrowing(_, _, _, _) => {}
            Constraint::Join(_, incoming_types, result_type, span) => {
                let union_type = if incoming_types.is_empty() {
                    Type::Con(TypeCtor::NoneType)
                } else if incoming_types.len() == 1 {
                    incoming_types[0].clone()
                } else {
                    Type::union(incoming_types.clone())
                };

                match Unifier::unify(&subst.apply(&result_type), &subst.apply(&union_type), typevar_registry) {
                    Ok(s) => {
                        subst = s.compose(subst);
                    }
                    Err(BeaconError::TypeError(type_err)) => {
                        type_errors.push(TypeErrorInfo::new(type_err, span));
                    }
                    Err(_) => {}
                }
            }
        }
    }

    let simplified_subst = simplify_substitution(subst);

    tracing::debug!("Constraint solving completed: {} type errors found", type_errors.len());

    if !type_errors.is_empty() {
        tracing::debug!("Type errors summary:");
        for (idx, error) in type_errors.iter().enumerate() {
            tracing::debug!(
                "  [{}] {} at {}:{}",
                idx + 1,
                match &error.error {
                    TypeError::UnificationError(_, _) => "Unification error",
                    TypeError::ArgumentTypeMismatch { .. } => "Argument type mismatch",
                    TypeError::ArgumentCountMismatch { .. } => "Argument count mismatch",
                    TypeError::AttributeNotFound(_, _) => "Attribute not found",
                    TypeError::VarianceError { .. } => "Variance error",
                    TypeError::PatternNonExhaustive(_) => "Pattern non-exhaustive",
                    TypeError::PatternTypeMismatch { .. } => "Pattern type mismatch",
                    _ => "Other type error",
                },
                error.line(),
                error.col()
            );
        }
    }

    Ok((simplified_subst, type_errors))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Span;
    use crate::predicate::TypePredicate;
    use beacon_core::{ClassMetadata, MethodType, TypeVar, TypeVarConstraintRegistry};
    use beacon_parser::{AstNode, LiteralValue, Pattern};

    /// Helper to create a test span
    fn test_span() -> Span {
        Span::new(1, 1)
    }

    /// Helper to create a type variable
    fn tvar(id: u32) -> Type {
        Type::Var(TypeVar::new(id))
    }

    #[test]
    fn test_type_to_method_signature_success() {
        let func_ty = Type::fun_unnamed(vec![Type::int(), Type::string()], Type::bool());
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

        let result = check_user_defined_protocol(
            &Type::int(),
            "EmptyProtocol",
            &registry,
            &beacon_core::TypeVarConstraintRegistry::new(),
        );
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
            MethodType::Single(Type::fun_unnamed(vec![Type::any()], Type::any())),
        );
        registry.register_class("TestProtocol".to_string(), protocol_meta);

        let result = check_user_defined_protocol(
            &Type::any(),
            "TestProtocol",
            &registry,
            &beacon_core::TypeVarConstraintRegistry::new(),
        );
        assert!(result, "Any type should satisfy any protocol");
    }

    #[test]
    fn test_solve_equal_constraint_success() {
        let constraints = ConstraintSet { constraints: vec![Constraint::Equal(Type::int(), Type::int(), test_span())] };
        let registry = ClassRegistry::new();
        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
        assert!(result.is_ok());

        let (_, errors) = result.unwrap();
        assert!(errors.is_empty());
    }

    #[test]
    fn test_solve_equal_constraint_unification_error() {
        let constraints =
            ConstraintSet { constraints: vec![Constraint::Equal(Type::int(), Type::string(), test_span())] };

        let registry = ClassRegistry::new();
        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
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
        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
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
        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
        assert!(result.is_ok());

        let (subst, errors) = result.unwrap();
        assert!(errors.is_empty());
        assert!(!subst.is_empty());
    }

    #[test]
    fn test_solve_call_constraint_simple_function() {
        let func_ty = Type::fun_unnamed(vec![Type::int()], Type::string());
        let arg_types = vec![(Type::int(), test_span())];
        let ret_ty = tvar(0);
        let constraints =
            ConstraintSet { constraints: vec![Constraint::Call(func_ty, arg_types, vec![], ret_ty, test_span())] };

        let registry = ClassRegistry::new();
        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
        assert!(result.is_ok());

        let (subst, errors) = result.unwrap();
        assert!(errors.is_empty());
        assert!(!subst.is_empty());
    }

    #[test]
    fn test_solve_call_constraint_wrong_arg_count() {
        let func_ty = Type::fun_unnamed(vec![Type::int()], Type::string());
        let arg_types = vec![(Type::int(), test_span()), (Type::int(), test_span())];
        let ret_ty = tvar(0);
        let constraints =
            ConstraintSet { constraints: vec![Constraint::Call(func_ty, arg_types, vec![], ret_ty, test_span())] };

        let registry = ClassRegistry::new();
        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
        assert!(result.is_ok());

        let (_, errors) = result.unwrap();
        assert!(!errors.is_empty(), "Wrong argument count should produce an error");
    }

    #[test]
    fn test_solve_call_constraint_class_constructor() {
        let mut registry = ClassRegistry::new();
        let mut class_meta = ClassMetadata::new("TestClass".to_string());
        class_meta.init_type = Some(Type::fun_unnamed(vec![Type::any(), Type::int()], Type::any()));
        registry.register_class("TestClass".to_string(), class_meta);

        let class_ty = Type::Con(TypeCtor::Class("TestClass".to_string()));
        let arg_types = vec![(Type::int(), test_span())];
        let ret_ty = tvar(0);
        let constraints =
            ConstraintSet { constraints: vec![Constraint::Call(class_ty, arg_types, vec![], ret_ty, test_span())] };

        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
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
            MethodType::Single(Type::fun_unnamed(vec![Type::any(), Type::int()], Type::string())),
        );
        registry.register_class("TestClass".to_string(), class_meta);

        let receiver = Type::Con(TypeCtor::Class("TestClass".to_string()));
        let method_ty = Type::fun_unnamed(vec![Type::any(), Type::int()], Type::string());
        let bound_method = Type::BoundMethod(Box::new(receiver), "method".to_string(), Box::new(method_ty));
        let arg_types = vec![(Type::int(), test_span())];
        let ret_ty = tvar(0);

        let constraints =
            ConstraintSet { constraints: vec![Constraint::Call(bound_method, arg_types, vec![], ret_ty, test_span())] };

        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
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

        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
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

        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
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
            MethodType::Single(Type::fun_unnamed(vec![Type::any()], Type::string())),
        );
        registry.register_class("str".to_string(), str_meta);

        let obj_ty = Type::string();
        let attr_ty = tvar(0);
        let constraints =
            ConstraintSet { constraints: vec![Constraint::HasAttr(obj_ty, "upper".to_string(), attr_ty, test_span())] };

        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
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

        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
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
        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
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
        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
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
        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
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
        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
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
        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
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

        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
        assert!(result.is_ok());

        let (_, errors) = result.unwrap();
        assert!(errors.is_empty(), "MatchPattern constraint should succeed");
    }

    #[test]
    fn test_solve_pattern_exhaustive_complete() {
        let subject_ty = Type::bool();
        let patterns = vec![
            (
                Pattern::MatchValue(AstNode::Literal {
                    value: LiteralValue::Boolean(true),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 5,
                }),
                false,
            ),
            (
                Pattern::MatchValue(AstNode::Literal {
                    value: LiteralValue::Boolean(false),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 6,
                }),
                false,
            ),
        ];
        let constraints =
            ConstraintSet { constraints: vec![Constraint::PatternExhaustive(subject_ty, patterns, test_span())] };

        let registry = ClassRegistry::new();
        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
        assert!(result.is_ok());

        let (_, errors) = result.unwrap();
        assert!(errors.is_empty(), "Exhaustive patterns should not produce errors");
    }

    #[test]
    fn test_solve_pattern_exhaustive_incomplete() {
        let subject_ty = Type::bool();
        let patterns = vec![(
            Pattern::MatchValue(AstNode::Literal {
                value: LiteralValue::Boolean(true),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 5,
            }),
            false,
        )];

        let constraints =
            ConstraintSet { constraints: vec![Constraint::PatternExhaustive(subject_ty, patterns, test_span())] };

        let registry = ClassRegistry::new();
        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
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
        let pattern = Pattern::MatchValue(AstNode::Literal {
            value: LiteralValue::Integer(42),
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 3,
        });
        let previous = vec![Pattern::MatchValue(AstNode::Literal {
            value: LiteralValue::Integer(43),
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 3,
        })];

        let constraints =
            ConstraintSet { constraints: vec![Constraint::PatternReachable(pattern, previous, test_span())] };

        let registry = ClassRegistry::new();
        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
        assert!(result.is_ok());

        let (_, errors) = result.unwrap();
        assert!(errors.is_empty(), "Different patterns should be reachable");
    }

    #[test]
    fn test_solve_pattern_unreachable() {
        let catch_all = Pattern::MatchAs { pattern: None, name: Some("_".to_string()) };
        let pattern = Pattern::MatchValue(AstNode::Literal {
            value: LiteralValue::Integer(42),
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 1,
        });
        let previous = vec![catch_all];
        let constraints =
            ConstraintSet { constraints: vec![Constraint::PatternReachable(pattern, previous, test_span())] };

        let registry = ClassRegistry::new();
        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
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
        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
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
        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
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
        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
        assert!(result.is_ok());

        let (_subst, errors) = result.unwrap();
        assert!(errors.is_empty() || errors.len() <= 1);
    }

    #[test]
    fn test_solve_call_constraint_wrong_arg_type() {
        let func_ty = Type::fun_unnamed(vec![Type::int()], Type::string());
        let arg_types = vec![(Type::string(), test_span())];
        let ret_ty = tvar(0);
        let constraints =
            ConstraintSet { constraints: vec![Constraint::Call(func_ty, arg_types, vec![], ret_ty, test_span())] };

        let registry = ClassRegistry::new();
        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
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
            MethodType::Single(Type::fun_unnamed(vec![Type::any()], Type::string())),
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

        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
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
        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
        assert!(result.is_ok());

        let (subst, errors) = result.unwrap();
        assert!(errors.is_empty());
        assert!(subst.is_empty());
    }

    #[test]
    fn test_call_with_fewer_args_than_params() {
        let func_ty = Type::fun_unnamed(vec![Type::string(), Type::string()], Type::string());
        let arg_types = vec![(Type::string(), test_span())];
        let ret_ty = tvar(0);
        let constraints = ConstraintSet {
            constraints: vec![Constraint::Call(
                func_ty,
                arg_types,
                vec![],
                ret_ty.clone(),
                test_span(),
            )],
        };

        let registry = ClassRegistry::new();
        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
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
        let func_ty = Type::fun_unnamed(
            vec![Type::string()],
            Type::Con(TypeCtor::Class("Processor".to_string())),
        );
        let arg_types = vec![];
        let ret_ty = tvar(0);
        let constraints = ConstraintSet {
            constraints: vec![Constraint::Call(
                func_ty,
                arg_types,
                vec![],
                ret_ty.clone(),
                test_span(),
            )],
        };

        let registry = ClassRegistry::new();
        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
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
        let func_ty = Type::fun_unnamed(vec![Type::int()], Type::string());
        let arg_types = vec![(Type::int(), test_span()), (Type::int(), test_span())];
        let ret_ty = tvar(0);
        let constraints =
            ConstraintSet { constraints: vec![Constraint::Call(func_ty, arg_types, vec![], ret_ty, test_span())] };

        let registry = ClassRegistry::new();
        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
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
            MethodType::Single(Type::fun_unnamed(
                vec![Type::any(), Type::int(), Type::string()],
                Type::bool(),
            )),
        );
        registry.register_class("TestClass".to_string(), class_meta);

        let receiver = Type::Con(TypeCtor::Class("TestClass".to_string()));
        let method_ty = Type::fun_unnamed(vec![Type::any(), Type::int(), Type::string()], Type::bool());
        let bound_method = Type::BoundMethod(Box::new(receiver), "method".to_string(), Box::new(method_ty));
        let arg_types = vec![(Type::int(), test_span())];
        let ret_ty = tvar(0);

        let constraints = ConstraintSet {
            constraints: vec![Constraint::Call(
                bound_method,
                arg_types,
                vec![],
                ret_ty.clone(),
                test_span(),
            )],
        };

        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
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
        class_meta.init_type = Some(Type::fun_unnamed(
            vec![Type::any(), Type::int(), Type::string()],
            Type::any(),
        ));
        registry.register_class("TestClass".to_string(), class_meta);

        let class_ty = Type::Con(TypeCtor::Class("TestClass".to_string()));
        let arg_types = vec![(Type::int(), test_span())];
        let ret_ty = tvar(0);
        let constraints = ConstraintSet {
            constraints: vec![Constraint::Call(
                class_ty.clone(),
                arg_types,
                vec![],
                ret_ty.clone(),
                test_span(),
            )],
        };

        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
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
    fn test_function_contravariance_valid() {
        let animal = Type::Con(TypeCtor::Class("Animal".to_string()));
        let func_animal = Type::fun_unnamed(vec![animal.clone()], Type::none());

        let arg_types = vec![(func_animal.clone(), test_span())];
        let ret_ty = tvar(0);
        let constraints = ConstraintSet {
            constraints: vec![Constraint::Call(
                Type::fun_unnamed(vec![func_animal], Type::none()),
                arg_types,
                vec![],
                ret_ty,
                test_span(),
            )],
        };

        let registry = ClassRegistry::new();
        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
        assert!(result.is_ok());

        let (_, errors) = result.unwrap();
        assert!(errors.is_empty(), "Same function type should be accepted");
    }

    #[test]
    fn test_function_contravariance_subtype() {
        let mut registry = ClassRegistry::new();
        let animal_meta = ClassMetadata::new("Animal".to_string());
        let mut dog_meta = ClassMetadata::new("Dog".to_string());
        dog_meta.base_classes.push("Animal".to_string());
        registry.register_class("Animal".to_string(), animal_meta);
        registry.register_class("Dog".to_string(), dog_meta);

        let animal = Type::Con(TypeCtor::Class("Animal".to_string()));
        let dog = Type::Con(TypeCtor::Class("Dog".to_string()));
        let func_dog = Type::fun_unnamed(vec![dog.clone()], Type::none());
        let func_animal = Type::fun_unnamed(vec![animal.clone()], Type::none());

        let arg_types = vec![(func_dog, test_span())];
        let ret_ty = tvar(0);
        let constraints = ConstraintSet {
            constraints: vec![Constraint::Call(
                Type::fun_unnamed(vec![func_animal], Type::none()),
                arg_types,
                vec![],
                ret_ty,
                test_span(),
            )],
        };

        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
        assert!(result.is_ok());

        let (_, errors) = result.unwrap();
        assert!(
            !errors.is_empty(),
            "Callable[[Dog], None] should NOT be accepted where Callable[[Animal], None] is expected"
        );
    }

    #[test]
    fn test_class_inheritance_setup() {
        let mut registry = ClassRegistry::new();
        let animal_meta = ClassMetadata::new("Animal".to_string());
        let mut dog_meta = ClassMetadata::new("Dog".to_string());
        dog_meta.base_classes.push("Animal".to_string());
        registry.register_class("Animal".to_string(), animal_meta);
        registry.register_class("Dog".to_string(), dog_meta);

        assert!(
            registry.is_subclass_of("Dog", "Animal"),
            "Dog should be a subclass of Animal"
        );
        assert!(
            !registry.is_subclass_of("Animal", "Dog"),
            "Animal should NOT be a subclass of Dog"
        );
    }

    #[test]
    fn test_function_contravariance_reverse_valid() {
        let mut registry = ClassRegistry::new();
        let animal_meta = ClassMetadata::new("Animal".to_string());
        let mut dog_meta = ClassMetadata::new("Dog".to_string());
        dog_meta.base_classes.push("Animal".to_string());
        registry.register_class("Animal".to_string(), animal_meta);
        registry.register_class("Dog".to_string(), dog_meta);

        assert!(
            registry.is_subclass_of("Dog", "Animal"),
            "Setup error: Dog should be subclass of Animal"
        );

        let animal = Type::Con(TypeCtor::Class("Animal".to_string()));
        let dog = Type::Con(TypeCtor::Class("Dog".to_string()));
        let func_animal = Type::fun_unnamed(vec![animal.clone()], Type::none());
        let func_dog = Type::fun_unnamed(vec![dog.clone()], Type::none());

        let arg_types = vec![(func_animal, test_span())];
        let ret_ty = tvar(0);
        let constraints = ConstraintSet {
            constraints: vec![Constraint::Call(
                Type::fun_unnamed(vec![func_dog], Type::none()),
                arg_types,
                vec![],
                ret_ty,
                test_span(),
            )],
        };

        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
        assert!(result.is_ok());

        let (_, errors) = result.unwrap();
        assert!(
            errors.is_empty(),
            "Callable[[Animal], None] should be accepted where Callable[[Dog], None] is expected (contravariance)"
        );
    }

    #[test]
    fn test_function_covariance_return_types() {
        let mut registry = ClassRegistry::new();
        let animal_meta = ClassMetadata::new("Animal".to_string());
        let mut dog_meta = ClassMetadata::new("Dog".to_string());
        dog_meta.base_classes.push("Animal".to_string());
        registry.register_class("Animal".to_string(), animal_meta);
        registry.register_class("Dog".to_string(), dog_meta);

        let animal = Type::Con(TypeCtor::Class("Animal".to_string()));
        let dog = Type::Con(TypeCtor::Class("Dog".to_string()));
        let func_returns_dog = Type::fun_unnamed(vec![], dog.clone());
        let func_returns_animal = Type::fun_unnamed(vec![], animal.clone());

        let arg_types = vec![(func_returns_dog, test_span())];
        let ret_ty = tvar(0);
        let constraints = ConstraintSet {
            constraints: vec![Constraint::Call(
                Type::fun_unnamed(vec![func_returns_animal], Type::none()),
                arg_types,
                vec![],
                ret_ty,
                test_span(),
            )],
        };

        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
        assert!(result.is_ok());

        let (_, errors) = result.unwrap();
        assert!(
            errors.is_empty(),
            "Callable[[], Dog] should be accepted where Callable[[], Animal] is expected (covariance)"
        );
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

        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
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

        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
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
            MethodType::Single(Type::fun_unnamed(
                vec![Type::any(), Type::int(), Type::int()],
                Type::int(),
            )),
        );
        registry.register_class("Calculator".to_string(), calc_class);

        let obj_ty = Type::optional(Type::Con(TypeCtor::Class("Calculator".to_string())));
        let attr_ty = tvar(0);
        let constraints =
            ConstraintSet { constraints: vec![Constraint::HasAttr(obj_ty, "add".to_string(), attr_ty, test_span())] };

        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
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
        list_meta.set_type_param_vars(vec![TypeVar::named(0, "_T")]);
        list_meta.methods.insert(
            "__getitem__".to_string(),
            MethodType::Single(Type::fun_unnamed(
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

        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
        assert!(result.is_ok());

        let (subst, errors) = result.unwrap();
        assert!(errors.is_empty(), "Should not have errors");

        let resolved = subst.apply(&attr_ty);
        match resolved {
            Type::BoundMethod(_, _, method) => match *method {
                Type::Fun(params, ret) => {
                    assert_eq!(params.len(), 2);
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
        dict_meta.set_type_param_vars(vec![TypeVar::named(1, "_KT"), TypeVar::named(2, "_VT")]);
        dict_meta.methods.insert(
            "get".to_string(),
            MethodType::Single(Type::fun_unnamed(
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

        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
        assert!(result.is_ok());

        let (subst, errors) = result.unwrap();
        assert!(errors.is_empty(), "Should not have errors");

        let resolved = subst.apply(&attr_ty);
        match resolved {
            Type::BoundMethod(_, _, method) => match *method {
                Type::Fun(params, ret) => {
                    assert_eq!(params.len(), 2);
                    assert_eq!(params[1].1, Type::string(), "Key param should be str");
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
            MethodType::Single(Type::fun_unnamed(vec![Type::any()], Type::int())),
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

        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
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
        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
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
        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
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
        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
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
        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
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
        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
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
        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
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
        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
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
        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
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
        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
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
        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
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
        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
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
        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
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
        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
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
        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
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
        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
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
        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
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
            MethodType::Single(Type::fun_unnamed(vec![Type::any(), Type::int()], Type::string())),
        );
        registry.register_class("MyClass".to_string(), class_meta);

        let obj_ty = Type::Con(TypeCtor::Class("MyClass".to_string()));
        let method_ty = Type::BoundMethod(
            Box::new(obj_ty),
            "process".to_string(),
            Box::new(Type::fun_unnamed(vec![Type::any(), Type::int()], Type::string())),
        );

        let ret_ty = tvar(0);
        let constraints = ConstraintSet {
            constraints: vec![Constraint::Call(
                method_ty,
                vec![(Type::int(), test_span())],
                vec![],
                ret_ty.clone(),
                test_span(),
            )],
        };

        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
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
            Box::new(Type::fun_unnamed(vec![Type::any(), Type::int()], Type::string())),
        );

        let ret_ty = tvar(0);
        let constraints = ConstraintSet {
            constraints: vec![Constraint::Call(
                method_ty,
                vec![(Type::int(), test_span()), (Type::bool(), test_span())],
                vec![],
                ret_ty,
                test_span(),
            )],
        };

        let registry = ClassRegistry::new();
        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
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
            Box::new(Type::fun_unnamed(vec![Type::any(), Type::int()], Type::string())),
        );

        let ret_ty = tvar(0);
        let constraints = ConstraintSet {
            constraints: vec![Constraint::Call(
                method_ty,
                vec![(Type::string(), test_span())],
                vec![],
                ret_ty,
                test_span(),
            )],
        };

        let registry = ClassRegistry::new();
        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
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
        subst.insert(TypeVar::new(0), Type::string());

        let constraints = ConstraintSet {
            constraints: vec![Constraint::Protocol(
                list_ty,
                beacon_core::ProtocolName::Iterable,
                elem_outer.clone(),
                test_span(),
            )],
        };

        let registry = ClassRegistry::new();
        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
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
        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
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
        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
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
        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
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
        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
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
        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
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
        let result = solve_constraints(constraints, &registry, &beacon_core::TypeVarConstraintRegistry::new());
        assert!(result.is_ok());

        let (_, errors) = result.unwrap();
        assert!(
            !errors.is_empty(),
            "Union[int, str] should not unify with float when not a subtype"
        );
    }

    #[test]
    fn test_classes_compatible_with_inheritance() {
        let mut registry = ClassRegistry::new();
        let tv_registry = TypeVarConstraintRegistry::new();
        let base_meta = ClassMetadata::new("Base".to_string());
        registry.register_class("Base".to_string(), base_meta);

        let mut derived_meta = ClassMetadata::new("Derived".to_string());
        derived_meta.add_base_class("Base".to_string());
        registry.register_class("Derived".to_string(), derived_meta);

        let derived_ty = Type::Con(TypeCtor::Class("Derived".to_string()));
        let base_ty = Type::Con(TypeCtor::Class("Base".to_string()));

        assert!(
            classes_compatible(&derived_ty, &base_ty, &registry, &tv_registry),
            "Derived class should be compatible with Base class"
        );
    }

    #[test]
    fn test_classes_compatible_with_protocol() {
        let mut registry = ClassRegistry::new();
        let tv_registry = TypeVarConstraintRegistry::new();
        let mut protocol_meta = ClassMetadata::new("MyProtocol".to_string());
        protocol_meta.is_protocol = true;
        protocol_meta.methods.insert(
            "my_method".to_string(),
            MethodType::Single(Type::fun_unnamed(vec![Type::any()], Type::int())),
        );
        registry.register_class("MyProtocol".to_string(), protocol_meta);

        let mut impl_meta = ClassMetadata::new("MyImpl".to_string());
        impl_meta.methods.insert(
            "my_method".to_string(),
            MethodType::Single(Type::fun_unnamed(vec![Type::any()], Type::int())),
        );
        registry.register_class("MyImpl".to_string(), impl_meta);

        let impl_ty = Type::Con(TypeCtor::Class("MyImpl".to_string()));
        let protocol_ty = Type::Con(TypeCtor::Class("MyProtocol".to_string()));

        assert!(
            classes_compatible(&impl_ty, &protocol_ty, &registry, &tv_registry),
            "Class implementing protocol methods should be compatible with protocol"
        );
    }

    #[test]
    fn test_types_compatible_any() {
        let registry = ClassRegistry::new();
        let tv_registry = TypeVarConstraintRegistry::new();
        let any_ty = Type::any();
        let int_ty = Type::int();

        assert!(
            types_compatible(&int_ty, &any_ty, &registry, &tv_registry),
            "Any type should be compatible with int"
        );
        assert!(
            types_compatible(&any_ty, &int_ty, &registry, &tv_registry),
            "int should be compatible with Any type"
        );
    }

    #[test]
    fn test_types_compatible_handles_inheritance() {
        let tv_registry = TypeVarConstraintRegistry::new();
        let mut registry = ClassRegistry::new();
        let mut data_provider_meta = ClassMetadata::new("DataProvider".to_string());
        data_provider_meta.is_protocol = true;
        data_provider_meta.add_base_class("Protocol[T]".to_string());
        data_provider_meta.type_params.push("T".to_string());
        registry.register_class("DataProvider".to_string(), data_provider_meta);

        let mut in_memory_meta = ClassMetadata::new("InMemoryProvider".to_string());
        in_memory_meta.add_base_class("DataProvider[T]".to_string());
        in_memory_meta.type_params.push("T".to_string());
        registry.register_class("InMemoryProvider".to_string(), in_memory_meta);

        let in_memory_ty = Type::Con(TypeCtor::Class("InMemoryProvider".to_string()));
        let data_provider_ty = Type::App(
            Box::new(Type::Con(TypeCtor::Class("DataProvider".to_string()))),
            Box::new(Type::Con(TypeCtor::Class("object".to_string()))),
        );

        assert!(
            types_compatible(&in_memory_ty, &data_provider_ty, &registry, &tv_registry),
            "InMemoryProvider should be compatible with DataProvider[object]"
        );
    }

    #[test]
    fn test_user_defined_covariant_variance() {
        let tv_registry = TypeVarConstraintRegistry::new();
        let mut registry = ClassRegistry::new();
        let mut producer_meta = ClassMetadata::new("Producer".to_string());
        producer_meta.add_base_class("Generic[T_Co]".to_string());
        producer_meta.type_params.push("T_Co".to_string());
        producer_meta.type_param_vars.push(TypeVar::with_variance(
            0,
            Some("T_Co".to_string()),
            beacon_core::Variance::Covariant,
        ));
        registry.register_class("Producer".to_string(), producer_meta);

        let mut dog_meta = ClassMetadata::new("Dog".to_string());
        dog_meta.add_base_class("Animal".to_string());
        registry.register_class("Dog".to_string(), dog_meta);

        registry.register_class("Animal".to_string(), ClassMetadata::new("Animal".to_string()));

        let dog_ty = Type::Con(TypeCtor::Class("Dog".to_string()));
        let animal_ty = Type::Con(TypeCtor::Class("Animal".to_string()));
        let producer_dog = Type::App(
            Box::new(Type::Con(TypeCtor::Class("Producer".to_string()))),
            Box::new(dog_ty.clone()),
        );
        let producer_animal = Type::App(
            Box::new(Type::Con(TypeCtor::Class("Producer".to_string()))),
            Box::new(animal_ty.clone()),
        );

        assert!(
            types_compatible(&producer_dog, &producer_animal, &registry, &tv_registry),
            "Producer[Dog] should be assignable to Producer[Animal] with covariant T_Co"
        );
        assert!(
            !types_compatible(&producer_animal, &producer_dog, &registry, &tv_registry),
            "Producer[Animal] should NOT be assignable to Producer[Dog]"
        );
    }

    #[test]
    fn test_user_defined_contravariant_variance() {
        let tv_registry = TypeVarConstraintRegistry::new();
        let mut registry = ClassRegistry::new();
        let mut consumer_meta = ClassMetadata::new("Consumer".to_string());
        consumer_meta.add_base_class("Generic[T_Contra]".to_string());
        consumer_meta.type_params.push("T_Contra".to_string());
        consumer_meta.type_param_vars.push(TypeVar::with_variance(
            0,
            Some("T_Contra".to_string()),
            beacon_core::Variance::Contravariant,
        ));
        registry.register_class("Consumer".to_string(), consumer_meta);

        let mut dog_meta = ClassMetadata::new("Dog".to_string());
        dog_meta.add_base_class("Animal".to_string());
        registry.register_class("Dog".to_string(), dog_meta);

        registry.register_class("Animal".to_string(), ClassMetadata::new("Animal".to_string()));

        let dog_ty = Type::Con(TypeCtor::Class("Dog".to_string()));
        let animal_ty = Type::Con(TypeCtor::Class("Animal".to_string()));
        let consumer_dog = Type::App(
            Box::new(Type::Con(TypeCtor::Class("Consumer".to_string()))),
            Box::new(dog_ty.clone()),
        );
        let consumer_animal = Type::App(
            Box::new(Type::Con(TypeCtor::Class("Consumer".to_string()))),
            Box::new(animal_ty.clone()),
        );

        assert!(
            types_compatible(&consumer_animal, &consumer_dog, &registry, &tv_registry),
            "Consumer[Animal] should be assignable to Consumer[Dog] with contravariant T_Contra"
        );
        assert!(
            !types_compatible(&consumer_dog, &consumer_animal, &registry, &tv_registry),
            "Consumer[Dog] should NOT be assignable to Consumer[Animal]"
        );
    }

    #[test]
    fn test_user_defined_invariant_variance() {
        let tv_registry = TypeVarConstraintRegistry::new();
        let mut registry = ClassRegistry::new();
        let mut box_meta = ClassMetadata::new("Box".to_string());
        box_meta.add_base_class("Generic[T_Inv]".to_string());
        box_meta.type_params.push("T_Inv".to_string());
        box_meta.type_param_vars.push(TypeVar::with_variance(
            0,
            Some("T_Inv".to_string()),
            beacon_core::Variance::Invariant,
        ));
        registry.register_class("Box".to_string(), box_meta);

        let mut dog_meta = ClassMetadata::new("Dog".to_string());
        dog_meta.add_base_class("Animal".to_string());
        registry.register_class("Dog".to_string(), dog_meta);

        registry.register_class("Animal".to_string(), ClassMetadata::new("Animal".to_string()));

        let dog_ty = Type::Con(TypeCtor::Class("Dog".to_string()));
        let animal_ty = Type::Con(TypeCtor::Class("Animal".to_string()));
        let box_dog = Type::App(
            Box::new(Type::Con(TypeCtor::Class("Box".to_string()))),
            Box::new(dog_ty.clone()),
        );
        let box_animal = Type::App(
            Box::new(Type::Con(TypeCtor::Class("Box".to_string()))),
            Box::new(animal_ty.clone()),
        );

        assert!(
            !types_compatible(&box_dog, &box_animal, &registry, &tv_registry),
            "Box[Dog] should NOT be assignable to Box[Animal] with invariant T_Inv"
        );
        assert!(
            !types_compatible(&box_animal, &box_dog, &registry, &tv_registry),
            "Box[Animal] should NOT be assignable to Box[Dog] with invariant T_Inv"
        );

        let box_dog2 = Type::App(
            Box::new(Type::Con(TypeCtor::Class("Box".to_string()))),
            Box::new(dog_ty.clone()),
        );
        assert!(
            types_compatible(&box_dog, &box_dog2, &registry, &tv_registry),
            "Box[Dog] should be assignable to Box[Dog] (same type)"
        );
    }
}

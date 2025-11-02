use crate::{
    Constraint, ConstraintSet, TypeErrorInfo,
    exhaustiveness::{ExhaustivenessResult, ReachabilityResult, check_exhaustiveness, check_reachability},
};

use beacon_core::{
    ClassRegistry, MethodSignature, ProtocolChecker, ProtocolName, Result, Subst, Type, TypeCtor, TypeError, Unifier,
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
                let applied_func = subst.apply(&func_ty);

                if let Type::Con(TypeCtor::Class(class_name)) = &applied_func {
                    if let Some(metadata) = class_registry.get_class(class_name) {
                        if let Some(ctor_ty) = metadata.new_type.as_ref().or(metadata.init_type.as_ref()) {
                            if let Type::Fun(params, _) = ctor_ty {
                                let ctor_params: Vec<Type> = params.iter().skip(1).cloned().collect();
                                if ctor_params.len() == arg_types.len() {
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
                        if bound_params.len() == arg_types.len() {
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
            Constraint::HasAttr(obj_ty, attr_name, attr_ty, span) => {
                let applied_obj = subst.apply(&obj_ty);

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
                    Type::App(base, _param) => {
                        let base_ctor = extract_base_constructor(base);

                        if let Some(class_name) = base_ctor {
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
                                    beacon_core::TypeError::AttributeNotFound(
                                        applied_obj.to_string(),
                                        attr_name.clone(),
                                    ),
                                    span,
                                ));
                            }
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
                let satisfies = match &protocol_name {
                    ProtocolName::UserDefined(proto_name) => {
                        check_user_defined_protocol(&applied_obj, proto_name, class_registry)
                    }
                    _ => ProtocolChecker::satisfies(&applied_obj, &protocol_name),
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

                    match Unifier::unify(&subst.apply(&elem_ty), &extracted_elem) {
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
        }
    }

    Ok((subst, type_errors))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Span;
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
}

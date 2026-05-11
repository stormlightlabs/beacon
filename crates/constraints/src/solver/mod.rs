use crate::{Constraint, ConstraintSet, TypeErrorInfo};

use beacon_core::{ClassRegistry, Result, Subst, TypeError};

mod attributes;
mod calls;
mod compat;
mod equality;
mod error_recovery;
mod joins;
mod narrowing;
mod patterns;
mod protocols;
mod state;

use attributes::solve_attribute_constraint;
#[cfg(test)]
use beacon_core::Type;
use calls::{simplify_substitution, solve_call_constraint};
#[cfg(test)]
use compat::{check_user_defined_protocol, classes_compatible, type_to_method_signature, types_compatible};
use equality::solve_equal_constraint;
use joins::solve_join_constraint;
use narrowing::record_narrowing_constraint;
use patterns::{
    solve_match_pattern_constraint, solve_pattern_exhaustive_constraint, solve_pattern_reachable_constraint,
    solve_pattern_structure_valid_constraint, solve_pattern_type_compatible_constraint,
};
use protocols::solve_protocol_constraint;
use state::SolveState;

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
                let mut state = SolveState::new(&mut subst, &mut type_errors, class_registry, typevar_registry);
                solve_equal_constraint(t1, t2, span, &mut state);
            }
            Constraint::Call(func_ty, pos_args, kw_args, ret_ty, span) => {
                let mut state = SolveState::new(&mut subst, &mut type_errors, class_registry, typevar_registry);
                solve_call_constraint(func_ty, pos_args, kw_args, ret_ty, span, &mut state);
            }
            Constraint::HasAttr(obj_ty, attr_name, attr_ty, span) => {
                let mut state = SolveState::new(&mut subst, &mut type_errors, class_registry, typevar_registry);
                solve_attribute_constraint(obj_ty, attr_name, attr_ty, span, &mut state);
            }
            Constraint::Protocol(obj_ty, protocol_name, elem_ty, span) => {
                let mut state = SolveState::new(&mut subst, &mut type_errors, class_registry, typevar_registry);
                solve_protocol_constraint(obj_ty, protocol_name, elem_ty, span, &mut state);
            }
            Constraint::MatchPattern(_subject_ty, _pattern, bindings, _span) => {
                solve_match_pattern_constraint(bindings, &subst);
            }
            Constraint::PatternExhaustive(subject_ty, patterns, span) => {
                solve_pattern_exhaustive_constraint(subject_ty, patterns, span, &mut type_errors, class_registry);
            }
            Constraint::PatternReachable(pattern, previous_patterns, span) => {
                solve_pattern_reachable_constraint(pattern, previous_patterns, span, &mut type_errors);
            }
            Constraint::PatternTypeCompatible(pattern, subject_ty, span) => {
                solve_pattern_type_compatible_constraint(pattern, subject_ty, span, &mut type_errors, class_registry);
            }
            Constraint::PatternStructureValid(pattern, subject_ty, span) => {
                solve_pattern_structure_valid_constraint(pattern, subject_ty, span, &mut type_errors);
            }
            Constraint::Narrowing(variable, predicate, narrowed_type, span) => {
                record_narrowing_constraint(variable, predicate, narrowed_type, span);
            }
            Constraint::Join(_, incoming_types, result_type, span) => {
                solve_join_constraint(
                    incoming_types,
                    result_type,
                    span,
                    &mut subst,
                    &mut type_errors,
                    class_registry,
                    typevar_registry,
                );
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
    use beacon_core::{ClassMetadata, MethodType, ProtocolName, TypeCtor, TypeVar, TypeVarConstraintRegistry};
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

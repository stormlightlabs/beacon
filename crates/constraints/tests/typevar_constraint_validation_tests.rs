//! TypeVar Constraint Validation Integration Tests
//!
//! Tests that verify function call argument unification properly triggers TypeVar
//! bound and constraint validation. These tests ensure the constraint solver correctly
//! validates TypeVar constraints when processing Call constraints.
//!
//! ## Coverage
//!
//! - TypeVar with bound (e.g., `T = TypeVar('T', bound=Animal)`)
//!   - Valid arguments satisfying the bound
//!   - Invalid arguments violating the bound
//! - TypeVar with constraints (e.g., `U = TypeVar('U', int, str)`)
//!   - Valid arguments in the constraint set
//!   - Invalid arguments not in the constraint set
//! - Multiple TypeVars in function signatures
//!   - Independent validation of each TypeVar
//!   - First argument fails validation
//!   - Second argument fails validation
//! - Repeated generic function calls
//!
//! ## Implementation Note
//!
//! The constraint generation phase creates Call constraints with argument types and spans.
//! When solved, handle_call_args invokes Unifier::unify which validates TypeVar constraints
//! through the TypeVarConstraintRegistry (crates/core/src/unify.rs:234).

use beacon_constraint::solver::solve_constraints;
use beacon_constraint::{Constraint, ConstraintSet, Span};
use beacon_core::{ClassMetadata, ClassRegistry, Type, TypeCtor, TypeVar, TypeVarConstraintRegistry};

fn test_span() -> Span {
    Span::with_end(1, 0, 1, 10)
}

fn tvar(id: u32) -> Type {
    Type::Var(TypeVar::new(id))
}

#[test]
fn test_function_call_with_typevar_bound_success() {
    let tv = TypeVar::new(0);
    let mut typevar_registry = TypeVarConstraintRegistry::new();
    let animal_ty = Type::Con(TypeCtor::Class("Animal".to_string()));
    typevar_registry.set_bound(tv.id, animal_ty.clone());

    let func_ty = Type::fun(vec![("animal".to_string(), Type::Var(tv.clone()))], Type::string());

    let dog_ty = Type::Con(TypeCtor::Class("Dog".to_string()));

    let mut class_registry = ClassRegistry::new();
    let mut dog_metadata = ClassMetadata::new("Dog".to_string());
    dog_metadata.add_base_class("Animal".to_string());
    class_registry.register_class("Dog".to_string(), dog_metadata);
    class_registry.register_class("Animal".to_string(), ClassMetadata::new("Animal".to_string()));

    let ret_ty = tvar(1);
    let constraints = ConstraintSet {
        constraints: vec![Constraint::Call(
            func_ty,
            vec![(dog_ty, test_span())],
            vec![],
            ret_ty.clone(),
            test_span(),
        )],
    };

    let result = solve_constraints(constraints, &class_registry, &typevar_registry);
    assert!(result.is_ok(), "Should accept Dog as argument when T: bound=Animal");

    let (subst, _) = result.unwrap();
    assert_eq!(subst.apply(&ret_ty), Type::string());
}

#[test]
fn test_function_call_with_typevar_bound_failure() {
    let tv = TypeVar::new(0);
    let mut typevar_registry = TypeVarConstraintRegistry::new();
    let animal_ty = Type::Con(TypeCtor::Class("Animal".to_string()));
    typevar_registry.set_bound(tv.id, animal_ty);

    let func_ty = Type::fun(vec![("animal".to_string(), Type::Var(tv.clone()))], Type::string());

    let arg_ty = Type::int();

    let class_registry = ClassRegistry::new();
    let ret_ty = tvar(1);
    let constraints = ConstraintSet {
        constraints: vec![Constraint::Call(
            func_ty,
            vec![(arg_ty, test_span())],
            vec![],
            ret_ty,
            test_span(),
        )],
    };

    let result = solve_constraints(constraints, &class_registry, &typevar_registry);
    assert!(result.is_ok(), "Solver should complete");

    let (_, errors) = result.unwrap();
    assert!(
        !errors.is_empty(),
        "Should have type error for int not satisfying Animal bound"
    );
}

#[test]
fn test_function_call_with_typevar_constraints_success_int() {
    let tv = TypeVar::new(0);
    let mut typevar_registry = TypeVarConstraintRegistry::new();
    typevar_registry.set_constraints(tv.id, vec![Type::int(), Type::string()]);

    let func_ty = Type::fun(
        vec![("value".to_string(), Type::Var(tv.clone()))],
        Type::Var(tv.clone()),
    );

    let arg_ty = Type::int();

    let class_registry = ClassRegistry::new();
    let ret_ty = tvar(1);
    let constraints = ConstraintSet {
        constraints: vec![Constraint::Call(
            func_ty,
            vec![(arg_ty, test_span())],
            vec![],
            ret_ty.clone(),
            test_span(),
        )],
    };

    let result = solve_constraints(constraints, &class_registry, &typevar_registry);
    assert!(result.is_ok(), "Should accept int when U constrained to {{int, str}}");

    let (subst, errors) = result.unwrap();
    assert!(errors.is_empty(), "Should have no errors");
    assert_eq!(subst.apply(&ret_ty), Type::int(), "Return type should be int");
}

#[test]
fn test_function_call_with_typevar_constraints_success_str() {
    let tv = TypeVar::new(0);
    let mut typevar_registry = TypeVarConstraintRegistry::new();
    typevar_registry.set_constraints(tv.id, vec![Type::int(), Type::string()]);

    let func_ty = Type::fun(
        vec![("value".to_string(), Type::Var(tv.clone()))],
        Type::Var(tv.clone()),
    );

    let arg_ty = Type::string();

    let class_registry = ClassRegistry::new();
    let ret_ty = tvar(1);
    let constraints = ConstraintSet {
        constraints: vec![Constraint::Call(
            func_ty,
            vec![(arg_ty, test_span())],
            vec![],
            ret_ty.clone(),
            test_span(),
        )],
    };

    let result = solve_constraints(constraints, &class_registry, &typevar_registry);
    assert!(result.is_ok(), "Should accept str when U constrained to {{int, str}}");

    let (subst, errors) = result.unwrap();
    assert!(errors.is_empty(), "Should have no errors");
    assert_eq!(subst.apply(&ret_ty), Type::string(), "Return type should be str");
}

#[test]
fn test_function_call_with_typevar_constraints_failure() {
    let tv = TypeVar::new(0);
    let mut typevar_registry = TypeVarConstraintRegistry::new();
    typevar_registry.set_constraints(tv.id, vec![Type::int(), Type::string()]);

    let func_ty = Type::fun(
        vec![("value".to_string(), Type::Var(tv.clone()))],
        Type::Var(tv.clone()),
    );

    let arg_ty = Type::float();

    let class_registry = ClassRegistry::new();
    let ret_ty = tvar(1);
    let constraints = ConstraintSet {
        constraints: vec![Constraint::Call(
            func_ty,
            vec![(arg_ty, test_span())],
            vec![],
            ret_ty,
            test_span(),
        )],
    };

    let result = solve_constraints(constraints, &class_registry, &typevar_registry);
    assert!(result.is_ok(), "Solver should complete");

    let (_, errors) = result.unwrap();
    assert!(
        !errors.is_empty(),
        "Should have type error for float not in {{int, str}} constraints"
    );
}

#[test]
fn test_function_call_with_multiple_typevars() {
    let tv_k = TypeVar::new(100);
    let tv_v = TypeVar::new(101);
    let mut typevar_registry = TypeVarConstraintRegistry::new();
    typevar_registry.set_constraints(tv_k.id, vec![Type::int(), Type::string()]);
    let animal_ty = Type::Con(TypeCtor::Class("Animal".to_string()));
    typevar_registry.set_bound(tv_v.id, animal_ty.clone());

    let func_ty = Type::fun(
        vec![
            ("key".to_string(), Type::Var(tv_k.clone())),
            ("value".to_string(), Type::Var(tv_v.clone())),
        ],
        Type::none(),
    );

    let key_arg = Type::string();
    let value_arg = animal_ty.clone();

    let class_registry = ClassRegistry::new();

    let ret_ty = tvar(2);
    let constraints = ConstraintSet {
        constraints: vec![Constraint::Call(
            func_ty,
            vec![(key_arg, test_span()), (value_arg, test_span())],
            vec![],
            ret_ty.clone(),
            test_span(),
        )],
    };

    let result = solve_constraints(constraints, &class_registry, &typevar_registry);
    assert!(
        result.is_ok(),
        "Should accept (str, Animal) when K in {{int, str}} and V: bound=Animal"
    );

    let (subst, errors) = result.unwrap();
    assert!(errors.is_empty(), "Should have no errors, got: {errors:?}");
    assert_eq!(subst.apply(&ret_ty), Type::none());
}

#[test]
fn test_function_call_with_multiple_typevars_first_arg_fails() {
    let tv_k = TypeVar::new(0);
    let tv_v = TypeVar::new(1);
    let mut typevar_registry = TypeVarConstraintRegistry::new();
    typevar_registry.set_constraints(tv_k.id, vec![Type::int(), Type::string()]);
    let animal_ty = Type::Con(TypeCtor::Class("Animal".to_string()));
    typevar_registry.set_bound(tv_v.id, animal_ty);

    let func_ty = Type::fun(
        vec![
            ("key".to_string(), Type::Var(tv_k.clone())),
            ("value".to_string(), Type::Var(tv_v.clone())),
        ],
        Type::none(),
    );

    let key_arg = Type::float();
    let value_arg = Type::Con(TypeCtor::Class("Animal".to_string()));

    let class_registry = ClassRegistry::new();
    let ret_ty = tvar(2);
    let constraints = ConstraintSet {
        constraints: vec![Constraint::Call(
            func_ty,
            vec![(key_arg, test_span()), (value_arg, test_span())],
            vec![],
            ret_ty,
            test_span(),
        )],
    };

    let result = solve_constraints(constraints, &class_registry, &typevar_registry);
    assert!(result.is_ok(), "Solver should complete");

    let (_, errors) = result.unwrap();
    assert!(!errors.is_empty(), "Should have error for float not in K constraints");
}

#[test]
fn test_function_call_with_multiple_typevars_second_arg_fails() {
    let tv_k = TypeVar::new(0);
    let tv_v = TypeVar::new(1);
    let mut typevar_registry = TypeVarConstraintRegistry::new();
    typevar_registry.set_constraints(tv_k.id, vec![Type::int(), Type::string()]);
    let animal_ty = Type::Con(TypeCtor::Class("Animal".to_string()));
    typevar_registry.set_bound(tv_v.id, animal_ty);

    let func_ty = Type::fun(
        vec![
            ("key".to_string(), Type::Var(tv_k.clone())),
            ("value".to_string(), Type::Var(tv_v.clone())),
        ],
        Type::none(),
    );

    let key_arg = Type::string();
    let value_arg = Type::int();

    let class_registry = ClassRegistry::new();
    let ret_ty = tvar(2);
    let constraints = ConstraintSet {
        constraints: vec![Constraint::Call(
            func_ty,
            vec![(key_arg, test_span()), (value_arg, test_span())],
            vec![],
            ret_ty,
            test_span(),
        )],
    };

    let result = solve_constraints(constraints, &class_registry, &typevar_registry);
    assert!(result.is_ok(), "Solver should complete");

    let (_, errors) = result.unwrap();
    assert!(
        !errors.is_empty(),
        "Should have error for int not satisfying V: bound=Animal"
    );
}

#[test]
fn test_generic_function_in_list_context() {
    let tv = TypeVar::new(200);
    let mut typevar_registry = TypeVarConstraintRegistry::new();
    let animal_ty = Type::Con(TypeCtor::Class("Animal".to_string()));
    typevar_registry.set_bound(tv.id, animal_ty.clone());

    let mut class_registry = ClassRegistry::new();
    class_registry.register_class("Animal".to_string(), ClassMetadata::new("Animal".to_string()));

    let func_ty = Type::fun(vec![("animal".to_string(), Type::Var(tv.clone()))], Type::string());
    let ret_ty_1 = tvar(1);
    let ret_ty_2 = tvar(2);

    let constraints = ConstraintSet {
        constraints: vec![
            Constraint::Call(
                func_ty.clone(),
                vec![(animal_ty.clone(), test_span())],
                vec![],
                ret_ty_1.clone(),
                test_span(),
            ),
            Constraint::Call(
                func_ty,
                vec![(animal_ty.clone(), test_span())],
                vec![],
                ret_ty_2.clone(),
                test_span(),
            ),
            // Both should unify to str
            Constraint::Equal(ret_ty_1.clone(), Type::string(), test_span()),
            Constraint::Equal(ret_ty_2.clone(), Type::string(), test_span()),
        ],
    };

    let result = solve_constraints(constraints, &class_registry, &typevar_registry);
    assert!(result.is_ok(), "Should succeed calling generic function multiple times");

    let (subst, errors) = result.unwrap();
    assert!(errors.is_empty(), "Should have no errors, got: {errors:?}");
    assert_eq!(subst.apply(&ret_ty_1), Type::string());
    assert_eq!(subst.apply(&ret_ty_2), Type::string());
}

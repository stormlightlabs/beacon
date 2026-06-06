use super::*;
use crate::TypeVarConstraintRegistry;
use crate::types::TypeVar;

#[test]
fn test_unify_same_types() {
    let t1 = Type::int();
    let t2 = Type::int();
    let subst = Unifier::unify(&t1, &t2, &TypeVarConstraintRegistry::new()).unwrap();
    assert!(subst.is_empty());
}

#[test]
fn test_unify_type_variable() {
    let tv = TypeVar::new(0);
    let t = Type::int();
    let subst = Unifier::unify(&Type::Var(tv.clone()), &t, &TypeVarConstraintRegistry::new()).unwrap();
    assert_eq!(subst.get(&tv), Some(&Type::int()));
    assert_eq!(subst.apply(&Type::Var(tv)), Type::int());
}

#[test]
fn test_unify_function_types() {
    let t1 = Type::fun_unnamed(vec![Type::int()], Type::string());
    let t2 = Type::fun_unnamed(vec![Type::int()], Type::string());
    let subst = Unifier::unify(&t1, &t2, &TypeVarConstraintRegistry::new()).unwrap();
    assert!(subst.is_empty());
}

#[test]
fn test_unify_function_with_variables() {
    let tv1 = TypeVar::new(0);
    let tv2 = TypeVar::new(1);
    let t1 = Type::fun_unnamed(vec![Type::Var(tv1.clone())], Type::Var(tv2.clone()));
    let t2 = Type::fun_unnamed(vec![Type::int()], Type::string());

    let subst = Unifier::unify(&t1, &t2, &TypeVarConstraintRegistry::new()).unwrap();

    assert_eq!(subst.get(&tv1), Some(&Type::int()));
    assert_eq!(subst.get(&tv2), Some(&Type::string()));
}

#[test]
fn test_unify_type_applications() {
    let tv = TypeVar::new(0);
    let t1 = Type::list(Type::Var(tv.clone()));
    let t2 = Type::list(Type::int());
    let subst = Unifier::unify(&t1, &t2, &TypeVarConstraintRegistry::new()).unwrap();
    assert_eq!(subst.get(&tv), Some(&Type::int()));
}

#[test]
fn test_occurs_check_basic() {
    let tv = TypeVar::new(0);
    let recursive_type = Type::list(Type::Var(tv.clone()));
    let result = Unifier::unify(
        &Type::Var(tv),
        &recursive_type,
        &TypeVarConstraintRegistry::new(),
    );
    assert!(result.is_err());
}

#[test]
fn test_occurs_check_in_nested_app() {
    let tv = TypeVar::new(0);
    let nested = Type::list(Type::list(Type::Var(tv.clone())));
    let result = Unifier::unify(&Type::Var(tv), &nested, &TypeVarConstraintRegistry::new());
    assert!(result.is_err());
}

#[test]
fn test_occurs_check_in_function_args() {
    let tv = TypeVar::new(0);
    let fun_type = Type::fun_unnamed(vec![Type::Var(tv.clone())], Type::int());
    let result = Unifier::unify(&Type::Var(tv), &fun_type, &TypeVarConstraintRegistry::new());
    assert!(result.is_err());
}

#[test]
fn test_occurs_check_in_function_return() {
    let tv = TypeVar::new(0);
    let fun_type = Type::fun_unnamed(vec![Type::int()], Type::Var(tv.clone()));
    let result = Unifier::unify(&Type::Var(tv), &fun_type, &TypeVarConstraintRegistry::new());
    assert!(result.is_err());
}

#[test]
fn test_occurs_check_in_union() {
    let tv = TypeVar::new(0);
    let union = Type::union(vec![Type::int(), Type::Var(tv.clone())]);
    let result = Unifier::unify(&Type::Var(tv), &union, &TypeVarConstraintRegistry::new());
    assert!(result.is_err());
}

#[test]
fn test_occurs_check_in_record_fields() {
    let tv = TypeVar::new(0);
    let record = Type::Record(vec![("x".to_string(), Type::Var(tv.clone()))], None);
    let result = Unifier::unify(&Type::Var(tv), &record, &TypeVarConstraintRegistry::new());
    assert!(result.is_err());
}

#[test]
fn test_occurs_check_in_record_row_var() {
    let tv = TypeVar::new(0);
    let record = Type::Record(vec![("x".to_string(), Type::int())], Some(tv.clone()));
    let result = Unifier::unify(&Type::Var(tv), &record, &TypeVarConstraintRegistry::new());
    assert!(result.is_err());
}

#[test]
fn test_occurs_check_does_not_trigger_for_different_vars() {
    let tv1 = TypeVar::new(0);
    let tv2 = TypeVar::new(1);
    let list_type = Type::list(Type::Var(tv2.clone()));
    let result = Unifier::unify(&Type::Var(tv1.clone()), &list_type, &TypeVarConstraintRegistry::new());
    assert!(result.is_ok());
    let subst = result.unwrap();
    assert_eq!(subst.get(&tv1), Some(&Type::list(Type::Var(tv2))));
}

#[test]
fn test_unify_union_types() {
    let union1 = Type::union(vec![Type::int(), Type::string()]);
    let union2 = Type::union(vec![Type::string(), Type::int()]);
    let subst = Unifier::unify(&union1, &union2, &TypeVarConstraintRegistry::new()).unwrap();
    assert!(subst.is_empty());
}

#[test]
fn test_unify_union_with_type() {
    let union = Type::union(vec![Type::int(), Type::string()]);
    let t = Type::int();

    let subst = Unifier::unify(&union, &t, &TypeVarConstraintRegistry::new()).unwrap();
    assert!(subst.is_empty());
}

#[test]
fn test_unify_records() {
    let record1 = Type::Record(vec![("x".to_string(), Type::int())], None);
    let record2 = Type::Record(vec![("x".to_string(), Type::int())], None);

    let subst = Unifier::unify(&record1, &record2, &TypeVarConstraintRegistry::new()).unwrap();
    assert!(subst.is_empty());
}

#[test]
fn test_unify_records_with_row_variable() {
    let row_var = TypeVar::new(0);
    let record1 = Type::Record(vec![("x".to_string(), Type::int())], Some(row_var.clone()));
    let record2 = Type::Record(
        vec![("x".to_string(), Type::int()), ("y".to_string(), Type::string())],
        None,
    );

    let subst = Unifier::unify(&record1, &record2, &TypeVarConstraintRegistry::new()).unwrap();

    if let Some(Type::Record(fields, _)) = subst.get(&row_var) {
        assert_eq!(fields.len(), 1);
        assert_eq!(fields[0].0, "y");
        assert_eq!(fields[0].1, Type::string());
    } else {
        panic!("Row variable should be bound to a record");
    }
}

#[test]
fn test_unify_many() {
    let tv1 = TypeVar::new(0);
    let tv2 = TypeVar::new(1);
    let types = vec![Type::Var(tv1.clone()), Type::int(), Type::Var(tv2.clone())];

    let (unified_type, subst) = Unifier::unify_many(&types, &TypeVarConstraintRegistry::new()).unwrap();

    assert_eq!(unified_type, Type::int());
    assert_eq!(subst.get(&tv1), Some(&Type::int()));
    assert_eq!(subst.get(&tv2), Some(&Type::int()));
}

#[test]
fn test_any_unifies_with_everything() {
    let subst = Unifier::unify(&Type::any(), &Type::int(), &TypeVarConstraintRegistry::new()).unwrap();
    assert!(subst.is_empty());

    let subst = Unifier::unify(&Type::string(), &Type::any(), &TypeVarConstraintRegistry::new()).unwrap();
    assert!(subst.is_empty());

    let tv = TypeVar::new(0);
    let subst = Unifier::unify(&Type::any(), &Type::Var(tv), &TypeVarConstraintRegistry::new()).unwrap();
    assert!(subst.is_empty());

    let subst = Unifier::unify(
        &Type::any(),
        &Type::list(Type::int()),
        &TypeVarConstraintRegistry::new(),
    )
    .unwrap();
    assert!(subst.is_empty());

    let subst = Unifier::unify(
        &Type::fun_unnamed(vec![Type::int()], Type::string()),
        &Type::any(),
        &TypeVarConstraintRegistry::new(),
    )
    .unwrap();
    assert!(subst.is_empty());
}

#[test]
fn test_top_unifies_only_with_itself() {
    let subst = Unifier::unify(&Type::top(), &Type::top(), &TypeVarConstraintRegistry::new()).unwrap();
    assert!(subst.is_empty());

    assert!(Unifier::unify(&Type::top(), &Type::int(), &TypeVarConstraintRegistry::new()).is_err());
    assert!(Unifier::unify(&Type::string(), &Type::top(), &TypeVarConstraintRegistry::new()).is_err());

    let tv = TypeVar::new(0);
    let subst = Unifier::unify(&Type::top(), &Type::Var(tv.clone()), &TypeVarConstraintRegistry::new()).unwrap();
    assert_eq!(subst.get(&tv), Some(&Type::top()));
}

#[test]
fn test_never_unifies_only_with_itself() {
    let subst = Unifier::unify(&Type::never(), &Type::never(), &TypeVarConstraintRegistry::new()).unwrap();
    assert!(subst.is_empty());

    assert!(Unifier::unify(&Type::never(), &Type::int(), &TypeVarConstraintRegistry::new()).is_err());
    assert!(Unifier::unify(&Type::bool(), &Type::never(), &TypeVarConstraintRegistry::new()).is_err());

    let tv = TypeVar::new(0);
    let subst = Unifier::unify(
        &Type::never(),
        &Type::Var(tv.clone()),
        &TypeVarConstraintRegistry::new(),
    )
    .unwrap();
    assert_eq!(subst.get(&tv), Some(&Type::never()));
}

#[test]
fn test_top_any_never_distinct_unification() {
    let subst = Unifier::unify(&Type::top(), &Type::any(), &TypeVarConstraintRegistry::new()).unwrap();
    assert!(subst.is_empty());
    let subst = Unifier::unify(&Type::any(), &Type::top(), &TypeVarConstraintRegistry::new()).unwrap();
    assert!(subst.is_empty());

    assert!(Unifier::unify(&Type::top(), &Type::never(), &TypeVarConstraintRegistry::new()).is_err());
    assert!(Unifier::unify(&Type::never(), &Type::top(), &TypeVarConstraintRegistry::new()).is_err());

    let subst = Unifier::unify(&Type::any(), &Type::never(), &TypeVarConstraintRegistry::new()).unwrap();
    assert!(subst.is_empty());
    let subst = Unifier::unify(&Type::never(), &Type::any(), &TypeVarConstraintRegistry::new()).unwrap();
    assert!(subst.is_empty());
}

#[test]
fn test_any_in_complex_types() {
    let f1 = Type::fun_unnamed(vec![Type::any()], Type::int());
    let f2 = Type::fun_unnamed(vec![Type::string()], Type::int());
    let subst = Unifier::unify(&f1, &f2, &TypeVarConstraintRegistry::new()).unwrap();
    assert!(subst.is_empty());

    let list_any = Type::list(Type::any());
    let list_int = Type::list(Type::int());
    let subst = Unifier::unify(&list_any, &list_int, &TypeVarConstraintRegistry::new()).unwrap();
    assert!(subst.is_empty());
}

#[test]
fn test_record_row_variable_round_trip() {
    let row_var = TypeVar::new(10);
    let record1 = Type::Record(vec![("x".to_string(), Type::int())], Some(row_var.clone()));
    let record2 = Type::Record(
        vec![("x".to_string(), Type::int()), ("y".to_string(), Type::string())],
        None,
    );

    let subst = Unifier::unify(&record1, &record2, &TypeVarConstraintRegistry::new()).unwrap();

    match subst.get(&row_var).expect("Row variable should be bound") {
        Type::Record(fields, None) => {
            assert_eq!(fields.len(), 1);
            assert_eq!(fields[0].0, "y");
            assert_eq!(fields[0].1, Type::string());
        }
        _ => panic!("Expected record binding for row variable"),
    }

    match subst.apply(&record1) {
        Type::Record(fields, row_var) => {
            assert!(row_var.is_none() || fields.len() == 1);
        }
        _ => panic!("Expected record after substitution"),
    }
}

#[test]
fn test_record_unification_idempotence() {
    let record = Type::Record(
        vec![("x".to_string(), Type::int()), ("y".to_string(), Type::string())],
        None,
    );

    let subst = Unifier::unify(&record, &record, &TypeVarConstraintRegistry::new()).unwrap();
    assert!(subst.is_empty());

    let result = subst.apply(&record);
    assert_eq!(result, record);
}

#[test]
fn test_record_with_nested_row_variables() {
    let row_var1 = TypeVar::new(11);
    let row_var2 = TypeVar::new(12);

    let record1 = Type::Record(vec![("x".to_string(), Type::int())], Some(row_var1.clone()));
    let record2 = Type::Record(vec![("x".to_string(), Type::int())], Some(row_var2.clone()));

    let subst = Unifier::unify(&record1, &record2, &TypeVarConstraintRegistry::new()).unwrap();
    assert!(subst.contains_var(&row_var1) || subst.contains_var(&row_var2) || subst.is_empty());
}

#[test]
fn test_complex_record_round_trip() {
    let tv1 = TypeVar::new(20);
    let tv2 = TypeVar::new(21);
    let record = Type::Record(
        vec![
            ("x".to_string(), Type::Var(tv1.clone())),
            ("y".to_string(), Type::Var(tv2.clone())),
        ],
        None,
    );

    let subst1 = Subst::singleton(tv1, Type::int());
    let record_after_1 = subst1.apply(&record);

    let subst2 = Subst::singleton(tv2, Type::string());
    let record_after_2 = subst2.apply(&record_after_1);

    match record_after_2 {
        Type::Record(fields, None) => {
            assert_eq!(fields.len(), 2);
            assert_eq!(fields.iter().find(|(k, _)| k == "x").unwrap().1, Type::int());
            assert_eq!(fields.iter().find(|(k, _)| k == "y").unwrap().1, Type::string());
        }
        _ => panic!("Expected concrete record"),
    }
}

#[test]
fn test_union_with_type_var_unifies_with_concrete() {
    let tv = TypeVar::new(30);
    let union = Type::union(vec![Type::Var(tv.clone()), Type::none()]);
    let concrete = Type::int();

    let subst = Unifier::unify(&union, &concrete, &TypeVarConstraintRegistry::new()).unwrap();
    assert_eq!(subst.get(&tv), Some(&Type::int()));

    let union_after = subst.apply(&union);
    match union_after {
        Type::Union(types) => {
            assert_eq!(types.len(), 2);
            assert!(types.contains(&Type::int()));
            assert!(types.contains(&Type::none()));
        }
        _ => panic!("Expected Union type after substitution"),
    }
}

#[test]
fn test_union_concrete_types_unifies_with_member() {
    let union = Type::union(vec![Type::int(), Type::string()]);
    let concrete = Type::int();

    let subst = Unifier::unify(&union, &concrete, &TypeVarConstraintRegistry::new()).unwrap();
    assert!(subst.is_empty());

    let union2 = Type::union(vec![Type::int(), Type::none()]);
    let none = Type::none();
    let subst2 = Unifier::unify(&union2, &none, &TypeVarConstraintRegistry::new()).unwrap();
    assert!(subst2.is_empty());
}

#[test]
fn test_union_fails_when_no_member_matches() {
    let union = Type::union(vec![Type::int(), Type::string()]);
    let concrete = Type::bool();
    let result = Unifier::unify(&union, &concrete, &TypeVarConstraintRegistry::new());
    assert!(result.is_err());
}

#[test]
fn test_optional_type_pattern() {
    let tv = TypeVar::new(31);
    let optional_t = Type::union(vec![Type::Var(tv.clone()), Type::none()]);

    let concrete = Type::string();
    let subst = Unifier::unify(&optional_t, &concrete, &TypeVarConstraintRegistry::new()).unwrap();
    assert_eq!(subst.get(&tv), Some(&Type::string()));

    let tv2 = TypeVar::new(310);
    let optional_t2 = Type::union(vec![Type::Var(tv2.clone()), Type::none()]);
    let none = Type::none();
    let subst2 = Unifier::unify(&optional_t2, &none, &TypeVarConstraintRegistry::new()).unwrap();
    assert!(subst2.is_empty() || subst2.get(&tv2) == Some(&Type::none()));
}

#[test]
fn test_union_with_multiple_type_vars() {
    let tv1 = TypeVar::new(32);
    let tv2 = TypeVar::new(33);
    let union = Type::union(vec![Type::Var(tv1.clone()), Type::Var(tv2.clone())]);
    let concrete = Type::int();

    let subst = Unifier::unify(&union, &concrete, &TypeVarConstraintRegistry::new()).unwrap();
    assert_eq!(subst.get(&tv1), Some(&Type::int()));
    assert!(!subst.contains_var(&tv2));
}

#[test]
fn test_union_type_var_flow() {
    let tv_registry = TypeVarConstraintRegistry::new();
    let tv_union_member = TypeVar::new(34);
    let tv_result = TypeVar::new(35);
    let union = Type::union(vec![Type::Var(tv_union_member.clone()), Type::none()]);
    let subst1 = Unifier::unify(&Type::Var(tv_result.clone()), &union, &TypeVarConstraintRegistry::new()).unwrap();

    assert_eq!(subst1.get(&tv_result), Some(&union));

    let union_type = subst1.get(&tv_result).unwrap();
    let subst2 = Unifier::unify(union_type, &Type::int(), &tv_registry).unwrap();
    assert_eq!(subst2.get(&tv_union_member), Some(&Type::int()));

    let final_subst = subst2.compose(subst1);
    let result_type = final_subst.apply(&Type::Var(tv_result));

    match result_type {
        Type::Union(types) => {
            assert_eq!(types.len(), 2);
            assert!(types.contains(&Type::int()));
            assert!(types.contains(&Type::none()));
        }
        _ => panic!("Expected Union[int, None] but got: {result_type}"),
    }
}

#[test]
fn test_union_with_complex_types() {
    let tv = TypeVar::new(36);
    let list_t = Type::list(Type::Var(tv.clone()));
    let union = Type::union(vec![list_t, Type::none()]);
    let list_int = Type::list(Type::int());

    let subst = Unifier::unify(&union, &list_int, &TypeVarConstraintRegistry::new()).unwrap();
    assert_eq!(subst.get(&tv), Some(&Type::int()));
}

#[test]
fn test_union_error_messages() {
    let union = Type::union(vec![Type::int(), Type::string()]);
    let concrete = Type::bool();

    match Unifier::unify(&union, &concrete, &TypeVarConstraintRegistry::new()) {
        Err(e) => {
            let msg = format!("{e}");
            assert!(msg.contains("union") || msg.contains("Union"));
        }
        Ok(_) => panic!("Expected unification to fail"),
    }
}

#[test]
fn test_heterogeneous_tuple_unify_same() {
    let tuple1 = Type::tuple_heterogeneous(vec![Type::int(), Type::string(), Type::bool()]);
    let tuple2 = Type::tuple_heterogeneous(vec![Type::int(), Type::string(), Type::bool()]);
    let subst = Unifier::unify(&tuple1, &tuple2, &TypeVarConstraintRegistry::new()).unwrap();
    assert!(subst.is_empty());
}

#[test]
fn test_heterogeneous_tuple_unify_with_vars() {
    let tv1 = TypeVar::new(0);
    let tv2 = TypeVar::new(1);
    let tuple1 = Type::tuple_heterogeneous(vec![Type::Var(tv1.clone()), Type::Var(tv2.clone())]);
    let tuple2 = Type::tuple_heterogeneous(vec![Type::int(), Type::string()]);
    let subst = Unifier::unify(&tuple1, &tuple2, &TypeVarConstraintRegistry::new()).unwrap();
    assert_eq!(subst.get(&tv1), Some(&Type::int()));
    assert_eq!(subst.get(&tv2), Some(&Type::string()));
}

#[test]
fn test_heterogeneous_tuple_length_mismatch() {
    let tuple1 = Type::tuple_heterogeneous(vec![Type::int(), Type::string()]);
    let tuple2 = Type::tuple_heterogeneous(vec![Type::int(), Type::string(), Type::bool()]);
    let result = Unifier::unify(&tuple1, &tuple2, &TypeVarConstraintRegistry::new());
    assert!(result.is_err());
}

#[test]
fn test_heterogeneous_tuple_element_type_mismatch() {
    let tuple1 = Type::tuple_heterogeneous(vec![Type::int(), Type::string()]);
    let tuple2 = Type::tuple_heterogeneous(vec![Type::int(), Type::bool()]);
    let result = Unifier::unify(&tuple1, &tuple2, &TypeVarConstraintRegistry::new());
    assert!(result.is_err());
}

#[test]
fn test_heterogeneous_tuple_occurs_check() {
    let tv = TypeVar::new(0);
    let tuple_with_var = Type::tuple_heterogeneous(vec![Type::Var(tv.clone()), Type::int()]);
    let result = Unifier::unify(
        &Type::Var(tv),
        &tuple_with_var,
        &TypeVarConstraintRegistry::new(),
    );
    assert!(result.is_err(), "Occurs check should prevent recursive type");
}

#[test]
fn test_heterogeneous_tuple_nested() {
    let tv1 = TypeVar::new(0);
    let tv2 = TypeVar::new(1);
    let inner_tuple = Type::tuple_heterogeneous(vec![Type::Var(tv1.clone()), Type::int()]);
    let outer_tuple1 = Type::tuple_heterogeneous(vec![inner_tuple, Type::Var(tv2.clone())]);
    let inner_concrete = Type::tuple_heterogeneous(vec![Type::string(), Type::int()]);
    let outer_tuple2 = Type::tuple_heterogeneous(vec![inner_concrete, Type::bool()]);
    let subst = Unifier::unify(&outer_tuple1, &outer_tuple2, &TypeVarConstraintRegistry::new()).unwrap();
    assert_eq!(subst.get(&tv1), Some(&Type::string()));
    assert_eq!(subst.get(&tv2), Some(&Type::bool()));
}

#[test]
fn test_variance_error_invariant_list() {
    let list_dog = Type::list(Type::Con(TypeCtor::Class("Dog".to_string())));
    let list_animal = Type::list(Type::Con(TypeCtor::Class("Animal".to_string())));

    let result = Unifier::unify(&list_dog, &list_animal, &TypeVarConstraintRegistry::new());
    assert!(result.is_err(), "Invariant lists with different types should not unify");

    if let Err(e) = result {
        let err_msg = format!("{e}");
        assert!(
            err_msg.contains("Variance") || err_msg.contains("invariant") || err_msg.contains("Cannot unify"),
            "Error should mention variance or unification failure: {err_msg}"
        );
    }
}

#[test]
fn test_variance_covariant_tuple_with_vars() {
    let tv = TypeVar::new(0);
    let tuple_var = Type::App(Box::new(Type::Con(TypeCtor::Tuple)), Box::new(Type::Var(tv.clone())));
    let tuple_int = Type::App(Box::new(Type::Con(TypeCtor::Tuple)), Box::new(Type::int()));

    let subst = Unifier::unify(&tuple_var, &tuple_int, &TypeVarConstraintRegistry::new()).unwrap();
    assert_eq!(subst.get(&tv), Some(&Type::int()));
}

#[test]
fn test_variance_invariant_dict() {
    let dict_str_int = Type::App(
        Box::new(Type::App(Box::new(Type::Con(TypeCtor::Dict)), Box::new(Type::string()))),
        Box::new(Type::int()),
    );
    let dict_str_float = Type::App(
        Box::new(Type::App(Box::new(Type::Con(TypeCtor::Dict)), Box::new(Type::string()))),
        Box::new(Type::float()),
    );

    let result = Unifier::unify(&dict_str_int, &dict_str_float, &TypeVarConstraintRegistry::new());
    assert!(result.is_err(), "Invariant dict types should not unify");
}

#[test]
fn test_variance_with_type_variables_unifies() {
    let tv = TypeVar::new(0);
    let list_var = Type::list(Type::Var(tv.clone()));
    let list_int = Type::list(Type::int());
    let subst = Unifier::unify(&list_var, &list_int, &TypeVarConstraintRegistry::new()).unwrap();
    assert_eq!(subst.get(&tv), Some(&Type::int()));
}

#[test]
fn test_variance_contravariant_generator_send() {
    let tv = TypeVar::new(0);

    let gen_var = Type::App(
        Box::new(Type::App(
            Box::new(Type::App(
                Box::new(Type::Con(TypeCtor::Generator)),
                Box::new(Type::int()),
            )),
            Box::new(Type::Var(tv.clone())),
        )),
        Box::new(Type::string()),
    );

    let gen_str = Type::App(
        Box::new(Type::App(
            Box::new(Type::App(
                Box::new(Type::Con(TypeCtor::Generator)),
                Box::new(Type::int()),
            )),
            Box::new(Type::string()),
        )),
        Box::new(Type::string()),
    );

    let subst = Unifier::unify(&gen_var, &gen_str, &TypeVarConstraintRegistry::new()).unwrap();
    assert_eq!(subst.get(&tv), Some(&Type::string()));
}

#[test]
fn test_forall_alpha_equivalence() {
    let tv1 = TypeVar::new(100);
    let tv2 = TypeVar::new(200);

    let forall1 = Type::ForAll(
        vec![tv1.clone()],
        Box::new(Type::fun_unnamed(vec![Type::Var(tv1.clone())], Type::Var(tv1))),
    );

    let forall2 = Type::ForAll(
        vec![tv2.clone()],
        Box::new(Type::fun_unnamed(vec![Type::Var(tv2.clone())], Type::Var(tv2))),
    );

    let result = Unifier::unify(&forall1, &forall2, &TypeVarConstraintRegistry::new());
    assert!(result.is_ok(), "ForAll types with alpha-equivalent bodies should unify");
}

#[test]
fn test_forall_with_monomorphic() {
    let tv = TypeVar::new(300);
    let forall_type = Type::ForAll(vec![tv.clone()], Box::new(Type::list(Type::Var(tv.clone()))));

    let mono_type = Type::list(Type::int());

    let result = Unifier::unify(&forall_type, &mono_type, &TypeVarConstraintRegistry::new());
    assert!(result.is_ok(), "ForAll should unify with compatible monomorphic type");

    let subst = result.unwrap();
    assert_eq!(subst.get(&tv), Some(&Type::int()));
}

#[test]
fn test_forall_different_param_count() {
    let tv1 = TypeVar::new(400);
    let tv2 = TypeVar::new(401);
    let tv3 = TypeVar::new(402);

    let forall1 = Type::ForAll(
        vec![tv1.clone()],
        Box::new(Type::fun_unnamed(vec![Type::Var(tv1)], Type::int())),
    );

    let forall2 = Type::ForAll(
        vec![tv2.clone(), tv3.clone()],
        Box::new(Type::fun_unnamed(vec![Type::Var(tv2)], Type::Var(tv3))),
    );

    let result = Unifier::unify(&forall1, &forall2, &TypeVarConstraintRegistry::new());
    assert!(
        result.is_err(),
        "ForAll with different parameter counts should not unify"
    );
}

#[test]
fn test_forall_nested_in_list() {
    let tv = TypeVar::new(500);
    let forall_type = Type::ForAll(
        vec![tv.clone()],
        Box::new(Type::fun_unnamed(vec![Type::Var(tv.clone())], Type::Var(tv))),
    );

    let list_forall = Type::list(forall_type);
    let list_mono = Type::list(Type::fun_unnamed(vec![Type::int()], Type::int()));

    let result = Unifier::unify(&list_forall, &list_mono, &TypeVarConstraintRegistry::new());
    assert!(result.is_ok(), "Nested ForAll in type constructor should unify");
}

#[test]
fn test_extract_type_ctor_and_param_count_single_param() {
    let list_int = Type::list(Type::int());
    let result = Unifier::extract_type_ctor_and_param_count(&list_int);
    assert!(result.is_some());
    let (ctor, count) = result.unwrap();
    assert_eq!(*ctor, TypeCtor::List);
    assert_eq!(count, 1);
}

#[test]
fn test_extract_type_ctor_and_param_count_two_params() {
    let dict_str_int = Type::dict(Type::string(), Type::int());
    let result = Unifier::extract_type_ctor_and_param_count(&dict_str_int);
    assert!(result.is_some());
    let (ctor, count) = result.unwrap();
    assert_eq!(*ctor, TypeCtor::Dict);
    assert_eq!(count, 2);
}

#[test]
fn test_extract_type_ctor_and_param_count_three_params() {
    let gen_type = Type::generator(Type::int(), Type::string(), Type::bool());
    let result = Unifier::extract_type_ctor_and_param_count(&gen_type);
    assert!(result.is_some());
    let (ctor, count) = result.unwrap();
    assert_eq!(*ctor, TypeCtor::Generator);
    assert_eq!(count, 3);
}

#[test]
fn test_variance_multi_param_dict_key_invariant() {
    let tv1 = TypeVar::new(600);
    let dict_var_int = Type::dict(Type::Var(tv1.clone()), Type::int());
    let dict_str_int = Type::dict(Type::string(), Type::int());

    let result = Unifier::unify(&dict_var_int, &dict_str_int, &TypeVarConstraintRegistry::new());
    assert!(result.is_ok());
    let subst = result.unwrap();
    assert_eq!(subst.get(&tv1), Some(&Type::string()));
}

#[test]
fn test_variance_multi_param_dict_value_invariant() {
    let tv = TypeVar::new(602);
    let dict_str_var = Type::dict(Type::string(), Type::Var(tv.clone()));
    let dict_str_int = Type::dict(Type::string(), Type::int());

    let result = Unifier::unify(&dict_str_var, &dict_str_int, &TypeVarConstraintRegistry::new());
    assert!(result.is_ok());
    let subst = result.unwrap();
    assert_eq!(subst.get(&tv), Some(&Type::int()));
}

#[test]
fn test_variance_multi_param_dict_both_invariant() {
    let dog = Type::Con(TypeCtor::Class("Dog".to_string()));
    let animal = Type::Con(TypeCtor::Class("Animal".to_string()));

    let dict_dog_dog = Type::dict(dog.clone(), dog);
    let dict_animal_animal = Type::dict(animal.clone(), animal);

    let result = Unifier::unify(&dict_dog_dog, &dict_animal_animal, &TypeVarConstraintRegistry::new());
    assert!(result.is_err(), "Dict is invariant in both parameters");
}

#[test]
fn test_variance_generator_all_three_params() {
    let tv_yield = TypeVar::new(700);
    let tv_send = TypeVar::new(701);
    let tv_return = TypeVar::new(702);

    let gen_var = Type::generator(
        Type::Var(tv_yield.clone()),
        Type::Var(tv_send.clone()),
        Type::Var(tv_return.clone()),
    );
    let gen_concrete = Type::generator(Type::int(), Type::string(), Type::bool());

    let result = Unifier::unify(&gen_var, &gen_concrete, &TypeVarConstraintRegistry::new());
    assert!(result.is_ok());
    let subst = result.unwrap();
    assert_eq!(subst.get(&tv_yield), Some(&Type::int()));
    assert_eq!(subst.get(&tv_send), Some(&Type::string()));
    assert_eq!(subst.get(&tv_return), Some(&Type::bool()));
}

#[test]
fn test_variance_async_generator_both_params() {
    let tv_yield = TypeVar::new(800);
    let tv_send = TypeVar::new(801);

    let gen_var = Type::async_generator(Type::Var(tv_yield.clone()), Type::Var(tv_send.clone()));
    let gen_concrete = Type::async_generator(Type::int(), Type::string());

    let result = Unifier::unify(&gen_var, &gen_concrete, &TypeVarConstraintRegistry::new());
    assert!(result.is_ok());
    let subst = result.unwrap();
    assert_eq!(subst.get(&tv_yield), Some(&Type::int()));
    assert_eq!(subst.get(&tv_send), Some(&Type::string()));
}

#[test]
fn test_variance_nested_generic_hierarchy() {
    let dog = Type::Con(TypeCtor::Class("Dog".to_string()));
    let animal = Type::Con(TypeCtor::Class("Animal".to_string()));

    let producer_ctor = Type::Con(TypeCtor::Class("Producer".to_string()));
    let consumer_ctor = Type::Con(TypeCtor::Class("Consumer".to_string()));

    let consumer_dog = Type::App(Box::new(consumer_ctor.clone()), Box::new(dog));
    let producer_consumer_dog = Type::App(Box::new(producer_ctor.clone()), Box::new(consumer_dog));

    let consumer_animal = Type::App(Box::new(consumer_ctor), Box::new(animal));
    let producer_consumer_animal = Type::App(Box::new(producer_ctor), Box::new(consumer_animal));

    let result = Unifier::unify(
        &producer_consumer_dog,
        &producer_consumer_dog,
        &TypeVarConstraintRegistry::new(),
    );
    assert!(result.is_ok());

    let result = Unifier::unify(
        &producer_consumer_animal,
        &producer_consumer_animal,
        &TypeVarConstraintRegistry::new(),
    );
    assert!(result.is_ok());
}

#[test]
fn test_variance_deeply_nested_list() {
    let tv = TypeVar::new(900);
    let list_list_list_var = Type::list(Type::list(Type::list(Type::Var(tv.clone()))));
    let list_list_list_int = Type::list(Type::list(Type::list(Type::int())));

    let result = Unifier::unify(
        &list_list_list_var,
        &list_list_list_int,
        &TypeVarConstraintRegistry::new(),
    );
    assert!(result.is_ok());
    let subst = result.unwrap();
    assert_eq!(subst.get(&tv), Some(&Type::int()));
}

#[test]
fn test_variance_dict_with_nested_generics() {
    let tv = TypeVar::new(901);
    let dict_str_list_var = Type::dict(Type::string(), Type::list(Type::Var(tv.clone())));
    let dict_str_list_int = Type::dict(Type::string(), Type::list(Type::int()));

    let result = Unifier::unify(
        &dict_str_list_var,
        &dict_str_list_int,
        &TypeVarConstraintRegistry::new(),
    );
    assert!(result.is_ok());
    let subst = result.unwrap();
    assert_eq!(subst.get(&tv), Some(&Type::int()));
}

#[test]
fn test_typevar_bound_validation_success() {
    let tv = TypeVar::new(0);
    let mut registry = TypeVarConstraintRegistry::new();
    let animal = Type::Con(TypeCtor::Class("Animal".to_string()));
    registry.set_bound(tv.id, animal.clone());

    let result = Unifier::unify(&Type::Var(tv), &animal, &registry);
    assert!(result.is_ok(), "Should unify TypeVar with its exact bound");
}

#[test]
fn test_typevar_bound_validation_failure() {
    let tv = TypeVar::new(0);
    let mut registry = TypeVarConstraintRegistry::new();
    let animal = Type::Con(TypeCtor::Class("Animal".to_string()));
    registry.set_bound(tv.id, animal);

    let result = Unifier::unify(&Type::Var(tv), &Type::int(), &registry);
    assert!(
        result.is_err(),
        "Should fail to unify TypeVar with type that violates bound"
    );
}

#[test]
fn test_typevar_constraints_validation_success() {
    let tv = TypeVar::new(0);
    let mut registry = TypeVarConstraintRegistry::new();
    registry.set_constraints(tv.id, vec![Type::int(), Type::string()]);

    let result_int = Unifier::unify(&Type::Var(tv.clone()), &Type::int(), &registry);
    assert!(result_int.is_ok(), "Should unify TypeVar with int constraint");

    let result_str = Unifier::unify(&Type::Var(tv), &Type::string(), &registry);
    assert!(result_str.is_ok(), "Should unify TypeVar with str constraint");
}

#[test]
fn test_typevar_constraints_validation_failure() {
    let tv = TypeVar::new(0);
    let mut registry = TypeVarConstraintRegistry::new();
    registry.set_constraints(tv.id, vec![Type::int(), Type::string()]);

    let result = Unifier::unify(&Type::Var(tv), &Type::bool(), &registry);
    assert!(
        result.is_err(),
        "Should fail to unify TypeVar with type not in constraints"
    );
}

#[test]
fn test_typevar_bound_in_generic_context() {
    let tv = TypeVar::new(0);
    let mut registry = TypeVarConstraintRegistry::new();
    let animal = Type::Con(TypeCtor::Class("Animal".to_string()));
    registry.set_bound(tv.id, animal.clone());

    let list_tv = Type::list(Type::Var(tv.clone()));
    let list_animal = Type::list(animal.clone());

    let result = Unifier::unify(&list_tv, &list_animal, &registry);
    assert!(
        result.is_ok(),
        "Should unify list[T] with list[Animal] when T: bound=Animal"
    );

    let subst = result.unwrap();
    assert_eq!(subst.get(&tv), Some(&animal), "Should substitute T with Animal");
}

#[test]
fn test_typevar_constraints_in_generic_context() {
    let tv = TypeVar::new(0);
    let mut registry = TypeVarConstraintRegistry::new();
    registry.set_constraints(tv.id, vec![Type::int(), Type::string()]);

    let list_tv = Type::list(Type::Var(tv.clone()));
    let list_int = Type::list(Type::int());

    let result = Unifier::unify(&list_tv, &list_int, &registry);
    assert!(
        result.is_ok(),
        "Should unify list[T] with list[int] when T constrained to int|str"
    );

    let subst = result.unwrap();
    assert_eq!(subst.get(&tv), Some(&Type::int()), "Should substitute T with int");
}

#[test]
fn test_typevar_bound_and_constraints_both_validated() {
    let tv = TypeVar::new(0);
    let mut registry = TypeVarConstraintRegistry::new();
    let animal = Type::Con(TypeCtor::Class("Animal".to_string()));

    registry.set_bound(tv.id, animal.clone());
    registry.set_constraints(tv.id, vec![Type::int(), Type::string()]);

    let result = Unifier::unify(&Type::Var(tv.clone()), &animal, &registry);
    assert!(
        result.is_err(),
        "Should fail when type satisfies bound but not constraints (Animal not in [int, str])"
    );

    let result_int = Unifier::unify(&Type::Var(tv), &Type::int(), &registry);
    assert!(
        result_int.is_err(),
        "Should fail when type satisfies constraints but not bound (int not subtype of Animal)"
    );
}

#[test]
fn test_multiple_typevars_with_different_bounds() {
    let tv1 = TypeVar::new(0);
    let tv2 = TypeVar::new(1);
    let mut registry = TypeVarConstraintRegistry::new();

    let animal = Type::Con(TypeCtor::Class("Animal".to_string()));
    let vehicle = Type::Con(TypeCtor::Class("Vehicle".to_string()));

    registry.set_bound(tv1.id, animal.clone());
    registry.set_bound(tv2.id, vehicle.clone());

    let result1 = Unifier::unify(&Type::Var(tv1.clone()), &animal, &registry);
    assert!(result1.is_ok());

    let result2 = Unifier::unify(&Type::Var(tv2), &vehicle, &registry);
    assert!(result2.is_ok());

    let result_cross = Unifier::unify(&Type::Var(tv1), &vehicle, &registry);
    assert!(result_cross.is_err(), "Should fail to unify T1:Animal with Vehicle");
}

#[test]
fn test_invariant_types_allow_unresolved_typevars() {
    let registry = TypeVarConstraintRegistry::new();
    let tv = TypeVar::new(42);

    let actual_value = Type::union(vec![Type::Var(tv), Type::int(), Type::bool()]);
    let annotated_value = Type::union(vec![Type::int(), Type::bool()]);

    let actual = Type::dict(Type::int(), actual_value);
    let annotated = Type::dict(Type::int(), annotated_value);

    let result = Unifier::unify(&actual, &annotated, &registry);
    assert!(
        result.is_ok(),
        "Invariant containers with unresolved type vars should not raise variance errors"
    );
}

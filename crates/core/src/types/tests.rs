use crate::{ClassMetadata, ClassRegistry};

use super::*;
use rustc_hash::FxHashMap;

#[test]
fn test_kind_arity() {
    assert_eq!(Kind::arity(0), Kind::Star);
    assert_eq!(Kind::arity(1), Kind::Arrow(Box::new(Kind::Star), Box::new(Kind::Star)));
    assert_eq!(
        Kind::arity(2),
        Kind::Arrow(
            Box::new(Kind::Star),
            Box::new(Kind::Arrow(Box::new(Kind::Star), Box::new(Kind::Star)))
        )
    );
}

#[test]
fn test_type_var_display() {
    let tv1 = TypeVar::new(0);
    let tv2 = TypeVar::named(1, "alpha");
    assert_eq!(tv1.to_string(), "'t0");
    assert_eq!(tv2.to_string(), "'alpha1");
}

#[test]
fn test_type_constructors() {
    assert_eq!(Type::int(), Type::Con(TypeCtor::Int));
    assert_eq!(Type::string(), Type::Con(TypeCtor::String));

    let list_int = Type::list(Type::int());
    assert_eq!(
        list_int,
        Type::App(Box::new(Type::Con(TypeCtor::List)), Box::new(Type::int()))
    );
}

#[test]
fn test_function_types() {
    let fun_type = Type::fun_unnamed(vec![Type::int(), Type::string()], Type::bool());
    assert_eq!(
        fun_type,
        Type::Fun(
            vec![("_0".to_string(), Type::int()), ("_1".to_string(), Type::string())],
            Box::new(Type::bool())
        )
    );
    assert_eq!(fun_type.to_string(), "(int, str) -> bool");
}

#[test]
fn test_union_types() {
    let union = Type::union(vec![Type::int(), Type::string()]);
    if let Type::Union(types) = union {
        assert_eq!(types.len(), 2);
        assert!(types.contains(&Type::int()));
        assert!(types.contains(&Type::string()));
    } else {
        panic!("Expected union type");
    }
}

#[test]
fn test_optional_type() {
    let opt_int = Type::optional(Type::int());
    if let Type::Union(types) = opt_int {
        assert_eq!(types.len(), 2);
        assert!(types.contains(&Type::int()));
        assert!(types.contains(&Type::none()));
    } else {
        panic!("Expected union type for optional");
    }
}

#[test]
fn test_free_vars() {
    let tv1 = TypeVar::new(0);
    let tv2 = TypeVar::new(1);
    let t = Type::fun_unnamed(vec![Type::Var(tv1.clone())], Type::Var(tv2.clone()));

    let free_vars = t.free_vars();
    assert_eq!(free_vars.len(), 2);
    assert!(free_vars.contains_key(&tv1));
    assert!(free_vars.contains_key(&tv2));
}

#[test]
fn test_type_scheme_generalization() {
    let tv1 = TypeVar::new(0);
    let tv2 = TypeVar::new(1);
    let ty = Type::fun_unnamed(vec![Type::Var(tv1.clone())], Type::Var(tv1.clone()));

    let mut env_vars = FxHashMap::default();
    env_vars.insert(tv2, ());

    let scheme = TypeScheme::generalize(ty.clone(), &env_vars);
    assert_eq!(scheme.quantified_vars.len(), 1);
    assert_eq!(scheme.quantified_vars[0], tv1);
    assert_eq!(scheme.ty, ty);
}

#[test]
fn test_type_display() {
    assert_eq!(Type::int().to_string(), "int");
    assert_eq!(Type::list(Type::string()).to_string(), "list[str]");
    assert_eq!(Type::dict(Type::string(), Type::int()).to_string(), "dict[str, int]");

    let fun = Type::fun_unnamed(vec![Type::int()], Type::bool());
    assert_eq!(fun.to_string(), "int -> bool");
}

#[test]
fn test_structural_protocol_subtyping_with_registry() {
    let mut class_registry = ClassRegistry::new();

    let mut readable = ClassMetadata::new("Readable".to_string());
    readable.set_protocol(true);
    readable.add_method("read".to_string(), Type::fun(vec![], Type::string()));
    class_registry.register_class("Readable".to_string(), readable);

    let mut file_reader = ClassMetadata::new("FileReader".to_string());
    file_reader.add_method("read".to_string(), Type::fun(vec![], Type::string()));
    class_registry.register_class("FileReader".to_string(), file_reader);

    let mut writer = ClassMetadata::new("Writer".to_string());
    writer.add_method(
        "write".to_string(),
        Type::fun(vec![("value".to_string(), Type::string())], Type::none()),
    );
    class_registry.register_class("Writer".to_string(), writer);

    let readable_ty = Type::Con(TypeCtor::Class("Readable".to_string()));
    let file_reader_ty = Type::Con(TypeCtor::Class("FileReader".to_string()));
    let writer_ty = Type::Con(TypeCtor::Class("Writer".to_string()));

    assert!(file_reader_ty.is_subtype_of_with_registry(&readable_ty, Some(&class_registry)));
    assert!(!writer_ty.is_subtype_of_with_registry(&readable_ty, Some(&class_registry)));
}

#[test]
fn test_type_constructor_kinds() {
    assert_eq!(TypeCtor::Int.kind(), Kind::Star);
    assert_eq!(TypeCtor::List.kind(), Kind::arity(1));
    assert_eq!(TypeCtor::Dict.kind(), Kind::arity(2));
}

#[test]
fn test_kind_display() {
    assert_eq!(Kind::Star.to_string(), "*");
    assert_eq!(
        Kind::Arrow(Box::new(Kind::Star), Box::new(Kind::Star)).to_string(),
        "* -> *"
    );
    assert_eq!(Kind::arity(2).to_string(), "* -> * -> *");
}

#[test]
fn test_type_var_display_with_hint() {
    let tv_no_hint = TypeVar::new(5);
    let tv_with_hint = TypeVar::named(5, "alpha");

    assert_eq!(tv_no_hint.to_string(), "'t5");
    assert_eq!(tv_with_hint.to_string(), "'alpha5");
}

#[test]
fn test_type_constructor_display() {
    assert_eq!(TypeCtor::Int.to_string(), "int");
    assert_eq!(TypeCtor::Float.to_string(), "float");
    assert_eq!(TypeCtor::String.to_string(), "str");
    assert_eq!(TypeCtor::Bool.to_string(), "bool");
    assert_eq!(TypeCtor::NoneType.to_string(), "None");
    assert_eq!(TypeCtor::Any.to_string(), "Any");
    assert_eq!(TypeCtor::Never.to_string(), "Never");
    assert_eq!(TypeCtor::Class("MyClass".to_string()).to_string(), "MyClass");
    assert_eq!(TypeCtor::Module("os".to_string()).to_string(), "module<os>");
}

#[test]
fn test_complex_type_app_display() {
    let nested_list = Type::list(Type::list(Type::int()));
    assert_eq!(nested_list.to_string(), "list[list[int]]");

    let dict_str_list = Type::dict(Type::string(), Type::list(Type::int()));
    assert_eq!(dict_str_list.to_string(), "dict[str, list[int]]");
}

#[test]
fn test_function_type_display() {
    let no_args = Type::fun_unnamed(vec![], Type::int());
    assert_eq!(no_args.to_string(), "() -> int");

    let one_arg = Type::fun_unnamed(vec![Type::string()], Type::bool());
    assert_eq!(one_arg.to_string(), "str -> bool");

    let multi_args = Type::fun_unnamed(vec![Type::int(), Type::string(), Type::bool()], Type::none());
    assert_eq!(multi_args.to_string(), "(int, str, bool) -> None");
}

#[test]
fn test_forall_display() {
    let tv1 = TypeVar::new(0);
    let tv2 = TypeVar::new(1);

    let simple_forall = Type::ForAll(vec![tv1.clone()], Box::new(Type::Var(tv1.clone())));
    assert_eq!(simple_forall.to_string(), "∀'t0. 't0");

    let multi_forall = Type::ForAll(
        vec![tv1.clone(), tv2.clone()],
        Box::new(Type::fun_unnamed(vec![Type::Var(tv1)], Type::Var(tv2))),
    );
    assert_eq!(multi_forall.to_string(), "∀'t0 't1. 't0 -> 't1");
}

#[test]
fn test_union_display() {
    let simple_union = Type::union(vec![Type::int(), Type::string()]);
    assert_eq!(simple_union.to_string(), "int | str");

    let multi_union = Type::union(vec![Type::int(), Type::string(), Type::bool(), Type::none()]);
    let display_str = multi_union.to_string();
    assert!(display_str.contains("int"));
    assert!(display_str.contains("str"));
    assert!(display_str.contains("bool"));
    assert!(display_str.contains("None"));
}

#[test]
fn test_record_display() {
    let simple_record = Type::Record(
        vec![("x".to_string(), Type::int()), ("y".to_string(), Type::string())],
        None,
    );
    assert_eq!(simple_record.to_string(), "{ x: int, y: str }");

    let row_var = TypeVar::new(0);
    let record_with_row = Type::Record(vec![("x".to_string(), Type::int())], Some(row_var));
    assert_eq!(record_with_row.to_string(), "{ x: int | 't0 }");

    let empty_record = Type::Record(vec![], None);
    assert_eq!(empty_record.to_string(), "{  }");
}

#[test]
fn test_type_scheme_display() {
    let tv = TypeVar::new(0);

    let mono_scheme = TypeScheme::mono(Type::int());
    assert_eq!(mono_scheme.to_string(), "int");

    let poly_scheme = TypeScheme::new(
        vec![tv.clone()],
        Type::fun_unnamed(vec![Type::Var(tv.clone())], Type::Var(tv)),
    );
    assert_eq!(poly_scheme.to_string(), "∀'t0. 't0 -> 't0");
}

#[test]
fn test_set_type_display() {
    let set_int = Type::App(Box::new(Type::Con(TypeCtor::Set)), Box::new(Type::int()));
    assert_eq!(set_int.to_string(), "set[int]");

    let set_str = Type::App(Box::new(Type::Con(TypeCtor::Set)), Box::new(Type::string()));
    assert_eq!(set_str.to_string(), "set[str]");

    let nested_set = Type::App(Box::new(Type::Con(TypeCtor::Set)), Box::new(Type::list(Type::bool())));
    assert_eq!(nested_set.to_string(), "set[list[bool]]");
}

#[test]
fn test_generic_type_app_display() {
    let tv = TypeVar::new(0);
    let generic_app = Type::App(Box::new(Type::Var(tv)), Box::new(Type::int()));
    assert_eq!(generic_app.to_string(), "('t0 int)");

    let fun_ctor = Type::Con(TypeCtor::Function);
    let fun_app = Type::App(Box::new(fun_ctor), Box::new(Type::string()));
    assert_eq!(fun_app.to_string(), "(function str)");
}

#[test]
fn test_top_type_constructor() {
    let top = Type::top();
    assert_eq!(top, Type::Con(TypeCtor::Top));
    assert_eq!(top.to_string(), "Top");
    assert_eq!(TypeCtor::Top.kind(), Kind::Star);
    assert!(TypeCtor::Top.is_builtin());
}

#[test]
fn test_any_type_constructor() {
    let any = Type::any();
    assert_eq!(any, Type::Con(TypeCtor::Any));
    assert_eq!(any.to_string(), "Any");
    assert_eq!(TypeCtor::Any.kind(), Kind::Star);
    assert!(TypeCtor::Any.is_builtin());
}

#[test]
fn test_never_type_constructor() {
    let never = Type::never();
    assert_eq!(never, Type::Con(TypeCtor::Never));
    assert_eq!(never.to_string(), "Never");
    assert_eq!(TypeCtor::Never.kind(), Kind::Star);
    assert!(TypeCtor::Never.is_builtin());
}

#[test]
fn test_top_any_never_are_distinct() {
    let top = Type::top();
    let any = Type::any();
    let never = Type::never();

    assert_ne!(top, any);
    assert_ne!(top, never);
    assert_ne!(any, never);
}

#[test]
fn test_lattice_types_have_no_free_vars() {
    assert_eq!(Type::top().free_vars().len(), 0);
    assert_eq!(Type::any().free_vars().len(), 0);
    assert_eq!(Type::never().free_vars().len(), 0);
}

#[test]
fn test_well_kinded_simple_types() {
    assert!(Type::int().check_well_kinded().is_ok());
    assert!(Type::string().check_well_kinded().is_ok());
    assert!(Type::bool().check_well_kinded().is_ok());
    assert!(Type::any().check_well_kinded().is_ok());
    assert!(Type::top().check_well_kinded().is_ok());
    assert!(Type::never().check_well_kinded().is_ok());
}

#[test]
fn test_well_kinded_type_applications() {
    let list_int = Type::list(Type::int());
    assert!(list_int.check_well_kinded().is_ok());
    assert_eq!(list_int.kind_of().unwrap(), Kind::Star);

    let dict_str_int = Type::dict(Type::string(), Type::int());
    assert!(dict_str_int.check_well_kinded().is_ok());
    assert_eq!(dict_str_int.kind_of().unwrap(), Kind::Star);

    let nested_list = Type::list(Type::list(Type::int()));
    assert!(nested_list.check_well_kinded().is_ok());
}

#[test]
fn test_ill_kinded_type_applications() {
    let int_ctor = Type::Con(TypeCtor::Int);
    let ill_kinded = Type::App(Box::new(int_ctor), Box::new(Type::string()));
    assert!(ill_kinded.check_well_kinded().is_err());

    let unapplied_list = Type::Con(TypeCtor::List);
    assert!(unapplied_list.check_well_kinded().is_err());

    let dict_ctor = Type::Con(TypeCtor::Dict);
    let partial_dict = Type::App(Box::new(dict_ctor), Box::new(Type::int()));
    assert!(partial_dict.check_well_kinded().is_err());
}

#[test]
fn test_kind_of_type_constructors() {
    assert_eq!(Type::Con(TypeCtor::Int).kind_of().unwrap(), Kind::Star);
    assert_eq!(Type::Con(TypeCtor::List).kind_of().unwrap(), Kind::arity(1));
    assert_eq!(Type::Con(TypeCtor::Dict).kind_of().unwrap(), Kind::arity(2));
}

#[test]
fn test_kind_of_function_types() {
    let fun_type = Type::fun_unnamed(vec![Type::int(), Type::string()], Type::bool());
    assert_eq!(fun_type.kind_of().unwrap(), Kind::Star);
    assert!(fun_type.check_well_kinded().is_ok());
}

#[test]
fn test_kind_of_union_types() {
    let union = Type::union(vec![Type::int(), Type::string()]);
    assert_eq!(union.kind_of().unwrap(), Kind::Star);
    assert!(union.check_well_kinded().is_ok());
}

#[test]
fn test_kind_of_record_types() {
    let record = Type::Record(vec![("x".to_string(), Type::int())], None);
    assert_eq!(record.kind_of().unwrap(), Kind::Star);
    assert!(record.check_well_kinded().is_ok());
}

#[test]
fn test_kind_of_forall_types() {
    let tv = TypeVar::new(0);
    let forall = Type::ForAll(vec![tv.clone()], Box::new(Type::Var(tv)));
    assert_eq!(forall.kind_of().unwrap(), Kind::Star);
    assert!(forall.check_well_kinded().is_ok());
}

#[test]
fn test_kind_mismatch_in_application() {
    let list_ctor = Type::Con(TypeCtor::List);
    let dict_ctor = Type::Con(TypeCtor::Dict);
    let mismatched = Type::App(Box::new(list_ctor), Box::new(dict_ctor));

    let result = mismatched.kind_of();
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("Kind mismatch"));
}

#[test]
fn test_union_normalization_flattens_nested_unions() {
    let inner_union = Type::union(vec![Type::string(), Type::bool()]);
    let outer_union = Type::union(vec![Type::int(), inner_union]);

    match outer_union {
        Type::Union(types) => {
            assert_eq!(types.len(), 3);
            assert!(types.contains(&Type::bool()));
            assert!(types.contains(&Type::int()));
            assert!(types.contains(&Type::string()));
        }
        _ => panic!("Expected union type"),
    }
}

#[test]
fn test_union_normalization_removes_duplicates() {
    let union = Type::union(vec![Type::int(), Type::string(), Type::int(), Type::string()]);

    match union {
        Type::Union(types) => {
            assert_eq!(types.len(), 2);
            assert!(types.contains(&Type::int()));
            assert!(types.contains(&Type::string()));
        }
        _ => panic!("Expected union type"),
    }
}

#[test]
fn test_union_normalization_is_sorted() {
    let union = Type::union(vec![Type::string(), Type::bool(), Type::int()]);

    match union {
        Type::Union(types) => {
            for i in 1..types.len() {
                assert!(types[i - 1] <= types[i], "Union types should be sorted");
            }
        }
        _ => panic!("Expected union type"),
    }
}

#[test]
fn test_union_normalization_idempotence() {
    let union1 = Type::union(vec![Type::int(), Type::string()]);
    let union2 = Type::union(vec![Type::int(), Type::string()]);

    assert_eq!(union1, union2);

    if let Type::Union(ref types1) = union1 {
        let union3 = Type::union(types1.clone());
        assert_eq!(union1, union3);
    }
}

#[test]
fn test_union_single_element_unwraps() {
    let union = Type::union(vec![Type::int()]);
    assert_eq!(union, Type::int());
}

#[test]
fn test_optional_is_normalized_union() {
    let opt = Type::optional(Type::int());

    match opt {
        Type::Union(types) => {
            assert_eq!(types.len(), 2);
            assert!(types.contains(&Type::int()));
            assert!(types.contains(&Type::none()));
        }
        _ => panic!("Expected union for optional"),
    }
}

#[test]
fn test_is_optional() {
    let opt = Type::optional(Type::int());
    assert!(opt.is_optional());

    let not_opt = Type::int();
    assert!(!not_opt.is_optional());

    let union = Type::union(vec![Type::int(), Type::string()]);
    assert!(!union.is_optional());

    let triple_union = Type::union(vec![Type::int(), Type::string(), Type::none()]);
    assert!(!triple_union.is_optional());
}

#[test]
fn test_unwrap_optional() {
    let opt_int = Type::optional(Type::int());
    assert_eq!(opt_int.unwrap_optional(), Some(Type::int()));

    let opt_str = Type::optional(Type::string());
    assert_eq!(opt_str.unwrap_optional(), Some(Type::string()));

    let not_opt = Type::int();
    assert_eq!(not_opt.unwrap_optional(), None);

    let union = Type::union(vec![Type::int(), Type::string()]);
    assert_eq!(union.unwrap_optional(), None);
}

#[test]
fn test_remove_from_union_basic() {
    let opt_int = Type::optional(Type::int());
    let just_int = opt_int.remove_from_union(&Type::none());
    assert_eq!(just_int, Type::int());
}

#[test]
fn test_remove_from_union_multi() {
    let union = Type::union(vec![Type::int(), Type::string(), Type::bool()]);
    let without_str = union.remove_from_union(&Type::string());

    match without_str {
        Type::Union(types) => {
            assert_eq!(types.len(), 2);
            assert!(types.contains(&Type::int()));
            assert!(types.contains(&Type::bool()));
        }
        _ => panic!("Expected union type"),
    }
}

#[test]
fn test_remove_from_union_non_union() {
    let int_type = Type::int();
    let result = int_type.remove_from_union(&Type::string());
    assert_eq!(result, Type::int());

    let result2 = int_type.remove_from_union(&Type::int());
    assert_eq!(result2, Type::never());
}

#[test]
fn test_remove_from_union_all_elements() {
    let union = Type::union(vec![Type::int(), Type::string()]);
    let without_int = union.remove_from_union(&Type::int());
    let without_both = without_int.remove_from_union(&Type::string());
    assert_eq!(without_both, Type::never());
}

#[test]
fn test_type_scheme_generalization_basic() {
    let tv = TypeVar::new(0);
    let ty = Type::fun_unnamed(vec![Type::Var(tv.clone())], Type::Var(tv.clone()));

    let env_vars = FxHashMap::default();
    let scheme = TypeScheme::generalize(ty.clone(), &env_vars);

    assert_eq!(scheme.quantified_vars.len(), 1);
    assert_eq!(scheme.quantified_vars[0], tv);
    assert_eq!(scheme.ty, ty);
}

#[test]
fn test_type_scheme_generalization_respects_environment() {
    let tv_a = TypeVar::new(0);
    let tv_b = TypeVar::new(1);
    let ty = Type::fun_unnamed(vec![Type::Var(tv_a.clone())], Type::Var(tv_b.clone()));

    let mut env_vars = FxHashMap::default();
    env_vars.insert(tv_a, ());

    let scheme = TypeScheme::generalize(ty, &env_vars);

    assert_eq!(scheme.quantified_vars.len(), 1);
    assert_eq!(scheme.quantified_vars[0], tv_b);
}

#[test]
fn test_type_scheme_monomorphic() {
    let ty = Type::int();
    let scheme = TypeScheme::mono(ty.clone());

    assert_eq!(scheme.quantified_vars.len(), 0);
    assert_eq!(scheme.ty, ty);
}

#[test]
fn test_type_scheme_display_polymorphic() {
    let tv = TypeVar::new(0);
    let ty = Type::fun_unnamed(vec![Type::Var(tv.clone())], Type::Var(tv.clone()));
    let scheme = TypeScheme::new(vec![tv], ty);

    let display = scheme.to_string();
    assert!(display.contains("∀"));
    assert!(display.contains("'t0"));
}

#[test]
fn test_type_scheme_display_monomorphic() {
    let scheme = TypeScheme::mono(Type::int());
    let display = scheme.to_string();
    assert!(!display.contains("∀"));
    assert_eq!(display, "int");
}

#[test]
fn test_generalization_with_complex_types() {
    let tv = TypeVar::new(5);
    let ty = Type::list(Type::Var(tv.clone()));

    let env_vars = FxHashMap::default();
    let scheme = TypeScheme::generalize(ty, &env_vars);

    assert_eq!(scheme.quantified_vars.len(), 1);
    assert_eq!(scheme.quantified_vars[0], tv);
}

#[test]
fn test_generalization_with_multiple_vars() {
    let tv_a = TypeVar::new(10);
    let tv_b = TypeVar::new(11);
    let ty = Type::dict(Type::Var(tv_a.clone()), Type::Var(tv_b.clone()));

    let env_vars = FxHashMap::default();
    let scheme = TypeScheme::generalize(ty, &env_vars);

    assert_eq!(scheme.quantified_vars.len(), 2);
    assert!(scheme.quantified_vars.contains(&tv_a));
    assert!(scheme.quantified_vars.contains(&tv_b));
}

#[test]
fn test_generalization_excludes_concrete_types() {
    let tv = TypeVar::new(20);
    let ty = Type::fun_unnamed(vec![Type::int()], Type::Var(tv.clone()));

    let env_vars = FxHashMap::default();
    let scheme = TypeScheme::generalize(ty, &env_vars);

    assert_eq!(scheme.quantified_vars.len(), 1);
    assert_eq!(scheme.quantified_vars[0], tv);
}

#[test]
fn test_value_restriction_prevents_generalization() {
    let tv = TypeVar::new(30);
    let ty = Type::fun_unnamed(vec![Type::Var(tv.clone())], Type::Var(tv.clone()));

    let env_vars = FxHashMap::default();
    let scheme_non_expansive = TypeScheme::generalize_with_restriction(ty.clone(), &env_vars, true);

    assert_eq!(scheme_non_expansive.quantified_vars.len(), 1);
    assert_eq!(scheme_non_expansive.quantified_vars[0], tv);

    let scheme_expansive = TypeScheme::generalize_with_restriction(ty.clone(), &env_vars, false);

    assert_eq!(scheme_expansive.quantified_vars.len(), 0);
    assert_eq!(scheme_expansive.ty, ty);
}

#[test]
fn test_value_restriction_with_free_vars() {
    let tv_a = TypeVar::new(40);
    let tv_b = TypeVar::new(41);
    let ty = Type::fun_unnamed(vec![Type::Var(tv_a.clone())], Type::Var(tv_b.clone()));

    let env_vars = FxHashMap::default();

    let non_exp_scheme = TypeScheme::generalize_with_restriction(ty.clone(), &env_vars, true);
    assert_eq!(non_exp_scheme.quantified_vars.len(), 2);

    let exp_scheme = TypeScheme::generalize_with_restriction(ty.clone(), &env_vars, false);
    assert_eq!(exp_scheme.quantified_vars.len(), 0);
    assert_eq!(exp_scheme.ty, ty);
}

#[test]
fn test_default_generalize_assumes_non_expansive() {
    let tv = TypeVar::new(50);
    let ty = Type::Var(tv.clone());

    let env_vars = FxHashMap::default();
    let scheme1 = TypeScheme::generalize(ty.clone(), &env_vars);
    let scheme2 = TypeScheme::generalize_with_restriction(ty, &env_vars, true);

    assert_eq!(scheme1.quantified_vars, scheme2.quantified_vars);
    assert_eq!(scheme1.ty, scheme2.ty);
}

#[test]
fn test_subtype_reflexivity() {
    assert!(Type::int().is_subtype_of(&Type::int()));
    assert!(Type::string().is_subtype_of(&Type::string()));
    let list_int = Type::list(Type::int());
    assert!(list_int.is_subtype_of(&list_int));
}

/// Any acts as a supertype (for gradual typing) but not as a universal subtype
/// This enables proper variance checking for protocol method signatures
#[test]
fn test_subtype_any_as_supertype() {
    assert!(Type::int().is_subtype_of(&Type::any()));
    assert!(Type::string().is_subtype_of(&Type::any()));
    assert!(Type::list(Type::int()).is_subtype_of(&Type::any()));
    assert!(Type::never().is_subtype_of(&Type::any()));

    assert!(!Type::any().is_subtype_of(&Type::int()));
    assert!(!Type::any().is_subtype_of(&Type::string()));

    assert!(Type::any().is_subtype_of(&Type::any()));
    assert!(Type::any().is_subtype_of(&Type::top()));
}

#[test]
fn test_subtype_never_is_bottom() {
    assert!(Type::never().is_subtype_of(&Type::int()));
    assert!(Type::never().is_subtype_of(&Type::string()));
    assert!(Type::never().is_subtype_of(&Type::any()));
    assert!(Type::never().is_subtype_of(&Type::top()));

    assert!(!Type::int().is_subtype_of(&Type::never()));
    assert!(!Type::string().is_subtype_of(&Type::never()));
}

#[test]
fn test_subtype_top_is_top() {
    assert!(Type::int().is_subtype_of(&Type::top()));
    assert!(Type::string().is_subtype_of(&Type::top()));
    assert!(Type::list(Type::int()).is_subtype_of(&Type::top()));
    assert!(Type::never().is_subtype_of(&Type::top()));

    assert!(!Type::top().is_subtype_of(&Type::int()));
    assert!(!Type::top().is_subtype_of(&Type::string()));
}

#[test]
fn test_subtype_concrete_types_not_related() {
    assert!(!Type::int().is_subtype_of(&Type::string()));
    assert!(!Type::string().is_subtype_of(&Type::bool()));
    assert!(!Type::bool().is_subtype_of(&Type::int()));
}

#[test]
fn test_subtype_union_covariance() {
    let int_or_str = Type::union(vec![Type::int(), Type::string()]);

    assert!(Type::int().is_subtype_of(&int_or_str));
    assert!(Type::string().is_subtype_of(&int_or_str));
    assert!(!Type::bool().is_subtype_of(&int_or_str));
    assert!(!int_or_str.is_subtype_of(&Type::int()));

    let int_str_bool = Type::union(vec![Type::int(), Type::string(), Type::bool()]);
    assert!(int_or_str.is_subtype_of(&int_str_bool));
}

/// (Any -> str) <: (int -> str)
/// A function accepting Any can substitute for one accepting int
/// (int -> str) </: (Any -> str)
/// A function accepting only int cannot substitute for one accepting Any
#[test]
fn test_subtype_function_contravariance() {
    let f_any_str = Type::fun_unnamed(vec![Type::any()], Type::string());
    let f_int_str = Type::fun_unnamed(vec![Type::int()], Type::string());

    assert!(f_any_str.is_subtype_of(&f_int_str));
    assert!(!f_int_str.is_subtype_of(&f_any_str));
}

#[test]
fn test_subtype_function_return_covariance() {
    let f_int_never = Type::fun_unnamed(vec![Type::int()], Type::never());
    let f_int_str = Type::fun_unnamed(vec![Type::int()], Type::string());
    assert!(f_int_never.is_subtype_of(&f_int_str));

    let f_int_top = Type::fun_unnamed(vec![Type::int()], Type::top());
    assert!(!f_int_top.is_subtype_of(&f_int_str));
}

#[test]
fn test_subtype_function_arity_mismatch() {
    let f_one_arg = Type::fun_unnamed(vec![Type::int()], Type::string());
    let f_two_args = Type::fun_unnamed(vec![Type::int(), Type::int()], Type::string());

    assert!(!f_one_arg.is_subtype_of(&f_two_args));
    assert!(!f_two_args.is_subtype_of(&f_one_arg));
}

#[test]
fn test_subtype_function_complex_variance() {
    let inner1 = Type::fun_unnamed(vec![Type::int()], Type::string());
    let inner2 = Type::fun_unnamed(vec![Type::any()], Type::string());
    let outer1 = Type::fun_unnamed(vec![inner1.clone()], Type::bool());
    let outer2 = Type::fun_unnamed(vec![inner2.clone()], Type::bool());

    assert!(inner2.is_subtype_of(&inner1));
    assert!(!inner1.is_subtype_of(&inner2));
    assert!(outer1.is_subtype_of(&outer2));
    assert!(!outer2.is_subtype_of(&outer1));
}

#[test]
fn test_subtype_type_application() {
    let list_int = Type::list(Type::int());
    let list_str = Type::list(Type::string());

    assert!(list_int.is_subtype_of(&list_int));
    assert!(!list_int.is_subtype_of(&list_str));
    assert!(!list_str.is_subtype_of(&list_int));
}

#[test]
fn test_subtype_record_structural() {
    let r1 = Type::Record(
        vec![("x".to_string(), Type::int()), ("y".to_string(), Type::string())],
        None,
    );
    let r2 = Type::Record(vec![("x".to_string(), Type::int())], None);

    assert!(r1.is_subtype_of(&r2));
    assert!(!r2.is_subtype_of(&r1));
}

#[test]
fn test_subtype_record_field_types() {
    let r1 = Type::Record(vec![("x".to_string(), Type::int())], None);
    let r2 = Type::Record(vec![("x".to_string(), Type::string())], None);

    assert!(!r1.is_subtype_of(&r2));
    assert!(!r2.is_subtype_of(&r1));
}

#[test]
fn test_subtype_record_with_top() {
    let r_int = Type::Record(vec![("x".to_string(), Type::int())], None);
    let r_top = Type::Record(vec![("x".to_string(), Type::top())], None);

    assert!(r_int.is_subtype_of(&r_top));
    assert!(!r_top.is_subtype_of(&r_int));
}

#[test]
fn test_subtype_bound_method() {
    let bm1 = Type::BoundMethod(
        Box::new(Type::int()),
        "test".to_string(),
        Box::new(Type::fun_unnamed(vec![], Type::string())),
    );
    let bm2 = Type::BoundMethod(
        Box::new(Type::int()),
        "test".to_string(),
        Box::new(Type::fun_unnamed(vec![], Type::string())),
    );
    let bm3 = Type::BoundMethod(
        Box::new(Type::string()),
        "test".to_string(),
        Box::new(Type::fun_unnamed(vec![], Type::string())),
    );

    assert!(bm1.is_subtype_of(&bm2));
    assert!(!bm1.is_subtype_of(&bm3));
}

#[test]
fn test_intersection_normalization_flattens_nested() {
    let inner = Type::intersection(vec![Type::string(), Type::bool()]);
    let outer = Type::intersection(vec![Type::int(), inner]);

    match outer {
        Type::Intersection(types) => {
            assert_eq!(types.len(), 3);
            assert!(types.contains(&Type::bool()));
            assert!(types.contains(&Type::int()));
            assert!(types.contains(&Type::string()));
        }
        _ => panic!("Expected intersection type"),
    }
}

#[test]
fn test_intersection_normalization_removes_duplicates() {
    let intersection = Type::intersection(vec![Type::int(), Type::string(), Type::int(), Type::string()]);

    match intersection {
        Type::Intersection(types) => {
            assert_eq!(types.len(), 2);
            assert!(types.contains(&Type::int()));
            assert!(types.contains(&Type::string()));
        }
        _ => panic!("Expected intersection type"),
    }
}

#[test]
fn test_intersection_normalization_is_sorted() {
    let intersection = Type::intersection(vec![Type::string(), Type::bool(), Type::int()]);

    match intersection {
        Type::Intersection(types) => {
            for i in 1..types.len() {
                assert!(types[i - 1] <= types[i], "Intersection types should be sorted");
            }
        }
        _ => panic!("Expected intersection type"),
    }
}

#[test]
fn test_intersection_single_element_unwraps() {
    let intersection = Type::intersection(vec![Type::int()]);
    assert_eq!(intersection, Type::int());
}

#[test]
fn test_intersection_display() {
    let intersection = Type::intersection(vec![Type::int(), Type::string()]);
    let display = intersection.to_string();
    assert!(display.contains("int"));
    assert!(display.contains("str"));
    assert!(display.contains(" & "));
}

/// T <: Intersection[A, B] means T <: A and T <: B
#[test]
fn test_subtype_intersection_all_must_satisfy() {
    let intersection = Type::intersection(vec![Type::int(), Type::string()]);

    assert!(!Type::int().is_subtype_of(&intersection));
    assert!(Type::never().is_subtype_of(&intersection));
}

/// Intersection[A, B] is more specific than either A or B, so it's a subtype of both
#[test]
fn test_subtype_intersection_to_single_type() {
    let proto1 = Type::Con(TypeCtor::Protocol(Some("Iterable".to_string()), vec![]));
    let proto2 = Type::Con(TypeCtor::Protocol(Some("Sized".to_string()), vec![]));
    let intersection = Type::intersection(vec![proto1.clone(), proto2.clone()]);

    assert!(intersection.is_subtype_of(&proto1));
    assert!(intersection.is_subtype_of(&proto2));
    assert!(intersection.is_subtype_of(&Type::any()));
    assert!(intersection.is_subtype_of(&Type::top()));
}

#[test]
fn test_subtype_intersection_with_protocols() {
    let proto1 = Type::Con(TypeCtor::Protocol(Some("Iterable".to_string()), vec![]));
    let proto2 = Type::Con(TypeCtor::Protocol(Some("Sized".to_string()), vec![]));
    let both = Type::intersection(vec![proto1.clone(), proto2.clone()]);

    assert!(both.is_subtype_of(&proto1));
    assert!(both.is_subtype_of(&proto2));
}

#[test]
fn test_intersection_kind() {
    let intersection = Type::intersection(vec![Type::int(), Type::string()]);
    assert_eq!(intersection.kind_of().unwrap(), Kind::Star);
}

#[test]
fn test_overload_set_creation() {
    let sig1 = Type::fun_unnamed(vec![Type::int()], Type::string());
    let sig2 = Type::fun_unnamed(vec![Type::string()], Type::string());
    let overload = OverloadSet::new(vec![sig1, sig2]);

    assert_eq!(overload.signatures.len(), 2);
    assert!(overload.implementation.is_none());
}

#[test]
fn test_overload_set_with_implementation() {
    let sig1 = Type::fun_unnamed(vec![Type::int()], Type::string());
    let sig2 = Type::fun_unnamed(vec![Type::string()], Type::string());
    let impl_sig = Type::fun_unnamed(vec![Type::any()], Type::string());
    let overload = OverloadSet::with_implementation(vec![sig1, sig2], impl_sig.clone());

    assert_eq!(overload.signatures.len(), 2);
    assert_eq!(overload.implementation, Some(impl_sig));
}

#[test]
fn test_overload_resolution_exact_match() {
    let sig1 = Type::fun_unnamed(vec![Type::int()], Type::string());
    let sig2 = Type::fun_unnamed(vec![Type::string()], Type::bool());
    let overload = OverloadSet::new(vec![sig1.clone(), sig2]);

    let resolved = overload.resolve(&[Type::int()]);
    assert_eq!(resolved, Some(&sig1));
}

#[test]
fn test_overload_resolution_subtype_match() {
    let sig1 = Type::fun_unnamed(vec![Type::any()], Type::string());
    let sig2 = Type::fun_unnamed(vec![Type::int()], Type::bool());
    let overload = OverloadSet::new(vec![sig1.clone(), sig2.clone()]);

    let resolved = overload.resolve(&[Type::int()]);
    assert_eq!(resolved, Some(&sig1));
}

#[test]
fn test_overload_resolution_first_match_wins() {
    let sig1 = Type::fun_unnamed(vec![Type::any()], Type::string());
    let sig2 = Type::fun_unnamed(vec![Type::int()], Type::bool());
    let overload = OverloadSet::new(vec![sig1.clone(), sig2]);

    let resolved = overload.resolve(&[Type::int()]);
    assert_eq!(resolved, Some(&sig1));
}

#[test]
fn test_overload_resolution_no_match() {
    let sig1 = Type::fun_unnamed(vec![Type::int()], Type::string());
    let sig2 = Type::fun_unnamed(vec![Type::string()], Type::bool());
    let overload = OverloadSet::new(vec![sig1, sig2]);

    let resolved = overload.resolve(&[Type::bool()]);
    assert!(resolved.is_none());
}

#[test]
fn test_overload_resolution_fallback_to_implementation() {
    let sig1 = Type::fun_unnamed(vec![Type::int()], Type::string());
    let impl_sig = Type::fun_unnamed(vec![Type::any()], Type::string());
    let mut overload = OverloadSet::new(vec![sig1]);
    overload.set_implementation(impl_sig.clone());

    let resolved = overload.resolve(&[Type::string()]);
    assert_eq!(resolved, Some(&impl_sig));
}

#[test]
fn test_overload_resolution_arity_mismatch() {
    let sig1 = Type::fun_unnamed(vec![Type::int()], Type::string());
    let sig2 = Type::fun_unnamed(vec![Type::int(), Type::int()], Type::bool());
    let overload = OverloadSet::new(vec![sig1, sig2.clone()]);

    let resolved = overload.resolve(&[Type::int(), Type::int()]);
    assert_eq!(resolved, Some(&sig2));
}

#[test]
fn test_overload_display() {
    let sig1 = Type::fun_unnamed(vec![Type::int()], Type::string());
    let sig2 = Type::fun_unnamed(vec![Type::string()], Type::bool());
    let overload = OverloadSet::new(vec![sig1, sig2]);

    let display = overload.to_string();
    assert!(display.contains("Overload["));
    assert!(display.contains("int"));
    assert!(display.contains("str"));
}

#[test]
fn test_simplify_union_with_any() {
    let union = Type::union(vec![Type::int(), Type::any(), Type::string()]);
    let simplified = union.simplify();
    assert_eq!(simplified, Type::any());
}

#[test]
fn test_simplify_union_without_any() {
    let union = Type::union(vec![Type::int(), Type::string()]);
    let simplified = union.simplify();
    match simplified {
        Type::Union(types) => {
            assert_eq!(types.len(), 2);
            assert!(types.contains(&Type::int()));
            assert!(types.contains(&Type::string()));
        }
        _ => panic!("Expected union type"),
    }
}

#[test]
fn test_simplify_single_element_union() {
    let union = Type::union(vec![Type::int()]);
    let simplified = union.simplify();
    assert_eq!(simplified, Type::int());
}

#[test]
fn test_simplify_non_union_types() {
    assert_eq!(Type::int().simplify(), Type::int());
    assert_eq!(Type::string().simplify(), Type::string());
    assert_eq!(Type::any().simplify(), Type::any());
}

#[test]
fn test_simplify_nested_types() {
    let fun_type = Type::fun_unnamed(
        vec![Type::union(vec![Type::int(), Type::any()])],
        Type::union(vec![Type::string(), Type::bool()]),
    );
    let simplified = fun_type.simplify();

    match simplified {
        Type::Fun(args, ret) => {
            assert_eq!(args[0].1, Type::any());
            match ret.as_ref() {
                Type::Union(types) => {
                    assert_eq!(types.len(), 2);
                }
                _ => panic!("Expected union return type"),
            }
        }
        _ => panic!("Expected function type"),
    }
}

#[test]
fn test_simplify_list_of_union_with_any() {
    let list_type = Type::list(Type::union(vec![Type::int(), Type::any()]));
    let simplified = list_type.simplify();

    match simplified {
        Type::App(_, elem) => {
            assert_eq!(*elem, Type::any());
        }
        _ => panic!("Expected list type"),
    }
}

#[test]
fn test_unapply_list() {
    let list_int = Type::list(Type::int());
    let result = list_int.unapply();

    assert!(result.is_some());
    let (ctor, args) = result.unwrap();
    assert_eq!(*ctor, TypeCtor::List);
    assert_eq!(args.len(), 1);
    assert_eq!(args[0], Type::int());
}

#[test]
fn test_unapply_dict() {
    let dict_str_int = Type::App(
        Box::new(Type::App(Box::new(Type::Con(TypeCtor::Dict)), Box::new(Type::string()))),
        Box::new(Type::int()),
    );

    let result = dict_str_int.unapply();
    assert!(result.is_some());
    let (ctor, args) = result.unwrap();
    assert_eq!(*ctor, TypeCtor::Dict);
    assert_eq!(args.len(), 2);
    assert_eq!(args[0], Type::string());
    assert_eq!(args[1], Type::int());
}

#[test]
fn test_unapply_set() {
    let set_str = Type::App(Box::new(Type::Con(TypeCtor::Set)), Box::new(Type::string()));

    let result = set_str.unapply();
    assert!(result.is_some());
    let (ctor, args) = result.unwrap();
    assert_eq!(*ctor, TypeCtor::Set);
    assert_eq!(args.len(), 1);
    assert_eq!(args[0], Type::string());
}

#[test]
fn test_unapply_non_app() {
    let int_type = Type::int();
    let result = int_type.unapply();
    assert!(result.is_none());

    let var_type = Type::Var(TypeVar::new(0));
    let result2 = var_type.unapply();
    assert!(result2.is_none());
}

#[test]
fn test_unapply_class() {
    let generic_class = Type::App(
        Box::new(Type::Con(TypeCtor::Class("MyGeneric".to_string()))),
        Box::new(Type::int()),
    );

    let result = generic_class.unapply_class();
    assert!(result.is_some());
    let (class_name, args) = result.unwrap();
    assert_eq!(class_name, "MyGeneric");
    assert_eq!(args.len(), 1);
    assert_eq!(args[0], Type::int());
}

#[test]
fn test_unapply_class_two_params() {
    let generic_class = Type::App(
        Box::new(Type::App(
            Box::new(Type::Con(TypeCtor::Class("Pair".to_string()))),
            Box::new(Type::string()),
        )),
        Box::new(Type::int()),
    );

    let result = generic_class.unapply_class();
    assert!(result.is_some());
    let (class_name, args) = result.unwrap();
    assert_eq!(class_name, "Pair");
    assert_eq!(args.len(), 2);
    assert_eq!(args[0], Type::string());
    assert_eq!(args[1], Type::int());
}

#[test]
fn test_unapply_class_non_class() {
    let list_int = Type::list(Type::int());
    let result = list_int.unapply_class();
    assert!(result.is_none());
}

#[test]
fn test_unapply_protocol_single_param() {
    let protocol_str = Type::App(
        Box::new(Type::Con(TypeCtor::Protocol(
            Some("ReadOnly".to_string()),
            vec![Variance::Covariant],
        ))),
        Box::new(Type::string()),
    );

    let result = protocol_str.unapply_protocol();
    assert!(result.is_some());
    let (protocol_name, args) = result.unwrap();
    assert_eq!(protocol_name, "ReadOnly");
    assert_eq!(args.len(), 1);
    assert_eq!(args[0], Type::string());
}

#[test]
fn test_unapply_protocol_two_params() {
    let protocol_two_param = Type::App(
        Box::new(Type::App(
            Box::new(Type::Con(TypeCtor::Protocol(
                Some("TwoParam".to_string()),
                vec![Variance::Covariant, Variance::Contravariant],
            ))),
            Box::new(Type::int()),
        )),
        Box::new(Type::string()),
    );

    let result = protocol_two_param.unapply_protocol();
    assert!(result.is_some());
    let (protocol_name, args) = result.unwrap();
    assert_eq!(protocol_name, "TwoParam");
    assert_eq!(args.len(), 2);
    assert_eq!(args[0], Type::int());
    assert_eq!(args[1], Type::string());
}

#[test]
fn test_unapply_protocol_non_protocol() {
    let class_int = Type::App(
        Box::new(Type::Con(TypeCtor::Class("MyClass".to_string()))),
        Box::new(Type::int()),
    );
    let result = class_int.unapply_protocol();
    assert!(result.is_none());

    let list_int = Type::list(Type::int());
    let result2 = list_int.unapply_protocol();
    assert!(result2.is_none());
}

#[test]
fn test_unapply_protocol_unnamed() {
    let protocol_unnamed = Type::App(
        Box::new(Type::Con(TypeCtor::Protocol(None, vec![]))),
        Box::new(Type::int()),
    );
    let result = protocol_unnamed.unapply_protocol();
    assert!(result.is_none());
}

#[test]
fn test_union_simplification_eliminates_never() {
    let union = Type::union(vec![Type::never(), Type::int()]);
    assert_eq!(union, Type::int());

    let union = Type::union(vec![Type::int(), Type::never(), Type::string()]);
    assert_eq!(union, Type::union(vec![Type::int(), Type::string()]));
}

#[test]
fn test_union_simplification_eliminates_subtypes_with_any() {
    let union = Type::union(vec![Type::int(), Type::any()]);
    assert_eq!(union, Type::any());

    let union = Type::union(vec![Type::string(), Type::int(), Type::any()]);
    assert_eq!(union, Type::any());
}

#[test]
fn test_union_simplification_preserves_non_subtype_relations() {
    let union = Type::union(vec![Type::int(), Type::string()]);
    match union {
        Type::Union(types) => {
            assert_eq!(types.len(), 2);
            assert!(types.contains(&Type::int()));
            assert!(types.contains(&Type::string()));
        }
        _ => panic!("Expected Union type"),
    }
}

#[test]
fn test_union_simplification_with_duplicates() {
    let union = Type::union(vec![Type::int(), Type::int(), Type::string(), Type::string()]);
    match union {
        Type::Union(types) => {
            assert_eq!(types.len(), 2);
            assert!(types.contains(&Type::int()));
            assert!(types.contains(&Type::string()));
        }
        _ => panic!("Expected Union type"),
    }
}

#[test]
fn test_union_simplification_flattens_nested() {
    let inner1 = Type::union(vec![Type::int(), Type::string()]);
    let inner2 = Type::union(vec![Type::bool(), Type::float()]);
    let union = Type::union(vec![inner1, inner2]);

    match union {
        Type::Union(types) => {
            assert_eq!(types.len(), 4);
            assert!(types.contains(&Type::int()));
            assert!(types.contains(&Type::string()));
            assert!(types.contains(&Type::bool()));
            assert!(types.contains(&Type::float()));
        }
        _ => panic!("Expected Union type"),
    }
}

#[test]
fn test_union_simplification_complex_case() {
    let inner1 = Type::union(vec![Type::int(), Type::never()]);
    let inner2 = Type::union(vec![Type::string(), Type::int()]);
    let union = Type::union(vec![inner1, inner2]);

    match union {
        Type::Union(types) => {
            assert_eq!(types.len(), 2);
            assert!(types.contains(&Type::int()));
            assert!(types.contains(&Type::string()));
        }
        _ => panic!("Expected Union type"),
    }
}

#[test]
fn test_union_single_type_after_simplification() {
    let union = Type::union(vec![Type::never(), Type::int()]);
    assert_eq!(union, Type::int());
}

#[test]
fn test_subtype_dict_invariant_both_params() {
    let dict_str_int = Type::dict(Type::string(), Type::int());
    let dict_str_str = Type::dict(Type::string(), Type::string());
    let dict_int_int = Type::dict(Type::int(), Type::int());

    assert!(dict_str_int.is_subtype_of(&dict_str_int));

    assert!(!dict_str_int.is_subtype_of(&dict_str_str));

    assert!(!dict_str_int.is_subtype_of(&dict_int_int));
}

#[test]
fn test_subtype_generator_with_same_types() {
    let gen_type = Type::generator(Type::int(), Type::string(), Type::bool());
    assert!(gen_type.is_subtype_of(&gen_type));
}

#[test]
fn test_subtype_generator_different_params() {
    let gen1 = Type::generator(Type::int(), Type::string(), Type::bool());
    let gen2 = Type::generator(Type::string(), Type::string(), Type::bool());

    assert!(!gen1.is_subtype_of(&gen2));
    assert!(!gen2.is_subtype_of(&gen1));
}

#[test]
fn test_get_app_variance_single_param() {
    let list_int = Type::list(Type::int());
    let variance = Type::get_app_variance(&list_int);
    assert_eq!(variance, Variance::Invariant);
}

#[test]
fn test_get_app_variance_dict_first_param() {
    let dict_str_int = Type::dict(Type::string(), Type::int());
    let variance = Type::get_app_variance(&dict_str_int);
    assert_eq!(variance, Variance::Invariant);
}

#[test]
fn test_get_app_variance_generator_params() {
    let gen_type = Type::generator(Type::int(), Type::string(), Type::bool());

    let variance = Type::get_app_variance(&gen_type);
    assert_eq!(variance, Variance::Covariant);
}

#[test]
fn test_subtype_nested_list_hierarchy() {
    let nested_list = Type::list(Type::list(Type::list(Type::int())));
    assert!(nested_list.is_subtype_of(&nested_list));

    let nested_list_str = Type::list(Type::list(Type::list(Type::string())));
    assert!(!nested_list.is_subtype_of(&nested_list_str));
}

#[test]
fn test_protocol_variance_covariant() {
    let mut registry = ClassRegistry::new();
    let mut metadata = ClassMetadata::new("ReadOnly".to_string());
    metadata.set_protocol(true);
    metadata.set_type_param_vars(vec![TypeVar::with_variance(
        0,
        Some("T_co".to_string()),
        Variance::Covariant,
    )]);
    registry.register_class("ReadOnly".to_string(), metadata);

    let readonly_proto = Type::Con(TypeCtor::Protocol(Some("ReadOnly".to_string()), vec![]));
    let readonly_int = Type::App(Box::new(readonly_proto.clone()), Box::new(Type::int()));

    let enriched = readonly_int.enrich_protocol_variance(&registry);

    match enriched {
        Type::App(ctor, _) => {
            if let Type::Con(TypeCtor::Protocol(_, variances)) = *ctor {
                assert_eq!(variances.len(), 1);
                assert_eq!(variances[0], Variance::Covariant);
            } else {
                panic!("Expected Protocol constructor");
            }
        }
        _ => panic!("Expected App type"),
    }
}

#[test]
fn test_protocol_variance_contravariant() {
    let mut registry = ClassRegistry::new();
    let mut metadata = ClassMetadata::new("Consumer".to_string());
    metadata.set_protocol(true);
    metadata.set_type_param_vars(vec![TypeVar::with_variance(
        0,
        Some("T_contra".to_string()),
        Variance::Contravariant,
    )]);
    registry.register_class("Consumer".to_string(), metadata);

    let protocol_type = Type::Con(TypeCtor::Protocol(Some("Consumer".to_string()), vec![]));
    let consumer = Type::App(Box::new(protocol_type), Box::new(Type::int()));

    let enriched = consumer.enrich_protocol_variance(&registry);

    match enriched {
        Type::App(ctor, _) => {
            if let Type::Con(TypeCtor::Protocol(_, variances)) = *ctor {
                assert_eq!(variances.len(), 1);
                assert_eq!(variances[0], Variance::Contravariant);
            } else {
                panic!("Expected Protocol constructor");
            }
        }
        _ => panic!("Expected App type"),
    }
}

#[test]
fn test_protocol_multi_param_variance() {
    let mut registry = ClassRegistry::new();
    let mut metadata = ClassMetadata::new("BiVariant".to_string());
    metadata.set_protocol(true);
    metadata.set_type_param_vars(vec![
        TypeVar::with_variance(0, Some("T_co".to_string()), Variance::Covariant),
        TypeVar::with_variance(1, Some("U_contra".to_string()), Variance::Contravariant),
    ]);
    registry.register_class("BiVariant".to_string(), metadata);

    let protocol_type = Type::Con(TypeCtor::Protocol(Some("BiVariant".to_string()), vec![]));
    let bivariant = Type::App(
        Box::new(Type::App(Box::new(protocol_type), Box::new(Type::int()))),
        Box::new(Type::string()),
    );

    let enriched = bivariant.enrich_protocol_variance(&registry);

    if let Type::App(outer_app, _) = enriched {
        if let Type::App(inner_ctor, _) = *outer_app {
            if let Type::Con(TypeCtor::Protocol(_, variances)) = *inner_ctor {
                assert_eq!(variances.len(), 2);
                assert_eq!(variances[0], Variance::Covariant);
                assert_eq!(variances[1], Variance::Contravariant);
            } else {
                panic!("Expected Protocol constructor");
            }
        } else {
            panic!("Expected nested App");
        }
    } else {
        panic!("Expected App type");
    }
}

#[test]
fn test_protocol_variance_enrichment_in_union() {
    let mut registry = ClassRegistry::new();
    let mut metadata = ClassMetadata::new("MyProto".to_string());
    metadata.set_protocol(true);
    metadata.set_type_param_vars(vec![TypeVar::with_variance(
        0,
        Some("T".to_string()),
        Variance::Covariant,
    )]);
    registry.register_class("MyProto".to_string(), metadata);

    let proto = Type::Con(TypeCtor::Protocol(Some("MyProto".to_string()), vec![]));
    let proto_int = Type::App(Box::new(proto.clone()), Box::new(Type::int()));
    let proto_str = Type::App(Box::new(proto.clone()), Box::new(Type::string()));
    let union = Type::union(vec![proto_int, proto_str]);

    let enriched = union.enrich_protocol_variance(&registry);

    match enriched {
        Type::Union(types) => {
            assert_eq!(types.len(), 2);
            for ty in types {
                if let Type::App(ctor, _) = ty {
                    if let Type::Con(TypeCtor::Protocol(_, variances)) = *ctor {
                        assert_eq!(variances.len(), 1);
                        assert_eq!(variances[0], Variance::Covariant);
                    } else {
                        panic!("Expected Protocol in union");
                    }
                } else {
                    panic!("Expected App in union");
                }
            }
        }
        _ => panic!("Expected Union type"),
    }
}

use beacon_core::{ClassMetadata, MethodType, OverloadSet, Type};

#[test]
fn test_overloaded_method_without_implementation() {
    let mut metadata = ClassMetadata::new("TestClass".to_string());

    let overload_set = OverloadSet {
        signatures: vec![
            Type::fun(vec![("self".to_string(), Type::any())], Type::string()),
            Type::fun(
                vec![("self".to_string(), Type::any())],
                Type::Con(beacon_core::TypeCtor::Class("LiteralString".to_string())),
            ),
        ],
        implementation: None,
    };

    metadata.add_overloaded_method("upper".to_string(), overload_set);

    let method = metadata.lookup_method("upper");
    assert!(method.is_some(), "Method should be found even without implementation");

    let method_type = method.unwrap();
    assert!(matches!(method_type, Type::Fun(_, _)), "Should return a function type");
}

#[test]
fn test_overloaded_method_with_implementation() {
    let mut metadata = ClassMetadata::new("TestClass".to_string());

    let impl_sig = Type::fun(
        vec![("self".to_string(), Type::any()), ("key".to_string(), Type::string())],
        Type::int(),
    );

    let overload_set = OverloadSet {
        signatures: vec![
            Type::fun(vec![("self".to_string(), Type::any())], Type::int()),
            Type::fun(
                vec![("self".to_string(), Type::any()), ("key".to_string(), Type::int())],
                Type::string(),
            ),
        ],
        implementation: Some(impl_sig.clone()),
    };

    metadata.add_overloaded_method("get".to_string(), overload_set);

    let method = metadata.lookup_method("get");
    assert!(method.is_some());

    let method_type = method.unwrap();
    assert_eq!(method_type, &impl_sig, "Should return the implementation signature");
}

#[test]
fn test_primary_type_for_overloaded_without_implementation() {
    let overload_set = OverloadSet {
        signatures: vec![
            Type::fun(vec![("self".to_string(), Type::any())], Type::int()),
            Type::fun(vec![("self".to_string(), Type::any())], Type::string()),
        ],
        implementation: None,
    };

    let method_type = MethodType::Overloaded(overload_set);

    let primary = method_type.primary_type();
    assert!(
        primary.is_some(),
        "Should return first signature when no implementation"
    );

    if let Some(ty) = primary {
        assert!(matches!(ty, Type::Fun(_, _)), "Should be a function type");
    }
}

#[test]
fn test_primary_type_for_single_method() {
    let single_sig = Type::fun(vec![("self".to_string(), Type::any())], Type::int());
    let method_type = MethodType::Single(single_sig.clone());

    let primary = method_type.primary_type();
    assert!(primary.is_some());
    assert_eq!(primary.unwrap(), &single_sig);
}

#[test]
fn test_lookup_attribute_finds_overloaded_method_without_implementation() {
    let mut metadata = ClassMetadata::new("str".to_string());

    let overload_set = OverloadSet {
        signatures: vec![Type::fun(vec![("self".to_string(), Type::any())], Type::string())],
        implementation: None,
    };

    metadata.add_overloaded_method("upper".to_string(), overload_set);

    let attr = metadata.lookup_attribute("upper");
    assert!(attr.is_some(), "Should find overloaded method without implementation");
}

#[test]
fn test_is_method_recognizes_overloaded_methods() {
    let mut metadata = ClassMetadata::new("TestClass".to_string());

    let overload_set = OverloadSet {
        signatures: vec![Type::fun(vec![("self".to_string(), Type::any())], Type::int())],
        implementation: None,
    };

    metadata.add_overloaded_method("method".to_string(), overload_set);

    assert!(
        metadata.is_method("method"),
        "Should recognize overloaded method as a method"
    );
}

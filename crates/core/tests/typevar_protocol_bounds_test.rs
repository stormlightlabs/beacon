use beacon_core::{ClassMetadata, ClassRegistry, Type, TypeCtor, TypeVar, TypeVarConstraintRegistry, Variance};

#[test]
fn test_protocol_bound_validation_skipped_for_generator() {
    let mut registry = TypeVarConstraintRegistry::new();

    let generator_bound = Type::Con(TypeCtor::Generator);
    let tv = TypeVar::with_variance(1, Some("T".to_string()), Variance::Invariant);
    registry.set_bound(tv.id, generator_bound);

    let some_type = Type::Con(TypeCtor::Class("SomeClass".to_string()));
    assert!(registry.validate_bound(tv.id, &some_type).is_ok());
}

#[test]
fn test_protocol_bound_validation_skipped_for_iterable() {
    let mut registry = TypeVarConstraintRegistry::new();

    let iterable_bound = Type::Con(TypeCtor::Iterable);
    let tv = TypeVar::with_variance(2, Some("T".to_string()), Variance::Invariant);
    registry.set_bound(tv.id, iterable_bound);

    let list_type = Type::list(Type::int());
    assert!(registry.validate_bound(tv.id, &list_type).is_ok());
}

#[test]
fn test_protocol_bound_validation_skipped_for_supports_next() {
    let mut registry = TypeVarConstraintRegistry::new();

    let supports_next = Type::Con(TypeCtor::Class("SupportsNext".to_string()));
    let tv = TypeVar::with_variance(3, Some("T".to_string()), Variance::Invariant);
    registry.set_bound(tv.id, supports_next);

    let iterator_type = Type::Con(TypeCtor::Class("Iterator".to_string()));
    assert!(registry.validate_bound(tv.id, &iterator_type).is_ok());
}

#[test]
fn test_protocol_bound_validation_skipped_for_supports_add() {
    let mut registry = TypeVarConstraintRegistry::new();

    let supports_add = Type::Con(TypeCtor::Class("SupportsAdd".to_string()));
    let tv = TypeVar::with_variance(4, Some("T".to_string()), Variance::Invariant);
    registry.set_bound(tv.id, supports_add);

    assert!(registry.validate_bound(tv.id, &Type::int()).is_ok());

    assert!(registry.validate_bound(tv.id, &Type::string()).is_ok());
}

#[test]
fn test_non_protocol_bound_validation_still_enforced() {
    let mut registry = TypeVarConstraintRegistry::new();

    let animal_bound = Type::Con(TypeCtor::Class("Animal".to_string()));
    let tv = TypeVar::with_variance(5, Some("T".to_string()), Variance::Invariant);
    registry.set_bound(tv.id, animal_bound.clone());

    assert!(registry.validate_bound(tv.id, &animal_bound).is_ok());

    let result = registry.validate_bound(tv.id, &Type::int());
    assert!(result.is_err());
}

#[test]
fn test_user_defined_protocol_bound_validation_skipped() {
    let mut registry = TypeVarConstraintRegistry::new();

    let protocol_bound = Type::Con(TypeCtor::Protocol(Some("MyProtocol".to_string()), vec![]));
    let tv = TypeVar::with_variance(6, Some("T".to_string()), Variance::Invariant);
    registry.set_bound(tv.id, protocol_bound);

    let some_type = Type::Con(TypeCtor::Class("MyClass".to_string()));
    assert!(registry.validate_bound(tv.id, &some_type).is_ok());
}

#[test]
fn test_generic_protocol_bound_validation_skipped() {
    let mut registry = TypeVarConstraintRegistry::new();

    let iterable_int = Type::App(Box::new(Type::Con(TypeCtor::Iterable)), Box::new(Type::int()));
    let tv = TypeVar::with_variance(7, Some("T".to_string()), Variance::Invariant);
    registry.set_bound(tv.id, iterable_int);

    let list_int = Type::list(Type::int());
    assert!(registry.validate_bound(tv.id, &list_int).is_ok());
}

#[test]
fn test_any_satisfies_protocol_bound() {
    let mut registry = TypeVarConstraintRegistry::new();
    let tv = TypeVar::with_variance(10, Some("T".to_string()), Variance::Invariant);
    registry.set_bound(tv.id, Type::Con(TypeCtor::Protocol(Some("Proto".to_string()), vec![])));

    assert!(
        registry
            .validate_bound_with_class_registry(tv.id, &Type::any(), None)
            .is_ok()
    );
}

#[test]
fn test_protocol_bound_validation_with_class_registry_success() {
    let mut registry = TypeVarConstraintRegistry::new();
    let mut class_registry = ClassRegistry::new();

    let mut readable = ClassMetadata::new("Readable".to_string());
    readable.set_protocol(true);
    readable.add_method("read".to_string(), Type::fun(vec![], Type::string()));
    class_registry.register_class("Readable".to_string(), readable);

    let mut impl_class = ClassMetadata::new("FileReader".to_string());
    impl_class.add_method("read".to_string(), Type::fun(vec![], Type::string()));
    class_registry.register_class("FileReader".to_string(), impl_class);

    let tv = TypeVar::with_variance(8, Some("T".to_string()), Variance::Invariant);
    registry.set_bound(tv.id, Type::Con(TypeCtor::Class("Readable".to_string())));

    let ty = Type::Con(TypeCtor::Class("FileReader".to_string()));
    assert!(
        registry
            .validate_bound_with_class_registry(tv.id, &ty, Some(&class_registry))
            .is_ok()
    );
}

#[test]
fn test_protocol_bound_validation_with_class_registry_failure() {
    let mut registry = TypeVarConstraintRegistry::new();
    let mut class_registry = ClassRegistry::new();

    let mut readable = ClassMetadata::new("Readable".to_string());
    readable.set_protocol(true);
    readable.add_method("read".to_string(), Type::fun(vec![], Type::string()));
    class_registry.register_class("Readable".to_string(), readable);

    let mut impl_class = ClassMetadata::new("IncompleteReader".to_string());
    impl_class.add_method("close".to_string(), Type::fun(vec![], Type::none()));
    class_registry.register_class("IncompleteReader".to_string(), impl_class);

    let tv = TypeVar::with_variance(9, Some("T".to_string()), Variance::Invariant);
    registry.set_bound(tv.id, Type::Con(TypeCtor::Class("Readable".to_string())));

    let ty = Type::Con(TypeCtor::Class("IncompleteReader".to_string()));
    assert!(
        registry
            .validate_bound_with_class_registry(tv.id, &ty, Some(&class_registry))
            .is_err()
    );
}

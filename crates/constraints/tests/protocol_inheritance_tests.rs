//! Protocol Inheritance and Composition Edge Case Tests
//!
//! Tests that verify protocol inheritance works correctly, including:
//! - Transitive protocol inheritance (Protocol A extends Protocol B)
//! - Protocol inheritance chains (A extends B extends C)
//! - Multiple protocol inheritance (A extends B and C)
//! - Classes implementing protocols with base protocols
//! - Intersection types with protocols
//!
//! ## Edge Cases Covered
//!
//! 1. **Transitive Inheritance**: When Protocol1 extends Protocol2, a class implementing
//!    Protocol1 must also satisfy Protocol2's method requirements.
//! 2. **Inheritance Chains**: Protocol inheritance through multiple levels.
//! 3. **Multiple Inheritance**: A protocol extending multiple base protocols.
//! 4. **Circular Prevention**: Graceful handling of circular protocol inheritance.
//!
//! ## Implementation
//!
//! The constraint solver's `check_user_defined_protocol()` function recursively checks base protocols to ensure transitive satisfaction

use beacon_constraint::solver::solve_constraints;
use beacon_constraint::{Constraint, ConstraintSet, Span};
use beacon_core::{ClassMetadata, ClassRegistry, Type, TypeCtor, TypeVarConstraintRegistry};

fn test_span() -> Span {
    Span::with_end(1, 0, 1, 10)
}

/// Test that a class implementing a protocol that extends another protocol must satisfy both protocols' method requirements
#[test]
fn test_protocol_extends_another_protocol() {
    let mut class_registry = ClassRegistry::new();
    let typevar_registry = TypeVarConstraintRegistry::new();

    let mut readable_proto = ClassMetadata::new("Readable".to_string());
    readable_proto.set_protocol(true);
    readable_proto.add_method("get".to_string(), Type::fun(vec![], Type::string()));
    class_registry.register_class("Readable".to_string(), readable_proto);

    let mut writable_proto = ClassMetadata::new("Writable".to_string());
    writable_proto.set_protocol(true);
    writable_proto.add_base_class("Readable".to_string());
    writable_proto.add_method(
        "set".to_string(),
        Type::fun(vec![("value".to_string(), Type::string())], Type::none()),
    );
    class_registry.register_class("Writable".to_string(), writable_proto);

    let mut storage_class = ClassMetadata::new("Storage".to_string());
    storage_class.add_method("get".to_string(), Type::fun(vec![], Type::string()));
    storage_class.add_method(
        "set".to_string(),
        Type::fun(vec![("value".to_string(), Type::string())], Type::none()),
    );
    class_registry.register_class("Storage".to_string(), storage_class);

    let storage_ty = Type::Con(TypeCtor::Class("Storage".to_string()));
    let writable_ty = Type::Con(TypeCtor::Protocol(Some("Writable".to_string()), vec![]));

    let constraints =
        ConstraintSet { constraints: vec![Constraint::Equal(storage_ty.clone(), writable_ty, test_span())] };

    let result = solve_constraints(constraints, &class_registry, &typevar_registry);
    assert!(result.is_ok(), "Solver should complete");

    let (_, errors) = result.unwrap();
    assert!(
        errors.is_empty(),
        "Storage should satisfy Writable (which extends Readable): {errors:?}"
    );
}

/// Test that a class missing base protocol methods fails to satisfy derived protocol
#[test]
fn test_protocol_inheritance_missing_base_method() {
    let mut class_registry = ClassRegistry::new();
    let typevar_registry = TypeVarConstraintRegistry::new();

    let mut readable_proto = ClassMetadata::new("Readable".to_string());
    readable_proto.set_protocol(true);
    readable_proto.add_method("get".to_string(), Type::fun(vec![], Type::string()));
    class_registry.register_class("Readable".to_string(), readable_proto);

    let mut writable_proto = ClassMetadata::new("Writable".to_string());
    writable_proto.set_protocol(true);
    writable_proto.add_base_class("Readable".to_string());
    writable_proto.add_method(
        "set".to_string(),
        Type::fun(vec![("value".to_string(), Type::string())], Type::none()),
    );
    class_registry.register_class("Writable".to_string(), writable_proto);

    let mut incomplete_class = ClassMetadata::new("IncompleteStorage".to_string());
    incomplete_class.add_method(
        "set".to_string(),
        Type::fun(vec![("value".to_string(), Type::string())], Type::none()),
    );
    class_registry.register_class("IncompleteStorage".to_string(), incomplete_class);

    let incomplete_ty = Type::Con(TypeCtor::Class("IncompleteStorage".to_string()));
    let writable_ty = Type::Con(TypeCtor::Protocol(Some("Writable".to_string()), vec![]));

    let constraints =
        ConstraintSet { constraints: vec![Constraint::Equal(incomplete_ty.clone(), writable_ty, test_span())] };

    let result = solve_constraints(constraints, &class_registry, &typevar_registry);
    assert!(result.is_ok(), "Solver should complete");

    let (_, errors) = result.unwrap();
    assert!(
        !errors.is_empty(),
        "IncompleteStorage should fail to satisfy Writable (missing base protocol method get)"
    );
}

/// Test protocol inheritance chain: A extends B extends C
#[test]
fn test_protocol_inheritance_chain() {
    let mut class_registry = ClassRegistry::new();
    let typevar_registry = TypeVarConstraintRegistry::new();

    let mut proto_a = ClassMetadata::new("ProtoA".to_string());
    proto_a.set_protocol(true);
    proto_a.add_method("method_a".to_string(), Type::fun(vec![], Type::int()));
    class_registry.register_class("ProtoA".to_string(), proto_a);

    let mut proto_b = ClassMetadata::new("ProtoB".to_string());
    proto_b.set_protocol(true);
    proto_b.add_base_class("ProtoA".to_string());
    proto_b.add_method("method_b".to_string(), Type::fun(vec![], Type::string()));
    class_registry.register_class("ProtoB".to_string(), proto_b);

    let mut proto_c = ClassMetadata::new("ProtoC".to_string());
    proto_c.set_protocol(true);
    proto_c.add_base_class("ProtoB".to_string());
    proto_c.add_method("method_c".to_string(), Type::fun(vec![], Type::bool()));
    class_registry.register_class("ProtoC".to_string(), proto_c);

    let mut impl_class = ClassMetadata::new("FullImpl".to_string());
    impl_class.add_method("method_a".to_string(), Type::fun(vec![], Type::int()));
    impl_class.add_method("method_b".to_string(), Type::fun(vec![], Type::string()));
    impl_class.add_method("method_c".to_string(), Type::fun(vec![], Type::bool()));
    class_registry.register_class("FullImpl".to_string(), impl_class);

    let impl_ty = Type::Con(TypeCtor::Class("FullImpl".to_string()));
    let proto_c_ty = Type::Con(TypeCtor::Protocol(Some("ProtoC".to_string()), vec![]));

    let constraints = ConstraintSet { constraints: vec![Constraint::Equal(impl_ty.clone(), proto_c_ty, test_span())] };

    let result = solve_constraints(constraints, &class_registry, &typevar_registry);
    assert!(result.is_ok(), "Solver should complete");

    let (_, errors) = result.unwrap();
    assert!(
        errors.is_empty(),
        "FullImpl should satisfy ProtoC (entire inheritance chain): {errors:?}"
    );
}

/// Test protocol inheritance chain failure when missing middle protocol method
#[test]
fn test_protocol_inheritance_chain_missing_middle() {
    let mut class_registry = ClassRegistry::new();
    let typevar_registry = TypeVarConstraintRegistry::new();

    let mut proto_a = ClassMetadata::new("ProtoA".to_string());
    proto_a.set_protocol(true);
    proto_a.add_method("method_a".to_string(), Type::fun(vec![], Type::int()));
    class_registry.register_class("ProtoA".to_string(), proto_a);

    let mut proto_b = ClassMetadata::new("ProtoB".to_string());
    proto_b.set_protocol(true);
    proto_b.add_base_class("ProtoA".to_string());
    proto_b.add_method("method_b".to_string(), Type::fun(vec![], Type::string()));
    class_registry.register_class("ProtoB".to_string(), proto_b);

    let mut proto_c = ClassMetadata::new("ProtoC".to_string());
    proto_c.set_protocol(true);
    proto_c.add_base_class("ProtoB".to_string());
    proto_c.add_method("method_c".to_string(), Type::fun(vec![], Type::bool()));
    class_registry.register_class("ProtoC".to_string(), proto_c);

    let mut partial_impl = ClassMetadata::new("PartialImpl".to_string());
    partial_impl.add_method("method_a".to_string(), Type::fun(vec![], Type::int()));
    partial_impl.add_method("method_c".to_string(), Type::fun(vec![], Type::bool()));
    class_registry.register_class("PartialImpl".to_string(), partial_impl);

    let impl_ty = Type::Con(TypeCtor::Class("PartialImpl".to_string()));
    let proto_c_ty = Type::Con(TypeCtor::Protocol(Some("ProtoC".to_string()), vec![]));

    let constraints = ConstraintSet { constraints: vec![Constraint::Equal(impl_ty.clone(), proto_c_ty, test_span())] };

    let result = solve_constraints(constraints, &class_registry, &typevar_registry);
    assert!(result.is_ok(), "Solver should complete");

    let (_, errors) = result.unwrap();
    assert!(
        !errors.is_empty(),
        "PartialImpl should fail (missing method_b from middle of chain)"
    );
}

/// Test multiple protocol inheritance: Protocol extends multiple base protocols
#[test]
fn test_multiple_protocol_inheritance() {
    let mut class_registry = ClassRegistry::new();
    let typevar_registry = TypeVarConstraintRegistry::new();

    let mut readable = ClassMetadata::new("Readable".to_string());
    readable.set_protocol(true);
    readable.add_method("read".to_string(), Type::fun(vec![], Type::string()));
    class_registry.register_class("Readable".to_string(), readable);

    let mut closeable = ClassMetadata::new("Closeable".to_string());
    closeable.set_protocol(true);
    closeable.add_method("close".to_string(), Type::fun(vec![], Type::none()));
    class_registry.register_class("Closeable".to_string(), closeable);

    let mut resource = ClassMetadata::new("Resource".to_string());
    resource.set_protocol(true);
    resource.add_base_class("Readable".to_string());
    resource.add_base_class("Closeable".to_string());
    resource.add_method("name".to_string(), Type::fun(vec![], Type::string()));
    class_registry.register_class("Resource".to_string(), resource);

    let mut file_class = ClassMetadata::new("File".to_string());
    file_class.add_method("read".to_string(), Type::fun(vec![], Type::string()));
    file_class.add_method("close".to_string(), Type::fun(vec![], Type::none()));
    file_class.add_method("name".to_string(), Type::fun(vec![], Type::string()));
    class_registry.register_class("File".to_string(), file_class);

    let file_ty = Type::Con(TypeCtor::Class("File".to_string()));
    let resource_ty = Type::Con(TypeCtor::Protocol(Some("Resource".to_string()), vec![]));

    let constraints = ConstraintSet { constraints: vec![Constraint::Equal(file_ty.clone(), resource_ty, test_span())] };

    let result = solve_constraints(constraints, &class_registry, &typevar_registry);
    assert!(result.is_ok(), "Solver should complete");

    let (_, errors) = result.unwrap();
    assert!(
        errors.is_empty(),
        "File should satisfy Resource (multiple base protocols): {errors:?}"
    );
}

/// Test that circular protocol inheritance is handled gracefully
#[test]
fn test_circular_protocol_inheritance_handled() {
    let mut class_registry = ClassRegistry::new();
    let typevar_registry = TypeVarConstraintRegistry::new();

    let mut proto_a = ClassMetadata::new("ProtoA".to_string());
    proto_a.set_protocol(true);
    proto_a.add_base_class("ProtoB".to_string());
    proto_a.add_method("method_a".to_string(), Type::fun(vec![], Type::int()));
    class_registry.register_class("ProtoA".to_string(), proto_a);

    let mut proto_b = ClassMetadata::new("ProtoB".to_string());
    proto_b.set_protocol(true);
    proto_b.add_base_class("ProtoA".to_string());
    proto_b.add_method("method_b".to_string(), Type::fun(vec![], Type::string()));
    class_registry.register_class("ProtoB".to_string(), proto_b);

    let mut impl_class = ClassMetadata::new("Impl".to_string());
    impl_class.add_method("method_a".to_string(), Type::fun(vec![], Type::int()));
    impl_class.add_method("method_b".to_string(), Type::fun(vec![], Type::string()));
    class_registry.register_class("Impl".to_string(), impl_class);

    let impl_ty = Type::Con(TypeCtor::Class("Impl".to_string()));
    let proto_a_ty = Type::Con(TypeCtor::Protocol(Some("ProtoA".to_string()), vec![]));

    let constraints = ConstraintSet { constraints: vec![Constraint::Equal(impl_ty.clone(), proto_a_ty, test_span())] };
    let _ = solve_constraints(constraints, &class_registry, &typevar_registry);
}

/// Test intersection type with protocols
#[test]
fn test_intersection_with_protocol_inheritance() {
    let mut class_registry = ClassRegistry::new();
    let typevar_registry = TypeVarConstraintRegistry::new();

    let mut readable = ClassMetadata::new("Readable".to_string());
    readable.set_protocol(true);
    readable.add_method("read".to_string(), Type::fun(vec![], Type::string()));
    class_registry.register_class("Readable".to_string(), readable);

    let mut buffered = ClassMetadata::new("Buffered".to_string());
    buffered.set_protocol(true);
    buffered.add_base_class("Readable".to_string());
    buffered.add_method("flush".to_string(), Type::fun(vec![], Type::none()));
    class_registry.register_class("Buffered".to_string(), buffered);

    let mut stream_class = ClassMetadata::new("Stream".to_string());
    stream_class.add_method("read".to_string(), Type::fun(vec![], Type::string()));
    stream_class.add_method("flush".to_string(), Type::fun(vec![], Type::none()));
    class_registry.register_class("Stream".to_string(), stream_class);

    let stream_ty = Type::Con(TypeCtor::Class("Stream".to_string()));
    let buffered_ty = Type::Con(TypeCtor::Protocol(Some("Buffered".to_string()), vec![]));

    let constraints =
        ConstraintSet { constraints: vec![Constraint::Equal(stream_ty.clone(), buffered_ty, test_span())] };

    let result = solve_constraints(constraints, &class_registry, &typevar_registry);
    assert!(result.is_ok(), "Solver should complete");

    let (_, errors) = result.unwrap();
    assert!(
        errors.is_empty(),
        "Stream should satisfy Buffered (with inherited method from Readable): {errors:?}"
    );
}

/// Test that empty protocol (no methods) extends another protocol correctly
#[test]
fn test_empty_protocol_extends_nonempty() {
    let mut class_registry = ClassRegistry::new();
    let typevar_registry = TypeVarConstraintRegistry::new();

    let mut base_proto = ClassMetadata::new("Base".to_string());
    base_proto.set_protocol(true);
    base_proto.add_method("method".to_string(), Type::fun(vec![], Type::int()));
    class_registry.register_class("Base".to_string(), base_proto);

    let mut derived_proto = ClassMetadata::new("Derived".to_string());
    derived_proto.set_protocol(true);
    derived_proto.add_base_class("Base".to_string());

    class_registry.register_class("Derived".to_string(), derived_proto);

    let mut impl_class = ClassMetadata::new("Impl".to_string());
    impl_class.add_method("method".to_string(), Type::fun(vec![], Type::int()));
    class_registry.register_class("Impl".to_string(), impl_class);

    let impl_ty = Type::Con(TypeCtor::Class("Impl".to_string()));
    let derived_ty = Type::Con(TypeCtor::Protocol(Some("Derived".to_string()), vec![]));

    let constraints = ConstraintSet { constraints: vec![Constraint::Equal(impl_ty.clone(), derived_ty, test_span())] };

    let result = solve_constraints(constraints, &class_registry, &typevar_registry);
    assert!(result.is_ok(), "Solver should complete");

    let (_, errors) = result.unwrap();
    assert!(
        errors.is_empty(),
        "Impl should satisfy empty Derived protocol (inherits Base requirement): {errors:?}"
    );
}

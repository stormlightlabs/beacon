//! Pattern matching validation
//!
//! Validates pattern type compatibility and structure during constraint solving.

use crate::pattern_compat::{
    class_pattern_matches_type, extract_tuple_arity, is_mapping_type, is_open_subject_type, is_sequence_type,
    literal_pattern_type, types_could_match,
};

use beacon_core::{ClassRegistry, Type, TypeCtor, TypeError};
use beacon_parser::{AstNode, Pattern};

/// Validate that a pattern is type-compatible with the subject type
///
/// Performs structural validation to detect obvious type mismatches that would never unify.
pub fn validate_pattern_type_compatibility(
    pattern: &Pattern, subject_type: &Type, class_registry: &ClassRegistry,
) -> Result<(), TypeError> {
    if matches!(subject_type, Type::Var(_)) || is_open_subject_type(subject_type) {
        return Ok(());
    }

    match pattern {
        Pattern::MatchValue(literal) => validate_literal_pattern_type(literal, subject_type),
        Pattern::MatchClass { cls, .. } => validate_class_pattern_type(cls, subject_type, class_registry),
        Pattern::MatchMapping { .. } => validate_mapping_pattern_type(subject_type),
        Pattern::MatchSequence(_) => validate_sequence_pattern_type(subject_type),
        Pattern::MatchAs { pattern: Some(sub), .. } => {
            validate_pattern_type_compatibility(sub, subject_type, class_registry)
        }
        Pattern::MatchOr(alternatives) => {
            for alt in alternatives {
                validate_pattern_type_compatibility(alt, subject_type, class_registry)?;
            }
            Ok(())
        }
        _ => Ok(()),
    }
}

/// Validate that a literal pattern matches the subject type
fn validate_literal_pattern_type(literal: &AstNode, subject_type: &Type) -> Result<(), TypeError> {
    if is_open_subject_type(subject_type) {
        return Ok(());
    }

    let Some(pattern_type) = literal_pattern_type(literal) else {
        return Ok(());
    };

    if !types_could_match(&pattern_type, subject_type) {
        return Err(TypeError::PatternTypeMismatch {
            pattern_type: pattern_type.display_for_diagnostics(),
            subject_type: subject_type.display_for_diagnostics(),
        });
    }

    Ok(())
}

/// Validate that a class pattern can match the subject type
///
/// Class patterns like `case int():` or `case str():` match instances of those types.
/// Built-in types are represented as `Type::Con(TypeCtor::Int)` not `Type::Con(TypeCtor::Class("int"))`
fn validate_class_pattern_type(
    cls: &str, subject_type: &Type, class_registry: &ClassRegistry,
) -> Result<(), TypeError> {
    if is_open_subject_type(subject_type) {
        return Ok(());
    }
    if class_pattern_matches_type(cls, subject_type, class_registry) {
        return Ok(());
    }

    match subject_type {
        Type::Union(variants) => {
            let any_compatible = variants
                .iter()
                .any(|variant| class_pattern_matches_type(cls, variant, class_registry));
            if any_compatible {
                Ok(())
            } else {
                Err(TypeError::PatternTypeMismatch {
                    pattern_type: format!("class {cls}"),
                    subject_type: subject_type.display_for_diagnostics(),
                })
            }
        }
        Type::Var(_) => Ok(()),
        _ => Err(TypeError::PatternTypeMismatch {
            pattern_type: format!("class {cls}"),
            subject_type: subject_type.to_string(),
        }),
    }
}

/// Validate that a mapping pattern can match the subject type
fn validate_mapping_pattern_type(subject_type: &Type) -> Result<(), TypeError> {
    if is_open_subject_type(subject_type) {
        return Ok(());
    }
    match subject_type {
        Type::Union(variants) => {
            if variants.iter().any(is_mapping_type) {
                Ok(())
            } else {
                Err(TypeError::PatternTypeMismatch {
                    pattern_type: "mapping pattern".to_string(),
                    subject_type: subject_type.display_for_diagnostics(),
                })
            }
        }
        ty if is_mapping_type(ty) => Ok(()),
        Type::Var(_) => Ok(()),
        _ => Err(TypeError::PatternTypeMismatch {
            pattern_type: "mapping pattern".to_string(),
            subject_type: subject_type.to_string(),
        }),
    }
}

/// Validate that a sequence pattern can match the subject type
fn validate_sequence_pattern_type(subject_type: &Type) -> Result<(), TypeError> {
    if is_open_subject_type(subject_type) {
        return Ok(());
    }
    match subject_type {
        Type::Union(variants) => {
            if variants.iter().any(is_sequence_type) {
                Ok(())
            } else {
                Err(TypeError::PatternTypeMismatch {
                    pattern_type: "sequence pattern".to_string(),
                    subject_type: subject_type.display_for_diagnostics(),
                })
            }
        }
        ty if is_sequence_type(ty) => Ok(()),
        Type::Var(_) => Ok(()),
        _ => Err(TypeError::PatternTypeMismatch {
            pattern_type: "sequence pattern".to_string(),
            subject_type: subject_type.to_string(),
        }),
    }
}

/// Validate pattern structure compatibility with subject type
///
/// TODO: Add validation for sequences where we can infer constraints.
pub fn validate_pattern_structure(pattern: &Pattern, subject_type: &Type) -> Result<(), TypeError> {
    match (pattern, subject_type) {
        (Pattern::MatchSequence(patterns), Type::App(ctor, _)) => {
            if let Type::Con(TypeCtor::Tuple) = ctor.as_ref()
                && let Some(arity) = extract_tuple_arity(subject_type)
                && patterns.len() != arity
            {
                return Err(TypeError::PatternStructureMismatch {
                    expected: format!("{arity} elements"),
                    found: format!("{} pattern bindings", patterns.len()),
                });
            }
            // If arity is unknown (e.g., tuple[int, ...]), we can't validate
            Ok(())
        }
        _ => Ok(()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use beacon_core::ClassMetadata;
    use beacon_parser::Pattern;

    #[test]
    fn test_class_pattern_matches_int() {
        let class_registry = ClassRegistry::new();
        let int_type = Type::int();
        assert!(
            class_pattern_matches_type("int", &int_type, &class_registry),
            "case int() should match int type"
        );
    }

    #[test]
    fn test_class_pattern_matches_str() {
        let class_registry = ClassRegistry::new();
        let str_type = Type::string();
        assert!(
            class_pattern_matches_type("str", &str_type, &class_registry),
            "case str() should match str type"
        );
    }

    #[test]
    fn test_class_pattern_matches_bool() {
        let class_registry = ClassRegistry::new();
        let bool_type = Type::bool();
        assert!(
            class_pattern_matches_type("bool", &bool_type, &class_registry),
            "case bool() should match bool type"
        );
    }

    #[test]
    fn test_class_pattern_matches_float() {
        let class_registry = ClassRegistry::new();
        let float_type = Type::float();
        assert!(
            class_pattern_matches_type("float", &float_type, &class_registry),
            "case float() should match float type"
        );
    }

    #[test]
    fn test_class_pattern_int_matches_bool() {
        let class_registry = ClassRegistry::new();
        let bool_type = Type::bool();
        assert!(
            class_pattern_matches_type("int", &bool_type, &class_registry),
            "case int() should match bool type (bool is subtype of int)"
        );
    }

    #[test]
    fn test_class_pattern_bool_does_not_match_int() {
        let class_registry = ClassRegistry::new();
        let int_type = Type::int();
        assert!(
            !class_pattern_matches_type("bool", &int_type, &class_registry),
            "case bool() should NOT match general int type"
        );
    }

    #[test]
    fn test_class_pattern_int_union() {
        let class_registry = ClassRegistry::new();
        let union_type = Type::union(vec![Type::int(), Type::string()]);
        let pattern = Pattern::MatchClass { cls: "int".to_string(), patterns: vec![] };

        let result = validate_pattern_type_compatibility(&pattern, &union_type, &class_registry);
        assert!(result.is_ok(), "case int() should match int | str union without error");
    }

    #[test]
    fn test_class_pattern_str_union() {
        let class_registry = ClassRegistry::new();
        let union_type = Type::union(vec![Type::int(), Type::string()]);
        let pattern = Pattern::MatchClass { cls: "str".to_string(), patterns: vec![] };

        let result = validate_pattern_type_compatibility(&pattern, &union_type, &class_registry);
        assert!(result.is_ok(), "case str() should match int | str union without error");
    }

    #[test]
    fn test_class_pattern_int_simple() {
        let class_registry = ClassRegistry::new();
        let int_type = Type::int();
        let pattern = Pattern::MatchClass { cls: "int".to_string(), patterns: vec![] };

        let result = validate_pattern_type_compatibility(&pattern, &int_type, &class_registry);
        assert!(result.is_ok(), "case int() should match int type without error");
    }

    #[test]
    fn test_class_pattern_mismatch() {
        let class_registry = ClassRegistry::new();
        let int_type = Type::int();
        let pattern = Pattern::MatchClass { cls: "str".to_string(), patterns: vec![] };

        let result = validate_pattern_type_compatibility(&pattern, &int_type, &class_registry);
        assert!(result.is_err(), "case str() should not match int type");
    }

    #[test]
    fn test_class_pattern_custom_class() {
        let class_registry = ClassRegistry::new();
        let point_type = Type::Con(TypeCtor::Class("Point".to_string()));
        let pattern = Pattern::MatchClass { cls: "Point".to_string(), patterns: vec![] };

        let result = validate_pattern_type_compatibility(&pattern, &point_type, &class_registry);
        assert!(result.is_ok(), "case Point() should match Point class");
    }

    #[test]
    fn test_class_pattern_union_with_none() {
        let class_registry = ClassRegistry::new();
        let union_type = Type::union(vec![Type::int(), Type::none()]);
        let pattern = Pattern::MatchClass { cls: "int".to_string(), patterns: vec![] };

        let result = validate_pattern_type_compatibility(&pattern, &union_type, &class_registry);
        assert!(result.is_ok(), "case int() should match int | None union");
    }

    #[test]
    fn test_class_pattern_inheritance() {
        let mut class_registry = ClassRegistry::new();
        let mut animal = ClassMetadata::new("Animal".to_string());
        animal.add_field("name".to_string(), Type::string());
        class_registry.register_class("Animal".to_string(), animal);

        let mut dog = ClassMetadata::new("Dog".to_string());
        dog.add_base_class("Animal".to_string());
        dog.add_field("breed".to_string(), Type::string());
        class_registry.register_class("Dog".to_string(), dog);

        let dog_type = Type::Con(TypeCtor::Class("Dog".to_string()));

        assert!(
            class_pattern_matches_type("Animal", &dog_type, &class_registry),
            "case Animal() should match Dog type (Dog inherits from Animal)"
        );

        let animal_type = Type::Con(TypeCtor::Class("Animal".to_string()));
        assert!(
            !class_pattern_matches_type("Dog", &animal_type, &class_registry),
            "case Dog() should NOT match Animal type (inheritance only works one way)"
        );
    }
}

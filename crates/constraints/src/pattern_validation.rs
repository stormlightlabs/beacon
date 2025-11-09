//! Pattern matching validation
//!
//! Validates pattern type compatibility and structure during constraint solving.

use beacon_core::{ClassRegistry, Type, TypeCtor, TypeError};
use beacon_parser::{AstNode, LiteralValue, Pattern};

/// Validate that a pattern is type-compatible with the subject type
///
/// Performs structural validation to detect obvious type mismatches that would never unify.
pub fn validate_pattern_type_compatibility(
    pattern: &Pattern, subject_type: &Type, class_registry: &ClassRegistry,
) -> Result<(), TypeError> {
    if matches!(subject_type, Type::Var(_)) {
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
    let pattern_type = match literal {
        AstNode::Literal { value, .. } => match value {
            LiteralValue::Integer(_) => Type::int(),
            LiteralValue::Float(_) => Type::float(),
            LiteralValue::String { .. } => Type::string(),
            LiteralValue::Boolean(_) => Type::bool(),
            LiteralValue::None => Type::none(),
        },
        AstNode::Identifier { .. } => return Ok(()),
        _ => return Ok(()),
    };

    if !types_could_match(&pattern_type, subject_type) {
        return Err(TypeError::PatternTypeMismatch {
            pattern_type: pattern_type.to_string(),
            subject_type: subject_type.to_string(),
        });
    }

    Ok(())
}

/// Validate that a class pattern can match the subject type
fn validate_class_pattern_type(
    cls: &str, subject_type: &Type, _class_registry: &ClassRegistry,
) -> Result<(), TypeError> {
    match subject_type {
        Type::Con(TypeCtor::Class(subject_class)) => {
            if subject_class != cls {
                return Err(TypeError::PatternTypeMismatch {
                    pattern_type: format!("class {cls}"),
                    subject_type: format!("class {subject_class}"),
                });
            }
            Ok(())
        }
        Type::Union(variants) => {
            let any_compatible = variants
                .iter()
                .any(|variant| matches!(variant, Type::Con(TypeCtor::Class(subject_class)) if subject_class == cls));
            if any_compatible {
                Ok(())
            } else {
                Err(TypeError::PatternTypeMismatch {
                    pattern_type: format!("class {cls}"),
                    subject_type: subject_type.to_string(),
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
    match subject_type {
        Type::Union(variants) => {
            if variants.iter().any(is_mapping_type) {
                Ok(())
            } else {
                Err(TypeError::PatternTypeMismatch {
                    pattern_type: "mapping pattern".to_string(),
                    subject_type: subject_type.to_string(),
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
    match subject_type {
        Type::Union(variants) => {
            if variants.iter().any(is_sequence_type) {
                Ok(())
            } else {
                Err(TypeError::PatternTypeMismatch {
                    pattern_type: "sequence pattern".to_string(),
                    subject_type: subject_type.to_string(),
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

/// Check if two types could potentially unify
fn types_could_match(pattern_type: &Type, subject_type: &Type) -> bool {
    match (pattern_type, subject_type) {
        // Type variables can match anything
        (Type::Var(_), _) | (_, Type::Var(_)) => true,
        // Same concrete types match
        (Type::Con(a), Type::Con(b)) => a == b,
        // Union types - pattern must match at least one variant
        (pt, Type::Union(variants)) => variants.iter().any(|v| types_could_match(pt, v)),
        // Type applications - check structural compatibility
        (Type::App(a_ctor, _), Type::App(b_ctor, _)) => types_could_match(a_ctor, b_ctor),
        // Default: conservative - allow it
        _ => true,
    }
}

/// Check if a type is a sequence type (list, tuple, etc.)
fn is_sequence_type(ty: &Type) -> bool {
    match ty {
        Type::App(ctor, _) => matches!(ctor.as_ref(), Type::Con(TypeCtor::List) | Type::Con(TypeCtor::Tuple)),
        _ => false,
    }
}

/// Check if a type is a mapping type (dict, etc.)
fn is_mapping_type(ty: &Type) -> bool {
    match ty {
        Type::App(inner, _) => {
            matches!(inner.as_ref(), Type::App(ctor, _) if matches!(ctor.as_ref(), Type::Con(TypeCtor::Dict)))
        }
        _ => false,
    }
}

/// Validate pattern structure compatibility with subject type
///
/// TODO: Add validation for sequences where we can infer constraints.
pub fn validate_pattern_structure(pattern: &Pattern, subject_type: &Type) -> Result<(), TypeError> {
    match (pattern, subject_type) {
        (Pattern::MatchSequence(patterns), Type::App(ctor, _)) => {
            if let Type::Con(TypeCtor::Tuple) = ctor.as_ref() {
                if let Some(arity) = extract_tuple_arity(subject_type) {
                    if patterns.len() != arity {
                        return Err(TypeError::PatternStructureMismatch {
                            expected: format!("{arity} elements"),
                            found: format!("{} pattern bindings", patterns.len()),
                        });
                    }
                }
                // If arity is unknown (e.g., tuple[int, ...]), we can't validate
                // TODO: Add comment in code to indicate this limitation for sequences
            }
            Ok(())
        }
        _ => Ok(()),
    }
}

/// Extract tuple arity if the type is a concrete heterogeneous tuple with known length
fn extract_tuple_arity(ty: &Type) -> Option<usize> {
    match ty {
        // Heterogeneous tuple: tuple[T1, T2, ..., Tn] - has known arity
        Type::Tuple(types) => Some(types.len()),
        // Homogeneous tuple: tuple[T] - arbitrary length, no known arity
        _ => None,
    }
}

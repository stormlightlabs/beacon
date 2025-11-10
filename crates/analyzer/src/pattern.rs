//! Pattern matching type extraction and analysis
//!
//! This module implements type extraction and binding for PEP 634 pattern matching.
//! It handles type narrowing, variable binding, and pattern-subject compatibility checking.

use super::type_env::TypeEnvironment;

use beacon_core::{
    ClassRegistry, Type, TypeCtor, TypeError,
    errors::{AnalysisError, Result},
};
use beacon_parser::{AstNode, LiteralValue, Pattern};
use std::collections::HashSet;

/// Extract type bindings from a pattern matched against a subject type
///
/// Returns a vector of (variable_name, type) pairs for bindings introduced by the pattern.
/// The subject type may be narrowed based on the pattern structure.
///
/// # Pattern Types
///
/// - [Pattern::MatchValue] : No bindings, checks literal compatibility
/// - [Pattern::MatchSequence] : Binds variables to element types
/// - [Pattern::MatchMapping] : Binds variables to value types from dict-like subjects
/// - [Pattern::MatchClass] : Binds variables from constructor positional/keyword patterns
/// - [Pattern::MatchAs] : Binds name to narrowed subject type
/// - [Pattern::MatchOr] : Union of bindings from all alternatives (variables must appear in all branches)
///
/// # Examples
///
/// ```ignore
/// // case [x, y]:
/// //   Binds x: T, y: T where subject: list[T]
/// // case {"key": value}:
/// //   Binds value: V where subject: dict[str, V]
/// // case Point(x, y):
/// //   Binds x: int, y: int where subject: Point
/// // case x:
/// //   Binds x: subject_type
/// ```
pub fn extract_pattern_bindings(
    pattern: &Pattern, subject_type: &Type, env: &mut TypeEnvironment, class_registry: &ClassRegistry,
) -> Result<Vec<(String, Type)>> {
    match pattern {
        Pattern::MatchValue(literal) => {
            let _ = extract_literal_type(literal, env)?;
            Ok(vec![])
        }
        Pattern::MatchSequence(patterns) => extract_sequence_bindings(patterns, subject_type, env, class_registry),
        Pattern::MatchMapping { keys, patterns } => {
            extract_mapping_bindings(keys, patterns, subject_type, env, class_registry)
        }
        Pattern::MatchClass { cls, patterns } => {
            extract_class_bindings(cls, patterns, subject_type, env, class_registry)
        }
        Pattern::MatchAs { pattern: sub_pattern, name } => {
            let mut bindings = Vec::new();
            if let Some(sub) = sub_pattern {
                bindings.extend(extract_pattern_bindings(sub, subject_type, env, class_registry)?);
            }

            if let Some(var_name) = name {
                let narrowed_type = narrow_type_for_pattern(subject_type, sub_pattern.as_ref().map(|p| p.as_ref()));
                bindings.push((var_name.clone(), narrowed_type));
            }

            Ok(bindings)
        }
        Pattern::MatchOr(alternatives) => extract_or_bindings(alternatives, subject_type, env, class_registry),
    }
}

/// Extract the type of a literal value
fn extract_literal_type(literal: &AstNode, env: &mut TypeEnvironment) -> Result<Type> {
    match literal {
        AstNode::Literal { value, .. } => {
            let ty = match value {
                LiteralValue::Integer(_) => Type::int(),
                LiteralValue::Float(_) => Type::float(),
                LiteralValue::String { .. } => Type::string(),
                LiteralValue::Boolean(_) => Type::bool(),
                LiteralValue::None => Type::none(),
            };
            Ok(ty)
        }
        AstNode::Identifier { name, .. } => Ok(env.lookup(name).unwrap_or_else(|| Type::Var(env.fresh_var()))),
        _ => Ok(Type::Var(env.fresh_var())),
    }
}

/// Extract bindings from a sequence pattern like [x, y, *rest]
fn extract_sequence_bindings(
    patterns: &[Pattern], subject_type: &Type, env: &mut TypeEnvironment, class_registry: &ClassRegistry,
) -> Result<Vec<(String, Type)>> {
    let element_type = extract_sequence_element_type(subject_type, env);
    let mut bindings = Vec::new();
    for pattern in patterns {
        let pattern_bindings = extract_pattern_bindings(pattern, &element_type, env, class_registry)?;
        bindings.extend(pattern_bindings);
    }
    Ok(bindings)
}

/// Extract the element type from a sequence type (list, tuple, etc.)
fn extract_sequence_element_type(subject_type: &Type, env: &mut TypeEnvironment) -> Type {
    match subject_type {
        Type::App(ctor, elem) if matches!(ctor.as_ref(), Type::Con(TypeCtor::List)) => elem.as_ref().clone(),
        Type::App(ctor, elem) if matches!(ctor.as_ref(), Type::Con(TypeCtor::Tuple)) => elem.as_ref().clone(),
        _ => Type::Var(env.fresh_var()),
    }
}

/// Extract bindings from a mapping pattern like {"key": value}
/// Keys are matched but don't create bindings in match stmts
/// The exprs are evaluated and compared, but unbound
fn extract_mapping_bindings(
    _keys: &[AstNode], patterns: &[Pattern], subject_type: &Type, env: &mut TypeEnvironment,
    class_registry: &ClassRegistry,
) -> Result<Vec<(String, Type)>> {
    let value_type = extract_mapping_value_type(subject_type, env);
    let mut bindings = Vec::new();
    for pattern in patterns {
        let pattern_bindings = extract_pattern_bindings(pattern, &value_type, env, class_registry)?;
        bindings.extend(pattern_bindings);
    }
    Ok(bindings)
}

/// Extract the value type from a mapping type (dict, etc.)
fn extract_mapping_value_type(subject_type: &Type, env: &mut TypeEnvironment) -> Type {
    match subject_type {
        Type::App(inner, val) => {
            if let Type::App(ctor, _key) = inner.as_ref() {
                if matches!(ctor.as_ref(), Type::Con(TypeCtor::Dict)) {
                    return val.as_ref().clone();
                }
            }
            Type::Var(env.fresh_var())
        }
        _ => Type::Var(env.fresh_var()),
    }
}

/// Extract bindings from a class pattern like Point(x, y)
///
/// Looks up the class constructor signature from the class registry and validates that the number of
/// patterns matches the number of constructor parameters.
/// Extracts proper field types from the constructor signature instead of using fresh type variables.
fn extract_class_bindings(
    cls: &str, patterns: &[Pattern], _subject_type: &Type, env: &mut TypeEnvironment, class_registry: &ClassRegistry,
) -> Result<Vec<(String, Type)>> {
    let mut bindings = Vec::new();

    if let Some(class_meta) = class_registry.get_class(cls) {
        if let Some(init_type) = &class_meta.init_type {
            let param_types = extract_constructor_params(init_type);
            if patterns.len() != param_types.len() {
                return Err(AnalysisError::ConstraintGeneration(format!(
                    "Class pattern for '{}' expects {} arguments but got {}",
                    cls,
                    param_types.len(),
                    patterns.len()
                ))
                .into());
            }

            for (pattern, param_type) in patterns.iter().zip(param_types.iter()) {
                let pattern_bindings = extract_pattern_bindings(pattern, &param_type.1, env, class_registry)?;
                bindings.extend(pattern_bindings);
            }

            return Ok(bindings);
        }
    }

    for pattern in patterns {
        let field_type = Type::Var(env.fresh_var());
        let pattern_bindings = extract_pattern_bindings(pattern, &field_type, env, class_registry)?;
        bindings.extend(pattern_bindings);
    }

    Ok(bindings)
}

/// Extract constructor parameter types from an __init__ function type
fn extract_constructor_params(init_type: &Type) -> Vec<(String, Type)> {
    match init_type {
        Type::Fun(params, _ret) => {
            if params.len() > 1 {
                params[1..].to_vec()
            } else {
                vec![]
            }
        }
        _ => vec![],
    }
}

/// Extract bindings from an OR pattern like case 1 | 2 | 3:
///
/// In Python, OR patterns must bind the same set of variables in all alternatives.
/// This function validates that all alternatives bind exactly the same variable names.
fn extract_or_bindings(
    alternatives: &[Pattern], subject_type: &Type, env: &mut TypeEnvironment, class_registry: &ClassRegistry,
) -> Result<Vec<(String, Type)>> {
    if alternatives.is_empty() {
        return Ok(vec![]);
    }

    let mut common_bindings = extract_pattern_bindings(&alternatives[0], subject_type, env, class_registry)?;
    let first_names: HashSet<String> = common_bindings.iter().map(|(name, _)| name.clone()).collect();

    for (idx, alt) in alternatives[1..].iter().enumerate() {
        let alt_bindings = extract_pattern_bindings(alt, subject_type, env, class_registry)?;
        let alt_names: HashSet<String> = alt_bindings.iter().map(|(name, _)| name.clone()).collect();

        if first_names != alt_names {
            let missing_in_alt: Vec<_> = first_names.difference(&alt_names).collect();
            let extra_in_alt: Vec<_> = alt_names.difference(&first_names).collect();

            let mut error_msg = format!("OR pattern alternative {} has inconsistent bindings:", idx + 1);
            if !missing_in_alt.is_empty() {
                error_msg.push_str(&format!(" missing {missing_in_alt:?}"));
            }
            if !extra_in_alt.is_empty() {
                error_msg.push_str(&format!(" extra {extra_in_alt:?}"));
            }

            return Err(AnalysisError::ConstraintGeneration(error_msg).into());
        }

        for (name, alt_type) in alt_bindings {
            if let Some((_, common_type)) = common_bindings.iter_mut().find(|(n, _)| n == &name) {
                *common_type = Type::union(vec![common_type.clone(), alt_type]);
            }
        }
    }

    Ok(common_bindings)
}

/// Narrow a type based on a pattern
///
/// For example:
/// - Matching against a literal narrows to that literal's type
/// - Matching against a class pattern narrows to that class
/// - Matching against a sequence pattern narrows to a sequence type
fn narrow_type_for_pattern(subject_type: &Type, pattern: Option<&Pattern>) -> Type {
    match pattern {
        None => subject_type.clone(),
        Some(Pattern::MatchValue(_literal)) => subject_type.clone(),
        Some(Pattern::MatchSequence(_)) => match subject_type {
            Type::Union(variants) => {
                let seq_variants: Vec<Type> = variants.iter().filter(|t| is_sequence_type(t)).cloned().collect();
                if seq_variants.is_empty() { subject_type.clone() } else { Type::union(seq_variants) }
            }
            t if is_sequence_type(t) => t.clone(),
            _ => subject_type.clone(),
        },
        Some(Pattern::MatchMapping { .. }) => match subject_type {
            Type::Union(variants) => {
                let map_variants: Vec<Type> = variants.iter().filter(|t| is_mapping_type(t)).cloned().collect();

                if map_variants.is_empty() { subject_type.clone() } else { Type::union(map_variants) }
            }
            t if is_mapping_type(t) => t.clone(),
            _ => subject_type.clone(),
        },
        Some(Pattern::MatchClass { cls, .. }) => Type::Con(TypeCtor::Class(cls.clone())),
        Some(Pattern::MatchAs { pattern: sub, .. }) => {
            narrow_type_for_pattern(subject_type, sub.as_ref().map(|p| p.as_ref()))
        }
        Some(Pattern::MatchOr(alternatives)) => {
            if alternatives.is_empty() {
                subject_type.clone()
            } else {
                let narrowed_types: Vec<Type> = alternatives
                    .iter()
                    .map(|alt| narrow_type_for_pattern(subject_type, Some(alt)))
                    .collect();
                Type::union(narrowed_types)
            }
        }
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

/// Validate that a pattern is type-compatible with the subject type
///
/// Performs structural validation to detect obvious type mismatches that would never unify:
/// - Literal patterns must match the subject's concrete type
/// - Class patterns must match against class types
/// - Mapping patterns must match against dict-like types
/// - Sequence patterns must match against list/tuple types
pub fn validate_pattern_type_compatibility(
    pattern: &Pattern, subject_type: &Type, env: &TypeEnvironment, class_registry: &ClassRegistry,
) -> std::result::Result<(), TypeError> {
    if matches!(subject_type, Type::Var(_)) {
        return Ok(());
    }

    match pattern {
        Pattern::MatchValue(literal) => validate_literal_pattern_type(literal, subject_type, env),
        Pattern::MatchClass { cls, .. } => validate_class_pattern_type(cls, subject_type, class_registry),
        Pattern::MatchMapping { .. } => validate_mapping_pattern_type(subject_type),
        Pattern::MatchSequence(_) => validate_sequence_pattern_type(subject_type),
        Pattern::MatchAs { pattern: Some(sub), .. } => {
            validate_pattern_type_compatibility(sub, subject_type, env, class_registry)
        }
        Pattern::MatchOr(alternatives) => {
            for alt in alternatives {
                validate_pattern_type_compatibility(alt, subject_type, env, class_registry)?;
            }
            Ok(())
        }
        _ => Ok(()),
    }
}

/// Validate that a literal pattern matches the subject type
fn validate_literal_pattern_type(
    literal: &AstNode, subject_type: &Type, _env: &TypeEnvironment,
) -> std::result::Result<(), TypeError> {
    let pattern_type = match literal {
        AstNode::Literal { value, .. } => match value {
            LiteralValue::Integer(_) => Type::int(),
            LiteralValue::Float(_) => Type::float(),
            LiteralValue::String { .. } => Type::string(),
            LiteralValue::Boolean(_) => Type::bool(),
            LiteralValue::None => Type::none(),
        },
        AstNode::Identifier { .. } => {
            return Ok(());
        }
        _ => return Ok(()),
    };

    if !types_could_match(&pattern_type, subject_type) {
        Err(TypeError::PatternTypeMismatch {
            pattern_type: pattern_type.to_string(),
            subject_type: subject_type.to_string(),
        })
    } else {
        Ok(())
    }
}

/// Validate that a class pattern can match the subject type
/// TODO: use registry lookup for inheritance checking
fn validate_class_pattern_type(
    cls: &str, subject_type: &Type, _class_registry: &ClassRegistry,
) -> std::result::Result<(), TypeError> {
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
        Type::Var(_) => Ok(()), // Type variables are fine
        _ => Err(TypeError::PatternTypeMismatch {
            pattern_type: format!("class {cls}"),
            subject_type: subject_type.to_string(),
        }),
    }
}

/// Validate that a mapping pattern can match the subject type
fn validate_mapping_pattern_type(subject_type: &Type) -> std::result::Result<(), TypeError> {
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
fn validate_sequence_pattern_type(subject_type: &Type) -> std::result::Result<(), TypeError> {
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

/// Conservatively check if two types could potentially unify: false only if types are fundamentally incompatible.
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

/// Validate pattern structure compatibility with subject type
pub fn validate_pattern_structure(
    pattern: &Pattern, subject_type: &Type, _env: &TypeEnvironment, _class_registry: &ClassRegistry,
) -> std::result::Result<(), TypeError> {
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

#[cfg(test)]
mod tests {
    use super::*;
    use beacon_core::ClassMetadata;
    use beacon_parser::Pattern;

    #[test]
    fn test_extract_literal_type() {
        let mut env = TypeEnvironment::new();
        let int_literal =
            AstNode::Literal { value: LiteralValue::Integer(42), line: 1, col: 1, end_line: 1, end_col: 2 };
        let ty = extract_literal_type(&int_literal, &mut env).unwrap();
        assert_eq!(ty, Type::int());
    }

    #[test]
    fn test_extract_sequence_element_type() {
        let mut env = TypeEnvironment::new();
        let list_type = Type::list(Type::int());
        let elem_type = extract_sequence_element_type(&list_type, &mut env);
        assert_eq!(elem_type, Type::int());
    }

    #[test]
    fn test_is_sequence_type() {
        assert!(is_sequence_type(&Type::list(Type::int())));
        assert!(!is_sequence_type(&Type::int()));
    }

    #[test]
    fn test_is_mapping_type() {
        assert!(is_mapping_type(&Type::dict(Type::string(), Type::int())));
        assert!(!is_mapping_type(&Type::list(Type::int())));
    }

    #[test]
    fn test_narrow_type_for_class_pattern() {
        let subject = Type::union(vec![
            Type::Con(TypeCtor::Class("Point".to_string())),
            Type::Con(TypeCtor::Class("Circle".to_string())),
        ]);
        let pattern = Pattern::MatchClass { cls: "Point".to_string(), patterns: vec![] };
        let narrowed = narrow_type_for_pattern(&subject, Some(&pattern));
        assert_eq!(narrowed, Type::Con(TypeCtor::Class("Point".to_string())));
    }

    #[test]
    fn test_match_as_bindings() {
        let mut env = TypeEnvironment::new();
        let class_registry = ClassRegistry::new();
        let subject_type = Type::list(Type::int());
        let pattern = Pattern::MatchAs { pattern: None, name: Some("x".to_string()) };
        let bindings = extract_pattern_bindings(&pattern, &subject_type, &mut env, &class_registry).unwrap();
        assert_eq!(bindings.len(), 1);
        assert_eq!(bindings[0].0, "x");
        assert_eq!(bindings[0].1, subject_type);
    }

    #[test]
    fn test_match_sequence_bindings() {
        let mut env = TypeEnvironment::new();
        let class_registry = ClassRegistry::new();
        let subject_type = Type::list(Type::int());
        let pattern = Pattern::MatchSequence(vec![
            Pattern::MatchAs { pattern: None, name: Some("x".to_string()) },
            Pattern::MatchAs { pattern: None, name: Some("y".to_string()) },
        ]);
        let bindings = extract_pattern_bindings(&pattern, &subject_type, &mut env, &class_registry).unwrap();
        assert_eq!(bindings.len(), 2);
        assert_eq!(bindings[0].0, "x");
        assert_eq!(bindings[0].1, Type::int());
        assert_eq!(bindings[1].0, "y");
        assert_eq!(bindings[1].1, Type::int());
    }

    #[test]
    fn test_match_mapping_bindings() {
        let mut env = TypeEnvironment::new();
        let class_registry = ClassRegistry::new();
        let subject_type = Type::dict(Type::string(), Type::int());
        let key = AstNode::Literal {
            value: LiteralValue::String { value: "key".to_string(), prefix: String::new() },
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 1,
        };
        let pattern = Pattern::MatchMapping {
            keys: vec![key],
            patterns: vec![Pattern::MatchAs { pattern: None, name: Some("value".to_string()) }],
        };
        let bindings = extract_pattern_bindings(&pattern, &subject_type, &mut env, &class_registry).unwrap();
        assert_eq!(bindings.len(), 1);
        assert_eq!(bindings[0].0, "value");
        assert_eq!(bindings[0].1, Type::int());
    }

    #[test]
    fn test_match_or_bindings() {
        let mut env = TypeEnvironment::new();
        let class_registry = ClassRegistry::new();
        let subject_type = Type::union(vec![Type::int(), Type::string()]);
        let pattern = Pattern::MatchOr(vec![
            Pattern::MatchAs { pattern: None, name: Some("val".to_string()) },
            Pattern::MatchAs { pattern: None, name: Some("val".to_string()) },
        ]);
        let bindings = extract_pattern_bindings(&pattern, &subject_type, &mut env, &class_registry).unwrap();
        assert_eq!(bindings.len(), 1);
        assert_eq!(bindings[0].0, "val");
        assert!(matches!(bindings[0].1, Type::Union(_)));
    }

    #[test]
    fn test_match_value_no_bindings() {
        let mut env = TypeEnvironment::new();
        let class_registry = ClassRegistry::new();
        let subject_type = Type::int();
        let literal = AstNode::Literal { value: LiteralValue::Integer(42), line: 1, col: 1, end_line: 1, end_col: 2 };
        let pattern = Pattern::MatchValue(literal);
        let bindings = extract_pattern_bindings(&pattern, &subject_type, &mut env, &class_registry).unwrap();
        assert_eq!(bindings.len(), 0);
    }

    #[test]
    fn test_narrow_type_for_sequence() {
        let subject = Type::union(vec![Type::list(Type::int()), Type::string()]);
        let pattern = Pattern::MatchSequence(vec![]);
        let narrowed = narrow_type_for_pattern(&subject, Some(&pattern));
        assert_eq!(narrowed, Type::list(Type::int()));
    }

    #[test]
    fn test_narrow_type_for_mapping() {
        let subject = Type::union(vec![Type::dict(Type::string(), Type::int()), Type::list(Type::int())]);
        let pattern = Pattern::MatchMapping { keys: vec![], patterns: vec![] };
        let narrowed = narrow_type_for_pattern(&subject, Some(&pattern));
        assert_eq!(narrowed, Type::dict(Type::string(), Type::int()));
    }

    #[test]
    fn test_or_pattern_bindings_consistent() {
        let mut env = TypeEnvironment::new();
        let class_registry = ClassRegistry::new();
        let subject_type = Type::union(vec![Type::int(), Type::string()]);
        let pattern = Pattern::MatchOr(vec![
            Pattern::MatchAs { pattern: None, name: Some("x".to_string()) },
            Pattern::MatchAs { pattern: None, name: Some("x".to_string()) },
        ]);
        let result = extract_pattern_bindings(&pattern, &subject_type, &mut env, &class_registry);
        assert!(result.is_ok(), "OR pattern with consistent bindings should succeed");
    }

    #[test]
    fn test_or_pattern_bindings_inconsistent() {
        let mut env = TypeEnvironment::new();
        let class_registry = ClassRegistry::new();
        let subject_type = Type::union(vec![Type::int(), Type::string()]);
        let pattern = Pattern::MatchOr(vec![
            Pattern::MatchAs { pattern: None, name: Some("x".to_string()) },
            Pattern::MatchAs { pattern: None, name: Some("y".to_string()) },
        ]);
        let result = extract_pattern_bindings(&pattern, &subject_type, &mut env, &class_registry);

        assert!(result.is_err(), "OR pattern with inconsistent bindings should fail");

        match result {
            Err(e) => {
                let error_msg = e.to_string();
                assert!(
                    error_msg.contains("inconsistent bindings"),
                    "Error should mention inconsistent bindings, got: {error_msg}"
                );
            }
            Ok(_) => panic!("Expected error for inconsistent OR pattern bindings"),
        }
    }

    #[test]
    fn test_or_pattern_bindings_extra_variable() {
        let mut env = TypeEnvironment::new();
        let class_registry = ClassRegistry::new();
        let subject_type = Type::union(vec![Type::int(), Type::string()]);
        let pattern = Pattern::MatchOr(vec![
            Pattern::MatchAs { pattern: None, name: Some("x".to_string()) },
            Pattern::MatchSequence(vec![
                Pattern::MatchAs { pattern: None, name: Some("x".to_string()) },
                Pattern::MatchAs { pattern: None, name: Some("y".to_string()) },
            ]),
        ]);

        let result = extract_pattern_bindings(&pattern, &subject_type, &mut env, &class_registry);
        assert!(
            result.is_err(),
            "OR pattern with extra bindings in one alternative should fail"
        );
    }

    #[test]
    fn test_class_pattern_with_valid_arity() {
        let mut env = TypeEnvironment::new();
        let mut class_registry = ClassRegistry::new();

        let mut point_meta = ClassMetadata::new("Point".to_string());
        point_meta.init_type = Some(Type::Fun(
            vec![
                (String::new(), Type::Con(TypeCtor::Class("Point".to_string()))),
                (String::new(), Type::int()),
                (String::new(), Type::int()),
            ],
            Box::new(Type::none()),
        ));
        class_registry.register_class("Point".to_string(), point_meta);

        let subject_type = Type::Con(TypeCtor::Class("Point".to_string()));
        let pattern = Pattern::MatchClass {
            cls: "Point".to_string(),
            patterns: vec![
                Pattern::MatchAs { pattern: None, name: Some("x".to_string()) },
                Pattern::MatchAs { pattern: None, name: Some("y".to_string()) },
            ],
        };

        let bindings = extract_pattern_bindings(&pattern, &subject_type, &mut env, &class_registry).unwrap();
        assert_eq!(bindings.len(), 2);
        assert_eq!(bindings[0].0, "x");
        assert_eq!(bindings[0].1, Type::int());
        assert_eq!(bindings[1].0, "y");
        assert_eq!(bindings[1].1, Type::int());
    }

    #[test]
    fn test_class_pattern_with_too_many_args() {
        let mut env = TypeEnvironment::new();
        let mut class_registry = ClassRegistry::new();

        let mut point_meta = ClassMetadata::new("Point".to_string());
        point_meta.init_type = Some(Type::Fun(
            vec![
                (String::new(), Type::Con(TypeCtor::Class("Point".to_string()))),
                (String::new(), Type::int()),
                (String::new(), Type::int()),
            ],
            Box::new(Type::none()),
        ));
        class_registry.register_class("Point".to_string(), point_meta);

        let subject_type = Type::Con(TypeCtor::Class("Point".to_string()));
        let pattern = Pattern::MatchClass {
            cls: "Point".to_string(),
            patterns: vec![
                Pattern::MatchAs { pattern: None, name: Some("x".to_string()) },
                Pattern::MatchAs { pattern: None, name: Some("y".to_string()) },
                Pattern::MatchAs { pattern: None, name: Some("z".to_string()) },
            ],
        };

        let result = extract_pattern_bindings(&pattern, &subject_type, &mut env, &class_registry);
        assert!(result.is_err(), "Should fail with too many arguments");

        match result {
            Err(e) => {
                let error_msg = e.to_string();
                assert!(
                    error_msg.contains("expects 2 arguments but got 3"),
                    "Error should mention argument count mismatch, got: {error_msg}"
                );
            }
            Ok(_) => panic!("Expected error for wrong number of arguments"),
        }
    }

    #[test]
    fn test_class_pattern_with_too_few_args() {
        let mut env = TypeEnvironment::new();
        let mut class_registry = ClassRegistry::new();

        let mut point_meta = ClassMetadata::new("Point".to_string());
        point_meta.init_type = Some(Type::Fun(
            vec![
                (String::new(), Type::Con(TypeCtor::Class("Point".to_string()))),
                (String::new(), Type::int()),
                (String::new(), Type::int()),
            ],
            Box::new(Type::none()),
        ));
        class_registry.register_class("Point".to_string(), point_meta);

        let subject_type = Type::Con(TypeCtor::Class("Point".to_string()));
        let pattern = Pattern::MatchClass {
            cls: "Point".to_string(),
            patterns: vec![Pattern::MatchAs { pattern: None, name: Some("x".to_string()) }],
        };

        let result = extract_pattern_bindings(&pattern, &subject_type, &mut env, &class_registry);
        assert!(result.is_err(), "Should fail with too few arguments");

        match result {
            Err(e) => {
                let error_msg = e.to_string();
                assert!(
                    error_msg.contains("expects 2 arguments but got 1"),
                    "Error should mention argument count mismatch, got: {error_msg}"
                );
            }
            Ok(_) => panic!("Expected error for wrong number of arguments"),
        }
    }

    #[test]
    fn test_class_pattern_with_nested_patterns() {
        let mut env = TypeEnvironment::new();
        let mut class_registry = ClassRegistry::new();

        let mut container_meta = ClassMetadata::new("Container".to_string());
        container_meta.init_type = Some(Type::Fun(
            vec![
                (String::new(), Type::Con(TypeCtor::Class("Container".to_string()))),
                (String::new(), Type::list(Type::int())),
            ],
            Box::new(Type::none()),
        ));
        class_registry.register_class("Container".to_string(), container_meta);

        let subject_type = Type::Con(TypeCtor::Class("Container".to_string()));
        let pattern = Pattern::MatchClass {
            cls: "Container".to_string(),
            patterns: vec![Pattern::MatchSequence(vec![
                Pattern::MatchAs { pattern: None, name: Some("first".to_string()) },
                Pattern::MatchAs { pattern: None, name: Some("second".to_string()) },
            ])],
        };

        let bindings = extract_pattern_bindings(&pattern, &subject_type, &mut env, &class_registry).unwrap();
        assert_eq!(bindings.len(), 2);
        assert_eq!(bindings[0].0, "first");
        assert_eq!(bindings[0].1, Type::int());
        assert_eq!(bindings[1].0, "second");
        assert_eq!(bindings[1].1, Type::int());
    }

    #[test]
    fn test_class_pattern_for_unknown_class_fallback() {
        let mut env = TypeEnvironment::new();
        let class_registry = ClassRegistry::new();
        let subject_type = Type::Con(TypeCtor::Class("UnknownClass".to_string()));
        let pattern = Pattern::MatchClass {
            cls: "UnknownClass".to_string(),
            patterns: vec![
                Pattern::MatchAs { pattern: None, name: Some("x".to_string()) },
                Pattern::MatchAs { pattern: None, name: Some("y".to_string()) },
            ],
        };

        let bindings = extract_pattern_bindings(&pattern, &subject_type, &mut env, &class_registry).unwrap();
        assert_eq!(bindings.len(), 2);
        assert_eq!(bindings[0].0, "x");
        assert_eq!(bindings[1].0, "y");
    }

    #[test]
    fn test_class_pattern_without_init_fallback() {
        let mut env = TypeEnvironment::new();
        let mut class_registry = ClassRegistry::new();
        let simple_meta = ClassMetadata::new("SimpleClass".to_string());
        class_registry.register_class("SimpleClass".to_string(), simple_meta);

        let subject_type = Type::Con(TypeCtor::Class("SimpleClass".to_string()));
        let pattern = Pattern::MatchClass {
            cls: "SimpleClass".to_string(),
            patterns: vec![Pattern::MatchAs { pattern: None, name: Some("field".to_string()) }],
        };

        let bindings = extract_pattern_bindings(&pattern, &subject_type, &mut env, &class_registry).unwrap();
        assert_eq!(bindings.len(), 1);
        assert_eq!(bindings[0].0, "field");
    }
}

//! Exhaustiveness and reachability checking for PEP 634 pattern matching analysis
//!
//! - Exhaustiveness: Check that all possible values of the subject type are covered by patterns
//! - Reachability: Check that no pattern is subsumed by earlier patterns
//!
//! ## Overview
//!
//! Exhaustiveness checking uses a coverage-based approach:
//! 1. Start with the full subject type as "uncovered"
//! 2. For each pattern, compute what values it covers
//! 3. Remove covered values from the uncovered set
//! 4. If any values remain uncovered, the match is non-exhaustive
//!
//! Reachability checking uses subsumption:
//! 1. For each pattern P, check if any earlier pattern P' covers all values that P covers
//! 2. If P is fully subsumed by P', it's unreachable
//!
//! ## Examples
//!
//! ```python
//! # Non-exhaustive (missing True case)
//! match value:  # value: bool
//!     case False: ...
//!
//! # Unreachable (second pattern subsumed by first)
//! match value:  # value: int | str
//!     case _: ...        # covers everything
//!     case 42: ...       # unreachable!
//! ```

use beacon_core::{Type, TypeCtor};
use beacon_parser::{AstNode, LiteralValue, Pattern};

/// Result of exhaustiveness checking
#[derive(Debug, Clone)]
pub enum ExhaustivenessResult {
    /// All values are covered by the patterns
    Exhaustive,
    /// Some values are not covered
    NonExhaustive {
        /// Types that are not covered by any pattern
        uncovered: Vec<Type>,
    },
}

/// Result of reachability checking
#[derive(Debug, Clone, PartialEq)]
pub enum ReachabilityResult {
    /// Pattern is reachable (not subsumed by earlier patterns)
    Reachable,
    /// Pattern is unreachable (fully subsumed by an earlier pattern)
    Unreachable {
        /// Index of the pattern that subsumes this one
        subsumed_by: usize,
    },
}

/// Check if a set of patterns exhaustively covers a subject type
pub fn check_exhaustiveness(
    subject_type: &Type, patterns: &[Pattern], class_registry: &beacon_core::ClassRegistry,
) -> ExhaustivenessResult {
    tracing::debug!(
        "Checking exhaustiveness for subject type: {} with {} patterns",
        subject_type,
        patterns.len()
    );

    let initial_uncovered = match subject_type {
        Type::Con(TypeCtor::Bool) => {
            vec![Type::union(vec![Type::literal_bool(true), Type::literal_bool(false)])]
        }
        _ => vec![subject_type.clone()],
    };

    let mut uncovered = initial_uncovered;
    for (idx, pattern) in patterns.iter().enumerate() {
        let covered = compute_coverage(pattern, subject_type, class_registry);
        tracing::trace!("Pattern {}: {:?} covers {} types", idx, pattern, covered.len());
        uncovered = subtract_coverage(uncovered, covered);

        if uncovered.is_empty() {
            tracing::debug!("Match is exhaustive after pattern {}", idx);
            return ExhaustivenessResult::Exhaustive;
        }
    }

    if uncovered.is_empty() {
        ExhaustivenessResult::Exhaustive
    } else {
        tracing::debug!(
            "Match is non-exhaustive. Uncovered types: {}",
            uncovered.iter().map(|ty| ty.to_string()).collect::<Vec<_>>().join(", ")
        );
        ExhaustivenessResult::NonExhaustive { uncovered }
    }
}

/// Remove covered types from the uncovered set
///
/// This implements type subtraction: uncovered - covered
fn subtract_coverage(uncovered: Vec<Type>, covered: Vec<Type>) -> Vec<Type> {
    let mut result = Vec::new();

    for uncov_ty in uncovered {
        match &uncov_ty {
            Type::Union(variants) => {
                let remaining: Vec<Type> = variants
                    .iter()
                    .filter(|variant| !covered.iter().any(|cov| types_equal_or_subtype(variant, cov)))
                    .cloned()
                    .collect();

                match remaining.len() {
                    0 => {}
                    1 => result.push(remaining[0].clone()),
                    _ => result.push(Type::Union(remaining)),
                }
            }
            _ => {
                if !covered.iter().any(|cov| types_equal_or_subtype(&uncov_ty, cov)) {
                    result.push(uncov_ty);
                }
            }
        }
    }

    result
}

/// Check if type1 is equal to or a subtype of type2
fn types_equal_or_subtype(type1: &Type, type2: &Type) -> bool {
    match (type1, type2) {
        (Type::Con(c1), Type::Con(c2)) => {
            if let TypeCtor::Literal(lit) = c1 {
                if let Some(base_type) = lit_to_base_type(lit) {
                    return c1 == c2 || c2 == &base_type;
                }
            }
            c1 == c2
        }
        (_, Type::Con(TypeCtor::Any)) => true,
        (Type::Var(v1), Type::Var(v2)) => v1 == v2,
        (Type::App(ctor1, arg1), Type::App(ctor2, arg2)) => {
            types_equal_or_subtype(ctor1, ctor2) && types_equal_or_subtype(arg1, arg2)
        }
        (ty, Type::Union(variants)) => variants.iter().any(|v| types_equal_or_subtype(ty, v)),

        _ => false,
    }
}

/// Check if a pattern is reachable (not subsumed by previous patterns):
/// - For each previous pattern, check if it subsumes the current pattern.
/// - If any previous pattern fully subsumes this one, the pattern is unreachable.
pub fn check_reachability(pattern: &Pattern, previous_patterns: &[Pattern]) -> ReachabilityResult {
    tracing::trace!(
        "Checking reachability for pattern against {} previous patterns",
        previous_patterns.len()
    );

    for (idx, prev_pattern) in previous_patterns.iter().enumerate() {
        if pattern_subsumes(prev_pattern, pattern) {
            tracing::debug!("Pattern is unreachable, subsumed by pattern {}", idx);
            return ReachabilityResult::Unreachable { subsumed_by: idx };
        }
    }

    tracing::trace!("Pattern is reachable");
    ReachabilityResult::Reachable
}

/// Compute the coverage of a pattern for a given subject type for both exhaustiveness and reachability checking.
fn compute_coverage(pattern: &Pattern, subject_type: &Type, class_registry: &beacon_core::ClassRegistry) -> Vec<Type> {
    match pattern {
        Pattern::MatchValue(literal) => {
            let literal_type = match literal {
                AstNode::Literal { value, .. } => match value {
                    LiteralValue::Integer(n) => Type::literal_int(*n),
                    LiteralValue::Float(_) => Type::float(),
                    LiteralValue::String { value, .. } => Type::literal_string(value.clone()),
                    LiteralValue::Boolean(b) => Type::literal_bool(*b),
                    LiteralValue::None => Type::literal_none(),
                },
                AstNode::Identifier { name, .. } => match name.as_str() {
                    "None" => Type::literal_none(),
                    "True" => Type::literal_bool(true),
                    "False" => Type::literal_bool(false),
                    _ => return vec![],
                },
                _ => return vec![],
            };

            if types_overlap(&literal_type, subject_type) { vec![literal_type] } else { vec![] }
        }
        Pattern::MatchAs { pattern: None, .. } => vec![subject_type.clone()],
        Pattern::MatchAs { pattern: Some(sub), .. } => compute_coverage(sub, subject_type, class_registry),
        Pattern::MatchSequence(_patterns) => filter_types(subject_type, |ty| match ty {
            Type::App(ctor, _) => {
                matches!(ctor.as_ref(), Type::Con(TypeCtor::List) | Type::Con(TypeCtor::Tuple))
            }
            _ => false,
        }),
        Pattern::MatchMapping { .. } => filter_types(subject_type, |ty| match ty {
            Type::App(inner, _) => matches!(
                inner.as_ref(),
                Type::App(ctor, _) if matches!(ctor.as_ref(), Type::Con(TypeCtor::Dict))
            ),
            _ => false,
        }),
        Pattern::MatchClass { cls, .. } => filter_types(subject_type, |ty| match (cls.as_str(), ty) {
            ("int", Type::Con(TypeCtor::Int)) => true,
            ("int", Type::Con(TypeCtor::Bool)) => true,
            ("str", Type::Con(TypeCtor::String)) => true,
            ("bool", Type::Con(TypeCtor::Bool)) => true,
            ("float", Type::Con(TypeCtor::Float)) => true,
            (pattern_class, Type::Con(TypeCtor::Class(subject_class))) => {
                pattern_class == subject_class || class_registry.is_subclass_of(subject_class, pattern_class)
            }
            _ => false,
        }),
        Pattern::MatchOr(alternatives) => {
            let mut coverage = Vec::new();
            for alt in alternatives {
                coverage.extend(compute_coverage(alt, subject_type, class_registry));
            }
            coverage.sort_by(|a, b| format!("{a:?}").cmp(&format!("{b:?}")));
            coverage.dedup();
            coverage
        }
    }
}

/// Check if pattern1 subsumes pattern2 (pattern2 is redundant after pattern1)
fn pattern_subsumes(pattern1: &Pattern, pattern2: &Pattern) -> bool {
    match (pattern1, pattern2) {
        (Pattern::MatchAs { pattern: None, .. }, _) => true,
        (Pattern::MatchAs { pattern: Some(sub1), .. }, _) => pattern_subsumes(sub1, pattern2),
        (_, Pattern::MatchAs { pattern: Some(sub2), .. }) => pattern_subsumes(pattern1, sub2),
        (_, Pattern::MatchAs { pattern: None, .. }) => false,
        (Pattern::MatchValue(lit1), Pattern::MatchValue(lit2)) => literals_equal(lit1, lit2),
        (Pattern::MatchClass { cls: cls1, patterns: pats1 }, Pattern::MatchClass { cls: cls2, patterns: pats2 }) => {
            let class_subsumes = if cls1 == cls2 { true } else { cls1 == "int" && cls2 == "bool" };

            class_subsumes
                && pats1.len() == pats2.len()
                && pats1.iter().zip(pats2.iter()).all(|(p1, p2)| pattern_subsumes(p1, p2))
        }
        (Pattern::MatchSequence(pats1), Pattern::MatchSequence(pats2)) => {
            pats1.len() == pats2.len() && pats1.iter().zip(pats2.iter()).all(|(p1, p2)| pattern_subsumes(p1, p2))
        }
        (
            Pattern::MatchMapping { keys: keys1, patterns: pats1 },
            Pattern::MatchMapping { keys: keys2, patterns: pats2 },
        ) => {
            keys1.len() == keys2.len()
                && keys1.iter().zip(keys2.iter()).all(|(k1, k2)| literals_equal(k1, k2))
                && pats1.len() == pats2.len()
                && pats1.iter().zip(pats2.iter()).all(|(p1, p2)| pattern_subsumes(p1, p2))
        }
        (Pattern::MatchOr(alts), _) => alts.iter().any(|alt| pattern_subsumes(alt, pattern2)),
        (_, Pattern::MatchOr(alts)) => alts.iter().all(|alt| pattern_subsumes(pattern1, alt)),
        _ => false,
    }
}

/// Check if two literal AST nodes represent the same value
fn literals_equal(lit1: &beacon_parser::AstNode, lit2: &beacon_parser::AstNode) -> bool {
    match (lit1, lit2) {
        (AstNode::Literal { value: v1, .. }, AstNode::Literal { value: v2, .. }) => match (v1, v2) {
            (LiteralValue::Integer(i1), LiteralValue::Integer(i2)) => i1 == i2,
            (LiteralValue::Float(f1), LiteralValue::Float(f2)) => f1 == f2,
            (LiteralValue::String { value: s1, prefix: p1 }, LiteralValue::String { value: s2, prefix: p2 }) => {
                s1 == s2 && p1 == p2
            }
            (LiteralValue::Boolean(b1), LiteralValue::Boolean(b2)) => b1 == b2,
            (LiteralValue::None, LiteralValue::None) => true,
            _ => false,
        },
        (AstNode::Identifier { name: n1, .. }, AstNode::Identifier { name: n2, .. }) => n1 == n2,
        _ => false,
    }
}

/// Check if two types can have overlapping values
fn types_overlap(type1: &Type, type2: &Type) -> bool {
    match (type1, type2) {
        (Type::Var(_), _) | (_, Type::Var(_)) => true,
        (Type::Con(TypeCtor::Any), _) | (_, Type::Con(TypeCtor::Any)) => true,
        (Type::Union(vars), other) | (other, Type::Union(vars)) => vars.iter().any(|v| types_overlap(v, other)),
        (Type::Con(c1), Type::Con(c2)) => match (c1, c2) {
            (TypeCtor::Literal(lit), base) | (base, TypeCtor::Literal(lit)) => {
                if let Some(base_type) = lit_to_base_type(lit) { base == &base_type || c1 == c2 } else { c1 == c2 }
            }
            _ => c1 == c2,
        },
        (Type::App(ctor1, arg1), Type::App(ctor2, arg2)) => types_overlap(ctor1, ctor2) && types_overlap(arg1, arg2),
        _ => false,
    }
}

/// Convert a literal type to its base type constructor
fn lit_to_base_type(lit: &beacon_core::LiteralType) -> Option<beacon_core::TypeCtor> {
    use beacon_core::{LiteralType, TypeCtor};

    match lit {
        LiteralType::Int(_) => Some(TypeCtor::Int),
        LiteralType::Bool(_) => Some(TypeCtor::Bool),
        LiteralType::String(_) => Some(TypeCtor::String),
        LiteralType::None => Some(TypeCtor::NoneType),
    }
}

/// Filter types from a subject type based on a predicate
///
/// - If subject is a union, returns matching variants.
/// - If subject is a single type, returns it if it matches the predicate.
fn filter_types<F>(subject_type: &Type, predicate: F) -> Vec<Type>
where
    F: Fn(&Type) -> bool,
{
    match subject_type {
        Type::Union(variants) => variants.iter().filter(|ty| predicate(ty)).cloned().collect(),
        ty if predicate(ty) => vec![ty.clone()],
        _ => vec![],
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use beacon_core::TypeVar;
    use beacon_parser::{AstNode, LiteralValue};

    /// Helper to create a boolean literal pattern
    fn bool_pattern(value: bool) -> Pattern {
        Pattern::MatchValue(AstNode::Literal {
            value: LiteralValue::Boolean(value),
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 1,
        })
    }

    /// Helper to create an integer literal pattern
    fn int_pattern(value: i64) -> Pattern {
        Pattern::MatchValue(AstNode::Literal {
            value: LiteralValue::Integer(value),
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 1,
        })
    }

    /// Helper to create a catch-all pattern
    fn catch_all(name: &str) -> Pattern {
        Pattern::MatchAs { pattern: None, name: Some(name.to_string()) }
    }

    #[test]
    fn test_exhaustiveness_bool_complete() {
        let subject = Type::bool();
        let patterns = vec![bool_pattern(true), bool_pattern(false)];
        let result = check_exhaustiveness(&subject, &patterns, &beacon_core::ClassRegistry::new());
        assert!(matches!(result, ExhaustivenessResult::Exhaustive));
    }

    #[test]
    fn test_exhaustiveness_bool_incomplete() {
        let subject = Type::bool();
        let patterns = vec![bool_pattern(true)];
        let result = check_exhaustiveness(&subject, &patterns, &beacon_core::ClassRegistry::new());

        match result {
            ExhaustivenessResult::NonExhaustive { uncovered } => {
                assert!(!uncovered.is_empty(), "Should have uncovered cases");
            }
            ExhaustivenessResult::Exhaustive => {
                panic!("Expected non-exhaustive: only True is matched, False is missing")
            }
        }
    }

    #[test]
    fn test_exhaustiveness_catch_all() {
        let subject = Type::bool();
        let patterns = vec![catch_all("x")];
        let result = check_exhaustiveness(&subject, &patterns, &beacon_core::ClassRegistry::new());
        assert!(matches!(result, ExhaustivenessResult::Exhaustive));
    }

    #[test]
    fn test_exhaustiveness_union_complete() {
        let subject = Type::union(vec![Type::int(), Type::string()]);
        let patterns = vec![
            Pattern::MatchValue(AstNode::Literal {
                value: LiteralValue::Integer(0),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 1,
            }),
            catch_all("rest"),
        ];

        let result = check_exhaustiveness(&subject, &patterns, &beacon_core::ClassRegistry::new());
        assert!(
            matches!(result, ExhaustivenessResult::Exhaustive),
            "Literal + catch-all should be exhaustive for union"
        );
    }

    #[test]
    fn test_exhaustiveness_union_incomplete() {
        let subject = Type::union(vec![Type::int(), Type::string(), Type::bool()]);
        let patterns = vec![
            Pattern::MatchValue(AstNode::Literal {
                value: LiteralValue::Integer(0),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 1,
            }),
            Pattern::MatchValue(AstNode::Literal {
                value: LiteralValue::String { value: "".to_string(), prefix: String::new() },
                line: 1,
                col: 1,
                end_col: 1,
                end_line: 1,
            }),
        ];

        match check_exhaustiveness(&subject, &patterns, &beacon_core::ClassRegistry::new()) {
            ExhaustivenessResult::NonExhaustive { uncovered } => {
                assert!(!uncovered.is_empty())
            }
            ExhaustivenessResult::Exhaustive => panic!("Expected non-exhaustive match"),
        }
    }

    #[test]
    fn test_reachability_after_catch_all() {
        let catch_all = Pattern::MatchAs { pattern: None, name: Some("_".to_string()) };
        let specific = int_pattern(42);
        let result = check_reachability(&specific, &[catch_all]);
        assert!(matches!(result, ReachabilityResult::Unreachable { subsumed_by: 0 }));
    }

    #[test]
    fn test_reachability_duplicate_literal() {
        let pattern1 = int_pattern(42);
        let pattern2 = int_pattern(42);
        let result = check_reachability(&pattern2, &[pattern1]);
        assert!(matches!(result, ReachabilityResult::Unreachable { subsumed_by: 0 }));
    }

    #[test]
    fn test_reachability_different_literals() {
        let pattern1 = int_pattern(42);
        let pattern2 = int_pattern(43);

        let result = check_reachability(&pattern2, &[pattern1]);
        assert!(matches!(result, ReachabilityResult::Reachable));
    }

    #[test]
    fn test_reachability_sequence_patterns() {
        let pattern1 = Pattern::MatchSequence(vec![catch_all("x"), catch_all("y")]);
        let pattern2 = Pattern::MatchSequence(vec![int_pattern(1), int_pattern(2)]);
        let result = check_reachability(&pattern2, &[pattern1]);
        assert!(matches!(result, ReachabilityResult::Unreachable { subsumed_by: 0 }));
    }

    #[test]
    fn test_reachability_class_patterns() {
        let pattern1 = Pattern::MatchClass { cls: "Point".to_string(), patterns: vec![catch_all("x"), catch_all("y")] };
        let pattern2 = Pattern::MatchClass { cls: "Point".to_string(), patterns: vec![int_pattern(0), int_pattern(0)] };
        let result = check_reachability(&pattern2, &[pattern1]);
        assert!(matches!(result, ReachabilityResult::Unreachable { subsumed_by: 0 }));
    }

    #[test]
    fn test_reachability_different_classes() {
        let pattern1 = Pattern::MatchClass { cls: "Point".to_string(), patterns: vec![] };
        let pattern2 = Pattern::MatchClass { cls: "Circle".to_string(), patterns: vec![] };
        let result = check_reachability(&pattern2, &[pattern1]);
        assert!(matches!(result, ReachabilityResult::Reachable));
    }

    #[test]
    fn test_pattern_subsumes_or_pattern() {
        let or_pattern = Pattern::MatchOr(vec![int_pattern(1), int_pattern(2)]);
        let specific = int_pattern(1);
        assert!(pattern_subsumes(&or_pattern, &specific));
    }

    #[test]
    fn test_pattern_subsumes_nested_as() {
        let as_pattern = Pattern::MatchAs {
            pattern: Some(Box::new(Pattern::MatchSequence(vec![catch_all("x")]))),
            name: Some("y".to_string()),
        };
        let specific = Pattern::MatchSequence(vec![int_pattern(1)]);

        assert!(pattern_subsumes(&as_pattern, &specific));
    }

    #[test]
    fn test_types_overlap() {
        assert!(types_overlap(&Type::int(), &Type::int()));
        assert!(!types_overlap(&Type::int(), &Type::string()));
        assert!(types_overlap(
            &Type::int(),
            &Type::union(vec![Type::int(), Type::string()])
        ));
        assert!(types_overlap(&Type::Var(TypeVar::new(0)), &Type::int()));
    }

    #[test]
    fn test_filter_types_union() {
        let subject = Type::union(vec![Type::int(), Type::string(), Type::bool()]);
        let filtered = filter_types(&subject, |ty| matches!(ty, Type::Con(beacon_core::TypeCtor::Int)));
        assert_eq!(filtered.len(), 1);
        assert!(matches!(filtered[0], Type::Con(beacon_core::TypeCtor::Int)));
    }

    #[test]
    fn test_filter_types_single() {
        let subject = Type::int();
        let filtered = filter_types(&subject, |ty| matches!(ty, Type::Con(beacon_core::TypeCtor::Int)));
        assert_eq!(filtered.len(), 1);
    }

    #[test]
    fn test_subtract_coverage_complete() {
        let uncovered = vec![Type::bool()];
        let covered = vec![Type::bool()];
        let result = subtract_coverage(uncovered, covered);
        assert!(result.is_empty());
    }

    #[test]
    fn test_subtract_coverage_partial_union() {
        let uncovered = vec![Type::union(vec![Type::int(), Type::string(), Type::bool()])];
        let covered = vec![Type::int()];
        let result = subtract_coverage(uncovered, covered);
        assert_eq!(result.len(), 1);

        match &result[0] {
            Type::Union(variants) => {
                assert_eq!(variants.len(), 2);
            }
            _ => panic!("Expected union type"),
        }
    }

    #[test]
    fn test_compute_coverage_literal() {
        let subject = Type::int();
        let pattern = int_pattern(42);
        let coverage = compute_coverage(&pattern, &subject, &beacon_core::ClassRegistry::new());
        assert_eq!(coverage.len(), 1);
        assert!(
            matches!(coverage[0], Type::Con(beacon_core::TypeCtor::Literal(_))),
            "Literal pattern should produce literal type"
        );
    }

    #[test]
    fn test_compute_coverage_catch_all() {
        let subject = Type::union(vec![Type::int(), Type::string()]);
        let pattern = catch_all("x");
        let coverage = compute_coverage(&pattern, &subject, &beacon_core::ClassRegistry::new());
        assert_eq!(coverage.len(), 1);
        assert!(matches!(coverage[0], Type::Union(_)));
    }

    #[test]
    fn test_literal_bool_exhaustive() {
        let subject = Type::bool();
        let patterns = vec![bool_pattern(true), bool_pattern(false)];
        let result = check_exhaustiveness(&subject, &patterns, &beacon_core::ClassRegistry::new());
        assert!(
            matches!(result, ExhaustivenessResult::Exhaustive),
            "Both True and False should be exhaustive for bool"
        );
    }

    #[test]
    fn test_literal_bool_only_true() {
        let subject = Type::bool();
        let patterns = vec![bool_pattern(true)];
        let result = check_exhaustiveness(&subject, &patterns, &beacon_core::ClassRegistry::new());
        match result {
            ExhaustivenessResult::NonExhaustive { .. } => {}
            ExhaustivenessResult::Exhaustive => {
                panic!("Only matching True should not be exhaustive for bool")
            }
        }
    }

    #[test]
    fn test_literal_bool_only_false() {
        let subject = Type::bool();
        let patterns = vec![bool_pattern(false)];
        let result = check_exhaustiveness(&subject, &patterns, &beacon_core::ClassRegistry::new());
        match result {
            ExhaustivenessResult::NonExhaustive { .. } => {}
            ExhaustivenessResult::Exhaustive => {
                panic!("Only matching False should not be exhaustive for bool")
            }
        }
    }

    #[test]
    fn test_literal_int_different_values() {
        let subject = Type::int();
        let patterns = vec![int_pattern(1), int_pattern(2)];
        let coverage1 = compute_coverage(&patterns[0], &subject, &beacon_core::ClassRegistry::new());
        let coverage2 = compute_coverage(&patterns[1], &subject, &beacon_core::ClassRegistry::new());

        assert_eq!(coverage1.len(), 1);
        assert_eq!(coverage2.len(), 1);
        assert_ne!(
            coverage1[0], coverage2[0],
            "Different int literals should have different types"
        );
    }

    #[test]
    fn test_reachability_duplicate_bool_literal() {
        let pattern1 = bool_pattern(true);
        let pattern2 = bool_pattern(true);
        let result = check_reachability(&pattern2, &[pattern1]);
        assert!(
            matches!(result, ReachabilityResult::Unreachable { subsumed_by: 0 }),
            "Duplicate True pattern should be unreachable"
        );
    }

    #[test]
    fn test_reachability_different_bool_literals() {
        let pattern1 = bool_pattern(true);
        let pattern2 = bool_pattern(false);
        let result = check_reachability(&pattern2, &[pattern1]);
        assert!(
            matches!(result, ReachabilityResult::Reachable),
            "False pattern after True pattern should be reachable"
        );
    }

    #[test]
    fn test_reachability_duplicate_int_literal() {
        let pattern1 = int_pattern(42);
        let pattern2 = int_pattern(42);
        let result = check_reachability(&pattern2, &[pattern1]);
        assert!(
            matches!(result, ReachabilityResult::Unreachable { subsumed_by: 0 }),
            "Duplicate 42 pattern should be unreachable"
        );
    }

    #[test]
    fn test_reachability_different_int_literals() {
        let pattern1 = int_pattern(42);
        let pattern2 = int_pattern(43);
        let result = check_reachability(&pattern2, &[pattern1]);
        assert!(
            matches!(result, ReachabilityResult::Reachable),
            "Different int literals should both be reachable"
        );
    }

    #[test]
    fn test_compute_coverage_sequence() {
        let subject = Type::union(vec![Type::list(Type::int()), Type::string()]);
        let pattern = Pattern::MatchSequence(vec![]);
        let coverage = compute_coverage(&pattern, &subject, &beacon_core::ClassRegistry::new());
        assert_eq!(coverage.len(), 1);
        assert!(matches!(coverage[0], Type::App(_, _)));
    }

    #[test]
    fn test_exhaustiveness_union_with_or_pattern() {
        let subject = Type::union(vec![Type::int(), Type::string()]);
        let pattern = Pattern::MatchOr(vec![
            Pattern::MatchClass { cls: "str".to_string(), patterns: vec![] },
            Pattern::MatchClass { cls: "int".to_string(), patterns: vec![] },
        ]);
        let patterns = vec![pattern];
        let result = check_exhaustiveness(&subject, &patterns, &beacon_core::ClassRegistry::new());
        assert!(
            matches!(result, ExhaustivenessResult::Exhaustive),
            "str() | int() should be exhaustive for int | str"
        );
    }

    #[test]
    fn test_compute_coverage_class_int() {
        let subject = Type::union(vec![Type::int(), Type::string()]);
        let pattern = Pattern::MatchClass { cls: "int".to_string(), patterns: vec![] };
        let coverage = compute_coverage(&pattern, &subject, &beacon_core::ClassRegistry::new());
        assert_eq!(coverage.len(), 1);
        assert!(
            matches!(coverage[0], Type::Con(TypeCtor::Int)),
            "case int() should cover int type"
        );
    }

    #[test]
    fn test_compute_coverage_class_str() {
        let subject = Type::union(vec![Type::int(), Type::string()]);
        let pattern = Pattern::MatchClass { cls: "str".to_string(), patterns: vec![] };
        let coverage = compute_coverage(&pattern, &subject, &beacon_core::ClassRegistry::new());
        assert_eq!(coverage.len(), 1);
        assert!(
            matches!(coverage[0], Type::Con(TypeCtor::String)),
            "case str() should cover str type"
        );
    }

    #[test]
    fn test_compute_coverage_class_bool() {
        let subject = Type::union(vec![Type::bool(), Type::string()]);
        let pattern = Pattern::MatchClass { cls: "bool".to_string(), patterns: vec![] };
        let coverage = compute_coverage(&pattern, &subject, &beacon_core::ClassRegistry::new());
        assert_eq!(coverage.len(), 1);
        assert!(
            matches!(coverage[0], Type::Con(TypeCtor::Bool)),
            "case bool() should cover bool type"
        );
    }

    #[test]
    fn test_compute_coverage_class_float() {
        let subject = Type::union(vec![Type::float(), Type::string()]);
        let pattern = Pattern::MatchClass { cls: "float".to_string(), patterns: vec![] };
        let coverage = compute_coverage(&pattern, &subject, &beacon_core::ClassRegistry::new());
        assert_eq!(coverage.len(), 1);
        assert!(
            matches!(coverage[0], Type::Con(TypeCtor::Float)),
            "case float() should cover float type"
        );
    }
}

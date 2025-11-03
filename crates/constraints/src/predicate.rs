use crate::pattern::narrow_type_by_pattern;

use beacon_core::{Type, TypeCtor};

/// Type predicates for flow-sensitive narrowing
///
/// Represents conditions that can refine types through control flow analysis.
/// Each predicate describes a runtime check that, when true, allows the type
/// checker to narrow the type of a variable.
#[derive(Debug, Clone, PartialEq)]
pub enum TypePredicate {
    /// Variable is not None: `x is not None`
    IsNotNone,

    /// Variable is None: `x is None`
    IsNone,

    /// Variable is an instance of a type: `isinstance(x, T)`
    /// Can also represent isinstance with multiple types: `isinstance(x, (T1, T2))`
    IsInstance(Type),

    /// Variable is truthy (excludes None, False, 0, "", [], {}, etc.): `if x:`
    IsTruthy,

    /// Variable is falsy: `if not x:`
    IsFalsy,

    /// Conjunction of predicates (for `and` expressions): `if x and y:`
    And(Box<TypePredicate>, Box<TypePredicate>),

    /// Disjunction of predicates (for `or` expressions): `if x or y:`
    Or(Box<TypePredicate>, Box<TypePredicate>),

    /// Negation of a predicate (for `not` expressions): `if not x:`
    Not(Box<TypePredicate>),

    /// Variable matches a pattern: `case Pattern:`
    MatchesPattern(beacon_parser::Pattern),

    /// Stores the target type that the guard narrows to
    /// User-defined type guard: `is_str(x)` where `is_str` has return type `TypeGuard[str]`
    UserDefinedGuard(Type),
}

impl TypePredicate {
    /// Compute which types are eliminated by this predicate
    pub fn eliminated_types(&self, original_type: &Type) -> Vec<Type> {
        match self {
            TypePredicate::IsNotNone | TypePredicate::IsTruthy => {
                if let Type::Union(variants) = original_type {
                    variants
                        .iter()
                        .filter(|v| matches!(v, Type::Con(beacon_core::TypeCtor::NoneType)))
                        .cloned()
                        .collect()
                } else if matches!(original_type, Type::Con(beacon_core::TypeCtor::NoneType)) {
                    vec![original_type.clone()]
                } else {
                    vec![]
                }
            }
            TypePredicate::IsNone => {
                if let Type::Union(variants) = original_type {
                    variants
                        .iter()
                        .filter(|v| !matches!(v, Type::Con(beacon_core::TypeCtor::NoneType)))
                        .cloned()
                        .collect()
                } else if !matches!(original_type, Type::Con(beacon_core::TypeCtor::NoneType)) {
                    vec![original_type.clone()]
                } else {
                    vec![]
                }
            }
            TypePredicate::IsInstance(target) => {
                if let Type::Union(variants) = original_type {
                    let target_types = if let Type::Union(target_variants) = target {
                        target_variants.clone()
                    } else {
                        vec![target.clone()]
                    };

                    variants.iter().filter(|v| !target_types.contains(v)).cloned().collect()
                } else if original_type != target {
                    vec![original_type.clone()]
                } else {
                    vec![]
                }
            }
            TypePredicate::IsFalsy => {
                if let Type::Union(variants) = original_type {
                    variants.iter().filter(|v| !Self::is_falsy_type(v)).cloned().collect()
                } else if !Self::is_falsy_type(original_type) {
                    vec![original_type.clone()]
                } else {
                    vec![]
                }
            }
            TypePredicate::MatchesPattern(pattern) => {
                let narrowed = narrow_type_by_pattern(original_type, pattern);
                if let Type::Union(orig_variants) = original_type {
                    let narrowed_variants =
                        if let Type::Union(n_variants) = &narrowed { n_variants.clone() } else { vec![narrowed] };

                    orig_variants
                        .iter()
                        .filter(|v| !narrowed_variants.contains(v))
                        .cloned()
                        .collect()
                } else if &narrowed != original_type {
                    vec![original_type.clone()]
                } else {
                    vec![]
                }
            }
            TypePredicate::And(p1, p2) => {
                let mut eliminated = p1.eliminated_types(original_type);
                eliminated.extend(p2.eliminated_types(original_type));
                eliminated.sort();
                eliminated.dedup();
                eliminated
            }
            TypePredicate::Or(p1, p2) => {
                let elim1 = p1.eliminated_types(original_type);
                let elim2 = p2.eliminated_types(original_type);
                elim1.into_iter().filter(|t| elim2.contains(t)).collect()
            }
            TypePredicate::Not(p) => {
                let narrowed = p.apply(original_type);
                if let Type::Union(orig_variants) = original_type {
                    let narrowed_variants =
                        if let Type::Union(n_variants) = &narrowed { n_variants.clone() } else { vec![narrowed] };

                    orig_variants
                        .iter()
                        .filter(|v| !narrowed_variants.contains(v))
                        .cloned()
                        .collect()
                } else if &narrowed != original_type {
                    vec![original_type.clone()]
                } else {
                    vec![]
                }
            }
            TypePredicate::UserDefinedGuard(target) => {
                if let Type::Union(variants) = original_type {
                    let target_types = if let Type::Union(target_variants) = target {
                        target_variants.clone()
                    } else {
                        vec![target.clone()]
                    };

                    variants.iter().filter(|v| !target_types.contains(v)).cloned().collect()
                } else if original_type != target {
                    vec![original_type.clone()]
                } else {
                    vec![]
                }
            }
        }
    }

    /// Apply the predicate to a type, returning the narrowed type
    pub fn apply(&self, ty: &Type) -> Type {
        match self {
            TypePredicate::IsNotNone => ty.remove_from_union(&Type::none()),
            TypePredicate::IsNone => Type::none(),
            TypePredicate::IsInstance(target) => match ty {
                Type::Union(variants) => {
                    if variants.contains(target) {
                        target.clone()
                    } else if let Type::Union(target_variants) = target {
                        let intersection: Vec<Type> = variants
                            .iter()
                            .filter(|v| target_variants.contains(v))
                            .cloned()
                            .collect();

                        if intersection.is_empty() {
                            target.clone()
                        } else if intersection.len() == 1 {
                            intersection.into_iter().next().unwrap()
                        } else {
                            Type::union(intersection)
                        }
                    } else {
                        target.clone()
                    }
                }
                _ => target.clone(),
            },
            TypePredicate::IsTruthy => ty.remove_from_union(&Type::none()),
            TypePredicate::IsFalsy => match ty {
                Type::Union(variants) => {
                    let falsy: Vec<Type> = variants.iter().filter(|t| Self::is_falsy_type(t)).cloned().collect();

                    if falsy.is_empty() {
                        Type::none()
                    } else if falsy.len() == 1 {
                        falsy.into_iter().next().unwrap()
                    } else {
                        Type::union(falsy)
                    }
                }
                t if Self::is_falsy_type(t) => t.clone(),
                _ => Type::none(),
            },
            TypePredicate::And(p1, p2) => {
                let t1 = p1.apply(ty);
                p2.apply(&t1)
            }
            TypePredicate::Or(p1, p2) => Type::union(vec![p1.apply(ty), p2.apply(ty)]),
            TypePredicate::Not(p) => p.negate().apply(ty),
            TypePredicate::MatchesPattern(pattern) => narrow_type_by_pattern(ty, pattern),
            TypePredicate::UserDefinedGuard(target) => match ty {
                Type::Union(variants) => {
                    if variants.contains(target) {
                        target.clone()
                    } else if let Type::Union(target_variants) = target {
                        let intersection: Vec<Type> = variants
                            .iter()
                            .filter(|v| target_variants.contains(v))
                            .cloned()
                            .collect();

                        if intersection.is_empty() {
                            target.clone()
                        } else if intersection.len() == 1 {
                            intersection.into_iter().next().unwrap()
                        } else {
                            Type::union(intersection)
                        }
                    } else {
                        target.clone()
                    }
                }
                _ => target.clone(),
            },
        }
    }

    /// Check if a type is always falsy
    fn is_falsy_type(ty: &Type) -> bool {
        matches!(ty, Type::Con(TypeCtor::NoneType))
    }

    /// Get the inverse predicate for narrowing in the else branch of an if statement.
    ///
    /// De Morgan's law
    /// - not (A and B) = (not A) or (not B)
    /// - not (A or B) = (not A) and (not B)
    pub fn negate(&self) -> TypePredicate {
        match self {
            TypePredicate::IsNotNone => TypePredicate::IsNone,
            TypePredicate::IsNone => TypePredicate::IsNotNone,
            TypePredicate::IsInstance(_) => TypePredicate::IsNotNone,
            TypePredicate::IsTruthy => TypePredicate::IsFalsy,
            TypePredicate::IsFalsy => TypePredicate::IsTruthy,
            TypePredicate::And(p1, p2) => TypePredicate::Or(Box::new(p1.negate()), Box::new(p2.negate())),
            TypePredicate::Or(p1, p2) => TypePredicate::And(Box::new(p1.negate()), Box::new(p2.negate())),
            TypePredicate::Not(p) => p.as_ref().clone(),
            // TODO: full pattern analysis
            TypePredicate::MatchesPattern(_) => TypePredicate::Not(Box::new(self.clone())),
            // User-defined type guards don't have simple negation
            TypePredicate::UserDefinedGuard(_) => TypePredicate::Not(Box::new(self.clone())),
        }
    }

    /// Check if this predicate has a meaningful negation for constraint generation
    ///
    /// Some predicates like isinstance don't have simple inverse predicates, and
    /// their negation is handled by type subtraction in detect_inverse_type_guard.
    pub fn has_simple_negation(&self) -> bool {
        !matches!(
            self,
            TypePredicate::IsInstance(_) | TypePredicate::MatchesPattern(_) | TypePredicate::UserDefinedGuard(_)
        )
    }
}

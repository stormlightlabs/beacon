use crate::{Result, Subst, Type, TypeError, TypeVar};
use rustc_hash::FxHashSet;

/// Unification algorithm for Hindley-Milner type system
pub struct Unifier;

impl Unifier {
    /// Unify two types, returning a substitution that makes them equal
    pub fn unify(t1: &Type, t2: &Type) -> Result<Subst> {
        Self::unify_impl(t1, t2)
    }

    fn unify_impl(t1: &Type, t2: &Type) -> Result<Subst> {
        match (t1, t2) {
            (Type::Var(tv1), Type::Var(tv2)) if tv1 == tv2 => Ok(Subst::empty()),
            (Type::Var(tv), t) | (t, Type::Var(tv)) => Self::unify_var(tv, t),
            (Type::Con(tc1), Type::Con(tc2)) if tc1 == tc2 => Ok(Subst::empty()),
            (Type::App(f1, a1), Type::App(f2, a2)) => {
                let s1 = Self::unify_impl(f1, f2)?;
                let s2 = Self::unify_impl(&s1.apply(a1), &s1.apply(a2))?;
                Ok(s2.compose(s1))
            }
            (Type::Fun(args1, ret1), Type::Fun(args2, ret2)) => {
                if args1.len() != args2.len() {
                    return Err(TypeError::UnificationError(
                        format!("function with {} arguments", args1.len()),
                        format!("function with {} arguments", args2.len()),
                    )
                    .into());
                }

                let mut subst = Subst::empty();
                for (arg1, arg2) in args1.iter().zip(args2.iter()) {
                    let s = Self::unify_impl(&subst.apply(arg1), &subst.apply(arg2))?;
                    subst = s.compose(subst);
                }

                let s = Self::unify_impl(&subst.apply(ret1), &subst.apply(ret2))?;
                Ok(s.compose(subst))
            }
            (Type::Union(types1), Type::Union(types2)) => Self::unify_unions(types1, types2),
            (Type::Union(types), t) | (t, Type::Union(types)) => Self::unify_union_with_type(types, t),
            (Type::Record(fields1, row1), Type::Record(fields2, row2)) => {
                Self::unify_records(fields1, row1, fields2, row2)
            }
            (Type::ForAll(_, _), _) | (_, Type::ForAll(_, _)) => {
                Err(TypeError::UnificationError("polymorphic type".to_string(), "monomorphic type".to_string()).into())
            }
            _ => Err(TypeError::UnificationError(t1.to_string(), t2.to_string()).into()),
        }
    }

    /// Unify a type variable with a type (performs occurs check)
    fn unify_var(tv: &TypeVar, t: &Type) -> Result<Subst> {
        match t {
            Type::Var(tv2) if tv == tv2 => Ok(Subst::empty()),
            _ => {
                if Self::occurs_check(tv, t) {
                    Err(TypeError::OccursCheckFailed(tv.clone(), t.to_string()).into())
                } else {
                    Ok(Subst::singleton(tv.clone(), t.clone()))
                }
            }
        }
    }

    /// Occurs check: returns true if type variable occurs in the type
    fn occurs_check(tv: &TypeVar, t: &Type) -> bool {
        match t {
            Type::Var(tv2) => tv == tv2,
            Type::Con(_) => false,
            Type::App(t1, t2) => Self::occurs_check(tv, t1) || Self::occurs_check(tv, t2),
            Type::Fun(args, ret) => args.iter().any(|arg| Self::occurs_check(tv, arg)) || Self::occurs_check(tv, ret),
            Type::ForAll(quantified, t_inner) => {
                if quantified.contains(tv) {
                    false
                } else {
                    Self::occurs_check(tv, t_inner)
                }
            }
            Type::Union(types) => types.iter().any(|t_inner| Self::occurs_check(tv, t_inner)),
            Type::Record(fields, row_var) => {
                fields.iter().any(|(_, field_type)| Self::occurs_check(tv, field_type))
                    || row_var.as_ref().map_or(false, |rv| tv == rv)
            }
        }
    }

    /// Unify two union types
    ///
    /// TODO: Subset relationships
    /// TODO: Find bijections
    fn unify_unions(types1: &[Type], types2: &[Type]) -> Result<Subst> {
        if types1.len() != types2.len() {
            return Err(TypeError::UnificationError(
                format!("union with {} alternatives", types1.len()),
                format!("union with {} alternatives", types2.len()),
            )
            .into());
        }

        let mut sorted1 = types1.to_vec();
        let mut sorted2 = types2.to_vec();
        sorted1.sort();
        sorted2.sort();

        let mut subst = Subst::empty();
        for (t1, t2) in sorted1.iter().zip(sorted2.iter()) {
            let s = Self::unify_impl(&subst.apply(t1), &subst.apply(t2))?;
            subst = s.compose(subst);
        }

        Ok(subst)
    }

    /// Unify a union type with a non-union type
    fn unify_union_with_type(union_types: &[Type], t: &Type) -> Result<Subst> {
        for union_member in union_types {
            if let Ok(subst) = Self::unify_impl(union_member, t) {
                return Ok(subst);
            }
        }

        Err(TypeError::UnificationError(format!("union {}", Type::Union(union_types.to_vec())), t.to_string()).into())
    }

    /// Unify record types (simplified row polymorphism)
    fn unify_records(
        fields1: &[(String, Type)], row1: &Option<TypeVar>, fields2: &[(String, Type)], row2: &Option<TypeVar>,
    ) -> Result<Subst> {
        let mut subst = Subst::empty();
        let map1: std::collections::HashMap<_, _> = fields1.iter().map(|(k, v)| (k, v)).collect();
        let map2: std::collections::HashMap<_, _> = fields2.iter().map(|(k, v)| (k, v)).collect();
        let mut unified_fields = FxHashSet::default();
        for (name, type1) in &map1 {
            if let Some(type2) = map2.get(name) {
                let s = Self::unify_impl(&subst.apply(type1), &subst.apply(type2))?;
                subst = s.compose(subst);
                unified_fields.insert(name);
            }
        }

        let remaining1: Vec<_> = map1.iter().filter(|(name, _)| !unified_fields.contains(name)).collect();
        let remaining2: Vec<_> = map2.iter().filter(|(name, _)| !unified_fields.contains(name)).collect();

        match (remaining1.is_empty(), remaining2.is_empty(), row1, row2) {
            // Both records have same fields, no row variables
            (true, true, None, None) => Ok(subst),
            // Record 1 has extra fields, record 2 has row variable
            (false, true, None, Some(rv2)) => {
                // Bind row variable to record containing the extra fields
                let extra_record = Type::Record(
                    remaining1
                        .into_iter()
                        .map(|(k, v)| ((*k).clone(), (*v).clone()))
                        .collect(),
                    None,
                );
                let s = Subst::singleton(rv2.clone(), extra_record);
                Ok(s.compose(subst))
            }
            (true, false, Some(rv1), None) => {
                let extra_record = Type::Record(
                    remaining2
                        .into_iter()
                        .map(|(k, v)| ((*k).clone(), (*v).clone()))
                        .collect(),
                    None,
                );
                let s = Subst::singleton(rv1.clone(), extra_record);
                Ok(s.compose(subst))
            }
            (_, _, Some(rv1), Some(rv2)) => {
                let s = Self::unify_var(rv1, &Type::Var(rv2.clone()))?;
                Ok(s.compose(subst))
            }
            _ => Err(TypeError::UnificationError(
                format!(
                    "record with fields: {}",
                    fields1.iter().map(|(k, _)| k).cloned().collect::<Vec<_>>().join(", ")
                ),
                format!(
                    "record with fields: {}",
                    fields2.iter().map(|(k, _)| k).cloned().collect::<Vec<_>>().join(", ")
                ),
            )
            .into()),
        }
    }

    /// Unify a list of types (all must unify to the same type)
    pub fn unify_many(types: &[Type]) -> Result<(Type, Subst)> {
        if types.is_empty() {
            return Err(
                TypeError::UnificationError("empty type list".to_string(), "non-empty type list".to_string()).into(),
            );
        }

        if types.len() == 1 {
            return Ok((types[0].clone(), Subst::empty()));
        }

        let mut result_type = types[0].clone();
        let mut subst = Subst::empty();

        for t in &types[1..] {
            let s = Self::unify_impl(&subst.apply(&result_type), &subst.apply(t))?;
            subst = s.compose(subst);
            result_type = subst.apply(&result_type);
        }

        Ok((result_type, subst))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::TypeVar;

    #[test]
    fn test_unify_same_types() {
        let t1 = Type::int();
        let t2 = Type::int();
        let subst = Unifier::unify(&t1, &t2).unwrap();
        assert!(subst.is_empty());
    }

    #[test]
    fn test_unify_type_variable() {
        let tv = TypeVar::new(0);
        let t = Type::int();
        let subst = Unifier::unify(&Type::Var(tv.clone()), &t).unwrap();

        assert_eq!(subst.get(&tv), Some(&Type::int()));
        assert_eq!(subst.apply(&Type::Var(tv)), Type::int());
    }

    #[test]
    fn test_unify_function_types() {
        let t1 = Type::fun(vec![Type::int()], Type::string());
        let t2 = Type::fun(vec![Type::int()], Type::string());
        let subst = Unifier::unify(&t1, &t2).unwrap();
        assert!(subst.is_empty());
    }

    #[test]
    fn test_unify_function_with_variables() {
        let tv1 = TypeVar::new(0);
        let tv2 = TypeVar::new(1);
        let t1 = Type::fun(vec![Type::Var(tv1.clone())], Type::Var(tv2.clone()));
        let t2 = Type::fun(vec![Type::int()], Type::string());

        let subst = Unifier::unify(&t1, &t2).unwrap();

        assert_eq!(subst.get(&tv1), Some(&Type::int()));
        assert_eq!(subst.get(&tv2), Some(&Type::string()));
    }

    #[test]
    fn test_unify_type_applications() {
        let tv = TypeVar::new(0);
        let t1 = Type::list(Type::Var(tv.clone()));
        let t2 = Type::list(Type::int());

        let subst = Unifier::unify(&t1, &t2).unwrap();
        assert_eq!(subst.get(&tv), Some(&Type::int()));
    }

    // #[test]
    // fn test_occurs_check() {
    //     let tv = TypeVar::new(0);
    //     let recursive_type = Type::list(Type::Var(tv.clone()));

    //     let result = Unifier::unify(&Type::Var(tv.clone()), &recursive_type);
    //     assert!(matches!(result.err(), Some(TypeError::OccursCheckFailed(_, _).into())));
    // }

    #[test]
    fn test_unify_union_types() {
        let union1 = Type::union(vec![Type::int(), Type::string()]);
        let union2 = Type::union(vec![Type::string(), Type::int()]);

        let subst = Unifier::unify(&union1, &union2).unwrap();
        assert!(subst.is_empty());
    }

    #[test]
    fn test_unify_union_with_type() {
        let union = Type::union(vec![Type::int(), Type::string()]);
        let t = Type::int();

        let subst = Unifier::unify(&union, &t).unwrap();
        assert!(subst.is_empty());
    }

    #[test]
    fn test_unify_records() {
        let record1 = Type::Record(vec![("x".to_string(), Type::int())], None);
        let record2 = Type::Record(vec![("x".to_string(), Type::int())], None);

        let subst = Unifier::unify(&record1, &record2).unwrap();
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

        let subst = Unifier::unify(&record1, &record2).unwrap();

        if let Some(Type::Record(fields, _)) = subst.get(&row_var) {
            assert_eq!(fields.len(), 1);
            assert_eq!(fields[0].0, "y");
            assert_eq!(fields[0].1, Type::string());
        } else {
            panic!("Row variable should be bound to a record");
        }
    }

    // #[test]
    // fn test_unification_failure() {
    //     let t1 = Type::int();
    //     let t2 = Type::string();

    //     let result = Unifier::unify(&t1, &t2);
    //     assert!(matches!(result, Err(TypeError::UnificationError(_, _))));
    // }

    #[test]
    fn test_unify_many() {
        let tv1 = TypeVar::new(0);
        let tv2 = TypeVar::new(1);
        let types = vec![Type::Var(tv1.clone()), Type::int(), Type::Var(tv2.clone())];

        let (unified_type, subst) = Unifier::unify_many(&types).unwrap();

        assert_eq!(unified_type, Type::int());
        assert_eq!(subst.get(&tv1), Some(&Type::int()));
        assert_eq!(subst.get(&tv2), Some(&Type::int()));
    }
}

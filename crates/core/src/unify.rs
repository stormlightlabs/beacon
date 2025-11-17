//! Unification Algorithm for the Hindley-Milner Type System
//!
//! This module implements Robinson's unification algorithm with extensions for Python types.
//!
//! # Unification Rules
//!
//! The unifier finds the most general substitution that makes two types equal:
//!
//! - **Type Variables**: `'a ~ T` produces substitution `['a ↦ T]` if occurs check passes
//! - **Type Constructors**: `C ~ C` succeeds with empty substitution
//! - **Type Applications**: `F A ~ G B` unifies `F ~ G` and `A ~ B`
//! - **Function Types**: `(A₁,...,Aₙ) -> R ~ (B₁,...,Bₙ) -> S` unifies arguments and return types
//!
//! # Special Types
//!
//! - **`Any`**: Unifies with everything (returns empty substitution)
//! - **`Top`**: Only unifies with itself or type variables
//! - **`Never`**: Only unifies with itself or type variables
//!
//! # Row Polymorphism
//!
//! Records with row variables support flexible unification:
//!
//! ```text
//! { x: int | r } ~ { x: int, y: str }
//! // Produces substitution: r ↦ { y: str }
//! ```
//!
//! # Occurs Check
//!
//! The occurs check prevents infinite types by rejecting unifications like `'a ~ list['a]`.

use crate::{Result, Subst, Type, TypeCtor, TypeError, TypeVar, TypeVarConstraintRegistry, Variance};

use rustc_hash::FxHashSet;

/// Unification algorithm for Hindley-Milner type system
pub struct Unifier;

impl Unifier {
    /// Unify two types, returning a substitution that makes them equal
    ///
    /// The TypeVarConstraintRegistry is used to validate that any TypeVar substitutions
    /// respect bounds and constraints defined for those TypeVars.
    pub fn unify(t1: &Type, t2: &Type, registry: &TypeVarConstraintRegistry) -> Result<Subst> {
        Self::unify_impl(t1, t2, registry)
    }

    /// Extract a human-readable name from a type constructor for error messages
    fn get_type_constructor_name(ty: &Type) -> String {
        match ty {
            Type::Con(TypeCtor::List) => "list element".to_string(),
            Type::Con(TypeCtor::Dict) => "dict key/value".to_string(),
            Type::Con(TypeCtor::Set) => "set element".to_string(),
            Type::Con(TypeCtor::Tuple) => "tuple element".to_string(),
            Type::Con(TypeCtor::Iterator) => "iterator element".to_string(),
            Type::Con(TypeCtor::Iterable) => "iterable element".to_string(),
            Type::Con(TypeCtor::Generator) => "generator type argument".to_string(),
            Type::Con(TypeCtor::AsyncGenerator) => "async generator type argument".to_string(),
            Type::Con(TypeCtor::Coroutine) => "coroutine type argument".to_string(),
            Type::Con(TypeCtor::Class(name)) => format!("{name} type argument"),
            Type::App(ctor, _) => Self::get_type_constructor_name(ctor),
            _ => "type argument".to_string(),
        }
    }

    /// Extract the root type constructor and count parameters from a type application chain.
    ///
    /// For example:
    /// - `Dict[K, V]` = `App(App(Dict, K), V)` → `(Dict, 2)`
    /// - `List[T]` = `App(List, T)` → `(List, 1)`
    /// - `Generator[Y, S, R]` = `App(App(App(Gen, Y), S), R)` → `(Generator, 3)`
    fn extract_type_ctor_and_param_count(ty: &Type) -> Option<(&TypeCtor, usize)> {
        let mut current = ty;
        let mut param_count = 0;

        while let Type::App(ctor, _) = current {
            param_count += 1;
            current = ctor.as_ref();
        }

        if let Type::Con(tc) = current { Some((tc, param_count)) } else { None }
    }

    /// Get the variance for a specific parameter index in a type application chain.
    ///
    /// Multi-parameter types like `Dict[K, V]` are represented as nested applications:
    /// `App(App(Dict, K), V)` where K is at parameter 0 and V is at parameter 1.
    ///
    /// This function determines which parameter position we're at based on the nesting depth.
    fn get_variance_for_app(f: &Type) -> Variance {
        if let Some((root_ctor, total_params)) = Self::extract_type_ctor_and_param_count(f) {
            let param_index = total_params.saturating_sub(1);
            root_ctor.variance(param_index)
        } else {
            Variance::Invariant
        }
    }

    fn unify_impl(t1: &Type, t2: &Type, registry: &TypeVarConstraintRegistry) -> Result<Subst> {
        match (t1, t2) {
            (Type::Con(TypeCtor::Any), _) | (_, Type::Con(TypeCtor::Any)) => Ok(Subst::empty()),
            (Type::Var(tv1), Type::Var(tv2)) if tv1 == tv2 => Ok(Subst::empty()),
            (Type::Var(tv), t) | (t, Type::Var(tv)) => Self::unify_var(tv, t, registry),
            (Type::Con(tc1), Type::Con(tc2)) if tc1 == tc2 => Ok(Subst::empty()),
            (Type::App(f1, a1), Type::App(f2, a2)) => {
                let s1 = Self::unify_impl(f1, f2, registry)?;

                let applied_a1 = s1.apply(a1);
                let applied_a2 = s1.apply(a2);

                let variance = Self::get_variance_for_app(f1);

                match variance {
                    Variance::Invariant => {
                        if applied_a1 != applied_a2 {
                            match Self::unify_impl(&applied_a1, &applied_a2, registry) {
                                Ok(s2) => Ok(s2.compose(s1)),
                                Err(_)
                                    if !matches!(applied_a1, Type::Var(_)) && !matches!(applied_a2, Type::Var(_)) =>
                                {
                                    Err(TypeError::VarianceError {
                                        position: Self::get_type_constructor_name(f1),
                                        expected_variance: "invariant".to_string(),
                                        got_type: applied_a1.to_string(),
                                        expected_type: applied_a2.to_string(),
                                    }
                                    .into())
                                }
                                Err(e) => Err(e),
                            }
                        } else {
                            Ok(s1)
                        }
                    }
                    Variance::Covariant => {
                        let s2 = Self::unify_impl(&applied_a1, &applied_a2, registry)?;
                        Ok(s2.compose(s1))
                    }
                    Variance::Contravariant => {
                        let s2 = Self::unify_impl(&applied_a2, &applied_a1, registry)?;
                        Ok(s2.compose(s1))
                    }
                }
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
                for ((_, ty1), (_, ty2)) in args1.iter().zip(args2.iter()) {
                    let s = Self::unify_impl(&subst.apply(ty1), &subst.apply(ty2), registry)?;
                    subst = s.compose(subst);
                }

                let s = Self::unify_impl(&subst.apply(ret1), &subst.apply(ret2), registry)?;
                Ok(s.compose(subst))
            }
            (Type::Union(types1), Type::Union(types2)) => Self::unify_unions(types1, types2, registry),
            (Type::Union(types), t) | (t, Type::Union(types)) => Self::unify_union_with_type(types, t, registry),
            (Type::Record(fields1, row1), Type::Record(fields2, row2)) => {
                Self::unify_records(fields1, row1, fields2, row2, registry)
            }
            (Type::BoundMethod(receiver1, _, method1), Type::BoundMethod(receiver2, _, method2)) => {
                let s1 = Self::unify_impl(receiver1, receiver2, registry)?;
                let s2 = Self::unify_impl(&s1.apply(method1), &s1.apply(method2), registry)?;
                Ok(s2.compose(s1))
            }
            (Type::BoundMethod(_, _, method), fun @ Type::Fun(_, _)) => Self::unify_impl(method, fun, registry),
            (fun @ Type::Fun(_, _), Type::BoundMethod(_, _, method)) => Self::unify_impl(fun, method, registry),
            (Type::BoundMethod(_, _, method), other)
                if !matches!(other, Type::BoundMethod(_, _, _) | Type::Fun(_, _)) =>
            {
                Self::unify_impl(method, other, registry)
            }
            (other, Type::BoundMethod(_, _, method))
                if !matches!(other, Type::BoundMethod(_, _, _) | Type::Fun(_, _)) =>
            {
                Self::unify_impl(other, method, registry)
            }
            (Type::Tuple(types1), Type::Tuple(types2)) => {
                if types1.len() != types2.len() {
                    return Err(TypeError::UnificationError(
                        format!("tuple with {} elements", types1.len()),
                        format!("tuple with {} elements", types2.len()),
                    )
                    .into());
                }

                let mut subst = Subst::empty();
                for (t1, t2) in types1.iter().zip(types2.iter()) {
                    let s = Self::unify_impl(&subst.apply(t1), &subst.apply(t2), registry)?;
                    subst = s.compose(subst);
                }
                Ok(subst)
            }
            (Type::ForAll(quantified1, body1), Type::ForAll(quantified2, body2)) => {
                if quantified1.len() != quantified2.len() {
                    return Err(TypeError::UnificationError(
                        format!("ForAll with {} type parameters", quantified1.len()),
                        format!("ForAll with {} type parameters", quantified2.len()),
                    )
                    .into());
                }

                let mut renaming = Subst::empty();
                for (tv1, tv2) in quantified1.iter().zip(quantified2.iter()) {
                    renaming.insert(tv1.clone(), Type::Var(tv2.clone()));
                }

                let renamed_body1 = renaming.apply(body1);
                Self::unify_impl(&renamed_body1, body2, registry)
            }
            (Type::ForAll(_, body), t) | (t, Type::ForAll(_, body)) => Self::unify_impl(body, t, registry),
            (Type::Con(TypeCtor::TypeVariable(_)), _) | (_, Type::Con(TypeCtor::TypeVariable(_))) => Ok(Subst::empty()),
            _ => Err(TypeError::UnificationError(t1.to_string(), t2.to_string()).into()),
        }
    }

    /// Unify a type variable with a type (performs occurs check and validates bounds/constraints)
    fn unify_var(tv: &TypeVar, t: &Type, registry: &TypeVarConstraintRegistry) -> Result<Subst> {
        match t {
            Type::Var(tv2) if tv == tv2 => Ok(Subst::empty()),
            _ => {
                if Self::occurs_check(tv, t) {
                    return Err(TypeError::OccursCheckFailed(tv.clone(), t.to_string()).into());
                }

                if let Err(msg) = registry.validate(tv.id, t) {
                    return Err(TypeError::UnificationError(format!("TypeVar {tv}"), format!("{t} ({msg})")).into());
                }

                Ok(Subst::singleton(tv.clone(), t.clone()))
            }
        }
    }

    /// Occurs check: returns true if type variable occurs in the type
    fn occurs_check(tv: &TypeVar, t: &Type) -> bool {
        match t {
            Type::Var(tv2) => tv == tv2,
            Type::Con(_) => false,
            Type::App(t1, t2) => Self::occurs_check(tv, t1) || Self::occurs_check(tv, t2),
            Type::Fun(args, ret) => {
                args.iter().any(|(_, ty)| Self::occurs_check(tv, ty)) || Self::occurs_check(tv, ret)
            }
            Type::ForAll(quantified, t_inner) => {
                if quantified.contains(tv) {
                    false
                } else {
                    Self::occurs_check(tv, t_inner)
                }
            }
            Type::Union(types) => types.iter().any(|t_inner| Self::occurs_check(tv, t_inner)),
            Type::Intersection(types) => types.iter().any(|t_inner| Self::occurs_check(tv, t_inner)),
            Type::Record(fields, row_var) => {
                fields.iter().any(|(_, field_type)| Self::occurs_check(tv, field_type))
                    || (row_var.as_ref() == Some(tv))
            }
            Type::BoundMethod(receiver, _, method) => {
                Self::occurs_check(tv, receiver) || Self::occurs_check(tv, method)
            }
            Type::Tuple(types) => types.iter().any(|t_inner| Self::occurs_check(tv, t_inner)),
        }
    }

    /// Unify two union types
    ///
    /// TODO: Subset relationships
    /// TODO: Find bijections
    fn unify_unions(types1: &[Type], types2: &[Type], registry: &TypeVarConstraintRegistry) -> Result<Subst> {
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
            let s = Self::unify_impl(&subst.apply(t1), &subst.apply(t2), registry)?;
            subst = s.compose(subst);
        }

        Ok(subst)
    }

    /// Unify a union type with a non-union type
    ///
    /// When unifying `Union[T1, T2, ..., Tn]` with concrete type `C`:
    /// - Finds which `Ti` can unify with `C`
    /// - Returns substitution that makes `Ti = C`, including any type variable constraints
    /// - The first successfully unifying member is selected
    ///
    /// # Union Narrowing
    ///
    /// When a Union unifies with a concrete type, the Union should conceptually be "narrowed"
    /// to that type. However, the unifier only returns substitutions for type variables within
    /// the Union members. The constraint solver must handle Union elimination by recognizing
    /// when a Union type becomes equivalent to one of its members after substitution.
    ///
    /// # Examples
    ///
    /// - `Union[Var('t), None]` ~ `int` → `['t ↦ int]`
    /// - `Union[int, str]` ~ `int` → empty substitution (picks int branch)
    /// - `Union[Calculator, None]` ~ `None` → empty substitution (picks None branch)
    fn unify_union_with_type(union_types: &[Type], t: &Type, registry: &TypeVarConstraintRegistry) -> Result<Subst> {
        let mut errors = Vec::new();

        for union_member in union_types {
            match Self::unify_impl(union_member, t, registry) {
                Ok(subst) => {
                    return Ok(subst);
                }
                Err(e) => {
                    errors.push(format!("  {union_member} with {t}: {e}"));
                }
            }
        }

        let union_str = Type::Union(union_types.to_vec()).to_string();
        let error_details = if errors.len() > 1 {
            format!("\nAttempted unifications:\n{}", errors.join("\n"))
        } else {
            String::new()
        };

        Err(TypeError::UnificationError(format!("union {union_str}{error_details}"), t.to_string()).into())
    }

    /// Unify record types (simplified row polymorphism)
    fn unify_records(
        fields1: &[(String, Type)], row1: &Option<TypeVar>, fields2: &[(String, Type)], row2: &Option<TypeVar>,
        registry: &TypeVarConstraintRegistry,
    ) -> Result<Subst> {
        let mut subst = Subst::empty();
        let map1: std::collections::HashMap<_, _> = fields1.iter().map(|(k, v)| (k, v)).collect();
        let map2: std::collections::HashMap<_, _> = fields2.iter().map(|(k, v)| (k, v)).collect();
        let mut unified_fields = FxHashSet::default();
        for (name, type1) in &map1 {
            if let Some(type2) = map2.get(name) {
                let s = Self::unify_impl(&subst.apply(type1), &subst.apply(type2), registry)?;
                subst = s.compose(subst);
                unified_fields.insert(name);
            }
        }

        let remaining1: Vec<_> = map1.iter().filter(|(name, _)| !unified_fields.contains(name)).collect();
        let remaining2: Vec<_> = map2.iter().filter(|(name, _)| !unified_fields.contains(name)).collect();

        match (remaining1.is_empty(), remaining2.is_empty(), row1, row2) {
            (true, true, None, None) => Ok(subst),
            (false, true, None, Some(rv2)) => {
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
                let s = Self::unify_var(rv1, &Type::Var(rv2.clone()), registry)?;
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
    pub fn unify_many(types: &[Type], registry: &TypeVarConstraintRegistry) -> Result<(Type, Subst)> {
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
            let s = Self::unify_impl(&subst.apply(&result_type), &subst.apply(t), registry)?;
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
        let subst = Unifier::unify(&t1, &t2, &TypeVarConstraintRegistry::new()).unwrap();
        assert!(subst.is_empty());
    }

    #[test]
    fn test_unify_type_variable() {
        let tv = TypeVar::new(0);
        let t = Type::int();
        let subst = Unifier::unify(&Type::Var(tv.clone()), &t, &TypeVarConstraintRegistry::new()).unwrap();
        assert_eq!(subst.get(&tv), Some(&Type::int()));
        assert_eq!(subst.apply(&Type::Var(tv)), Type::int());
    }

    #[test]
    fn test_unify_function_types() {
        let t1 = Type::fun_unnamed(vec![Type::int()], Type::string());
        let t2 = Type::fun_unnamed(vec![Type::int()], Type::string());
        let subst = Unifier::unify(&t1, &t2, &TypeVarConstraintRegistry::new()).unwrap();
        assert!(subst.is_empty());
    }

    #[test]
    fn test_unify_function_with_variables() {
        let tv1 = TypeVar::new(0);
        let tv2 = TypeVar::new(1);
        let t1 = Type::fun_unnamed(vec![Type::Var(tv1.clone())], Type::Var(tv2.clone()));
        let t2 = Type::fun_unnamed(vec![Type::int()], Type::string());

        let subst = Unifier::unify(&t1, &t2, &TypeVarConstraintRegistry::new()).unwrap();

        assert_eq!(subst.get(&tv1), Some(&Type::int()));
        assert_eq!(subst.get(&tv2), Some(&Type::string()));
    }

    #[test]
    fn test_unify_type_applications() {
        let tv = TypeVar::new(0);
        let t1 = Type::list(Type::Var(tv.clone()));
        let t2 = Type::list(Type::int());
        let subst = Unifier::unify(&t1, &t2, &TypeVarConstraintRegistry::new()).unwrap();
        assert_eq!(subst.get(&tv), Some(&Type::int()));
    }

    #[test]
    fn test_occurs_check_basic() {
        let tv = TypeVar::new(0);
        let recursive_type = Type::list(Type::Var(tv.clone()));
        let result = Unifier::unify(
            &Type::Var(tv.clone()),
            &recursive_type,
            &TypeVarConstraintRegistry::new(),
        );
        assert!(result.is_err());
    }

    #[test]
    fn test_occurs_check_in_nested_app() {
        let tv = TypeVar::new(0);
        let nested = Type::list(Type::list(Type::Var(tv.clone())));
        let result = Unifier::unify(&Type::Var(tv.clone()), &nested, &TypeVarConstraintRegistry::new());
        assert!(result.is_err());
    }

    #[test]
    fn test_occurs_check_in_function_args() {
        let tv = TypeVar::new(0);
        let fun_type = Type::fun_unnamed(vec![Type::Var(tv.clone())], Type::int());
        let result = Unifier::unify(&Type::Var(tv.clone()), &fun_type, &TypeVarConstraintRegistry::new());
        assert!(result.is_err());
    }

    #[test]
    fn test_occurs_check_in_function_return() {
        let tv = TypeVar::new(0);
        let fun_type = Type::fun_unnamed(vec![Type::int()], Type::Var(tv.clone()));
        let result = Unifier::unify(&Type::Var(tv.clone()), &fun_type, &TypeVarConstraintRegistry::new());
        assert!(result.is_err());
    }

    #[test]
    fn test_occurs_check_in_union() {
        let tv = TypeVar::new(0);
        let union = Type::union(vec![Type::int(), Type::Var(tv.clone())]);
        let result = Unifier::unify(&Type::Var(tv.clone()), &union, &TypeVarConstraintRegistry::new());
        assert!(result.is_err());
    }

    #[test]
    fn test_occurs_check_in_record_fields() {
        let tv = TypeVar::new(0);
        let record = Type::Record(vec![("x".to_string(), Type::Var(tv.clone()))], None);
        let result = Unifier::unify(&Type::Var(tv.clone()), &record, &TypeVarConstraintRegistry::new());
        assert!(result.is_err());
    }

    #[test]
    fn test_occurs_check_in_record_row_var() {
        let tv = TypeVar::new(0);
        let record = Type::Record(vec![("x".to_string(), Type::int())], Some(tv.clone()));
        let result = Unifier::unify(&Type::Var(tv.clone()), &record, &TypeVarConstraintRegistry::new());
        assert!(result.is_err());
    }

    #[test]
    fn test_occurs_check_does_not_trigger_for_different_vars() {
        let tv1 = TypeVar::new(0);
        let tv2 = TypeVar::new(1);
        let list_type = Type::list(Type::Var(tv2.clone()));
        let result = Unifier::unify(&Type::Var(tv1.clone()), &list_type, &TypeVarConstraintRegistry::new());
        assert!(result.is_ok());
        let subst = result.unwrap();
        assert_eq!(subst.get(&tv1), Some(&Type::list(Type::Var(tv2))));
    }

    #[test]
    fn test_unify_union_types() {
        let union1 = Type::union(vec![Type::int(), Type::string()]);
        let union2 = Type::union(vec![Type::string(), Type::int()]);
        let subst = Unifier::unify(&union1, &union2, &TypeVarConstraintRegistry::new()).unwrap();
        assert!(subst.is_empty());
    }

    #[test]
    fn test_unify_union_with_type() {
        let union = Type::union(vec![Type::int(), Type::string()]);
        let t = Type::int();

        let subst = Unifier::unify(&union, &t, &TypeVarConstraintRegistry::new()).unwrap();
        assert!(subst.is_empty());
    }

    #[test]
    fn test_unify_records() {
        let record1 = Type::Record(vec![("x".to_string(), Type::int())], None);
        let record2 = Type::Record(vec![("x".to_string(), Type::int())], None);

        let subst = Unifier::unify(&record1, &record2, &TypeVarConstraintRegistry::new()).unwrap();
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

        let subst = Unifier::unify(&record1, &record2, &TypeVarConstraintRegistry::new()).unwrap();

        if let Some(Type::Record(fields, _)) = subst.get(&row_var) {
            assert_eq!(fields.len(), 1);
            assert_eq!(fields[0].0, "y");
            assert_eq!(fields[0].1, Type::string());
        } else {
            panic!("Row variable should be bound to a record");
        }
    }

    #[test]
    fn test_unify_many() {
        let tv1 = TypeVar::new(0);
        let tv2 = TypeVar::new(1);
        let types = vec![Type::Var(tv1.clone()), Type::int(), Type::Var(tv2.clone())];

        let (unified_type, subst) = Unifier::unify_many(&types, &TypeVarConstraintRegistry::new()).unwrap();

        assert_eq!(unified_type, Type::int());
        assert_eq!(subst.get(&tv1), Some(&Type::int()));
        assert_eq!(subst.get(&tv2), Some(&Type::int()));
    }

    #[test]
    fn test_any_unifies_with_everything() {
        let subst = Unifier::unify(&Type::any(), &Type::int(), &TypeVarConstraintRegistry::new()).unwrap();
        assert!(subst.is_empty());

        let subst = Unifier::unify(&Type::string(), &Type::any(), &TypeVarConstraintRegistry::new()).unwrap();
        assert!(subst.is_empty());

        let tv = TypeVar::new(0);
        let subst = Unifier::unify(&Type::any(), &Type::Var(tv.clone()), &TypeVarConstraintRegistry::new()).unwrap();
        assert!(subst.is_empty());

        let subst = Unifier::unify(
            &Type::any(),
            &Type::list(Type::int()),
            &TypeVarConstraintRegistry::new(),
        )
        .unwrap();
        assert!(subst.is_empty());

        let subst = Unifier::unify(
            &Type::fun_unnamed(vec![Type::int()], Type::string()),
            &Type::any(),
            &TypeVarConstraintRegistry::new(),
        )
        .unwrap();
        assert!(subst.is_empty());
    }

    #[test]
    fn test_top_unifies_only_with_itself() {
        let subst = Unifier::unify(&Type::top(), &Type::top(), &TypeVarConstraintRegistry::new()).unwrap();
        assert!(subst.is_empty());

        assert!(Unifier::unify(&Type::top(), &Type::int(), &TypeVarConstraintRegistry::new()).is_err());
        assert!(Unifier::unify(&Type::string(), &Type::top(), &TypeVarConstraintRegistry::new()).is_err());

        let tv = TypeVar::new(0);
        let subst = Unifier::unify(&Type::top(), &Type::Var(tv.clone()), &TypeVarConstraintRegistry::new()).unwrap();
        assert_eq!(subst.get(&tv), Some(&Type::top()));
    }

    #[test]
    fn test_never_unifies_only_with_itself() {
        let subst = Unifier::unify(&Type::never(), &Type::never(), &TypeVarConstraintRegistry::new()).unwrap();
        assert!(subst.is_empty());

        assert!(Unifier::unify(&Type::never(), &Type::int(), &TypeVarConstraintRegistry::new()).is_err());
        assert!(Unifier::unify(&Type::bool(), &Type::never(), &TypeVarConstraintRegistry::new()).is_err());

        let tv = TypeVar::new(0);
        let subst = Unifier::unify(
            &Type::never(),
            &Type::Var(tv.clone()),
            &TypeVarConstraintRegistry::new(),
        )
        .unwrap();
        assert_eq!(subst.get(&tv), Some(&Type::never()));
    }

    #[test]
    fn test_top_any_never_distinct_unification() {
        let subst = Unifier::unify(&Type::top(), &Type::any(), &TypeVarConstraintRegistry::new()).unwrap();
        assert!(subst.is_empty());
        let subst = Unifier::unify(&Type::any(), &Type::top(), &TypeVarConstraintRegistry::new()).unwrap();
        assert!(subst.is_empty());

        assert!(Unifier::unify(&Type::top(), &Type::never(), &TypeVarConstraintRegistry::new()).is_err());
        assert!(Unifier::unify(&Type::never(), &Type::top(), &TypeVarConstraintRegistry::new()).is_err());

        let subst = Unifier::unify(&Type::any(), &Type::never(), &TypeVarConstraintRegistry::new()).unwrap();
        assert!(subst.is_empty());
        let subst = Unifier::unify(&Type::never(), &Type::any(), &TypeVarConstraintRegistry::new()).unwrap();
        assert!(subst.is_empty());
    }

    #[test]
    fn test_any_in_complex_types() {
        let f1 = Type::fun_unnamed(vec![Type::any()], Type::int());
        let f2 = Type::fun_unnamed(vec![Type::string()], Type::int());
        let subst = Unifier::unify(&f1, &f2, &TypeVarConstraintRegistry::new()).unwrap();
        assert!(subst.is_empty());

        let list_any = Type::list(Type::any());
        let list_int = Type::list(Type::int());
        let subst = Unifier::unify(&list_any, &list_int, &TypeVarConstraintRegistry::new()).unwrap();
        assert!(subst.is_empty());
    }

    #[test]
    fn test_record_row_variable_round_trip() {
        let row_var = TypeVar::new(10);
        let record1 = Type::Record(vec![("x".to_string(), Type::int())], Some(row_var.clone()));
        let record2 = Type::Record(
            vec![("x".to_string(), Type::int()), ("y".to_string(), Type::string())],
            None,
        );

        let subst = Unifier::unify(&record1, &record2, &TypeVarConstraintRegistry::new()).unwrap();

        match subst.get(&row_var).expect("Row variable should be bound") {
            Type::Record(fields, None) => {
                assert_eq!(fields.len(), 1);
                assert_eq!(fields[0].0, "y");
                assert_eq!(fields[0].1, Type::string());
            }
            _ => panic!("Expected record binding for row variable"),
        }

        match subst.apply(&record1) {
            Type::Record(fields, row_var) => {
                assert!(row_var.is_none() || fields.len() == 1);
            }
            _ => panic!("Expected record after substitution"),
        }
    }

    #[test]
    fn test_record_unification_idempotence() {
        let record = Type::Record(
            vec![("x".to_string(), Type::int()), ("y".to_string(), Type::string())],
            None,
        );

        let subst = Unifier::unify(&record, &record, &TypeVarConstraintRegistry::new()).unwrap();
        assert!(subst.is_empty());

        let result = subst.apply(&record);
        assert_eq!(result, record);
    }

    #[test]
    fn test_record_with_nested_row_variables() {
        let row_var1 = TypeVar::new(11);
        let row_var2 = TypeVar::new(12);

        let record1 = Type::Record(vec![("x".to_string(), Type::int())], Some(row_var1.clone()));
        let record2 = Type::Record(vec![("x".to_string(), Type::int())], Some(row_var2.clone()));

        let subst = Unifier::unify(&record1, &record2, &TypeVarConstraintRegistry::new()).unwrap();
        assert!(subst.contains_var(&row_var1) || subst.contains_var(&row_var2) || subst.is_empty());
    }

    #[test]
    fn test_complex_record_round_trip() {
        let tv1 = TypeVar::new(20);
        let tv2 = TypeVar::new(21);
        let record = Type::Record(
            vec![
                ("x".to_string(), Type::Var(tv1.clone())),
                ("y".to_string(), Type::Var(tv2.clone())),
            ],
            None,
        );

        let subst1 = Subst::singleton(tv1.clone(), Type::int());
        let record_after_1 = subst1.apply(&record);

        let subst2 = Subst::singleton(tv2, Type::string());
        let record_after_2 = subst2.apply(&record_after_1);

        match record_after_2 {
            Type::Record(fields, None) => {
                assert_eq!(fields.len(), 2);
                assert_eq!(fields.iter().find(|(k, _)| k == "x").unwrap().1, Type::int());
                assert_eq!(fields.iter().find(|(k, _)| k == "y").unwrap().1, Type::string());
            }
            _ => panic!("Expected concrete record"),
        }
    }

    #[test]
    fn test_union_with_type_var_unifies_with_concrete() {
        let tv = TypeVar::new(30);
        let union = Type::union(vec![Type::Var(tv.clone()), Type::none()]);
        let concrete = Type::int();

        let subst = Unifier::unify(&union, &concrete, &TypeVarConstraintRegistry::new()).unwrap();
        assert_eq!(subst.get(&tv), Some(&Type::int()));

        let union_after = subst.apply(&union);
        match union_after {
            Type::Union(types) => {
                assert_eq!(types.len(), 2);
                assert!(types.contains(&Type::int()));
                assert!(types.contains(&Type::none()));
            }
            _ => panic!("Expected Union type after substitution"),
        }
    }

    #[test]
    fn test_union_concrete_types_unifies_with_member() {
        let union = Type::union(vec![Type::int(), Type::string()]);
        let concrete = Type::int();

        let subst = Unifier::unify(&union, &concrete, &TypeVarConstraintRegistry::new()).unwrap();
        assert!(subst.is_empty());

        let union2 = Type::union(vec![Type::int(), Type::none()]);
        let none = Type::none();
        let subst2 = Unifier::unify(&union2, &none, &TypeVarConstraintRegistry::new()).unwrap();
        assert!(subst2.is_empty());
    }

    #[test]
    fn test_union_fails_when_no_member_matches() {
        let union = Type::union(vec![Type::int(), Type::string()]);
        let concrete = Type::bool();
        let result = Unifier::unify(&union, &concrete, &TypeVarConstraintRegistry::new());
        assert!(result.is_err());
    }

    #[test]
    fn test_optional_type_pattern() {
        let tv = TypeVar::new(31);
        let optional_t = Type::union(vec![Type::Var(tv.clone()), Type::none()]);

        let concrete = Type::string();
        let subst = Unifier::unify(&optional_t, &concrete, &TypeVarConstraintRegistry::new()).unwrap();
        assert_eq!(subst.get(&tv), Some(&Type::string()));

        let tv2 = TypeVar::new(310);
        let optional_t2 = Type::union(vec![Type::Var(tv2.clone()), Type::none()]);
        let none = Type::none();
        let subst2 = Unifier::unify(&optional_t2, &none, &TypeVarConstraintRegistry::new()).unwrap();
        assert!(subst2.is_empty() || subst2.get(&tv2) == Some(&Type::none()));
    }

    #[test]
    fn test_union_with_multiple_type_vars() {
        let tv1 = TypeVar::new(32);
        let tv2 = TypeVar::new(33);
        let union = Type::union(vec![Type::Var(tv1.clone()), Type::Var(tv2.clone())]);
        let concrete = Type::int();

        let subst = Unifier::unify(&union, &concrete, &TypeVarConstraintRegistry::new()).unwrap();
        assert_eq!(subst.get(&tv1), Some(&Type::int()));
        assert!(!subst.contains_var(&tv2));
    }

    #[test]
    fn test_union_type_var_flow() {
        let tv_registry = TypeVarConstraintRegistry::new();
        let tv_union_member = TypeVar::new(34);
        let tv_result = TypeVar::new(35);
        let union = Type::union(vec![Type::Var(tv_union_member.clone()), Type::none()]);
        let subst1 = Unifier::unify(&Type::Var(tv_result.clone()), &union, &TypeVarConstraintRegistry::new()).unwrap();

        assert_eq!(subst1.get(&tv_result), Some(&union));

        let union_type = subst1.get(&tv_result).unwrap();
        let subst2 = Unifier::unify(union_type, &Type::int(), &tv_registry).unwrap();
        assert_eq!(subst2.get(&tv_union_member), Some(&Type::int()));

        let final_subst = subst2.compose(subst1);
        let result_type = final_subst.apply(&Type::Var(tv_result));

        match result_type {
            Type::Union(types) => {
                assert_eq!(types.len(), 2);
                assert!(types.contains(&Type::int()));
                assert!(types.contains(&Type::none()));
            }
            _ => panic!("Expected Union[int, None] but got: {result_type}"),
        }
    }

    #[test]
    fn test_union_with_complex_types() {
        let tv = TypeVar::new(36);
        let list_t = Type::list(Type::Var(tv.clone()));
        let union = Type::union(vec![list_t, Type::none()]);
        let list_int = Type::list(Type::int());

        let subst = Unifier::unify(&union, &list_int, &TypeVarConstraintRegistry::new()).unwrap();
        assert_eq!(subst.get(&tv), Some(&Type::int()));
    }

    #[test]
    fn test_union_error_messages() {
        let union = Type::union(vec![Type::int(), Type::string()]);
        let concrete = Type::bool();

        match Unifier::unify(&union, &concrete, &TypeVarConstraintRegistry::new()) {
            Err(e) => {
                let msg = format!("{e}");
                assert!(msg.contains("union") || msg.contains("Union"));
            }
            Ok(_) => panic!("Expected unification to fail"),
        }
    }

    #[test]
    fn test_heterogeneous_tuple_unify_same() {
        let tuple1 = Type::tuple_heterogeneous(vec![Type::int(), Type::string(), Type::bool()]);
        let tuple2 = Type::tuple_heterogeneous(vec![Type::int(), Type::string(), Type::bool()]);
        let subst = Unifier::unify(&tuple1, &tuple2, &TypeVarConstraintRegistry::new()).unwrap();
        assert!(subst.is_empty());
    }

    #[test]
    fn test_heterogeneous_tuple_unify_with_vars() {
        let tv1 = TypeVar::new(0);
        let tv2 = TypeVar::new(1);
        let tuple1 = Type::tuple_heterogeneous(vec![Type::Var(tv1.clone()), Type::Var(tv2.clone())]);
        let tuple2 = Type::tuple_heterogeneous(vec![Type::int(), Type::string()]);
        let subst = Unifier::unify(&tuple1, &tuple2, &TypeVarConstraintRegistry::new()).unwrap();
        assert_eq!(subst.get(&tv1), Some(&Type::int()));
        assert_eq!(subst.get(&tv2), Some(&Type::string()));
    }

    #[test]
    fn test_heterogeneous_tuple_length_mismatch() {
        let tuple1 = Type::tuple_heterogeneous(vec![Type::int(), Type::string()]);
        let tuple2 = Type::tuple_heterogeneous(vec![Type::int(), Type::string(), Type::bool()]);
        let result = Unifier::unify(&tuple1, &tuple2, &TypeVarConstraintRegistry::new());
        assert!(result.is_err());
    }

    #[test]
    fn test_heterogeneous_tuple_element_type_mismatch() {
        let tuple1 = Type::tuple_heterogeneous(vec![Type::int(), Type::string()]);
        let tuple2 = Type::tuple_heterogeneous(vec![Type::int(), Type::bool()]);
        let result = Unifier::unify(&tuple1, &tuple2, &TypeVarConstraintRegistry::new());
        assert!(result.is_err());
    }

    #[test]
    fn test_heterogeneous_tuple_occurs_check() {
        let tv = TypeVar::new(0);
        let tuple_with_var = Type::tuple_heterogeneous(vec![Type::Var(tv.clone()), Type::int()]);
        let result = Unifier::unify(
            &Type::Var(tv.clone()),
            &tuple_with_var,
            &TypeVarConstraintRegistry::new(),
        );
        assert!(result.is_err(), "Occurs check should prevent recursive type");
    }

    #[test]
    fn test_heterogeneous_tuple_nested() {
        let tv1 = TypeVar::new(0);
        let tv2 = TypeVar::new(1);
        let inner_tuple = Type::tuple_heterogeneous(vec![Type::Var(tv1.clone()), Type::int()]);
        let outer_tuple1 = Type::tuple_heterogeneous(vec![inner_tuple.clone(), Type::Var(tv2.clone())]);
        let inner_concrete = Type::tuple_heterogeneous(vec![Type::string(), Type::int()]);
        let outer_tuple2 = Type::tuple_heterogeneous(vec![inner_concrete, Type::bool()]);
        let subst = Unifier::unify(&outer_tuple1, &outer_tuple2, &TypeVarConstraintRegistry::new()).unwrap();
        assert_eq!(subst.get(&tv1), Some(&Type::string()));
        assert_eq!(subst.get(&tv2), Some(&Type::bool()));
    }

    #[test]
    fn test_variance_error_invariant_list() {
        let list_dog = Type::list(Type::Con(TypeCtor::Class("Dog".to_string())));
        let list_animal = Type::list(Type::Con(TypeCtor::Class("Animal".to_string())));

        let result = Unifier::unify(&list_dog, &list_animal, &TypeVarConstraintRegistry::new());
        assert!(result.is_err(), "Invariant lists with different types should not unify");

        if let Err(e) = result {
            let err_msg = format!("{e}");
            assert!(
                err_msg.contains("Variance") || err_msg.contains("invariant") || err_msg.contains("Cannot unify"),
                "Error should mention variance or unification failure: {err_msg}"
            );
        }
    }

    #[test]
    fn test_variance_covariant_tuple_with_vars() {
        let tv = TypeVar::new(0);
        let tuple_var = Type::App(Box::new(Type::Con(TypeCtor::Tuple)), Box::new(Type::Var(tv.clone())));
        let tuple_int = Type::App(Box::new(Type::Con(TypeCtor::Tuple)), Box::new(Type::int()));

        let subst = Unifier::unify(&tuple_var, &tuple_int, &TypeVarConstraintRegistry::new()).unwrap();
        assert_eq!(subst.get(&tv), Some(&Type::int()));
    }

    #[test]
    fn test_variance_invariant_dict() {
        let dict_str_int = Type::App(
            Box::new(Type::App(Box::new(Type::Con(TypeCtor::Dict)), Box::new(Type::string()))),
            Box::new(Type::int()),
        );
        let dict_str_float = Type::App(
            Box::new(Type::App(Box::new(Type::Con(TypeCtor::Dict)), Box::new(Type::string()))),
            Box::new(Type::float()),
        );

        let result = Unifier::unify(&dict_str_int, &dict_str_float, &TypeVarConstraintRegistry::new());
        assert!(result.is_err(), "Invariant dict types should not unify");
    }

    #[test]
    fn test_variance_with_type_variables_unifies() {
        let tv = TypeVar::new(0);
        let list_var = Type::list(Type::Var(tv.clone()));
        let list_int = Type::list(Type::int());
        let subst = Unifier::unify(&list_var, &list_int, &TypeVarConstraintRegistry::new()).unwrap();
        assert_eq!(subst.get(&tv), Some(&Type::int()));
    }

    #[test]
    fn test_variance_contravariant_generator_send() {
        let tv = TypeVar::new(0);

        let gen_var = Type::App(
            Box::new(Type::App(
                Box::new(Type::App(
                    Box::new(Type::Con(TypeCtor::Generator)),
                    Box::new(Type::int()),
                )),
                Box::new(Type::Var(tv.clone())),
            )),
            Box::new(Type::string()),
        );

        let gen_str = Type::App(
            Box::new(Type::App(
                Box::new(Type::App(
                    Box::new(Type::Con(TypeCtor::Generator)),
                    Box::new(Type::int()),
                )),
                Box::new(Type::string()),
            )),
            Box::new(Type::string()),
        );

        let subst = Unifier::unify(&gen_var, &gen_str, &TypeVarConstraintRegistry::new()).unwrap();
        assert_eq!(subst.get(&tv), Some(&Type::string()));
    }

    #[test]
    fn test_forall_alpha_equivalence() {
        let tv1 = TypeVar::new(100);
        let tv2 = TypeVar::new(200);

        let forall1 = Type::ForAll(
            vec![tv1.clone()],
            Box::new(Type::fun_unnamed(vec![Type::Var(tv1.clone())], Type::Var(tv1))),
        );

        let forall2 = Type::ForAll(
            vec![tv2.clone()],
            Box::new(Type::fun_unnamed(vec![Type::Var(tv2.clone())], Type::Var(tv2))),
        );

        let result = Unifier::unify(&forall1, &forall2, &TypeVarConstraintRegistry::new());
        assert!(result.is_ok(), "ForAll types with alpha-equivalent bodies should unify");
    }

    #[test]
    fn test_forall_with_monomorphic() {
        let tv = TypeVar::new(300);
        let forall_type = Type::ForAll(vec![tv.clone()], Box::new(Type::list(Type::Var(tv.clone()))));

        let mono_type = Type::list(Type::int());

        let result = Unifier::unify(&forall_type, &mono_type, &TypeVarConstraintRegistry::new());
        assert!(result.is_ok(), "ForAll should unify with compatible monomorphic type");

        let subst = result.unwrap();
        assert_eq!(subst.get(&tv), Some(&Type::int()));
    }

    #[test]
    fn test_forall_different_param_count() {
        let tv1 = TypeVar::new(400);
        let tv2 = TypeVar::new(401);
        let tv3 = TypeVar::new(402);

        let forall1 = Type::ForAll(
            vec![tv1.clone()],
            Box::new(Type::fun_unnamed(vec![Type::Var(tv1)], Type::int())),
        );

        let forall2 = Type::ForAll(
            vec![tv2.clone(), tv3.clone()],
            Box::new(Type::fun_unnamed(vec![Type::Var(tv2)], Type::Var(tv3))),
        );

        let result = Unifier::unify(&forall1, &forall2, &TypeVarConstraintRegistry::new());
        assert!(
            result.is_err(),
            "ForAll with different parameter counts should not unify"
        );
    }

    #[test]
    fn test_forall_nested_in_list() {
        let tv = TypeVar::new(500);
        let forall_type = Type::ForAll(
            vec![tv.clone()],
            Box::new(Type::fun_unnamed(vec![Type::Var(tv.clone())], Type::Var(tv))),
        );

        let list_forall = Type::list(forall_type);
        let list_mono = Type::list(Type::fun_unnamed(vec![Type::int()], Type::int()));

        let result = Unifier::unify(&list_forall, &list_mono, &TypeVarConstraintRegistry::new());
        assert!(result.is_ok(), "Nested ForAll in type constructor should unify");
    }

    #[test]
    fn test_extract_type_ctor_and_param_count_single_param() {
        let list_int = Type::list(Type::int());
        let result = Unifier::extract_type_ctor_and_param_count(&list_int);
        assert!(result.is_some());
        let (ctor, count) = result.unwrap();
        assert_eq!(*ctor, TypeCtor::List);
        assert_eq!(count, 1);
    }

    #[test]
    fn test_extract_type_ctor_and_param_count_two_params() {
        let dict_str_int = Type::dict(Type::string(), Type::int());
        let result = Unifier::extract_type_ctor_and_param_count(&dict_str_int);
        assert!(result.is_some());
        let (ctor, count) = result.unwrap();
        assert_eq!(*ctor, TypeCtor::Dict);
        assert_eq!(count, 2);
    }

    #[test]
    fn test_extract_type_ctor_and_param_count_three_params() {
        let gen_type = Type::generator(Type::int(), Type::string(), Type::bool());
        let result = Unifier::extract_type_ctor_and_param_count(&gen_type);
        assert!(result.is_some());
        let (ctor, count) = result.unwrap();
        assert_eq!(*ctor, TypeCtor::Generator);
        assert_eq!(count, 3);
    }

    #[test]
    fn test_variance_multi_param_dict_key_invariant() {
        let tv1 = TypeVar::new(600);
        let dict_var_int = Type::dict(Type::Var(tv1.clone()), Type::int());
        let dict_str_int = Type::dict(Type::string(), Type::int());

        let result = Unifier::unify(&dict_var_int, &dict_str_int, &TypeVarConstraintRegistry::new());
        assert!(result.is_ok());
        let subst = result.unwrap();
        assert_eq!(subst.get(&tv1), Some(&Type::string()));
    }

    #[test]
    fn test_variance_multi_param_dict_value_invariant() {
        let tv = TypeVar::new(602);
        let dict_str_var = Type::dict(Type::string(), Type::Var(tv.clone()));
        let dict_str_int = Type::dict(Type::string(), Type::int());

        let result = Unifier::unify(&dict_str_var, &dict_str_int, &TypeVarConstraintRegistry::new());
        assert!(result.is_ok());
        let subst = result.unwrap();
        assert_eq!(subst.get(&tv), Some(&Type::int()));
    }

    #[test]
    fn test_variance_multi_param_dict_both_invariant() {
        let dog = Type::Con(TypeCtor::Class("Dog".to_string()));
        let animal = Type::Con(TypeCtor::Class("Animal".to_string()));

        let dict_dog_dog = Type::dict(dog.clone(), dog.clone());
        let dict_animal_animal = Type::dict(animal.clone(), animal.clone());

        let result = Unifier::unify(&dict_dog_dog, &dict_animal_animal, &TypeVarConstraintRegistry::new());
        assert!(result.is_err(), "Dict is invariant in both parameters");
    }

    #[test]
    fn test_variance_generator_all_three_params() {
        let tv_yield = TypeVar::new(700);
        let tv_send = TypeVar::new(701);
        let tv_return = TypeVar::new(702);

        let gen_var = Type::generator(
            Type::Var(tv_yield.clone()),
            Type::Var(tv_send.clone()),
            Type::Var(tv_return.clone()),
        );
        let gen_concrete = Type::generator(Type::int(), Type::string(), Type::bool());

        let result = Unifier::unify(&gen_var, &gen_concrete, &TypeVarConstraintRegistry::new());
        assert!(result.is_ok());
        let subst = result.unwrap();
        assert_eq!(subst.get(&tv_yield), Some(&Type::int()));
        assert_eq!(subst.get(&tv_send), Some(&Type::string()));
        assert_eq!(subst.get(&tv_return), Some(&Type::bool()));
    }

    #[test]
    fn test_variance_async_generator_both_params() {
        let tv_yield = TypeVar::new(800);
        let tv_send = TypeVar::new(801);

        let gen_var = Type::async_generator(Type::Var(tv_yield.clone()), Type::Var(tv_send.clone()));
        let gen_concrete = Type::async_generator(Type::int(), Type::string());

        let result = Unifier::unify(&gen_var, &gen_concrete, &TypeVarConstraintRegistry::new());
        assert!(result.is_ok());
        let subst = result.unwrap();
        assert_eq!(subst.get(&tv_yield), Some(&Type::int()));
        assert_eq!(subst.get(&tv_send), Some(&Type::string()));
    }

    #[test]
    fn test_variance_nested_generic_hierarchy() {
        let dog = Type::Con(TypeCtor::Class("Dog".to_string()));
        let animal = Type::Con(TypeCtor::Class("Animal".to_string()));

        let producer_ctor = Type::Con(TypeCtor::Class("Producer".to_string()));
        let consumer_ctor = Type::Con(TypeCtor::Class("Consumer".to_string()));

        let consumer_dog = Type::App(Box::new(consumer_ctor.clone()), Box::new(dog.clone()));
        let producer_consumer_dog = Type::App(Box::new(producer_ctor.clone()), Box::new(consumer_dog));

        let consumer_animal = Type::App(Box::new(consumer_ctor), Box::new(animal));
        let producer_consumer_animal = Type::App(Box::new(producer_ctor), Box::new(consumer_animal));

        let result = Unifier::unify(
            &producer_consumer_dog,
            &producer_consumer_dog,
            &TypeVarConstraintRegistry::new(),
        );
        assert!(result.is_ok());

        let result = Unifier::unify(
            &producer_consumer_animal,
            &producer_consumer_animal,
            &TypeVarConstraintRegistry::new(),
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_variance_deeply_nested_list() {
        let tv = TypeVar::new(900);
        let list_list_list_var = Type::list(Type::list(Type::list(Type::Var(tv.clone()))));
        let list_list_list_int = Type::list(Type::list(Type::list(Type::int())));

        let result = Unifier::unify(
            &list_list_list_var,
            &list_list_list_int,
            &TypeVarConstraintRegistry::new(),
        );
        assert!(result.is_ok());
        let subst = result.unwrap();
        assert_eq!(subst.get(&tv), Some(&Type::int()));
    }

    #[test]
    fn test_variance_dict_with_nested_generics() {
        let tv = TypeVar::new(901);
        let dict_str_list_var = Type::dict(Type::string(), Type::list(Type::Var(tv.clone())));
        let dict_str_list_int = Type::dict(Type::string(), Type::list(Type::int()));

        let result = Unifier::unify(
            &dict_str_list_var,
            &dict_str_list_int,
            &TypeVarConstraintRegistry::new(),
        );
        assert!(result.is_ok());
        let subst = result.unwrap();
        assert_eq!(subst.get(&tv), Some(&Type::int()));
    }
}

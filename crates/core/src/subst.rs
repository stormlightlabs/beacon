//! Type Substitution System
//!
//! This module implements type substitutions for the Hindley-Milner type system.
//!
//! ## Substitutions
//!
//! A substitution is a finite mapping from type variables to types: `σ = [α₁ ↦ τ₁, ..., αₙ ↦ τₙ]`
//!
//! Substitutions are applied to types recursively, replacing type variables with their mapped types.
//! The implementation ensures:
//!
//! - **Idempotence**: Applying a substitution multiple times has the same effect as applying it once
//! - **Recursive application**: When applying `[a ↦ b, b ↦ int]`, the result fully resolves to `int`
//! - **Bound variable respect**: Substitutions don't affect variables bound by `ForAll`
//!
//! ## Composition
//!
//! Substitutions compose: `(σ₂ ∘ σ₁)(τ) = σ₂(σ₁(τ))`
//!
//! Composition is implemented by applying σ₂ to all types in σ₁'s range, then adding σ₂'s mappings.
//!
//! ## Example
//!
//! ```
//! use beacon_core::{Subst, Type, TypeVar};
//!
//! fn example() {
//!     let tv = TypeVar::new(0);
//!     let subst = Subst::singleton(tv.clone(), Type::int());
//!
//!     let ty = Type::list(Type::Var(tv));
//!     let result = subst.apply(&ty);
//!
//!     assert_eq!(result, Type::list(Type::int()));
//! }
//! ```

use crate::{Type, TypeVar};
use rustc_hash::FxHashMap;
use std::fmt;

/// Type substitution mapping type variables to types
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Subst {
    map: FxHashMap<TypeVar, Type>,
}

impl Subst {
    /// Create an empty substitution
    pub fn empty() -> Self {
        Self { map: FxHashMap::default() }
    }

    /// Create a single variable substitution
    pub fn singleton(tv: TypeVar, ty: Type) -> Self {
        let mut map = FxHashMap::default();
        map.insert(tv, ty);
        Self { map }
    }

    /// Check if substitution is empty
    pub fn is_empty(&self) -> bool {
        self.map.is_empty()
    }

    /// Get the mapping for a type variable
    pub fn get(&self, tv: &TypeVar) -> Option<&Type> {
        self.map.get(tv)
    }

    /// Insert a new mapping
    pub fn insert(&mut self, tv: TypeVar, ty: Type) {
        self.map.insert(tv, ty);
    }

    /// Get all mappings as an iterator
    pub fn iter(&self) -> impl Iterator<Item = (&TypeVar, &Type)> {
        self.map.iter()
    }

    /// Apply substitution to a type
    pub fn apply(&self, ty: &Type) -> Type {
        match ty {
            Type::Var(tv) => match self.map.get(tv) {
                Some(replacement) => self.apply(replacement),
                None => ty.clone(),
            },
            Type::Con(_) => ty.clone(),
            Type::App(t1, t2) => Type::App(Box::new(self.apply(t1)), Box::new(self.apply(t2))),
            Type::Fun(args, ret) => {
                let new_args: Vec<Type> = args.iter().map(|arg| self.apply(arg)).collect();
                Type::Fun(new_args, Box::new(self.apply(ret)))
            }
            Type::ForAll(tvs, t) => {
                let filtered_subst = Subst {
                    map: self
                        .map
                        .iter()
                        .filter(|(tv, _)| !tvs.contains(tv))
                        .map(|(tv, ty)| (tv.clone(), ty.clone()))
                        .collect(),
                };
                Type::ForAll(tvs.clone(), Box::new(filtered_subst.apply(t)))
            }
            Type::Union(types) => Type::Union(types.iter().map(|t| self.apply(t)).collect()),
            Type::Intersection(types) => Type::Intersection(types.iter().map(|t| self.apply(t)).collect()),
            Type::Record(fields, row_var) => {
                let new_fields: Vec<(String, Type)> =
                    fields.iter().map(|(name, ty)| (name.clone(), self.apply(ty))).collect();
                let new_row_var = row_var.as_ref().and_then(|rv| match self.map.get(rv) {
                    Some(Type::Var(new_rv)) => Some(new_rv.clone()),
                    Some(_) => None,
                    None => Some(rv.clone()),
                });
                Type::Record(new_fields, new_row_var)
            }
            Type::BoundMethod(receiver, method) => {
                Type::BoundMethod(Box::new(self.apply(receiver)), Box::new(self.apply(method)))
            }
        }
    }

    /// Compose two substitutions: (s2 ∘ s1)(t) = s2(s1(t))
    pub fn compose(self, other: Subst) -> Subst {
        let mut result = FxHashMap::default();

        for (tv, ty) in self.map {
            result.insert(tv, other.apply(&ty));
        }

        for (tv, ty) in other.map {
            result.entry(tv).or_insert(ty);
        }

        Subst { map: result }
    }

    /// Apply substitution to all types in the substitution (for occurs check)
    pub fn apply_to_self(&self) -> Subst {
        let mut result = FxHashMap::default();
        for (tv, ty) in &self.map {
            result.insert(tv.clone(), self.apply(ty));
        }
        Subst { map: result }
    }

    /// Get the domain (type variables being substituted)
    pub fn domain(&self) -> impl Iterator<Item = &TypeVar> {
        self.map.keys()
    }

    /// Get the range (types being substituted in)
    pub fn range(&self) -> impl Iterator<Item = &Type> {
        self.map.values()
    }

    /// Check if the substitution contains a type variable in its domain
    pub fn contains_var(&self, tv: &TypeVar) -> bool {
        self.map.contains_key(tv)
    }
}

impl fmt::Display for Subst {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.map.is_empty() {
            write!(f, "[]")
        } else {
            let mappings: Vec<String> = self.map.iter().map(|(tv, ty)| format!("{tv} ↦ {ty}")).collect();
            write!(f, "[{}]", mappings.join(", "))
        }
    }
}

impl Default for Subst {
    fn default() -> Self {
        Self::empty()
    }
}

/// Trait for types that can have substitutions applied to them
pub trait Substitutable {
    fn apply_subst(&self, subst: &Subst) -> Self;
    fn free_vars(&self) -> FxHashMap<TypeVar, ()>;
}

impl Substitutable for Type {
    fn apply_subst(&self, subst: &Subst) -> Self {
        subst.apply(self)
    }

    fn free_vars(&self) -> FxHashMap<TypeVar, ()> {
        self.free_vars()
    }
}

impl Substitutable for Vec<Type> {
    fn apply_subst(&self, subst: &Subst) -> Self {
        self.iter().map(|t| subst.apply(t)).collect()
    }

    fn free_vars(&self) -> FxHashMap<TypeVar, ()> {
        let mut vars = FxHashMap::default();
        for ty in self {
            vars.extend(ty.free_vars());
        }
        vars
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{TypeCtor, types::TypeVar};

    #[test]
    fn test_substitution_empty() {
        assert_eq!(Subst::empty().map.len(), 0);
    }

    #[test]
    fn test_empty_substitution() {
        let subst = Subst::empty();
        let ty = Type::Var(TypeVar::new(0));
        assert_eq!(subst.apply(&ty), ty);
    }

    #[test]
    fn test_singleton_substitution() {
        let tv = TypeVar::new(0);
        let subst = Subst::singleton(tv.clone(), Type::int());
        let ty = Type::Var(tv);
        assert_eq!(subst.apply(&ty), Type::int());
    }

    #[test]
    fn test_substitution_application() {
        let tv1 = TypeVar::new(0);
        let tv2 = TypeVar::new(1);
        let mut subst = Subst::empty();
        subst.insert(tv1.clone(), Type::int());
        subst.insert(tv2.clone(), Type::string());

        let fun_type = Type::fun(vec![Type::Var(tv1)], Type::Var(tv2));
        let expected = Type::fun(vec![Type::int()], Type::string());

        assert_eq!(subst.apply(&fun_type), expected);
    }

    #[test]
    fn test_substitution_composition() {
        let tv1 = TypeVar::new(0);
        let tv2 = TypeVar::new(1);
        let _tv3 = TypeVar::new(2);

        let s1 = Subst::singleton(tv1.clone(), Type::Var(tv2.clone()));
        let s2 = Subst::singleton(tv2, Type::int());

        let composed = s1.compose(s2);
        let ty = Type::Var(tv1);

        assert_eq!(composed.apply(&ty), Type::int());
    }

    #[test]
    fn test_recursive_substitution() {
        let tv1 = TypeVar::new(0);
        let tv2 = TypeVar::new(1);

        let mut subst = Subst::empty();
        subst.insert(tv1.clone(), Type::Var(tv2.clone()));
        subst.insert(tv2, Type::int());

        let ty = Type::Var(tv1);
        assert_eq!(subst.apply(&ty), Type::int());
    }

    #[test]
    fn test_forall_substitution() {
        let tv1 = TypeVar::new(0);
        let _tv2 = TypeVar::new(1);

        let subst = Subst::singleton(tv1.clone(), Type::int());
        let forall_type = Type::ForAll(vec![tv1.clone()], Box::new(Type::Var(tv1.clone())));

        let result = subst.apply(&forall_type);
        assert_eq!(result, forall_type);
    }

    #[test]
    fn test_union_substitution() {
        let tv = TypeVar::new(0);
        let subst = Subst::singleton(tv.clone(), Type::int());

        let union_type = Type::union(vec![Type::Var(tv), Type::string()]);
        let expected = Type::union(vec![Type::int(), Type::string()]);

        assert_eq!(subst.apply(&union_type), expected);
    }

    #[test]
    fn test_record_substitution() {
        let tv = TypeVar::new(0);
        let row_var = TypeVar::new(1);

        let mut subst = Subst::empty();
        subst.insert(tv.clone(), Type::int());

        let record = Type::Record(vec![("x".to_string(), Type::Var(tv))], Some(row_var.clone()));

        let expected = Type::Record(vec![("x".to_string(), Type::int())], Some(row_var));

        assert_eq!(subst.apply(&record), expected);
    }

    #[test]
    fn test_record_row_variable_replacement() {
        let original_row = TypeVar::new(10);
        let replacement_row = TypeVar::new(11);
        let subst = Subst::singleton(original_row.clone(), Type::Var(replacement_row.clone()));

        let record = Type::Record(vec![], Some(original_row));
        let applied = subst.apply(&record);

        assert_eq!(applied, Type::Record(vec![], Some(replacement_row)));
    }

    #[test]
    fn test_record_row_variable_removed_when_non_var() {
        let row_var = TypeVar::new(12);
        let subst = Subst::singleton(row_var.clone(), Type::int());

        let record = Type::Record(vec![], Some(row_var));
        let applied = subst.apply(&record);

        assert_eq!(applied, Type::Record(vec![], None));
    }

    #[test]
    fn test_apply_to_self_normalises_chain() {
        let tv1 = TypeVar::new(0);
        let tv2 = TypeVar::new(1);

        let mut subst = Subst::empty();
        subst.insert(tv1.clone(), Type::Var(tv2.clone()));
        subst.insert(tv2.clone(), Type::int());

        let normalised = subst.apply_to_self();
        assert_eq!(normalised.get(&tv1), Some(&Type::int()));
        assert_eq!(normalised.get(&tv2), Some(&Type::int()));
    }

    #[test]
    fn test_domain_and_range_helpers() {
        let tv1 = TypeVar::new(3);
        let tv2 = TypeVar::new(4);
        let mut subst = Subst::empty();
        subst.insert(tv1.clone(), Type::int());
        subst.insert(tv2.clone(), Type::string());

        let domain: Vec<TypeVar> = subst.domain().cloned().collect();
        assert!(domain.contains(&tv1));
        assert!(domain.contains(&tv2));

        let mut range = subst.range().map(|ty| ty.to_string()).collect::<Vec<_>>();
        range.sort();
        assert_eq!(range, vec!["int".to_string(), "str".to_string()]);
    }

    #[test]
    fn test_iter_and_contains_var_behaviour() {
        let tv1 = TypeVar::new(5);
        let tv2 = TypeVar::new(6);
        let mut subst = Subst::empty();
        assert!(subst.is_empty());

        subst.insert(tv1.clone(), Type::int());
        subst.insert(tv2.clone(), Type::string());
        assert!(subst.contains_var(&tv1));
        assert!(!subst.is_empty());

        let collected: Vec<(TypeVar, Type)> = subst.iter().map(|(tv, ty)| (tv.clone(), ty.clone())).collect();
        assert!(collected.contains(&(tv1, Type::int())));
        assert!(collected.contains(&(tv2, Type::string())));
    }

    #[test]
    fn test_default_is_empty_substitution() {
        let subst = Subst::default();
        assert!(subst.is_empty());
    }

    #[test]
    fn test_substitution_display() {
        let tv1 = TypeVar::new(0);
        let tv2 = TypeVar::new(1);

        let mut subst = Subst::empty();
        subst.insert(tv1, Type::int());
        subst.insert(tv2, Type::string());

        let display = subst.to_string();
        assert!(display.contains("↦"));
        assert!(display.contains("int"));
        assert!(display.contains("str"));
    }

    #[test]
    fn test_substitution_apply_type_var() {
        let mut map = FxHashMap::default();
        let tv1 = TypeVar::new(1);
        let tv2 = TypeVar::new(2);
        map.insert(tv1.clone(), Type::int());
        let subst = Subst { map };

        let result = subst.apply(&Type::Var(tv1));
        assert!(matches!(result, Type::Con(TypeCtor::Int)));

        let result2 = subst.apply(&Type::Var(tv2));
        assert!(matches!(result2, Type::Var(_)));
    }

    #[test]
    fn test_substitution_apply_function() {
        let mut map = FxHashMap::default();
        let tv = TypeVar::new(1);
        map.insert(tv.clone(), Type::int());
        let subst = Subst { map };

        let fn_type = Type::fun(vec![Type::Var(tv)], Type::string());
        let result = subst.apply(&fn_type);

        match result {
            Type::Fun(args, _ret) => {
                assert_eq!(args.len(), 1);
                assert!(matches!(args[0], Type::Con(TypeCtor::Int)));
            }
            _ => panic!("Expected function type"),
        }
    }

    #[test]
    fn test_substitution_apply_union() {
        let mut map = FxHashMap::default();
        let tv = TypeVar::new(1);
        map.insert(tv.clone(), Type::int());
        let subst = Subst { map };

        let union_type = Type::Union(vec![Type::Var(tv), Type::string()]);
        let result = subst.apply(&union_type);

        match result {
            Type::Union(types) => {
                assert_eq!(types.len(), 2);
                assert!(matches!(types[0], Type::Con(TypeCtor::Int)));
                assert!(matches!(types[1], Type::Con(TypeCtor::String)));
            }
            _ => panic!("Expected union type"),
        }
    }

    #[test]
    fn test_substitution_apply_record() {
        let mut map = FxHashMap::default();
        let tv = TypeVar::new(1);
        map.insert(tv.clone(), Type::int());
        let subst = Subst { map };

        let record_type = Type::Record(vec![("x".to_string(), Type::Var(tv))], None);
        let result = subst.apply(&record_type);

        match result {
            Type::Record(fields, _) => {
                assert_eq!(fields.len(), 1);
                assert_eq!(fields[0].0, "x");
                assert!(matches!(fields[0].1, Type::Con(TypeCtor::Int)));
            }
            _ => panic!("Expected record type"),
        }
    }
}

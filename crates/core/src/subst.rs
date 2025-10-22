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
        Self {
            map: FxHashMap::default(),
        }
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
            Type::Var(tv) => {
                match self.map.get(tv) {
                    Some(replacement) => self.apply(replacement), // Apply recursively
                    None => ty.clone(),
                }
            }
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
            Type::Record(fields, row_var) => {
                let new_fields: Vec<(String, Type)> = fields
                    .iter()
                    .map(|(name, ty)| (name.clone(), self.apply(ty)))
                    .collect();
                let new_row_var = row_var.as_ref().and_then(|rv| {
                    match self.map.get(rv) {
                        Some(Type::Var(new_rv)) => Some(new_rv.clone()),
                        Some(_) => None, // Row variable substituted with non-variable
                        None => Some(rv.clone()),
                    }
                });
                Type::Record(new_fields, new_row_var)
            }
        }
    }

    /// Compose two substitutions: (s2 ∘ s1)(t) = s2(s1(t))
    pub fn compose(self, other: Subst) -> Subst {
        let mut result = FxHashMap::default();

        // Apply other to all mappings in self, then add to result
        for (tv, ty) in self.map {
            result.insert(tv, other.apply(&ty));
        }

        // Add mappings from other that are not in self
        for (tv, ty) in other.map {
            if !result.contains_key(&tv) {
                result.insert(tv, ty);
            }
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
            let mappings: Vec<String> = self
                .map
                .iter()
                .map(|(tv, ty)| format!("{} ↦ {}", tv, ty))
                .collect();
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
    use crate::types::TypeVar;

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

        let record = Type::Record(
            vec![("x".to_string(), Type::Var(tv))],
            Some(row_var.clone()),
        );

        let expected = Type::Record(vec![("x".to_string(), Type::int())], Some(row_var));

        assert_eq!(subst.apply(&record), expected);
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
}

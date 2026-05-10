//! Polymorphic type schemes.

use super::{Type, TypeVar};
use rustc_hash::FxHashMap;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeScheme {
    pub quantified_vars: Vec<TypeVar>,
    pub ty: Type,
}

impl TypeScheme {
    pub fn new(quantified_vars: Vec<TypeVar>, ty: Type) -> Self {
        Self { quantified_vars, ty }
    }

    /// Create a monomorphic type scheme (no quantified variables)
    pub fn mono(ty: Type) -> Self {
        Self::new(Vec::new(), ty)
    }

    /// Generalize a type into a type scheme by quantifying over free variables
    ///
    /// This respects the value restriction: if `is_non_expansive` is false,
    /// the type is not generalized (returns a monomorphic scheme).
    pub fn generalize(ty: Type, env_vars: &FxHashMap<TypeVar, ()>) -> Self {
        Self::generalize_with_restriction(ty, env_vars, true)
    }

    /// Generalize a type with explicit control over value restriction
    ///
    /// ## Arguments
    /// * `ty` - The type to generalize
    /// * `env_vars` - Variables bound in the environment (not generalized)
    /// * `is_non_expansive` - Whether the expression is non-expansive (value restriction)
    ///
    /// If `is_non_expansive` is false, returns a monomorphic scheme (no generalization).
    /// This implements the value restriction to ensure soundness.
    pub fn generalize_with_restriction(ty: Type, env_vars: &FxHashMap<TypeVar, ()>, is_non_expansive: bool) -> Self {
        if !is_non_expansive {
            return Self::mono(ty);
        }

        let free_vars = ty.free_vars();
        let quantified: Vec<TypeVar> = free_vars
            .keys()
            .filter(|tv| !env_vars.contains_key(tv))
            .cloned()
            .collect();

        Self::new(quantified, ty)
    }
}

impl std::fmt::Display for TypeScheme {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.quantified_vars.is_empty() {
            write!(f, "{}", self.ty)
        } else {
            write!(
                f,
                "∀{}. {}",
                self.quantified_vars
                    .iter()
                    .map(|tv| tv.to_string())
                    .collect::<Vec<_>>()
                    .join(" "),
                self.ty
            )
        }
    }
}

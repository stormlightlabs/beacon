//! Beacon Core - Hindley-Milner Type System Implementation
//!
//! This module implements the core type system for Beacon, including:
//! - Type and Kind representations
//! - Type variable management
//! - Substitution and unification algorithms
//! - Type schemes and generalization

pub mod errors;
pub mod subst;
pub mod types;
pub mod unify;

pub use errors::*;
pub use subst::*;
pub use types::*;
pub use unify::*;

/// Type variable generator for creating fresh type variables
#[derive(Debug, Clone)]
pub struct TypeVarGen {
    counter: u32,
}

impl TypeVarGen {
    pub fn new() -> Self {
        Self { counter: 0 }
    }

    /// Generate a fresh type variable
    pub fn fresh(&mut self) -> TypeVar {
        let tv = TypeVar::new(self.counter);
        self.counter += 1;
        tv
    }

    /// Generate a fresh type variable with a hint name
    pub fn fresh_named(&mut self, hint: &str) -> TypeVar {
        let tv = TypeVar::named(self.counter, hint);
        self.counter += 1;
        tv
    }
}

impl Default for TypeVarGen {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type_var_gen() {
        let mut generator = TypeVarGen::new();
        let tv1 = generator.fresh();
        let tv2 = generator.fresh();
        assert_ne!(tv1, tv2);
        assert_eq!(tv1.id, 0);
        assert_eq!(tv2.id, 1);
    }

    #[test]
    fn test_type_var_gen_named() {
        let mut generator = TypeVarGen::new();
        let tv = generator.fresh_named("test");
        assert_eq!(tv.hint, Some("test".to_string()));
    }
}

//! Beacon Core - Hindley-Milner Type System Implementation
//!
//! This module implements the core type system for Beacon, including:
//! - Type and Kind representations
//! - Type variable management
//! - Substitution and unification algorithms
//! - Type schemes and generalization
//! - Value restriction for sound generalization

pub mod annotation_parser;
pub mod class_metadata;
pub mod errors;
pub mod protocols;
pub mod subst;
pub mod types;
pub mod unify;

pub use annotation_parser::*;
pub use class_metadata::*;
pub use errors::*;
pub use protocols::*;
pub use subst::*;
pub use types::*;
pub use unify::*;

/// Trait for expressions that can be tested for expansiveness
///
/// In ML-style type systems, the value restriction prevents unsound generalization by only generalizing non-expansive expressions.
/// An expression is non-expansive if:
///
/// - It's a literal (number, string, bool, None)
/// - It's a lambda expression
/// - It's a variable reference
/// - It's a constructor application with non-expansive arguments (tuples, lists)
///
/// Expansive expressions include:
/// - Function calls
/// - Attribute access
/// - Subscript operations
/// - Most other computations
///
/// This trait should be implemented by expression types in the AST.
pub trait Expansiveness {
    /// Returns true if this expression is non-expansive (safe to generalize)
    fn is_non_expansive(&self) -> bool;
}

/// Helper for determining if a binding should be generalized based on value restriction
///
/// This function encapsulates the value restriction logic.
/// In Python, we follow similar rules to ML but adapted for Python's semantics:
///
/// ## Non-expansive (can generalize):
/// - Literals: `42`, `"hello"`, `True`, `None`
/// - Lambda expressions: `lambda x: x`
/// - Variable bindings: `x = y` where `y` is a variable
/// - Tuple/List/Dict literals with non-expansive elements: `(1, 2)`, `[x]`
///
/// ## Expansive (cannot generalize):
/// - Function calls: `f(x)`
/// - Method calls: `obj.method()`
/// - Attribute access: `obj.attr`
/// - Subscripts: `lst[0]`
/// - Comprehensions: `[x for x in xs]`
///
/// ## Example:
/// ```ignore
/// # Safe to generalize (non-expansive)
/// x = lambda a: a  # ∀'a. 'a -> 'a
///
/// # Not safe to generalize (expansive)
/// y = f()  # 'b (not ∀'b. 'b), where 'b is fresh
/// ```
pub fn should_generalize<E: Expansiveness>(expr: &E) -> bool {
    expr.is_non_expansive()
}

/// Type variable generator for creating fresh type variables
use std::sync::{
    Arc,
    atomic::{AtomicU32, Ordering},
};

#[derive(Debug, Clone)]
pub struct TypeVarGen {
    counter: Arc<AtomicU32>,
}

impl TypeVarGen {
    pub fn new() -> Self {
        Self { counter: Arc::new(AtomicU32::new(0)) }
    }

    /// Generate a fresh type variable
    pub fn fresh(&mut self) -> TypeVar {
        let id = self.counter.fetch_add(1, Ordering::Relaxed);
        TypeVar::new(id)
    }

    /// Generate a fresh type variable with a hint name
    pub fn fresh_named(&mut self, hint: &str) -> TypeVar {
        let id = self.counter.fetch_add(1, Ordering::Relaxed);
        TypeVar::named(id, hint)
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

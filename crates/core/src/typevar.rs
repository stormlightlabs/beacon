//! Type variable generation.

use std::sync::{
    Arc, OnceLock,
    atomic::{AtomicU32, Ordering},
};

use crate::TypeVar;

fn global_typevar_counter() -> Arc<AtomicU32> {
    static GLOBAL_COUNTER: OnceLock<Arc<AtomicU32>> = OnceLock::new();
    GLOBAL_COUNTER.get_or_init(|| Arc::new(AtomicU32::new(0))).clone()
}

/// Type variable generator for creating fresh type variables.
#[derive(Debug, Clone)]
pub struct TypeVarGen {
    counter: Arc<AtomicU32>,
}

impl TypeVarGen {
    pub fn new() -> Self {
        Self { counter: global_typevar_counter() }
    }

    /// Generate a fresh type variable.
    pub fn fresh(&mut self) -> TypeVar {
        let id = self.counter.fetch_add(1, Ordering::Relaxed);
        TypeVar::new(id)
    }

    /// Generate a fresh type variable with a hint name.
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
    }

    #[test]
    fn test_type_var_gen_named() {
        let mut generator = TypeVarGen::new();
        let tv = generator.fresh_named("test");
        assert_eq!(tv.hint, Some("test".to_string()));
    }
}

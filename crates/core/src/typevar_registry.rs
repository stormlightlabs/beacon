//! TypeVar Constraint and Bound Registry
//!
//! This module provides a registry for tracking TypeVar bounds and constraints.
//! In Python's typing system:
//! - **Bounds**: A TypeVar can have a bound, meaning it must be a subtype of the bound type.
//!   Example: `T = TypeVar('T', bound=Animal)` means T must be Animal or a subtype.
//! - **Constraints**: A TypeVar can have multiple constraint types, restricting it to only those specific types.
//!   Example: `T = TypeVar('T', int, str)` means T can only be int or str, not subtypes.

use crate::types::Type;
use rustc_hash::FxHashMap;
use std::fmt;

/// Registry mapping TypeVar IDs to their bounds and constraints
///
/// This registry is passed through the analysis pipeline to track TypeVar metadata
/// extracted from TypeVar() calls in Python code or stub files.
#[derive(Debug, Clone, Default)]
pub struct TypeVarConstraintRegistry {
    /// Maps TypeVar ID to its bound type (if any)
    /// Example: TypeVar('T', bound=Animal) -> bounds[T.id] = Animal
    bounds: FxHashMap<u32, Type>,

    /// Maps TypeVar ID to its constraint types (if any)
    /// Example: TypeVar('T', int, str) -> constraints[T.id] = [int, str]
    constraints: FxHashMap<u32, Vec<Type>>,
}

impl TypeVarConstraintRegistry {
    /// Create a new empty registry
    pub fn new() -> Self {
        Self { bounds: FxHashMap::default(), constraints: FxHashMap::default() }
    }

    /// Register a bound for a TypeVar
    ///
    /// # Arguments
    /// * `typevar_id` - The ID of the TypeVar
    /// * `bound` - The bound type (TypeVar must be a subtype of this)
    ///
    /// # Example
    /// ```ignore
    /// // Python: T = TypeVar('T', bound=Animal)
    /// registry.set_bound(t.id, Type::Con(TypeCtor::Class("Animal".to_string())));
    /// ```
    pub fn set_bound(&mut self, typevar_id: u32, bound: Type) {
        self.bounds.insert(typevar_id, bound);
    }

    /// Register constraints for a TypeVar
    ///
    /// # Arguments
    /// * `typevar_id` - The ID of the TypeVar
    /// * `constraints` - The allowed types (TypeVar must be one of these, not a subtype)
    ///
    /// # Example
    /// ```ignore
    /// // Python: T = TypeVar('T', int, str)
    /// registry.set_constraints(t.id, vec![Type::int(), Type::string()]);
    /// ```
    pub fn set_constraints(&mut self, typevar_id: u32, constraints: Vec<Type>) {
        if !constraints.is_empty() {
            self.constraints.insert(typevar_id, constraints);
        }
    }

    /// Get the bound for a TypeVar, if any
    pub fn get_bound(&self, typevar_id: u32) -> Option<&Type> {
        self.bounds.get(&typevar_id)
    }

    /// Get the constraints for a TypeVar, if any
    pub fn get_constraints(&self, typevar_id: u32) -> Option<&Vec<Type>> {
        self.constraints.get(&typevar_id)
    }

    /// Check if a TypeVar has a bound
    pub fn has_bound(&self, typevar_id: u32) -> bool {
        self.bounds.contains_key(&typevar_id)
    }

    /// Check if a TypeVar has constraints
    pub fn has_constraints(&self, typevar_id: u32) -> bool {
        self.constraints.contains_key(&typevar_id)
    }

    /// Validate that a type satisfies a TypeVar's bound (if any)
    ///
    /// TODO: Bound validation currently produces false positives for some typeshed stubs.
    /// For example, TypeVars with bounds like SupportsNext, SupportsAdd, Awaitable may
    /// incorrectly reject valid types like int, str, or list due to:
    /// 1. Missing protocol implementation checks in is_subtype_of
    /// 2. Incomplete structural subtyping for protocol types
    /// 3. TypeVar bound inference not handling implicit protocol satisfaction
    pub fn validate_bound(&self, typevar_id: u32, ty: &Type) -> Result<(), String> {
        if let Some(bound) = self.get_bound(typevar_id) {
            if !ty.is_subtype_of(bound) {
                return Err(format!("Type {ty} does not satisfy bound {bound} for TypeVar"));
            }
        }
        Ok(())
    }

    /// Validate that a type satisfies a TypeVar's constraints (if any)
    pub fn validate_constraints(&self, typevar_id: u32, ty: &Type) -> Result<(), String> {
        if let Some(constraint_types) = self.get_constraints(typevar_id) {
            let matches_any = constraint_types.iter().any(|constraint| ty == constraint);

            if !matches_any {
                return Err(format!(
                    "Type {} does not match any constraint {:?} for TypeVar",
                    ty,
                    constraint_types.iter().map(|t| t.to_string()).collect::<Vec<_>>()
                ));
            }
        }
        Ok(())
    }

    /// Validate that a type satisfies both bounds and constraints for a TypeVar
    pub fn validate(&self, typevar_id: u32, ty: &Type) -> Result<(), String> {
        self.validate_bound(typevar_id, ty)?;
        self.validate_constraints(typevar_id, ty)?;
        Ok(())
    }

    /// Merge another registry into this one
    ///
    /// This is useful when combining registries from different scopes or modules.
    /// If a TypeVar ID exists in both registries, the incoming registry's entry takes precedence.
    pub fn merge(&mut self, other: &TypeVarConstraintRegistry) {
        self.bounds.extend(other.bounds.clone());
        self.constraints.extend(other.constraints.clone());
    }

    /// Clear all bounds and constraints
    pub fn clear(&mut self) {
        self.bounds.clear();
        self.constraints.clear();
    }

    /// Get the number of registered TypeVars with bounds
    pub fn bound_count(&self) -> usize {
        self.bounds.len()
    }

    /// Get the number of registered TypeVars with constraints
    pub fn constraint_count(&self) -> usize {
        self.constraints.len()
    }
}

impl fmt::Display for TypeVarConstraintRegistry {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "TypeVarConstraintRegistry:")?;
        writeln!(f, "  Bounds: {} entries", self.bounds.len())?;
        for (id, bound) in &self.bounds {
            writeln!(f, "    TypeVar({id}) bound={bound}")?;
        }
        writeln!(f, "  Constraints: {} entries", self.constraints.len())?;
        for (id, constraints) in &self.constraints {
            writeln!(
                f,
                "    TypeVar({}) constraints=[{}]",
                id,
                constraints.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(", ")
            )?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::{Type, TypeCtor};

    #[test]
    fn test_registry_set_and_get_bound() {
        let mut registry = TypeVarConstraintRegistry::new();
        let animal = Type::Con(TypeCtor::Class("Animal".to_string()));

        registry.set_bound(1, animal.clone());

        assert!(registry.has_bound(1));
        assert_eq!(registry.get_bound(1), Some(&animal));
        assert!(!registry.has_constraints(1));
    }

    #[test]
    fn test_registry_set_and_get_constraints() {
        let mut registry = TypeVarConstraintRegistry::new();
        let constraints = vec![Type::int(), Type::string()];

        registry.set_constraints(2, constraints.clone());

        assert!(registry.has_constraints(2));
        assert_eq!(registry.get_constraints(2), Some(&constraints));
        assert!(!registry.has_bound(2));
    }

    #[test]
    fn test_validate_bound_success() {
        let mut registry = TypeVarConstraintRegistry::new();
        let animal = Type::Con(TypeCtor::Class("Animal".to_string()));
        let dog = Type::Con(TypeCtor::Class("Dog".to_string()));

        registry.set_bound(1, animal.clone());

        assert!(registry.validate_bound(1, &animal).is_ok());

        // TODO: implement subtyping hierarchy
        let _ = registry.validate_bound(1, &dog);
    }

    #[test]
    fn test_validate_bound_failure() {
        let mut registry = TypeVarConstraintRegistry::new();
        let animal = Type::Con(TypeCtor::Class("Animal".to_string()));

        registry.set_bound(1, animal);

        let result = registry.validate_bound(1, &Type::int());
        assert!(result.is_err());
    }

    #[test]
    fn test_validate_constraints_success() {
        let mut registry = TypeVarConstraintRegistry::new();
        let constraints = vec![Type::int(), Type::string()];

        registry.set_constraints(1, constraints);

        assert!(registry.validate_constraints(1, &Type::int()).is_ok());

        assert!(registry.validate_constraints(1, &Type::string()).is_ok());
    }

    #[test]
    fn test_validate_constraints_failure() {
        let mut registry = TypeVarConstraintRegistry::new();
        let constraints = vec![Type::int(), Type::string()];

        registry.set_constraints(1, constraints);

        let result = registry.validate_constraints(1, &Type::bool());
        assert!(result.is_err());
    }

    #[test]
    fn test_validate_no_bound_or_constraints() {
        let registry = TypeVarConstraintRegistry::new();

        assert!(registry.validate(1, &Type::int()).is_ok());
        assert!(registry.validate(1, &Type::string()).is_ok());
    }

    #[test]
    fn test_merge_registries() {
        let mut registry1 = TypeVarConstraintRegistry::new();
        let mut registry2 = TypeVarConstraintRegistry::new();

        let animal = Type::Con(TypeCtor::Class("Animal".to_string()));
        let constraints = vec![Type::int(), Type::string()];

        registry1.set_bound(1, animal.clone());
        registry2.set_constraints(2, constraints.clone());

        registry1.merge(&registry2);

        assert!(registry1.has_bound(1));
        assert!(registry1.has_constraints(2));
        assert_eq!(registry1.get_bound(1), Some(&animal));
        assert_eq!(registry1.get_constraints(2), Some(&constraints));
    }

    #[test]
    fn test_clear() {
        let mut registry = TypeVarConstraintRegistry::new();
        let animal = Type::Con(TypeCtor::Class("Animal".to_string()));

        registry.set_bound(1, animal);
        registry.set_constraints(2, vec![Type::int(), Type::string()]);

        assert_eq!(registry.bound_count(), 1);
        assert_eq!(registry.constraint_count(), 1);

        registry.clear();

        assert_eq!(registry.bound_count(), 0);
        assert_eq!(registry.constraint_count(), 0);
    }

    #[test]
    fn test_empty_constraints_not_stored() {
        let mut registry = TypeVarConstraintRegistry::new();
        registry.set_constraints(1, vec![]);

        assert!(!registry.has_constraints(1));
        assert_eq!(registry.constraint_count(), 0);
    }
}

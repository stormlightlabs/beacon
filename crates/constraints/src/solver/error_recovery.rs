use super::compat::extract_base_constructor;

use beacon_core::{Subst, Type, TypeCtor};

/// Provide a contextual fallback type when unification fails
///
/// Instead of always falling back to `Any`, use context to provide a more specific type:
/// - If one type is concrete and the other is a type variable, prefer the concrete type
/// - For container types, preserve the container structure with Any for unknown elements
/// - This allows partial type information to be preserved even when full inference fails
pub(super) fn contextual_fallback_type(t1: &Type, t2: &Type) -> Type {
    match (t1, t2) {
        (Type::Con(TypeCtor::Any), other) | (other, Type::Con(TypeCtor::Any)) => other.clone(),
        (Type::Var(_), concrete) | (concrete, Type::Var(_)) if !matches!(concrete, Type::Var(_)) => concrete.clone(),
        (Type::App(f1, _), Type::App(f2, _)) if extract_base_constructor(f1) == extract_base_constructor(f2) => {
            t1.clone()
        }
        (Type::Union(types1), Type::Union(types2)) => {
            let common: Vec<Type> = types1
                .iter()
                .filter(|t1| types2.iter().any(|t2| *t1 == t2))
                .cloned()
                .collect();

            if !common.is_empty() { Type::union(common) } else { Type::any() }
        }
        _ => {
            tracing::debug!(
                "Type inference failed, falling back to Any for types: {} and {}",
                t1.display_for_diagnostics(),
                t2.display_for_diagnostics()
            );
            Type::any()
        }
    }
}

/// Try to apply a partial substitution even when unification fails
///
/// When unification fails, we might still have learned something useful
/// from the successful parts of the unification attempt. This function
/// attempts to extract and apply any partial progress.
pub(super) fn try_partial_unify(
    t1: &Type, t2: &Type, _typevar_registry: &beacon_core::TypeVarConstraintRegistry,
) -> Option<Subst> {
    match (t1, t2) {
        (Type::Var(tv), other) | (other, Type::Var(tv)) if !matches!(other, Type::Var(_)) => {
            let fallback = contextual_fallback_type(t1, t2);
            Some(Subst::singleton(tv.clone(), fallback))
        }
        _ => None,
    }
}

//! Union unification logic.

use super::Unifier;
use crate::{ClassRegistry, Result, Subst, Type, TypeVarConstraintRegistry};

impl Unifier {
    pub(super) fn unify_unions(
        types1: &[Type], types2: &[Type], registry: &TypeVarConstraintRegistry, class_registry: Option<&ClassRegistry>,
    ) -> Result<Subst> {
        if types1.len() != types2.len() {
            return Err(Self::unification_error(
                format!("union with {} alternatives", types1.len()),
                format!("union with {} alternatives", types2.len()),
            ));
        }

        let mut sorted1 = types1.to_vec();
        let mut sorted2 = types2.to_vec();
        sorted1.sort();
        sorted2.sort();

        let mut subst = Subst::empty();
        for (t1, t2) in sorted1.iter().zip(sorted2.iter()) {
            let s = Self::unify_impl(&subst.apply(t1), &subst.apply(t2), registry, class_registry)?;
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
    pub(super) fn unify_union_with_type(
        union_types: &[Type], t: &Type, registry: &TypeVarConstraintRegistry, class_registry: Option<&ClassRegistry>,
    ) -> Result<Subst> {
        let mut errors = Vec::new();

        for union_member in union_types {
            match Self::unify_impl(union_member, t, registry, class_registry) {
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

        Err(Self::unification_error(
            format!("union {union_str}{error_details}"),
            t.to_string(),
        ))
    }
}

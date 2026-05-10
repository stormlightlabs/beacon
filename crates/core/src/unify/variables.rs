//! Type-variable binding and occurs-check logic.

use super::Unifier;
use crate::{ClassRegistry, Result, Subst, Type, TypeError, TypeVar, TypeVarConstraintRegistry};

impl Unifier {
    pub(super) fn unify_var(
        tv: &TypeVar, t: &Type, registry: &TypeVarConstraintRegistry, class_registry: Option<&ClassRegistry>,
    ) -> Result<Subst> {
        match t {
            Type::Var(tv2) if tv == tv2 => Ok(Subst::empty()),
            _ => {
                if Self::occurs_check(tv, t) {
                    return Err(TypeError::OccursCheckFailed(tv.clone(), t.to_string()).into());
                }

                if let Err(msg) = registry.validate_with_class_registry(tv.id, t, class_registry) {
                    return Err(TypeError::UnificationError(format!("TypeVar {tv}"), format!("{t} ({msg})")).into());
                }

                Ok(Subst::singleton(tv.clone(), t.clone()))
            }
        }
    }

    /// Occurs check: returns true if type variable occurs in the type
    pub(super) fn occurs_check(tv: &TypeVar, t: &Type) -> bool {
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
}

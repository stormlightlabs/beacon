//! Compound type unification for functions, tuples, and quantified types.

use super::Unifier;
use crate::{ClassRegistry, Result, Subst, Type, TypeVar, TypeVarConstraintRegistry};

impl Unifier {
    pub(super) fn unify_functions(
        args1: &[(String, Type)], ret1: &Type, args2: &[(String, Type)], ret2: &Type,
        registry: &TypeVarConstraintRegistry, class_registry: Option<&ClassRegistry>,
    ) -> Result<Subst> {
        if args1.len() != args2.len() {
            return Err(Self::unification_error(
                format!("function with {} arguments", args1.len()),
                format!("function with {} arguments", args2.len()),
            ));
        }

        let mut subst = Subst::empty();
        for ((_, ty1), (_, ty2)) in args1.iter().zip(args2.iter()) {
            let s = Self::unify_impl(&subst.apply(ty1), &subst.apply(ty2), registry, class_registry)?;
            subst = s.compose(subst);
        }

        let s = Self::unify_impl(&subst.apply(ret1), &subst.apply(ret2), registry, class_registry)?;
        Ok(s.compose(subst))
    }

    pub(super) fn unify_tuples(
        types1: &[Type], types2: &[Type], registry: &TypeVarConstraintRegistry, class_registry: Option<&ClassRegistry>,
    ) -> Result<Subst> {
        if types1.len() != types2.len() {
            return Err(Self::unification_error(
                format!("tuple with {} elements", types1.len()),
                format!("tuple with {} elements", types2.len()),
            ));
        }

        let mut subst = Subst::empty();
        for (t1, t2) in types1.iter().zip(types2.iter()) {
            let s = Self::unify_impl(&subst.apply(t1), &subst.apply(t2), registry, class_registry)?;
            subst = s.compose(subst);
        }
        Ok(subst)
    }

    pub(super) fn unify_foralls(
        quantified1: &[TypeVar], body1: &Type, quantified2: &[TypeVar], body2: &Type,
        registry: &TypeVarConstraintRegistry, class_registry: Option<&ClassRegistry>,
    ) -> Result<Subst> {
        if quantified1.len() != quantified2.len() {
            return Err(Self::unification_error(
                format!("ForAll with {} type parameters", quantified1.len()),
                format!("ForAll with {} type parameters", quantified2.len()),
            ));
        }

        let mut renaming = Subst::empty();
        for (tv1, tv2) in quantified1.iter().zip(quantified2.iter()) {
            renaming.insert(tv1.clone(), Type::Var(tv2.clone()));
        }

        let renamed_body1 = renaming.apply(body1);
        Self::unify_impl(&renamed_body1, body2, registry, class_registry)
    }
}

//! Unification Algorithm for the Hindley-Milner Type System
//!
//! This module implements Robinson's unification algorithm with extensions for Python types.

mod application;
mod compound;
mod errors;
mod records;
mod unions;
mod variables;

use crate::{ClassRegistry, Result, Subst, Type, TypeCtor, TypeError, TypeVarConstraintRegistry, Variance};

/// Unification algorithm for Hindley-Milner type system
pub struct Unifier;

impl Unifier {
    pub fn unify(t1: &Type, t2: &Type, registry: &TypeVarConstraintRegistry) -> Result<Subst> {
        Self::unify_impl(t1, t2, registry, None)
    }

    /// Unify two types with access to class metadata for structural protocol checking
    pub fn unify_with_class_registry(
        t1: &Type, t2: &Type, registry: &TypeVarConstraintRegistry, class_registry: &ClassRegistry,
    ) -> Result<Subst> {
        Self::unify_impl(t1, t2, registry, Some(class_registry))
    }
    fn unify_impl(
        t1: &Type, t2: &Type, registry: &TypeVarConstraintRegistry, class_registry: Option<&ClassRegistry>,
    ) -> Result<Subst> {
        match (t1, t2) {
            (Type::Con(TypeCtor::Any), _) | (_, Type::Con(TypeCtor::Any)) => Ok(Subst::empty()),
            (Type::Var(tv1), Type::Var(tv2)) if tv1 == tv2 => Ok(Subst::empty()),
            (Type::Var(tv), t) | (t, Type::Var(tv)) => Self::unify_var(tv, t, registry, class_registry),
            (Type::Con(tc1), Type::Con(tc2)) if tc1 == tc2 => Ok(Subst::empty()),
            (Type::App(f1, a1), Type::App(f2, a2)) => {
                let s1 = Self::unify_impl(f1, f2, registry, class_registry)?;

                let applied_a1 = s1.apply(a1);
                let applied_a2 = s1.apply(a2);

                let variance = Self::get_variance_for_app(f1);

                match variance {
                    Variance::Invariant => {
                        let has_type_var = Self::contains_type_var(&applied_a1) || Self::contains_type_var(&applied_a2);

                        if applied_a1 != applied_a2 {
                            match Self::unify_impl(&applied_a1, &applied_a2, registry, class_registry) {
                                Ok(s2) => Ok(s2.compose(s1)),
                                Err(_)
                                    if !matches!(applied_a1, Type::Var(_))
                                        && !matches!(applied_a2, Type::Var(_))
                                        && !has_type_var =>
                                {
                                    Err(TypeError::VarianceError {
                                        position: Self::get_type_constructor_name(f1),
                                        expected_variance: "invariant".to_string(),
                                        got_type: applied_a1.display_for_diagnostics(),
                                        expected_type: applied_a2.display_for_diagnostics(),
                                    }
                                    .into())
                                }
                                Err(_) if has_type_var => Ok(s1),
                                Err(e) => Err(e),
                            }
                        } else {
                            Ok(s1)
                        }
                    }
                    Variance::Covariant => {
                        let s2 = Self::unify_impl(&applied_a1, &applied_a2, registry, class_registry)?;
                        Ok(s2.compose(s1))
                    }
                    Variance::Contravariant => {
                        let s2 = Self::unify_impl(&applied_a2, &applied_a1, registry, class_registry)?;
                        Ok(s2.compose(s1))
                    }
                }
            }
            (Type::Fun(args1, ret1), Type::Fun(args2, ret2)) => {
                Self::unify_functions(args1, ret1, args2, ret2, registry, class_registry)
            }
            (Type::FunWithParams(args1, ret1), Type::FunWithParams(args2, ret2)) => {
                Self::unify_function_param_metadata(args1, ret1, args2, ret2, registry, class_registry)
            }
            (Type::Fun(args1, ret1), Type::FunWithParams(args2, ret2)) => {
                let args2 = args2.iter().map(|p| (p.name.clone(), p.ty.clone())).collect::<Vec<_>>();
                Self::unify_functions(args1, ret1, &args2, ret2, registry, class_registry)
            }
            (Type::FunWithParams(args1, ret1), Type::Fun(args2, ret2)) => {
                let args1 = args1.iter().map(|p| (p.name.clone(), p.ty.clone())).collect::<Vec<_>>();
                Self::unify_functions(&args1, ret1, args2, ret2, registry, class_registry)
            }
            (Type::Union(types1), Type::Union(types2)) => Self::unify_unions(types1, types2, registry, class_registry),
            (Type::Union(types), t) | (t, Type::Union(types)) => {
                Self::unify_union_with_type(types, t, registry, class_registry)
            }
            (Type::Record(fields1, row1), Type::Record(fields2, row2)) => {
                Self::unify_records(fields1, row1, fields2, row2, registry, class_registry)
            }
            (Type::BoundMethod(receiver1, _, method1), Type::BoundMethod(receiver2, _, method2)) => {
                let s1 = Self::unify_impl(receiver1, receiver2, registry, class_registry)?;
                let s2 = Self::unify_impl(&s1.apply(method1), &s1.apply(method2), registry, class_registry)?;
                Ok(s2.compose(s1))
            }
            (Type::BoundMethod(_, _, method), fun @ (Type::Fun(_, _) | Type::FunWithParams(_, _))) => {
                Self::unify_impl(method, fun, registry, class_registry)
            }
            (fun @ (Type::Fun(_, _) | Type::FunWithParams(_, _)), Type::BoundMethod(_, _, method)) => {
                Self::unify_impl(fun, method, registry, class_registry)
            }
            (Type::BoundMethod(_, _, method), other)
                if !matches!(
                    other,
                    Type::BoundMethod(_, _, _) | Type::Fun(_, _) | Type::FunWithParams(_, _)
                ) =>
            {
                Self::unify_impl(method, other, registry, class_registry)
            }
            (other, Type::BoundMethod(_, _, method))
                if !matches!(
                    other,
                    Type::BoundMethod(_, _, _) | Type::Fun(_, _) | Type::FunWithParams(_, _)
                ) =>
            {
                Self::unify_impl(other, method, registry, class_registry)
            }
            (Type::Tuple(types1), Type::Tuple(types2)) => Self::unify_tuples(types1, types2, registry, class_registry),
            (Type::ForAll(quantified1, body1), Type::ForAll(quantified2, body2)) => {
                Self::unify_foralls(quantified1, body1, quantified2, body2, registry, class_registry)
            }
            (Type::ForAll(_, body), t) | (t, Type::ForAll(_, body)) => {
                Self::unify_impl(body, t, registry, class_registry)
            }
            (Type::Con(TypeCtor::TypeVariable(_)), _) | (_, Type::Con(TypeCtor::TypeVariable(_))) => Ok(Subst::empty()),
            _ => Err(Self::unification_error(
                t1.display_for_diagnostics(),
                t2.display_for_diagnostics(),
            )),
        }
    }
    pub fn unify_many(types: &[Type], registry: &TypeVarConstraintRegistry) -> Result<(Type, Subst)> {
        if types.is_empty() {
            return Err(Self::unification_error("empty type list", "non-empty type list"));
        }

        if types.len() == 1 {
            return Ok((types[0].clone(), Subst::empty()));
        }

        let mut result_type = types[0].clone();
        let mut subst = Subst::empty();

        for t in &types[1..] {
            let s = Self::unify_impl(&subst.apply(&result_type), &subst.apply(t), registry, None)?;
            subst = s.compose(subst);
            result_type = subst.apply(&result_type);
        }

        Ok((result_type, subst))
    }
}

#[cfg(test)]
mod tests;

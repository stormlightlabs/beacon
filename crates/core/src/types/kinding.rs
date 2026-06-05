//! Free-variable and kinding helpers.

use super::{Kind, Type, TypeVar};
use rustc_hash::FxHashMap;
use std::ops::Not;

impl Type {
    pub fn free_vars(&self) -> FxHashMap<TypeVar, ()> {
        let mut vars = FxHashMap::default();
        self.collect_free_vars(&mut vars, &FxHashMap::default());
        vars
    }

    /// Compute the kind of this type
    ///
    /// Returns an error if the type is ill-kinded (e.g., `Int[String]`)
    pub fn kind_of(&self) -> Result<Kind, String> {
        match self {
            Type::Var(_) => Ok(Kind::Star),
            Type::Con(tc) => Ok(tc.kind()),
            Type::App(f, a) => match f.kind_of()? {
                Kind::Arrow(k1, k2) => {
                    let a_kind = a.kind_of()?;
                    if *k1 == a_kind {
                        Ok(*k2)
                    } else {
                        Err(format!(
                            "Kind mismatch in type application: expected {k1}, found {a_kind}"
                        ))
                    }
                }
                Kind::Star => Err(format!("Cannot apply type {f} :: * to argument {a}")),
            },
            Type::Fun(_, _) | Type::FunWithParams(_, _) => Ok(Kind::Star),
            Type::ForAll(_, t) => t.kind_of(),
            Type::Union(_) => Ok(Kind::Star),
            Type::Intersection(_) => Ok(Kind::Star),
            Type::Record(_, _) => Ok(Kind::Star),
            Type::BoundMethod(_, _, _) => Ok(Kind::Star),
            Type::Tuple(_) => Ok(Kind::Star),
        }
    }

    /// Check if this type is well-kinded
    ///
    /// A type is well-kinded if:
    /// 1. All type applications are kind-correct
    /// 2. All subterms are well-kinded
    /// 3. The type has kind * (is a proper type)
    pub fn check_well_kinded(&self) -> Result<(), String> {
        let kind = self.kind_of()?;
        match kind {
            Kind::Star => Ok(()),
            _ => Err(format!("Type {self} has kind {kind} but expected kind *")),
        }
    }

    fn collect_free_vars(&self, vars: &mut FxHashMap<TypeVar, ()>, bound: &FxHashMap<TypeVar, ()>) {
        match self {
            Type::Var(tv) => {
                if bound.contains_key(tv).not() {
                    vars.insert(tv.clone(), ());
                }
            }
            Type::Con(_) => {}
            Type::App(t1, t2) => {
                t1.collect_free_vars(vars, bound);
                t2.collect_free_vars(vars, bound);
            }
            Type::Fun(args, ret) => {
                for (_, ty) in args {
                    ty.collect_free_vars(vars, bound);
                }
                ret.collect_free_vars(vars, bound);
            }
            Type::FunWithParams(params, ret) => {
                for param in params {
                    param.ty.collect_free_vars(vars, bound);
                }
                ret.collect_free_vars(vars, bound);
            }
            Type::ForAll(tvs, t) => {
                let mut new_bound = bound.clone();
                for tv in tvs {
                    new_bound.insert(tv.clone(), ());
                }
                t.collect_free_vars(vars, &new_bound);
            }
            Type::Union(types) => {
                for t in types {
                    t.collect_free_vars(vars, bound);
                }
            }
            Type::Intersection(types) => {
                for t in types {
                    t.collect_free_vars(vars, bound);
                }
            }
            Type::Record(fields, row_var) => {
                for (_, t) in fields {
                    t.collect_free_vars(vars, bound);
                }
                if let Some(rv) = row_var
                    && bound.contains_key(rv).not()
                {
                    vars.insert(rv.clone(), ());
                }
            }
            Type::BoundMethod(receiver, _, method) => {
                receiver.collect_free_vars(vars, bound);
                method.collect_free_vars(vars, bound);
            }
            Type::Tuple(types) => {
                for t in types {
                    t.collect_free_vars(vars, bound);
                }
            }
        }
    }
}

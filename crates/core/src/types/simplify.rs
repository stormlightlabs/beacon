//! Post-substitution simplification helpers.

use super::{Type, TypeCtor};

impl Type {
    pub fn simplify(self) -> Type {
        match self {
            Type::Union(types) => {
                if types.iter().any(|t| matches!(t, Type::Con(TypeCtor::Any))) {
                    return Type::any();
                }

                Type::union(types)
            }
            Type::App(f, a) => Type::App(Box::new(f.simplify()), Box::new(a.simplify())),
            Type::Fun(args, ret) => Type::Fun(
                args.into_iter().map(|(name, ty)| (name, ty.simplify())).collect(),
                Box::new(ret.simplify()),
            ),
            Type::ForAll(tvs, t) => Type::ForAll(tvs, Box::new(t.simplify())),
            Type::Intersection(types) => Type::intersection(types.into_iter().map(|t| t.simplify()).collect()),
            Type::Record(fields, row_var) => {
                Type::Record(fields.into_iter().map(|(k, v)| (k, v.simplify())).collect(), row_var)
            }
            Type::BoundMethod(recv, name, method) => {
                Type::BoundMethod(Box::new(recv.simplify()), name, Box::new(method.simplify()))
            }
            other => other,
        }
    }
}

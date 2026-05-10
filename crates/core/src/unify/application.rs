//! Type application variance helpers for unification.

use super::Unifier;
use crate::{Type, TypeCtor, Variance};

impl Unifier {
    pub(super) fn get_type_constructor_name(ty: &Type) -> String {
        match ty {
            Type::Con(TypeCtor::List) => "list element".to_string(),
            Type::Con(TypeCtor::Dict) => "dict key/value".to_string(),
            Type::Con(TypeCtor::Set) => "set element".to_string(),
            Type::Con(TypeCtor::Tuple) => "tuple element".to_string(),
            Type::Con(TypeCtor::Iterator) => "iterator element".to_string(),
            Type::Con(TypeCtor::Iterable) => "iterable element".to_string(),
            Type::Con(TypeCtor::Generator) => "generator type argument".to_string(),
            Type::Con(TypeCtor::AsyncGenerator) => "async generator type argument".to_string(),
            Type::Con(TypeCtor::Coroutine) => "coroutine type argument".to_string(),
            Type::Con(TypeCtor::Class(name)) => format!("{name} type argument"),
            Type::App(ctor, _) => Self::get_type_constructor_name(ctor),
            _ => "type argument".to_string(),
        }
    }

    /// Extract the root type constructor and count parameters from a type application chain.
    ///
    /// For example:
    /// - `Dict[K, V]` = `App(App(Dict, K), V)` → `(Dict, 2)`
    /// - `List[T]` = `App(List, T)` → `(List, 1)`
    /// - `Generator[Y, S, R]` = `App(App(App(Gen, Y), S), R)` → `(Generator, 3)`
    pub(super) fn extract_type_ctor_and_param_count(ty: &Type) -> Option<(&TypeCtor, usize)> {
        let mut current = ty;
        let mut param_count = 0;

        while let Type::App(ctor, _) = current {
            param_count += 1;
            current = ctor.as_ref();
        }

        if let Type::Con(tc) = current { Some((tc, param_count)) } else { None }
    }

    /// Get the variance for a specific parameter index in a type application chain.
    ///
    /// Multi-parameter types like `Dict[K, V]` are represented as nested applications:
    /// `App(App(Dict, K), V)` where K is at parameter 0 and V is at parameter 1.
    ///
    /// This function determines which parameter position we're at based on the nesting depth.
    pub(super) fn get_variance_for_app(f: &Type) -> Variance {
        if let Some((root_ctor, total_params)) = Self::extract_type_ctor_and_param_count(f) {
            let param_index = total_params.saturating_sub(1);
            root_ctor.variance(param_index)
        } else {
            Variance::Invariant
        }
    }

    pub(super) fn contains_type_var(ty: &Type) -> bool {
        match ty {
            Type::Var(_) => true,
            Type::App(ctor, arg) => Self::contains_type_var(ctor) || Self::contains_type_var(arg),
            Type::Fun(args, ret) => {
                args.iter().any(|(_, arg)| Self::contains_type_var(arg)) || Self::contains_type_var(ret)
            }
            Type::Union(types) => types.iter().any(Self::contains_type_var),
            Type::Intersection(types) => types.iter().any(Self::contains_type_var),
            Type::Record(fields, row) => {
                fields.iter().any(|(_, field_ty)| Self::contains_type_var(field_ty)) || row.is_some()
            }
            Type::Tuple(elements) => elements.iter().any(Self::contains_type_var),
            Type::BoundMethod(receiver, _, method) => {
                Self::contains_type_var(receiver) || Self::contains_type_var(method)
            }
            _ => false,
        }
    }
}

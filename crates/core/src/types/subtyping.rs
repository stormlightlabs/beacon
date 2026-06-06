//! Subtyping and structural protocol checks.

use super::{Type, TypeCtor, Variance};
use crate::class_metadata::{ClassMetadata, ClassRegistry};

impl Type {
    pub fn is_subtype_of(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::Con(TypeCtor::Never), _) => return true,
            (_, Type::Con(TypeCtor::Top | TypeCtor::Any | TypeCtor::Unknown)) => return true,
            (_, Type::Con(TypeCtor::Never)) => return false,
            (Type::Con(TypeCtor::Top), _) => return false,
            (Type::Con(TypeCtor::Any | TypeCtor::Unknown), _) => return false,
            _ => {}
        }

        if self == other {
            return true;
        }

        match (self, other) {
            // Union[A, B] <: T if both A <: T and B <: T
            (Type::Union(xs), _) => return xs.iter().all(|x| x.is_subtype_of(other)),
            // T <: Union[A, B] if T <: A or T <: B
            (_, Type::Union(ys)) => return ys.iter().any(|y| self.is_subtype_of(y)),
            _ => {}
        }

        match (self, other) {
            // Intersection[A, B, ...] <: T if ANY component is a subtype of T
            // Rationale: Intersection is more specific than any of its components
            (Type::Intersection(xs), _) => return xs.iter().any(|x| x.is_subtype_of(other)),
            // T <: Intersection[A, B, ...] if T is a subtype of ALL components
            // Rationale: T must satisfy all requirements in the intersection
            (_, Type::Intersection(ys)) => return ys.iter().all(|y| self.is_subtype_of(y)),
            _ => {}
        }

        if let (Type::Fun(self_params, self_ret), Type::Fun(other_params, other_ret)) = (self, other) {
            if self_params.len() != other_params.len() {
                return false;
            }

            let params_ok = self_params
                .iter()
                .zip(other_params)
                .all(|((_s_name, s_ty), (_o_name, o_ty))| o_ty.is_subtype_of(s_ty));

            if !params_ok {
                return false;
            }

            return self_ret.is_subtype_of(other_ret);
        }

        if let (Type::App(self_ctor, self_arg), Type::App(other_ctor, other_arg)) = (self, other) {
            if !self_ctor.is_subtype_of(other_ctor) {
                return false;
            }

            let variance = Self::get_app_variance(self_ctor.as_ref());

            return match variance {
                Variance::Covariant => self_arg.is_subtype_of(other_arg),
                Variance::Contravariant => other_arg.is_subtype_of(self_arg),
                Variance::Invariant => self_arg == other_arg,
            };
        }

        if let (Type::Record(self_fields, _), Type::Record(other_fields, _)) = (self, other) {
            for (k_other, t_other) in other_fields {
                match self_fields.iter().find(|(k, _)| k == k_other) {
                    Some((_, t_self)) if t_self.is_subtype_of(t_other) => continue,
                    _ => return false,
                }
            }
            return true;
        }

        if let (Type::BoundMethod(self_recv, _, self_meth), Type::BoundMethod(other_recv, _, other_meth)) =
            (self, other)
        {
            return self_recv.is_subtype_of(other_recv) && self_meth.is_subtype_of(other_meth);
        }

        if let (Type::ForAll(_, self_body), Type::ForAll(_, other_body)) = (self, other) {
            return self_body.is_subtype_of(other_body);
        }

        false
    }

    /// Check subtyping with structural protocol support using class metadata when available.
    ///
    /// Falls back to nominal/structural checks from [Self::is_subtype_of] when class metadata
    /// is not provided.
    pub fn is_subtype_of_with_registry(&self, other: &Type, class_registry: Option<&ClassRegistry>) -> bool {
        if self.is_subtype_of(other) {
            return true;
        }

        let Some(registry) = class_registry else {
            return false;
        };

        self.structural_subtype_check(other, registry).unwrap_or(false)
    }

    fn structural_subtype_check(&self, other: &Type, class_registry: &ClassRegistry) -> Option<bool> {
        match other {
            Type::Union(types) => {
                return Some(
                    types
                        .iter()
                        .any(|t| self.is_subtype_of_with_registry(t, Some(class_registry))),
                );
            }
            Type::Intersection(types) => {
                return Some(
                    types
                        .iter()
                        .all(|t| self.is_subtype_of_with_registry(t, Some(class_registry))),
                );
            }
            _ => {}
        }

        match self {
            Type::Union(types) => {
                return Some(
                    types
                        .iter()
                        .all(|t| t.is_subtype_of_with_registry(other, Some(class_registry))),
                );
            }
            Type::Intersection(types) => {
                return Some(
                    types
                        .iter()
                        .any(|t| t.is_subtype_of_with_registry(other, Some(class_registry))),
                );
            }
            _ => {}
        }

        let protocol_record = other.protocol_structural_record(class_registry)?;
        let actual_record = self.class_structural_record(class_registry)?;
        Some(actual_record.is_subtype_of(&protocol_record))
    }

    /// Convert a class or protocol type into its structural record representation if possible.
    fn class_structural_record(&self, class_registry: &ClassRegistry) -> Option<Type> {
        if matches!(self, Type::Record(_, _)) {
            return Some(self.clone());
        }

        let (name, args) = self.class_like_name_and_args()?;
        let mut record = class_registry.class_to_record(&name)?;

        if let Some(metadata) = class_registry.get_class(&name)
            && !metadata.type_params.is_empty()
            && !args.is_empty()
        {
            let subst = metadata.create_type_substitution(&args);
            record = ClassMetadata::substitute_type_params(&record, &subst);
        }

        Some(record)
    }

    /// Convert a protocol type into its structural record representation if possible.
    fn protocol_structural_record(&self, class_registry: &ClassRegistry) -> Option<Type> {
        let (name, args) = self.class_like_name_and_args()?;
        let metadata = class_registry.get_class(&name)?;
        if !metadata.is_protocol {
            return None;
        }

        let mut record = class_registry.class_to_record(&name)?;
        if !metadata.type_params.is_empty() && !args.is_empty() {
            let subst = metadata.create_type_substitution(&args);
            record = ClassMetadata::substitute_type_params(&record, &subst);
        }
        Some(record)
    }

    fn class_like_name_and_args(&self) -> Option<(String, Vec<Type>)> {
        match self {
            Type::Con(TypeCtor::Class(name)) => return Some((name.clone(), Vec::new())),
            Type::Con(TypeCtor::Protocol(Some(name), _)) => return Some((name.clone(), Vec::new())),
            Type::Con(TypeCtor::Literal(lit)) => {
                if let Some(base_ctor) = TypeCtor::Literal(lit.clone()).base_type()
                    && let Some(name) = Self::builtin_class_name(&base_ctor)
                {
                    return Some((name.to_string(), Vec::new()));
                }
            }
            Type::Con(ctor) => {
                if let Some(name) = Self::builtin_class_name(ctor) {
                    return Some((name.to_string(), Vec::new()));
                }
            }
            _ => {}
        }

        if let Some((ctor, args)) = self.unapply() {
            let name = match ctor {
                TypeCtor::Class(name) => Some(name.clone()),
                TypeCtor::Protocol(Some(name), _) => Some(name.clone()),
                TypeCtor::Literal(lit) => TypeCtor::Literal(lit.clone())
                    .base_type()
                    .and_then(|base| Self::builtin_class_name(&base).map(|s| s.to_string())),
                other => Self::builtin_class_name(other).map(|s| s.to_string()),
            };

            if let Some(name) = name {
                return Some((name, args));
            }
        }

        None
    }

    fn builtin_class_name(ctor: &TypeCtor) -> Option<&'static str> {
        match ctor {
            TypeCtor::Int => Some("int"),
            TypeCtor::Float => Some("float"),
            TypeCtor::String => Some("str"),
            TypeCtor::Bool => Some("bool"),
            TypeCtor::NoneType => Some("None"),
            TypeCtor::List => Some("list"),
            TypeCtor::Dict => Some("dict"),
            TypeCtor::Set => Some("set"),
            TypeCtor::Tuple => Some("tuple"),
            TypeCtor::Iterator => Some("Iterator"),
            TypeCtor::Iterable => Some("Iterable"),
            TypeCtor::Generator => Some("Generator"),
            TypeCtor::AsyncGenerator => Some("AsyncGenerator"),
            TypeCtor::Coroutine => Some("Coroutine"),
            _ => None,
        }
    }
}

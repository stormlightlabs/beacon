//! Shared type helper APIs used by downstream crates.

use super::{LiteralType, Type, TypeCtor};

/// Return Beacon's canonical Python builtin name for a type constructor.
pub fn builtin_type_name(ctor: &TypeCtor) -> Option<&'static str> {
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
        TypeCtor::Function => Some("function"),
        TypeCtor::Iterator => Some("Iterator"),
        TypeCtor::Iterable => Some("Iterable"),
        TypeCtor::Generator => Some("Generator"),
        TypeCtor::AsyncGenerator => Some("AsyncGenerator"),
        TypeCtor::Coroutine => Some("Coroutine"),
        TypeCtor::Any => Some("Any"),
        TypeCtor::Top => Some("Top"),
        TypeCtor::Never => Some("Never"),
        TypeCtor::Generic => Some("Generic"),
        TypeCtor::Protocol(_, _) => Some("Protocol"),
        TypeCtor::Literal(literal) => literal_base_ctor(literal).and_then(|base| builtin_type_name(&base)),
        TypeCtor::TypeVariable(_) | TypeCtor::Class(_) | TypeCtor::Module(_) => None,
    }
}

/// Convert a literal singleton into its non-literal base constructor.
pub fn literal_base_ctor(literal: &LiteralType) -> Option<TypeCtor> {
    match literal {
        LiteralType::Int(_) => Some(TypeCtor::Int),
        LiteralType::Bool(_) => Some(TypeCtor::Bool),
        LiteralType::String(_) => Some(TypeCtor::String),
        LiteralType::None => Some(TypeCtor::NoneType),
    }
}

/// Convert a literal type into its non-literal base type.
pub fn literal_to_base_type(ty: &Type) -> Option<Type> {
    match ty {
        Type::Con(TypeCtor::Literal(literal)) => literal_base_ctor(literal).map(Type::Con),
        _ => None,
    }
}

/// True when the type is Beacon's gradual `Any`.
pub fn is_any(ty: &Type) -> bool {
    matches!(ty, Type::Con(TypeCtor::Any))
}

/// True when the type contains at least one type variable.
pub fn contains_type_var(ty: &Type) -> bool {
    match ty {
        Type::Var(_) => true,
        Type::Con(_) => false,
        Type::App(ctor, arg) => contains_type_var(ctor) || contains_type_var(arg),
        Type::Fun(args, ret) => args.iter().any(|(_, arg)| contains_type_var(arg)) || contains_type_var(ret),
        Type::ForAll(_, body) => contains_type_var(body),
        Type::Union(types) | Type::Intersection(types) | Type::Tuple(types) => types.iter().any(contains_type_var),
        Type::Record(fields, row) => row.is_some() || fields.iter().any(|(_, field)| contains_type_var(field)),
        Type::BoundMethod(receiver, _, method) => contains_type_var(receiver) || contains_type_var(method),
    }
}

/// Decompose a type application into its constructor and type arguments.
pub fn decompose_app(ty: &Type) -> Option<(&TypeCtor, Vec<Type>)> {
    ty.unapply()
}

/// Decompose a parameterized class type into its class name and type arguments.
pub fn decompose_class_app(ty: &Type) -> Option<(&str, Vec<Type>)> {
    ty.unapply_class()
}

/// Decompose a parameterized protocol type into its protocol name and type arguments.
pub fn decompose_protocol_app(ty: &Type) -> Option<(&str, Vec<Type>)> {
    ty.unapply_protocol()
}

/// Format a type for diagnostics.
pub fn format_type_for_diagnostic(ty: &Type) -> String {
    ty.display_for_diagnostics()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn literal_base_conversion_matches_core_literals() {
        assert_eq!(literal_to_base_type(&Type::literal_int(1)), Some(Type::int()));
        assert_eq!(literal_to_base_type(&Type::literal_bool(true)), Some(Type::bool()));
        assert_eq!(
            literal_to_base_type(&Type::literal_string("x".to_string())),
            Some(Type::string())
        );
        assert_eq!(literal_to_base_type(&Type::literal_none()), Some(Type::none()));
    }

    #[test]
    fn app_decomposition_reuses_core_shape() {
        let ty = Type::dict(Type::string(), Type::int());
        let (ctor, args) = decompose_app(&ty).expect("dict should decompose");
        assert_eq!(ctor, &TypeCtor::Dict);
        assert_eq!(args, vec![Type::string(), Type::int()]);
    }
}

use beacon_core::{ClassRegistry, Type, TypeCtor};
use beacon_parser::{AstNode, LiteralValue};

pub fn is_sequence_type(ty: &Type) -> bool {
    match ty {
        Type::App(ctor, _) => is_sequence_type(ctor),
        Type::Con(TypeCtor::List | TypeCtor::Tuple) | Type::Tuple(_) => true,
        _ => false,
    }
}

pub fn is_dict_type(ty: &Type) -> bool {
    is_mapping_type(ty)
}

pub fn is_mapping_type(ty: &Type) -> bool {
    match ty {
        Type::App(ctor, _) => is_mapping_type(ctor),
        Type::Con(TypeCtor::Dict) => true,
        _ => false,
    }
}

pub fn is_open_subject_type(subject_type: &Type) -> bool {
    match subject_type {
        Type::Con(TypeCtor::Any) | Type::Con(TypeCtor::Top) => true,
        Type::Con(TypeCtor::Class(class_name)) if class_name == "object" => true,
        _ => false,
    }
}

pub fn literal_pattern_type(literal: &AstNode) -> Option<Type> {
    match literal {
        AstNode::Literal { value, .. } => match value {
            LiteralValue::Integer(_) => Some(Type::int()),
            LiteralValue::Float(_) => Some(Type::float()),
            LiteralValue::String { .. } => Some(Type::string()),
            LiteralValue::Boolean(_) => Some(Type::bool()),
            LiteralValue::None => Some(Type::none()),
        },
        AstNode::Identifier { .. } => None,
        _ => None,
    }
}

pub fn type_compatible_with_class(ty: &Type, cls: &str) -> bool {
    type_compatible_with_class_registry(ty, cls, None)
}

pub fn class_pattern_matches_type(cls: &str, ty: &Type, class_registry: &ClassRegistry) -> bool {
    type_compatible_with_class_registry(ty, cls, Some(class_registry))
}

pub fn type_compatible_with_class_registry(ty: &Type, cls: &str, class_registry: Option<&ClassRegistry>) -> bool {
    match (cls, ty) {
        ("int", Type::Con(TypeCtor::Int))
        | ("int", Type::Con(TypeCtor::Bool))
        | ("str", Type::Con(TypeCtor::String))
        | ("bool", Type::Con(TypeCtor::Bool))
        | ("float", Type::Con(TypeCtor::Float)) => true,
        (pattern_class, Type::Con(TypeCtor::Class(subject_class))) => {
            pattern_class == subject_class
                || class_registry.is_some_and(|registry| registry.is_subclass_of(subject_class, pattern_class))
        }
        _ => false,
    }
}

pub fn types_could_match(pattern_type: &Type, subject_type: &Type) -> bool {
    match (pattern_type, subject_type) {
        (Type::Var(_), _) | (_, Type::Var(_)) => true,
        (Type::Con(a), Type::Con(b)) => a == b,
        (pt, Type::Union(variants)) => variants.iter().any(|v| types_could_match(pt, v)),
        (Type::App(a_ctor, _), Type::App(b_ctor, _)) => types_could_match(a_ctor, b_ctor),
        _ => true,
    }
}

pub fn extract_tuple_arity(ty: &Type) -> Option<usize> {
    match ty {
        Type::Tuple(types) => Some(types.len()),
        _ => None,
    }
}

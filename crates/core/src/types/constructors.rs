//! Constructors for common Beacon types.

use super::{LiteralType, Type, TypeCtor};

impl Type {
    pub fn int() -> Self {
        Type::Con(TypeCtor::Int)
    }

    pub fn float() -> Self {
        Type::Con(TypeCtor::Float)
    }

    pub fn string() -> Self {
        Type::Con(TypeCtor::String)
    }

    pub fn bool() -> Self {
        Type::Con(TypeCtor::Bool)
    }

    pub fn none() -> Self {
        Type::Con(TypeCtor::NoneType)
    }

    /// Create a literal int type (e.g., Literal[42])
    pub fn literal_int(value: i64) -> Self {
        Type::Con(TypeCtor::Literal(LiteralType::Int(value)))
    }

    /// Create a literal bool type (e.g., Literal[True])
    pub fn literal_bool(value: bool) -> Self {
        Type::Con(TypeCtor::Literal(LiteralType::Bool(value)))
    }

    /// Create a literal string type (e.g., Literal["hello"])
    pub fn literal_string(value: String) -> Self {
        Type::Con(TypeCtor::Literal(LiteralType::String(value)))
    }

    /// Create a literal None type (Literal[None])
    pub fn literal_none() -> Self {
        Type::Con(TypeCtor::Literal(LiteralType::None))
    }

    pub fn any() -> Self {
        Type::Con(TypeCtor::Any)
    }

    pub fn top() -> Self {
        Type::Con(TypeCtor::Top)
    }

    pub fn never() -> Self {
        Type::Con(TypeCtor::Never)
    }

    /// Create a list type
    pub fn list(element_type: Type) -> Self {
        Type::App(Box::new(Type::Con(TypeCtor::List)), Box::new(element_type))
    }

    /// Create a dict type
    pub fn dict(key_type: Type, value_type: Type) -> Self {
        Type::App(
            Box::new(Type::App(Box::new(Type::Con(TypeCtor::Dict)), Box::new(key_type))),
            Box::new(value_type),
        )
    }

    /// Create a set type
    pub fn set(element_type: Type) -> Self {
        Type::App(Box::new(Type::Con(TypeCtor::Set)), Box::new(element_type))
    }

    /// Create a homogeneous tuple type: tuple[T]
    pub fn tuple(element_type: Type) -> Self {
        Type::App(Box::new(Type::Con(TypeCtor::Tuple)), Box::new(element_type))
    }

    /// Create a heterogeneous tuple type: tuple[T1, T2, ..., Tn]
    pub fn tuple_heterogeneous(element_types: Vec<Type>) -> Self {
        Type::Tuple(element_types)
    }

    /// Create a Generator type: Generator[YieldType, SendType, ReturnType]
    pub fn generator(yield_ty: Type, send_ty: Type, return_ty: Type) -> Self {
        Type::App(
            Box::new(Type::App(
                Box::new(Type::App(Box::new(Type::Con(TypeCtor::Generator)), Box::new(yield_ty))),
                Box::new(send_ty),
            )),
            Box::new(return_ty),
        )
    }

    /// Create an AsyncGenerator type: AsyncGenerator[YieldType, SendType]
    pub fn async_generator(yield_ty: Type, send_ty: Type) -> Self {
        Type::App(
            Box::new(Type::App(
                Box::new(Type::Con(TypeCtor::AsyncGenerator)),
                Box::new(yield_ty),
            )),
            Box::new(send_ty),
        )
    }

    /// Create a Coroutine type: Coroutine[YieldType, SendType, ReturnType]
    pub fn coroutine(yield_ty: Type, send_ty: Type, return_ty: Type) -> Self {
        Type::App(
            Box::new(Type::App(
                Box::new(Type::App(Box::new(Type::Con(TypeCtor::Coroutine)), Box::new(yield_ty))),
                Box::new(send_ty),
            )),
            Box::new(return_ty),
        )
    }
}

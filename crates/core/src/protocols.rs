//! Protocol definitions for structural typing
//!
//! This module defines Python protocols (also known as interfaces or traits in other languages).
//! Protocols enable structural subtyping: A type satisfies a protocol if it provides the required methods and attributes,
//! regardless of explicit inheritance.
//!
//! ## Protocol System
//!
//! Python protocols are checked structurally rather than nominally:
//! - **Nominal typing**: explicit inheritance (class Foo(Protocol))
//! - **Structural typing**: implicit satisfaction (has required methods/attributes)
//!
//! ## Supported Protocols
//!
//! - **Iterator[T]**: Objects that can be iterated (__iter__, __next__)
//! - **Iterable[T]**: Objects that return iterators (__iter__)
//! - **Sized**: Objects with a length (__len__)
//! - **Sequence[T]**: Ordered collections with indexing (__getitem__, __len__)
//! - **Mapping[K,V]**: Key-value stores (__getitem__, __len__, keys)
//! - **ContextManager[T]**: Objects usable in `with` statements (__enter__, __exit__)
//!
//! ## Example
//!
//! ```
//! use beacon_core::{ProtocolName, ProtocolChecker, Type};
//!
//! let list_type = Type::list(Type::int());
//! assert!(ProtocolChecker::satisfies(&list_type, &ProtocolName::Iterable));
//! ```

use crate::{Type, TypeCtor};

/// Protocol names for structural typing
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ProtocolName {
    /// Iterator protocol: __iter__() -> Self, __next__() -> T
    Iterator,
    /// Iterable protocol: __iter__() -> Iterator[T]
    Iterable,
    /// Async iterator protocol: __aiter__() -> Self, __anext__() -> Awaitable[T]
    AsyncIterator,
    /// Async iterable protocol: __aiter__() -> AsyncIterator[T]
    AsyncIterable,
    /// Awaitable protocol: __await__() -> Iterator[Any, Any, T]
    Awaitable,
    /// Sized protocol: __len__() -> int
    Sized,
    /// Sequence protocol: __getitem__(int) -> T, __len__() -> int
    Sequence,
    /// Mapping protocol: __getitem__(K) -> V, __len__() -> int
    Mapping,
    /// Context manager protocol: __enter__() -> T, __exit__(...) -> bool
    ContextManager,
    /// Async context manager protocol: __aenter__() -> Awaitable[T], __aexit__(...) -> Awaitable[bool]
    AsyncContextManager,
    /// Callable protocol: __call__(...) -> T
    Callable,
    /// User-defined protocol with the given name
    UserDefined(String),
}

impl std::fmt::Display for ProtocolName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ProtocolName::Iterator => write!(f, "Iterator"),
            ProtocolName::Iterable => write!(f, "Iterable"),
            ProtocolName::AsyncIterator => write!(f, "AsyncIterator"),
            ProtocolName::AsyncIterable => write!(f, "AsyncIterable"),
            ProtocolName::Awaitable => write!(f, "Awaitable"),
            ProtocolName::Sized => write!(f, "Sized"),
            ProtocolName::Sequence => write!(f, "Sequence"),
            ProtocolName::Mapping => write!(f, "Mapping"),
            ProtocolName::ContextManager => write!(f, "ContextManager"),
            ProtocolName::AsyncContextManager => write!(f, "AsyncContextManager"),
            ProtocolName::Callable => write!(f, "Callable"),
            ProtocolName::UserDefined(name) => write!(f, "{name}"),
        }
    }
}

/// Required method signature for a protocol
#[derive(Debug, Clone)]
pub struct MethodSignature {
    pub name: String,
    pub params: Vec<Type>,
    pub return_type: Type,
}

/// Protocol definition with required methods
#[derive(Debug, Clone)]
pub struct ProtocolDef {
    pub name: ProtocolName,
    pub required_methods: Vec<MethodSignature>,
}

impl ProtocolDef {
    /// Get the protocol definition for a protocol name
    pub fn get(protocol: &ProtocolName) -> Self {
        match protocol {
            ProtocolName::Iterator => Self {
                name: ProtocolName::Iterator,
                required_methods: vec![
                    MethodSignature { name: "__iter__".to_string(), params: vec![], return_type: Type::any() },
                    MethodSignature { name: "__next__".to_string(), params: vec![], return_type: Type::any() },
                ],
            },
            ProtocolName::Iterable => Self {
                name: ProtocolName::Iterable,
                required_methods: vec![MethodSignature {
                    name: "__iter__".to_string(),
                    params: vec![],
                    return_type: Type::any(),
                }],
            },
            ProtocolName::AsyncIterator => Self {
                name: ProtocolName::AsyncIterator,
                required_methods: vec![
                    MethodSignature { name: "__aiter__".to_string(), params: vec![], return_type: Type::any() },
                    MethodSignature { name: "__anext__".to_string(), params: vec![], return_type: Type::any() },
                ],
            },
            ProtocolName::AsyncIterable => Self {
                name: ProtocolName::AsyncIterable,
                required_methods: vec![MethodSignature {
                    name: "__aiter__".to_string(),
                    params: vec![],
                    return_type: Type::any(),
                }],
            },
            ProtocolName::Awaitable => Self {
                name: ProtocolName::Awaitable,
                required_methods: vec![MethodSignature {
                    name: "__await__".to_string(),
                    params: vec![],
                    return_type: Type::any(),
                }],
            },
            ProtocolName::Sized => Self {
                name: ProtocolName::Sized,
                required_methods: vec![MethodSignature {
                    name: "__len__".to_string(),
                    params: vec![],
                    return_type: Type::int(),
                }],
            },
            ProtocolName::Sequence => Self {
                name: ProtocolName::Sequence,
                required_methods: vec![
                    MethodSignature {
                        name: "__getitem__".to_string(),
                        params: vec![Type::int()],
                        return_type: Type::any(),
                    },
                    MethodSignature { name: "__len__".to_string(), params: vec![], return_type: Type::int() },
                ],
            },
            ProtocolName::Mapping => Self {
                name: ProtocolName::Mapping,
                required_methods: vec![
                    MethodSignature {
                        name: "__getitem__".to_string(),
                        params: vec![Type::any()],
                        return_type: Type::any(),
                    },
                    MethodSignature { name: "__len__".to_string(), params: vec![], return_type: Type::int() },
                ],
            },
            ProtocolName::ContextManager => Self {
                name: ProtocolName::ContextManager,
                required_methods: vec![
                    MethodSignature { name: "__enter__".to_string(), params: vec![], return_type: Type::any() },
                    MethodSignature {
                        name: "__exit__".to_string(),
                        params: vec![Type::any(), Type::any(), Type::any()],
                        return_type: Type::bool(),
                    },
                ],
            },
            ProtocolName::AsyncContextManager => Self {
                name: ProtocolName::AsyncContextManager,
                required_methods: vec![
                    MethodSignature { name: "__aenter__".to_string(), params: vec![], return_type: Type::any() },
                    MethodSignature {
                        name: "__aexit__".to_string(),
                        params: vec![Type::any(), Type::any(), Type::any()],
                        return_type: Type::any(),
                    },
                ],
            },
            ProtocolName::Callable => Self {
                name: ProtocolName::Callable,
                required_methods: vec![MethodSignature {
                    name: "__call__".to_string(),
                    params: vec![],
                    return_type: Type::any(),
                }],
            },
            ProtocolName::UserDefined(name) => {
                Self { name: ProtocolName::UserDefined(name.clone()), required_methods: vec![] }
            }
        }
    }

    /// Check if required methods are present.
    /// For full signature checking with variance, use [Self::check_method_signatures].
    pub fn check_method_names(&self, available_methods: &[String]) -> bool {
        self.required_methods
            .iter()
            .all(|req| available_methods.contains(&req.name))
    }

    /// Check if required methods are present with compatible signatures
    ///
    /// Performs full structural subtyping with variance rules:
    /// - Parameter types: protocol requirements must be subtypes of implementation params (contravariant)
    /// - Return types: implementation returns must be subtypes of protocol requirements (covariant)
    ///
    /// This enables proper protocol satisfaction checking where an implementing class can have:
    /// - More general parameter types (can accept a wider range of inputs)
    /// - More specific return types (provides more specific outputs)
    ///
    /// # Arguments
    /// * `available_methods` - Methods available on the type being checked, as (name, signature) pairs
    ///
    /// # Returns
    /// * `Ok(())` if all required methods are present with compatible signatures
    /// * `Err(String)` with a description of the first incompatibility found
    ///
    /// # Examples
    /// ```
    /// use beacon_core::{ProtocolDef, ProtocolName, MethodSignature, Type};
    ///
    /// let protocol = ProtocolDef::get(&ProtocolName::Sized);
    /// let available = vec![(
    ///     "__len__".to_string(),
    ///     MethodSignature {
    ///         name: "__len__".to_string(),
    ///         params: vec![],
    ///         return_type: Type::int(),
    ///     }
    /// )];
    ///
    /// assert!(protocol.check_method_signatures(&available).is_ok());
    /// ```
    pub fn check_method_signatures(&self, available_methods: &[(String, MethodSignature)]) -> Result<(), String> {
        for required in &self.required_methods {
            let found = available_methods.iter().find(|(name, _)| name == &required.name);

            let Some((_, provided)) = found else {
                return Err(format!("Missing required method '{}'", required.name));
            };

            if provided.params.len() != required.params.len() {
                return Err(format!(
                    "Method '{}' has {} parameters but protocol requires {}",
                    required.name,
                    provided.params.len(),
                    required.params.len()
                ));
            }

            for (i, (req_param, prov_param)) in required.params.iter().zip(&provided.params).enumerate() {
                if !req_param.is_subtype_of(prov_param) {
                    return Err(format!(
                        "Method '{}' parameter {} incompatible: expected parameter accepting {}, but found {}",
                        required.name, i, req_param, prov_param
                    ));
                }
            }

            if !provided.return_type.is_subtype_of(&required.return_type) {
                return Err(format!(
                    "Method '{}' return type incompatible: expected {}, but found {}",
                    required.name, required.return_type, provided.return_type
                ));
            }
        }

        Ok(())
    }
}

/// Protocol checker for determining if a type satisfies a protocol
pub struct ProtocolChecker;

impl ProtocolChecker {
    /// Check if a type satisfies a protocol
    ///
    /// Handles builtin types, union types, and intersection types.
    /// - For union types: T satisfies the protocol if ALL variants satisfy it
    /// - For intersection types: The type must satisfy ALL protocol requirements
    pub fn satisfies(ty: &Type, protocol: &ProtocolName) -> bool {
        if let Type::Union(variants) = ty {
            return variants.iter().all(|variant| Self::satisfies(variant, protocol));
        }

        if let Type::Intersection(types) = ty {
            return types.iter().any(|t| Self::satisfies(t, protocol));
        }

        match (ty, protocol) {
            (Type::App(ctor, _), ProtocolName::Iterable) => {
                matches!(
                    ctor.as_ref(),
                    Type::Con(TypeCtor::List)
                        | Type::Con(TypeCtor::Set)
                        | Type::Con(TypeCtor::Tuple)
                        | Type::Con(TypeCtor::Iterable)
                        | Type::Con(TypeCtor::Iterator)
                ) || matches!(ctor.as_ref(), Type::App(inner, _) if matches!(inner.as_ref(), Type::Con(TypeCtor::Dict)))
            }
            (Type::App(ctor, _), ProtocolName::Sequence) => {
                matches!(ctor.as_ref(), Type::Con(TypeCtor::List) | Type::Con(TypeCtor::Tuple))
            }
            (Type::App(inner, _), ProtocolName::Mapping) => {
                matches!(inner.as_ref(), Type::App(ctor, _) if matches!(ctor.as_ref(), Type::Con(TypeCtor::Dict)))
            }
            (Type::App(ctor, _), ProtocolName::Sized) => {
                matches!(
                    ctor.as_ref(),
                    Type::Con(TypeCtor::List) | Type::Con(TypeCtor::Set) | Type::Con(TypeCtor::Tuple)
                ) || matches!(ctor.as_ref(), Type::App(inner_ctor, _) if matches!(inner_ctor.as_ref(), Type::Con(TypeCtor::Dict)))
            }
            (Type::App(inner, _), ProtocolName::AsyncIterator) => {
                matches!(inner.as_ref(), Type::App(ctor, _) if matches!(ctor.as_ref(), Type::Con(TypeCtor::AsyncGenerator)))
            }
            (Type::App(inner, _), ProtocolName::AsyncIterable) => {
                matches!(inner.as_ref(), Type::App(ctor, _) if matches!(ctor.as_ref(), Type::Con(TypeCtor::AsyncGenerator)))
            }
            (Type::App(inner, _), ProtocolName::Awaitable) => {
                if let Type::App(ctor, _) = inner.as_ref()
                    && matches!(ctor.as_ref(), Type::Con(TypeCtor::AsyncGenerator)) {
                        return true;
                    }
                if let Type::App(inner2, _) = inner.as_ref()
                    && let Type::App(ctor, _) = inner2.as_ref()
                        && matches!(ctor.as_ref(), Type::Con(TypeCtor::Coroutine)) {
                            return true;
                        }

                false
            }
            (Type::Con(TypeCtor::String), ProtocolName::Sized) => true,
            (Type::Con(TypeCtor::Any), _) => true,
            (Type::Con(TypeCtor::Protocol(Some(name), _)), ProtocolName::UserDefined(proto_name)) => name == proto_name,
            _ => false,
        }
    }

    /// Extract element type from a type that satisfies Iterable[T]
    pub fn extract_iterable_element(ty: &Type) -> Type {
        match ty {
            Type::App(ctor, elem) if matches!(ctor.as_ref(), Type::Con(TypeCtor::List)) => elem.as_ref().clone(),
            Type::App(ctor, elem) if matches!(ctor.as_ref(), Type::Con(TypeCtor::Set)) => elem.as_ref().clone(),
            Type::App(ctor, elem) if matches!(ctor.as_ref(), Type::Con(TypeCtor::Tuple)) => elem.as_ref().clone(),
            Type::App(ctor, elem) if matches!(ctor.as_ref(), Type::Con(TypeCtor::Iterable)) => elem.as_ref().clone(),
            Type::App(ctor, elem) if matches!(ctor.as_ref(), Type::Con(TypeCtor::Iterator)) => elem.as_ref().clone(),
            Type::App(inner, _) => {
                if let Type::App(ctor, key) = inner.as_ref()
                    && matches!(ctor.as_ref(), Type::Con(TypeCtor::Dict)) {
                        return key.as_ref().clone();
                    }
                Type::any()
            }
            Type::Con(TypeCtor::String) => Type::string(),
            _ => Type::any(),
        }
    }

    /// Extract value type from a type that satisfies Mapping[K, V]
    ///
    /// Returns the value type V if the type is a mapping, otherwise returns Any.
    pub fn extract_mapping_value(ty: &Type) -> Type {
        match ty {
            Type::App(inner, val) => {
                if let Type::App(ctor, _) = inner.as_ref()
                    && matches!(ctor.as_ref(), Type::Con(TypeCtor::Dict)) {
                        return val.as_ref().clone();
                    }
                Type::any()
            }
            _ => Type::any(),
        }
    }

    /// Extract key type from a type that satisfies Mapping[K, V]
    ///
    /// Returns the key type K if the type is a mapping, otherwise returns Any.
    pub fn extract_mapping_key(ty: &Type) -> Type {
        match ty {
            Type::App(inner, _) => {
                if let Type::App(ctor, key) = inner.as_ref()
                    && matches!(ctor.as_ref(), Type::Con(TypeCtor::Dict)) {
                        return key.as_ref().clone();
                    }
                Type::any()
            }
            _ => Type::any(),
        }
    }

    /// Extract yield type from a type that satisfies AsyncIterable[T]
    ///
    /// Returns the yield type T if the type is async iterable (AsyncGenerator[T, S]), otherwise returns Any.
    pub fn extract_async_iterable_element(ty: &Type) -> Type {
        match ty {
            Type::App(inner, _send_type) => {
                if let Type::App(ctor, yield_type) = inner.as_ref()
                    && matches!(ctor.as_ref(), Type::Con(TypeCtor::AsyncGenerator)) {
                        return yield_type.as_ref().clone();
                    }
                Type::any()
            }
            _ => Type::any(),
        }
    }

    /// Extract result type from a type that satisfies Awaitable[T]
    ///
    /// Returns the result type T if the type is awaitable (Coroutine[Y, S, T] or AsyncGenerator[T, S]), otherwise returns Any.
    pub fn extract_awaitable_result(ty: &Type) -> Type {
        match ty {
            Type::App(inner, result_type) => {
                if let Type::App(inner2, _send_type) = inner.as_ref()
                    && let Type::App(ctor, _yield_type) = inner2.as_ref()
                        && matches!(ctor.as_ref(), Type::Con(TypeCtor::Coroutine)) {
                            return result_type.as_ref().clone();
                        }
                Type::any()
            }
            _ => Type::any(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_list_satisfies_iterable() {
        let list_type = Type::list(Type::int());
        assert!(ProtocolChecker::satisfies(&list_type, &ProtocolName::Iterable));
    }

    #[test]
    fn test_list_satisfies_sequence() {
        let list_type = Type::list(Type::string());
        assert!(ProtocolChecker::satisfies(&list_type, &ProtocolName::Sequence));
    }

    #[test]
    fn test_dict_satisfies_mapping() {
        let dict_type = Type::dict(Type::string(), Type::int());
        assert!(ProtocolChecker::satisfies(&dict_type, &ProtocolName::Mapping));
    }

    #[test]
    fn test_dict_satisfies_iterable() {
        let dict_type = Type::dict(Type::string(), Type::int());
        assert!(ProtocolChecker::satisfies(&dict_type, &ProtocolName::Iterable));
    }

    #[test]
    fn test_string_satisfies_sized() {
        assert!(ProtocolChecker::satisfies(&Type::string(), &ProtocolName::Sized));
    }

    #[test]
    fn test_int_does_not_satisfy_iterable() {
        assert!(!ProtocolChecker::satisfies(&Type::int(), &ProtocolName::Iterable));
    }

    #[test]
    fn test_extract_list_element() {
        let list_type = Type::list(Type::int());
        let element = ProtocolChecker::extract_iterable_element(&list_type);
        assert_eq!(element, Type::int());
    }

    #[test]
    fn test_extract_dict_key() {
        let dict_type = Type::dict(Type::string(), Type::int());
        let key = ProtocolChecker::extract_iterable_element(&dict_type);
        assert_eq!(key, Type::string());
    }

    #[test]
    fn test_extract_dict_value() {
        let dict_type = Type::dict(Type::string(), Type::int());
        let value = ProtocolChecker::extract_mapping_value(&dict_type);
        assert_eq!(value, Type::int());
    }

    #[test]
    fn test_any_satisfies_all_protocols() {
        assert!(ProtocolChecker::satisfies(&Type::any(), &ProtocolName::Iterable));
        assert!(ProtocolChecker::satisfies(&Type::any(), &ProtocolName::Sequence));
        assert!(ProtocolChecker::satisfies(&Type::any(), &ProtocolName::Mapping));
        assert!(ProtocolChecker::satisfies(&Type::any(), &ProtocolName::Sized));
    }

    #[test]
    fn test_protocol_def_iterable() {
        let def = ProtocolDef::get(&ProtocolName::Iterable);
        assert_eq!(def.name, ProtocolName::Iterable);
        assert_eq!(def.required_methods.len(), 1);
        assert_eq!(def.required_methods[0].name, "__iter__");
    }

    #[test]
    fn test_protocol_def_sequence() {
        let def = ProtocolDef::get(&ProtocolName::Sequence);
        assert_eq!(def.name, ProtocolName::Sequence);
        assert_eq!(def.required_methods.len(), 2);
        assert!(def.required_methods.iter().any(|m| m.name == "__getitem__"));
        assert!(def.required_methods.iter().any(|m| m.name == "__len__"));
    }

    #[test]
    fn test_check_method_names() {
        let def = ProtocolDef::get(&ProtocolName::Iterable);
        assert!(def.check_method_names(&["__iter__".to_string()]));
        assert!(!def.check_method_names(&["__len__".to_string()]));
    }

    #[test]
    fn test_check_method_signatures_exact_match() {
        let protocol = ProtocolDef::get(&ProtocolName::Sized);
        let available = vec![(
            "__len__".to_string(),
            MethodSignature { name: "__len__".to_string(), params: vec![], return_type: Type::int() },
        )];

        assert!(protocol.check_method_signatures(&available).is_ok());
    }

    #[test]
    fn test_check_method_signatures_missing_method() {
        let protocol = ProtocolDef::get(&ProtocolName::Sized);
        let available = vec![];

        let result = protocol.check_method_signatures(&available);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("Missing required method"));
    }

    #[test]
    fn test_check_method_signatures_parameter_contravariance() {
        let protocol = ProtocolDef {
            name: ProtocolName::UserDefined("Test".to_string()),
            required_methods: vec![MethodSignature {
                name: "process".to_string(),
                params: vec![Type::int()],
                return_type: Type::string(),
            }],
        };

        let available = vec![(
            "process".to_string(),
            MethodSignature { name: "process".to_string(), params: vec![Type::any()], return_type: Type::string() },
        )];

        assert!(protocol.check_method_signatures(&available).is_ok());
    }

    #[test]
    fn test_check_method_signatures_parameter_contravariance_invalid() {
        let protocol = ProtocolDef {
            name: ProtocolName::UserDefined("Test".to_string()),
            required_methods: vec![MethodSignature {
                name: "process".to_string(),
                params: vec![Type::any()],
                return_type: Type::string(),
            }],
        };

        let available = vec![(
            "process".to_string(),
            MethodSignature { name: "process".to_string(), params: vec![Type::int()], return_type: Type::string() },
        )];

        let result = protocol.check_method_signatures(&available);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("parameter 0 incompatible"));
    }

    #[test]
    fn test_check_method_signatures_return_covariance() {
        let protocol = ProtocolDef {
            name: ProtocolName::UserDefined("Test".to_string()),
            required_methods: vec![MethodSignature {
                name: "get_value".to_string(),
                params: vec![],
                return_type: Type::any(),
            }],
        };

        let available = vec![(
            "get_value".to_string(),
            MethodSignature { name: "get_value".to_string(), params: vec![], return_type: Type::int() },
        )];

        assert!(protocol.check_method_signatures(&available).is_ok());
    }

    #[test]
    fn test_check_method_signatures_return_covariance_invalid() {
        let protocol = ProtocolDef {
            name: ProtocolName::UserDefined("Test".to_string()),
            required_methods: vec![MethodSignature {
                name: "get_value".to_string(),
                params: vec![],
                return_type: Type::int(),
            }],
        };

        let available = vec![(
            "get_value".to_string(),
            MethodSignature { name: "get_value".to_string(), params: vec![], return_type: Type::any() },
        )];

        let result = protocol.check_method_signatures(&available);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("return type incompatible"));
    }

    #[test]
    fn test_check_method_signatures_arity_mismatch() {
        let protocol = ProtocolDef {
            name: ProtocolName::UserDefined("Test".to_string()),
            required_methods: vec![MethodSignature {
                name: "method".to_string(),
                params: vec![Type::int()],
                return_type: Type::string(),
            }],
        };

        let available = vec![(
            "method".to_string(),
            MethodSignature {
                name: "method".to_string(),
                params: vec![Type::int(), Type::int()],
                return_type: Type::string(),
            },
        )];

        let result = protocol.check_method_signatures(&available);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("parameters but protocol requires"));
    }

    #[test]
    fn test_check_method_signatures_multiple_methods() {
        let protocol = ProtocolDef::get(&ProtocolName::Sequence);
        let available = vec![
            (
                "__getitem__".to_string(),
                MethodSignature {
                    name: "__getitem__".to_string(),
                    params: vec![Type::int()],
                    return_type: Type::string(),
                },
            ),
            (
                "__len__".to_string(),
                MethodSignature { name: "__len__".to_string(), params: vec![], return_type: Type::int() },
            ),
        ];

        assert!(protocol.check_method_signatures(&available).is_ok());
    }

    #[test]
    fn test_protocol_satisfies_union_all_variants() {
        let union = Type::union(vec![Type::list(Type::int()), Type::list(Type::string())]);
        assert!(ProtocolChecker::satisfies(&union, &ProtocolName::Iterable));

        let mixed_union = Type::union(vec![Type::list(Type::int()), Type::int()]);
        assert!(!ProtocolChecker::satisfies(&mixed_union, &ProtocolName::Iterable));
    }

    #[test]
    fn test_protocol_satisfies_intersection() {
        let iterable_proto = Type::Con(TypeCtor::Protocol(Some("Iterable".to_string()), vec![]));
        let sized_proto = Type::Con(TypeCtor::Protocol(Some("Sized".to_string()), vec![]));
        let both = Type::intersection(vec![iterable_proto, sized_proto]);

        assert!(ProtocolChecker::satisfies(
            &both,
            &ProtocolName::UserDefined("Iterable".to_string())
        ));

        assert!(ProtocolChecker::satisfies(
            &both,
            &ProtocolName::UserDefined("Sized".to_string())
        ));
    }

    /// Intersection[list[int], Sized] - list satisfies both
    #[test]
    fn test_protocol_satisfies_intersection_with_concrete_types() {
        let list_type = Type::list(Type::int());
        let sized_proto = Type::Con(TypeCtor::Protocol(Some("Sized".to_string()), vec![]));
        let intersection = Type::intersection(vec![list_type, sized_proto]);

        assert!(ProtocolChecker::satisfies(&intersection, &ProtocolName::Iterable));
        assert!(ProtocolChecker::satisfies(&intersection, &ProtocolName::Sized));
    }

    #[test]
    fn test_async_generator_satisfies_async_iterator() {
        let async_gen = Type::async_generator(Type::int(), Type::none());
        assert!(ProtocolChecker::satisfies(&async_gen, &ProtocolName::AsyncIterator));
    }

    #[test]
    fn test_async_generator_satisfies_async_iterable() {
        let async_gen = Type::async_generator(Type::string(), Type::none());
        assert!(ProtocolChecker::satisfies(&async_gen, &ProtocolName::AsyncIterable));
    }

    #[test]
    fn test_regular_types_dont_satisfy_async_protocols() {
        assert!(!ProtocolChecker::satisfies(
            &Type::list(Type::int()),
            &ProtocolName::AsyncIterator
        ));
        assert!(!ProtocolChecker::satisfies(
            &Type::list(Type::int()),
            &ProtocolName::AsyncIterable
        ));
        assert!(!ProtocolChecker::satisfies(&Type::int(), &ProtocolName::AsyncIterator));
        assert!(!ProtocolChecker::satisfies(
            &Type::string(),
            &ProtocolName::AsyncIterable
        ));
    }

    #[test]
    fn test_async_generator_doesnt_satisfy_sync_iterator() {
        let async_gen = Type::async_generator(Type::int(), Type::none());
        assert!(!ProtocolChecker::satisfies(&async_gen, &ProtocolName::Iterable));
    }

    #[test]
    fn test_any_satisfies_async_protocols() {
        assert!(ProtocolChecker::satisfies(&Type::any(), &ProtocolName::AsyncIterator));
        assert!(ProtocolChecker::satisfies(&Type::any(), &ProtocolName::AsyncIterable));
    }

    #[test]
    fn test_extract_async_iterable_element() {
        let async_gen = Type::async_generator(Type::int(), Type::none());
        let element = ProtocolChecker::extract_async_iterable_element(&async_gen);
        assert_eq!(element, Type::int());
    }

    #[test]
    fn test_extract_async_iterable_element_complex_type() {
        let async_gen = Type::async_generator(Type::list(Type::string()), Type::none());
        let element = ProtocolChecker::extract_async_iterable_element(&async_gen);
        assert_eq!(element, Type::list(Type::string()));
    }

    #[test]
    fn test_extract_async_iterable_element_from_non_async() {
        let list_type = Type::list(Type::int());
        let element = ProtocolChecker::extract_async_iterable_element(&list_type);
        assert_eq!(element, Type::any());
    }

    #[test]
    fn test_async_protocol_with_union() {
        let async_gen1 = Type::async_generator(Type::int(), Type::none());
        let async_gen2 = Type::async_generator(Type::string(), Type::none());
        let union = Type::union(vec![async_gen1, async_gen2]);

        assert!(ProtocolChecker::satisfies(&union, &ProtocolName::AsyncIterator));
        assert!(ProtocolChecker::satisfies(&union, &ProtocolName::AsyncIterable));
    }

    #[test]
    fn test_async_protocol_with_mixed_union() {
        let async_gen = Type::async_generator(Type::int(), Type::none());
        let list = Type::list(Type::int());
        let mixed_union = Type::union(vec![async_gen, list]);

        assert!(!ProtocolChecker::satisfies(&mixed_union, &ProtocolName::AsyncIterator));
        assert!(!ProtocolChecker::satisfies(&mixed_union, &ProtocolName::AsyncIterable));
    }

    #[test]
    fn test_async_protocol_with_intersection() {
        let async_iter_proto = Type::Con(TypeCtor::Protocol(Some("AsyncIterator".to_string()), vec![]));
        let sized_proto = Type::Con(TypeCtor::Protocol(Some("Sized".to_string()), vec![]));
        let both = Type::intersection(vec![async_iter_proto, sized_proto]);

        assert!(ProtocolChecker::satisfies(
            &both,
            &ProtocolName::UserDefined("AsyncIterator".to_string())
        ));
    }

    #[test]
    fn test_coroutine_satisfies_awaitable() {
        let coro = Type::coroutine(Type::none(), Type::none(), Type::int());
        assert!(ProtocolChecker::satisfies(&coro, &ProtocolName::Awaitable));
    }

    #[test]
    fn test_async_generator_satisfies_awaitable() {
        let async_gen = Type::async_generator(Type::int(), Type::none());
        assert!(ProtocolChecker::satisfies(&async_gen, &ProtocolName::Awaitable));
    }

    #[test]
    fn test_regular_types_dont_satisfy_awaitable() {
        assert!(!ProtocolChecker::satisfies(&Type::int(), &ProtocolName::Awaitable));
        assert!(!ProtocolChecker::satisfies(
            &Type::list(Type::int()),
            &ProtocolName::Awaitable
        ));
        assert!(!ProtocolChecker::satisfies(&Type::string(), &ProtocolName::Awaitable));
    }

    #[test]
    fn test_any_satisfies_awaitable() {
        assert!(ProtocolChecker::satisfies(&Type::any(), &ProtocolName::Awaitable));
    }

    #[test]
    fn test_extract_awaitable_result_from_coroutine() {
        let coro = Type::coroutine(Type::none(), Type::none(), Type::int());
        let result = ProtocolChecker::extract_awaitable_result(&coro);
        assert_eq!(result, Type::int());
    }

    #[test]
    fn test_extract_awaitable_result_complex_type() {
        let coro = Type::coroutine(Type::string(), Type::none(), Type::list(Type::string()));
        let result = ProtocolChecker::extract_awaitable_result(&coro);
        assert_eq!(result, Type::list(Type::string()));
    }

    #[test]
    fn test_extract_awaitable_result_from_non_awaitable() {
        let list_type = Type::list(Type::int());
        let result = ProtocolChecker::extract_awaitable_result(&list_type);
        assert_eq!(result, Type::any());
    }

    #[test]
    fn test_awaitable_protocol_def() {
        let def = ProtocolDef::get(&ProtocolName::Awaitable);
        assert_eq!(def.name, ProtocolName::Awaitable);
        assert_eq!(def.required_methods.len(), 1);
        assert_eq!(def.required_methods[0].name, "__await__");
    }

    #[test]
    fn test_awaitable_with_union() {
        let coro1 = Type::coroutine(Type::none(), Type::none(), Type::int());
        let coro2 = Type::coroutine(Type::none(), Type::none(), Type::string());
        let union = Type::union(vec![coro1, coro2]);
        assert!(ProtocolChecker::satisfies(&union, &ProtocolName::Awaitable));
    }

    #[test]
    fn test_awaitable_with_mixed_union() {
        let coro = Type::coroutine(Type::none(), Type::none(), Type::int());
        let list = Type::list(Type::int());
        let mixed_union = Type::union(vec![coro, list]);
        assert!(!ProtocolChecker::satisfies(&mixed_union, &ProtocolName::Awaitable));
    }

    #[test]
    fn test_protocol_display_awaitable() {
        let protocol = ProtocolName::Awaitable;
        assert_eq!(protocol.to_string(), "Awaitable");
    }
}

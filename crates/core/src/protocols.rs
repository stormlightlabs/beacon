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
    /// Sized protocol: __len__() -> int
    Sized,
    /// Sequence protocol: __getitem__(int) -> T, __len__() -> int
    Sequence,
    /// Mapping protocol: __getitem__(K) -> V, __len__() -> int
    Mapping,
    /// Context manager protocol: __enter__() -> T, __exit__(...) -> bool
    ContextManager,
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
            ProtocolName::Sized => write!(f, "Sized"),
            ProtocolName::Sequence => write!(f, "Sequence"),
            ProtocolName::Mapping => write!(f, "Mapping"),
            ProtocolName::ContextManager => write!(f, "ContextManager"),
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

    /// Check if required methods are present (simplified check)
    ///
    /// This is a basic implementation that checks for method names only.
    /// For full signature checking with variance, use `check_method_signatures()`.
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
    /// This is a simplified implementation that handles builtin types.
    ///
    /// TODO: Extend to handle user-defined types via record/class inspection
    /// TODO: Extract element types from generic protocols
    pub fn satisfies(ty: &Type, protocol: &ProtocolName) -> bool {
        match (ty, protocol) {
            (Type::App(ctor, _), ProtocolName::Iterable) => {
                matches!(
                    ctor.as_ref(),
                    Type::Con(TypeCtor::List) | Type::Con(TypeCtor::Set) | Type::Con(TypeCtor::Tuple)
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
            (Type::Con(TypeCtor::String), ProtocolName::Sized) => true,
            (Type::Con(TypeCtor::Any), _) => true,
            _ => false,
        }
    }

    /// Extract element type from a type that satisfies Iterable[T]
    ///
    /// Returns the element type T if the type is iterable, otherwise returns Any.
    pub fn extract_iterable_element(ty: &Type) -> Type {
        match ty {
            Type::App(ctor, elem) if matches!(ctor.as_ref(), Type::Con(TypeCtor::List)) => elem.as_ref().clone(),
            Type::App(ctor, elem) if matches!(ctor.as_ref(), Type::Con(TypeCtor::Set)) => elem.as_ref().clone(),
            Type::App(ctor, elem) if matches!(ctor.as_ref(), Type::Con(TypeCtor::Tuple)) => elem.as_ref().clone(),
            Type::App(inner, _) => {
                if let Type::App(ctor, key) = inner.as_ref() {
                    if matches!(ctor.as_ref(), Type::Con(TypeCtor::Dict)) {
                        return key.as_ref().clone();
                    }
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
                if let Type::App(ctor, _) = inner.as_ref() {
                    if matches!(ctor.as_ref(), Type::Con(TypeCtor::Dict)) {
                        return val.as_ref().clone();
                    }
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
                if let Type::App(ctor, key) = inner.as_ref() {
                    if matches!(ctor.as_ref(), Type::Con(TypeCtor::Dict)) {
                        return key.as_ref().clone();
                    }
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
}

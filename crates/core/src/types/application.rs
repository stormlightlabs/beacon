//! Type application decomposition and protocol application helpers.

use super::{Type, TypeCtor, Variance};

impl Type {
    pub fn extract_generator_params(&self) -> Option<(&Type, &Type, &Type)> {
        match self {
            Type::App(app2, r) => match app2.as_ref() {
                Type::App(app1, s) => match app1.as_ref() {
                    Type::App(ctor, y) => match ctor.as_ref() {
                        Type::Con(TypeCtor::Generator) => Some((y, s, r)),
                        _ => None,
                    },
                    _ => None,
                },
                _ => None,
            },
            _ => None,
        }
    }

    /// Extract parameters from AsyncGenerator[Y, S] type
    pub fn extract_async_generator_params(&self) -> Option<(&Type, &Type)> {
        match self {
            Type::App(app1, s) => match app1.as_ref() {
                Type::App(ctor, y) => match ctor.as_ref() {
                    Type::Con(TypeCtor::AsyncGenerator) => Some((y, s)),
                    _ => None,
                },
                _ => None,
            },
            _ => None,
        }
    }

    /// Extract parameters from Coroutine[Y, S, R] type
    pub fn extract_coroutine_params(&self) -> Option<(&Type, &Type, &Type)> {
        match self {
            Type::App(app2, r) => match app2.as_ref() {
                Type::App(app1, s) => match app1.as_ref() {
                    Type::App(ctor, y) => match ctor.as_ref() {
                        Type::Con(TypeCtor::Coroutine) => Some((y, s, r)),
                        _ => None,
                    },
                    _ => None,
                },
                _ => None,
            },
            _ => None,
        }
    }

    /// Extract element type from Iterator[T] type
    pub fn extract_iterator_elem(&self) -> Option<&Type> {
        match self {
            Type::App(ctor, elem) => match ctor.as_ref() {
                Type::Con(TypeCtor::Iterator) => Some(elem),
                _ => None,
            },
            _ => None,
        }
    }

    /// Create a function type with named parameters
    pub fn fun(args: Vec<(String, Type)>, ret: Type) -> Self {
        Type::Fun(args, Box::new(ret))
    }

    /// Create a function type with unnamed parameters (generates default names _0, _1, etc.)
    ///
    /// This is a convenience method for creating function types when parameter
    /// names are not available, such as in tests or when parsing Callable types.
    pub fn fun_unnamed(arg_types: Vec<Type>, ret: Type) -> Self {
        let args = arg_types
            .into_iter()
            .enumerate()
            .map(|(i, ty)| (format!("_{i}"), ty))
            .collect();
        Type::Fun(args, Box::new(ret))
    }

    /// Create a union type, flattening nested unions, removing duplications, and sorting for canonical form
    ///
    /// This method also performs subtype elimination: if type A is a subtype of type B, then Union[A, B] simplifies to B.
    /// For example:
    /// - Union[Never, int] → int (Never is subtype of everything)
    /// - Union[int, Any] → Any (int is subtype of Any)
    pub fn unapply(&self) -> Option<(&TypeCtor, Vec<Type>)> {
        match self {
            Type::App(constructor, arg) => {
                if let Type::Con(ctor) = constructor.as_ref() {
                    Some((ctor, vec![arg.as_ref().clone()]))
                } else if let Type::App(inner_constructor, first_arg) = constructor.as_ref() {
                    if let Type::Con(ctor) = inner_constructor.as_ref() {
                        Some((ctor, vec![first_arg.as_ref().clone(), arg.as_ref().clone()]))
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Extract the class name and type arguments from a parameterized class type
    ///
    /// For generic user-defined classes like `MyGeneric[int, str]`, this extracts the class name and its type arguments.
    pub fn unapply_class(&self) -> Option<(&str, Vec<Type>)> {
        match self {
            Type::App(constructor, arg) => match constructor.as_ref() {
                Type::Con(TypeCtor::Class(name)) => Some((name.as_str(), vec![arg.as_ref().clone()])),
                Type::App(inner_constructor, first_arg) => match inner_constructor.as_ref() {
                    Type::Con(TypeCtor::Class(name)) => {
                        Some((name.as_str(), vec![first_arg.as_ref().clone(), arg.as_ref().clone()]))
                    }
                    _ => None,
                },
                _ => None,
            },
            _ => None,
        }
    }

    /// Extract the protocol name and type arguments from a parameterized protocol type
    pub fn unapply_protocol(&self) -> Option<(&str, Vec<Type>)> {
        match self {
            Type::App(constructor, arg) => match constructor.as_ref() {
                Type::Con(TypeCtor::Protocol(Some(name), _)) => Some((name.as_str(), vec![arg.as_ref().clone()])),
                Type::App(inner_constructor, first_arg) => match inner_constructor.as_ref() {
                    Type::Con(TypeCtor::Protocol(Some(name), _)) => {
                        Some((name.as_str(), vec![first_arg.as_ref().clone(), arg.as_ref().clone()]))
                    }
                    _ => None,
                },
                _ => None,
            },
            _ => None,
        }
    }

    /// Enrich a Protocol type with variance information from ClassMetadata
    ///
    /// This method recursively traverses a type and replaces Protocol TypeCtors that have empty
    /// variance vectors with ones populated from the ClassMetadata.
    pub fn enrich_protocol_variance(self, class_registry: &crate::class_metadata::ClassRegistry) -> Self {
        match self {
            Type::Con(TypeCtor::Protocol(Some(name), variances)) if variances.is_empty() => {
                match class_registry.get_class(&name) {
                    Some(metadata) => {
                        let enriched_variances: Vec<Variance> =
                            metadata.type_param_vars.iter().map(|tv| tv.variance).collect();
                        Type::Con(TypeCtor::Protocol(Some(name), enriched_variances))
                    }
                    None => Type::Con(TypeCtor::Protocol(Some(name), variances)),
                }
            }
            Type::App(ctor, arg) => Type::App(
                Box::new(ctor.enrich_protocol_variance(class_registry)),
                Box::new(arg.enrich_protocol_variance(class_registry)),
            ),
            Type::Fun(params, ret) => Type::Fun(
                params
                    .into_iter()
                    .map(|(name, ty)| (name, ty.enrich_protocol_variance(class_registry)))
                    .collect(),
                Box::new(ret.enrich_protocol_variance(class_registry)),
            ),
            Type::ForAll(tvs, body) => Type::ForAll(tvs, Box::new(body.enrich_protocol_variance(class_registry))),
            Type::Union(types) => Type::Union(
                types
                    .into_iter()
                    .map(|ty| ty.enrich_protocol_variance(class_registry))
                    .collect(),
            ),
            Type::Intersection(types) => Type::Intersection(
                types
                    .into_iter()
                    .map(|ty| ty.enrich_protocol_variance(class_registry))
                    .collect(),
            ),
            Type::Record(fields, row_var) => Type::Record(
                fields
                    .into_iter()
                    .map(|(name, ty)| (name, ty.enrich_protocol_variance(class_registry)))
                    .collect(),
                row_var,
            ),
            Type::BoundMethod(receiver, method_name, method) => Type::BoundMethod(
                Box::new(receiver.enrich_protocol_variance(class_registry)),
                method_name,
                Box::new(method.enrich_protocol_variance(class_registry)),
            ),
            Type::Tuple(types) => Type::Tuple(
                types
                    .into_iter()
                    .map(|ty| ty.enrich_protocol_variance(class_registry))
                    .collect(),
            ),
            other => other,
        }
    }

    /// Get the variance for a specific parameter position in a type application chain.
    ///
    /// Multi-parameter types like `Dict[K, V]` are represented as nested applications:
    /// `App(App(Dict, K), V)` where K is at parameter 0 and V is at parameter 1.
    ///
    /// This function determines which parameter position we're at based on the nesting depth.
    pub(crate) fn get_app_variance(ty: &Type) -> Variance {
        let mut current = ty;
        let mut param_count: usize = 0;

        while let Type::App(ctor, _) = current {
            param_count += 1;
            current = ctor.as_ref();
        }

        if let Type::Con(tc) = current {
            let param_index = param_count.saturating_sub(1);
            tc.variance(param_index)
        } else {
            Variance::Invariant
        }
    }
}

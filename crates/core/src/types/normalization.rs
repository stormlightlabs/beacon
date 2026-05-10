//! Type normalization helpers.

use super::{Type, TypeCtor};

impl Type {
    pub fn union(mut types: Vec<Type>) -> Self {
        if types.len() == 1 {
            return types.pop().unwrap();
        }

        let mut flattened = Vec::new();
        for t in types {
            match t {
                Type::Union(inner) => flattened.extend(inner),
                other => flattened.push(other),
            }
        }

        flattened.sort();
        flattened.dedup();

        let mut simplified = Vec::new();
        for i in 0..flattened.len() {
            let mut is_redundant = false;
            for j in 0..flattened.len() {
                if i != j && flattened[i].is_subtype_of(&flattened[j]) {
                    is_redundant = true;
                    break;
                }
            }
            if !is_redundant {
                simplified.push(flattened[i].clone());
            }
        }

        if simplified.is_empty() {
            Type::Con(TypeCtor::Never)
        } else if simplified.len() == 1 {
            simplified.pop().unwrap()
        } else {
            Type::Union(simplified)
        }
    }

    /// Create an intersection type, flattening nested intersections, removing duplications, and sorting for canonical form
    pub fn intersection(mut types: Vec<Type>) -> Self {
        if types.len() == 1 {
            return types.pop().unwrap();
        }

        let mut flattened = Vec::new();
        for t in types {
            match t {
                Type::Intersection(inner) => flattened.extend(inner),
                other => flattened.push(other),
            }
        }

        flattened.sort();
        flattened.dedup();

        if flattened.len() == 1 { flattened.pop().unwrap() } else { Type::Intersection(flattened) }
    }

    /// Create Optional[T] which is Union[T, None]
    pub fn optional(t: Type) -> Self {
        Type::union(vec![t, Type::none()])
    }

    /// Check if this type is Optional[T] (i.e., Union[T, None])
    ///
    /// Returns true if the type is a union containing exactly None and one other type.
    pub fn is_optional(&self) -> bool {
        match self {
            Type::Union(types) => types.len() == 2 && types.iter().any(|t| matches!(t, Type::Con(TypeCtor::NoneType))),
            _ => false,
        }
    }

    /// Extract T from Optional[T] (Union[T, None])
    ///
    /// Returns Some(T) if this is Optional[T], None otherwise.
    pub fn unwrap_optional(&self) -> Option<Type> {
        match self {
            Type::Union(types) if types.len() == 2 => {
                let none_idx = types.iter().position(|t| matches!(t, Type::Con(TypeCtor::NoneType)))?;
                let other_idx = 1 - none_idx;
                Some(types[other_idx].clone())
            }
            _ => None,
        }
    }

    /// Remove a type from a union
    pub fn remove_from_union(&self, to_remove: &Type) -> Type {
        match self {
            Type::Union(types) => {
                let filtered: Vec<Type> = types.iter().filter(|t| *t != to_remove).cloned().collect();
                if filtered.is_empty() {
                    Type::never()
                } else if filtered.len() == 1 {
                    filtered.into_iter().next().unwrap()
                } else {
                    Type::Union(filtered)
                }
            }
            other => {
                if other == to_remove {
                    Type::never()
                } else {
                    other.clone()
                }
            }
        }
    }
}

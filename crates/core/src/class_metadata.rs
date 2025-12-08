use crate::types::{OverloadSet, Type, TypeCtor, TypeVar};

use indexmap::IndexMap;
use std::collections::{HashMap, HashSet};

/// Represents either a single method signature or an overloaded method with multiple signatures
#[derive(Debug, Clone, PartialEq)]
pub enum MethodType {
    /// A single method signature
    Single(Type),
    /// Multiple overloaded signatures
    Overloaded(OverloadSet),
}

impl MethodType {
    /// Get the primary type for this method (single signature or implementation signature for overloads)
    ///
    /// For overloaded methods, returns the implementation if available, otherwise the first overload signature.
    /// Some stub files (e.g., typeshed) use @overload on all signatures without a separate implementation.
    pub fn primary_type(&self) -> Option<&Type> {
        match self {
            MethodType::Single(ty) => Some(ty),
            MethodType::Overloaded(overload_set) => overload_set
                .implementation
                .as_ref()
                .or_else(|| overload_set.signatures.first()),
        }
    }

    /// Resolve to a specific signature based on argument types
    pub fn resolve_for_args(&self, arg_types: &[Type]) -> Option<&Type> {
        match self {
            MethodType::Single(ty) => Some(ty),
            MethodType::Overloaded(overload_set) => {
                overload_set.resolve(arg_types).or(overload_set.implementation.as_ref())
            }
        }
    }

    /// Get all signatures (single signature or all overloads)
    pub fn all_signatures(&self) -> Vec<&Type> {
        match self {
            MethodType::Single(ty) => vec![ty],
            MethodType::Overloaded(overload_set) => {
                let mut sigs: Vec<&Type> = overload_set.signatures.iter().collect();
                if let Some(impl_sig) = &overload_set.implementation {
                    sigs.push(impl_sig);
                }
                sigs
            }
        }
    }
}

/// Metadata about a class definition including fields, methods, and __init__ signature
#[derive(Debug, Clone)]
pub struct ClassMetadata {
    /// The name of the class
    pub name: String,

    /// Field types discovered from __init__ and class body, mapping field name to its type
    /// Uses IndexMap to preserve field definition order for dataclass constructors
    pub fields: IndexMap<String, Type>,

    /// Maps method name to its method type (single signature or overload set)
    pub methods: HashMap<String, MethodType>,

    /// The type signature of __init__ if present
    /// Parameters include 'self' as first parameter
    pub init_type: Option<Type>,

    /// Whether this class is a Protocol (from typing.Protocol)
    pub is_protocol: bool,

    /// Properties defined with @property decorator (method name → return type)
    pub properties: HashMap<String, Type>,

    /// Methods decorated with @classmethod
    pub classmethods: HashMap<String, Type>,

    /// Methods decorated with @staticmethod
    pub staticmethods: HashMap<String, Type>,

    /// Custom metaclass if specified (e.g., class Foo(metaclass=Meta))
    pub metaclass: Option<String>,

    /// The type signature of __new__ if present
    pub new_type: Option<Type>,

    /// Base classes for inheritance
    pub base_classes: Vec<String>,

    /// Type parameters for generic classes (e.g., ["_T"] for list, ["_KT", "_VT"] for dict)
    pub type_params: Vec<String>,
    /// Concrete type variables corresponding to the parameters when available
    pub type_param_vars: Vec<TypeVar>,
}

impl ClassMetadata {
    /// Create new class metadata with the given name
    pub fn new(name: String) -> Self {
        Self {
            name,
            fields: IndexMap::new(),
            methods: HashMap::new(),
            init_type: None,
            is_protocol: false,
            properties: HashMap::new(),
            classmethods: HashMap::new(),
            staticmethods: HashMap::new(),
            metaclass: None,
            new_type: None,
            base_classes: Vec::new(),
            type_params: Vec::new(),
            type_param_vars: Vec::new(),
        }
    }

    /// Add a field with its type
    pub fn add_field(&mut self, field_name: String, field_type: Type) {
        self.fields.insert(field_name, field_type);
    }

    /// Add a method with its type
    pub fn add_method(&mut self, method_name: String, method_type: Type) {
        self.methods.insert(method_name, MethodType::Single(method_type));
    }

    /// Add an overloaded method with multiple signatures
    pub fn add_overloaded_method(&mut self, method_name: String, overload_set: OverloadSet) {
        self.methods.insert(method_name, MethodType::Overloaded(overload_set));
    }

    /// Add a signature to an existing method, converting to overload set if needed
    pub fn add_method_signature(&mut self, method_name: String, signature: Type) {
        match self.methods.get_mut(&method_name) {
            Some(MethodType::Single(existing)) => {
                let overload_set = OverloadSet { signatures: vec![existing.clone(), signature], implementation: None };
                self.methods.insert(method_name, MethodType::Overloaded(overload_set));
            }
            Some(MethodType::Overloaded(overload_set)) => overload_set.signatures.push(signature),
            None => {
                self.methods.insert(method_name, MethodType::Single(signature));
            }
        }
    }

    /// Set the __init__ method type
    pub fn set_init_type(&mut self, init_type: Type) {
        self.init_type = Some(init_type);
    }

    /// Look up a field or method by name
    /// Priority: properties > fields > classmethods > staticmethods > methods
    /// For overloaded methods, returns the implementation signature if available
    pub fn lookup_attribute(&self, attr_name: &str) -> Option<&Type> {
        self.properties
            .get(attr_name)
            .or_else(|| self.fields.get(attr_name))
            .or_else(|| self.classmethods.get(attr_name))
            .or_else(|| self.staticmethods.get(attr_name))
            .or_else(|| {
                self.methods
                    .get(attr_name)
                    .and_then(|method_type| method_type.primary_type())
            })
    }

    /// Look up a method by name, returning the MethodType (single or overloaded)
    pub fn lookup_method_type(&self, method_name: &str) -> Option<&MethodType> {
        self.methods.get(method_name)
    }

    /// Look up a method and resolve for specific argument types
    pub fn lookup_method_for_args(&self, method_name: &str, arg_types: &[Type]) -> Option<&Type> {
        self.methods
            .get(method_name)
            .and_then(|method_type| method_type.resolve_for_args(arg_types))
    }

    /// Check if an attribute is a method
    pub fn is_method(&self, attr_name: &str) -> bool {
        self.methods.contains_key(attr_name)
            || self.classmethods.contains_key(attr_name)
            || self.staticmethods.contains_key(attr_name)
    }

    /// Look up a method by name, returning the primary type
    /// For overloaded methods, returns the implementation signature if available
    pub fn lookup_method(&self, method_name: &str) -> Option<&Type> {
        self.classmethods
            .get(method_name)
            .or_else(|| self.staticmethods.get(method_name))
            .or_else(|| {
                self.methods
                    .get(method_name)
                    .and_then(|method_type| method_type.primary_type())
            })
    }

    /// Mark this class as a Protocol
    pub fn set_protocol(&mut self, is_protocol: bool) {
        self.is_protocol = is_protocol;
    }

    /// Add a property (from @property decorator)
    pub fn add_property(&mut self, name: String, return_type: Type) {
        self.properties.insert(name, return_type);
    }

    /// Add a classmethod (from @classmethod decorator)
    pub fn add_classmethod(&mut self, name: String, method_type: Type) {
        self.classmethods.insert(name, method_type);
    }

    /// Add a staticmethod (from @staticmethod decorator)
    pub fn add_staticmethod(&mut self, name: String, method_type: Type) {
        self.staticmethods.insert(name, method_type);
    }

    /// Set the custom metaclass
    pub fn set_metaclass(&mut self, metaclass: String) {
        self.metaclass = Some(metaclass);
    }

    /// Set the __new__ method type
    pub fn set_new_type(&mut self, new_type: Type) {
        self.new_type = Some(new_type);
    }

    /// Add a base class
    pub fn add_base_class(&mut self, base: String) {
        self.base_classes.push(base);
    }

    /// Add a type parameter for generic classes
    pub fn add_type_param(&mut self, param: String) {
        self.type_params.push(param);
    }

    /// Set all type parameters at once
    pub fn set_type_params(&mut self, params: Vec<String>) {
        self.type_params = params;
    }

    /// Set the concrete type variables corresponding to generic parameters
    pub fn set_type_param_vars(&mut self, params: Vec<TypeVar>) {
        self.type_param_vars = params;
    }

    /// Given type arguments that match this class's type parameters, creates a mapping to substitute type variables in method signatures.
    ///
    /// For example, for `list[int]`:
    /// - type_params: ["_T"]
    /// - type_args: [int]
    /// - Returns a substitution that maps "_T" → int
    pub fn create_type_substitution(&self, type_args: &[Type]) -> HashMap<String, Type> {
        let mut subst = HashMap::new();
        for (param, arg) in self.type_params.iter().zip(type_args.iter()) {
            subst.insert(param.clone(), arg.clone());
        }
        subst
    }

    /// Apply type parameter substitution to a type by recursively replacing all
    /// [TypeCtor::TypeVariable] references with their instantiated types from the substitution map.
    pub fn substitute_type_params(ty: &Type, subst: &HashMap<String, Type>) -> Type {
        match ty {
            Type::Con(TypeCtor::TypeVariable(name)) => subst.get(name).cloned().unwrap_or_else(|| ty.clone()),
            Type::Var(tv) => {
                if let Some(hint) = &tv.hint {
                    subst.get(hint).cloned().unwrap_or_else(|| ty.clone())
                } else {
                    ty.clone()
                }
            }
            Type::Con(_) => ty.clone(),
            Type::App(t1, t2) => Type::App(
                Box::new(Self::substitute_type_params(t1, subst)),
                Box::new(Self::substitute_type_params(t2, subst)),
            ),
            Type::Fun(args, ret) => Type::Fun(
                args.iter()
                    .map(|(name, ty)| (name.clone(), Self::substitute_type_params(ty, subst)))
                    .collect(),
                Box::new(Self::substitute_type_params(ret, subst)),
            ),
            Type::ForAll(tvs, t) => Type::ForAll(tvs.clone(), Box::new(Self::substitute_type_params(t, subst))),
            Type::Union(types) => Type::Union(types.iter().map(|t| Self::substitute_type_params(t, subst)).collect()),
            Type::Intersection(types) => {
                Type::Intersection(types.iter().map(|t| Self::substitute_type_params(t, subst)).collect())
            }
            Type::Record(fields, row_var) => Type::Record(
                fields
                    .iter()
                    .map(|(name, ty)| (name.clone(), Self::substitute_type_params(ty, subst)))
                    .collect(),
                row_var.clone(),
            ),
            Type::BoundMethod(receiver, method_name, method) => Type::BoundMethod(
                Box::new(Self::substitute_type_params(receiver, subst)),
                method_name.clone(),
                Box::new(Self::substitute_type_params(method, subst)),
            ),
            Type::Tuple(types) => Type::Tuple(types.iter().map(|t| Self::substitute_type_params(t, subst)).collect()),
        }
    }

    /// Get all required method signatures for protocol checking
    /// Returns (method_name, method_type) pairs for all methods in this protocol
    /// For overloaded methods, returns the primary type (implementation signature)
    pub fn get_protocol_methods(&self) -> Vec<(&str, &Type)> {
        self.methods
            .iter()
            .filter_map(|(name, method_type)| method_type.primary_type().map(|ty| (name.as_str(), ty)))
            .collect()
    }

    /// Get all method signatures including all overload variants
    /// Returns (method_name, method_type) pairs
    pub fn get_all_method_signatures(&self) -> Vec<(&str, &MethodType)> {
        self.methods
            .iter()
            .map(|(name, method_type)| (name.as_str(), method_type))
            .collect()
    }

    /// Build a structural record type from this class's fields and methods
    ///
    /// Creates a record type like: { field1: T1, field2: T2, method1: (Self, Args) -> R, ... }
    /// This enables structural subtyping via row polymorphism.
    /// For overloaded methods, uses the implementation signature if available, otherwise the first overload.
    pub fn to_record_type(&self) -> Type {
        let mut fields_vec = Vec::new();

        for (name, ty) in &self.properties {
            fields_vec.push((name.clone(), ty.clone()));
        }

        for (name, ty) in &self.fields {
            fields_vec.push((name.clone(), ty.clone()));
        }

        for (name, method_type) in &self.methods {
            if let Some(ty) = method_type.primary_type() {
                fields_vec.push((name.clone(), ty.clone()));
            } else if let MethodType::Overloaded(overload_set) = method_type
                && let Some(first_sig) = overload_set.signatures.first() {
                    fields_vec.push((name.clone(), first_sig.clone()));
                }
        }
        for (name, ty) in &self.classmethods {
            fields_vec.push((name.clone(), ty.clone()));
        }
        for (name, ty) in &self.staticmethods {
            fields_vec.push((name.clone(), ty.clone()));
        }

        fields_vec.sort_by(|a, b| a.0.cmp(&b.0));

        Type::Record(fields_vec, None)
    }
}

/// Registry of all class definitions in the analyzed module
#[derive(Debug, Default, Clone)]
pub struct ClassRegistry {
    /// Maps class name to its metadata
    classes: HashMap<String, ClassMetadata>,
}

impl ClassRegistry {
    /// Create a new empty class registry
    pub fn new() -> Self {
        Self { classes: HashMap::new() }
    }

    /// Register a new class or update existing metadata
    pub fn register_class(&mut self, name: String, metadata: ClassMetadata) {
        self.classes.insert(name, metadata);
    }

    /// Look up class metadata by name
    pub fn get_class(&self, name: &str) -> Option<&ClassMetadata> {
        self.classes.get(name)
    }

    /// Merge another registry into this one, copying all classes
    pub fn merge(&mut self, other: &ClassRegistry) {
        for (name, metadata) in &other.classes {
            self.classes.insert(name.clone(), metadata.clone());
        }
    }

    /// Look up an attribute in a class
    pub fn lookup_attribute(&self, class_name: &str, attr_name: &str) -> Option<&Type> {
        self.get_class(class_name)
            .and_then(|metadata| metadata.lookup_attribute(attr_name))
    }

    /// Check if an attribute is a method in a class
    pub fn is_method(&self, class_name: &str, attr_name: &str) -> bool {
        self.get_class(class_name)
            .map(|metadata| metadata.is_method(attr_name))
            .unwrap_or(false)
    }

    /// Look up a method in a class
    pub fn lookup_method(&self, class_name: &str, method_name: &str) -> Option<&Type> {
        self.get_class(class_name)
            .and_then(|metadata| metadata.lookup_method(method_name))
    }

    /// Build a structural record type for a class including inherited members
    ///
    /// Uses row polymorphism to represent inheritance:
    /// - Base class: { x: int, foo: () -> str }
    /// - Derived class: { x: int, foo: () -> str, y: float, bar: () -> bool }
    ///
    /// The derived class record extends the base class record with additional fields.
    pub fn class_to_record(&self, class_name: &str) -> Option<Type> {
        let metadata = self.get_class(class_name)?;
        let mut all_fields = HashMap::new();

        for base_name in &metadata.base_classes {
            if let Some(Type::Record(base_fields, _)) = self.class_to_record(base_name) {
                for (name, ty) in base_fields {
                    all_fields.insert(name, ty);
                }
            }
        }

        if let Type::Record(own_fields, _) = metadata.to_record_type() {
            for (name, ty) in own_fields {
                all_fields.insert(name, ty);
            }
        }

        let mut fields_vec: Vec<(String, Type)> = all_fields.into_iter().collect();
        fields_vec.sort_by(|a, b| a.0.cmp(&b.0));

        Some(Type::Record(fields_vec, None))
    }

    /// Look up an attribute in a class, checking base classes if needed
    pub fn lookup_attribute_with_inheritance(&self, class_name: &str, attr_name: &str) -> Option<Type> {
        if let Some(ty) = self.lookup_attribute(class_name, attr_name) {
            return Some(ty.clone());
        }

        if let Some(metadata) = self.get_class(class_name) {
            for base_name in &metadata.base_classes {
                if let Some(ty) = self.lookup_attribute_with_inheritance(base_name, attr_name) {
                    return Some(ty);
                }
            }
        }

        None
    }

    /// Return true if `derived` matches or inherits from `base`.
    pub fn is_subclass_of(&self, derived: &str, base: &str) -> bool {
        if normalize_base_name(derived) == normalize_base_name(base) {
            return true;
        }

        let mut stack: Vec<String> = vec![normalize_base_name(derived).to_string()];
        let mut visited = HashSet::new();

        while let Some(current) = stack.pop() {
            if !visited.insert(current.clone()) {
                continue;
            }

            if let Some(metadata) = self.get_class(&current) {
                for base_name in &metadata.base_classes {
                    let normalized = normalize_base_name(base_name);
                    if normalized == normalize_base_name(base) {
                        return true;
                    }
                    stack.push(normalized.to_string());
                }
            }
        }

        false
    }
}

fn normalize_base_name(name: &str) -> &str {
    name.split(['[', '(']).next().unwrap_or(name).trim()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::{Type, TypeCtor};

    #[test]
    fn test_class_metadata_creation() {
        let meta = ClassMetadata::new("Person".to_string());
        assert_eq!(meta.name, "Person");
        assert!(meta.fields.is_empty());
        assert!(meta.methods.is_empty());
        assert!(meta.init_type.is_none());
    }

    #[test]
    fn test_add_field() {
        let mut meta = ClassMetadata::new("Person".to_string());
        let name_type = Type::Con(TypeCtor::String);
        meta.add_field("name".to_string(), name_type.clone());

        assert_eq!(meta.fields.len(), 1);
        assert_eq!(meta.fields.get("name"), Some(&name_type));
    }

    #[test]
    fn test_lookup_attribute() {
        let mut meta = ClassMetadata::new("Person".to_string());
        let name_type = Type::Con(TypeCtor::String);
        let age_type = Type::Con(TypeCtor::Int);

        meta.add_field("name".to_string(), name_type.clone());
        meta.add_field("age".to_string(), age_type.clone());

        assert_eq!(meta.lookup_attribute("name"), Some(&name_type));
        assert_eq!(meta.lookup_attribute("age"), Some(&age_type));
        assert_eq!(meta.lookup_attribute("unknown"), None);
    }

    #[test]
    fn test_class_registry() {
        let mut registry = ClassRegistry::new();
        let mut meta = ClassMetadata::new("Person".to_string());
        let name_type = Type::Con(TypeCtor::String);
        meta.add_field("name".to_string(), name_type.clone());

        registry.register_class("Person".to_string(), meta);

        assert!(registry.get_class("Person").is_some());
        assert!(registry.get_class("Unknown").is_none());
        assert_eq!(registry.lookup_attribute("Person", "name"), Some(&name_type));
    }

    #[test]
    fn test_method_type_single() {
        let method_type = MethodType::Single(Type::Fun(
            vec![(String::new(), Type::Con(TypeCtor::Int))],
            Box::new(Type::Con(TypeCtor::String)),
        ));

        assert!(method_type.primary_type().is_some());
        assert_eq!(method_type.all_signatures().len(), 1);
    }

    #[test]
    fn test_method_type_overloaded() {
        let overload_set = OverloadSet {
            signatures: vec![
                Type::Fun(
                    vec![(String::new(), Type::Con(TypeCtor::Int))],
                    Box::new(Type::Con(TypeCtor::String)),
                ),
                Type::Fun(
                    vec![(String::new(), Type::Con(TypeCtor::String))],
                    Box::new(Type::Con(TypeCtor::Int)),
                ),
            ],
            implementation: Some(Type::Fun(
                vec![(String::new(), Type::Con(TypeCtor::Top))],
                Box::new(Type::Con(TypeCtor::Top)),
            )),
        };

        let method_type = MethodType::Overloaded(overload_set);
        assert!(method_type.primary_type().is_some());
        assert_eq!(method_type.all_signatures().len(), 3);
    }

    #[test]
    fn test_add_method_signature_creates_overload() {
        let mut meta = ClassMetadata::new("Example".to_string());

        let sig1 = Type::Fun(
            vec![(String::new(), Type::Con(TypeCtor::Int))],
            Box::new(Type::Con(TypeCtor::String)),
        );
        let sig2 = Type::Fun(
            vec![(String::new(), Type::Con(TypeCtor::String))],
            Box::new(Type::Con(TypeCtor::Int)),
        );

        meta.add_method_signature("convert".to_string(), sig1);
        meta.add_method_signature("convert".to_string(), sig2);

        let method_type = meta.lookup_method_type("convert");
        assert!(matches!(method_type, Some(MethodType::Overloaded(_))));

        if let Some(MethodType::Overloaded(overload_set)) = method_type {
            assert_eq!(overload_set.signatures.len(), 2);
        }
    }

    #[test]
    fn test_lookup_method_for_args() {
        let mut meta = ClassMetadata::new("Example".to_string());

        let overload_set = OverloadSet {
            signatures: vec![
                Type::Fun(
                    vec![(String::new(), Type::Con(TypeCtor::Int))],
                    Box::new(Type::Con(TypeCtor::String)),
                ),
                Type::Fun(
                    vec![(String::new(), Type::Con(TypeCtor::String))],
                    Box::new(Type::Con(TypeCtor::Int)),
                ),
            ],
            implementation: None,
        };

        meta.add_overloaded_method("convert".to_string(), overload_set);

        let result = meta.lookup_method_for_args("convert", &[Type::Con(TypeCtor::Int)]);
        assert!(result.is_some(), "Should resolve overload for int argument");

        if let Some(Type::Fun(params, ret)) = result {
            assert_eq!(
                params.len(),
                1,
                "Should have int parameter (self excluded from signature)"
            );
            assert!(matches!(ret.as_ref(), Type::Con(TypeCtor::String)));
        }
    }

    #[test]
    fn test_get_all_method_signatures() {
        let mut meta = ClassMetadata::new("Example".to_string());

        let single_method = Type::Fun(
            vec![(String::new(), Type::Con(TypeCtor::Int))],
            Box::new(Type::Con(TypeCtor::Bool)),
        );
        meta.add_method("check".to_string(), single_method);

        let overload_set = OverloadSet {
            signatures: vec![
                Type::Fun(
                    vec![(String::new(), Type::Con(TypeCtor::Int))],
                    Box::new(Type::Con(TypeCtor::String)),
                ),
                Type::Fun(
                    vec![(String::new(), Type::Con(TypeCtor::String))],
                    Box::new(Type::Con(TypeCtor::Int)),
                ),
            ],
            implementation: None,
        };
        meta.add_overloaded_method("convert".to_string(), overload_set);

        let all_sigs = meta.get_all_method_signatures();
        assert_eq!(all_sigs.len(), 2);

        let check_type = all_sigs.iter().find(|(name, _)| *name == "check");
        assert!(matches!(check_type, Some((_, MethodType::Single(_)))));

        let convert_type = all_sigs.iter().find(|(name, _)| *name == "convert");
        assert!(matches!(convert_type, Some((_, MethodType::Overloaded(_)))));
    }

    #[test]
    fn test_to_record_type_with_overloads() {
        let mut meta = ClassMetadata::new("Example".to_string());

        meta.add_field("value".to_string(), Type::Con(TypeCtor::Int));

        let overload_set = OverloadSet {
            signatures: vec![Type::Fun(
                vec![(String::new(), Type::Con(TypeCtor::Int))],
                Box::new(Type::Con(TypeCtor::String)),
            )],
            implementation: Some(Type::Fun(
                vec![(String::new(), Type::Con(TypeCtor::Top))],
                Box::new(Type::Con(TypeCtor::String)),
            )),
        };
        meta.add_overloaded_method("convert".to_string(), overload_set);

        let record = meta.to_record_type();
        if let Type::Record(fields, _) = record {
            assert_eq!(fields.len(), 2);
            assert!(fields.iter().any(|(name, _)| name == "value"));
            assert!(fields.iter().any(|(name, _)| name == "convert"));
        } else {
            panic!("Expected Record type");
        }
    }

    #[test]
    fn test_type_params_tracking() {
        let mut meta = ClassMetadata::new("list".to_string());
        meta.add_type_param("_T".to_string());

        assert_eq!(meta.type_params.len(), 1);
        assert_eq!(meta.type_params[0], "_T");

        let mut dict_meta = ClassMetadata::new("dict".to_string());
        dict_meta.set_type_params(vec!["_KT".to_string(), "_VT".to_string()]);

        assert_eq!(dict_meta.type_params.len(), 2);
        assert_eq!(dict_meta.type_params[0], "_KT");
        assert_eq!(dict_meta.type_params[1], "_VT");
    }

    #[test]
    fn test_create_type_substitution() {
        let mut meta = ClassMetadata::new("list".to_string());
        meta.add_type_param("_T".to_string());

        let type_args = vec![Type::Con(TypeCtor::Int)];
        let subst = meta.create_type_substitution(&type_args);

        assert_eq!(subst.len(), 1);
        assert_eq!(subst.get("_T"), Some(&Type::Con(TypeCtor::Int)));

        let mut dict_meta = ClassMetadata::new("dict".to_string());
        dict_meta.set_type_params(vec!["_KT".to_string(), "_VT".to_string()]);

        let dict_args = vec![Type::Con(TypeCtor::String), Type::Con(TypeCtor::Int)];
        let dict_subst = dict_meta.create_type_substitution(&dict_args);

        assert_eq!(dict_subst.len(), 2);
        assert_eq!(dict_subst.get("_KT"), Some(&Type::Con(TypeCtor::String)));
        assert_eq!(dict_subst.get("_VT"), Some(&Type::Con(TypeCtor::Int)));
    }

    #[test]
    fn test_substitute_type_params() {
        let mut subst = HashMap::new();
        subst.insert("_T".to_string(), Type::Con(TypeCtor::Int));

        let method_type = Type::Fun(
            vec![
                (String::new(), Type::any()),
                (String::new(), Type::Con(TypeCtor::TypeVariable("_T".to_string()))),
            ],
            Box::new(Type::Con(TypeCtor::TypeVariable("_T".to_string()))),
        );

        let substituted = ClassMetadata::substitute_type_params(&method_type, &subst);

        match substituted {
            Type::Fun(params, ret) => {
                assert_eq!(params.len(), 2);
                assert_eq!(params[1].1, Type::Con(TypeCtor::Int));
                assert_eq!(*ret, Type::Con(TypeCtor::Int));
            }
            _ => panic!("Expected Fun type"),
        }
    }

    #[test]
    fn test_substitute_nested_types() {
        let mut subst = HashMap::new();
        subst.insert("_T".to_string(), Type::Con(TypeCtor::String));

        let method_type = Type::Fun(
            vec![(String::new(), Type::any())],
            Box::new(Type::Union(vec![
                Type::Con(TypeCtor::TypeVariable("_T".to_string())),
                Type::Con(TypeCtor::NoneType),
            ])),
        );

        let substituted = ClassMetadata::substitute_type_params(&method_type, &subst);

        match substituted {
            Type::Fun(_, ret) => match *ret {
                Type::Union(types) => {
                    assert_eq!(types.len(), 2);
                    assert_eq!(types[0], Type::Con(TypeCtor::String));
                    assert_eq!(types[1], Type::Con(TypeCtor::NoneType));
                }
                _ => panic!("Expected Union type"),
            },
            _ => panic!("Expected Fun type"),
        }
    }

    #[test]
    fn test_substitute_multiple_params() {
        let mut subst = HashMap::new();
        subst.insert("_KT".to_string(), Type::Con(TypeCtor::String));
        subst.insert("_VT".to_string(), Type::Con(TypeCtor::Int));

        let method_type = Type::Fun(
            vec![
                (String::new(), Type::any()),
                (String::new(), Type::Con(TypeCtor::TypeVariable("_KT".to_string()))),
            ],
            Box::new(Type::Union(vec![
                Type::Con(TypeCtor::TypeVariable("_VT".to_string())),
                Type::Con(TypeCtor::NoneType),
            ])),
        );

        let substituted = ClassMetadata::substitute_type_params(&method_type, &subst);

        match substituted {
            Type::Fun(params, ret) => {
                assert_eq!(params.len(), 2);
                assert_eq!(params[1].1, Type::Con(TypeCtor::String));
                match *ret {
                    Type::Union(types) => {
                        assert_eq!(types[0], Type::Con(TypeCtor::Int));
                        assert_eq!(types[1], Type::Con(TypeCtor::NoneType));
                    }
                    _ => panic!("Expected Union return type"),
                }
            }
            _ => panic!("Expected Fun type"),
        }
    }

    #[test]
    fn test_lookup_attribute_with_inheritance_single_level() {
        let mut registry = ClassRegistry::new();

        let mut base = ClassMetadata::new("Base".to_string());
        base.add_method(
            "base_method".to_string(),
            Type::Fun(
                vec![(String::new(), Type::any())],
                Box::new(Type::Con(TypeCtor::String)),
            ),
        );
        registry.register_class("Base".to_string(), base);

        let mut derived = ClassMetadata::new("Derived".to_string());
        derived.add_base_class("Base".to_string());
        derived.add_method(
            "derived_method".to_string(),
            Type::Fun(vec![(String::new(), Type::any())], Box::new(Type::Con(TypeCtor::Int))),
        );
        registry.register_class("Derived".to_string(), derived);

        assert!(
            registry
                .lookup_attribute_with_inheritance("Derived", "derived_method")
                .is_some()
        );
        assert!(
            registry
                .lookup_attribute_with_inheritance("Derived", "base_method")
                .is_some()
        );
        assert!(
            registry
                .lookup_attribute_with_inheritance("Derived", "nonexistent")
                .is_none()
        );
    }

    #[test]
    fn test_lookup_attribute_with_inheritance_multi_level() {
        let mut registry = ClassRegistry::new();

        let mut grandparent = ClassMetadata::new("GrandParent".to_string());
        grandparent.add_method(
            "grandparent_method".to_string(),
            Type::Fun(vec![(String::new(), Type::any())], Box::new(Type::Con(TypeCtor::Bool))),
        );
        registry.register_class("GrandParent".to_string(), grandparent);

        let mut parent = ClassMetadata::new("Parent".to_string());
        parent.add_base_class("GrandParent".to_string());
        parent.add_method(
            "parent_method".to_string(),
            Type::Fun(
                vec![(String::new(), Type::any())],
                Box::new(Type::Con(TypeCtor::String)),
            ),
        );
        registry.register_class("Parent".to_string(), parent);

        let mut child = ClassMetadata::new("Child".to_string());
        child.add_base_class("Parent".to_string());
        child.add_method(
            "child_method".to_string(),
            Type::Fun(vec![(String::new(), Type::any())], Box::new(Type::Con(TypeCtor::Int))),
        );
        registry.register_class("Child".to_string(), child);

        assert!(
            registry
                .lookup_attribute_with_inheritance("Child", "child_method")
                .is_some()
        );
        assert!(
            registry
                .lookup_attribute_with_inheritance("Child", "parent_method")
                .is_some()
        );
        assert!(
            registry
                .lookup_attribute_with_inheritance("Child", "grandparent_method")
                .is_some()
        );
    }

    #[test]
    fn test_lookup_attribute_with_inheritance_protocol_base() {
        let mut registry = ClassRegistry::new();

        let mut protocol = ClassMetadata::new("MutableSequence".to_string());
        protocol.set_protocol(true);
        protocol.add_method(
            "append".to_string(),
            Type::Fun(
                vec![
                    (String::new(), Type::any()),
                    (String::new(), Type::Con(TypeCtor::TypeVariable("_T".to_string()))),
                ],
                Box::new(Type::Con(TypeCtor::NoneType)),
            ),
        );
        protocol.add_method(
            "extend".to_string(),
            Type::Fun(
                vec![
                    (String::new(), Type::any()),
                    (String::new(), Type::Con(TypeCtor::Iterable)),
                ],
                Box::new(Type::Con(TypeCtor::NoneType)),
            ),
        );
        registry.register_class("MutableSequence".to_string(), protocol);

        let mut list_class = ClassMetadata::new("list".to_string());
        list_class.add_base_class("MutableSequence".to_string());
        list_class.add_type_param("_T".to_string());
        registry.register_class("list".to_string(), list_class);

        let append_method = registry.lookup_attribute_with_inheritance("list", "append");
        assert!(
            append_method.is_some(),
            "list should inherit append from MutableSequence"
        );

        let extend_method = registry.lookup_attribute_with_inheritance("list", "extend");
        assert!(
            extend_method.is_some(),
            "list should inherit extend from MutableSequence"
        );
    }

    #[test]
    fn test_lookup_attribute_with_inheritance_multiple_bases() {
        let mut registry = ClassRegistry::new();

        let mut base1 = ClassMetadata::new("Base1".to_string());
        base1.add_method(
            "method1".to_string(),
            Type::Fun(vec![(String::new(), Type::any())], Box::new(Type::Con(TypeCtor::Int))),
        );
        registry.register_class("Base1".to_string(), base1);

        let mut base2 = ClassMetadata::new("Base2".to_string());
        base2.add_method(
            "method2".to_string(),
            Type::Fun(
                vec![(String::new(), Type::any())],
                Box::new(Type::Con(TypeCtor::String)),
            ),
        );
        registry.register_class("Base2".to_string(), base2);

        let mut derived = ClassMetadata::new("Derived".to_string());
        derived.add_base_class("Base1".to_string());
        derived.add_base_class("Base2".to_string());
        registry.register_class("Derived".to_string(), derived);

        assert!(
            registry
                .lookup_attribute_with_inheritance("Derived", "method1")
                .is_some()
        );
        assert!(
            registry
                .lookup_attribute_with_inheritance("Derived", "method2")
                .is_some()
        );
    }

    #[test]
    fn test_is_subclass_of_with_protocol() {
        let mut registry = ClassRegistry::new();

        let mut protocol = ClassMetadata::new("Sized".to_string());
        protocol.set_protocol(true);
        registry.register_class("Sized".to_string(), protocol);

        let mut list_class = ClassMetadata::new("list".to_string());
        list_class.add_base_class("Sized".to_string());
        registry.register_class("list".to_string(), list_class);

        assert!(registry.is_subclass_of("list", "Sized"));
    }
}

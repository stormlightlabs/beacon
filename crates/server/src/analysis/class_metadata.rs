use beacon_core::types::{OverloadSet, Type};
use std::collections::HashMap;

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
    pub fn primary_type(&self) -> Option<&Type> {
        match self {
            MethodType::Single(ty) => Some(ty),
            MethodType::Overloaded(overload_set) => overload_set.implementation.as_ref(),
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
    pub fields: HashMap<String, Type>,

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
}

impl ClassMetadata {
    /// Create new class metadata with the given name
    pub fn new(name: String) -> Self {
        Self {
            name,
            fields: HashMap::new(),
            methods: HashMap::new(),
            init_type: None,
            is_protocol: false,
            properties: HashMap::new(),
            classmethods: HashMap::new(),
            staticmethods: HashMap::new(),
            metaclass: None,
            new_type: None,
            base_classes: Vec::new(),
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
            } else if let MethodType::Overloaded(overload_set) = method_type {
                if let Some(first_sig) = overload_set.signatures.first() {
                    fields_vec.push((name.clone(), first_sig.clone()));
                }
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
#[derive(Debug, Default)]
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
}

#[cfg(test)]
mod tests {
    use super::*;
    use beacon_core::types::{Type, TypeCtor};

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
            vec![Type::Con(TypeCtor::Int)],
            Box::new(Type::Con(TypeCtor::String)),
        ));

        assert!(method_type.primary_type().is_some());
        assert_eq!(method_type.all_signatures().len(), 1);
    }

    #[test]
    fn test_method_type_overloaded() {
        let overload_set = OverloadSet {
            signatures: vec![
                Type::Fun(vec![Type::Con(TypeCtor::Int)], Box::new(Type::Con(TypeCtor::String))),
                Type::Fun(vec![Type::Con(TypeCtor::String)], Box::new(Type::Con(TypeCtor::Int))),
            ],
            implementation: Some(Type::Fun(
                vec![Type::Con(TypeCtor::Top)],
                Box::new(Type::Con(TypeCtor::Top)),
            )),
        };

        let method_type = MethodType::Overloaded(overload_set);
        assert!(method_type.primary_type().is_some());
        assert_eq!(method_type.all_signatures().len(), 3); // 2 overloads + 1 impl
    }

    #[test]
    fn test_add_method_signature_creates_overload() {
        let mut meta = ClassMetadata::new("Example".to_string());

        let sig1 = Type::Fun(vec![Type::Con(TypeCtor::Int)], Box::new(Type::Con(TypeCtor::String)));
        let sig2 = Type::Fun(vec![Type::Con(TypeCtor::String)], Box::new(Type::Con(TypeCtor::Int)));

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
                    vec![Type::Con(TypeCtor::Int)],
                    Box::new(Type::Con(TypeCtor::String)),
                ),
                Type::Fun(
                    vec![Type::Con(TypeCtor::String)],
                    Box::new(Type::Con(TypeCtor::Int)),
                ),
            ],
            implementation: None,
        };

        meta.add_overloaded_method("convert".to_string(), overload_set);

        let result = meta.lookup_method_for_args("convert", &[Type::Con(TypeCtor::Int)]);
        assert!(result.is_some(), "Should resolve overload for int argument");

        if let Some(Type::Fun(params, ret)) = result {
            assert_eq!(params.len(), 1, "Should have int parameter (self excluded from signature)");
            assert!(matches!(ret.as_ref(), Type::Con(TypeCtor::String)));
        }
    }

    #[test]
    fn test_get_all_method_signatures() {
        let mut meta = ClassMetadata::new("Example".to_string());

        let single_method = Type::Fun(vec![Type::Con(TypeCtor::Int)], Box::new(Type::Con(TypeCtor::Bool)));
        meta.add_method("check".to_string(), single_method);

        let overload_set = OverloadSet {
            signatures: vec![
                Type::Fun(vec![Type::Con(TypeCtor::Int)], Box::new(Type::Con(TypeCtor::String))),
                Type::Fun(vec![Type::Con(TypeCtor::String)], Box::new(Type::Con(TypeCtor::Int))),
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
                vec![Type::Con(TypeCtor::Int)],
                Box::new(Type::Con(TypeCtor::String)),
            )],
            implementation: Some(Type::Fun(
                vec![Type::Con(TypeCtor::Top)],
                Box::new(Type::Con(TypeCtor::String)),
            )),
        };
        meta.add_overloaded_method("convert".to_string(), overload_set);

        let record = meta.to_record_type();
        if let Type::Record(fields, _) = record {
            assert_eq!(fields.len(), 2); // value + convert
            assert!(fields.iter().any(|(name, _)| name == "value"));
            assert!(fields.iter().any(|(name, _)| name == "convert"));
        } else {
            panic!("Expected Record type");
        }
    }
}

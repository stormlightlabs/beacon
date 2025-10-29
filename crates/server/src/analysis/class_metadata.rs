use beacon_core::types::Type;
use std::collections::HashMap;

/// Metadata about a class definition including fields, methods, and __init__ signature
#[derive(Debug, Clone)]
pub struct ClassMetadata {
    /// The name of the class
    pub name: String,

    /// Field types discovered from __init__ and class body, mapping field name to its type
    pub fields: HashMap<String, Type>,

    /// Maps method name to its function type (excluding __init__)
    pub methods: HashMap<String, Type>,

    /// The type signature of __init__ if present
    /// Parameters include 'self' as first parameter
    pub init_type: Option<Type>,
}

impl ClassMetadata {
    /// Create new class metadata with the given name
    pub fn new(name: String) -> Self {
        Self { name, fields: HashMap::new(), methods: HashMap::new(), init_type: None }
    }

    /// Add a field with its type
    pub fn add_field(&mut self, field_name: String, field_type: Type) {
        self.fields.insert(field_name, field_type);
    }

    /// Add a method with its type
    pub fn add_method(&mut self, method_name: String, method_type: Type) {
        self.methods.insert(method_name, method_type);
    }

    /// Set the __init__ method type
    pub fn set_init_type(&mut self, init_type: Type) {
        self.init_type = Some(init_type);
    }

    /// Look up a field or method by name
    pub fn lookup_attribute(&self, attr_name: &str) -> Option<&Type> {
        self.fields.get(attr_name).or_else(|| self.methods.get(attr_name))
    }

    /// Check if an attribute is a method
    pub fn is_method(&self, attr_name: &str) -> bool {
        self.methods.contains_key(attr_name)
    }

    /// Look up a method by name
    pub fn lookup_method(&self, method_name: &str) -> Option<&Type> {
        self.methods.get(method_name)
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
}

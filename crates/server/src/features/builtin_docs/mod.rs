//! Embedded documentation for Python built-in types
//!
//! This version loads documentation entries from a JSON file at compile time
//! using `include_bytes!` and deserializes them into `BuiltinDoc` structs.
//! This drastically simplifies maintenance and keeps source cleaner.

use once_cell::sync::Lazy;
use serde::Deserialize;
use std::collections::HashMap;

/// Documentation for a built-in type or method.
#[derive(Debug, Clone, Deserialize)]
pub struct BuiltinDoc {
    /// The name of the type or method
    pub name: String,
    /// Brief description
    pub description: String,
    /// Common methods (for types) or signature (for methods)
    pub details: Option<String>,
    /// Link to official Python documentation
    pub doc_link: Option<String>,
}

/// Static lazy map of built-in documentation loaded from JSON.
pub static BUILTIN_TYPES: Lazy<HashMap<String, BuiltinDoc>> = Lazy::new(|| {
    // Load embedded JSON data at compile time
    let bytes = include_bytes!("builtin_docs.json");

    // Deserialize into Vec<BuiltinDoc>
    let docs: Vec<BuiltinDoc> = serde_json::from_slice(bytes).expect("Failed to parse builtin_docs.json");

    // Convert into HashMap for fast lookup
    docs.into_iter().map(|doc| (doc.name.clone(), doc)).collect()
});

/// Get documentation for a built-in type
pub fn get_builtin_doc(name: &str) -> Option<&BuiltinDoc> {
    BUILTIN_TYPES.get(name)
}

/// Check if a name refers to a built-in type
pub fn is_builtin_type(name: &str) -> bool {
    BUILTIN_TYPES.contains_key(name)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_builtin_types_exist() {
        assert!(is_builtin_type("str"));
        assert!(is_builtin_type("int"));
        assert!(is_builtin_type("list"));
        assert!(is_builtin_type("dict"));
        assert!(!is_builtin_type("NotABuiltinType"));
    }

    #[test]
    fn test_get_str_doc() {
        let doc = get_builtin_doc("str").unwrap();
        assert_eq!(doc.name, "str");
        assert!(doc.description.contains("string"));
        assert!(doc.details.is_some());
        assert!(doc.doc_link.is_some());
    }

    #[test]
    fn test_get_list_doc() {
        let doc = get_builtin_doc("list").unwrap();
        assert_eq!(doc.name, "list");
        assert!(doc.description.contains("sequence"));
        let details = doc.details.as_ref().unwrap();
        assert!(details.contains("append"));
        assert!(details.contains("pop"));
    }

    #[test]
    fn test_get_dict_doc() {
        let doc = get_builtin_doc("dict").unwrap();
        assert_eq!(doc.name, "dict");
        let details = doc.details.as_ref().unwrap();
        assert!(details.contains("get"));
        assert!(details.contains("keys"));
    }
}

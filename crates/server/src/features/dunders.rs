//! Dunder (magic method and builtin variable) metadata and documentation
//!
//! Provides access to documentation for Python's special methods and builtin variables

use once_cell::sync::Lazy;
use rustc_hash::FxHashMap;
use serde::Deserialize;

/// Information about a dunder (magic method or builtin variable)
#[derive(Debug, Clone, Deserialize)]
pub struct DunderInfo {
    pub name: String,
    pub category: String,
    pub doc: String,
    pub link: String,
}

/// Registry of all dunders loaded from dunders.json
static DUNDER_REGISTRY: Lazy<FxHashMap<String, DunderInfo>> = Lazy::new(|| {
    let json_data = include_str!("../data/dunders.json");
    let dunders: Vec<DunderInfo> = serde_json::from_str(json_data).unwrap_or_else(|e| {
        eprintln!("Failed to parse dunders.json: {e}");
        Vec::new()
    });

    dunders.into_iter().map(|info| (info.name.clone(), info)).collect()
});

/// Look up documentation for a dunder by name
pub fn get_dunder_info(name: &str) -> Option<&'static DunderInfo> {
    DUNDER_REGISTRY.get(name)
}

/// Check if a name is a known dunder
pub fn is_dunder(name: &str) -> bool {
    DUNDER_REGISTRY.contains_key(name)
}

/// Get all dunder names
pub fn all_dunder_names() -> Vec<&'static str> {
    DUNDER_REGISTRY.keys().map(|s| s.as_str()).collect()
}

/// Get all dunders of a specific category (method or variable)
pub fn get_dunders_by_category(category: &str) -> Vec<&'static DunderInfo> {
    DUNDER_REGISTRY
        .values()
        .filter(|info| info.category == category)
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_dunder_info() {
        let init_info = get_dunder_info("__init__");
        assert!(init_info.is_some());
        let info = init_info.unwrap();
        assert_eq!(info.name, "__init__");
        assert_eq!(info.category, "method");
        assert!(info.doc.contains("Initializer"));
    }

    #[test]
    fn test_is_dunder() {
        assert!(is_dunder("__init__"));
        assert!(is_dunder("__name__"));
        assert!(!is_dunder("regular_function"));
    }

    #[test]
    fn test_get_dunders_by_category() {
        let methods = get_dunders_by_category("method");
        assert!(!methods.is_empty());
        assert!(methods.iter().any(|info| info.name == "__init__"));

        let variables = get_dunders_by_category("variable");
        assert!(!variables.is_empty());
        assert!(variables.iter().any(|info| info.name == "__name__"));
    }

    #[test]
    fn test_all_dunder_names() {
        let names = all_dunder_names();
        assert!(!names.is_empty());
        assert!(names.contains(&"__init__"));
        assert!(names.contains(&"__name__"));
    }

    #[test]
    fn test_dunder_info_fields() {
        let name_info = get_dunder_info("__name__").unwrap();
        assert!(!name_info.doc.is_empty());
        assert!(!name_info.link.is_empty());
        assert!(name_info.link.starts_with("https://"));
    }
}

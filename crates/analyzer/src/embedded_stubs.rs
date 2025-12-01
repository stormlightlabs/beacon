//! Embedded typeshed stdlib stubs module
//!
//! This module provides access to the typeshed stdlib stubs that are bundled
//! into the binary at compile time.

include!(concat!(env!("OUT_DIR"), "/embedded_stubs.rs"));

use crate::loader::StubFile;
use rustc_hash::FxHashMap;
use std::path::PathBuf;

/// Get an embedded stub file by module name
pub fn get_embedded_stub(module_name: &str) -> Option<StubFile> {
    EMBEDDED_STUBS.get(module_name).map(|content| StubFile {
        module: module_name.to_string(),
        path: PathBuf::from(format!("typeshed/stdlib/{module_name}.pyi")),
        exports: FxHashMap::default(),
        is_partial: false,
        reexports: Vec::new(),
        all_exports: None,
        content: Some((*content).to_string()),
    })
}

/// Get all available embedded stub module names
pub fn available_stubs() -> Vec<&'static str> {
    EMBEDDED_STUBS.keys().copied().collect()
}

/// Get typeshed version information
pub fn version_info() -> &'static TypeshedVersion {
    &TYPESHED_VERSION
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_version_info_available() {
        let version = version_info();
        assert!(!version.commit_hash.is_empty());
        assert!(!version.build_timestamp.is_empty());
    }

    #[test]
    fn test_embedded_stubs_available() {
        let stubs = available_stubs();
        assert!(
            !stubs.is_empty() || version_info().stub_count == 0,
            "Should have stubs or explicitly indicate zero stubs"
        );
    }

    #[test]
    fn test_get_embedded_stub_creates_valid_stub_file() {
        let stubs = available_stubs();
        if let Some(module_name) = stubs.first() {
            let stub = get_embedded_stub(module_name).expect("Should find stub");
            assert_eq!(stub.module, *module_name);
            assert!(stub.content.is_some());
            assert!(!stub.content.unwrap().is_empty());
        }
    }
}

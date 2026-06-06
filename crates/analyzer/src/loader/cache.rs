use beacon_core::Type;
use rustc_hash::FxHashMap;
use std::path::PathBuf;

#[derive(Default)]
pub struct StubCache {
    cache: FxHashMap<String, StubFile>,
}

impl StubCache {
    pub fn new() -> Self {
        Self::default()
    }

    /// Get a stub from the cache
    pub fn get(&self, module_name: &str) -> Option<&StubFile> {
        self.cache.get(module_name)
    }

    /// Insert a stub into the cache
    pub fn insert(&mut self, module_name: String, stub: StubFile) {
        self.cache.insert(module_name, stub);
    }

    /// Check if a stub exists in the cache
    pub fn contains(&self, module_name: &str) -> bool {
        self.cache.contains_key(module_name)
    }

    /// Remove a stub from the cache
    pub fn remove(&mut self, module_name: &str) -> Option<StubFile> {
        self.cache.remove(module_name)
    }

    /// Get the number of stubs in the cache
    pub fn len(&self) -> usize {
        self.cache.len()
    }

    /// Check if the cache is empty
    pub fn is_empty(&self) -> bool {
        self.cache.is_empty()
    }

    /// Get an iterator over the cached stubs
    pub fn iter(&self) -> impl Iterator<Item = (&String, &StubFile)> {
        self.cache.iter()
    }
}

/// Parsed stub file (.pyi)
#[derive(Debug, Clone)]
pub struct StubFile {
    /// Module name
    pub module: String,
    /// File path to the stub
    pub path: PathBuf,
    /// Exported symbols and their types
    pub exports: FxHashMap<String, Type>,
    /// Whether this is a partial stub
    pub is_partial: bool,
    /// Re-exported modules (from X import Y as Y)
    pub reexports: Vec<String>,
    /// __all__ declaration if present
    pub all_exports: Option<Vec<String>>,
    /// Embedded content for built-in stubs (avoids filesystem access)
    pub content: Option<String>,
}

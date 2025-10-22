//! Caching layer for parse results and type inference
//!
//! Implements an LRU cache for storing expensive analysis artifacts:
//! - Parse trees and ASTs
//! - Type inference results
//! - Symbol tables
//! - Constraint solve results

use beacon_core::Type;
use lru::LruCache;
use rustc_hash::FxHashMap;
use std::num::NonZeroUsize;
use std::sync::{Arc, RwLock};
use url::Url;

/// Cache key for type inference results at specific AST node locations
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct TypeCacheKey {
    /// Document URI
    pub uri: Url,

    /// Node ID or byte offset in the document
    pub node_id: usize,
}

/// Cached type inference result for a node
#[derive(Debug, Clone)]
pub struct CachedType {
    /// The inferred type
    pub ty: Type,

    /// Document version when this was cached
    pub version: i32,
}

/// LRU cache for type inference results
///
/// Caches inferred types per document and node location.
/// Invalidates entries when document version changes.
pub struct TypeCache {
    cache: Arc<RwLock<LruCache<TypeCacheKey, CachedType>>>,
}

impl TypeCache {
    /// Create a new type cache with the given capacity
    pub fn new(capacity: usize) -> Self {
        let cap = NonZeroUsize::new(capacity).unwrap_or(NonZeroUsize::new(100).unwrap());

        Self {
            cache: Arc::new(RwLock::new(LruCache::new(cap))),
        }
    }

    /// Get a cached type for a node
    ///
    /// Returns None if not cached or version mismatch.
    pub fn get(&self, uri: &Url, node_id: usize, version: i32) -> Option<Type> {
        let mut cache = self.cache.write().unwrap();

        let key = TypeCacheKey {
            uri: uri.clone(),
            node_id,
        };

        cache.get(&key).and_then(|cached| {
            if cached.version == version {
                Some(cached.ty.clone())
            } else {
                None
            }
        })
    }

    /// Store a type in the cache
    pub fn insert(&self, uri: Url, node_id: usize, version: i32, ty: Type) {
        let mut cache = self.cache.write().unwrap();

        let key = TypeCacheKey { uri, node_id };
        let cached = CachedType { ty, version };

        cache.put(key, cached);
    }

    /// Invalidate all entries for a document
    ///
    /// Called when a document is edited or closed.
    pub fn invalidate_document(&self, uri: &Url) {
        let mut cache = self.cache.write().unwrap();

        // Collect keys to remove
        let keys_to_remove: Vec<_> = cache
            .iter()
            .filter(|(key, _)| key.uri == *uri)
            .map(|(key, _)| key.clone())
            .collect();

        for key in keys_to_remove {
            cache.pop(&key);
        }
    }

    /// Clear the entire cache
    pub fn clear(&self) {
        let mut cache = self.cache.write().unwrap();
        cache.clear();
    }

    /// Get cache statistics
    ///
    /// TODO: Implement detailed statistics (hit rate, memory usage, etc.)
    pub fn stats(&self) -> CacheStats {
        let cache = self.cache.read().unwrap();

        CacheStats {
            size: cache.len(),
            capacity: cache.cap().get(),
        }
    }
}

impl Default for TypeCache {
    fn default() -> Self {
        Self::new(100)
    }
}

/// Cache statistics
#[derive(Debug, Clone)]
pub struct CacheStats {
    pub size: usize,
    pub capacity: usize,
}

/// Cache for constraint solving results
///
/// TODO: Implement caching for constraint generation and solving
/// This will cache the constraint sets and substitutions per document/scope.
pub struct ConstraintCache {
    _cache: Arc<RwLock<FxHashMap<String, ()>>>,
}

impl ConstraintCache {
    pub fn new() -> Self {
        Self {
            _cache: Arc::new(RwLock::new(FxHashMap::default())),
        }
    }

    /// TODO: Implement constraint caching
    pub fn get(&self, _key: &str) -> Option<()> {
        None
    }

    /// TODO: Implement constraint storage
    pub fn insert(&self, _key: String, _value: ()) {}

    /// TODO: Implement invalidation
    pub fn invalidate(&self, _key: &str) {}
}

impl Default for ConstraintCache {
    fn default() -> Self {
        Self::new()
    }
}

/// Main cache manager coordinating all caches
///
/// Provides a unified interface for caching all analysis artifacts.
pub struct CacheManager {
    /// Type inference cache
    pub type_cache: TypeCache,

    /// Constraint solving cache
    pub constraint_cache: ConstraintCache,
    // TODO: Add more specialized caches:
    // - Symbol resolution cache
    // - Module import cache
    // - Stub file cache
}

impl CacheManager {
    /// Create a new cache manager with default settings
    pub fn new() -> Self {
        Self {
            type_cache: TypeCache::default(),
            constraint_cache: ConstraintCache::default(),
        }
    }

    /// Create a new cache manager with custom capacity
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            type_cache: TypeCache::new(capacity),
            constraint_cache: ConstraintCache::default(),
        }
    }

    /// Invalidate all caches for a document
    pub fn invalidate_document(&self, uri: &Url) {
        self.type_cache.invalidate_document(uri);
        // TODO: Invalidate other caches
    }

    /// Clear all caches
    pub fn clear_all(&self) {
        self.type_cache.clear();
        // TODO: Clear other caches
    }

    /// Get aggregate cache statistics
    ///
    /// TODO: Combine stats from all caches
    pub fn stats(&self) -> CacheStats {
        self.type_cache.stats()
    }
}

impl Default for CacheManager {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use beacon_core::{Type, TypeCtor};

    #[test]
    fn test_type_cache_insert_get() {
        let cache = TypeCache::new(10);
        let uri = Url::parse("file:///test.py").unwrap();

        let ty = Type::Con(TypeCtor::Int);
        cache.insert(uri.clone(), 42, 1, ty.clone());

        let retrieved = cache.get(&uri, 42, 1);
        assert_eq!(retrieved, Some(ty));
    }

    #[test]
    fn test_type_cache_version_mismatch() {
        let cache = TypeCache::new(10);
        let uri = Url::parse("file:///test.py").unwrap();

        let ty = Type::Con(TypeCtor::Int);
        cache.insert(uri.clone(), 42, 1, ty);

        // Different version should return None
        let retrieved = cache.get(&uri, 42, 2);
        assert_eq!(retrieved, None);
    }

    #[test]
    fn test_type_cache_invalidate_document() {
        let cache = TypeCache::new(10);
        let uri = Url::parse("file:///test.py").unwrap();

        let ty = Type::Con(TypeCtor::Int);
        cache.insert(uri.clone(), 42, 1, ty);

        cache.invalidate_document(&uri);

        let retrieved = cache.get(&uri, 42, 1);
        assert_eq!(retrieved, None);
    }

    #[test]
    fn test_cache_manager_creation() {
        let manager = CacheManager::new();
        let stats = manager.stats();

        assert_eq!(stats.size, 0);
        assert!(stats.capacity > 0);
    }

    #[test]
    fn test_cache_manager_with_capacity() {
        let manager = CacheManager::with_capacity(50);
        let stats = manager.stats();

        assert_eq!(stats.capacity, 50);
    }
}

//! Caching layer for parse results and type inference
//!
//! Implements an LRU cache for storing expensive analysis artifacts:
//! - Parse trees and ASTs
//! - Type inference results
//! - Symbol tables
//! - Constraint solve results
//! - Scope-level analysis results for incremental re-analysis

use beacon_core::Type;
use beacon_parser::ScopeId;
use lru::LruCache;
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
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
/// Caches inferred types per document and node location. Invalidates entries when document version changes.
pub struct TypeCache {
    cache: Arc<RwLock<LruCache<TypeCacheKey, CachedType>>>,
}

impl TypeCache {
    /// Create a new type cache with the given capacity
    pub fn new(capacity: usize) -> Self {
        let cap = NonZeroUsize::new(capacity).unwrap_or(NonZeroUsize::new(100).unwrap());
        Self { cache: Arc::new(RwLock::new(LruCache::new(cap))) }
    }

    /// Get a cached type for a node
    pub fn get(&self, uri: &Url, node_id: usize, version: i32) -> Option<Type> {
        let mut cache = self.cache.write().unwrap();

        let key = TypeCacheKey { uri: uri.clone(), node_id };

        cache.get(&key).and_then(
            |cached| {
                if cached.version == version { Some(cached.ty.clone()) } else { None }
            },
        )
    }

    /// Store a type in the cache
    pub fn insert(&self, uri: Url, node_id: usize, version: i32, ty: Type) {
        let mut cache = self.cache.write().unwrap();

        let key = TypeCacheKey { uri, node_id };
        let cached = CachedType { ty, version };

        cache.put(key, cached);
    }

    /// Invalidate all entries for a document when edited or closed.
    pub fn invalidate_document(&self, uri: &Url) {
        let mut cache = self.cache.write().unwrap();

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

        CacheStats { size: cache.len(), capacity: cache.cap().get() }
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

/// Cache key for introspection results
#[derive(Debug, Clone, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub struct IntrospectionCacheKey {
    /// Module name (e.g., "math", "os.path")
    pub module: String,
    /// Symbol name (e.g., "sqrt", "join")
    pub symbol: String,
}

/// Persistent on disk cache (signature and docstring data from external modules) for Python introspection results
pub struct IntrospectionCache {
    cache: Arc<RwLock<LruCache<IntrospectionCacheKey, crate::introspection::IntrospectionResult>>>,
    cache_file: Option<std::path::PathBuf>,
}

impl IntrospectionCache {
    /// Create a new introspection cache
    pub fn new(workspace_root: Option<&std::path::Path>) -> Self {
        let capacity = NonZeroUsize::new(1000).unwrap();
        let mut cache = LruCache::new(capacity);

        let cache_file = workspace_root
            .map(|root| root.join(".beacon-cache").join("introspection.json"))
            .or_else(|| dirs::cache_dir().map(|dir| dir.join("beacon-lsp").join("introspection.json")));

        if let Some(ref file) = cache_file {
            if file.exists() {
                if let Ok(loaded) = Self::load_from_disk(file) {
                    for (key, value) in loaded {
                        cache.put(key, value);
                    }
                    tracing::debug!(
                        "Loaded {} introspection cache entries from {}",
                        cache.len(),
                        file.display()
                    );
                }
            }
        }

        Self { cache: Arc::new(RwLock::new(cache)), cache_file }
    }

    /// Get a cached introspection result
    pub fn get(&self, module: &str, symbol: &str) -> Option<crate::introspection::IntrospectionResult> {
        let mut cache = self.cache.write().unwrap();
        let key = IntrospectionCacheKey { module: module.to_string(), symbol: symbol.to_string() };
        cache.get(&key).cloned()
    }

    /// Store an introspection result in the cache and persists to disk asynchronously.
    pub fn insert(&self, module: String, symbol: String, result: crate::introspection::IntrospectionResult) {
        let mut cache = self.cache.write().unwrap();
        let key = IntrospectionCacheKey { module, symbol };

        cache.put(key, result);
        drop(cache);

        if let Err(e) = self.save_to_disk() {
            tracing::warn!("Failed to persist introspection cache: {}", e);
        }
    }

    pub fn clear(&self) {
        let mut cache = self.cache.write().unwrap();
        cache.clear();

        if let Some(ref file) = self.cache_file {
            let _ = std::fs::remove_file(file);
        }
    }

    /// Save cache to disk
    fn save_to_disk(&self) -> std::io::Result<()> {
        let cache = self.cache.read().unwrap();

        if let Some(ref file) = self.cache_file {
            if let Some(parent) = file.parent() {
                std::fs::create_dir_all(parent)?;
            }

            let entries: Vec<_> = cache.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
            let json = serde_json::to_string_pretty(&entries).map_err(std::io::Error::other)?;
            std::fs::write(file, json)?;
            tracing::debug!(
                "Saved {} introspection cache entries to {}",
                entries.len(),
                file.display()
            );
        }

        Ok(())
    }

    /// Load cache from disk
    fn load_from_disk(
        file: &std::path::Path,
    ) -> std::io::Result<Vec<(IntrospectionCacheKey, crate::introspection::IntrospectionResult)>> {
        let json = std::fs::read_to_string(file)?;

        let entries: Vec<(IntrospectionCacheKey, crate::introspection::IntrospectionResult)> =
            serde_json::from_str(&json).map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, e))?;

        Ok(entries)
    }

    /// Get cache statistics
    pub fn stats(&self) -> CacheStats {
        let cache = self.cache.read().unwrap();
        CacheStats { size: cache.len(), capacity: cache.cap().get() }
    }
}

impl Default for IntrospectionCache {
    fn default() -> Self {
        Self::new(None)
    }
}

/// Cache key for scope-level analysis results
///
/// Uniquely identifies a scope within a document for granular incremental re-analysis.
/// The content hash allows detecting changes to the scope's source code.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ScopeCacheKey {
    /// Document URI
    pub uri: Url,
    /// Unique identifier for the scope within the document
    pub scope_id: ScopeId,
    /// Hash of the scope's source content (start_byte..end_byte)
    /// Used to detect if the scope's code has changed
    pub content_hash: u64,
}

impl ScopeCacheKey {
    /// Create a new scope cache key by hashing the source content
    pub fn new(uri: Url, scope_id: ScopeId, source_content: &str) -> Self {
        let mut hasher = DefaultHasher::new();
        source_content.hash(&mut hasher);
        let content_hash = hasher.finish();

        Self { uri, scope_id, content_hash }
    }
}

/// Cached analysis result for a single scope
///
/// Stores the analysis artifacts for a specific scope, enabling fine-grained
/// incremental re-analysis. When a document changes, only modified scopes
/// and their dependents need to be re-analyzed.
#[derive(Debug, Clone)]
pub struct CachedScopeResult {
    /// Map from AST node IDs to inferred types (only for nodes in this scope)
    pub type_map: FxHashMap<usize, Type>,
    /// Map from source positions to node IDs (only for positions in this scope)
    pub position_map: FxHashMap<(usize, usize), usize>,
    /// Scope IDs that this scope depends on (e.g., parent scope, referenced scopes)
    pub dependencies: Vec<ScopeId>,
}

/// LRU cache for scope-level analysis results
///
/// Provides granular incremental re-analysis by caching at scope level rather than
/// document level. When a document is edited:
///
/// 1. Identify which scopes changed (by comparing content hashes)
/// 2. Invalidate changed scopes and their dependents
/// 3. Re-use cached results for unchanged scopes
///
/// This dramatically reduces re-analysis cost for large files with localized changes.
pub struct ScopeCache {
    cache: Arc<RwLock<LruCache<ScopeCacheKey, CachedScopeResult>>>,
    /// Track hit/miss statistics for performance monitoring
    hits: Arc<RwLock<usize>>,
    misses: Arc<RwLock<usize>>,
}

impl ScopeCache {
    /// Create a new scope cache with the given capacity
    pub fn new(capacity: usize) -> Self {
        let cap = NonZeroUsize::new(capacity).unwrap_or(NonZeroUsize::new(200).unwrap());
        Self {
            cache: Arc::new(RwLock::new(LruCache::new(cap))),
            hits: Arc::new(RwLock::new(0)),
            misses: Arc::new(RwLock::new(0)),
        }
    }

    /// Get a cached scope result
    ///
    /// Returns the cached result if the scope exists with matching content hash.
    /// Records hit/miss statistics for performance monitoring.
    pub fn get(&self, key: &ScopeCacheKey) -> Option<CachedScopeResult> {
        let mut cache = self.cache.write().unwrap();

        if let Some(result) = cache.get(key).cloned() {
            *self.hits.write().unwrap() += 1;
            Some(result)
        } else {
            *self.misses.write().unwrap() += 1;
            None
        }
    }

    /// Store a scope analysis result in the cache
    pub fn insert(&self, key: ScopeCacheKey, result: CachedScopeResult) {
        let mut cache = self.cache.write().unwrap();
        cache.put(key, result);
    }

    /// Invalidate a specific scope
    pub fn invalidate_scope(&self, uri: &Url, scope_id: ScopeId) {
        let mut cache = self.cache.write().unwrap();

        let keys_to_remove: Vec<_> = cache
            .iter()
            .filter(|(key, _)| key.uri == *uri && key.scope_id == scope_id)
            .map(|(key, _)| key.clone())
            .collect();

        for key in keys_to_remove {
            cache.pop(&key);
        }
    }

    /// Invalidate all scopes for a document
    pub fn invalidate_document(&self, uri: &Url) {
        let mut cache = self.cache.write().unwrap();

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
        *self.hits.write().unwrap() = 0;
        *self.misses.write().unwrap() = 0;
    }

    /// Get cache statistics including hit rate
    pub fn stats(&self) -> ScopeCacheStats {
        let cache = self.cache.read().unwrap();
        let hits = *self.hits.read().unwrap();
        let misses = *self.misses.read().unwrap();
        let total_requests = hits + misses;
        let hit_rate = if total_requests > 0 { (hits as f64 / total_requests as f64) * 100.0 } else { 0.0 };

        ScopeCacheStats { size: cache.len(), capacity: cache.cap().get(), hits, misses, hit_rate }
    }
}

impl Default for ScopeCache {
    fn default() -> Self {
        Self::new(200)
    }
}

/// Scope cache statistics including hit rate
#[derive(Debug, Clone)]
pub struct ScopeCacheStats {
    pub size: usize,
    pub capacity: usize,
    pub hits: usize,
    pub misses: usize,
    pub hit_rate: f64,
}

/// Cache key for full analysis results
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct AnalysisCacheKey {
    /// Document URI
    pub uri: Url,
    /// Document version
    pub version: i32,
}

/// Cached analysis result for a document
///
/// Stores the complete analysis artifacts including type map, diagnostics, CFG, and static analysis.
/// Enables incremental re-analysis by caching expensive computations.
#[derive(Debug, Clone)]
pub struct CachedAnalysisResult {
    /// Map from AST node IDs to inferred types
    pub type_map: FxHashMap<usize, beacon_core::Type>,
    /// Map from source positions to node IDs
    pub position_map: FxHashMap<(usize, usize), usize>,
    /// Type errors encountered during analysis
    pub type_errors: Vec<crate::analysis::TypeErrorInfo>,
    /// Static analysis results (data flow analysis)
    pub static_analysis: Option<crate::analysis::data_flow::DataFlowResult>,
}

/// LRU cache for full analysis results per document
///
/// This cache stores complete analysis artifacts to enable incremental re-analysis.
///
/// Cache invalidation happens automatically based on document version:
/// - Cache key includes both URI and version
/// - When document changes, version increments
pub struct AnalysisCache {
    cache: Arc<RwLock<LruCache<AnalysisCacheKey, CachedAnalysisResult>>>,
}

impl AnalysisCache {
    /// Create a new analysis cache with the given capacity
    pub fn new(capacity: usize) -> Self {
        let cap = NonZeroUsize::new(capacity).unwrap_or(NonZeroUsize::new(50).unwrap());
        Self { cache: Arc::new(RwLock::new(LruCache::new(cap))) }
    }

    /// Get a cached analysis result for a document
    pub fn get(&self, uri: &Url, version: i32) -> Option<CachedAnalysisResult> {
        let mut cache = self.cache.write().unwrap();
        let key = AnalysisCacheKey { uri: uri.clone(), version };
        cache.get(&key).cloned()
    }

    /// Store an analysis result in the cache
    pub fn insert(&self, uri: Url, version: i32, result: CachedAnalysisResult) {
        let mut cache = self.cache.write().unwrap();
        let key = AnalysisCacheKey { uri, version };
        cache.put(key, result);
    }

    /// Invalidate all cached entries for a document (all versions) when a document is closed or when we want to free memory.
    pub fn invalidate_document(&self, uri: &Url) {
        let mut cache = self.cache.write().unwrap();
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
    pub fn stats(&self) -> CacheStats {
        let cache = self.cache.read().unwrap();
        CacheStats { size: cache.len(), capacity: cache.cap().get() }
    }
}

impl Default for AnalysisCache {
    fn default() -> Self {
        Self::new(50)
    }
}

/// Main cache manager coordinating all caches
///
/// Provides a unified interface for caching all analysis artifacts.
pub struct CacheManager {
    /// Type inference cache (node-level caching)
    pub type_cache: TypeCache,
    /// Python introspection cache (persistent)
    pub introspection_cache: IntrospectionCache,
    /// Full analysis result cache (document-level caching for incremental re-analysis)
    pub analysis_cache: AnalysisCache,
    /// Scope-level analysis cache for granular incremental re-analysis
    pub scope_cache: ScopeCache,
}

impl CacheManager {
    /// Create a new cache manager with default settings
    pub fn new() -> Self {
        Self {
            type_cache: TypeCache::default(),
            introspection_cache: IntrospectionCache::default(),
            analysis_cache: AnalysisCache::default(),
            scope_cache: ScopeCache::default(),
        }
    }

    /// Create a new cache manager with custom capacity
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            type_cache: TypeCache::new(capacity),
            introspection_cache: IntrospectionCache::default(),
            analysis_cache: AnalysisCache::new(capacity),
            scope_cache: ScopeCache::new(capacity * 2),
        }
    }

    /// Create a new cache manager with workspace root for persistent caching
    pub fn with_workspace(capacity: usize, workspace_root: Option<&std::path::Path>) -> Self {
        Self {
            type_cache: TypeCache::new(capacity),
            introspection_cache: IntrospectionCache::new(workspace_root),
            analysis_cache: AnalysisCache::new(capacity),
            scope_cache: ScopeCache::new(capacity * 2),
        }
    }

    /// Invalidate all caches for a document
    pub fn invalidate_document(&self, uri: &Url) {
        self.type_cache.invalidate_document(uri);
        self.analysis_cache.invalidate_document(uri);
        self.scope_cache.invalidate_document(uri);
    }

    /// Clear all caches
    pub fn clear_all(&self) {
        self.type_cache.clear();
        self.analysis_cache.clear();
        self.scope_cache.clear();
    }

    /// Get aggregate cache statistics
    pub fn stats(&self) -> CacheStats {
        self.analysis_cache.stats()
    }

    /// Get scope cache statistics
    pub fn scope_stats(&self) -> ScopeCacheStats {
        self.scope_cache.stats()
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
    use crate::introspection::IntrospectionResult;
    use beacon_core::{Type, TypeCtor};
    use tempfile::tempdir;

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

    #[test]
    fn test_introspection_cache_load_from_disk() {
        let dir = tempdir().unwrap();
        let file_path = dir.path().join("introspection.json");
        let entries = vec![(
            IntrospectionCacheKey { module: "math".to_string(), symbol: "sqrt".to_string() },
            IntrospectionResult { signature: "(x)".to_string(), docstring: "Square root".to_string() },
        )];

        let json = serde_json::to_string(&entries).unwrap();
        std::fs::write(&file_path, json).unwrap();

        let loaded = IntrospectionCache::load_from_disk(&file_path).unwrap();
        assert_eq!(loaded, entries);
    }

    #[test]
    fn test_introspection_cache_stats() {
        let dir = tempdir().unwrap();
        let cache = IntrospectionCache::new(Some(dir.path()));

        cache.insert(
            "math".to_string(),
            "sqrt".to_string(),
            IntrospectionResult { signature: "(x)".to_string(), docstring: "Square root".to_string() },
        );
        cache.insert(
            "math".to_string(),
            "cos".to_string(),
            IntrospectionResult { signature: "(x)".to_string(), docstring: "Cosine".to_string() },
        );

        let stats = cache.stats();
        assert_eq!(stats.size, 2);
        assert_eq!(stats.capacity, 1000);
    }

    #[test]
    fn test_introspection_cache_clear() {
        let dir = tempdir().unwrap();
        let cache = IntrospectionCache::new(Some(dir.path()));
        let cache_file = dir.path().join(".beacon-cache").join("introspection.json");

        cache.insert(
            "math".to_string(),
            "sqrt".to_string(),
            IntrospectionResult { signature: "(x)".to_string(), docstring: "Square root".to_string() },
        );
        assert!(cache_file.exists());
        assert_eq!(cache.stats().size, 1);

        cache.clear();
        assert_eq!(cache.stats().size, 0);
        assert!(!cache_file.exists());
    }

    #[test]
    fn test_analysis_cache_creation() {
        let cache = AnalysisCache::new(10);
        let stats = cache.stats();
        assert_eq!(stats.size, 0);
        assert_eq!(stats.capacity, 10);
    }

    #[test]
    fn test_analysis_cache_insert_get() {
        let cache = AnalysisCache::new(10);
        let uri = Url::parse("file:///test.py").unwrap();

        let mut type_map = FxHashMap::default();
        type_map.insert(1, Type::Con(TypeCtor::Int));
        type_map.insert(2, Type::Con(TypeCtor::String));

        let cached_result = CachedAnalysisResult {
            type_map: type_map.clone(),
            position_map: FxHashMap::default(),
            type_errors: vec![],
            static_analysis: None,
        };

        cache.insert(uri.clone(), 1, cached_result);

        let retrieved = cache.get(&uri, 1);
        assert!(retrieved.is_some());

        let result = retrieved.unwrap();
        assert_eq!(result.type_map.len(), 2);
        assert_eq!(result.type_map.get(&1), Some(&Type::Con(TypeCtor::Int)));
        assert_eq!(result.type_map.get(&2), Some(&Type::Con(TypeCtor::String)));
    }

    #[test]
    fn test_analysis_cache_version_mismatch() {
        let cache = AnalysisCache::new(10);
        let uri = Url::parse("file:///test.py").unwrap();

        let cached_result = CachedAnalysisResult {
            type_map: FxHashMap::default(),
            position_map: FxHashMap::default(),
            type_errors: vec![],
            static_analysis: None,
        };

        cache.insert(uri.clone(), 1, cached_result);

        let retrieved = cache.get(&uri, 2);
        assert!(retrieved.is_none());
    }

    #[test]
    fn test_analysis_cache_invalidate_document() {
        let cache = AnalysisCache::new(10);
        let uri = Url::parse("file:///test.py").unwrap();

        let cached_result = CachedAnalysisResult {
            type_map: FxHashMap::default(),
            position_map: FxHashMap::default(),
            type_errors: vec![],
            static_analysis: None,
        };

        cache.insert(uri.clone(), 1, cached_result.clone());
        cache.insert(uri.clone(), 2, cached_result.clone());
        cache.insert(uri.clone(), 3, cached_result);
        assert_eq!(cache.stats().size, 3);

        cache.invalidate_document(&uri);
        assert_eq!(cache.stats().size, 0);
        assert!(cache.get(&uri, 1).is_none());
        assert!(cache.get(&uri, 2).is_none());
        assert!(cache.get(&uri, 3).is_none());
    }

    #[test]
    fn test_analysis_cache_lru_eviction() {
        let cache = AnalysisCache::new(2);

        let uri1 = Url::parse("file:///test1.py").unwrap();
        let uri2 = Url::parse("file:///test2.py").unwrap();
        let uri3 = Url::parse("file:///test3.py").unwrap();

        let cached_result = CachedAnalysisResult {
            type_map: FxHashMap::default(),
            position_map: FxHashMap::default(),
            type_errors: vec![],
            static_analysis: None,
        };

        cache.insert(uri1.clone(), 1, cached_result.clone());
        cache.insert(uri2.clone(), 1, cached_result.clone());
        cache.insert(uri3.clone(), 1, cached_result);

        assert_eq!(cache.stats().size, 2);

        assert!(cache.get(&uri1, 1).is_none());
        assert!(cache.get(&uri2, 1).is_some());
        assert!(cache.get(&uri3, 1).is_some());
    }

    #[test]
    fn test_cache_manager_with_analysis_cache() {
        let manager = CacheManager::new();
        let stats = manager.stats();
        assert_eq!(stats.size, 0);
        assert!(stats.capacity > 0);
    }

    #[test]
    fn test_cache_manager_invalidate_all() {
        let manager = CacheManager::new();
        let uri = Url::parse("file:///test.py").unwrap();

        let cached_result = CachedAnalysisResult {
            type_map: FxHashMap::default(),
            position_map: FxHashMap::default(),
            type_errors: vec![],
            static_analysis: None,
        };

        manager.analysis_cache.insert(uri.clone(), 1, cached_result);
        assert_eq!(manager.analysis_cache.stats().size, 1);

        manager.invalidate_document(&uri);
        assert_eq!(manager.analysis_cache.stats().size, 0);
    }

    #[test]
    fn test_scope_cache_key_content_hashing() {
        let uri = Url::parse("file:///test.py").unwrap();
        let scope_id = beacon_parser::ScopeId::from_raw(0);

        let source1 = "x = 42";
        let source2 = "x = 42";
        let source3 = "x = 43";

        let key1 = ScopeCacheKey::new(uri.clone(), scope_id, source1);
        let key2 = ScopeCacheKey::new(uri.clone(), scope_id, source2);
        let key3 = ScopeCacheKey::new(uri.clone(), scope_id, source3);

        assert_eq!(key1.content_hash, key2.content_hash);
        assert_eq!(key1, key2);
        assert_ne!(key1.content_hash, key3.content_hash);
        assert_ne!(key1, key3);
    }

    #[test]
    fn test_scope_cache_insert_and_get() {
        let cache = ScopeCache::new(10);
        let uri = Url::parse("file:///test.py").unwrap();
        let scope_id = beacon_parser::ScopeId::from_raw(0);

        let source = "def foo(): pass";
        let key = ScopeCacheKey::new(uri, scope_id, source);

        let mut type_map = FxHashMap::default();
        type_map.insert(1, Type::Con(TypeCtor::Int));

        let result =
            CachedScopeResult { type_map: type_map.clone(), position_map: FxHashMap::default(), dependencies: vec![] };

        cache.insert(key.clone(), result);

        let retrieved = cache.get(&key);
        assert!(retrieved.is_some());
        let retrieved_result = retrieved.unwrap();
        assert_eq!(retrieved_result.type_map.len(), 1);
        assert_eq!(retrieved_result.type_map.get(&1), Some(&Type::Con(TypeCtor::Int)));
    }

    #[test]
    fn test_scope_cache_content_change_invalidation() {
        let cache = ScopeCache::new(10);
        let uri = Url::parse("file:///test.py").unwrap();
        let scope_id = beacon_parser::ScopeId::from_raw(0);

        let source1 = "def foo(): pass";
        let source2 = "def foo(): return 42";

        let key1 = ScopeCacheKey::new(uri.clone(), scope_id, source1);
        let key2 = ScopeCacheKey::new(uri, scope_id, source2);

        let result = CachedScopeResult {
            type_map: FxHashMap::default(),
            position_map: FxHashMap::default(),
            dependencies: vec![],
        };

        cache.insert(key1.clone(), result.clone());

        assert!(cache.get(&key1).is_some());
        assert!(cache.get(&key2).is_none());
    }

    #[test]
    fn test_scope_cache_invalidate_scope() {
        let cache = ScopeCache::new(10);
        let uri = Url::parse("file:///test.py").unwrap();
        let scope_id1 = beacon_parser::ScopeId::from_raw(0);
        let scope_id2 = beacon_parser::ScopeId::from_raw(1);

        let source = "def foo(): pass";
        let key1 = ScopeCacheKey::new(uri.clone(), scope_id1, source);
        let key2 = ScopeCacheKey::new(uri.clone(), scope_id2, source);

        let result = CachedScopeResult {
            type_map: FxHashMap::default(),
            position_map: FxHashMap::default(),
            dependencies: vec![],
        };

        cache.insert(key1.clone(), result.clone());
        cache.insert(key2.clone(), result);
        assert!(cache.get(&key1).is_some());
        assert!(cache.get(&key2).is_some());

        cache.invalidate_scope(&uri, scope_id1);
        assert!(cache.get(&key1).is_none());
        assert!(cache.get(&key2).is_some());
    }

    #[test]
    fn test_scope_cache_invalidate_document() {
        let cache = ScopeCache::new(10);
        let uri1 = Url::parse("file:///test1.py").unwrap();
        let uri2 = Url::parse("file:///test2.py").unwrap();
        let scope_id = beacon_parser::ScopeId::from_raw(0);

        let source = "def foo(): pass";
        let key1 = ScopeCacheKey::new(uri1.clone(), scope_id, source);
        let key2 = ScopeCacheKey::new(uri2.clone(), scope_id, source);

        let result = CachedScopeResult {
            type_map: FxHashMap::default(),
            position_map: FxHashMap::default(),
            dependencies: vec![],
        };

        cache.insert(key1.clone(), result.clone());
        cache.insert(key2.clone(), result);
        assert!(cache.get(&key1).is_some());
        assert!(cache.get(&key2).is_some());

        cache.invalidate_document(&uri1);
        assert!(cache.get(&key1).is_none());
        assert!(cache.get(&key2).is_some());
    }

    #[test]
    fn test_scope_cache_statistics() {
        let cache = ScopeCache::new(10);
        let uri = Url::parse("file:///test.py").unwrap();
        let scope_id = beacon_parser::ScopeId::from_raw(0);

        let stats = cache.stats();
        assert_eq!(stats.size, 0);
        assert_eq!(stats.hits, 0);
        assert_eq!(stats.misses, 0);
        assert_eq!(stats.hit_rate, 0.0);

        let source = "def foo(): pass";
        let key = ScopeCacheKey::new(uri, scope_id, source);

        let result = CachedScopeResult {
            type_map: FxHashMap::default(),
            position_map: FxHashMap::default(),
            dependencies: vec![],
        };

        cache.insert(key.clone(), result);
        let _ = cache.get(&key);
        let stats = cache.stats();
        assert_eq!(stats.size, 1);
        assert!(stats.hits > 0);
        assert!(stats.hit_rate > 0.0);
    }

    #[test]
    fn test_scope_cache_lru_eviction() {
        let cache = ScopeCache::new(2);
        let uri = Url::parse("file:///test.py").unwrap();

        let scope_id1 = beacon_parser::ScopeId::from_raw(0);
        let scope_id2 = beacon_parser::ScopeId::from_raw(1);
        let scope_id3 = beacon_parser::ScopeId::from_raw(2);

        let source = "def foo(): pass";
        let key1 = ScopeCacheKey::new(uri.clone(), scope_id1, source);
        let key2 = ScopeCacheKey::new(uri.clone(), scope_id2, source);
        let key3 = ScopeCacheKey::new(uri.clone(), scope_id3, source);

        let result = CachedScopeResult {
            type_map: FxHashMap::default(),
            position_map: FxHashMap::default(),
            dependencies: vec![],
        };

        cache.insert(key1.clone(), result.clone());
        cache.insert(key2.clone(), result.clone());
        cache.insert(key3.clone(), result);

        assert_eq!(cache.stats().size, 2);
        assert!(cache.get(&key1).is_none());
        assert!(cache.get(&key2).is_some());
        assert!(cache.get(&key3).is_some());
    }

    #[test]
    fn test_cache_manager_with_scope_cache() {
        let manager = CacheManager::new();
        let scope_stats = manager.scope_stats();
        assert_eq!(scope_stats.size, 0);
        assert!(scope_stats.capacity > 0);
    }

    #[test]
    fn test_cache_manager_scope_invalidation() {
        let manager = CacheManager::new();
        let uri = Url::parse("file:///test.py").unwrap();
        let scope_id = beacon_parser::ScopeId::from_raw(0);

        let source = "def foo(): pass";
        let key = ScopeCacheKey::new(uri.clone(), scope_id, source);

        let result = CachedScopeResult {
            type_map: FxHashMap::default(),
            position_map: FxHashMap::default(),
            dependencies: vec![],
        };

        manager.scope_cache.insert(key.clone(), result);
        assert_eq!(manager.scope_stats().size, 1);

        manager.invalidate_document(&uri);
        assert_eq!(manager.scope_stats().size, 0);
    }
}

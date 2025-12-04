//! Cache Regression Tests
//!
//! Prevents regressions in cache behavior and ensure cache components work together correctly.

use beacon_core::{Type, TypeCtor};
use beacon_lsp::cache::{
    AnalysisCache, CacheManager, CachedAnalysisResult, CachedScopeResult, IntrospectionCache, ScopeCache,
    ScopeCacheKey, TypeCache,
};
use beacon_lsp::introspection::IntrospectionResult;
use rustc_hash::{FxHashMap, FxHashSet};
use tempfile::tempdir;
use url::Url;

/// TODO: move to shared helper module ([beacon_core]?)
fn file_uri(path: &str) -> Url {
    Url::parse(&format!("file:///{path}")).unwrap()
}

#[test]
fn test_type_cache_version_invalidation() {
    let cache = TypeCache::new(10);
    let uri = file_uri("test.py");

    let int_type = Type::Con(TypeCtor::Int);

    cache.insert(uri.clone(), 42, 1, int_type.clone());

    assert_eq!(cache.get(&uri, 42, 1), Some(int_type.clone()));

    assert_eq!(cache.get(&uri, 42, 2), None);

    cache.insert(uri.clone(), 42, 2, int_type.clone());
    assert_eq!(cache.get(&uri, 42, 2), Some(int_type.clone()));
    assert_eq!(cache.get(&uri, 42, 1), None);

    cache.invalidate_document(&uri);

    assert_eq!(cache.get(&uri, 42, 1), None);
    assert_eq!(cache.get(&uri, 42, 2), None);
}

#[test]
fn test_type_cache_node_isolation() {
    let cache = TypeCache::new(10);
    let uri = file_uri("test.py");

    let int_type = Type::Con(TypeCtor::Int);
    let string_type = Type::Con(TypeCtor::String);

    cache.insert(uri.clone(), 1, 1, int_type.clone());
    cache.insert(uri.clone(), 2, 1, string_type.clone());

    assert_eq!(cache.get(&uri, 1, 1), Some(int_type));
    assert_eq!(cache.get(&uri, 2, 1), Some(string_type));

    assert_eq!(cache.get(&uri, 1, 2), None);
    assert_eq!(cache.get(&uri, 2, 2), None);
}

#[test]
fn test_type_cache_multi_document() {
    let cache = TypeCache::new(10);
    let uri1 = file_uri("test1.py");
    let uri2 = file_uri("test2.py");

    let int_type = Type::Con(TypeCtor::Int);
    let string_type = Type::Con(TypeCtor::String);

    cache.insert(uri1.clone(), 1, 1, int_type.clone());
    cache.insert(uri2.clone(), 1, 1, string_type.clone());

    assert_eq!(cache.get(&uri1, 1, 1), Some(int_type));
    assert_eq!(cache.get(&uri2, 1, 1), Some(string_type.clone()));

    cache.invalidate_document(&uri1);

    assert_eq!(cache.get(&uri1, 1, 1), None);
    assert_eq!(cache.get(&uri2, 1, 1), Some(string_type));
}

#[test]
fn test_introspection_cache_persistence() {
    let temp_dir = tempdir().unwrap();
    let cache = IntrospectionCache::new(Some(temp_dir.path()));

    cache.insert(
        "math".to_string(),
        "sqrt".to_string(),
        IntrospectionResult {
            signature: "(x: float) -> float".to_string(),
            docstring: "Return the square root of x.".to_string(),
        },
    );

    cache.insert(
        "os".to_string(),
        "getcwd".to_string(),
        IntrospectionResult {
            signature: "() -> str".to_string(),
            docstring: "Return current working directory.".to_string(),
        },
    );

    assert_eq!(cache.stats().size, 2);

    drop(cache);

    let reloaded_cache = IntrospectionCache::new(Some(temp_dir.path()));
    assert_eq!(reloaded_cache.stats().size, 2);

    let sqrt_result = reloaded_cache.get("math", "sqrt");
    assert!(sqrt_result.is_some());
    assert_eq!(sqrt_result.unwrap().signature, "(x: float) -> float");

    let getcwd_result = reloaded_cache.get("os", "getcwd");
    assert!(getcwd_result.is_some());
    assert_eq!(getcwd_result.unwrap().signature, "() -> str");
}

#[test]
fn test_introspection_cache_clear_removes_file() {
    let temp_dir = tempdir().unwrap();
    let cache = IntrospectionCache::new(Some(temp_dir.path()));
    let cache_file = temp_dir.path().join(".beacon-cache").join("introspection.json");

    cache.insert(
        "sys".to_string(),
        "exit".to_string(),
        IntrospectionResult { signature: "(code=0)".to_string(), docstring: "Exit".to_string() },
    );

    assert!(cache_file.exists());

    cache.clear();

    assert!(!cache_file.exists());
    assert_eq!(cache.stats().size, 0);
}

#[test]
fn test_introspection_cache_overwrite() {
    let cache = IntrospectionCache::default();

    cache.insert(
        "math".to_string(),
        "sqrt".to_string(),
        IntrospectionResult { signature: "old".to_string(), docstring: "old".to_string() },
    );

    let initial_size = cache.stats().size;

    cache.insert(
        "math".to_string(),
        "sqrt".to_string(),
        IntrospectionResult { signature: "new".to_string(), docstring: "new".to_string() },
    );

    let result = cache.get("math", "sqrt").unwrap();
    assert_eq!(result.signature, "new");
    assert_eq!(result.docstring, "new");

    assert_eq!(cache.stats().size, initial_size);
}

#[test]
fn test_scope_cache_content_hash_sensitivity() {
    let cache = ScopeCache::new(10);
    let uri = file_uri("test.py");
    let scope_id = beacon_parser::ScopeId::from_raw(0);

    let source1 = "def foo():\n    x = 1\n    return x";
    let source2 = "def foo():\n    x = 2\n    return x";
    let source3 = "def foo():\n    x = 1\n    return x";

    let key1 = ScopeCacheKey::new(uri.clone(), scope_id, source1);
    let key2 = ScopeCacheKey::new(uri.clone(), scope_id, source2);
    let key3 = ScopeCacheKey::new(uri.clone(), scope_id, source3);

    let result =
        CachedScopeResult { type_map: FxHashMap::default(), position_map: FxHashMap::default(), dependencies: vec![] };

    cache.insert(key1.clone(), result.clone());

    assert!(cache.get(&key1).is_some());
    assert!(cache.get(&key2).is_none());
    assert!(cache.get(&key3).is_some());

    assert_eq!(key1.content_hash, key3.content_hash);
    assert_ne!(key1.content_hash, key2.content_hash);
}

#[test]
fn test_scope_cache_whitespace_sensitivity() {
    let uri = file_uri("test.py");
    let scope_id = beacon_parser::ScopeId::from_raw(0);

    let source1 = "x = 1";
    let source2 = "x=1";
    let source3 = "x = 1\n";

    let key1 = ScopeCacheKey::new(uri.clone(), scope_id, source1);
    let key2 = ScopeCacheKey::new(uri.clone(), scope_id, source2);
    let key3 = ScopeCacheKey::new(uri.clone(), scope_id, source3);

    assert_ne!(key1.content_hash, key2.content_hash);
    assert_ne!(key1.content_hash, key3.content_hash);
    assert_ne!(key2.content_hash, key3.content_hash);
}

#[test]
fn test_scope_cache_multiple_scopes_per_document() {
    let cache = ScopeCache::new(20);
    let uri = file_uri("test.py");

    let scope_id1 = beacon_parser::ScopeId::from_raw(0);
    let scope_id2 = beacon_parser::ScopeId::from_raw(1);
    let scope_id3 = beacon_parser::ScopeId::from_raw(2);

    let key1 = ScopeCacheKey::new(uri.clone(), scope_id1, "def foo(): pass");
    let key2 = ScopeCacheKey::new(uri.clone(), scope_id2, "def bar(): pass");
    let key3 = ScopeCacheKey::new(uri.clone(), scope_id3, "def baz(): pass");

    let result =
        CachedScopeResult { type_map: FxHashMap::default(), position_map: FxHashMap::default(), dependencies: vec![] };

    cache.insert(key1.clone(), result.clone());
    cache.insert(key2.clone(), result.clone());
    cache.insert(key3.clone(), result.clone());

    assert_eq!(cache.stats().size, 3);

    cache.invalidate_scope(&uri, scope_id2);

    assert!(cache.get(&key1).is_some());
    assert!(cache.get(&key2).is_none());
    assert!(cache.get(&key3).is_some());
    assert_eq!(cache.stats().size, 2);
}

#[test]
fn test_scope_cache_hit_rate_calculation() {
    let cache = ScopeCache::new(10);
    let uri = file_uri("test.py");
    let scope_id = beacon_parser::ScopeId::from_raw(0);

    let key = ScopeCacheKey::new(uri, scope_id, "x = 1");

    let result =
        CachedScopeResult { type_map: FxHashMap::default(), position_map: FxHashMap::default(), dependencies: vec![] };

    assert_eq!(cache.stats().hit_rate, 0.0);

    cache.get(&key);
    let stats = cache.stats();
    assert_eq!(stats.misses, 1);
    assert_eq!(stats.hits, 0);

    cache.insert(key.clone(), result);

    cache.get(&key);
    cache.get(&key);
    cache.get(&key);

    let stats = cache.stats();
    assert_eq!(stats.hits, 3);
    assert_eq!(stats.misses, 1);
    assert_eq!(stats.hit_rate, 75.0);
}

#[test]
fn test_analysis_cache_multiple_versions_same_document() {
    let cache = AnalysisCache::new(10);
    let uri = file_uri("test.py");

    let mut type_map1 = FxHashMap::default();
    type_map1.insert(1, Type::Con(TypeCtor::Int));

    let mut type_map2 = FxHashMap::default();
    type_map2.insert(1, Type::Con(TypeCtor::String));

    let result1 = CachedAnalysisResult {
        type_map: type_map1.clone(),
        position_map: FxHashMap::default(),
        node_spans: FxHashMap::default(),
        safe_any_nodes: FxHashSet::default(),
        type_errors: vec![],
        static_analysis: None,
    };

    let result2 = CachedAnalysisResult {
        type_map: type_map2.clone(),
        position_map: FxHashMap::default(),
        node_spans: FxHashMap::default(),
        safe_any_nodes: FxHashSet::default(),
        type_errors: vec![],
        static_analysis: None,
    };

    cache.insert(uri.clone(), 1, result1);
    cache.insert(uri.clone(), 2, result2);

    let retrieved1 = cache.get(&uri, 1).unwrap();
    let retrieved2 = cache.get(&uri, 2).unwrap();

    assert_eq!(retrieved1.type_map.get(&1), Some(&Type::Con(TypeCtor::Int)));
    assert_eq!(retrieved2.type_map.get(&1), Some(&Type::Con(TypeCtor::String)));
}

#[test]
fn test_analysis_cache_invalidation_removes_all_versions() {
    let cache = AnalysisCache::new(10);
    let uri = file_uri("test.py");

    let result = CachedAnalysisResult {
        type_map: FxHashMap::default(),
        position_map: FxHashMap::default(),
        node_spans: FxHashMap::default(),
        safe_any_nodes: FxHashSet::default(),
        type_errors: vec![],
        static_analysis: None,
    };

    cache.insert(uri.clone(), 1, result.clone());
    cache.insert(uri.clone(), 2, result.clone());
    cache.insert(uri.clone(), 3, result);

    assert_eq!(cache.stats().size, 3);

    cache.invalidate_document(&uri);

    assert_eq!(cache.stats().size, 0);
    assert!(cache.get(&uri, 1).is_none());
    assert!(cache.get(&uri, 2).is_none());
    assert!(cache.get(&uri, 3).is_none());
}

#[test]
fn test_cache_manager_coordinated_invalidation() {
    let manager = CacheManager::new();
    let uri = file_uri("test.py");

    let int_type = Type::Con(TypeCtor::Int);
    manager.type_cache.insert(uri.clone(), 1, 1, int_type);

    let result = CachedAnalysisResult {
        type_map: FxHashMap::default(),
        position_map: FxHashMap::default(),
        node_spans: FxHashMap::default(),
        safe_any_nodes: FxHashSet::default(),
        type_errors: vec![],
        static_analysis: None,
    };
    manager.analysis_cache.insert(uri.clone(), 1, result);

    let scope_id = beacon_parser::ScopeId::from_raw(0);
    let key = ScopeCacheKey::new(uri.clone(), scope_id, "x = 1");
    let scope_result =
        CachedScopeResult { type_map: FxHashMap::default(), position_map: FxHashMap::default(), dependencies: vec![] };
    manager.scope_cache.insert(key.clone(), scope_result);

    assert!(manager.type_cache.get(&uri, 1, 1).is_some());
    assert!(manager.analysis_cache.get(&uri, 1).is_some());
    assert!(manager.scope_cache.get(&key).is_some());

    manager.invalidate_document(&uri);

    assert!(manager.type_cache.get(&uri, 1, 1).is_none());
    assert!(manager.analysis_cache.get(&uri, 1).is_none());
    assert!(manager.scope_cache.get(&key).is_none());
}

#[test]
fn test_cache_manager_selective_scope_invalidation() {
    let manager = CacheManager::new();
    let uri = file_uri("test.py");

    let scope_id1 = beacon_parser::ScopeId::from_raw(0);
    let scope_id2 = beacon_parser::ScopeId::from_raw(1);
    let scope_id3 = beacon_parser::ScopeId::from_raw(2);

    let key1 = ScopeCacheKey::new(uri.clone(), scope_id1, "def foo(): pass");
    let key2 = ScopeCacheKey::new(uri.clone(), scope_id2, "def bar(): pass");
    let key3 = ScopeCacheKey::new(uri.clone(), scope_id3, "def baz(): pass");

    let result =
        CachedScopeResult { type_map: FxHashMap::default(), position_map: FxHashMap::default(), dependencies: vec![] };

    manager.scope_cache.insert(key1.clone(), result.clone());
    manager.scope_cache.insert(key2.clone(), result.clone());
    manager.scope_cache.insert(key3.clone(), result);

    manager.invalidate_selective(&uri, &[scope_id1, scope_id3]);

    assert!(manager.scope_cache.get(&key1).is_none());
    assert!(manager.scope_cache.get(&key2).is_some());
    assert!(manager.scope_cache.get(&key3).is_none());
}

#[test]
fn test_cache_manager_clear_all() {
    let manager = CacheManager::new();
    let uri = file_uri("test.py");

    manager.type_cache.insert(uri.clone(), 1, 1, Type::Con(TypeCtor::Int));

    let result = CachedAnalysisResult {
        type_map: FxHashMap::default(),
        position_map: FxHashMap::default(),
        node_spans: FxHashMap::default(),
        safe_any_nodes: FxHashSet::default(),
        type_errors: vec![],
        static_analysis: None,
    };
    manager.analysis_cache.insert(uri.clone(), 1, result);

    let scope_id = beacon_parser::ScopeId::from_raw(0);
    let key = ScopeCacheKey::new(uri.clone(), scope_id, "x = 1");
    let scope_result =
        CachedScopeResult { type_map: FxHashMap::default(), position_map: FxHashMap::default(), dependencies: vec![] };
    manager.scope_cache.insert(key.clone(), scope_result);

    manager.clear_all();

    assert!(manager.type_cache.get(&uri, 1, 1).is_none());
    assert!(manager.analysis_cache.get(&uri, 1).is_none());
    assert!(manager.scope_cache.get(&key).is_none());
    assert_eq!(manager.stats().size, 0);
    assert_eq!(manager.scope_stats().size, 0);
}

#[test]
fn test_lru_eviction_preserves_most_recent() {
    let cache = AnalysisCache::new(3);

    let uri1 = file_uri("test1.py");
    let uri2 = file_uri("test2.py");
    let uri3 = file_uri("test3.py");
    let uri4 = file_uri("test4.py");

    let result = CachedAnalysisResult {
        type_map: FxHashMap::default(),
        position_map: FxHashMap::default(),
        node_spans: FxHashMap::default(),
        safe_any_nodes: FxHashSet::default(),
        type_errors: vec![],
        static_analysis: None,
    };

    cache.insert(uri1.clone(), 1, result.clone());
    cache.insert(uri2.clone(), 1, result.clone());
    cache.insert(uri3.clone(), 1, result.clone());

    cache.get(&uri1, 1);

    cache.insert(uri4.clone(), 1, result);

    assert!(cache.get(&uri1, 1).is_some());
    assert!(cache.get(&uri2, 1).is_none());
    assert!(cache.get(&uri3, 1).is_some());
    assert!(cache.get(&uri4, 1).is_some());
}

#[test]
fn test_cache_stats_accuracy() {
    let cache = AnalysisCache::new(10);
    assert_eq!(cache.stats().size, 0);
    assert_eq!(cache.stats().capacity, 10);

    let uri = file_uri("test.py");
    let result = CachedAnalysisResult {
        type_map: FxHashMap::default(),
        position_map: FxHashMap::default(),
        node_spans: FxHashMap::default(),
        safe_any_nodes: FxHashSet::default(),
        type_errors: vec![],
        static_analysis: None,
    };

    cache.insert(uri.clone(), 1, result.clone());
    assert_eq!(cache.stats().size, 1);

    cache.insert(uri.clone(), 2, result.clone());
    assert_eq!(cache.stats().size, 2);

    cache.insert(uri.clone(), 3, result);
    assert_eq!(cache.stats().size, 3);

    cache.invalidate_document(&uri);
    assert_eq!(cache.stats().size, 0);
}

#[test]
fn test_cache_manager_with_workspace_persistence() {
    let temp_dir = tempdir().unwrap();
    let manager = CacheManager::with_workspace(50, Some(temp_dir.path()));

    manager.introspection_cache.insert(
        "collections".to_string(),
        "defaultdict".to_string(),
        IntrospectionResult {
            signature: "(default_factory)".to_string(),
            docstring: "Dict with default values".to_string(),
        },
    );

    drop(manager);

    let reloaded_manager = CacheManager::with_workspace(50, Some(temp_dir.path()));
    let result = reloaded_manager.introspection_cache.get("collections", "defaultdict");

    assert!(result.is_some());
    assert_eq!(result.unwrap().signature, "(default_factory)");
}

#[test]
fn test_scope_cache_lru_with_same_document() {
    let cache = ScopeCache::new(2);
    let uri = file_uri("test.py");

    let scope_id1 = beacon_parser::ScopeId::from_raw(0);
    let scope_id2 = beacon_parser::ScopeId::from_raw(1);
    let scope_id3 = beacon_parser::ScopeId::from_raw(2);

    let key1 = ScopeCacheKey::new(uri.clone(), scope_id1, "def foo(): pass");
    let key2 = ScopeCacheKey::new(uri.clone(), scope_id2, "def bar(): pass");
    let key3 = ScopeCacheKey::new(uri.clone(), scope_id3, "def baz(): pass");

    let result =
        CachedScopeResult { type_map: FxHashMap::default(), position_map: FxHashMap::default(), dependencies: vec![] };

    cache.insert(key1.clone(), result.clone());
    cache.insert(key2.clone(), result.clone());
    cache.insert(key3.clone(), result);

    assert_eq!(cache.stats().size, 2);
    assert!(cache.get(&key1).is_none());
    assert!(cache.get(&key2).is_some());
    assert!(cache.get(&key3).is_some());
}

#[test]
fn test_type_cache_clear() {
    let cache = TypeCache::new(10);
    let uri1 = file_uri("test1.py");
    let uri2 = file_uri("test2.py");

    cache.insert(uri1.clone(), 1, 1, Type::Con(TypeCtor::Int));
    cache.insert(uri2.clone(), 1, 1, Type::Con(TypeCtor::String));

    assert!(cache.get(&uri1, 1, 1).is_some());
    assert!(cache.get(&uri2, 1, 1).is_some());

    cache.clear();

    assert!(cache.get(&uri1, 1, 1).is_none());
    assert!(cache.get(&uri2, 1, 1).is_none());
}

#[test]
fn test_scope_cache_different_content_same_scope() {
    let cache = ScopeCache::new(10);
    let uri = file_uri("test.py");
    let scope_id = beacon_parser::ScopeId::from_raw(0);

    let version1 = "def foo():\n    return 1";
    let version2 = "def foo():\n    return 2";

    let key1 = ScopeCacheKey::new(uri.clone(), scope_id, version1);
    let key2 = ScopeCacheKey::new(uri.clone(), scope_id, version2);

    let mut type_map1 = FxHashMap::default();
    type_map1.insert(1, Type::Con(TypeCtor::Int));

    let mut type_map2 = FxHashMap::default();
    type_map2.insert(1, Type::Con(TypeCtor::String));

    let result1 =
        CachedScopeResult { type_map: type_map1.clone(), position_map: FxHashMap::default(), dependencies: vec![] };

    let result2 =
        CachedScopeResult { type_map: type_map2.clone(), position_map: FxHashMap::default(), dependencies: vec![] };

    cache.insert(key1.clone(), result1);
    cache.insert(key2.clone(), result2);

    let retrieved1 = cache.get(&key1).unwrap();
    let retrieved2 = cache.get(&key2).unwrap();

    assert_eq!(retrieved1.type_map.get(&1), Some(&Type::Con(TypeCtor::Int)));
    assert_eq!(retrieved2.type_map.get(&1), Some(&Type::Con(TypeCtor::String)));
}

#[test]
fn test_cache_manager_capacity_configuration() {
    let manager = CacheManager::with_capacity(25);
    assert_eq!(manager.stats().capacity, 25);
    assert_eq!(manager.scope_stats().capacity, 50);
}

#[test]
fn test_introspection_cache_without_workspace() {
    let cache = IntrospectionCache::default();

    cache.insert(
        "json".to_string(),
        "dumps".to_string(),
        IntrospectionResult { signature: "(obj)".to_string(), docstring: "Serialize to JSON".to_string() },
    );

    let result = cache.get("json", "dumps");
    assert!(result.is_some());
    assert_eq!(result.unwrap().signature, "(obj)");
}

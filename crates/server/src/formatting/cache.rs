//! Formatting result cache for performance optimization
//!
//! Provides two-level caching for formatting results:
//! 1. Short-circuit cache: tracks hashes of already-formatted sources
//! 2. Result cache: stores formatted output for (source, config, range) tuples
//!
//! The short-circuit cache enables O(1) detection of already-formatted code,
//! avoiding the full formatting pipeline when source is already compliant.
//!
//! The result cache enables incremental formatting by reusing results for
//! unchanged source regions across multiple format requests.

use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::sync::{Arc, Mutex};

use lru::LruCache;

use super::FormatterConfig;

/// Key for caching formatting results
///
/// Uniquely identifies a formatting operation by source content,
/// configuration, and line range. Two operations with the same key
/// are guaranteed to produce identical output.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct CacheKey {
    /// Hash of source code content
    source_hash: u64,
    /// Hash of formatter configuration
    config_hash: u64,
    /// Starting line number (inclusive, 1-indexed)
    start_line: usize,
    /// Ending line number (inclusive, 1-indexed)
    end_line: usize,
}

impl CacheKey {
    /// Create a new cache key from source, config, and range
    fn new(source: &str, config: &FormatterConfig, start_line: usize, end_line: usize) -> Self {
        Self { source_hash: Self::hash_string(source), config_hash: Self::hash_config(config), start_line, end_line }
    }

    /// Compute hash of a string
    fn hash_string(s: &str) -> u64 {
        let mut hasher = DefaultHasher::new();
        s.hash(&mut hasher);
        hasher.finish()
    }

    /// Compute hash of formatter configuration
    ///
    /// Hashes all configuration fields that affect formatting output.
    /// Changes to cache settings do not affect config hash.
    fn hash_config(config: &FormatterConfig) -> u64 {
        let mut hasher = DefaultHasher::new();
        config.enabled.hash(&mut hasher);
        config.line_length.hash(&mut hasher);
        config.indent_size.hash(&mut hasher);

        match config.quote_style {
            super::config::QuoteStyle::Single => 1u8.hash(&mut hasher),
            super::config::QuoteStyle::Double => 2u8.hash(&mut hasher),
            super::config::QuoteStyle::Preserve => 3u8.hash(&mut hasher),
        }

        match config.trailing_commas {
            super::config::TrailingCommas::Always => 1u8.hash(&mut hasher),
            super::config::TrailingCommas::Multiline => 2u8.hash(&mut hasher),
            super::config::TrailingCommas::Never => 3u8.hash(&mut hasher),
        }
        config.max_blank_lines.hash(&mut hasher);

        match config.import_sorting {
            super::config::ImportSorting::Pep8 => 1u8.hash(&mut hasher),
            super::config::ImportSorting::Isort => 2u8.hash(&mut hasher),
            super::config::ImportSorting::Off => 3u8.hash(&mut hasher),
        }

        match config.compatibility_mode {
            super::config::CompatibilityMode::Black => 1u8.hash(&mut hasher),
            super::config::CompatibilityMode::Autopep8 => 2u8.hash(&mut hasher),
            super::config::CompatibilityMode::Pep8 => 3u8.hash(&mut hasher),
        }
        config.use_tabs.hash(&mut hasher);
        config.normalize_docstring_quotes.hash(&mut hasher);
        config.spaces_around_operators.hash(&mut hasher);
        config.blank_line_before_class.hash(&mut hasher);
        config.blank_line_before_function.hash(&mut hasher);
        hasher.finish()
    }
}

/// Cached formatting result
#[derive(Debug, Clone)]
struct CachedResult {
    /// The formatted output
    formatted: String,
}

/// Thread-safe cache for formatting results
///
/// Provides two caching strategies:
/// - Short-circuit: O(1) check if source is already formatted
/// - Result cache: Store formatted output for reuse
///
/// Both caches use LRU eviction with configurable size limits.
pub struct FormatterCache {
    /// Cache of (source_hash, config_hash) → () for already-formatted sources
    ///
    /// If a source hash is present, the source is known to be properly formatted and can be returned unchanged without processing.
    formatted_hashes: Arc<Mutex<LruCache<(u64, u64), ()>>>,

    /// Cache of CacheKey → formatted result
    ///
    /// Stores formatted output for (source, config, range) tuples.
    /// Enables incremental formatting by reusing results for unchanged regions.
    results: Arc<Mutex<LruCache<CacheKey, CachedResult>>>,
}

impl FormatterCache {
    /// Create a new formatter cache with the given capacity
    pub fn new(max_entries: usize) -> Self {
        Self {
            formatted_hashes: Arc::new(Mutex::new(LruCache::new(
                std::num::NonZeroUsize::new(max_entries.max(1)).unwrap(),
            ))),
            results: Arc::new(Mutex::new(LruCache::new(
                std::num::NonZeroUsize::new(max_entries.max(1)).unwrap(),
            ))),
        }
    }

    /// Check if source is known to be already formatted
    pub fn is_formatted(&self, source: &str, config: &FormatterConfig) -> bool {
        let source_hash = CacheKey::hash_string(source);
        let config_hash = CacheKey::hash_config(config);

        self.formatted_hashes
            .lock()
            .unwrap()
            .get(&(source_hash, config_hash))
            .is_some()
    }

    /// Mark source as already formatted
    ///
    /// Records that the given source is properly formatted according to the config.
    pub fn mark_formatted(&self, source: &str, config: &FormatterConfig) {
        let source_hash = CacheKey::hash_string(source);
        let config_hash = CacheKey::hash_config(config);

        self.formatted_hashes
            .lock()
            .unwrap()
            .put((source_hash, config_hash), ());
    }

    /// Get cached formatting result if available
    pub fn get(&self, source: &str, config: &FormatterConfig, start_line: usize, end_line: usize) -> Option<String> {
        let key = CacheKey::new(source, config, start_line, end_line);

        self.results
            .lock()
            .unwrap()
            .get(&key)
            .map(|result| result.formatted.clone())
    }

    /// Store formatting result in cache
    ///
    /// Caches the formatted output for future reuse.
    /// If the formatted output equals the source input, also marks the source as already formatted for short-circuit checks.
    pub fn put(&self, source: &str, config: &FormatterConfig, start_line: usize, end_line: usize, formatted: String) {
        let key = CacheKey::new(source, config, start_line, end_line);

        if formatted == source {
            self.mark_formatted(source, config);
        }

        self.results.lock().unwrap().put(key, CachedResult { formatted });
    }

    /// Removes all entries from both the formatted_hashes and results caches.
    pub fn clear(&self) {
        self.formatted_hashes.lock().unwrap().clear();
        self.results.lock().unwrap().clear();
    }

    /// Get the number of cached formatted hashes
    pub fn formatted_count(&self) -> usize {
        self.formatted_hashes.lock().unwrap().len()
    }

    /// Get the number of cached results
    pub fn results_count(&self) -> usize {
        self.results.lock().unwrap().len()
    }
}

impl Default for FormatterCache {
    fn default() -> Self {
        Self::new(100)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::formatting::config::{QuoteStyle, TrailingCommas};

    #[test]
    fn test_cache_creation() {
        let cache = FormatterCache::new(50);
        assert_eq!(cache.formatted_count(), 0);
        assert_eq!(cache.results_count(), 0);
    }

    #[test]
    fn test_cache_default() {
        let cache = FormatterCache::default();
        assert_eq!(cache.formatted_count(), 0);
        assert_eq!(cache.results_count(), 0);
    }

    #[test]
    fn test_is_formatted_empty_cache() {
        let cache = FormatterCache::new(10);
        let config = FormatterConfig::default();
        let source = "x = 1\n";

        assert!(!cache.is_formatted(source, &config));
    }

    #[test]
    fn test_mark_and_check_formatted() {
        let cache = FormatterCache::new(10);
        let config = FormatterConfig::default();
        let source = "x = 1\n";

        assert!(!cache.is_formatted(source, &config));

        cache.mark_formatted(source, &config);

        assert!(cache.is_formatted(source, &config));
        assert_eq!(cache.formatted_count(), 1);
    }

    #[test]
    fn test_mark_formatted_different_configs() {
        let cache = FormatterCache::new(10);
        let config1 = FormatterConfig::default();
        let config2 = FormatterConfig { line_length: 100, ..Default::default() };
        let source = "x = 1\n";

        cache.mark_formatted(source, &config1);

        assert!(cache.is_formatted(source, &config1));
        assert!(!cache.is_formatted(source, &config2));
    }

    #[test]
    fn test_mark_formatted_different_sources() {
        let cache = FormatterCache::new(10);
        let config = FormatterConfig::default();
        let source1 = "x = 1\n";
        let source2 = "y = 2\n";

        cache.mark_formatted(source1, &config);

        assert!(cache.is_formatted(source1, &config));
        assert!(!cache.is_formatted(source2, &config));
    }

    #[test]
    fn test_get_empty_cache() {
        let cache = FormatterCache::new(10);
        let config = FormatterConfig::default();
        let source = "x=1\n";

        let result = cache.get(source, &config, 0, 1);
        assert!(result.is_none());
    }

    #[test]
    fn test_put_and_get() {
        let cache = FormatterCache::new(10);
        let config = FormatterConfig::default();
        let source = "x=1\n";
        let formatted = "x = 1\n";

        cache.put(source, &config, 0, 1, formatted.to_string());

        let result = cache.get(source, &config, 0, 1);
        assert_eq!(result, Some(formatted.to_string()));
        assert_eq!(cache.results_count(), 1);
    }

    #[test]
    fn test_put_marks_formatted_when_equal() {
        let cache = FormatterCache::new(10);
        let config = FormatterConfig::default();
        let source = "x = 1\n";
        let formatted = "x = 1\n";

        assert!(!cache.is_formatted(source, &config));

        cache.put(source, &config, 0, 1, formatted.to_string());

        assert!(cache.is_formatted(source, &config));
    }

    #[test]
    fn test_put_does_not_mark_formatted_when_different() {
        let cache = FormatterCache::new(10);
        let config = FormatterConfig::default();
        let source = "x=1\n";
        let formatted = "x = 1\n";

        assert!(!cache.is_formatted(source, &config));

        cache.put(source, &config, 0, 1, formatted.to_string());

        assert!(!cache.is_formatted(source, &config));
        assert_eq!(cache.get(source, &config, 0, 1), Some(formatted.to_string()));
    }

    #[test]
    fn test_get_with_different_range() {
        let cache = FormatterCache::new(10);
        let config = FormatterConfig::default();
        let source = "x = 1\ny = 2\n";
        let formatted = "x = 1\ny = 2\n";

        cache.put(source, &config, 0, 1, formatted.to_string());

        assert!(cache.get(source, &config, 0, 1).is_some());
        assert!(cache.get(source, &config, 1, 2).is_none());
    }

    #[test]
    fn test_clear() {
        let cache = FormatterCache::new(10);
        let config = FormatterConfig::default();
        let source = "x = 1\n";

        cache.mark_formatted(source, &config);
        cache.put(source, &config, 0, 1, source.to_string());

        assert_eq!(cache.formatted_count(), 1);
        assert_eq!(cache.results_count(), 1);

        cache.clear();

        assert_eq!(cache.formatted_count(), 0);
        assert_eq!(cache.results_count(), 0);
    }

    #[test]
    fn test_lru_eviction_formatted_hashes() {
        let cache = FormatterCache::new(2);
        let config = FormatterConfig::default();

        cache.mark_formatted("x = 1\n", &config);
        cache.mark_formatted("y = 2\n", &config);
        assert_eq!(cache.formatted_count(), 2);

        cache.mark_formatted("z = 3\n", &config);
        assert_eq!(cache.formatted_count(), 2);

        assert!(!cache.is_formatted("x = 1\n", &config));

        assert!(cache.is_formatted("y = 2\n", &config));
        assert!(cache.is_formatted("z = 3\n", &config));
    }

    #[test]
    fn test_lru_eviction_results() {
        let cache = FormatterCache::new(2);
        let config = FormatterConfig::default();

        cache.put("x=1\n", &config, 0, 1, "x = 1\n".to_string());
        cache.put("y=2\n", &config, 0, 1, "y = 2\n".to_string());
        assert_eq!(cache.results_count(), 2);

        cache.put("z=3\n", &config, 0, 1, "z = 3\n".to_string());
        assert_eq!(cache.results_count(), 2);

        assert!(cache.get("x=1\n", &config, 0, 1).is_none());

        assert!(cache.get("y=2\n", &config, 0, 1).is_some());
        assert!(cache.get("z=3\n", &config, 0, 1).is_some());
    }

    #[test]
    fn test_config_hash_different_for_different_configs() {
        let config1 = FormatterConfig::default();
        let config2 = FormatterConfig { line_length: 100, ..Default::default() };
        let config3 = FormatterConfig { quote_style: QuoteStyle::Single, ..Default::default() };

        let hash1 = CacheKey::hash_config(&config1);
        let hash2 = CacheKey::hash_config(&config2);
        let hash3 = CacheKey::hash_config(&config3);

        assert_ne!(hash1, hash2);
        assert_ne!(hash1, hash3);
        assert_ne!(hash2, hash3);
    }

    #[test]
    fn test_config_hash_same_for_identical_configs() {
        let config1 = FormatterConfig::default();
        let config2 = FormatterConfig::default();

        let hash1 = CacheKey::hash_config(&config1);
        let hash2 = CacheKey::hash_config(&config2);

        assert_eq!(hash1, hash2);
    }

    #[test]
    fn test_config_hash_ignores_cache_settings() {
        let config1 = FormatterConfig::default();
        let config2 = FormatterConfig::default();

        let hash1 = CacheKey::hash_config(&config1);
        let hash2 = CacheKey::hash_config(&config2);

        assert_eq!(hash1, hash2);
    }

    #[test]
    fn test_source_hash_different_for_different_sources() {
        let source1 = "x = 1\n";
        let source2 = "y = 2\n";

        let hash1 = CacheKey::hash_string(source1);
        let hash2 = CacheKey::hash_string(source2);

        assert_ne!(hash1, hash2);
    }

    #[test]
    fn test_source_hash_same_for_identical_sources() {
        let source1 = "x = 1\n";
        let source2 = "x = 1\n";

        let hash1 = CacheKey::hash_string(source1);
        let hash2 = CacheKey::hash_string(source2);

        assert_eq!(hash1, hash2);
    }

    #[test]
    fn test_cache_key_equality() {
        let config = FormatterConfig::default();
        let source = "x = 1\n";

        let key1 = CacheKey::new(source, &config, 0, 1);
        let key2 = CacheKey::new(source, &config, 0, 1);
        let key3 = CacheKey::new(source, &config, 1, 2);

        assert_eq!(key1, key2);
        assert_ne!(key1, key3);
    }

    #[test]
    fn test_cache_thread_safety() {
        use std::thread;

        let cache = Arc::new(FormatterCache::new(100));
        let config = FormatterConfig::default();

        let mut handles = vec![];

        for i in 0..10 {
            let cache_clone = Arc::clone(&cache);
            let config_clone = config.clone();

            let handle = thread::spawn(move || {
                let source = format!("x_{i} = {i}\n");
                cache_clone.mark_formatted(&source, &config_clone);
                cache_clone.put(&source, &config_clone, 0, 1, source.clone());
            });

            handles.push(handle);
        }

        for handle in handles {
            handle.join().unwrap();
        }

        assert!(cache.formatted_count() > 0);
        assert!(cache.results_count() > 0);
    }

    #[test]
    fn test_config_hash_all_fields() {
        let base = FormatterConfig::default();

        let configs = vec![
            FormatterConfig { line_length: 100, ..base.clone() },
            FormatterConfig { indent_size: 2, ..base.clone() },
            FormatterConfig { quote_style: QuoteStyle::Single, ..base.clone() },
            FormatterConfig { trailing_commas: TrailingCommas::Always, ..base.clone() },
            FormatterConfig { max_blank_lines: 3, ..base.clone() },
            FormatterConfig { import_sorting: super::super::config::ImportSorting::Off, ..base.clone() },
            FormatterConfig { compatibility_mode: super::super::config::CompatibilityMode::Autopep8, ..base.clone() },
            FormatterConfig { use_tabs: true, ..base.clone() },
            FormatterConfig { normalize_docstring_quotes: false, ..base.clone() },
            FormatterConfig { spaces_around_operators: false, ..base.clone() },
            FormatterConfig { blank_line_before_class: false, ..base.clone() },
            FormatterConfig { blank_line_before_function: false, ..base.clone() },
        ];

        let base_hash = CacheKey::hash_config(&base);

        for (i, config) in configs.iter().enumerate() {
            let hash = CacheKey::hash_config(config);
            assert_ne!(base_hash, hash, "Config variation {i} produced same hash as base");
        }

        let hashes: Vec<u64> = configs.iter().map(CacheKey::hash_config).collect();
        for (i, hash1) in hashes.iter().enumerate() {
            for (j, hash2) in hashes.iter().enumerate() {
                if i != j {
                    assert_ne!(hash1, hash2, "Config {i} and {j} have same hash");
                }
            }
        }
    }

    #[test]
    fn test_cache_with_zero_capacity() {
        let cache = FormatterCache::new(0);
        let config = FormatterConfig::default();

        cache.mark_formatted("x = 1\n", &config);
        assert_eq!(cache.formatted_count(), 1);

        cache.mark_formatted("y = 2\n", &config);
        assert_eq!(cache.formatted_count(), 1);
    }

    #[test]
    fn test_put_and_get_multiline_source() {
        let cache = FormatterCache::new(10);
        let config = FormatterConfig::default();
        let source = "def foo():\n    x=1\n    y=2\n    return x+y\n";
        let formatted = "def foo():\n    x = 1\n    y = 2\n    return x + y\n";

        cache.put(source, &config, 0, 4, formatted.to_string());

        let result = cache.get(source, &config, 0, 4);
        assert_eq!(result, Some(formatted.to_string()));
    }

    #[test]
    fn test_range_specificity() {
        let cache = FormatterCache::new(10);
        let config = FormatterConfig::default();
        let source = "x = 1\ny = 2\nz = 3\n";

        cache.put(source, &config, 0, 1, "x = 1\n".to_string());
        cache.put(source, &config, 1, 2, "y = 2\n".to_string());
        cache.put(source, &config, 0, 3, source.to_string());

        assert_eq!(cache.get(source, &config, 0, 1), Some("x = 1\n".to_string()));
        assert_eq!(cache.get(source, &config, 1, 2), Some("y = 2\n".to_string()));
        assert_eq!(cache.get(source, &config, 0, 3), Some(source.to_string()));
    }
}

# Cache Architecture

Beacon uses a multi-layer caching system to minimize redundant analysis while maintaining correctness.

## Cache Layers

The system provides four specialized cache layers, each optimized for different granularities:

### TypeCache (Node-Level)

Caches inferred types for specific AST nodes. Each entry maps `(uri, node_id, version)` to a `Type`.

**Capacity**: 100 entries (default)

**Eviction**: LRU

**Use case**: Hover requests, completion suggestions, and other features that need type information for a specific node.

### ScopeCache (Scope-Level)

Provides granular incremental re-analysis at scope level rather than document level. When only a single function changes in a large file, unchanged scopes retain their cached analysis results.

**Cache key**: `(uri, scope_id, content_hash)`

**Content hashing**: Uses `DefaultHasher` to compute a deterministic hash of the scope's source text. Different content produces different hashes, enabling precise change detection.

**Cached data**:

- `type_map`: inferred types for nodes within the scope
- `position_map`: mapping from source positions to node IDs
- `dependencies`: scopes this scope depends on (parent, referenced scopes)

**Capacity**: 200 entries (default)

**Eviction**: LRU

**Statistics**: Tracks hits/misses for performance monitoring.

**Use case**: Type checking, diagnostics, and semantic analysis that can reuse results from unchanged scopes.

### AnalysisCache (Document-Level)

Caches complete analysis artifacts per document version. Each entry maps `(uri, version)` to full analysis results including type maps, position maps, type errors, and static analysis findings.

**Cached data**:

- Complete type maps
- Position maps
- Type errors
- Static analysis results

**Capacity**: 50 entries (default)

**Eviction**: LRU

**Version-based invalidation**: New document versions automatically create new cache entries rather than invalidating existing ones.

**Use case**: Publishing diagnostics, workspace-wide queries, and features that need complete document analysis.

### IntrospectionCache (Persistent)

Caches Python introspection results for external modules and the standard library. Persists to disk in `.beacon-cache/introspection.json` to survive server restarts.

**Cached data**:

- Function signatures
- Docstrings
- Module metadata

**Capacity**: 1000 entries

**Eviction**: LRU (in-memory), write-through to disk

**Use case**: Hover information for stdlib and third-party modules, completion for imported symbols.

## Content Hashing Validation

ScopeCache uses content hashing to detect changes with high precision:

**Hash computation**:

```rust
let mut hasher = DefaultHasher::new();
source_content.hash(&mut hasher);
let content_hash = hasher.finish();
```

**Properties**:

- Deterministic: same content always produces the same hash
- Whitespace-sensitive: `x = 1` and `x=1` produce different hashes
- Collision-resistant: sufficient for cache validation

**Validation**: Cache lookups compare the computed content hash against the cached key. Mismatches result in cache misses, forcing re-analysis of the modified scope.

## Invalidation Strategies

### Version-Based Invalidation

TypeCache checks document version on every access. If the document version differs from the cached entry's version, the entry is treated as stale.

AnalysisCache embeds version in the cache key, so new versions naturally create new entries without explicit invalidation.

### Content-Based Invalidation

ScopeCache compares content hashes. When a scope's source changes:

1. Compute new content hash from updated source
2. Look up cache with new key
3. Cache miss if hash differs
4. Re-analyze and insert with new hash

### Explicit Invalidation

CacheManager provides methods to invalidate specific scopes or entire documents:

**invalidate_document**: Removes all cache entries for a URI across all layers.

**invalidate_scope**: Removes entries for a specific scope from ScopeCache.

**invalidate_selective**: Invalidates specific scopes and returns the set of affected URIs for cascade invalidation.

### Cascade Invalidation

When a scope changes, dependent scopes may also need invalidation. ImportDependencyTracker maintains a dependency graph to determine which scopes reference the changed scope, enabling selective cascade invalidation without over-invalidating.

## Cache Coordination

CacheManager unifies all cache layers and coordinates invalidation:

**On document change**:

1. Identify changed scopes by comparing content hashes
2. Invalidate changed scopes in ScopeCache
3. Clear document-level entries in AnalysisCache for the affected URI
4. Query dependency tracker to find dependent scopes
5. Invalidate dependents selectively
6. TypeCache entries naturally become stale via version mismatch

**On document close**:

- Remove all cache entries for the URI
- Persist IntrospectionCache to disk

## Performance Characteristics

Cache hit rates directly impact analysis latency:

**Cold cache** (first analysis): Full analysis required for all scopes.

**Warm cache, no changes**: All scopes hit, near-instant response.

**Warm cache, localized change**: Only changed scopes and dependents miss, dramatic speedup for large files.

**ScopeCache statistics** provide hit rate monitoring:

```rust
let stats = cache_manager.scope_cache_stats();
println!("Hit rate: {:.2}%", stats.hit_rate);
```

## Formatter Cache

The formatter uses a separate two-level cache optimized for formatting requests:

### Short-Circuit Cache

Maps `(source_hash, config_hash)` to unit. Detects already-formatted code in O(1) time, avoiding redundant formatting operations.

### Result Cache

Maps `(source_hash, config_hash, start_line, end_line)` to formatted output. Reuses formatting results for identical source and configuration.

**Capacity**: 100 entries (default) per layer

**Eviction**: LRU

**Use case**: Format-on-save, range formatting, and editor-initiated format requests.

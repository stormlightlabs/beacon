# Beacon Roadmap to v1.0

## Current State (v0.1.0)

- **Type Checker**: HM inference with variance, protocols, generics, pattern matching, constant expressions
- **Static Analysis**: Full CFG construction, data flow analysis, reachability checking
- **Linting**: 30 PyFlakes-level rules (BEA001-BEA030) with suppression support
- **Formatting**: Production-ready PEP8 formatter with two-level caching
- **LSP Features**: All major features working (hover, completion, goto-def, references, rename, etc.)
- **Cache**: Advanced three-tier caching with scope-level invalidation and import tracking

## Release Ladder to v1.0

| Version | Theme                                      | Key Deliverables                              | Status                                    |
| ------- | ------------------------------------------ | --------------------------------------------- | ----------------------------------------- |
| v0.1.0  | Baseline formatter + parser + analyzer     | Initial release                               | shipped                                   |
| v0.2.0  | Cache granularity & selective invalidation | Merge ft/cache-granularity, import tracking   | done                                      |
| v0.3.0  | Type checker bug fixes                     | Fix known HM issues, improve test coverage    | [shipped](#v030---type-checker-bug-fixes) |
| v0.4.0  | Type checking modes                        | Implement strict/balanced/relaxed enforcement | [shipped](#v040---type-checking-modes)    |
| v0.5.0  | Stdlib stubs & error messages              | Expand stubs, "did you mean" suggestions      | planned                                   |
| v0.6.0  | Cross-file static analysis                 | Workspace-aware CFG, import/export tracking   | planned                                   |
| v0.7.0  | Advanced data flow                         | Taint analysis, null safety improvements      | planned                                   |
| v0.8.0  | Linting expansion                          | 10 new rules, autofix infrastructure          | planned                                   |
| v0.9.0  | Formatting polish & config                 | Additional PEP8 options, config validation    | planned                                   |
| v0.10.0 | Multi-root workspace support               | workspace/didChangeWorkspaceFolders           | planned                                   |
| v0.11.0 | Snippet engine                             | Context-aware snippets, LSP integration       | planned                                   |
| v0.12.0 | Auto-import & code actions                 | Import management, quick fixes                | planned                                   |
| v0.13.0 | Diagnostics publishing baseline            | publishDiagnostics, severity toggles          | planned                                   |
| v0.14.0 | Monorepo performance - caching             | Workspace-aware cache, parallelization        | planned                                   |
| v0.15.0 | Monorepo performance - scale               | Handle 100k+ LOC projects, benchmarks         | planned                                   |
| v0.16.0 | Telemetry & observability                  | Performance metrics, cache hit rates          | planned                                   |
| v0.17.0 | Production hardening                       | Stress testing, memory profiling, stability   | planned                                   |
| v0.18.0 | Beta stabilization                         | Full QA pass, regression suite                | planned                                   |
| v0.19.0 | Release candidate                          | Migration guide, docs freeze                  | planned                                   |
| v1.0.0  | General availability                       | Stable release                                | planned                                   |

## Type Checker Stabilization (v0.2.0 - v0.5.0)

**Focus**: MVP + Fix bugs, add modes, expand stubs, improve error quality, optimize performance

### v0.2.0 - Cache Granularity & Selective Invalidation

**Requirements:**

- ImportDependencyTracker working for transitive invalidation
- Scope-level content hashing for change detection
- Add regression tests for cache behavior

### v0.3.0 - Type Checker Bug Fixes

[Completed & cut 2025-11-19](https://github.com/stormlightlabs/beacon/releases/tag/v0.3.0)

### v0.4.0 - Type Checking Modes

[Completed & cut 2025-11-21](https://github.com/stormlightlabs/beacon/releases/tag/v0.4.0)

### v0.5.0 - Stdlib Stubs & Error Messages

**Goals:**

- Expand stdlib stub coverage (os, sys, pathlib, asyncio, typing, collections, itertools, functools)
- Integrate [typeshed-stdlib-mirror](https://github.com/stormlightlabs/typeshed-stdlib-mirror)
- Automatic stub updates from typeshed mirror
- Implement "did you mean" suggestions for typos
- Improve type error messages with context
- Add quick fixes for common type errors

**Architecture:**

- Fetch and cache stubs from typeshed-stdlib-mirror
- Version-aware stub selection based on configured Python version
- Merge custom stubs with typeshed stubs (custom takes precedence)
- Incremental stub updates without breaking existing analysis

**Requirements:**

- Typeshed integration working with version selection
- 80%+ stdlib coverage for common modules via typeshed
- "Did you mean" working for undefined names
- Type error messages include suggestions
- Documentation of stub coverage and update process

## Cross-File Static Analysis (v0.6.0 - v0.7.0)

**Focus**: Workspace-aware analysis, advanced data flow

### v0.6.0 - Cross-File Static Analysis

**Goals:**

- Workspace-level CFG construction across modules
- Import/export symbol tracking
- Cross-file goto definition and references
- Inconsistent export detection (**all** mismatches)
- Conflicting stub definitions across files

**Architecture:**

- Extend ImportDependencyTracker to track symbol-level dependencies
- Build workspace symbol table with module resolution
- Cross-file reachability analysis
- Transitive type propagation across module boundaries

**Requirements:**

- Goto definition works across files
- Find references works workspace-wide
- Cross-file diagnostics for import/export issues
- Performance benchmarks for multi-file analysis

### v0.7.0 - Advanced Data Flow

**Goals:**

- Taint analysis for security-sensitive data flow
- Null safety improvements beyond use-before-def
- Definite assignment analysis
- Constant propagation across scopes

**Requirements:**

- Taint analysis detects basic injection risks
- Null safety catches more patterns
- Performance remains acceptable on large codebases

## Linting & Formatting Polish (v0.8.0 - v0.9.0)

**Focus**: Expand linting rules, add autofix, polish formatter config

### v0.8.0 - Linting Expansion

**Goals:**

- Implement 10+ new lint rules (see TODO.md for candidates)
- Fix BEA022 (augmented assignments, nested scopes)
- Fix BEA023 (complex nested generic annotations)
- Autofix infrastructure for simple rules
- Per-rule configuration and severity

**New rules:**

- MutableDefaultArgument
- ReturnInFinally
- ForElseWithoutBreak
- UnnecessaryElse
- DuplicateExcept
- Others from TODO.md backlog

**Requirements:**

- 40+ working lint rules
- Autofix working for 10+ rules
- Per-rule enable/disable configuration
- Rule documentation

### v0.9.0 - Formatting Polish & Config

**Goals:**

- Additional PEP8 configuration options
- Format-on-save integration
- Format-on-paste support
- Configuration validation
- Compatibility mode improvements (Black, autopep8)

**Requirements:**

- Format-on-save working
- Config validation catches errors
- Documentation for all config options

## LSP Features & Multi-Root (v0.10.0 - v0.13.0)

**Focus**: Workspace support, snippets, auto-import, diagnostics publishing

### v0.10.0 - Multi-Root Workspace Support

**Goals:**

- Implement workspace/didChangeWorkspaceFolders
- Per-folder configuration overrides
- Multi-project symbol resolution
- Workspace-wide search and indexing

**Requirements:**

- Multi-root workspaces working
- Per-folder config tested
- Workspace symbol search across roots

### v0.11.0 - Snippet Engine

**Goals:**

- Context-aware snippet insertion based on scope and type context
- LSP textDocument/completion integration with snippet support
- Snippet registry with Python-specific templates
- Dynamic placeholder generation using type inference
- Multi-cursor snippet expansion

**Architecture:**

- Snippet definition format (JSON/YAML schema)
- Context matcher (scope type, surrounding code, imports)
- Placeholder resolver using HM type inference
- LSP CompletionItem with insertTextFormat=Snippet
- Snippet variable expansion ($0, ${1:default}, etc.)

**Built-in snippets:**

- Common patterns: if/elif/else, for/while loops, try/except, with statements
- Function/class templates with type annotations
- Dataclass/Protocol/TypedDict templates
- Common decorators (@property, @classmethod, @staticmethod)
- Type guard patterns, match statement templates

**Requirements:**

- Snippet registry working with 20+ built-in snippets
- Context-aware filtering based on scope
- Type-aware placeholder generation
- LSP integration tested with VS Code/Neovim
- User-defined snippet support
- Documentation and examples

### v0.12.0 - Auto-Import & Code Actions

**Goals:**

- Auto-import for undefined symbols
- Import management (add, remove, organize)
- Quick fixes for common errors
- Refactoring actions (extract function, rename)

**Requirements:**

- Auto-import suggests correct imports
- Import organization working
- Quick fixes for type errors, lint warnings
- Code action tests passing

### v0.13.0 - Diagnostics Publishing Baseline

**Goals:**

- textDocument/publishDiagnostics implementation
- Config-driven severity toggles
- Diagnostic categories (syntax, type, lint)
- Performance optimization for rapid diagnostics

**Requirements:**

- Diagnostics publishing working
- Severity configuration tested
- Performance acceptable on large files

## Monorepo Performance & Scale (v0.14.0 - v0.15.0)

**Focus**: Handle Django-scale projects (100k+ LOC) with acceptable performance

### v0.14.0 - Monorepo Performance - Caching

**Goals:**

- Workspace-aware cache coordination
- Parallelization for multi-file analysis
- Incremental re-analysis optimization
- Cache persistence across sessions
- LRU eviction tuning for large projects

**Architecture:**

- SCC slicing for constraint solving
- Disk cache for analysis results
- Parallel analysis with work-stealing
- Cache statistics and monitoring

**Requirements:**

- 10k+ file workspace analysis completes in <30s (cold)
- Incremental re-analysis <1s for single-file changes
- Cache hit rate >80% for typical workflows
- Memory usage reasonable for large projects

### v0.15.0 - Monorepo Performance - Scale Testing

**Goals:**

- Performance benchmarks on real Django projects
- Stress testing with 100k+ LOC codebases
- Memory profiling and optimization
- CPU profiling and hotspot fixes
- Scalability documentation

**Benchmarks:**

- Django (large real-world project)
- Synthetic 100k LOC test case
- Rapid edit scenario (type while analyzing)
- Workspace-wide refactoring

**Requirements:**

- Benchmarks pass performance targets
- Memory usage <2GB for 100k LOC project
- Documentation of scale limits
- Performance regression tests in CI

## Production Hardening (v0.16.0 - v0.18.0)

**Focus**: Telemetry, stability, QA

### v0.16.0 - Telemetry & Observability

**Goals:**

- Performance metrics collection
- Cache hit/miss rates
- Analysis time breakdowns
- Error rate tracking
- Opt-in telemetry for diagnostics

**Requirements:**

- Telemetry infrastructure working
- Metrics exposed via logging
- Privacy-respecting implementation
- Documentation

### v0.17.0 - Production Hardening

**Goals:**

- Stress testing and stability improvements
- Memory leak detection and fixes
- Crash recovery and error handling
- Fuzzing for parser and analyzer
- Security audit

**Requirements:**

- 24h stress test passes
- Zero memory leaks detected
- Crash recovery tested
- Fuzzing integrated in CI

### v0.18.0 - Beta Stabilization

**Goals:**

- Full QA pass on all features
- Regression test suite completion
- Bug fix sprint
- Documentation review
- User feedback collection

**Requirements:**

- All major features QA'd
- Zero critical bugs
- Documentation complete
- User feedback addressed

## Release Candidate (v0.19.0 - v1.0.0)

**Focus**: Release candidate, docs, migration, GA

### v0.19.0 - Release Candidate

**Goals:**

- Feature freeze
- Migration guide for users
- Sample configurations
- Breaking change documentation
- Limited external feedback

**Requirements:**

- No new features
- Migration guide complete
- Sample configs provided
- External feedback collected

### v1.0.0 - General Availability

**Goals:**

- Finalize documentation
- Publish stable tag
- Announcement and marketing
- Support channels established

**Requirements:**

- All blockers closed
- Docs published
- Tag pushed
- Announcement made

## Post-1.0 Backlog

- **Advanced Formatter Features**: Additional PEP8 options, custom formatting rules

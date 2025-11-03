//! Workspace management for multi-file analysis
//!
//! Handles:
//! - Dependency graph between modules
//! - Module resolution and imports
//! - Project-wide type checking

use crate::config::Config;
use crate::document::DocumentManager;

use beacon_parser::AstNode;
use rayon::prelude::*;
use rustc_hash::{FxHashMap, FxHashSet};
use std::collections::VecDeque;
use std::path::{Path, PathBuf};
use std::sync::{Arc, RwLock};
use url::Url;

/// Embedded Python stdlib stub files
const BUILTINS_STUB: &str = include_str!("../../../stubs/builtins.pyi");
const TYPING_STUB: &str = include_str!("../../../stubs/typing.pyi");
const DATACLASSES_STUB: &str = include_str!("../../../stubs/dataclasses.pyi");
const OS_STUB: &str = include_str!("../../../stubs/os.pyi");
const ENUM_STUB: &str = include_str!("../../../stubs/enum.pyi");
const PATHLIB_STUB: &str = include_str!("../../../stubs/pathlib.pyi");

/// Information about a Python module in the workspace
#[derive(Debug, Clone)]
pub struct ModuleInfo {
    /// File URI for this module
    pub uri: Url,
    /// Python module name (e.g., "foo.bar")
    pub module_name: String,
    /// Source root this module belongs to
    pub source_root: PathBuf,
    /// Module names this module imports (not yet resolved to URIs)
    pub dependencies: FxHashSet<String>,
    /// Whether this is a package (has __init__.py or is a directory)
    pub is_package: bool,
}

impl ModuleInfo {
    pub fn new(uri: Url, module_name: String, source_root: PathBuf, is_package: bool) -> Self {
        Self { uri, module_name, source_root, dependencies: FxHashSet::default(), is_package }
    }
}

/// Index of all Python modules in the workspace
#[derive(Debug)]
pub struct WorkspaceIndex {
    /// All modules indexed by URI
    modules: FxHashMap<Url, ModuleInfo>,
    /// Reverse lookup: module name to URI
    module_by_name: FxHashMap<String, Url>,
}

impl WorkspaceIndex {
    fn new() -> Self {
        Self { modules: FxHashMap::default(), module_by_name: FxHashMap::default() }
    }

    fn insert(&mut self, info: ModuleInfo) {
        self.module_by_name.insert(info.module_name.clone(), info.uri.clone());
        self.modules.insert(info.uri.clone(), info);
    }

    fn _get(&self, uri: &Url) -> Option<&ModuleInfo> {
        self.modules.get(uri)
    }

    fn get_by_name(&self, name: &str) -> Option<&ModuleInfo> {
        self.module_by_name.get(name).and_then(|uri| self.modules.get(uri))
    }

    fn _remove(&mut self, uri: &Url) -> Option<ModuleInfo> {
        if let Some(info) = self.modules.remove(uri) {
            self.module_by_name.remove(&info.module_name);
            Some(info)
        } else {
            None
        }
    }

    fn _contains(&self, uri: &Url) -> bool {
        self.modules.contains_key(uri)
    }

    fn iter(&self) -> impl Iterator<Item = (&Url, &ModuleInfo)> {
        self.modules.iter()
    }

    /// Get all modules as a vec of (URI, module name) pairs
    pub fn all_modules(&self) -> Vec<(Url, String)> {
        self.iter()
            .map(|(uri, info)| (uri.clone(), info.module_name.clone()))
            .collect()
    }
}

/// Workspace manager for coordinating multi-file analysis
pub struct Workspace {
    /// Root URI of the workspace
    pub root_uri: Option<Url>,
    /// Configuration
    config: Config,
    /// Document manager
    documents: DocumentManager,
    /// Index of all workspace modules
    index: WorkspaceIndex,
    /// Module dependency graph
    dependency_graph: DependencyGraph,
    /// Loaded stub files (shared across workspace and analyzer)
    stubs: Arc<RwLock<StubCache>>,
    /// Tracks which modules have been analyzed and their versions
    analyzed_modules: FxHashMap<Url, i32>,
}

impl Workspace {
    /// Create a new workspace
    pub fn new(root_uri: Option<Url>, config: Config, documents: DocumentManager) -> Self {
        Self {
            root_uri,
            config,
            documents,
            index: WorkspaceIndex::new(),
            dependency_graph: DependencyGraph::new(),
            stubs: Arc::new(RwLock::new(StubCache::new())),
            analyzed_modules: FxHashMap::default(),
        }
    }

    /// Get a reference to the stub cache for sharing with analyzer
    pub fn stub_cache(&self) -> Arc<RwLock<StubCache>> {
        Arc::clone(&self.stubs)
    }

    /// Initialize workspace by discovering Python files and stubs
    ///
    /// Scans the workspace for all Python files, builds the module index, constructs the initial dependency graph, and discovers stub files following PEP 561.
    /// Stubs are always loaded even if file discovery fails, since stdlib stubs are essential for type checking.
    pub fn initialize(&mut self) -> Result<(), WorkspaceError> {
        tracing::info!("Starting workspace initialization & discovering stubs");
        self.discover_stubs();

        tracing::debug!("Discovering Python files");
        self.discover_files()?;

        tracing::debug!("Building dependency graph");
        self.build_dependency_graph();

        tracing::info!(
            "Workspace initialization complete. Index contains {} modules",
            self.index.modules.len()
        );
        Ok(())
    }

    /// Build the dependency graph by extracting imports from all indexed modules
    ///
    /// Uses [rayon] for parallel processing of files.
    fn build_dependency_graph(&mut self) {
        let uris: Vec<Url> = self.index.modules.keys().cloned().collect();
        let imports: Vec<(Url, Vec<String>)> = uris
            .par_iter()
            .filter_map(|uri| {
                let module_imports = self.extract_imports(uri)?;
                Some((uri.clone(), module_imports))
            })
            .collect();

        for (from_uri, import_names) in imports {
            let from_module = self.uri_to_module_name(&from_uri).unwrap_or_default();

            for import_name in import_names {
                let resolved = if import_name.starts_with('.') {
                    let leading_dots = import_name.chars().take_while(|&c| c == '.').count();
                    let rest = &import_name[leading_dots..];
                    self.resolve_relative_import(&from_module, rest, leading_dots)
                } else {
                    self.resolve_import(&import_name)
                };

                if let Some(to_uri) = resolved {
                    self.dependency_graph.add_edge(&from_uri, &to_uri);
                }
            }
        }
    }

    /// Extract import statements from a Python file
    ///
    /// Returns a list of module names that this file imports.
    /// Relative imports are returned with their leading dots (e.g., "..foo", ".bar")
    fn extract_imports(&self, uri: &Url) -> Option<Vec<String>> {
        let text = self.documents.get_document(uri, |doc| doc.text())?;
        let mut parser = crate::parser::LspParser::new().ok()?;
        let parse_result = parser.parse(&text).ok()?;
        let mut imports = Vec::new();

        Self::collect_imports_from_ast(&parse_result.ast, &mut imports);

        Some(imports)
    }

    /// Recursively collect imports from an AST node
    fn collect_imports_from_ast(node: &beacon_parser::AstNode, imports: &mut Vec<String>) {
        match node {
            AstNode::Module { body, .. } => {
                for stmt in body {
                    Self::collect_imports_from_ast(stmt, imports);
                }
            }
            AstNode::Import { module, .. } => imports.push(module.clone()),
            AstNode::ImportFrom { module, .. } => imports.push(module.clone()),
            AstNode::FunctionDef { body, .. } => {
                for stmt in body {
                    Self::collect_imports_from_ast(stmt, imports);
                }
            }
            AstNode::ClassDef { body, .. } => {
                for stmt in body {
                    Self::collect_imports_from_ast(stmt, imports);
                }
            }
            AstNode::If { body, elif_parts, else_body, .. } => {
                for stmt in body {
                    Self::collect_imports_from_ast(stmt, imports);
                }
                for (_test, elif_body) in elif_parts {
                    for stmt in elif_body {
                        Self::collect_imports_from_ast(stmt, imports);
                    }
                }
                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        Self::collect_imports_from_ast(stmt, imports);
                    }
                }
            }
            AstNode::For { body, else_body, .. } | AstNode::While { body, else_body, .. } => {
                for stmt in body {
                    Self::collect_imports_from_ast(stmt, imports);
                }
                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        Self::collect_imports_from_ast(stmt, imports);
                    }
                }
            }
            AstNode::Try { body, handlers, else_body, finally_body, .. } => {
                for stmt in body {
                    Self::collect_imports_from_ast(stmt, imports);
                }
                for handler in handlers {
                    for stmt in &handler.body {
                        Self::collect_imports_from_ast(stmt, imports);
                    }
                }
                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        Self::collect_imports_from_ast(stmt, imports);
                    }
                }
                if let Some(finally_stmts) = finally_body {
                    for stmt in finally_stmts {
                        Self::collect_imports_from_ast(stmt, imports);
                    }
                }
            }
            AstNode::With { body, .. } => {
                for stmt in body {
                    Self::collect_imports_from_ast(stmt, imports);
                }
            }
            _ => {}
        }
    }

    /// Discover all Python files in the workspace
    ///
    /// Uses [ignore] crate to walk the workspace directory tree, respecting .gitignore files and excluding common virtual environment patterns.
    /// TODO: Make overrides configurable via [Config::exclude_patterns]
    fn discover_files(&mut self) -> Result<(), WorkspaceError> {
        tracing::debug!("discover_files: Starting file discovery");

        let root_path = match &self.root_uri {
            Some(uri) if uri.scheme() == "file" => {
                let path = PathBuf::from(uri.path());
                tracing::debug!("discover_files: Root URI is {}, path is {}", uri, path.display());
                path
            }
            _ => {
                tracing::debug!("discover_files: No file:// root URI, skipping file discovery");
                return Ok(());
            }
        };

        if !root_path.exists() || !root_path.is_dir() {
            tracing::error!(
                "discover_files: Root path does not exist or is not a directory: {}",
                root_path.display()
            );
            return Err(WorkspaceError::InvalidRoot(format!(
                "Workspace root does not exist or is not a directory: {}",
                root_path.display()
            )));
        }

        let mut builder = ignore::WalkBuilder::new(&root_path);
        builder
            .hidden(false)
            .git_ignore(true)
            .git_global(true)
            .git_exclude(true);

        let mut overrides_builder = ignore::overrides::OverrideBuilder::new(&root_path);
        let venv_patterns = vec![
            "!venv/",
            "!.venv/",
            "!env/",
            "!.env/",
            "!virtualenv/",
            "!__pycache__/",
            "!*.pyc",
            "!.tox/",
            "!.nox/",
            "!.pytest_cache/",
            "!.mypy_cache/",
            "!.ruff_cache/",
        ];

        for pattern in venv_patterns {
            if let Err(e) = overrides_builder.add(pattern) {
                eprintln!("Warning: Failed to add exclude pattern {pattern}: {e}");
            }
        }

        if let Ok(overrides) = overrides_builder.build() {
            builder.overrides(overrides);
        }

        let walker = builder.build();
        let python_files: Vec<PathBuf> = walker
            .filter_map(|entry| entry.ok())
            .filter(|entry| {
                entry.file_type().is_some_and(|ft| ft.is_file())
                    && entry.path().extension().and_then(|s| s.to_str()) == Some("py")
            })
            .map(|entry| entry.path().to_path_buf())
            .collect();

        for file_path in python_files {
            if let Ok(uri) = Url::from_file_path(&file_path) {
                if let Some(module_name) = self.uri_to_module_name(&uri) {
                    let source_root = self.find_source_root_for_file(&file_path);
                    let is_package = file_path.file_name().and_then(|n| n.to_str()) == Some("__init__.py");
                    let info = ModuleInfo::new(uri, module_name, source_root, is_package);
                    self.index.insert(info);
                }
            }
        }

        Ok(())
    }

    fn find_source_root_for_file(&self, file_path: &Path) -> PathBuf {
        let source_roots = self.get_source_roots();
        for root in source_roots {
            if file_path.starts_with(&root) {
                return root;
            }
        }

        self.root_uri
            .as_ref()
            .and_then(
                |uri| {
                    if uri.scheme() == "file" { Some(PathBuf::from(uri.path())) } else { None }
                },
            )
            .unwrap_or_default()
    }

    /// Get the module name for a URI
    ///
    /// Converts a file URI to a Python module name by finding the appropriate source root.
    /// Examples:
    /// - `/workspace/src/foo/bar.py` → `foo.bar`
    /// - `/workspace/foo/__init__.py` → `foo`
    /// - `/workspace/lib/pkg/mod.py` → `pkg.mod`
    pub fn uri_to_module_name(&self, uri: &Url) -> Option<String> {
        if uri.scheme() != "file" {
            return None;
        }

        let file_path = PathBuf::from(uri.path());

        if file_path.extension()? != "py" {
            return None;
        }

        let source_roots = self.get_source_roots();
        for root in source_roots {
            if let Ok(rel_path) = file_path.strip_prefix(&root) {
                return self.path_to_module_name(rel_path);
            }
        }

        None
    }

    /// Convert a relative path to a module name
    ///
    /// Examples:
    /// - `foo/bar.py` → `foo.bar`
    /// - `foo/__init__.py` → `foo`
    /// - `baz.py` → `baz`
    fn path_to_module_name(&self, rel_path: &Path) -> Option<String> {
        let mut components: Vec<_> = rel_path.components().filter_map(|c| c.as_os_str().to_str()).collect();

        if components.is_empty() {
            return None;
        }

        let last_idx = components.len() - 1;
        let last = components[last_idx];
        if let Some(stripped) = last.strip_suffix(".py") {
            components[last_idx] = stripped;
        } else {
            return None;
        }

        if components[last_idx] == "__init__" {
            components.pop();
            if components.is_empty() {
                return None;
            }
        }

        Some(components.join("."))
    }

    /// Get all source roots to search for modules
    ///
    /// Auto-detects workspace root, src/, and lib/ as per user requirements.
    /// TODO: Make source_roots configurable via [Config]
    fn get_source_roots(&self) -> Vec<PathBuf> {
        let mut roots = Vec::new();

        roots.extend(self.config.source_roots.clone());

        if let Some(root_uri) = &self.root_uri {
            if root_uri.scheme() == "file" {
                let root_path = PathBuf::from(root_uri.path());
                let src_path = root_path.join("src");
                if src_path.is_dir() {
                    roots.push(src_path);
                }

                let lib_path = root_path.join("lib");
                if lib_path.is_dir() {
                    roots.push(lib_path);
                }

                roots.push(root_path);
            }
        }

        roots
    }

    /// Resolve a module import to a file URI
    ///
    /// Resolves an absolute import (e.g., "foo.bar") to a file URI.
    /// For relative imports, use [Self::resolve_relative_import] instead.
    ///
    /// Searches:
    /// 1. Workspace index first (fast path)
    /// 2. File system across all source roots
    ///
    /// Tries both module.py and module/__init__.py
    pub fn resolve_import(&self, module_name: &str) -> Option<Url> {
        if let Some(info) = self.index.get_by_name(module_name) {
            Some(info.uri.clone())
        } else {
            self.resolve_import_from_filesystem(module_name)
        }
    }

    /// Resolve a relative import from a given module
    ///
    /// Handles imports like:
    /// - `from . import foo` (same package)
    /// - `from .. import bar` (parent package)
    /// - `from ..sibling import baz` (sibling package)
    ///
    /// `from_module`: the module doing the importing (e.g., "pkg.sub.mod")
    /// `relative_import`: the import statement (e.g., "..sibling.utils")
    /// `leading_dots`: number of leading dots (1 for ., 2 for .., etc.)
    pub fn resolve_relative_import(
        &self, from_module: &str, relative_import: &str, leading_dots: usize,
    ) -> Option<Url> {
        if leading_dots == 0 {
            return self.resolve_import(relative_import);
        }

        let mut parts: Vec<&str> = from_module.split('.').collect();

        for _ in 0..leading_dots {
            if parts.is_empty() {
                return None;
            }
            parts.pop();
        }

        let mut absolute_module = parts.join(".");
        if !relative_import.is_empty() {
            if !absolute_module.is_empty() {
                absolute_module.push('.');
            }
            absolute_module.push_str(relative_import);
        }

        self.resolve_import(&absolute_module)
    }

    /// Resolve import by searching the filesystem across all source roots
    fn resolve_import_from_filesystem(&self, module_name: &str) -> Option<Url> {
        let module_path = module_name.replace('.', "/");
        let source_roots = self.get_source_roots();

        for root in source_roots {
            let module_file = root.join(format!("{module_path}.py"));
            if module_file.exists() {
                return Url::from_file_path(module_file).ok();
            }

            let package_init = root.join(&module_path).join("__init__.py");
            if package_init.exists() {
                return Url::from_file_path(package_init).ok();
            }
        }

        None
    }

    /// Update dependency graph when a document changes by reextracting imports from the changed document and updates the dependency graph.
    ///
    /// Returns the list of dependents that may need re-analysis.
    pub fn update_dependencies(&mut self, uri: &Url) -> Vec<Url> {
        self.dependency_graph.rm_edges(uri);

        let import_names = self.extract_imports(uri).unwrap_or_default();
        let from_module = self.uri_to_module_name(uri).unwrap_or_default();

        for import_name in import_names {
            let resolved = if import_name.starts_with('.') {
                let leading_dots = import_name.chars().take_while(|&c| c == '.').count();
                let rest = &import_name[leading_dots..];
                self.resolve_relative_import(&from_module, rest, leading_dots)
            } else {
                self.resolve_import(&import_name)
            };

            if let Some(to_uri) = resolved {
                self.dependency_graph.add_edge(uri, &to_uri);
            }
        }

        self.get_dependents(uri)
    }

    /// Get all documents that depend on a given document
    ///
    /// Returns URIs of modules that import this module.
    pub fn get_dependents(&self, uri: &Url) -> Vec<Url> {
        self.dependency_graph
            .get_dependents(uri)
            .map(|deps| deps.iter().cloned().collect())
            .unwrap_or_default()
    }

    /// Get all documents that a given document depends on
    ///
    /// Returns URIs of modules that this module imports.
    pub fn get_dependencies(&self, uri: &Url) -> Vec<Url> {
        self.dependency_graph
            .get_dependencies(uri)
            .map(|deps| deps.iter().cloned().collect())
            .unwrap_or_default()
    }

    /// Get the topologically sorted order for analysis
    ///
    /// Returns SCCs in topological order for incremental analysis.
    /// Each inner Vec is either a single module or a group of circularly dependent modules.
    pub fn analysis_order(&self) -> Vec<Vec<Url>> {
        self.dependency_graph.analysis_order()
    }

    /// Get circular dependencies (SCCs with size > 1)
    ///
    /// Returns a list of circular dependency groups for diagnostic reporting.
    pub fn circular_dependencies(&self) -> Vec<Vec<Url>> {
        self.dependency_graph
            .compute_sccs()
            .into_iter()
            .filter(|scc| scc.len() > 1)
            .collect()
    }

    /// Get unresolved imports for a document
    ///
    /// Returns a list of module names that could not be resolved to workspace files.
    pub fn unresolved_imports(&self, uri: &Url) -> Vec<String> {
        let imports = self.extract_imports(uri).unwrap_or_default();
        let from_module = self.uri_to_module_name(uri).unwrap_or_default();

        imports
            .into_iter()
            .filter(|import_name| {
                let resolved = if import_name.starts_with('.') {
                    let leading_dots = import_name.chars().take_while(|&c| c == '.').count();
                    let rest = &import_name[leading_dots..];
                    self.resolve_relative_import(&from_module, rest, leading_dots)
                } else {
                    self.resolve_import(import_name)
                };
                resolved.is_none()
            })
            .collect()
    }

    /// Invalidate analysis cache for a document and all its dependents
    ///
    /// Returns the list of URIs that need to be re-analyzed (includes the original URI).
    /// URIs are returned in dependency order (leaf dependencies first).
    pub fn invalidate_dependents(&mut self, uri: &Url) -> Vec<Url> {
        let mut to_invalidate = FxHashSet::default();
        let mut queue = VecDeque::new();
        queue.push_back(uri.clone());
        to_invalidate.insert(uri.clone());

        while let Some(current) = queue.pop_front() {
            self.analyzed_modules.remove(&current);

            if let Some(deps) = self.dependency_graph.get_dependents(&current) {
                for dep in deps {
                    if !to_invalidate.contains(dep) {
                        to_invalidate.insert(dep.clone());
                        queue.push_back(dep.clone());
                    }
                }
            }
        }

        let mut invalidated: Vec<Url> = to_invalidate.into_iter().collect();
        invalidated.sort_by_cached_key(|u| u.to_string());
        invalidated
    }

    /// Get a list of modules that need re-analysis in dependency order
    ///
    /// Returns URIs sorted so that dependencies are analyzed before dependents.
    /// Only includes modules that have been previously analyzed but are now invalidated.
    pub fn modules_to_reanalyze(&self, invalidated: &[Url]) -> Vec<Url> {
        let invalidated_set: FxHashSet<_> = invalidated.iter().cloned().collect();
        let full_order = self.analysis_order();

        let mut result = Vec::new();
        for group in full_order {
            for uri in group {
                if invalidated_set.contains(&uri) {
                    result.push(uri);
                }
            }
        }

        result
    }

    /// Mark a module as analyzed with the given version
    pub fn mark_analyzed(&mut self, uri: &Url, version: i32) {
        self.analyzed_modules.insert(uri.clone(), version);
    }

    /// Check if a module has been analyzed
    pub fn is_analyzed(&self, uri: &Url) -> bool {
        self.analyzed_modules.contains_key(uri)
    }

    /// Load a workspace file that is not currently open
    ///
    /// Reads the file from disk, parses it, and returns the parse result without opening it in the document manager.
    /// This is useful for analyzing workspace files on-demand.
    pub fn load_workspace_file(&self, uri: &Url) -> Option<crate::parser::ParseResult> {
        if uri.scheme() != "file" {
            return None;
        }

        let path = std::path::PathBuf::from(uri.path());
        let text = std::fs::read_to_string(path).ok()?;

        let mut parser = crate::parser::LspParser::new().ok()?;
        parser.parse(&text).ok()
    }

    /// Re-analyze affected modules in dependency order
    ///
    /// Takes a list of invalidated URIs and re-analyzes them in the correct order and skips unchanged files to avoid unnecessary work.
    /// Returns the number of modules successfully analyzed.
    pub fn reanalyze_affected(&mut self, invalidated: &[Url], analyzer: &mut crate::analysis::Analyzer) -> usize {
        let to_analyze = self.modules_to_reanalyze(invalidated);
        let mut analyzed_count = 0;

        for uri in to_analyze {
            let version = self.documents.get_document(&uri, |doc| doc.version).unwrap_or(0);

            if let Ok(_result) = analyzer.analyze(&uri) {
                self.mark_analyzed(&uri, version);
                analyzed_count += 1;
            }
        }

        analyzed_count
    }

    /// Load stub file for a module following PEP 561 resolution order
    ///
    /// Resolution order:
    /// 1. Manual stubs in config.stub_paths
    /// 2. Stub packages (*-stubs)
    /// 3. Inline stubs (py.typed packages)
    /// 4. Typeshed (TODO)
    pub fn load_stub(&self, module_name: &str) -> Option<StubFile> {
        if let Some(stub) = self.find_manual_stub(module_name) {
            return Some(stub);
        }

        if let Some(stub) = self.find_stub_package(module_name) {
            return Some(stub);
        }

        if let Some(stub) = self.find_inline_stub(module_name) {
            return Some(stub);
        }

        None
    }

    /// Find stub in manual stub paths (config.stub_paths)
    fn find_manual_stub(&self, module_name: &str) -> Option<StubFile> {
        for stub_path in &self.config.stub_paths {
            if let Some(stub) = self.find_stub_in_directory(stub_path, module_name) {
                return Some(stub);
            }
        }
        None
    }

    /// Find stub package (*-stubs) for a module
    fn find_stub_package(&self, module_name: &str) -> Option<StubFile> {
        let root_path = self.root_uri.as_ref()?.to_file_path().ok()?;
        let top_level_package = module_name.split('.').next()?;
        let stub_package_name = format!("{top_level_package}-stubs");

        let stub_package_path = root_path.join(&stub_package_name);
        if stub_package_path.exists() && stub_package_path.is_dir() {
            return self.find_stub_in_directory(&stub_package_path, module_name);
        }

        None
    }

    /// Find inline stub (py.typed package)
    fn find_inline_stub(&self, module_name: &str) -> Option<StubFile> {
        let module_info = self.index.get_by_name(module_name)?;
        let module_path = PathBuf::from(module_info.uri.path());

        let pyi_path = module_path.with_extension("pyi");
        if pyi_path.exists() {
            let is_partial = self.check_if_partial_stub(&module_info.source_root);
            return Some(StubFile {
                module: module_name.to_string(),
                path: pyi_path,
                exports: FxHashMap::default(),
                is_partial,
                reexports: Vec::new(),
                all_exports: None,
                content: None,
            });
        }

        None
    }

    /// Find stub file in a directory for a given module name
    fn find_stub_in_directory(&self, directory: &Path, module_name: &str) -> Option<StubFile> {
        let parts: Vec<&str> = module_name.split('.').collect();
        let mut path = directory.to_path_buf();
        for part in &parts[..parts.len() - 1] {
            path = path.join(part);
        }
        path = path.join(format!("{}.pyi", parts.last()?));

        if path.exists() {
            let is_partial = self.check_if_partial_stub(directory);
            return Some(StubFile {
                module: module_name.to_string(),
                path,
                exports: FxHashMap::default(),
                is_partial,
                reexports: Vec::new(),
                all_exports: None,
                content: None,
            });
        }

        let mut path = directory.to_path_buf();
        for part in &parts {
            path = path.join(part);
        }
        path = path.join("__init__.pyi");

        if path.exists() {
            let is_partial = self.check_if_partial_stub(directory);
            return Some(StubFile {
                module: module_name.to_string(),
                path,
                exports: FxHashMap::default(),
                is_partial,
                reexports: Vec::new(),
                all_exports: None,
                content: None,
            });
        }

        None
    }

    /// Check if a stub directory contains py.typed with "partial" marker
    fn check_if_partial_stub(&self, directory: &Path) -> bool {
        let py_typed_path = directory.join("py.typed");
        if let Ok(contents) = std::fs::read_to_string(&py_typed_path) {
            contents.trim() == "partial"
        } else {
            false
        }
    }

    /// Discover all stub files in workspace and populate stub cache during workspace initialization
    pub fn discover_stubs(&mut self) {
        tracing::debug!("discover_stubs: Loading builtin stubs");
        self.load_builtin_stubs();

        tracing::debug!("discover_stubs: Scanning {} stub paths", self.config.stub_paths.len());
        for stub_path in self.config.stub_paths.clone() {
            self.discover_stubs_in_directory(&stub_path);
        }

        if let Some(root_uri) = &self.root_uri {
            if let Ok(root_path) = root_uri.to_file_path() {
                tracing::debug!("discover_stubs: Scanning for stub packages in {}", root_path.display());
                self.discover_stub_packages(&root_path);
            }
        } else {
            tracing::debug!("discover_stubs: No root_uri, skipping stub package discovery");
        }

        let stub_count = self.stubs.read().ok().map(|s| s.cache.len()).unwrap_or(0);
        tracing::info!("discover_stubs: Completed. Loaded {} stub modules", stub_count);
    }

    /// Load built-in stubs from embedded stdlib stub files
    ///
    /// Pre-loads core Python stdlib types that are always available. Includes builtins, typing, dataclasses, os, enum, and pathlib.
    /// Registers these modules in the workspace index so they can be resolved during import resolution.
    fn load_builtin_stubs(&mut self) {
        let stdlib_stubs = vec![
            ("builtins", BUILTINS_STUB),
            ("typing", TYPING_STUB),
            ("dataclasses", DATACLASSES_STUB),
            ("os", OS_STUB),
            ("enum", ENUM_STUB),
            ("pathlib", PATHLIB_STUB),
        ];

        let stub_count = stdlib_stubs.len();
        tracing::debug!("Loading {} stdlib modules", stub_count);

        for (module_name, stub_content) in stdlib_stubs {
            match self.parse_stub_from_string(module_name, stub_content) {
                Ok(stub) => {
                    tracing::debug!(
                        "Parsed stdlib module '{}' ({} exports)",
                        module_name,
                        stub.exports.len()
                    );

                    if let Ok(mut cache) = self.stubs.write() {
                        cache.insert(module_name.to_string(), stub);
                    }

                    if let Ok(uri) = Url::parse(&format!("builtin://{module_name}")) {
                        let module_info =
                            ModuleInfo::new(uri.clone(), module_name.to_string(), PathBuf::from("<builtin>"), false);
                        self.index.insert(module_info);
                        tracing::debug!("Registered stdlib module '{}' at {}", module_name, uri);
                    }
                }
                Err(e) => {
                    tracing::error!("Failed to parse stdlib module '{}': {:?}", module_name, e);
                }
            }
        }

        tracing::info!("Loaded {} stdlib stub modules", stub_count);
    }

    /// Parse a stub file from string content (used for embedded stdlib stubs)
    fn parse_stub_from_string(&self, module_name: &str, content: &str) -> Result<StubFile, WorkspaceError> {
        let mut parser = crate::parser::LspParser::new()
            .map_err(|e| WorkspaceError::StubLoadFailed(format!("Failed to create parser: {e:?}")))?;

        let parse_result = parser
            .parse(content)
            .map_err(|e| WorkspaceError::StubLoadFailed(format!("Failed to parse stub {module_name}: {e:?}")))?;

        let mut exports = FxHashMap::default();
        let mut reexports = Vec::new();
        let mut all_exports = None;

        self.extract_stub_signatures(&parse_result.ast, &mut exports, &mut reexports, &mut all_exports);

        Ok(StubFile {
            module: module_name.to_string(),
            path: PathBuf::from(format!("<embedded>/{module_name}.pyi")),
            exports,
            reexports,
            all_exports,
            is_partial: false,
            content: Some(content.to_string()),
        })
    }

    /// Discover all .pyi files in a directory
    fn discover_stubs_in_directory(&mut self, directory: &Path) {
        if !directory.exists() || !directory.is_dir() {
            return;
        }

        let walker = ignore::WalkBuilder::new(directory).hidden(false).build();

        for entry in walker.filter_map(|e| e.ok()) {
            if entry.file_type().is_some_and(|ft| ft.is_file()) {
                if let Some(ext) = entry.path().extension() {
                    if ext == "pyi" {
                        if let Some(module_name) = self.path_to_module_name_from_base(entry.path(), directory) {
                            let is_partial = self.check_if_partial_stub(directory);
                            let stub = StubFile {
                                module: module_name.clone(),
                                path: entry.path().to_path_buf(),
                                exports: FxHashMap::default(),
                                is_partial,
                                reexports: Vec::new(),
                                all_exports: None,
                                content: None,
                            };

                            if let Ok(mut cache) = self.stubs.write() {
                                cache.insert(module_name.clone(), stub);
                            }

                            if let Ok(uri) = Url::from_file_path(entry.path()) {
                                let module_info =
                                    ModuleInfo::new(uri.clone(), module_name.clone(), directory.to_path_buf(), false);
                                self.index.insert(module_info);
                                tracing::debug!("Registered stub module '{}' at {}", module_name, uri);
                            }
                        }
                    }
                }
            }
        }
    }

    /// Discover stub packages (*-stubs) in a directory
    fn discover_stub_packages(&mut self, directory: &Path) {
        if !directory.exists() || !directory.is_dir() {
            return;
        }

        if let Ok(entries) = std::fs::read_dir(directory) {
            for entry in entries.filter_map(|e| e.ok()) {
                if let Ok(file_type) = entry.file_type() {
                    if file_type.is_dir() {
                        if let Some(name) = entry.file_name().to_str() {
                            if name.ends_with("-stubs") {
                                self.discover_stubs_in_directory(&entry.path());
                            }
                        }
                    }
                }
            }
        }
    }

    /// Convert a file path to a module name relative to a base directory
    fn path_to_module_name_from_base(&self, path: &Path, base: &Path) -> Option<String> {
        let relative = path.strip_prefix(base).ok()?;
        let relative_str = relative.to_str()?;
        let without_ext = relative_str.strip_suffix(".pyi")?;
        let module_name = without_ext.replace(std::path::MAIN_SEPARATOR, ".");

        Some(if module_name.ends_with(".__init__") {
            module_name.strip_suffix(".__init__").unwrap().to_string()
        } else {
            module_name
        })
    }

    /// Parse a stub file and extract type signatures
    pub fn parse_stub_file(&self, stub_path: &Path) -> Result<StubFile, WorkspaceError> {
        let content = std::fs::read_to_string(stub_path)
            .map_err(|e| WorkspaceError::StubLoadFailed(format!("Failed to read stub file: {e}")))?;

        let mut parser = crate::parser::LspParser::new()
            .map_err(|e| WorkspaceError::StubLoadFailed(format!("Failed to create parser: {e:?}")))?;

        let parse_result = parser
            .parse(&content)
            .map_err(|e| WorkspaceError::StubLoadFailed(format!("Failed to parse stub: {e:?}")))?;

        let mut exports = FxHashMap::default();
        let mut reexports = Vec::new();
        let mut all_exports = None;

        self.extract_stub_signatures(&parse_result.ast, &mut exports, &mut reexports, &mut all_exports);

        let module_name = self
            .path_to_module_name(stub_path)
            .unwrap_or_else(|| "unknown".to_string());
        let is_partial = stub_path
            .parent()
            .map(|p| self.check_if_partial_stub(p))
            .unwrap_or(false);

        Ok(StubFile {
            module: module_name,
            path: stub_path.to_path_buf(),
            exports,
            is_partial,
            reexports,
            all_exports,
            content: None,
        })
    }

    /// Extract class metadata from stub AST nodes
    ///
    /// Returns a map of class names to their metadata for registration in ClassRegistry.
    pub fn extract_stub_classes(&self, node: &beacon_parser::AstNode) -> FxHashMap<String, beacon_core::ClassMetadata> {
        let mut classes = FxHashMap::default();
        self.extract_classes_recursive(node, &mut classes);
        classes
    }

    fn extract_classes_recursive(
        &self, node: &beacon_parser::AstNode, classes: &mut FxHashMap<String, beacon_core::ClassMetadata>,
    ) {
        match node {
            AstNode::Module { body, .. } => {
                for stmt in body {
                    self.extract_classes_recursive(stmt, classes);
                }
            }
            AstNode::ClassDef { name, body, .. } => {
                let mut metadata = beacon_core::ClassMetadata::new(name.clone());

                for stmt in body {
                    if let AstNode::FunctionDef { name: method_name, args: params, return_type, .. } = stmt {
                        let param_types: Vec<beacon_core::Type> = params
                            .iter()
                            .filter_map(|p| p.type_annotation.as_ref())
                            .filter_map(|ann| self.parse_annotation_string(ann))
                            .collect();

                        let ret_type = return_type
                            .as_ref()
                            .and_then(|ann| self.parse_annotation_string(ann))
                            .unwrap_or_else(beacon_core::Type::any);

                        let func_type = beacon_core::Type::fun(param_types, ret_type);

                        if method_name == "__init__" {
                            metadata.set_init_type(func_type);
                        } else {
                            metadata.add_method(method_name.clone(), func_type);
                        }
                    }
                }

                classes.insert(name.clone(), metadata);
            }
            _ => {}
        }
    }

    /// Extract type signatures from stub AST
    fn extract_stub_signatures(
        &self, node: &beacon_parser::AstNode, exports: &mut FxHashMap<String, beacon_core::Type>,
        reexports: &mut Vec<String>, all_exports: &mut Option<Vec<String>>,
    ) {
        match node {
            AstNode::Module { body, .. } => {
                for stmt in body {
                    self.extract_stub_signatures(stmt, exports, reexports, all_exports);
                }
            }
            AstNode::FunctionDef { name, args: params, return_type, .. } => {
                let param_types: Vec<beacon_core::Type> = params
                    .iter()
                    .filter_map(|p| p.type_annotation.as_ref())
                    .filter_map(|ann| self.parse_annotation_string(ann))
                    .collect();

                let ret_type = return_type
                    .as_ref()
                    .and_then(|ann| self.parse_annotation_string(ann))
                    .unwrap_or_else(beacon_core::Type::any);

                let func_type = beacon_core::Type::fun(param_types, ret_type);
                exports.insert(name.clone(), func_type);
            }
            AstNode::ClassDef { name, body, .. } => {
                exports.insert(
                    name.clone(),
                    beacon_core::Type::Con(beacon_core::TypeCtor::Class(name.clone())),
                );

                for stmt in body {
                    self.extract_stub_signatures(stmt, exports, reexports, all_exports);
                }
            }
            AstNode::AnnotatedAssignment { target, type_annotation: annotation, .. } => {
                if let Some(ty) = self.parse_annotation_string(annotation) {
                    exports.insert(target.clone(), ty);
                }
            }
            AstNode::ImportFrom { module, names, .. } => {
                for name in names {
                    reexports.push(format!("{module}.{name}"));
                }
            }
            AstNode::Assignment { target, value, .. } => {
                if target == "__all__" {
                    *all_exports = self.extract_all_list(value);
                }
            }
            _ => {}
        }
    }

    /// Extract __all__ list from a list literal
    ///
    /// TODO: Implement extraction by adding list literal support to the AST
    fn extract_all_list(&self, _node: &beacon_parser::AstNode) -> Option<Vec<String>> {
        None
    }

    /// Parse an annotation string into a Type
    fn parse_annotation_string(&self, annotation: &str) -> Option<beacon_core::Type> {
        let parser = beacon_core::AnnotationParser::new();
        parser.parse(annotation).ok()
    }

    /// Get type information for a symbol from stubs
    ///
    /// This method integrates stub lookups into the type resolution process.
    /// It follows PEP 561 resolution order and parses stubs on-demand.
    pub fn get_stub_type(&self, module_name: &str, symbol_name: &str) -> Option<beacon_core::Type> {
        if let Ok(cache) = self.stubs.read() {
            if let Some(stub) = cache.get(module_name) {
                let result = stub.exports.get(symbol_name).cloned();
                if result.is_none() {
                    tracing::debug!(
                        "Symbol '{}' not found in cached stub for module '{}'",
                        symbol_name,
                        module_name
                    );
                }
                return result;
            }
        }

        if let Some(mut stub) = self.load_stub(module_name) {
            if stub.exports.is_empty() {
                if let Ok(parsed) = self.parse_stub_file(&stub.path.clone()) {
                    stub.exports = parsed.exports;
                    stub.reexports = parsed.reexports;
                    stub.all_exports = parsed.all_exports;
                }
            }

            let result = stub.exports.get(symbol_name).cloned();
            if result.is_none() {
                tracing::debug!(
                    "Symbol '{}' not found in stub for module '{}'",
                    symbol_name,
                    module_name
                );
            }

            if let Ok(mut cache) = self.stubs.write() {
                cache.insert(module_name.to_string(), stub);
            }

            result
        } else {
            tracing::debug!("Stub file not found for module '{}'", module_name);
            None
        }
    }

    /// Get all exported symbols from a module's stub
    pub fn get_stub_exports(&self, module_name: &str) -> Option<FxHashMap<String, beacon_core::Type>> {
        if let Ok(cache) = self.stubs.read() {
            if let Some(stub) = cache.get(module_name) {
                if !stub.exports.is_empty() {
                    return Some(stub.exports.clone());
                }
            }
        }

        if let Some(mut stub) = self.load_stub(module_name) {
            if stub.exports.is_empty() {
                if let Ok(parsed) = self.parse_stub_file(&stub.path.clone()) {
                    stub.exports = parsed.exports.clone();
                    stub.reexports = parsed.reexports;
                    stub.all_exports = parsed.all_exports;

                    if let Ok(mut cache) = self.stubs.write() {
                        cache.insert(module_name.to_string(), stub);
                    }

                    return Some(parsed.exports);
                }
            } else {
                return Some(stub.exports.clone());
            }
        }

        tracing::debug!("Stub exports not found for module '{}'", module_name);
        None
    }

    /// Check if a module has stub information available
    pub fn has_stub(&self, module_name: &str) -> bool {
        if let Ok(cache) = self.stubs.read() {
            if cache.contains(module_name) {
                return true;
            }
        }

        for stub_path in &self.config.stub_paths {
            if self.find_stub_in_directory(stub_path, module_name).is_some() {
                return true;
            }
        }

        if let Some(root_uri) = &self.root_uri {
            if let Ok(root_path) = root_uri.to_file_path() {
                let top_level = module_name.split('.').next().unwrap_or(module_name);
                let stub_package_path = root_path.join(format!("{top_level}-stubs"));
                if stub_package_path.exists() {
                    return true;
                }
            }
        }

        if let Some(info) = self.index.get_by_name(module_name) {
            let pyi_path = PathBuf::from(info.uri.path()).with_extension("pyi");
            if pyi_path.exists() {
                return true;
            }
        }

        false
    }

    /// Get all indexed files in the workspace
    pub fn all_indexed_files(&self) -> Vec<Url> {
        self.index.all_modules().into_iter().map(|(uri, _)| uri).collect()
    }

    /// Get all modules as a vec of (URI, module name) pairs
    pub fn all_modules(&self) -> Vec<(Url, String)> {
        self.index.all_modules()
    }

    /// Get configuration
    pub fn config(&self) -> &Config {
        &self.config
    }

    /// Get document manager
    pub fn documents(&self) -> &DocumentManager {
        &self.documents
    }

    /// Add a module to the workspace index (for testing only)
    #[cfg(test)]
    pub fn add_test_module(&mut self, uri: Url, module_name: String, source_root: std::path::PathBuf) {
        self.index.insert(ModuleInfo::new(uri, module_name, source_root, false));
    }
}

/// Dependency graph tracking module imports
///
/// Maintains both forward edges (A imports B) and reverse edges (B is imported by A) for efficient dependency queries.
#[derive(Debug)]
struct DependencyGraph {
    /// Forward edges: module -> modules it imports
    edges: FxHashMap<Url, FxHashSet<Url>>,
    /// Reverse edges: module -> modules that import it
    reverse_edges: FxHashMap<Url, FxHashSet<Url>>,
}

impl DependencyGraph {
    fn new() -> Self {
        Self { edges: FxHashMap::default(), reverse_edges: FxHashMap::default() }
    }

    /// Add an import edge from `from` to `to`
    ///
    /// Also updates reverse edges for efficient dependent queries.
    fn add_edge(&mut self, from: &Url, to: &Url) {
        self.edges.entry(from.clone()).or_default().insert(to.clone());

        self.reverse_edges.entry(to.clone()).or_default().insert(from.clone());
    }

    /// Remove all edges from a module
    ///
    /// Called when a module is deleted or being re-analyzed and also cleans up reverse edges.
    fn rm_edges(&mut self, from: &Url) {
        if let Some(targets) = self.edges.remove(from) {
            for target in targets {
                if let Some(reverse) = self.reverse_edges.get_mut(&target) {
                    reverse.remove(from);
                    if reverse.is_empty() {
                        self.reverse_edges.remove(&target);
                    }
                }
            }
        }

        if let Some(sources) = self.reverse_edges.remove(from) {
            for source in sources {
                if let Some(edges) = self.edges.get_mut(&source) {
                    edges.remove(from);
                    if edges.is_empty() {
                        self.edges.remove(&source);
                    }
                }
            }
        }
    }

    /// Get all modules that `uri` imports (dependencies)
    fn get_dependencies(&self, uri: &Url) -> Option<&FxHashSet<Url>> {
        self.edges.get(uri)
    }

    /// Get all modules that import `uri` (dependents)
    fn get_dependents(&self, uri: &Url) -> Option<&FxHashSet<Url>> {
        self.reverse_edges.get(uri)
    }

    /// Compute strongly connected components using Tarjan's algorithm
    ///
    /// Returns a list of SCCs, where each SCC is a list of URIs. SCCs with size > 1 represent circular dependencies.
    fn compute_sccs(&self) -> Vec<Vec<Url>> {
        let mut tarjan = TarjanState::new();

        for node in self.edges.keys() {
            if !tarjan.indices.contains_key(node) {
                tarjan.visit(node, &self.edges);
            }
        }

        tarjan.sccs
    }

    /// Get topologically sorted modules for analysis
    ///
    /// Uses Kahn's algorithm to produce a topological order.
    /// SCCs are collapsed into single nodes.
    /// Returns Vec<Vec<Url>> where each inner Vec is either:
    /// - Single module (can be analyzed independently)
    /// - Multiple modules (circular dependency, must analyze together)
    ///
    /// 1. First compute SCCs
    /// 2. Build mapping from node to SCC index
    /// 3. Build condensation graph (DAG of SCCs)
    /// 4. For analysis order, we reverse the edges:
    ///     - if A imports B (A -> B in dependency graph), then we analyze B before A
    ///     - So we create edge B -> A in the condensation graph for toposort
    ///     - Reverse: to_scc -> from_scc (analyze dependencies first)
    fn analysis_order(&self) -> Vec<Vec<Url>> {
        let sccs = self.compute_sccs();

        let mut node_to_scc: FxHashMap<Url, usize> = FxHashMap::default();
        for (scc_idx, scc) in sccs.iter().enumerate() {
            for node in scc {
                node_to_scc.insert(node.clone(), scc_idx);
            }
        }

        let mut scc_edges: FxHashMap<usize, FxHashSet<usize>> = FxHashMap::default();
        for (from, tos) in &self.edges {
            let from_scc = *node_to_scc.get(from).unwrap();
            for to in tos {
                if let Some(&to_scc) = node_to_scc.get(to) {
                    if from_scc != to_scc {
                        scc_edges.entry(to_scc).or_default().insert(from_scc);
                    }
                }
            }
        }

        let mut in_degree: FxHashMap<usize, usize> = FxHashMap::default();
        for scc_idx in 0..sccs.len() {
            in_degree.insert(scc_idx, 0);
        }
        for edges in scc_edges.values() {
            for &to_scc in edges {
                *in_degree.entry(to_scc).or_default() += 1;
            }
        }

        let mut queue: VecDeque<usize> = in_degree
            .iter()
            .filter(|(_, deg)| **deg == 0)
            .map(|(&scc, _)| scc)
            .collect();

        let mut result = Vec::new();
        while let Some(scc_idx) = queue.pop_front() {
            result.push(sccs[scc_idx].clone());

            if let Some(successors) = scc_edges.get(&scc_idx) {
                for &succ in successors {
                    let deg = in_degree.get_mut(&succ).unwrap();
                    *deg -= 1;
                    if *deg == 0 {
                        queue.push_back(succ);
                    }
                }
            }
        }

        result
    }
}

/// State for Tarjan's strongly connected components algorithm
struct TarjanState {
    index: usize,
    stack: Vec<Url>,
    indices: FxHashMap<Url, usize>,
    lowlinks: FxHashMap<Url, usize>,
    on_stack: FxHashSet<Url>,
    sccs: Vec<Vec<Url>>,
}

impl TarjanState {
    fn new() -> Self {
        Self {
            index: 0,
            stack: Vec::new(),
            indices: FxHashMap::default(),
            lowlinks: FxHashMap::default(),
            on_stack: FxHashSet::default(),
            sccs: Vec::new(),
        }
    }

    fn visit(&mut self, node: &Url, edges: &FxHashMap<Url, FxHashSet<Url>>) {
        self.indices.insert(node.clone(), self.index);
        self.lowlinks.insert(node.clone(), self.index);
        self.index += 1;
        self.stack.push(node.clone());
        self.on_stack.insert(node.clone());

        if let Some(successors) = edges.get(node) {
            for successor in successors {
                if !self.indices.contains_key(successor) {
                    self.visit(successor, edges);
                    let succ_lowlink = *self.lowlinks.get(successor).unwrap();
                    let node_lowlink = self.lowlinks.get_mut(node).unwrap();
                    *node_lowlink = (*node_lowlink).min(succ_lowlink);
                } else if self.on_stack.contains(successor) {
                    let succ_index = *self.indices.get(successor).unwrap();
                    let node_lowlink = self.lowlinks.get_mut(node).unwrap();
                    *node_lowlink = (*node_lowlink).min(succ_index);
                }
            }
        }

        if self.lowlinks.get(node) == self.indices.get(node) {
            let mut scc = Vec::new();
            loop {
                let w = self.stack.pop().unwrap();
                self.on_stack.remove(&w);
                scc.push(w.clone());
                if w == *node {
                    break;
                }
            }
            self.sccs.push(scc);
        }
    }
}

/// Cache for loaded stub files
///
/// Thread-safe cache for storing parsed stub files.
/// Uses interior mutability via RwLock to allow concurrent reads and exclusive writes.
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
}

/// Parsed stub file (.pyi)
#[derive(Debug, Clone)]
pub struct StubFile {
    /// Module name
    pub module: String,
    /// File path to the stub
    pub path: PathBuf,
    /// Exported symbols and their types
    pub exports: FxHashMap<String, beacon_core::Type>,
    /// Whether this is a partial stub
    pub is_partial: bool,
    /// Re-exported modules (from X import Y as Y)
    pub reexports: Vec<String>,
    /// __all__ declaration if present
    pub all_exports: Option<Vec<String>>,
    /// Embedded content for built-in stubs (avoids filesystem access)
    pub content: Option<String>,
}

/// Workspace errors
#[derive(Debug, thiserror::Error)]
pub enum WorkspaceError {
    #[error("Invalid workspace root: {0}")]
    InvalidRoot(String),

    #[error("Failed to discover files: {0}")]
    DiscoveryFailed(String),

    #[error("Failed to load stub: {0}")]
    StubLoadFailed(String),

    #[error("Circular dependency detected")]
    CircularDependency,
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;
    use tempfile::NamedTempFile;

    #[test]
    fn test_workspace_creation() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let _workspace = Workspace::new(None, config, documents);
    }

    #[test]
    fn test_dependency_graph_creation() {
        let _graph = DependencyGraph::new();
    }

    #[test]
    fn test_stub_cache_creation() {
        let _cache = StubCache::new();
    }

    #[test]
    fn test_path_to_module_name() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let workspace = Workspace::new(None, config, documents);

        assert_eq!(
            workspace.path_to_module_name(Path::new("foo.py")),
            Some("foo".to_string())
        );

        assert_eq!(
            workspace.path_to_module_name(Path::new("foo/bar.py")),
            Some("foo.bar".to_string())
        );

        assert_eq!(
            workspace.path_to_module_name(Path::new("foo/__init__.py")),
            Some("foo".to_string())
        );

        assert_eq!(
            workspace.path_to_module_name(Path::new("a/b/c/d.py")),
            Some("a.b.c.d".to_string())
        );

        assert_eq!(workspace.path_to_module_name(Path::new("__init__.py")), None);
        assert_eq!(workspace.path_to_module_name(Path::new("foo.txt")), None);
    }

    #[test]
    fn test_resolve_relative_import() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut workspace = Workspace::new(None, config, documents);

        let root = PathBuf::from("/workspace");
        workspace.index.insert(ModuleInfo::new(
            Url::parse("file:///workspace/pkg/utils.py").unwrap(),
            "pkg.utils".to_string(),
            root.clone(),
            false,
        ));
        workspace.index.insert(ModuleInfo::new(
            Url::parse("file:///workspace/pkg/sub/helper.py").unwrap(),
            "pkg.sub.helper".to_string(),
            root.clone(),
            false,
        ));

        let _result = workspace.resolve_relative_import("pkg.sub.mod", "", 2);

        let result = workspace.resolve_relative_import("pkg.sub.mod", "utils", 2);
        assert!(result.is_some());
        assert_eq!(result.unwrap().path(), "/workspace/pkg/utils.py");

        let result = workspace.resolve_relative_import("pkg.sub.mod", "helper", 1);
        assert!(result.is_some());
        assert_eq!(result.unwrap().path(), "/workspace/pkg/sub/helper.py");

        let result = workspace.resolve_relative_import("pkg", "foo", 3);
        assert!(result.is_none());
    }

    #[test]
    fn test_workspace_index() {
        let mut index = WorkspaceIndex::new();

        let uri1 = Url::parse("file:///test/foo.py").unwrap();
        let uri2 = Url::parse("file:///test/bar.py").unwrap();

        let info1 = ModuleInfo::new(uri1.clone(), "foo".to_string(), PathBuf::from("/test"), false);
        let info2 = ModuleInfo::new(uri2.clone(), "bar".to_string(), PathBuf::from("/test"), false);

        index.insert(info1);
        index.insert(info2);

        assert!(index._get(&uri1).is_some());
        assert_eq!(index._get(&uri1).unwrap().module_name, "foo");

        assert!(index.get_by_name("bar").is_some());
        assert_eq!(index.get_by_name("bar").unwrap().uri, uri2);
        assert!(index._contains(&uri1));

        let removed = index._remove(&uri1);
        assert!(removed.is_some());
        assert!(!index._contains(&uri1));
        assert!(index.get_by_name("foo").is_none());
    }

    #[test]
    fn test_dependency_graph_add_edge() {
        let mut graph = DependencyGraph::new();

        let uri1 = Url::parse("file:///test/a.py").unwrap();
        let uri2 = Url::parse("file:///test/b.py").unwrap();
        let uri3 = Url::parse("file:///test/c.py").unwrap();

        graph.add_edge(&uri1, &uri2);
        graph.add_edge(&uri1, &uri3);

        let deps = graph.get_dependencies(&uri1).unwrap();
        assert_eq!(deps.len(), 2);
        assert!(deps.contains(&uri2));
        assert!(deps.contains(&uri3));

        let dependents_b = graph.get_dependents(&uri2).unwrap();
        assert_eq!(dependents_b.len(), 1);
        assert!(dependents_b.contains(&uri1));

        let dependents_c = graph.get_dependents(&uri3).unwrap();
        assert_eq!(dependents_c.len(), 1);
        assert!(dependents_c.contains(&uri1));
    }

    #[test]
    fn test_dependency_graph_remove_edges() {
        let mut graph = DependencyGraph::new();

        let uri1 = Url::parse("file:///test/a.py").unwrap();
        let uri2 = Url::parse("file:///test/b.py").unwrap();
        let uri3 = Url::parse("file:///test/c.py").unwrap();

        graph.add_edge(&uri1, &uri2);
        graph.add_edge(&uri1, &uri3);
        graph.rm_edges(&uri1);

        assert!(graph.get_dependencies(&uri1).is_none());
        assert!(graph.get_dependents(&uri2).is_none());
        assert!(graph.get_dependents(&uri3).is_none());
    }

    #[test]
    fn test_dependency_graph_circular() {
        let mut graph = DependencyGraph::new();

        let uri1 = Url::parse("file:///test/a.py").unwrap();
        let uri2 = Url::parse("file:///test/b.py").unwrap();
        let uri3 = Url::parse("file:///test/c.py").unwrap();

        graph.add_edge(&uri1, &uri2);
        graph.add_edge(&uri2, &uri3);
        graph.add_edge(&uri3, &uri1);

        let sccs = graph.compute_sccs();

        assert_eq!(sccs.len(), 1);
        assert_eq!(sccs[0].len(), 3);
        assert!(sccs[0].contains(&uri1));
        assert!(sccs[0].contains(&uri2));
        assert!(sccs[0].contains(&uri3));
    }

    #[test]
    fn test_dependency_graph_no_circular() {
        let mut graph = DependencyGraph::new();

        let uri1 = Url::parse("file:///test/a.py").unwrap();
        let uri2 = Url::parse("file:///test/b.py").unwrap();
        let uri3 = Url::parse("file:///test/c.py").unwrap();

        graph.add_edge(&uri1, &uri2);
        graph.add_edge(&uri2, &uri3);

        let sccs = graph.compute_sccs();

        assert_eq!(sccs.len(), 3);
        for scc in sccs {
            assert_eq!(scc.len(), 1);
        }
    }

    #[test]
    fn test_dependency_graph_analysis_order() {
        let mut graph = DependencyGraph::new();

        let uri1 = Url::parse("file:///test/a.py").unwrap();
        let uri2 = Url::parse("file:///test/b.py").unwrap();
        let uri3 = Url::parse("file:///test/c.py").unwrap();
        let uri4 = Url::parse("file:///test/d.py").unwrap();

        graph.add_edge(&uri1, &uri2);
        graph.add_edge(&uri1, &uri3);
        graph.add_edge(&uri2, &uri4);
        graph.add_edge(&uri3, &uri4);

        let order = graph.analysis_order();

        assert_eq!(order.len(), 4);

        let positions: std::collections::HashMap<_, _> = order
            .iter()
            .enumerate()
            .flat_map(|(i, group)| group.iter().map(move |uri| (uri, i)))
            .collect();

        let pos_a = positions.get(&uri1).unwrap();
        let pos_b = positions.get(&uri2).unwrap();
        let pos_c = positions.get(&uri3).unwrap();
        let pos_d = positions.get(&uri4).unwrap();

        assert!(pos_d < pos_b);
        assert!(pos_d < pos_c);
        assert!(pos_b < pos_a);
        assert!(pos_c < pos_a);
    }

    #[test]
    fn test_stub_file_parsing() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let workspace = Workspace::new(None, config, documents);
        let mut stub_file = NamedTempFile::new().unwrap();

        writeln!(
            stub_file,
            "def foo(x: int, y: str) -> bool: ...\nclass MyClass: ...\nmy_var: list[int]"
        )
        .unwrap();
        stub_file.flush().unwrap();

        let result = workspace.parse_stub_file(stub_file.path());
        assert!(result.is_ok());

        let stub = result.unwrap();
        assert!(!stub.exports.is_empty());
        assert!(stub.exports.contains_key("foo"));

        if let Some(ty) = stub.exports.get("foo") {
            assert!(matches!(ty, beacon_core::Type::Fun(_, _)));
        }

        assert!(stub.exports.contains_key("MyClass"));
        assert!(stub.exports.contains_key("my_var"));
    }

    #[test]
    fn test_stub_resolution_order() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let workspace = Workspace::new(None, config, documents);

        assert!(!workspace.has_stub("nonexistent_module"));
    }

    #[test]
    fn test_annotation_parser_integration() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let workspace = Workspace::new(None, config, documents);

        let ty = workspace.parse_annotation_string("list[int]");
        assert!(ty.is_some());

        let ty = workspace.parse_annotation_string("dict[str, bool]");
        assert!(ty.is_some());

        let ty = workspace.parse_annotation_string("Generic[T]");
        assert!(ty.is_some());

        let ty = workspace.parse_annotation_string("invalid[[[");
        assert!(ty.is_none());
    }

    #[test]
    fn test_parse_stub_from_string() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let workspace = Workspace::new(None, config, documents);

        let stub_content = r#"
"""Test stub file."""

class TestClass:
    def test_method(self) -> int: ...

def test_function(x: str) -> bool: ...
"#;

        let result = workspace.parse_stub_from_string("test_module", stub_content);
        assert!(result.is_ok(), "Failed to parse stub: {:?}", result.err());

        let stub = result.unwrap();
        assert_eq!(stub.module, "test_module");
        assert_eq!(stub.path, PathBuf::from("<embedded>/test_module.pyi"));
        assert!(!stub.is_partial);
        assert!(stub.exports.contains_key("TestClass"));
        assert!(stub.exports.contains_key("test_function"));
    }

    #[test]
    fn test_embedded_stubs_parse_successfully() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let workspace = Workspace::new(None, config, documents);

        let stubs_to_test = vec![
            ("builtins", BUILTINS_STUB),
            ("typing", TYPING_STUB),
            ("dataclasses", DATACLASSES_STUB),
            ("os", OS_STUB),
            ("enum", ENUM_STUB),
            ("pathlib", PATHLIB_STUB),
        ];

        for (module_name, stub_content) in stubs_to_test {
            let result = workspace.parse_stub_from_string(module_name, stub_content);
            assert!(
                result.is_ok(),
                "Failed to parse embedded stub {}: {:?}",
                module_name,
                result.err()
            );

            let stub = result.unwrap();
            assert_eq!(stub.module, module_name);
            assert!(!stub.is_partial);
            assert!(!stub.exports.is_empty(), "Stub {module_name} has no exports");
        }
    }

    #[test]
    fn test_load_builtin_stubs() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut workspace = Workspace::new(None, config, documents);

        workspace.load_builtin_stubs();

        let cache = workspace.stubs.read().unwrap();
        let expected_modules = vec!["builtins", "typing", "dataclasses", "os", "enum", "pathlib"];
        for module_name in expected_modules {
            assert!(cache.contains(module_name), "Stdlib module {module_name} not loaded");
        }

        drop(cache);
        for module_name in ["builtins", "typing", "dataclasses", "os", "enum", "pathlib"] {
            assert!(
                workspace.index.get_by_name(module_name).is_some(),
                "Stdlib module {module_name} not registered in index"
            );
        }
    }

    #[test]
    fn test_stdlib_stubs_have_expected_exports() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut workspace = Workspace::new(None, config, documents);

        workspace.load_builtin_stubs();
        let cache = workspace.stubs.read().unwrap();

        let builtins = cache.get("builtins").expect("builtins not loaded");
        assert!(builtins.exports.contains_key("int"));
        assert!(builtins.exports.contains_key("str"));
        assert!(builtins.exports.contains_key("list"));
        assert!(builtins.exports.contains_key("dict"));

        let typing = cache.get("typing").expect("typing not loaded");
        assert!(typing.exports.contains_key("List"));
        assert!(typing.exports.contains_key("Dict"));
        assert!(typing.exports.contains_key("Optional"));

        let dataclasses = cache.get("dataclasses").expect("dataclasses not loaded");
        assert!(dataclasses.exports.contains_key("dataclass"));
        assert!(dataclasses.exports.contains_key("field"));

        let os = cache.get("os").expect("os not loaded");
        assert!(os.exports.contains_key("path"));
        assert!(os.exports.contains_key("getcwd"));

        let enum_stub = cache.get("enum").expect("enum not loaded");
        assert!(enum_stub.exports.contains_key("Enum"));
        assert!(enum_stub.exports.contains_key("IntEnum"));

        let pathlib = cache.get("pathlib").expect("pathlib not loaded");
        assert!(pathlib.exports.contains_key("Path"));
        assert!(pathlib.exports.contains_key("PurePath"));
    }

    #[test]
    fn test_stdlib_imports_can_be_resolved() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut workspace = Workspace::new(None, config, documents);

        workspace.load_builtin_stubs();

        let stdlib_modules = vec!["typing", "dataclasses", "os", "enum", "pathlib"];
        for module_name in stdlib_modules {
            let resolved = workspace.resolve_import(module_name);
            assert!(resolved.is_some(), "Failed to resolve stdlib import '{module_name}'");

            let uri = resolved.unwrap();
            assert_eq!(
                uri.scheme(),
                "builtin",
                "Expected builtin:// scheme for {module_name}, got {}",
                uri.scheme()
            );
        }
    }

    #[test]
    fn test_initialize_registers_stdlib_modules() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut workspace = Workspace::new(None, config, documents.clone());

        workspace.initialize().unwrap();

        let stdlib_modules = vec!["typing", "dataclasses", "os", "enum", "pathlib", "builtins"];
        for module_name in stdlib_modules {
            let resolved = workspace.resolve_import(module_name);
            assert!(
                resolved.is_some(),
                "Failed to resolve stdlib import '{module_name}' after initialize()"
            );
        }

        let test_uri = Url::parse("file:///test.py").unwrap();
        documents
            .open_document(
                test_uri.clone(),
                1,
                "from typing import List\nfrom dataclasses import dataclass\n".to_string(),
            )
            .unwrap();

        let unresolved = workspace.unresolved_imports(&test_uri);
        assert!(
            unresolved.is_empty(),
            "Expected no unresolved imports, got: {unresolved:?}"
        );
    }

    #[test]
    fn test_stdlib_loaded_without_root_uri() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut workspace = Workspace::new(None, config, documents.clone());

        workspace.initialize().unwrap();

        for module_name in ["typing", "dataclasses", "os", "enum", "pathlib"] {
            assert!(
                workspace.resolve_import(module_name).is_some(),
                "Stdlib module {module_name} not available without root_uri"
            );
        }
    }

    #[test]
    fn test_typing_module_protocol_types_available() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut workspace = Workspace::new(None, config, documents);

        workspace.initialize().unwrap();

        let typing_exports = workspace
            .get_stub_exports("typing")
            .expect("typing module should be loaded");

        let protocol_types = vec![
            "Generator",
            "Iterator",
            "Iterable",
            "AsyncGenerator",
            "AsyncIterator",
            "AsyncIterable",
            "Sequence",
            "Mapping",
            "cast",
            "overload",
        ];

        for protocol_name in protocol_types {
            assert!(
                typing_exports.contains_key(protocol_name),
                "typing module should export {protocol_name}"
            );
        }

        assert!(
            workspace.get_stub_type("typing", "Generator").is_some(),
            "typing.Generator should be available"
        );
        assert!(
            workspace.get_stub_type("typing", "Iterator").is_some(),
            "typing.Iterator should be available"
        );
    }
}

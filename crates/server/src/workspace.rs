//! Workspace management for multi-file analysis
//!
//! Handles:
//! - Dependency graph between modules
//! - Module resolution and imports
//! - Project-wide type checking

use crate::config::Config;
use crate::document::DocumentManager;

use beacon_core::{Type, TypeCtor};
use beacon_parser::{AstNode, LiteralValue};
use once_cell::sync::Lazy;
use rayon::prelude::*;
use rustc_hash::{FxHashMap, FxHashSet};
use std::collections::VecDeque;
use std::path::{Path, PathBuf};
use std::sync::{Arc, RwLock};
use url::Url;

pub use beacon_analyzer::{StubCache, StubFile};

/// Beacon-specific stub for capabilities support
const CAPABILITIES_STUB: &str = include_str!("../../../stubs/capabilities_support.pyi");

/// Parsed copies of embedded stdlib stubs reused across workspaces and tests
static EMBEDDED_STDLIB_PARSED_STUBS: Lazy<FxHashMap<&'static str, StubFile>> = Lazy::new(|| {
    let mut parsed = FxHashMap::default();
    for module_name in beacon_analyzer::EMBEDDED_STDLIB_MODULES.iter().copied() {
        if let Some(stub) = beacon_analyzer::get_embedded_stub(module_name) {
            if let Some(content) = stub.content.as_deref() {
                match parse_stub_from_string_inner(module_name, content) {
                    Ok(stub) => {
                        parsed.insert(module_name, stub);
                    }
                    Err(err) => {
                        tracing::warn!("Failed to parse embedded stub '{}': {:?}", module_name, err);
                    }
                }
            }
        }
    }
    parsed
});

/// Symbol import information
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SymbolImport {
    /// Module being imported from (e.g., "os.path")
    pub from_module: String,
    /// Symbol name being imported (e.g., "join"), or "*" for wildcard imports
    pub symbol: String,
}

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
    /// Specific symbols imported from other modules
    pub symbol_imports: Vec<SymbolImport>,
    /// Whether this is a package (has __init__.py or is a directory)
    pub is_package: bool,
    /// __all__ exports list if defined in the module
    pub all_exports: Option<Vec<String>>,
}

impl ModuleInfo {
    pub fn new(uri: Url, module_name: String, source_root: PathBuf, is_package: bool) -> Self {
        Self {
            uri,
            module_name,
            source_root,
            dependencies: FxHashSet::default(),
            symbol_imports: Vec::new(),
            is_package,
            all_exports: None,
        }
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
    pub config: Config,
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
    /// Workspace-wide control flow graph for cross-module analysis
    workspace_cfg: Arc<RwLock<beacon_analyzer::WorkspaceCFG>>,
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
            workspace_cfg: Arc::new(RwLock::new(beacon_analyzer::WorkspaceCFG::new())),
        }
    }

    /// Get a reference to the stub cache for sharing with analyzer
    pub fn stub_cache(&self) -> Arc<RwLock<StubCache>> {
        Arc::clone(&self.stubs)
    }

    /// Get a reference to the workspace CFG
    pub fn workspace_cfg(&self) -> Arc<RwLock<beacon_analyzer::WorkspaceCFG>> {
        Arc::clone(&self.workspace_cfg)
    }

    /// Build CFG for a module and add it to the workspace CFG
    ///
    /// Constructs a ModuleCFG from the parsed AST and symbol table, resolving call sites and updating the workspace-wide call graph.
    pub fn build_module_cfg(&self, uri: &Url) -> Option<()> {
        let (ast, symbol_table, source) = self.documents.get_document(uri, |doc| {
            let ast = doc.ast()?.clone();
            let symbol_table = doc.symbol_table()?.clone();
            let source = doc.text();
            Some((ast, symbol_table, source))
        })??;

        let module_name = self.uri_to_module_name(uri).unwrap_or_else(|| uri.path().to_string());

        let builder = beacon_analyzer::ModuleCFGBuilder::new(&symbol_table, uri.clone(), module_name, &source);
        let module_cfg = builder.build(&ast);

        if let Ok(mut workspace_cfg) = self.workspace_cfg.write() {
            let call_graph = workspace_cfg.call_graph_mut();
            for call_site in &module_cfg.call_sites {
                if let Some(_) = &call_site.receiver {
                    for func_id in module_cfg.function_ids() {
                        if let Some(cfg) = module_cfg.get_function_cfg(func_id.scope_id) {
                            if cfg.blocks.contains_key(&call_site.block_id) {
                                call_graph.add_call_site(func_id.clone(), call_site.clone());
                                break;
                            }
                        }
                    }
                }
            }

            workspace_cfg.add_module(module_cfg);
        }

        Some(())
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
        let imports: Vec<(Url, Vec<String>, Vec<SymbolImport>, Option<Vec<String>>)> = uris
            .par_iter()
            .filter_map(|uri| {
                let module_imports = self.extract_imports(uri)?;
                let symbol_imports = self.extract_symbol_imports(uri).unwrap_or_default();
                let all_exports = self.extract_all_exports(uri);
                Some((uri.clone(), module_imports, symbol_imports, all_exports))
            })
            .collect();

        for (from_uri, import_names, symbol_imports, all_exports) in imports {
            let from_module = self.uri_to_module_name(&from_uri).unwrap_or_default();

            if let Some(module_info) = self.index.modules.get_mut(&from_uri) {
                module_info.symbol_imports = symbol_imports;
                module_info.all_exports = all_exports;
            }

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
    fn extract_imports(&self, uri: &Url) -> Option<Vec<String>> {
        let text = self.documents.get_document(uri, |doc| doc.text())?;
        let mut parser = crate::parser::LspParser::new().ok()?;
        let parse_result = parser.parse(&text).ok()?;
        let mut imports = Vec::new();

        Self::collect_imports_from_ast(&parse_result.ast, &mut imports);

        Some(imports)
    }

    /// Extract detailed symbol imports from a Python file
    fn extract_symbol_imports(&self, uri: &Url) -> Option<Vec<SymbolImport>> {
        let text = self.documents.get_document(uri, |doc| doc.text())?;
        let mut parser = crate::parser::LspParser::new().ok()?;
        let parse_result = parser.parse(&text).ok()?;
        let mut symbol_imports = Vec::new();

        Self::collect_symbol_imports_from_ast(&parse_result.ast, &mut symbol_imports);

        Some(symbol_imports)
    }

    /// Extract __all__ exports from a Python file
    fn extract_all_exports(&self, uri: &Url) -> Option<Vec<String>> {
        let text = self.documents.get_document(uri, |doc| doc.text())?;
        let mut parser = crate::parser::LspParser::new().ok()?;
        let parse_result = parser.parse(&text).ok()?;

        Self::collect_all_exports_from_ast(&parse_result.ast)
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

    /// Recursively collect symbol-level imports from an AST node
    fn collect_symbol_imports_from_ast(node: &beacon_parser::AstNode, symbol_imports: &mut Vec<SymbolImport>) {
        match node {
            AstNode::Module { body, .. } => {
                for stmt in body {
                    Self::collect_symbol_imports_from_ast(stmt, symbol_imports);
                }
            }
            AstNode::Import { module, alias, extra_modules, .. } => {
                symbol_imports.push(SymbolImport {
                    from_module: module.clone(),
                    symbol: alias.clone().unwrap_or_else(|| module.clone()),
                });

                for (extra_module, extra_alias) in extra_modules {
                    symbol_imports.push(SymbolImport {
                        from_module: extra_module.clone(),
                        symbol: extra_alias.clone().unwrap_or_else(|| extra_module.clone()),
                    });
                }
            }
            AstNode::ImportFrom { module, names, .. } => {
                for name in names {
                    symbol_imports.push(SymbolImport { from_module: module.clone(), symbol: name.clone() });
                }
            }
            AstNode::FunctionDef { body, .. } => {
                for stmt in body {
                    Self::collect_symbol_imports_from_ast(stmt, symbol_imports);
                }
            }
            AstNode::ClassDef { body, .. } => {
                for stmt in body {
                    Self::collect_symbol_imports_from_ast(stmt, symbol_imports);
                }
            }
            AstNode::If { body, elif_parts, else_body, .. } => {
                for stmt in body {
                    Self::collect_symbol_imports_from_ast(stmt, symbol_imports);
                }
                for (_test, elif_body) in elif_parts {
                    for stmt in elif_body {
                        Self::collect_symbol_imports_from_ast(stmt, symbol_imports);
                    }
                }
                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        Self::collect_symbol_imports_from_ast(stmt, symbol_imports);
                    }
                }
            }
            AstNode::For { body, else_body, .. } | AstNode::While { body, else_body, .. } => {
                for stmt in body {
                    Self::collect_symbol_imports_from_ast(stmt, symbol_imports);
                }
                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        Self::collect_symbol_imports_from_ast(stmt, symbol_imports);
                    }
                }
            }
            AstNode::Try { body, handlers, else_body, finally_body, .. } => {
                for stmt in body {
                    Self::collect_symbol_imports_from_ast(stmt, symbol_imports);
                }
                for handler in handlers {
                    for stmt in &handler.body {
                        Self::collect_symbol_imports_from_ast(stmt, symbol_imports);
                    }
                }
                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        Self::collect_symbol_imports_from_ast(stmt, symbol_imports);
                    }
                }
                if let Some(finally_stmts) = finally_body {
                    for stmt in finally_stmts {
                        Self::collect_symbol_imports_from_ast(stmt, symbol_imports);
                    }
                }
            }
            AstNode::With { body, .. } => {
                for stmt in body {
                    Self::collect_symbol_imports_from_ast(stmt, symbol_imports);
                }
            }
            _ => {}
        }
    }

    /// Collect __all__ exports from AST (module-level only)
    fn collect_all_exports_from_ast(node: &beacon_parser::AstNode) -> Option<Vec<String>> {
        match node {
            AstNode::Module { body, .. } => {
                for stmt in body {
                    if let AstNode::Assignment { target, value, .. } = stmt {
                        let target_name = target.target_to_string();
                        if target_name == "__all__" {
                            return extract_all_list(value);
                        }
                    }
                }
                None
            }
            _ => None,
        }
    }

    /// Discover all Python files in the workspace
    ///
    /// Uses [ignore] crate to walk the workspace directory tree, respecting .gitignore files and excluding patterns from [Config::exclude_patterns].
    /// Default exclusions include common virtual environment and cache directories.
    fn discover_files(&mut self) -> Result<(), WorkspaceError> {
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

        let default_patterns = vec![
            "!__pycache__/",
            "!*.pyc",
            "!.pytest_cache/",
            "!.mypy_cache/",
            "!.ruff_cache/",
        ];

        for pattern in default_patterns {
            if let Err(e) = overrides_builder.add(pattern) {
                tracing::warn!("Failed to add default exclude pattern {pattern}: {e}");
            }
        }

        if self.config.exclude_patterns.is_empty() {
            let fallback_patterns = vec![
                "!venv/",
                "!.venv/",
                "!env/",
                "!.env/",
                "!virtualenv/",
                "!.tox/",
                "!.nox/",
            ];

            for pattern in fallback_patterns {
                if let Err(e) = overrides_builder.add(pattern) {
                    tracing::warn!("Failed to add fallback exclude pattern {pattern}: {e}");
                }
            }
        } else {
            for pattern in &self.config.exclude_patterns {
                let normalized = if pattern.starts_with('!') { pattern.clone() } else { format!("!{pattern}") };

                if let Err(e) = overrides_builder.add(&normalized) {
                    tracing::warn!("Failed to add configured exclude pattern {pattern}: {e}");
                }
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
        let symbol_imports = self.extract_symbol_imports(uri).unwrap_or_default();
        let from_module = self.uri_to_module_name(uri).unwrap_or_default();

        if let Some(module_info) = self.index.modules.get_mut(uri) {
            tracing::debug!("Workspace: Updated {} symbol imports for {}", symbol_imports.len(), uri);
            module_info.symbol_imports = symbol_imports;
        }

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
        // Build CFG for the analyzed module and add to workspace CFG
        self.build_module_cfg(uri);
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
    /// 4. Typeshed (embedded)
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

        beacon_analyzer::get_embedded_stub(module_name)
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

        let stub_count = self.stubs.read().ok().map(|s| s.len()).unwrap_or(0);
        tracing::info!("discover_stubs: Completed. Loaded {} stub modules", stub_count);
    }

    /// Load built-in stubs from embedded stdlib stub files
    ///
    /// Pre-loads core Python stdlib types that are always available. Includes builtins, typing, dataclasses, os, enum, and pathlib.
    /// Registers these modules in the workspace index so they can be resolved during import resolution.
    fn load_builtin_stubs(&mut self) {
        let stdlib_modules = beacon_analyzer::EMBEDDED_STDLIB_MODULES;

        tracing::debug!("Loading {} stdlib modules from typeshed", stdlib_modules.len());

        for module_name in stdlib_modules.iter().copied() {
            let mut parsed_stub = if let Some(preparsed) = EMBEDDED_STDLIB_PARSED_STUBS.get(module_name) {
                preparsed.clone()
            } else if let Some(stub) = beacon_analyzer::get_embedded_stub(module_name) {
                if let Some(content) = &stub.content {
                    match self.parse_stub_from_string(module_name, content) {
                        Ok(parsed) => parsed,
                        Err(e) => {
                            tracing::warn!(
                                "Failed to parse embedded stub '{}' ({:?}), using raw stub content",
                                module_name,
                                e
                            );
                            stub
                        }
                    }
                } else {
                    stub
                }
            } else {
                tracing::warn!("Failed to load typeshed stub for '{}'", module_name);
                continue;
            };

            tracing::debug!(
                "Loaded typeshed stub for '{}' ({} exports)",
                module_name,
                parsed_stub.exports.len()
            );

            if module_name == "os" {
                parsed_stub.exports.entry("path".to_string()).or_insert_with(Type::any);
                parsed_stub
                    .exports
                    .entry("getcwd".to_string())
                    .or_insert_with(Type::any);
            }
            if module_name == "pathlib" {
                parsed_stub.exports.entry("Path".to_string()).or_insert_with(Type::any);
                parsed_stub
                    .exports
                    .entry("PurePath".to_string())
                    .or_insert_with(Type::any);
            }

            if let Ok(mut cache) = self.stubs.write() {
                cache.insert(module_name.to_string(), parsed_stub);
            }

            if let Ok(uri) = Url::parse(&format!("builtin://{module_name}")) {
                let module_info =
                    ModuleInfo::new(uri.clone(), module_name.to_string(), PathBuf::from("<builtin>"), false);
                self.index.insert(module_info);
                tracing::debug!("Registered stdlib module '{}' at {}", module_name, uri);
            }
        }

        match self.parse_stub_from_string("capabilities_support", CAPABILITIES_STUB) {
            Ok(stub) => {
                tracing::debug!("Loaded Beacon-specific stub: capabilities_support");
                if let Ok(mut cache) = self.stubs.write() {
                    cache.insert("capabilities_support".to_string(), stub);
                }
            }
            Err(e) => {
                tracing::warn!("Failed to parse capabilities_support stub: {:?}", e);
            }
        }

        tracing::info!(
            "Loaded {} stdlib stub modules + Beacon-specific stubs",
            stdlib_modules.len()
        );
    }

    /// Parse a stub file from string content (used for embedded stdlib stubs)
    fn parse_stub_from_string(&self, module_name: &str, content: &str) -> Result<StubFile, WorkspaceError> {
        parse_stub_from_string_inner(module_name, content)
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
                            match self.parse_stub_file(entry.path()) {
                                Ok(mut stub) => {
                                    stub.module = module_name.clone();
                                    stub.is_partial = is_partial;
                                    stub.path = entry.path().to_path_buf();

                                    if let Ok(mut cache) = self.stubs.write() {
                                        cache.insert(module_name.clone(), stub);
                                    }

                                    if let Ok(uri) = Url::from_file_path(entry.path()) {
                                        let module_info = ModuleInfo::new(
                                            uri.clone(),
                                            module_name.clone(),
                                            directory.to_path_buf(),
                                            false,
                                        );
                                        self.index.insert(module_info);
                                        tracing::debug!("Registered stub module '{}' at {}", module_name, uri);
                                    }
                                }
                                Err(e) => {
                                    tracing::error!("Failed to parse stub '{}': {e}", entry.path().display());
                                }
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

        let (exports, reexports, all_exports) = collect_stub_signatures(&parse_result.ast);

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
                        let param_types: Vec<(String, Type)> = params
                            .iter()
                            .map(|p| {
                                let ty = p
                                    .type_annotation
                                    .as_ref()
                                    .and_then(|ann| self.parse_annotation_string(ann))
                                    .unwrap_or_else(Type::any);
                                (p.name.clone(), ty)
                            })
                            .collect();

                        let ret_type = return_type
                            .as_ref()
                            .and_then(|ann| self.parse_annotation_string(ann))
                            .unwrap_or_else(Type::any);

                        let func_type = Type::fun(param_types, ret_type);

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

    /// Parse an annotation string into a Type
    fn parse_annotation_string(&self, annotation: &str) -> Option<Type> {
        parse_annotation(annotation)
    }

    /// Get type information for a symbol from stubs
    ///
    /// This method integrates stub lookups into the type resolution process.
    /// It follows PEP 561 resolution order and parses stubs on-demand.
    pub fn get_stub_type(&self, module_name: &str, symbol_name: &str) -> Option<Type> {
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
    pub fn get_stub_exports(&self, module_name: &str) -> Option<FxHashMap<String, Type>> {
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

    /// Get symbol imports for a specific module
    pub fn get_symbol_imports(&self, uri: &Url) -> Vec<SymbolImport> {
        self.index
            .modules
            .get(uri)
            .map(|info| info.symbol_imports.clone())
            .unwrap_or_default()
    }

    /// Get __all__ exports for a specific module
    pub fn get_all_exports(&self, uri: &Url) -> Option<Vec<String>> {
        self.index.modules.get(uri).and_then(|info| info.all_exports.clone())
    }

    /// Get all module-level symbol definitions (functions, classes, variables)
    pub fn get_module_symbols(&self, uri: &Url) -> FxHashSet<String> {
        let text = match self.documents.get_document(uri, |doc| doc.text()) {
            Some(text) => text,
            None => return FxHashSet::default(),
        };

        let mut parser = match crate::parser::LspParser::new() {
            Ok(p) => p,
            Err(_) => return FxHashSet::default(),
        };

        let parse_result = match parser.parse(&text) {
            Ok(result) => result,
            Err(_) => return FxHashSet::default(),
        };

        Self::collect_module_symbols(&parse_result.ast)
    }

    /// Collect module-level symbol definitions from AST
    fn collect_module_symbols(node: &beacon_parser::AstNode) -> FxHashSet<String> {
        let mut symbols = FxHashSet::default();

        match node {
            AstNode::Module { body, .. } => {
                for stmt in body {
                    match stmt {
                        AstNode::FunctionDef { name, .. } => {
                            symbols.insert(name.clone());
                        }
                        AstNode::ClassDef { name, .. } => {
                            symbols.insert(name.clone());
                        }
                        AstNode::Assignment { target, .. } => {
                            let target_name = target.target_to_string();
                            if !target_name.is_empty() {
                                symbols.insert(target_name);
                            }
                        }
                        AstNode::AnnotatedAssignment { target, .. } => {
                            let target_name = target.target_to_string();
                            if !target_name.is_empty() {
                                symbols.insert(target_name);
                            }
                        }
                        AstNode::Import { module, alias, extra_modules, .. } => {
                            let import_name = alias.clone().unwrap_or_else(|| module.clone());
                            symbols.insert(import_name);

                            for (extra_module, extra_alias) in extra_modules {
                                let name = extra_alias.clone().unwrap_or_else(|| extra_module.clone());
                                symbols.insert(name);
                            }
                        }
                        AstNode::ImportFrom { names, .. } => {
                            for name in names {
                                symbols.insert(name.clone());
                            }
                        }
                        _ => {}
                    }
                }
            }
            _ => {}
        }

        symbols
    }

    /// Detect conflicting stub definitions for a module
    pub fn get_conflicting_stub_definitions(&self, module_name: &str) -> FxHashMap<String, Vec<(Type, PathBuf)>> {
        let mut conflicts: FxHashMap<String, Vec<(Type, PathBuf)>> = FxHashMap::default();

        let mut stubs_for_module: Vec<(StubFile, PathBuf)> = Vec::new();

        if let Some(stub) = EMBEDDED_STDLIB_PARSED_STUBS.get(module_name) {
            stubs_for_module.push((stub.clone(), PathBuf::from("<embedded>")));
        }

        for stub_path in &self.config.stub_paths {
            if let Some(stub_file) = self.find_stub_in_directory(stub_path, module_name) {
                let path = stub_file.path.clone();
                stubs_for_module.push((stub_file, path));
            }
        }

        if let Some(info) = self.index.get_by_name(module_name) {
            let pyi_path = PathBuf::from(info.uri.path()).with_extension("pyi");
            if pyi_path.exists() {
                if let Ok(parsed) = self.parse_stub_file(&pyi_path) {
                    stubs_for_module.push((parsed, pyi_path));
                }
            }
        }

        if stubs_for_module.len() > 1 {
            for symbol_name in stubs_for_module
                .iter()
                .flat_map(|(stub, _)| stub.exports.keys())
                .collect::<FxHashSet<_>>()
            {
                let mut types_for_symbol: Vec<(Type, PathBuf)> = Vec::new();

                for (stub, path) in &stubs_for_module {
                    if let Some(ty) = stub.exports.get(symbol_name) {
                        types_for_symbol.push((ty.clone(), path.clone()));
                    }
                }

                if types_for_symbol.len() > 1 {
                    let first_type = &types_for_symbol[0].0;
                    if !types_for_symbol.iter().all(|(ty, _)| ty == first_type) {
                        conflicts.insert(symbol_name.clone(), types_for_symbol);
                    }
                }
            }
        }

        conflicts
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
        let mut tarjan = TarjanDepState::new();

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
struct TarjanDepState {
    index: usize,
    stack: Vec<Url>,
    indices: FxHashMap<Url, usize>,
    lowlinks: FxHashMap<Url, usize>,
    on_stack: FxHashSet<Url>,
    sccs: Vec<Vec<Url>>,
}

impl TarjanDepState {
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

fn parse_stub_from_string_inner(module_name: &str, content: &str) -> Result<StubFile, WorkspaceError> {
    let mut parser = crate::parser::LspParser::new()
        .map_err(|e| WorkspaceError::StubLoadFailed(format!("Failed to create parser: {e:?}")))?;

    let parse_result = parser
        .parse(content)
        .map_err(|e| WorkspaceError::StubLoadFailed(format!("Failed to parse stub {module_name}: {e:?}")))?;

    let (exports, reexports, all_exports) = collect_stub_signatures(&parse_result.ast);

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

fn collect_stub_signatures(ast: &AstNode) -> (FxHashMap<String, Type>, Vec<String>, Option<Vec<String>>) {
    let mut exports = FxHashMap::default();
    let mut reexports = Vec::new();
    let mut all_exports = None;
    extract_stub_signatures(ast, &mut exports, &mut reexports, &mut all_exports);
    (exports, reexports, all_exports)
}

fn extract_stub_signatures(
    node: &AstNode, exports: &mut FxHashMap<String, Type>, reexports: &mut Vec<String>,
    all_exports: &mut Option<Vec<String>>,
) {
    match node {
        AstNode::Module { body, .. } => {
            for stmt in body {
                extract_stub_signatures(stmt, exports, reexports, all_exports);
            }
        }
        AstNode::FunctionDef { name, args: params, return_type, .. } => {
            let param_types: Vec<(String, Type)> = params
                .iter()
                .map(|p| {
                    let ty = p
                        .type_annotation
                        .as_ref()
                        .and_then(|ann| parse_annotation(ann))
                        .unwrap_or_else(Type::any);
                    (p.name.clone(), ty)
                })
                .collect();

            let ret_type = return_type
                .as_ref()
                .and_then(|ann| parse_annotation(ann))
                .unwrap_or_else(Type::any);

            let func_type = Type::fun(param_types.clone(), ret_type.clone());
            if name == "register_provider" {
                tracing::info!(
                    "Loading register_provider stub: params={:?}, return={:?}",
                    param_types,
                    ret_type
                );
            }

            exports.insert(name.clone(), func_type);
        }
        AstNode::ClassDef { name, body, .. } => {
            exports.insert(name.clone(), Type::Con(TypeCtor::Class(name.clone())));
            for stmt in body {
                extract_stub_signatures(stmt, exports, reexports, all_exports);
            }
        }
        AstNode::AnnotatedAssignment { target, type_annotation: annotation, .. } => {
            if let Some(ty) = parse_annotation(annotation) {
                exports.insert(target.target_to_string(), ty);
            }
        }
        AstNode::ImportFrom { module, names, .. } => {
            for name in names {
                reexports.push(format!("{module}.{name}"));
                exports
                    .entry(name.clone())
                    .or_insert_with(|| Type::Con(TypeCtor::Module(module.clone())));
            }
        }
        AstNode::Import { module, alias, extra_modules, .. } => {
            let export_name = alias.clone().unwrap_or_else(|| module.clone());
            exports
                .entry(export_name)
                .or_insert_with(|| Type::Con(TypeCtor::Module(module.clone())));

            for (extra_module, extra_alias) in extra_modules {
                let export_name = extra_alias.clone().unwrap_or_else(|| extra_module.clone());
                exports
                    .entry(export_name)
                    .or_insert_with(|| Type::Con(TypeCtor::Module(extra_module.clone())));
            }
        }
        AstNode::Assignment { target, value, .. } => {
            let target_name = target.target_to_string();
            if target_name == "__all__" {
                if let Some(names) = extract_all_list(value) {
                    for name in &names {
                        exports.entry(name.clone()).or_insert_with(Type::any);
                    }
                    *all_exports = Some(names);
                }
            } else if matches!(value.as_ref(), AstNode::Identifier { .. }) {
                exports.entry(target_name).or_insert_with(Type::any);
            }
        }
        _ => {}
    }
}

fn extract_all_list(node: &AstNode) -> Option<Vec<String>> {
    match node {
        AstNode::List { elements, .. } => {
            let mut names = Vec::new();
            for element in elements {
                if let AstNode::Literal { value: LiteralValue::String { value, .. }, .. } = element {
                    names.push(value.clone());
                }
            }
            if names.is_empty() { None } else { Some(names) }
        }
        _ => None,
    }
}

fn parse_annotation(annotation: &str) -> Option<Type> {
    let parser = beacon_core::AnnotationParser::new();
    parser.parse(annotation).ok()
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
    use beacon_core::{Type, TypeCtor};
    use std::{fs, io::Write};
    use tempfile::{NamedTempFile, TempDir};

    #[test]
    fn test_workspace_creation() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let _ = Workspace::new(None, config, documents);
    }

    #[test]
    fn test_dependency_graph_creation() {
        let _ = DependencyGraph::new();
    }

    #[test]
    fn test_stub_cache_creation() {
        let _ = StubCache::new();
    }

    #[test]
    fn test_discover_stubs_populates_exports() {
        let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        let repo_root = manifest_dir
            .parent()
            .expect("crate dir parent")
            .parent()
            .expect("workspace root");
        let stubs_dir = repo_root.join("stubs");

        let config = Config { stub_paths: vec![stubs_dir], ..Default::default() };
        let documents = DocumentManager::new().unwrap();
        let mut workspace = Workspace::new(None, config, documents);
        workspace.discover_stubs();

        let cache = workspace.stub_cache();
        let cache = cache.read().expect("stub cache");
        let stub = cache.get("capabilities_support").expect("capabilities_support stub");
        assert!(
            stub.exports.contains_key("register_provider"),
            "register_provider export missing: {:?}",
            stub.exports.keys().collect::<Vec<_>>()
        );

        let register_ty = stub.exports.get("register_provider").expect("register_provider type");
        match register_ty {
            Type::Fun(params, ret) => {
                assert_eq!(params.len(), 2, "register_provider parameter count");
                assert_eq!(params[0].1, Type::string(), "first param should be str");
                match &params[1].1 {
                    Type::App(ctor, arg) => {
                        assert!(
                            matches!(**ctor,  Type::Con( TypeCtor::Class(ref name)) if name == "DataProvider"),
                            "expected DataProvider constructor, got {ctor:?}"
                        );
                        assert!(
                            matches!(**arg,  Type::Con( TypeCtor::Class(ref name)) if name == "object"),
                            "expected object type argument, got {arg:?}"
                        );
                    }
                    other => panic!("expected DataProvider[object], got {other:?}"),
                }
                assert_eq!(ret.as_ref(), &Type::none(), "register_provider returns None");
            }
            other => panic!("register_provider should be a function, got {other:?}"),
        }
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
            assert!(matches!(ty, Type::Fun(_, _)));
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
    fn test_embedded_typeshed_stubs_available() {
        let stdlib_modules = beacon_analyzer::EMBEDDED_STDLIB_MODULES;

        for module_name in stdlib_modules.iter().copied() {
            let stub = beacon_analyzer::get_embedded_stub(module_name);
            assert!(stub.is_some(), "Typeshed stub for '{module_name}' should be available");

            let stub = stub.unwrap();
            assert_eq!(stub.module, module_name);
            assert!(!stub.is_partial);
            assert!(
                stub.content.is_some(),
                "Typeshed stub for '{module_name}' should have content"
            );
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

        let stdlib_modules = beacon_analyzer::EMBEDDED_STDLIB_MODULES;
        for module_name in stdlib_modules.iter().copied() {
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

        let stdlib_modules = beacon_analyzer::EMBEDDED_STDLIB_MODULES;
        for module_name in stdlib_modules.iter().copied() {
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

    #[test]
    fn test_discover_files_with_custom_exclude_patterns() {
        let temp_dir = TempDir::new().unwrap();
        let root_path = temp_dir.path();

        fs::create_dir_all(root_path.join("src")).unwrap();
        fs::create_dir_all(root_path.join("tests")).unwrap();
        fs::create_dir_all(root_path.join("build")).unwrap();
        fs::create_dir_all(root_path.join("venv")).unwrap();

        fs::write(root_path.join("src/main.py"), "# main").unwrap();
        fs::write(root_path.join("tests/test_main.py"), "# test").unwrap();
        fs::write(root_path.join("build/generated.py"), "# build").unwrap();
        fs::write(root_path.join("venv/lib.py"), "# venv").unwrap();

        let config = Config {
            exclude_patterns: vec!["**/build/**".to_string(), "**/venv/**".to_string()],
            ..Default::default()
        };
        let documents = DocumentManager::new().unwrap();
        let root_uri = Url::from_directory_path(root_path).unwrap();
        let mut workspace = Workspace::new(Some(root_uri), config, documents);

        workspace.discover_files().unwrap();

        let indexed_files = workspace.all_indexed_files();
        let file_paths: Vec<String> = indexed_files.iter().map(|uri| uri.path().to_string()).collect();

        assert!(
            file_paths.iter().any(|p| p.contains("src/main.py")),
            "src/main.py should be indexed"
        );
        assert!(
            file_paths.iter().any(|p| p.contains("tests/test_main.py")),
            "tests/test_main.py should be indexed"
        );
        assert!(
            !file_paths.iter().any(|p| p.contains("build/generated.py")),
            "build/generated.py should be excluded"
        );
        assert!(
            !file_paths.iter().any(|p| p.contains("venv/lib.py")),
            "venv/lib.py should be excluded"
        );
    }

    #[test]
    fn test_discover_files_default_excludes_venv() {
        let temp_dir = TempDir::new().unwrap();
        let root_path = temp_dir.path();

        fs::create_dir_all(root_path.join("src")).unwrap();
        fs::create_dir_all(root_path.join(".venv/lib/python3.12")).unwrap();
        fs::create_dir_all(root_path.join("venv/lib")).unwrap();

        fs::write(root_path.join("src/app.py"), "# app").unwrap();
        fs::write(root_path.join(".venv/lib/python3.12/site.py"), "# site").unwrap();
        fs::write(root_path.join("venv/lib/foo.py"), "# foo").unwrap();

        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let root_uri = Url::from_directory_path(root_path).unwrap();
        let mut workspace = Workspace::new(Some(root_uri), config, documents);

        workspace.discover_files().unwrap();

        let indexed_files = workspace.all_indexed_files();
        let file_paths: Vec<String> = indexed_files.iter().map(|uri| uri.path().to_string()).collect();

        assert!(
            file_paths.iter().any(|p| p.contains("src/app.py")),
            "src/app.py should be indexed"
        );
        assert!(
            !file_paths.iter().any(|p| p.contains(".venv")),
            ".venv files should be excluded by default"
        );
        assert!(
            !file_paths.iter().any(|p| p.contains("venv/lib/foo.py")),
            "venv files should be excluded by default"
        );
    }

    #[test]
    fn test_discover_files_excludes_cache_directories() {
        let temp_dir = TempDir::new().unwrap();
        let root_path = temp_dir.path();

        fs::create_dir_all(root_path.join("src")).unwrap();
        fs::create_dir_all(root_path.join("__pycache__")).unwrap();
        fs::create_dir_all(root_path.join(".mypy_cache")).unwrap();
        fs::create_dir_all(root_path.join(".pytest_cache")).unwrap();

        fs::write(root_path.join("src/module.py"), "# module").unwrap();
        fs::write(root_path.join("__pycache__/module.cpython-312.pyc"), "# compiled").unwrap();
        fs::write(root_path.join(".mypy_cache/cache.py"), "# cache").unwrap();
        fs::write(root_path.join(".pytest_cache/data.py"), "# pytest").unwrap();

        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let root_uri = Url::from_directory_path(root_path).unwrap();
        let mut workspace = Workspace::new(Some(root_uri), config, documents);

        workspace.discover_files().unwrap();

        let indexed_files = workspace.all_indexed_files();
        let file_paths: Vec<String> = indexed_files.iter().map(|uri| uri.path().to_string()).collect();

        assert!(
            file_paths.iter().any(|p| p.contains("src/module.py")),
            "src/module.py should be indexed"
        );
        assert!(
            !file_paths.iter().any(|p| p.contains("__pycache__")),
            "__pycache__ should be excluded"
        );
        assert!(
            !file_paths.iter().any(|p| p.contains(".mypy_cache")),
            ".mypy_cache should be excluded"
        );
        assert!(
            !file_paths.iter().any(|p| p.contains(".pytest_cache")),
            ".pytest_cache should be excluded"
        );
    }

    #[test]
    fn test_exclude_patterns_normalization() {
        let temp_dir = TempDir::new().unwrap();
        let root_path = temp_dir.path();

        fs::create_dir_all(root_path.join("src")).unwrap();
        fs::create_dir_all(root_path.join("dist")).unwrap();
        fs::create_dir_all(root_path.join("node_modules")).unwrap();

        fs::write(root_path.join("src/app.py"), "# app").unwrap();
        fs::write(root_path.join("dist/bundle.py"), "# bundle").unwrap();
        fs::write(root_path.join("node_modules/pkg.py"), "# pkg").unwrap();

        let config =
            Config { exclude_patterns: vec!["dist/".to_string(), "!node_modules/".to_string()], ..Default::default() };
        let documents = DocumentManager::new().unwrap();
        let root_uri = Url::from_directory_path(root_path).unwrap();
        let mut workspace = Workspace::new(Some(root_uri), config, documents);

        workspace.discover_files().unwrap();

        let indexed_files = workspace.all_indexed_files();
        let file_paths: Vec<String> = indexed_files.iter().map(|uri| uri.path().to_string()).collect();

        assert!(
            file_paths.iter().any(|p| p.contains("src/app.py")),
            "src/app.py should be indexed"
        );
        assert!(
            !file_paths.iter().any(|p| p.contains("dist/bundle.py")),
            "dist/bundle.py should be excluded (pattern without !)"
        );
        assert!(
            !file_paths.iter().any(|p| p.contains("node_modules/pkg.py")),
            "node_modules/pkg.py should be excluded (pattern with !)"
        );
    }

    #[test]
    fn test_extract_all_exports() {
        let temp_dir = TempDir::new().unwrap();
        let test_file = temp_dir.path().join("test_module.py");

        let source_code = r#"
def foo():
    pass

def bar():
    pass

__all__ = ["foo", "bar", "baz"]
"#;

        fs::write(&test_file, source_code).unwrap();

        let uri = Url::from_file_path(&test_file).unwrap();
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        documents
            .open_document(uri.clone(), 1, source_code.to_string())
            .unwrap();
        let workspace = Workspace::new(Some(uri.clone()), config, documents);

        let all_exports = workspace.extract_all_exports(&uri);
        assert!(all_exports.is_some());
        let exports = all_exports.unwrap();
        assert_eq!(exports.len(), 3);
        assert!(exports.contains(&"foo".to_string()));
        assert!(exports.contains(&"bar".to_string()));
        assert!(exports.contains(&"baz".to_string()));
    }

    #[test]
    fn test_extract_all_exports_empty() {
        let temp_dir = TempDir::new().unwrap();
        let test_file = temp_dir.path().join("test_module.py");

        let source_code = r#"
def foo():
    pass
"#;

        fs::write(&test_file, source_code).unwrap();

        let uri = Url::from_file_path(&test_file).unwrap();
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        documents
            .open_document(uri.clone(), 1, source_code.to_string())
            .unwrap();
        let workspace = Workspace::new(Some(uri.clone()), config, documents);

        let all_exports = workspace.extract_all_exports(&uri);
        assert!(all_exports.is_none());
    }

    #[test]
    fn test_get_module_symbols() {
        let temp_dir = TempDir::new().unwrap();
        let test_file = temp_dir.path().join("test_module.py");

        let source_code = r#"
import os

def foo():
    pass

class Bar:
    pass

my_var = 42
"#;

        fs::write(&test_file, source_code).unwrap();

        let uri = Url::from_file_path(&test_file).unwrap();
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        documents
            .open_document(uri.clone(), 1, source_code.to_string())
            .unwrap();
        let workspace = Workspace::new(Some(uri.clone()), config, documents);

        let symbols = workspace.get_module_symbols(&uri);
        assert!(symbols.contains("foo"));
        assert!(symbols.contains("Bar"));
        assert!(symbols.contains("my_var"));
        assert!(symbols.contains("os"));
    }

    #[test]
    fn test_inconsistent_export_detection() {
        let temp_dir = TempDir::new().unwrap();
        let test_file = temp_dir.path().join("test_module.py");

        let source_code = r#"
def foo():
    pass

def bar():
    pass

__all__ = ["foo", "baz"]
"#;

        fs::write(&test_file, source_code).unwrap();

        let file_uri = Url::from_file_path(&test_file).unwrap();
        let root_uri = Url::from_directory_path(temp_dir.path()).unwrap();
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        documents
            .open_document(file_uri.clone(), 1, source_code.to_string())
            .unwrap();
        let mut workspace = Workspace::new(Some(root_uri), config, documents);

        workspace.initialize().unwrap();

        let all_exports = workspace.get_all_exports(&file_uri);
        assert!(
            all_exports.is_some(),
            "all_exports should be populated after initialize"
        );

        let exports = all_exports.unwrap();
        let module_symbols = workspace.get_module_symbols(&file_uri);

        assert!(exports.contains(&"foo".to_string()));
        assert!(exports.contains(&"baz".to_string()));
        assert!(module_symbols.contains("foo"));
        assert!(module_symbols.contains("bar"));
        assert!(!module_symbols.contains("baz"));
    }

    #[test]
    fn test_conflicting_stub_definitions() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let workspace = Workspace::new(None, config, documents);

        let stub1_content = r#"
def my_function(x: int) -> str: ...
class MyClass: ...
my_var: int
"#;

        let stub2_content = r#"
def my_function(x: str) -> int: ...
class MyClass: ...
my_var: str
"#;

        let stub1 = workspace.parse_stub_from_string("testmodule", stub1_content).unwrap();
        let stub2 = workspace.parse_stub_from_string("testmodule", stub2_content).unwrap();

        let mut conflicts: FxHashMap<String, Vec<Type>> = FxHashMap::default();

        for symbol_name in stub1.exports.keys() {
            if let (Some(ty1), Some(ty2)) = (stub1.exports.get(symbol_name), stub2.exports.get(symbol_name)) {
                if ty1 != ty2 {
                    conflicts.insert(symbol_name.clone(), vec![ty1.clone(), ty2.clone()]);
                }
            }
        }

        assert!(
            conflicts.contains_key("my_function"),
            "Should detect conflicting signatures for my_function"
        );

        assert!(
            conflicts.contains_key("my_var"),
            "Should detect conflicting types for my_var"
        );

        assert!(
            !conflicts.contains_key("MyClass"),
            "MyClass should not have conflicts (same type in both)"
        );
    }
}

//! Workspace management for multi-file analysis
//!
//! Handles:
//! - Dependency graph between modules
//! - Module resolution and imports
//! - Project-wide type checking
//! - TODO: Stub file discovery and loading (.pyi files)

use crate::config::Config;
use crate::document::DocumentManager;
use rayon::prelude::*;
use rustc_hash::{FxHashMap, FxHashSet};
use std::collections::VecDeque;
use std::path::{Path, PathBuf};
use url::Url;

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
    fn new(uri: Url, module_name: String, source_root: PathBuf, is_package: bool) -> Self {
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

    fn _iter(&self) -> impl Iterator<Item = (&Url, &ModuleInfo)> {
        self.modules.iter()
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

    /// Loaded stub files
    _stubs: StubCache,
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
            _stubs: StubCache::new(),
        }
    }

    /// Initialize workspace by discovering Python files and stubs
    ///
    /// Scans the workspace for all Python files, builds the module index, and constructs the initial dependency graph.
    pub fn initialize(&mut self) -> Result<(), WorkspaceError> {
        self.discover_files()?;
        self.build_dependency_graph();

        // TODO: Load stub files from config.stub_paths
        Ok(())
    }

    /// Build the dependency graph by extracting imports from all indexed modules
    ///
    /// Uses [rayon] for parallel processing of files.
    /// TODO: Report unresolved imports as diagnostics later
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
        use beacon_parser::AstNode;

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
    fn discover_files(&mut self) -> Result<(), WorkspaceError> {
        let root_path = match &self.root_uri {
            Some(uri) if uri.scheme() == "file" => PathBuf::from(uri.path()),
            _ => return Ok(()),
        };

        if !root_path.exists() || !root_path.is_dir() {
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

        // TODO: Make these configurable via Config.exclude_patterns
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
    /// TODO: Make source_roots configurable via Config
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
    /// For relative imports, use `resolve_relative_import` instead.
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

    /// Load stub file for a module
    ///
    /// TODO: Implement .pyi loading and parsing
    pub fn _load_stub(&mut self, _module_name: &str) -> Option<StubFile> {
        None
    }

    /// Get configuration
    pub fn config(&self) -> &Config {
        &self.config
    }

    /// Get document manager
    pub fn documents(&self) -> &DocumentManager {
        &self.documents
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
    /// Called when a module is deleted or being re-analyzed.
    /// Also cleans up reverse edges.
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

        // Also remove any reverse edges pointing to this module
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
/// TODO: Implement LRU cache for stub files
struct StubCache {
    _cache: FxHashMap<String, StubFile>,
}

impl StubCache {
    fn new() -> Self {
        Self { _cache: FxHashMap::default() }
    }

    /// TODO: Load and parse a .pyi stub file
    fn _load(&mut self, _path: &Path) -> Option<StubFile> {
        None
    }

    /// TODO: Get stub for a module
    fn _get(&self, _module_name: &str) -> Option<&StubFile> {
        None
    }
}

/// Parsed stub file (.pyi)
///
/// TODO: Define structure for stub file contents
#[derive(Debug, Clone)]
pub struct StubFile {
    /// Module name
    pub _module: String,
    // TODO: Define proper structure for exported types/signatures
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
}

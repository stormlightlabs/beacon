use beacon_parser::ScopeId;
use rustc_hash::FxHashMap;
use url::Url;

use super::call_graph::{CallGraph, CallSite, FunctionId};
use super::graph::{BlockId, ControlFlowGraph};

/// Module-level CFG containing all functions and module initialization
///
/// Represents the control flow for an entire Python module, including module-level statements and all function definitions.
#[derive(Debug, Clone)]
pub struct ModuleCFG {
    /// URI of the module file
    pub uri: Url,
    /// Module name (e.g., "foo.bar")
    pub module_name: String,
    /// CFG for module-level initialization code
    pub module_init_cfg: ControlFlowGraph,
    /// Per-function CFGs indexed by scope ID
    pub function_cfgs: FxHashMap<ScopeId, ControlFlowGraph>,
    /// Function names indexed by scope ID
    function_names: FxHashMap<ScopeId, String>,
    /// All call sites in this module
    pub call_sites: Vec<CallSite>,
}

impl ModuleCFG {
    pub fn new(uri: Url, module_name: String) -> Self {
        Self {
            uri,
            module_name,
            module_init_cfg: ControlFlowGraph::new(),
            function_cfgs: FxHashMap::default(),
            function_names: FxHashMap::default(),
            call_sites: Vec::new(),
        }
    }

    /// Add a function CFG to this module
    pub fn add_function_cfg(&mut self, scope_id: ScopeId, name: String, cfg: ControlFlowGraph) {
        self.function_cfgs.insert(scope_id, cfg);
        self.function_names.insert(scope_id, name);
    }

    /// Add a call site to this module
    pub fn add_call_site(&mut self, call_site: CallSite) {
        self.call_sites.push(call_site);
    }

    /// Get all function IDs defined in this module
    pub fn function_ids(&self) -> Vec<FunctionId> {
        self.function_cfgs
            .keys()
            .map(|scope_id| {
                let name = self
                    .function_names
                    .get(scope_id)
                    .cloned()
                    .unwrap_or_else(|| format!("func_{scope_id:?}"));
                FunctionId::new(self.uri.clone(), *scope_id, name)
            })
            .collect()
    }

    /// Get a function CFG by scope ID
    pub fn get_function_cfg(&self, scope_id: ScopeId) -> Option<&ControlFlowGraph> {
        self.function_cfgs.get(&scope_id)
    }
}

/// Workspace-wide CFG coordinating all modules and the call graph
///
/// Top-level container for cross-module CFG analysis, maintaining all module CFGs and the workspace-wide call graph.
#[derive(Debug, Clone)]
pub struct WorkspaceCFG {
    /// All module CFGs indexed by URI
    modules: FxHashMap<Url, ModuleCFG>,
    /// Workspace-wide call graph
    call_graph: CallGraph,
    /// Entry point functions for whole-program analysis
    entry_points: Vec<FunctionId>,
}

impl WorkspaceCFG {
    pub fn new() -> Self {
        Self { modules: FxHashMap::default(), call_graph: CallGraph::new(), entry_points: Vec::new() }
    }

    /// Add a module CFG to the workspace
    pub fn add_module(&mut self, module_cfg: ModuleCFG) {
        self.modules.insert(module_cfg.uri.clone(), module_cfg);
    }

    /// Get a module CFG by URI
    pub fn get_module(&self, uri: &Url) -> Option<&ModuleCFG> {
        self.modules.get(uri)
    }

    /// Get a mutable reference to a module CFG
    pub fn get_module_mut(&mut self, uri: &Url) -> Option<&mut ModuleCFG> {
        self.modules.get_mut(uri)
    }

    /// Get the call graph
    pub fn call_graph(&self) -> &CallGraph {
        &self.call_graph
    }

    /// Get mutable reference to call graph
    pub fn call_graph_mut(&mut self) -> &mut CallGraph {
        &mut self.call_graph
    }

    /// Add an entry point function
    pub fn add_entry_point(&mut self, function_id: FunctionId) {
        self.entry_points.push(function_id);
    }

    /// Clear all entry points
    pub fn clear_entry_points(&mut self) {
        self.entry_points.clear();
    }

    /// Get all entry points
    pub fn entry_points(&self) -> &[FunctionId] {
        &self.entry_points
    }

    /// Compute all reachable functions from entry points
    pub fn reachable_functions(&self) -> Vec<FunctionId> {
        self.call_graph.reachable_functions(&self.entry_points)
    }

    /// Find all unreachable (dead) functions in the workspace
    ///
    /// Returns functions that are never called from any entry point.
    /// Excludes special methods and private functions (starting with _).
    pub fn unreachable_functions(&self) -> Vec<FunctionId> {
        let reachable = self
            .reachable_functions()
            .into_iter()
            .collect::<rustc_hash::FxHashSet<_>>();

        let mut all_functions = rustc_hash::FxHashSet::default();
        for module_cfg in self.modules.values() {
            for func_id in module_cfg.function_ids() {
                all_functions.insert(func_id.clone());
            }
        }

        all_functions
            .into_iter()
            .filter(|func| {
                if reachable.contains(func) {
                    return false;
                }

                if func.name.starts_with('_') {
                    return false;
                }

                true
            })
            .collect()
    }

    /// Perform cross-module reachability analysis
    ///
    /// Returns all blocks reachable from the given entry points, traversing across function and module boundaries.
    pub fn cross_module_reachable_blocks(&self, entry_points: &[FunctionId]) -> FxHashMap<FunctionId, Vec<BlockId>> {
        let mut reachable = FxHashMap::default();
        let reachable_functions = self.call_graph.reachable_functions(entry_points);

        for function_id in reachable_functions {
            if let Some(module) = self.modules.get(&function_id.uri)
                && let Some(cfg) = module.function_cfgs.get(&function_id.scope_id)
            {
                let blocks = cfg.reachable_blocks();
                reachable.insert(function_id, blocks);
            }
        }

        reachable
    }

    pub fn modules(&self) -> FxHashMap<Url, ModuleCFG> {
        self.modules.clone()
    }

    pub fn entry_point_fns(self) -> Vec<FunctionId> {
        self.entry_points
    }

    /// Link import stubs to actual function definitions across modules
    ///
    /// When CFG is built, calls to imported functions create FunctionIds pointing
    /// to the import symbol in the importing module. This method resolves those
    /// references to point to the actual function definitions in the target modules.
    ///
    /// The `import_map` maps (source_module_uri, symbol_name) -> target FunctionId.
    /// This allows the call graph to correctly track cross-module dependencies
    /// and detect circular dependencies between modules.
    pub fn link_cross_module_calls(&mut self, import_map: &FxHashMap<(Url, String), FunctionId>) {
        let mut relinks: Vec<(FunctionId, FunctionId, FunctionId)> = Vec::new();

        for (caller_fn, call_sites) in self.call_graph.all_call_sites() {
            for call_site in call_sites {
                if let Some(callee) = &call_site.receiver {
                    let key = (callee.uri.clone(), callee.name.clone());
                    if let Some(target_fn) = import_map.get(&key)
                        && target_fn != callee
                    {
                        relinks.push((caller_fn.clone(), callee.clone(), target_fn.clone()));
                    }
                }
            }
        }

        for (caller, old_callee, new_callee) in relinks {
            self.call_graph.relink_callee(&caller, &old_callee, new_callee);
        }
    }

    /// Build a summary of the workspace CFG for debugging
    pub fn debug_summary(&self) -> WorkspaceCfgSummary {
        let mut functions = Vec::new();
        let mut call_edges = Vec::new();

        for (uri, module) in &self.modules {
            for func_id in module.function_ids() {
                functions.push(FunctionSummary {
                    module: module.module_name.clone(),
                    name: func_id.name.clone(),
                    uri: uri.clone(),
                });
            }
        }

        for (caller, call_sites) in self.call_graph.all_call_sites() {
            for call_site in call_sites {
                if let Some(callee) = &call_site.receiver {
                    call_edges.push(CallEdgeSummary {
                        caller_module: caller.uri.path().to_string(),
                        caller_name: caller.name.clone(),
                        callee_module: callee.uri.path().to_string(),
                        callee_name: callee.name.clone(),
                        line: call_site.line,
                    });
                }
            }
        }

        WorkspaceCfgSummary {
            module_count: self.modules.len(),
            function_count: functions.len(),
            call_edge_count: call_edges.len(),
            has_circular_deps: self.call_graph.has_circular_dependencies(),
            functions,
            call_edges,
        }
    }
}

/// Summary of workspace CFG for debugging output
#[derive(Debug, Clone)]
pub struct WorkspaceCfgSummary {
    pub module_count: usize,
    pub function_count: usize,
    pub call_edge_count: usize,
    pub has_circular_deps: bool,
    pub functions: Vec<FunctionSummary>,
    pub call_edges: Vec<CallEdgeSummary>,
}

/// Summary of a function for debugging output
#[derive(Debug, Clone)]
pub struct FunctionSummary {
    pub module: String,
    pub name: String,
    pub uri: Url,
}

/// Summary of a call edge for debugging output
#[derive(Debug, Clone)]
pub struct CallEdgeSummary {
    pub caller_module: String,
    pub caller_name: String,
    pub callee_module: String,
    pub callee_name: String,
    pub line: usize,
}
impl Default for WorkspaceCFG {
    fn default() -> Self {
        Self::new()
    }
}

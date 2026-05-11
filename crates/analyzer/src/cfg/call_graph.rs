use beacon_parser::ScopeId;
use rustc_hash::FxHashMap;
use std::collections::VecDeque;
use url::Url;

use super::graph::BlockId;

/// Unique identifier for a function across the workspace
///
/// Combines file URI and scope ID to uniquely identify functions across multiple modules.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionId {
    /// URI of the file containing this function
    pub uri: Url,
    /// Scope ID from the symbol table
    pub scope_id: ScopeId,
    /// Function name for debugging
    pub name: String,
}

impl FunctionId {
    pub fn new(uri: Url, scope_id: ScopeId, name: String) -> Self {
        Self { uri, scope_id, name }
    }
}

/// Type of function call
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CallKind {
    /// Direct function call (e.g., foo())
    Direct,
    /// Method call (e.g., obj.method())
    Method,
    /// Dynamic call (e.g., callable_var())
    Dynamic,
}

/// Node tracking information for constraint generation
#[derive(Debug, Clone, Default)]
pub struct NodeTracking {
    /// Node ID of the call expression (for constraint generation)
    pub call_node_id: Option<usize>,
    /// Node IDs of positional arguments (for constraint generation)
    pub arg_node_ids: Vec<usize>,
    /// Node IDs of keyword arguments as (name, node_id) pairs (for constraint generation)
    pub kwarg_node_ids: Vec<(String, usize)>,
    /// Node ID where the result is stored (for constraint generation)
    pub result_node_id: Option<usize>,
}

/// A call site within a basic block
///
/// Represents a function or method invocation that creates an inter-procedural edge in the call graph.
#[derive(Debug, Clone)]
pub struct CallSite {
    /// Block containing this call
    pub block_id: BlockId,
    /// Statement index within the block
    pub stmt_index: usize,
    /// Resolved callee function (None if unresolved or dynamic)
    pub receiver: Option<FunctionId>,
    /// Type of call
    pub kind: CallKind,
    /// Source location for diagnostics
    pub line: usize,
    pub col: usize,
    /// Node ID of the call expression (for constraint generation)
    pub call_node_id: Option<usize>,
    /// Node IDs of positional arguments (for constraint generation)
    pub arg_node_ids: Vec<usize>,
    /// Node IDs of keyword arguments as (name, node_id) pairs (for constraint generation)
    pub kwarg_node_ids: Vec<(String, usize)>,
    /// Node ID where the result is stored (for constraint generation)
    pub result_node_id: Option<usize>,
}

impl CallSite {
    pub fn new(id: BlockId, index: usize, rec: Option<FunctionId>, kind: CallKind, ln: usize, col: usize) -> Self {
        Self {
            block_id: id,
            stmt_index: index,
            receiver: rec,
            kind,
            line: ln,
            col,
            call_node_id: None,
            arg_node_ids: Vec::new(),
            kwarg_node_ids: Vec::new(),
            result_node_id: None,
        }
    }

    /// Create a CallSite with node tracking for constraint generation
    pub fn with_nodes(
        id: BlockId, index: usize, rec: Option<FunctionId>, kind: CallKind, ln: usize, col: usize, nodes: NodeTracking,
    ) -> Self {
        Self {
            block_id: id,
            stmt_index: index,
            receiver: rec,
            kind,
            line: ln,
            col,
            call_node_id: nodes.call_node_id,
            arg_node_ids: nodes.arg_node_ids,
            kwarg_node_ids: nodes.kwarg_node_ids,
            result_node_id: nodes.result_node_id,
        }
    }
}

/// Call graph mapping functions to their call sites and callees
///
/// Maintains both forward edges (function -> callees) and
/// reverse edges (function -> callers) for efficient traversal.
#[derive(Debug, Clone)]
pub struct CallGraph {
    /// Maps each function to the call sites it contains
    call_sites: FxHashMap<FunctionId, Vec<CallSite>>,
    /// Reverse edges: maps each function to its callers
    callers: FxHashMap<FunctionId, Vec<FunctionId>>,
}

impl CallGraph {
    pub fn new() -> Self {
        Self { call_sites: FxHashMap::default(), callers: FxHashMap::default() }
    }

    /// Get a reference to all call sites
    pub fn all_call_sites(&self) -> &FxHashMap<FunctionId, Vec<CallSite>> {
        &self.call_sites
    }

    /// Add a call site for a function
    pub fn add_call_site(&mut self, caller: FunctionId, call_site: &CallSite) {
        self.call_sites
            .entry(caller.clone())
            .or_default()
            .push(call_site.clone());

        if let Some(callee) = &call_site.receiver {
            self.callers.entry(callee.clone()).or_default().push(caller);
        }
    }

    /// Get all call sites for a function
    pub fn get_call_sites(&self, function: &FunctionId) -> Option<&Vec<CallSite>> {
        self.call_sites.get(function)
    }

    /// Get all callers of a function
    pub fn get_callers(&self, function: &FunctionId) -> Option<&Vec<FunctionId>> {
        self.callers.get(function)
    }

    /// Get all callees for a function
    pub fn get_callees(&self, function: &FunctionId) -> Vec<FunctionId> {
        self.call_sites
            .get(function)
            .map(|sites| sites.iter().filter_map(|site| site.receiver.clone()).collect())
            .unwrap_or_default()
    }

    /// Compute reachable functions from a set of entry points
    ///
    /// Uses BFS to find all functions transitively called from the given entry points.
    /// This method handles circular dependencies gracefully by tracking visited nodes.
    pub fn reachable_functions(&self, entry_points: &[FunctionId]) -> Vec<FunctionId> {
        let mut visited = rustc_hash::FxHashSet::default();
        let mut queue = VecDeque::new();

        for entry in entry_points {
            if visited.insert(entry.clone()) {
                queue.push_back(entry.clone());
            }
        }

        while let Some(function) = queue.pop_front() {
            for callee in self.get_callees(&function) {
                if visited.insert(callee.clone()) {
                    queue.push_back(callee);
                }
            }
        }

        visited.into_iter().collect()
    }

    /// Detect strongly connected components (SCCs) using Tarjan's algorithm
    ///
    /// Returns a vector of SCCs, where each SCC is a vector of FunctionIds.
    /// Functions in the same SCC are mutually recursive (can reach each other).
    /// SCCs are returned in reverse topological order.
    pub fn strongly_connected_components(&self) -> Vec<Vec<FunctionId>> {
        let mut tarjan = TarjanCallState::new(self);
        tarjan.run()
    }

    /// Check if there are any circular dependencies in the call graph
    pub fn has_circular_dependencies(&self) -> bool {
        self.strongly_connected_components().iter().any(|scc| scc.len() > 1)
    }

    /// Get all functions involved in circular dependencies
    pub fn circular_dependency_functions(&self) -> rustc_hash::FxHashSet<FunctionId> {
        self.strongly_connected_components()
            .into_iter()
            .filter(|scc| scc.len() > 1)
            .flatten()
            .collect()
    }

    /// Get all functions in the call graph
    fn all_functions(&self) -> rustc_hash::FxHashSet<FunctionId> {
        let mut functions = rustc_hash::FxHashSet::default();

        for function in self.call_sites.keys() {
            functions.insert(function.clone());
        }

        for function in self.callers.keys() {
            functions.insert(function.clone());
        }

        functions
    }

    /// Replace a callee reference in call sites for cross-module linking
    ///
    /// Updates the caller's call sites to point to the new callee, and updates
    /// the callers map accordingly.
    pub fn relink_callee(&mut self, caller: &FunctionId, old_callee: &FunctionId, new_callee: FunctionId) {
        if let Some(sites) = self.call_sites.get_mut(caller) {
            for site in sites.iter_mut() {
                if site.receiver.as_ref() == Some(old_callee) {
                    site.receiver = Some(new_callee.clone());
                }
            }
        }

        if let Some(callers) = self.callers.get_mut(old_callee) {
            callers.retain(|c| c != caller);
        }

        self.callers.entry(new_callee).or_default().push(caller.clone());
    }
}

impl Default for CallGraph {
    fn default() -> Self {
        Self::new()
    }
}

/// Tarjan's algorithm for finding strongly connected components
///
/// Implements Tarjan's SCC algorithm to detect cycles and strongly connected components in the call graph.
/// This is used to identify mutually recursive functions and handle circular dependencies gracefully.
struct TarjanCallState<'a> {
    graph: &'a CallGraph,
    index_counter: usize,
    stack: Vec<FunctionId>,
    on_stack: rustc_hash::FxHashSet<FunctionId>,
    indices: FxHashMap<FunctionId, usize>,
    lowlinks: FxHashMap<FunctionId, usize>,
    sccs: Vec<Vec<FunctionId>>,
}

impl<'a> TarjanCallState<'a> {
    fn new(graph: &'a CallGraph) -> Self {
        Self {
            graph,
            index_counter: 0,
            stack: Vec::new(),
            on_stack: rustc_hash::FxHashSet::default(),
            indices: FxHashMap::default(),
            lowlinks: FxHashMap::default(),
            sccs: Vec::new(),
        }
    }

    fn run(&mut self) -> Vec<Vec<FunctionId>> {
        let functions: Vec<FunctionId> = self.graph.all_functions().into_iter().collect();

        for function in functions {
            if !self.indices.contains_key(&function) {
                self.strong_connect(&function);
            }
        }

        std::mem::take(&mut self.sccs)
    }

    fn strong_connect(&mut self, v: &FunctionId) {
        self.indices.insert(v.clone(), self.index_counter);
        self.lowlinks.insert(v.clone(), self.index_counter);
        self.index_counter += 1;

        self.stack.push(v.clone());
        self.on_stack.insert(v.clone());

        for w in self.graph.get_callees(v) {
            if !self.indices.contains_key(&w) {
                self.strong_connect(&w.clone());
                let w_lowlink = *self.lowlinks.get(&w).unwrap();
                let v_lowlink = self.lowlinks.get_mut(v).unwrap();
                *v_lowlink = (*v_lowlink).min(w_lowlink);
            } else if self.on_stack.contains(&w) {
                let w_index = *self.indices.get(&w).unwrap();
                let v_lowlink = self.lowlinks.get_mut(v).unwrap();
                *v_lowlink = (*v_lowlink).min(w_index);
            }
        }

        if self.lowlinks.get(v) == self.indices.get(v) {
            let mut scc = Vec::new();
            loop {
                let w = self.stack.pop().unwrap();
                self.on_stack.remove(&w);
                scc.push(w.clone());
                if w == *v {
                    break;
                }
            }
            self.sccs.push(scc);
        }
    }
}

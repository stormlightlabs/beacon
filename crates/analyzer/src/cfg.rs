//! Control Flow Graph (CFG) construction and analysis
//!
//! Represents the control flow structure of Python functions for static analysis.
//! Each function gets its own CFG with basic blocks connected by edges.
//!
//! Cross-module CFG construction enables workspace-wide analysis including:
//! - Inter-function call graph
//! - Cross-file reachability analysis
//! - Transitive type propagation across module boundaries

use beacon_parser::{AstNode, ScopeId};
use rustc_hash::FxHashMap;
use std::collections::VecDeque;
use url::Url;

/// Unique identifier for basic blocks
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BlockId(pub usize);

/// Type of control flow edge between blocks
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EdgeKind {
    /// Normal sequential flow
    Normal,
    /// True branch of conditional
    True,
    /// False branch of conditional
    False,
    /// Exception edge (from try to except handler)
    Exception,
    /// Break statement edge (to loop exit)
    Break,
    /// Continue statement edge (to loop header)
    Continue,
    /// Finally edge (always executed)
    Finally,
}

/// A basic block in the control flow graph
///
/// Contains a sequence of statements that execute sequentially without branches.
#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub id: BlockId,
    /// Statements in this block (indices into the function body)
    pub statements: Vec<usize>,
    /// Outgoing edges from this block
    pub successors: Vec<(BlockId, EdgeKind)>,
    /// Incoming edges to this block
    pub predecessors: Vec<(BlockId, EdgeKind)>,
}

impl BasicBlock {
    fn new(id: BlockId) -> Self {
        Self { id, statements: Vec::new(), successors: Vec::new(), predecessors: Vec::new() }
    }
}

/// Control Flow Graph for a function
#[derive(Debug, Clone)]
pub struct ControlFlowGraph {
    pub blocks: FxHashMap<BlockId, BasicBlock>,
    pub entry: BlockId,
    pub exit: BlockId,
    next_block_id: usize,
}

impl Default for ControlFlowGraph {
    fn default() -> Self {
        let entry = BlockId(0);
        let exit = BlockId(1);
        let mut blocks = FxHashMap::default();

        blocks.insert(entry, BasicBlock::new(entry));
        blocks.insert(exit, BasicBlock::new(exit));

        Self { blocks, entry, exit, next_block_id: 2 }
    }
}

impl ControlFlowGraph {
    pub fn new() -> Self {
        Self::default()
    }

    fn new_block(&mut self) -> BlockId {
        let id = BlockId(self.next_block_id);
        self.next_block_id += 1;
        self.blocks.insert(id, BasicBlock::new(id));
        id
    }

    fn add_edge(&mut self, from: BlockId, to: BlockId, kind: EdgeKind) {
        if let Some(from_block) = self.blocks.get_mut(&from) {
            from_block.successors.push((to, kind));
        }
        if let Some(to_block) = self.blocks.get_mut(&to) {
            to_block.predecessors.push((from, kind));
        }
    }

    /// Get all reachable blocks from entry
    pub fn reachable_blocks(&self) -> Vec<BlockId> {
        let mut visited = rustc_hash::FxHashSet::default();
        let mut queue = VecDeque::new();
        queue.push_back(self.entry);
        visited.insert(self.entry);

        while let Some(block_id) = queue.pop_front() {
            if let Some(block) = self.blocks.get(&block_id) {
                for &(succ_id, _) in &block.successors {
                    if visited.insert(succ_id) {
                        queue.push_back(succ_id);
                    }
                }
            }
        }

        visited.into_iter().collect()
    }

    /// Get unreachable blocks (dead code)
    pub fn unreachable_blocks(&self) -> Vec<BlockId> {
        let reachable = self
            .reachable_blocks()
            .into_iter()
            .collect::<rustc_hash::FxHashSet<_>>();
        self.blocks
            .keys()
            .filter(|id| !reachable.contains(id))
            .copied()
            .collect()
    }
}

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
        id: BlockId, index: usize, rec: Option<FunctionId>, kind: CallKind, ln: usize, col: usize,
        call_node_id: Option<usize>, arg_node_ids: Vec<usize>, kwarg_node_ids: Vec<(String, usize)>,
        result_node_id: Option<usize>,
    ) -> Self {
        Self {
            block_id: id,
            stmt_index: index,
            receiver: rec,
            kind,
            line: ln,
            col,
            call_node_id,
            arg_node_ids,
            kwarg_node_ids,
            result_node_id,
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
    pub fn add_call_site(&mut self, caller: FunctionId, call_site: CallSite) {
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
                self.strong_connect(function);
            }
        }

        std::mem::take(&mut self.sccs)
    }

    fn strong_connect(&mut self, v: FunctionId) {
        self.indices.insert(v.clone(), self.index_counter);
        self.lowlinks.insert(v.clone(), self.index_counter);
        self.index_counter += 1;

        self.stack.push(v.clone());
        self.on_stack.insert(v.clone());

        for w in self.graph.get_callees(&v) {
            if !self.indices.contains_key(&w) {
                self.strong_connect(w.clone());
                let w_lowlink = *self.lowlinks.get(&w).unwrap();
                let v_lowlink = self.lowlinks.get_mut(&v).unwrap();
                *v_lowlink = (*v_lowlink).min(w_lowlink);
            } else if self.on_stack.contains(&w) {
                let w_index = *self.indices.get(&w).unwrap();
                let v_lowlink = self.lowlinks.get_mut(&v).unwrap();
                *v_lowlink = (*v_lowlink).min(w_index);
            }
        }

        if self.lowlinks.get(&v) == self.indices.get(&v) {
            let mut scc = Vec::new();
            loop {
                let w = self.stack.pop().unwrap();
                self.on_stack.remove(&w);
                scc.push(w.clone());
                if w == v {
                    break;
                }
            }
            self.sccs.push(scc);
        }
    }
}

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
                    .unwrap_or_else(|| format!("func_{:?}", scope_id));
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

    /// Get all entry points
    pub fn entry_points(&self) -> &[FunctionId] {
        &self.entry_points
    }

    /// Compute all reachable functions from entry points
    pub fn reachable_functions(&self) -> Vec<FunctionId> {
        self.call_graph.reachable_functions(&self.entry_points)
    }

    /// Perform cross-module reachability analysis
    ///
    /// Returns all blocks reachable from the given entry points, traversing across function and module boundaries.
    pub fn cross_module_reachable_blocks(&self, entry_points: &[FunctionId]) -> FxHashMap<FunctionId, Vec<BlockId>> {
        let mut reachable = FxHashMap::default();
        let reachable_functions = self.call_graph.reachable_functions(entry_points);

        for function_id in reachable_functions {
            if let Some(module) = self.modules.get(&function_id.uri) {
                if let Some(cfg) = module.function_cfgs.get(&function_id.scope_id) {
                    let blocks = cfg.reachable_blocks();
                    reachable.insert(function_id, blocks);
                }
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
}

impl Default for WorkspaceCFG {
    fn default() -> Self {
        Self::new()
    }
}

/// Context for building CFG, tracking loop and exception contexts
#[derive(Debug, Clone)]
struct BuildContext {
    /// Current block being built
    current: BlockId,
    /// Stack of loop contexts (continue_target, break_target)
    loops: Vec<(BlockId, BlockId)>,
    /// Stack of finally blocks that must execute on exit
    finally_blocks: Vec<BlockId>,
}

/// Builder for constructing control flow graphs
pub struct CfgBuilder {
    cfg: ControlFlowGraph,
    ctx: BuildContext,
    /// Call sites discovered during CFG construction
    call_sites: Vec<(BlockId, usize, AstNode)>,
}

/// Builder for constructing module-level CFGs
///
/// Builds a complete ModuleCFG including module initialization code and all function CFGs.
pub struct ModuleCFGBuilder<'a> {
    symbol_table: &'a beacon_parser::SymbolTable,
    uri: Url,
    module_name: String,
    source: &'a str,
}

impl<'a> ModuleCFGBuilder<'a> {
    pub fn new(symbol_table: &'a beacon_parser::SymbolTable, uri: Url, module_name: String, source: &'a str) -> Self {
        Self { symbol_table, uri, module_name, source }
    }

    /// Build a ModuleCFG from an AST
    ///
    /// Constructs CFGs for the module initialization code and all function definitions,  resolving call sites using the symbol table.
    pub fn build(&self, ast: &AstNode) -> ModuleCFG {
        let mut module_cfg = ModuleCFG::new(self.uri.clone(), self.module_name.clone());

        match ast {
            AstNode::Module { body, .. } => {
                let mut init_builder = CfgBuilder::new();
                init_builder.build_module(body);
                let (init_cfg, init_call_sites) =
                    init_builder.build_with_resolution(self.symbol_table, &self.uri, self.symbol_table.root_scope);
                module_cfg.module_init_cfg = init_cfg;
                for call_site in init_call_sites {
                    module_cfg.add_call_site(call_site);
                }

                self.process_module_body(body, &mut module_cfg);
            }
            _ => {}
        }

        module_cfg
    }

    fn process_module_body(&self, body: &[AstNode], module_cfg: &mut ModuleCFG) {
        for stmt in body {
            match stmt {
                AstNode::FunctionDef { name, body: func_body, line, col, .. } => {
                    let byte_offset = self.line_col_to_byte_offset(*line, *col);
                    let scope_id = self.symbol_table.find_scope_at_position(byte_offset);

                    let mut builder = CfgBuilder::new();
                    builder.build_function(func_body);
                    let (cfg, call_sites) = builder.build_with_resolution(self.symbol_table, &self.uri, scope_id);

                    module_cfg.add_function_cfg(scope_id, name.clone(), cfg);
                    for call_site in call_sites {
                        module_cfg.add_call_site(call_site);
                    }
                }
                AstNode::ClassDef { body: class_body, .. } => self.process_module_body(class_body, module_cfg),
                _ => {}
            }
        }
    }

    fn line_col_to_byte_offset(&self, line: usize, col: usize) -> usize {
        let mut current_line = 1;
        let mut byte_offset = 0;

        for (i, ch) in self.source.char_indices() {
            if current_line == line {
                let mut current_col = 1;
                for (j, _) in self.source[i..].char_indices() {
                    if current_col == col {
                        return i + j;
                    }
                    current_col += 1;
                }
                return i + self.source[i..].len();
            }

            if ch == '\n' {
                current_line += 1;
                byte_offset = i + 1;
            }
        }

        byte_offset
    }
}

/// Resolves call targets to FunctionIds using symbol tables
///
/// Performs call target resolution by analyzing call expressions and looking up the target function in the symbol table.
/// Supports direct calls, method calls, and detects dynamic calls that cannot be statically resolved.
pub struct CallResolver<'a> {
    symbol_table: &'a beacon_parser::SymbolTable,
    current_uri: &'a Url,
    current_scope: beacon_parser::ScopeId,
}

impl<'a> CallResolver<'a> {
    pub fn new(symbol_table: &'a beacon_parser::SymbolTable, uri: &'a Url, scope: beacon_parser::ScopeId) -> Self {
        Self { symbol_table, current_uri: uri, current_scope: scope }
    }

    /// Resolve a call expression to a FunctionId and CallKind
    pub fn resolve_call(&self, call_node: &AstNode) -> (Option<FunctionId>, CallKind) {
        match call_node {
            AstNode::Call { function, .. } => self.resolve_call_target(function),
            _ => (None, CallKind::Dynamic),
        }
    }

    fn resolve_call_target(&self, function: &AstNode) -> (Option<FunctionId>, CallKind) {
        match function {
            AstNode::Identifier { name, .. } => {
                if let Some(symbol) = self.symbol_table.lookup_symbol(name, self.current_scope) {
                    match symbol.kind {
                        beacon_parser::SymbolKind::Function => {
                            let func_id = FunctionId::new(self.current_uri.clone(), symbol.scope_id, name.clone());
                            (Some(func_id), CallKind::Direct)
                        }
                        _ => (None, CallKind::Dynamic),
                    }
                } else {
                    (None, CallKind::Direct)
                }
            }
            // FIXME: Full method resolution requires type inference
            AstNode::Attribute { .. } => (None, CallKind::Method),
            _ => (None, CallKind::Dynamic),
        }
    }
}

impl CfgBuilder {
    pub fn new() -> Self {
        let cfg = ControlFlowGraph::new();
        let ctx = BuildContext { current: cfg.entry, loops: Vec::new(), finally_blocks: Vec::new() };
        Self { cfg, ctx, call_sites: Vec::new() }
    }

    pub fn build(self) -> ControlFlowGraph {
        self.cfg
    }

    /// Get the discovered call sites (block_id, stmt_index, call_node)
    pub fn call_sites(&self) -> &[(BlockId, usize, AstNode)] {
        &self.call_sites
    }

    /// Extract call sites from the builder
    pub fn take_call_sites(self) -> (ControlFlowGraph, Vec<(BlockId, usize, AstNode)>) {
        (self.cfg, self.call_sites)
    }

    /// Build CFG and resolve call sites using symbol table
    pub fn build_with_resolution(
        self, symbol_table: &beacon_parser::SymbolTable, uri: &Url, scope_id: beacon_parser::ScopeId,
    ) -> (ControlFlowGraph, Vec<CallSite>) {
        let resolver = CallResolver::new(symbol_table, uri, scope_id);
        let mut resolved_sites = Vec::new();

        for (block_id, stmt_index, call_node) in &self.call_sites {
            let (line, col) = match call_node {
                AstNode::Call { line, col, .. } => (*line, *col),
                _ => (0, 0),
            };

            let (receiver, kind) = resolver.resolve_call(call_node);
            let call_site = CallSite::new(*block_id, *stmt_index, receiver, kind, line, col);
            resolved_sites.push(call_site);
        }

        (self.cfg, resolved_sites)
    }

    /// Build CFG for a function body
    pub fn build_function(&mut self, body: &[AstNode]) {
        let mut stmt_index = 0;
        for stmt in body {
            self.build_stmt(stmt, &mut stmt_index);
        }

        self.cfg.add_edge(self.ctx.current, self.cfg.exit, EdgeKind::Normal);
    }

    /// Build CFG for module-level initialization code
    ///
    /// Processes top-level statements in a module, excluding function and class definitions.
    /// Module initialization runs sequentially when the module is imported.
    pub fn build_module(&mut self, body: &[AstNode]) {
        let mut stmt_index = 0;
        for stmt in body {
            match stmt {
                AstNode::FunctionDef { .. } | AstNode::ClassDef { .. } => stmt_index += 1,
                _ => self.build_stmt(stmt, &mut stmt_index),
            }
        }

        self.cfg.add_edge(self.ctx.current, self.cfg.exit, EdgeKind::Normal);
    }

    /// Extract call sites from an expression recursively
    fn extract_calls_from_expr(&mut self, expr: &AstNode, stmt_index: usize) {
        match expr {
            AstNode::Call { function, args, keywords, .. } => {
                self.call_sites.push((self.ctx.current, stmt_index, expr.clone()));
                self.extract_calls_from_expr(function, stmt_index);
                for arg in args {
                    self.extract_calls_from_expr(arg, stmt_index);
                }
                for (_, value) in keywords {
                    self.extract_calls_from_expr(value, stmt_index);
                }
            }
            AstNode::BinaryOp { left, right, .. } => {
                self.extract_calls_from_expr(left, stmt_index);
                self.extract_calls_from_expr(right, stmt_index);
            }
            AstNode::UnaryOp { operand, .. } => self.extract_calls_from_expr(operand, stmt_index),
            AstNode::Attribute { object, .. } => self.extract_calls_from_expr(object, stmt_index),
            AstNode::Subscript { value, slice, .. } => {
                self.extract_calls_from_expr(value, stmt_index);
                self.extract_calls_from_expr(slice, stmt_index);
            }
            AstNode::List { elements, .. } => {
                for elem in elements {
                    self.extract_calls_from_expr(elem, stmt_index);
                }
            }
            AstNode::Set { elements, .. } => {
                for elem in elements {
                    self.extract_calls_from_expr(elem, stmt_index);
                }
            }
            AstNode::Tuple { elements, .. } => {
                for elem in elements {
                    self.extract_calls_from_expr(elem, stmt_index);
                }
            }
            AstNode::Dict { keys, values, .. } => {
                for key in keys {
                    self.extract_calls_from_expr(key, stmt_index);
                }
                for value in values {
                    self.extract_calls_from_expr(value, stmt_index);
                }
            }
            AstNode::ListComp { element, generators, .. } => {
                self.extract_calls_from_expr(element, stmt_index);
                for comprehension in generators {
                    self.extract_calls_from_expr(&comprehension.iter, stmt_index);
                    for cond in &comprehension.ifs {
                        self.extract_calls_from_expr(cond, stmt_index);
                    }
                }
            }
            AstNode::DictComp { key, value, generators, .. } => {
                self.extract_calls_from_expr(key, stmt_index);
                self.extract_calls_from_expr(value, stmt_index);
                for comprehension in generators {
                    self.extract_calls_from_expr(&comprehension.iter, stmt_index);
                    for cond in &comprehension.ifs {
                        self.extract_calls_from_expr(cond, stmt_index);
                    }
                }
            }
            AstNode::SetComp { element, generators, .. } => {
                self.extract_calls_from_expr(element, stmt_index);
                for comprehension in generators {
                    self.extract_calls_from_expr(&comprehension.iter, stmt_index);
                    for cond in &comprehension.ifs {
                        self.extract_calls_from_expr(cond, stmt_index);
                    }
                }
            }
            AstNode::GeneratorExp { element, generators, .. } => {
                self.extract_calls_from_expr(element, stmt_index);
                for comprehension in generators {
                    self.extract_calls_from_expr(&comprehension.iter, stmt_index);
                    for cond in &comprehension.ifs {
                        self.extract_calls_from_expr(cond, stmt_index);
                    }
                }
            }
            AstNode::Compare { left, comparators, .. } => {
                self.extract_calls_from_expr(left, stmt_index);
                for comp in comparators {
                    self.extract_calls_from_expr(comp, stmt_index);
                }
            }
            AstNode::Lambda { body, .. } => self.extract_calls_from_expr(body, stmt_index),
            AstNode::Yield { value, .. } => {
                if let Some(val) = value {
                    self.extract_calls_from_expr(val, stmt_index)
                }
            }
            AstNode::YieldFrom { value, .. } => self.extract_calls_from_expr(value, stmt_index),
            AstNode::Await { value, .. } => self.extract_calls_from_expr(value, stmt_index),
            AstNode::Starred { value, .. } => self.extract_calls_from_expr(value, stmt_index),
            AstNode::NamedExpr { value, .. } => self.extract_calls_from_expr(value, stmt_index),
            _ => {}
        }
    }

    fn build_stmt(&mut self, stmt: &AstNode, stmt_index: &mut usize) {
        match stmt {
            AstNode::If { test, body, elif_parts, else_body, .. } => {
                self.build_if(test, body, elif_parts, else_body, stmt_index)
            }
            AstNode::For { target: _, iter: _, body, else_body, .. } => {
                self.build_for(body, else_body.as_ref(), stmt_index)
            }
            AstNode::While { test: _, body, else_body, .. } => self.build_while(body, else_body.as_ref(), stmt_index),
            AstNode::Try { body, handlers, else_body, finally_body, .. } => {
                self.build_try(body, handlers, else_body.as_ref(), finally_body.as_ref(), stmt_index)
            }
            AstNode::With { items: _, body, .. } => self.build_with(body, stmt_index),
            AstNode::Match { subject: _, cases, .. } => self.build_match(cases, stmt_index),
            AstNode::Return { value, .. } => {
                if let Some(val) = value {
                    self.extract_calls_from_expr(val, *stmt_index);
                }

                if let Some(block) = self.cfg.blocks.get_mut(&self.ctx.current) {
                    block.statements.push(*stmt_index);
                }
                *stmt_index += 1;

                let exit_target = if self.ctx.finally_blocks.is_empty() {
                    self.cfg.exit
                } else {
                    *self.ctx.finally_blocks.last().unwrap()
                };
                self.cfg.add_edge(self.ctx.current, exit_target, EdgeKind::Normal);

                let unreachable = self.cfg.new_block();
                self.ctx.current = unreachable;
            }
            AstNode::Raise { exc, .. } => {
                if let Some(exception) = exc {
                    self.extract_calls_from_expr(exception, *stmt_index);
                }

                if let Some(block) = self.cfg.blocks.get_mut(&self.ctx.current) {
                    block.statements.push(*stmt_index);
                }
                *stmt_index += 1;

                let exit_target = if self.ctx.finally_blocks.is_empty() {
                    self.cfg.exit
                } else {
                    *self.ctx.finally_blocks.last().unwrap()
                };
                self.cfg.add_edge(self.ctx.current, exit_target, EdgeKind::Normal);

                let unreachable = self.cfg.new_block();
                self.ctx.current = unreachable;
            }
            AstNode::Break { .. } => {
                if let Some(block) = self.cfg.blocks.get_mut(&self.ctx.current) {
                    block.statements.push(*stmt_index);
                }
                *stmt_index += 1;

                if let Some((_, break_target)) = self.ctx.loops.last() {
                    self.cfg.add_edge(self.ctx.current, *break_target, EdgeKind::Break);
                }

                let unreachable = self.cfg.new_block();
                self.ctx.current = unreachable;
            }
            AstNode::Continue { .. } => {
                if let Some(block) = self.cfg.blocks.get_mut(&self.ctx.current) {
                    block.statements.push(*stmt_index);
                }
                *stmt_index += 1;

                if let Some((continue_target, _)) = self.ctx.loops.last() {
                    self.cfg
                        .add_edge(self.ctx.current, *continue_target, EdgeKind::Continue);
                }

                let unreachable = self.cfg.new_block();
                self.ctx.current = unreachable;
            }
            _ => {
                match stmt {
                    AstNode::Assignment { value, .. } => self.extract_calls_from_expr(value, *stmt_index),
                    AstNode::AnnotatedAssignment { value, .. } => {
                        if let Some(val) = value {
                            self.extract_calls_from_expr(val, *stmt_index);
                        }
                    }
                    AstNode::Assert { test, msg, .. } => {
                        self.extract_calls_from_expr(test, *stmt_index);
                        if let Some(msg_expr) = msg {
                            self.extract_calls_from_expr(msg_expr, *stmt_index);
                        }
                    }
                    AstNode::Call { .. }
                    | AstNode::Yield { .. }
                    | AstNode::YieldFrom { .. }
                    | AstNode::Await { .. } => self.extract_calls_from_expr(stmt, *stmt_index),
                    _ => {}
                }

                if let Some(block) = self.cfg.blocks.get_mut(&self.ctx.current) {
                    block.statements.push(*stmt_index);
                }
                *stmt_index += 1;
            }
        }
    }

    fn build_if(
        &mut self, _test: &AstNode, body: &[AstNode], elif_parts: &[(AstNode, Vec<AstNode>)],
        else_body: &Option<Vec<AstNode>>, stmt_index: &mut usize,
    ) {
        let test_block = self.ctx.current;
        let merge_block = self.cfg.new_block();
        let true_block = self.cfg.new_block();
        self.cfg.add_edge(test_block, true_block, EdgeKind::True);
        self.ctx.current = true_block;
        for stmt in body {
            self.build_stmt(stmt, stmt_index);
        }
        self.cfg.add_edge(self.ctx.current, merge_block, EdgeKind::Normal);

        let mut current_test = test_block;
        for (elif_test, elif_body) in elif_parts {
            let elif_test_block = self.cfg.new_block();
            self.cfg.add_edge(current_test, elif_test_block, EdgeKind::False);

            let elif_true_block = self.cfg.new_block();
            self.cfg.add_edge(elif_test_block, elif_true_block, EdgeKind::True);
            self.ctx.current = elif_true_block;
            for stmt in elif_body {
                self.build_stmt(stmt, stmt_index);
            }
            self.cfg.add_edge(self.ctx.current, merge_block, EdgeKind::Normal);

            current_test = elif_test_block;
            _ = elif_test;
        }

        if let Some(else_stmts) = else_body {
            let else_block = self.cfg.new_block();
            self.cfg.add_edge(current_test, else_block, EdgeKind::False);
            self.ctx.current = else_block;
            for stmt in else_stmts {
                self.build_stmt(stmt, stmt_index);
            }
            self.cfg.add_edge(self.ctx.current, merge_block, EdgeKind::Normal);
        } else {
            self.cfg.add_edge(current_test, merge_block, EdgeKind::False);
        }

        self.ctx.current = merge_block;
    }

    fn build_for(&mut self, body: &[AstNode], else_body: Option<&Vec<AstNode>>, stmt_index: &mut usize) {
        let loop_header = self.cfg.new_block();
        let loop_body = self.cfg.new_block();
        let loop_exit = self.cfg.new_block();

        self.cfg.add_edge(self.ctx.current, loop_header, EdgeKind::Normal);
        self.cfg.add_edge(loop_header, loop_body, EdgeKind::True);
        self.cfg.add_edge(loop_header, loop_exit, EdgeKind::False);

        self.ctx.loops.push((loop_header, loop_exit));
        self.ctx.current = loop_body;
        for stmt in body {
            self.build_stmt(stmt, stmt_index);
        }

        self.cfg.add_edge(self.ctx.current, loop_header, EdgeKind::Normal);
        self.ctx.loops.pop();

        if let Some(else_stmts) = else_body {
            let else_block = self.cfg.new_block();
            if let Some(header_block) = self.cfg.blocks.get_mut(&loop_header) {
                header_block
                    .successors
                    .retain(|(id, kind)| !(*id == loop_exit && *kind == EdgeKind::False));
            }
            self.cfg.add_edge(loop_header, else_block, EdgeKind::False);

            self.ctx.current = else_block;
            for stmt in else_stmts {
                self.build_stmt(stmt, stmt_index);
            }
            self.cfg.add_edge(self.ctx.current, loop_exit, EdgeKind::Normal);
        }

        self.ctx.current = loop_exit;
    }

    fn build_while(&mut self, body: &[AstNode], else_body: Option<&Vec<AstNode>>, stmt_index: &mut usize) {
        self.build_for(body, else_body, stmt_index);
    }

    fn build_try(
        &mut self, body: &[AstNode], handlers: &[beacon_parser::ExceptHandler], else_body: Option<&Vec<AstNode>>,
        finally_body: Option<&Vec<AstNode>>, stmt_index: &mut usize,
    ) {
        let try_block = self.cfg.new_block();
        let merge_block = self.cfg.new_block();

        let finally_block = if finally_body.is_some() {
            let fb = self.cfg.new_block();
            self.ctx.finally_blocks.push(fb);
            Some(fb)
        } else {
            None
        };

        self.cfg.add_edge(self.ctx.current, try_block, EdgeKind::Normal);
        self.ctx.current = try_block;
        for stmt in body {
            self.build_stmt(stmt, stmt_index);
        }
        let try_exit = self.ctx.current;

        for handler in handlers {
            let handler_block = self.cfg.new_block();
            self.cfg.add_edge(try_block, handler_block, EdgeKind::Exception);
            self.ctx.current = handler_block;
            for stmt in &handler.body {
                self.build_stmt(stmt, stmt_index);
            }
            self.cfg.add_edge(self.ctx.current, merge_block, EdgeKind::Normal);
        }

        self.cfg.add_edge(try_exit, merge_block, EdgeKind::Normal);

        if let Some(else_stmts) = else_body {
            let else_block = self.cfg.new_block();
            if let Some(try_block_mut) = self.cfg.blocks.get_mut(&try_exit) {
                try_block_mut.successors.retain(|(id, _)| *id != merge_block);
            }
            self.cfg.add_edge(try_exit, else_block, EdgeKind::Normal);

            self.ctx.current = else_block;
            for stmt in else_stmts {
                self.build_stmt(stmt, stmt_index);
            }
            self.cfg.add_edge(self.ctx.current, merge_block, EdgeKind::Normal);
        }

        if let Some(finally_stmts) = finally_body {
            if let Some(fb) = finally_block {
                self.cfg.add_edge(merge_block, fb, EdgeKind::Finally);
                let post_finally = self.cfg.new_block();
                self.ctx.current = fb;
                for stmt in finally_stmts {
                    self.build_stmt(stmt, stmt_index);
                }
                self.cfg.add_edge(self.ctx.current, post_finally, EdgeKind::Normal);
                self.ctx.current = post_finally;
                self.ctx.finally_blocks.pop();
            }
        } else {
            self.ctx.current = merge_block;
        }
    }

    /// Desugars with as try/finall with ctx: body → try: body finally: ctx.__exit__()
    fn build_with(&mut self, body: &[AstNode], stmt_index: &mut usize) {
        let with_body_block = self.cfg.new_block();
        let finally_block = self.cfg.new_block();
        let merge_block = self.cfg.new_block();

        self.cfg.add_edge(self.ctx.current, with_body_block, EdgeKind::Normal);
        self.ctx.finally_blocks.push(finally_block);

        self.ctx.current = with_body_block;
        for stmt in body {
            self.build_stmt(stmt, stmt_index);
        }
        self.cfg.add_edge(self.ctx.current, finally_block, EdgeKind::Finally);

        self.ctx.current = finally_block;
        self.cfg.add_edge(self.ctx.current, merge_block, EdgeKind::Normal);

        self.ctx.finally_blocks.pop();
        self.ctx.current = merge_block;
    }

    fn build_match(&mut self, cases: &[beacon_parser::MatchCase], stmt_index: &mut usize) {
        let match_block = self.ctx.current;
        let merge_block = self.cfg.new_block();

        for case in cases {
            let case_block = self.cfg.new_block();
            self.cfg.add_edge(match_block, case_block, EdgeKind::True);

            self.ctx.current = case_block;
            for stmt in &case.body {
                self.build_stmt(stmt, stmt_index);
            }
            self.cfg.add_edge(self.ctx.current, merge_block, EdgeKind::Normal);
        }

        self.cfg.add_edge(match_block, merge_block, EdgeKind::False);
        self.ctx.current = merge_block;
    }
}

impl Default for CfgBuilder {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use beacon_parser::{AstNode, ExceptHandler, LiteralValue, MatchCase, Pattern, WithItem};

    #[test]
    fn test_cfg_creation() {
        let cfg = ControlFlowGraph::new();
        assert_eq!(cfg.blocks.len(), 2);
        assert!(cfg.blocks.contains_key(&cfg.entry));
        assert!(cfg.blocks.contains_key(&cfg.exit));
    }

    #[test]
    fn test_cfg_builder_empty_function() {
        let mut builder = CfgBuilder::new();
        builder.build_function(&[]);

        let cfg = builder.build();
        let entry_block = cfg.blocks.get(&cfg.entry).unwrap();
        assert_eq!(entry_block.successors.len(), 1);
        assert_eq!(entry_block.successors[0].0, cfg.exit);
    }

    #[test]
    fn test_reachable_blocks() {
        let mut cfg = ControlFlowGraph::new();
        let block1 = cfg.new_block();
        let block2 = cfg.new_block();

        cfg.add_edge(cfg.entry, block1, EdgeKind::Normal);
        cfg.add_edge(block1, block2, EdgeKind::Normal);
        cfg.add_edge(block2, cfg.exit, EdgeKind::Normal);

        let reachable = cfg.reachable_blocks();
        assert!(reachable.contains(&cfg.entry));
        assert!(reachable.contains(&block1));
        assert!(reachable.contains(&block2));
        assert!(reachable.contains(&cfg.exit));
    }

    #[test]
    fn test_unreachable_blocks() {
        let mut cfg = ControlFlowGraph::new();
        let block1 = cfg.new_block();
        let unreachable = cfg.new_block();

        cfg.add_edge(cfg.entry, block1, EdgeKind::Normal);
        cfg.add_edge(block1, cfg.exit, EdgeKind::Normal);

        let unreachable_blocks = cfg.unreachable_blocks();
        assert!(unreachable_blocks.contains(&unreachable));
        assert!(!unreachable_blocks.contains(&block1));
    }

    #[test]
    fn test_cfg_if_statement() {
        let mut builder = CfgBuilder::new();
        let body = vec![AstNode::If {
            test: Box::new(AstNode::Identifier { name: "cond".to_string(), line: 1, col: 4, end_line: 1, end_col: 4 }),
            body: vec![AstNode::Pass { line: 2, col: 5, end_line: 2, end_col: 9 }],
            elif_parts: vec![],
            else_body: None,
            line: 1,
            end_line: 1,
            col: 1,
            end_col: 1,
        }];

        builder.build_function(&body);
        let cfg = builder.build();

        assert!(cfg.blocks.len() >= 2);

        let has_true_edge = cfg
            .blocks
            .values()
            .any(|b| b.successors.iter().any(|(_, k)| *k == EdgeKind::True));
        let has_false_edge = cfg
            .blocks
            .values()
            .any(|b| b.successors.iter().any(|(_, k)| *k == EdgeKind::False));
        assert!(has_true_edge);
        assert!(has_false_edge);
    }

    #[test]
    fn test_cfg_if_else_statement() {
        let mut builder = CfgBuilder::new();
        let body = vec![AstNode::If {
            test: Box::new(AstNode::Identifier { name: "cond".to_string(), line: 1, col: 4, end_line: 1, end_col: 8 }),
            body: vec![AstNode::Pass { line: 2, col: 5, end_line: 2, end_col: 9 }],
            elif_parts: vec![],
            else_body: Some(vec![AstNode::Pass { line: 4, col: 5, end_line: 4, end_col: 9 }]),
            line: 1,
            end_line: 1,
            col: 1,
            end_col: 1,
        }];

        builder.build_function(&body);
        let cfg = builder.build();

        let has_true_edge = cfg
            .blocks
            .values()
            .any(|b| b.successors.iter().any(|(_, k)| *k == EdgeKind::True));
        let has_false_edge = cfg
            .blocks
            .values()
            .any(|b| b.successors.iter().any(|(_, k)| *k == EdgeKind::False));
        assert!(has_true_edge);
        assert!(has_false_edge);
    }

    #[test]
    fn test_cfg_for_loop() {
        let mut builder = CfgBuilder::new();
        let body = vec![AstNode::For {
            target: Box::new(AstNode::Identifier { name: "i".to_string(), line: 1, col: 5, end_line: 1, end_col: 6 }),
            iter: Box::new(AstNode::Identifier {
                name: "items".to_string(),
                line: 1,
                col: 14,
                end_line: 1,
                end_col: 19,
            }),
            body: vec![AstNode::Pass { line: 2, col: 5, end_line: 2, end_col: 9 }],
            else_body: None,
            is_async: false,
            line: 1,
            end_line: 1,
            col: 1,
            end_col: 1,
        }];

        builder.build_function(&body);
        let cfg = builder.build();

        assert!(cfg.blocks.len() >= 5);

        let reachable = cfg.reachable_blocks();
        assert!(reachable.contains(&cfg.entry));
        assert!(reachable.contains(&cfg.exit));
    }

    #[test]
    fn test_cfg_for_loop_with_break() {
        let mut builder = CfgBuilder::new();
        let body = vec![AstNode::For {
            target: Box::new(AstNode::Identifier { name: "i".to_string(), line: 1, col: 5, end_line: 1, end_col: 6 }),
            iter: Box::new(AstNode::Identifier {
                name: "items".to_string(),
                line: 1,
                col: 14,
                end_line: 1,
                end_col: 19,
            }),
            body: vec![AstNode::Break { line: 2, col: 5, end_line: 2, end_col: 10 }],
            else_body: None,
            is_async: false,
            line: 1,
            end_line: 1,
            col: 1,
            end_col: 1,
        }];

        builder.build_function(&body);
        let cfg = builder.build();

        let has_break_edge = cfg
            .blocks
            .values()
            .any(|block| block.successors.iter().any(|(_, kind)| *kind == EdgeKind::Break));
        assert!(has_break_edge);
    }

    #[test]
    fn test_cfg_for_loop_with_continue() {
        let mut builder = CfgBuilder::new();
        let body = vec![AstNode::For {
            target: Box::new(AstNode::Identifier { name: "i".to_string(), line: 1, col: 5, end_line: 1, end_col: 6 }),
            iter: Box::new(AstNode::Identifier {
                name: "items".to_string(),
                line: 1,
                col: 14,
                end_line: 1,
                end_col: 19,
            }),
            body: vec![AstNode::Continue { line: 2, col: 5, end_line: 2, end_col: 13 }],
            else_body: None,
            is_async: false,
            line: 1,
            end_line: 1,
            col: 1,
            end_col: 1,
        }];

        builder.build_function(&body);
        let cfg = builder.build();

        let has_continue_edge = cfg
            .blocks
            .values()
            .any(|block| block.successors.iter().any(|(_, kind)| *kind == EdgeKind::Continue));
        assert!(has_continue_edge);
    }

    #[test]
    fn test_cfg_while_loop() {
        let mut builder = CfgBuilder::new();
        let body = vec![AstNode::While {
            test: Box::new(AstNode::Identifier { name: "cond".to_string(), line: 1, col: 7, end_line: 1, end_col: 11 }),
            body: vec![AstNode::Pass { line: 2, col: 5, end_line: 2, end_col: 9 }],
            else_body: None,
            line: 1,
            end_line: 1,
            col: 1,
            end_col: 1,
        }];

        builder.build_function(&body);
        let cfg = builder.build();

        assert!(cfg.blocks.len() >= 5);
        let reachable = cfg.reachable_blocks();
        assert!(reachable.contains(&cfg.entry));
        assert!(reachable.contains(&cfg.exit));
    }

    #[test]
    fn test_cfg_try_except() {
        let mut builder = CfgBuilder::new();
        let body = vec![AstNode::Try {
            body: vec![AstNode::Pass { line: 2, col: 5, end_line: 2, end_col: 9 }],
            handlers: vec![ExceptHandler {
                exception_type: None,
                name: None,
                body: vec![AstNode::Pass { line: 4, col: 5, end_col: 9, end_line: 4 }],
                line: 3,
                col: 1,
                end_line: 3,
                end_col: 1,
            }],
            else_body: None,
            finally_body: None,
            line: 1,
            col: 1,
            end_col: 1,
            end_line: 1,
        }];

        builder.build_function(&body);
        let cfg = builder.build();

        let has_exception_edge = cfg
            .blocks
            .values()
            .any(|block| block.successors.iter().any(|(_, kind)| *kind == EdgeKind::Exception));
        assert!(has_exception_edge);
    }

    #[test]
    fn test_cfg_try_finally() {
        let mut builder = CfgBuilder::new();
        let body = vec![AstNode::Try {
            body: vec![AstNode::Pass { line: 2, col: 5, end_line: 2, end_col: 9 }],
            handlers: vec![],
            else_body: None,
            finally_body: Some(vec![AstNode::Pass { line: 4, col: 5, end_line: 4, end_col: 9 }]),
            line: 1,
            col: 1,
            end_col: 1,
            end_line: 1,
        }];

        builder.build_function(&body);
        let cfg = builder.build();

        let has_finally_edge = cfg
            .blocks
            .values()
            .any(|block| block.successors.iter().any(|(_, kind)| *kind == EdgeKind::Finally));
        assert!(has_finally_edge);
    }

    #[test]
    fn test_cfg_with_statement() {
        let mut builder = CfgBuilder::new();
        let body = vec![AstNode::With {
            items: vec![WithItem {
                context_expr: AstNode::Identifier { name: "ctx".to_string(), line: 1, col: 6, end_col: 9, end_line: 1 },
                optional_vars: None,
            }],
            body: vec![AstNode::Pass { line: 2, col: 5, end_col: 9, end_line: 2 }],
            is_async: false,
            line: 1,
            col: 1,
            end_col: 1,
            end_line: 1,
        }];

        builder.build_function(&body);
        let cfg = builder.build();

        let has_finally_edge = cfg
            .blocks
            .values()
            .any(|block| block.successors.iter().any(|(_, kind)| *kind == EdgeKind::Finally));
        assert!(has_finally_edge);
    }

    #[test]
    fn test_cfg_match_statement() {
        let mut builder = CfgBuilder::new();
        let body = vec![AstNode::Match {
            subject: Box::new(AstNode::Identifier { name: "x".to_string(), line: 1, col: 7, end_col: 8, end_line: 1 }),
            cases: vec![
                MatchCase {
                    pattern: Pattern::MatchValue(AstNode::Literal {
                        value: LiteralValue::Integer(1),
                        line: 2,
                        col: 10,
                        end_line: 2,
                        end_col: 11,
                    }),
                    guard: None,
                    body: vec![AstNode::Pass { line: 3, col: 9, end_col: 13, end_line: 3 }],
                    line: 2,
                    col: 5,
                    end_line: 3,
                    end_col: 13,
                },
                MatchCase {
                    pattern: Pattern::MatchValue(AstNode::Literal {
                        value: LiteralValue::Integer(2),
                        line: 4,
                        col: 10,
                        end_col: 11,
                        end_line: 4,
                    }),
                    guard: None,
                    body: vec![AstNode::Pass { line: 5, col: 9, end_line: 5, end_col: 13 }],
                    line: 4,
                    col: 5,
                    end_line: 5,
                    end_col: 13,
                },
            ],
            line: 1,
            col: 1,
            end_col: 1,
            end_line: 1,
        }];

        builder.build_function(&body);
        let cfg = builder.build();

        assert!(
            cfg.blocks
                .values()
                .flat_map(|block| block.successors.iter())
                .filter(|(_, kind)| *kind == EdgeKind::True)
                .count()
                >= 2
        );
    }

    #[test]
    fn test_cfg_return_creates_unreachable_block() {
        let mut builder = CfgBuilder::new();
        let body = vec![
            AstNode::Return { value: None, line: 1, col: 1, end_col: 7, end_line: 1 },
            AstNode::Pass { line: 2, col: 1, end_col: 5, end_line: 2 },
        ];

        builder.build_function(&body);
        let cfg = builder.build();

        let unreachable = cfg.unreachable_blocks();
        assert!(!unreachable.is_empty());
    }

    #[test]
    fn test_cfg_nested_if_statements() {
        let mut builder = CfgBuilder::new();
        let body = vec![AstNode::If {
            test: Box::new(AstNode::Identifier { name: "cond1".to_string(), line: 1, col: 4, end_col: 9, end_line: 1 }),
            body: vec![AstNode::If {
                test: Box::new(AstNode::Identifier {
                    name: "cond2".to_string(),
                    line: 2,
                    col: 8,
                    end_col: 0,
                    end_line: 0,
                }),
                body: vec![AstNode::Pass { line: 3, col: 9, end_col: 0, end_line: 0 }],
                elif_parts: vec![],
                else_body: None,
                line: 2,
                col: 5,
                end_col: 0,
                end_line: 0,
            }],
            elif_parts: vec![],
            else_body: None,
            line: 1,
            col: 1,
            end_col: 0,
            end_line: 0,
        }];

        builder.build_function(&body);
        let cfg = builder.build();

        assert!(cfg.blocks.len() >= 2);

        let true_edge_count = cfg
            .blocks
            .values()
            .flat_map(|b| &b.successors)
            .filter(|(_, k)| *k == EdgeKind::True)
            .count();
        assert!(
            true_edge_count >= 2,
            "Expected at least 2 True edges for nested if statements"
        );
    }

    #[test]
    fn test_cfg_complex_control_flow() {
        let mut builder = CfgBuilder::new();
        let body = vec![
            AstNode::Assignment {
                target: Box::new(AstNode::Identifier {
                    name: "x".to_string(),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 2,
                }),
                value: Box::new(AstNode::Literal {
                    value: beacon_parser::LiteralValue::Integer(1),
                    line: 1,
                    col: 5,
                    end_col: 0,
                    end_line: 0,
                }),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 1,
            },
            AstNode::For {
                target: Box::new(AstNode::Identifier {
                    name: "i".to_string(),
                    line: 1,
                    col: 5,
                    end_line: 1,
                    end_col: 6,
                }),
                iter: Box::new(AstNode::Identifier {
                    name: "range(10)".to_string(),
                    line: 2,
                    col: 14,
                    end_col: 0,
                    end_line: 0,
                }),
                body: vec![AstNode::If {
                    test: Box::new(AstNode::Identifier {
                        name: "cond".to_string(),
                        line: 3,
                        col: 12,
                        end_col: 0,
                        end_line: 0,
                    }),
                    body: vec![AstNode::Break { line: 4, col: 13, end_line: 4, end_col: 18 }],
                    elif_parts: vec![],
                    else_body: Some(vec![AstNode::Continue { line: 6, col: 13, end_line: 6, end_col: 21 }]),
                    line: 3,
                    col: 9,
                    end_line: 3,
                    end_col: 11,
                }],
                else_body: None,
                is_async: false,
                line: 2,
                col: 1,
                end_line: 2,
                end_col: 1,
            },
            AstNode::Return { value: None, line: 7, col: 1, end_line: 7, end_col: 7 },
        ];

        builder.build_function(&body);
        let cfg = builder.build();

        let has_break = cfg
            .blocks
            .values()
            .any(|b| b.successors.iter().any(|(_, k)| *k == EdgeKind::Break));
        let has_continue = cfg
            .blocks
            .values()
            .any(|b| b.successors.iter().any(|(_, k)| *k == EdgeKind::Continue));

        assert!(has_break);
        assert!(has_continue);
    }

    #[test]
    fn test_cfg_if_elif_chain() {
        let mut builder = CfgBuilder::new();
        let body = vec![AstNode::If {
            test: Box::new(AstNode::Identifier { name: "cond1".to_string(), line: 1, col: 4, end_line: 1, end_col: 9 }),
            body: vec![AstNode::Pass { line: 2, col: 5, end_line: 2, end_col: 9 }],
            elif_parts: vec![
                (
                    AstNode::Identifier { name: "cond2".to_string(), line: 3, col: 6, end_line: 3, end_col: 11 },
                    vec![AstNode::Pass { line: 4, col: 5, end_line: 4, end_col: 9 }],
                ),
                (
                    AstNode::Identifier { name: "cond3".to_string(), line: 5, col: 6, end_line: 5, end_col: 11 },
                    vec![AstNode::Pass { line: 6, col: 5, end_line: 6, end_col: 9 }],
                ),
            ],
            else_body: Some(vec![AstNode::Pass { line: 8, col: 5, end_line: 8, end_col: 9 }]),
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 1,
        }];

        builder.build_function(&body);
        let cfg = builder.build();

        let true_edge_count = cfg
            .blocks
            .values()
            .flat_map(|b| &b.successors)
            .filter(|(_, k)| *k == EdgeKind::True)
            .count();

        assert!(
            true_edge_count >= 3,
            "Expected at least 3 True edges for if-elif-elif chain, got {true_edge_count}"
        );
    }

    #[test]
    fn test_cfg_nested_loops() {
        let mut builder = CfgBuilder::new();
        let body = vec![AstNode::For {
            target: Box::new(AstNode::Identifier { name: "i".to_string(), line: 1, col: 5, end_line: 1, end_col: 6 }),
            iter: Box::new(AstNode::Identifier {
                name: "outer".to_string(),
                line: 1,
                col: 10,
                end_line: 1,
                end_col: 15,
            }),
            body: vec![AstNode::For {
                target: Box::new(AstNode::Identifier {
                    name: "j".to_string(),
                    line: 2,
                    col: 9,
                    end_line: 2,
                    end_col: 10,
                }),
                iter: Box::new(AstNode::Identifier {
                    name: "inner".to_string(),
                    line: 2,
                    col: 14,
                    end_line: 2,
                    end_col: 19,
                }),
                body: vec![AstNode::Pass { line: 3, col: 9, end_line: 3, end_col: 13 }],
                else_body: None,
                is_async: false,
                line: 2,
                col: 5,
                end_line: 2,
                end_col: 5,
            }],
            else_body: None,
            is_async: false,
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 1,
        }];

        builder.build_function(&body);
        let cfg = builder.build();

        assert!(cfg.blocks.len() >= 7);
        let reachable = cfg.reachable_blocks();
        assert!(reachable.contains(&cfg.entry));
        assert!(reachable.contains(&cfg.exit));
    }

    #[test]
    fn test_cfg_try_except_else_finally() {
        let mut builder = CfgBuilder::new();
        let body = vec![AstNode::Try {
            body: vec![AstNode::Pass { line: 2, col: 5, end_line: 2, end_col: 9 }],
            handlers: vec![ExceptHandler {
                exception_type: None,
                name: None,
                body: vec![AstNode::Pass { line: 4, col: 5, end_col: 9, end_line: 4 }],
                line: 3,
                col: 1,
                end_line: 3,
                end_col: 1,
            }],
            else_body: Some(vec![AstNode::Pass { line: 6, col: 5, end_line: 6, end_col: 9 }]),
            finally_body: Some(vec![AstNode::Pass { line: 8, col: 5, end_line: 8, end_col: 9 }]),
            line: 1,
            col: 1,
            end_col: 1,
            end_line: 1,
        }];

        builder.build_function(&body);
        let cfg = builder.build();

        let has_exception = cfg
            .blocks
            .values()
            .any(|block| block.successors.iter().any(|(_, kind)| *kind == EdgeKind::Exception));
        let has_finally = cfg
            .blocks
            .values()
            .any(|block| block.successors.iter().any(|(_, kind)| *kind == EdgeKind::Finally));

        assert!(has_exception);
        assert!(has_finally);
    }

    #[test]
    fn test_cfg_for_loop_with_else() {
        let mut builder = CfgBuilder::new();
        let body = vec![AstNode::For {
            target: Box::new(AstNode::Identifier { name: "i".to_string(), line: 1, col: 5, end_line: 1, end_col: 6 }),
            iter: Box::new(AstNode::Identifier {
                name: "items".to_string(),
                line: 1,
                col: 10,
                end_line: 1,
                end_col: 15,
            }),
            body: vec![AstNode::Pass { line: 2, col: 5, end_line: 2, end_col: 9 }],
            else_body: Some(vec![AstNode::Pass { line: 4, col: 5, end_line: 4, end_col: 9 }]),
            is_async: false,
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 1,
        }];

        builder.build_function(&body);
        let cfg = builder.build();

        assert!(cfg.blocks.len() >= 5);
        let reachable = cfg.reachable_blocks();
        assert!(reachable.contains(&cfg.entry));
        assert!(reachable.contains(&cfg.exit));
    }
}

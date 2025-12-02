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
}

impl CallSite {
    pub fn new(id: BlockId, index: usize, rec: Option<FunctionId>, kind: CallKind, ln: usize, col: usize) -> Self {
        Self { block_id: id, stmt_index: index, receiver: rec, kind, line: ln, col }
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
}

impl Default for CallGraph {
    fn default() -> Self {
        Self::new()
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
            call_sites: Vec::new(),
        }
    }

    /// Add a function CFG to this module
    pub fn add_function_cfg(&mut self, scope_id: ScopeId, cfg: ControlFlowGraph) {
        self.function_cfgs.insert(scope_id, cfg);
    }

    /// Add a call site to this module
    pub fn add_call_site(&mut self, call_site: CallSite) {
        self.call_sites.push(call_site);
    }

    /// Get all function IDs defined in this module
    ///
    /// TODO: Populate function IDs from symbol table during analysis.
    pub fn function_ids(&self) -> Vec<FunctionId> {
        Vec::new()
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

    /// Build CFG for a function body
    pub fn build_function(&mut self, body: &[AstNode]) {
        let mut stmt_index = 0;
        for stmt in body {
            self.build_stmt(stmt, &mut stmt_index);
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
            AstNode::UnaryOp { operand, .. } => {
                self.extract_calls_from_expr(operand, stmt_index);
            }
            AstNode::Attribute { object, .. } => {
                self.extract_calls_from_expr(object, stmt_index);
            }
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
            AstNode::Lambda { body, .. } => {
                self.extract_calls_from_expr(body, stmt_index);
            }
            AstNode::Yield { value, .. } => {
                if let Some(val) = value {
                    self.extract_calls_from_expr(val, stmt_index);
                }
            }
            AstNode::YieldFrom { value, .. } => {
                self.extract_calls_from_expr(value, stmt_index);
            }
            AstNode::Await { value, .. } => {
                self.extract_calls_from_expr(value, stmt_index);
            }
            AstNode::Starred { value, .. } => {
                self.extract_calls_from_expr(value, stmt_index);
            }
            AstNode::NamedExpr { value, .. } => {
                self.extract_calls_from_expr(value, stmt_index);
            }
            _ => {}
        }
    }

    fn build_stmt(&mut self, stmt: &AstNode, stmt_index: &mut usize) {
        match stmt {
            AstNode::If { test, body, elif_parts, else_body, .. } => {
                self.build_if(test, body, elif_parts, else_body, stmt_index);
            }
            AstNode::For { target: _, iter: _, body, else_body, .. } => {
                self.build_for(body, else_body.as_ref(), stmt_index);
            }
            AstNode::While { test: _, body, else_body, .. } => {
                self.build_while(body, else_body.as_ref(), stmt_index);
            }
            AstNode::Try { body, handlers, else_body, finally_body, .. } => {
                self.build_try(body, handlers, else_body.as_ref(), finally_body.as_ref(), stmt_index);
            }
            AstNode::With { items: _, body, .. } => {
                self.build_with(body, stmt_index);
            }
            AstNode::Match { subject: _, cases, .. } => {
                self.build_match(cases, stmt_index);
            }
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
                    AstNode::Assignment { value, .. } => {
                        self.extract_calls_from_expr(value, *stmt_index);
                    }
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
                    | AstNode::Await { .. } => {
                        self.extract_calls_from_expr(stmt, *stmt_index);
                    }
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

    mod cross_module {
        use super::*;

        fn test_uri(path: &str) -> Url {
            Url::parse(&format!("file:///{}", path)).unwrap()
        }

        #[test]
        fn test_function_id_creation() {
            let uri = test_uri("test.py");
            let scope_id = ScopeId::from_raw(1);
            let func_id = FunctionId::new(uri.clone(), scope_id, "test_func".to_string());

            assert_eq!(func_id.uri, uri);
            assert_eq!(func_id.scope_id, scope_id);
            assert_eq!(func_id.name, "test_func");
        }

        #[test]
        fn test_function_id_equality() {
            let uri = test_uri("test.py");
            let scope_id = ScopeId::from_raw(1);

            let func1 = FunctionId::new(uri.clone(), scope_id, "foo".to_string());
            let func2 = FunctionId::new(uri.clone(), scope_id, "foo".to_string());
            let func3 = FunctionId::new(uri.clone(), ScopeId::from_raw(2), "bar".to_string());

            assert_eq!(func1, func2);
            assert_ne!(func1, func3);
        }

        #[test]
        fn test_call_site_creation() {
            let block_id = BlockId(5);
            let callee = FunctionId::new(test_uri("test.py"), ScopeId::from_raw(1), "foo".to_string());

            let call_site = CallSite::new(block_id, 10, Some(callee.clone()), CallKind::Direct, 42, 15);

            assert_eq!(call_site.block_id, block_id);
            assert_eq!(call_site.stmt_index, 10);
            assert_eq!(call_site.receiver, Some(callee));
            assert_eq!(call_site.kind, CallKind::Direct);
            assert_eq!(call_site.line, 42);
            assert_eq!(call_site.col, 15);
        }

        #[test]
        fn test_call_site_unresolved_callee() {
            let call_site = CallSite::new(BlockId(1), 5, None, CallKind::Dynamic, 10, 5);

            assert!(call_site.receiver.is_none());
            assert_eq!(call_site.kind, CallKind::Dynamic);
        }

        #[test]
        fn test_call_graph_add_call_site() {
            let mut call_graph = CallGraph::new();

            let caller = FunctionId::new(test_uri("a.py"), ScopeId::from_raw(1), "caller".to_string());
            let callee = FunctionId::new(test_uri("b.py"), ScopeId::from_raw(2), "callee".to_string());

            let call_site = CallSite::new(BlockId(1), 0, Some(callee.clone()), CallKind::Direct, 10, 5);

            call_graph.add_call_site(caller.clone(), call_site);

            let sites = call_graph.get_call_sites(&caller).unwrap();
            assert_eq!(sites.len(), 1);
            assert_eq!(sites[0].receiver, Some(callee.clone()));

            let callers = call_graph.get_callers(&callee).unwrap();
            assert_eq!(callers.len(), 1);
            assert_eq!(callers[0], caller);
        }

        #[test]
        fn test_call_graph_multiple_call_sites() {
            let mut call_graph = CallGraph::new();

            let caller = FunctionId::new(test_uri("a.py"), ScopeId::from_raw(1), "caller".to_string());
            let callee1 = FunctionId::new(test_uri("b.py"), ScopeId::from_raw(2), "callee1".to_string());
            let callee2 = FunctionId::new(test_uri("c.py"), ScopeId::from_raw(3), "callee2".to_string());

            let call1 = CallSite::new(BlockId(1), 0, Some(callee1.clone()), CallKind::Direct, 10, 5);
            let call2 = CallSite::new(BlockId(2), 1, Some(callee2.clone()), CallKind::Method, 15, 8);

            call_graph.add_call_site(caller.clone(), call1);
            call_graph.add_call_site(caller.clone(), call2);

            let sites = call_graph.get_call_sites(&caller).unwrap();
            assert_eq!(sites.len(), 2);

            let callees = call_graph.get_callees(&caller);
            assert_eq!(callees.len(), 2);
            assert!(callees.contains(&callee1));
            assert!(callees.contains(&callee2));
        }

        #[test]
        fn test_call_graph_get_callees_with_unresolved() {
            let mut call_graph = CallGraph::new();

            let caller = FunctionId::new(test_uri("a.py"), ScopeId::from_raw(1), "caller".to_string());
            let callee = FunctionId::new(test_uri("b.py"), ScopeId::from_raw(2), "callee".to_string());

            call_graph.add_call_site(
                caller.clone(),
                CallSite::new(BlockId(1), 0, Some(callee.clone()), CallKind::Direct, 10, 5),
            );
            call_graph.add_call_site(
                caller.clone(),
                CallSite::new(BlockId(2), 1, None, CallKind::Dynamic, 15, 8),
            );

            let callees = call_graph.get_callees(&caller);
            assert_eq!(callees.len(), 1);
            assert_eq!(callees[0], callee);
        }

        #[test]
        fn test_call_graph_reachable_functions_simple_chain() {
            let mut call_graph = CallGraph::new();

            let func1 = FunctionId::new(test_uri("a.py"), ScopeId::from_raw(1), "func1".to_string());
            let func2 = FunctionId::new(test_uri("b.py"), ScopeId::from_raw(2), "func2".to_string());
            let func3 = FunctionId::new(test_uri("c.py"), ScopeId::from_raw(3), "func3".to_string());

            call_graph.add_call_site(
                func1.clone(),
                CallSite::new(BlockId(1), 0, Some(func2.clone()), CallKind::Direct, 10, 5),
            );
            call_graph.add_call_site(
                func2.clone(),
                CallSite::new(BlockId(1), 0, Some(func3.clone()), CallKind::Direct, 20, 5),
            );

            let reachable = call_graph.reachable_functions(&[func1.clone()]);

            assert_eq!(reachable.len(), 3);
            assert!(reachable.contains(&func1));
            assert!(reachable.contains(&func2));
            assert!(reachable.contains(&func3));
        }

        #[test]
        fn test_call_graph_reachable_functions_diamond() {
            let mut call_graph = CallGraph::new();

            let entry = FunctionId::new(test_uri("a.py"), ScopeId::from_raw(1), "entry".to_string());
            let left = FunctionId::new(test_uri("b.py"), ScopeId::from_raw(2), "left".to_string());
            let right = FunctionId::new(test_uri("c.py"), ScopeId::from_raw(3), "right".to_string());
            let bottom = FunctionId::new(test_uri("d.py"), ScopeId::from_raw(4), "bottom".to_string());

            call_graph.add_call_site(
                entry.clone(),
                CallSite::new(BlockId(1), 0, Some(left.clone()), CallKind::Direct, 10, 5),
            );
            call_graph.add_call_site(
                entry.clone(),
                CallSite::new(BlockId(2), 1, Some(right.clone()), CallKind::Direct, 15, 5),
            );
            call_graph.add_call_site(
                left.clone(),
                CallSite::new(BlockId(1), 0, Some(bottom.clone()), CallKind::Direct, 20, 5),
            );
            call_graph.add_call_site(
                right.clone(),
                CallSite::new(BlockId(1), 0, Some(bottom.clone()), CallKind::Direct, 25, 5),
            );

            let reachable = call_graph.reachable_functions(&[entry.clone()]);

            assert_eq!(reachable.len(), 4);
            assert!(reachable.contains(&entry));
            assert!(reachable.contains(&left));
            assert!(reachable.contains(&right));
            assert!(reachable.contains(&bottom));
        }

        #[test]
        fn test_call_graph_reachable_functions_cycle() {
            let mut call_graph = CallGraph::new();

            let func1 = FunctionId::new(test_uri("a.py"), ScopeId::from_raw(1), "func1".to_string());
            let func2 = FunctionId::new(test_uri("b.py"), ScopeId::from_raw(2), "func2".to_string());
            let func3 = FunctionId::new(test_uri("c.py"), ScopeId::from_raw(3), "func3".to_string());

            call_graph.add_call_site(
                func1.clone(),
                CallSite::new(BlockId(1), 0, Some(func2.clone()), CallKind::Direct, 10, 5),
            );
            call_graph.add_call_site(
                func2.clone(),
                CallSite::new(BlockId(1), 0, Some(func3.clone()), CallKind::Direct, 20, 5),
            );
            call_graph.add_call_site(
                func3.clone(),
                CallSite::new(BlockId(1), 0, Some(func1.clone()), CallKind::Direct, 30, 5),
            );

            let reachable = call_graph.reachable_functions(&[func1.clone()]);

            assert_eq!(reachable.len(), 3);
            assert!(reachable.contains(&func1));
            assert!(reachable.contains(&func2));
            assert!(reachable.contains(&func3));
        }

        #[test]
        fn test_call_graph_unreachable_functions() {
            let mut call_graph = CallGraph::new();

            let func1 = FunctionId::new(test_uri("a.py"), ScopeId::from_raw(1), "func1".to_string());
            let func2 = FunctionId::new(test_uri("b.py"), ScopeId::from_raw(2), "func2".to_string());
            let func3 = FunctionId::new(test_uri("c.py"), ScopeId::from_raw(3), "func3".to_string());

            call_graph.add_call_site(
                func1.clone(),
                CallSite::new(BlockId(1), 0, Some(func2.clone()), CallKind::Direct, 10, 5),
            );

            let reachable = call_graph.reachable_functions(&[func1.clone()]);

            assert_eq!(reachable.len(), 2);
            assert!(reachable.contains(&func1));
            assert!(reachable.contains(&func2));
            assert!(!reachable.contains(&func3));
        }

        #[test]
        fn test_module_cfg_creation() {
            let uri = test_uri("test.py");
            let module_cfg = ModuleCFG::new(uri.clone(), "test".to_string());

            assert_eq!(module_cfg.uri, uri);
            assert_eq!(module_cfg.module_name, "test");
            assert_eq!(module_cfg.function_cfgs.len(), 0);
            assert_eq!(module_cfg.call_sites.len(), 0);
        }

        #[test]
        fn test_module_cfg_add_function() {
            let uri = test_uri("test.py");
            let mut module_cfg = ModuleCFG::new(uri, "test".to_string());

            let cfg = ControlFlowGraph::new();
            let scope_id = ScopeId::from_raw(1);

            module_cfg.add_function_cfg(scope_id, cfg);

            assert_eq!(module_cfg.function_cfgs.len(), 1);
            assert!(module_cfg.function_cfgs.contains_key(&scope_id));
        }

        #[test]
        fn test_module_cfg_add_call_site() {
            let uri = test_uri("test.py");
            let mut module_cfg = ModuleCFG::new(uri.clone(), "test".to_string());

            let callee = FunctionId::new(test_uri("other.py"), ScopeId::from_raw(2), "foo".to_string());
            let call_site = CallSite::new(BlockId(1), 0, Some(callee), CallKind::Direct, 10, 5);

            module_cfg.add_call_site(call_site);

            assert_eq!(module_cfg.call_sites.len(), 1);
        }

        #[test]
        fn test_workspace_cfg_creation() {
            let workspace_cfg = WorkspaceCFG::new();
            assert_eq!(workspace_cfg.modules.len(), 0);
            assert_eq!(workspace_cfg.entry_points.len(), 0);
        }

        #[test]
        fn test_workspace_cfg_add_module() {
            let mut workspace_cfg = WorkspaceCFG::new();

            let uri = test_uri("test.py");
            let module_cfg = ModuleCFG::new(uri.clone(), "test".to_string());

            workspace_cfg.add_module(module_cfg);

            assert_eq!(workspace_cfg.modules.len(), 1);
            assert!(workspace_cfg.get_module(&uri).is_some());
        }

        #[test]
        fn test_workspace_cfg_add_entry_point() {
            let mut workspace_cfg = WorkspaceCFG::new();

            let entry = FunctionId::new(test_uri("main.py"), ScopeId::from_raw(1), "main".to_string());
            workspace_cfg.add_entry_point(entry.clone());

            let entry_points = workspace_cfg.entry_points();
            assert_eq!(entry_points.len(), 1);
            assert_eq!(entry_points[0], entry);
        }

        #[test]
        fn test_workspace_cfg_reachable_functions() {
            let mut workspace_cfg = WorkspaceCFG::new();

            let func1 = FunctionId::new(test_uri("a.py"), ScopeId::from_raw(1), "func1".to_string());
            let func2 = FunctionId::new(test_uri("b.py"), ScopeId::from_raw(2), "func2".to_string());

            workspace_cfg.add_entry_point(func1.clone());

            let call_site = CallSite::new(BlockId(1), 0, Some(func2.clone()), CallKind::Direct, 10, 5);
            workspace_cfg.call_graph_mut().add_call_site(func1.clone(), call_site);

            let reachable = workspace_cfg.reachable_functions();
            assert_eq!(reachable.len(), 2);
            assert!(reachable.contains(&func1));
            assert!(reachable.contains(&func2));
        }

        #[test]
        fn test_workspace_cfg_cross_module_reachable_blocks() {
            let mut workspace_cfg = WorkspaceCFG::new();

            let uri1 = test_uri("a.py");
            let uri2 = test_uri("b.py");

            let func1 = FunctionId::new(uri1.clone(), ScopeId::from_raw(1), "func1".to_string());
            let func2 = FunctionId::new(uri2.clone(), ScopeId::from_raw(2), "func2".to_string());

            let mut module1 = ModuleCFG::new(uri1.clone(), "a".to_string());
            let cfg1 = ControlFlowGraph::new();
            module1.add_function_cfg(ScopeId::from_raw(1), cfg1);

            let mut module2 = ModuleCFG::new(uri2.clone(), "b".to_string());
            let cfg2 = ControlFlowGraph::new();
            module2.add_function_cfg(ScopeId::from_raw(2), cfg2);

            workspace_cfg.add_module(module1);
            workspace_cfg.add_module(module2);

            let call_site = CallSite::new(BlockId(1), 0, Some(func2.clone()), CallKind::Direct, 10, 5);
            workspace_cfg.call_graph_mut().add_call_site(func1.clone(), call_site);

            let reachable = workspace_cfg.cross_module_reachable_blocks(&[func1.clone()]);

            assert_eq!(reachable.len(), 2);
            assert!(reachable.contains_key(&func1));
            assert!(reachable.contains_key(&func2));
        }

        #[test]
        fn test_call_extraction_simple_call() {
            let mut builder = CfgBuilder::new();
            let body = vec![AstNode::Call {
                function: Box::new(AstNode::Identifier {
                    name: "foo".to_string(),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 4,
                }),
                args: vec![],
                keywords: vec![],
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 6,
            }];

            builder.build_function(&body);
            let call_sites = builder.call_sites();

            assert_eq!(call_sites.len(), 1);
            assert_eq!(call_sites[0].1, 0);
        }

        #[test]
        fn test_call_extraction_assignment() {
            let mut builder = CfgBuilder::new();
            let body = vec![AstNode::Assignment {
                target: Box::new(AstNode::Identifier {
                    name: "x".to_string(),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 2,
                }),
                value: Box::new(AstNode::Call {
                    function: Box::new(AstNode::Identifier {
                        name: "foo".to_string(),
                        line: 1,
                        col: 5,
                        end_line: 1,
                        end_col: 8,
                    }),
                    args: vec![],
                    keywords: vec![],
                    line: 1,
                    col: 5,
                    end_line: 1,
                    end_col: 10,
                }),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 10,
            }];

            builder.build_function(&body);
            let call_sites = builder.call_sites();

            assert_eq!(call_sites.len(), 1);
        }

        #[test]
        fn test_call_extraction_nested_calls() {
            let mut builder = CfgBuilder::new();
            let body = vec![AstNode::Call {
                function: Box::new(AstNode::Identifier {
                    name: "outer".to_string(),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 6,
                }),
                args: vec![AstNode::Call {
                    function: Box::new(AstNode::Identifier {
                        name: "inner".to_string(),
                        line: 1,
                        col: 7,
                        end_line: 1,
                        end_col: 12,
                    }),
                    args: vec![],
                    keywords: vec![],
                    line: 1,
                    col: 7,
                    end_line: 1,
                    end_col: 14,
                }],
                keywords: vec![],
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 15,
            }];

            builder.build_function(&body);
            let call_sites = builder.call_sites();

            assert_eq!(call_sites.len(), 2);
        }

        #[test]
        fn test_call_extraction_return_statement() {
            let mut builder = CfgBuilder::new();
            let body = vec![AstNode::Return {
                value: Some(Box::new(AstNode::Call {
                    function: Box::new(AstNode::Identifier {
                        name: "foo".to_string(),
                        line: 1,
                        col: 8,
                        end_line: 1,
                        end_col: 11,
                    }),
                    args: vec![],
                    keywords: vec![],
                    line: 1,
                    col: 8,
                    end_line: 1,
                    end_col: 13,
                })),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 13,
            }];

            builder.build_function(&body);
            let call_sites = builder.call_sites();

            assert_eq!(call_sites.len(), 1);
        }

        #[test]
        fn test_call_extraction_method_call() {
            let mut builder = CfgBuilder::new();
            let body = vec![AstNode::Call {
                function: Box::new(AstNode::Attribute {
                    object: Box::new(AstNode::Identifier {
                        name: "obj".to_string(),
                        line: 1,
                        col: 1,
                        end_line: 1,
                        end_col: 4,
                    }),
                    attribute: "method".to_string(),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 11,
                }),
                args: vec![],
                keywords: vec![],
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 13,
            }];

            builder.build_function(&body);
            let call_sites = builder.call_sites();

            assert_eq!(call_sites.len(), 1);
        }

        #[test]
        fn test_call_extraction_in_list_comp() {
            let mut builder = CfgBuilder::new();
            let body = vec![AstNode::Assignment {
                target: Box::new(AstNode::Identifier {
                    name: "result".to_string(),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 7,
                }),
                value: Box::new(AstNode::ListComp {
                    element: Box::new(AstNode::Call {
                        function: Box::new(AstNode::Identifier {
                            name: "transform".to_string(),
                            line: 1,
                            col: 11,
                            end_line: 1,
                            end_col: 20,
                        }),
                        args: vec![AstNode::Identifier {
                            name: "x".to_string(),
                            line: 1,
                            col: 21,
                            end_line: 1,
                            end_col: 22,
                        }],
                        keywords: vec![],
                        line: 1,
                        col: 11,
                        end_line: 1,
                        end_col: 23,
                    }),
                    generators: vec![],
                    line: 1,
                    col: 10,
                    end_line: 1,
                    end_col: 40,
                }),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 40,
            }];

            builder.build_function(&body);
            let call_sites = builder.call_sites();

            assert_eq!(call_sites.len(), 1);
        }

        #[test]
        fn test_take_call_sites() {
            let mut builder = CfgBuilder::new();
            let body = vec![AstNode::Call {
                function: Box::new(AstNode::Identifier {
                    name: "foo".to_string(),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 4,
                }),
                args: vec![],
                keywords: vec![],
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 6,
            }];

            builder.build_function(&body);
            let (cfg, call_sites) = builder.take_call_sites();

            assert_eq!(call_sites.len(), 1);
            assert!(!cfg.blocks.is_empty());
        }
    }
}

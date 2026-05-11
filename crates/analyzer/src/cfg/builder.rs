use beacon_parser::AstNode;
use url::Url;

use super::call_graph::CallSite;
use super::graph::{BlockId, ControlFlowGraph, EdgeKind};
use super::resolver::CallResolver;

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
            AstNode::Yield { value: Some(val), .. } => self.extract_calls_from_expr(val, stmt_index),
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
                self.extract_calls_from_expr(test, *stmt_index);
                if let Some(block) = self.cfg.blocks.get_mut(&self.ctx.current) {
                    block.statements.push(*stmt_index);
                }
                *stmt_index += 1;
                self.build_if(test, body, elif_parts, else_body, stmt_index)
            }
            AstNode::For { iter, body, else_body, .. } => {
                self.extract_calls_from_expr(iter, *stmt_index);
                let for_stmt_idx = *stmt_index;
                *stmt_index += 1;
                self.build_for(for_stmt_idx, body, else_body.as_ref(), stmt_index)
            }
            AstNode::While { test, body, else_body, .. } => {
                self.extract_calls_from_expr(test, *stmt_index);
                let while_stmt_idx = *stmt_index;
                *stmt_index += 1;
                self.build_while(while_stmt_idx, body, else_body.as_ref(), stmt_index)
            }
            AstNode::Try { body, handlers, else_body, finally_body, .. } => {
                self.build_try(body, handlers, else_body.as_ref(), finally_body.as_ref(), stmt_index)
            }
            AstNode::With { body, .. } => {
                // TODO: capture context manager expressions for call extraction
                if let Some(block) = self.cfg.blocks.get_mut(&self.ctx.current) {
                    block.statements.push(*stmt_index);
                }
                *stmt_index += 1;
                self.build_with(body, stmt_index)
            }
            AstNode::Match { subject, cases, .. } => {
                self.extract_calls_from_expr(subject, *stmt_index);
                if let Some(block) = self.cfg.blocks.get_mut(&self.ctx.current) {
                    block.statements.push(*stmt_index);
                }
                *stmt_index += 1;
                self.build_match(cases, stmt_index)
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
                    AstNode::Assignment { value, .. } => self.extract_calls_from_expr(value, *stmt_index),
                    AstNode::AnnotatedAssignment { value: Some(val), .. } => {
                        self.extract_calls_from_expr(val, *stmt_index);
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

    fn build_for(
        &mut self, stmt_idx: usize, body: &[AstNode], else_body: Option<&Vec<AstNode>>, stmt_index: &mut usize,
    ) {
        let loop_header = self.cfg.new_block();
        let loop_body = self.cfg.new_block();
        let loop_exit = self.cfg.new_block();

        self.cfg.add_edge(self.ctx.current, loop_header, EdgeKind::Normal);
        if let Some(header_block) = self.cfg.blocks.get_mut(&loop_header) {
            header_block.statements.push(stmt_idx);
        }
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

    fn build_while(
        &mut self, stmt_idx: usize, body: &[AstNode], else_body: Option<&Vec<AstNode>>, stmt_index: &mut usize,
    ) {
        self.build_for(stmt_idx, body, else_body, stmt_index);
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

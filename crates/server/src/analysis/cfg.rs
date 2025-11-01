//! Control Flow Graph (CFG) construction and analysis
//!
//! Represents the control flow structure of Python functions for static analysis.
//! Each function gets its own CFG with basic blocks connected by edges.

use beacon_parser::AstNode;
use rustc_hash::FxHashMap;
use std::collections::VecDeque;

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
}

impl CfgBuilder {
    pub fn new() -> Self {
        let cfg = ControlFlowGraph::new();
        let ctx = BuildContext { current: cfg.entry, loops: Vec::new(), finally_blocks: Vec::new() };
        Self { cfg, ctx }
    }

    pub fn build(self) -> ControlFlowGraph {
        self.cfg
    }

    /// Build CFG for a function body
    pub fn build_function(&mut self, body: &[AstNode]) {
        let mut stmt_index = 0;
        for stmt in body {
            self.build_stmt(stmt, &mut stmt_index);
        }

        self.cfg.add_edge(self.ctx.current, self.cfg.exit, EdgeKind::Normal);
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
            AstNode::Return { .. } | AstNode::Raise { .. } => {
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
    use beacon_parser::{AstNode, ExceptHandler, WithItem};

    #[test]
    fn test_cfg_creation() {
        let cfg = ControlFlowGraph::new();
        assert_eq!(cfg.blocks.len(), 2); // entry + exit
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
            test: Box::new(AstNode::Identifier { name: "cond".to_string(), line: 1, col: 4 }),
            body: vec![AstNode::Pass { line: 2, col: 5 }],
            elif_parts: vec![],
            else_body: None,
            line: 1,
            col: 1,
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
            test: Box::new(AstNode::Identifier { name: "cond".to_string(), line: 1, col: 4 }),
            body: vec![AstNode::Pass { line: 2, col: 5 }],
            elif_parts: vec![],
            else_body: Some(vec![AstNode::Pass { line: 4, col: 5 }]),
            line: 1,
            col: 1,
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
            target: "i".to_string(),
            iter: Box::new(AstNode::Identifier { name: "items".to_string(), line: 1, col: 14 }),
            body: vec![AstNode::Pass { line: 2, col: 5 }],
            else_body: None,
            is_async: false,
            line: 1,
            col: 1,
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
            target: "i".to_string(),
            iter: Box::new(AstNode::Identifier { name: "items".to_string(), line: 1, col: 14 }),
            body: vec![AstNode::Break { line: 2, col: 5 }],
            else_body: None,
            is_async: false,
            line: 1,
            col: 1,
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
            target: "i".to_string(),
            iter: Box::new(AstNode::Identifier { name: "items".to_string(), line: 1, col: 14 }),
            body: vec![AstNode::Continue { line: 2, col: 5 }],
            else_body: None,
            is_async: false,
            line: 1,
            col: 1,
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
        use beacon_parser::AstNode;

        let mut builder = CfgBuilder::new();
        let body = vec![AstNode::While {
            test: Box::new(AstNode::Identifier { name: "cond".to_string(), line: 1, col: 7 }),
            body: vec![AstNode::Pass { line: 2, col: 5 }],
            else_body: None,
            line: 1,
            col: 1,
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
            body: vec![AstNode::Pass { line: 2, col: 5 }],
            handlers: vec![ExceptHandler {
                exception_type: None,
                name: None,
                body: vec![AstNode::Pass { line: 4, col: 5 }],
                line: 3,
                col: 1,
            }],
            else_body: None,
            finally_body: None,
            line: 1,
            col: 1,
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
            body: vec![AstNode::Pass { line: 2, col: 5 }],
            handlers: vec![],
            else_body: None,
            finally_body: Some(vec![AstNode::Pass { line: 4, col: 5 }]),
            line: 1,
            col: 1,
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
                context_expr: AstNode::Identifier { name: "ctx".to_string(), line: 1, col: 6 },
                optional_vars: None,
            }],
            body: vec![AstNode::Pass { line: 2, col: 5 }],
            is_async: false,
            line: 1,
            col: 1,
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
        use beacon_parser::{AstNode, LiteralValue, MatchCase, Pattern};

        let mut builder = CfgBuilder::new();
        let body = vec![AstNode::Match {
            subject: Box::new(AstNode::Identifier { name: "x".to_string(), line: 1, col: 7 }),
            cases: vec![
                MatchCase {
                    pattern: Pattern::MatchValue(AstNode::Literal {
                        value: LiteralValue::Integer(1),
                        line: 2,
                        col: 10,
                    }),
                    guard: None,
                    body: vec![AstNode::Pass { line: 3, col: 9 }],
                },
                MatchCase {
                    pattern: Pattern::MatchValue(AstNode::Literal {
                        value: LiteralValue::Integer(2),
                        line: 4,
                        col: 10,
                    }),
                    guard: None,
                    body: vec![AstNode::Pass { line: 5, col: 9 }],
                },
            ],
            line: 1,
            col: 1,
        }];

        builder.build_function(&body);
        let cfg = builder.build();

        let true_edges: Vec<_> = cfg
            .blocks
            .values()
            .flat_map(|block| block.successors.iter())
            .filter(|(_, kind)| *kind == EdgeKind::True)
            .collect();
        assert!(true_edges.len() >= 2);
    }

    #[test]
    fn test_cfg_return_creates_unreachable_block() {
        use beacon_parser::AstNode;

        let mut builder = CfgBuilder::new();
        let body = vec![
            AstNode::Return { value: None, line: 1, col: 1 },
            AstNode::Pass { line: 2, col: 1 },
        ];

        builder.build_function(&body);
        let cfg = builder.build();

        let unreachable = cfg.unreachable_blocks();
        assert!(!unreachable.is_empty());
    }

    #[test]
    fn test_cfg_nested_if_statements() {
        use beacon_parser::AstNode;

        let mut builder = CfgBuilder::new();
        let body = vec![AstNode::If {
            test: Box::new(AstNode::Identifier { name: "cond1".to_string(), line: 1, col: 4 }),
            body: vec![AstNode::If {
                test: Box::new(AstNode::Identifier { name: "cond2".to_string(), line: 2, col: 8 }),
                body: vec![AstNode::Pass { line: 3, col: 9 }],
                elif_parts: vec![],
                else_body: None,
                line: 2,
                col: 5,
            }],
            elif_parts: vec![],
            else_body: None,
            line: 1,
            col: 1,
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
        use beacon_parser::AstNode;

        let mut builder = CfgBuilder::new();
        let body = vec![
            AstNode::Assignment {
                target: "x".to_string(),
                value: Box::new(AstNode::Literal { value: beacon_parser::LiteralValue::Integer(1), line: 1, col: 5 }),
                line: 1,
                col: 1,
            },
            AstNode::For {
                target: "i".to_string(),
                iter: Box::new(AstNode::Identifier { name: "range(10)".to_string(), line: 2, col: 14 }),
                body: vec![AstNode::If {
                    test: Box::new(AstNode::Identifier { name: "cond".to_string(), line: 3, col: 12 }),
                    body: vec![AstNode::Break { line: 4, col: 13 }],
                    elif_parts: vec![],
                    else_body: Some(vec![AstNode::Continue { line: 6, col: 13 }]),
                    line: 3,
                    col: 9,
                }],
                else_body: None,
                is_async: false,
                line: 2,
                col: 1,
            },
            AstNode::Return { value: None, line: 7, col: 1 },
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
}

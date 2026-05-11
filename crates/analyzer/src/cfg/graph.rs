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
    pub(super) fn new(id: BlockId) -> Self {
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

    pub fn new_block(&mut self) -> BlockId {
        let id = BlockId(self.next_block_id);
        self.next_block_id += 1;
        self.blocks.insert(id, BasicBlock::new(id));
        id
    }

    pub fn add_edge(&mut self, from: BlockId, to: BlockId, kind: EdgeKind) {
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

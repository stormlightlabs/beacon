pub mod exhaustiveness;
pub mod solver;

use beacon_core::{ClassRegistry, Type, TypeError};
use rustc_hash::FxHashMap;

/// Type error with location information
#[derive(Debug, Clone)]
pub struct TypeErrorInfo {
    pub error: TypeError,
    /// Span where the error occurred
    pub span: Span,
}

impl TypeErrorInfo {
    /// Create a new type error with location
    pub fn new(error: TypeError, span: Span) -> Self {
        Self { error, span }
    }

    pub fn line(&self) -> usize {
        self.span.line
    }

    pub fn col(&self) -> usize {
        self.span.col
    }

    pub fn end_line(&self) -> Option<usize> {
        self.span.end_line
    }

    pub fn end_col(&self) -> Option<usize> {
        self.span.end_col
    }
}

/// Type constraint with source position tracking
///
/// Each constraint variant includes a [Span] to track where the constraint
/// originated in the source code, enabling precise error reporting.
#[derive(Debug, Clone)]
pub enum Constraint {
    /// Type equality constraint: t1 ~ t2
    Equal(Type, Type, Span),
    /// HasAttr constraint: τ has attribute "name" : τ'
    HasAttr(Type, String, Type, Span),
    /// Call constraint: f(args) -> ret
    Call(Type, Vec<Type>, Type, Span),
    /// Protocol constraint: τ satisfies protocol P
    /// The type parameter is the element type extracted from satisfying the protocol
    Protocol(Type, beacon_core::ProtocolName, Type, Span),
    /// Pattern matching constraint: subject type must be compatible with pattern
    /// Stores bindings extracted from the pattern (variable name -> type)
    MatchPattern(Type, beacon_parser::Pattern, Vec<(String, Type)>, Span),
    /// Exhaustiveness constraint: patterns must cover all possible values of subject type
    /// Stores subject type and all case patterns for checking
    PatternExhaustive(Type, Vec<beacon_parser::Pattern>, Span),
    /// Reachability constraint: pattern must be reachable (not subsumed by previous patterns)
    /// Stores the pattern and all previous patterns in the match statement
    PatternReachable(beacon_parser::Pattern, Vec<beacon_parser::Pattern>, Span),
}

pub struct ConstraintResult(
    pub ConstraintSet,
    pub FxHashMap<usize, Type>,
    pub FxHashMap<(usize, usize), usize>,
    pub ClassRegistry,
);

/// Context for tracking node information during constraint generation
pub struct ConstraintGenContext {
    pub constraints: Vec<Constraint>,
    pub node_counter: usize,
    pub type_map: FxHashMap<usize, Type>,
    pub position_map: FxHashMap<(usize, usize), usize>,
    pub class_registry: ClassRegistry,
}

impl ConstraintGenContext {
    pub fn new() -> Self {
        Self::default()
    }

    /// Record a type for a node at a specific position, returning the node ID
    pub fn record_type(&mut self, line: usize, col: usize, ty: Type) -> usize {
        let node_id = self.node_counter;
        self.node_counter += 1;
        self.type_map.insert(node_id, ty);
        self.position_map.insert((line, col), node_id);
        node_id
    }
}

impl Default for ConstraintGenContext {
    fn default() -> Self {
        Self {
            constraints: Vec::new(),
            node_counter: 0,
            type_map: FxHashMap::default(),
            position_map: FxHashMap::default(),
            class_registry: ClassRegistry::new(),
        }
    }
}

/// Set of type constraints
///
/// TODO: Replace with [beacon_core] constraint types
pub struct ConstraintSet {
    pub constraints: Vec<Constraint>,
}

/// Source code span for tracking positions
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    /// Line number (1-indexed)
    pub line: usize,
    /// Column number (1-indexed)
    pub col: usize,
    /// Optional end position for range
    pub end_line: Option<usize>,
    pub end_col: Option<usize>,
}

impl Span {
    /// Create a new span from a line and column
    pub fn new(line: usize, col: usize) -> Self {
        Self { line, col, end_line: None, end_col: None }
    }

    /// Create a new span with an end position
    pub fn with_end(line: usize, col: usize, end_line: usize, end_col: usize) -> Self {
        Self { line, col, end_line: Some(end_line), end_col: Some(end_col) }
    }
}

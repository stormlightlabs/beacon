pub mod exhaustiveness;
pub mod solver;

use beacon_core::{ClassRegistry, Type, TypeError};
use rustc_hash::FxHashMap;

/// Type predicates for flow-sensitive narrowing
///
/// Represents conditions that can refine types through control flow analysis.
/// Each predicate describes a runtime check that, when true, allows the type
/// checker to narrow the type of a variable.
#[derive(Debug, Clone, PartialEq)]
pub enum TypePredicate {
    /// Variable is not None: `x is not None`
    IsNotNone,

    /// Variable is None: `x is None`
    IsNone,

    /// Variable is an instance of a type: `isinstance(x, T)`
    /// Can also represent isinstance with multiple types: `isinstance(x, (T1, T2))`
    IsInstance(Type),
}

impl TypePredicate {
    /// Apply the predicate to a type, returning the narrowed type
    ///
    /// This method implements the semantics of each predicate by transforming
    /// the input type according to the runtime check the predicate represents.
    pub fn apply(&self, ty: &Type) -> Type {
        match self {
            TypePredicate::IsNotNone => ty.remove_from_union(&Type::none()),
            TypePredicate::IsNone => Type::none(),
            TypePredicate::IsInstance(target) => match ty {
                Type::Union(variants) => {
                    if variants.contains(target) {
                        target.clone()
                    } else if let Type::Union(target_variants) = target {
                        let intersection: Vec<Type> = variants
                            .iter()
                            .filter(|v| target_variants.contains(v))
                            .cloned()
                            .collect();

                        if intersection.is_empty() {
                            target.clone()
                        } else if intersection.len() == 1 {
                            intersection.into_iter().next().unwrap()
                        } else {
                            Type::union(intersection)
                        }
                    } else {
                        target.clone()
                    }
                }
                _ => target.clone(),
            },
        }
    }

    /// Get the inverse predicate for narrowing in the else branch of an if statement.
    pub fn negate(&self) -> TypePredicate {
        match self {
            TypePredicate::IsNotNone => TypePredicate::IsNone,
            TypePredicate::IsNone => TypePredicate::IsNotNone,
            TypePredicate::IsInstance(_) => TypePredicate::IsNotNone,
        }
    }

    /// Check if this predicate has a meaningful negation for constraint generation
    ///
    /// Some predicates like isinstance don't have simple inverse predicates,
    /// and their negation is handled by type subtraction in detect_inverse_type_guard.
    pub fn has_simple_negation(&self) -> bool {
        !matches!(self, TypePredicate::IsInstance(_))
    }
}

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
    /// Type narrowing constraint based on a predicate
    /// Narrowing(variable, predicate, narrowed_type, span)
    ///
    /// Semantics: If predicate holds, variable has type narrowed_type
    Narrowing(String, TypePredicate, Type, Span),
    /// Join point constraint - merge types from multiple paths
    /// Join(variable, incoming_types, result_type, span)
    ///
    /// Semantics: result_type is the union of all incoming_types
    Join(String, Vec<Type>, Type, Span),
}

pub struct ConstraintResult(
    pub ConstraintSet,
    pub FxHashMap<usize, Type>,
    pub FxHashMap<(usize, usize), usize>,
    pub ClassRegistry,
);

/// A single narrowing scope in the control flow stack
#[derive(Debug, Clone, Default)]
struct NarrowingScope {
    /// Variables that have been narrowed in this scope
    narrowed_vars: FxHashMap<String, Type>,
    /// The predicate that enables this narrowing (TODO)
    #[allow(dead_code)]
    predicate: Option<TypePredicate>,
}

/// A join point where control flow paths merge
#[derive(Debug, Clone)]
#[allow(dead_code)]
struct JoinPoint {
    /// Variable being joined
    variable: String,

    /// Types from different control flow paths
    incoming_types: Vec<Type>,
    /// Location of the join point
    span: Span,
}

/// Control flow context for tracking narrowing scope
///
/// Manages the stack of narrowing scopes and pending join points during
/// constraint generation for flow-sensitive type narrowing.
#[derive(Debug, Clone)]
pub struct ControlFlowContext {
    /// Stack of active narrowing scopes (for nested ifs, loops, etc.)
    scopes: Vec<NarrowingScope>,
    /// Join points waiting to be resolved
    pending_joins: Vec<JoinPoint>,
}

impl ControlFlowContext {
    /// Create a new control flow context with a root scope
    pub fn new() -> Self {
        Self { scopes: vec![NarrowingScope::default()], pending_joins: Vec::new() }
    }

    /// Enter a new narrowing scope (e.g., inside an if body)
    pub fn push_scope(&mut self, predicate: Option<TypePredicate>) {
        self.scopes
            .push(NarrowingScope { narrowed_vars: FxHashMap::default(), predicate });
    }

    /// Exit a narrowing scope
    #[allow(private_interfaces)]
    pub fn pop_scope(&mut self) -> Option<NarrowingScope> {
        if self.scopes.len() > 1 { self.scopes.pop() } else { None }
    }

    /// Record a narrowing in the current scope
    pub fn narrow(&mut self, var: String, ty: Type) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.narrowed_vars.insert(var, ty);
        }
    }

    /// Look up a variable's narrowed type
    pub fn get_narrowed_type(&self, var: &str) -> Option<&Type> {
        for scope in self.scopes.iter().rev() {
            if let Some(ty) = scope.narrowed_vars.get(var) {
                return Some(ty);
            }
        }
        None
    }

    /// Register a join point
    pub fn add_join(&mut self, variable: String, types: Vec<Type>, span: Span) {
        self.pending_joins
            .push(JoinPoint { variable, incoming_types: types, span });
    }

    /// Get and clear all pending join points
    #[allow(private_interfaces)]
    pub fn take_pending_joins(&mut self) -> Vec<JoinPoint> {
        std::mem::take(&mut self.pending_joins)
    }
}

impl Default for ControlFlowContext {
    fn default() -> Self {
        Self::new()
    }
}

/// Context for tracking node information during constraint generation
pub struct ConstraintGenContext {
    pub constraints: Vec<Constraint>,
    pub node_counter: usize,
    pub type_map: FxHashMap<usize, Type>,
    pub position_map: FxHashMap<(usize, usize), usize>,
    pub class_registry: ClassRegistry,
    /// Control flow context for flow-sensitive type narrowing
    pub control_flow: ControlFlowContext,
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
            control_flow: ControlFlowContext::new(),
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

pub mod exhaustiveness;
pub mod solver;

mod pattern;
mod predicate;

pub use predicate::TypePredicate;

use beacon_core::{ClassRegistry, Type, TypeCtor, TypeError};
use rustc_hash::FxHashMap;

/// Check if a type is a sequence type (list, tuple)
pub fn is_sequence_type(ty: &Type) -> bool {
    match ty {
        Type::App(ctor, _) => matches!(ctor.as_ref(), Type::Con(TypeCtor::List) | Type::Con(TypeCtor::Tuple)),
        Type::Con(TypeCtor::List | TypeCtor::Tuple) => true,
        _ => false,
    }
}

/// Check if a type is a dict type
pub fn is_dict_type(ty: &Type) -> bool {
    match ty {
        Type::App(ctor, _) => matches!(ctor.as_ref(), Type::Con(TypeCtor::Dict)),
        Type::Con(TypeCtor::Dict) => true,
        _ => false,
    }
}

/// Check if a type is compatible with a class pattern
pub fn type_compatible_with_class(ty: &Type, cls: &str) -> bool {
    match ty {
        Type::Con(TypeCtor::Class(name)) => name == cls,
        _ => false,
    }
}

/// Type guard kind for user-defined type guard functions
#[derive(Debug, Clone, PartialEq)]
pub enum TypeGuardKind {
    /// TypeGuard[T] - narrows but maintains original type relationship
    TypeGuard,
    /// TypeIs[T] - asserts exact type (PEP 742)
    TypeIs,
}

/// Stores metadata for functions with `TypeGuard[T]` or `TypeIs[T]` return annotations (user-defined type guard function).
#[derive(Debug, Clone)]
pub struct TypeGuardInfo {
    /// Which parameter (by index) is being guarded
    /// For example, in `def is_str(x: object) -> TypeGuard[str]`, param_index is 0
    pub param_index: usize,

    /// The type being guarded to (the T in TypeGuard[T])
    pub guarded_type: Type,

    /// Whether this is TypeGuard (narrow) or TypeIs (assert exact type)
    pub kind: TypeGuardKind,
}

impl TypeGuardInfo {
    /// Create a new type guard info
    pub fn new(param_index: usize, guarded_type: Type, kind: TypeGuardKind) -> Self {
        Self { param_index, guarded_type, kind }
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

/// Tracks type elimination through successive narrowing for exhaustiveness checking
#[derive(Debug, Clone)]
pub struct TypeSetTracker {
    /// The variable being tracked
    variable: String,

    /// Original type before any narrowing
    original_type: Type,

    /// Types eliminated in branches so far (accumulated)
    eliminated_types: Vec<Type>,
}

impl TypeSetTracker {
    /// Create a new type set tracker for a variable
    pub fn new(variable: String, original_type: Type) -> Self {
        Self { variable, original_type, eliminated_types: Vec::new() }
    }

    /// Record that a type has been eliminated in a branch
    pub fn eliminate_type(&mut self, ty: Type) {
        // Only add if not already eliminated
        if !self.eliminated_types.contains(&ty) {
            self.eliminated_types.push(ty);
        }
    }

    /// Compute the remaining types after eliminations
    ///
    /// For a union type, this subtracts all eliminated types from the original union.
    pub fn remaining_types(&self) -> Type {
        match &self.original_type {
            Type::Union(variants) => {
                let remaining: Vec<Type> = variants
                    .iter()
                    .filter(|v| !self.eliminated_types.contains(v))
                    .cloned()
                    .collect();

                if remaining.is_empty() {
                    Type::never()
                } else if remaining.len() == 1 {
                    remaining.into_iter().next().unwrap()
                } else {
                    Type::union(remaining)
                }
            }
            _ => {
                if self.eliminated_types.contains(&self.original_type) {
                    Type::never()
                } else {
                    self.original_type.clone()
                }
            }
        }
    }

    /// Check if all variants have been exhaustively covered
    pub fn is_exhaustive(&self) -> bool {
        match &self.original_type {
            Type::Union(variants) => variants.iter().all(|v| self.eliminated_types.contains(v)),
            _ => self.eliminated_types.contains(&self.original_type),
        }
    }

    /// Get the variable name being tracked
    pub fn variable(&self) -> &str {
        &self.variable
    }

    /// Get the original type
    pub fn original_type(&self) -> &Type {
        &self.original_type
    }

    /// Get the list of eliminated types
    pub fn eliminated_types(&self) -> &[Type] {
        &self.eliminated_types
    }
}

/// Control flow context for tracking narrowing scope
#[derive(Debug, Clone)]
pub struct ControlFlowContext {
    /// Stack of active narrowing scopes (for nested ifs, loops, etc.)
    scopes: Vec<NarrowingScope>,
    /// Join points waiting to be resolved
    pending_joins: Vec<JoinPoint>,
    /// Type set trackers for exhaustiveness checking (variable -> tracker)
    type_trackers: FxHashMap<String, TypeSetTracker>,
}

impl ControlFlowContext {
    /// Create a new control flow context with a root scope
    pub fn new() -> Self {
        Self::default()
    }

    /// Enter a new narrowing scope (e.g., inside an if body)
    pub fn push_scope(&mut self, predicate: Option<TypePredicate>) {
        self.scopes
            .push(NarrowingScope { narrowed_vars: FxHashMap::default(), predicate });
    }

    /// Exit a narrowing scope, returning true if a scope was popped
    pub fn pop_scope(&mut self) -> bool {
        if self.scopes.len() > 1 {
            self.scopes.pop();
            true
        } else {
            false
        }
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

    /// Start tracking type elimination for a variable
    pub fn start_tracking(&mut self, variable: String, original_type: Type) {
        self.type_trackers
            .insert(variable.clone(), TypeSetTracker::new(variable, original_type));
    }

    /// Record that a type has been eliminated for a variable
    pub fn eliminate_type(&mut self, variable: &str, eliminated_type: Type) {
        if let Some(tracker) = self.type_trackers.get_mut(variable) {
            tracker.eliminate_type(eliminated_type);
        }
    }

    /// Get the remaining types for a variable after all eliminations
    pub fn get_remaining_types(&self, variable: &str) -> Option<Type> {
        self.type_trackers
            .get(variable)
            .map(|tracker| tracker.remaining_types())
    }

    /// Check if all variants have been exhaustively covered for a variable
    pub fn is_exhaustive(&self, variable: &str) -> bool {
        self.type_trackers
            .get(variable)
            .map(|tracker| tracker.is_exhaustive())
            .unwrap_or(false)
    }

    /// Stop tracking a variable (cleanup after if-elif-else chain)
    pub fn stop_tracking(&mut self, variable: &str) {
        self.type_trackers.remove(variable);
    }

    /// Get a reference to the tracker for a variable
    pub fn get_tracker(&self, variable: &str) -> Option<&TypeSetTracker> {
        self.type_trackers.get(variable)
    }
}

impl Default for ControlFlowContext {
    fn default() -> Self {
        Self {
            scopes: vec![NarrowingScope::default()],
            pending_joins: Vec::new(),
            type_trackers: FxHashMap::default(),
        }
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

#[cfg(test)]
mod tests {
    use super::*;
    use beacon_parser::Pattern;

    #[test]
    fn test_is_sequence_type() {
        let list_type = Type::Con(TypeCtor::List);
        assert!(is_sequence_type(&list_type));

        let tuple_type = Type::Con(TypeCtor::Tuple);
        assert!(is_sequence_type(&tuple_type));

        let list_int = Type::App(Box::new(Type::Con(TypeCtor::List)), Box::new(Type::int()));
        assert!(is_sequence_type(&list_int));

        let int_type = Type::int();
        assert!(!is_sequence_type(&int_type));

        let dict_type = Type::Con(TypeCtor::Dict);
        assert!(!is_sequence_type(&dict_type));
    }

    #[test]
    fn test_is_dict_type() {
        let dict_type = Type::Con(TypeCtor::Dict);
        assert!(is_dict_type(&dict_type));

        let dict_type_app = Type::App(Box::new(Type::Con(TypeCtor::Dict)), Box::new(Type::string()));
        assert!(is_dict_type(&dict_type_app));

        let int_type = Type::int();
        assert!(!is_dict_type(&int_type));

        let list_type = Type::Con(TypeCtor::List);
        assert!(!is_dict_type(&list_type));
    }

    #[test]
    fn test_type_compatible_with_class() {
        let point_type = Type::Con(TypeCtor::Class("Point".to_string()));
        assert!(type_compatible_with_class(&point_type, "Point"));
        assert!(!type_compatible_with_class(&point_type, "Circle"));

        let int_type = Type::int();
        assert!(!type_compatible_with_class(&int_type, "Point"));
    }

    #[test]
    fn test_type_predicate_matches_pattern_apply() {
        let union_type = Type::union(vec![Type::Con(TypeCtor::List), Type::string()]);
        let pattern = Pattern::MatchSequence(vec![]);
        let predicate = TypePredicate::MatchesPattern(pattern);
        let result = predicate.apply(&union_type);
        assert_eq!(result, Type::Con(TypeCtor::List));
    }

    #[test]
    fn test_type_predicate_matches_pattern_negate() {
        let pattern = Pattern::MatchSequence(vec![]);
        let predicate = TypePredicate::MatchesPattern(pattern.clone());
        let negated = predicate.negate();
        assert!(matches!(negated, TypePredicate::Not(_)));
    }

    #[test]
    fn test_type_predicate_matches_pattern_has_no_simple_negation() {
        let pattern = Pattern::MatchSequence(vec![]);
        let predicate = TypePredicate::MatchesPattern(pattern);
        assert!(!predicate.has_simple_negation());
    }

    #[test]
    fn test_control_flow_context_narrowing_scope() {
        let mut ctx = ControlFlowContext::new();
        ctx.push_scope(Some(TypePredicate::IsNotNone));
        ctx.narrow("x".to_string(), Type::int());

        assert_eq!(ctx.get_narrowed_type("x"), Some(&Type::int()));

        assert!(ctx.pop_scope());
        assert_eq!(ctx.get_narrowed_type("x"), None);
    }

    #[test]
    fn test_control_flow_context_nested_scopes() {
        let mut ctx = ControlFlowContext::new();

        ctx.push_scope(None);
        ctx.narrow("x".to_string(), Type::int());

        ctx.push_scope(None);
        ctx.narrow("x".to_string(), Type::string());

        assert_eq!(ctx.get_narrowed_type("x"), Some(&Type::string()));

        ctx.pop_scope();
        assert_eq!(ctx.get_narrowed_type("x"), Some(&Type::int()));

        ctx.pop_scope();
        assert_eq!(ctx.get_narrowed_type("x"), None);
    }

    #[test]
    fn test_control_flow_context_cannot_pop_root_scope() {
        let mut ctx = ControlFlowContext::new();
        assert!(!ctx.pop_scope());

        ctx.narrow("x".to_string(), Type::int());
        assert_eq!(ctx.get_narrowed_type("x"), Some(&Type::int()));
    }

    #[test]
    fn test_type_set_tracker_new() {
        let union_type = Type::union(vec![Type::int(), Type::string(), Type::none()]);
        let tracker = TypeSetTracker::new("x".to_string(), union_type.clone());
        assert_eq!(tracker.variable(), "x");
        assert_eq!(tracker.original_type(), &union_type);
        assert_eq!(tracker.eliminated_types().len(), 0);
    }

    #[test]
    fn test_type_set_tracker_eliminate_type() {
        let union_type = Type::union(vec![Type::int(), Type::string(), Type::none()]);
        let mut tracker = TypeSetTracker::new("x".to_string(), union_type);

        tracker.eliminate_type(Type::none());
        assert_eq!(tracker.eliminated_types(), &[Type::none()]);

        tracker.eliminate_type(Type::int());
        assert_eq!(tracker.eliminated_types().len(), 2);
        assert!(tracker.eliminated_types().contains(&Type::none()));
        assert!(tracker.eliminated_types().contains(&Type::int()));
    }

    #[test]
    fn test_type_set_tracker_eliminate_type_deduplication() {
        let union_type = Type::union(vec![Type::int(), Type::string()]);
        let mut tracker = TypeSetTracker::new("x".to_string(), union_type);

        tracker.eliminate_type(Type::int());
        tracker.eliminate_type(Type::int());
        tracker.eliminate_type(Type::int());

        assert_eq!(tracker.eliminated_types(), &[Type::int()]);
    }

    #[test]
    fn test_type_set_tracker_remaining_types_union() {
        let union_type = Type::union(vec![Type::int(), Type::string(), Type::none()]);
        let mut tracker = TypeSetTracker::new("x".to_string(), union_type);
        let remaining = tracker.remaining_types();
        assert_eq!(remaining, Type::union(vec![Type::int(), Type::string(), Type::none()]));

        tracker.eliminate_type(Type::none());
        let remaining = tracker.remaining_types();
        assert_eq!(remaining, Type::union(vec![Type::int(), Type::string()]));

        tracker.eliminate_type(Type::int());
        let remaining = tracker.remaining_types();
        assert_eq!(remaining, Type::string());

        tracker.eliminate_type(Type::string());
        let remaining = tracker.remaining_types();
        assert_eq!(remaining, Type::never());
    }

    #[test]
    fn test_type_set_tracker_remaining_types_non_union() {
        let int_type = Type::int();
        let mut tracker = TypeSetTracker::new("x".to_string(), int_type.clone());

        assert_eq!(tracker.remaining_types(), int_type);

        tracker.eliminate_type(int_type.clone());
        assert_eq!(tracker.remaining_types(), Type::never());
    }

    #[test]
    fn test_type_set_tracker_is_exhaustive_union() {
        let union_type = Type::union(vec![Type::int(), Type::string(), Type::none()]);
        let mut tracker = TypeSetTracker::new("x".to_string(), union_type);

        assert!(!tracker.is_exhaustive());

        tracker.eliminate_type(Type::none());
        assert!(!tracker.is_exhaustive());

        tracker.eliminate_type(Type::int());
        assert!(!tracker.is_exhaustive());

        tracker.eliminate_type(Type::string());
        assert!(tracker.is_exhaustive());
    }

    #[test]
    fn test_type_set_tracker_is_exhaustive_non_union() {
        let int_type = Type::int();
        let mut tracker = TypeSetTracker::new("x".to_string(), int_type.clone());

        assert!(!tracker.is_exhaustive());

        tracker.eliminate_type(int_type);
        assert!(tracker.is_exhaustive());
    }

    #[test]
    fn test_control_flow_start_and_stop_tracking() {
        let mut ctx = ControlFlowContext::new();
        let union_type = Type::union(vec![Type::int(), Type::string()]);

        ctx.start_tracking("x".to_string(), union_type.clone());
        assert!(ctx.get_tracker("x").is_some());

        ctx.stop_tracking("x");
        assert!(ctx.get_tracker("x").is_none());
    }

    #[test]
    fn test_control_flow_eliminate_type() {
        let mut ctx = ControlFlowContext::new();
        let union_type = Type::union(vec![Type::int(), Type::string(), Type::none()]);

        ctx.start_tracking("x".to_string(), union_type);
        ctx.eliminate_type("x", Type::none());

        let tracker = ctx.get_tracker("x").unwrap();
        assert_eq!(tracker.eliminated_types(), &[Type::none()]);
    }

    #[test]
    fn test_control_flow_get_remaining_types() {
        let mut ctx = ControlFlowContext::new();
        let union_type = Type::union(vec![Type::int(), Type::string(), Type::none()]);

        ctx.start_tracking("x".to_string(), union_type);
        ctx.eliminate_type("x", Type::none());

        let remaining = ctx.get_remaining_types("x").unwrap();
        assert_eq!(remaining, Type::union(vec![Type::int(), Type::string()]));
    }

    #[test]
    fn test_control_flow_is_exhaustive() {
        let mut ctx = ControlFlowContext::new();
        let union_type = Type::union(vec![Type::int(), Type::string()]);

        ctx.start_tracking("x".to_string(), union_type);

        assert!(!ctx.is_exhaustive("x"));

        ctx.eliminate_type("x", Type::int());
        assert!(!ctx.is_exhaustive("x"));

        ctx.eliminate_type("x", Type::string());
        assert!(ctx.is_exhaustive("x"));
    }

    #[test]
    fn test_control_flow_exhaustiveness_successive_eliminations() {
        let mut ctx = ControlFlowContext::new();
        let union_type = Type::union(vec![Type::int(), Type::string(), Type::none()]);

        ctx.start_tracking("x".to_string(), union_type);

        ctx.eliminate_type("x", Type::none());
        let remaining = ctx.get_remaining_types("x").unwrap();
        assert_eq!(remaining, Type::union(vec![Type::int(), Type::string()]));

        ctx.eliminate_type("x", Type::int());
        let remaining = ctx.get_remaining_types("x").unwrap();
        assert_eq!(remaining, Type::string());

        assert!(!ctx.is_exhaustive("x"));

        ctx.eliminate_type("x", Type::string());
        assert!(ctx.is_exhaustive("x"));
        assert_eq!(ctx.get_remaining_types("x").unwrap(), Type::never());
    }

    #[test]
    fn test_predicate_eliminated_types_is_not_none() {
        let union_type = Type::union(vec![Type::int(), Type::none()]);
        let predicate = TypePredicate::IsNotNone;
        let eliminated = predicate.eliminated_types(&union_type);
        assert_eq!(eliminated, vec![Type::none()]);
    }

    #[test]
    fn test_predicate_eliminated_types_is_none() {
        let union_type = Type::union(vec![Type::int(), Type::string(), Type::none()]);
        let predicate = TypePredicate::IsNone;
        let eliminated = predicate.eliminated_types(&union_type);
        assert_eq!(eliminated.len(), 2);
        assert!(eliminated.contains(&Type::int()));
        assert!(eliminated.contains(&Type::string()));
    }

    #[test]
    fn test_predicate_eliminated_types_isinstance() {
        let union_type = Type::union(vec![Type::int(), Type::string(), Type::none()]);
        let predicate = TypePredicate::IsInstance(Type::int());
        let eliminated = predicate.eliminated_types(&union_type);
        assert_eq!(eliminated.len(), 2);
        assert!(eliminated.contains(&Type::string()));
        assert!(eliminated.contains(&Type::none()));
    }

    #[test]
    fn test_predicate_eliminated_types_isinstance_union_target() {
        let union_type = Type::union(vec![Type::int(), Type::string(), Type::none()]);
        let target_union = Type::union(vec![Type::int(), Type::string()]);
        let predicate = TypePredicate::IsInstance(target_union);
        let eliminated = predicate.eliminated_types(&union_type);
        assert_eq!(eliminated, vec![Type::none()]);
    }

    #[test]
    fn test_predicate_eliminated_types_is_truthy() {
        let union_type = Type::union(vec![Type::int(), Type::none()]);
        let predicate = TypePredicate::IsTruthy;
        let eliminated = predicate.eliminated_types(&union_type);
        assert_eq!(eliminated, vec![Type::none()]);
    }

    #[test]
    fn test_predicate_eliminated_types_and() {
        let union_type = Type::union(vec![Type::int(), Type::none()]);
        let pred1 = TypePredicate::IsNotNone;
        let pred2 = TypePredicate::IsTruthy;
        let predicate = TypePredicate::And(Box::new(pred1), Box::new(pred2));
        let eliminated = predicate.eliminated_types(&union_type);
        assert_eq!(eliminated.len(), 1);
        assert!(eliminated.contains(&Type::none()));
    }

    #[test]
    fn test_predicate_eliminated_types_or() {
        let union_type = Type::union(vec![Type::int(), Type::string(), Type::none()]);
        let pred1 = TypePredicate::IsInstance(Type::int());
        let pred2 = TypePredicate::IsInstance(Type::string());
        let predicate = TypePredicate::Or(Box::new(pred1), Box::new(pred2));
        let eliminated = predicate.eliminated_types(&union_type);
        assert_eq!(eliminated, vec![Type::none()]);
    }
}

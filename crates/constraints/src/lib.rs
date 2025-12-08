pub mod exhaustiveness;
pub mod pattern_validation;
pub mod solver;

mod pattern;
mod predicate;

pub use predicate::TypePredicate;

use beacon_core::{ClassRegistry, Type, TypeCtor, TypeError};
use beacon_parser::ScopeId;
use rustc_hash::{FxHashMap, FxHashSet};

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
///
/// Python-specific semantics:
/// - bool is a subtype of int, so int() pattern matches bool values
/// - However, bool() pattern only matches bool, not all ints
pub fn type_compatible_with_class(ty: &Type, cls: &str) -> bool {
    match (cls, ty) {
        ("int", Type::Con(TypeCtor::Int))
        | ("int", Type::Con(TypeCtor::Bool))
        | ("str", Type::Con(TypeCtor::String))
        | ("bool", Type::Con(TypeCtor::Bool))
        | ("float", Type::Con(TypeCtor::Float)) => true,
        (pattern_class, Type::Con(TypeCtor::Class(subject_class))) => pattern_class == subject_class,
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
    /// Call constraint: f(positional_args, keyword_args) -> ret
    /// Stores function type, positional arguments (type, span), keyword arguments (name, type, span), return type, and call span
    Call(Type, Vec<(Type, Span)>, Vec<(String, Type, Span)>, Type, Span),
    /// Protocol constraint: τ satisfies protocol P
    /// The type parameter is the element type extracted from satisfying the protocol
    Protocol(Type, beacon_core::ProtocolName, Type, Span),
    /// Pattern matching constraint: subject type must be compatible with pattern
    /// Stores bindings extracted from the pattern (variable name -> type)
    MatchPattern(Type, beacon_parser::Pattern, Vec<(String, Type)>, Span),
    /// Exhaustiveness constraint: patterns must cover all possible values of subject type
    /// Stores subject type and all case patterns for checking
    /// Each pattern is paired with a boolean indicating whether it has a guard.
    /// Patterns with guards do not contribute to exhaustiveness coverage.
    PatternExhaustive(Type, Vec<(beacon_parser::Pattern, bool)>, Span),
    /// Reachability constraint: pattern must be reachable (not subsumed by previous patterns)
    /// Stores the pattern and all previous patterns in the match statement
    PatternReachable(beacon_parser::Pattern, Vec<beacon_parser::Pattern>, Span),
    /// Type compatibility constraint: pattern type must be compatible with subject type
    /// Validates that the pattern can potentially match the subject (e.g., no int pattern on str subject)
    PatternTypeCompatible(beacon_parser::Pattern, Type, Span),
    /// Structure validity constraint: pattern structure must match subject type structure
    /// Validates arity for tuples, required keys for mappings, etc.
    PatternStructureValid(beacon_parser::Pattern, Type, Span),
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

#[derive(Debug, Clone)]
pub struct ConstraintResult(
    pub ConstraintSet,
    pub FxHashMap<usize, Type>,
    pub FxHashMap<(usize, usize), usize>,
    pub FxHashMap<usize, Span>,
    pub FxHashSet<usize>,
    pub ClassRegistry,
    pub FxHashMap<usize, ScopeId>,
    pub FxHashMap<ScopeId, FxHashSet<ScopeId>>,
    pub beacon_core::TypeVarConstraintRegistry,
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

    /// Apply recorded eliminations to the provided type, removing variants that were ruled out.
    pub fn apply_remaining_types(&self, variable: &str, current_type: &Type) -> Option<Type> {
        self.type_trackers.get(variable).and_then(|tracker| {
            let refined = tracker
                .eliminated_types()
                .iter()
                .fold(current_type.clone(), |acc, eliminated| {
                    acc.remove_from_union(eliminated)
                });

            if matches!(refined, Type::Con(TypeCtor::Never)) || refined == *current_type {
                None
            } else {
                Some(refined)
            }
        })
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
pub struct ConstraintGenContext<'a> {
    pub constraints: Vec<Constraint>,
    pub node_counter: usize,
    pub type_map: FxHashMap<usize, Type>,
    pub position_map: FxHashMap<(usize, usize), usize>,
    pub node_spans: FxHashMap<usize, Span>,
    pub safe_any_nodes: FxHashSet<usize>,
    pub class_registry: ClassRegistry,
    /// Control flow context for flow-sensitive type narrowing
    pub control_flow: ControlFlowContext,
    /// Tracks which stub modules have been loaded into the class registry
    pub loaded_stub_modules: FxHashSet<String>,
    /// Stack of active scopes during AST traversal (innermost scope is last)
    pub scope_stack: Vec<ScopeId>,
    /// Maps each node (by node_id) to its containing scope
    pub node_to_scope: FxHashMap<usize, ScopeId>,
    /// Tracks dependencies between scopes (scope_id -> set of scopes it depends on)
    pub scope_dependencies: FxHashMap<ScopeId, FxHashSet<ScopeId>>,
    /// TypeVar constraint and bound registry for tracking TypeVar metadata
    pub typevar_registry: beacon_core::TypeVarConstraintRegistry,
    /// Reference to the symbol table for scope lookups
    symbol_table: Option<&'a beacon_parser::SymbolTable>,
    /// Source code for calculating byte offsets from line/col positions
    pub source: Option<&'a str>,
}

impl<'a> ConstraintGenContext<'a> {
    pub fn new() -> Self {
        Self::default()
    }

    /// Set the symbol table and source code for scope tracking
    pub fn set_context(&mut self, symbol_table: &'a beacon_parser::SymbolTable, source: &'a str) {
        self.symbol_table = Some(symbol_table);
        self.source = Some(source);
    }

    /// Calculate byte offset from line and column numbers (1-indexed)
    fn line_col_to_byte_offset(&self, line: usize, col: usize) -> Option<usize> {
        let source = self.source?;
        let mut current_line = 1;

        for (idx, ch) in source.char_indices() {
            if current_line == line {
                let line_start = source[..idx].rfind('\n').map(|pos| pos + 1).unwrap_or(0);
                let chars_in_line = source[line_start..idx].chars().count();
                if chars_in_line + 1 == col {
                    return Some(idx);
                }
            }

            if ch == '\n' {
                current_line += 1;
                if current_line > line {
                    break;
                }
            }
        }

        if current_line == line {
            let line_start = source[..].rfind('\n').map(|pos| pos + 1).unwrap_or(0);
            let chars_in_line = source[line_start..].chars().count();
            if chars_in_line + 1 >= col {
                return Some(source.len());
            }
        }

        None
    }

    /// Find the scope at a given line and column position
    pub fn find_scope_at_position(&self, line: usize, col: usize) -> Option<ScopeId> {
        let symbol_table = self.symbol_table?;
        let byte_offset = self.line_col_to_byte_offset(line, col)?;
        Some(symbol_table.find_scope_at_position(byte_offset))
    }

    /// Get a reference to the symbol table
    pub fn symbol_table(&self) -> Option<&beacon_parser::SymbolTable> {
        self.symbol_table
    }

    /// Record a type for a node at a specific position, returning the node ID
    pub fn record_type(&mut self, line: usize, col: usize, ty: Type) -> usize {
        self.record_type_with_end(line, col, line, col + 1, ty)
    }

    /// Record a type for a node with explicit span information
    pub fn record_type_with_end(
        &mut self, line: usize, col: usize, end_line: usize, end_col: usize, ty: Type,
    ) -> usize {
        let span = Self::normalized_span(line, col, end_line, end_col);
        self.record_type_with_span(span, ty)
    }

    /// Record a type for a node given a span
    pub fn record_type_with_span(&mut self, span: Span, ty: Type) -> usize {
        let node_id = self.node_counter;
        self.node_counter += 1;
        self.type_map.insert(node_id, ty);
        self.position_map.insert((span.line, span.col), node_id);
        self.node_spans.insert(node_id, span);

        if let Some(&scope_id) = self.scope_stack.last() {
            self.node_to_scope.insert(node_id, scope_id);
        }

        node_id
    }

    /// Override the recorded span for a node without altering its primary position
    pub fn override_span(&mut self, node_id: usize, span: Span) {
        self.node_spans.insert(node_id, span);
    }

    /// Mark a node to suppress unsafe Any diagnostics
    pub fn mark_safe_any_node(&mut self, node_id: usize) {
        self.safe_any_nodes.insert(node_id);
    }

    fn line_text(&self, line: usize) -> Option<&str> {
        let source = self.source?;
        let idx = line.checked_sub(1)?;
        source.lines().nth(idx)
    }

    fn column_to_byte(line_text: &str, column: usize) -> usize {
        if column <= 1 {
            return 0;
        }
        let mut chars_seen = 1;
        for (byte_idx, _) in line_text.char_indices() {
            if chars_seen == column {
                return byte_idx;
            }
            chars_seen += 1;
        }
        line_text.len()
    }

    fn byte_to_column(line_text: &str, byte_idx: usize) -> usize {
        line_text[..byte_idx].chars().count() + 1
    }

    /// Calculate a span that tightly wraps the identifier for definitions
    pub fn span_for_identifier(&self, line: usize, column_hint: usize, name: &str) -> Span {
        let name_width = name.chars().count().max(1);
        let fallback_end = column_hint + name_width;

        if let Some(line_text) = self.line_text(line) {
            let search_start = Self::column_to_byte(line_text, column_hint);
            if search_start <= line_text.len()
                && let Some(rel_idx) = line_text[search_start..].find(name)
            {
                let byte_idx = search_start + rel_idx;
                let start_col = Self::byte_to_column(line_text, byte_idx);
                let end_col = start_col + name_width;
                return Span::with_end(line, start_col, line, end_col);
            }
        }

        Span::with_end(line, column_hint, line, fallback_end)
    }

    fn normalized_span(line: usize, col: usize, end_line: usize, end_col: usize) -> Span {
        let normalized_end_line = end_line.max(line);
        let mut normalized_end_col = end_col;
        if normalized_end_line == line && normalized_end_col <= col {
            normalized_end_col = col + 1;
        }
        Span::with_end(line, col, normalized_end_line, normalized_end_col)
    }

    /// Push a scope onto the scope stack when entering a new scope (module, function, class, block)
    pub fn push_scope(&mut self, scope_id: ScopeId) {
        self.scope_stack.push(scope_id);
    }

    /// Pop a scope from the scope stack when exiting a scope
    pub fn pop_scope(&mut self) -> Option<ScopeId> {
        self.scope_stack.pop()
    }

    /// Track a dependency between scopes
    /// Records that `from_scope` depends on `to_scope` (e.g., because it references a symbol defined in `to_scope`)
    pub fn add_scope_dependency(&mut self, from_scope: ScopeId, to_scope: ScopeId) {
        self.scope_dependencies.entry(from_scope).or_default().insert(to_scope);
    }
}

impl<'a> Default for ConstraintGenContext<'a> {
    fn default() -> Self {
        Self {
            constraints: Vec::new(),
            node_counter: 0,
            type_map: FxHashMap::default(),
            position_map: FxHashMap::default(),
            node_spans: FxHashMap::default(),
            safe_any_nodes: FxHashSet::default(),
            class_registry: ClassRegistry::new(),
            control_flow: ControlFlowContext::new(),
            loaded_stub_modules: FxHashSet::default(),
            scope_stack: Vec::new(),
            node_to_scope: FxHashMap::default(),
            scope_dependencies: FxHashMap::default(),
            typevar_registry: beacon_core::TypeVarConstraintRegistry::new(),
            symbol_table: None,
            source: None,
        }
    }
}

/// Set of type constraints
///
/// TODO: Replace with [beacon_core] constraint types
#[derive(Debug, Clone)]
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
    use beacon_parser::{Pattern, ScopeKind};

    #[test]
    fn test_scope_stack_push_pop() {
        let mut ctx = ConstraintGenContext::new();
        let scope_id1 = ScopeId::from_raw(1);
        let scope_id2 = ScopeId::from_raw(2);
        assert!(ctx.scope_stack.is_empty());

        ctx.push_scope(scope_id1);
        assert_eq!(ctx.scope_stack.len(), 1);
        assert_eq!(ctx.scope_stack.last(), Some(&scope_id1));

        ctx.push_scope(scope_id2);
        assert_eq!(ctx.scope_stack.len(), 2);
        assert_eq!(ctx.scope_stack.last(), Some(&scope_id2));

        assert_eq!(ctx.pop_scope(), Some(scope_id2));
        assert_eq!(ctx.scope_stack.len(), 1);
        assert_eq!(ctx.scope_stack.last(), Some(&scope_id1));

        assert_eq!(ctx.pop_scope(), Some(scope_id1));
        assert!(ctx.scope_stack.is_empty());

        assert_eq!(ctx.pop_scope(), None);
    }

    #[test]
    fn test_node_to_scope_mapping() {
        let mut ctx = ConstraintGenContext::new();
        let scope_id = ScopeId::from_raw(1);

        let node_id1 = ctx.record_type(1, 1, Type::int());
        assert!(!ctx.node_to_scope.contains_key(&node_id1));

        ctx.push_scope(scope_id);
        let node_id2 = ctx.record_type(2, 2, Type::string());
        assert_eq!(ctx.node_to_scope.get(&node_id2), Some(&scope_id));

        let node_id3 = ctx.record_type(3, 3, Type::bool());
        assert_eq!(ctx.node_to_scope.get(&node_id3), Some(&scope_id));

        ctx.pop_scope();
        let node_id4 = ctx.record_type(4, 4, Type::float());
        assert!(!ctx.node_to_scope.contains_key(&node_id4));
    }

    #[test]
    fn test_scope_dependencies() {
        let mut ctx = ConstraintGenContext::new();
        let scope1 = ScopeId::from_raw(1);
        let scope2 = ScopeId::from_raw(2);
        let scope3 = ScopeId::from_raw(3);

        assert!(ctx.scope_dependencies.is_empty());

        ctx.add_scope_dependency(scope1, scope2);
        assert_eq!(ctx.scope_dependencies.get(&scope1).map(|s| s.len()), Some(1));
        assert!(ctx.scope_dependencies.get(&scope1).unwrap().contains(&scope2));

        ctx.add_scope_dependency(scope1, scope3);
        assert_eq!(ctx.scope_dependencies.get(&scope1).map(|s| s.len()), Some(2));
        assert!(ctx.scope_dependencies.get(&scope1).unwrap().contains(&scope2));
        assert!(ctx.scope_dependencies.get(&scope1).unwrap().contains(&scope3));

        ctx.add_scope_dependency(scope1, scope2);
        assert_eq!(ctx.scope_dependencies.get(&scope1).map(|s| s.len()), Some(2));

        assert!(!ctx.scope_dependencies.contains_key(&scope2));
    }

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

    #[test]
    fn test_line_col_to_byte_offset() {
        let source = "def foo():\n    return 42\n";
        let mut ctx = ConstraintGenContext::new();
        let symbol_table = beacon_parser::SymbolTable::new();
        ctx.set_context(&symbol_table, source);

        assert_eq!(ctx.line_col_to_byte_offset(1, 1), Some(0));

        assert_eq!(ctx.line_col_to_byte_offset(1, 4), Some(3));

        assert_eq!(ctx.line_col_to_byte_offset(2, 1), Some(11));

        assert_eq!(ctx.line_col_to_byte_offset(2, 5), Some(15));
    }

    #[test]
    fn test_find_scope_at_position() {
        let source = "x = 1\ndef foo():\n    return 42\n";
        let mut symbol_table = beacon_parser::SymbolTable::new();

        let func_scope = symbol_table.create_scope(ScopeKind::Function, symbol_table.root_scope, 17, source.len());

        let mut ctx = ConstraintGenContext::new();
        ctx.set_context(&symbol_table, source);

        let scope = ctx.find_scope_at_position(3, 5);
        assert_eq!(scope, Some(func_scope));

        let scope = ctx.find_scope_at_position(1, 1);
        assert_eq!(scope, Some(symbol_table.root_scope));
    }

    #[test]
    fn test_set_context_stores_references() {
        let source = "x = 1\n";
        let symbol_table = beacon_parser::SymbolTable::new();
        let mut ctx = ConstraintGenContext::new();

        assert!(ctx.symbol_table().is_none());
        assert!(ctx.source.is_none());

        ctx.set_context(&symbol_table, source);

        assert!(ctx.symbol_table().is_some());
        assert!(ctx.source.is_some());
    }
}

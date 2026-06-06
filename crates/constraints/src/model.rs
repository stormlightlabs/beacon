use crate::TypePredicate;

use beacon_core::{ClassRegistry, Type, TypeError};
use beacon_parser::ScopeId;
use rustc_hash::{FxHashMap, FxHashSet};

/// Type guard kind for user-defined type guard functions.
#[derive(Debug, Clone, PartialEq)]
pub enum TypeGuardKind {
    /// TypeGuard[T] narrows while maintaining the original type relationship.
    TypeGuard,
    /// TypeIs[T] asserts an exact type (PEP 742).
    TypeIs,
}

/// Metadata for functions with `TypeGuard[T]` or `TypeIs[T]` return annotations.
#[derive(Debug, Clone)]
pub struct TypeGuardInfo {
    /// Which parameter is guarded.
    pub param_index: usize,
    /// Name of the guarded parameter, when available from the definition.
    pub param_name: Option<String>,
    /// The type being guarded to.
    pub guarded_type: Type,
    /// Which guard semantics apply.
    pub kind: TypeGuardKind,
}

impl TypeGuardInfo {
    pub fn new(param_index: usize, guarded_type: Type, kind: TypeGuardKind) -> Self {
        Self { param_index, param_name: None, guarded_type, kind }
    }

    pub fn with_param_name(mut self, param_name: impl Into<String>) -> Self {
        self.param_name = Some(param_name.into());
        self
    }
}

/// Type error with source location information.
#[derive(Debug, Clone)]
pub struct TypeErrorInfo {
    pub error: TypeError,
    pub span: Span,
}

impl TypeErrorInfo {
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

/// Type constraint with source position tracking.
#[derive(Debug, Clone)]
pub enum Constraint {
    /// Type equality constraint: t1 ~ t2.
    Equal(Type, Type, Span),
    /// Attribute constraint: type has attribute `name` with the provided type.
    HasAttr(Type, String, Type, Span),
    /// Call constraint: function, positional args, keyword args, return type, call span.
    Call(Type, Vec<(Type, Span)>, Vec<(String, Type, Span)>, Type, Span),
    /// Protocol constraint: type satisfies protocol P and exposes an element type.
    Protocol(Type, beacon_core::ProtocolName, Type, Span),
    /// Pattern matching constraint with extracted bindings.
    MatchPattern(Type, beacon_parser::Pattern, Vec<(String, Type)>, Span),
    /// Exhaustiveness constraint. Guarded patterns do not contribute to coverage.
    PatternExhaustive(Type, Vec<(beacon_parser::Pattern, bool)>, Span),
    /// Reachability constraint for a pattern and all previous patterns.
    PatternReachable(beacon_parser::Pattern, Vec<beacon_parser::Pattern>, Span),
    /// Pattern type compatibility constraint.
    PatternTypeCompatible(beacon_parser::Pattern, Type, Span),
    /// Pattern structure validity constraint.
    PatternStructureValid(beacon_parser::Pattern, Type, Span),
    /// Flow-sensitive type narrowing.
    ///
    /// This is consumed by the solver as metadata for validation/tracing; it does
    /// not currently rewrite solver output. The analyzer owns the concrete
    /// environment updates that make narrowed types visible to later AST nodes.
    Narrowing(String, TypePredicate, Type, Span),
    /// Join point constraint for merging control-flow paths.
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

/// Set of solver-local constraints.
#[derive(Debug, Clone)]
pub struct ConstraintSet {
    pub constraints: Vec<Constraint>,
}

/// Source code span for tracking positions.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    /// Line number (1-indexed).
    pub line: usize,
    /// Column number (1-indexed).
    pub col: usize,
    /// Optional end line for range diagnostics.
    pub end_line: Option<usize>,
    /// Optional end column for range diagnostics.
    pub end_col: Option<usize>,
}

impl Span {
    pub fn new(line: usize, col: usize) -> Self {
        Self { line, col, end_line: None, end_col: None }
    }

    pub fn with_end(line: usize, col: usize, end_line: usize, end_col: usize) -> Self {
        Self { line, col, end_line: Some(end_line), end_col: Some(end_col) }
    }
}

pub mod exhaustiveness;
pub mod solver;

use beacon_core::{ClassRegistry, Type, TypeCtor, TypeError};
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

    /// Variable is truthy (excludes None, False, 0, "", [], {}, etc.): `if x:`
    IsTruthy,

    /// Variable is falsy: `if not x:`
    IsFalsy,

    /// Conjunction of predicates (for `and` expressions): `if x and y:`
    And(Box<TypePredicate>, Box<TypePredicate>),

    /// Disjunction of predicates (for `or` expressions): `if x or y:`
    Or(Box<TypePredicate>, Box<TypePredicate>),

    /// Negation of a predicate (for `not` expressions): `if not x:`
    Not(Box<TypePredicate>),

    /// Variable matches a pattern: `case Pattern:`
    MatchesPattern(beacon_parser::Pattern),
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
            TypePredicate::IsTruthy => ty.remove_from_union(&Type::none()),
            TypePredicate::IsFalsy => match ty {
                Type::Union(variants) => {
                    let falsy: Vec<Type> = variants.iter().filter(|t| Self::is_falsy_type(t)).cloned().collect();

                    if falsy.is_empty() {
                        Type::none()
                    } else if falsy.len() == 1 {
                        falsy.into_iter().next().unwrap()
                    } else {
                        Type::union(falsy)
                    }
                }
                t if Self::is_falsy_type(t) => t.clone(),
                _ => Type::none(),
            },
            TypePredicate::And(p1, p2) => {
                let t1 = p1.apply(ty);
                p2.apply(&t1)
            }
            TypePredicate::Or(p1, p2) => Type::union(vec![p1.apply(ty), p2.apply(ty)]),
            TypePredicate::Not(p) => p.negate().apply(ty),
            TypePredicate::MatchesPattern(pattern) => narrow_type_by_pattern(ty, pattern),
        }
    }

    /// Check if a type is always falsy
    fn is_falsy_type(ty: &Type) -> bool {
        matches!(ty, Type::Con(TypeCtor::NoneType))
    }

    /// Get the inverse predicate for narrowing in the else branch of an if statement.
    ///
    /// De Morgan's law
    /// - not (A and B) = (not A) or (not B)
    /// - not (A or B) = (not A) and (not B)
    pub fn negate(&self) -> TypePredicate {
        match self {
            TypePredicate::IsNotNone => TypePredicate::IsNone,
            TypePredicate::IsNone => TypePredicate::IsNotNone,
            TypePredicate::IsInstance(_) => TypePredicate::IsNotNone,
            TypePredicate::IsTruthy => TypePredicate::IsFalsy,
            TypePredicate::IsFalsy => TypePredicate::IsTruthy,
            TypePredicate::And(p1, p2) => TypePredicate::Or(Box::new(p1.negate()), Box::new(p2.negate())),
            TypePredicate::Or(p1, p2) => TypePredicate::And(Box::new(p1.negate()), Box::new(p2.negate())),
            TypePredicate::Not(p) => p.as_ref().clone(),
            // TODO: full pattern analysis
            TypePredicate::MatchesPattern(_) => TypePredicate::Not(Box::new(self.clone())),
        }
    }

    /// Check if this predicate has a meaningful negation for constraint generation
    ///
    /// Some predicates like isinstance don't have simple inverse predicates,
    /// and their negation is handled by type subtraction in detect_inverse_type_guard.
    pub fn has_simple_negation(&self) -> bool {
        !matches!(self, TypePredicate::IsInstance(_) | TypePredicate::MatchesPattern(_))
    }
}

/// Narrow a type based on a pattern match
///
/// For example, matching `case int(x):` narrows a `int | str` union to just `int`.
fn narrow_type_by_pattern(ty: &Type, pattern: &beacon_parser::Pattern) -> Type {
    use beacon_parser::Pattern;

    match pattern {
        Pattern::MatchValue(_) => ty.clone(),
        Pattern::MatchSequence(_) => match ty {
            Type::Union(variants) => {
                let sequence_types: Vec<Type> = variants.iter().filter(|t| is_sequence_type(t)).cloned().collect();

                if sequence_types.is_empty() {
                    ty.clone()
                } else if sequence_types.len() == 1 {
                    sequence_types.into_iter().next().unwrap()
                } else {
                    Type::union(sequence_types)
                }
            }
            t if is_sequence_type(t) => t.clone(),
            _ => ty.clone(),
        },
        Pattern::MatchMapping { .. } => match ty {
            Type::Union(variants) => {
                let dict_types: Vec<Type> = variants.iter().filter(|t| is_dict_type(t)).cloned().collect();

                if dict_types.is_empty() {
                    ty.clone()
                } else if dict_types.len() == 1 {
                    dict_types.into_iter().next().unwrap()
                } else {
                    Type::union(dict_types)
                }
            }
            t if is_dict_type(t) => t.clone(),
            _ => ty.clone(),
        },
        Pattern::MatchClass { cls, .. } => {
            let target_type = Type::Con(TypeCtor::Class(cls.clone()));

            match ty {
                Type::Union(variants) => {
                    if variants.contains(&target_type) {
                        target_type
                    } else {
                        let compatible: Vec<Type> = variants
                            .iter()
                            .filter(|v| type_compatible_with_class(v, cls))
                            .cloned()
                            .collect();

                        if compatible.is_empty() {
                            target_type
                        } else if compatible.len() == 1 {
                            compatible.into_iter().next().unwrap()
                        } else {
                            Type::union(compatible)
                        }
                    }
                }
                _ => target_type,
            }
        }
        Pattern::MatchAs { pattern: Some(sub_pattern), .. } => narrow_type_by_pattern(ty, sub_pattern),
        Pattern::MatchAs { pattern: None, .. } => ty.clone(),
        Pattern::MatchOr(alternatives) => {
            let narrowed_types: Vec<Type> = alternatives.iter().map(|alt| narrow_type_by_pattern(ty, alt)).collect();

            if narrowed_types.is_empty() {
                ty.clone()
            } else if narrowed_types.len() == 1 {
                narrowed_types.into_iter().next().unwrap()
            } else {
                Type::union(narrowed_types)
            }
        }
    }
}

/// Check if a type is a sequence type (list, tuple)
fn is_sequence_type(ty: &Type) -> bool {
    match ty {
        Type::App(ctor, _) => matches!(ctor.as_ref(), Type::Con(TypeCtor::List) | Type::Con(TypeCtor::Tuple)),
        Type::Con(TypeCtor::List | TypeCtor::Tuple) => true,
        _ => false,
    }
}

/// Check if a type is a dict type
fn is_dict_type(ty: &Type) -> bool {
    match ty {
        Type::App(ctor, _) => matches!(ctor.as_ref(), Type::Con(TypeCtor::Dict)),
        Type::Con(TypeCtor::Dict) => true,
        _ => false,
    }
}

/// Check if a type is compatible with a class pattern
fn type_compatible_with_class(ty: &Type, cls: &str) -> bool {
    match ty {
        Type::Con(TypeCtor::Class(name)) => name == cls,
        _ => false,
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

#[cfg(test)]
mod tests {
    use super::*;
    use beacon_parser::{AstNode, LiteralValue, Pattern};

    #[test]
    fn test_is_sequence_type() {
        let list_type = Type::Con(beacon_core::TypeCtor::List);
        assert!(is_sequence_type(&list_type));

        let tuple_type = Type::Con(beacon_core::TypeCtor::Tuple);
        assert!(is_sequence_type(&tuple_type));

        let list_int = Type::App(Box::new(Type::Con(beacon_core::TypeCtor::List)), Box::new(Type::int()));
        assert!(is_sequence_type(&list_int));

        let int_type = Type::int();
        assert!(!is_sequence_type(&int_type));

        let dict_type = Type::Con(beacon_core::TypeCtor::Dict);
        assert!(!is_sequence_type(&dict_type));
    }

    #[test]
    fn test_is_dict_type() {
        let dict_type = Type::Con(beacon_core::TypeCtor::Dict);
        assert!(is_dict_type(&dict_type));

        let dict_type_app = Type::App(
            Box::new(Type::Con(beacon_core::TypeCtor::Dict)),
            Box::new(Type::string()),
        );
        assert!(is_dict_type(&dict_type_app));

        let int_type = Type::int();
        assert!(!is_dict_type(&int_type));

        let list_type = Type::Con(beacon_core::TypeCtor::List);
        assert!(!is_dict_type(&list_type));
    }

    #[test]
    fn test_type_compatible_with_class() {
        let point_type = Type::Con(beacon_core::TypeCtor::Class("Point".to_string()));
        assert!(type_compatible_with_class(&point_type, "Point"));
        assert!(!type_compatible_with_class(&point_type, "Circle"));

        let int_type = Type::int();
        assert!(!type_compatible_with_class(&int_type, "Point"));
    }

    #[test]
    fn test_narrow_type_by_pattern_match_value() {
        let union_type = Type::union(vec![Type::int(), Type::string()]);
        let literal = AstNode::Literal { value: LiteralValue::Integer(42), line: 1, col: 1 };
        let pattern = Pattern::MatchValue(literal);

        let result = narrow_type_by_pattern(&union_type, &pattern);
        assert_eq!(result, union_type);
    }

    #[test]
    fn test_narrow_type_by_pattern_sequence() {
        let union_type = Type::union(vec![Type::Con(beacon_core::TypeCtor::List), Type::string()]);
        let pattern = Pattern::MatchSequence(vec![]);
        let result = narrow_type_by_pattern(&union_type, &pattern);
        assert_eq!(result, Type::Con(beacon_core::TypeCtor::List));
    }

    #[test]
    fn test_narrow_type_by_pattern_mapping() {
        let union_type = Type::union(vec![
            Type::Con(beacon_core::TypeCtor::Dict),
            Type::Con(beacon_core::TypeCtor::List),
        ]);
        let pattern = Pattern::MatchMapping { keys: vec![], patterns: vec![] };
        let result = narrow_type_by_pattern(&union_type, &pattern);
        assert_eq!(result, Type::Con(beacon_core::TypeCtor::Dict));
    }

    #[test]
    fn test_narrow_type_by_pattern_class() {
        let point_type = Type::Con(beacon_core::TypeCtor::Class("Point".to_string()));
        let circle_type = Type::Con(beacon_core::TypeCtor::Class("Circle".to_string()));
        let union_type = Type::union(vec![point_type.clone(), circle_type]);
        let pattern = Pattern::MatchClass { cls: "Point".to_string(), patterns: vec![] };
        let result = narrow_type_by_pattern(&union_type, &pattern);
        assert_eq!(result, point_type);
    }

    #[test]
    fn test_narrow_type_by_pattern_as_with_subpattern() {
        let union_type = Type::union(vec![Type::int(), Type::string()]);
        let sub_pattern = Pattern::MatchClass { cls: "int".to_string(), patterns: vec![] };
        let pattern = Pattern::MatchAs { pattern: Some(Box::new(sub_pattern)), name: Some("x".to_string()) };
        let result = narrow_type_by_pattern(&union_type, &pattern);
        let expected = Type::Con(beacon_core::TypeCtor::Class("int".to_string()));
        assert_eq!(result, expected);
    }

    #[test]
    fn test_narrow_type_by_pattern_as_without_subpattern() {
        let union_type = Type::union(vec![Type::int(), Type::string()]);
        let pattern = Pattern::MatchAs { pattern: None, name: Some("x".to_string()) };
        let result = narrow_type_by_pattern(&union_type, &pattern);
        assert_eq!(result, union_type);
    }

    #[test]
    fn test_narrow_type_by_pattern_or() {
        let int_type = Type::Con(beacon_core::TypeCtor::Class("int".to_string()));
        let float_type = Type::Con(beacon_core::TypeCtor::Class("float".to_string()));
        let union_type = Type::union(vec![int_type.clone(), float_type.clone(), Type::string()]);

        let pattern1 = Pattern::MatchClass { cls: "int".to_string(), patterns: vec![] };
        let pattern2 = Pattern::MatchClass { cls: "float".to_string(), patterns: vec![] };
        let or_pattern = Pattern::MatchOr(vec![pattern1, pattern2]);

        let result = narrow_type_by_pattern(&union_type, &or_pattern);
        let expected = Type::union(vec![int_type, float_type]);
        assert_eq!(result, expected);
    }

    #[test]
    fn test_type_predicate_matches_pattern_apply() {
        let union_type = Type::union(vec![Type::Con(beacon_core::TypeCtor::List), Type::string()]);
        let pattern = Pattern::MatchSequence(vec![]);
        let predicate = TypePredicate::MatchesPattern(pattern);
        let result = predicate.apply(&union_type);
        assert_eq!(result, Type::Con(beacon_core::TypeCtor::List));
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
}

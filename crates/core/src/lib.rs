//! Beacon Core - Hindley-Milner Type System Implementation
//!
//! This module implements the core type system for Beacon, including:
//! - Type and Kind representations
//! - Type variable management
//! - Substitution and unification algorithms
//! - Type schemes and generalization
//! - Value restriction for sound generalization

pub mod annotation_parser;
pub mod class_metadata;
pub mod errors;
pub mod fixtures;
pub mod logging;
pub mod protocols;
pub mod subst;
pub mod suppressor;
pub mod types;
pub mod typevar;
pub mod typevar_registry;
pub mod unify;

pub use annotation_parser::{AnnotationParser, parse_annotation, parse_annotation_or_any};
pub use class_metadata::{ClassMetadata, ClassRegistry, MethodType};
pub use errors::{AnalysisError, BeaconError, ConfigError, DocumentError, ParseError, ResolveError, Result, TypeError};
pub use logging::{LogConfig, LogFormat, default_log_path, init, install_panic_hook, read_log_file};
pub use protocols::{MethodSignature, ProtocolChecker, ProtocolDef, ProtocolName};
pub use subst::{Subst, Substitutable};
pub use suppressor::{Suppression, SuppressionMap};
pub use types::{
    Kind, LiteralType, OverloadSet, Type, TypeCtor, TypeScheme, TypeVar, Variance, builtin_type_from_name,
    builtin_type_name, contains_type_var, decompose_app, decompose_class_app, decompose_protocol_app,
    format_type_for_diagnostic, is_any, literal_base_ctor, literal_to_base_type,
};
pub use typevar::TypeVarGen;
pub use typevar_registry::TypeVarConstraintRegistry;
pub use unify::Unifier;

/// Trait for expressions that can be tested for expansiveness
///
/// In ML-style type systems, the value restriction prevents unsound generalization by only generalizing non-expansive expressions.
/// An expression is non-expansive if:
///
/// - It's a literal (number, string, bool, None)
/// - It's a lambda expression
/// - It's a variable reference
/// - It's a constructor application with non-expansive arguments (tuples, lists)
///
/// Expansive expressions include:
/// - Function calls
/// - Attribute access
/// - Subscript operations
/// - Most other computations
///
/// This trait should be implemented by expression types in the AST.
pub trait Expansiveness {
    /// Returns true if this expression is non-expansive (safe to generalize)
    fn is_non_expansive(&self) -> bool;
}

/// Helper for determining if a binding should be generalized based on value restriction
///
/// This function encapsulates the value restriction logic.
/// In Python, we follow similar rules to ML but adapted for Python's semantics:
///
/// ## Non-expansive (can generalize):
/// - Literals: `42`, `"hello"`, `True`, `None`
/// - Lambda expressions: `lambda x: x`
/// - Variable bindings: `x = y` where `y` is a variable
/// - Tuple/List/Dict literals with non-expansive elements: `(1, 2)`, `[x]`
///
/// ## Expansive (cannot generalize):
/// - Function calls: `f(x)`
/// - Method calls: `obj.method()`
/// - Attribute access: `obj.attr`
/// - Subscripts: `lst[0]`
/// - Comprehensions: `[x for x in xs]`
///
/// ## Example:
/// ```ignore
/// # Safe to generalize (non-expansive)
/// x = lambda a: a  # ∀'a. 'a -> 'a
///
/// # Not safe to generalize (expansive)
/// y = f()  # 'b (not ∀'b. 'b), where 'b is fresh
/// ```
pub fn should_generalize<E: Expansiveness>(expr: &E) -> bool {
    expr.is_non_expansive()
}

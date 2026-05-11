use crate::{Span, TypePredicate};

use beacon_core::Type;

pub(super) fn record_narrowing_constraint(
    _variable: String, _predicate: TypePredicate, _narrowed_type: Type, _span: Span,
) {
    // Narrowing constraints document flow-sensitive facts for the solver, but
    // solver output is intentionally not rewritten here. The analyzer applies
    // narrowing while generating constraints so later AST nodes see refined
    // types in their local environment.
}

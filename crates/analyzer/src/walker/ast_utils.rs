//! Shared AST helpers for constraint generation.
//!
//! Keep these helpers focused on AST shape and source positions so visitor
//! modules do not each grow their own extraction logic.

use crate::type_env::TypeEnvironment;

use beacon_core::{Type, TypeScheme};
use beacon_parser::Comprehension;

pub(super) fn bind_comprehension_target(env: &mut TypeEnvironment, generator: &Comprehension, ty: &Type) {
    for name in generator.target_names() {
        env.bind(name.to_string(), TypeScheme::mono(ty.clone()));
    }
}

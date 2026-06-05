//! Type System Implementation for Beacon
//!
//! This module implements a Hindley-Milner type system with extensions for Python,
//! including row-polymorphic records, union types, and gradual typing support.

mod application;
mod constructors;
mod diagnostics;
pub mod helpers;
mod kinding;
mod model;
mod normalization;
mod overloads;
mod schemes;
mod simplify;
mod subtyping;

pub use helpers::{
    builtin_type_from_name, builtin_type_name, contains_type_var, decompose_app, decompose_class_app,
    decompose_protocol_app, format_type_for_diagnostic, is_any, literal_base_ctor, literal_to_base_type,
};
pub use model::{FunctionParam, FunctionParamKind, Kind, LiteralType, Type, TypeCtor, TypeVar, Variance};
pub use overloads::OverloadSet;
pub use schemes::TypeScheme;

#[cfg(test)]
mod tests;

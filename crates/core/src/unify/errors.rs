//! Unification error constructors.

use super::Unifier;
use crate::{BeaconError, TypeError};

impl Unifier {
    pub(super) fn unification_error(expected: impl Into<String>, actual: impl Into<String>) -> BeaconError {
        TypeError::UnificationError(expected.into(), actual.into()).into()
    }
}

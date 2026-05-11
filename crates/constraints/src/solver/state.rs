use crate::TypeErrorInfo;

use beacon_core::{ClassRegistry, Subst};

pub(super) struct SolveState<'a> {
    pub(super) subst: &'a mut Subst,
    pub(super) type_errors: &'a mut Vec<TypeErrorInfo>,
    pub(super) class_registry: &'a ClassRegistry,
    pub(super) typevar_registry: &'a beacon_core::TypeVarConstraintRegistry,
}

impl<'a> SolveState<'a> {
    pub(super) fn new(
        subst: &'a mut Subst, type_errors: &'a mut Vec<TypeErrorInfo>, class_registry: &'a ClassRegistry,
        typevar_registry: &'a beacon_core::TypeVarConstraintRegistry,
    ) -> Self {
        Self { subst, type_errors, class_registry, typevar_registry }
    }
}

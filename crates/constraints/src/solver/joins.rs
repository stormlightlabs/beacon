use crate::{Span, TypeErrorInfo};

use beacon_core::{BeaconError, ClassRegistry, Subst, Type, TypeCtor, Unifier};

pub(super) fn solve_join_constraint(
    incoming_types: Vec<Type>, result_type: Type, span: Span, subst: &mut Subst, type_errors: &mut Vec<TypeErrorInfo>,
    class_registry: &ClassRegistry, typevar_registry: &beacon_core::TypeVarConstraintRegistry,
) {
    let union_type = if incoming_types.is_empty() {
        Type::Con(TypeCtor::NoneType)
    } else if incoming_types.len() == 1 {
        incoming_types[0].clone()
    } else {
        Type::union(incoming_types)
    };

    match Unifier::unify_with_class_registry(
        &subst.apply(&result_type),
        &subst.apply(&union_type),
        typevar_registry,
        class_registry,
    ) {
        Ok(s) => {
            *subst = s.compose(subst.clone());
        }
        Err(BeaconError::TypeError(type_err)) => {
            type_errors.push(TypeErrorInfo::new(type_err, span));
        }
        Err(_) => {}
    }
}

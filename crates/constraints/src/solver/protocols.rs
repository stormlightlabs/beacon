use super::compat::{check_builtin_protocol_on_class, check_user_defined_protocol};
use super::state::SolveState;
use crate::{Span, TypeErrorInfo};

use beacon_core::{BeaconError, ProtocolChecker, ProtocolName, Type, Unifier};

pub(super) fn solve_protocol_constraint(
    obj_ty: &Type, protocol_name: &ProtocolName, elem_ty: &Type, span: Span, state: &mut SolveState<'_>,
) {
    let applied_obj = state.subst.apply(obj_ty);
    if matches!(applied_obj, Type::Var(_)) {
        return;
    }

    let satisfies = match &protocol_name {
        ProtocolName::UserDefined(proto_name) => {
            check_user_defined_protocol(&applied_obj, proto_name, state.class_registry, state.typevar_registry)
        }
        _ => {
            check_builtin_protocol_on_class(&applied_obj, protocol_name, state.class_registry)
                || ProtocolChecker::satisfies(&applied_obj, protocol_name)
        }
    };

    if satisfies {
        let extracted_elem = match protocol_name {
            ProtocolName::Iterable | ProtocolName::Iterator | ProtocolName::Sequence => {
                ProtocolChecker::extract_iterable_element(&applied_obj)
            }
            ProtocolName::AsyncIterable | ProtocolName::AsyncIterator => {
                ProtocolChecker::extract_async_iterable_element(&applied_obj)
            }
            ProtocolName::Awaitable => ProtocolChecker::extract_awaitable_result(&applied_obj),
            _ => Type::any(),
        };

        let applied_extracted = state.subst.apply(&extracted_elem);
        match Unifier::unify_with_class_registry(
            &state.subst.apply(elem_ty),
            &applied_extracted,
            state.typevar_registry,
            state.class_registry,
        ) {
            Ok(s) => {
                *state.subst = s.compose(state.subst.clone());
            }
            Err(BeaconError::TypeError(type_err)) => {
                state.type_errors.push(TypeErrorInfo::new(type_err, span));
            }
            Err(_) => {}
        }
    } else {
        state.type_errors.push(TypeErrorInfo::new(
            beacon_core::TypeError::ProtocolNotSatisfied(applied_obj.to_string(), protocol_name.to_string()),
            span,
        ));
    }
}

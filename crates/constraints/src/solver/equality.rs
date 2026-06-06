use super::{
    compat::{class_info, types_compatible},
    error_recovery::try_partial_unify,
    state::SolveState,
};
use crate::Span;

use beacon_core::{BeaconError, Type, TypeError, Unifier};

pub(super) fn solve_equal_constraint(t1: &Type, t2: &Type, span: Span, state: &mut SolveState<'_>) {
    let applied_t1 = state.subst.apply(t1);
    let applied_t2 = state.subst.apply(t2);

    let needs_variance_check =
        if let (Some((name1, args1)), Some((name2, args2))) = (class_info(&applied_t1), class_info(&applied_t2)) {
            name1 == name2
                && !args1.is_empty()
                && !args2.is_empty()
                && state
                    .class_registry
                    .get_class(&name1)
                    .is_some_and(|m| !m.type_param_vars.is_empty())
        } else {
            false
        };

    if needs_variance_check {
        if !types_compatible(&applied_t1, &applied_t2, state.class_registry, state.typevar_registry) {
            let (class_name, _) = class_info(&applied_t1).unwrap();
            state.type_errors.push(crate::TypeErrorInfo::new(
                TypeError::VarianceError {
                    position: format!("{class_name} type argument"),
                    expected_variance: "user-defined".to_string(),
                    got_type: applied_t1.display_for_diagnostics(),
                    expected_type: applied_t2.display_for_diagnostics(),
                },
                span,
            ));
        }
        return;
    }

    let involves_union = matches!(applied_t1, Type::Union(_)) || matches!(applied_t2, Type::Union(_));
    if involves_union && (applied_t1.is_subtype_of(&applied_t2) || applied_t2.is_subtype_of(&applied_t1)) {
        return;
    }

    match Unifier::unify_with_class_registry(&applied_t1, &applied_t2, state.typevar_registry, state.class_registry) {
        Ok(s) => {
            *state.subst = s.compose(state.subst.clone());
        }
        Err(BeaconError::TypeError(type_err)) => {
            let are_compatible =
                types_compatible(&applied_t1, &applied_t2, state.class_registry, state.typevar_registry)
                    || types_compatible(&applied_t2, &applied_t1, state.class_registry, state.typevar_registry);

            if !are_compatible {
                state.type_errors.push(crate::TypeErrorInfo::new(type_err, span));

                if let Some(partial_subst) = try_partial_unify(&applied_t1, &applied_t2, state.typevar_registry) {
                    tracing::debug!(
                        "Applied partial substitution after unification failure: {} = {}",
                        applied_t1.display_for_diagnostics(),
                        applied_t2.display_for_diagnostics()
                    );
                    *state.subst = partial_subst.compose(state.subst.clone());
                }
            }
        }
        Err(_) => {}
    }
}

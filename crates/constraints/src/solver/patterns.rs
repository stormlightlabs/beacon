use crate::{
    Span, TypeErrorInfo,
    exhaustiveness::{ExhaustivenessResult, ReachabilityResult, check_exhaustiveness, check_reachability},
    pattern_validation::{validate_pattern_structure, validate_pattern_type_compatibility},
};

use beacon_core::{ClassRegistry, Subst, Type, TypeError};
use beacon_parser::Pattern;

pub(super) fn solve_match_pattern_constraint(bindings: Vec<(String, Type)>, subst: &Subst) {
    for (_var_name, binding_ty) in bindings {
        let applied_binding = subst.apply(&binding_ty);
        let _ = applied_binding;
    }
}

pub(super) fn solve_pattern_exhaustive_constraint(
    subject_ty: &Type, patterns: &[(Pattern, bool)], span: Span, type_errors: &mut Vec<TypeErrorInfo>,
    class_registry: &ClassRegistry,
) {
    match check_exhaustiveness(subject_ty, patterns, class_registry) {
        ExhaustivenessResult::Exhaustive => {}
        ExhaustivenessResult::NonExhaustive { uncovered } => {
            let uncovered_str = uncovered
                .iter()
                .map(|ty| ty.display_for_diagnostics())
                .collect::<Vec<_>>()
                .join(", ");
            type_errors.push(TypeErrorInfo::new(TypeError::PatternNonExhaustive(uncovered_str), span));
        }
    }
}

pub(super) fn solve_pattern_reachable_constraint(
    pattern: &Pattern, previous_patterns: &[Pattern], span: Span, type_errors: &mut Vec<TypeErrorInfo>,
) {
    match check_reachability(pattern, previous_patterns) {
        ReachabilityResult::Reachable => {}
        ReachabilityResult::Unreachable { subsumed_by: _ } => {
            type_errors.push(TypeErrorInfo::new(TypeError::PatternUnreachable, span));
        }
    }
}

pub(super) fn solve_pattern_type_compatible_constraint(
    pattern: &Pattern, subject_ty: &Type, span: Span, type_errors: &mut Vec<TypeErrorInfo>,
    class_registry: &ClassRegistry,
) {
    if let Err(type_err) = validate_pattern_type_compatibility(pattern, subject_ty, class_registry) {
        type_errors.push(TypeErrorInfo::new(type_err, span));
    }
}

pub(super) fn solve_pattern_structure_valid_constraint(
    pattern: &Pattern, subject_ty: &Type, span: Span, type_errors: &mut Vec<TypeErrorInfo>,
) {
    if let Err(type_err) = validate_pattern_structure(pattern, subject_ty) {
        type_errors.push(TypeErrorInfo::new(type_err, span));
    }
}

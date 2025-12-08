//! Type guard detection for flow-sensitive type narrowing
//!
//! This module provides functionality for detecting and extracting type guards
//! from conditional expressions, enabling flow-sensitive typing where types are
//! narrowed based on runtime checks like isinstance, None checks, and user-defined guards.

use beacon_constraint::{TypeGuardInfo, TypeGuardKind, TypePredicate};
use beacon_core::Type;
use beacon_parser::{AstNode, CompareOperator, LiteralValue, UnaryOperator};

use crate::TypeEnvironment;

use super::utils::type_name_to_type;

/// Detect type guard patterns for flow-sensitive type narrowing
///
/// Supported patterns:
/// - `isinstance(x, int)` -> (x, int)
/// - `isinstance(x, str)` -> (x, str)
/// - `isinstance(x, (int, str))` -> (x, Union[int, str])
/// - `x is None` -> (x, None)
/// - `x is not None` -> (x, T) where x: Optional[T]
/// - `x == None` -> (x, None)
/// - `x != None` -> (x, T) where x: Optional[T]
/// - `if x:` -> (x, T) where x: Optional[T] (truthiness narrows out None)
pub fn detect_type_guard(test: &AstNode, env: &mut TypeEnvironment) -> (Option<String>, Option<Type>) {
    if let AstNode::Identifier { name: var_name, .. } = test {
        if let Some(current_type) = env.lookup(var_name)
            && (current_type.is_optional() || matches!(current_type, Type::Union(_)))
        {
            let narrowed = current_type.remove_from_union(&Type::none());
            return (Some(var_name.clone()), Some(narrowed));
        }
        return (None, None);
    }

    if let AstNode::Call { function, args, keywords, .. } = test
        && function.function_to_string() == "isinstance"
        && args.len() == 2
        && keywords.is_empty()
        && let AstNode::Identifier { name: var_name, .. } = &args[0]
    {
        if let AstNode::Identifier { name: type_name, .. } = &args[1] {
            let refined_type = type_name_to_type(type_name);
            return (Some(var_name.clone()), Some(refined_type));
        }

        if let AstNode::Tuple { elements, .. } = &args[1] {
            let types: Vec<Type> = elements
                .iter()
                .filter_map(|elem| {
                    if let AstNode::Identifier { name: type_name, .. } = elem {
                        Some(type_name_to_type(type_name))
                    } else {
                        None
                    }
                })
                .collect();
            if !types.is_empty() {
                let refined_type =
                    if types.len() == 1 { types.into_iter().next().unwrap() } else { Type::union(types) };
                return (Some(var_name.clone()), Some(refined_type));
            }
        }
    }

    if let AstNode::Compare { left, ops, comparators, .. } = test
        && ops.len() == 1
        && comparators.len() == 1
        && let AstNode::Identifier { name: var_name, .. } = left.as_ref()
        && let AstNode::Literal { value: LiteralValue::None, .. } = &comparators[0]
    {
        match &ops[0] {
            CompareOperator::Is | CompareOperator::Eq => {
                return (Some(var_name.clone()), Some(Type::none()));
            }
            CompareOperator::IsNot | CompareOperator::NotEq => {
                if let Some(current_type) = env.lookup(var_name) {
                    let narrowed = current_type.remove_from_union(&Type::none());
                    return (Some(var_name.clone()), Some(narrowed));
                }
                return (None, None);
            }
            _ => {}
        }
    }

    (None, None)
}

/// Detect inverse type guard for else branches
///
/// For example:
/// - `if x is not None:` -> else branch has type None
/// - `if isinstance(x, int):` -> else branch removes int from union
/// - `if x:` -> else branch narrows to None (for Optional types)
pub fn detect_inverse_type_guard(test: &AstNode, env: &mut TypeEnvironment) -> (Option<String>, Option<Type>) {
    if let AstNode::Identifier { name: var_name, .. } = test {
        if let Some(current_type) = env.lookup(var_name)
            && (current_type.is_optional() || matches!(current_type, Type::Union(_)))
        {
            return (Some(var_name.clone()), Some(Type::none()));
        }
        return (None, None);
    }

    if let AstNode::Call { function, args, keywords, .. } = test
        && function.function_to_string() == "isinstance"
        && args.len() == 2
        && keywords.is_empty()
        && let AstNode::Identifier { name: var_name, .. } = &args[0]
        && let Some(current_type) = env.lookup(var_name)
    {
        if let AstNode::Identifier { name: type_name, .. } = &args[1] {
            let checked_type = type_name_to_type(type_name);
            let narrowed = current_type.remove_from_union(&checked_type);
            return (Some(var_name.clone()), Some(narrowed));
        }

        if let AstNode::Tuple { elements, .. } = &args[1] {
            let mut result_type = current_type;
            for elem in elements {
                if let AstNode::Identifier { name: type_name, .. } = elem {
                    let checked_type = type_name_to_type(type_name);
                    result_type = result_type.remove_from_union(&checked_type);
                }
            }
            return (Some(var_name.clone()), Some(result_type));
        }
    }

    if let AstNode::Compare { left, ops, comparators, .. } = test
        && ops.len() == 1
        && comparators.len() == 1
        && let AstNode::Identifier { name: var_name, .. } = left.as_ref()
        && let AstNode::Literal { value: LiteralValue::None, .. } = &comparators[0]
    {
        match &ops[0] {
            CompareOperator::Is | CompareOperator::Eq => {
                if let Some(current_type) = env.lookup(var_name) {
                    let narrowed = current_type.remove_from_union(&Type::none());
                    return (Some(var_name.clone()), Some(narrowed));
                }
                return (None, None);
            }
            CompareOperator::IsNot | CompareOperator::NotEq => {
                return (Some(var_name.clone()), Some(Type::none()));
            }
            _ => {}
        }
    }

    (None, None)
}

/// Extract type guard information from a function's return annotation
///
/// Detects `TypeGuard[T]` or `TypeIs[T]` annotations and extracts the guarded type.
/// These annotations allow functions to act as user-defined type guards that narrow
/// types within conditional branches.
pub fn extract_type_guard_info(return_annotation: &str, _params: &[beacon_parser::Parameter]) -> Option<TypeGuardInfo> {
    let trimmed = return_annotation.trim();

    if trimmed.starts_with("TypeGuard[") || trimmed.starts_with("TypeIs[") {
        let kind = if trimmed.starts_with("TypeGuard[") { TypeGuardKind::TypeGuard } else { TypeGuardKind::TypeIs };
        let prefix = if kind == TypeGuardKind::TypeGuard { "TypeGuard[" } else { "TypeIs[" };

        if let Some(inner) = trimmed.strip_prefix(prefix)
            && let Some(type_str) = inner.strip_suffix(']')
        {
            let parser = beacon_core::AnnotationParser::new();
            if let Ok(guarded_type) = parser.parse(type_str) {
                // TODO: Support guarding specific parameters
                let param_index = 0;
                return Some(TypeGuardInfo::new(param_index, guarded_type, kind));
            }
        }
    }

    None
}

/// Extract a type predicate from a test expression for flow-sensitive narrowing
///
/// Supports:
/// - None checks: `x is not None`, `x is None`
/// - isinstance checks: `isinstance(x, Type)`, `isinstance(x, (Type1, Type2))`
/// - Truthiness checks: `if x:`
/// - Negation: `if not x:`
/// - User-defined type guards: `is_str(x)` where `is_str` returns `TypeGuard[str]`
pub fn extract_type_predicate(test: &AstNode, env: &TypeEnvironment) -> Option<TypePredicate> {
    match test {
        AstNode::Compare { left: _, ops, comparators, .. } if ops.len() == 1 && comparators.len() == 1 => {
            if let AstNode::Literal { value: LiteralValue::None, .. } = &comparators[0] {
                match &ops[0] {
                    CompareOperator::IsNot | CompareOperator::NotEq => Some(TypePredicate::IsNotNone),
                    CompareOperator::Is | CompareOperator::Eq => Some(TypePredicate::IsNone),
                    _ => None,
                }
            } else {
                None
            }
        }

        AstNode::Call { function, args, keywords, .. }
            if function.function_to_string() == "isinstance" && args.len() == 2 && keywords.is_empty() =>
        {
            let target_type = match &args[1] {
                AstNode::Identifier { name: type_name, .. } => Some(type_name_to_type(type_name)),
                AstNode::Tuple { elements, .. } => {
                    let types: Vec<Type> = elements
                        .iter()
                        .filter_map(|elem| {
                            if let AstNode::Identifier { name: type_name, .. } = elem {
                                Some(type_name_to_type(type_name))
                            } else {
                                None
                            }
                        })
                        .collect();

                    if types.is_empty() {
                        None
                    } else if types.len() == 1 {
                        Some(types.into_iter().next().unwrap())
                    } else {
                        Some(Type::union(types))
                    }
                }
                _ => None,
            }?;

            Some(TypePredicate::IsInstance(target_type))
        }
        AstNode::Call { function, args, .. } if !args.is_empty() => {
            if let Some(guard_info) = env.get_type_guard(&function.function_to_string())
                && args.len() > guard_info.param_index
            {
                return Some(TypePredicate::UserDefinedGuard(guard_info.guarded_type.clone()));
            }
            None
        }
        AstNode::Identifier { .. } => Some(TypePredicate::IsTruthy),
        AstNode::UnaryOp { op: UnaryOperator::Not, operand, .. } => {
            let inner_pred = extract_type_predicate(operand, env)?;
            Some(TypePredicate::Not(Box::new(inner_pred)))
        }
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extract_type_guard_info_with_typeguard() {
        let params = vec![];
        let result = extract_type_guard_info("TypeGuard[str]", &params);
        assert!(result.is_some(), "Should extract TypeGuard info");

        let guard_info = result.unwrap();
        assert_eq!(guard_info.kind, TypeGuardKind::TypeGuard);
    }

    #[test]
    fn test_extract_type_guard_info_with_typeis() {
        let params = vec![];
        let result = extract_type_guard_info("TypeIs[int]", &params);
        assert!(result.is_some(), "Should extract TypeIs info");

        let guard_info = result.unwrap();
        assert_eq!(guard_info.kind, TypeGuardKind::TypeIs);
    }

    #[test]
    fn test_extract_type_guard_info_with_whitespace() {
        let params = vec![];
        let result = extract_type_guard_info("  TypeGuard[str]  ", &params);
        assert!(result.is_some(), "Should handle whitespace around TypeGuard");
    }

    #[test]
    fn test_extract_type_guard_info_with_complex_type() {
        let params = vec![];
        let result = extract_type_guard_info("TypeGuard[list[str]]", &params);
        assert!(result.is_some(), "Should handle complex type parameters");
    }

    #[test]
    fn test_extract_type_guard_info_without_type_guard() {
        let params = vec![];
        let result = extract_type_guard_info("bool", &params);
        assert!(result.is_none(), "Should return None for non-TypeGuard annotations");
    }

    #[test]
    fn test_extract_type_guard_info_with_invalid_syntax() {
        let params = vec![];
        let result = extract_type_guard_info("TypeGuard[", &params);
        assert!(result.is_none(), "Should return None for invalid syntax");
    }
}

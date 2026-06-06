//! Type guard detection for flow-sensitive type narrowing
//!
//! This module provides functionality for detecting and extracting type guards
//! from conditional expressions, enabling flow-sensitive typing where types are
//! narrowed based on runtime checks like isinstance, None checks, and user-defined guards.

use super::TypeEnvironment;
use super::utils::type_name_to_type;
use beacon_constraint::{TypeGuardInfo, TypeGuardKind, TypePredicate};
use beacon_core::{Type, TypeCtor};
use beacon_parser::{AstNode, CompareOperator, LiteralValue, ParameterKind, UnaryOperator};

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
/// - `type(x) is T` / `type(x) == T` -> (x, T)
/// - `callable(x)` -> callable variants from a union, or `function`
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

    if let AstNode::Call { function, args, keywords, .. } = test {
        match function.qualified_name().as_deref() {
            Some("isinstance") if args.len() == 2 && keywords.is_empty() => {
                if let AstNode::Identifier { name: var_name, .. } = &args[0]
                    && let Some(refined_type) = type_expr_to_type(&args[1])
                {
                    return (Some(var_name.clone()), Some(refined_type));
                }
            }
            Some("callable") if args.len() == 1 && keywords.is_empty() => {
                if let AstNode::Identifier { name: var_name, .. } = &args[0] {
                    let refined_type = env
                        .lookup(var_name)
                        .map(narrow_to_callable)
                        .unwrap_or_else(callable_type);
                    return (Some(var_name.clone()), Some(refined_type));
                }
            }
            _ => {
                if let Some(function_name) = function.qualified_name()
                    && let Some(guard_info) = env.get_type_guard(&function_name)
                    && let Some(AstNode::Identifier { name: var_name, .. }) =
                        guarded_argument(args, keywords, guard_info)
                {
                    return (Some(var_name.clone()), Some(guard_info.guarded_type.clone()));
                }
            }
        }
    }

    if let AstNode::Compare { left, ops, comparators, .. } = test
        && ops.len() == 1
        && comparators.len() == 1
    {
        if let AstNode::Identifier { name: var_name, .. } = left.as_ref()
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

        if matches!(ops[0], CompareOperator::Is | CompareOperator::Eq)
            && let Some(var_name) = type_call_subject(left)
            && let Some(refined_type) = type_expr_to_type(&comparators[0])
        {
            return (Some(var_name), Some(refined_type));
        }
    }

    (None, None)
}

fn type_expr_to_type(expr: &AstNode) -> Option<Type> {
    match expr {
        AstNode::Identifier { name, .. } => Some(type_name_to_type(name)),
        AstNode::Tuple { elements, .. } => {
            let types: Vec<Type> = elements.iter().filter_map(type_expr_to_type).collect();
            match types.len() {
                0 => None,
                1 => types.into_iter().next(),
                _ => Some(Type::union(types)),
            }
        }
        _ => None,
    }
}

fn type_call_subject(expr: &AstNode) -> Option<String> {
    if let AstNode::Call { function, args, keywords, .. } = expr
        && function.qualified_name().as_deref() == Some("type")
        && args.len() == 1
        && keywords.is_empty()
        && let AstNode::Identifier { name, .. } = &args[0]
    {
        return Some(name.clone());
    }
    None
}

fn callable_type() -> Type {
    Type::Con(TypeCtor::Function)
}

fn is_callable_type(ty: &Type) -> bool {
    matches!(
        ty,
        Type::Fun(_, _) | Type::FunWithParams(_, _) | Type::BoundMethod(_, _, _)
    ) || matches!(ty, Type::Con(TypeCtor::Function))
}

fn narrow_to_callable(current_type: Type) -> Type {
    match current_type {
        Type::Union(types) => {
            let callable_types: Vec<Type> = types.into_iter().filter(is_callable_type).collect();
            if callable_types.is_empty() { callable_type() } else { Type::union(callable_types) }
        }
        ty if is_callable_type(&ty) => ty,
        _ => callable_type(),
    }
}

fn guarded_param_index(params: &[beacon_parser::Parameter]) -> usize {
    if params.len() > 1
        && matches!(
            params[0].kind,
            ParameterKind::PositionalOnly | ParameterKind::PositionalOrKeyword
        )
        && matches!(params[0].name.as_str(), "self" | "cls")
    {
        1
    } else {
        0
    }
}

fn guarded_argument<'a>(
    args: &'a [AstNode], keywords: &'a [(String, AstNode)], guard_info: &TypeGuardInfo,
) -> Option<&'a AstNode> {
    args.get(guard_info.param_index).or_else(|| {
        let param_name = guard_info.param_name.as_ref()?;
        keywords
            .iter()
            .find_map(|(name, value)| (name == param_name).then_some(value))
    })
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

    if let AstNode::Call { function, args, keywords, .. } = test {
        match function.qualified_name().as_deref() {
            Some("isinstance") if args.len() == 2 && keywords.is_empty() => {
                if let AstNode::Identifier { name: var_name, .. } = &args[0]
                    && let Some(current_type) = env.lookup(var_name)
                    && let Some(checked_type) = type_expr_to_type(&args[1])
                {
                    let narrowed = match checked_type {
                        Type::Union(types) => types
                            .into_iter()
                            .fold(current_type, |result, checked| result.remove_from_union(&checked)),
                        checked => current_type.remove_from_union(&checked),
                    };
                    return (Some(var_name.clone()), Some(narrowed));
                }
            }
            Some("callable") if args.len() == 1 && keywords.is_empty() => {
                if let AstNode::Identifier { name: var_name, .. } = &args[0]
                    && let Some(current_type) = env.lookup(var_name)
                    && let Type::Union(types) = current_type
                {
                    let non_callable: Vec<Type> = types.into_iter().filter(|ty| !is_callable_type(ty)).collect();
                    return (Some(var_name.clone()), Some(Type::union(non_callable)));
                }
            }
            _ => {
                if let Some(function_name) = function.qualified_name()
                    && let Some(guard_info) = env.get_type_guard(&function_name).cloned()
                    && guard_info.kind == TypeGuardKind::TypeIs
                    && let Some(AstNode::Identifier { name: var_name, .. }) =
                        guarded_argument(args, keywords, &guard_info)
                    && let Some(current_type) = env.lookup(var_name)
                {
                    return (
                        Some(var_name.clone()),
                        Some(current_type.remove_from_union(&guard_info.guarded_type)),
                    );
                }
            }
        }
    }

    if let AstNode::Compare { left, ops, comparators, .. } = test
        && ops.len() == 1
        && comparators.len() == 1
    {
        if let AstNode::Identifier { name: var_name, .. } = left.as_ref()
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

        if matches!(ops[0], CompareOperator::IsNot | CompareOperator::NotEq)
            && let Some(var_name) = type_call_subject(left)
            && let Some(current_type) = env.lookup(&var_name)
            && let Some(checked_type) = type_expr_to_type(&comparators[0])
        {
            return (Some(var_name), Some(current_type.remove_from_union(&checked_type)));
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
                let param_index = guarded_param_index(_params);
                let mut info = TypeGuardInfo::new(param_index, guarded_type, kind);
                if let Some(param) = _params.get(param_index) {
                    info = info.with_param_name(param.name.clone());
                }
                return Some(info);
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
            if function.qualified_name().as_deref() == Some("isinstance") && args.len() == 2 && keywords.is_empty() =>
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
        AstNode::Call { function, args, keywords, .. } if !args.is_empty() || !keywords.is_empty() => {
            if let Some(function_name) = function.qualified_name()
                && let Some(guard_info) = env.get_type_guard(&function_name)
                && guarded_argument(args, keywords, guard_info).is_some()
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

    #[test]
    fn test_extract_type_guard_info_skips_self_parameter() {
        let params = vec![
            beacon_parser::Parameter {
                name: "self".to_string(),
                line: 1,
                col: 0,
                end_line: 1,
                end_col: 4,
                type_annotation: None,
                default_value: None,
                kind: ParameterKind::PositionalOrKeyword,
            },
            beacon_parser::Parameter {
                name: "value".to_string(),
                line: 1,
                col: 6,
                end_line: 1,
                end_col: 11,
                type_annotation: None,
                default_value: None,
                kind: ParameterKind::PositionalOrKeyword,
            },
        ];

        let result = extract_type_guard_info("TypeIs[str]", &params).unwrap();
        assert_eq!(result.param_index, 1);
        assert_eq!(result.param_name.as_deref(), Some("value"));
    }

    #[test]
    fn test_detect_user_defined_guard_for_keyword_argument() {
        let mut env = TypeEnvironment::new();
        env.bind(
            "candidate".to_string(),
            beacon_core::TypeScheme::mono(Type::union(vec![Type::int(), Type::string()])),
        );
        env.register_type_guard(
            "is_text".to_string(),
            TypeGuardInfo::new(0, Type::string(), TypeGuardKind::TypeGuard).with_param_name("value"),
        );

        let test = keyword_guard_call();

        let (var_name, narrowed_type) = detect_type_guard(&test, &mut env);
        assert_eq!(var_name.as_deref(), Some("candidate"));
        assert_eq!(narrowed_type, Some(Type::string()));
    }

    #[test]
    fn test_detect_inverse_typeis_for_keyword_argument() {
        let mut env = TypeEnvironment::new();
        env.bind(
            "candidate".to_string(),
            beacon_core::TypeScheme::mono(Type::union(vec![Type::int(), Type::string()])),
        );
        env.register_type_guard(
            "is_text".to_string(),
            TypeGuardInfo::new(0, Type::string(), TypeGuardKind::TypeIs).with_param_name("value"),
        );

        let test = keyword_guard_call();

        let (var_name, narrowed_type) = detect_inverse_type_guard(&test, &mut env);
        assert_eq!(var_name.as_deref(), Some("candidate"));
        assert_eq!(narrowed_type, Some(Type::int()));
    }

    fn keyword_guard_call() -> AstNode {
        AstNode::Call {
            function: Box::new(AstNode::Identifier {
                name: "is_text".to_string(),
                line: 1,
                col: 3,
                end_line: 1,
                end_col: 10,
            }),
            args: vec![],
            keywords: vec![(
                "value".to_string(),
                AstNode::Identifier { name: "candidate".to_string(), line: 1, col: 17, end_line: 1, end_col: 26 },
            )],
            line: 1,
            col: 3,
            end_line: 1,
            end_col: 27,
        }
    }
}

use crate::{is_dict_type, is_sequence_type, type_compatible_with_class};

use beacon_core::{Type, TypeCtor};
use beacon_parser::Pattern;

/// Narrow a type based on a pattern match
///
/// For example, matching `case int(x):` narrows a `int | str` union to just `int`.
pub fn narrow_type_by_pattern(ty: &Type, pattern: &beacon_parser::Pattern) -> Type {
    match pattern {
        Pattern::MatchValue(_) => ty.clone(),
        Pattern::MatchSequence(_) => match ty {
            Type::Union(variants) => {
                let sequence_types: Vec<Type> = variants.iter().filter(|t| is_sequence_type(t)).cloned().collect();

                if sequence_types.is_empty() {
                    ty.clone()
                } else if sequence_types.len() == 1 {
                    sequence_types.into_iter().next().unwrap()
                } else {
                    Type::union(sequence_types)
                }
            }
            t if is_sequence_type(t) => t.clone(),
            _ => ty.clone(),
        },
        Pattern::MatchMapping { .. } => match ty {
            Type::Union(variants) => {
                let dict_types: Vec<Type> = variants.iter().filter(|t| is_dict_type(t)).cloned().collect();

                if dict_types.is_empty() {
                    ty.clone()
                } else if dict_types.len() == 1 {
                    dict_types.into_iter().next().unwrap()
                } else {
                    Type::union(dict_types)
                }
            }
            t if is_dict_type(t) => t.clone(),
            _ => ty.clone(),
        },
        Pattern::MatchClass { cls, .. } => {
            let target_type = Type::Con(TypeCtor::Class(cls.clone()));

            match ty {
                Type::Union(variants) => {
                    if variants.contains(&target_type) {
                        target_type
                    } else {
                        let compatible: Vec<Type> = variants
                            .iter()
                            .filter(|v| type_compatible_with_class(v, cls))
                            .cloned()
                            .collect();

                        if compatible.is_empty() {
                            target_type
                        } else if compatible.len() == 1 {
                            compatible.into_iter().next().unwrap()
                        } else {
                            Type::union(compatible)
                        }
                    }
                }
                _ => target_type,
            }
        }
        Pattern::MatchAs { pattern: Some(sub_pattern), .. } => narrow_type_by_pattern(ty, sub_pattern),
        Pattern::MatchAs { pattern: None, .. } => ty.clone(),
        Pattern::MatchOr(alternatives) => {
            let narrowed_types: Vec<Type> = alternatives.iter().map(|alt| narrow_type_by_pattern(ty, alt)).collect();

            if narrowed_types.is_empty() {
                ty.clone()
            } else if narrowed_types.len() == 1 {
                narrowed_types.into_iter().next().unwrap()
            } else {
                Type::union(narrowed_types)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use beacon_parser::{AstNode, LiteralValue, Pattern};

    #[test]
    fn test_narrow_type_by_pattern_match_value() {
        let union_type = Type::union(vec![Type::int(), Type::string()]);
        let literal = AstNode::Literal { value: LiteralValue::Integer(42), line: 1, col: 1, end_line: 1, end_col: 1 };
        let pattern = Pattern::MatchValue(literal);

        let result = narrow_type_by_pattern(&union_type, &pattern);
        assert_eq!(result, union_type);
    }

    #[test]
    fn test_narrow_type_by_pattern_sequence() {
        let union_type = Type::union(vec![Type::Con(TypeCtor::List), Type::string()]);
        let pattern = Pattern::MatchSequence(vec![]);
        let result = narrow_type_by_pattern(&union_type, &pattern);
        assert_eq!(result, Type::Con(TypeCtor::List));
    }

    #[test]
    fn test_narrow_type_by_pattern_mapping() {
        let union_type = Type::union(vec![Type::Con(TypeCtor::Dict), Type::Con(TypeCtor::List)]);
        let pattern = Pattern::MatchMapping { keys: vec![], patterns: vec![] };
        let result = narrow_type_by_pattern(&union_type, &pattern);
        assert_eq!(result, Type::Con(TypeCtor::Dict));
    }

    #[test]
    fn test_narrow_type_by_pattern_class() {
        let point_type = Type::Con(TypeCtor::Class("Point".to_string()));
        let circle_type = Type::Con(TypeCtor::Class("Circle".to_string()));
        let union_type = Type::union(vec![point_type.clone(), circle_type]);
        let pattern = Pattern::MatchClass { cls: "Point".to_string(), patterns: vec![] };
        let result = narrow_type_by_pattern(&union_type, &pattern);
        assert_eq!(result, point_type);
    }

    #[test]
    fn test_narrow_type_by_pattern_as_with_subpattern() {
        let union_type = Type::union(vec![Type::int(), Type::string()]);
        let sub_pattern = Pattern::MatchClass { cls: "int".to_string(), patterns: vec![] };
        let pattern = Pattern::MatchAs { pattern: Some(Box::new(sub_pattern)), name: Some("x".to_string()) };
        let result = narrow_type_by_pattern(&union_type, &pattern);
        let expected = Type::Con(TypeCtor::Class("int".to_string()));
        assert_eq!(result, expected);
    }

    #[test]
    fn test_narrow_type_by_pattern_as_without_subpattern() {
        let union_type = Type::union(vec![Type::int(), Type::string()]);
        let pattern = Pattern::MatchAs { pattern: None, name: Some("x".to_string()) };
        let result = narrow_type_by_pattern(&union_type, &pattern);
        assert_eq!(result, union_type);
    }

    #[test]
    fn test_narrow_type_by_pattern_or() {
        let int_type = Type::Con(TypeCtor::Class("int".to_string()));
        let float_type = Type::Con(TypeCtor::Class("float".to_string()));
        let union_type = Type::union(vec![int_type.clone(), float_type.clone(), Type::string()]);

        let pattern1 = Pattern::MatchClass { cls: "int".to_string(), patterns: vec![] };
        let pattern2 = Pattern::MatchClass { cls: "float".to_string(), patterns: vec![] };
        let or_pattern = Pattern::MatchOr(vec![pattern1, pattern2]);

        let result = narrow_type_by_pattern(&union_type, &or_pattern);
        let expected = Type::union(vec![int_type, float_type]);
        assert_eq!(result, expected);
    }
}

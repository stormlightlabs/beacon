//! Diagnostic-oriented type formatting.

use super::{Type, TypeCtor};

impl Type {
    pub fn display_for_diagnostics(&self) -> String {
        match self {
            Type::BoundMethod(receiver, method_name, _) => match receiver.as_ref() {
                Type::Con(TypeCtor::Class(class_name)) => {
                    format!("{class_name}.{method_name}")
                }
                _ => {
                    format!("{receiver}.{method_name}")
                }
            },
            Type::Union(types) => {
                const MAX_UNION_DISPLAY: usize = 5;

                if types.len() == 2 {
                    let has_none = types.iter().any(|t| matches!(t, Type::Con(TypeCtor::NoneType)));
                    if has_none {
                        return self.to_string();
                    }
                }

                if types.len() > MAX_UNION_DISPLAY {
                    let shown: Vec<String> = types
                        .iter()
                        .take(MAX_UNION_DISPLAY)
                        .map(|t| t.display_for_diagnostics())
                        .collect();
                    format!("{} | ... ({} types total)", shown.join(" | "), types.len())
                } else {
                    types
                        .iter()
                        .map(|t| t.display_for_diagnostics())
                        .collect::<Vec<_>>()
                        .join(" | ")
                }
            }
            Type::Fun(args, ret) => {
                if args.is_empty() {
                    format!("() -> {}", ret.display_for_diagnostics())
                } else {
                    let all_auto_generated = args.iter().all(|(name, _)| {
                        name.is_empty() || (name.starts_with('_') && name[1..].chars().all(|c| c.is_ascii_digit()))
                    });

                    if all_auto_generated {
                        if args.len() == 1 {
                            format!(
                                "{} -> {}",
                                args[0].1.display_for_diagnostics(),
                                ret.display_for_diagnostics()
                            )
                        } else {
                            let params = args
                                .iter()
                                .map(|(_, t)| t.display_for_diagnostics())
                                .collect::<Vec<_>>()
                                .join(", ");
                            format!("({params}) -> {}", ret.display_for_diagnostics())
                        }
                    } else if args.len() == 1 {
                        format!(
                            "({}: {}) -> {}",
                            args[0].0,
                            args[0].1.display_for_diagnostics(),
                            ret.display_for_diagnostics()
                        )
                    } else {
                        let params = args
                            .iter()
                            .map(|(name, t)| format!("{name}: {}", t.display_for_diagnostics()))
                            .collect::<Vec<_>>()
                            .join(", ");
                        format!("({params}) -> {}", ret.display_for_diagnostics())
                    }
                }
            }
            Type::App(_t1, _t2) => self.to_string(),
            Type::ForAll(tvs, t) => {
                if tvs.is_empty() {
                    t.display_for_diagnostics()
                } else {
                    self.to_string()
                }
            }
            Type::Tuple(types) => {
                let elements = types
                    .iter()
                    .map(|t| t.display_for_diagnostics())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("tuple[{elements}]")
            }
            Type::Record(fields, row_var) => {
                let field_strs: Vec<String> = fields
                    .iter()
                    .map(|(k, v)| format!("{k}: {}", v.display_for_diagnostics()))
                    .collect();
                match row_var {
                    Some(rv) => format!("{{ {} | {rv} }}", field_strs.join(", ")),
                    None => format!("{{ {} }}", field_strs.join(", ")),
                }
            }
            Type::Intersection(types) => types
                .iter()
                .map(|t| t.display_for_diagnostics())
                .collect::<Vec<_>>()
                .join(" & "),
            _ => self.to_string(),
        }
    }

    /// Check if this type represents an Optional type (Union[T, None])
    pub fn is_optional_type(&self) -> Option<&Type> {
        match self {
            Type::Union(types) if types.len() == 2 => {
                let mut non_none_type = None;
                let mut has_none = false;

                for ty in types {
                    match ty {
                        Type::Con(TypeCtor::NoneType) => has_none = true,
                        _ => non_none_type = Some(ty),
                    }
                }

                if has_none && non_none_type.is_some() { non_none_type } else { None }
            }
            _ => None,
        }
    }
}

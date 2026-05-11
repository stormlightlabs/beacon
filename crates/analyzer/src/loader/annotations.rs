use super::typevars::StubTypeContext;

use beacon_core::{AnnotationParser, Type, TypeCtor};

pub(crate) fn extract_generic_params(annotation: &str) -> Option<String> {
    let start = annotation.find('[')?;
    let end = annotation.rfind(']')?;
    if end > start { Some(annotation[start + 1..end].to_string()) } else { None }
}

/// Parse comma-separated generic parameters, respecting nested brackets
/// e.g., "str, int" → [Type::string(), Type::int()]
/// e.g., "dict[str, int], list[str]" → [Type::App(...), Type::App(...)]
pub(crate) fn parse_generic_params(params_str: &str, ctx: &mut StubTypeContext) -> Vec<beacon_core::Type> {
    let mut types = Vec::new();
    let mut current = String::new();
    let mut bracket_depth = 0;

    for ch in params_str.chars() {
        match ch {
            '[' => {
                bracket_depth += 1;
                current.push(ch);
            }
            ']' => {
                bracket_depth -= 1;
                current.push(ch);
            }
            ',' if bracket_depth == 0 => {
                if let Some(ty) = parse_type_annotation(current.trim(), ctx) {
                    types.push(ty);
                }
                current.clear();
            }
            _ => {
                current.push(ch);
            }
        }
    }

    if !current.is_empty()
        && let Some(ty) = parse_type_annotation(current.trim(), ctx)
    {
        types.push(ty);
    }

    types
}

/// Parse a type annotation string into a Type
/// This is a simplified version for stub parsing
pub fn parse_type_annotation(annotation: &str, ctx: &mut StubTypeContext) -> Option<beacon_core::Type> {
    let annotation = annotation.trim();

    if ctx.is_type_var(annotation) {
        return Some(beacon_core::Type::Con(TypeCtor::TypeVariable(annotation.to_string())));
    }

    let parser = AnnotationParser::new();
    if let Ok(parsed) = parser.parse(annotation) {
        return Some(convert_type_vars(parsed, ctx));
    }

    if annotation.contains('|') {
        let types = annotation
            .split('|')
            .map(|s| s.trim())
            .filter_map(|p| parse_type_annotation(p, ctx))
            .collect();
        return Some(Type::Union(types));
    }

    if let Some(idx) = annotation.find('[') {
        let base = &annotation[..idx];

        if let Some(params_str) = extract_generic_params(annotation) {
            let type_params = parse_generic_params(&params_str, ctx);

            if type_params.is_empty() {
                let base_type = match base {
                    "list" => beacon_core::Type::Con(TypeCtor::List),
                    "dict" => beacon_core::Type::Con(TypeCtor::Dict),
                    "set" => beacon_core::Type::Con(TypeCtor::Set),
                    "tuple" => beacon_core::Type::Con(TypeCtor::Tuple),
                    _ => beacon_core::Type::Con(TypeCtor::Class(base.to_string())),
                };
                return Some(base_type);
            }

            match base {
                "list" | "set" | "tuple" => {
                    let elem_ty = type_params.first()?;
                    let ctor = match base {
                        "list" => TypeCtor::List,
                        "set" => TypeCtor::Set,
                        "tuple" => TypeCtor::Tuple,
                        _ => unreachable!(),
                    };
                    return Some(beacon_core::Type::App(
                        Box::new(beacon_core::Type::Con(ctor)),
                        Box::new(elem_ty.clone()),
                    ));
                }
                "dict" => {
                    if type_params.len() >= 2 {
                        let key_ty = &type_params[0];
                        let val_ty = &type_params[1];
                        return Some(beacon_core::Type::App(
                            Box::new(beacon_core::Type::App(
                                Box::new(beacon_core::Type::Con(TypeCtor::Dict)),
                                Box::new(key_ty.clone()),
                            )),
                            Box::new(val_ty.clone()),
                        ));
                    } else {
                        let key_ty = &type_params[0];
                        return Some(beacon_core::Type::App(
                            Box::new(beacon_core::Type::App(
                                Box::new(beacon_core::Type::Con(TypeCtor::Dict)),
                                Box::new(key_ty.clone()),
                            )),
                            Box::new(beacon_core::Type::any()),
                        ));
                    }
                }
                _ => {
                    let mut result = beacon_core::Type::Con(TypeCtor::Class(base.to_string()));
                    for param in type_params {
                        result = beacon_core::Type::App(Box::new(result), Box::new(param));
                    }
                    return Some(result);
                }
            }
        }
    }

    match annotation {
        "int" => Some(beacon_core::Type::Con(TypeCtor::Int)),
        "float" => Some(beacon_core::Type::Con(TypeCtor::Float)),
        "str" => Some(beacon_core::Type::Con(TypeCtor::String)),
        "bool" => Some(beacon_core::Type::Con(TypeCtor::Bool)),
        "None" => Some(beacon_core::Type::Con(TypeCtor::NoneType)),
        "NoneType" => Some(beacon_core::Type::Con(TypeCtor::NoneType)),
        _ => Some(beacon_core::Type::Con(TypeCtor::Class(annotation.to_string()))),
    }
}

fn convert_type_vars(ty: beacon_core::Type, ctx: &mut StubTypeContext) -> beacon_core::Type {
    match ty {
        beacon_core::Type::Con(TypeCtor::Class(name)) if ctx.is_type_var(&name) => {
            beacon_core::Type::Var(ctx.get_or_create_type_var(&name))
        }
        beacon_core::Type::App(base, arg) => beacon_core::Type::App(
            Box::new(convert_type_vars(*base, ctx)),
            Box::new(convert_type_vars(*arg, ctx)),
        ),
        beacon_core::Type::Fun(params, ret) => beacon_core::Type::Fun(
            params.into_iter().map(|p| (p.0, convert_type_vars(p.1, ctx))).collect(),
            Box::new(convert_type_vars(*ret, ctx)),
        ),
        beacon_core::Type::Union(types) => {
            beacon_core::Type::union(types.into_iter().map(|t| convert_type_vars(t, ctx)).collect())
        }
        beacon_core::Type::Intersection(types) => {
            beacon_core::Type::intersection(types.into_iter().map(|t| convert_type_vars(t, ctx)).collect())
        }
        beacon_core::Type::ForAll(vars, inner) => {
            beacon_core::Type::ForAll(vars, Box::new(convert_type_vars(*inner, ctx)))
        }
        beacon_core::Type::Record(fields, row) => beacon_core::Type::Record(
            fields
                .into_iter()
                .map(|(name, t)| (name, convert_type_vars(t, ctx)))
                .collect(),
            row,
        ),
        other => other,
    }
}

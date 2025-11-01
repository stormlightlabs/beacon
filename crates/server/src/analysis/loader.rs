use crate::analysis::MethodInfo;
use beacon_core::TypeCtor;
use beacon_core::{
    ClassMetadata, ClassRegistry, Type,
    errors::{AnalysisError, Result},
};
use beacon_parser::AstNode;
use std::collections::HashMap;
use std::path::Path;

/// Extract class metadata from stub AST and register them in the ClassRegistry
pub fn extract_stub_classes_into_registry(node: &AstNode, class_registry: &mut ClassRegistry) {
    match node {
        AstNode::Module { body, .. } => {
            for stmt in body {
                extract_stub_classes_into_registry(stmt, class_registry);
            }
        }
        AstNode::ClassDef { name, bases, metaclass, body, .. } => {
            let mut metadata = ClassMetadata::new(name.clone());
            let is_protocol = bases
                .iter()
                .any(|base| base == "Protocol" || base.ends_with(".Protocol") || base == "typing.Protocol");
            metadata.set_protocol(is_protocol);

            for base in bases {
                metadata.add_base_class(base.clone());
            }

            if let Some(meta) = metaclass {
                metadata.set_metaclass(meta.clone());
            }

            process_class_methods(body, &mut metadata);

            class_registry.register_class(name.clone(), metadata);
        }
        _ => {}
    }
}

/// Process class methods, grouping overloads together
///
/// Handles @overload decorators by collecting multiple signatures for the same method name.
/// Functions with @overload are signature declarations only; the function without @overload
/// (if present) becomes the implementation signature.
pub fn process_class_methods(body: &[AstNode], metadata: &mut ClassMetadata) {
    let mut methods_by_name: HashMap<String, Vec<MethodInfo>> = HashMap::new();

    for stmt in body {
        if let AstNode::FunctionDef { name: method_name, args: params, return_type, decorators, .. } = stmt {
            let param_types: Vec<beacon_core::Type> = params
                .iter()
                .filter_map(|p| p.type_annotation.as_ref())
                .filter_map(|ann| parse_type_annotation(ann))
                .collect();

            let ret_type = return_type
                .as_ref()
                .and_then(|ann| parse_type_annotation(ann))
                .unwrap_or_else(beacon_core::Type::any);

            let is_overload = decorators.iter().any(|d| d == "overload");

            methods_by_name
                .entry(method_name.clone())
                .or_default()
                .push(MethodInfo {
                    params: param_types,
                    return_type: ret_type,
                    decorators: decorators.clone(),
                    is_overload,
                });
        }
    }

    for (method_name, method_infos) in methods_by_name {
        if method_name == "__init__" {
            if let Some(info) = method_infos.first() {
                let func_type = beacon_core::Type::fun(info.params.clone(), info.return_type.clone());
                metadata.set_init_type(func_type);
            }
            continue;
        }

        if method_name == "__new__" {
            if let Some(info) = method_infos.first() {
                let func_type = beacon_core::Type::fun(info.params.clone(), info.return_type.clone());
                metadata.set_new_type(func_type);
            }
            continue;
        }

        let first_info = &method_infos[0];
        let has_property = first_info.decorators.iter().any(|d| d == "property");
        let has_staticmethod = first_info.decorators.iter().any(|d| d == "staticmethod");
        let has_classmethod = first_info.decorators.iter().any(|d| d == "classmethod");

        if has_property {
            metadata.add_property(method_name.clone(), first_info.return_type.clone());
            continue;
        }

        let has_overloads = method_infos.iter().any(|info| info.is_overload);

        if has_overloads && method_infos.len() > 1 {
            let mut overload_sigs = Vec::new();
            let mut implementation = None;

            for info in method_infos {
                let mut params = info.params.clone();

                if has_staticmethod && !params.is_empty() {
                    params = params.iter().skip(1).cloned().collect();
                }

                let func_type = Type::fun(params, info.return_type.clone());

                if info.is_overload {
                    overload_sigs.push(func_type);
                } else {
                    implementation = Some(func_type);
                }
            }

            let overload_set = beacon_core::OverloadSet { signatures: overload_sigs, implementation };

            if has_staticmethod {
                if let Some(impl_type) = overload_set.implementation.clone() {
                    metadata.add_staticmethod(method_name.clone(), impl_type);
                } else if let Some(first_sig) = overload_set.signatures.first() {
                    metadata.add_staticmethod(method_name.clone(), first_sig.clone());
                }
            } else if has_classmethod {
                if let Some(impl_type) = overload_set.implementation.clone() {
                    metadata.add_classmethod(method_name.clone(), impl_type);
                } else if let Some(first_sig) = overload_set.signatures.first() {
                    metadata.add_classmethod(method_name.clone(), first_sig.clone());
                }
            } else {
                metadata.add_overloaded_method(method_name.clone(), overload_set);
            }
        } else {
            let mut params = first_info.params.clone();

            if has_staticmethod && !params.is_empty() {
                params = params.iter().skip(1).cloned().collect();
            }

            let func_type = Type::fun(params, first_info.return_type.clone());

            if has_staticmethod {
                metadata.add_staticmethod(method_name.clone(), func_type);
            } else if has_classmethod {
                metadata.add_classmethod(method_name.clone(), func_type);
            } else {
                metadata.add_method(method_name.clone(), func_type);
            }
        }
    }
}

/// Parse a type annotation string into a Type
/// This is a simplified version for stub parsing
pub fn parse_type_annotation(annotation: &str) -> Option<beacon_core::Type> {
    let annotation = annotation.trim();

    if annotation.contains(" | ") {
        let parts: Vec<&str> = annotation.split(" | ").collect();
        let types: Vec<beacon_core::Type> = parts.iter().filter_map(|p| parse_type_annotation(p)).collect();
        if types.len() > 1 {
            return Some(beacon_core::Type::Union(types));
        }
    }

    if let Some(idx) = annotation.find('[') {
        let base = &annotation[..idx];
        let base_type = match base {
            "list" => beacon_core::Type::Con(TypeCtor::List),
            "dict" => beacon_core::Type::Con(TypeCtor::Dict),
            "set" => beacon_core::Type::Con(TypeCtor::Set),
            "tuple" => beacon_core::Type::Con(TypeCtor::Tuple),
            _ => beacon_core::Type::Con(TypeCtor::Class(base.to_string())),
        };
        // TODO: Parse and apply generic parameters properly
        return Some(base_type);
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

pub fn load_builtins_into_registry(stub_path: &Path, class_registry: &mut ClassRegistry) -> Result<()> {
    match std::fs::read_to_string(stub_path) {
        Ok(content) => {
            let mut parser = crate::parser::LspParser::new()?;
            let parse_result = parser.parse(&content)?;
            extract_stub_classes_into_registry(&parse_result.ast, class_registry);
            Ok(())
        }
        Err(e) => Err(AnalysisError::from(e).into()),
    }
}

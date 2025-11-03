use crate::analysis::MethodInfo;
use beacon_core::TypeCtor;
use beacon_core::{
    ClassMetadata, ClassRegistry, Type,
    errors::{AnalysisError, Result},
};
use beacon_parser::AstNode;
use std::collections::HashMap;

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

                if base.starts_with("Generic[") && base.ends_with(']') {
                    if let Some(params_str) = extract_generic_params(base) {
                        let type_params: Vec<String> = params_str.split(',').map(|s| s.trim().to_string()).collect();
                        metadata.set_type_params(type_params);
                    }
                }
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
            let mut param_types: Vec<beacon_core::Type> = Vec::new();

            let has_self = params.first().map(|p| p.name == "self").unwrap_or(false);
            let has_cls = params.first().map(|p| p.name == "cls").unwrap_or(false);

            if has_self || has_cls {
                param_types.push(beacon_core::Type::any());
            }

            let start_idx = if has_self || has_cls { 1 } else { 0 };
            for param in params.iter().skip(start_idx) {
                if let Some(ann) = &param.type_annotation {
                    if let Some(ty) = parse_type_annotation(ann) {
                        param_types.push(ty);
                    }
                }
            }

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

/// Extract the content between brackets in a generic type annotation
/// e.g., "list[str]" → Some("str"), "dict[str, int]" → Some("str, int")
fn extract_generic_params(annotation: &str) -> Option<String> {
    let start = annotation.find('[')?;
    let end = annotation.rfind(']')?;
    if end > start { Some(annotation[start + 1..end].to_string()) } else { None }
}

/// Parse comma-separated generic parameters, respecting nested brackets
/// e.g., "str, int" → [Type::string(), Type::int()]
/// e.g., "dict[str, int], list[str]" → [Type::App(...), Type::App(...)]
fn parse_generic_params(params_str: &str) -> Vec<beacon_core::Type> {
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
                if let Some(ty) = parse_type_annotation(current.trim()) {
                    types.push(ty);
                }
                current.clear();
            }
            _ => {
                current.push(ch);
            }
        }
    }

    if !current.is_empty() {
        if let Some(ty) = parse_type_annotation(current.trim()) {
            types.push(ty);
        }
    }

    types
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

        if let Some(params_str) = extract_generic_params(annotation) {
            let type_params = parse_generic_params(&params_str);

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

pub fn load_builtins_into_registry(
    stub: &crate::workspace::StubFile, class_registry: &mut ClassRegistry,
) -> Result<()> {
    let content = match &stub.content {
        Some(embedded_content) => embedded_content.clone(),
        None => std::fs::read_to_string(&stub.path).map_err(AnalysisError::from)?,
    };

    let mut parser = crate::parser::LspParser::new()?;
    let parse_result = parser.parse(&content)?;
    extract_stub_classes_into_registry(&parse_result.ast, class_registry);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::workspace::StubFile;

    use beacon_core::{ClassRegistry, Type, TypeCtor};
    use rustc_hash::FxHashMap;
    use std::path::PathBuf;

    #[test]
    fn test_parse_list_with_generic_param() {
        let result = parse_type_annotation("list[str]").unwrap();

        match &result {
            Type::App(ctor, elem) => {
                assert!(matches!(**ctor, Type::Con(TypeCtor::List)));
                assert!(matches!(**elem, Type::Con(TypeCtor::String)));
            }
            _ => panic!("Expected Type::App for list[str], got {result:?}"),
        }
    }

    #[test]
    fn test_parse_dict_with_generic_params() {
        let result = parse_type_annotation("dict[str, int]").unwrap();

        match &result {
            Type::App(outer, val_ty) => {
                assert!(matches!(**val_ty, Type::Con(TypeCtor::Int)));
                match &**outer {
                    Type::App(inner_ctor, key_ty) => {
                        assert!(matches!(**inner_ctor, Type::Con(TypeCtor::Dict)));
                        assert!(matches!(**key_ty, Type::Con(TypeCtor::String)));
                    }
                    _ => panic!("Expected nested Type::App for dict key type"),
                }
            }
            _ => panic!("Expected Type::App for dict[str, int], got {result:?}"),
        }
    }

    #[test]
    fn test_parse_set_with_generic_param() {
        let result = parse_type_annotation("set[int]").unwrap();

        match &result {
            Type::App(ctor, elem) => {
                assert!(matches!(**ctor, Type::Con(TypeCtor::Set)));
                assert!(matches!(**elem, Type::Con(TypeCtor::Int)));
            }
            _ => panic!("Expected Type::App for set[int], got {result:?}"),
        }
    }

    #[test]
    fn test_parse_tuple_with_generic_param() {
        let result = parse_type_annotation("tuple[str]").unwrap();

        match &result {
            Type::App(ctor, elem) => {
                assert!(matches!(**ctor, Type::Con(TypeCtor::Tuple)));
                assert!(matches!(**elem, Type::Con(TypeCtor::String)));
            }
            _ => panic!("Expected Type::App for tuple[str], got {result:?}"),
        }
    }

    #[test]
    fn test_parse_nested_generic() {
        let result = parse_type_annotation("list[dict[str, int]]").unwrap();

        match &result {
            Type::App(list_ctor, dict_ty) => {
                assert!(matches!(**list_ctor, Type::Con(TypeCtor::List)));

                match &**dict_ty {
                    Type::App(outer, val_ty) => {
                        assert!(matches!(**val_ty, Type::Con(TypeCtor::Int)));
                        match &**outer {
                            Type::App(dict_ctor, key_ty) => {
                                assert!(matches!(**dict_ctor, Type::Con(TypeCtor::Dict)));
                                assert!(matches!(**key_ty, Type::Con(TypeCtor::String)));
                            }
                            _ => panic!("Expected nested Type::App for dict"),
                        }
                    }
                    _ => panic!("Expected Type::App for dict element type"),
                }
            }
            _ => panic!("Expected Type::App for list[dict[str, int]], got {result:?}"),
        }
    }

    #[test]
    fn test_parse_union_with_generics() {
        let result = parse_type_annotation("list[str] | None").unwrap();

        match &result {
            Type::Union(types) => {
                assert_eq!(types.len(), 2);

                match &types[0] {
                    Type::App(ctor, elem) => {
                        assert!(matches!(**ctor, Type::Con(TypeCtor::List)));
                        assert!(matches!(**elem, Type::Con(TypeCtor::String)));
                    }
                    _ => panic!("Expected Type::App for first union member"),
                }

                assert!(matches!(types[1], Type::Con(TypeCtor::NoneType)));
            }
            _ => panic!("Expected Union type, got {result:?}"),
        }
    }

    #[test]
    fn test_extract_generic_params() {
        assert_eq!(extract_generic_params("list[str]"), Some("str".to_string()));
        assert_eq!(extract_generic_params("dict[str, int]"), Some("str, int".to_string()));
        assert_eq!(
            extract_generic_params("list[dict[str, int]]"),
            Some("dict[str, int]".to_string())
        );
        assert_eq!(extract_generic_params("list"), None);
    }

    #[test]
    fn test_parse_generic_params_simple() {
        let params = parse_generic_params("str");
        assert_eq!(params.len(), 1);
        assert!(matches!(params[0], Type::Con(TypeCtor::String)));
    }

    #[test]
    fn test_parse_generic_params_multiple() {
        let params = parse_generic_params("str, int");
        assert_eq!(params.len(), 2);
        assert!(matches!(params[0], Type::Con(TypeCtor::String)));
        assert!(matches!(params[1], Type::Con(TypeCtor::Int)));
    }

    #[test]
    fn test_parse_generic_params_nested() {
        let params = parse_generic_params("dict[str, int], list[str]");
        assert_eq!(params.len(), 2);

        match &params[0] {
            Type::App(outer, val_ty) => {
                assert!(matches!(**val_ty, Type::Con(TypeCtor::Int)));
                match &**outer {
                    Type::App(dict_ctor, key_ty) => {
                        assert!(matches!(**dict_ctor, Type::Con(TypeCtor::Dict)));
                        assert!(matches!(**key_ty, Type::Con(TypeCtor::String)));
                    }
                    _ => panic!("Expected nested Type::App for dict"),
                }
            }
            _ => panic!("Expected Type::App for dict parameter"),
        }

        match &params[1] {
            Type::App(ctor, elem) => {
                assert!(matches!(**ctor, Type::Con(TypeCtor::List)));
                assert!(matches!(**elem, Type::Con(TypeCtor::String)));
            }
            _ => panic!("Expected Type::App for list parameter"),
        }
    }

    #[test]
    fn test_stub_loading_list_methods() {
        use crate::workspace::StubFile;
        use beacon_core::{ClassRegistry, MethodType};
        use rustc_hash::FxHashMap;
        use std::path::PathBuf;

        let stub_content = r#"
from typing import TypeVar, Generic

_T = TypeVar("_T")

class list(Generic[_T]):
    def append(self, item: _T) -> None: ...
    def pop(self) -> _T: ...
"#;

        let stub = StubFile {
            module: "builtins".to_string(),
            path: PathBuf::from("test.pyi"),
            exports: FxHashMap::default(),
            is_partial: false,
            reexports: Vec::new(),
            all_exports: None,
            content: Some(stub_content.to_string()),
        };

        let mut class_registry = ClassRegistry::new();
        load_builtins_into_registry(&stub, &mut class_registry).unwrap();

        let list_class = class_registry.get_class("list");
        assert!(list_class.is_some(), "list class should be registered");

        let list_metadata = list_class.unwrap();

        let append_method = list_metadata.methods.get("append");
        assert!(append_method.is_some(), "append method should exist");

        if let Some(append_ty) = append_method {
            match append_ty {
                MethodType::Single(Type::Fun(params, _)) => {
                    assert_eq!(
                        params.len(),
                        2,
                        "append should have 2 parameters (self + item), got {}: {:?}",
                        params.len(),
                        params
                    );
                }
                _ => panic!("append should be a single function type, got {append_ty:?}"),
            }
        }

        let pop_method = list_metadata.methods.get("pop");
        assert!(pop_method.is_some(), "pop method should exist");
    }

    #[test]
    fn test_stub_loading_real_builtins() {
        use crate::workspace::StubFile;
        use beacon_core::ClassRegistry;
        use rustc_hash::FxHashMap;
        use std::path::PathBuf;

        let stub_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .unwrap()
            .parent()
            .unwrap()
            .join("stubs/builtins.pyi");

        let stub_content = std::fs::read_to_string(&stub_path).unwrap();

        let stub = StubFile {
            module: "builtins".to_string(),
            path: stub_path,
            exports: FxHashMap::default(),
            is_partial: false,
            reexports: Vec::new(),
            all_exports: None,
            content: Some(stub_content),
        };

        let mut class_registry = ClassRegistry::new();
        load_builtins_into_registry(&stub, &mut class_registry).unwrap();

        let list_class = class_registry.get_class("list");
        assert!(list_class.is_some(), "list class should be registered in builtins.pyi");

        let list_metadata = list_class.unwrap();

        let append_method = list_metadata.methods.get("append");
        assert!(append_method.is_some(), "append method should exist in builtins.pyi");
    }

    #[test]
    fn test_stub_loading_typing_module() {
        let stub_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .unwrap()
            .parent()
            .unwrap()
            .join("stubs/typing.pyi");

        let stub_content = std::fs::read_to_string(&stub_path).unwrap();

        let stub = StubFile {
            module: "typing".to_string(),
            path: stub_path,
            exports: FxHashMap::default(),
            is_partial: false,
            reexports: Vec::new(),
            all_exports: None,
            content: Some(stub_content),
        };

        let mut class_registry = ClassRegistry::new();
        load_builtins_into_registry(&stub, &mut class_registry).unwrap();

        let generator_class = class_registry.get_class("Generator");
        assert!(
            generator_class.is_some(),
            "Generator class should be registered in typing.pyi"
        );

        let generator_metadata = generator_class.unwrap();
        assert!(
            generator_metadata.methods.contains_key("__iter__"),
            "Generator should have __iter__ method"
        );
        assert!(
            generator_metadata.methods.contains_key("__next__"),
            "Generator should have __next__ method"
        );
        assert!(
            generator_metadata.methods.contains_key("send"),
            "Generator should have send method"
        );

        let iterator_class = class_registry.get_class("Iterator");
        assert!(
            iterator_class.is_some(),
            "Iterator class should be registered in typing.pyi"
        );

        let iterator_metadata = iterator_class.unwrap();
        assert!(
            iterator_metadata.methods.contains_key("__next__"),
            "Iterator should have __next__ method"
        );
        assert!(
            iterator_metadata.methods.contains_key("__iter__"),
            "Iterator should have __iter__ method"
        );

        let iterable_class = class_registry.get_class("Iterable");
        assert!(
            iterable_class.is_some(),
            "Iterable class should be registered in typing.pyi"
        );

        let iterable_metadata = iterable_class.unwrap();
        assert!(
            iterable_metadata.methods.contains_key("__iter__"),
            "Iterable should have __iter__ method"
        );
    }

    #[test]
    fn test_typing_protocol_types_loaded() {
        let stub_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .unwrap()
            .parent()
            .unwrap()
            .join("stubs/typing.pyi");

        let stub_content = std::fs::read_to_string(&stub_path).unwrap();

        let stub = StubFile {
            module: "typing".to_string(),
            path: stub_path,
            exports: FxHashMap::default(),
            is_partial: false,
            reexports: Vec::new(),
            all_exports: None,
            content: Some(stub_content),
        };

        let mut class_registry = ClassRegistry::new();
        load_builtins_into_registry(&stub, &mut class_registry).unwrap();

        let protocol_types = vec![
            "Generator",
            "Iterator",
            "Iterable",
            "AsyncGenerator",
            "AsyncIterator",
            "AsyncIterable",
            "Sequence",
            "Mapping",
        ];

        for protocol_name in protocol_types {
            let protocol_class = class_registry.get_class(protocol_name);
            assert!(
                protocol_class.is_some(),
                "{protocol_name} class should be registered in typing.pyi"
            );
        }
    }
}

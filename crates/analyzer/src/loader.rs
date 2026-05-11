//! Stub loading and stdlib registry bootstrap.

mod annotations;
mod cache;
mod class_registry;
mod stdlib;
mod typevars;

pub use annotations::parse_type_annotation;
pub use cache::{StubCache, StubFile};
pub use class_registry::{extract_stub_classes_into_registry, process_class_methods};
pub use stdlib::{load_stub_into_registry, new_class_registry_with_stdlib, new_typevar_registry_with_stdlib};
pub use typevars::{StubTypeContext, collect_stub_type_vars};

#[cfg(test)]
use annotations::{extract_generic_params, parse_generic_params};

#[cfg(test)]
mod tests {
    use super::StubFile;
    use super::*;

    use beacon_core::{ClassRegistry, Type, TypeCtor, Variance};
    use rustc_hash::FxHashMap;
    use std::path::PathBuf;

    fn new_ctx() -> StubTypeContext {
        StubTypeContext::new()
    }

    #[test]
    fn test_parse_list_with_generic_param() {
        let mut ctx = new_ctx();
        let result = parse_type_annotation("list[str]", &mut ctx).unwrap();

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
        let mut ctx = new_ctx();
        let result = parse_type_annotation("dict[str, int]", &mut ctx).unwrap();

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
        let mut ctx = new_ctx();
        let result = parse_type_annotation("set[int]", &mut ctx).unwrap();

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
        let mut ctx = new_ctx();
        let result = parse_type_annotation("tuple[str]", &mut ctx).unwrap();

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
        let mut ctx = new_ctx();
        let result = parse_type_annotation("list[dict[str, int]]", &mut ctx).unwrap();

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
        let mut ctx = new_ctx();
        let result = parse_type_annotation("list[str] | None", &mut ctx).unwrap();

        match &result {
            Type::Union(types) => {
                assert_eq!(types.len(), 2);

                let has_list = types.iter().any(|t| {
                    matches!(
                        t,
                        Type::App(ctor, elem)
                            if matches!(**ctor, Type::Con(TypeCtor::List))
                                && matches!(**elem, Type::Con(TypeCtor::String))
                    )
                });
                let has_none = types.iter().any(|t| matches!(t, Type::Con(TypeCtor::NoneType)));

                assert!(has_list, "Expected list[str] in union");
                assert!(has_none, "Expected None in union");
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
        let mut ctx = new_ctx();
        let params = parse_generic_params("str", &mut ctx);
        assert_eq!(params.len(), 1);
        assert!(matches!(params[0], Type::Con(TypeCtor::String)));
    }

    #[test]
    fn test_parse_generic_params_multiple() {
        let mut ctx = new_ctx();
        let params = parse_generic_params("str, int", &mut ctx);
        assert_eq!(params.len(), 2);
        assert!(matches!(params[0], Type::Con(TypeCtor::String)));
        assert!(matches!(params[1], Type::Con(TypeCtor::Int)));
    }

    #[test]
    fn test_parse_generic_params_nested() {
        let mut ctx = new_ctx();
        let params = parse_generic_params("dict[str, int], list[str]", &mut ctx);
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
        use super::StubFile;
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
        let mut typevar_registry = beacon_core::TypeVarConstraintRegistry::new();
        load_stub_into_registry(&stub, &mut class_registry, &mut typevar_registry).unwrap();

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
    fn test_stub_loading_typeshed_builtins() {
        let class_registry = new_class_registry_with_stdlib();
        let list_class = class_registry.get_class("list");
        assert!(
            list_class.is_some(),
            "list class should be registered in typeshed builtins.pyi"
        );

        let list_metadata = list_class.unwrap();

        let append_method = list_metadata.methods.get("append");
        assert!(
            append_method.is_some(),
            "append method should exist in typeshed builtins.pyi"
        );
    }

    #[test]
    fn test_stub_loading_typeshed_typing_module() {
        let class_registry = new_class_registry_with_stdlib();
        let generator_class = class_registry.get_class("Generator");
        assert!(
            generator_class.is_some(),
            "Generator class should be registered in typeshed typing.pyi"
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
            "Iterator class should be registered in typeshed typing.pyi"
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
            "Iterable class should be registered in typeshed typing.pyi"
        );

        let iterable_metadata = iterable_class.unwrap();
        assert!(
            iterable_metadata.methods.contains_key("__iter__"),
            "Iterable should have __iter__ method"
        );
    }

    #[test]
    fn test_typeshed_typing_protocol_types_loaded() {
        let class_registry = new_class_registry_with_stdlib();

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
                "{protocol_name} class should be registered in typeshed typing.pyi"
            );
        }
    }

    #[test]
    fn test_typevar_with_bound() {
        let stub_content = r#"
from typing import TypeVar

class Animal: ...

T = TypeVar("T", bound=Animal)
"#;

        let stub = StubFile {
            module: "test_bound".to_string(),
            path: PathBuf::from("test_bound.pyi"),
            exports: FxHashMap::default(),
            is_partial: false,
            reexports: Vec::new(),
            all_exports: None,
            content: Some(stub_content.to_string()),
        };

        let mut class_registry = ClassRegistry::new();
        let mut typevar_registry = beacon_core::TypeVarConstraintRegistry::new();
        load_stub_into_registry(&stub, &mut class_registry, &mut typevar_registry).unwrap();

        let mut parser = beacon_parser::PythonParser::new().unwrap();
        let (ast, _) = parser.parse_and_resolve(stub_content).unwrap();
        let mut ctx = StubTypeContext::new();
        collect_stub_type_vars(&ast, &mut ctx);

        assert!(ctx.is_type_var("T"), "T should be registered as a TypeVar");

        let bound = ctx.get_bound("T");
        assert!(bound.is_some(), "T should have a bound");

        if let Some(bound_ty) = bound {
            assert!(
                matches!(bound_ty, Type::Con(TypeCtor::Class(name)) if name == "Animal"),
                "T should be bound to Animal, got {bound_ty:?}"
            );
        }
    }

    #[test]
    fn test_typevar_with_constraints() {
        let stub_content = r#"
from typing import TypeVar

T = TypeVar("T", int, str)
"#;

        let stub = StubFile {
            module: "test_constraints".to_string(),
            path: PathBuf::from("test_constraints.pyi"),
            exports: FxHashMap::default(),
            is_partial: false,
            reexports: Vec::new(),
            all_exports: None,
            content: Some(stub_content.to_string()),
        };

        let mut class_registry = ClassRegistry::new();
        let mut typevar_registry = beacon_core::TypeVarConstraintRegistry::new();
        load_stub_into_registry(&stub, &mut class_registry, &mut typevar_registry).unwrap();

        let mut parser = beacon_parser::PythonParser::new().unwrap();
        let (ast, _) = parser.parse_and_resolve(stub_content).unwrap();
        let mut ctx = StubTypeContext::new();
        collect_stub_type_vars(&ast, &mut ctx);

        assert!(ctx.is_type_var("T"), "T should be registered as a TypeVar");

        let constraints = ctx.get_constraints("T");
        assert!(constraints.is_some(), "T should have constraints");

        if let Some(constraint_types) = constraints {
            assert_eq!(constraint_types.len(), 2, "T should have 2 constraints");
            assert!(
                constraint_types.iter().any(|t| matches!(t, Type::Con(TypeCtor::Int))),
                "T should have int as a constraint"
            );
            assert!(
                constraint_types
                    .iter()
                    .any(|t| matches!(t, Type::Con(TypeCtor::String))),
                "T should have str as a constraint"
            );
        }
    }

    #[test]
    fn test_typevar_with_bound_and_variance() {
        let stub_content = r#"
from typing import TypeVar

class Animal: ...

T_Co = TypeVar("T_Co", bound=Animal, covariant=True)
"#;

        let stub = StubFile {
            module: "test_bound_variance".to_string(),
            path: PathBuf::from("test_bound_variance.pyi"),
            exports: FxHashMap::default(),
            is_partial: false,
            reexports: Vec::new(),
            all_exports: None,
            content: Some(stub_content.to_string()),
        };

        let mut class_registry = ClassRegistry::new();
        let mut typevar_registry = beacon_core::TypeVarConstraintRegistry::new();
        load_stub_into_registry(&stub, &mut class_registry, &mut typevar_registry).unwrap();

        let mut parser = beacon_parser::PythonParser::new().unwrap();
        let (ast, _) = parser.parse_and_resolve(stub_content).unwrap();
        let mut ctx = StubTypeContext::new();
        collect_stub_type_vars(&ast, &mut ctx);

        assert!(ctx.is_type_var("T_Co"), "T_Co should be registered as a TypeVar");

        let bound = ctx.get_bound("T_Co");
        assert!(bound.is_some(), "T_Co should have a bound");

        let type_var = ctx.get_or_create_type_var("T_Co");
        assert_eq!(type_var.variance, Variance::Covariant, "T_Co should be covariant");
    }

    #[test]
    fn test_typevar_registry_populated_from_stub() {
        let stub_content = r#"
from typing import TypeVar

class Animal: ...

T = TypeVar("T", bound=Animal)
U = TypeVar("U", int, str)
"#;

        let stub = StubFile {
            module: "test_registry".to_string(),
            path: PathBuf::from("test_registry.pyi"),
            exports: FxHashMap::default(),
            is_partial: false,
            reexports: Vec::new(),
            all_exports: None,
            content: Some(stub_content.to_string()),
        };

        let mut class_registry = ClassRegistry::new();
        let mut typevar_registry = beacon_core::TypeVarConstraintRegistry::new();
        load_stub_into_registry(&stub, &mut class_registry, &mut typevar_registry).unwrap();

        assert!(
            typevar_registry.bound_count() >= 1,
            "Registry should have at least 1 bound"
        );
        assert!(
            typevar_registry.constraint_count() >= 1,
            "Registry should have at least 1 constraint"
        );
    }
}

//! Class metadata extraction and dataclass/enum support
//!
//! This module provides utilities for extracting metadata from class definitions,
//! including support for dataclasses, enums, and special decorators.

use beacon_core::{ClassMetadata, Type, TypeCtor};
use beacon_parser::AstNode;

use super::TypeEnvironment;

/// Extract class metadata from a ClassDef node
///
/// Scans the class body for class-level fields and methods to build [ClassMetadata].
/// This metadata is used during Constraint::HasAttr constraint solving to resolve attribute types.
///
/// ## Field Extraction Strategy
///
/// This function extracts fields from two sources:
///
/// 1. **Class-level fields**: Annotations and assignments at class scope
///    - `x: int` or `x: int = 0` become class attributes
///    - In Python these are shared across instances, but for type checking we treat them as
///      "objects of this class have attribute x of type int"
///
/// 2. **Instance fields**: Assignments in `__init__` method
///    - `self.field = value` patterns found by recursively scanning `__init__` body
///    - These represent per-instance attributes
///
/// Both types are registered using `add_field` since the distinction matters for Python runtime
/// semantics (class variables are shared) but not for structural type checking (both are attributes
/// the object provides). If a name appears in both places, both get registered and the constraint
/// solver handles shadowing semantics.
///
/// ## TODO: Multiple Methods Issue
///
/// Classes with multiple methods (beyond just __init__) experience type unification errors during construction.
/// The issue appears to be related to how Type::fun() constructs function types when processing multiple
/// methods with a shared environment. Type variables or parameter lists may be getting confused between methods.
/// Consider cloning env for each method to isolate type variable generation.
pub fn extract_class_metadata(name: &str, body: &[AstNode], env: &mut TypeEnvironment) -> ClassMetadata {
    let mut metadata = ClassMetadata::new(name.to_string());

    for stmt in body {
        match stmt {
            AstNode::AnnotatedAssignment { target, type_annotation, .. } => {
                let field_type = env.parse_annotation_or_any(type_annotation);
                metadata.add_field(target.target_to_string(), field_type);
            }
            AstNode::Assignment { target, .. } => {
                let field_type = Type::Var(env.fresh_var());
                metadata.add_field(target.target_to_string(), field_type);
            }
            _ => {}
        }
    }

    for stmt in body {
        if let AstNode::FunctionDef { name: method_name, args, return_type, body: method_body, decorators, .. } = stmt {
            let params: Vec<(String, Type)> = args
                .iter()
                .map(|param| {
                    let param_type = param
                        .type_annotation
                        .as_ref()
                        .map(|ann| env.parse_annotation_or_any(ann))
                        .unwrap_or_else(|| Type::Var(env.fresh_var()));
                    (param.name.clone(), param_type)
                })
                .collect();

            let ret_type = return_type
                .as_ref()
                .map(|ann| env.parse_annotation_or_any(ann))
                .unwrap_or_else(|| Type::Var(env.fresh_var()));

            let has_property = decorators.iter().any(|d| d == "property");
            let has_staticmethod = decorators.iter().any(|d| d == "staticmethod");
            let has_classmethod = decorators.iter().any(|d| d == "classmethod");

            if method_name == "__init__" {
                let method_type = Type::fun(params.clone(), ret_type);
                metadata.set_init_type(method_type);

                for body_stmt in method_body {
                    extract_field_assignments(body_stmt, &mut metadata, env);
                }
            } else if method_name == "__new__" {
                metadata.set_new_type(Type::fun(params.clone(), ret_type));
            } else if has_property {
                metadata.add_property(method_name.clone(), ret_type);
            } else if has_staticmethod {
                let static_params: Vec<(String, Type)> =
                    if !params.is_empty() { params.iter().skip(1).cloned().collect() } else { params.clone() };
                metadata.add_staticmethod(method_name.clone(), Type::fun(static_params, ret_type));
            } else if has_classmethod {
                metadata.add_classmethod(method_name.clone(), Type::fun(params.clone(), ret_type));
            } else {
                metadata.add_method(method_name.clone(), Type::fun(params.clone(), ret_type));
            }
        }
    }

    metadata
}

/// Recursively extract field assignments from statement nodes by looking for patterns like
/// `self.field = value` or `self.field: Type = value` and registers the field in ClassMetadata.
///
/// This function traverses control flow constructs (if, for, while, try, with) to find all
/// possible field assignments within the method body.
pub fn extract_field_assignments(stmt: &AstNode, metadata: &mut ClassMetadata, env: &mut TypeEnvironment) {
    match stmt {
        AstNode::Assignment { target, .. } => {
            if let Some(field_name) = target.target_to_string().strip_prefix("self.") {
                let field_type = Type::Var(env.fresh_var());
                metadata.add_field(field_name.to_string(), field_type);
            }
        }
        AstNode::AnnotatedAssignment { target, type_annotation, .. } => {
            if let Some(field_name) = target.target_to_string().strip_prefix("self.") {
                let field_type = env.parse_annotation_or_any(type_annotation);
                metadata.add_field(field_name.to_string(), field_type);
            }
        }
        AstNode::If { body, elif_parts, else_body, .. } => {
            for body_stmt in body {
                extract_field_assignments(body_stmt, metadata, env);
            }
            for (_, elif_body) in elif_parts {
                for body_stmt in elif_body {
                    extract_field_assignments(body_stmt, metadata, env);
                }
            }
            if let Some(else_stmts) = else_body {
                for body_stmt in else_stmts {
                    extract_field_assignments(body_stmt, metadata, env);
                }
            }
        }
        AstNode::For { body, .. } | AstNode::While { body, .. } => {
            for body_stmt in body {
                extract_field_assignments(body_stmt, metadata, env);
            }
        }
        AstNode::Try { body, handlers, else_body, finally_body, .. } => {
            for body_stmt in body {
                extract_field_assignments(body_stmt, metadata, env);
            }
            for handler in handlers {
                for body_stmt in &handler.body {
                    extract_field_assignments(body_stmt, metadata, env);
                }
            }
            if let Some(else_stmts) = else_body {
                for body_stmt in else_stmts {
                    extract_field_assignments(body_stmt, metadata, env);
                }
            }
            if let Some(finally_stmts) = finally_body {
                for body_stmt in finally_stmts {
                    extract_field_assignments(body_stmt, metadata, env);
                }
            }
        }
        AstNode::With { body, .. } => {
            for body_stmt in body {
                extract_field_assignments(body_stmt, metadata, env);
            }
        }
        _ => {}
    }
}

/// Check if a decorator is @dataclass or a variant (e.g., dataclasses.dataclass)
pub fn is_dataclass_decorator(decorators: &[String]) -> bool {
    decorators.iter().any(|d| d == "dataclass" || d.ends_with(".dataclass"))
}

/// Check if any base class is Enum or a variant (e.g., enum.Enum, IntEnum, StrEnum)
pub fn has_enum_base(bases: &[String]) -> bool {
    bases.iter().any(|b| {
        b == "Enum"
            || b.ends_with(".Enum")
            || b == "IntEnum"
            || b.ends_with(".IntEnum")
            || b == "StrEnum"
            || b.ends_with(".StrEnum")
            || b == "Flag"
            || b.ends_with(".Flag")
            || b == "IntFlag"
            || b.ends_with(".IntFlag")
    })
}

/// Check if a decorator is a special class decorator that doesn't transform the type
///
/// These decorators modify class behavior but return the same class type:
/// - @dataclass: adds __init__, __repr__, etc., but type remains the same class
/// - @unique: enum decorator that enforces unique values
/// - Other non-type-transforming decorators can be added here
pub fn is_special_class_decorator(decorator: &str) -> bool {
    decorator == "dataclass"
        || decorator.ends_with(".dataclass")
        || decorator == "unique"
        || decorator.ends_with(".unique")
}

/// Synthesize __init__ method for a @dataclass from class-level fields
///
/// For a dataclass with fields `x: int` and `y: str`, this generates `__init__(self, x: int, y: str) -> None`.
/// This function only synthesizes __init__ if one doesn't already exist.
pub fn synthesize_dataclass_init(metadata: &mut ClassMetadata, _env: &mut TypeEnvironment) {
    if metadata.init_type.is_some() {
        return;
    }

    let mut params = vec![("self".to_string(), Type::any())];

    for (field_name, field_type) in &metadata.fields {
        params.push((field_name.clone(), field_type.clone()));
    }

    let init_type = Type::fun(params, Type::none());
    metadata.set_init_type(init_type);
}

/// Extract enum members from class body and register them as class attributes
///
/// For an Enum class, class-level assignments like `RED = 1` or `BLUE = auto()`
/// are enum members, not regular class fields. Each member has the type of the enum class itself.
///
/// Example:
/// ```python
/// class Color(Enum):
///     RED = 1
///     GREEN = 2
///     BLUE = auto()
/// ```
/// This creates attributes: Color.RED, Color.GREEN, Color.BLUE all of type Color
pub fn extract_enum_members(
    metadata: &mut ClassMetadata, body: &[AstNode], class_name: &str, _env: &mut TypeEnvironment,
) {
    let enum_type = Type::Con(TypeCtor::Class(class_name.to_string()));

    for stmt in body {
        match stmt {
            AstNode::Assignment { target, .. } | AstNode::AnnotatedAssignment { target, .. } => {
                let target_str = target.target_to_string();
                if !target_str.contains('.') && !target_str.starts_with('_') {
                    metadata.add_field(target_str, enum_type.clone());
                }
            }
            _ => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use beacon_parser::SymbolTable;
    use serde_json;

    macro_rules! walker_class_fixture {
        ($name:literal) => {{
            serde_json::from_str::<AstNode>(include_str!(concat!(
                env!("CARGO_MANIFEST_DIR"),
                "/src/fixtures/walker_class/",
                $name,
                ".json"
            )))
            .expect("invalid walker_class fixture JSON")
        }};
    }

    struct ClassFixture {
        ast: AstNode,
    }

    impl ClassFixture {
        fn new(source: &str, ast: AstNode) -> Self {
            let _ = source;
            Self { ast }
        }

        fn class_body(&self) -> &[AstNode] {
            if let AstNode::ClassDef { body, .. } = &self.ast {
                body
            } else {
                panic!("Expected top-level ClassDef in walker_class fixture");
            }
        }
    }

    #[test]
    fn test_is_dataclass_decorator() {
        assert!(is_dataclass_decorator(&["dataclass".to_string()]));
        assert!(is_dataclass_decorator(&["dataclasses.dataclass".to_string()]));
        assert!(is_dataclass_decorator(&["other".to_string(), "dataclass".to_string()]));
        assert!(!is_dataclass_decorator(&["property".to_string()]));
        assert!(!is_dataclass_decorator(&[]));
    }

    #[test]
    fn test_has_enum_base() {
        assert!(has_enum_base(&["Enum".to_string()]));
        assert!(has_enum_base(&["enum.Enum".to_string()]));
        assert!(has_enum_base(&["IntEnum".to_string()]));
        assert!(has_enum_base(&["enum.IntEnum".to_string()]));
        assert!(has_enum_base(&["StrEnum".to_string()]));
        assert!(has_enum_base(&["Flag".to_string()]));
        assert!(has_enum_base(&["IntFlag".to_string()]));
        assert!(has_enum_base(&["object".to_string(), "Enum".to_string()]));
        assert!(!has_enum_base(&["object".to_string()]));
        assert!(!has_enum_base(&[]));
    }

    #[test]
    fn test_is_special_class_decorator() {
        assert!(is_special_class_decorator("dataclass"));
        assert!(is_special_class_decorator("dataclasses.dataclass"));
        assert!(is_special_class_decorator("unique"));
        assert!(is_special_class_decorator("enum.unique"));
        assert!(!is_special_class_decorator("property"));
        assert!(!is_special_class_decorator("classmethod"));
    }

    #[test]
    fn test_synthesize_dataclass_init() {
        let source = "class Point:\n    x: int\n    y: str";
        let fixture = ClassFixture::new(source, walker_class_fixture!("test_synthesize_dataclass_init"));
        let symbol_table = SymbolTable::new();
        let mut env = crate::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let mut metadata = extract_class_metadata("Point", fixture.class_body(), &mut env);
        assert!(metadata.init_type.is_none());

        synthesize_dataclass_init(&mut metadata, &mut env);

        assert!(
            metadata.init_type.is_some(),
            "Dataclass should have synthesized __init__"
        );

        if let Some(Type::Fun(params, ret_ty)) = &metadata.init_type {
            assert!(!params.is_empty(), "__init__ should have at least self parameter");
            assert!(
                matches!(ret_ty.as_ref(), Type::Con(TypeCtor::NoneType)),
                "__init__ should return None"
            );
        } else {
            panic!("Expected function type for __init__");
        }
    }

    #[test]
    fn test_synthesize_dataclass_init_respects_explicit_init() {
        let source = "class CustomPoint:\n    x: int\n\n    def __init__(self):\n        pass";
        let fixture = ClassFixture::new(
            source,
            walker_class_fixture!("test_synthesize_dataclass_init_respects_explicit_init"),
        );
        let symbol_table = SymbolTable::new();
        let mut env = crate::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let mut metadata = extract_class_metadata("CustomPoint", fixture.class_body(), &mut env);
        let original_init = metadata.init_type.clone();
        synthesize_dataclass_init(&mut metadata, &mut env);
        assert_eq!(metadata.init_type, original_init);
    }

    #[test]
    fn test_extract_enum_members() {
        let source = "class Color:\n    RED = 1\n    GREEN = 2\n    BLUE = 3";
        let fixture = ClassFixture::new(source, walker_class_fixture!("test_extract_enum_members"));
        let symbol_table = SymbolTable::new();
        let mut env = crate::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let mut metadata = beacon_core::ClassMetadata::new("Color".to_string());
        extract_enum_members(&mut metadata, fixture.class_body(), "Color", &mut env);

        assert!(metadata.fields.contains_key("RED"), "Should extract RED as enum member");
        assert!(
            metadata.fields.contains_key("GREEN"),
            "Should extract GREEN as enum member"
        );
        assert!(
            metadata.fields.contains_key("BLUE"),
            "Should extract BLUE as enum member"
        );

        if let Some(red_type) = metadata.fields.get("RED") {
            match red_type {
                Type::Con(TypeCtor::Class(name)) => {
                    assert_eq!(name, "Color", "Enum member should have enum class type");
                }
                _ => panic!("Expected class type for enum member"),
            }
        }
    }

    #[test]
    fn test_extract_enum_members_ignores_methods() {
        let source = "class Color:\n    MEMBER = 1\n\n    def helper(self):\n        pass";
        let fixture = ClassFixture::new(
            source,
            walker_class_fixture!("test_extract_enum_members_ignores_methods"),
        );
        let symbol_table = SymbolTable::new();
        let mut env = crate::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let mut metadata = beacon_core::ClassMetadata::new("Color".to_string());
        extract_enum_members(&mut metadata, fixture.class_body(), "Color", &mut env);

        assert!(
            metadata.fields.contains_key("MEMBER"),
            "Should extract MEMBER as enum member"
        );
        assert!(
            !metadata.methods.contains_key("helper"),
            "Methods should not be treated as enum members"
        );
    }

    #[test]
    fn test_extract_enum_members_ignores_private() {
        let source = "class Color:\n    _PRIVATE = 1\n    VALUE = 2";
        let fixture = ClassFixture::new(
            source,
            walker_class_fixture!("test_extract_enum_members_ignores_private"),
        );
        let symbol_table = SymbolTable::new();
        let mut env = crate::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let mut metadata = beacon_core::ClassMetadata::new("Color".to_string());
        extract_enum_members(&mut metadata, fixture.class_body(), "Color", &mut env);

        assert!(
            !metadata.fields.contains_key("_PRIVATE"),
            "Private members should be ignored"
        );
        assert!(metadata.fields.contains_key("VALUE"));
    }

    #[test]
    fn test_class_with_only_class_level_annotations() {
        let source = "class Data:\n    name: str\n    count: int";
        let fixture = ClassFixture::new(
            source,
            walker_class_fixture!("test_class_with_only_class_level_annotations"),
        );
        let symbol_table = SymbolTable::new();
        let mut env = crate::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let metadata = extract_class_metadata("Data", fixture.class_body(), &mut env);
        assert!(metadata.fields.contains_key("name"));
        assert!(metadata.fields.contains_key("count"));
    }

    #[test]
    fn test_class_with_both_class_and_instance_fields() {
        let source = "class Config:\n    version: int\n\n    def __init__(self):\n        self.enabled = False";
        let fixture = ClassFixture::new(
            source,
            walker_class_fixture!("test_class_with_both_class_and_instance_fields"),
        );
        let symbol_table = SymbolTable::new();
        let mut env = crate::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let metadata = extract_class_metadata("Config", fixture.class_body(), &mut env);
        assert!(metadata.fields.contains_key("version"));
        assert!(metadata.fields.contains_key("enabled"));
    }

    #[test]
    fn test_class_with_non_annotated_class_assignments() {
        let source = "class Service:\n    timeout = 30\n    retries = 3";
        let fixture = ClassFixture::new(
            source,
            walker_class_fixture!("test_class_with_non_annotated_class_assignments"),
        );
        let symbol_table = SymbolTable::new();
        let mut env = crate::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let metadata = extract_class_metadata("Service", fixture.class_body(), &mut env);
        assert!(metadata.fields.contains_key("timeout"));
        assert!(metadata.fields.contains_key("retries"));
    }

    #[test]
    fn test_class_level_fields_dont_conflict_with_nested_classes() {
        let source = "class Outer:\n    name: str\n\n    class Inner:\n        value: int";
        let fixture = ClassFixture::new(
            source,
            walker_class_fixture!("test_class_level_fields_dont_conflict_with_nested_classes"),
        );
        let symbol_table = SymbolTable::new();
        let mut env = crate::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let metadata = extract_class_metadata("Outer", fixture.class_body(), &mut env);
        assert!(metadata.fields.contains_key("name"));
        assert!(
            !metadata.fields.contains_key("Inner"),
            "Nested class definitions should not overwrite fields"
        );
    }
}

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

    // First pass: extract class-level fields
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

    // Second pass: extract methods
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

            let ret_type =
                return_type.as_ref().map(|ann| env.parse_annotation_or_any(ann)).unwrap_or_else(|| Type::Var(env.fresh_var()));

            let has_property = decorators.iter().any(|d| d == "property");
            let has_staticmethod = decorators.iter().any(|d| d == "staticmethod");
            let has_classmethod = decorators.iter().any(|d| d == "classmethod");

            if method_name == "__init__" {
                let method_type = Type::fun(params.clone(), ret_type);
                metadata.set_init_type(method_type);

                // Extract instance fields from __init__ body
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
pub fn extract_enum_members(metadata: &mut ClassMetadata, body: &[AstNode], class_name: &str, _env: &mut TypeEnvironment) {
    let enum_type = Type::Con(TypeCtor::Class(class_name.to_string()));

    for stmt in body {
        match stmt {
            AstNode::Assignment { target, .. } | AstNode::AnnotatedAssignment { target, .. } => {
                let target_str = target.target_to_string();
                // Only extract public (non-underscore) top-level assignments as enum members
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
    use beacon_parser::{LiteralValue, Parameter, SymbolTable};

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
        let symbol_table = SymbolTable::new();
        let mut env = crate::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let class_body = vec![
            AstNode::AnnotatedAssignment {
                target: Box::new(AstNode::Identifier { name: "x".to_string(), line: 1, col: 1, end_line: 1, end_col: 2 }),
                type_annotation: "int".to_string(),
                value: None,
                line: 2,
                col: 5,
                end_line: 2,
                end_col: 5,
            },
            AstNode::AnnotatedAssignment {
                target: Box::new(AstNode::Identifier { name: "y".to_string(), line: 1, col: 1, end_line: 1, end_col: 2 }),
                type_annotation: "str".to_string(),
                value: None,
                line: 3,
                col: 5,
                end_line: 3,
                end_col: 5,
            },
        ];

        let mut metadata = extract_class_metadata("Point", &class_body, &mut env);

        assert!(metadata.init_type.is_none());

        synthesize_dataclass_init(&mut metadata, &mut env);

        assert!(metadata.init_type.is_some(), "Dataclass should have synthesized __init__");

        if let Some(Type::Fun(params, ret_ty)) = &metadata.init_type {
            assert!(!params.is_empty(), "__init__ should have at least self parameter");
            assert!(matches!(ret_ty.as_ref(), Type::Con(TypeCtor::NoneType)), "__init__ should return None");
        } else {
            panic!("Expected function type for __init__");
        }
    }

    #[test]
    fn test_synthesize_dataclass_init_respects_explicit_init() {
        let symbol_table = SymbolTable::new();
        let mut env = crate::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let class_body = vec![
            AstNode::AnnotatedAssignment {
                target: Box::new(AstNode::Identifier { name: "x".to_string(), line: 1, col: 1, end_line: 1, end_col: 2 }),
                type_annotation: "int".to_string(),
                value: None,
                line: 2,
                col: 5,
                end_line: 2,
                end_col: 5,
            },
            AstNode::FunctionDef {
                name: "__init__".to_string(),
                args: vec![],
                body: vec![],
                docstring: None,
                return_type: None,
                decorators: vec![],
                is_async: false,
                line: 3,
                col: 5,
                end_line: 3,
                end_col: 5,
            },
        ];

        let mut metadata = extract_class_metadata("CustomPoint", &class_body, &mut env);

        assert!(metadata.init_type.is_some());

        let original_init = metadata.init_type.clone();
        synthesize_dataclass_init(&mut metadata, &mut env);
        assert_eq!(metadata.init_type, original_init, "Should not override explicit __init__");
    }

    #[test]
    fn test_extract_enum_members() {
        let symbol_table = SymbolTable::new();
        let mut env = crate::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let class_body = vec![
            AstNode::Assignment {
                target: Box::new(AstNode::Identifier { name: "RED".to_string(), line: 1, col: 1, end_line: 1, end_col: 4 }),
                value: Box::new(AstNode::Literal {
                    value: LiteralValue::Integer(1),
                    line: 2,
                    col: 11,
                    end_line: 2,
                    end_col: 12,
                }),
                line: 2,
                col: 5,
                end_line: 2,
                end_col: 5,
            },
            AstNode::Assignment {
                target: Box::new(AstNode::Identifier {
                    name: "GREEN".to_string(),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 6,
                }),
                value: Box::new(AstNode::Literal {
                    value: LiteralValue::Integer(2),
                    line: 3,
                    col: 13,
                    end_line: 3,
                    end_col: 14,
                }),
                line: 3,
                col: 5,
                end_line: 3,
                end_col: 5,
            },
            AstNode::Assignment {
                target: Box::new(AstNode::Identifier { name: "BLUE".to_string(), line: 1, col: 1, end_line: 1, end_col: 5 }),
                value: Box::new(AstNode::Literal {
                    value: LiteralValue::Integer(3),
                    line: 4,
                    col: 12,
                    end_line: 4,
                    end_col: 13,
                }),
                line: 4,
                col: 5,
                end_line: 4,
                end_col: 5,
            },
        ];

        let mut metadata = beacon_core::ClassMetadata::new("Color".to_string());
        extract_enum_members(&mut metadata, &class_body, "Color", &mut env);

        assert!(metadata.fields.contains_key("RED"), "Should extract RED as enum member");
        assert!(metadata.fields.contains_key("GREEN"), "Should extract GREEN as enum member");
        assert!(metadata.fields.contains_key("BLUE"), "Should extract BLUE as enum member");

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
        let symbol_table = SymbolTable::new();
        let mut env = crate::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let class_body = vec![
            AstNode::Assignment {
                target: Box::new(AstNode::Identifier { name: "MEMBER".to_string(), line: 1, col: 1, end_line: 1, end_col: 7 }),
                value: Box::new(AstNode::Literal {
                    value: LiteralValue::Integer(1),
                    line: 2,
                    col: 14,
                    end_line: 2,
                    end_col: 14,
                }),
                line: 2,
                col: 5,
                end_line: 2,
                end_col: 5,
            },
            AstNode::FunctionDef {
                name: "some_method".to_string(),
                args: vec![],
                body: vec![],
                docstring: None,
                return_type: None,
                decorators: vec![],
                is_async: false,
                line: 3,
                col: 5,
                end_line: 3,
                end_col: 5,
            },
        ];

        let mut metadata = beacon_core::ClassMetadata::new("MyEnum".to_string());
        extract_enum_members(&mut metadata, &class_body, "MyEnum", &mut env);

        assert!(metadata.fields.contains_key("MEMBER"), "Should extract MEMBER as enum member");
        assert_eq!(metadata.fields.len(), 1, "Should only extract assignment, not method");
    }

    #[test]
    fn test_extract_enum_members_ignores_private() {
        let symbol_table = SymbolTable::new();
        let mut env = crate::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let class_body = vec![
            AstNode::Assignment {
                target: Box::new(AstNode::Identifier {
                    name: "PUBLIC".to_string(),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 7,
                }),
                value: Box::new(AstNode::Literal {
                    value: LiteralValue::Integer(1),
                    line: 2,
                    col: 14,
                    end_line: 2,
                    end_col: 15,
                }),
                line: 2,
                col: 5,
                end_line: 2,
                end_col: 5,
            },
            AstNode::Assignment {
                target: Box::new(AstNode::Identifier {
                    name: "_private".to_string(),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 9,
                }),
                value: Box::new(AstNode::Literal {
                    value: LiteralValue::Integer(2),
                    line: 3,
                    col: 16,
                    end_line: 3,
                    end_col: 17,
                }),
                line: 3,
                col: 5,
                end_line: 3,
                end_col: 5,
            },
        ];

        let mut metadata = beacon_core::ClassMetadata::new("MyEnum".to_string());
        extract_enum_members(&mut metadata, &class_body, "MyEnum", &mut env);

        assert!(metadata.fields.contains_key("PUBLIC"), "Should extract public member");
        assert!(!metadata.fields.contains_key("_private"), "Should ignore private members");
    }

    #[test]
    fn test_class_with_only_class_level_annotations() {
        let symbol_table = SymbolTable::new();
        let mut env = crate::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let class_body = vec![
            AstNode::AnnotatedAssignment {
                target: Box::new(AstNode::Identifier { name: "x".to_string(), line: 1, col: 1, end_line: 1, end_col: 2 }),
                type_annotation: "int".to_string(),
                value: None,
                line: 2,
                col: 5,
                end_line: 2,
                end_col: 5,
            },
            AstNode::AnnotatedAssignment {
                target: Box::new(AstNode::Identifier { name: "y".to_string(), line: 1, col: 1, end_line: 1, end_col: 2 }),
                type_annotation: "str".to_string(),
                value: Some(Box::new(AstNode::Literal {
                    value: LiteralValue::String { value: "default".to_string(), prefix: "".to_string() },
                    line: 3,
                    end_line: 3,
                    col: 13,
                    end_col: 13,
                })),
                line: 3,
                col: 5,
                end_line: 3,
                end_col: 5,
            },
        ];

        let metadata = extract_class_metadata("Point", &class_body, &mut env);

        assert!(metadata.fields.contains_key("x"), "Should extract class-level field x");
        assert!(metadata.fields.contains_key("y"), "Should extract class-level field y");

        if let Some(x_type) = metadata.fields.get("x") {
            assert!(matches!(x_type, Type::Con(TypeCtor::Int)), "Field x should have type int, got {x_type:?}");
        }

        if let Some(y_type) = metadata.fields.get("y") {
            assert!(matches!(y_type, Type::Con(TypeCtor::String)), "Field y should have type str, got {y_type:?}");
        }
    }

    #[test]
    fn test_class_with_both_class_and_instance_fields() {
        let symbol_table = SymbolTable::new();
        let mut env = crate::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let class_body = vec![
            AstNode::AnnotatedAssignment {
                target: Box::new(AstNode::Identifier {
                    name: "class_var".to_string(),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 10,
                }),
                type_annotation: "int".to_string(),
                value: Some(Box::new(AstNode::Literal {
                    value: LiteralValue::Integer(0),
                    line: 2,
                    end_line: 2,
                    col: 21,
                    end_col: 21,
                })),
                line: 2,
                col: 5,
                end_line: 2,
                end_col: 5,
            },
            AstNode::FunctionDef {
                name: "__init__".to_string(),
                args: vec![Parameter {
                    name: "self".to_string(),
                    line: 4,
                    col: 14,
                    end_line: 4,
                    end_col: 18,
                    type_annotation: None,
                    default_value: None,
                }],
                body: vec![AstNode::AnnotatedAssignment {
                    target: Box::new(AstNode::Identifier {
                        name: "self.instance_var".to_string(),
                        line: 1,
                        col: 1,
                        end_line: 1,
                        end_col: 18,
                    }),
                    type_annotation: "str".to_string(),
                    value: Some(Box::new(AstNode::Literal {
                        value: LiteralValue::String { value: "hello".to_string(), prefix: "".to_string() },
                        line: 5,
                        end_line: 5,
                        col: 33,
                        end_col: 33,
                    })),
                    line: 5,
                    end_line: 5,
                    col: 9,
                    end_col: 9,
                }],
                docstring: None,
                return_type: None,
                decorators: vec![],
                is_async: false,
                line: 4,
                col: 5,
                end_line: 4,
                end_col: 5,
            },
        ];

        let metadata = extract_class_metadata("MyClass", &class_body, &mut env);

        assert!(metadata.fields.contains_key("class_var"), "Should extract class-level field class_var");
        assert!(metadata.fields.contains_key("instance_var"), "Should extract instance field instance_var from __init__");

        if let Some(class_var_type) = metadata.fields.get("class_var") {
            assert!(
                matches!(class_var_type, Type::Con(TypeCtor::Int)),
                "class_var should have type int, got {class_var_type:?}"
            );
        }

        if let Some(instance_var_type) = metadata.fields.get("instance_var") {
            assert!(
                matches!(instance_var_type, Type::Con(TypeCtor::String)),
                "instance_var should have type str, got {instance_var_type:?}"
            );
        }
    }

    #[test]
    fn test_class_with_non_annotated_class_assignments() {
        let symbol_table = SymbolTable::new();
        let mut env = crate::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let class_body = vec![
            AstNode::Assignment {
                target: Box::new(AstNode::Identifier {
                    name: "counter".to_string(),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 8,
                }),
                value: Box::new(AstNode::Literal {
                    value: LiteralValue::Integer(0),
                    line: 2,
                    col: 17,
                    end_line: 2,
                    end_col: 18,
                }),
                line: 2,
                col: 5,
                end_line: 2,
                end_col: 5,
            },
            AstNode::Assignment {
                target: Box::new(AstNode::Identifier { name: "name".to_string(), line: 1, col: 1, end_line: 1, end_col: 5 }),
                value: Box::new(AstNode::Literal {
                    value: LiteralValue::String { value: "default".to_string(), prefix: "".to_string() },
                    line: 3,
                    end_line: 3,
                    col: 14,
                    end_col: 14,
                }),
                line: 3,
                col: 5,
                end_line: 3,
                end_col: 5,
            },
        ];

        let metadata = extract_class_metadata("Config", &class_body, &mut env);

        assert!(metadata.fields.contains_key("counter"), "Should extract non-annotated class-level field counter");
        assert!(metadata.fields.contains_key("name"), "Should extract non-annotated class-level field name");

        if let Some(counter_type) = metadata.fields.get("counter") {
            assert!(
                matches!(counter_type, Type::Var(_)),
                "Non-annotated field should get fresh type variable, got {counter_type:?}"
            );
        }
    }

    #[test]
    fn test_class_level_fields_dont_conflict_with_nested_classes() {
        let symbol_table = SymbolTable::new();
        let mut env = crate::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let class_body = vec![
            AstNode::AnnotatedAssignment {
                target: Box::new(AstNode::Identifier {
                    name: "outer_field".to_string(),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 12,
                }),
                type_annotation: "int".to_string(),
                value: None,
                line: 2,
                col: 5,
                end_line: 2,
                end_col: 5,
            },
            AstNode::ClassDef {
                name: "Inner".to_string(),
                bases: vec![],
                metaclass: None,
                body: vec![AstNode::Assignment {
                    target: Box::new(AstNode::Identifier {
                        name: "inner_field".to_string(),
                        line: 1,
                        col: 1,
                        end_line: 1,
                        end_col: 12,
                    }),
                    value: Box::new(AstNode::Literal {
                        value: LiteralValue::Integer(1),
                        line: 4,
                        col: 21,
                        end_line: 4,
                        end_col: 22,
                    }),
                    line: 4,
                    end_line: 4,
                    col: 9,
                    end_col: 9,
                }],
                docstring: None,
                decorators: vec![],
                line: 3,
                col: 5,
                end_line: 3,
                end_col: 5,
            },
        ];

        let metadata = extract_class_metadata("Outer", &class_body, &mut env);

        assert!(metadata.fields.contains_key("outer_field"), "Should extract outer_field from outer class");
        assert!(!metadata.fields.contains_key("inner_field"), "Should NOT extract inner_field from nested class");
    }
}

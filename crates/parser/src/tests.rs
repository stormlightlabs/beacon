use crate::*;

#[test]
fn test_parser_creation() {
    let parser = PythonParser::new();
    assert!(parser.is_ok());
}

#[test]
fn test_simple_function_parse() {
    let mut parser = PythonParser::new().unwrap();
    let source = "def hello(name):\n    return f'Hello {name}'";
    let parsed = parser.parse(source).unwrap();
    assert!(!parsed.tree.root_node().has_error());
}

#[test]
fn test_function_to_ast() {
    let mut parser = PythonParser::new().unwrap();
    let source = "def add(x, y):\n    return x + y";
    let parsed = parser.parse(source).unwrap();
    let ast = parser.to_ast(&parsed).unwrap();

    match ast {
        AstNode::Module { body, .. } => {
            assert_eq!(body.len(), 1);
            match &body[0] {
                AstNode::FunctionDef { name, args, .. } => {
                    assert_eq!(name, "add");
                    assert_eq!(args.len(), 2);
                    assert_eq!(args[0].name, "x");
                    assert_eq!(args[1].name, "y");
                }
                _ => panic!("Expected function definition"),
            }
        }
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_assignment_to_ast() {
    let mut parser = PythonParser::new().unwrap();
    let source = "x = 42";
    let parsed = parser.parse(source).unwrap();
    let ast = parser.to_ast(&parsed).unwrap();

    match ast {
        AstNode::Module { body, .. } => {
            assert_eq!(body.len(), 1);
            match &body[0] {
                AstNode::Assignment { target, value, .. } => {
                    match target.as_ref() {
                        AstNode::Identifier { name, .. } => assert_eq!(name, "x"),
                        _ => panic!("Expected identifier target"),
                    }
                    match value.as_ref() {
                        AstNode::Literal { value: LiteralValue::Integer(42), .. } => {}
                        _ => panic!("Expected integer literal 42"),
                    }
                }
                _ => panic!("Expected assignment"),
            }
        }
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_call_to_ast() {
    let mut parser = PythonParser::new().unwrap();
    let source = "print('hello')";
    let parsed = parser.parse(source).unwrap();
    let ast = parser.to_ast(&parsed).unwrap();

    match ast {
        AstNode::Module { body, .. } => {
            assert_eq!(body.len(), 1);
            match &body[0] {
                AstNode::Call { function, args, .. } => {
                    assert!(matches!(**function, AstNode::Identifier { name: ref n, .. } if n == "print"));
                    assert_eq!(args.len(), 1);
                    match &args[0] {
                        AstNode::Literal { value: LiteralValue::String { value: s, .. }, .. } => {
                            assert_eq!(s, "hello");
                        }
                        _ => panic!("Expected string literal"),
                    }
                }
                _ => panic!("Expected call expression"),
            }
        }
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_call_with_keyword_args() {
    let mut parser = PythonParser::new().unwrap();
    let source = "Point(x=5, y=10)";
    let parsed = parser.parse(source).unwrap();
    let ast = parser.to_ast(&parsed).unwrap();

    match ast {
        AstNode::Module { body, .. } => {
            assert_eq!(body.len(), 1);
            match &body[0] {
                AstNode::Call { function, args, keywords, .. } => {
                    assert!(matches!(**function, AstNode::Identifier { name: ref n, .. } if n == "Point"));
                    assert_eq!(args.len(), 0, "Should have no positional args");
                    assert_eq!(keywords.len(), 2, "Should have 2 keyword args");
                    assert_eq!(keywords[0].0, "x");
                    match &keywords[0].1 {
                        AstNode::Literal { value: LiteralValue::Integer(5), .. } => {}
                        _ => panic!("Expected integer literal 5 for x"),
                    }

                    assert_eq!(keywords[1].0, "y");
                    match &keywords[1].1 {
                        AstNode::Literal { value: LiteralValue::Integer(10), .. } => {}
                        _ => panic!("Expected integer literal 10 for y"),
                    }
                }
                _ => panic!("Expected call expression"),
            }
        }
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_call_with_mixed_args() {
    let mut parser = PythonParser::new().unwrap();
    let source = "func(1, 2, key=3)";
    let parsed = parser.parse(source).unwrap();
    let ast = parser.to_ast(&parsed).unwrap();

    match ast {
        AstNode::Module { body, .. } => {
            assert_eq!(body.len(), 1);
            match &body[0] {
                AstNode::Call { function, args, keywords, .. } => {
                    assert!(matches!(**function, AstNode::Identifier { name: ref n, .. } if n == "func"));
                    assert_eq!(args.len(), 2, "Should have 2 positional args");
                    assert_eq!(keywords.len(), 1, "Should have 1 keyword arg");
                    assert_eq!(keywords[0].0, "key");
                    match &keywords[0].1 {
                        AstNode::Literal { value: LiteralValue::Integer(3), .. } => {}
                        _ => panic!("Expected integer literal 3 for key"),
                    }
                }
                _ => panic!("Expected call expression"),
            }
        }
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_chained_method_calls() {
    let mut parser = PythonParser::new().unwrap();
    let source = "result = obj.method1().method2()";
    let parsed = parser.parse(source).unwrap();
    let ast = parser.to_ast(&parsed).unwrap();

    match ast {
        AstNode::Module { body, .. } => {
            assert_eq!(body.len(), 1);
            match &body[0] {
                AstNode::Assignment { value, .. } => match &**value {
                    AstNode::Call { function, .. } => match &**function {
                        AstNode::Attribute { object, attribute, .. } => {
                            assert_eq!(attribute, "method2");
                            match &**object {
                                AstNode::Call { function: inner_func, .. } => match &**inner_func {
                                    AstNode::Attribute { object: inner_obj, attribute: inner_attr, .. } => {
                                        assert_eq!(inner_attr, "method1");
                                        assert!(
                                            matches!(**inner_obj, AstNode::Identifier { name: ref n, .. } if n == "obj")
                                        );
                                    }
                                    _ => panic!("Expected Attribute for method1"),
                                },
                                _ => panic!("Expected Call for method1()"),
                            }
                        }
                        _ => panic!("Expected Attribute for method2"),
                    },
                    _ => panic!("Expected Call for method2()"),
                },
                _ => panic!("Expected Assignment"),
            }
        }
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_call_with_lambda_arguments() {
    let mut parser = PythonParser::new().unwrap();
    let source = "compose(lambda x: x + 1, lambda y: y * 2)";
    let parsed = parser.parse(source).unwrap();
    let ast = parser.to_ast(&parsed).unwrap();

    match ast {
        AstNode::Module { body, .. } => {
            assert_eq!(body.len(), 1);
            match &body[0] {
                AstNode::Call { function, args, .. } => {
                    assert!(matches!(**function, AstNode::Identifier { name: ref n, .. } if n == "compose"));
                    assert_eq!(args.len(), 2);
                    assert!(matches!(args[0], AstNode::Lambda { .. }));
                    assert!(matches!(args[1], AstNode::Lambda { .. }));
                }
                _ => panic!("Expected Call"),
            }
        }
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_nested_function_calls() {
    let mut parser = PythonParser::new().unwrap();
    let source = "outer(inner(value))";
    let parsed = parser.parse(source).unwrap();
    let ast = parser.to_ast(&parsed).unwrap();

    match ast {
        AstNode::Module { body, .. } => {
            assert_eq!(body.len(), 1);
            match &body[0] {
                AstNode::Call { function, args, .. } => {
                    assert!(matches!(**function, AstNode::Identifier { name: ref n, .. } if n == "outer"));
                    assert_eq!(args.len(), 1);

                    match &args[0] {
                        AstNode::Call { function: inner_func, args: inner_args, .. } => {
                            assert!(matches!(**inner_func, AstNode::Identifier { name: ref n, .. } if n == "inner"));
                            assert_eq!(inner_args.len(), 1);
                            assert!(matches!(inner_args[0], AstNode::Identifier { name: ref n, .. } if n == "value"));
                        }
                        _ => panic!("Expected nested Call"),
                    }
                }
                _ => panic!("Expected Call"),
            }
        }
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_class_to_ast() {
    let mut parser = PythonParser::new().unwrap();
    let source = "class Person:\n    pass";
    let parsed = parser.parse(source).unwrap();
    let ast = parser.to_ast(&parsed).unwrap();

    match ast {
        AstNode::Module { body, .. } => {
            assert_eq!(body.len(), 1);
            match &body[0] {
                AstNode::ClassDef { name, .. } => {
                    assert_eq!(name, "Person");
                }
                _ => panic!("Expected class definition"),
            }
        }
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_complex_python_code() {
    let mut parser = PythonParser::new().unwrap();
    let source = r#"
class Calculator:
    def __init__(self):
        self.value = 0

    def add(self, x):
        self.value += x
        return self.value

def main():
    calc = Calculator()
    result = calc.add(5)
    print(f"Result: {result}")
    return result

if __name__ == "__main__":
    main()
"#;

    let parsed = parser.parse(source).unwrap();
    assert!(!parsed.tree.root_node().has_error());

    let ast = parser.to_ast(&parsed).unwrap();
    match ast {
        AstNode::Module { body, .. } => assert!(body.len() >= 3),
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_literal_values() {
    let mut parser = PythonParser::new().unwrap();

    let test_cases = vec![
        ("x = 42", LiteralValue::Integer(42)),
        ("x = True", LiteralValue::Boolean(true)),
        ("x = False", LiteralValue::Boolean(false)),
        ("x = None", LiteralValue::None),
        (
            "x = 'hello'",
            LiteralValue::String { value: "hello".to_string(), prefix: String::new() },
        ),
    ];

    for (source, expected) in test_cases {
        let parsed = parser.parse(source).unwrap();
        let ast = parser.to_ast(&parsed).unwrap();

        match ast {
            AstNode::Module { body, .. } => match &body[0] {
                AstNode::Assignment { value, .. } => match value.as_ref() {
                    AstNode::Literal { value, .. } => {
                        assert_eq!(value, &expected, "Failed for source: {source}")
                    }
                    _ => panic!("Expected literal in assignment: {source}"),
                },
                _ => panic!("Expected assignment: {source}"),
            },
            _ => panic!("Expected module: {source}"),
        }
    }
}

#[test]
fn test_function_with_multiple_args() {
    let mut parser = PythonParser::new().unwrap();
    let source = "def multiply(a, b, c):\n    return a * b * c";
    let parsed = parser.parse(source).unwrap();
    let ast = parser.to_ast(&parsed).unwrap();

    match ast {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::FunctionDef { args, .. } => {
                assert_eq!(args.len(), 3);
                assert_eq!(args[0].name, "a");
                assert_eq!(args[1].name, "b");
                assert_eq!(args[2].name, "c");
            }
            _ => panic!("Expected function definition"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_nested_calls() {
    let mut parser = PythonParser::new().unwrap();
    let source = "result = max(min(5, 10), 3)";
    let parsed = parser.parse(source).unwrap();

    assert!(!parsed.tree.root_node().has_error());

    match parser.to_ast(&parsed).unwrap() {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::Assignment { target, .. } => match target.as_ref() {
                AstNode::Identifier { name, .. } => assert_eq!(name, "result"),
                _ => panic!("Expected identifier target"),
            },
            _ => panic!("Expected assignment"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_error_handling() {
    let mut parser = PythonParser::new().unwrap();
    let source = "def incomplete_func(";
    let parsed = parser.parse(source).unwrap();

    assert!(parsed.tree.root_node().child_count() > 0);
}

#[test]
fn test_debug_tree() {
    let mut parser = PythonParser::new().unwrap();
    let source = "x = 42";

    let parsed = parser.parse(source).unwrap();
    let debug_output = parser.debug_tree(&parsed);

    assert!(!debug_output.is_empty());
    assert!(debug_output.contains("module"));
    assert!(debug_output.contains("assignment"));
}

#[test]
fn test_empty_source() {
    let mut parser = PythonParser::new().unwrap();
    let source = "";

    let parsed = parser.parse(source).unwrap();
    let ast = parser.to_ast(&parsed).unwrap();

    match ast {
        AstNode::Module { body, .. } => {
            assert_eq!(body.len(), 0);
        }
        _ => panic!("Expected empty module"),
    }
}

#[test]
fn test_whitespace_and_comments() {
    let mut parser = PythonParser::new().unwrap();
    let source = r#"
# This is a comment
x = 42  # Another comment

# Function definition
def hello():
    pass
"#;

    let parsed = parser.parse(source).unwrap();
    assert!(!parsed.tree.root_node().has_error());
    match parser.to_ast(&parsed).unwrap() {
        AstNode::Module { body, .. } => assert!(body.len() >= 2),
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_name_resolution_integration() {
    let mut parser = PythonParser::new().unwrap();
    let source = r#"
def factorial(n):
    if n <= 1:
        return 1
    return n * factorial(n - 1)

result = factorial(5)
"#;

    let (ast, symbol_table) = parser.parse_and_resolve(source).unwrap();

    match ast {
        AstNode::Module { body, .. } => assert_eq!(body.len(), 2),
        _ => panic!("Expected module"),
    }

    let root_scope = symbol_table.root_scope;
    let factorial_symbol = symbol_table.lookup_symbol("factorial", root_scope);
    assert!(factorial_symbol.is_some());
    assert_eq!(factorial_symbol.unwrap().kind, SymbolKind::Function);

    let result_symbol = symbol_table.lookup_symbol("result", root_scope);
    assert!(result_symbol.is_some());
    assert_eq!(result_symbol.unwrap().kind, SymbolKind::Variable);

    let func_scope_id = symbol_table.scopes.get(&root_scope).unwrap().children[0];
    let param_symbol = symbol_table.lookup_symbol("n", func_scope_id);
    assert!(param_symbol.is_some());
    assert_eq!(param_symbol.unwrap().kind, SymbolKind::Parameter);
}

#[test]
fn test_nested_scope_resolution() {
    let mut parser = PythonParser::new().unwrap();
    let source = r#"
global_var = "hello"

class MyClass:
    class_var = 42

    def method(self, param):
        local_var = global_var + str(self.class_var)
        return local_var
"#;

    let (_ast, symbol_table) = parser.parse_and_resolve(source).unwrap();

    let root_scope = symbol_table.root_scope;

    assert!(symbol_table.lookup_symbol("global_var", root_scope).is_some());
    assert!(symbol_table.lookup_symbol("MyClass", root_scope).is_some());

    let root_children = &symbol_table.scopes.get(&root_scope).unwrap().children;
    assert!(!root_children.is_empty());

    let class_scope = root_children[0];
    assert!(symbol_table.lookup_symbol("class_var", class_scope).is_some());
    assert!(symbol_table.lookup_symbol("method", class_scope).is_some());
}

#[test]
fn test_import_statement() {
    let mut parser = PythonParser::new().unwrap();
    let source = "import os";

    let parsed = parser.parse(source).unwrap();
    let ast = parser.to_ast(&parsed).unwrap();

    match ast {
        AstNode::Module { body, .. } => {
            assert_eq!(body.len(), 1);
            match &body[0] {
                AstNode::Import { module, alias, .. } => {
                    assert_eq!(module, "os");
                    assert_eq!(*alias, None);
                }
                _ => panic!("Expected Import node"),
            }
        }
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_import_with_alias() {
    let mut parser = PythonParser::new().unwrap();
    let source = "import numpy as np";

    let parsed = parser.parse(source).unwrap();
    let ast = parser.to_ast(&parsed).unwrap();

    match ast {
        AstNode::Module { body, .. } => {
            assert_eq!(body.len(), 1);
            match &body[0] {
                AstNode::Import { module, alias, .. } => {
                    assert_eq!(module, "numpy");
                    assert_eq!(*alias, Some("np".to_string()));
                }
                _ => panic!("Expected Import node"),
            }
        }
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_import_from() {
    let mut parser = PythonParser::new().unwrap();
    let source = "from math import sqrt, pi";
    let parsed = parser.parse(source).unwrap();
    let ast = parser.to_ast(&parsed).unwrap();

    match ast {
        AstNode::Module { body, .. } => {
            assert_eq!(body.len(), 1);
            match &body[0] {
                AstNode::ImportFrom { module, names, .. } => {
                    assert_eq!(module, "math");
                    assert!(names.iter().any(|n| n.name == "sqrt"));
                    assert!(names.iter().any(|n| n.name == "pi"));
                }
                _ => panic!("Expected ImportFrom node"),
            }
        }
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_attribute_access() {
    let mut parser = PythonParser::new().unwrap();
    let source = "x = os.path";
    let parsed = parser.parse(source).unwrap();
    let ast = parser.to_ast(&parsed).unwrap();

    match ast {
        AstNode::Module { body, .. } => {
            assert_eq!(body.len(), 1);
            match &body[0] {
                AstNode::Assignment { value, .. } => match value.as_ref() {
                    AstNode::Attribute { object, attribute, .. } => {
                        match object.as_ref() {
                            AstNode::Identifier { name, .. } => {
                                assert_eq!(name, "os");
                            }
                            _ => panic!("Expected Identifier for object"),
                        }
                        assert_eq!(attribute, "path");
                    }
                    _ => panic!("Expected Attribute node"),
                },
                _ => panic!("Expected Assignment"),
            }
        }
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_nested_attribute_access() {
    let mut parser = PythonParser::new().unwrap();
    let source = "result = os.path.join";
    let parsed = parser.parse(source).unwrap();

    match parser.to_ast(&parsed).unwrap() {
        AstNode::Module { body, .. } => {
            assert_eq!(body.len(), 1);
            match &body[0] {
                AstNode::Assignment { value, .. } => match value.as_ref() {
                    AstNode::Attribute { attribute, .. } => {
                        assert_eq!(attribute, "join");
                    }
                    _ => panic!("Expected Attribute node"),
                },
                _ => panic!("Expected Assignment"),
            }
        }
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_import_resolution() {
    let mut parser = PythonParser::new().unwrap();
    let source = r#"
import os
import sys as system
from math import sqrt

x = os
y = system
z = sqrt(16)
"#;

    let (ast, symbol_table) = parser.parse_and_resolve(source).unwrap();

    match ast {
        AstNode::Module { body, .. } => assert!(body.len() >= 6),
        _ => panic!("Expected module"),
    }

    let root_scope = symbol_table.root_scope;

    let os_symbol = symbol_table.lookup_symbol("os", root_scope);
    assert!(os_symbol.is_some());
    assert_eq!(os_symbol.unwrap().kind, SymbolKind::Import);

    let sys_symbol = symbol_table.lookup_symbol("system", root_scope);
    assert!(sys_symbol.is_some());
    assert_eq!(sys_symbol.unwrap().kind, SymbolKind::Import);

    let sqrt_symbol = symbol_table.lookup_symbol("sqrt", root_scope);
    assert!(sqrt_symbol.is_some());
    assert_eq!(sqrt_symbol.unwrap().kind, SymbolKind::Import);
}

#[test]
fn test_function_docstring_extraction() {
    let mut parser = PythonParser::new().unwrap();
    let source = r#"def greet(name):
    """Say hello to someone."""
    return f"Hello {name}""#;

    let parsed = parser.parse(source).unwrap();
    let ast = parser.to_ast(&parsed).unwrap();

    match ast {
        AstNode::Module { body, .. } => {
            assert_eq!(body.len(), 1);
            match &body[0] {
                AstNode::FunctionDef { name, docstring, .. } => {
                    assert_eq!(name, "greet");
                    assert!(docstring.is_some());
                    assert_eq!(docstring.as_ref().unwrap(), "Say hello to someone.");
                }
                _ => panic!("Expected function definition"),
            }
        }
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_class_docstring_extraction() {
    let mut parser = PythonParser::new().unwrap();
    let source = r#"class Person:
    """A person class."""
    pass"#;

    let parsed = parser.parse(source).unwrap();
    let ast = parser.to_ast(&parsed).unwrap();

    match ast {
        AstNode::Module { body, .. } => {
            assert_eq!(body.len(), 1);
            match &body[0] {
                AstNode::ClassDef { name, docstring, .. } => {
                    assert_eq!(name, "Person");
                    assert!(docstring.is_some());
                    assert_eq!(docstring.as_ref().unwrap(), "A person class.");
                }
                _ => panic!("Expected class definition"),
            }
        }
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_module_docstring_extraction() {
    let mut parser = PythonParser::new().unwrap();
    let source = r#""""This is a module docstring."""

def foo():
    pass"#;

    let parsed = parser.parse(source).unwrap();
    let ast = parser.to_ast(&parsed).unwrap();

    match ast {
        AstNode::Module { docstring, .. } => {
            assert!(docstring.is_some());
            assert_eq!(docstring.as_ref().unwrap(), "This is a module docstring.");
        }
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_multiline_docstring_extraction() {
    let mut parser = PythonParser::new().unwrap();
    let source = r#"def calculate(x, y):
    """Calculate something.

    This function does a calculation.
    It takes two parameters.
    """
    return x + y"#;

    let parsed = parser.parse(source).unwrap();
    let ast = parser.to_ast(&parsed).unwrap();

    match ast {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::FunctionDef { docstring, .. } => {
                assert!(docstring.is_some());
                let doc = docstring.as_ref().unwrap();
                assert!(doc.contains("Calculate something"));
                assert!(doc.contains("This function does a calculation"));
            }
            _ => panic!("Expected function definition"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_no_docstring() {
    let mut parser = PythonParser::new().unwrap();
    let source = "def foo():\n    pass";

    let parsed = parser.parse(source).unwrap();
    let ast = parser.to_ast(&parsed).unwrap();

    match ast {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::FunctionDef { docstring, .. } => {
                assert!(docstring.is_none());
            }
            _ => panic!("Expected function definition"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_future_import_parsing() {
    let mut parser = PythonParser::new().unwrap();
    let source = "from __future__ import annotations\n";

    let parsed = parser.parse(source).unwrap();
    let ast = parser.to_ast(&parsed).unwrap();

    match ast {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::ImportFrom { module, names, .. } => {
                assert_eq!(module, "__future__");
                assert_eq!(names.len(), 1);
                assert_eq!(names[0].name, "annotations");
            }
            _ => panic!("Expected ImportFrom node"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_class_bases_with_generic() {
    let mut parser = PythonParser::new().unwrap();
    let source = "class Foo(Generic[T]):\n    pass\n";

    let parsed = parser.parse(source).unwrap();
    let ast = parser.to_ast(&parsed).unwrap();

    match ast {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::ClassDef { bases, .. } => {
                assert_eq!(bases, &["Generic[T]".to_string()]);
            }
            _ => panic!("Expected ClassDef"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_fstring_interpolation_preserved() {
    let mut parser = PythonParser::new().unwrap();
    let source = r#"
def describe(self, item):
    return f"{self.value}: {item}"
"#;

    let parsed = parser.parse(source).unwrap();
    let ast = parser.to_ast(&parsed).unwrap();

    match ast {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::FunctionDef { body, .. } => match &body[0] {
                AstNode::Return { value: Some(literal), .. } => match literal.as_ref() {
                    AstNode::Literal { value: LiteralValue::String { value, prefix }, .. } => {
                        assert_eq!(prefix, "f");
                        assert_eq!(value, r#"{self.value}: {item}"#);
                    }
                    _ => panic!("Expected literal string"),
                },
                _ => panic!("Expected return statement"),
            },
            _ => panic!("Expected function definition"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_typed_parameters() {
    let mut parser = PythonParser::new().unwrap();
    let source = r#"
from typing import List

class MyClass:
    def __init__(self, filters: List[str], count: int = 0):
        self.filters = filters
        self.count = count

def process(x, y: int, z=5, w: str = "default"):
    return x + y + z + len(w)
"#;
    let parsed = parser.parse(source).unwrap();
    let ast = parser.to_ast(&parsed).unwrap();

    match ast {
        AstNode::Module { body, .. } => {
            match &body[1] {
                AstNode::ClassDef { body: class_body, .. } => match &class_body[0] {
                    AstNode::FunctionDef { name, args, .. } => {
                        assert_eq!(name, "__init__");
                        assert_eq!(args.len(), 3);
                        assert_eq!(args[0].name, "self");
                        assert_eq!(args[0].type_annotation, None);
                        assert_eq!(args[1].name, "filters");
                        assert!(args[1].type_annotation.is_some());
                        assert_eq!(args[2].name, "count");
                        assert!(args[2].type_annotation.is_some());
                    }
                    _ => panic!("Expected function definition"),
                },
                _ => panic!("Expected class definition"),
            }

            match &body[2] {
                AstNode::FunctionDef { name, args, .. } => {
                    assert_eq!(name, "process");
                    assert_eq!(args.len(), 4);
                    assert_eq!(args[0].name, "x");
                    assert_eq!(args[0].type_annotation, None);
                    assert_eq!(args[1].name, "y");
                    assert!(args[1].type_annotation.is_some());
                    assert_eq!(args[2].name, "z");
                    assert_eq!(args[3].name, "w");
                    assert!(args[3].type_annotation.is_some());
                }
                _ => panic!("Expected function definition"),
            }
        }
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_function_return_type() {
    let mut parser = PythonParser::new().unwrap();
    let source = "def add(x: int, y: int) -> int:\n    return x + y";
    let parsed = parser.parse(source).unwrap();
    let ast = parser.to_ast(&parsed).unwrap();

    match ast {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::FunctionDef { name, return_type, args, .. } => {
                assert_eq!(name, "add");
                assert!(return_type.is_some(), "Expected return type to be captured");
                assert!(return_type.as_ref().unwrap().contains("int"));
                assert_eq!(args.len(), 2);
                assert!(args[0].type_annotation.is_some());
                assert!(args[1].type_annotation.is_some());
            }
            _ => panic!("Expected function definition"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_annotated_assignment_debug() {
    let mut parser = PythonParser::new().unwrap();
    let source = "x: int = 5";
    let parsed = parser.parse(source).unwrap();
    let debug_output = parser.debug_tree(&parsed);
    println!("{debug_output}");
    assert!(!debug_output.is_empty());
}

#[test]
fn test_decorator_debug() {
    let mut parser = PythonParser::new().unwrap();
    let source = "@property\ndef foo():\n    pass";
    let parsed = parser.parse(source).unwrap();
    let debug_output = parser.debug_tree(&parsed);
    println!("{debug_output}");
    assert!(!debug_output.is_empty());
}

#[test]
fn test_decorator_extraction() {
    let mut parser = PythonParser::new().unwrap();
    let source = "@property\ndef foo():\n    pass";
    let parsed = parser.parse(source).unwrap();
    let ast = parser.to_ast(&parsed).unwrap();

    match ast {
        AstNode::Module { body, .. } => {
            assert_eq!(body.len(), 1);
            match &body[0] {
                AstNode::FunctionDef { name, decorators, .. } => {
                    assert_eq!(name, "foo");
                    assert_eq!(decorators.len(), 1);
                    assert_eq!(decorators[0], "property");
                }
                _ => panic!("Expected FunctionDef, got {:?}", &body[0]),
            }
        }
        _ => panic!("Expected Module"),
    }
}

#[test]
fn test_multiple_decorators() {
    let mut parser = PythonParser::new().unwrap();
    let source = "@staticmethod\n@cached\ndef bar():\n    pass";
    let parsed = parser.parse(source).unwrap();
    let ast = parser.to_ast(&parsed).unwrap();

    match ast {
        AstNode::Module { body, .. } => {
            assert_eq!(body.len(), 1);
            match &body[0] {
                AstNode::FunctionDef { name, decorators, .. } => {
                    assert_eq!(name, "bar");
                    assert_eq!(decorators.len(), 2);
                    assert_eq!(decorators[0], "staticmethod");
                    assert_eq!(decorators[1], "cached");
                }
                _ => panic!("Expected FunctionDef"),
            }
        }
        _ => panic!("Expected Module"),
    }
}

#[test]
fn test_class_decorator() {
    let mut parser = PythonParser::new().unwrap();
    let source = "@dataclass\nclass Point:\n    pass";
    let parsed = parser.parse(source).unwrap();
    let ast = parser.to_ast(&parsed).unwrap();

    match ast {
        AstNode::Module { body, .. } => {
            assert_eq!(body.len(), 1);
            match &body[0] {
                AstNode::ClassDef { name, decorators, .. } => {
                    assert_eq!(name, "Point");
                    assert_eq!(decorators.len(), 1);
                    assert_eq!(decorators[0], "dataclass");
                }
                _ => panic!("Expected ClassDef"),
            }
        }
        _ => panic!("Expected Module"),
    }
}

#[test]
fn test_annotated_assignment() {
    let mut parser = PythonParser::new().unwrap();
    let source = r#"x: int = 5
y: str
count: int = 0"#;
    let parsed = parser.parse(source).unwrap();
    let ast = parser.to_ast(&parsed).unwrap();

    match ast {
        AstNode::Module { body, .. } => {
            match &body[0] {
                AstNode::AnnotatedAssignment { target, type_annotation, value, .. } => {
                    match target.as_ref() {
                        AstNode::Identifier { name, .. } => assert_eq!(name, "x"),
                        _ => panic!("Expected identifier target"),
                    }
                    assert!(type_annotation.contains("int"));
                    assert!(value.is_some());
                }
                _ => panic!("Expected annotated assignment, got {:?}", &body[0]),
            }

            match &body[1] {
                AstNode::AnnotatedAssignment { target, type_annotation, value, .. } => {
                    match target.as_ref() {
                        AstNode::Identifier { name, .. } => assert_eq!(name, "y"),
                        _ => panic!("Expected identifier target"),
                    }
                    assert!(type_annotation.contains("str"));
                    assert!(value.is_none());
                }
                _ => panic!("Expected annotated assignment, got {:?}", &body[1]),
            }
        }
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_default_parameter_simple() {
    let mut parser = PythonParser::new().unwrap();
    let source = "def foo(x=5): pass";
    let parsed = parser.parse(source).unwrap();
    let ast = parser.to_ast(&parsed).unwrap();

    match ast {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::FunctionDef { name, args, .. } => {
                assert_eq!(name, "foo");
                assert_eq!(args.len(), 1);
                assert_eq!(args[0].name, "x");
                assert_eq!(args[0].type_annotation, None);
                assert!(args[0].default_value.is_some(), "Expected default value");

                match args[0].default_value.as_ref().unwrap().as_ref() {
                    AstNode::Literal { value, .. } => match value {
                        LiteralValue::Integer(5) => {}
                        _ => panic!("Expected integer 5, got {value:?}"),
                    },
                    _ => panic!("Expected Literal node"),
                }
            }
            _ => panic!("Expected function definition"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_default_parameter_typed() {
    let mut parser = PythonParser::new().unwrap();
    let source = "def foo(x: int = 5): pass";
    let parsed = parser.parse(source).unwrap();
    let ast = parser.to_ast(&parsed).unwrap();

    match ast {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::FunctionDef { name, args, .. } => {
                assert_eq!(name, "foo");
                assert_eq!(args.len(), 1);
                assert_eq!(args[0].name, "x");
                assert!(args[0].type_annotation.is_some());
                assert!(args[0].type_annotation.as_ref().unwrap().contains("int"));
                assert!(args[0].default_value.is_some(), "Expected default value");

                match args[0].default_value.as_ref().unwrap().as_ref() {
                    AstNode::Literal { value, .. } => match value {
                        LiteralValue::Integer(5) => {}
                        _ => panic!("Expected integer 5"),
                    },
                    _ => panic!("Expected Literal node"),
                }
            }
            _ => panic!("Expected function definition"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_default_parameter_complex() {
    let mut parser = PythonParser::new().unwrap();
    let source = r#"def foo(x=None): pass"#;
    let parsed = parser.parse(source).unwrap();
    let ast = parser.to_ast(&parsed).unwrap();

    match ast {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::FunctionDef { args, .. } => {
                assert_eq!(args.len(), 1);
                assert!(args[0].default_value.is_some(), "Expected default value");

                match args[0].default_value.as_ref().unwrap().as_ref() {
                    AstNode::Literal { value, .. } => match value {
                        LiteralValue::None => {}
                        _ => panic!("Expected None"),
                    },
                    _ => panic!("Expected Literal node"),
                }
            }
            _ => panic!("Expected function definition"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_mixed_parameters() {
    let mut parser = PythonParser::new().unwrap();
    let source = "def foo(a, b=2, c: int = 3): pass";
    let parsed = parser.parse(source).unwrap();
    let ast = parser.to_ast(&parsed).unwrap();

    match ast {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::FunctionDef { name, args, .. } => {
                assert_eq!(name, "foo");
                assert_eq!(args.len(), 3);

                assert_eq!(args[0].name, "a");
                assert_eq!(args[0].type_annotation, None);
                assert_eq!(args[0].default_value, None);

                assert_eq!(args[1].name, "b");
                assert_eq!(args[1].type_annotation, None);
                assert!(args[1].default_value.is_some());

                assert_eq!(args[2].name, "c");
                assert!(args[2].type_annotation.is_some());
                assert!(args[2].default_value.is_some());
            }
            _ => panic!("Expected function definition"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_default_with_identifier() {
    let mut parser = PythonParser::new().unwrap();
    let source = "def foo(x=CONST): pass";
    let parsed = parser.parse(source).unwrap();
    let ast = parser.to_ast(&parsed).unwrap();

    match ast {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::FunctionDef { args, .. } => {
                assert_eq!(args.len(), 1);
                assert!(args[0].default_value.is_some(), "Expected default value");

                match args[0].default_value.as_ref().unwrap().as_ref() {
                    AstNode::Identifier { name, .. } => {
                        assert_eq!(name, "CONST");
                    }
                    node => panic!("Expected Identifier node, got {node:?}"),
                }
            }
            _ => panic!("Expected function definition"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_if_statement() {
    let mut parser = PythonParser::new().unwrap();
    let source = "if x > 0:\n    y = 1";
    let parsed = parser.parse(source).unwrap();
    let ast = parser.to_ast(&parsed).unwrap();

    match ast {
        AstNode::Module { body, .. } => {
            assert_eq!(body.len(), 1);
            match &body[0] {
                AstNode::If { test, body, .. } => {
                    assert!(matches!(test.as_ref(), AstNode::Compare { .. }));
                    assert_eq!(body.len(), 1);
                }
                _ => panic!("Expected If node"),
            }
        }
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_if_elif_else() {
    let mut parser = PythonParser::new().unwrap();
    let source = "if x > 0:\n    y = 1\nelif x < 0:\n    y = -1\nelse:\n    y = 0";
    let parsed = parser.parse(source).unwrap();
    match parser.to_ast(&parsed).unwrap() {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::If { elif_parts, else_body, .. } => {
                assert_eq!(elif_parts.len(), 1, "Should have one elif");
                assert!(else_body.is_some(), "Should have else body");
            }
            _ => panic!("Expected If node"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_if_multiple_elif_else() {
    let mut parser = PythonParser::new().unwrap();
    let source = "if x > 10:\n    y = 1\nelif x > 5:\n    y = 2\nelif x > 0:\n    y = 3\nelse:\n    y = 0";
    let parsed = parser.parse(source).unwrap();
    match parser.to_ast(&parsed).unwrap() {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::If { body: if_body, elif_parts, else_body, .. } => {
                assert_eq!(if_body.len(), 1, "If body should have 1 statement");
                assert_eq!(elif_parts.len(), 2, "Should have two elif clauses");
                assert_eq!(elif_parts[0].1.len(), 1, "First elif should have 1 statement");
                assert_eq!(elif_parts[1].1.len(), 1, "Second elif should have 1 statement");
                assert!(else_body.is_some(), "Should have else body");
                assert_eq!(
                    else_body.as_ref().unwrap().len(),
                    1,
                    "Else body should have 1 statement"
                );
            }
            _ => panic!("Expected If node"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_for_loop() {
    let mut parser = PythonParser::new().unwrap();
    let source = "for x in items:\n    print(x)";
    let parsed = parser.parse(source).unwrap();

    match parser.to_ast(&parsed).unwrap() {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::For { target, iter, body, .. } => {
                assert!(matches!(target.as_ref(), AstNode::Identifier { name, .. } if name == "x"));
                assert!(matches!(iter.as_ref(), AstNode::Identifier { .. }));
                assert_eq!(body.len(), 1);
            }
            _ => panic!("Expected For node"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_for_loop_with_else() {
    let mut parser = PythonParser::new().unwrap();
    let source = "for x in items:\n    print(x)\nelse:\n    print('done')";
    let parsed = parser.parse(source).unwrap();

    match parser.to_ast(&parsed).unwrap() {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::For { else_body, .. } => {
                assert!(else_body.is_some(), "Should have else body");
                let else_stmts = else_body.as_ref().unwrap();
                assert_eq!(else_stmts.len(), 1, "Else body should have 1 statement");
                assert!(
                    matches!(else_stmts[0], AstNode::Call { .. }),
                    "Else body should contain Call node"
                );
            }
            _ => panic!("Expected For node"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_for_loop_tuple_target() {
    let mut parser = PythonParser::new().unwrap();
    let source = "for x, y in pairs:\n    print(x, y)";
    let parsed = parser.parse(source).unwrap();

    match parser.to_ast(&parsed).unwrap() {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::For { target, iter, body, .. } => {
                match target.as_ref() {
                    AstNode::Tuple { elements, is_parenthesized, .. } => {
                        assert!(!is_parenthesized, "Tuple should not be parenthesized");
                        assert_eq!(elements.len(), 2);
                        assert!(matches!(&elements[0], AstNode::Identifier { name, .. } if name == "x"));
                        assert!(matches!(&elements[1], AstNode::Identifier { name, .. } if name == "y"));
                    }
                    _ => panic!("Expected Tuple target"),
                }
                assert!(matches!(iter.as_ref(), AstNode::Identifier { name, .. } if name == "pairs"));
                assert_eq!(body.len(), 1);
            }
            _ => panic!("Expected For node"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_for_loop_nested_tuple_target() {
    let mut parser = PythonParser::new().unwrap();
    let source = "for (a, b), c in nested:\n    print(a, b, c)";
    let parsed = parser.parse(source).unwrap();

    match parser.to_ast(&parsed).unwrap() {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::For { target, .. } => match target.as_ref() {
                AstNode::Tuple { elements, .. } => {
                    assert_eq!(elements.len(), 2);
                    assert!(matches!(&elements[0], AstNode::Tuple { is_parenthesized, .. } if *is_parenthesized));
                    assert!(matches!(&elements[1], AstNode::Identifier { name, .. } if name == "c"));
                }
                _ => panic!("Expected Tuple target"),
            },
            _ => panic!("Expected For node"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_for_loop_list_target() {
    let mut parser = PythonParser::new().unwrap();
    let source = "for [x, y, z] in items:\n    print(x, y, z)";
    let parsed = parser.parse(source).unwrap();

    match parser.to_ast(&parsed).unwrap() {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::For { target, .. } => match target.as_ref() {
                AstNode::List { elements, .. } => {
                    assert_eq!(elements.len(), 3);
                    assert!(matches!(&elements[0], AstNode::Identifier { name, .. } if name == "x"));
                    assert!(matches!(&elements[1], AstNode::Identifier { name, .. } if name == "y"));
                    assert!(matches!(&elements[2], AstNode::Identifier { name, .. } if name == "z"));
                }
                _ => panic!("Expected List target"),
            },
            _ => panic!("Expected For node"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_for_loop_target_display() {
    let mut parser = PythonParser::new().unwrap();

    let source = "for x in items:\n    pass";
    let parsed = parser.parse(source).unwrap();
    match parser.to_ast(&parsed).unwrap() {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::For { target, .. } => {
                assert_eq!(target.target_display(), "x");
            }
            _ => panic!("Expected For node"),
        },
        _ => panic!("Expected module"),
    }

    let source = "for x, y in pairs:\n    pass";
    let parsed = parser.parse(source).unwrap();
    match parser.to_ast(&parsed).unwrap() {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::For { target, .. } => {
                assert_eq!(target.target_display(), "x, y");
            }
            _ => panic!("Expected For node"),
        },
        _ => panic!("Expected module"),
    }

    let source = "for [a, b] in items:\n    pass";
    let parsed = parser.parse(source).unwrap();
    match parser.to_ast(&parsed).unwrap() {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::For { target, .. } => {
                assert_eq!(target.target_display(), "[a, b]");
            }
            _ => panic!("Expected For node"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_for_loop_target_extract_names() {
    let mut parser = PythonParser::new().unwrap();

    let source = "for x in items:\n    pass";
    let parsed = parser.parse(source).unwrap();
    match parser.to_ast(&parsed).unwrap() {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::For { target, .. } => {
                let names = target.binding_names();
                assert_eq!(names, vec!["x"]);
            }
            _ => panic!("Expected For node"),
        },
        _ => panic!("Expected module"),
    }

    let source = "for x, y, z in items:\n    pass";
    let parsed = parser.parse(source).unwrap();
    match parser.to_ast(&parsed).unwrap() {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::For { target, .. } => {
                let names = target.binding_names();
                assert_eq!(names, vec!["x", "y", "z"]);
            }
            _ => panic!("Expected For node"),
        },
        _ => panic!("Expected module"),
    }

    let source = "for [a, b] in items:\n    pass";
    let parsed = parser.parse(source).unwrap();
    match parser.to_ast(&parsed).unwrap() {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::For { target, .. } => {
                let names = target.binding_names();
                assert_eq!(names, vec!["a", "b"]);
            }
            _ => panic!("Expected For node"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_while_loop() {
    let mut parser = PythonParser::new().unwrap();
    let source = "while x > 0:\n    x -= 1";
    let parsed = parser.parse(source).unwrap();

    match parser.to_ast(&parsed).unwrap() {
        AstNode::Module { body, .. } => {
            assert_eq!(body.len(), 1);
            assert!(matches!(body[0], AstNode::While { .. }));
        }
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_while_loop_with_else() {
    let mut parser = PythonParser::new().unwrap();
    let source = "while x > 0:\n    x -= 1\nelse:\n    print('done')";
    let parsed = parser.parse(source).unwrap();

    match parser.to_ast(&parsed).unwrap() {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::While { else_body, .. } => {
                assert!(else_body.is_some(), "Should have else body");
                let else_stmts = else_body.as_ref().unwrap();
                assert_eq!(else_stmts.len(), 1, "Else body should have 1 statement");
                assert!(
                    matches!(else_stmts[0], AstNode::Call { .. }),
                    "Else body should contain Call node"
                );
            }
            _ => panic!("Expected While node"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_try_except() {
    let mut parser = PythonParser::new().unwrap();
    let source = "try:\n    x = 1\nexcept ValueError:\n    x = 0";
    let parsed = parser.parse(source).unwrap();

    match parser.to_ast(&parsed).unwrap() {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::Try { body, handlers, .. } => {
                assert_eq!(body.len(), 1);
                assert_eq!(handlers.len(), 1);
            }
            _ => panic!("Expected Try node"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_try_except_finally() {
    let mut parser = PythonParser::new().unwrap();
    let source = "try:\n    x = 1\nexcept:\n    x = 0\nfinally:\n    cleanup()";
    let parsed = parser.parse(source).unwrap();

    match parser.to_ast(&parsed).unwrap() {
        AstNode::Module { body, .. } => {
            assert!(matches!(body[0], AstNode::Try { .. }));
        }
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_try_except_tuple_exception_types() {
    let mut parser = PythonParser::new().unwrap();
    let source = "try:\n    x = 1\nexcept (ValueError, TypeError):\n    x = 0";
    let parsed = parser.parse(source).unwrap();

    match parser.to_ast(&parsed).unwrap() {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::Try { handlers, .. } => {
                assert_eq!(handlers.len(), 1);
                assert_eq!(handlers[0].exception_type, Some("(ValueError, TypeError)".to_string()));
            }
            _ => panic!("Expected Try node"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_try_except_bare_vs_specific() {
    let mut parser = PythonParser::new().unwrap();
    let source = "try:\n    x = 1\nexcept ValueError:\n    x = 0\nexcept:\n    x = -1";
    let parsed = parser.parse(source).unwrap();

    match parser.to_ast(&parsed).unwrap() {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::Try { handlers, .. } => {
                assert_eq!(handlers.len(), 2);
                assert_eq!(handlers[0].exception_type, Some("ValueError".to_string()));
                assert_eq!(handlers[1].exception_type, None);
            }
            _ => panic!("Expected Try node"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_with_statement() {
    let mut parser = PythonParser::new().unwrap();
    let source = "with open('file') as f:\n    data = f.read()";
    let parsed = parser.parse(source).unwrap();

    match parser.to_ast(&parsed).unwrap() {
        AstNode::Module { body, .. } => {
            assert!(matches!(body[0], AstNode::With { .. }));
        }
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_list_comprehension_ast() {
    let mut parser = PythonParser::new().unwrap();
    let source = "result = [x * 2 for x in items if x > 0]";
    let parsed = parser.parse(source).unwrap();

    match parser.to_ast(&parsed).unwrap() {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::Assignment { value, .. } => match value.as_ref() {
                AstNode::ListComp { generators, .. } => {
                    assert_eq!(generators.len(), 1);
                    assert_eq!(generators[0].ifs.len(), 1);
                }
                _ => panic!("Expected ListComp"),
            },
            _ => panic!("Expected Assignment"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_dict_comprehension_ast() {
    let mut parser = PythonParser::new().unwrap();
    let source = "result = {k: v for k, v in items}";
    let parsed = parser.parse(source).unwrap();

    match parser.to_ast(&parsed).unwrap() {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::Assignment { value, .. } => {
                assert!(matches!(value.as_ref(), AstNode::DictComp { .. }));
            }
            _ => panic!("Expected Assignment"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_set_comprehension_ast() {
    let mut parser = PythonParser::new().unwrap();
    let source = "result = {x for x in items}";
    let parsed = parser.parse(source).unwrap();

    match parser.to_ast(&parsed).unwrap() {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::Assignment { value, .. } => {
                assert!(matches!(value.as_ref(), AstNode::SetComp { .. }));
            }
            _ => panic!("Expected Assignment"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_generator_expression_ast() {
    let mut parser = PythonParser::new().unwrap();
    let source = "result = (x for x in items)";
    let parsed = parser.parse(source).unwrap();

    match parser.to_ast(&parsed).unwrap() {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::Assignment { value, .. } => {
                assert!(matches!(value.as_ref(), AstNode::GeneratorExp { .. }));
            }
            _ => panic!("Expected Assignment"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_lambda_expression() {
    let mut parser = PythonParser::new().unwrap();
    let source = "f = lambda x: x * 2";
    let parsed = parser.parse(source).unwrap();

    match parser.to_ast(&parsed).unwrap() {
        AstNode::Module { body, .. } => {
            assert_eq!(body.len(), 1);
        }
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_walrus_operator() {
    let mut parser = PythonParser::new().unwrap();
    let source = "if (n := len(items)) > 0:\n    print(n)";
    let parsed = parser.parse(source).unwrap();

    match parser.to_ast(&parsed).unwrap() {
        AstNode::Module { body, .. } => {
            assert!(matches!(body[0], AstNode::If { .. }));
        }
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_binary_operators() {
    let mut parser = PythonParser::new().unwrap();
    let source = "result = x + y * z";
    let parsed = parser.parse(source).unwrap();

    match parser.to_ast(&parsed).unwrap() {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::Assignment { value, .. } => {
                assert!(matches!(value.as_ref(), AstNode::BinaryOp { .. }));
            }
            _ => panic!("Expected Assignment"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_unary_operators() {
    let mut parser = PythonParser::new().unwrap();
    let source = "result = -x";
    let parsed = parser.parse(source).unwrap();

    match parser.to_ast(&parsed).unwrap() {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::Assignment { value, .. } => {
                assert!(matches!(value.as_ref(), AstNode::UnaryOp { .. }));
            }
            _ => panic!("Expected Assignment"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_unary_not_operator() {
    let mut parser = PythonParser::new().unwrap();
    let source = "if not x:\n    pass";
    let parsed = parser.parse(source).unwrap();

    match parser.to_ast(&parsed).unwrap() {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::If { test, .. } => {
                assert!(
                    matches!(test.as_ref(), AstNode::UnaryOp { op: UnaryOperator::Not, .. }),
                    "Expected UnaryOp with Not operator, got: {test:#?}"
                );
            }
            _ => panic!("Expected If node"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_comparison_operators() {
    let mut parser = PythonParser::new().unwrap();
    let source = "result = x < y";
    let parsed = parser.parse(source).unwrap();

    match parser.to_ast(&parsed).unwrap() {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::Assignment { value, .. } => {
                assert!(matches!(value.as_ref(), AstNode::Compare { .. }));
            }
            _ => panic!("Expected Assignment"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_subscript_access() {
    let mut parser = PythonParser::new().unwrap();
    let source = "item = arr[0]";
    let parsed = parser.parse(source).unwrap();

    match parser.to_ast(&parsed).unwrap() {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::Assignment { value, .. } => {
                assert!(matches!(value.as_ref(), AstNode::Subscript { .. }));
            }
            _ => panic!("Expected Assignment"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_match_statement() {
    let mut parser = PythonParser::new().unwrap();
    let source = "match value:\n    case 1:\n        print('one')";
    let parsed = parser.parse(source).unwrap();
    let _ = parser.to_ast(&parsed);
}

#[test]
fn test_pass_statement() {
    let mut parser = PythonParser::new().unwrap();
    let source = "def foo():\n    pass";
    let parsed = parser.parse(source).unwrap();

    match parser.to_ast(&parsed).unwrap() {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::FunctionDef { body, .. } => {
                assert!(matches!(body[0], AstNode::Pass { .. }));
            }
            _ => panic!("Expected FunctionDef"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_break_continue() {
    let mut parser = PythonParser::new().unwrap();
    let source = "for x in items:\n    if x < 0:\n        continue\n    if x > 10:\n        break";
    let parsed = parser.parse(source).unwrap();

    match parser.to_ast(&parsed).unwrap() {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::For { body, .. } => {
                assert!(body.iter().any(|node| matches!(node, AstNode::If { .. })));
            }
            _ => panic!("Expected For node"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_raise_statement_with_exception() {
    let mut parser = PythonParser::new().unwrap();
    let source = "raise NotImplementedError";
    let parsed = parser.parse(source).unwrap();

    match parser.to_ast(&parsed).unwrap() {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::Raise { exc, .. } => {
                assert!(exc.is_some());
                match exc.as_ref().unwrap().as_ref() {
                    AstNode::Identifier { name, .. } => {
                        assert_eq!(name, "NotImplementedError");
                    }
                    _ => panic!("Expected Identifier node for exception"),
                }
            }
            _ => panic!("Expected Raise node, got {:?}", &body[0]),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_raise_statement_bare() {
    let mut parser = PythonParser::new().unwrap();
    let source = "raise";
    let parsed = parser.parse(source).unwrap();

    match parser.to_ast(&parsed).unwrap() {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::Raise { exc, .. } => {
                assert!(exc.is_none(), "Expected bare raise with no exception");
            }
            _ => panic!("Expected Raise node"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_raise_not_implemented() {
    let mut parser = PythonParser::new().unwrap();
    let source = "raise NotImplemented";
    let parsed = parser.parse(source).unwrap();

    match parser.to_ast(&parsed).unwrap() {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::Raise { exc, .. } => {
                assert!(exc.is_some());
                match exc.as_ref().unwrap().as_ref() {
                    AstNode::Identifier { name, .. } => {
                        assert_eq!(name, "NotImplemented");
                    }
                    _ => panic!("Expected Identifier node for NotImplemented"),
                }
            }
            _ => panic!("Expected Raise node"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_tuple_with_parens() {
    let mut parser = PythonParser::new().unwrap();
    let source = "x = (1, 2, 3)";
    let parsed = parser.parse(source).unwrap();

    match parser.to_ast(&parsed).unwrap() {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::Assignment { value, .. } => match value.as_ref() {
                AstNode::Tuple { elements, .. } => {
                    assert_eq!(elements.len(), 3);
                    assert!(matches!(
                        &elements[0],
                        AstNode::Literal { value: LiteralValue::Integer(1), .. }
                    ));
                    assert!(matches!(
                        &elements[1],
                        AstNode::Literal { value: LiteralValue::Integer(2), .. }
                    ));
                    assert!(matches!(
                        &elements[2],
                        AstNode::Literal { value: LiteralValue::Integer(3), .. }
                    ));
                }
                _ => panic!("Expected Tuple node"),
            },
            _ => panic!("Expected Assignment node"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_tuple_without_parens() {
    let mut parser = PythonParser::new().unwrap();
    let source = "x = 1, 2, 3";
    let parsed = parser.parse(source).unwrap();

    match parser.to_ast(&parsed).unwrap() {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::Assignment { value, .. } => match value.as_ref() {
                AstNode::Tuple { elements, .. } => {
                    assert_eq!(elements.len(), 3);
                }
                _ => panic!("Expected Tuple node (expression_list)"),
            },
            _ => panic!("Expected Assignment node"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_single_element_tuple() {
    let mut parser = PythonParser::new().unwrap();
    let source = "x = (1,)";
    let parsed = parser.parse(source).unwrap();

    match parser.to_ast(&parsed).unwrap() {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::Assignment { value, .. } => match value.as_ref() {
                AstNode::Tuple { elements, .. } => {
                    assert_eq!(elements.len(), 1);
                    assert!(matches!(
                        &elements[0],
                        AstNode::Literal { value: LiteralValue::Integer(1), .. }
                    ));
                }
                _ => panic!("Expected Tuple node"),
            },
            _ => panic!("Expected Assignment node"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_parenthesized_expression_not_tuple() {
    let mut parser = PythonParser::new().unwrap();
    let source = "x = (1)";
    let parsed = parser.parse(source).unwrap();

    match parser.to_ast(&parsed).unwrap() {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::Assignment { value, .. } => match value.as_ref() {
                AstNode::Literal { value: LiteralValue::Integer(1), .. } => {}
                AstNode::ParenthesizedExpression { expression, .. } => {
                    assert!(matches!(
                        expression.as_ref(),
                        AstNode::Literal { value: LiteralValue::Integer(1), .. }
                    ));
                }
                other => panic!("Expected literal or parenthesized literal, got {other:?}"),
            },
            _ => panic!("Expected Assignment node"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_tuple_in_if_condition() {
    let mut parser = PythonParser::new().unwrap();
    let source = "if (x,):\n    pass";
    let parsed = parser.parse(source).unwrap();

    match parser.to_ast(&parsed).unwrap() {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::If { test, .. } => match test.as_ref() {
                AstNode::Tuple { elements, .. } => {
                    assert_eq!(elements.len(), 1);
                    assert!(matches!(&elements[0], AstNode::Identifier { name, .. } if name == "x"));
                }
                _ => panic!("Expected Tuple node in if condition"),
            },
            _ => panic!("Expected If node"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_string_prefix_extraction() {
    let mut parser = PythonParser::new().unwrap();

    let cases = vec![
        ("x = 'hello'", ""),
        ("x = f'hello'", "f"),
        ("x = r'hello'", "r"),
        ("x = b'hello'", "b"),
        ("x = rf'hello'", "rf"),
        ("x = F'hello'", "F"),
        ("x = R'hello'", "R"),
    ];

    for (source, expected_prefix) in cases {
        let parsed = parser.parse(source).unwrap();
        match parser.to_ast(&parsed).unwrap() {
            AstNode::Module { body, .. } => match &body[0] {
                AstNode::Assignment { value, .. } => match value.as_ref() {
                    AstNode::Literal { value: LiteralValue::String { value, prefix }, .. } => {
                        assert_eq!(prefix, expected_prefix, "Failed for source: {source}");
                        assert_eq!(value, "hello", "Failed for source: {source}");
                    }
                    _ => panic!("Expected String literal, got {value:?}"),
                },
                _ => panic!("Expected Assignment node"),
            },
            _ => panic!("Expected module"),
        }
    }
}

#[test]
fn test_extract_match_case_simple_value() {
    let mut parser = PythonParser::new().unwrap();
    let source = r#"match x:
    case 1:
        pass"#;

    let parsed = parser.parse(source).unwrap();
    let ast = parser.to_ast(&parsed).unwrap();

    match ast {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::Match { cases, .. } => {
                assert_eq!(cases.len(), 1);
                assert!(matches!(
                    &cases[0].pattern,
                    Pattern::MatchValue(AstNode::Literal { value: LiteralValue::Integer(1), .. })
                ));
                assert!(cases[0].guard.is_none());
                assert_eq!(cases[0].body.len(), 1);
            }
            other => panic!("Expected Match statement, got {other:?}"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_extract_match_case_with_guard() {
    let mut parser = PythonParser::new().unwrap();
    let source = r#"match x:
    case y if y > 0:
        pass"#;

    let parsed = parser.parse(source).unwrap();
    match parser.to_ast(&parsed).unwrap() {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::Match { cases, .. } => {
                assert_eq!(cases.len(), 1);
                assert!(cases[0].guard.is_some());
            }
            _ => panic!("Expected Match statement"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_extract_match_case_as_pattern() {
    let mut parser = PythonParser::new().unwrap();
    let source = r#"match x:
    case 1 as n:
        pass"#;

    let parsed = parser.parse(source).unwrap();
    match parser.to_ast(&parsed).unwrap() {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::Match { cases, .. } => {
                assert_eq!(cases.len(), 1);
                match &cases[0].pattern {
                    Pattern::MatchAs { pattern, name } => {
                        assert!(pattern.is_some());
                        assert_eq!(name.as_deref(), Some("n"));
                    }
                    _ => panic!("Expected MatchAs pattern"),
                }
            }
            _ => panic!("Expected Match statement"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_extract_pattern_list() {
    let mut parser = PythonParser::new().unwrap();
    let source = r#"match x:
    case [1, 2, 3]:
        pass"#;

    let parsed = parser.parse(source).unwrap();
    match parser.to_ast(&parsed).unwrap() {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::Match { cases, .. } => {
                assert_eq!(cases.len(), 1);
                match &cases[0].pattern {
                    Pattern::MatchSequence(patterns) => {
                        assert_eq!(patterns.len(), 3);
                    }
                    _ => panic!("Expected MatchSequence pattern"),
                }
            }
            _ => panic!("Expected Match statement"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_extract_pattern_tuple() {
    let mut parser = PythonParser::new().unwrap();
    let source = r#"match x:
    case (1, 2):
        pass"#;

    let parsed = parser.parse(source).unwrap();
    match parser.to_ast(&parsed).unwrap() {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::Match { cases, .. } => {
                assert_eq!(cases.len(), 1);
                match &cases[0].pattern {
                    Pattern::MatchSequence(patterns) => {
                        assert_eq!(patterns.len(), 2);
                    }
                    _ => panic!("Expected MatchSequence pattern"),
                }
            }
            _ => panic!("Expected Match statement"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_extract_pattern_dict() {
    let mut parser = PythonParser::new().unwrap();
    let source = r#"match x:
    case {"key": value}:
        pass"#;

    let parsed = parser.parse(source).unwrap();
    match parser.to_ast(&parsed).unwrap() {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::Match { cases, .. } => {
                assert_eq!(cases.len(), 1);
                match &cases[0].pattern {
                    Pattern::MatchMapping { keys, patterns } => {
                        assert_eq!(keys.len(), 1);
                        assert_eq!(patterns.len(), 1);
                    }
                    _ => panic!("Expected MatchMapping pattern"),
                }
            }
            _ => panic!("Expected Match statement"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_extract_pattern_or() {
    let mut parser = PythonParser::new().unwrap();
    let source = r#"match x:
    case 1 | 2 | 3:
        pass"#;

    let parsed = parser.parse(source).unwrap();
    match parser.to_ast(&parsed).unwrap() {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::Match { cases, .. } => {
                assert_eq!(cases.len(), 1);
                match &cases[0].pattern {
                    Pattern::MatchOr(patterns) => {
                        assert_eq!(patterns.len(), 3);
                    }
                    _ => panic!("Expected MatchOr pattern"),
                }
            }
            _ => panic!("Expected Match statement"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_extract_pattern_wildcard() {
    let mut parser = PythonParser::new().unwrap();
    let source = r#"match x:
    case _:
        pass"#;

    let parsed = parser.parse(source).unwrap();
    match parser.to_ast(&parsed).unwrap() {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::Match { cases, .. } => {
                assert_eq!(cases.len(), 1);
            }
            _ => panic!("Expected Match statement"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_extract_pattern_nested() {
    let mut parser = PythonParser::new().unwrap();
    let source = r#"match x:
    case [1, [2, 3]]:
        pass"#;

    let parsed = parser.parse(source).unwrap();
    match parser.to_ast(&parsed).unwrap() {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::Match { cases, .. } => {
                assert_eq!(cases.len(), 1);
                match &cases[0].pattern {
                    Pattern::MatchSequence(patterns) => {
                        assert_eq!(patterns.len(), 2);
                        match &patterns[1] {
                            Pattern::MatchSequence(inner) => {
                                assert_eq!(inner.len(), 2);
                            }
                            _ => panic!("Expected nested MatchSequence"),
                        }
                    }
                    _ => panic!("Expected MatchSequence pattern"),
                }
            }
            _ => panic!("Expected Match statement"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_extract_match_case_multiple_cases() {
    let mut parser = PythonParser::new().unwrap();
    let source = r#"match x:
    case 1:
        print("one")
    case 2:
        print("two")
    case _:
        print("other")"#;

    let parsed = parser.parse(source).unwrap();
    match parser.to_ast(&parsed).unwrap() {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::Match { cases, .. } => {
                assert_eq!(cases.len(), 3);
                assert_eq!(cases[0].body.len(), 1);
                assert_eq!(cases[1].body.len(), 1);
                assert_eq!(cases[2].body.len(), 1);
            }
            _ => panic!("Expected Match statement"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_extract_pattern_catch_all_with_underscore() {
    let mut parser = PythonParser::new().unwrap();
    let source = r#"match x:
    case _:
        pass"#;

    let parsed = parser.parse(source).unwrap();
    match parser.to_ast(&parsed).unwrap() {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::Match { cases, .. } => {
                assert_eq!(cases.len(), 1);
                match &cases[0].pattern {
                    Pattern::MatchAs { pattern, name } => {
                        assert!(pattern.is_none(), "Wildcard pattern should have no sub-pattern");
                        assert!(name.is_none(), "Wildcard _ should have no binding name");
                    }
                    _ => panic!("Expected MatchAs pattern for wildcard _"),
                }
            }
            _ => panic!("Expected Match statement"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_extract_pattern_catch_all_with_identifier() {
    let mut parser = PythonParser::new().unwrap();
    let source = r#"match x:
    case y:
        pass"#;

    let parsed = parser.parse(source).unwrap();
    match parser.to_ast(&parsed).unwrap() {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::Match { cases, .. } => {
                assert_eq!(cases.len(), 1);
                match &cases[0].pattern {
                    Pattern::MatchAs { pattern, name } => {
                        assert!(pattern.is_none(), "Capture pattern should have no sub-pattern");
                        assert_eq!(name.as_deref(), Some("y"), "Capture pattern should bind to 'y'");
                    }
                    _ => panic!("Expected MatchAs pattern for capture identifier"),
                }
            }
            _ => panic!("Expected Match statement"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_extract_pattern_catch_all_after_specific() {
    let mut parser = PythonParser::new().unwrap();
    let source = r#"match x:
    case 42:
        pass
    case y:
        pass"#;

    let parsed = parser.parse(source).unwrap();
    match parser.to_ast(&parsed).unwrap() {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::Match { cases, .. } => {
                assert_eq!(cases.len(), 2);
                match &cases[0].pattern {
                    Pattern::MatchValue(_) => {}
                    _ => panic!("First case should be MatchValue for literal"),
                }
                match &cases[1].pattern {
                    Pattern::MatchAs { pattern, name } => {
                        assert!(pattern.is_none());
                        assert_eq!(name.as_deref(), Some("y"));
                    }
                    _ => panic!("Second case should be MatchAs for capture"),
                }
            }
            _ => panic!("Expected Match statement"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_extract_pattern_multiple_catch_alls() {
    let mut parser = PythonParser::new().unwrap();
    let source = r#"match x:
    case y:
        pass
    case z:
        pass"#;

    let parsed = parser.parse(source).unwrap();
    match parser.to_ast(&parsed).unwrap() {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::Match { cases, .. } => {
                assert_eq!(cases.len(), 2);
                match &cases[0].pattern {
                    Pattern::MatchAs { pattern, name } => {
                        assert!(pattern.is_none());
                        assert_eq!(name.as_deref(), Some("y"));
                    }
                    _ => panic!("First case should be MatchAs"),
                }
                match &cases[1].pattern {
                    Pattern::MatchAs { pattern, name } => {
                        assert!(pattern.is_none());
                        assert_eq!(name.as_deref(), Some("z"));
                    }
                    _ => panic!("Second case should be MatchAs"),
                }
            }
            _ => panic!("Expected Match statement"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_async_function() {
    let mut parser = PythonParser::new().unwrap();
    let source = r#"
async def fetch_data():
    return 42
"#;

    let parsed = parser.parse(source).unwrap();
    let ast = parser.to_ast(&parsed).unwrap();
    match ast {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::FunctionDef { name, is_async, .. } => {
                assert_eq!(name, "fetch_data");
                assert!(*is_async, "is_async should be true for async function, got false");
            }
            other => panic!("Expected FunctionDef, got: {other:?}"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_generator_function_with_yield() {
    let mut parser = PythonParser::new().unwrap();
    let source = r#"
def count():
    yield 1
    yield 2
"#;

    let parsed = parser.parse(source).unwrap();
    match parser.to_ast(&parsed).unwrap() {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::FunctionDef { name, body, is_async, .. } => {
                assert_eq!(name, "count");
                assert!(!(*is_async));
                assert_eq!(body.len(), 2);
                assert!(matches!(body[0], AstNode::Yield { .. }));
                assert!(matches!(body[1], AstNode::Yield { .. }));
            }
            _ => panic!("Expected FunctionDef"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_await_expression() {
    let mut parser = PythonParser::new().unwrap();
    let source = r#"
async def fetch():
    result = await get_data()
    return result
"#;

    let parsed = parser.parse(source).unwrap();
    match parser.to_ast(&parsed).unwrap() {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::FunctionDef { name, body, is_async, .. } => {
                assert_eq!(name, "fetch");
                assert!(*is_async);
                match &body[0] {
                    AstNode::Assignment { value, .. } => {
                        assert!(matches!(**value, AstNode::Await { .. }));
                    }
                    _ => panic!("Expected Assignment"),
                }
            }
            _ => panic!("Expected FunctionDef"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_async_for_loop() {
    let mut parser = PythonParser::new().unwrap();
    let source = r#"
async for item in async_iterable:
    print(item)
"#;

    let parsed = parser.parse(source).unwrap();
    match parser.to_ast(&parsed).unwrap() {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::For { target, is_async, body, .. } => {
                assert!(matches!(target.as_ref(), AstNode::Identifier { name, .. } if name == "item"));
                assert!(*is_async, "is_async should be true for async for loop");
                assert_eq!(body.len(), 1);
            }
            other => panic!("Expected For node, got: {other:?}"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_regular_for_loop_not_async() {
    let mut parser = PythonParser::new().unwrap();
    let source = r#"
for item in iterable:
    print(item)
"#;

    let parsed = parser.parse(source).unwrap();
    match parser.to_ast(&parsed).unwrap() {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::For { target, is_async, .. } => {
                assert!(matches!(target.as_ref(), AstNode::Identifier { name, .. } if name == "item"));
                assert!(!(*is_async), "is_async should be false for regular for loop");
            }
            other => panic!("Expected For node, got: {other:?}"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_async_with_statement() {
    let mut parser = PythonParser::new().unwrap();
    let source = r#"
async with async_context_manager as value:
    print(value)
"#;

    let parsed = parser.parse(source).unwrap();
    match parser.to_ast(&parsed).unwrap() {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::With { items, is_async, body, .. } => {
                assert!(*is_async, "is_async should be true for async with statement");
                assert_eq!(items.len(), 1);
                assert_eq!(items[0].optional_vars, Some("value".to_string()));
                assert_eq!(body.len(), 1);
            }
            other => panic!("Expected With node, got: {other:?}"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_regular_with_statement_not_async() {
    let mut parser = PythonParser::new().unwrap();
    let source = r#"
with context_manager as value:
    print(value)
"#;

    let parsed = parser.parse(source).unwrap();
    match parser.to_ast(&parsed).unwrap() {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::With { items, is_async, .. } => {
                assert!(!(*is_async), "is_async should be false for regular with statement");
                assert_eq!(items.len(), 1);
            }
            other => panic!("Expected With node, got: {other:?}"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_assignment_span() {
    let mut parser = PythonParser::new().unwrap();
    let source = "x = 42";
    let parsed = parser.parse(source).unwrap();
    let ast = parser.to_ast(&parsed).unwrap();

    match ast {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::Assignment { line, col, end_line, end_col, .. } => {
                assert_eq!(*line, 1);
                assert_eq!(*col, 1);
                assert_eq!(*end_line, 1);
                assert_eq!(*end_col, 7);
            }
            _ => panic!("Expected assignment"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_identifier_span() {
    let mut parser = PythonParser::new().unwrap();
    let source = "print(hello)";
    let parsed = parser.parse(source).unwrap();
    let ast = parser.to_ast(&parsed).unwrap();

    match ast {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::Call { args, .. } => match &args[0] {
                AstNode::Identifier { name, line, col, end_line, end_col } => {
                    assert_eq!(name, "hello");
                    assert_eq!(*line, 1);
                    assert_eq!(*col, 7);
                    assert_eq!(*end_line, 1);
                    assert_eq!(*end_col, 12);
                }
                _ => panic!("Expected identifier"),
            },
            _ => panic!("Expected call"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_literal_span() {
    let mut parser = PythonParser::new().unwrap();
    let source = "x = 'test string'";
    let parsed = parser.parse(source).unwrap();
    let ast = parser.to_ast(&parsed).unwrap();

    match ast {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::Assignment { value, .. } => match value.as_ref() {
                AstNode::Literal { line, col, end_line, end_col, .. } => {
                    assert_eq!(*line, 1);
                    assert_eq!(*col, 5);
                    assert_eq!(*end_line, 1);
                    assert_eq!(*end_col, 18);
                }
                _ => panic!("Expected literal"),
            },
            _ => panic!("Expected assignment"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_binary_op_span() {
    let mut parser = PythonParser::new().unwrap();
    let source = "result = x + y";
    let parsed = parser.parse(source).unwrap();
    let ast = parser.to_ast(&parsed).unwrap();

    match ast {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::Assignment { value, .. } => match value.as_ref() {
                AstNode::BinaryOp { line, col, end_line, end_col, .. } => {
                    assert_eq!(*line, 1);
                    assert_eq!(*col, 10);
                    assert_eq!(*end_line, 1);
                    assert_eq!(*end_col, 15);
                }
                _ => panic!("Expected binary operation"),
            },
            _ => panic!("Expected assignment"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_function_def_span() {
    let mut parser = PythonParser::new().unwrap();
    let source = "def add(x, y):\n    return x + y";
    let parsed = parser.parse(source).unwrap();
    let ast = parser.to_ast(&parsed).unwrap();

    match ast {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::FunctionDef { line, col, end_line, end_col, .. } => {
                assert_eq!(*line, 1);
                assert_eq!(*col, 1);
                assert_eq!(*end_line, 2);
                assert!(*end_col > 1);
            }
            _ => panic!("Expected function definition"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_multiline_span() {
    let mut parser = PythonParser::new().unwrap();
    let source = "if x > 0:\n    print('positive')\nelse:\n    print('non-positive')";
    let parsed = parser.parse(source).unwrap();
    let ast = parser.to_ast(&parsed).unwrap();

    match ast {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::If { line, col, end_line, end_col, .. } => {
                assert_eq!(*line, 1);
                assert_eq!(*col, 1);
                assert_eq!(*end_line, 4);
                assert!(*end_col > 1);
            }
            _ => panic!("Expected if statement"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_parameter_span() {
    let mut parser = PythonParser::new().unwrap();
    let source = "def func(param1, param2: int):\n    pass";
    let parsed = parser.parse(source).unwrap();
    let ast = parser.to_ast(&parsed).unwrap();

    match ast {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::FunctionDef { args, .. } => {
                assert_eq!(args[0].name, "param1");
                assert_eq!(args[0].line, 1);
                assert_eq!(args[0].col, 10);
                assert_eq!(args[0].end_line, 1);
                assert_eq!(args[0].end_col, 16);
                assert_eq!(args[1].name, "param2");
                assert_eq!(args[1].line, 1);
                assert_eq!(args[1].col, 18);
                assert_eq!(args[1].end_line, 1);
                assert!(args[1].end_col > args[1].col);
            }
            _ => panic!("Expected function definition"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_generic_type_annotations() {
    let mut parser = PythonParser::new().unwrap();
    let source = r#"def process(items: List[str], mapping: Dict[str, int]) -> List[int]:
    pass"#;
    let parsed = parser.parse(source).unwrap();
    let ast = parser.to_ast(&parsed).unwrap();

    match ast {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::FunctionDef { args, return_type, .. } => {
                assert_eq!(args.len(), 2);
                assert_eq!(args[0].name, "items");
                assert_eq!(args[0].type_annotation, Some("List[str]".to_string()));
                assert_eq!(args[1].name, "mapping");
                assert_eq!(args[1].type_annotation, Some("Dict[str, int]".to_string()));
                assert_eq!(return_type, &Some("List[int]".to_string()));
            }
            _ => panic!("Expected function definition"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_nested_generic_type_annotations() {
    let mut parser = PythonParser::new().unwrap();
    let source = r#"def complex_func(data: List[Dict[str, int]]) -> Dict[str, List[int]]:
    pass"#;
    let parsed = parser.parse(source).unwrap();
    let ast = parser.to_ast(&parsed).unwrap();

    match ast {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::FunctionDef { args, return_type, .. } => {
                assert_eq!(args.len(), 1);
                assert_eq!(args[0].name, "data");
                assert_eq!(args[0].type_annotation, Some("List[Dict[str, int]]".to_string()));
                assert_eq!(return_type, &Some("Dict[str, List[int]]".to_string()));
            }
            _ => panic!("Expected function definition"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_pep585_lowercase_generic_annotations() {
    let mut parser = PythonParser::new().unwrap();
    let source = r#"def modern(data: list[int], mapping: dict[str, int]) -> list[str]:
    pass"#;
    let parsed = parser.parse(source).unwrap();
    let ast = parser.to_ast(&parsed).unwrap();

    match ast {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::FunctionDef { args, return_type, .. } => {
                assert_eq!(args.len(), 2);
                assert_eq!(args[0].type_annotation, Some("list[int]".to_string()));
                assert_eq!(args[1].type_annotation, Some("dict[str, int]".to_string()));
                assert_eq!(return_type, &Some("list[str]".to_string()));
            }
            _ => panic!("Expected function definition"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_typevar_declaration() {
    let mut parser = PythonParser::new().unwrap();
    let source = r#"T = TypeVar("T")"#;
    let parsed = parser.parse(source).unwrap();
    let ast = parser.to_ast(&parsed).unwrap();

    match ast {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::Assignment { target, value, .. } => {
                match target.as_ref() {
                    AstNode::Identifier { name, .. } => assert_eq!(name, "T"),
                    _ => panic!("Expected identifier"),
                }
                match value.as_ref() {
                    AstNode::Call { function, args, .. } => {
                        match function.as_ref() {
                            AstNode::Identifier { name, .. } => assert_eq!(name, "TypeVar"),
                            _ => panic!("Expected TypeVar identifier"),
                        }
                        assert_eq!(args.len(), 1);
                        match &args[0] {
                            AstNode::Literal { value: LiteralValue::String { value: s, .. }, .. } => {
                                assert_eq!(s, "T");
                            }
                            _ => panic!("Expected string literal"),
                        }
                    }
                    _ => panic!("Expected call"),
                }
            }
            _ => panic!("Expected assignment"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_typevar_constrained() {
    let mut parser = PythonParser::new().unwrap();
    let source = r#"T = TypeVar("T", int, str)"#;
    let parsed = parser.parse(source).unwrap();
    let ast = parser.to_ast(&parsed).unwrap();

    match ast {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::Assignment { value, .. } => match value.as_ref() {
                AstNode::Call { function, args, .. } => {
                    match function.as_ref() {
                        AstNode::Identifier { name, .. } => assert_eq!(name, "TypeVar"),
                        _ => panic!("Expected TypeVar identifier"),
                    }
                    assert_eq!(args.len(), 3, "Should have name + 2 constraints");
                }
                _ => panic!("Expected call"),
            },
            _ => panic!("Expected assignment"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_typevar_with_keyword_bound() {
    let mut parser = PythonParser::new().unwrap();
    let source = r#"T = TypeVar("T", bound=BaseClass)"#;
    let parsed = parser.parse(source).unwrap();
    let ast = parser.to_ast(&parsed).unwrap();

    match ast {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::Assignment { value, .. } => match value.as_ref() {
                AstNode::Call { function, args, keywords, .. } => {
                    match function.as_ref() {
                        AstNode::Identifier { name, .. } => assert_eq!(name, "TypeVar"),
                        _ => panic!("Expected TypeVar identifier"),
                    }
                    assert_eq!(args.len(), 1, "Should have just the name");
                    assert_eq!(keywords.len(), 1, "Should have one keyword argument");
                    assert_eq!(keywords[0].0, "bound");
                    match &keywords[0].1 {
                        AstNode::Identifier { name, .. } => assert_eq!(name, "BaseClass"),
                        _ => panic!("Expected identifier for bound value"),
                    }
                }
                _ => panic!("Expected call"),
            },
            _ => panic!("Expected assignment"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_annotated_assignment_with_generic_type() {
    let mut parser = PythonParser::new().unwrap();
    let source = r#"items: List[int] = []"#;
    let parsed = parser.parse(source).unwrap();
    let ast = parser.to_ast(&parsed).unwrap();

    match ast {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::AnnotatedAssignment { target, type_annotation, value, .. } => {
                match target.as_ref() {
                    AstNode::Identifier { name, .. } => assert_eq!(name, "items"),
                    _ => panic!("Expected identifier"),
                }
                assert_eq!(type_annotation, "List[int]");
                assert!(value.is_some());
            }
            _ => panic!("Expected annotated assignment"),
        },
        _ => panic!("Expected module"),
    }
}

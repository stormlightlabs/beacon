use beacon_parser::{AstNode, BinaryOperator, CompareOperator, LiteralValue, PythonParser, UnaryOperator};

/// Helper function to parse and return the AST
fn parse_to_ast(source: &str) -> AstNode {
    let mut parser = PythonParser::new().unwrap();
    let parsed = parser.parse(source).unwrap();
    parser.to_ast(&parsed).unwrap()
}

/// Helper function to extract the first statement from a module
fn extract_first_stmt(ast: AstNode) -> AstNode {
    match ast {
        AstNode::Module { body, .. } => body.into_iter().next().unwrap(),
        _ => panic!("Expected Module node"),
    }
}

// ============================================================================
// Basic Expressions
// ============================================================================

#[test]
fn test_simple_literals() {
    let ast = parse_to_ast("x = 42");
    match extract_first_stmt(ast) {
        AstNode::Assignment { value, .. } => match *value {
            AstNode::Literal { value: LiteralValue::Integer(n), .. } => {
                assert_eq!(n, 42);
            }
            _ => panic!("Expected integer literal"),
        },
        _ => panic!("Expected Assignment"),
    }

    let ast = parse_to_ast("x = \"hello\"");
    match extract_first_stmt(ast) {
        AstNode::Assignment { value, .. } => match *value {
            AstNode::Literal { value: LiteralValue::String { value: s, .. }, .. } => {
                assert_eq!(s, "hello");
            }
            _ => panic!("Expected string literal"),
        },
        _ => panic!("Expected Assignment"),
    }

    let ast = parse_to_ast("x = True");
    match extract_first_stmt(ast) {
        AstNode::Assignment { value, .. } => match *value {
            AstNode::Literal { value: LiteralValue::Boolean(b), .. } => {
                assert!(b);
            }
            _ => panic!("Expected boolean literal"),
        },
        _ => panic!("Expected Assignment"),
    }

    let ast = parse_to_ast("x = None");
    match extract_first_stmt(ast) {
        AstNode::Assignment { value, .. } => {
            assert!(matches!(*value, AstNode::Literal { value: LiteralValue::None, .. }));
        }
        _ => panic!("Expected Assignment"),
    }
}

#[test]
fn test_binary_operations_arithmetic() {
    let ast = parse_to_ast("result = x + y");
    match extract_first_stmt(ast) {
        AstNode::Assignment { value, .. } => match *value {
            AstNode::BinaryOp { op, left, right, .. } => {
                assert_eq!(op, BinaryOperator::Add);
                assert!(matches!(*left, AstNode::Identifier { name, .. } if name == "x"));
                assert!(matches!(*right, AstNode::Identifier { name, .. } if name == "y"));
            }
            _ => panic!("Expected BinaryOp"),
        },
        _ => panic!("Expected Assignment"),
    }

    let ast = parse_to_ast("result = a * b - c");
    match extract_first_stmt(ast) {
        AstNode::Assignment { value, .. } => match *value {
            AstNode::BinaryOp { op, .. } => {
                assert_eq!(op, BinaryOperator::Sub);
            }
            _ => panic!("Expected BinaryOp"),
        },
        _ => panic!("Expected Assignment"),
    }
}

#[test]
fn test_binary_operations_comparison() {
    let ast = parse_to_ast("result = x < y");
    match extract_first_stmt(ast) {
        AstNode::Assignment { value, .. } => match *value {
            AstNode::Compare { ops, .. } => {
                assert_eq!(ops[0], CompareOperator::Lt);
            }
            _ => panic!("Expected Compare"),
        },
        _ => panic!("Expected Assignment"),
    }

    let ast = parse_to_ast("result = a == b");
    match extract_first_stmt(ast) {
        AstNode::Assignment { value, .. } => match *value {
            AstNode::Compare { ops, .. } => {
                assert_eq!(ops[0], CompareOperator::Eq);
            }
            _ => panic!("Expected Compare"),
        },
        _ => panic!("Expected Assignment"),
    }
}

#[test]
fn test_unary_operations() {
    let ast = parse_to_ast("result = -x");
    match extract_first_stmt(ast) {
        AstNode::Assignment { value, .. } => match *value {
            AstNode::UnaryOp { op, operand, .. } => {
                assert_eq!(op, UnaryOperator::Minus);
                assert!(matches!(*operand, AstNode::Identifier { name, .. } if name == "x"));
            }
            _ => panic!("Expected UnaryOp"),
        },
        _ => panic!("Expected Assignment"),
    }

    let ast = parse_to_ast("result = ~x");
    match extract_first_stmt(ast) {
        AstNode::Assignment { value, .. } => match *value {
            AstNode::UnaryOp { op, .. } => {
                assert_eq!(op, UnaryOperator::Invert);
            }
            _ => panic!("Expected UnaryOp"),
        },
        _ => panic!("Expected Assignment"),
    }
}

#[test]
fn test_parenthesized_expressions() {
    let ast = parse_to_ast("result = (x + y) * z");
    match extract_first_stmt(ast) {
        AstNode::Assignment { value, .. } => match *value {
            AstNode::BinaryOp { op, left, .. } => {
                assert_eq!(op, BinaryOperator::Mult);
                match *left {
                    AstNode::ParenthesizedExpression { expression, .. } => {
                        assert!(matches!(*expression, AstNode::BinaryOp { op, .. } if op == BinaryOperator::Add));
                    }
                    _ => panic!("Expected ParenthesizedExpression"),
                }
            }
            _ => panic!("Expected BinaryOp"),
        },
        _ => panic!("Expected Assignment"),
    }
}

// ============================================================================
// Tuples and Collections
// ============================================================================

#[test]
fn test_tuple_with_parentheses() {
    let ast = parse_to_ast("x = (1, 2, 3)");
    match extract_first_stmt(ast) {
        AstNode::Assignment { value, .. } => match *value {
            AstNode::Tuple { elements, is_parenthesized, .. } => {
                assert!(is_parenthesized, "Tuple should be marked as parenthesized");
                assert_eq!(elements.len(), 3);
                assert!(matches!(
                    elements[0],
                    AstNode::Literal { value: LiteralValue::Integer(1), .. }
                ));
            }
            _ => panic!("Expected Tuple"),
        },
        _ => panic!("Expected Assignment"),
    }
}

#[test]
fn test_tuple_without_parentheses() {
    let ast = parse_to_ast("x = 1, 2, 3");
    match extract_first_stmt(ast) {
        AstNode::Assignment { value, .. } => match *value {
            AstNode::Tuple { elements, is_parenthesized, .. } => {
                assert!(!is_parenthesized, "Tuple should not be marked as parenthesized");
                assert_eq!(elements.len(), 3);
            }
            _ => panic!("Expected Tuple"),
        },
        _ => panic!("Expected Assignment"),
    }
}

#[test]
fn test_tuple_destructuring() {
    let ast = parse_to_ast("a, b, c = values");
    match extract_first_stmt(ast) {
        AstNode::Assignment { target, .. } => match *target {
            AstNode::Tuple { elements, is_parenthesized, .. } => {
                assert!(!is_parenthesized);
                assert_eq!(elements.len(), 3);
                assert!(matches!(&elements[0], AstNode::Identifier { name, .. } if name == "a"));
                assert!(matches!(&elements[1], AstNode::Identifier { name, .. } if name == "b"));
                assert!(matches!(&elements[2], AstNode::Identifier { name, .. } if name == "c"));
            }
            _ => panic!("Expected Tuple in target"),
        },
        _ => panic!("Expected Assignment"),
    }

    let ast = parse_to_ast("x, y, z = 1, 2, 3");
    match extract_first_stmt(ast) {
        AstNode::Assignment { target, value, .. } => {
            assert!(matches!(*target, AstNode::Tuple { elements, .. } if elements.len() == 3));
            assert!(matches!(*value, AstNode::Tuple { elements, .. } if elements.len() == 3));
        }
        _ => panic!("Expected Assignment"),
    }
}

#[test]
fn test_list_and_dict_literals() {
    let ast = parse_to_ast("x = [1, 2, 3]");
    match extract_first_stmt(ast) {
        AstNode::Assignment { value, .. } => match *value {
            AstNode::List { elements, .. } => {
                assert_eq!(elements.len(), 3);
            }
            _ => panic!("Expected List"),
        },
        _ => panic!("Expected Assignment"),
    }

    let ast = parse_to_ast("x = {'a': 1, 'b': 2}");
    match extract_first_stmt(ast) {
        AstNode::Assignment { value, .. } => match *value {
            AstNode::Dict { keys, values, .. } => {
                assert_eq!(keys.len(), 2);
                assert_eq!(values.len(), 2);
            }
            _ => panic!("Expected Dict"),
        },
        _ => panic!("Expected Assignment"),
    }
}

// ============================================================================
// Functions and Lambdas
// ============================================================================

#[test]
fn test_lambda_single_parameter() {
    let ast = parse_to_ast("f = lambda x: x * 2");
    match extract_first_stmt(ast) {
        AstNode::Assignment { value, .. } => match *value {
            AstNode::Lambda { args, body, .. } => {
                assert_eq!(args.len(), 1);
                assert_eq!(args[0].name, "x");
                assert!(matches!(*body, AstNode::BinaryOp { .. }));
            }
            _ => panic!("Expected Lambda"),
        },
        _ => panic!("Expected Assignment"),
    }
}

#[test]
fn test_lambda_multiple_parameters() {
    let ast = parse_to_ast("add = lambda a, b: a + b");
    match extract_first_stmt(ast) {
        AstNode::Assignment { value, .. } => match *value {
            AstNode::Lambda { args, body, .. } => {
                assert_eq!(args.len(), 2);
                assert_eq!(args[0].name, "a");
                assert_eq!(args[1].name, "b");
                assert!(matches!(
                    *body,
                    AstNode::BinaryOp { op, .. } if op == BinaryOperator::Add
                ));
            }
            _ => panic!("Expected Lambda"),
        },
        _ => panic!("Expected Assignment"),
    }
}

#[test]
fn test_lambda_with_default_arguments() {
    let ast = parse_to_ast("f = lambda x, y=10: x + y");
    match extract_first_stmt(ast) {
        AstNode::Assignment { value, .. } => match *value {
            AstNode::Lambda { args, .. } => {
                assert_eq!(args.len(), 2);
                assert_eq!(args[0].name, "x");
                assert!(args[0].default_value.is_none());
                assert_eq!(args[1].name, "y");
                assert!(args[1].default_value.is_some());
            }
            _ => panic!("Expected Lambda"),
        },
        _ => panic!("Expected Assignment"),
    }
}

#[test]
fn test_nested_lambdas() {
    let ast = parse_to_ast("outer = lambda x: lambda y: x + y");
    match extract_first_stmt(ast) {
        AstNode::Assignment { value, .. } => match *value {
            AstNode::Lambda { args, body, .. } => {
                assert_eq!(args.len(), 1);
                assert_eq!(args[0].name, "x");
                match *body {
                    AstNode::Lambda { args: inner_args, .. } => {
                        assert_eq!(inner_args.len(), 1);
                        assert_eq!(inner_args[0].name, "y");
                    }
                    _ => panic!("Expected nested Lambda"),
                }
            }
            _ => panic!("Expected Lambda"),
        },
        _ => panic!("Expected Assignment"),
    }
}

// ============================================================================
// Modern Syntax - Walrus Operator (3.8+)
// ============================================================================

#[test]
fn test_walrus_in_conditionals() {
    let ast = parse_to_ast("if (n := len(data)) > 10:\n    print(n)");
    match extract_first_stmt(ast) {
        AstNode::If { test, .. } => match *test {
            AstNode::Compare { left, .. } => match *left {
                AstNode::ParenthesizedExpression { expression, .. } => {
                    assert!(matches!(*expression, AstNode::NamedExpr { .. }));
                }
                _ => panic!("Expected ParenthesizedExpression containing NamedExpr"),
            },
            _ => panic!("Expected Compare with walrus operator"),
        },
        _ => panic!("Expected If statement"),
    }
}

/// TODO: inspect the comprehension's filter
#[test]
fn test_walrus_in_comprehensions() {
    let ast = parse_to_ast("results = [y for x in data if (y := transform(x)) is not None]");
    match extract_first_stmt(ast) {
        AstNode::Assignment { value, .. } => match *value {
            AstNode::ListComp { .. } => {}
            _ => panic!("Expected ListComp"),
        },
        _ => panic!("Expected Assignment"),
    }
}

#[test]
fn test_nested_walrus_expressions() {
    let ast = parse_to_ast("if (a := 1) and (b := 2):\n    print(a, b)");
    match extract_first_stmt(ast) {
        AstNode::If { test, .. } => {
            assert!(matches!(*test, AstNode::BinaryOp { .. }));
        }
        _ => panic!("Expected If statement"),
    }
}

// ============================================================================
// Comprehensions
// ============================================================================

#[test]
fn test_list_comprehension() {
    let ast = parse_to_ast("squares = [x**2 for x in range(10)]");
    match extract_first_stmt(ast) {
        AstNode::Assignment { value, .. } => match *value {
            AstNode::ListComp { element, generators, .. } => {
                assert!(matches!(*element, AstNode::BinaryOp { .. }));
                assert_eq!(generators.len(), 1);
            }
            _ => panic!("Expected ListComp"),
        },
        _ => panic!("Expected Assignment"),
    }
}

#[test]
fn test_dict_comprehension() {
    let ast = parse_to_ast("mapping = {k: v for k, v in pairs}");
    match extract_first_stmt(ast) {
        AstNode::Assignment { value, .. } => {
            assert!(matches!(*value, AstNode::DictComp { .. }));
        }
        _ => panic!("Expected Assignment"),
    }
}

#[test]
fn test_set_comprehension() {
    let ast = parse_to_ast("unique = {x for x in items}");
    match extract_first_stmt(ast) {
        AstNode::Assignment { value, .. } => {
            assert!(matches!(*value, AstNode::SetComp { .. }));
        }
        _ => panic!("Expected Assignment"),
    }
}

#[test]
fn test_generator_expression() {
    let ast = parse_to_ast("gen = (x for x in items)");
    match extract_first_stmt(ast) {
        AstNode::Assignment { value, .. } => {
            assert!(matches!(*value, AstNode::GeneratorExp { .. }));
        }
        _ => panic!("Expected Assignment"),
    }
}

#[test]
fn test_nested_list_comprehension() {
    let ast = parse_to_ast("flat = [item for row in matrix for item in row]");
    match extract_first_stmt(ast) {
        AstNode::Assignment { value, .. } => match *value {
            AstNode::ListComp { generators, .. } => {
                assert_eq!(generators.len(), 2, "Should have 2 generators for nested iteration");
            }
            _ => panic!("Expected ListComp"),
        },
        _ => panic!("Expected Assignment"),
    }
}

#[test]
fn test_nested_comprehension_with_inner_comprehension() {
    let ast = parse_to_ast("matrix = [[y for y in range(3)] for x in range(3)]");
    match extract_first_stmt(ast) {
        AstNode::Assignment { value, .. } => match *value {
            AstNode::ListComp { element, generators, .. } => {
                assert_eq!(generators.len(), 1, "Outer comprehension should have 1 generator");
                assert!(
                    matches!(*element, AstNode::ListComp { .. }),
                    "Element should be a nested ListComp"
                );
            }
            _ => panic!("Expected ListComp"),
        },
        _ => panic!("Expected Assignment"),
    }
}

#[test]
fn test_nested_dict_comprehension() {
    let ast = parse_to_ast("grouped = {k: [v for v in values if v.key == k] for k in keys}");
    match extract_first_stmt(ast) {
        AstNode::Assignment { value, .. } => match *value {
            AstNode::DictComp { value, .. } => {
                assert!(
                    matches!(*value, AstNode::ListComp { .. }),
                    "Dict value should be a ListComp"
                );
            }
            _ => panic!("Expected DictComp"),
        },
        _ => panic!("Expected Assignment"),
    }
}

#[test]
fn test_comprehension_with_multiple_filters() {
    let ast = parse_to_ast("evens = [x for x in range(100) if x % 2 == 0 if x > 10]");
    match extract_first_stmt(ast) {
        AstNode::Assignment { value, .. } => {
            assert!(matches!(*value, AstNode::ListComp { .. }));
        }
        _ => panic!("Expected Assignment"),
    }
}

// ============================================================================
// Control Flow
// ============================================================================

#[test]
fn test_if_elif_else_chain() {
    let source = r#"if x < 0:
    result = "negative"
elif x == 0:
    result = "zero"
elif x < 10:
    result = "small"
else:
    result = "large""#;

    let ast = parse_to_ast(source);
    match extract_first_stmt(ast) {
        AstNode::If { test, body, elif_parts, else_body, .. } => {
            assert!(matches!(*test, AstNode::Compare { .. }));
            assert_eq!(body.len(), 1);

            assert_eq!(elif_parts.len(), 2);

            assert!(matches!(&elif_parts[0].0, AstNode::Compare { .. }));
            assert_eq!(elif_parts[0].1.len(), 1);

            assert!(matches!(&elif_parts[1].0, AstNode::Compare { .. }));
            assert_eq!(elif_parts[1].1.len(), 1);

            assert!(else_body.is_some());
            assert_eq!(else_body.unwrap().len(), 1);
        }
        _ => panic!("Expected If statement"),
    }
}

#[test]
fn test_simple_if_else() {
    let source = r#"if condition:
    x = 1
else:
    x = 2"#;

    let ast = parse_to_ast(source);
    match extract_first_stmt(ast) {
        AstNode::If { test, body, else_body, .. } => {
            assert!(matches!(*test, AstNode::Identifier { .. }));
            assert_eq!(body.len(), 1);
            assert!(else_body.is_some());
            assert_eq!(else_body.unwrap().len(), 1);
        }
        _ => panic!("Expected If statement"),
    }
}

#[test]
fn test_while_loop() {
    let source = r#"while count < 10:
    count += 1
    process(count)"#;

    let ast = parse_to_ast(source);
    match extract_first_stmt(ast) {
        AstNode::While { test, body, else_body, .. } => {
            assert!(matches!(*test, AstNode::Compare { .. }));
            assert_eq!(body.len(), 2);
            assert!(else_body.is_none());
        }
        _ => panic!("Expected While statement"),
    }
}

#[test]
fn test_while_with_else() {
    let source = r#"while x > 0:
    x -= 1
else:
    print("done")"#;

    let ast = parse_to_ast(source);
    match extract_first_stmt(ast) {
        AstNode::While { body, else_body, .. } => {
            assert_eq!(body.len(), 1);
            assert!(else_body.is_some());
            assert_eq!(else_body.unwrap().len(), 1);
        }
        _ => panic!("Expected While statement"),
    }
}

#[test]
fn test_for_loop_simple() {
    let source = r#"for item in items:
    print(item)"#;

    let ast = parse_to_ast(source);
    match extract_first_stmt(ast) {
        AstNode::For { target, iter, body, else_body, .. } => {
            assert!(matches!(*target, AstNode::Identifier { name, .. } if name == "item"));
            assert!(matches!(*iter, AstNode::Identifier { name, .. } if name == "items"));
            assert_eq!(body.len(), 1);
            assert!(else_body.is_none());
        }
        _ => panic!("Expected For statement"),
    }
}

#[test]
fn test_for_loop_with_multiple_targets() {
    let source = r#"for key, value in pairs:
    print(key, value)"#;

    let ast = parse_to_ast(source);
    match extract_first_stmt(ast) {
        AstNode::For { target, body, .. } => {
            match *target {
                AstNode::Tuple { elements, .. } => {
                    assert_eq!(elements.len(), 2);
                    assert!(matches!(&elements[0], AstNode::Identifier { name, .. } if name == "key"));
                    assert!(matches!(&elements[1], AstNode::Identifier { name, .. } if name == "value"));
                }
                _ => panic!("Expected Tuple target"),
            }
            assert_eq!(body.len(), 1);
        }
        _ => panic!("Expected For statement"),
    }
}

#[test]
fn test_for_loop_with_enumerate() {
    let source = r#"for i, item in enumerate(items):
    print(i, item)"#;

    let ast = parse_to_ast(source);
    match extract_first_stmt(ast) {
        AstNode::For { target, iter, .. } => {
            match *target {
                AstNode::Tuple { elements, .. } => {
                    assert_eq!(elements.len(), 2);
                    assert!(matches!(&elements[0], AstNode::Identifier { name, .. } if name == "i"));
                    assert!(matches!(&elements[1], AstNode::Identifier { name, .. } if name == "item"));
                }
                _ => panic!("Expected Tuple target"),
            }
            assert!(matches!(*iter, AstNode::Call { .. }));
        }
        _ => panic!("Expected For statement"),
    }
}

#[test]
fn test_for_loop_with_else() {
    let source = r#"for x in range(5):
    if x == 3:
        break
else:
    print("completed")"#;

    let ast = parse_to_ast(source);
    match extract_first_stmt(ast) {
        AstNode::For { body, else_body, .. } => {
            assert_eq!(body.len(), 1);
            assert!(else_body.is_some());
            assert_eq!(else_body.unwrap().len(), 1);
        }
        _ => panic!("Expected For statement"),
    }
}

#[test]
fn test_try_except_basic() {
    let source = r#"try:
    risky_operation()
except ValueError:
    handle_error()"#;

    let ast = parse_to_ast(source);
    match extract_first_stmt(ast) {
        AstNode::Try { body, handlers, else_body, finally_body, .. } => {
            assert_eq!(body.len(), 1);
            assert_eq!(handlers.len(), 1);
            assert_eq!(handlers[0].exception_type, Some("ValueError".to_string()));
            assert_eq!(handlers[0].body.len(), 1);
            assert!(else_body.is_none());
            assert!(finally_body.is_none());
        }
        _ => panic!("Expected Try statement"),
    }
}

#[test]
fn test_try_except_multiple_handlers() {
    let source = r#"try:
    operation()
except ValueError:
    handle_value_error()
except KeyError:
    handle_key_error()
except Exception:
    handle_generic()"#;

    let ast = parse_to_ast(source);
    match extract_first_stmt(ast) {
        AstNode::Try { handlers, .. } => {
            assert_eq!(handlers.len(), 3);
            assert_eq!(handlers[0].exception_type, Some("ValueError".to_string()));
            assert_eq!(handlers[1].exception_type, Some("KeyError".to_string()));
            assert_eq!(handlers[2].exception_type, Some("Exception".to_string()));
        }
        _ => panic!("Expected Try statement"),
    }
}

#[test]
fn test_try_except_with_as() {
    let source = r#"try:
    risky()
except ValueError as e:
    print(e)"#;

    let ast = parse_to_ast(source);
    match extract_first_stmt(ast) {
        AstNode::Try { handlers, .. } => {
            assert_eq!(handlers.len(), 1);
            assert_eq!(handlers[0].exception_type, Some("ValueError".to_string()));
            assert_eq!(handlers[0].name, Some("e".to_string()));
        }
        _ => panic!("Expected Try statement"),
    }
}

#[test]
fn test_try_except_else_finally() {
    let source = r#"try:
    operation()
except Exception:
    handle_error()
else:
    success()
finally:
    cleanup()"#;

    let ast = parse_to_ast(source);
    match extract_first_stmt(ast) {
        AstNode::Try { body, handlers, else_body, finally_body, .. } => {
            assert_eq!(body.len(), 1);
            assert_eq!(handlers.len(), 1);
            assert!(else_body.is_some(), "Should have else block");
            assert_eq!(else_body.unwrap().len(), 1);
            assert!(finally_body.is_some(), "Should have finally block");
            assert_eq!(finally_body.unwrap().len(), 1);
        }
        _ => panic!("Expected Try statement"),
    }
}

#[test]
fn test_try_finally_without_except() {
    let source = r#"try:
    operation()
finally:
    cleanup()"#;

    let ast = parse_to_ast(source);
    match extract_first_stmt(ast) {
        AstNode::Try { body, handlers, finally_body, .. } => {
            assert_eq!(body.len(), 1);
            assert_eq!(handlers.len(), 0, "Should have no exception handlers");
            assert!(finally_body.is_some());
            assert_eq!(finally_body.unwrap().len(), 1);
        }
        _ => panic!("Expected Try statement"),
    }
}

#[test]
fn test_bare_except() {
    let source = r#"try:
    risky()
except:
    handle_any()"#;

    let ast = parse_to_ast(source);
    match extract_first_stmt(ast) {
        AstNode::Try { handlers, .. } => {
            assert_eq!(handlers.len(), 1);
            assert_eq!(
                handlers[0].exception_type, None,
                "Bare except should have no exception type"
            );
        }
        _ => panic!("Expected Try statement"),
    }
}

// ============================================================================
// Call Chains and Nested Calls
// ============================================================================

#[test]
fn test_attribute_call_chain() {
    let source = "result = obj.method1().method2().method3()";

    let ast = parse_to_ast(source);
    match extract_first_stmt(ast) {
        AstNode::Assignment { value, .. } => match *value {
            AstNode::Call { function, .. } => match *function {
                AstNode::Attribute { attribute, object, .. } => {
                    assert_eq!(attribute, "method3");

                    match *object {
                        AstNode::Call { function: method2_func, .. } => match *method2_func {
                            AstNode::Attribute { attribute: attr2, .. } => {
                                assert_eq!(attr2, "method2");
                            }
                            _ => panic!("Expected Attribute for method2"),
                        },
                        _ => panic!("Expected Call for method2()"),
                    }
                }
                _ => panic!("Expected Attribute for method3"),
            },
            _ => panic!("Expected Call for method3()"),
        },
        _ => panic!("Expected Assignment"),
    }
}

#[test]
fn test_function_call_chain() {
    let source = "result = get_manager().get_users()";

    let ast = parse_to_ast(source);
    match extract_first_stmt(ast) {
        AstNode::Assignment { value, .. } => match *value {
            AstNode::Call { function, .. } => match *function {
                AstNode::Attribute { attribute, object, .. } => {
                    assert_eq!(attribute, "get_users");
                    assert!(matches!(*object, AstNode::Call { .. }));
                }
                _ => panic!("Expected Attribute"),
            },
            _ => panic!("Expected Call"),
        },
        _ => panic!("Expected Assignment"),
    }
}

#[test]
fn test_call_with_lambda_args() {
    let source = "result = compose(lambda x: x + 1, lambda y: y * 2)";

    let ast = parse_to_ast(source);
    match extract_first_stmt(ast) {
        AstNode::Assignment { value, .. } => match *value {
            AstNode::Call { function, args, .. } => {
                assert!(matches!(*function, AstNode::Identifier { name, .. } if name == "compose"));
                assert_eq!(args.len(), 2);
                assert!(matches!(args[0], AstNode::Lambda { .. }));
                assert!(matches!(args[1], AstNode::Lambda { .. }));
            }
            _ => panic!("Expected Call"),
        },
        _ => panic!("Expected Assignment"),
    }
}

#[test]
fn test_deeply_nested_calls() {
    let source = "result = a.b().c().d().e()";

    let ast = parse_to_ast(source);
    match extract_first_stmt(ast) {
        AstNode::Assignment { value, .. } => {
            let mut current = &*value;
            let expected_methods = vec!["e", "d", "c", "b"];

            for method_name in expected_methods {
                match current {
                    AstNode::Call { function, .. } => match &**function {
                        AstNode::Attribute { attribute, object, .. } => {
                            assert_eq!(attribute, method_name);
                            current = object;
                        }
                        _ => panic!("Expected Attribute for {method_name}"),
                    },
                    _ => panic!("Expected Call for {method_name}()"),
                }
            }

            assert!(matches!(current, AstNode::Identifier { name, .. } if name == "a"));
        }
        _ => panic!("Expected Assignment"),
    }
}

#[test]
fn test_mixed_call_and_attribute_chain() {
    let source = "result = obj.attr.method().another_attr.final()";

    let ast = parse_to_ast(source);
    match extract_first_stmt(ast) {
        AstNode::Assignment { value, .. } => match *value {
            AstNode::Call { function, .. } => match *function {
                AstNode::Attribute { attribute, object, .. } => {
                    assert_eq!(attribute, "final");
                    match *object {
                        AstNode::Attribute { attribute: attr2, object: obj2, .. } => {
                            assert_eq!(attr2, "another_attr");
                            assert!(matches!(*obj2, AstNode::Call { .. }));
                        }
                        _ => panic!("Expected Attribute for another_attr"),
                    }
                }
                _ => panic!("Expected Attribute for final"),
            },
            _ => panic!("Expected Call for final()"),
        },
        _ => panic!("Expected Assignment"),
    }
}

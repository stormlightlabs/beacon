use beacon_parser::{AstNode, BinaryOperator, CompareOperator, LiteralValue, Pattern, PythonParser, UnaryOperator};

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

// ============================================================================
// Pattern Matching (3.10+)
// ============================================================================

#[test]
fn test_basic_match_statement() {
    let source = r#"match value:
    case 1:
        result = "one"
    case 2:
        result = "two""#;

    let ast = parse_to_ast(source);
    match extract_first_stmt(ast) {
        AstNode::Match { subject, cases, .. } => {
            assert!(matches!(*subject, AstNode::Identifier { name, .. } if name == "value"));
            assert_eq!(cases.len(), 2, "Should have two cases");

            match &cases[0].pattern {
                Pattern::MatchValue(node) => {
                    assert!(matches!(node, AstNode::Literal { value: LiteralValue::Integer(1), .. }));
                }
                _ => panic!("Expected MatchValue pattern for first case"),
            }

            match &cases[1].pattern {
                Pattern::MatchValue(node) => {
                    assert!(matches!(node, AstNode::Literal { value: LiteralValue::Integer(2), .. }));
                }
                _ => panic!("Expected MatchValue pattern for second case"),
            }

            assert_eq!(cases[0].body.len(), 1);
            assert_eq!(cases[1].body.len(), 1);
            assert!(cases[0].guard.is_none());
            assert!(cases[1].guard.is_none());
        }
        _ => panic!("Expected Match statement"),
    }
}

#[test]
fn test_match_with_sequence_pattern() {
    let source = r#"match point:
    case [x, y]:
        distance = (x**2 + y**2) ** 0.5"#;

    let ast = parse_to_ast(source);
    match extract_first_stmt(ast) {
        AstNode::Match { subject, cases, .. } => {
            assert!(matches!(*subject, AstNode::Identifier { name, .. } if name == "point"));
            assert_eq!(cases.len(), 1);

            match &cases[0].pattern {
                Pattern::MatchSequence(patterns) => {
                    assert_eq!(patterns.len(), 2);
                }
                _ => panic!("Expected MatchSequence pattern"),
            }

            assert_eq!(cases[0].body.len(), 1);
            assert!(cases[0].guard.is_none());
        }
        _ => panic!("Expected Match statement"),
    }
}

#[test]
fn test_pattern_matching_with_guards() {
    let source = r#"match value:
    case x if x > 0:
        result = "positive"
    case x if x < 0:
        result = "negative""#;

    let ast = parse_to_ast(source);
    match extract_first_stmt(ast) {
        AstNode::Match { cases, .. } => {
            assert_eq!(cases.len(), 2);

            assert!(cases[0].guard.is_some(), "First case should have a guard");
            assert!(cases[1].guard.is_some(), "Second case should have a guard");

            match &cases[0].guard {
                Some(guard) => {
                    assert!(matches!(guard, AstNode::Compare { .. }));
                }
                None => panic!("Expected guard on first case"),
            }

            match &cases[1].guard {
                Some(guard) => {
                    assert!(matches!(guard, AstNode::Compare { .. }));
                }
                None => panic!("Expected guard on second case"),
            }
        }
        _ => panic!("Expected Match statement"),
    }
}

#[test]
fn test_nested_patterns() {
    let source = r#"match data:
    case [1, [2, 3]]:
        result = "nested"
    case [[a, b], [c, d]]:
        result = "matrix""#;

    let ast = parse_to_ast(source);
    match extract_first_stmt(ast) {
        AstNode::Match { cases, .. } => {
            assert_eq!(cases.len(), 2);

            match &cases[0].pattern {
                Pattern::MatchSequence(patterns) => {
                    assert_eq!(patterns.len(), 2);
                    match &patterns[1] {
                        Pattern::MatchSequence(inner) => {
                            assert_eq!(inner.len(), 2, "Nested sequence should have 2 elements");
                        }
                        _ => panic!("Expected nested MatchSequence pattern"),
                    }
                }
                _ => panic!("Expected MatchSequence for first case"),
            }

            match &cases[1].pattern {
                Pattern::MatchSequence(patterns) => {
                    assert_eq!(patterns.len(), 2);
                    match &patterns[0] {
                        Pattern::MatchSequence(inner) => {
                            assert_eq!(inner.len(), 2);
                        }
                        _ => panic!("Expected nested MatchSequence"),
                    }
                    match &patterns[1] {
                        Pattern::MatchSequence(inner) => {
                            assert_eq!(inner.len(), 2);
                        }
                        _ => panic!("Expected nested MatchSequence"),
                    }
                }
                _ => panic!("Expected MatchSequence for second case"),
            }
        }
        _ => panic!("Expected Match statement"),
    }
}

#[test]
fn test_multiple_match_cases() {
    let source = r#"match command:
    case "quit":
        exit()
    case "help":
        show_help()
    case "start":
        start_game()
    case _:
        unknown_command()"#;

    let ast = parse_to_ast(source);
    match extract_first_stmt(ast) {
        AstNode::Match { cases, .. } => {
            assert_eq!(cases.len(), 4, "Should have four cases");

            for (i, case) in cases[0..3].iter().enumerate() {
                match &case.pattern {
                    Pattern::MatchValue(node) => {
                        assert!(matches!(
                            node,
                            AstNode::Literal { value: LiteralValue::String { .. }, .. }
                        ));
                    }
                    _ => panic!("Case {i} should have MatchValue pattern"),
                }
                assert_eq!(case.body.len(), 1);
            }

            match &cases[3].pattern {
                Pattern::MatchAs { pattern, name } => {
                    assert!(pattern.is_none(), "Wildcard should have no sub-pattern");
                    assert!(name.is_none(), "Wildcard _ should have no binding");
                }
                _ => panic!("Last case should be wildcard MatchAs"),
            }
        }
        _ => panic!("Expected Match statement"),
    }
}

#[test]
fn test_match_with_or_patterns() {
    let source = r#"match value:
    case 1 | 2 | 3:
        result = "small number""#;

    let ast = parse_to_ast(source);
    match extract_first_stmt(ast) {
        AstNode::Match { cases, .. } => {
            assert_eq!(cases.len(), 1);

            match &cases[0].pattern {
                Pattern::MatchOr(patterns) => {
                    assert_eq!(patterns.len(), 3, "Or pattern should have 3 alternatives");
                }
                _ => panic!("Expected MatchOr pattern"),
            }
        }
        _ => panic!("Expected Match statement"),
    }
}

#[test]
fn test_match_with_mapping_pattern() {
    let source = r#"match config:
    case {"mode": "debug"}:
        enable_debug()"#;

    let ast = parse_to_ast(source);
    match extract_first_stmt(ast) {
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
    }
}

#[test]
fn test_match_with_capture_pattern() {
    let source = r#"match value:
    case 1:
        result = "one"
    case x:
        result = x"#;

    let ast = parse_to_ast(source);
    match extract_first_stmt(ast) {
        AstNode::Match { cases, .. } => {
            assert_eq!(cases.len(), 2);

            match &cases[0].pattern {
                Pattern::MatchValue(_) => {}
                _ => panic!("First case should be MatchValue"),
            }

            match &cases[1].pattern {
                Pattern::MatchAs { pattern, name } => {
                    assert!(pattern.is_none(), "Capture pattern should have no sub-pattern");
                    assert_eq!(name.as_deref(), Some("x"), "Should capture to variable 'x'");
                }
                _ => panic!("Second case should be capture MatchAs"),
            }
        }
        _ => panic!("Expected Match statement"),
    }
}

#[test]
fn test_match_with_as_pattern() {
    let source = r#"match point:
    case [x, y] as p:
        distance = (x**2 + y**2) ** 0.5"#;

    let ast = parse_to_ast(source);
    match extract_first_stmt(ast) {
        AstNode::Match { cases, .. } => {
            assert_eq!(cases.len(), 1);

            match &cases[0].pattern {
                Pattern::MatchAs { pattern, name } => {
                    assert!(pattern.is_some(), "Should have sub-pattern");
                    assert_eq!(name.as_deref(), Some("p"), "Should bind to 'p'");

                    match pattern.as_deref() {
                        Some(Pattern::MatchSequence(patterns)) => {
                            assert_eq!(patterns.len(), 2);
                        }
                        _ => panic!("Sub-pattern should be MatchSequence"),
                    }
                }
                _ => panic!("Expected MatchAs pattern"),
            }
        }
        _ => panic!("Expected Match statement"),
    }
}

#[test]
fn test_complex_match_with_guards_and_nested_patterns() {
    let source = r#"match data:
    case [x, y] if x > y:
        result = "first larger"
    case [x, [y, z]] if y == z:
        result = "nested equal"
    case _:
        result = "default""#;

    let ast = parse_to_ast(source);
    match extract_first_stmt(ast) {
        AstNode::Match { cases, .. } => {
            assert_eq!(cases.len(), 3);

            match &cases[0].pattern {
                Pattern::MatchSequence(patterns) => {
                    assert_eq!(patterns.len(), 2);
                }
                _ => panic!("First case should be MatchSequence"),
            }
            assert!(cases[0].guard.is_some(), "First case should have guard");

            match &cases[1].pattern {
                Pattern::MatchSequence(patterns) => {
                    assert_eq!(patterns.len(), 2);
                    match &patterns[1] {
                        Pattern::MatchSequence(inner) => {
                            assert_eq!(inner.len(), 2);
                        }
                        _ => panic!("Second element should be nested MatchSequence"),
                    }
                }
                _ => panic!("Second case should be MatchSequence"),
            }
            assert!(cases[1].guard.is_some(), "Second case should have guard");

            match &cases[2].pattern {
                Pattern::MatchAs { pattern, name } => {
                    assert!(pattern.is_none());
                    assert!(name.is_none());
                }
                _ => panic!("Third case should be wildcard"),
            }
            assert!(cases[2].guard.is_none(), "Wildcard case should not have guard");
        }
        _ => panic!("Expected Match statement"),
    }
}

// ============================================================================
// Type Annotations
// ============================================================================

#[test]
fn test_function_parameter_annotations_simple() {
    let source = r#"def greet(name: str, age: int):
    pass"#;

    let ast = parse_to_ast(source);
    match extract_first_stmt(ast) {
        AstNode::FunctionDef { args, .. } => {
            assert_eq!(args.len(), 2);

            assert_eq!(args[0].name, "name");
            assert_eq!(args[0].type_annotation, Some("str".to_string()));

            assert_eq!(args[1].name, "age");
            assert_eq!(args[1].type_annotation, Some("int".to_string()));
        }
        _ => panic!("Expected FunctionDef"),
    }
}

#[test]
fn test_function_parameter_annotations_generic() {
    let source = r#"def process(items: List[str], mapping: Dict[str, int]):
    pass"#;

    let ast = parse_to_ast(source);
    match extract_first_stmt(ast) {
        AstNode::FunctionDef { args, .. } => {
            assert_eq!(args.len(), 2);

            assert_eq!(args[0].name, "items");
            assert_eq!(args[0].type_annotation, Some("List[str]".to_string()));

            assert_eq!(args[1].name, "mapping");
            assert_eq!(args[1].type_annotation, Some("Dict[str, int]".to_string()));
        }
        _ => panic!("Expected FunctionDef"),
    }
}

#[test]
fn test_function_parameter_annotations_mixed() {
    let source = r#"def func(a: int, b, c: str = "default"):
    pass"#;

    let ast = parse_to_ast(source);
    match extract_first_stmt(ast) {
        AstNode::FunctionDef { args, .. } => {
            assert_eq!(args.len(), 3);

            assert_eq!(args[0].name, "a");
            assert_eq!(args[0].type_annotation, Some("int".to_string()));
            assert!(args[0].default_value.is_none());

            assert_eq!(args[1].name, "b");
            assert_eq!(args[1].type_annotation, None, "Parameter b should not have annotation");
            assert!(args[1].default_value.is_none());

            assert_eq!(args[2].name, "c");
            assert_eq!(args[2].type_annotation, Some("str".to_string()));
            assert!(args[2].default_value.is_some(), "Parameter c should have default value");
        }
        _ => panic!("Expected FunctionDef"),
    }
}

#[test]
fn test_return_type_annotation_simple() {
    let source = r#"def calculate() -> int:
    return 42"#;

    let ast = parse_to_ast(source);
    match extract_first_stmt(ast) {
        AstNode::FunctionDef { return_type, .. } => {
            assert_eq!(return_type, Some("int".to_string()));
        }
        _ => panic!("Expected FunctionDef"),
    }
}

#[test]
fn test_return_type_annotation_generic() {
    let source = r#"def get_items() -> List[int]:
    return [1, 2, 3]"#;

    let ast = parse_to_ast(source);
    match extract_first_stmt(ast) {
        AstNode::FunctionDef { return_type, .. } => {
            assert_eq!(return_type, Some("List[int]".to_string()));
        }
        _ => panic!("Expected FunctionDef"),
    }
}

#[test]
fn test_return_type_annotation_complex() {
    let source = r#"def get_mapping() -> Dict[str, List[int]]:
    return {}"#;

    let ast = parse_to_ast(source);
    match extract_first_stmt(ast) {
        AstNode::FunctionDef { return_type, .. } => {
            assert_eq!(return_type, Some("Dict[str, List[int]]".to_string()));
        }
        _ => panic!("Expected FunctionDef"),
    }
}

#[test]
fn test_return_type_annotation_pep585() {
    let source = r#"def modern_syntax() -> list[str]:
    return []"#;

    let ast = parse_to_ast(source);
    match extract_first_stmt(ast) {
        AstNode::FunctionDef { return_type, .. } => {
            assert_eq!(return_type, Some("list[str]".to_string()));
        }
        _ => panic!("Expected FunctionDef"),
    }
}

#[test]
fn test_function_full_annotations() {
    let source = r#"def process(data: List[str], count: int = 10) -> Dict[str, int]:
    return {}"#;

    let ast = parse_to_ast(source);
    match extract_first_stmt(ast) {
        AstNode::FunctionDef { name, args, return_type, .. } => {
            assert_eq!(name, "process");
            assert_eq!(args.len(), 2);

            assert_eq!(args[0].name, "data");
            assert_eq!(args[0].type_annotation, Some("List[str]".to_string()));
            assert!(args[0].default_value.is_none());

            assert_eq!(args[1].name, "count");
            assert_eq!(args[1].type_annotation, Some("int".to_string()));
            assert!(args[1].default_value.is_some());

            assert_eq!(return_type, Some("Dict[str, int]".to_string()));
        }
        _ => panic!("Expected FunctionDef"),
    }
}

#[test]
fn test_generic_types_with_single_parameter() {
    let source = r#"def func(items: List[int]):
    pass"#;

    let ast = parse_to_ast(source);
    match extract_first_stmt(ast) {
        AstNode::FunctionDef { args, .. } => {
            assert_eq!(args[0].type_annotation, Some("List[int]".to_string()));
        }
        _ => panic!("Expected FunctionDef"),
    }
}

#[test]
fn test_generic_types_with_multiple_parameters() {
    let source = r#"def func(data: Dict[str, int]):
    pass"#;

    let ast = parse_to_ast(source);
    match extract_first_stmt(ast) {
        AstNode::FunctionDef { args, .. } => {
            assert_eq!(args[0].type_annotation, Some("Dict[str, int]".to_string()));
        }
        _ => panic!("Expected FunctionDef"),
    }
}

#[test]
fn test_generic_types_nested() {
    let source = r#"def func(data: List[Dict[str, int]]):
    pass"#;

    let ast = parse_to_ast(source);
    match extract_first_stmt(ast) {
        AstNode::FunctionDef { args, .. } => {
            assert_eq!(args[0].type_annotation, Some("List[Dict[str, int]]".to_string()));
        }
        _ => panic!("Expected FunctionDef"),
    }
}

#[test]
fn test_generic_types_pep585_lowercase() {
    let source = r#"def modern(data: list[int], mapping: dict[str, int]):
    pass"#;

    let ast = parse_to_ast(source);
    match extract_first_stmt(ast) {
        AstNode::FunctionDef { args, .. } => {
            assert_eq!(args.len(), 2);
            assert_eq!(args[0].type_annotation, Some("list[int]".to_string()));
            assert_eq!(args[1].type_annotation, Some("dict[str, int]".to_string()));
        }
        _ => panic!("Expected FunctionDef"),
    }
}

#[test]
fn test_typevar_simple_declaration() {
    let source = r#"T = TypeVar("T")"#;

    let ast = parse_to_ast(source);
    match extract_first_stmt(ast) {
        AstNode::Assignment { target, value, .. } => {
            assert!(matches!(*target, AstNode::Identifier { name, .. } if name == "T"));
            match *value {
                AstNode::Call { function, args, .. } => {
                    assert!(matches!(*function, AstNode::Identifier { name, .. } if name == "TypeVar"));
                    assert_eq!(args.len(), 1);
                    assert!(matches!(
                        args[0],
                        AstNode::Literal { value: LiteralValue::String { value: ref s, .. }, .. } if s == "T"
                    ));
                }
                _ => panic!("Expected Call to TypeVar"),
            }
        }
        _ => panic!("Expected Assignment"),
    }
}

#[test]
fn test_typevar_constrained() {
    let source = r#"T = TypeVar("T", int, str)"#;

    let ast = parse_to_ast(source);
    match extract_first_stmt(ast) {
        AstNode::Assignment { target, value, .. } => {
            assert!(matches!(*target, AstNode::Identifier { name, .. } if name == "T"));
            match *value {
                AstNode::Call { function, args, .. } => {
                    assert!(matches!(*function, AstNode::Identifier { name, .. } if name == "TypeVar"));
                    assert_eq!(args.len(), 3, "Should have name + 2 constraints");
                }
                _ => panic!("Expected Call to TypeVar"),
            }
        }
        _ => panic!("Expected Assignment"),
    }
}

#[test]
fn test_typevar_with_bound() {
    let source = r#"T = TypeVar("T", bound=BaseClass)"#;

    let ast = parse_to_ast(source);
    match extract_first_stmt(ast) {
        AstNode::Assignment { target, value, .. } => {
            assert!(matches!(*target, AstNode::Identifier { name, .. } if name == "T"));
            match *value {
                AstNode::Call { function, args, keywords, .. } => {
                    assert!(matches!(*function, AstNode::Identifier { name, .. } if name == "TypeVar"));
                    assert_eq!(args.len(), 1, "Should have just the name as positional arg");
                    assert_eq!(keywords.len(), 1, "Should have one keyword argument");
                    assert_eq!(keywords[0].0, "bound", "Keyword should be 'bound'");
                    assert!(matches!(keywords[0].1, AstNode::Identifier { name: ref n, .. } if n == "BaseClass"));
                }
                _ => panic!("Expected Call to TypeVar"),
            }
        }
        _ => panic!("Expected Assignment"),
    }
}

#[test]
fn test_variable_annotation() {
    let source = r#"x: int = 5"#;

    let ast = parse_to_ast(source);
    match extract_first_stmt(ast) {
        AstNode::AnnotatedAssignment { target, type_annotation, value, .. } => {
            assert!(matches!(*target, AstNode::Identifier { name, .. } if name == "x"));
            assert_eq!(type_annotation, "int");
            assert!(value.is_some());
            match value.unwrap().as_ref() {
                AstNode::Literal { value: LiteralValue::Integer(5), .. } => {}
                _ => panic!("Expected Integer literal with value 5"),
            }
        }
        _ => panic!("Expected AnnotatedAssignment"),
    }
}

#[test]
fn test_variable_annotation_without_value() {
    let source = r#"y: str"#;

    let ast = parse_to_ast(source);
    match extract_first_stmt(ast) {
        AstNode::AnnotatedAssignment { target, type_annotation, value, .. } => {
            assert!(matches!(*target, AstNode::Identifier { name, .. } if name == "y"));
            assert_eq!(type_annotation, "str");
            assert!(value.is_none(), "Should have no initial value");
        }
        _ => panic!("Expected AnnotatedAssignment"),
    }
}

#[test]
fn test_variable_annotation_generic() {
    let source = r#"items: List[int] = []"#;

    let ast = parse_to_ast(source);
    match extract_first_stmt(ast) {
        AstNode::AnnotatedAssignment { type_annotation, .. } => {
            assert_eq!(type_annotation, "List[int]");
        }
        _ => panic!("Expected AnnotatedAssignment"),
    }
}

use beacon_parser::{AstNode, LiteralValue, PythonParser};

fn parse_and_get_first_assignment_value(source: &str) -> LiteralValue {
    let mut parser = PythonParser::new().unwrap();
    let (ast, _) = parser.parse_and_resolve(source).unwrap();

    match ast {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::Assignment { value, .. } => match value.as_ref() {
                AstNode::Literal { value, .. } => value.clone(),
                _ => panic!("Expected literal value"),
            },
            _ => panic!("Expected assignment"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_decimal_integers() {
    assert!(matches!(
        parse_and_get_first_assignment_value("x = 0"),
        LiteralValue::Integer(0)
    ));
    assert!(matches!(
        parse_and_get_first_assignment_value("x = 123"),
        LiteralValue::Integer(123)
    ));
    assert!(matches!(
        parse_and_get_first_assignment_value("x = 2147483647"),
        LiteralValue::Integer(2147483647)
    ));
}

#[test]
fn test_hexadecimal_integers() {
    assert!(matches!(
        parse_and_get_first_assignment_value("x = 0xff"),
        LiteralValue::Integer(255)
    ));
    assert!(matches!(
        parse_and_get_first_assignment_value("x = 0xFF"),
        LiteralValue::Integer(255)
    ));
    assert!(matches!(
        parse_and_get_first_assignment_value("x = 0xffffffff"),
        LiteralValue::Integer(4294967295)
    ));

    let val = parse_and_get_first_assignment_value("x = 0Xffffffffffffffff");
    assert!(matches!(val, LiteralValue::Integer(_)));
}

#[test]
fn test_octal_integers() {
    assert!(matches!(
        parse_and_get_first_assignment_value("x = 0o377"),
        LiteralValue::Integer(255)
    ));
    assert!(matches!(
        parse_and_get_first_assignment_value("x = 0O777"),
        LiteralValue::Integer(511)
    ));
    assert!(matches!(
        parse_and_get_first_assignment_value("x = 0o17777777777"),
        LiteralValue::Integer(2147483647)
    ));
}

#[test]
fn test_binary_integers() {
    assert!(matches!(
        parse_and_get_first_assignment_value("x = 0b1001"),
        LiteralValue::Integer(9)
    ));
    assert!(matches!(
        parse_and_get_first_assignment_value("x = 0B11111111"),
        LiteralValue::Integer(255)
    ));
}

#[test]
fn test_integers_with_underscores() {
    assert!(matches!(
        parse_and_get_first_assignment_value("x = 1_0"),
        LiteralValue::Integer(10)
    ));
    assert!(matches!(
        parse_and_get_first_assignment_value("x = 123_456_789"),
        LiteralValue::Integer(123456789)
    ));
    assert!(matches!(
        parse_and_get_first_assignment_value("x = 0xabc_123_4_5"),
        LiteralValue::Integer(0xabc12345)
    ));
    assert!(matches!(
        parse_and_get_first_assignment_value("x = 0X_abc_123"),
        LiteralValue::Integer(0xabc123)
    ));
    assert!(matches!(
        parse_and_get_first_assignment_value("x = 0B11_01"),
        LiteralValue::Integer(0b1101)
    ));
    assert!(matches!(
        parse_and_get_first_assignment_value("x = 0b_11_01"),
        LiteralValue::Integer(0b1101)
    ));
    assert!(matches!(
        parse_and_get_first_assignment_value("x = 0o45_67"),
        LiteralValue::Integer(0o4567)
    ));
    assert!(matches!(
        parse_and_get_first_assignment_value("x = 0O_45_67"),
        LiteralValue::Integer(0o4567)
    ));
}

#[test]
fn test_large_integers_clamped() {
    let val = parse_and_get_first_assignment_value("x = 123456789012345678901234567890");
    assert!(matches!(val, LiteralValue::Integer(_)));
    if let LiteralValue::Integer(i) = val {
        assert_eq!(i, i64::MAX, "Very large integer should clamp to i64::MAX");
    }

    let val = parse_and_get_first_assignment_value("x = 0o1777777777777777777777");
    assert!(matches!(val, LiteralValue::Integer(_)));
    if let LiteralValue::Integer(i) = val {
        assert_eq!(i, i64::MAX, "Very large octal should clamp to i64::MAX");
    }
}

#[test]
fn test_floats_basic() {
    assert!(matches!(
        parse_and_get_first_assignment_value("x = 3.14"),
        LiteralValue::Float(_)
    ));
    assert!(matches!(
        parse_and_get_first_assignment_value("x = 314."),
        LiteralValue::Float(_)
    ));
    assert!(matches!(
        parse_and_get_first_assignment_value("x = 0.314"),
        LiteralValue::Float(_)
    ));
    assert!(matches!(
        parse_and_get_first_assignment_value("x = .314"),
        LiteralValue::Float(_)
    ));
}

#[test]
fn test_floats_with_exponent() {
    assert!(matches!(
        parse_and_get_first_assignment_value("x = 3e14"),
        LiteralValue::Float(_)
    ));
    assert!(matches!(
        parse_and_get_first_assignment_value("x = 3E14"),
        LiteralValue::Float(_)
    ));
    assert!(matches!(
        parse_and_get_first_assignment_value("x = 3e-14"),
        LiteralValue::Float(_)
    ));
    assert!(matches!(
        parse_and_get_first_assignment_value("x = 3e+14"),
        LiteralValue::Float(_)
    ));
    assert!(matches!(
        parse_and_get_first_assignment_value("x = 3.e14"),
        LiteralValue::Float(_)
    ));
    assert!(matches!(
        parse_and_get_first_assignment_value("x = .3e14"),
        LiteralValue::Float(_)
    ));
    assert!(matches!(
        parse_and_get_first_assignment_value("x = 3.1e4"),
        LiteralValue::Float(_)
    ));
}

#[test]
fn test_floats_with_underscores() {
    assert!(matches!(
        parse_and_get_first_assignment_value("x = 3_1.4"),
        LiteralValue::Float(_)
    ));
    assert!(matches!(
        parse_and_get_first_assignment_value("x = 03_1.4"),
        LiteralValue::Float(_)
    ));
    assert!(matches!(
        parse_and_get_first_assignment_value("x = 3_1."),
        LiteralValue::Float(_)
    ));
    assert!(matches!(
        parse_and_get_first_assignment_value("x = .3_1"),
        LiteralValue::Float(_)
    ));
    assert!(matches!(
        parse_and_get_first_assignment_value("x = 3.1_4"),
        LiteralValue::Float(_)
    ));
    assert!(matches!(
        parse_and_get_first_assignment_value("x = 0_3.1_4"),
        LiteralValue::Float(_)
    ));
    assert!(matches!(
        parse_and_get_first_assignment_value("x = 3e1_4"),
        LiteralValue::Float(_)
    ));
    assert!(matches!(
        parse_and_get_first_assignment_value("x = 3_1e+4_1"),
        LiteralValue::Float(_)
    ));
    assert!(matches!(
        parse_and_get_first_assignment_value("x = 3_1E-4_1"),
        LiteralValue::Float(_)
    ));
}

#[test]
fn test_boolean_literals() {
    assert!(matches!(
        parse_and_get_first_assignment_value("x = True"),
        LiteralValue::Boolean(true)
    ));
    assert!(matches!(
        parse_and_get_first_assignment_value("x = False"),
        LiteralValue::Boolean(false)
    ));
}

#[test]
fn test_none_literal() {
    assert!(matches!(
        parse_and_get_first_assignment_value("x = None"),
        LiteralValue::None
    ));
}

#[test]
fn test_string_literals() {
    assert!(matches!(
        parse_and_get_first_assignment_value("x = 'hello'"),
        LiteralValue::String { .. }
    ));
    assert!(matches!(
        parse_and_get_first_assignment_value(r#"x = "world""#),
        LiteralValue::String { .. }
    ));
    assert!(matches!(
        parse_and_get_first_assignment_value(r#"x = r'raw'"#),
        LiteralValue::String { .. }
    ));
    assert!(matches!(
        parse_and_get_first_assignment_value(r#"x = b'bytes'"#),
        LiteralValue::String { .. }
    ));
}

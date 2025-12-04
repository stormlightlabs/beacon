use beacon_parser::{AstNode, LiteralValue, PythonParser};

#[test]
fn test_decimal_integers() {
    let mut parser = PythonParser::new().unwrap();
    let source = "x = 123456789";
    let (ast, _) = parser.parse_and_resolve(source).unwrap();

    match ast {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::Assignment { value, .. } => match value.as_ref() {
                AstNode::Literal { value: LiteralValue::Integer(123456789), .. } => {}
                _ => panic!("Expected integer 123456789"),
            },
            _ => panic!("Expected assignment"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_hexadecimal_integers() {
    let mut parser = PythonParser::new().unwrap();

    let source = "x = 0xff";
    let (ast, _) = parser.parse_and_resolve(source).unwrap();
    match ast {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::Assignment { value, .. } => match value.as_ref() {
                AstNode::Literal { value: LiteralValue::Integer(255), .. } => {}
                _ => panic!("Expected integer 255"),
            },
            _ => panic!("Expected assignment"),
        },
        _ => panic!("Expected module"),
    }

    let source = "x = 0Xffffffff";
    let (ast, _) = parser.parse_and_resolve(source).unwrap();
    match ast {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::Assignment { value, .. } => match value.as_ref() {
                AstNode::Literal { value: LiteralValue::Integer(4294967295), .. } => {}
                v => panic!("Expected integer 4294967295, got {:?}", v),
            },
            _ => panic!("Expected assignment"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_octal_integers() {
    let mut parser = PythonParser::new().unwrap();

    let source = "x = 0o377";
    let (ast, _) = parser.parse_and_resolve(source).unwrap();
    match ast {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::Assignment { value, .. } => match value.as_ref() {
                AstNode::Literal { value: LiteralValue::Integer(255), .. } => {}
                _ => panic!("Expected integer 255"),
            },
            _ => panic!("Expected assignment"),
        },
        _ => panic!("Expected module"),
    }

    let source = "x = 0O777";
    let (ast, _) = parser.parse_and_resolve(source).unwrap();
    match ast {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::Assignment { value, .. } => match value.as_ref() {
                AstNode::Literal { value: LiteralValue::Integer(511), .. } => {}
                v => panic!("Expected integer 511, got {:?}", v),
            },
            _ => panic!("Expected assignment"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_binary_integers() {
    let mut parser = PythonParser::new().unwrap();

    let source = "x = 0b1001";
    let (ast, _) = parser.parse_and_resolve(source).unwrap();
    match ast {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::Assignment { value, .. } => match value.as_ref() {
                AstNode::Literal { value: LiteralValue::Integer(9), .. } => {}
                _ => panic!("Expected integer 9"),
            },
            _ => panic!("Expected assignment"),
        },
        _ => panic!("Expected module"),
    }

    let source = "x = 0B11111111";
    let (ast, _) = parser.parse_and_resolve(source).unwrap();
    match ast {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::Assignment { value, .. } => match value.as_ref() {
                AstNode::Literal { value: LiteralValue::Integer(255), .. } => {}
                v => panic!("Expected integer 255, got {:?}", v),
            },
            _ => panic!("Expected assignment"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_integers_with_underscores() {
    let mut parser = PythonParser::new().unwrap();

    let source = "x = 123_456_789";
    let (ast, _) = parser.parse_and_resolve(source).unwrap();
    match ast {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::Assignment { value, .. } => match value.as_ref() {
                AstNode::Literal { value: LiteralValue::Integer(123456789), .. } => {}
                v => panic!("Expected integer 123456789, got {:?}", v),
            },
            _ => panic!("Expected assignment"),
        },
        _ => panic!("Expected module"),
    }

    let source = "x = 0xabc_123";
    let (ast, _) = parser.parse_and_resolve(source).unwrap();
    match ast {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::Assignment { value, .. } => match value.as_ref() {
                AstNode::Literal { value: LiteralValue::Integer(11256099), .. } => {}
                v => panic!("Expected integer 11256099, got {:?}", v),
            },
            _ => panic!("Expected assignment"),
        },
        _ => panic!("Expected module"),
    }

    let source = "x = 0o45_67";
    let (ast, _) = parser.parse_and_resolve(source).unwrap();
    match ast {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::Assignment { value, .. } => match value.as_ref() {
                AstNode::Literal { value: LiteralValue::Integer(2423), .. } => {}
                v => panic!("Expected integer 2423, got {:?}", v),
            },
            _ => panic!("Expected assignment"),
        },
        _ => panic!("Expected module"),
    }

    let source = "x = 0b11_01";
    let (ast, _) = parser.parse_and_resolve(source).unwrap();
    match ast {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::Assignment { value, .. } => match value.as_ref() {
                AstNode::Literal { value: LiteralValue::Integer(13), .. } => {}
                v => panic!("Expected integer 13, got {:?}", v),
            },
            _ => panic!("Expected assignment"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_large_integers() {
    let mut parser = PythonParser::new().unwrap();

    let source = "x = 9223372036854775807";
    let (ast, _) = parser.parse_and_resolve(source).unwrap();
    match ast {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::Assignment { value, .. } => match value.as_ref() {
                AstNode::Literal { value: LiteralValue::Integer(9223372036854775807), .. } => {}
                v => panic!("Expected i64::MAX, got {:?}", v),
            },
            _ => panic!("Expected assignment"),
        },
        _ => panic!("Expected module"),
    }

    let source = "x = 0x7fffffffffffffff";
    let (ast, _) = parser.parse_and_resolve(source).unwrap();
    match ast {
        AstNode::Module { body, .. } => match &body[0] {
            AstNode::Assignment { value, .. } => match value.as_ref() {
                AstNode::Literal { value: LiteralValue::Integer(9223372036854775807), .. } => {}
                v => panic!("Expected i64::MAX, got {:?}", v),
            },
            _ => panic!("Expected assignment"),
        },
        _ => panic!("Expected module"),
    }
}

//! Python Type Annotation Parser
//!
//! Parses PEP 484/585 type annotation strings into Type structures.
//!
//! Supports:
//! - Basic types: int, str, bool, float, None
//! - Parameterized types: list[int], dict[str, int]
//! - Union types: Union[int, str], int | str (PEP 604)
//! - Intersection types: Protocol1 & Protocol2
//! - Optional: Optional[T] → Union[T, None]
//! - Callable: Callable[[args...], return]
//! - Any, Never, Top
//!
//! ## Examples
//!
//! ```
//! use beacon_core::AnnotationParser;
//!
//! let parser = AnnotationParser::new();
//! let ty = parser.parse("list[int]").unwrap();
//! let intersection = parser.parse("Iterable & Sized").unwrap();
//! ```

use crate::{Result, Type, TypeCtor, TypeError};
use std::iter::Peekable;
use std::str::Chars;

/// Parser for Python type annotations
#[derive(Debug, Clone, Copy)]
pub struct AnnotationParser;

impl AnnotationParser {
    /// Create a new annotation parser
    pub fn new() -> Self {
        Self
    }

    /// Parse a type annotation string into a Type
    pub fn parse(&self, annotation: &str) -> Result<Type> {
        let trimmed = annotation.trim();
        if trimmed.is_empty() {
            return Err(
                TypeError::UnificationError("non-empty annotation".to_string(), "empty string".to_string()).into(),
            );
        }

        let mut lexer = Lexer::new(trimmed);
        let tokens = lexer.tokenize()?;
        let mut parser = Parser::new(tokens);
        let ty = parser.parse_type()?;

        if parser.pos < parser.tokens.len() {
            Err(TypeError::UnificationError(
                "end of input".to_string(),
                format!("unexpected tokens: {:?}", &parser.tokens[parser.pos..]),
            )
            .into())
        } else {
            Ok(ty)
        }
    }

    /// Parse a type annotation, returning Any if parsing fails
    pub fn parse_or_any(&self, annotation: &str) -> Type {
        self.parse(annotation).unwrap_or(Type::any())
    }
}

impl Default for AnnotationParser {
    fn default() -> Self {
        Self::new()
    }
}

/// Tokens for type annotation lexer
#[derive(Debug, Clone, PartialEq)]
enum Token {
    /// Identifier (type name, type variable)
    Ident(String),
    /// [
    LBracket,
    /// ]
    RBracket,
    /// (
    LParen,
    /// )
    RParen,
    /// ,
    Comma,
    /// |
    Pipe,
    /// &
    Ampersand,
    /// ...
    Ellipsis,
}

/// Lexer for type annotations
struct Lexer<'a> {
    chars: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    fn new(input: &'a str) -> Self {
        Self { chars: input.chars().peekable() }
    }

    fn tokenize(&mut self) -> Result<Vec<Token>> {
        let mut tokens = Vec::new();

        while let Some(&ch) = self.chars.peek() {
            match ch {
                ' ' | '\t' | '\n' | '\r' => {
                    self.chars.next();
                }
                '[' => {
                    tokens.push(Token::LBracket);
                    self.chars.next();
                }
                ']' => {
                    tokens.push(Token::RBracket);
                    self.chars.next();
                }
                '(' => {
                    tokens.push(Token::LParen);
                    self.chars.next();
                }
                ')' => {
                    tokens.push(Token::RParen);
                    self.chars.next();
                }
                ',' => {
                    tokens.push(Token::Comma);
                    self.chars.next();
                }
                '|' => {
                    tokens.push(Token::Pipe);
                    self.chars.next();
                }
                '&' => {
                    tokens.push(Token::Ampersand);
                    self.chars.next();
                }
                '.' => {
                    self.chars.next();
                    if self.chars.peek() == Some(&'.') {
                        self.chars.next();
                        if self.chars.peek() == Some(&'.') {
                            self.chars.next();
                            tokens.push(Token::Ellipsis);
                        } else {
                            return Err(TypeError::UnificationError("valid token".to_string(), "..".to_string()).into());
                        }
                    } else {
                        return Err(TypeError::UnificationError("valid token".to_string(), ".".to_string()).into());
                    }
                }
                ch if ch.is_alphanumeric() || ch == '_' => tokens.push(self.read_identifier()),
                _ => {
                    return Err(TypeError::UnificationError(
                        "valid type annotation".to_string(),
                        format!("unexpected character '{ch}'"),
                    )
                    .into());
                }
            }
        }

        Ok(tokens)
    }

    fn read_identifier(&mut self) -> Token {
        let mut ident = String::new();

        while let Some(&ch) = self.chars.peek() {
            if ch.is_alphanumeric() || ch == '_' {
                ident.push(ch);
                self.chars.next();
            } else {
                break;
            }
        }

        Token::Ident(ident)
    }
}

/// Parser for type annotation tokens
struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, pos: 0 }
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.pos)
    }

    fn advance(&mut self) -> Option<Token> {
        if self.pos < self.tokens.len() {
            let token = self.tokens[self.pos].clone();
            self.pos += 1;
            Some(token)
        } else {
            None
        }
    }

    fn expect(&mut self, expected: Token) -> Result<()> {
        match self.advance() {
            Some(token) if token == expected => Ok(()),
            Some(token) => Err(TypeError::UnificationError(format!("{expected:?}"), format!("{token:?}")).into()),
            None => Err(TypeError::UnificationError(format!("{expected:?}"), "end of input".to_string()).into()),
        }
    }

    /// Parse a type annotation
    /// Precedence: | (union) < & (intersection)
    fn parse_type(&mut self) -> Result<Type> {
        let mut ty = self.parse_intersection_type()?;

        while matches!(self.peek(), Some(Token::Pipe)) {
            self.advance();
            let right = self.parse_intersection_type()?;
            ty = match ty {
                Type::Union(mut types) => {
                    types.push(right);
                    Type::union(types)
                }
                _ => Type::union(vec![ty, right]),
            };
        }

        Ok(ty)
    }

    /// Parse intersection types (higher precedence than union)
    fn parse_intersection_type(&mut self) -> Result<Type> {
        let mut ty = self.parse_primary_type()?;

        while matches!(self.peek(), Some(Token::Ampersand)) {
            self.advance();
            let right = self.parse_primary_type()?;
            ty = match ty {
                Type::Intersection(mut types) => {
                    types.push(right);
                    Type::intersection(types)
                }
                _ => Type::intersection(vec![ty, right]),
            };
        }

        Ok(ty)
    }

    /// Parse a primary type (non-union)
    fn parse_primary_type(&mut self) -> Result<Type> {
        match self.advance() {
            Some(Token::Ident(name)) => self.parse_named_type(&name),
            Some(Token::LParen) => {
                let ty = self.parse_type()?;
                self.expect(Token::RParen)?;
                Ok(ty)
            }
            Some(token) => Err(TypeError::UnificationError("type name".to_string(), format!("{token:?}")).into()),
            None => Err(TypeError::UnificationError("type name".to_string(), "end of input".to_string()).into()),
        }
    }

    /// Parse a named type (with optional type parameters)
    fn parse_named_type(&mut self, name: &str) -> Result<Type> {
        let base_type = match name {
            "int" => Type::int(),
            "float" => Type::float(),
            "str" => Type::string(),
            "bool" => Type::bool(),
            "None" => Type::none(),
            "Any" => Type::any(),
            "Top" => Type::top(),
            "Never" => Type::never(),
            "list" => {
                if matches!(self.peek(), Some(Token::LBracket)) {
                    self.advance();
                    let element_type = self.parse_type()?;
                    self.expect(Token::RBracket)?;
                    Type::list(element_type)
                } else {
                    Type::list(Type::any())
                }
            }
            "dict" => {
                if matches!(self.peek(), Some(Token::LBracket)) {
                    self.advance();
                    let key_type = self.parse_type()?;
                    self.expect(Token::Comma)?;
                    let value_type = self.parse_type()?;
                    self.expect(Token::RBracket)?;
                    Type::dict(key_type, value_type)
                } else {
                    Type::dict(Type::any(), Type::any())
                }
            }
            "set" => {
                if matches!(self.peek(), Some(Token::LBracket)) {
                    self.advance();
                    let element_type = self.parse_type()?;
                    self.expect(Token::RBracket)?;
                    Type::App(Box::new(Type::Con(TypeCtor::Set)), Box::new(element_type))
                } else {
                    Type::App(Box::new(Type::Con(TypeCtor::Set)), Box::new(Type::any()))
                }
            }
            // TODO: Handle heterogeneous tuples properly
            "tuple" => {
                if matches!(self.peek(), Some(Token::LBracket)) {
                    self.advance();
                    let element_type = self.parse_type()?;
                    while matches!(self.peek(), Some(Token::Comma)) {
                        self.advance();
                        if matches!(self.peek(), Some(Token::RBracket)) {
                            break;
                        }
                        let _ = self.parse_type()?;
                    }

                    self.expect(Token::RBracket)?;
                    Type::App(Box::new(Type::Con(TypeCtor::Tuple)), Box::new(element_type))
                } else {
                    Type::Con(TypeCtor::Tuple)
                }
            }
            "Union" => {
                self.expect(Token::LBracket)?;
                let mut types = vec![self.parse_type()?];
                while matches!(self.peek(), Some(Token::Comma)) {
                    self.advance();
                    types.push(self.parse_type()?);
                }
                self.expect(Token::RBracket)?;
                Type::union(types)
            }
            "Optional" => {
                self.expect(Token::LBracket)?;
                let ty = self.parse_type()?;
                self.expect(Token::RBracket)?;
                Type::optional(ty)
            }
            "Callable" => {
                if matches!(self.peek(), Some(Token::LBracket)) {
                    self.advance();

                    if !matches!(self.peek(), Some(Token::LBracket)) && !matches!(self.peek(), Some(Token::Ellipsis)) {
                        return Err(
                            TypeError::UnificationError("[ or ...".to_string(), format!("{:?}", self.peek())).into(),
                        );
                    }

                    let mut arg_types = Vec::new();
                    if matches!(self.peek(), Some(Token::Ellipsis)) {
                        self.advance();
                    } else {
                        self.expect(Token::LBracket)?;
                        if !matches!(self.peek(), Some(Token::RBracket)) {
                            arg_types.push(self.parse_type()?);
                            while matches!(self.peek(), Some(Token::Comma)) {
                                self.advance();
                                if matches!(self.peek(), Some(Token::RBracket)) {
                                    break;
                                }
                                arg_types.push(self.parse_type()?);
                            }
                        }
                        self.expect(Token::RBracket)?;
                    }

                    self.expect(Token::Comma)?;
                    let return_type = self.parse_type()?;
                    self.expect(Token::RBracket)?;

                    Type::fun(arg_types, return_type)
                } else {
                    Type::Con(TypeCtor::Function)
                }
            }
            "Generic" => {
                if matches!(self.peek(), Some(Token::LBracket)) {
                    self.advance();
                    let mut type_params = vec![self.parse_type()?];
                    while matches!(self.peek(), Some(Token::Comma)) {
                        self.advance();
                        type_params.push(self.parse_type()?);
                    }
                    self.expect(Token::RBracket)?;

                    let mut result = Type::Con(TypeCtor::Generic);
                    for param in type_params {
                        result = Type::App(Box::new(result), Box::new(param));
                    }
                    result
                } else {
                    Type::Con(TypeCtor::Generic)
                }
            }
            "Protocol" => {
                if matches!(self.peek(), Some(Token::LBracket)) {
                    self.advance();
                    let mut type_params = vec![self.parse_type()?];
                    while matches!(self.peek(), Some(Token::Comma)) {
                        self.advance();
                        type_params.push(self.parse_type()?);
                    }
                    self.expect(Token::RBracket)?;
                    let mut result = Type::Con(TypeCtor::Protocol(None));
                    for param in type_params {
                        result = Type::App(Box::new(result), Box::new(param));
                    }
                    result
                } else {
                    Type::Con(TypeCtor::Protocol(None))
                }
            }
            _ => {
                if name.len() == 1 && name.chars().next().unwrap().is_uppercase() {
                    Type::Con(TypeCtor::TypeVariable(name.to_string()))
                } else {
                    Type::Con(TypeCtor::Class(name.to_string()))
                }
            }
        };

        Ok(base_type)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_basic_types() {
        let parser = AnnotationParser::new();
        assert_eq!(parser.parse("int").unwrap(), Type::int());
        assert_eq!(parser.parse("str").unwrap(), Type::string());
        assert_eq!(parser.parse("bool").unwrap(), Type::bool());
        assert_eq!(parser.parse("float").unwrap(), Type::float());
        assert_eq!(parser.parse("None").unwrap(), Type::none());
    }

    #[test]
    fn test_parse_special_types() {
        let parser = AnnotationParser::new();
        assert_eq!(parser.parse("Any").unwrap(), Type::any());
        assert_eq!(parser.parse("Top").unwrap(), Type::top());
        assert_eq!(parser.parse("Never").unwrap(), Type::never());
    }

    #[test]
    fn test_parse_list() {
        let parser = AnnotationParser::new();
        assert_eq!(parser.parse("list[int]").unwrap(), Type::list(Type::int()));
        assert_eq!(parser.parse("list[str]").unwrap(), Type::list(Type::string()));
        assert_eq!(
            parser.parse("list[list[int]]").unwrap(),
            Type::list(Type::list(Type::int()))
        );
    }

    #[test]
    fn test_parse_dict() {
        let parser = AnnotationParser::new();
        assert_eq!(
            parser.parse("dict[str, int]").unwrap(),
            Type::dict(Type::string(), Type::int())
        );
        assert_eq!(
            parser.parse("dict[int, list[str]]").unwrap(),
            Type::dict(Type::int(), Type::list(Type::string()))
        );
    }

    #[test]
    fn test_parse_union() {
        let parser = AnnotationParser::new();
        let union = parser.parse("Union[int, str]").unwrap();
        match union {
            Type::Union(types) => {
                assert_eq!(types.len(), 2);
                assert!(types.contains(&Type::int()));
                assert!(types.contains(&Type::string()));
            }
            _ => panic!("Expected union type"),
        }
    }

    #[test]
    fn test_parse_pep604_union() {
        let parser = AnnotationParser::new();
        let union = parser.parse("int | str").unwrap();
        match union {
            Type::Union(types) => {
                assert_eq!(types.len(), 2);
                assert!(types.contains(&Type::int()));
                assert!(types.contains(&Type::string()));
            }
            _ => panic!("Expected union type"),
        }
    }

    #[test]
    fn test_parse_optional() {
        let parser = AnnotationParser::new();
        let opt = parser.parse("Optional[int]").unwrap();
        match opt {
            Type::Union(types) => {
                assert_eq!(types.len(), 2);
                assert!(types.contains(&Type::int()));
                assert!(types.contains(&Type::none()));
            }
            _ => panic!("Expected union type for Optional"),
        }
    }

    #[test]
    fn test_parse_callable() {
        let parser = AnnotationParser::new();
        let callable = parser.parse("Callable[[int, str], bool]").unwrap();
        assert_eq!(callable, Type::fun(vec![Type::int(), Type::string()], Type::bool()));
    }

    #[test]
    fn test_parse_callable_no_args() {
        let parser = AnnotationParser::new();
        let callable = parser.parse("Callable[[], int]").unwrap();
        assert_eq!(callable, Type::fun(vec![], Type::int()));
    }

    #[test]
    fn test_parse_callable_ellipsis() {
        let parser = AnnotationParser::new();
        let callable = parser.parse("Callable[..., int]").unwrap();
        assert_eq!(callable, Type::fun(vec![], Type::int()));
    }

    #[test]
    fn test_parse_user_defined_class() {
        let parser = AnnotationParser::new();
        assert_eq!(
            parser.parse("MyClass").unwrap(),
            Type::Con(TypeCtor::Class("MyClass".to_string()))
        );
    }

    #[test]
    fn test_parse_nested_types() {
        let parser = AnnotationParser::new();
        let ty = parser.parse("dict[str, list[Union[int, str]]]").unwrap();

        match ty {
            Type::App(_, value_box) => match value_box.as_ref() {
                Type::App(_, elem_box) => match elem_box.as_ref() {
                    Type::Union(types) => {
                        assert_eq!(types.len(), 2);
                        assert!(types.contains(&Type::int()));
                        assert!(types.contains(&Type::string()));
                    }
                    _ => panic!("Expected union in nested type"),
                },
                _ => panic!("Expected list in dict value"),
            },
            _ => panic!("Expected dict type"),
        }
    }

    #[test]
    fn test_parse_whitespace() {
        let parser = AnnotationParser::new();
        assert_eq!(parser.parse("  int  ").unwrap(), Type::int());
        assert_eq!(parser.parse("list[ int ]").unwrap(), Type::list(Type::int()));
        assert_eq!(
            parser.parse("Union[ int , str ]").unwrap(),
            parser.parse("Union[int, str]").unwrap()
        );
    }

    #[test]
    fn test_parse_empty_string_fails() {
        let parser = AnnotationParser::new();
        assert!(parser.parse("").is_err());
        assert!(parser.parse("   ").is_err());
    }

    #[test]
    fn test_parse_or_any_fallback() {
        let parser = AnnotationParser::new();
        assert_eq!(parser.parse_or_any("invalid[[[["), Type::any());
        assert_eq!(parser.parse_or_any("int"), Type::int());
    }

    #[test]
    fn test_parse_complex_callable() {
        let parser = AnnotationParser::new();
        let ty = parser
            .parse("Callable[[list[int], dict[str, bool]], Optional[str]]")
            .unwrap();

        match ty {
            Type::Fun(args, ret) => {
                assert_eq!(args.len(), 2);
                assert_eq!(args[0], Type::list(Type::int()));
                assert_eq!(args[1], Type::dict(Type::string(), Type::bool()));
                assert_eq!(ret.as_ref(), &Type::optional(Type::string()));
            }
            _ => panic!("Expected function type"),
        }
    }

    #[test]
    fn test_parse_multi_union_with_pipe() {
        let parser = AnnotationParser::new();
        let ty = parser.parse("int | str | bool").unwrap();

        match ty {
            Type::Union(types) => {
                assert_eq!(types.len(), 3);
                assert!(types.contains(&Type::int()));
                assert!(types.contains(&Type::string()));
                assert!(types.contains(&Type::bool()));
            }
            _ => panic!("Expected union type"),
        }
    }

    #[test]
    fn test_parse_set() {
        let parser = AnnotationParser::new();
        let ty = parser.parse("set[int]").unwrap();
        assert_eq!(ty, Type::App(Box::new(Type::Con(TypeCtor::Set)), Box::new(Type::int())));
    }

    #[test]
    fn test_parse_type_variable() {
        let parser = AnnotationParser::new();
        let ty = parser.parse("T").unwrap();
        assert_eq!(ty, Type::Con(TypeCtor::TypeVariable("T".to_string())));

        let ty = parser.parse("U").unwrap();
        assert_eq!(ty, Type::Con(TypeCtor::TypeVariable("U".to_string())));
    }

    #[test]
    fn test_parse_generic() {
        let parser = AnnotationParser::new();
        let ty = parser.parse("Generic[T]").unwrap();
        match ty {
            Type::App(generic, param) => {
                assert_eq!(*generic, Type::Con(TypeCtor::Generic));
                assert_eq!(*param, Type::Con(TypeCtor::TypeVariable("T".to_string())));
            }
            _ => panic!("Expected Generic[T] as App(Generic, T)"),
        }

        let ty = parser.parse("Generic[T, U]").unwrap();
        match ty {
            Type::App(outer, param_u) => {
                assert_eq!(*param_u, Type::Con(TypeCtor::TypeVariable("U".to_string())));
                match *outer {
                    Type::App(generic, param_t) => {
                        assert_eq!(*generic, Type::Con(TypeCtor::Generic));
                        assert_eq!(*param_t, Type::Con(TypeCtor::TypeVariable("T".to_string())));
                    }
                    _ => panic!("Expected App(Generic, T) for inner part"),
                }
            }
            _ => panic!("Expected Generic[T, U] as nested App"),
        }
    }

    #[test]
    fn test_parse_protocol() {
        let parser = AnnotationParser::new();
        let ty = parser.parse("Protocol").unwrap();
        match ty {
            Type::Con(TypeCtor::Protocol(name)) => {
                assert!(name.is_none());
            }
            _ => panic!("Expected Protocol type"),
        }

        let ty = parser.parse("Protocol[T]").unwrap();
        match ty {
            Type::App(protocol, param) => {
                match *protocol {
                    Type::Con(TypeCtor::Protocol(name)) => assert!(name.is_none()),
                    _ => panic!("Expected Protocol constructor"),
                }
                assert_eq!(*param, Type::Con(TypeCtor::TypeVariable("T".to_string())));
            }
            _ => panic!("Expected Protocol[T] as App(Protocol, T)"),
        }
    }

    #[test]
    fn test_parse_generic_class() {
        let parser = AnnotationParser::new();
        let ty = parser.parse("list[T]").unwrap();
        match ty {
            Type::App(ctor, param) => {
                assert_eq!(*ctor, Type::Con(TypeCtor::List));
                assert_eq!(*param, Type::Con(TypeCtor::TypeVariable("T".to_string())));
            }
            _ => panic!("Expected list[T] type"),
        }

        let ty = parser.parse("dict[K, V]").unwrap();
        match ty {
            Type::App(outer, value) => match outer.as_ref() {
                Type::App(dict, key) => {
                    assert_eq!(**dict, Type::Con(TypeCtor::Dict));
                    assert_eq!(**key, Type::Con(TypeCtor::TypeVariable("K".to_string())));
                    assert_eq!(*value, Type::Con(TypeCtor::TypeVariable("V".to_string())));
                }
                _ => panic!("Expected dict constructor"),
            },
            _ => panic!("Expected dict[K, V] type"),
        }
    }

    #[test]
    fn test_parse_callable_with_type_variables() {
        let parser = AnnotationParser::new();
        let ty = parser.parse("Callable[[T], T]").unwrap();
        match ty {
            Type::Fun(args, ret) => {
                assert_eq!(args.len(), 1);
                assert_eq!(args[0], Type::Con(TypeCtor::TypeVariable("T".to_string())));
                assert_eq!(*ret, Type::Con(TypeCtor::TypeVariable("T".to_string())));
            }
            _ => panic!("Expected function type"),
        }
    }

    #[test]
    fn test_parse_intersection_basic() {
        let parser = AnnotationParser::new();
        let ty = parser.parse("Iterable & Sized").unwrap();
        match ty {
            Type::Intersection(types) => {
                assert_eq!(types.len(), 2);
                let has_iterable = types
                    .iter()
                    .any(|t| matches!(t, Type::Con(TypeCtor::Class(name)) if name == "Iterable"));
                let has_sized = types
                    .iter()
                    .any(|t| matches!(t, Type::Con(TypeCtor::Class(name)) if name == "Sized"));
                assert!(has_iterable, "Expected Iterable in intersection");
                assert!(has_sized, "Expected Sized in intersection");
            }
            _ => panic!("Expected intersection type, got: {ty:?}"),
        }
    }

    #[test]
    fn test_parse_intersection_three_types() {
        let parser = AnnotationParser::new();
        let ty = parser.parse("A & B & C").unwrap();
        match ty {
            Type::Intersection(types) => {
                assert_eq!(types.len(), 3);
                let has_a = types
                    .iter()
                    .any(|t| matches!(t, Type::Con(TypeCtor::TypeVariable(name)) if name == "A"));
                let has_b = types
                    .iter()
                    .any(|t| matches!(t, Type::Con(TypeCtor::TypeVariable(name)) if name == "B"));
                let has_c = types
                    .iter()
                    .any(|t| matches!(t, Type::Con(TypeCtor::TypeVariable(name)) if name == "C"));
                assert!(has_a && has_b && has_c, "Expected A, B, C in intersection");
            }
            _ => panic!("Expected intersection type"),
        }
    }

    #[test]
    fn test_parse_intersection_precedence() {
        let parser = AnnotationParser::new();
        let ty = parser.parse("A & B | C").unwrap();
        match ty {
            Type::Union(union_types) => {
                assert_eq!(union_types.len(), 2);
                let has_intersection = union_types.iter().any(|t| matches!(t, Type::Intersection(_)));
                let has_c = union_types
                    .iter()
                    .any(|t| matches!(t, Type::Con(TypeCtor::TypeVariable(name)) if name == "C"));
                assert!(has_intersection, "Expected intersection in union");
                assert!(has_c, "Expected C in union");
            }
            _ => panic!("Expected union type, got: {ty:?}"),
        }
    }

    #[test]
    fn test_parse_mixed_intersection_union() {
        let parser = AnnotationParser::new();
        let ty = parser.parse("(A | B) & (C | D)").unwrap();
        match ty {
            Type::Intersection(types) => {
                assert_eq!(types.len(), 2);
                let all_unions = types.iter().all(|t| matches!(t, Type::Union(_)));
                assert!(all_unions, "Expected all intersection components to be unions");
            }
            _ => panic!("Expected intersection type"),
        }
    }

    #[test]
    fn test_parse_intersection_with_concrete_types() {
        let parser = AnnotationParser::new();
        let ty = parser.parse("int & str").unwrap();
        match ty {
            Type::Intersection(types) => {
                assert_eq!(types.len(), 2);
                assert!(types.contains(&Type::int()));
                assert!(types.contains(&Type::string()));
            }
            _ => panic!("Expected intersection type"),
        }
    }
}

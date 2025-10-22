use thiserror::Error;
use tree_sitter::{Node, Parser, Tree};

pub mod highlight;
pub mod resolve;

pub use highlight::PythonHighlighter;
pub use resolve::{NameResolver, ScopeId, ScopeKind, Symbol, SymbolKind, SymbolTable};

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("Failed to parse Python code: {0}")]
    TreeSitterError(String),
    #[error("Invalid UTF-8 in source code")]
    InvalidUtf8,
}

/// Python parser using tree-sitter
pub struct PythonParser {
    parser: Parser,
}

/// Represents a parsed Python source file
pub struct ParsedFile {
    pub tree: Tree,
    pub source: String,
}

/// Basic AST node types for Python
#[derive(Debug, Clone, PartialEq)]
pub enum AstNode {
    Module {
        body: Vec<AstNode>,
    },
    FunctionDef {
        name: String,
        args: Vec<String>,
        body: Vec<AstNode>,
        line: usize,
        col: usize,
    },
    ClassDef {
        name: String,
        body: Vec<AstNode>,
        line: usize,
        col: usize,
    },
    Assignment {
        target: String,
        value: Box<AstNode>,
        line: usize,
        col: usize,
    },
    Call {
        function: String,
        args: Vec<AstNode>,
        line: usize,
        col: usize,
    },
    Identifier {
        name: String,
        line: usize,
        col: usize,
    },
    Literal {
        value: LiteralValue,
        line: usize,
        col: usize,
    },
    Return {
        value: Option<Box<AstNode>>,
        line: usize,
        col: usize,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralValue {
    String(String),
    Integer(i64),
    Float(f64),
    Boolean(bool),
    None,
}

impl PythonParser {
    /// Create a new Python parser
    pub fn new() -> Result<Self, ParseError> {
        let language = tree_sitter_python::LANGUAGE;
        let mut parser = Parser::new();
        parser
            .set_language(&language.into())
            .map_err(|e| ParseError::TreeSitterError(e.to_string()))?;

        Ok(PythonParser { parser })
    }

    /// Parse Python source code into a tree
    pub fn parse(&mut self, source: &str) -> Result<ParsedFile, ParseError> {
        let tree = self
            .parser
            .parse(source, None)
            .ok_or_else(|| ParseError::TreeSitterError("Failed to parse source".to_string()))?;

        Ok(ParsedFile {
            tree,
            source: source.to_string(),
        })
    }

    /// Convert tree-sitter CST to our AST
    pub fn to_ast(&self, parsed: &ParsedFile) -> Result<AstNode, ParseError> {
        let root_node = parsed.tree.root_node();
        self.node_to_ast(root_node, &parsed.source)
    }

    /// Debug helper to print tree structure
    pub fn debug_tree(&self, parsed: &ParsedFile) -> String {
        let root_node = parsed.tree.root_node();
        self.debug_node(root_node, &parsed.source, 0)
    }

    fn debug_node(&self, node: tree_sitter::Node, source: &str, depth: usize) -> String {
        let indent = "  ".repeat(depth);
        let text = node.utf8_text(source.as_bytes()).unwrap_or("<invalid>");
        let mut result = format!(
            "{}{}({}): '{}'\n",
            indent,
            node.kind(),
            node.id(),
            text.replace('\n', "\\n")
        );

        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            result.push_str(&self.debug_node(child, source, depth + 1));
        }
        result
    }

    fn node_to_ast(&self, node: Node, source: &str) -> Result<AstNode, ParseError> {
        let start_position = node.start_position();
        let line = start_position.row + 1;
        let col = start_position.column + 1;

        match node.kind() {
            "module" => {
                let mut body = Vec::new();
                let mut cursor = node.walk();

                for child in node.children(&mut cursor) {
                    if !child.is_extra() {
                        body.push(self.node_to_ast(child, source)?);
                    }
                }

                Ok(AstNode::Module { body })
            }
            "function_definition" => {
                let name = self.extract_identifier(&node, source, "name")?;
                let args = self.extract_function_args(&node, source)?;
                let body = self.extract_function_body(&node, source)?;

                Ok(AstNode::FunctionDef {
                    name,
                    args,
                    body,
                    line,
                    col,
                })
            }
            "class_definition" => {
                let name = self.extract_identifier(&node, source, "name")?;
                let body = self.extract_class_body(&node, source)?;

                Ok(AstNode::ClassDef {
                    name,
                    body,
                    line,
                    col,
                })
            }
            "expression_statement" => {
                if let Some(child) = node.named_child(0) {
                    self.node_to_ast(child, source)
                } else {
                    Ok(AstNode::Identifier {
                        name: "<empty_expression>".to_string(),
                        line,
                        col,
                    })
                }
            }
            "assignment" => {
                let target = self.extract_assignment_target(&node, source)?;
                let value = self.extract_assignment_value(&node, source)?;

                Ok(AstNode::Assignment {
                    target,
                    value: Box::new(value),
                    line,
                    col,
                })
            }
            "call" => {
                let function = self.extract_call_function(&node, source)?;
                let args = self.extract_call_args(&node, source)?;

                Ok(AstNode::Call {
                    function,
                    args,
                    line,
                    col,
                })
            }
            "identifier" => {
                let name = node
                    .utf8_text(source.as_bytes())
                    .map_err(|_| ParseError::InvalidUtf8)?;

                Ok(AstNode::Identifier {
                    name: name.to_string(),
                    line,
                    col,
                })
            }
            "string" | "integer" | "float" | "true" | "false" | "none" => {
                let value = self.extract_literal_value(&node, source)?;

                Ok(AstNode::Literal { value, line, col })
            }
            "return_statement" => {
                let value = self.extract_return_value(&node, source)?;

                Ok(AstNode::Return {
                    value: value.map(Box::new),
                    line,
                    col,
                })
            }
            _ => match node.utf8_text(source.as_bytes()) {
                Ok(text) => Ok(AstNode::Identifier {
                    name: text.to_string(),
                    line,
                    col,
                }),
                Err(_) => Ok(AstNode::Identifier {
                    name: format!("<{}>]", node.kind()),
                    line,
                    col,
                }),
            },
        }
    }

    fn extract_identifier(
        &self,
        node: &Node,
        source: &str,
        field: &str,
    ) -> Result<String, ParseError> {
        let name_node = node
            .child_by_field_name(field)
            .ok_or_else(|| ParseError::TreeSitterError(format!("Missing {} field", field)))?;

        let name = name_node
            .utf8_text(source.as_bytes())
            .map_err(|_| ParseError::InvalidUtf8)?;

        Ok(name.to_string())
    }

    fn extract_function_args(&self, node: &Node, source: &str) -> Result<Vec<String>, ParseError> {
        let params_node = node.child_by_field_name("parameters");
        let mut args = Vec::new();

        if let Some(params) = params_node {
            let mut cursor = params.walk();
            for child in params.children(&mut cursor) {
                if child.kind() == "identifier" {
                    let arg_name = child
                        .utf8_text(source.as_bytes())
                        .map_err(|_| ParseError::InvalidUtf8)?;
                    args.push(arg_name.to_string());
                }
            }
        }

        Ok(args)
    }

    fn extract_function_body(&self, node: &Node, source: &str) -> Result<Vec<AstNode>, ParseError> {
        let body_node = node
            .child_by_field_name("body")
            .ok_or_else(|| ParseError::TreeSitterError("Missing function body".to_string()))?;

        let mut body = Vec::new();
        let mut cursor = body_node.walk();

        for child in body_node.children(&mut cursor) {
            if !child.is_extra() {
                body.push(self.node_to_ast(child, source)?);
            }
        }

        Ok(body)
    }

    fn extract_class_body(&self, node: &Node, source: &str) -> Result<Vec<AstNode>, ParseError> {
        let body_node = node
            .child_by_field_name("body")
            .ok_or_else(|| ParseError::TreeSitterError("Missing class body".to_string()))?;

        let mut body = Vec::new();
        let mut cursor = body_node.walk();

        for child in body_node.children(&mut cursor) {
            if !child.is_extra() {
                body.push(self.node_to_ast(child, source)?);
            }
        }

        Ok(body)
    }

    fn extract_assignment_target(&self, node: &Node, source: &str) -> Result<String, ParseError> {
        let left_node = node
            .child_by_field_name("left")
            .ok_or_else(|| ParseError::TreeSitterError("Missing assignment target".to_string()))?;

        let target = left_node
            .utf8_text(source.as_bytes())
            .map_err(|_| ParseError::InvalidUtf8)?;

        Ok(target.to_string())
    }

    fn extract_assignment_value(&self, node: &Node, source: &str) -> Result<AstNode, ParseError> {
        let right_node = node
            .child_by_field_name("right")
            .ok_or_else(|| ParseError::TreeSitterError("Missing assignment value".to_string()))?;

        self.node_to_ast(right_node, source)
    }

    fn extract_call_function(&self, node: &Node, source: &str) -> Result<String, ParseError> {
        let function_node = node
            .child_by_field_name("function")
            .ok_or_else(|| ParseError::TreeSitterError("Missing call function".to_string()))?;

        let function = function_node
            .utf8_text(source.as_bytes())
            .map_err(|_| ParseError::InvalidUtf8)?;

        Ok(function.to_string())
    }

    fn extract_call_args(&self, node: &Node, source: &str) -> Result<Vec<AstNode>, ParseError> {
        let args_node = node.child_by_field_name("arguments");
        let mut args = Vec::new();

        if let Some(arguments) = args_node {
            let mut cursor = arguments.walk();
            for child in arguments.children(&mut cursor) {
                if !child.is_extra()
                    && child.kind() != "("
                    && child.kind() != ")"
                    && child.kind() != ","
                {
                    args.push(self.node_to_ast(child, source)?);
                }
            }
        }

        Ok(args)
    }

    fn extract_literal_value(&self, node: &Node, source: &str) -> Result<LiteralValue, ParseError> {
        let text = node
            .utf8_text(source.as_bytes())
            .map_err(|_| ParseError::InvalidUtf8)?;

        match node.kind() {
            "string" => {
                let mut cursor = node.walk();
                let mut content = String::new();
                for child in node.children(&mut cursor) {
                    if child.kind() == "string_content" {
                        if let Ok(text) = child.utf8_text(source.as_bytes()) {
                            content.push_str(text);
                        }
                    }
                }
                Ok(LiteralValue::String(content))
            }
            "integer" => {
                let value = text
                    .parse::<i64>()
                    .map_err(|e| ParseError::TreeSitterError(format!("Invalid integer: {}", e)))?;
                Ok(LiteralValue::Integer(value))
            }
            "float" => {
                let value = text
                    .parse::<f64>()
                    .map_err(|e| ParseError::TreeSitterError(format!("Invalid float: {}", e)))?;
                Ok(LiteralValue::Float(value))
            }
            "true" => Ok(LiteralValue::Boolean(true)),
            "false" => Ok(LiteralValue::Boolean(false)),
            "none" => Ok(LiteralValue::None),
            _ => Err(ParseError::TreeSitterError(format!(
                "Unknown literal type: {}",
                node.kind()
            ))),
        }
    }

    fn extract_return_value(
        &self,
        node: &Node,
        source: &str,
    ) -> Result<Option<AstNode>, ParseError> {
        match node.named_child(0) {
            Some(value_node) => Ok(Some(self.node_to_ast(value_node, source)?)),
            None => Ok(None),
        }
    }

    /// Perform name resolution on an AST and return a symbol table
    pub fn resolve_names(&self, ast: &AstNode) -> Result<SymbolTable, resolve::ResolveError> {
        let mut resolver = NameResolver::new();
        resolver.resolve(ast)?;
        Ok(resolver.symbol_table)
    }

    /// Parse and resolve names in one step
    pub fn parse_and_resolve(
        &mut self,
        source: &str,
    ) -> Result<(AstNode, SymbolTable), ParseError> {
        let parsed = self.parse(source)?;
        let ast = self.to_ast(&parsed)?;
        let symbol_table = self
            .resolve_names(&ast)
            .map_err(|e| ParseError::TreeSitterError(format!("Name resolution failed: {}", e)))?;
        Ok((ast, symbol_table))
    }
}

impl Default for PythonParser {
    fn default() -> Self {
        Self::new().expect("Failed to create Python parser")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
            AstNode::Module { body } => {
                assert_eq!(body.len(), 1);
                match &body[0] {
                    AstNode::FunctionDef { name, args, .. } => {
                        assert_eq!(name, "add");
                        assert_eq!(args.len(), 2);
                        assert_eq!(args[0], "x");
                        assert_eq!(args[1], "y");
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
            AstNode::Module { body } => {
                assert_eq!(body.len(), 1);
                match &body[0] {
                    AstNode::Assignment { target, value, .. } => {
                        assert_eq!(target, "x");
                        match value.as_ref() {
                            AstNode::Literal {
                                value: LiteralValue::Integer(42),
                                ..
                            } => {}
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
            AstNode::Module { body } => {
                assert_eq!(body.len(), 1);
                match &body[0] {
                    AstNode::Call { function, args, .. } => {
                        assert_eq!(function, "print");
                        assert_eq!(args.len(), 1);
                        match &args[0] {
                            AstNode::Literal {
                                value: LiteralValue::String(s),
                                ..
                            } => {
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
    fn test_class_to_ast() {
        let mut parser = PythonParser::new().unwrap();
        let source = "class Person:\n    pass";

        let parsed = parser.parse(source).unwrap();
        let ast = parser.to_ast(&parsed).unwrap();

        match ast {
            AstNode::Module { body } => {
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
            AstNode::Module { body } => assert!(body.len() >= 3),
            _ => panic!("Expected module"),
        }
    }

    #[test]
    fn test_literal_values() {
        let mut parser = PythonParser::new().unwrap();

        let test_cases = vec![
            ("x = 42", LiteralValue::Integer(42)),
            ("x = 3.14", LiteralValue::Float(3.14)),
            ("x = True", LiteralValue::Boolean(true)),
            ("x = False", LiteralValue::Boolean(false)),
            ("x = None", LiteralValue::None),
            ("x = 'hello'", LiteralValue::String("hello".to_string())),
        ];

        for (source, expected) in test_cases {
            let parsed = parser.parse(source).unwrap();
            let ast = parser.to_ast(&parsed).unwrap();

            match ast {
                AstNode::Module { body } => match &body[0] {
                    AstNode::Assignment { value, .. } => match value.as_ref() {
                        AstNode::Literal { value, .. } => {
                            assert_eq!(value, &expected, "Failed for source: {}", source)
                        }
                        _ => panic!("Expected literal in assignment: {}", source),
                    },
                    _ => panic!("Expected assignment: {}", source),
                },
                _ => panic!("Expected module: {}", source),
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
            AstNode::Module { body } => match &body[0] {
                AstNode::FunctionDef { args, .. } => {
                    assert_eq!(args.len(), 3);
                    assert_eq!(
                        args,
                        &vec!["a".to_string(), "b".to_string(), "c".to_string()]
                    );
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
        let ast = parser.to_ast(&parsed).unwrap();

        match ast {
            AstNode::Module { body } => match &body[0] {
                AstNode::Assignment { target, .. } => {
                    assert_eq!(target, "result");
                }
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
            AstNode::Module { body } => {
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
            AstNode::Module { body } => assert!(body.len() >= 2),
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

        // Check AST structure
        match ast {
            AstNode::Module { body } => {
                assert_eq!(body.len(), 2); // function def + assignment
            }
            _ => panic!("Expected module"),
        }

        // Check symbol table
        let root_scope = symbol_table.root_scope;

        // Should have 'factorial' function and 'result' variable in module scope
        let factorial_symbol = symbol_table.lookup_symbol("factorial", root_scope);
        assert!(factorial_symbol.is_some());
        assert_eq!(factorial_symbol.unwrap().kind, SymbolKind::Function);

        let result_symbol = symbol_table.lookup_symbol("result", root_scope);
        assert!(result_symbol.is_some());
        assert_eq!(result_symbol.unwrap().kind, SymbolKind::Variable);

        // Check function scope has parameter
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

        // Check global scope
        assert!(
            symbol_table
                .lookup_symbol("global_var", root_scope)
                .is_some()
        );
        assert!(symbol_table.lookup_symbol("MyClass", root_scope).is_some());

        // Check class scope exists
        let root_children = &symbol_table.scopes.get(&root_scope).unwrap().children;
        assert!(!root_children.is_empty());

        let class_scope = root_children[0];
        assert!(
            symbol_table
                .lookup_symbol("class_var", class_scope)
                .is_some()
        );
        assert!(symbol_table.lookup_symbol("method", class_scope).is_some());
    }
}

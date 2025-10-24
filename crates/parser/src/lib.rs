use beacon_core::{ParseError, Result};
use tree_sitter::{Node, Parser, Tree};

pub mod highlight;
pub mod resolve;

pub use highlight::PythonHighlighter;
pub use resolve::{NameResolver, ScopeId, ScopeKind, Symbol, SymbolKind, SymbolTable};

/// Python parser using [`tree_sitter`]
pub struct PythonParser {
    parser: Parser,
}

/// Represents a parsed Python source file
pub struct ParsedFile {
    pub tree: Tree,
    pub source: String,
}

/// Function parameter with position information
#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub name: String,
    pub line: usize,
    pub col: usize,
}

/// Basic AST node types for Python
#[derive(Debug, Clone, PartialEq)]
pub enum AstNode {
    Module {
        body: Vec<AstNode>,
        docstring: Option<String>,
    },
    FunctionDef {
        name: String,
        args: Vec<Parameter>,
        body: Vec<AstNode>,
        docstring: Option<String>,
        line: usize,
        col: usize,
    },
    ClassDef {
        name: String,
        body: Vec<AstNode>,
        docstring: Option<String>,
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
    /// Import statement: import module [as alias]
    Import {
        module: String,
        alias: Option<String>,
        line: usize,
        col: usize,
    },
    /// Import from statement: from module import names
    ImportFrom {
        module: String,
        names: Vec<String>,
        line: usize,
        col: usize,
    },
    /// Attribute access: object.attribute
    Attribute {
        object: Box<AstNode>,
        attribute: String,
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
    pub fn new() -> Result<Self> {
        let language = tree_sitter_python::LANGUAGE;
        let mut parser = Parser::new();
        parser
            .set_language(&language.into())
            .map_err(|e| ParseError::TreeSitterError(e.to_string()))?;

        Ok(PythonParser { parser })
    }

    /// Parse Python source code into a tree
    pub fn parse(&mut self, source: &str) -> Result<ParsedFile> {
        let tree = self
            .parser
            .parse(source, None)
            .ok_or_else(|| ParseError::TreeSitterError("Failed to parse source".to_string()))?;

        Ok(ParsedFile { tree, source: source.to_string() })
    }

    /// Convert tree-sitter CST to our AST
    pub fn to_ast(&self, parsed: &ParsedFile) -> Result<AstNode> {
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

    fn node_to_ast(&self, node: Node, source: &str) -> Result<AstNode> {
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

                // Extract module-level docstring
                let docstring = self.extract_docstring(&node, source);

                Ok(AstNode::Module { body, docstring })
            }
            "function_definition" => {
                let name = self.extract_identifier(&node, source, "name")?;
                let args = self.extract_function_args(&node, source)?;
                let body = self.extract_function_body(&node, source)?;

                // Extract docstring from function body
                let docstring = node
                    .child_by_field_name("body")
                    .and_then(|body_node| self.extract_docstring(&body_node, source));

                Ok(AstNode::FunctionDef { name, args, body, docstring, line, col })
            }
            "class_definition" => {
                let name = self.extract_identifier(&node, source, "name")?;
                let body = self.extract_class_body(&node, source)?;

                // Extract docstring from class body
                let docstring = node
                    .child_by_field_name("body")
                    .and_then(|body_node| self.extract_docstring(&body_node, source));

                Ok(AstNode::ClassDef { name, body, docstring, line, col })
            }
            "expression_statement" => {
                if let Some(child) = node.named_child(0) {
                    self.node_to_ast(child, source)
                } else {
                    Ok(AstNode::Identifier { name: "<empty_expression>".to_string(), line, col })
                }
            }
            "assignment" => {
                let target = self.extract_assignment_target(&node, source)?;
                let value = self.extract_assignment_value(&node, source)?;

                Ok(AstNode::Assignment { target, value: Box::new(value), line, col })
            }
            "call" => {
                let function_node = node
                    .child_by_field_name("function")
                    .ok_or_else(|| ParseError::TreeSitterError("Missing call function".to_string()))?;

                if function_node.kind() == "attribute" {
                    // TODO: preserve the structure
                    let function = self.extract_call_function(&node, source)?;
                    let args = self.extract_call_args(&node, source)?;
                    Ok(AstNode::Call { function, args, line, col })
                } else {
                    let function = self.extract_call_function(&node, source)?;
                    let args = self.extract_call_args(&node, source)?;
                    Ok(AstNode::Call { function, args, line, col })
                }
            }
            "identifier" => {
                let name = node.utf8_text(source.as_bytes()).map_err(|_| ParseError::InvalidUtf8)?;
                Ok(AstNode::Identifier { name: name.to_string(), line, col })
            }
            "string" | "integer" | "float" | "true" | "false" | "none" => {
                let value = self.extract_literal_value(&node, source)?;
                Ok(AstNode::Literal { value, line, col })
            }
            "return_statement" => {
                let value = self.extract_return_value(&node, source)?;
                Ok(AstNode::Return { value: value.map(Box::new), line, col })
            }
            "import_statement" => {
                let (module, alias) = self.extract_import_info(&node, source)?;
                Ok(AstNode::Import { module, alias, line, col })
            }
            "import_from_statement" => {
                let (module, names) = self.extract_import_from_info(&node, source)?;
                Ok(AstNode::ImportFrom { module, names, line, col })
            }
            "attribute" => {
                let (object, attribute) = self.extract_attribute_info(&node, source)?;
                Ok(AstNode::Attribute { object: Box::new(object), attribute, line, col })
            }
            _ => match node.utf8_text(source.as_bytes()) {
                Ok(text) => Ok(AstNode::Identifier { name: text.to_string(), line, col }),
                Err(_) => Ok(AstNode::Identifier { name: format!("<{}>]", node.kind()), line, col }),
            },
        }
    }

    fn extract_identifier(&self, node: &Node, source: &str, field: &str) -> Result<String> {
        let name = node
            .child_by_field_name(field)
            .ok_or_else(|| ParseError::TreeSitterError(format!("Missing {} field", field)))?
            .utf8_text(source.as_bytes())
            .map_err(|_| ParseError::InvalidUtf8)?
            .to_string();

        Ok(name)
    }

    fn extract_function_args(&self, node: &Node, source: &str) -> Result<Vec<Parameter>> {
        let params_node = node.child_by_field_name("parameters");
        let mut args = Vec::new();

        if let Some(params) = params_node {
            let mut cursor = params.walk();
            for child in params.children(&mut cursor) {
                if child.kind() == "identifier" {
                    let arg_name = child
                        .utf8_text(source.as_bytes())
                        .map_err(|_| ParseError::InvalidUtf8)?;

                    let start_position = child.start_position();
                    args.push(Parameter {
                        name: arg_name.to_string(),
                        line: start_position.row + 1,
                        col: start_position.column + 1,
                    });
                }
            }
        }

        Ok(args)
    }

    fn extract_function_body(&self, node: &Node, source: &str) -> Result<Vec<AstNode>> {
        let body_node = node
            .child_by_field_name("body")
            .ok_or_else(|| ParseError::TreeSitterError("Missing function body".to_string()))?;

        let mut body = Vec::new();
        let mut cursor = body_node.walk();

        for child in body_node.children(&mut cursor) {
            if child.is_extra() {
                continue;
            }

            body.push(self.node_to_ast(child, source)?);
        }

        Ok(body)
    }

    fn extract_class_body(&self, node: &Node, source: &str) -> Result<Vec<AstNode>> {
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

    fn extract_assignment_target(&self, node: &Node, source: &str) -> Result<String> {
        let left_node = node
            .child_by_field_name("left")
            .ok_or_else(|| ParseError::TreeSitterError("Missing assignment target".to_string()))?;

        let target = left_node
            .utf8_text(source.as_bytes())
            .map_err(|_| ParseError::InvalidUtf8)?;

        Ok(target.to_string())
    }

    fn extract_assignment_value(&self, node: &Node, source: &str) -> Result<AstNode> {
        let right_node = node
            .child_by_field_name("right")
            .ok_or_else(|| ParseError::TreeSitterError("Missing assignment value".to_string()))?;

        self.node_to_ast(right_node, source)
    }

    fn extract_call_function(&self, node: &Node, source: &str) -> Result<String> {
        let function_node = node
            .child_by_field_name("function")
            .ok_or_else(|| ParseError::TreeSitterError("Missing call function".to_string()))?;

        let function = function_node
            .utf8_text(source.as_bytes())
            .map_err(|_| ParseError::InvalidUtf8)?;

        Ok(function.to_string())
    }

    fn extract_call_args(&self, node: &Node, source: &str) -> Result<Vec<AstNode>> {
        let args_node = node.child_by_field_name("arguments");
        let mut args = Vec::new();

        if let Some(arguments) = args_node {
            let mut cursor = arguments.walk();
            for child in arguments.children(&mut cursor) {
                if !child.is_extra() && child.kind() != "(" && child.kind() != ")" && child.kind() != "," {
                    args.push(self.node_to_ast(child, source)?);
                }
            }
        }

        Ok(args)
    }

    fn extract_literal_value(&self, node: &Node, source: &str) -> Result<LiteralValue> {
        let text = node.utf8_text(source.as_bytes()).map_err(|_| ParseError::InvalidUtf8)?;

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
            _ => Err(ParseError::TreeSitterError(format!("Unknown literal type: {}", node.kind())).into()),
        }
    }

    fn extract_return_value(&self, node: &Node, source: &str) -> Result<Option<AstNode>> {
        match node.named_child(0) {
            Some(value_node) => Ok(Some(self.node_to_ast(value_node, source)?)),
            None => Ok(None),
        }
    }

    fn extract_import_info(&self, node: &Node, source: &str) -> Result<(String, Option<String>)> {
        let mut module = String::new();
        let mut alias = None;

        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            match child.kind() {
                "dotted_name" | "identifier" => {
                    if module.is_empty() {
                        module = child
                            .utf8_text(source.as_bytes())
                            .map_err(|_| ParseError::InvalidUtf8)?
                            .to_string();
                    }
                }
                "aliased_import" => {
                    if let Some(name_node) = child.child_by_field_name("name") {
                        module = name_node
                            .utf8_text(source.as_bytes())
                            .map_err(|_| ParseError::InvalidUtf8)?
                            .to_string();
                    }
                    if let Some(alias_node) = child.child_by_field_name("alias") {
                        alias = Some(
                            alias_node
                                .utf8_text(source.as_bytes())
                                .map_err(|_| ParseError::InvalidUtf8)?
                                .to_string(),
                        );
                    }
                }
                _ => {}
            }
        }

        if module.is_empty() {
            Err(ParseError::TreeSitterError("Missing import name".to_string()).into())
        } else {
            Ok((module, alias))
        }
    }

    fn extract_import_from_info(&self, node: &Node, source: &str) -> Result<(String, Vec<String>)> {
        let module_node = node
            .child_by_field_name("module_name")
            .ok_or_else(|| ParseError::TreeSitterError("Missing module name in import from".to_string()))?;

        let module = module_node
            .utf8_text(source.as_bytes())
            .map_err(|_| ParseError::InvalidUtf8)?
            .to_string();

        let mut names = Vec::new();
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            match child.kind() {
                "dotted_name" | "identifier" => {
                    if child.id() != module_node.id() {
                        if let Ok(name) = child.utf8_text(source.as_bytes()) {
                            if name != "from" && name != "import" {
                                names.push(name.to_string());
                            }
                        }
                    }
                }
                "aliased_import" => {
                    if let Some(name_node) = child.child_by_field_name("name") {
                        if let Ok(name) = name_node.utf8_text(source.as_bytes()) {
                            names.push(name.to_string());
                        }
                    }
                }
                _ => {}
            }
        }

        Ok((module, names))
    }

    fn extract_attribute_info(&self, node: &Node, source: &str) -> Result<(AstNode, String)> {
        let object_node = node
            .child_by_field_name("object")
            .ok_or_else(|| ParseError::TreeSitterError("Missing object in attribute".to_string()))?;

        let attribute_node = node
            .child_by_field_name("attribute")
            .ok_or_else(|| ParseError::TreeSitterError("Missing attribute name".to_string()))?;

        let object = self.node_to_ast(object_node, source)?;

        let attribute = attribute_node
            .utf8_text(source.as_bytes())
            .map_err(|_| ParseError::InvalidUtf8)?
            .to_string();

        Ok((object, attribute))
    }

    /// Extract docstring from a body node if present
    ///
    /// Docstrings are expression_statement nodes containing a string as the first child of a body.
    /// Pattern: (body . (expression_statement (string)))
    fn extract_docstring(&self, body_node: &Node, source: &str) -> Option<String> {
        let mut cursor = body_node.walk();
        for child in body_node.children(&mut cursor) {
            if child.is_extra() {
                continue;
            }

            // Check if first non-extra child is expression_statement
            if child.kind() == "expression_statement" {
                // Check if it contains a string node
                let mut expr_cursor = child.walk();
                for expr_child in child.children(&mut expr_cursor) {
                    if expr_child.kind() == "string" {
                        return self.extract_string_content(&expr_child, source);
                    }
                }
            }
            // Only check the first statement
            break;
        }
        None
    }

    /// Extract the content of a string node, removing quotes
    fn extract_string_content(&self, string_node: &Node, source: &str) -> Option<String> {
        let mut cursor = string_node.walk();
        let mut content = String::new();

        for child in string_node.children(&mut cursor) {
            if child.kind() == "string_content" {
                if let Ok(text) = child.utf8_text(source.as_bytes()) {
                    content.push_str(text);
                }
            }
        }

        if content.is_empty() { None } else { Some(content) }
    }

    /// Perform name resolution on an AST and return a symbol table
    pub fn resolve_names(&self, ast: &AstNode, source: &str) -> Result<SymbolTable> {
        let mut resolver = NameResolver::new(source.to_string());
        resolver.resolve(ast)?;
        Ok(resolver.symbol_table)
    }

    pub fn parse_and_resolve(&mut self, source: &str) -> Result<(AstNode, SymbolTable)> {
        let parsed = self.parse(source)?;
        let ast = self.to_ast(&parsed)?;
        let symbol_table = self
            .resolve_names(&ast, source)
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
                        assert_eq!(target, "x");
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
                        assert_eq!(function, "print");
                        assert_eq!(args.len(), 1);
                        match &args[0] {
                            AstNode::Literal { value: LiteralValue::String(s), .. } => {
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
                AstNode::Module { body, .. } => match &body[0] {
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
                        assert!(names.contains(&"sqrt".to_string()));
                        assert!(names.contains(&"pi".to_string()));
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
}

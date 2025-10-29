use beacon_core::{ParseError, Result};
use tree_sitter::{Node, Parser, Tree};

pub mod docstring;
pub mod highlight;
pub mod resolve;
pub mod rst;

pub use docstring::{DocstringStyle, ParsedDocstring, parse as parse_docstring};
pub use highlight::PythonHighlighter;
pub use resolve::{
    BUILTIN_DUNDERS, MAGIC_METHODS, NameResolver, ReferenceKind, ScopeId, ScopeKind, Symbol, SymbolKind,
    SymbolReference, SymbolTable,
};

/// Python parser using [tree_sitter]
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
    pub type_annotation: Option<String>,
    pub default_value: Option<Box<AstNode>>,
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
        return_type: Option<String>,
        decorators: Vec<String>,
        line: usize,
        col: usize,
    },
    ClassDef {
        name: String,
        bases: Vec<String>,
        metaclass: Option<String>,
        body: Vec<AstNode>,
        docstring: Option<String>,
        decorators: Vec<String>,
        line: usize,
        col: usize,
    },
    Assignment {
        target: String,
        value: Box<AstNode>,
        line: usize,
        col: usize,
    },
    AnnotatedAssignment {
        target: String,
        type_annotation: String,
        value: Option<Box<AstNode>>,
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
    /// If statement: if test: body elif test: body else: body
    If {
        test: Box<AstNode>,
        body: Vec<AstNode>,
        elif_parts: Vec<(AstNode, Vec<AstNode>)>,
        else_body: Option<Vec<AstNode>>,
        line: usize,
        col: usize,
    },
    /// For loop: for target in iter: body
    For {
        target: String,
        iter: Box<AstNode>,
        body: Vec<AstNode>,
        else_body: Option<Vec<AstNode>>,
        line: usize,
        col: usize,
    },
    /// While loop: while test: body
    While {
        test: Box<AstNode>,
        body: Vec<AstNode>,
        else_body: Option<Vec<AstNode>>,
        line: usize,
        col: usize,
    },
    /// Try/except/finally: try: body except: handlers else: orelse finally: finalbody
    Try {
        body: Vec<AstNode>,
        handlers: Vec<ExceptHandler>,
        else_body: Option<Vec<AstNode>>,
        finally_body: Option<Vec<AstNode>>,
        line: usize,
        col: usize,
    },
    /// With statement: with item as target: body
    With {
        items: Vec<WithItem>,
        body: Vec<AstNode>,
        line: usize,
        col: usize,
    },
    /// List comprehension: [expr for target in iter if cond]
    ListComp {
        element: Box<AstNode>,
        generators: Vec<Comprehension>,
        line: usize,
        col: usize,
    },
    /// Dict comprehension: {key: value for target in iter if cond}
    DictComp {
        key: Box<AstNode>,
        value: Box<AstNode>,
        generators: Vec<Comprehension>,
        line: usize,
        col: usize,
    },
    /// Set comprehension: {expr for target in iter if cond}
    SetComp {
        element: Box<AstNode>,
        generators: Vec<Comprehension>,
        line: usize,
        col: usize,
    },
    /// Generator expression: (expr for target in iter if cond)
    GeneratorExp {
        element: Box<AstNode>,
        generators: Vec<Comprehension>,
        line: usize,
        col: usize,
    },
    /// Named expression (walrus operator): target := value
    NamedExpr {
        target: String,
        value: Box<AstNode>,
        line: usize,
        col: usize,
    },
    /// Binary operation: left op right
    BinaryOp {
        left: Box<AstNode>,
        op: BinaryOperator,
        right: Box<AstNode>,
        line: usize,
        col: usize,
    },
    /// Unary operation: op operand
    UnaryOp {
        op: UnaryOperator,
        operand: Box<AstNode>,
        line: usize,
        col: usize,
    },
    /// Comparison operation: left op right (can chain)
    Compare {
        left: Box<AstNode>,
        ops: Vec<CompareOperator>,
        comparators: Vec<AstNode>,
        line: usize,
        col: usize,
    },
    /// Lambda expression: lambda args: body
    Lambda {
        args: Vec<Parameter>,
        body: Box<AstNode>,
        line: usize,
        col: usize,
    },
    /// Subscript: value[slice]
    Subscript {
        value: Box<AstNode>,
        slice: Box<AstNode>,
        line: usize,
        col: usize,
    },
    /// Match statement (PEP 634): match subject: case pattern: body
    Match {
        subject: Box<AstNode>,
        cases: Vec<MatchCase>,
        line: usize,
        col: usize,
    },
    /// Pass statement
    Pass {
        line: usize,
        col: usize,
    },
    /// Break statement
    Break {
        line: usize,
        col: usize,
    },
    /// Continue statement
    Continue {
        line: usize,
        col: usize,
    },
    /// Raise statement
    Raise {
        exc: Option<Box<AstNode>>,
        line: usize,
        col: usize,
    },
    /// Tuple literal: (1, 2, 3) or (x,)
    Tuple {
        elements: Vec<AstNode>,
        line: usize,
        col: usize,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralValue {
    String { value: String, prefix: String },
    Integer(i64),
    Float(f64),
    Boolean(bool),
    None,
}

/// Exception handler for try/except
#[derive(Debug, Clone, PartialEq)]
pub struct ExceptHandler {
    pub exception_type: Option<String>,
    pub name: Option<String>,
    pub body: Vec<AstNode>,
    pub line: usize,
    pub col: usize,
}

/// With item for with statements
#[derive(Debug, Clone, PartialEq)]
pub struct WithItem {
    pub context_expr: AstNode,
    pub optional_vars: Option<String>,
}

/// Comprehension clause: for target in iter if conditions
#[derive(Debug, Clone, PartialEq)]
pub struct Comprehension {
    pub target: String,
    pub iter: AstNode,
    pub ifs: Vec<AstNode>,
}

/// Binary operators
#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mult,
    Div,
    FloorDiv,
    Mod,
    Pow,
    MatMult,
    BitAnd,
    BitOr,
    BitXor,
    LeftShift,
    RightShift,
    And,
    Or,
}

/// Unary operators
#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperator {
    Not,
    Invert,
    Plus,
    Minus,
}

/// Comparison operators
#[derive(Debug, Clone, PartialEq)]
pub enum CompareOperator {
    Eq,
    NotEq,
    Lt,
    LtE,
    Gt,
    GtE,
    Is,
    IsNot,
    In,
    NotIn,
}

/// Match case for pattern matching
#[derive(Debug, Clone, PartialEq)]
pub struct MatchCase {
    pub pattern: Pattern,
    pub guard: Option<AstNode>,
    pub body: Vec<AstNode>,
}

/// Pattern for pattern matching
#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    MatchValue(AstNode),
    MatchSequence(Vec<Pattern>),
    MatchMapping {
        keys: Vec<AstNode>,
        patterns: Vec<Pattern>,
    },
    MatchClass {
        cls: String,
        patterns: Vec<Pattern>,
    },
    MatchAs {
        pattern: Option<Box<Pattern>>,
        name: Option<String>,
    },
    MatchOr(Vec<Pattern>),
}

struct InfoIf(
    AstNode,
    Vec<AstNode>,
    Vec<(AstNode, Vec<AstNode>)>,
    Option<Vec<AstNode>>,
);

struct InfoTry(
    Vec<AstNode>,
    Vec<ExceptHandler>,
    Option<Vec<AstNode>>,
    Option<Vec<AstNode>>,
);

struct InfoFor(String, AstNode, Vec<AstNode>, Option<Vec<AstNode>>);

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
        Self::debug_node(root_node, &parsed.source, 0)
    }

    fn debug_node(node: tree_sitter::Node, source: &str, depth: usize) -> String {
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
            result.push_str(&Self::debug_node(child, source, depth + 1));
        }
        result
    }

    fn node_to_ast(&self, node: Node, source: &str) -> Result<AstNode> {
        let start_position = node.start_position();
        let line = start_position.row + 1;
        let col = start_position.column + 1;

        match node.kind() {
            "decorated_definition" => {
                let mut decorators = Vec::new();
                let mut definition_node = None;

                let mut cursor = node.walk();
                for child in node.children(&mut cursor) {
                    match child.kind() {
                        "decorator" => {
                            if let Some(dec_name) = self.extract_decorator_name(&child, source) {
                                decorators.push(dec_name);
                            }
                        }
                        "function_definition" | "class_definition" => {
                            definition_node = Some(child);
                        }
                        _ => {}
                    }
                }

                if let Some(def_node) = definition_node {
                    let mut ast = self.node_to_ast(def_node, source)?;

                    match &mut ast {
                        AstNode::FunctionDef { decorators: decs, .. } | AstNode::ClassDef { decorators: decs, .. } => {
                            *decs = decorators;
                        }
                        _ => {}
                    }

                    return Ok(ast);
                }

                Ok(AstNode::Identifier { name: "<decorated>".to_string(), line, col })
            }
            "module" => {
                let mut body = Vec::new();
                let mut cursor = node.walk();
                for child in node.children(&mut cursor) {
                    if !child.is_extra() {
                        body.push(self.node_to_ast(child, source)?);
                    }
                }

                let docstring = self.extract_docstring(&node, source);
                Ok(AstNode::Module { body, docstring })
            }
            // TODO: extract decorators
            "function_definition" => {
                let name = self.extract_identifier(&node, source, "name")?;
                let args = self.extract_function_args(&node, source)?;
                let body = self.extract_function_body(&node, source)?;
                let return_type = self.extract_return_type(&node, source);
                let decorators = Vec::new();
                let docstring = node
                    .child_by_field_name("body")
                    .and_then(|body_node| self.extract_docstring(&body_node, source));

                Ok(AstNode::FunctionDef { name, args, body, docstring, return_type, decorators, line, col })
            }
            // TODO: extract decorators
            "class_definition" => {
                let name = self.extract_identifier(&node, source, "name")?;
                let bases = self.extract_class_bases(&node, source);
                let metaclass = self.extract_class_metaclass(&node, source);
                let body = self.extract_class_body(&node, source)?;
                let decorators = Vec::new();
                let docstring = node
                    .child_by_field_name("body")
                    .and_then(|body_node| self.extract_docstring(&body_node, source));

                Ok(AstNode::ClassDef { name, bases, metaclass, body, docstring, decorators, line, col })
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
                if let Some(type_node) = node.child_by_field_name("type") {
                    let type_annotation = type_node
                        .utf8_text(source.as_bytes())
                        .map_err(|_| ParseError::InvalidUtf8)?
                        .to_string();

                    let value = node
                        .child_by_field_name("right")
                        .map(|v| self.node_to_ast(v, source))
                        .transpose()?
                        .map(Box::new);

                    Ok(AstNode::AnnotatedAssignment { target, type_annotation, value, line, col })
                } else {
                    let value = self.extract_assignment_value(&node, source)?;
                    Ok(AstNode::Assignment { target, value: Box::new(value), line, col })
                }
            }
            "call" => {
                let function = self.extract_call_function(&node, source)?;
                let args = self.extract_call_args(&node, source)?;
                Ok(AstNode::Call { function, args, line, col })
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
            "if_statement" => {
                let InfoIf(test, body, elif_parts, else_body) = self.extract_if_info(&node, source)?;
                Ok(AstNode::If { test: Box::new(test), body, elif_parts, else_body, line, col })
            }
            "for_statement" => {
                let InfoFor(target, iter, body, else_body) = self.extract_for_info(&node, source)?;
                Ok(AstNode::For { target, iter: Box::new(iter), body, else_body, line, col })
            }
            "while_statement" => {
                let (test, body, else_body) = self.extract_while_info(&node, source)?;
                Ok(AstNode::While { test: Box::new(test), body, else_body, line, col })
            }
            "try_statement" => {
                let InfoTry(body, handlers, else_body, finally_body) = self.extract_try_info(&node, source)?;
                Ok(AstNode::Try { body, handlers, else_body, finally_body, line, col })
            }
            "with_statement" => {
                let (items, body) = self.extract_with_info(&node, source)?;
                Ok(AstNode::With { items, body, line, col })
            }
            "list_comprehension" => {
                let (element, generators) = self.extract_list_comp_info(&node, source)?;
                Ok(AstNode::ListComp { element: Box::new(element), generators, line, col })
            }
            "dictionary_comprehension" => {
                let (key, value, generators) = self.extract_dict_comp_info(&node, source)?;
                Ok(AstNode::DictComp { key: Box::new(key), value: Box::new(value), generators, line, col })
            }
            "set_comprehension" => {
                let (element, generators) = self.extract_set_comp_info(&node, source)?;
                Ok(AstNode::SetComp { element: Box::new(element), generators, line, col })
            }
            "generator_expression" => {
                let (element, generators) = self.extract_generator_exp_info(&node, source)?;
                Ok(AstNode::GeneratorExp { element: Box::new(element), generators, line, col })
            }
            "named_expression" => {
                let (target, value) = self.extract_named_expr_info(&node, source)?;
                Ok(AstNode::NamedExpr { target, value: Box::new(value), line, col })
            }
            "binary_operator" => {
                let (left, op, right) = self.extract_binary_op_info(&node, source)?;
                Ok(AstNode::BinaryOp { left: Box::new(left), op, right: Box::new(right), line, col })
            }
            "unary_operator" => {
                let (op, operand) = self.extract_unary_op_info(&node, source)?;
                Ok(AstNode::UnaryOp { op, operand: Box::new(operand), line, col })
            }
            "comparison_operator" => {
                let (left, ops, comparators) = self.extract_comparison_info(&node, source)?;
                Ok(AstNode::Compare { left: Box::new(left), ops, comparators, line, col })
            }
            "lambda" => {
                let (args, body) = self.extract_lambda_info(&node, source)?;
                Ok(AstNode::Lambda { args, body: Box::new(body), line, col })
            }
            "subscript" => {
                let (value, slice) = self.extract_subscript_info(&node, source)?;
                Ok(AstNode::Subscript { value: Box::new(value), slice: Box::new(slice), line, col })
            }
            "match_statement" => {
                let (subject, cases) = self.extract_match_info(&node, source)?;
                Ok(AstNode::Match { subject: Box::new(subject), cases, line, col })
            }
            "pass_statement" => Ok(AstNode::Pass { line, col }),
            "break_statement" => Ok(AstNode::Break { line, col }),
            "continue_statement" => Ok(AstNode::Continue { line, col }),
            "raise_statement" => {
                let exc = self.extract_raise_value(&node, source)?;
                Ok(AstNode::Raise { exc: exc.map(Box::new), line, col })
            }
            "tuple" | "expression_list" => {
                let elements = self.extract_tuple_elements(&node, source)?;
                Ok(AstNode::Tuple { elements, line, col })
            }
            "parenthesized_expression" => match node.named_child(0) {
                Some(child) => self.node_to_ast(child, source),
                None => Ok(AstNode::Identifier { name: "<empty_parens>".to_string(), line, col }),
            },
            "boolean_operator" => {
                let (left, op, right) = self.extract_boolean_op_info(&node, source)?;
                Ok(AstNode::BinaryOp { left: Box::new(left), op, right: Box::new(right), line, col })
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
            .ok_or_else(|| ParseError::TreeSitterError(format!("Missing {field} field")))?
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
                if let Some(param) = self.extract_parameter(&child, source)? {
                    args.push(param);
                }
            }
        }

        Ok(args)
    }

    fn extract_parameter(&self, node: &Node, source: &str) -> Result<Option<Parameter>> {
        match node.kind() {
            "identifier" => {
                let arg_name = node.utf8_text(source.as_bytes()).map_err(|_| ParseError::InvalidUtf8)?;
                let start_position = node.start_position();
                Ok(Some(Parameter {
                    name: arg_name.to_string(),
                    line: start_position.row + 1,
                    col: start_position.column + 1,
                    type_annotation: None,
                    default_value: None,
                }))
            }
            "typed_parameter" | "typed_default_parameter" => {
                let mut name = None;
                let mut type_annotation = None;
                let mut default_value = None;
                let mut position = node.start_position();

                if let Some(name_node) = node.child_by_field_name("name") {
                    name = Some(
                        name_node
                            .utf8_text(source.as_bytes())
                            .map_err(|_| ParseError::InvalidUtf8)?
                            .to_string(),
                    );
                    position = name_node.start_position();
                } else {
                    let mut cursor = node.walk();
                    for child in node.children(&mut cursor) {
                        if child.kind() == "identifier" {
                            name = Some(
                                child
                                    .utf8_text(source.as_bytes())
                                    .map_err(|_| ParseError::InvalidUtf8)?
                                    .to_string(),
                            );
                            position = child.start_position();
                            break;
                        }
                    }
                }

                if let Some(type_node) = node.child_by_field_name("type") {
                    type_annotation = Some(
                        type_node
                            .utf8_text(source.as_bytes())
                            .map_err(|_| ParseError::InvalidUtf8)?
                            .to_string(),
                    );
                }

                if let Some(value_node) = node.child_by_field_name("value") {
                    default_value = Some(Box::new(self.node_to_ast(value_node, source)?));
                }

                match name {
                    Some(n) => Ok(Some(Parameter {
                        name: n,
                        line: position.row + 1,
                        col: position.column + 1,
                        type_annotation,
                        default_value,
                    })),
                    None => Ok(None),
                }
            }
            "default_parameter" => {
                let mut name = None;
                let mut default_value = None;
                let mut position = node.start_position();

                if let Some(name_node) = node.child_by_field_name("name") {
                    name = Some(
                        name_node
                            .utf8_text(source.as_bytes())
                            .map_err(|_| ParseError::InvalidUtf8)?
                            .to_string(),
                    );
                    position = name_node.start_position();
                }

                if let Some(value_node) = node.child_by_field_name("value") {
                    default_value = Some(Box::new(self.node_to_ast(value_node, source)?));
                }

                match name {
                    Some(n) => Ok(Some(Parameter {
                        name: n,
                        line: position.row + 1,
                        col: position.column + 1,
                        type_annotation: None,
                        default_value,
                    })),
                    None => Ok(None),
                }
            }
            _ => Ok(None),
        }
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

    fn extract_return_type(&self, node: &Node, source: &str) -> Option<String> {
        node.child_by_field_name("return_type")
            .and_then(|type_node| type_node.utf8_text(source.as_bytes()).ok())
            .map(|s| s.to_string())
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

    fn extract_class_bases(&self, node: &Node, source: &str) -> Vec<String> {
        let mut bases = Vec::new();

        if let Some(arg_list) = node.child_by_field_name("superclasses") {
            let mut cursor = arg_list.walk();
            for child in arg_list.children(&mut cursor) {
                if child.kind() == "keyword_argument" {
                    continue;
                }
                if child.kind() == "identifier" || child.kind() == "attribute" {
                    if let Ok(base_name) = child.utf8_text(source.as_bytes()) {
                        bases.push(base_name.to_string());
                    }
                }
            }
        }

        bases
    }

    fn extract_class_metaclass(&self, node: &Node, source: &str) -> Option<String> {
        if let Some(arg_list) = node.child_by_field_name("superclasses") {
            let mut cursor = arg_list.walk();
            for child in arg_list.children(&mut cursor) {
                if child.kind() == "keyword_argument" {
                    if let Some(name_node) = child.child(0) {
                        if let Ok(name) = name_node.utf8_text(source.as_bytes()) {
                            if name == "metaclass" {
                                if let Some(value_node) = child.child(2) {
                                    if let Ok(metaclass_name) = value_node.utf8_text(source.as_bytes()) {
                                        return Some(metaclass_name.to_string());
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        None
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
                let mut prefix = String::new();

                for child in node.children(&mut cursor) {
                    if child.kind() == "string_start" {
                        if let Ok(start_text) = child.utf8_text(source.as_bytes()) {
                            if let Some(pos) = start_text.find(|c: char| ['\'', '"'].contains(&c)) {
                                prefix = start_text[..pos].to_string();
                            }
                        }
                    } else if child.kind() == "string_content" {
                        if let Ok(text) = child.utf8_text(source.as_bytes()) {
                            content.push_str(text);
                        }
                    } else if child.kind() == "interpolation" {
                        content.push_str("{}");
                    }
                }
                Ok(LiteralValue::String { value: content, prefix })
            }
            "integer" => {
                let value = text
                    .parse::<i64>()
                    .map_err(|e| ParseError::TreeSitterError(format!("Invalid integer: {e}")))?;
                Ok(LiteralValue::Integer(value))
            }
            "float" => {
                let value = text
                    .parse::<f64>()
                    .map_err(|e| ParseError::TreeSitterError(format!("Invalid float: {e}")))?;
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

    fn extract_raise_value(&self, node: &Node, source: &str) -> Result<Option<AstNode>> {
        match node.named_child(0) {
            Some(value_node) => Ok(Some(self.node_to_ast(value_node, source)?)),
            None => Ok(None),
        }
    }

    fn extract_tuple_elements(&self, node: &Node, source: &str) -> Result<Vec<AstNode>> {
        let mut elements = Vec::new();
        let mut cursor = node.walk();

        for child in node.children(&mut cursor) {
            if !child.is_extra() && child.kind() != "(" && child.kind() != ")" && child.kind() != "," {
                elements.push(self.node_to_ast(child, source)?);
            }
        }

        Ok(elements)
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

    /// Extract decorator name from a decorator node
    fn extract_decorator_name(&self, decorator_node: &Node, source: &str) -> Option<String> {
        let mut cursor = decorator_node.walk();
        for child in decorator_node.children(&mut cursor) {
            if child.kind() == "identifier" || child.kind() == "attribute" {
                return child.utf8_text(source.as_bytes()).ok().map(|s| s.to_string());
            }
        }
        None
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

            if child.kind() == "expression_statement" {
                let mut expr_cursor = child.walk();
                for expr_child in child.children(&mut expr_cursor) {
                    if expr_child.kind() == "string" {
                        return self.extract_string_content(&expr_child, source);
                    }
                }
            }

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

    fn extract_if_info(&self, node: &Node, source: &str) -> Result<InfoIf> {
        let condition = node
            .child_by_field_name("condition")
            .ok_or_else(|| ParseError::TreeSitterError("Missing if condition".to_string()))?;
        let test = self.node_to_ast(condition, source)?;
        let consequence = node
            .child_by_field_name("consequence")
            .ok_or_else(|| ParseError::TreeSitterError("Missing if body".to_string()))?;
        let body = self.extract_body(&consequence, source)?;

        let mut elif_parts = Vec::new();
        let mut else_body = None;

        if let Some(alternative) = node.child_by_field_name("alternative") {
            match alternative.kind() {
                "elif_clause" => {
                    if let Some(elif_cond) = alternative.child_by_field_name("condition") {
                        let elif_test = self.node_to_ast(elif_cond, source)?;
                        if let Some(elif_cons) = alternative.child_by_field_name("consequence") {
                            let elif_body = self.extract_body(&elif_cons, source)?;
                            elif_parts.push((elif_test, elif_body));
                        }
                    }
                }
                "else_clause" => {
                    if let Some(else_block) = alternative.child_by_field_name("body") {
                        else_body = Some(self.extract_body(&else_block, source)?);
                    }
                }
                "if_statement" => {
                    let nested_if = self.node_to_ast(alternative, source)?;
                    if let AstNode::If {
                        test, body: if_body, elif_parts: nested_elif, else_body: nested_else, ..
                    } = nested_if
                    {
                        elif_parts.push((*test, if_body));
                        elif_parts.extend(nested_elif);
                        else_body = nested_else;
                    }
                }
                _ => {}
            }
        }

        Ok(InfoIf(test, body, elif_parts, else_body))
    }

    fn extract_for_info(&self, node: &Node, source: &str) -> Result<InfoFor> {
        let left = node
            .child_by_field_name("left")
            .ok_or_else(|| ParseError::TreeSitterError("Missing for target".to_string()))?;
        let target = left
            .utf8_text(source.as_bytes())
            .map_err(|_| ParseError::InvalidUtf8)?
            .to_string();

        let right = node
            .child_by_field_name("right")
            .ok_or_else(|| ParseError::TreeSitterError("Missing for iterator".to_string()))?;
        let iter = self.node_to_ast(right, source)?;

        let body_node = node
            .child_by_field_name("body")
            .ok_or_else(|| ParseError::TreeSitterError("Missing for body".to_string()))?;
        let body = self.extract_body(&body_node, source)?;

        let else_body = node
            .child_by_field_name("alternative")
            .map(|alt| self.extract_body(&alt, source))
            .transpose()?;

        Ok(InfoFor(target, iter, body, else_body))
    }

    fn extract_while_info(&self, node: &Node, source: &str) -> Result<(AstNode, Vec<AstNode>, Option<Vec<AstNode>>)> {
        let condition = node
            .child_by_field_name("condition")
            .ok_or_else(|| ParseError::TreeSitterError("Missing while condition".to_string()))?;
        let test = self.node_to_ast(condition, source)?;

        let body_node = node
            .child_by_field_name("body")
            .ok_or_else(|| ParseError::TreeSitterError("Missing while body".to_string()))?;
        let body = self.extract_body(&body_node, source)?;

        let else_body = node
            .child_by_field_name("alternative")
            .map(|alt| self.extract_body(&alt, source))
            .transpose()?;

        Ok((test, body, else_body))
    }

    fn extract_try_info(&self, node: &Node, source: &str) -> Result<InfoTry> {
        let body_node = node
            .child_by_field_name("body")
            .ok_or_else(|| ParseError::TreeSitterError("Missing try body".to_string()))?;
        let body = self.extract_body(&body_node, source)?;
        let mut handlers = Vec::new();
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if child.kind() == "except_clause" {
                let handler = self.extract_except_handler(&child, source)?;
                handlers.push(handler);
            }
        }

        let else_body = node
            .children(&mut node.walk())
            .find(|n| n.kind() == "else_clause")
            .and_then(|n| n.child_by_field_name("body"))
            .map(|n| self.extract_body(&n, source))
            .transpose()?;

        let finally_body = node
            .children(&mut node.walk())
            .find(|n| n.kind() == "finally_clause")
            .and_then(|n| n.child_by_field_name("body"))
            .map(|n| self.extract_body(&n, source))
            .transpose()?;

        Ok(InfoTry(body, handlers, else_body, finally_body))
    }

    fn extract_except_handler(&self, node: &Node, source: &str) -> Result<ExceptHandler> {
        let start_position = node.start_position();
        let line = start_position.row + 1;
        let col = start_position.column + 1;
        let exception_type = node
            .children(&mut node.walk())
            .find(|n| n.kind() == "dotted_name" || n.kind() == "identifier")
            .and_then(|n| n.utf8_text(source.as_bytes()).ok())
            .map(|s| s.to_string());

        let name = node
            .child_by_field_name("name")
            .and_then(|n| n.utf8_text(source.as_bytes()).ok())
            .map(|s| s.to_string());

        let body_node = node
            .children(&mut node.walk())
            .find(|n| n.kind() == "block")
            .ok_or_else(|| ParseError::TreeSitterError("Missing except body".to_string()))?;
        let body = self.extract_body(&body_node, source)?;

        Ok(ExceptHandler { exception_type, name, body, line, col })
    }

    fn extract_with_info(&self, node: &Node, source: &str) -> Result<(Vec<WithItem>, Vec<AstNode>)> {
        let mut items = Vec::new();
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if child.kind() == "with_item" {
                let context_expr_node = child.named_child(0).ok_or_else(|| {
                    ParseError::TreeSitterError("Missing context expression in with item".to_string())
                })?;
                let context_expr = self.node_to_ast(context_expr_node, source)?;

                let optional_vars = child
                    .child_by_field_name("alias")
                    .and_then(|n| n.utf8_text(source.as_bytes()).ok())
                    .map(|s| s.to_string());

                items.push(WithItem { context_expr, optional_vars });
            }
        }

        let body_node = node
            .child_by_field_name("body")
            .ok_or_else(|| ParseError::TreeSitterError("Missing with body".to_string()))?;
        let body = self.extract_body(&body_node, source)?;

        Ok((items, body))
    }

    fn extract_list_comp_info(&self, node: &Node, source: &str) -> Result<(AstNode, Vec<Comprehension>)> {
        let element = node
            .child_by_field_name("body")
            .ok_or_else(|| ParseError::TreeSitterError("Missing list comprehension body".to_string()))?;
        let element_ast = self.node_to_ast(element, source)?;
        let generators = self.extract_comprehension_clauses(node, source)?;

        Ok((element_ast, generators))
    }

    fn extract_dict_comp_info(&self, node: &Node, source: &str) -> Result<(AstNode, AstNode, Vec<Comprehension>)> {
        let mut key = None;
        let mut value = None;
        let mut cursor = node.walk();

        for child in node.children(&mut cursor) {
            if child.kind() == "pair" {
                if let Some(key_node) = child.child_by_field_name("key") {
                    key = Some(self.node_to_ast(key_node, source)?);
                }
                if let Some(value_node) = child.child_by_field_name("value") {
                    value = Some(self.node_to_ast(value_node, source)?);
                }
            }
        }

        let key = key.ok_or_else(|| ParseError::TreeSitterError("Missing dict comprehension key".to_string()))?;
        let value = value.ok_or_else(|| ParseError::TreeSitterError("Missing dict comprehension value".to_string()))?;
        let generators = self.extract_comprehension_clauses(node, source)?;

        Ok((key, value, generators))
    }

    fn extract_set_comp_info(&self, node: &Node, source: &str) -> Result<(AstNode, Vec<Comprehension>)> {
        let element = node
            .child_by_field_name("body")
            .ok_or_else(|| ParseError::TreeSitterError("Missing set comprehension body".to_string()))?;
        let element_ast = self.node_to_ast(element, source)?;
        let generators = self.extract_comprehension_clauses(node, source)?;

        Ok((element_ast, generators))
    }

    fn extract_generator_exp_info(&self, node: &Node, source: &str) -> Result<(AstNode, Vec<Comprehension>)> {
        let element = node
            .child_by_field_name("body")
            .ok_or_else(|| ParseError::TreeSitterError("Missing generator expression body".to_string()))?;
        let element_ast = self.node_to_ast(element, source)?;
        let generators = self.extract_comprehension_clauses(node, source)?;

        Ok((element_ast, generators))
    }

    fn extract_comprehension_clauses(&self, node: &Node, source: &str) -> Result<Vec<Comprehension>> {
        let mut generators = Vec::new();
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if child.kind() == "for_in_clause" {
                let left = child
                    .child_by_field_name("left")
                    .ok_or_else(|| ParseError::TreeSitterError("Missing comprehension target".to_string()))?;
                let target = left
                    .utf8_text(source.as_bytes())
                    .map_err(|_| ParseError::InvalidUtf8)?
                    .to_string();

                let right = child
                    .child_by_field_name("right")
                    .ok_or_else(|| ParseError::TreeSitterError("Missing comprehension iterator".to_string()))?;
                let iter = self.node_to_ast(right, source)?;

                let mut ifs = Vec::new();
                let mut sibling_cursor = child.walk();
                if let Some(parent) = child.parent() {
                    for sibling in parent.children(&mut sibling_cursor) {
                        if sibling.kind() == "if_clause" && sibling.start_byte() > child.end_byte() {
                            if let Some(cond) = sibling.named_child(0) {
                                ifs.push(self.node_to_ast(cond, source)?);
                            }
                        }
                    }
                }

                generators.push(Comprehension { target, iter, ifs });
            }
        }

        Ok(generators)
    }

    fn extract_named_expr_info(&self, node: &Node, source: &str) -> Result<(String, AstNode)> {
        let name = node
            .child_by_field_name("name")
            .ok_or_else(|| ParseError::TreeSitterError("Missing named expression target".to_string()))?
            .utf8_text(source.as_bytes())
            .map_err(|_| ParseError::InvalidUtf8)?
            .to_string();

        let value_node = node
            .child_by_field_name("value")
            .ok_or_else(|| ParseError::TreeSitterError("Missing named expression value".to_string()))?;
        let value = self.node_to_ast(value_node, source)?;

        Ok((name, value))
    }

    fn extract_binary_op_info(&self, node: &Node, source: &str) -> Result<(AstNode, BinaryOperator, AstNode)> {
        let left_node = node
            .child_by_field_name("left")
            .ok_or_else(|| ParseError::TreeSitterError("Missing binary operator left operand".to_string()))?;
        let left = self.node_to_ast(left_node, source)?;

        let right_node = node
            .child_by_field_name("right")
            .ok_or_else(|| ParseError::TreeSitterError("Missing binary operator right operand".to_string()))?;
        let right = self.node_to_ast(right_node, source)?;

        let mut cursor = node.walk();
        let op_str = node
            .children(&mut cursor)
            .find(|n| self.is_binary_operator(n.kind()))
            .and_then(|n| n.utf8_text(source.as_bytes()).ok())
            .ok_or_else(|| ParseError::TreeSitterError("Missing binary operator".to_string()))?;

        let op = self.parse_binary_operator(op_str)?;

        Ok((left, op, right))
    }

    fn extract_unary_op_info(&self, node: &Node, source: &str) -> Result<(UnaryOperator, AstNode)> {
        let operand_node = node
            .child_by_field_name("argument")
            .ok_or_else(|| ParseError::TreeSitterError("Missing unary operator operand".to_string()))?;
        let operand = self.node_to_ast(operand_node, source)?;

        let mut cursor = node.walk();
        let op_str = node
            .children(&mut cursor)
            .find(|n| self.is_unary_operator(n.kind()))
            .and_then(|n| n.utf8_text(source.as_bytes()).ok())
            .ok_or_else(|| ParseError::TreeSitterError("Missing unary operator".to_string()))?;

        let op = self.parse_unary_operator(op_str)?;

        Ok((op, operand))
    }

    fn extract_comparison_info(
        &self, node: &Node, source: &str,
    ) -> Result<(AstNode, Vec<CompareOperator>, Vec<AstNode>)> {
        let mut operands = Vec::new();
        let mut operators = Vec::new();
        let mut cursor = node.walk();

        for child in node.children(&mut cursor) {
            if !child.is_extra() {
                if self.is_compare_operator(child.kind()) {
                    if let Ok(text) = child.utf8_text(source.as_bytes()) {
                        if let Ok(op) = self.parse_compare_operator(text) {
                            operators.push(op);
                        }
                    }
                } else if child.kind() != "(" && child.kind() != ")" {
                    operands.push(self.node_to_ast(child, source)?);
                }
            }
        }

        if operands.is_empty() {
            return Err(ParseError::TreeSitterError("No operands in comparison".to_string()).into());
        }

        let left = operands.remove(0);

        Ok((left, operators, operands))
    }

    fn extract_boolean_op_info(&self, node: &Node, source: &str) -> Result<(AstNode, BinaryOperator, AstNode)> {
        let left_node = node
            .child_by_field_name("left")
            .ok_or_else(|| ParseError::TreeSitterError("Missing boolean operator left operand".to_string()))?;
        let left = self.node_to_ast(left_node, source)?;

        let right_node = node
            .child_by_field_name("right")
            .ok_or_else(|| ParseError::TreeSitterError("Missing boolean operator right operand".to_string()))?;
        let right = self.node_to_ast(right_node, source)?;

        let mut cursor = node.walk();
        let op_str = node
            .children(&mut cursor)
            .find(|n| n.kind() == "and" || n.kind() == "or")
            .and_then(|n| n.utf8_text(source.as_bytes()).ok())
            .ok_or_else(|| ParseError::TreeSitterError("Missing boolean operator".to_string()))?;

        let op = match op_str {
            "and" => BinaryOperator::And,
            "or" => BinaryOperator::Or,
            _ => return Err(ParseError::TreeSitterError(format!("Unknown boolean operator: {op_str}")).into()),
        };

        Ok((left, op, right))
    }

    fn extract_lambda_info(&self, node: &Node, source: &str) -> Result<(Vec<Parameter>, AstNode)> {
        let args = node
            .child_by_field_name("parameters")
            .map(|params| self.extract_function_args(&params, source))
            .transpose()?
            .unwrap_or_default();

        let body_node = node
            .child_by_field_name("body")
            .ok_or_else(|| ParseError::TreeSitterError("Missing lambda body".to_string()))?;
        let body = self.node_to_ast(body_node, source)?;

        Ok((args, body))
    }

    fn extract_subscript_info(&self, node: &Node, source: &str) -> Result<(AstNode, AstNode)> {
        let value_node = node
            .child_by_field_name("value")
            .ok_or_else(|| ParseError::TreeSitterError("Missing subscript value".to_string()))?;
        let value = self.node_to_ast(value_node, source)?;

        let subscript_node = node
            .child_by_field_name("subscript")
            .ok_or_else(|| ParseError::TreeSitterError("Missing subscript index".to_string()))?;
        let slice = self.node_to_ast(subscript_node, source)?;

        Ok((value, slice))
    }

    fn extract_match_info(&self, node: &Node, source: &str) -> Result<(AstNode, Vec<MatchCase>)> {
        let subject_node = node
            .child_by_field_name("subject")
            .ok_or_else(|| ParseError::TreeSitterError("Missing match subject".to_string()))?;
        let subject = self.node_to_ast(subject_node, source)?;

        let mut cases = Vec::new();
        let mut cursor = node.walk();

        for child in node.children(&mut cursor) {
            if child.kind() == "case_clause" {
                let case = self.extract_match_case(&child, source)?;
                cases.push(case);
            }
        }

        Ok((subject, cases))
    }

    fn extract_match_case(&self, node: &Node, source: &str) -> Result<MatchCase> {
        let pattern_node = node
            .child_by_field_name("pattern")
            .ok_or_else(|| ParseError::TreeSitterError("Missing case pattern".to_string()))?;
        let pattern = self.extract_pattern(&pattern_node, source)?;

        let guard = node
            .child_by_field_name("guard")
            .map(|g| self.node_to_ast(g, source))
            .transpose()?;

        let body_node = node
            .child_by_field_name("consequence")
            .ok_or_else(|| ParseError::TreeSitterError("Missing case body".to_string()))?;
        let body = self.extract_body(&body_node, source)?;

        Ok(MatchCase { pattern, guard, body })
    }

    fn extract_pattern(&self, node: &Node, source: &str) -> Result<Pattern> {
        match node.kind() {
            "as_pattern" => {
                let pattern = node
                    .child_by_field_name("pattern")
                    .map(|p| self.extract_pattern(&p, source))
                    .transpose()?
                    .map(Box::new);

                let name = node
                    .child_by_field_name("alias")
                    .and_then(|n| n.utf8_text(source.as_bytes()).ok())
                    .map(|s| s.to_string());

                Ok(Pattern::MatchAs { pattern, name })
            }
            "list_pattern" | "tuple_pattern" => {
                let mut patterns = Vec::new();
                let mut cursor = node.walk();

                for child in node.children(&mut cursor) {
                    if !child.is_extra()
                        && child.kind() != "["
                        && child.kind() != "]"
                        && child.kind() != "("
                        && child.kind() != ")"
                        && child.kind() != ","
                    {
                        patterns.push(self.extract_pattern(&child, source)?);
                    }
                }

                Ok(Pattern::MatchSequence(patterns))
            }
            "dict_pattern" => {
                let mut keys = Vec::new();
                let mut patterns = Vec::new();
                let mut cursor = node.walk();

                for child in node.children(&mut cursor) {
                    if child.kind() == "pair" {
                        if let Some(key_node) = child.child_by_field_name("key") {
                            keys.push(self.node_to_ast(key_node, source)?);
                        }
                        if let Some(value_node) = child.child_by_field_name("value") {
                            patterns.push(self.extract_pattern(&value_node, source)?);
                        }
                    }
                }

                Ok(Pattern::MatchMapping { keys, patterns })
            }
            "class_pattern" => {
                let cls = node
                    .child_by_field_name("class")
                    .and_then(|n| n.utf8_text(source.as_bytes()).ok())
                    .ok_or_else(|| ParseError::TreeSitterError("Missing class in class pattern".to_string()))?
                    .to_string();

                let mut patterns = Vec::new();
                let mut cursor = node.walk();

                for child in node.children(&mut cursor) {
                    if child.kind() == "pattern" {
                        patterns.push(self.extract_pattern(&child, source)?);
                    }
                }

                Ok(Pattern::MatchClass { cls, patterns })
            }
            "or_pattern" => {
                let mut patterns = Vec::new();
                let mut cursor = node.walk();

                for child in node.children(&mut cursor) {
                    if !child.is_extra() && child.kind() != "|" {
                        patterns.push(self.extract_pattern(&child, source)?);
                    }
                }

                Ok(Pattern::MatchOr(patterns))
            }
            _ => {
                let value = self.node_to_ast(*node, source)?;
                Ok(Pattern::MatchValue(value))
            }
        }
    }

    fn extract_body(&self, node: &Node, source: &str) -> Result<Vec<AstNode>> {
        let mut body = Vec::new();
        let mut cursor = node.walk();

        for child in node.children(&mut cursor) {
            if !child.is_extra() {
                body.push(self.node_to_ast(child, source)?);
            }
        }

        Ok(body)
    }

    fn is_binary_operator(&self, kind: &str) -> bool {
        matches!(
            kind,
            "+" | "-" | "*" | "/" | "//" | "%" | "**" | "@" | "&" | "|" | "^" | "<<" | ">>"
        )
    }

    fn is_unary_operator(&self, kind: &str) -> bool {
        matches!(kind, "not" | "~" | "+" | "-")
    }

    fn is_compare_operator(&self, kind: &str) -> bool {
        matches!(kind, "==" | "!=" | "<" | "<=" | ">" | ">=" | "is" | "in" | "not")
    }

    fn parse_binary_operator(&self, op: &str) -> Result<BinaryOperator> {
        match op {
            "+" => Ok(BinaryOperator::Add),
            "-" => Ok(BinaryOperator::Sub),
            "*" => Ok(BinaryOperator::Mult),
            "/" => Ok(BinaryOperator::Div),
            "//" => Ok(BinaryOperator::FloorDiv),
            "%" => Ok(BinaryOperator::Mod),
            "**" => Ok(BinaryOperator::Pow),
            "@" => Ok(BinaryOperator::MatMult),
            "&" => Ok(BinaryOperator::BitAnd),
            "|" => Ok(BinaryOperator::BitOr),
            "^" => Ok(BinaryOperator::BitXor),
            "<<" => Ok(BinaryOperator::LeftShift),
            ">>" => Ok(BinaryOperator::RightShift),
            _ => Err(ParseError::TreeSitterError(format!("Unknown binary operator: {op}")).into()),
        }
    }

    fn parse_unary_operator(&self, op: &str) -> Result<UnaryOperator> {
        match op {
            "not" => Ok(UnaryOperator::Not),
            "~" => Ok(UnaryOperator::Invert),
            "+" => Ok(UnaryOperator::Plus),
            "-" => Ok(UnaryOperator::Minus),
            _ => Err(ParseError::TreeSitterError(format!("Unknown unary operator: {op}")).into()),
        }
    }

    fn parse_compare_operator(&self, op: &str) -> Result<CompareOperator> {
        match op {
            "==" => Ok(CompareOperator::Eq),
            "!=" => Ok(CompareOperator::NotEq),
            "<" => Ok(CompareOperator::Lt),
            "<=" => Ok(CompareOperator::LtE),
            ">" => Ok(CompareOperator::Gt),
            ">=" => Ok(CompareOperator::GtE),
            "is" => Ok(CompareOperator::Is),
            "is not" => Ok(CompareOperator::IsNot),
            "in" => Ok(CompareOperator::In),
            "not in" => Ok(CompareOperator::NotIn),
            _ => Err(ParseError::TreeSitterError(format!("Unknown comparison operator: {op}")).into()),
        }
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
            .map_err(|e| ParseError::TreeSitterError(format!("Name resolution failed: {e}")))?;
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
                        assert_eq!(target, "x");
                        assert!(type_annotation.contains("int"));
                        assert!(value.is_some());
                    }
                    _ => panic!("Expected annotated assignment, got {:?}", &body[0]),
                }

                match &body[1] {
                    AstNode::AnnotatedAssignment { target, type_annotation, value, .. } => {
                        assert_eq!(target, "y");
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
            AstNode::Module { body, .. } => {
                assert!(matches!(body[0], AstNode::If { .. }));
            }
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
                    assert_eq!(target, "x");
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
                    assert!(else_body.is_some());
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
                AstNode::Assignment { value, .. } => {
                    assert!(matches!(
                        value.as_ref(),
                        AstNode::Literal { value: LiteralValue::Integer(1), .. }
                    ));
                }
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
}

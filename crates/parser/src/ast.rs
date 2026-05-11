//! Parser AST model.

/// Function parameter with position information
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub name: String,
    pub line: usize,
    pub col: usize,
    pub end_line: usize,
    pub end_col: usize,
    pub type_annotation: Option<String>,
    pub default_value: Option<Box<AstNode>>,
}

/// Imported name with position information
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct ImportName {
    pub name: String,
    pub line: usize,
    pub col: usize,
    pub end_line: usize,
    pub end_col: usize,
}

/// Basic AST node types for Python
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(tag = "type"))]
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
        is_async: bool,
        line: usize,
        col: usize,
        end_line: usize,
        end_col: usize,
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
        end_line: usize,
        end_col: usize,
    },
    Assignment {
        target: Box<AstNode>,
        value: Box<AstNode>,
        line: usize,
        col: usize,
        end_line: usize,
        end_col: usize,
    },
    AnnotatedAssignment {
        target: Box<AstNode>,
        type_annotation: String,
        value: Option<Box<AstNode>>,
        line: usize,
        col: usize,
        end_line: usize,
        end_col: usize,
    },
    Call {
        function: Box<AstNode>,
        args: Vec<AstNode>,
        keywords: Vec<(String, AstNode)>,
        line: usize,
        col: usize,
        end_line: usize,
        end_col: usize,
    },
    Identifier {
        name: String,
        line: usize,
        col: usize,
        end_line: usize,
        end_col: usize,
    },
    Literal {
        value: LiteralValue,
        line: usize,
        col: usize,
        end_line: usize,
        end_col: usize,
    },
    Return {
        value: Option<Box<AstNode>>,
        line: usize,
        col: usize,
        end_line: usize,
        end_col: usize,
    },
    /// Import statement: import module [as alias]
    Import {
        module: String,
        alias: Option<String>,
        extra_modules: Vec<(String, Option<String>)>,
        line: usize,
        col: usize,
        end_line: usize,
        end_col: usize,
    },
    /// Import from statement: from module import names
    ImportFrom {
        module: String,
        names: Vec<ImportName>,
        line: usize,
        col: usize,
        end_line: usize,
        end_col: usize,
    },
    /// Attribute access: object.attribute
    Attribute {
        object: Box<AstNode>,
        attribute: String,
        line: usize,
        col: usize,
        end_line: usize,
        end_col: usize,
    },
    /// If statement: if test: body elif test: body else: body
    If {
        test: Box<AstNode>,
        body: Vec<AstNode>,
        elif_parts: Vec<(AstNode, Vec<AstNode>)>,
        else_body: Option<Vec<AstNode>>,
        line: usize,
        col: usize,
        end_line: usize,
        end_col: usize,
    },
    /// For loop: for target in iter: body
    For {
        target: Box<AstNode>,
        iter: Box<AstNode>,
        body: Vec<AstNode>,
        else_body: Option<Vec<AstNode>>,
        is_async: bool,
        line: usize,
        col: usize,
        end_line: usize,
        end_col: usize,
    },
    /// While loop: while test: body
    While {
        test: Box<AstNode>,
        body: Vec<AstNode>,
        else_body: Option<Vec<AstNode>>,
        line: usize,
        col: usize,
        end_line: usize,
        end_col: usize,
    },
    /// Try/except/finally: try: body except: handlers else: orelse finally: finalbody
    Try {
        body: Vec<AstNode>,
        handlers: Vec<ExceptHandler>,
        else_body: Option<Vec<AstNode>>,
        finally_body: Option<Vec<AstNode>>,
        line: usize,
        col: usize,
        end_line: usize,
        end_col: usize,
    },
    /// With statement: with item as target: body
    With {
        items: Vec<WithItem>,
        body: Vec<AstNode>,
        is_async: bool,
        line: usize,
        col: usize,
        end_line: usize,
        end_col: usize,
    },
    /// List comprehension: [expr for target in iter if cond]
    ListComp {
        element: Box<AstNode>,
        generators: Vec<Comprehension>,
        line: usize,
        col: usize,
        end_line: usize,
        end_col: usize,
    },
    /// Dict comprehension: {key: value for target in iter if cond}
    DictComp {
        key: Box<AstNode>,
        value: Box<AstNode>,
        generators: Vec<Comprehension>,
        line: usize,
        col: usize,
        end_line: usize,
        end_col: usize,
    },
    /// Set comprehension: {expr for target in iter if cond}
    SetComp {
        element: Box<AstNode>,
        generators: Vec<Comprehension>,
        line: usize,
        col: usize,
        end_line: usize,
        end_col: usize,
    },
    /// Generator expression: (expr for target in iter if cond)
    GeneratorExp {
        element: Box<AstNode>,
        generators: Vec<Comprehension>,
        line: usize,
        col: usize,
        end_line: usize,
        end_col: usize,
    },
    /// Named expression (walrus operator): target := value
    NamedExpr {
        target: String,
        value: Box<AstNode>,
        line: usize,
        col: usize,
        end_line: usize,
        end_col: usize,
    },
    /// Binary operation: left op right
    BinaryOp {
        left: Box<AstNode>,
        op: BinaryOperator,
        right: Box<AstNode>,
        line: usize,
        col: usize,
        end_line: usize,
        end_col: usize,
    },
    /// Unary operation: op operand
    UnaryOp {
        op: UnaryOperator,
        operand: Box<AstNode>,
        line: usize,
        col: usize,
        end_line: usize,
        end_col: usize,
    },
    /// Comparison operation: left op right (can chain)
    Compare {
        left: Box<AstNode>,
        ops: Vec<CompareOperator>,
        comparators: Vec<AstNode>,
        line: usize,
        col: usize,
        end_line: usize,
        end_col: usize,
    },
    /// Lambda expression: lambda args: body
    Lambda {
        args: Vec<Parameter>,
        body: Box<AstNode>,
        line: usize,
        col: usize,
        end_line: usize,
        end_col: usize,
    },
    /// Subscript: value[slice]
    Subscript {
        value: Box<AstNode>,
        slice: Box<AstNode>,
        line: usize,
        col: usize,
        end_line: usize,
        end_col: usize,
    },
    /// Match statement (PEP 634): match subject: case pattern: body
    Match {
        subject: Box<AstNode>,
        cases: Vec<MatchCase>,
        line: usize,
        col: usize,
        end_line: usize,
        end_col: usize,
    },
    /// Pass statement
    Pass {
        line: usize,
        col: usize,
        end_line: usize,
        end_col: usize,
    },
    /// Break statement
    Break {
        line: usize,
        col: usize,
        end_line: usize,
        end_col: usize,
    },
    /// Continue statement
    Continue {
        line: usize,
        col: usize,
        end_line: usize,
        end_col: usize,
    },
    /// Raise statement
    Raise {
        exc: Option<Box<AstNode>>,
        line: usize,
        col: usize,
        end_line: usize,
        end_col: usize,
    },
    /// Global statement: global x, y, z
    Global {
        names: Vec<String>,
        line: usize,
        col: usize,
        end_line: usize,
        end_col: usize,
    },
    /// Nonlocal statement: nonlocal x, y, z
    Nonlocal {
        names: Vec<String>,
        line: usize,
        col: usize,
        end_line: usize,
        end_col: usize,
    },
    /// Tuple literal: (1, 2, 3) or (x,)
    Tuple {
        elements: Vec<AstNode>,
        is_parenthesized: bool,
        line: usize,
        col: usize,
        end_line: usize,
        end_col: usize,
    },
    /// List literal: [1, 2, 3] or []
    List {
        elements: Vec<AstNode>,
        line: usize,
        col: usize,
        end_line: usize,
        end_col: usize,
    },
    /// Dict literal: {'a': 1, 'b': 2} or {}
    Dict {
        keys: Vec<AstNode>,
        values: Vec<AstNode>,
        line: usize,
        col: usize,
        end_line: usize,
        end_col: usize,
    },
    /// Set literal: {1, 2, 3}
    Set {
        elements: Vec<AstNode>,
        line: usize,
        col: usize,
        end_line: usize,
        end_col: usize,
    },
    /// Yield expression: yield value
    Yield {
        value: Option<Box<AstNode>>,
        line: usize,
        col: usize,
        end_line: usize,
        end_col: usize,
    },
    /// Yield from expression: yield from iterable
    YieldFrom {
        value: Box<AstNode>,
        line: usize,
        col: usize,
        end_line: usize,
        end_col: usize,
    },
    /// Await expression: await coroutine
    Await {
        value: Box<AstNode>,
        line: usize,
        col: usize,
        end_line: usize,
        end_col: usize,
    },
    /// Assert statement: assert test, msg
    Assert {
        test: Box<AstNode>,
        msg: Option<Box<AstNode>>,
        line: usize,
        col: usize,
        end_line: usize,
        end_col: usize,
    },
    /// Starred expression: *value (used in unpacking)
    Starred {
        value: Box<AstNode>,
        line: usize,
        col: usize,
        end_line: usize,
        end_col: usize,
    },
    /// Parenthesized expression: (expr) - preserves explicit parentheses
    ParenthesizedExpression {
        expression: Box<AstNode>,
        line: usize,
        col: usize,
        end_line: usize,
        end_col: usize,
    },
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(tag = "kind", content = "data"))]
#[derive(Debug, Clone, PartialEq)]
pub enum LiteralValue {
    String { value: String, prefix: String },
    Integer(i64),
    Float(f64),
    Boolean(bool),
    None,
}

/// Exception handler for try/except
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct ExceptHandler {
    pub exception_type: Option<String>,
    pub name: Option<String>,
    pub body: Vec<AstNode>,
    pub line: usize,
    pub col: usize,
    pub end_line: usize,
    pub end_col: usize,
}

/// With item for with statements
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct WithItem {
    pub context_expr: AstNode,
    pub optional_vars: Option<String>,
}

/// Comprehension clause: for target in iter if conditions
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct Comprehension {
    pub target: String,
    pub iter: AstNode,
    pub ifs: Vec<AstNode>,
}

/// Binary operators
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
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
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperator {
    Not,
    Invert,
    Plus,
    Minus,
}

/// Comparison operators
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
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
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct MatchCase {
    pub pattern: Pattern,
    pub guard: Option<AstNode>,
    pub body: Vec<AstNode>,
    pub pattern_line: usize,
    pub pattern_col: usize,
    pub pattern_end_line: usize,
    pub pattern_end_col: usize,
    pub line: usize,
    pub col: usize,
    pub end_line: usize,
    pub end_col: usize,
}

/// Pattern for pattern matching
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
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

impl AstNode {
    /// Extract variable names from an assignment target
    ///
    /// This helper extracts all identifier names that will be bound by an assignment.
    /// For simple identifiers, returns a vec with one name. For tuple/list unpacking,
    /// returns all names. For starred expressions, includes the starred variable.
    /// For attribute access or subscripts, returns empty vec (no new variables bound).
    pub fn extract_target_names(&self) -> Vec<String> {
        match self {
            AstNode::Identifier { name, .. } => vec![name.clone()],
            AstNode::Tuple { elements, .. } | AstNode::List { elements, .. } => {
                elements.iter().flat_map(|e| e.extract_target_names()).collect()
            }
            AstNode::Starred { value, .. } => value.extract_target_names(),
            AstNode::Attribute { .. } | AstNode::Subscript { .. } => Vec::new(),
            _ => Vec::new(),
        }
    }

    /// Convert assignment target to a string representation
    ///
    /// This is a compatibility helper for code that expects string targets.
    /// For simple identifiers, returns the name. For complex patterns, returns
    /// a formatted representation.
    pub fn target_to_string(&self) -> String {
        match self {
            AstNode::Identifier { name, .. } => name.clone(),
            AstNode::Tuple { elements, is_parenthesized, .. } => {
                let names: Vec<String> = elements.iter().map(|e| e.target_to_string()).collect();
                if *is_parenthesized { format!("({})", names.join(", ")) } else { names.join(", ") }
            }
            AstNode::List { elements, .. } => {
                let names: Vec<String> = elements.iter().map(|e| e.target_to_string()).collect();
                format!("[{}]", names.join(", "))
            }
            AstNode::Starred { value, .. } => format!("*{}", value.target_to_string()),
            AstNode::Attribute { object, attribute, .. } => {
                format!("{}.{}", object.target_to_string(), attribute)
            }
            AstNode::Subscript { value, slice, .. } => {
                format!("{}[{}]", value.target_to_string(), slice.target_to_string())
            }
            _ => "<unknown>".to_string(),
        }
    }

    /// Extract function name as a string from a call function node
    ///
    /// This is a compatibility helper for code that expects string function names.
    /// For simple identifiers, returns the name. For attributes, returns the full
    /// dotted path (e.g., "obj.method").
    pub fn function_to_string(&self) -> String {
        match self {
            AstNode::Identifier { name, .. } => name.clone(),
            AstNode::Attribute { object, attribute, .. } => {
                format!("{}.{}", object.function_to_string(), attribute)
            }
            _ => "<unknown>".to_string(),
        }
    }
}

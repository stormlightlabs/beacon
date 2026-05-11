//! Parser AST model.

use crate::SourceRange;
use std::collections::BTreeSet;

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
    pub target: Box<AstNode>,
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
    /// Return this node's source range.
    pub fn source_range(&self) -> SourceRange {
        match self {
            AstNode::Module { .. } => SourceRange::new(1, 1, 1, 1),
            AstNode::FunctionDef { line, col, end_line, end_col, .. }
            | AstNode::ClassDef { line, col, end_line, end_col, .. }
            | AstNode::Assignment { line, col, end_line, end_col, .. }
            | AstNode::AnnotatedAssignment { line, col, end_line, end_col, .. }
            | AstNode::Call { line, col, end_line, end_col, .. }
            | AstNode::Identifier { line, col, end_line, end_col, .. }
            | AstNode::Literal { line, col, end_line, end_col, .. }
            | AstNode::Return { line, col, end_line, end_col, .. }
            | AstNode::Import { line, col, end_line, end_col, .. }
            | AstNode::ImportFrom { line, col, end_line, end_col, .. }
            | AstNode::Attribute { line, col, end_line, end_col, .. }
            | AstNode::If { line, col, end_line, end_col, .. }
            | AstNode::For { line, col, end_line, end_col, .. }
            | AstNode::While { line, col, end_line, end_col, .. }
            | AstNode::Try { line, col, end_line, end_col, .. }
            | AstNode::With { line, col, end_line, end_col, .. }
            | AstNode::ListComp { line, col, end_line, end_col, .. }
            | AstNode::DictComp { line, col, end_line, end_col, .. }
            | AstNode::SetComp { line, col, end_line, end_col, .. }
            | AstNode::GeneratorExp { line, col, end_line, end_col, .. }
            | AstNode::NamedExpr { line, col, end_line, end_col, .. }
            | AstNode::BinaryOp { line, col, end_line, end_col, .. }
            | AstNode::UnaryOp { line, col, end_line, end_col, .. }
            | AstNode::Compare { line, col, end_line, end_col, .. }
            | AstNode::Lambda { line, col, end_line, end_col, .. }
            | AstNode::Subscript { line, col, end_line, end_col, .. }
            | AstNode::Match { line, col, end_line, end_col, .. }
            | AstNode::Pass { line, col, end_line, end_col, .. }
            | AstNode::Break { line, col, end_line, end_col, .. }
            | AstNode::Continue { line, col, end_line, end_col, .. }
            | AstNode::Raise { line, col, end_line, end_col, .. }
            | AstNode::Global { line, col, end_line, end_col, .. }
            | AstNode::Nonlocal { line, col, end_line, end_col, .. }
            | AstNode::Tuple { line, col, end_line, end_col, .. }
            | AstNode::List { line, col, end_line, end_col, .. }
            | AstNode::Dict { line, col, end_line, end_col, .. }
            | AstNode::Set { line, col, end_line, end_col, .. }
            | AstNode::Yield { line, col, end_line, end_col, .. }
            | AstNode::YieldFrom { line, col, end_line, end_col, .. }
            | AstNode::Await { line, col, end_line, end_col, .. }
            | AstNode::Assert { line, col, end_line, end_col, .. }
            | AstNode::Starred { line, col, end_line, end_col, .. }
            | AstNode::ParenthesizedExpression { line, col, end_line, end_col, .. } => {
                SourceRange::new(*line, *col, *end_line, *end_col)
            }
        }
    }

    /// Return direct statement blocks owned by this node.
    pub fn body_blocks(&self) -> Vec<&[AstNode]> {
        match self {
            AstNode::Module { body, .. }
            | AstNode::FunctionDef { body, .. }
            | AstNode::ClassDef { body, .. }
            | AstNode::With { body, .. } => vec![body.as_slice()],
            AstNode::If { body, elif_parts, else_body, .. } => {
                let mut blocks = vec![body.as_slice()];
                blocks.extend(elif_parts.iter().map(|(_, block)| block.as_slice()));
                if let Some(block) = else_body {
                    blocks.push(block.as_slice());
                }
                blocks
            }
            AstNode::For { body, else_body, .. } | AstNode::While { body, else_body, .. } => {
                let mut blocks = vec![body.as_slice()];
                if let Some(block) = else_body {
                    blocks.push(block.as_slice());
                }
                blocks
            }
            AstNode::Try { body, handlers, else_body, finally_body, .. } => {
                let mut blocks = vec![body.as_slice()];
                blocks.extend(handlers.iter().map(|handler| handler.body.as_slice()));
                if let Some(block) = else_body {
                    blocks.push(block.as_slice());
                }
                if let Some(block) = finally_body {
                    blocks.push(block.as_slice());
                }
                blocks
            }
            AstNode::Match { cases, .. } => cases.iter().map(|case| case.body.as_slice()).collect(),
            _ => Vec::new(),
        }
    }

    /// Return direct child statements without forcing callers to duplicate body matching.
    pub fn child_statements(&self) -> Vec<&AstNode> {
        self.body_blocks().into_iter().flatten().collect()
    }

    /// Return direct child expressions owned by this node.
    pub fn child_expressions(&self) -> Vec<&AstNode> {
        match self {
            AstNode::Assignment { target, value, .. } => vec![target.as_ref(), value.as_ref()],
            AstNode::AnnotatedAssignment { target, value, .. } => {
                let mut children = vec![target.as_ref()];
                if let Some(value) = value {
                    children.push(value.as_ref());
                }
                children
            }
            AstNode::Call { function, args, keywords, .. } => {
                let mut children = Vec::with_capacity(1 + args.len() + keywords.len());
                children.push(function.as_ref());
                children.extend(args.iter());
                children.extend(keywords.iter().map(|(_, value)| value));
                children
            }
            AstNode::Return { value, .. } | AstNode::Yield { value, .. } => value.iter().map(Box::as_ref).collect(),
            AstNode::Attribute { object, .. } => vec![object.as_ref()],
            AstNode::If { test, elif_parts, .. } => {
                let mut children = vec![test.as_ref()];
                children.extend(elif_parts.iter().map(|(test, _)| test));
                children
            }
            AstNode::For { target, iter, .. } => vec![target.as_ref(), iter.as_ref()],
            AstNode::While { test, .. } => vec![test.as_ref()],
            AstNode::With { items, .. } => items.iter().map(|item| &item.context_expr).collect(),
            AstNode::ListComp { element, generators, .. }
            | AstNode::SetComp { element, generators, .. }
            | AstNode::GeneratorExp { element, generators, .. } => {
                let mut children = vec![element.as_ref()];
                children.extend(generators.iter().flat_map(Comprehension::child_expressions));
                children
            }
            AstNode::DictComp { key, value, generators, .. } => {
                let mut children = vec![key.as_ref(), value.as_ref()];
                children.extend(generators.iter().flat_map(Comprehension::child_expressions));
                children
            }
            AstNode::NamedExpr { value, .. } => vec![value.as_ref()],
            AstNode::BinaryOp { left, right, .. } => vec![left.as_ref(), right.as_ref()],
            AstNode::UnaryOp { operand, .. } => vec![operand.as_ref()],
            AstNode::Compare { left, comparators, .. } => {
                let mut children = vec![left.as_ref()];
                children.extend(comparators.iter());
                children
            }
            AstNode::Lambda { body, .. } => vec![body.as_ref()],
            AstNode::Subscript { value, slice, .. } => vec![value.as_ref(), slice.as_ref()],
            AstNode::Match { subject, cases, .. } => {
                let mut children = vec![subject.as_ref()];
                children.extend(cases.iter().filter_map(|case| case.guard.as_ref()));
                children
            }
            AstNode::Raise { exc, .. } => exc.iter().map(Box::as_ref).collect(),
            AstNode::Tuple { elements, .. } | AstNode::List { elements, .. } | AstNode::Set { elements, .. } => {
                elements.iter().collect()
            }
            AstNode::Dict { keys, values, .. } => keys.iter().chain(values).collect(),
            AstNode::YieldFrom { value, .. }
            | AstNode::Await { value, .. }
            | AstNode::Starred { value, .. }
            | AstNode::ParenthesizedExpression { expression: value, .. } => vec![value.as_ref()],
            AstNode::Assert { test, msg, .. } => {
                let mut children = vec![test.as_ref()];
                if let Some(msg) = msg {
                    children.push(msg.as_ref());
                }
                children
            }
            AstNode::Module { .. }
            | AstNode::FunctionDef { .. }
            | AstNode::ClassDef { .. }
            | AstNode::Identifier { .. }
            | AstNode::Literal { .. }
            | AstNode::Import { .. }
            | AstNode::ImportFrom { .. }
            | AstNode::Try { .. }
            | AstNode::Pass { .. }
            | AstNode::Break { .. }
            | AstNode::Continue { .. }
            | AstNode::Global { .. }
            | AstNode::Nonlocal { .. } => Vec::new(),
        }
    }

    /// Extract names introduced by an assignment-like target.
    ///
    /// This helper extracts all identifier names that will be bound by an assignment.
    /// - For simple identifiers, returns a vec with one name.
    /// - For tuple/list unpacking, returns all names.
    /// - For starred expressions, includes the starred variable.
    /// - For attribute access or subscripts, returns empty vec (no new variables bound).
    pub fn binding_names(&self) -> Vec<String> {
        match self {
            AstNode::Identifier { name, .. } => vec![name.clone()],
            AstNode::Tuple { elements, .. } | AstNode::List { elements, .. } => {
                elements.iter().flat_map(|e| e.binding_names()).collect()
            }
            AstNode::Starred { value, .. } => value.binding_names(),
            AstNode::Attribute { .. } | AstNode::Subscript { .. } => Vec::new(),
            _ => Vec::new(),
        }
    }

    /// Return a display string for an assignment-like target.
    pub fn target_display(&self) -> String {
        match self {
            AstNode::Identifier { name, .. } => name.clone(),
            AstNode::Tuple { elements, is_parenthesized, .. } => {
                let names: Vec<String> = elements.iter().map(|e| e.target_display()).collect();
                if *is_parenthesized { format!("({})", names.join(", ")) } else { names.join(", ") }
            }
            AstNode::List { elements, .. } => {
                let names: Vec<String> = elements.iter().map(|e| e.target_display()).collect();
                format!("[{}]", names.join(", "))
            }
            AstNode::Starred { value, .. } => format!("*{}", value.target_display()),
            AstNode::Attribute { object, attribute, .. } => {
                format!("{}.{}", object.target_display(), attribute)
            }
            AstNode::Subscript { value, slice, .. } => {
                format!("{}[{}]", value.target_display(), slice.target_display())
            }
            _ => "<unknown>".to_string(),
        }
    }

    /// Return a dotted qualified name for identifier and attribute expressions.
    pub fn qualified_name(&self) -> Option<String> {
        match self {
            AstNode::Identifier { name, .. } => Some(name.clone()),
            AstNode::Attribute { object, attribute, .. } => object
                .qualified_name()
                .map(|object_name| format!("{object_name}.{attribute}")),
            _ => None,
        }
    }
}

impl Comprehension {
    pub fn target_names(&self) -> Vec<String> {
        self.target.binding_names()
    }

    pub fn target_display(&self) -> String {
        self.target.target_display()
    }

    pub fn child_expressions(&self) -> Vec<&AstNode> {
        let mut children = vec![self.target.as_ref()];
        children.push(&self.iter);
        children.extend(self.ifs.iter());
        children
    }
}

impl Pattern {
    pub fn binding_names(&self) -> Vec<String> {
        match self {
            Pattern::MatchValue(_) => Vec::new(),
            Pattern::MatchSequence(patterns) => patterns.iter().flat_map(Pattern::binding_names).collect(),
            Pattern::MatchMapping { patterns, .. } | Pattern::MatchClass { patterns, .. } => {
                patterns.iter().flat_map(Pattern::binding_names).collect()
            }
            Pattern::MatchOr(patterns) => {
                let mut alternatives = patterns.iter();
                let Some(first) = alternatives.next() else {
                    return Vec::new();
                };
                let mut names: BTreeSet<String> = first.binding_names().into_iter().collect();
                for alternative in alternatives {
                    let alternative_names: BTreeSet<String> = alternative.binding_names().into_iter().collect();
                    names.retain(|name| alternative_names.contains(name));
                }
                names.into_iter().collect()
            }
            Pattern::MatchAs { pattern, name } => {
                let mut names = pattern.as_deref().map(Pattern::binding_names).unwrap_or_default();
                if let Some(name) = name {
                    names.push(name.clone());
                }
                names
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_builders;

    #[test]
    fn source_range_is_shared_for_all_callers() {
        let node = test_builders::ident("value");
        assert_eq!(node.source_range(), SourceRange::new(1, 1, 1, 6));
    }

    #[test]
    fn child_statement_helpers_expose_blocks() {
        let node =
            AstNode::Module { body: vec![test_builders::assignment("x", test_builders::int(1))], docstring: None };
        assert_eq!(node.child_statements().len(), 1);
        assert_eq!(node.body_blocks().len(), 1);
    }

    #[test]
    fn comprehension_uses_structured_target() {
        let generator = Comprehension {
            target: Box::new(AstNode::Tuple {
                elements: vec![test_builders::ident("x"), test_builders::ident("y")],
                is_parenthesized: false,
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 5,
            }),
            iter: test_builders::ident("items"),
            ifs: Vec::new(),
        };

        assert_eq!(generator.target_names(), vec!["x".to_string(), "y".to_string()]);
        assert_eq!(generator.target_display(), "x, y");
    }

    #[test]
    fn pattern_binding_names_keep_only_common_or_bindings() {
        let pattern = Pattern::MatchOr(vec![
            Pattern::MatchAs { pattern: None, name: Some("x".to_string()) },
            Pattern::MatchSequence(vec![
                Pattern::MatchAs { pattern: None, name: Some("x".to_string()) },
                Pattern::MatchAs { pattern: None, name: Some("y".to_string()) },
            ]),
        ]);

        assert_eq!(pattern.binding_names(), vec!["x".to_string()]);
    }
}

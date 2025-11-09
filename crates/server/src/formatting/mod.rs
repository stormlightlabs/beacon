//! Python code formatting infrastructure
//!
//! Provides PEP8-compliant code formatting for Python source files.
//! This module coordinates parsing, token stream generation, and formatting rules.

pub mod config;
pub mod context;
pub mod import;
pub mod rules;
pub mod state;
pub mod token_stream;
pub mod writer;

pub use config::FormatterConfig;
pub use context::FormattingContext;
pub use import::{ImportCategory, ImportSorter, ImportStatement};
pub use rules::FormattingRules;
pub use state::FormatterState;
pub use token_stream::TokenStream;
pub use writer::FormattedWriter;

use beacon_core::Result;
use beacon_parser::{AstNode, ParsedFile, PythonParser};

/// Main formatter for Python code
///
/// The formatter operates in multiple passes:
/// 1. Parse source to AST (already done via ParsedFile)
/// 2. Generate token stream from AST
/// 3. Apply formatting rules while tracking context
/// 4. Emit formatted output
pub struct Formatter {
    #[allow(dead_code)]
    config: FormatterConfig,
    parser: PythonParser,
}

impl Formatter {
    /// Create a new formatter with the given configuration
    pub fn new(config: FormatterConfig, parser: PythonParser) -> Self {
        Self { config, parser }
    }

    /// Create a formatter with default PEP8 configuration
    pub fn with_defaults() -> Self {
        Self::new(FormatterConfig::default(), PythonParser::default())
    }

    /// Format a parsed Python file
    pub fn format_file(&self, parsed: &ParsedFile) -> Result<String> {
        let ast = self.parser.to_ast(parsed)?;
        let token_stream = TokenStream::from_ast(&ast);

        let mut writer = FormattedWriter::new(&self.config);
        let _rules = FormattingRules::new(self.config.clone());

        for token in token_stream {
            writer.write_token(&token);
        }

        Ok(writer.output().to_string())
    }

    /// Format a specific AST node
    pub fn format_node(&self, node: &AstNode, _source: &str) -> Result<String> {
        let token_stream = TokenStream::from_ast(node);

        let mut writer = FormattedWriter::new(&self.config);

        for token in token_stream {
            writer.write_token(&token);
        }

        Ok(writer.output().to_string())
    }

    /// Format a range within source code
    ///
    /// Formats only the AST nodes that fall within the specified line range.
    /// Returns the formatted content for the specified range.
    pub fn format_range(&mut self, source: &str, start_line: usize, end_line: usize) -> Result<String> {
        let parsed = self.parser.parse(source)?;
        let ast = self.parser.to_ast(&parsed)?;

        let filtered_node = self.filter_node_by_range(&ast, start_line, end_line);

        let token_stream = TokenStream::from_ast(&filtered_node);
        let mut writer = FormattedWriter::new(&self.config);

        for token in token_stream {
            writer.write_token(&token);
        }

        Ok(writer.output().to_string())
    }

    /// Filter an AST node to only include nodes within the specified line range
    fn filter_node_by_range(&self, node: &AstNode, start_line: usize, end_line: usize) -> AstNode {
        match node {
            AstNode::Module { body, docstring, .. } => {
                let filtered_body: Vec<AstNode> = body
                    .iter()
                    .filter(|n| self.node_in_range(n, start_line, end_line))
                    .map(|n| self.filter_node_by_range(n, start_line, end_line))
                    .collect();
                AstNode::Module { body: filtered_body, docstring: docstring.clone() }
            }
            AstNode::FunctionDef {
                name,
                args,
                body,
                docstring,
                return_type,
                decorators,
                is_async,
                line,
                col,
                end_line,
                end_col,
            } => {
                let filtered_body: Vec<AstNode> = body
                    .iter()
                    .filter(|n| self.node_in_range(n, start_line, *end_line))
                    .map(|n| self.filter_node_by_range(n, start_line, *end_line))
                    .collect();
                AstNode::FunctionDef {
                    name: name.clone(),
                    args: args.clone(),
                    body: filtered_body,
                    docstring: docstring.clone(),
                    return_type: return_type.clone(),
                    decorators: decorators.clone(),
                    is_async: *is_async,
                    line: *line,
                    col: *col,
                    end_line: *end_line,
                    end_col: *end_col,
                }
            }
            AstNode::ClassDef { name, bases, metaclass, body, docstring, decorators, line, col, end_line, end_col } => {
                let filtered_body: Vec<AstNode> = body
                    .iter()
                    .filter(|n| self.node_in_range(n, start_line, *end_line))
                    .map(|n| self.filter_node_by_range(n, start_line, *end_line))
                    .collect();
                AstNode::ClassDef {
                    name: name.clone(),
                    bases: bases.clone(),
                    metaclass: metaclass.clone(),
                    body: filtered_body,
                    docstring: docstring.clone(),
                    decorators: decorators.clone(),
                    line: *line,
                    col: *col,
                    end_line: *end_line,
                    end_col: *end_col,
                }
            }
            AstNode::If { test, body, elif_parts, else_body, line, col, end_line, end_col } => {
                let filtered_body: Vec<AstNode> = body
                    .iter()
                    .filter(|n| self.node_in_range(n, start_line, *end_line))
                    .map(|n| self.filter_node_by_range(n, start_line, *end_line))
                    .collect();
                let filtered_elif_parts: Vec<(AstNode, Vec<AstNode>)> = elif_parts
                    .iter()
                    .map(|(test, body)| {
                        let filtered_elif_body: Vec<AstNode> = body
                            .iter()
                            .filter(|n| self.node_in_range(n, start_line, *end_line))
                            .map(|n| self.filter_node_by_range(n, start_line, *end_line))
                            .collect();
                        (test.clone(), filtered_elif_body)
                    })
                    .collect();
                let filtered_else_body = else_body.as_ref().map(|body| {
                    body.iter()
                        .filter(|n| self.node_in_range(n, start_line, *end_line))
                        .map(|n| self.filter_node_by_range(n, start_line, *end_line))
                        .collect()
                });
                AstNode::If {
                    test: test.clone(),
                    body: filtered_body,
                    elif_parts: filtered_elif_parts,
                    else_body: filtered_else_body,
                    line: *line,
                    col: *col,
                    end_line: *end_line,
                    end_col: *end_col,
                }
            }
            AstNode::For { target, iter, body, else_body, is_async, line, col, end_line, end_col } => {
                let filtered_body: Vec<AstNode> = body
                    .iter()
                    .filter(|n| self.node_in_range(n, start_line, *end_line))
                    .map(|n| self.filter_node_by_range(n, start_line, *end_line))
                    .collect();
                let filtered_else_body = else_body.as_ref().map(|body| {
                    body.iter()
                        .filter(|n| self.node_in_range(n, start_line, *end_line))
                        .map(|n| self.filter_node_by_range(n, start_line, *end_line))
                        .collect()
                });
                AstNode::For {
                    target: target.clone(),
                    iter: iter.clone(),
                    body: filtered_body,
                    else_body: filtered_else_body,
                    is_async: *is_async,
                    line: *line,
                    col: *col,
                    end_line: *end_line,
                    end_col: *end_col,
                }
            }
            AstNode::While { test, body, else_body, line, col, end_line, end_col } => {
                let filtered_body: Vec<AstNode> = body
                    .iter()
                    .filter(|n| self.node_in_range(n, start_line, *end_line))
                    .map(|n| self.filter_node_by_range(n, start_line, *end_line))
                    .collect();
                let filtered_else_body = else_body.as_ref().map(|body| {
                    body.iter()
                        .filter(|n| self.node_in_range(n, start_line, *end_line))
                        .map(|n| self.filter_node_by_range(n, start_line, *end_line))
                        .collect()
                });
                AstNode::While {
                    test: test.clone(),
                    body: filtered_body,
                    else_body: filtered_else_body,
                    line: *line,
                    col: *col,
                    end_line: *end_line,
                    end_col: *end_col,
                }
            }
            AstNode::Try { body, handlers, else_body, finally_body, line, col, end_line, end_col } => {
                let filtered_body: Vec<AstNode> = body
                    .iter()
                    .filter(|n| self.node_in_range(n, start_line, *end_line))
                    .map(|n| self.filter_node_by_range(n, start_line, *end_line))
                    .collect();
                let filtered_handlers: Vec<_> = handlers
                    .iter()
                    .map(|h| {
                        let filtered_handler_body: Vec<AstNode> = h
                            .body
                            .iter()
                            .filter(|n| self.node_in_range(n, start_line, *end_line))
                            .map(|n| self.filter_node_by_range(n, start_line, *end_line))
                            .collect();
                        beacon_parser::ExceptHandler {
                            exception_type: h.exception_type.clone(),
                            name: h.name.clone(),
                            body: filtered_handler_body,
                            line: h.line,
                            col: h.col,
                            end_line: h.end_line,
                            end_col: h.end_col,
                        }
                    })
                    .collect();
                let filtered_else_body = else_body.as_ref().map(|body| {
                    body.iter()
                        .filter(|n| self.node_in_range(n, start_line, *end_line))
                        .map(|n| self.filter_node_by_range(n, start_line, *end_line))
                        .collect()
                });
                let filtered_finally_body = finally_body.as_ref().map(|body| {
                    body.iter()
                        .filter(|n| self.node_in_range(n, start_line, *end_line))
                        .map(|n| self.filter_node_by_range(n, start_line, *end_line))
                        .collect()
                });
                AstNode::Try {
                    body: filtered_body,
                    handlers: filtered_handlers,
                    else_body: filtered_else_body,
                    finally_body: filtered_finally_body,
                    line: *line,
                    col: *col,
                    end_line: *end_line,
                    end_col: *end_col,
                }
            }
            AstNode::With { items, body, is_async, line, col, end_line, end_col } => {
                let filtered_body: Vec<AstNode> = body
                    .iter()
                    .filter(|n| self.node_in_range(n, start_line, *end_line))
                    .map(|n| self.filter_node_by_range(n, start_line, *end_line))
                    .collect();
                AstNode::With {
                    items: items.clone(),
                    body: filtered_body,
                    is_async: *is_async,
                    line: *line,
                    col: *col,
                    end_line: *end_line,
                    end_col: *end_col,
                }
            }
            _ => node.clone(),
        }
    }

    /// Check if a node falls within the specified line range
    fn node_in_range(&self, node: &AstNode, start_line: usize, end_line: usize) -> bool {
        let node_line = match node {
            AstNode::Module { .. } => return true,
            AstNode::FunctionDef { line, end_line: node_end, .. } => (*line, *node_end),
            AstNode::ClassDef { line, end_line: node_end, .. } => (*line, *node_end),
            AstNode::Assignment { line, end_line: node_end, .. } => (*line, *node_end),
            AstNode::AnnotatedAssignment { line, end_line: node_end, .. } => (*line, *node_end),
            AstNode::Call { line, end_line: node_end, .. } => (*line, *node_end),
            AstNode::Identifier { line, end_line: node_end, .. } => (*line, *node_end),
            AstNode::Literal { line, end_line: node_end, .. } => (*line, *node_end),
            AstNode::Return { line, end_line: node_end, .. } => (*line, *node_end),
            AstNode::Import { line, end_line: node_end, .. } => (*line, *node_end),
            AstNode::ImportFrom { line, end_line: node_end, .. } => (*line, *node_end),
            AstNode::Attribute { line, end_line: node_end, .. } => (*line, *node_end),
            AstNode::If { line, end_line: node_end, .. } => (*line, *node_end),
            AstNode::For { line, end_line: node_end, .. } => (*line, *node_end),
            AstNode::While { line, end_line: node_end, .. } => (*line, *node_end),
            AstNode::Try { line, end_line: node_end, .. } => (*line, *node_end),
            AstNode::With { line, end_line: node_end, .. } => (*line, *node_end),
            AstNode::BinaryOp { line, end_line: node_end, .. } => (*line, *node_end),
            AstNode::UnaryOp { line, end_line: node_end, .. } => (*line, *node_end),
            AstNode::Compare { line, end_line: node_end, .. } => (*line, *node_end),
            AstNode::Lambda { line, end_line: node_end, .. } => (*line, *node_end),
            AstNode::Subscript { line, end_line: node_end, .. } => (*line, *node_end),
            AstNode::Pass { line, end_line: node_end, .. } => (*line, *node_end),
            AstNode::Break { line, end_line: node_end, .. } => (*line, *node_end),
            AstNode::Continue { line, end_line: node_end, .. } => (*line, *node_end),
            AstNode::Raise { line, end_line: node_end, .. } => (*line, *node_end),
            AstNode::Tuple { line, end_line: node_end, .. } => (*line, *node_end),
            AstNode::List { line, end_line: node_end, .. } => (*line, *node_end),
            AstNode::Dict { line, end_line: node_end, .. } => (*line, *node_end),
            AstNode::Set { line, end_line: node_end, .. } => (*line, *node_end),
            AstNode::Yield { line, end_line: node_end, .. } => (*line, *node_end),
            AstNode::YieldFrom { line, end_line: node_end, .. } => (*line, *node_end),
            AstNode::Await { line, end_line: node_end, .. } => (*line, *node_end),
            AstNode::Assert { line, end_line: node_end, .. } => (*line, *node_end),
            AstNode::Starred { line, end_line: node_end, .. } => (*line, *node_end),
            AstNode::ListComp { line, end_line: node_end, .. } => (*line, *node_end),
            AstNode::DictComp { line, end_line: node_end, .. } => (*line, *node_end),
            AstNode::SetComp { line, end_line: node_end, .. } => (*line, *node_end),
            AstNode::GeneratorExp { line, end_line: node_end, .. } => (*line, *node_end),
            AstNode::NamedExpr { line, end_line: node_end, .. } => (*line, *node_end),
            AstNode::Match { line, end_line: node_end, .. } => (*line, *node_end),
        };

        node_line.0 <= end_line && node_line.1 >= start_line
    }

    /// Check if source code is already formatted according to config
    pub fn is_formatted(&mut self, source: &str) -> bool {
        match self.parser.parse(source) {
            Ok(parsed) => {
                if let Ok(formatted) = self.format_file(&parsed) {
                    source == formatted
                } else {
                    false
                }
            }
            Err(_) => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_formatter_creation() {
        let formatter = Formatter::with_defaults();
        assert_eq!(formatter.config.line_length, 88);
    }

    #[test]
    fn test_formatter_with_custom_config() {
        let config = FormatterConfig { line_length: 100, ..Default::default() };
        let formatter = Formatter::new(config, PythonParser::default());
        assert_eq!(formatter.config.line_length, 100);
    }

    #[test]
    fn test_range_formatting_single_function() {
        let source = r#"def foo():
    pass

def bar():
    pass
"#;
        let mut formatter = Formatter::with_defaults();
        let result = formatter.format_range(source, 3, 4);
        assert!(result.is_ok());

        let formatted = result.unwrap();
        assert!(formatted.contains("bar"));
        assert!(!formatted.contains("foo"));
    }

    #[test]
    fn test_range_formatting_nested_blocks() {
        let source = r#"if True:
    x = 1
    if False:
        y = 2
    z = 3
"#;
        let mut formatter = Formatter::with_defaults();
        let result = formatter.format_range(source, 3, 4);
        assert!(result.is_ok());
    }

    #[test]
    fn test_node_in_range() {
        let formatter = Formatter::with_defaults();
        let node = AstNode::FunctionDef {
            name: "test".to_string(),
            args: vec![],
            body: vec![],
            docstring: None,
            return_type: None,
            decorators: vec![],
            is_async: false,
            line: 5,
            col: 0,
            end_line: 10,
            end_col: 0,
        };

        assert!(formatter.node_in_range(&node, 1, 7));
        assert!(formatter.node_in_range(&node, 7, 15));
        assert!(formatter.node_in_range(&node, 1, 15));
        assert!(formatter.node_in_range(&node, 6, 9));

        assert!(!formatter.node_in_range(&node, 1, 4));
        assert!(!formatter.node_in_range(&node, 11, 15));
    }

    #[test]
    fn test_filter_node_by_range_module() {
        let formatter = Formatter::with_defaults();
        let module = AstNode::Module {
            body: vec![
                AstNode::FunctionDef {
                    name: "foo".to_string(),
                    args: vec![],
                    body: vec![],
                    docstring: None,
                    return_type: None,
                    decorators: vec![],
                    is_async: false,
                    line: 1,
                    col: 0,
                    end_line: 2,
                    end_col: 0,
                },
                AstNode::FunctionDef {
                    name: "bar".to_string(),
                    args: vec![],
                    body: vec![],
                    docstring: None,
                    return_type: None,
                    decorators: vec![],
                    is_async: false,
                    line: 4,
                    col: 0,
                    end_line: 5,
                    end_col: 0,
                },
            ],
            docstring: None,
        };

        let filtered = formatter.filter_node_by_range(&module, 4, 5);

        if let AstNode::Module { body, .. } = filtered {
            assert_eq!(body.len(), 1);
            if let AstNode::FunctionDef { name, .. } = &body[0] {
                assert_eq!(name, "bar");
            } else {
                panic!("Expected FunctionDef node");
            }
        } else {
            panic!("Expected Module node");
        }
    }
}

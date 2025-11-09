//! Python code formatting infrastructure
//!
//! Provides PEP8-compliant code formatting for Python source files.
//! This module coordinates parsing, token stream generation, and formatting rules.

pub mod config;
pub mod context;
pub mod rules;
pub mod state;
pub mod token_stream;
pub mod writer;

pub use config::FormatterConfig;
pub use context::FormattingContext;
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
        // Create token stream from AST
        let ast = self.parser.to_ast(parsed)?;
        let token_stream = TokenStream::from_ast(&ast);

        // Initialize writer and rules
        let mut writer = FormattedWriter::new(&self.config);
        let _rules = FormattingRules::new(self.config.clone());

        // Process tokens and write formatted output
        for token in token_stream {
            writer.write_token(&token);
        }

        Ok(writer.output().to_string())
    }

    /// Format a specific AST node
    pub fn format_node(&self, node: &AstNode, _source: &str) -> Result<String> {
        // Create token stream from the node
        let token_stream = TokenStream::from_ast(node);

        // Initialize writer
        let mut writer = FormattedWriter::new(&self.config);

        // Process tokens
        for token in token_stream {
            writer.write_token(&token);
        }

        Ok(writer.output().to_string())
    }

    /// Format a range within source code
    pub fn format_range(&mut self, source: &str, _start_line: usize, _end_line: usize) -> Result<String> {
        // For now, format the entire source
        // TODO: Implement proper range formatting by parsing and selecting nodes
        let parsed = self.parser.parse(source)?;
        self.format_file(&parsed)
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
}

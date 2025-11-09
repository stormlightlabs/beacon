//! Formatter configuration
//!
//! Defines PEP8 formatting options and style preferences.

use serde::{Deserialize, Serialize};

/// Quote style preference for string literals
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum QuoteStyle {
    /// Use single quotes for strings
    Single,
    /// Use double quotes for strings
    Double,
    /// Preserve existing quote style
    Preserve,
}

impl Default for QuoteStyle {
    fn default() -> Self {
        Self::Double
    }
}

/// Trailing comma preference for multi-line structures
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum TrailingCommas {
    /// Always add trailing commas in multi-line structures
    Always,
    /// Add trailing commas only in multi-line structures
    Multiline,
    /// Never add trailing commas
    Never,
}

impl Default for TrailingCommas {
    fn default() -> Self {
        Self::Multiline
    }
}

/// Import sorting style
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum ImportSorting {
    /// PEP8 style: stdlib, third-party, local
    Pep8,
    /// isort-compatible sorting
    Isort,
    /// Disable import sorting
    Off,
}

impl Default for ImportSorting {
    fn default() -> Self {
        Self::Pep8
    }
}

/// Compatibility mode with other formatters
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum CompatibilityMode {
    /// Black formatter compatibility
    Black,
    /// autopep8 compatibility
    Autopep8,
    /// Strict PEP8
    Pep8,
}

impl Default for CompatibilityMode {
    fn default() -> Self {
        Self::Black
    }
}

/// Configuration for the Python code formatter
///
/// Controls formatting behavior, line length limits, indentation,
/// quote styles, and other PEP8-related preferences.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct FormatterConfig {
    /// Enable formatting (master toggle)
    #[serde(default = "default_true")]
    pub enabled: bool,

    /// Maximum line length (default: 88, matching Black)
    #[serde(default = "default_line_length")]
    pub line_length: usize,

    /// Number of spaces per indentation level (default: 4)
    #[serde(default = "default_indent_size")]
    pub indent_size: usize,

    /// Quote style for string literals
    #[serde(default)]
    pub quote_style: QuoteStyle,

    /// Trailing comma preference
    #[serde(default)]
    pub trailing_commas: TrailingCommas,

    /// Maximum consecutive blank lines (default: 2)
    #[serde(default = "default_max_blank_lines")]
    pub max_blank_lines: usize,

    /// Import sorting style
    #[serde(default)]
    pub import_sorting: ImportSorting,

    /// Compatibility mode with other formatters
    #[serde(default)]
    pub compatibility_mode: CompatibilityMode,

    /// Use tabs instead of spaces (not recommended, default: false)
    #[serde(default)]
    pub use_tabs: bool,

    /// Normalize string quotes in docstrings
    #[serde(default = "default_true")]
    pub normalize_docstring_quotes: bool,

    /// Add spaces around binary operators
    #[serde(default = "default_true")]
    pub spaces_around_operators: bool,

    /// Add blank line before class definitions
    #[serde(default = "default_true")]
    pub blank_line_before_class: bool,

    /// Add blank line before function definitions
    #[serde(default = "default_true")]
    pub blank_line_before_function: bool,
}

fn default_true() -> bool {
    true
}

fn default_line_length() -> usize {
    88
}

fn default_indent_size() -> usize {
    4
}

fn default_max_blank_lines() -> usize {
    2
}

impl Default for FormatterConfig {
    fn default() -> Self {
        Self {
            enabled: true,
            line_length: default_line_length(),
            indent_size: default_indent_size(),
            quote_style: QuoteStyle::default(),
            trailing_commas: TrailingCommas::default(),
            max_blank_lines: default_max_blank_lines(),
            import_sorting: ImportSorting::default(),
            compatibility_mode: CompatibilityMode::default(),
            use_tabs: false,
            normalize_docstring_quotes: true,
            spaces_around_operators: true,
            blank_line_before_class: true,
            blank_line_before_function: true,
        }
    }
}

impl FormatterConfig {
    /// Create a new configuration with default values
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a Black-compatible configuration
    pub fn black() -> Self {
        Self { line_length: 88, compatibility_mode: CompatibilityMode::Black, ..Default::default() }
    }

    /// Create an autopep8-compatible configuration
    pub fn autopep8() -> Self {
        Self { line_length: 79, compatibility_mode: CompatibilityMode::Autopep8, ..Default::default() }
    }

    /// Create a strict PEP8 configuration
    pub fn pep8() -> Self {
        Self { line_length: 79, compatibility_mode: CompatibilityMode::Pep8, ..Default::default() }
    }

    /// Get the indent string based on configuration
    pub fn indent_string(&self) -> String {
        if self.use_tabs { "\t".to_string() } else { " ".repeat(self.indent_size) }
    }

    /// Validate configuration
    ///
    /// Checks that configuration values are reasonable.
    pub fn validate(&self) -> beacon_core::Result<()> {
        if self.line_length < 20 {
            tracing::warn!("Line length {} is too short, recommend at least 79", self.line_length);
        }
        if self.line_length > 200 {
            tracing::warn!("Line length {} is very long, recommend 79-120", self.line_length);
        }
        if self.indent_size == 0 || self.indent_size > 8 {
            tracing::warn!("Indent size {} is unusual, recommend 2 or 4", self.indent_size);
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_config() {
        let config = FormatterConfig::default();
        assert_eq!(config.line_length, 88);
        assert_eq!(config.indent_size, 4);
        assert_eq!(config.quote_style, QuoteStyle::Double);
        assert!(config.enabled);
    }

    #[test]
    fn test_black_config() {
        let config = FormatterConfig::black();
        assert_eq!(config.line_length, 88);
        assert_eq!(config.compatibility_mode, CompatibilityMode::Black);
    }

    #[test]
    fn test_pep8_config() {
        let config = FormatterConfig::pep8();
        assert_eq!(config.line_length, 79);
        assert_eq!(config.compatibility_mode, CompatibilityMode::Pep8);
    }

    #[test]
    fn test_indent_string_spaces() {
        let config = FormatterConfig::default();
        assert_eq!(config.indent_string(), "    ");
    }

    #[test]
    fn test_indent_string_tabs() {
        let config = FormatterConfig { use_tabs: true, ..Default::default() };
        assert_eq!(config.indent_string(), "\t");
    }

    #[test]
    fn test_validation() {
        let config = FormatterConfig::default();
        assert!(config.validate().is_ok());
    }

    #[test]
    fn test_serialization() {
        let config = FormatterConfig::default();
        let json = serde_json::to_string(&config).unwrap();
        let deserialized: FormatterConfig = serde_json::from_str(&json).unwrap();
        assert_eq!(config, deserialized);
    }
}

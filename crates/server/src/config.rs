//! LSP configuration and workspace settings
//!
//! This module defines the configuration options for the Beacon LSP server,
//! including type checking modes, Python version targeting, stub paths, and
//! other customizable behaviors.

use beacon_core::{Result, errors};
use serde::{Deserialize, Serialize};
use std::fs;
use std::path::{Path, PathBuf};
use std::str::FromStr;

/// Diagnostic severity level for configurable diagnostics
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Default)]
#[serde(rename_all = "lowercase")]
pub enum DiagnosticSeverity {
    /// Informational message
    Info,
    /// Warning message
    #[default]
    Warning,
    /// Error message
    Error,
}

/// Configuration for inlay hints display
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct InlayHintsConfig {
    /// Enable all inlay hints (master toggle)
    #[serde(default = "default_true")]
    pub enable: bool,

    /// Show inlay hints for inferred variable types
    #[serde(default = "default_true")]
    pub variable_types: bool,

    /// Show inlay hints for inferred function return types
    #[serde(default = "default_true")]
    pub function_return_types: bool,

    /// Show inlay hints for parameter names in function calls
    #[serde(default)]
    pub parameter_names: bool,
}

impl Default for InlayHintsConfig {
    fn default() -> Self {
        Self { enable: true, variable_types: true, function_return_types: true, parameter_names: false }
    }
}

/// Type checking strictness mode
///
/// Controls how the type checker handles annotation mismatches and inference.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Default)]
#[serde(rename_all = "lowercase")]
pub enum TypeCheckingMode {
    /// Annotation mismatches are hard errors, strict enforcement
    Strict,
    /// Annotation mismatches are diagnostics with quick fixes, but inference still proceeds (default)
    #[default]
    Balanced,
    /// Annotations supply upper/lower bounds but can be overridden by inference, very lenient
    Relaxed,
}

impl TypeCheckingMode {
    /// Validate that the mode is one of the supported values
    ///
    /// This is a no-op since the enum variants are valid by construction,
    /// but we keep the method for API consistency and future extensibility.
    pub fn validate(&self) -> Result<()> {
        tracing::debug!("Validating type checking mode: {}", self.as_str());
        Ok(())
    }

    /// Check if this mode requires explicit type annotations
    pub fn requires_annotations(&self) -> bool {
        matches!(self, Self::Strict)
    }

    /// Check if this mode allows implicit Any types
    pub fn allows_implicit_any(&self) -> bool {
        matches!(self, Self::Relaxed)
    }

    /// Get the diagnostic severity for missing annotations in this mode
    pub fn missing_annotation_severity(&self) -> Option<DiagnosticSeverity> {
        match self {
            Self::Strict => Some(DiagnosticSeverity::Error),
            Self::Balanced => Some(DiagnosticSeverity::Warning),
            Self::Relaxed => None,
        }
    }

    /// Get the mode name as a string
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Strict => "strict",
            Self::Balanced => "balanced",
            Self::Relaxed => "relaxed",
        }
    }
}

/// Error type for parsing TypeCheckingMode
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseModeError(String);

impl std::fmt::Display for ParseModeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Invalid type checking mode: '{}'. Valid modes are: strict, balanced, relaxed",
            self.0
        )
    }
}

impl std::error::Error for ParseModeError {}

impl FromStr for TypeCheckingMode {
    type Err = ParseModeError;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s.trim().to_lowercase().as_str() {
            "strict" => Ok(Self::Strict),
            "balanced" => Ok(Self::Balanced),
            "relaxed" => Ok(Self::Relaxed),
            _ => Err(ParseModeError(s.to_string())),
        }
    }
}

/// Parse per-file mode directive from Python source code
///
/// Searches for comments like:
/// - `# beacon: mode=strict`
/// - `# beacon: mode=balanced`
/// - `# beacon: mode=relaxed`
///
/// The directive must appear within the first 10 lines of the file.
pub fn parse_mode_directive(source: &str) -> Option<TypeCheckingMode> {
    parse_mode_directive_recursive(source.lines(), 0)
}

/// Recursive helper for parsing mode directives
fn parse_mode_directive_recursive<'a, I>(mut lines: I, line_num: usize) -> Option<TypeCheckingMode>
where
    I: Iterator<Item = &'a str>,
{
    if line_num >= 10 {
        return None;
    }

    let line = lines.next()?;

    if let Some(mode) = extract_mode_from_line(line) {
        return Some(mode);
    }

    parse_mode_directive_recursive(lines, line_num + 1)
}

/// Extract mode directive from a single line if present
fn extract_mode_from_line(line: &str) -> Option<TypeCheckingMode> {
    let comment_start = line.find('#')?;
    let comment = &line[comment_start + 1..].trim();

    let directive_start = comment.to_lowercase().find("beacon:")?;
    let directive = &comment[directive_start + 7..].trim();

    let mode_start = directive.to_lowercase().find("mode=")?;
    let mode_value = &directive[mode_start + 5..].trim();

    let mode_value = mode_value
        .split(|c: char| c.is_whitespace() || c == ',')
        .next()
        .unwrap_or("");

    mode_value.parse::<TypeCheckingMode>().ok()
}

/// Type checking configuration
///
/// Controls type checking behavior including strictness mode and diagnostic filtering.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Default)]
#[serde(rename_all = "camelCase")]
pub struct TypeCheckingConfig {
    /// Type checking strictness mode
    #[serde(default)]
    pub mode: TypeCheckingMode,
}

impl TypeCheckingConfig {
    /// Create a new type checking configuration with default values
    pub fn new() -> Self {
        Self::default()
    }

    /// Validate the type checking configuration
    pub fn validate(&self) -> Result<()> {
        self.mode.validate()?;
        Ok(())
    }
}

/// Python version target for feature support
///
/// Determines which Python language features are enabled (e.g., pattern matching in 3.10+, ParamSpec in 3.10+).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum PythonVersion {
    #[serde(rename = "3.9")]
    Py39,
    #[serde(rename = "3.10")]
    Py310,
    #[serde(rename = "3.11")]
    Py311,
    #[serde(rename = "3.12")]
    Py312,
    #[serde(rename = "3.13")]
    Py313,
}

impl Default for PythonVersion {
    fn default() -> Self {
        Self::Py312
    }
}

impl PythonVersion {
    /// Check if pattern matching (PEP 634) is supported
    pub fn supports_pattern_matching(&self) -> bool {
        matches!(self, Self::Py310 | Self::Py311 | Self::Py312 | Self::Py313)
    }

    /// Check if PEP 695 type parameter syntax is supported
    pub fn supports_pep695_syntax(&self) -> bool {
        matches!(self, Self::Py312 | Self::Py313)
    }
}

/// Configuration for decorator type stubs
///
/// TODO: Implement custom decorator stub loading and signature transformation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DecoratorStub {
    /// Fully qualified name of the decorator (e.g., "functools.wraps")
    pub name: String,

    /// Type signature transformation rule
    /// TODO: Define transformation DSL or use .pyi-like syntax
    pub signature: String,
}

/// Main configuration for the Beacon LSP server
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Config {
    /// Type checking configuration
    #[serde(default)]
    pub type_checking: TypeCheckingConfig,

    /// Target Python version
    #[serde(default)]
    pub python_version: PythonVersion,

    /// Additional paths to search for .pyi stub files
    #[serde(default)]
    pub stub_paths: Vec<PathBuf>,

    /// Maximum depth for Any type propagation before elevating diagnostics
    ///
    /// Controls how far `Any` can flow through the codebase before generating warnings. Higher values are more permissive.
    #[serde(default = "default_max_any_depth")]
    pub max_any_depth: u32,

    /// Custom decorator type stubs
    ///
    /// TODO: Load from .pyi files or custom configuration
    #[serde(default)]
    pub decorator_stubs: Vec<DecoratorStub>,

    /// Enable incremental type checking
    ///
    /// TODO: Implement constraint slicing and incremental re-solve
    #[serde(default = "default_true")]
    pub incremental: bool,

    /// Enable workspace-wide analysis
    ///
    /// TODO: Implement project dependency graph and SCC analysis
    #[serde(default = "default_true")]
    pub workspace_analysis: bool,

    /// Enable caching of parse trees and type inference results
    ///
    /// TODO: Implement LRU cache with optional disk persistence
    #[serde(default = "default_true")]
    pub enable_caching: bool,

    /// Maximum cache size in number of documents
    #[serde(default = "default_cache_size")]
    pub cache_size: usize,

    /// Enable experimental features
    ///
    /// TODO: Gate bleeding-edge features behind this flag
    #[serde(default)]
    pub experimental: bool,

    /// Source roots for module resolution (in addition to workspace root)
    ///
    /// Auto-detected roots include workspace root, src/, and lib/
    #[serde(default)]
    pub source_roots: Vec<PathBuf>,

    /// Patterns to exclude from workspace scanning
    ///
    /// Common patterns include venv/, .venv/, __pycache__/, etc.
    #[serde(default)]
    pub exclude_patterns: Vec<String>,

    /// Diagnostic severity for unresolved imports
    ///
    /// Defaults to Warning
    #[serde(default)]
    pub unresolved_import_severity: DiagnosticSeverity,

    /// Diagnostic severity for circular imports
    ///
    /// Defaults to Warning
    #[serde(default)]
    pub circular_import_severity: DiagnosticSeverity,

    /// Inlay hints configuration
    #[serde(default)]
    pub inlay_hints: InlayHintsConfig,

    /// Formatting configuration
    #[serde(default)]
    pub formatting: crate::formatting::FormatterConfig,
}

fn default_max_any_depth() -> u32 {
    3
}

fn default_true() -> bool {
    true
}

fn default_cache_size() -> usize {
    100
}

impl Default for Config {
    fn default() -> Self {
        Self {
            type_checking: TypeCheckingConfig::default(),
            python_version: PythonVersion::default(),
            stub_paths: vec![PathBuf::from("stubs")],
            max_any_depth: default_max_any_depth(),
            decorator_stubs: Vec::new(),
            incremental: default_true(),
            workspace_analysis: default_true(),
            enable_caching: default_true(),
            cache_size: default_cache_size(),
            experimental: false,
            source_roots: Vec::new(),
            exclude_patterns: Vec::new(),
            unresolved_import_severity: DiagnosticSeverity::default(),
            circular_import_severity: DiagnosticSeverity::default(),
            inlay_hints: InlayHintsConfig::default(),
            formatting: crate::formatting::FormatterConfig::default(),
        }
    }
}

impl Config {
    /// Create a new configuration with default values
    pub fn new() -> Self {
        Self::default()
    }

    /// Update configuration from LSP initialization options or TOML value
    ///
    /// Merges user-provided configuration with existing settings.
    /// LSP settings take precedence over TOML configuration.
    pub fn update_from_value(&mut self, value: serde_json::Value) {
        if let Ok(partial) = serde_json::from_value::<Config>(value) {
            let defaults = Config::default();

            if partial.type_checking != defaults.type_checking {
                self.type_checking = partial.type_checking;
            }
            if partial.python_version != defaults.python_version {
                self.python_version = partial.python_version;
            }
            if !partial.stub_paths.is_empty() && partial.stub_paths != defaults.stub_paths {
                self.stub_paths = partial.stub_paths;
            }
            if partial.max_any_depth != defaults.max_any_depth {
                self.max_any_depth = partial.max_any_depth;
            }
            if !partial.decorator_stubs.is_empty() {
                self.decorator_stubs = partial.decorator_stubs;
            }
            if partial.incremental != defaults.incremental {
                self.incremental = partial.incremental;
            }
            if partial.workspace_analysis != defaults.workspace_analysis {
                self.workspace_analysis = partial.workspace_analysis;
            }
            if partial.enable_caching != defaults.enable_caching {
                self.enable_caching = partial.enable_caching;
            }
            if partial.cache_size != defaults.cache_size {
                self.cache_size = partial.cache_size;
            }
            if partial.experimental != defaults.experimental {
                self.experimental = partial.experimental;
            }
            if !partial.source_roots.is_empty() {
                self.source_roots = partial.source_roots;
            }
            if !partial.exclude_patterns.is_empty() {
                self.exclude_patterns = partial.exclude_patterns;
            }
            if partial.unresolved_import_severity != defaults.unresolved_import_severity {
                self.unresolved_import_severity = partial.unresolved_import_severity;
            }
            if partial.circular_import_severity != defaults.circular_import_severity {
                self.circular_import_severity = partial.circular_import_severity;
            }
            if partial.inlay_hints != defaults.inlay_hints {
                self.inlay_hints = partial.inlay_hints;
            }
            if partial.formatting != defaults.formatting {
                self.formatting = partial.formatting;
            }
        }
    }

    /// Validate configuration and return any errors
    pub fn validate(&self) -> Result<()> {
        tracing::debug!(
            "Validating configuration: mode={}, python_version={:?}",
            self.type_checking.mode.as_str(),
            self.python_version
        );

        self.type_checking.validate()?;

        for stub_path in &self.stub_paths {
            if !stub_path.exists() {
                tracing::warn!("Stub path does not exist: {}", stub_path.display());
            } else if !stub_path.is_dir() {
                tracing::warn!("Stub path is not a directory: {}", stub_path.display());
            }
        }

        for source_root in &self.source_roots {
            if !source_root.exists() {
                tracing::warn!("Source root does not exist: {}", source_root.display());
            }
        }

        if self.cache_size == 0 {
            tracing::warn!("Cache size is 0, caching will be ineffective");
        }

        tracing::info!(
            "Configuration validated successfully with type checking mode: {}",
            self.type_checking.mode.as_str()
        );

        Ok(())
    }

    /// Get effective stub search paths including standard library
    ///
    /// TODO: Include stdlib typeshed paths based on python_version
    pub fn effective_stub_paths(&self) -> Vec<PathBuf> {
        // TODO: Add typeshed stdlib paths
        // TODO: Add site-packages paths
        self.stub_paths.clone()
    }

    /// Discover and load configuration from TOML files in the workspace
    ///
    /// Searches for configuration in the following order:
    /// 1. `beacon.toml` in workspace root
    /// 2. `[tool.beacon]` section in `pyproject.toml`
    ///
    /// Returns a Config with TOML settings merged over defaults.
    pub fn discover_and_load(workspace_root: &Path) -> Result<Self> {
        let mut config = Config::default();

        let beacon_toml = workspace_root.join("beacon.toml");
        if beacon_toml.exists() {
            tracing::info!("Loading configuration from {}", beacon_toml.display());
            config.load_from_toml_file(&beacon_toml)?;
            return Ok(config);
        }

        let pyproject_toml = workspace_root.join("pyproject.toml");
        if pyproject_toml.exists() {
            tracing::info!(
                "Loading configuration from [tool.beacon] in {}",
                pyproject_toml.display()
            );
            if let Ok(content) = fs::read_to_string(&pyproject_toml) {
                if let Ok(table) = toml::from_str::<toml::Table>(&content) {
                    if let Some(tool) = table.get("tool").and_then(|t| t.as_table()) {
                        if let Some(beacon_config) = tool.get("beacon") {
                            config.load_from_toml_value(beacon_config.clone())?;
                            return Ok(config);
                        }
                    }
                }
            }
        }

        tracing::debug!("No beacon.toml or pyproject.toml found, using defaults");
        Ok(config)
    }

    /// Load configuration from a TOML file
    ///
    /// Merges settings from the TOML file into this configuration.
    fn load_from_toml_file(&mut self, path: &Path) -> Result<()> {
        let content = fs::read_to_string(path).map_err(|e| {
            let err: beacon_core::ConfigError = std::io::Error::new(
                e.kind(),
                format!("Failed to read config file {}: {}", path.display(), e),
            )
            .into();

            err
        })?;

        let table: toml::Table = toml::from_str(&content).map_err(errors::ConfigError::TOMLError)?;
        let value = toml::Value::Table(table);

        self.load_from_toml_value(value)
    }

    /// Load configuration from a TOML value
    ///
    /// TOML files use snake_case, so we need to convert to the camelCase JSON that update_from_value expects.
    fn load_from_toml_value(&mut self, value: toml::Value) -> Result<()> {
        let json_value = toml_to_json(value);
        self.update_from_value(json_value);
        Ok(())
    }
}

/// Convert TOML value to JSON value, handling snake_case to camelCase conversion
fn toml_to_json(value: toml::Value) -> serde_json::Value {
    match value {
        toml::Value::String(s) => serde_json::Value::String(s),
        toml::Value::Integer(i) => serde_json::Value::Number(i.into()),
        toml::Value::Float(f) => {
            serde_json::Value::Number(serde_json::Number::from_f64(f).unwrap_or(serde_json::Number::from(0)))
        }
        toml::Value::Boolean(b) => serde_json::Value::Bool(b),
        toml::Value::Array(arr) => serde_json::Value::Array(arr.into_iter().map(toml_to_json).collect()),
        toml::Value::Table(table) => {
            let mut map = serde_json::Map::new();
            for (key, val) in table {
                let camel_key = match key.as_str() {
                    "type_checking" => "typeChecking".to_string(),
                    "python_version" => "pythonVersion".to_string(),
                    "stub_paths" => "stubPaths".to_string(),
                    "max_any_depth" => "maxAnyDepth".to_string(),
                    "decorator_stubs" => "decoratorStubs".to_string(),
                    "workspace_analysis" => "workspaceAnalysis".to_string(),
                    "enable_caching" => "enableCaching".to_string(),
                    "cache_size" => "cacheSize".to_string(),
                    "source_roots" => "sourceRoots".to_string(),
                    "exclude_patterns" => "excludePatterns".to_string(),
                    "unresolved_import_severity" => "unresolvedImportSeverity".to_string(),
                    "circular_import_severity" => "circularImportSeverity".to_string(),
                    "inlay_hints" => "inlayHints".to_string(),
                    "variable_types" => "variableTypes".to_string(),
                    "function_return_types" => "functionReturnTypes".to_string(),
                    "parameter_names" => "parameterNames".to_string(),
                    "line_length" => "lineLength".to_string(),
                    "indent_size" => "indentSize".to_string(),
                    "quote_style" => "quoteStyle".to_string(),
                    "trailing_commas" => "trailingCommas".to_string(),
                    "max_blank_lines" => "maxBlankLines".to_string(),
                    "import_sorting" => "importSorting".to_string(),
                    "compatibility_mode" => "compatibilityMode".to_string(),
                    "use_tabs" => "useTabs".to_string(),
                    "normalize_docstring_quotes" => "normalizeDocstringQuotes".to_string(),
                    "spaces_around_operators" => "spacesAroundOperators".to_string(),
                    "blank_line_before_class" => "blankLineBeforeClass".to_string(),
                    "blank_line_before_function" => "blankLineBeforeFunction".to_string(),
                    _ => key,
                };
                map.insert(camel_key, toml_to_json(val));
            }
            serde_json::Value::Object(map)
        }
        toml::Value::Datetime(dt) => serde_json::Value::String(dt.to_string()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use std::path::PathBuf;
    use tempfile::TempDir;

    struct ParseModeTestCase {
        name: &'static str,
        source: &'static str,
        want: Option<TypeCheckingMode>,
    }

    #[test]
    fn test_default_config() {
        let config = Config::default();
        assert_eq!(config.type_checking.mode, TypeCheckingMode::Balanced);
        assert_eq!(config.python_version, PythonVersion::Py312);
        assert!(config.incremental);
        assert!(config.workspace_analysis);
    }

    #[test]
    fn test_python_version_features() {
        assert!(PythonVersion::Py310.supports_pattern_matching());
        assert!(!PythonVersion::Py39.supports_pattern_matching());
        assert!(PythonVersion::Py312.supports_pep695_syntax());
        assert!(!PythonVersion::Py311.supports_pep695_syntax());
    }

    #[test]
    fn test_serialization() {
        let config = Config {
            type_checking: TypeCheckingConfig { mode: TypeCheckingMode::Strict },
            python_version: PythonVersion::Py311,
            ..Default::default()
        };

        let json = serde_json::to_string(&config).unwrap();
        let deserialized: Config = serde_json::from_str(&json).unwrap();

        assert_eq!(config.type_checking.mode, deserialized.type_checking.mode);
        assert_eq!(config.python_version, deserialized.python_version);
    }

    #[test]
    fn test_toml_to_json_conversion() {
        let toml_str = r#"
python_version = "3.11"
max_any_depth = 5
incremental = false

[type_checking]
mode = "strict"
"#;

        let table: toml::Table = toml::from_str(toml_str).unwrap();
        let toml_value = toml::Value::Table(table);
        let json_value = toml_to_json(toml_value);

        let obj = json_value.as_object().unwrap();
        assert!(obj.contains_key("typeChecking"));
        assert!(obj.contains_key("pythonVersion"));
        assert!(obj.contains_key("maxAnyDepth"));
        assert!(obj.contains_key("incremental"));

        let type_checking = obj.get("typeChecking").unwrap().as_object().unwrap();
        assert_eq!(type_checking.get("mode").unwrap().as_str().unwrap(), "strict");
        assert_eq!(obj.get("pythonVersion").unwrap().as_str().unwrap(), "3.11");
        assert_eq!(obj.get("maxAnyDepth").unwrap().as_i64().unwrap(), 5);
        assert!(!obj.get("incremental").unwrap().as_bool().unwrap());
    }

    #[test]
    fn test_load_from_toml_value() {
        let toml_str = r#"
python_version = "3.11"
max_any_depth = 5

[type_checking]
mode = "strict"
"#;

        let table: toml::Table = toml::from_str(toml_str).unwrap();
        let toml_value = toml::Value::Table(table);
        let mut config = Config::default();
        config.load_from_toml_value(toml_value).unwrap();

        assert_eq!(config.type_checking.mode, TypeCheckingMode::Strict);
        assert_eq!(config.python_version, PythonVersion::Py311);
        assert_eq!(config.max_any_depth, 5);
    }

    #[test]
    fn test_update_from_value_json() {
        let json_str = r#"{
            "typeChecking": {
                "mode": "relaxed"
            },
            "pythonVersion": "3.13",
            "maxAnyDepth": 10
        }"#;

        let json_value: serde_json::Value = serde_json::from_str(json_str).unwrap();
        let mut config = Config::default();
        config.update_from_value(json_value);

        assert_eq!(config.type_checking.mode, TypeCheckingMode::Relaxed);
        assert_eq!(config.python_version, PythonVersion::Py313);
        assert_eq!(config.max_any_depth, 10);
    }

    #[test]
    fn test_validate_warns_on_missing_paths() {
        let config = Config {
            stub_paths: vec![PathBuf::from("/nonexistent/path")],
            source_roots: vec![PathBuf::from("/another/nonexistent/path")],
            ..Default::default()
        };
        let result = config.validate();
        assert!(result.is_ok());
    }

    #[test]
    fn test_validate_zero_cache_size() {
        let config = Config { cache_size: 0, ..Default::default() };
        let result = config.validate();
        assert!(result.is_ok());
    }

    #[test]
    fn test_config_discover_no_files() {
        let temp_dir = TempDir::new().unwrap();
        let config = Config::discover_and_load(temp_dir.path()).unwrap();
        assert_eq!(config.type_checking.mode, TypeCheckingMode::default());
        assert_eq!(config.python_version, PythonVersion::default());
    }

    #[test]
    fn test_config_discover_beacon_toml() {
        let temp_dir = TempDir::new().unwrap();
        let beacon_toml_path = temp_dir.path().join("beacon.toml");
        let toml_content = r#"
python_version = "3.10"
max_any_depth = 7
incremental = false

[type_checking]
mode = "strict"
"#;

        fs::write(&beacon_toml_path, toml_content).unwrap();

        let config = Config::discover_and_load(temp_dir.path()).unwrap();

        assert_eq!(config.type_checking.mode, TypeCheckingMode::Strict);
        assert_eq!(config.python_version, PythonVersion::Py310);
        assert_eq!(config.max_any_depth, 7);
        assert!(!config.incremental);
    }

    #[test]
    fn test_config_discover_pyproject_toml() {
        let temp_dir = TempDir::new().unwrap();
        let pyproject_path = temp_dir.path().join("pyproject.toml");
        let toml_content = r#"[tool.beacon.type_checking]
mode = "relaxed"

[tool.beacon]
python_version = "3.13""#;

        fs::write(&pyproject_path, toml_content).unwrap();

        let config = Config::discover_and_load(temp_dir.path()).unwrap();
        assert_eq!(config.type_checking.mode, TypeCheckingMode::Relaxed);
        assert_eq!(config.python_version, PythonVersion::Py313);
    }

    #[test]
    fn test_beacon_toml_takes_precedence() {
        let temp_dir = TempDir::new().unwrap();
        let beacon_toml_path = temp_dir.path().join("beacon.toml");
        let pyproject_path = temp_dir.path().join("pyproject.toml");

        fs::write(
            &beacon_toml_path,
            r#"[type_checking]
mode = "strict""#,
        )
        .unwrap();
        fs::write(
            &pyproject_path,
            r#"[tool.beacon.type_checking]
mode = "relaxed""#,
        )
        .unwrap();

        let config = Config::discover_and_load(temp_dir.path()).unwrap();
        assert_eq!(config.type_checking.mode, TypeCheckingMode::Strict);
    }

    #[test]
    fn test_update_from_value_preserves_defaults() {
        let json_str = r#"{
            "typeChecking": {
                "mode": "strict"
            }
        }"#;

        let json_value: serde_json::Value = serde_json::from_str(json_str).unwrap();
        let mut config = Config::default();
        let original_version = config.python_version;

        config.update_from_value(json_value);

        assert_eq!(config.type_checking.mode, TypeCheckingMode::Strict);
        assert_eq!(config.python_version, original_version);
    }

    #[test]
    fn test_inlay_hints_config_defaults() {
        let config = InlayHintsConfig::default();
        assert!(config.enable);
        assert!(config.variable_types);
        assert!(config.function_return_types);
        assert!(!config.parameter_names);
    }

    #[test]
    fn test_inlay_hints_config_serialization() {
        let config = InlayHintsConfig {
            enable: true,
            variable_types: false,
            function_return_types: true,
            parameter_names: true,
        };
        let json = serde_json::to_string(&config).unwrap();
        let deserialized: InlayHintsConfig = serde_json::from_str(&json).unwrap();
        assert_eq!(config, deserialized);
    }

    #[test]
    fn test_config_with_inlay_hints() {
        let config = Config::default();
        assert!(config.inlay_hints.enable);
        assert!(config.inlay_hints.variable_types);
        assert!(config.inlay_hints.function_return_types);
        assert!(!config.inlay_hints.parameter_names);
    }

    #[test]
    fn test_update_inlay_hints_from_json() {
        let json_str = r#"{
            "inlayHints": {
                "enable": false,
                "variableTypes": false,
                "functionReturnTypes": false,
                "parameterNames": true
            }
        }"#;

        let json_value: serde_json::Value = serde_json::from_str(json_str).unwrap();
        let mut config = Config::default();
        config.update_from_value(json_value);

        assert!(!config.inlay_hints.enable);
        assert!(!config.inlay_hints.variable_types);
        assert!(!config.inlay_hints.function_return_types);
        assert!(config.inlay_hints.parameter_names);
    }

    #[test]
    fn test_inlay_hints_from_toml() {
        let toml_str = r#"
[inlay_hints]
enable = false
variable_types = false
function_return_types = true
parameter_names = true
"#;

        let table: toml::Table = toml::from_str(toml_str).unwrap();
        let toml_value = toml::Value::Table(table);
        let mut config = Config::default();
        config.load_from_toml_value(toml_value).unwrap();

        assert!(!config.inlay_hints.enable);
        assert!(!config.inlay_hints.variable_types);
        assert!(config.inlay_hints.function_return_types);
        assert!(config.inlay_hints.parameter_names);
    }

    #[test]
    fn test_inlay_hints_in_beacon_toml() {
        let temp_dir = TempDir::new().unwrap();
        let beacon_toml_path = temp_dir.path().join("beacon.toml");
        let toml_content = r#"
[type_checking]
mode = "strict"

[inlay_hints]
enable = true
variable_types = false
parameter_names = true
"#;

        fs::write(&beacon_toml_path, toml_content).unwrap();

        let config = Config::discover_and_load(temp_dir.path()).unwrap();

        assert_eq!(config.type_checking.mode, TypeCheckingMode::Strict);
        assert!(config.inlay_hints.enable);
        assert!(!config.inlay_hints.variable_types);
        assert!(config.inlay_hints.parameter_names);
        assert!(config.inlay_hints.function_return_types);
    }

    #[test]
    fn test_partial_inlay_hints_update() {
        let json_str = r#"{
            "inlayHints": {
                "parameterNames": true
            }
        }"#;

        let json_value: serde_json::Value = serde_json::from_str(json_str).unwrap();
        let mut config = Config::default();
        let original_enable = config.inlay_hints.enable;

        config.update_from_value(json_value);

        assert_eq!(config.inlay_hints.enable, original_enable);
        assert!(config.inlay_hints.parameter_names);
    }

    #[test]
    fn test_source_roots_from_json() {
        let json_str = r#"{
            "sourceRoots": ["/path/to/src", "/path/to/lib"]
        }"#;

        let json_value: serde_json::Value = serde_json::from_str(json_str).unwrap();
        let mut config = Config::default();
        config.update_from_value(json_value);

        assert_eq!(config.source_roots.len(), 2);
        assert_eq!(config.source_roots[0], PathBuf::from("/path/to/src"));
        assert_eq!(config.source_roots[1], PathBuf::from("/path/to/lib"));
    }

    #[test]
    fn test_source_roots_from_toml() {
        let toml_str = r#"source_roots = ["/path/to/src", "/path/to/lib"]"#;

        let table: toml::Table = toml::from_str(toml_str).unwrap();
        let toml_value = toml::Value::Table(table);
        let mut config = Config::default();
        config.load_from_toml_value(toml_value).unwrap();

        assert_eq!(config.source_roots.len(), 2);
        assert_eq!(config.source_roots[0], PathBuf::from("/path/to/src"));
        assert_eq!(config.source_roots[1], PathBuf::from("/path/to/lib"));
    }

    #[test]
    fn test_exclude_patterns_from_json() {
        let json_str = r#"{
            "excludePatterns": ["venv", ".venv", "__pycache__"]
        }"#;

        let json_value: serde_json::Value = serde_json::from_str(json_str).unwrap();
        let mut config = Config::default();
        config.update_from_value(json_value);

        assert_eq!(config.exclude_patterns.len(), 3);
        assert_eq!(config.exclude_patterns[0], "venv");
        assert_eq!(config.exclude_patterns[1], ".venv");
        assert_eq!(config.exclude_patterns[2], "__pycache__");
    }

    #[test]
    fn test_exclude_patterns_from_toml() {
        let toml_str = r#"exclude_patterns = ["venv", ".venv", "__pycache__"]"#;

        let table: toml::Table = toml::from_str(toml_str).unwrap();
        let toml_value = toml::Value::Table(table);
        let mut config = Config::default();
        config.load_from_toml_value(toml_value).unwrap();

        assert_eq!(config.exclude_patterns.len(), 3);
        assert_eq!(config.exclude_patterns[0], "venv");
        assert_eq!(config.exclude_patterns[1], ".venv");
        assert_eq!(config.exclude_patterns[2], "__pycache__");
    }

    #[test]
    fn test_stub_paths_from_json() {
        let json_str = r#"{
            "stubPaths": ["/path/to/stubs", "/another/path"]
        }"#;

        let json_value: serde_json::Value = serde_json::from_str(json_str).unwrap();
        let mut config = Config::default();
        config.update_from_value(json_value);

        assert_eq!(config.stub_paths.len(), 2);
        assert_eq!(config.stub_paths[0], PathBuf::from("/path/to/stubs"));
        assert_eq!(config.stub_paths[1], PathBuf::from("/another/path"));
    }

    #[test]
    fn test_stub_paths_from_toml() {
        let toml_str = r#"stub_paths = ["/path/to/stubs", "/another/path"]"#;

        let table: toml::Table = toml::from_str(toml_str).unwrap();
        let toml_value = toml::Value::Table(table);
        let mut config = Config::default();
        config.load_from_toml_value(toml_value).unwrap();

        assert_eq!(config.stub_paths.len(), 2);
        assert_eq!(config.stub_paths[0], PathBuf::from("/path/to/stubs"));
        assert_eq!(config.stub_paths[1], PathBuf::from("/another/path"));
    }

    #[test]
    fn test_unresolved_import_severity_from_json() {
        let json_str = r#"{
            "unresolvedImportSeverity": "error"
        }"#;

        let json_value: serde_json::Value = serde_json::from_str(json_str).unwrap();
        let mut config = Config::default();
        config.update_from_value(json_value);

        assert_eq!(config.unresolved_import_severity, DiagnosticSeverity::Error);
    }

    #[test]
    fn test_unresolved_import_severity_from_toml() {
        let toml_str = r#"unresolved_import_severity = "info""#;

        let table: toml::Table = toml::from_str(toml_str).unwrap();
        let toml_value = toml::Value::Table(table);
        let mut config = Config::default();
        config.load_from_toml_value(toml_value).unwrap();

        assert_eq!(config.unresolved_import_severity, DiagnosticSeverity::Info);
    }

    #[test]
    fn test_circular_import_severity_from_json() {
        let json_str = r#"{
            "circularImportSeverity": "error"
        }"#;

        let json_value: serde_json::Value = serde_json::from_str(json_str).unwrap();
        let mut config = Config::default();
        config.update_from_value(json_value);

        assert_eq!(config.circular_import_severity, DiagnosticSeverity::Error);
    }

    #[test]
    fn test_circular_import_severity_from_toml() {
        let toml_str = r#"circular_import_severity = "info""#;

        let table: toml::Table = toml::from_str(toml_str).unwrap();
        let toml_value = toml::Value::Table(table);
        let mut config = Config::default();
        config.load_from_toml_value(toml_value).unwrap();

        assert_eq!(config.circular_import_severity, DiagnosticSeverity::Info);
    }

    #[test]
    fn test_combined_configuration_from_toml() {
        let temp_dir = TempDir::new().unwrap();
        let beacon_toml_path = temp_dir.path().join("beacon.toml");
        let toml_content = r#"
python_version = "3.11"
source_roots = ["/custom/src"]
exclude_patterns = ["venv", ".venv"]
stub_paths = ["/custom/stubs"]
unresolved_import_severity = "error"
circular_import_severity = "info"

[type_checking]
mode = "strict"
"#;

        fs::write(&beacon_toml_path, toml_content).unwrap();

        let config = Config::discover_and_load(temp_dir.path()).unwrap();

        assert_eq!(config.type_checking.mode, TypeCheckingMode::Strict);
        assert_eq!(config.python_version, PythonVersion::Py311);
        assert_eq!(config.source_roots.len(), 1);
        assert_eq!(config.source_roots[0], PathBuf::from("/custom/src"));
        assert_eq!(config.exclude_patterns.len(), 2);
        assert_eq!(config.exclude_patterns[0], "venv");
        assert_eq!(config.stub_paths.len(), 1);
        assert_eq!(config.stub_paths[0], PathBuf::from("/custom/stubs"));
        assert_eq!(config.unresolved_import_severity, DiagnosticSeverity::Error);
        assert_eq!(config.circular_import_severity, DiagnosticSeverity::Info);
    }

    #[test]
    fn test_formatting_config_default() {
        let config = Config::default();
        assert!(config.formatting.enabled);
        assert_eq!(config.formatting.line_length, 88);
        assert_eq!(config.formatting.indent_size, 4);
    }

    #[test]
    fn test_formatting_config_from_json() {
        let json_str = r#"{
            "formatting": {
                "enabled": true,
                "lineLength": 100,
                "indentSize": 2,
                "quoteStyle": "single"
            }
        }"#;

        let json_value: serde_json::Value = serde_json::from_str(json_str).unwrap();
        let mut config = Config::default();
        config.update_from_value(json_value);

        assert!(config.formatting.enabled);
        assert_eq!(config.formatting.line_length, 100);
        assert_eq!(config.formatting.indent_size, 2);
        assert_eq!(
            config.formatting.quote_style,
            crate::formatting::config::QuoteStyle::Single
        );
    }

    #[test]
    fn test_formatting_config_from_toml() {
        let temp_dir = TempDir::new().unwrap();
        let beacon_toml_path = temp_dir.path().join("beacon.toml");
        let toml_content = r#"
[formatting]
enabled = true
line_length = 120
indent_size = 2
quote_style = "single"
trailing_commas = "always"
"#;

        fs::write(&beacon_toml_path, toml_content).unwrap();

        let config = Config::discover_and_load(temp_dir.path()).unwrap();

        assert!(config.formatting.enabled);
        assert_eq!(config.formatting.line_length, 120);
        assert_eq!(config.formatting.indent_size, 2);
        assert_eq!(
            config.formatting.quote_style,
            crate::formatting::config::QuoteStyle::Single
        );
        assert_eq!(
            config.formatting.trailing_commas,
            crate::formatting::config::TrailingCommas::Always
        );
    }

    #[test]
    fn test_formatting_config_hot_reload() {
        let json_str = r#"{
            "formatting": {
                "lineLength": 79
            }
        }"#;

        let json_value: serde_json::Value = serde_json::from_str(json_str).unwrap();
        let mut config = Config::default();
        let original_enabled = config.formatting.enabled;

        config.update_from_value(json_value);

        assert_eq!(config.formatting.enabled, original_enabled);
        assert_eq!(config.formatting.line_length, 79);
    }

    #[test]
    fn test_type_checking_mode_validation() {
        let strict = TypeCheckingMode::Strict;
        let balanced = TypeCheckingMode::Balanced;
        let relaxed = TypeCheckingMode::Relaxed;

        assert!(strict.validate().is_ok());
        assert!(balanced.validate().is_ok());
        assert!(relaxed.validate().is_ok());
    }

    #[test]
    fn test_type_checking_mode_requires_annotations() {
        assert!(TypeCheckingMode::Strict.requires_annotations());
        assert!(!TypeCheckingMode::Balanced.requires_annotations());
        assert!(!TypeCheckingMode::Relaxed.requires_annotations());
    }

    #[test]
    fn test_type_checking_mode_allows_implicit_any() {
        assert!(!TypeCheckingMode::Strict.allows_implicit_any());
        assert!(!TypeCheckingMode::Balanced.allows_implicit_any());
        assert!(TypeCheckingMode::Relaxed.allows_implicit_any());
    }

    #[test]
    fn test_type_checking_mode_missing_annotation_severity() {
        assert_eq!(
            TypeCheckingMode::Strict.missing_annotation_severity(),
            Some(DiagnosticSeverity::Error)
        );
        assert_eq!(
            TypeCheckingMode::Balanced.missing_annotation_severity(),
            Some(DiagnosticSeverity::Warning)
        );
        assert_eq!(TypeCheckingMode::Relaxed.missing_annotation_severity(), None);
    }

    #[test]
    fn test_type_checking_config_validation() {
        let config = TypeCheckingConfig { mode: TypeCheckingMode::Strict };
        assert!(config.validate().is_ok());

        let config = TypeCheckingConfig { mode: TypeCheckingMode::Balanced };
        assert!(config.validate().is_ok());

        let config = TypeCheckingConfig { mode: TypeCheckingMode::Relaxed };
        assert!(config.validate().is_ok());
    }

    #[test]
    fn test_type_checking_config_default() {
        let config = TypeCheckingConfig::default();
        assert_eq!(config.mode, TypeCheckingMode::Balanced);
    }

    #[test]
    fn test_type_checking_config_new() {
        let config = TypeCheckingConfig::new();
        assert_eq!(config.mode, TypeCheckingMode::Balanced);
    }

    #[test]
    fn test_config_validates_type_checking() {
        let config =
            Config { type_checking: TypeCheckingConfig { mode: TypeCheckingMode::Strict }, ..Default::default() };
        assert!(config.validate().is_ok());
    }

    #[test]
    fn test_type_checking_serialization() {
        let config = TypeCheckingConfig { mode: TypeCheckingMode::Strict };
        let json = serde_json::to_string(&config).unwrap();
        let deserialized: TypeCheckingConfig = serde_json::from_str(&json).unwrap();
        assert_eq!(config.mode, deserialized.mode);
    }

    #[test]
    fn test_type_checking_from_json() {
        let json_str = r#"{"mode": "strict"}"#;
        let config: TypeCheckingConfig = serde_json::from_str(json_str).unwrap();
        assert_eq!(config.mode, TypeCheckingMode::Strict);

        let json_str = r#"{"mode": "balanced"}"#;
        let config: TypeCheckingConfig = serde_json::from_str(json_str).unwrap();
        assert_eq!(config.mode, TypeCheckingMode::Balanced);

        let json_str = r#"{"mode": "relaxed"}"#;
        let config: TypeCheckingConfig = serde_json::from_str(json_str).unwrap();
        assert_eq!(config.mode, TypeCheckingMode::Relaxed);
    }

    #[test]
    fn test_type_checking_from_toml() {
        let toml_str = r#"mode = "strict""#;
        let config: TypeCheckingConfig = toml::from_str(toml_str).unwrap();
        assert_eq!(config.mode, TypeCheckingMode::Strict);

        let toml_str = r#"mode = "balanced""#;
        let config: TypeCheckingConfig = toml::from_str(toml_str).unwrap();
        assert_eq!(config.mode, TypeCheckingMode::Balanced);

        let toml_str = r#"mode = "relaxed""#;
        let config: TypeCheckingConfig = toml::from_str(toml_str).unwrap();
        assert_eq!(config.mode, TypeCheckingMode::Relaxed);
    }

    #[test]
    fn test_type_checking_mode_from_str() {
        assert_eq!("strict".parse::<TypeCheckingMode>(), Ok(TypeCheckingMode::Strict));
        assert_eq!("balanced".parse::<TypeCheckingMode>(), Ok(TypeCheckingMode::Balanced));
        assert_eq!("relaxed".parse::<TypeCheckingMode>(), Ok(TypeCheckingMode::Relaxed));

        assert_eq!("STRICT".parse::<TypeCheckingMode>(), Ok(TypeCheckingMode::Strict));
        assert_eq!(
            "  balanced  ".parse::<TypeCheckingMode>(),
            Ok(TypeCheckingMode::Balanced)
        );

        assert!("invalid".parse::<TypeCheckingMode>().is_err());
        assert!("".parse::<TypeCheckingMode>().is_err());
    }

    #[test]
    fn test_parse_mode_error_display() {
        let err = "invalid".parse::<TypeCheckingMode>().unwrap_err();
        assert!(err.to_string().contains("invalid"));
        assert!(err.to_string().contains("strict"));
        assert!(err.to_string().contains("balanced"));
        assert!(err.to_string().contains("relaxed"));
    }

    #[test]
    fn test_type_checking_mode_as_str() {
        assert_eq!(TypeCheckingMode::Strict.as_str(), "strict");
        assert_eq!(TypeCheckingMode::Balanced.as_str(), "balanced");
        assert_eq!(TypeCheckingMode::Relaxed.as_str(), "relaxed");
    }

    #[test]
    fn test_parse_mode_directive() {
        let test_cases = [
            ParseModeTestCase {
                name: "basic strict mode",
                source: r#"
# beacon: mode=strict
def foo():
    pass
"#,
                want: Some(TypeCheckingMode::Strict),
            },
            ParseModeTestCase {
                name: "basic balanced mode",
                source: r#"
# beacon: mode=balanced
def foo():
    pass
"#,
                want: Some(TypeCheckingMode::Balanced),
            },
            ParseModeTestCase {
                name: "basic relaxed mode",
                source: r#"
# beacon: mode=relaxed
def foo():
    pass
"#,
                want: Some(TypeCheckingMode::Relaxed),
            },
            ParseModeTestCase {
                name: "case insensitive BEACON MODE=STRICT",
                source: r#"
# BEACON: MODE=STRICT
def foo():
    pass
"#,
                want: Some(TypeCheckingMode::Strict),
            },
            ParseModeTestCase {
                name: "case insensitive Beacon Mode=Relaxed",
                source: r#"
# Beacon: Mode=Relaxed
def foo():
    pass
"#,
                want: Some(TypeCheckingMode::Relaxed),
            },
            ParseModeTestCase {
                name: "with extra whitespace",
                source: r#"
#   beacon:   mode=strict
def foo():
    pass
"#,
                want: Some(TypeCheckingMode::Strict),
            },
            ParseModeTestCase {
                name: "no whitespace",
                source: r#"
#beacon:mode=balanced
def foo():
    pass
"#,
                want: Some(TypeCheckingMode::Balanced),
            },
            ParseModeTestCase {
                name: "first line",
                source: r#"# beacon: mode=strict
def foo():
    pass
"#,
                want: Some(TypeCheckingMode::Strict),
            },
            ParseModeTestCase {
                name: "within 10 lines (line 9)",
                source: r#"
# Line 1
# Line 2
# Line 3
# Line 4
# Line 5
# Line 6
# Line 7
# Line 8
# beacon: mode=relaxed
def foo():
    pass
"#,
                want: Some(TypeCheckingMode::Relaxed),
            },
            ParseModeTestCase {
                name: "beyond 10 lines (line 11)",
                source: r#"
# Line 1
# Line 2
# Line 3
# Line 4
# Line 5
# Line 6
# Line 7
# Line 8
# Line 9
# Line 10
# beacon: mode=strict
def foo():
    pass
"#,
                want: None,
            },
            ParseModeTestCase {
                name: "not found - regular comment",
                source: r#"
# Just a regular comment
def foo():
    pass
"#,
                want: None,
            },
            ParseModeTestCase {
                name: "not found - no comments",
                source: r#"
def foo():
    pass
"#,
                want: None,
            },
            ParseModeTestCase {
                name: "invalid mode value",
                source: r#"
# beacon: mode=invalid
def foo():
    pass
"#,
                want: None,
            },
            ParseModeTestCase {
                name: "empty mode value",
                source: r#"
# beacon: mode=
def foo():
    pass
"#,
                want: None,
            },
            ParseModeTestCase {
                name: "with additional directives",
                source: r#"
# beacon: mode=strict, other=value
def foo():
    pass
"#,
                want: Some(TypeCheckingMode::Strict),
            },
            ParseModeTestCase {
                name: "inline code comment",
                source: r#"
x = 5  # beacon: mode=strict
def foo():
    pass
"#,
                want: Some(TypeCheckingMode::Strict),
            },
            ParseModeTestCase {
                name: "after docstring",
                source: r#"""Module docstring"""
# beacon: mode=balanced
def foo():
    pass
"#,
                want: Some(TypeCheckingMode::Balanced),
            },
            ParseModeTestCase {
                name: "after shebang",
                source: r#"#!/usr/bin/env python3
# beacon: mode=strict
def foo():
    pass
"#,
                want: Some(TypeCheckingMode::Strict),
            },
            ParseModeTestCase {
                name: "only first match",
                source: r#"
# beacon: mode=strict
# beacon: mode=relaxed
def foo():
    pass
"#,
                want: Some(TypeCheckingMode::Strict),
            },
        ];

        for test_case in test_cases {
            let got = parse_mode_directive(test_case.source);
            assert_eq!(
                got, test_case.want,
                "Test case '{}' failed: expected {:?}, got {:?}",
                test_case.name, test_case.want, got
            );
        }
    }
}

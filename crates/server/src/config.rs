//! LSP configuration and workspace settings
//!
//! This module defines the configuration options for the Beacon LSP server,
//! including type checking modes, Python version targeting, stub paths, and
//! other customizable behaviors.

use beacon_core::{Result, errors};
use serde::{Deserialize, Serialize};
use std::fs;
use std::path::{Path, PathBuf};

/// Diagnostic severity level for configurable diagnostics
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum DiagnosticSeverity {
    /// Informational message
    Info,
    /// Warning message
    Warning,
    /// Error message
    Error,
}

impl Default for DiagnosticSeverity {
    fn default() -> Self {
        Self::Warning
    }
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum TypeCheckingMode {
    /// Annotation mismatches are hard errors, strict enforcement
    Strict,
    /// Annotation mismatches are diagnostics with quick fixes, but inference still proceeds (default)
    Balanced,
    /// Annotations supply upper/lower bounds but can be overridden by inference, very lenient
    Loose,
}

impl Default for TypeCheckingMode {
    fn default() -> Self {
        Self::Balanced
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
    /// Type checking strictness mode
    #[serde(default)]
    pub mode: TypeCheckingMode,

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
            mode: TypeCheckingMode::default(),
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
        // Parse as Config (camelCase for LSP) and merge non-default values
        if let Ok(partial) = serde_json::from_value::<Config>(value) {
            // Only update fields that differ from defaults
            let defaults = Config::default();

            if partial.mode != defaults.mode {
                self.mode = partial.mode;
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
        }
    }

    /// Validate configuration and return any errors
    ///
    /// Checks that stub paths exist and are accessible.
    pub fn validate(&self) -> Result<()> {
        // Validate stub paths exist and are readable
        for stub_path in &self.stub_paths {
            if !stub_path.exists() {
                tracing::warn!("Stub path does not exist: {}", stub_path.display());
            } else if !stub_path.is_dir() {
                tracing::warn!("Stub path is not a directory: {}", stub_path.display());
            }
        }

        // Validate source roots exist
        for source_root in &self.source_roots {
            if !source_root.exists() {
                tracing::warn!("Source root does not exist: {}", source_root.display());
            }
        }

        // Validate cache size is reasonable
        if self.cache_size == 0 {
            tracing::warn!("Cache size is 0, caching will be ineffective");
        }

        // TODO: Validate decorator stub syntax when decorator stub loading is implemented
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

        // Try beacon.toml first
        let beacon_toml = workspace_root.join("beacon.toml");
        if beacon_toml.exists() {
            tracing::info!("Loading configuration from {}", beacon_toml.display());
            config.load_from_toml_file(&beacon_toml)?;
            return Ok(config);
        }

        // Try pyproject.toml [tool.beacon] section
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

        // No configuration found, return defaults
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

    #[test]
    fn test_default_config() {
        let config = Config::default();
        assert_eq!(config.mode, TypeCheckingMode::Balanced);
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
        let config =
            Config { mode: TypeCheckingMode::Strict, python_version: PythonVersion::Py311, ..Default::default() };

        let json = serde_json::to_string(&config).unwrap();
        let deserialized: Config = serde_json::from_str(&json).unwrap();

        assert_eq!(config.mode, deserialized.mode);
        assert_eq!(config.python_version, deserialized.python_version);
    }

    #[test]
    fn test_toml_to_json_conversion() {
        let toml_str = r#"mode = "strict"
python_version = "3.11"
max_any_depth = 5
incremental = false"#;

        let table: toml::Table = toml::from_str(toml_str).unwrap();
        let toml_value = toml::Value::Table(table);
        let json_value = toml_to_json(toml_value);

        let obj = json_value.as_object().unwrap();
        assert!(obj.contains_key("mode"));
        assert!(obj.contains_key("pythonVersion"));
        assert!(obj.contains_key("maxAnyDepth"));
        assert!(obj.contains_key("incremental"));

        assert_eq!(obj.get("mode").unwrap().as_str().unwrap(), "strict");
        assert_eq!(obj.get("pythonVersion").unwrap().as_str().unwrap(), "3.11");
        assert_eq!(obj.get("maxAnyDepth").unwrap().as_i64().unwrap(), 5);
        assert!(!obj.get("incremental").unwrap().as_bool().unwrap());
    }

    #[test]
    fn test_load_from_toml_value() {
        let toml_str = r#"mode = "strict"
python_version = "3.11"
max_any_depth = 5"#;

        let table: toml::Table = toml::from_str(toml_str).unwrap();
        let toml_value = toml::Value::Table(table);
        let mut config = Config::default();
        config.load_from_toml_value(toml_value).unwrap();

        assert_eq!(config.mode, TypeCheckingMode::Strict);
        assert_eq!(config.python_version, PythonVersion::Py311);
        assert_eq!(config.max_any_depth, 5);
    }

    #[test]
    fn test_update_from_value_json() {
        let json_str = r#"{
            "mode": "loose",
            "pythonVersion": "3.13",
            "maxAnyDepth": 10
        }"#;

        let json_value: serde_json::Value = serde_json::from_str(json_str).unwrap();
        let mut config = Config::default();
        config.update_from_value(json_value);

        assert_eq!(config.mode, TypeCheckingMode::Loose);
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
        assert_eq!(config.mode, TypeCheckingMode::default());
        assert_eq!(config.python_version, PythonVersion::default());
    }

    #[test]
    fn test_config_discover_beacon_toml() {
        let temp_dir = TempDir::new().unwrap();
        let beacon_toml_path = temp_dir.path().join("beacon.toml");
        let toml_content = r#"mode = "strict"
python_version = "3.10"
max_any_depth = 7
incremental = false"#;

        fs::write(&beacon_toml_path, toml_content).unwrap();

        let config = Config::discover_and_load(temp_dir.path()).unwrap();

        assert_eq!(config.mode, TypeCheckingMode::Strict);
        assert_eq!(config.python_version, PythonVersion::Py310);
        assert_eq!(config.max_any_depth, 7);
        assert!(!config.incremental);
    }

    #[test]
    fn test_config_discover_pyproject_toml() {
        let temp_dir = TempDir::new().unwrap();
        let pyproject_path = temp_dir.path().join("pyproject.toml");
        let toml_content = r#"[tool.beacon]
mode = "loose"
python_version = "3.13""#;

        fs::write(&pyproject_path, toml_content).unwrap();

        let config = Config::discover_and_load(temp_dir.path()).unwrap();
        assert_eq!(config.mode, TypeCheckingMode::Loose);
        assert_eq!(config.python_version, PythonVersion::Py313);
    }

    #[test]
    fn test_beacon_toml_takes_precedence() {
        let temp_dir = TempDir::new().unwrap();
        let beacon_toml_path = temp_dir.path().join("beacon.toml");
        let pyproject_path = temp_dir.path().join("pyproject.toml");

        fs::write(&beacon_toml_path, r#"mode = "strict""#).unwrap();
        fs::write(
            &pyproject_path,
            r#"[tool.beacon]
mode = "loose""#,
        )
        .unwrap();

        let config = Config::discover_and_load(temp_dir.path()).unwrap();
        assert_eq!(config.mode, TypeCheckingMode::Strict);
    }

    #[test]
    fn test_update_from_value_preserves_defaults() {
        let json_str = r#"{
            "mode": "strict"
        }"#;

        let json_value: serde_json::Value = serde_json::from_str(json_str).unwrap();
        let mut config = Config::default();
        let original_version = config.python_version;

        config.update_from_value(json_value);

        assert_eq!(config.mode, TypeCheckingMode::Strict);
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
        let toml_content = r#"mode = "strict"

[inlay_hints]
enable = true
variable_types = false
parameter_names = true
"#;

        fs::write(&beacon_toml_path, toml_content).unwrap();

        let config = Config::discover_and_load(temp_dir.path()).unwrap();

        assert_eq!(config.mode, TypeCheckingMode::Strict);
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
        let toml_content = r#"mode = "strict"
python_version = "3.11"
source_roots = ["/custom/src"]
exclude_patterns = ["venv", ".venv"]
stub_paths = ["/custom/stubs"]
unresolved_import_severity = "error"
circular_import_severity = "info"
"#;

        fs::write(&beacon_toml_path, toml_content).unwrap();

        let config = Config::discover_and_load(temp_dir.path()).unwrap();

        assert_eq!(config.mode, TypeCheckingMode::Strict);
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
}

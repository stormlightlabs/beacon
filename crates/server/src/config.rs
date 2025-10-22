//! LSP configuration and workspace settings
//!
//! This module defines the configuration options for the Beacon LSP server,
//! including type checking modes, Python version targeting, stub paths, and
//! other customizable behaviors.

use beacon_core::Result;
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

/// Type checking strictness mode
///
/// Controls how the type checker handles annotation mismatches and inference.
/// See ROADMAP.md "Annotation Interaction" section for details.
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
    /// Controls how far `Any` can flow through the codebase before
    /// generating warnings. Higher values are more permissive.
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
            stub_paths: Vec::new(),
            max_any_depth: default_max_any_depth(),
            decorator_stubs: Vec::new(),
            incremental: default_true(),
            workspace_analysis: default_true(),
            enable_caching: default_true(),
            cache_size: default_cache_size(),
            experimental: false,
        }
    }
}

impl Config {
    /// Create a new configuration with default values
    pub fn new() -> Self {
        Self::default()
    }

    /// Update configuration from LSP initialization options
    ///
    /// TODO: Implement merging of user-provided options
    pub fn update_from_value(&mut self, _value: serde_json::Value) {
        // TODO: Parse and merge configuration from LSP client
    }

    /// Validate configuration and return any errors
    ///
    /// TODO: Implement validation (e.g., stub paths exist, decorator stubs parse)
    pub fn validate(&self) -> Result<()> {
        // TODO: Validate stub paths exist
        // TODO: Validate decorator stub syntax
        Ok(())
    }

    /// Get effective stub search paths including standard library
    ///
    /// TODO: Include stdlib typeshed paths based on python_version
    pub fn effective_stub_paths(&self) -> Vec<PathBuf> {
        let paths = self.stub_paths.clone();
        // TODO: Add typeshed stdlib paths
        // TODO: Add site-packages paths
        paths
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
}

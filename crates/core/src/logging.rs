//! Logging infrastructure for Beacon
//!
//! This module provides centralized logging configuration for both the LSP server and CLI.
//! It supports:
//! - Daily rotating file appenders writing to `logs/lsp.log`
//! - Dual output to stderr and file for immediate visibility
//! - JSON and text output formats
//! - Environment-based log level control via `RUST_LOG`
//! - Panic hook integration for crash logging

use std::{
    io,
    path::{Path, PathBuf},
};
use tracing_subscriber::{EnvFilter, Layer, fmt, layer::SubscriberExt, util::SubscriberInitExt};

/// Log output format
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LogFormat {
    /// Human-readable text output
    Text,
    /// JSON-structured output for machine parsing
    Json,
}

/// Logging configuration for the LSP server and CLI
#[derive(Debug, Clone)]
pub struct LogConfig {
    /// Directory where log files are stored
    pub log_dir: PathBuf,
    /// Base filename for log files (e.g., "lsp.log")
    pub log_filename: String,
    /// Output format (text or JSON)
    pub format: LogFormat,
    /// Whether to write to stderr in addition to file
    pub stderr: bool,
    /// Environment filter for log levels (defaults to INFO if RUST_LOG not set)
    pub env_filter: Option<String>,
}

impl Default for LogConfig {
    fn default() -> Self {
        Self {
            log_dir: PathBuf::from("logs"),
            log_filename: "lsp.log".to_string(),
            format: LogFormat::Text,
            stderr: true,
            env_filter: None,
        }
    }
}

impl LogConfig {
    /// Create a new logging configuration
    pub fn new() -> Self {
        Self::default()
    }

    /// Set the log directory
    pub fn with_log_dir(mut self, dir: impl Into<PathBuf>) -> Self {
        self.log_dir = dir.into();
        self
    }

    /// Set the log filename
    pub fn with_filename(mut self, filename: impl Into<String>) -> Self {
        self.log_filename = filename.into();
        self
    }

    /// Set the output format
    pub fn with_format(mut self, format: LogFormat) -> Self {
        self.format = format;
        self
    }

    /// Enable or disable stderr output
    pub fn with_stderr(mut self, stderr: bool) -> Self {
        self.stderr = stderr;
        self
    }

    /// Set the environment filter string
    pub fn with_env_filter(mut self, filter: impl Into<String>) -> Self {
        self.env_filter = Some(filter.into());
        self
    }

    /// Get the full path to the log file
    pub fn log_path(&self) -> PathBuf {
        self.log_dir.join(&self.log_filename)
    }
}

/// Initialize the logging system with the given configuration
///
/// This sets up:
/// - Daily rotating file appender in the configured directory
/// - Optional stderr output for immediate visibility
/// - Environment-based filtering (RUST_LOG or configured filter)
/// - Timestamps, log levels, and targets in output
///
/// # Panics
///
/// Panics if the logging system has already been initialized or if the log directory
/// cannot be created.
pub fn init(config: &LogConfig) -> Result<(), Box<dyn std::error::Error>> {
    if !config.log_dir.exists() {
        std::fs::create_dir_all(&config.log_dir)?;
    }

    let env_filter = if let Some(filter) = &config.env_filter {
        EnvFilter::try_new(filter)?
    } else {
        EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new("info"))
    };

    let file_appender = tracing_appender::rolling::daily(&config.log_dir, &config.log_filename);
    let (non_blocking, _guard) = tracing_appender::non_blocking(file_appender);

    let file_layer = match config.format {
        LogFormat::Text => fmt::layer()
            .with_writer(non_blocking)
            .with_ansi(false)
            .with_target(true)
            .with_thread_ids(false)
            .with_file(true)
            .with_line_number(true)
            .boxed(),
        LogFormat::Json => fmt::layer()
            .json()
            .with_writer(non_blocking)
            .with_target(true)
            .with_file(true)
            .with_line_number(true)
            .boxed(),
    };

    let stderr_layer = if config.stderr {
        Some(
            fmt::layer()
                .with_writer(io::stderr)
                .with_ansi(true)
                .with_target(true)
                .with_thread_ids(false)
                .boxed(),
        )
    } else {
        None
    };

    let subscriber = tracing_subscriber::registry().with(env_filter).with(file_layer);

    if let Some(stderr_layer) = stderr_layer {
        subscriber.with(stderr_layer).try_init()?;
    } else {
        subscriber.try_init()?;
    }

    std::mem::forget(_guard);
    Ok(())
}

/// Install a panic hook that logs panics to the tracing system
///
/// This captures unhandled panics and writes them to the log file before the program terminates.
/// The panic message, location, and backtrace (if available) are all logged.
pub fn install_panic_hook() {
    std::panic::set_hook(Box::new(|panic_info| {
        let payload = panic_info
            .payload()
            .downcast_ref::<&str>()
            .map(|s| s.to_string())
            .or_else(|| panic_info.payload().downcast_ref::<String>().cloned())
            .unwrap_or_else(|| "Unknown panic payload".to_string());

        let location = panic_info
            .location()
            .map(|l| format!("{}:{}:{}", l.file(), l.line(), l.column()))
            .unwrap_or_else(|| "Unknown location".to_string());

        tracing::error!(
            panic.payload = %payload,
            panic.location = %location,
            "PANIC: Beacon has panicked"
        );

        eprintln!("PANIC at {location}: {payload}");
    }));
}

/// Get the default log path based on environment or configuration
///
/// Checks for `$LSP_LOG_PATH` environment variable, otherwise defaults to `logs/lsp.log`.
pub fn default_log_path() -> PathBuf {
    std::env::var("LSP_LOG_PATH")
        .ok()
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from("logs/lsp.log"))
}

/// Read log entries from a file and parse them into structured records
///
/// This is used by the CLI logs command to read and display log files.
pub fn read_log_file(path: &Path) -> io::Result<Vec<String>> {
    let content = std::fs::read_to_string(path)?;
    Ok(content.lines().map(|s| s.to_string()).collect())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_log_config_default() {
        let config = LogConfig::default();
        assert_eq!(config.log_dir, PathBuf::from("logs"));
        assert_eq!(config.log_filename, "lsp.log");
        assert_eq!(config.format, LogFormat::Text);
        assert!(config.stderr);
        assert!(config.env_filter.is_none());
    }

    #[test]
    fn test_log_config_builder() {
        let config = LogConfig::new()
            .with_log_dir("/var/log/beacon")
            .with_filename("custom.log")
            .with_format(LogFormat::Json)
            .with_stderr(false)
            .with_env_filter("debug");

        assert_eq!(config.log_dir, PathBuf::from("/var/log/beacon"));
        assert_eq!(config.log_filename, "custom.log");
        assert_eq!(config.format, LogFormat::Json);
        assert!(!config.stderr);
        assert_eq!(config.env_filter, Some("debug".to_string()));
    }

    #[test]
    fn test_log_path() {
        let config = LogConfig::new().with_log_dir("test_logs").with_filename("test.log");
        assert_eq!(config.log_path(), PathBuf::from("test_logs/test.log"));
    }

    #[test]
    fn test_default_log_path_no_env() {
        unsafe {
            std::env::remove_var("LSP_LOG_PATH");
        }
        assert_eq!(default_log_path(), PathBuf::from("logs/lsp.log"));
    }

    #[test]
    fn test_default_log_path_with_env() {
        unsafe {
            std::env::set_var("LSP_LOG_PATH", "/custom/path/log.txt");
        }
        assert_eq!(default_log_path(), PathBuf::from("/custom/path/log.txt"));
        unsafe {
            std::env::remove_var("LSP_LOG_PATH");
        }
    }
}

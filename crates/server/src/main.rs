//! Beacon LSP
//!
//! Entry point for the Beacon Language Server Protocol implementation.
//! Supports both stdio (default) and TCP modes.

use beacon_core::logging::{LogConfig, init, install_panic_hook};
use beacon_lsp::{run_server_stdio, run_server_tcp};
use clap::Parser;

#[derive(Parser)]
#[command(name = "beacon-lsp")]
#[command(about = "Beacon Language Server - Hindley-Milner type system for Python")]
#[command(version = env!("CARGO_PKG_VERSION"))]
struct Args {
    /// Use stdio for communication (default)
    #[arg(long, conflicts_with = "tcp", help = "Use stdio for communication (default)")]
    stdio: bool,

    /// Use TCP for communication
    #[arg(long, conflicts_with = "stdio", help = "Use TCP for communication")]
    tcp: bool,

    /// Host to bind TCP server to (requires --tcp)
    #[arg(long, default_value = "127.0.0.1", help = "Host to bind TCP server to")]
    host: String,

    /// Port to bind TCP server to (requires --tcp)
    #[arg(long, default_value = "9350", help = "Port to bind TCP server to")]
    port: u16,
}

#[tokio::main]
async fn main() {
    let args = Args::parse();

    let config = LogConfig::new()
        .with_log_dir("logs")
        .with_filename("lsp.log")
        .with_stderr(true);

    if let Err(e) = init(&config) {
        eprintln!("Failed to initialize logging: {e}");
        std::process::exit(1);
    }

    install_panic_hook();

    tracing::info!(version = env!("CARGO_PKG_VERSION"), "Starting Beacon LSP server");

    if args.tcp {
        tracing::info!("Running in TCP mode on {}:{}", args.host, args.port);
        if let Err(e) = run_server_tcp(&args.host, args.port).await {
            tracing::error!("TCP server error: {}", e);
            eprintln!("Failed to start TCP server: {e}");
            std::process::exit(1);
        }
    } else {
        tracing::info!("Running in stdio mode");
        run_server_stdio().await;
    }

    tracing::info!("Beacon LSP server shut down");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_args_default_stdio() {
        let args = Args::parse_from(&["beacon-lsp"]);
        assert!(!args.tcp);
        assert!(!args.stdio);
        assert_eq!(args.host, "127.0.0.1");
        assert_eq!(args.port, 9350);
    }

    #[test]
    fn test_args_explicit_stdio() {
        let args = Args::parse_from(&["beacon-lsp", "--stdio"]);
        assert!(!args.tcp);
        assert!(args.stdio);
    }

    #[test]
    fn test_args_tcp_mode() {
        let args = Args::parse_from(&["beacon-lsp", "--tcp"]);
        assert!(args.tcp);
        assert!(!args.stdio);
        assert_eq!(args.host, "127.0.0.1");
        assert_eq!(args.port, 9350);
    }

    #[test]
    fn test_args_tcp_custom_host() {
        let args = Args::parse_from(&["beacon-lsp", "--tcp", "--host", "0.0.0.0"]);
        assert!(args.tcp);
        assert_eq!(args.host, "0.0.0.0");
        assert_eq!(args.port, 9350);
    }

    #[test]
    fn test_args_tcp_custom_port() {
        let args = Args::parse_from(&["beacon-lsp", "--tcp", "--port", "8080"]);
        assert!(args.tcp);
        assert_eq!(args.host, "127.0.0.1");
        assert_eq!(args.port, 8080);
    }

    #[test]
    fn test_args_tcp_full_config() {
        let args = Args::parse_from(&["beacon-lsp", "--tcp", "--host", "0.0.0.0", "--port", "8080"]);
        assert!(args.tcp);
        assert_eq!(args.host, "0.0.0.0");
        assert_eq!(args.port, 8080);
    }

    #[test]
    fn test_args_conflicts_stdio_tcp() {
        let result = Args::try_parse_from(&["beacon-lsp", "--stdio", "--tcp"]);
        assert!(
            result.is_err(),
            "Expected error when both --stdio and --tcp are specified"
        );
    }
}

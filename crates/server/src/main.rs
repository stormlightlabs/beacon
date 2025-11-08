//! Beacon LSP
//!
//! Entry point for the Beacon Language Server Protocol implementation.
//! Starts the LSP server and listens on stdin/stdout for LSP messages.

use beacon_core::logging::{LogConfig, init, install_panic_hook};
use beacon_lsp::run_server;

#[tokio::main]
async fn main() {
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

    run_server().await;

    tracing::info!("Beacon LSP server shut down");
}

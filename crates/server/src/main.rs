//! Beacon LSP
//!
//! Entry point for the Beacon Language Server Protocol implementation.
//! Starts the LSP server and listens on stdin/stdout for LSP messages.

use beacon_lsp::run_server;

#[tokio::main]
async fn main() {
    // TODO: switch to file based logging in local dev
    tracing_subscriber::fmt()
        .with_env_filter(tracing_subscriber::EnvFilter::from_default_env().add_directive(tracing::Level::INFO.into()))
        .with_writer(std::io::stderr)
        .init();

    tracing::info!("Starting Beacon LSP server");

    run_server().await;

    tracing::info!("Beacon LSP server shut down");
}

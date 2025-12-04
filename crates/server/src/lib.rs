//! Beacon LSP Server
//!
//! A Language Server Protocol implementation for Beacon, a Hindley-Milner
//! type inference engine for Python.
//!
//! This crate provides:
//! - Full LSP server implementation using tower-lsp
//! - Type inference and constraint solving
//! - Diagnostics, hover, completion, and other IDE features
//! - Incremental analysis and caching
//!
//! # Example
//!
//! ```no_run
//! use beacon_lsp::run_server;
//!
//! #[tokio::main]
//! async fn main() {
//!     run_server().await;
//! }
//! ```

pub mod analysis;
pub mod backend;
pub mod cache;
pub mod config;
pub mod document;
pub mod features;
pub mod formatting;
pub mod interpreter;
pub mod introspection;
pub mod parser;
pub mod utils;
pub mod workspace;

pub use backend::{Backend, SharedState};
pub use config::Config;

use tokio::io::{stdin, stdout};
use tokio::net::TcpListener;
use tower_lsp::{LspService, Server};

/// Run the Beacon LSP server using stdio (default mode)
///
/// Starts the language server and listens on stdin/stdout for LSP messages.
/// This function blocks until the server is shut down.
pub async fn run_server_stdio() {
    let (service, socket) = LspService::new(Backend::new);
    Server::new(stdin(), stdout(), socket).serve(service).await;
}

/// Run the Beacon LSP server using TCP
///
/// Starts the language server and listens for TCP connections on the specified host and port.
/// The server runs indefinitely, accepting sequential connections.
/// Backend state persists across reconnections, following gopls conventions.
pub async fn run_server_tcp(host: &str, port: u16) -> std::io::Result<()> {
    let addr = format!("{host}:{port}");
    let listener = TcpListener::bind(&addr).await?;
    tracing::info!("Beacon LSP server listening on TCP {}", addr);

    let shared_state = SharedState::new();

    loop {
        match listener.accept().await {
            Ok((stream, peer_addr)) => {
                tracing::info!("Client connected from {}", peer_addr);

                let (read, write) = tokio::io::split(stream);
                let shared = &shared_state;

                let (service, socket) = LspService::new(move |client| Backend::with_shared_state(client, shared));

                Server::new(read, write, socket).serve(service).await;

                tracing::info!("Client disconnected from {}", peer_addr);
            }
            Err(e) => {
                tracing::error!("Failed to accept connection: {}", e);
            }
        }
    }
}

/// Run the Beacon LSP server (backward compatibility wrapper)
///
/// Defaults to stdio mode. For TCP mode, use `run_server_tcp()` directly.
pub async fn run_server() {
    run_server_stdio().await;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_backend_creation() {
        let (_service, _socket) = LspService::new(Backend::new);
    }
}

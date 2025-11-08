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

pub use backend::Backend;
pub use config::Config;

use tokio::io::{stdin, stdout};
use tower_lsp::{LspService, Server};

/// Run the Beacon LSP server
///
/// Starts the language server and listens on stdin/stdout for LSP messages.
/// This function blocks until the server is shut down.
pub async fn run_server() {
    let (service, socket) = LspService::new(Backend::new);
    Server::new(stdin(), stdout(), socket).serve(service).await;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_backend_creation() {
        let (_service, _socket) = LspService::new(Backend::new);
    }
}

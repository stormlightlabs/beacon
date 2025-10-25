//! LSP feature implementations
//!
//! This module contains all LSP feature providers:
//! - Diagnostics: type errors, parse errors, linting
//! - Hover: type information and documentation
//! - Completion: code completion suggestions
//! - Go to definition: navigate to symbol definitions
//! - References: find all symbol uses
//! - Inlay hints: inline type annotations
//! - Code actions: quick fixes and refactorings
//! - Semantic tokens: semantic highlighting
//! - Document symbols: outline view for navigation
//! - Document highlight: highlight symbol occurrences
//! - Rename: rename symbols across workspace
//! - Workspace symbols: search symbols across workspace

pub mod code_actions;
pub mod completion;
pub mod diagnostics;
pub mod document_highlight;
pub mod document_symbols;
pub mod dunders;
pub mod goto_definition;
pub mod hover;
pub mod inlay_hints;
pub mod references;
pub mod rename;
pub mod semantic_tokens;
pub mod workspace_symbols;

pub use code_actions::CodeActionsProvider;
pub use completion::CompletionProvider;
pub use diagnostics::DiagnosticProvider;
pub use document_highlight::DocumentHighlightProvider;
pub use document_symbols::DocumentSymbolsProvider;
pub use goto_definition::GotoDefinitionProvider;
pub use hover::HoverProvider;
pub use inlay_hints::InlayHintsProvider;
pub use references::ReferencesProvider;
pub use rename::RenameProvider;
pub use semantic_tokens::{SUPPORTED_MODIFIERS, SUPPORTED_TYPES, SemanticTokensProvider};
pub use workspace_symbols::WorkspaceSymbolsProvider;

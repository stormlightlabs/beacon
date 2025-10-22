//! LSP backend implementation using Tower-LSP
//!
//! Implements the Language Server Protocol for Beacon type checker.
//! Handles all LSP requests and routes them to appropriate feature providers.

use crate::analysis::Analyzer;
use crate::config::Config;
use crate::document::DocumentManager;
use crate::features::*;
use crate::workspace::Workspace;
use lsp_types::{
    CodeActionProviderCapability, CompletionOptions, HoverProviderCapability, InitializeParams, InitializeResult,
    InlayHintServerCapabilities, OneOf, SemanticTokensFullOptions, SemanticTokensLegend, SemanticTokensOptions,
    ServerCapabilities, ServerInfo, TextDocumentSyncCapability, TextDocumentSyncKind, WorkDoneProgressOptions,
};
use std::sync::Arc;
use tokio::sync::RwLock;
use tower_lsp::{Client, LanguageServer, jsonrpc::Result, lsp_types::*};

/// Beacon LSP backend
///
/// Implements the LSP LanguageServer trait and coordinates all features.
pub struct Backend {
    /// LSP client for sending notifications and requests
    client: Client,
    /// Document manager (shared across threads)
    documents: Arc<DocumentManager>,
    /// Type analyzer (shared and mutable)
    analyzer: Arc<RwLock<Analyzer>>,
    /// Workspace manager
    workspace: Arc<RwLock<Workspace>>,
    /// Feature providers
    features: Arc<Features>,
}

/// Collection of all feature providers
struct Features {
    diagnostics: DiagnosticProvider,
    hover: HoverProvider,
    completion: CompletionProvider,
    goto_definition: GotoDefinitionProvider,
    references: ReferencesProvider,
    inlay_hints: InlayHintsProvider,
    code_actions: CodeActionsProvider,
    semantic_tokens: SemanticTokensProvider,
}

impl Backend {
    /// Create a new backend instance
    pub fn new(client: Client) -> Self {
        let config = Config::default();
        let documents = Arc::new(DocumentManager::new().expect("Failed to create document manager"));

        let analyzer = Arc::new(RwLock::new(Analyzer::new(config.clone(), (*documents).clone())));

        let workspace = Arc::new(RwLock::new(Workspace::new(None, config, (*documents).clone())));

        let features = Arc::new(Features {
            diagnostics: DiagnosticProvider::new((*documents).clone()),
            hover: HoverProvider::new((*documents).clone()),
            completion: CompletionProvider::new((*documents).clone()),
            goto_definition: GotoDefinitionProvider::new((*documents).clone()),
            references: ReferencesProvider::new((*documents).clone()),
            inlay_hints: InlayHintsProvider::new((*documents).clone()),
            code_actions: CodeActionsProvider::new((*documents).clone()),
            semantic_tokens: SemanticTokensProvider::new((*documents).clone()),
        });

        Self { client, documents, analyzer, workspace, features }
    }

    /// Publish diagnostics for a document
    async fn publish_diagnostics(&self, uri: Url) {
        let mut analyzer = self.analyzer.write().await;
        let diagnostics = self.features.diagnostics.generate_diagnostics(&uri, &mut analyzer);

        // Get document version
        let version = self.documents.get_document(&uri, |doc| doc.version);

        self.client.publish_diagnostics(uri, diagnostics, version).await;
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    /// Initialize the language server
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        // TODO: Parse initialization options and update config
        // TODO: Initialize workspace with root_uri

        let root_uri = params.root_uri;
        let mut workspace = self.workspace.write().await;
        workspace.root_uri = root_uri;

        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::INCREMENTAL)),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec![".".to_string()]),
                    ..Default::default()
                }),
                definition_provider: Some(OneOf::Left(true)),
                type_definition_provider: Some(TypeDefinitionProviderCapability::Simple(true)),
                references_provider: Some(OneOf::Left(true)),
                document_highlight_provider: Some(OneOf::Left(true)),
                code_action_provider: Some(CodeActionProviderCapability::Simple(true)),
                inlay_hint_provider: Some(OneOf::Right(InlayHintServerCapabilities::Options(InlayHintOptions {
                    work_done_progress_options: WorkDoneProgressOptions::default(),
                    resolve_provider: Some(false),
                }))),
                semantic_tokens_provider: Some(SemanticTokensServerCapabilities::SemanticTokensOptions(
                    SemanticTokensOptions {
                        legend: SemanticTokensLegend {
                            token_types: SUPPORTED_TYPES.to_vec(),
                            token_modifiers: SUPPORTED_MODIFIERS.to_vec(),
                        },
                        full: Some(SemanticTokensFullOptions::Bool(true)),
                        range: Some(true),
                        work_done_progress_options: WorkDoneProgressOptions::default(),
                    },
                )),
                // TODO: Add more capabilities as features are implemented
                ..Default::default()
            },
            server_info: Some(ServerInfo {
                name: "beacon-lsp".to_string(),
                version: Some(env!("CARGO_PKG_VERSION").to_string()),
            }),
        })
    }

    async fn initialized(&self, _params: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "Beacon LSP server initialized")
            .await;

        // TODO: Initialize workspace scanning
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let url = params.text_document.uri.clone();

        let version = params.text_document.version;
        let text = params.text_document.text;

        if let Err(e) = self.documents.open_document(url.clone(), version, text) {
            self.client
                .log_message(MessageType::ERROR, format!("Failed to open document: {}", e))
                .await;
            return;
        }

        self.publish_diagnostics(url).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri.clone();
        // let version = params.text_document.version;

        if let Err(e) = self
            .documents
            .update_document(params.text_document, params.content_changes)
        {
            self.client
                .log_message(MessageType::ERROR, format!("Failed to update document: {}", e))
                .await;
            return;
        }

        let mut analyzer = self.analyzer.write().await;
        analyzer.invalidate(&uri);

        drop(analyzer);
        self.publish_diagnostics(uri).await;
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri;
        self.documents.close_document(&uri);

        self.client.publish_diagnostics(uri, Vec::new(), None).await;
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        let uri = params.text_document.uri;

        self.publish_diagnostics(uri).await;
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let mut analyzer = self.analyzer.write().await;
        Ok(self.features.hover.hover(params, &mut analyzer))
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        Ok(self.features.completion.completion(params))
    }

    async fn goto_definition(&self, params: GotoDefinitionParams) -> Result<Option<GotoDefinitionResponse>> {
        Ok(self.features.goto_definition.goto_definition(params))
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        Ok(Some(self.features.references.find_references(params)))
    }

    async fn inlay_hint(&self, params: InlayHintParams) -> Result<Option<Vec<InlayHint>>> {
        let mut analyzer = self.analyzer.write().await;
        Ok(Some(self.features.inlay_hints.inlay_hints(params, &mut analyzer)))
    }

    async fn code_action(&self, params: CodeActionParams) -> Result<Option<CodeActionResponse>> {
        Ok(Some(self.features.code_actions.code_actions(params)))
    }

    async fn semantic_tokens_full(&self, params: SemanticTokensParams) -> Result<Option<SemanticTokensResult>> {
        Ok(self.features.semantic_tokens.semantic_tokens_full(params))
    }

    async fn semantic_tokens_range(
        &self, params: SemanticTokensRangeParams,
    ) -> Result<Option<SemanticTokensRangeResult>> {
        Ok(self.features.semantic_tokens.semantic_tokens_range(params))
    }

    // TODO: Implement additional LSP methods:
    // - formatting
    // - range_formatting
    // - on_type_formatting
    // - rename
    // - document_symbol
    // - workspace_symbol
    // - execute_command
}

#[cfg(test)]
mod tests {
    use super::*;
    use tower_lsp::LspService;

    #[test]
    fn test_backend_creation() {
        let (service, _socket) = LspService::new(Backend::new);
        let _backend = service;
    }
}

//! LSP backend implementation using Tower-LSP
//!
//! Implements the Language Server Protocol for Beacon type checker.
//! Handles all LSP requests and routes them to appropriate feature providers.

use crate::config::Config;
use crate::document::DocumentManager;
use crate::workspace::Workspace;
use crate::{analysis, cache, features::*, interpreter};

use lsp_types::{
    CodeActionProviderCapability, CompletionOptions, HoverProviderCapability, InitializeParams, InitializeResult,
    InlayHintServerCapabilities, OneOf, SemanticTokensFullOptions, SemanticTokensLegend, SemanticTokensOptions,
    ServerCapabilities, ServerInfo, SignatureHelpOptions, TextDocumentSyncCapability, TextDocumentSyncKind,
    WorkDoneProgressOptions,
};
use rustc_hash::FxHashSet;
use std::sync::Arc;
use tokio::sync::RwLock;
use tower_lsp::{Client, LanguageServer, jsonrpc::Result, lsp_types::*};

/// LSP Backend implements the LSP LanguageServer trait and coordinates all features.
/// TODO: Implement additional LSP methods:
/// - on_type_formatting
/// - execute_command
pub struct Backend {
    /// LSP client for sending notifications and requests
    client: Client,
    /// Document manager (shared across threads)
    documents: Arc<DocumentManager>,
    /// Type analyzer (shared and mutable)
    analyzer: Arc<RwLock<analysis::Analyzer>>,
    /// Workspace manager
    workspace: Arc<RwLock<Workspace>>,
    /// Feature providers
    features: Arc<Features>,
    /// Python interpreter path for runtime introspection
    /// TODO: Make configurable via LSP settings
    #[allow(dead_code)]
    interpreter_path: Option<std::path::PathBuf>,
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
    document_symbols: DocumentSymbolsProvider,
    document_highlight: DocumentHighlightProvider,
    rename: RenameProvider,
    workspace_symbols: WorkspaceSymbolsProvider,
    folding_range: FoldingRangeProvider,
    signature_help: SignatureHelpProvider,
    formatting: FormattingProvider,
}

impl Features {
    fn new(
        documents: DocumentManager, interpreter_path: Option<std::path::PathBuf>,
        introspection_cache: cache::IntrospectionCache, workspace: Arc<RwLock<Workspace>>,
        analyzer: Arc<RwLock<analysis::Analyzer>>,
    ) -> Self {
        Self {
            diagnostics: DiagnosticProvider::new(documents.clone(), workspace.clone()),
            hover: HoverProvider::with_introspection(documents.clone(), interpreter_path, introspection_cache),
            completion: CompletionProvider::new(documents.clone(), workspace.clone(), analyzer.clone()),
            goto_definition: GotoDefinitionProvider::new(documents.clone(), workspace.clone()),
            references: ReferencesProvider::new(documents.clone(), workspace.clone()),
            inlay_hints: InlayHintsProvider::new(documents.clone()),
            code_actions: CodeActionsProvider::new(documents.clone(), analyzer.clone()),
            semantic_tokens: SemanticTokensProvider::new(documents.clone()),
            document_symbols: DocumentSymbolsProvider::new(documents.clone()),
            document_highlight: DocumentHighlightProvider::new(documents.clone()),
            rename: RenameProvider::new(documents.clone()),
            workspace_symbols: WorkspaceSymbolsProvider::new(documents.clone(), workspace),
            folding_range: FoldingRangeProvider::new(documents.clone()),
            signature_help: SignatureHelpProvider::new(documents.clone()),
            formatting: FormattingProvider::new(documents),
        }
    }
}

/// Shared state that persists across LSP connections (for TCP mode)
pub struct SharedState {
    documents: Arc<DocumentManager>,
    analyzer: Arc<RwLock<analysis::Analyzer>>,
    workspace: Arc<RwLock<Workspace>>,
    features: Arc<Features>,
    interpreter_path: Option<std::path::PathBuf>,
}

impl Default for SharedState {
    fn default() -> Self {
        Self::new()
    }
}

impl SharedState {
    /// Create new shared state for TCP mode where state persists across reconnections
    pub fn new() -> Self {
        let config = Config::default();
        let documents = Arc::new(DocumentManager::new().expect("Failed to create document manager"));
        let workspace = Arc::new(RwLock::new(Workspace::new(None, config.clone(), (*documents).clone())));

        let analyzer = Arc::new(RwLock::new(analysis::Analyzer::with_workspace(
            config,
            (*documents).clone(),
            workspace.clone(),
        )));

        let interpreter_path = interpreter::find_python_interpreter(None);
        let introspection_cache = cache::IntrospectionCache::new(None);

        let features = Arc::new(Features::new(
            (*documents).clone(),
            interpreter_path.clone(),
            introspection_cache,
            workspace.clone(),
            analyzer.clone(),
        ));

        Self { documents, analyzer, workspace, features, interpreter_path }
    }
}

impl Backend {
    /// Create a new Backend with fresh state (stdio mode)
    pub fn new(client: Client) -> Self {
        let config = Config::default();
        let documents = Arc::new(DocumentManager::new().expect("Failed to create document manager"));
        let workspace = Arc::new(RwLock::new(Workspace::new(None, config.clone(), (*documents).clone())));

        let analyzer = Arc::new(RwLock::new(analysis::Analyzer::with_workspace(
            config,
            (*documents).clone(),
            workspace.clone(),
        )));

        let interpreter_path = interpreter::find_python_interpreter(None);
        let introspection_cache = cache::IntrospectionCache::new(None);

        let features = Arc::new(Features::new(
            (*documents).clone(),
            interpreter_path.clone(),
            introspection_cache,
            workspace.clone(),
            analyzer.clone(),
        ));

        Self { client, documents, analyzer, workspace, features, interpreter_path }
    }

    /// Create a new Backend using shared state (TCP mode with persistent state)
    pub fn with_shared_state(client: Client, shared: &SharedState) -> Self {
        Self {
            client,
            documents: shared.documents.clone(),
            analyzer: shared.analyzer.clone(),
            workspace: shared.workspace.clone(),
            features: shared.features.clone(),
            interpreter_path: shared.interpreter_path.clone(),
        }
    }

    /// Publish diagnostics for a document
    async fn publish_diagnostics(&self, uri: Url) {
        let mut analyzer = self.analyzer.write().await;
        let diagnostics = self.features.diagnostics.generate_diagnostics(&uri, &mut analyzer);
        let version = self.documents.get_document(&uri, |doc| doc.version);

        self.client.publish_diagnostics(uri, diagnostics, version).await;
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    /// Initialize the language server
    ///
    /// Loads configuration from TOML files (beacon.toml or pyproject.toml) and merges with LSP initialization options.
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        tracing::info!("Received initialize request");
        tracing::debug!(?params.root_uri, "Initialize params");

        let root_uri = params.root_uri.clone();
        let mut config = if let Some(ref uri) = root_uri {
            if let Ok(path) = uri.to_file_path() {
                tracing::debug!(?path, "Discovering configuration");
                match Config::discover_and_load(&path) {
                    Ok(cfg) => {
                        tracing::info!("Configuration loaded from workspace");
                        self.client
                            .log_message(MessageType::INFO, "Configuration loaded from workspace")
                            .await;
                        cfg
                    }
                    Err(e) => {
                        tracing::warn!(?e, "Failed to load configuration, using defaults");
                        self.client
                            .log_message(MessageType::WARNING, format!("Failed to load configuration: {e}"))
                            .await;
                        Config::default()
                    }
                }
            } else {
                tracing::debug!("Root URI is not a file path, using default config");
                Config::default()
            }
        } else {
            tracing::debug!("No root URI provided, using default config");
            Config::default()
        };

        if let Some(init_options) = params.initialization_options {
            tracing::debug!("Applying LSP initialization options");
            config.update_from_value(init_options);
            self.client
                .log_message(MessageType::INFO, "LSP configuration options applied")
                .await;
        }

        if let Err(e) = config.validate() {
            tracing::warn!(?e, "Configuration validation warning");
            self.client
                .log_message(MessageType::WARNING, format!("Configuration validation warning: {e}"))
                .await;
        }

        let mut workspace = self.workspace.write().await;
        workspace.root_uri = root_uri;
        workspace.config = config.clone();

        let mut analyzer = self.analyzer.write().await;
        analyzer.update_config(config);

        drop(workspace);
        drop(analyzer);

        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::INCREMENTAL)),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec![".".to_string()]),
                    ..Default::default()
                }),
                signature_help_provider: Some(SignatureHelpOptions {
                    trigger_characters: Some(vec!["(".to_string(), ",".to_string()]),
                    retrigger_characters: None,
                    work_done_progress_options: WorkDoneProgressOptions::default(),
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
                document_symbol_provider: Some(OneOf::Left(true)),
                folding_range_provider: Some(FoldingRangeProviderCapability::Simple(true)),
                document_formatting_provider: Some(OneOf::Left(true)),
                document_range_formatting_provider: Some(OneOf::Left(true)),
                document_on_type_formatting_provider: Some(DocumentOnTypeFormattingOptions {
                    first_trigger_character: ":".to_string(),
                    more_trigger_character: Some(vec!["\n".to_string()]),
                }),
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

        let mut workspace = self.workspace.write().await;
        let client = self.client.clone();

        match workspace.initialize() {
            Ok(()) => {
                client
                    .log_message(MessageType::INFO, "Workspace indexing completed successfully")
                    .await;
            }
            Err(e) => {
                client
                    .log_message(MessageType::ERROR, format!("Workspace indexing failed: {e}"))
                    .await;
            }
        }
    }

    async fn shutdown(&self) -> Result<()> {
        tracing::info!("Shutdown request received");
        Ok(())
    }

    async fn did_change_configuration(&self, params: DidChangeConfigurationParams) {
        tracing::info!("Configuration change notification received");

        if let Some(settings) = params.settings.as_object() {
            if let Some(beacon_settings) = settings.get("beacon") {
                tracing::debug!("Updating configuration from LSP settings");
                let mut workspace = self.workspace.write().await;
                let mut config = workspace.config.clone();

                config.update_from_value(beacon_settings.clone());

                if let Err(e) = config.validate() {
                    tracing::warn!(?e, "Configuration validation warning");
                    self.client
                        .log_message(MessageType::WARNING, format!("Configuration validation warning: {e}"))
                        .await;
                }

                workspace.config = config.clone();
                drop(workspace);

                let mut analyzer = self.analyzer.write().await;
                analyzer.update_config(config);
                drop(analyzer);

                tracing::info!("Configuration updated successfully");
                self.client
                    .log_message(MessageType::INFO, "Configuration updated successfully")
                    .await;
            }
        }
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let url = params.text_document.uri.clone();
        let version = params.text_document.version;
        let text = params.text_document.text;

        tracing::info!(uri = %url, version, "Document opened");
        tracing::trace!(uri = %url, text_length = text.len(), "Document content");

        if let Err(e) = self.documents.open_document(url.clone(), version, text) {
            tracing::error!(uri = %url, ?e, "Failed to open document");
            self.client
                .log_message(MessageType::ERROR, format!("Failed to open document: {e}"))
                .await;
            return;
        }

        let mut workspace = self.workspace.write().await;
        workspace.update_dependencies(&url);

        let symbol_imports = workspace.get_symbol_imports(&url);
        let mut resolved_imports = Vec::new();
        for import in &symbol_imports {
            if let Some(resolved_uri) = workspace.resolve_import(&import.from_module) {
                if import.symbol == "*" {
                    let symbols = workspace.resolve_star_import(&resolved_uri);
                    tracing::debug!(
                        from_module = %import.from_module,
                        resolved_uri = %resolved_uri,
                        symbol_count = symbols.len(),
                        "Expanding star import"
                    );
                    for symbol in symbols {
                        resolved_imports.push((resolved_uri.clone(), symbol));
                    }
                } else {
                    resolved_imports.push((resolved_uri, import.symbol.clone()));
                }
            }
        }
        drop(workspace);

        if !resolved_imports.is_empty() {
            let mut analyzer = self.analyzer.write().await;
            analyzer.record_imports(&url, &resolved_imports);
            drop(analyzer);
        }

        tracing::debug!(uri = %url, "Publishing diagnostics for opened document");
        self.publish_diagnostics(url).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri.clone();
        let version = params.text_document.version;
        tracing::debug!(uri = %uri, version, "Document changed");

        match self
            .documents
            .update_document(params.text_document, params.content_changes)
        {
            Ok(_) => {
                tracing::trace!(uri = %uri, "Document updated, performing selective invalidation");
                let mut analyzer = self.analyzer.write().await;

                let changed_scopes = analyzer.invalidate_selective(&uri);

                let mut workspace = self.workspace.write().await;
                workspace.update_dependencies(&uri);

                let graph_dependents = workspace.invalidate_dependents(&uri);

                let symbol_imports = workspace.get_symbol_imports(&uri);
                let mut resolved_imports = Vec::new();
                for import in &symbol_imports {
                    if let Some(resolved_uri) = workspace.resolve_import(&import.from_module) {
                        if import.symbol == "*" {
                            let symbols = workspace.resolve_star_import(&resolved_uri);
                            for symbol in symbols {
                                resolved_imports.push((resolved_uri.clone(), symbol));
                            }
                        } else {
                            resolved_imports.push((resolved_uri, import.symbol.clone()));
                        }
                    }
                }
                if !resolved_imports.is_empty() {
                    analyzer.record_imports(&uri, &resolved_imports);
                }

                let mut import_dependents = FxHashSet::default();
                if !changed_scopes.is_empty() {
                    let affected_exports = analyzer.get_affected_exports(&uri, &changed_scopes);

                    if !affected_exports.is_empty() {
                        let importers_map = analyzer.get_importers_by_symbol(&uri, &affected_exports);
                        for importers in importers_map.values() {
                            for importer in importers {
                                import_dependents.insert(importer.clone());
                            }
                        }

                        if !import_dependents.is_empty() {
                            tracing::info!(
                                uri = %uri,
                                affected_symbols = affected_exports.len(),
                                import_dependent_count = import_dependents.len(),
                                "Cascading invalidation: {} affected exports, {} importing files",
                                affected_exports.len(),
                                import_dependents.len()
                            );
                        }
                    }
                }

                let mut all_dependents = graph_dependents;
                all_dependents.extend(import_dependents);

                let reanalyzed_count = workspace.reanalyze_affected(&all_dependents, &mut analyzer);

                tracing::info!(
                    uri = %uri,
                    changed_scopes = changed_scopes.len(),
                    reanalyzed_count,
                    "Selective invalidation complete: {} scopes changed, {} modules reanalyzed",
                    changed_scopes.len(),
                    reanalyzed_count
                );
                self.client
                    .log_message(
                        MessageType::INFO,
                        format!(
                            "Reanalyzed {} module(s) after change to {} ({} scopes changed)",
                            reanalyzed_count,
                            uri.path(),
                            changed_scopes.len()
                        ),
                    )
                    .await;

                drop(workspace);
                drop(analyzer);

                self.publish_diagnostics(uri).await;
            }
            Err(e) => {
                tracing::error!(uri = %uri, ?e, "Failed to update document");
                self.client
                    .log_message(MessageType::ERROR, format!("Failed to update document: {e}"))
                    .await;
            }
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        tracing::info!(uri = %params.text_document.uri, "Document closed");
        let uri = params.text_document.uri;
        self.documents.close_document(&uri);
        self.client.publish_diagnostics(uri, Vec::new(), None).await;
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        self.publish_diagnostics(params.text_document.uri).await;
    }

    #[tracing::instrument(skip(self), level = "debug")]
    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let mut analyzer = self.analyzer.write().await;
        Ok(self.features.hover.hover(params, &mut analyzer))
    }

    #[tracing::instrument(skip(self), level = "debug")]
    async fn signature_help(&self, params: SignatureHelpParams) -> Result<Option<SignatureHelp>> {
        let mut analyzer = self.analyzer.write().await;
        Ok(self.features.signature_help.signature_help(params, &mut analyzer))
    }

    #[tracing::instrument(skip(self), level = "debug")]
    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        Ok(self.features.completion.completion(params).await)
    }

    #[tracing::instrument(skip(self), level = "debug")]
    async fn goto_definition(&self, params: GotoDefinitionParams) -> Result<Option<GotoDefinitionResponse>> {
        Ok(self.features.goto_definition.goto_definition(params))
    }

    #[tracing::instrument(skip(self), level = "debug")]
    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        Ok(Some(self.features.references.find_references(params).await))
    }

    #[tracing::instrument(skip(self), level = "debug")]
    async fn inlay_hint(&self, params: InlayHintParams) -> Result<Option<Vec<InlayHint>>> {
        let mut analyzer = self.analyzer.write().await;
        let workspace = self.workspace.read().await;
        let config = &workspace.config.inlay_hints;
        Ok(Some(self.features.inlay_hints.inlay_hints(
            params,
            &mut analyzer,
            config,
        )))
    }

    #[tracing::instrument(skip(self), level = "debug")]
    async fn code_action(&self, params: CodeActionParams) -> Result<Option<CodeActionResponse>> {
        Ok(Some(self.features.code_actions.code_actions(params).await))
    }

    #[tracing::instrument(skip(self), level = "debug")]
    async fn semantic_tokens_full(&self, params: SemanticTokensParams) -> Result<Option<SemanticTokensResult>> {
        Ok(self.features.semantic_tokens.semantic_tokens_full(params))
    }

    #[tracing::instrument(skip(self), level = "debug")]
    async fn semantic_tokens_range(
        &self, params: SemanticTokensRangeParams,
    ) -> Result<Option<SemanticTokensRangeResult>> {
        Ok(self.features.semantic_tokens.semantic_tokens_range(params))
    }

    #[tracing::instrument(skip(self), level = "debug")]
    async fn document_symbol(&self, params: DocumentSymbolParams) -> Result<Option<DocumentSymbolResponse>> {
        Ok(self.features.document_symbols.document_symbols(params))
    }

    #[tracing::instrument(skip(self), level = "debug")]
    async fn document_highlight(&self, params: DocumentHighlightParams) -> Result<Option<Vec<DocumentHighlight>>> {
        Ok(self.features.document_highlight.document_highlight(params))
    }

    #[tracing::instrument(skip(self), level = "debug")]
    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        Ok(self.features.rename.rename(params))
    }

    #[tracing::instrument(skip(self), level = "debug")]
    async fn symbol(&self, params: WorkspaceSymbolParams) -> Result<Option<Vec<SymbolInformation>>> {
        Ok(self.features.workspace_symbols.workspace_symbol(params))
    }

    async fn symbol_resolve(&self, params: WorkspaceSymbol) -> Result<WorkspaceSymbol> {
        Ok(self.features.workspace_symbols.symbol_resolve(params))
    }

    #[tracing::instrument(skip(self), level = "debug")]
    async fn folding_range(&self, params: FoldingRangeParams) -> Result<Option<Vec<FoldingRange>>> {
        Ok(self.features.folding_range.folding_range(params))
    }

    #[tracing::instrument(skip(self), level = "debug")]
    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        let workspace = self.workspace.read().await;
        let config = &workspace.config.formatting;
        Ok(self.features.formatting.format_document(&params, config))
    }

    #[tracing::instrument(skip(self), level = "debug")]
    async fn range_formatting(&self, params: DocumentRangeFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        let workspace = self.workspace.read().await;
        let config = &workspace.config.formatting;
        Ok(self.features.formatting.format_range(&params, config))
    }

    #[tracing::instrument(skip(self), level = "debug")]
    async fn on_type_formatting(&self, params: DocumentOnTypeFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        let workspace = self.workspace.read().await;
        let config = &workspace.config.formatting;
        Ok(self.features.formatting.on_type_format(&params, config))
    }

    async fn will_save_wait_until(&self, params: WillSaveTextDocumentParams) -> Result<Option<Vec<TextEdit>>> {
        let workspace = self.workspace.read().await;
        let config = &workspace.config.formatting;
        Ok(self.features.formatting.format_on_save(&params, config))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::str::FromStr;
    use tower_lsp::LspService;

    fn create_backend() -> LspService<Backend> {
        let (service, _socket) = LspService::new(Backend::new);
        service
    }

    #[test]
    fn test_backend_creation() {
        let (service, _socket) = LspService::new(Backend::new);
        let _backend = service;
    }

    #[test]
    fn test_features_creation() {
        let service = create_backend();
        let backend = service.inner();

        let _ = &backend.features.diagnostics;
        let _ = &backend.features.hover;
        let _ = &backend.features.completion;
        let _ = &backend.features.goto_definition;
        let _ = &backend.features.references;
        let _ = &backend.features.inlay_hints;
        let _ = &backend.features.code_actions;
        let _ = &backend.features.semantic_tokens;
        let _ = &backend.features.document_symbols;
        let _ = &backend.features.document_highlight;
        let _ = &backend.features.rename;
        let _ = &backend.features.workspace_symbols;
        let _ = &backend.features.folding_range;
        let _ = &backend.features.signature_help;
    }

    #[tokio::test]
    async fn test_initialize() {
        let service = create_backend();
        let backend = service.inner();
        let params = InitializeParams {
            root_uri: Some(Url::from_str("file:///test").unwrap()),
            capabilities: ClientCapabilities::default(),
            ..Default::default()
        };

        let result = backend.initialize(params).await;
        assert!(result.is_ok());

        let init_result = result.unwrap();
        assert!(init_result.capabilities.text_document_sync.is_some());
        assert!(init_result.capabilities.hover_provider.is_some());
        assert!(init_result.capabilities.completion_provider.is_some());
        assert!(init_result.capabilities.signature_help_provider.is_some());
        assert!(init_result.capabilities.definition_provider.is_some());
        assert!(init_result.capabilities.references_provider.is_some());
        assert!(init_result.capabilities.document_highlight_provider.is_some());
        assert!(init_result.capabilities.code_action_provider.is_some());
        assert!(init_result.capabilities.inlay_hint_provider.is_some());
        assert!(init_result.capabilities.semantic_tokens_provider.is_some());
        assert!(init_result.capabilities.document_symbol_provider.is_some());
        assert!(init_result.server_info.is_some());
    }

    #[tokio::test]
    async fn test_initialize_without_root_uri() {
        let service = create_backend();
        let backend = service.inner();
        let params =
            InitializeParams { root_uri: None, capabilities: ClientCapabilities::default(), ..Default::default() };

        let result = backend.initialize(params).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_initialized() {
        let service = create_backend();
        let backend = service.inner();
        let params = InitializedParams {};
        let _ = backend.initialized(params).await;
    }

    #[tokio::test]
    async fn test_shutdown() {
        let service = create_backend();
        let backend = service.inner();
        let result = backend.shutdown().await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_did_open() {
        let service = create_backend();
        let backend = service.inner();
        let uri = Url::from_str("file:///test.py").unwrap();

        let params = DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri: uri.clone(),
                language_id: "python".to_string(),
                version: 1,
                text: "x = 42".to_string(),
            },
        };

        backend.did_open(params).await;

        assert!(backend.documents.has_document(&uri));
    }

    #[tokio::test]
    async fn test_did_change() {
        let service = create_backend();
        let backend = service.inner();
        let uri = Url::from_str("file:///test.py").unwrap();

        backend
            .documents
            .open_document(uri.clone(), 1, "x = 42".to_string())
            .unwrap();

        let params = DidChangeTextDocumentParams {
            text_document: VersionedTextDocumentIdentifier { uri: uri.clone(), version: 2 },
            content_changes: vec![TextDocumentContentChangeEvent {
                range: None,
                range_length: None,
                text: "y = 100".to_string(),
            }],
        };

        backend.did_change(params).await;

        let text = backend.documents.get_document(&uri, |doc| doc.text());
        assert_eq!(text, Some("y = 100".to_string()));
    }

    #[tokio::test]
    async fn test_did_close() {
        let service = create_backend();
        let backend = service.inner();
        let uri = Url::from_str("file:///test.py").unwrap();

        backend
            .documents
            .open_document(uri.clone(), 1, "x = 42".to_string())
            .unwrap();

        let params = DidCloseTextDocumentParams { text_document: TextDocumentIdentifier { uri: uri.clone() } };

        backend.did_close(params).await;

        assert!(!backend.documents.has_document(&uri));
    }

    #[tokio::test]
    async fn test_did_save() {
        let service = create_backend();
        let backend = service.inner();
        let uri = Url::from_str("file:///test.py").unwrap();

        backend
            .documents
            .open_document(uri.clone(), 1, "x = 42".to_string())
            .unwrap();

        let params =
            DidSaveTextDocumentParams { text_document: TextDocumentIdentifier { uri: uri.clone() }, text: None };

        let _ = backend.did_save(params).await;
    }

    #[tokio::test]
    async fn test_hover() {
        let service = create_backend();
        let backend = service.inner();
        let uri = Url::from_str("file:///test.py").unwrap();

        backend
            .documents
            .open_document(uri.clone(), 1, "x = 42".to_string())
            .unwrap();

        let params = HoverParams {
            text_document_position_params: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri },
                position: Position { line: 0, character: 0 },
            },
            work_done_progress_params: WorkDoneProgressParams::default(),
        };

        let result = backend.hover(params).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_signature_help() {
        let service = create_backend();
        let backend = service.inner();
        let uri = Url::from_str("file:///test.py").unwrap();

        backend
            .documents
            .open_document(
                uri.clone(),
                1,
                "def greet(name: str) -> str:\n    return f\"Hello {name}\"\n\ngreet(".to_string(),
            )
            .unwrap();

        let params = SignatureHelpParams {
            text_document_position_params: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri },
                position: Position { line: 3, character: 6 },
            },
            work_done_progress_params: WorkDoneProgressParams::default(),
            context: None,
        };

        let result = backend.signature_help(params).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_completion() {
        let service = create_backend();
        let backend = service.inner();
        let uri = Url::from_str("file:///test.py").unwrap();

        backend
            .documents
            .open_document(uri.clone(), 1, "x = 42\ny = ".to_string())
            .unwrap();

        let params = CompletionParams {
            text_document_position: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri },
                position: Position { line: 1, character: 4 },
            },
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
            context: None,
        };

        let result = backend.completion(params).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_goto_definition() {
        let service = create_backend();
        let backend = service.inner();
        let uri = Url::from_str("file:///test.py").unwrap();

        backend
            .documents
            .open_document(uri.clone(), 1, "def hello():\n    pass".to_string())
            .unwrap();

        let params = GotoDefinitionParams {
            text_document_position_params: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri },
                position: Position { line: 0, character: 4 },
            },
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        };

        let result = backend.goto_definition(params).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_references() {
        let service = create_backend();
        let backend = service.inner();
        let uri = Url::from_str("file:///test.py").unwrap();

        backend
            .documents
            .open_document(uri.clone(), 1, "x = 42\ny = x".to_string())
            .unwrap();

        let params = ReferenceParams {
            text_document_position: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri },
                position: Position { line: 0, character: 0 },
            },
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
            context: ReferenceContext { include_declaration: true },
        };

        let result = backend.references(params).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_inlay_hint() {
        let service = create_backend();
        let backend = service.inner();
        let uri = Url::from_str("file:///test.py").unwrap();

        backend
            .documents
            .open_document(uri.clone(), 1, "x = 42".to_string())
            .unwrap();

        let params = InlayHintParams {
            text_document: TextDocumentIdentifier { uri },
            range: Range { start: Position { line: 0, character: 0 }, end: Position { line: 1, character: 0 } },
            work_done_progress_params: WorkDoneProgressParams::default(),
        };

        let result = backend.inlay_hint(params).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_code_action() {
        let service = create_backend();
        let backend = service.inner();
        let uri = Url::from_str("file:///test.py").unwrap();

        backend
            .documents
            .open_document(uri.clone(), 1, "x = 42".to_string())
            .unwrap();

        let params = CodeActionParams {
            text_document: TextDocumentIdentifier { uri },
            range: Range { start: Position { line: 0, character: 0 }, end: Position { line: 0, character: 6 } },
            context: CodeActionContext { diagnostics: vec![], only: None, trigger_kind: None },
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        };

        let result = backend.code_action(params).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_semantic_tokens_full() {
        let service = create_backend();
        let backend = service.inner();
        let uri = Url::from_str("file:///test.py").unwrap();

        backend
            .documents
            .open_document(uri.clone(), 1, "x = 42".to_string())
            .unwrap();

        let params = SemanticTokensParams {
            text_document: TextDocumentIdentifier { uri },
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        };

        let result = backend.semantic_tokens_full(params).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_semantic_tokens_range() {
        let service = create_backend();
        let backend = service.inner();
        let uri = Url::from_str("file:///test.py").unwrap();

        backend
            .documents
            .open_document(uri.clone(), 1, "x = 42".to_string())
            .unwrap();

        let params = SemanticTokensRangeParams {
            text_document: TextDocumentIdentifier { uri },
            range: Range { start: Position { line: 0, character: 0 }, end: Position { line: 1, character: 0 } },
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        };

        let result = backend.semantic_tokens_range(params).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_document_symbol() {
        let service = create_backend();
        let backend = service.inner();
        let uri = Url::from_str("file:///test.py").unwrap();

        backend
            .documents
            .open_document(uri.clone(), 1, "def hello():\n    pass".to_string())
            .unwrap();

        let params = DocumentSymbolParams {
            text_document: TextDocumentIdentifier { uri },
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        };

        let result = backend.document_symbol(params).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_document_highlight() {
        let service = create_backend();
        let backend = service.inner();
        let uri = Url::from_str("file:///test.py").unwrap();

        backend
            .documents
            .open_document(uri.clone(), 1, "x = 42\ny = x".to_string())
            .unwrap();

        let params = DocumentHighlightParams {
            text_document_position_params: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri },
                position: Position { line: 0, character: 0 },
            },
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        };

        let result = backend.document_highlight(params).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_rename() {
        let service = create_backend();
        let backend = service.inner();
        let uri = Url::from_str("file:///test.py").unwrap();

        backend
            .documents
            .open_document(uri.clone(), 1, "x = 42\ny = x".to_string())
            .unwrap();

        let params = RenameParams {
            text_document_position: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri },
                position: Position { line: 0, character: 0 },
            },
            new_name: "renamed_var".to_string(),
            work_done_progress_params: WorkDoneProgressParams::default(),
        };

        let result = backend.rename(params).await;
        assert!(result.is_ok());
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn test_workspace_symbol() {
        let service = create_backend();
        let backend = service.inner();
        let uri = Url::from_str("file:///test.py").unwrap();

        backend
            .documents
            .open_document(uri, 1, "def hello():\n    pass".to_string())
            .unwrap();

        let params = WorkspaceSymbolParams {
            query: "hello".to_string(),
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        };

        let result = backend.symbol(params).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_symbol_resolve() {
        let service = create_backend();
        let backend = service.inner();
        let uri = Url::from_str("file:///test.py").unwrap();
        let location = Location {
            uri,
            range: Range { start: Position { line: 0, character: 0 }, end: Position { line: 0, character: 5 } },
        };

        let symbol = WorkspaceSymbol {
            name: "test".to_string(),
            kind: SymbolKind::FUNCTION,
            tags: None,
            location: OneOf::Left(location),
            container_name: None,
            data: None,
        };

        let result = backend.symbol_resolve(symbol).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_publish_diagnostics() {
        let service = create_backend();
        let backend = service.inner();
        let uri = Url::from_str("file:///test.py").unwrap();

        backend
            .documents
            .open_document(uri.clone(), 1, "x = undefined_var".to_string())
            .unwrap();

        let _ = backend.publish_diagnostics(uri).await;
    }

    #[tokio::test]
    async fn test_did_change_incremental() {
        let service = create_backend();
        let backend = service.inner();
        let uri = Url::from_str("file:///test.py").unwrap();

        backend
            .documents
            .open_document(uri.clone(), 1, "x = 42".to_string())
            .unwrap();

        let params = DidChangeTextDocumentParams {
            text_document: VersionedTextDocumentIdentifier { uri: uri.clone(), version: 2 },
            content_changes: vec![TextDocumentContentChangeEvent {
                range: Some(Range {
                    start: Position { line: 0, character: 4 },
                    end: Position { line: 0, character: 6 },
                }),
                range_length: Some(2),
                text: "100".to_string(),
            }],
        };

        backend.did_change(params).await;

        let text = backend.documents.get_document(&uri, |doc| doc.text());
        assert_eq!(text, Some("x = 100".to_string()));
    }

    #[tokio::test]
    async fn test_multiple_documents() {
        let service = create_backend();
        let backend = service.inner();
        let uri1 = Url::from_str("file:///test1.py").unwrap();
        let uri2 = Url::from_str("file:///test2.py").unwrap();

        backend
            .documents
            .open_document(uri1.clone(), 1, "x = 1".to_string())
            .unwrap();
        backend
            .documents
            .open_document(uri2.clone(), 1, "y = 2".to_string())
            .unwrap();

        assert!(backend.documents.has_document(&uri1));
        assert!(backend.documents.has_document(&uri2));

        backend.documents.close_document(&uri1);
        assert!(!backend.documents.has_document(&uri1));
        assert!(backend.documents.has_document(&uri2));
    }

    #[tokio::test]
    async fn test_server_info() {
        let service = create_backend();
        let backend = service.inner();
        let params = InitializeParams::default();
        let result = backend.initialize(params).await.unwrap();
        let server_info = result.server_info.unwrap();

        assert_eq!(server_info.name, "beacon-lsp");
        assert!(server_info.version.is_some());
    }

    #[tokio::test]
    async fn test_text_document_sync_capability() {
        let service = create_backend();
        let backend = service.inner();
        let params = InitializeParams::default();
        let result = backend.initialize(params).await.unwrap();
        let sync = result.capabilities.text_document_sync.unwrap();

        match sync {
            TextDocumentSyncCapability::Kind(kind) => {
                assert_eq!(kind, TextDocumentSyncKind::INCREMENTAL);
            }
            _ => panic!("Expected TextDocumentSyncKind"),
        }
    }

    #[tokio::test]
    async fn test_completion_trigger_characters() {
        let service = create_backend();
        let backend = service.inner();
        let params = InitializeParams::default();
        let result = backend.initialize(params).await.unwrap();
        let completion = result.capabilities.completion_provider.unwrap();

        assert!(completion.trigger_characters.is_some());
        assert!(completion.trigger_characters.unwrap().contains(&".".to_string()));
    }

    #[tokio::test]
    async fn test_signature_help_trigger_characters() {
        let service = create_backend();
        let backend = service.inner();
        let params = InitializeParams::default();
        let result = backend.initialize(params).await.unwrap();
        let signature_help = result.capabilities.signature_help_provider.unwrap();

        assert!(signature_help.trigger_characters.is_some());
        let triggers = signature_help.trigger_characters.unwrap();
        assert!(triggers.contains(&"(".to_string()));
        assert!(triggers.contains(&",".to_string()));
    }

    #[tokio::test]
    async fn test_inlay_hint_capability() {
        let service = create_backend();
        let backend = service.inner();
        let params = InitializeParams::default();
        let result = backend.initialize(params).await.unwrap();
        assert!(result.capabilities.inlay_hint_provider.is_some());
    }

    #[tokio::test]
    async fn test_semantic_tokens_legend() {
        let service = create_backend();
        let backend = service.inner();
        let params = InitializeParams::default();
        let result = backend.initialize(params).await.unwrap();

        match result.capabilities.semantic_tokens_provider {
            Some(SemanticTokensServerCapabilities::SemanticTokensOptions(options)) => {
                assert!(!options.legend.token_types.is_empty());
                assert!(!options.legend.token_modifiers.is_empty());
            }
            _ => panic!("Expected SemanticTokensOptions"),
        }
    }

    #[test]
    fn test_backend_has_all_features() {
        let service = create_backend();
        let backend = service.inner();

        let _ = &backend.features.diagnostics;
        let _ = &backend.features.hover;
        let _ = &backend.features.completion;
        let _ = &backend.features.goto_definition;
        let _ = &backend.features.references;
        let _ = &backend.features.inlay_hints;
        let _ = &backend.features.code_actions;
        let _ = &backend.features.semantic_tokens;
        let _ = &backend.features.document_symbols;
        let _ = &backend.features.document_highlight;
        let _ = &backend.features.rename;
        let _ = &backend.features.workspace_symbols;
        let _ = &backend.features.signature_help;
    }

    #[test]
    fn test_backend_components() {
        let service = create_backend();
        let backend = service.inner();

        let _ = &backend.client;
        let _ = &backend.documents;
        let _ = &backend.analyzer;
        let _ = &backend.workspace;
        let _ = &backend.features;
    }

    #[test]
    fn test_backend_has_formatting_feature() {
        let service = create_backend();
        let backend = service.inner();
        let _ = &backend.features.formatting;
    }

    #[tokio::test]
    async fn test_formatting_capability_advertised() {
        let service = create_backend();
        let backend = service.inner();
        let params = InitializeParams::default();
        let result = backend.initialize(params).await.unwrap();

        assert!(result.capabilities.document_formatting_provider.is_some());
        assert!(result.capabilities.document_range_formatting_provider.is_some());
    }

    #[test]
    fn test_shared_state_creation() {
        let shared = SharedState::new();

        assert!(Arc::strong_count(&shared.documents) > 0);
        assert!(Arc::strong_count(&shared.analyzer) > 0);
        assert!(Arc::strong_count(&shared.workspace) > 0);
        assert!(Arc::strong_count(&shared.features) > 0);
    }

    #[test]
    fn test_backend_with_shared_state() {
        let shared = SharedState::new();

        let (service1, _socket1) = LspService::new(|client| Backend::with_shared_state(client, &shared));
        let (service2, _socket2) = LspService::new(|client| Backend::with_shared_state(client, &shared));

        let backend1 = service1.inner();
        let backend2 = service2.inner();

        assert!(Arc::ptr_eq(&backend1.documents, &backend2.documents));
        assert!(Arc::ptr_eq(&backend1.analyzer, &backend2.analyzer));
        assert!(Arc::ptr_eq(&backend1.workspace, &backend2.workspace));
        assert!(Arc::ptr_eq(&backend1.features, &backend2.features));
    }

    #[test]
    fn test_shared_state_vs_fresh_state() {
        let shared = SharedState::new();

        let (service_shared, _) = LspService::new(|client| Backend::with_shared_state(client, &shared));
        let backend_shared = service_shared.inner();

        let (service_fresh, _) = LspService::new(Backend::new);
        let backend_fresh = service_fresh.inner();

        assert!(!Arc::ptr_eq(&backend_shared.documents, &backend_fresh.documents));
        assert!(!Arc::ptr_eq(&backend_shared.analyzer, &backend_fresh.analyzer));
        assert!(!Arc::ptr_eq(&backend_shared.workspace, &backend_fresh.workspace));
    }
}

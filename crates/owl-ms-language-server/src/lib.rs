mod catalog;
mod consts;
pub mod debugging;
mod error;
mod pos;
mod queries;
mod range;
pub mod rope_provider;
mod sync_backend;
#[cfg(test)]
#[allow(clippy::pedantic)]
mod test_helpers;
#[cfg(test)]
#[allow(clippy::pedantic)]
mod tests;
pub mod web;
mod workspace;

use debugging::timeit;
use error::{Error, ResultExt, ResultIterator};
use itertools::Itertools;
use log::{debug, error, info, warn};
use pos::Position;
use range::Range;
use std::collections::{HashMap, HashSet, LinkedList};
use std::path::Path;
use std::sync::{Arc, LazyLock};
use tokio::sync::{OnceCell, RwLock, RwLockReadGuard, RwLockWriteGuard};
use tokio::task::{self};
use tower_lsp::jsonrpc::Result;
// There are too many LSP types
#[allow(clippy::wildcard_imports)]
use tower_lsp::lsp_types::{self, *};
use tower_lsp::{Client, LanguageServer};
use tree_sitter_c2rust::Language;
use workspace::{node_text, trim_full_iri, Workspace};

use crate::sync_backend::SyncBackend;
use crate::web::HttpClient;
use crate::workspace::{Document, DocumentReference, FormattingSettings, InternalDocument};
// Constants

pub static LANGUAGE: LazyLock<Language> = LazyLock::new(|| tree_sitter_owl_ms::LANGUAGE.into());

// Model

#[derive(Clone)]
pub struct Backend {
    pub client: Client,
    pub http_client: Arc<dyn HttpClient>,
    position_encoding: OnceCell<PositionEncodingKind>,
    options: OnceCell<Options>,
    sync: SyncRef,
}

pub type SyncRef = Arc<RwLock<SyncBackend>>;

impl Backend {
    /// Creates a new [`Backend`] with a Ureq http client and UTF16 encoding.
    /// The workspaces are created empty.
    #[must_use]
    pub fn new(client: Client, http_client: Box<dyn HttpClient>) -> Self {
        Backend {
            client,
            http_client: http_client.into(),
            position_encoding: OnceCell::new(),
            sync: Arc::new(RwLock::new(SyncBackend::default())),
            options: OnceCell::new(),
        }
    }

    /// Take a document with the profided file url and generate diagnostics for it.
    /// Then do the same thing with documents that depend on this one.
    /// # Panics
    /// If an documents path is not convertable into an URL
    pub fn update_diagnostics_for_url_and_dependent(&self, file_url: Url) {
        let mini_backend = self.clone();

        task::spawn(async move {
            let encoding = mini_backend.encoding();
            let sync = mini_backend.sync.read().await;

            // So my error type is not send and terefore we need the conversion to ok()
            #[allow(clippy::match_result_ok)]
            if let Some((document, workspace)) = sync.get_internal_document(&file_url).ok() {
                document
                    .publish_lsp_diagnostics(workspace, encoding, &mini_backend.client)
                    .await;

                // Create diagnostics for files that depend on this file
                for other_internal_doc in workspace.internal_documents() {
                    let depends_on_me = other_internal_doc.reachable_urls(false).iter().any(|u| {
                        workspace.document_by_url(u) == Some(DocumentReference::Internal(document))
                    });

                    if depends_on_me {
                        mini_backend.update_diagnostics_for_url_and_dependent(
                            Url::from_file_path(&other_internal_doc.path)
                                .expect("Document path should be convertable into file url"),
                        );
                    }
                }
            }
        });
    }

    /// Loads all documents that can be reached by this internal document path
    /// # Panics
    /// When the path is not a file path
    pub fn load_dependencies(&self, path: &Path) -> tokio::task::JoinHandle<()> {
        let mini_backend = self.clone();
        let path = path.to_owned();
        tokio::spawn(async move {
            let mut todo: LinkedList<(Url, u32)> = { build_todo_list(&mini_backend, &path).await };

            let mut done = HashSet::<Url>::new();

            debug!(
                "Loading dependencies of {} len = {}",
                path.display(),
                todo.len()
            );

            while let Some((url, depth)) = todo.pop_front() {
                debug!("Indexing {url} ...");
                if done.contains(&url) {
                    debug!("URL {url} was indexed. Skip");
                    continue;
                }
                if depth > 2 {
                    debug!("Depth {depth} exceeded 2 for {url}. Skip");
                    continue;
                }

                let resolved_doc_or_err = {
                    let sync = mini_backend.sync.read().await;
                    let workspace = sync
                        .get_workspace(
                            &Url::from_file_path(&path)
                                .expect("File path should be convertable to URL"),
                        )
                        .expect("Workspace for document should exist");

                    // TODO well the Error is not Send. That's why we convert to Option
                    Workspace::resolve_url_to_document(
                        workspace,
                        &url,
                        mini_backend.http_client.clone(),
                    )
                    .await
                    .ok()
                };

                let resolved_doc = if let Some(resolved_doc) = resolved_doc_or_err {
                    resolved_doc
                } else {
                    // This is the error case
                    warn!("Resolving {url} did not work");
                    None
                };

                if let Some(doc) = resolved_doc {
                    match doc {
                        Document::Internal(internal_document) => {
                            for ele in internal_document.reachable_urls(true) {
                                todo.push_back((ele.clone(), 1));
                            }
                            let file_url = Url::from_file_path(&internal_document.path)
                                .expect("Path should also be a Url");
                            {
                                let mut sync = mini_backend.sync.write().await;
                                let workspace = sync.get_or_insert_workspace_mut(
                                    &Url::from_file_path(&path)
                                        .expect("File path should be convertable to URL"),
                                );
                                workspace.insert_internal_document(internal_document);
                            }

                            mini_backend.update_diagnostics_for_url_and_dependent(file_url);
                        }
                        Document::External(external_document) => {
                            // TODO maybe remove this?
                            // Lets not do that yet
                            for ele in external_document.reachable_urls() {
                                todo.push_back((ele.clone(), depth + 1));
                            }
                            {
                                let mut sync = mini_backend.sync.write().await;
                                let workspace = sync.get_or_insert_workspace_mut(
                                    &Url::from_file_path(&path)
                                        .expect("File path should be convertable to URL"),
                                );
                                workspace.insert_external_document(external_document);
                            }
                        }
                    }
                } else {
                    // The doc is already resolved
                }

                // Refresh the inline hints, because we got new information now
                refresh_inlay_hints(&mini_backend).await;

                done.insert(url);
            }

            // Every dependency is loaded
        })
    }

    fn server_capabilities(position_encoding_kind: PositionEncodingKind) -> ServerCapabilities {
        ServerCapabilities {
            text_document_sync: Some(TextDocumentSyncCapability::Kind(
                TextDocumentSyncKind::INCREMENTAL,
            )),
            hover_provider: Some(HoverProviderCapability::Simple(true)),
            document_formatting_provider: Some(OneOf::Left(true)),
            position_encoding: Some(position_encoding_kind),
            inlay_hint_provider: Some(OneOf::Left(true)),
            definition_provider: Some(OneOf::Left(true)),
            code_action_provider: Some(CodeActionProviderCapability::Simple(true)),
            completion_provider: Some(CompletionOptions {
                ..Default::default()
            }),
            semantic_tokens_provider: Some(
                SemanticTokensServerCapabilities::SemanticTokensOptions(SemanticTokensOptions {
                    legend: SemanticTokensLegend {
                        token_types: vec![
                            SemanticTokenType::NAMESPACE,
                            SemanticTokenType::TYPE,
                            SemanticTokenType::CLASS,
                            SemanticTokenType::ENUM,
                            SemanticTokenType::INTERFACE,
                            SemanticTokenType::STRUCT,
                            SemanticTokenType::TYPE_PARAMETER,
                            SemanticTokenType::PARAMETER,
                            SemanticTokenType::VARIABLE,
                            SemanticTokenType::PROPERTY,
                            SemanticTokenType::ENUM_MEMBER,
                            SemanticTokenType::EVENT,
                            SemanticTokenType::FUNCTION,
                            SemanticTokenType::METHOD,
                            SemanticTokenType::MACRO,
                            SemanticTokenType::KEYWORD,
                            SemanticTokenType::MODIFIER,
                            SemanticTokenType::COMMENT,
                            SemanticTokenType::STRING,
                            SemanticTokenType::NUMBER,
                            SemanticTokenType::REGEXP,
                            SemanticTokenType::OPERATOR,
                            SemanticTokenType::DECORATOR,
                        ],
                        token_modifiers: vec![],
                    },
                    full: Some(SemanticTokensFullOptions::Bool(true)),
                    range: Some(true),
                    ..Default::default()
                }),
            ),
            document_symbol_provider: Some(OneOf::Left(true)),
            workspace_symbol_provider: Some(OneOf::Right(WorkspaceSymbolOptions {
                work_done_progress_options: WorkDoneProgressOptions {
                    work_done_progress: Some(false),
                },
                resolve_provider: Some(false),
            })),
            references_provider: Some(OneOf::Left(true)),
            rename_provider: Some(OneOf::Right(RenameOptions {
                prepare_provider: Some(true),
                work_done_progress_options: WorkDoneProgressOptions {
                    work_done_progress: None,
                },
            })),
            ..Default::default()
        }
    }
}

#[derive(Debug, Clone)]
struct Options {
    order_frames: bool,
}

fn parse_options(options: Option<serde_json::Value>) -> Options {
    {
        let mut is_order_frames = false;
        if let Some(o) = options {
            if let Some(root) = o.as_object() {
                if let Some(omn) = root.get("omn") {
                    if let Some(omn) = omn.as_object() {
                        if let Some(order_frames) = omn.get("orderFrames") {
                            if let Some(order_frames) = order_frames.as_bool() {
                                is_order_frames = order_frames;
                            }
                        }
                    }
                }
            }
        }
        Options {
            order_frames: is_order_frames,
        }
    }
}

async fn refresh_inlay_hints(mini_backend: &Backend) {
    match mini_backend.client.inline_value_refresh().await {
        Ok(()) => {
            debug!("Refresh inline hints");
        }
        // Looks like I dont have a specific tower lsp error variant.
        // Buts thats not that bad. Just log it.
        Err(err) => {
            error!("{err}");
        }
    }
}

async fn build_todo_list(
    mini_backend: &Backend,
    path: &std::path::PathBuf,
) -> LinkedList<(Url, u32)> {
    let sync = mini_backend.sync.read().await;
    let workspace = sync
        .get_workspace(&Url::from_file_path(path).expect("File path should be convertable to URL"))
        .expect("Workspace for document should exist");

    let document = workspace.get_internal_document(path).unwrap();
    document
        .reachable_urls(true)
        .iter()
        .map(|u| (u.clone(), 1))
        .collect()
}

/// This is the main language server implamentation. It is the entry point for all requests to the language server.
#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    /// Initilizes the language server and loads the workspaces with catalog files.
    ///
    /// Does not load or index the files inside the workspaces.
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        info!("Initialize language server -----------------------------");
        info!("Client info:\n{:#?}", params.client_info);
        debug!("Client capabilities:\n{:#?}", params.capabilities);
        debug!("Options: {:#?}", params.initialization_options);

        let options = params.initialization_options;
        let options = parse_options(options);
        debug!("Parsed Options: {options:?}");
        self.options
            .set(options)
            .expect("options should not be set");

        let encodings = params
            .capabilities
            .general
            .and_then(|g| g.position_encodings)
            .unwrap_or_default();

        self.position_encoding
            .set(if encodings.contains(&PositionEncodingKind::UTF8) {
                PositionEncodingKind::UTF8
            } else {
                PositionEncodingKind::UTF16
            })
            .expect("the encoding to be unset");

        let mut sync = self.write_sync().await;

        for wf in params.workspace_folders.iter().flatten() {
            sync.push_workspace(Workspace::new(wf.clone()));
        }

        // Done with init, lets return the findings

        let position_encoding_kind = self
            .position_encoding
            .get()
            .expect("encoding should be set")
            .clone();

        Ok(InitializeResult {
            server_info: Some(ServerInfo {
                name: "owl-ms-language-server".to_string(),
                version: None,
            }),
            capabilities: Backend::server_capabilities(position_encoding_kind),
        })
    }

    async fn initialized(&self, _params: InitializedParams) {
        let sync = self.read_sync().await;
        let workspace_paths = sync
            .workspaces()
            .iter()
            .map(|w| format!("{w}"))
            .collect_vec();

        info!("Initialized languag server with workspaces: {workspace_paths:?}");
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        async {
            let file_url = params.text_document.uri;
            info!("Did open {file_url} ...",);

            let mut sync = self.write_sync().await;

            let workspace = sync.get_or_insert_workspace_mut(&file_url);
            let internal_document = InternalDocument::new(
                file_url.clone(),
                params.text_document.version,
                params.text_document.text,
            );

            let doc = workspace.insert_internal_document(internal_document);
            let path = doc.path.clone();

            let handle = self.load_dependencies(&path);

            #[cfg(test)]
            {
                // This is just for tests, so that they dont produce a race condition
                drop(sync);
                handle.await.unwrap();
            }
            #[cfg(not(test))]
            {
                workspace.index_handles.push(handle);
            }

            self.update_diagnostics_for_url_and_dependent(file_url);

            debug!("Did open!");

            Ok(())
        }
        .await
        .log_if_error();
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        async {
            debug!(
                "Did change at {} with version {}",
                params
                    .text_document
                    .uri
                    .path_segments()
                    .ok_or(Error::InvalidUrl(params.text_document.uri.clone()))?
                    .next_back()
                    .ok_or(Error::InvalidUrl(params.text_document.uri.clone()))?,
                params.text_document.version
            );

            let url = params.text_document.uri.clone();

            // Do the document edit
            let mut sync = self.write_sync().await;
            let (document, workspace) = sync.take_internal_document(&url)?;

            let new_document = timeit("document.edit", || document.edit(&params, self.encoding()))?;

            workspace.insert_internal_document(new_document);

            // TODO make this join handle one of a kind maybe. So no two diagnostics threads at the same time.
            // Async diagnostics
            self.update_diagnostics_for_url_and_dependent(url);

            Ok(())
        }
        .await
        .log_if_error();
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        (|| {
            debug!(
                "Did close at {}",
                params
                    .text_document
                    .uri
                    .path_segments()
                    .ok_or(Error::InvalidUrl(params.text_document.uri.clone()))?
                    .next_back()
                    .ok_or(Error::InvalidUrl(params.text_document.uri.clone()))?
            );

            Ok(())
        })()
        .log_if_error();

        // We do not close yet :> because of refences
        // TODO should data be deleted if a file is closed?
    }

    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        info!("formatting {params:#?}");
        let url = params.text_document.uri;

        let tab_size = params.options.tab_size;

        let sync = self.read_sync().await;
        let (doc, _) = sync.get_internal_document(&url)?;

        let options = FormattingSettings {
            tab_size: if tab_size == 0 { 4 } else { tab_size },
            ruler_width: 80,
            order_frames: self
                .options
                .get()
                .expect("options should be initilized")
                .order_frames,
        };
        // TODO just send the diff
        let text = doc.formatted(&options);

        let range: Range = doc.tree().root_node().range().into();

        return Ok(Some(vec![TextEdit {
            range: range.into_lsp(doc.rope(), self.encoding())?,
            new_text: text,
        }]));
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let url = params.text_document_position_params.text_document.uri;
        info!(
            "Hover at {:?} in file {url}",
            params.text_document_position_params.position
        );

        let sync = self.read_sync().await;
        let (doc, ws) = sync.get_internal_document(&url)?;

        let pos: Position = Position::from_lsp(
            params.text_document_position_params.position,
            doc.rope(),
            self.encoding(),
        )?;
        let node = doc
            .tree()
            .root_node()
            .named_descendant_for_point_range(pos.into(), pos.into())
            .ok_or(Error::PositionOutOfBounds(pos))?;

        let info = ws.node_info(&node, doc);

        Ok(if info.is_empty() {
            None
        } else {
            // Transitive into
            let range: Range = node.range().into();
            Some(Hover {
                contents: HoverContents::Scalar(MarkedString::String(info)),
                range: Some(range.into_lsp(doc.rope(), self.encoding())?),
            })
        })
    }

    async fn inlay_hint(&self, params: InlayHintParams) -> Result<Option<Vec<InlayHint>>> {
        let url = params.text_document.uri;
        info!("Inlay hint at {url}");

        let sync = self.read_sync().await;
        let (document, workspace) = sync.get_internal_document(&url)?;

        let range = Range::from_lsp(&params.range, document.rope(), self.encoding())?;

        debug!(
            "inlay_hint at {}:{range}",
            url.path_segments()
                .ok_or(Error::InvalidUrl(url.clone()))?
                .next_back()
                .ok_or(Error::InvalidUrl(url.clone()))?
        );

        let hints = document.inlay_hint(range, self.encoding(), workspace);

        Ok(Some(hints))
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let url = params.text_document_position_params.text_document.uri;
        debug!("goto_definition at {url}");

        let sync = self.read_sync().await;
        let (doc, workspace) = sync.get_internal_document(&url)?;
        let pos: Position = Position::from_lsp(
            params.text_document_position_params.position,
            doc.rope(),
            self.encoding(),
        )?;

        let leaf_node = doc
            .tree()
            .root_node()
            .named_descendant_for_point_range(pos.into(), pos.into())
            .ok_or(Error::PositionOutOfBounds(pos))?;

        let reachable_docs = doc.reachable_docs_recursive(workspace, true);

        let node_is_iri = ["full_iri", "simple_iri", "abbreviated_iri"].contains(&leaf_node.kind());
        if node_is_iri {
            let iri = trim_full_iri(node_text(&leaf_node, doc.rope()));
            let iri = doc.abbreviated_iri_to_full_iri(&iri).unwrap_or(iri);

            debug!("Try goto definition of {iri}");

            let iri_is_import_iri = leaf_node
                .parent()
                .expect("iri node should have parent")
                .kind()
                == "import";

            if iri_is_import_iri {
                let url = Url::parse(&iri).map_err(|_| Error::InvalidUrl(url.clone()))?;
                // This does not work for external documents from prefixes
                let path = workspace.url_to_path_with_catalog(&url);
                if let Some(path) = path {
                    return Ok(Some(single_path_response(&path)));
                }
            } else {
                let frame_info =
                    Workspace::get_frame_info_recursive(workspace, &iri, &reachable_docs);

                if let Some(frame_info) = frame_info {
                    let locations = frame_info
                        .definitions
                        .iter()
                        .sorted_by_key(|l| {
                            if l.range == Range::ZERO {
                                u32::MAX // No range? Then put this at the end
                            } else {
                                l.range.start.line()
                            }
                        })
                        .map(|l| l.clone().into_lsp(doc.rope(), self.encoding()))
                        .filter_and_log()
                        .collect_vec();

                    return Ok(Some(GotoDefinitionResponse::Array(locations)));
                }
            }
        }
        Ok(None)
    }

    async fn code_action(&self, params: CodeActionParams) -> Result<Option<CodeActionResponse>> {
        debug!("code_action at {}", params.text_document.uri);
        let url = params.text_document.uri;
        let sync = self.read_sync().await;
        let (doc, _) = sync.get_internal_document(&url)?;
        // let doc = doc.read();
        let end: Position = doc.tree().root_node().range().end_point.into();

        Ok(Some(vec![CodeActionOrCommand::CodeAction(CodeAction {
            title: "add class".to_string(),
            edit: Some(WorkspaceEdit {
                changes: Some(HashMap::from([(
                    url,
                    vec![TextEdit {
                        range: lsp_types::Range {
                            start: end.into_lsp(doc.rope(), self.encoding())?,
                            end: end.into_lsp(doc.rope(), self.encoding())?,
                        },
                        new_text:
                            "\nClass: new_class\n    Annotations:\n        rdfs:label \"new class\""
                                .to_string(),
                    }],
                )])),
                document_changes: None,
                change_annotations: None,
            }),
            ..Default::default()
        })]))
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        debug!(
            "completion at {} {:?} {:?}",
            params.text_document_position.text_document.uri,
            params.text_document_position.position,
            self.encoding()
        );
        let url = params.text_document_position.text_document.uri;
        let sync = self.read_sync().await;
        let (doc, workspace) = sync.get_internal_document(&url)?;
        let pos: Position = Position::from_lsp(
            params.text_document_position.position,
            doc.rope(),
            self.encoding(),
        )?;

        let kws = timeit("try_keywords_at_position", || {
            doc.try_keywords_at_position(pos)
        });

        debug!("The resultingn kws are {kws:#?}");

        let pos_one_left = pos.moved_left(1, doc.rope());

        let keywords_completion_items = kws.into_iter().map(|keyword| CompletionItem {
            label: keyword,
            kind: Some(CompletionItemKind::KEYWORD),
            ..Default::default()
        });

        let mut items = vec![];

        let node = doc
            .tree()
            .root_node()
            .named_descendant_for_point_range(pos_one_left.into(), pos_one_left.into())
            .expect("The pos to be in at least one node");

        // Generate the list of iris that can be inserted.
        let partial_text = node_text(&node, doc.rope()).to_string();

        if node.kind() == "simple_iri" {
            debug!("Try iris...");

            let iris: Vec<CompletionItem> = workspace
                .search_frame(&partial_text)
                .into_iter()
                .unique_by(|(_, iri, _)| iri.clone())
                .sorted_unstable_by_key(|(v, _, _)| v.clone())
                .filter_map(|(full, maybe_full_iri, frame)| {
                    let iri = doc.full_iri_to_abbreviated_iri(&maybe_full_iri).unwrap_or(
                        // This means it was not a full iri
                        maybe_full_iri.clone(),
                    );

                    if iri == partial_text {
                        None
                    } else {
                        Some(CompletionItem {
                            label: frame.label().unwrap_or(full),
                            kind: Some(CompletionItemKind::REFERENCE),
                            detail: Some(frame.info_display(workspace)),
                            insert_text: Some(iri),
                            // TODO #29 add details from the frame
                            ..Default::default()
                        })
                    }
                })
                // TODO #29 add items for simple iri, abbriviated iri and full iri
                // Take the shortest one maybe
                .collect();
            items.extend(iris);
        }

        items.extend(keywords_completion_items);

        debug!("completion item count {}", items.len());

        Ok(Some(CompletionResponse::Array(items)))
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        debug!("semantic_tokens_full at {}", params.text_document.uri);
        let url = params.text_document.uri;
        let sync = self.read_sync().await;
        let (doc, _) = sync.get_internal_document(&url)?;

        let tokens = doc.sematic_tokens(None, self.encoding())?;

        Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
            result_id: None,
            data: tokens,
        })))
    }

    async fn semantic_tokens_range(
        &self,
        params: SemanticTokensRangeParams,
    ) -> Result<Option<SemanticTokensRangeResult>> {
        debug!("semantic_tokens_range at {}", params.text_document.uri);
        let url = params.text_document.uri;
        let sync = self.read_sync().await;
        let (doc, _) = sync.get_internal_document(&url)?;
        let range = Range::from_lsp(&params.range, doc.rope(), self.encoding())?;
        let tokens = doc.sematic_tokens(Some(range), self.encoding())?;

        return Ok(Some(SemanticTokensRangeResult::Tokens(SemanticTokens {
            result_id: None,
            data: tokens,
        })));
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let url = params.text_document.uri;
        let sync = self.read_sync().await;
        let (doc, _) = sync.get_internal_document(&url)?;
        let infos = doc.all_frame_infos();
        return Ok(Some(DocumentSymbolResponse::Flat(
            infos
                .flat_map(|info| {
                    let name = info.label().unwrap_or_else(|| {
                        doc.full_iri_to_abbreviated_iri(&info.iri)
                            .unwrap_or(info.iri.clone())
                    });
                    let kind: SymbolKind = info.frame_type.into();
                    let url = url.clone();
                    info.definitions.iter().map(move |def| {
                        #[allow(deprecated)] // All fields need to be specified
                        Ok(SymbolInformation {
                            name: name.clone(),
                            kind,
                            tags: None,
                            deprecated: None,
                            location: Location {
                                uri: url.clone(),
                                range: def.range.into_lsp(doc.rope(), self.encoding())?,
                            },
                            container_name: None,
                        })
                    })
                })
                .filter_and_log()
                .filter(|s| !s.name.is_empty())
                .sorted_by_cached_key(|s| format!("{:?}{}", s.kind, s.name))
                .collect_vec(),
        )));
    }

    async fn symbol(
        &self,
        params: WorkspaceSymbolParams,
    ) -> Result<Option<Vec<SymbolInformation>>> {
        let query = params.query;
        info!("symbol with query: {query}");

        let sync = self.read_sync().await;
        let workspaces = sync.workspaces();
        let all_frame_infos = workspaces
            .iter()
            .flat_map(workspace::Workspace::all_frame_infos);

        let symbols = all_frame_infos
            .filter_map(|fi| {
                let score = fi.matches(&query);
                if score > 0 {
                    Some((score, fi))
                } else {
                    None
                }
            })
            .sorted_by_key(|(score, _)| *score)
            .rev()
            .flat_map(|(_, fi)| {
                let name = fi.label().unwrap_or(fi.iri.clone());

                #[allow(deprecated)] // All fields need to be specified
                fi.definitions
                    .iter()
                    .map(|definition| {
                        let url = &definition.uri;
                        let location = sync
                            .get_internal_document(url)
                            .map(|(doc, _)| {
                                definition.clone().into_lsp(doc.rope(), self.encoding())
                            })
                            .and_then(|r| r.inspect_err(|e| error!("{e}")))
                            .unwrap_or(Location {
                                uri: url.clone(),
                                range: lsp_types::Range::default(),
                            });

                        SymbolInformation {
                            name: name.clone(),
                            kind: fi.frame_type.into(),
                            tags: None,
                            deprecated: None,
                            location,
                            container_name: None,
                        }
                    })
                    .collect_vec()
            })
            .collect_vec();

        Ok(Some(symbols))
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        info!(
            "references {}:{}:{} {:?}",
            params.text_document_position.text_document.uri,
            params.text_document_position.position.line,
            params.text_document_position.position.character,
            params.context
        );

        let url = params.text_document_position.text_document.uri;
        let sync = self.read_sync().await;
        let (doc, workspace) = sync.get_internal_document(&url)?;

        let pos: Position = Position::from_lsp(
            params.text_document_position.position,
            doc.rope(),
            self.encoding(),
        )?;

        let node = doc
            .tree()
            .root_node()
            .named_descendant_for_point_range(pos.into(), pos.into())
            .ok_or(Error::PositionOutOfBounds(pos))?;

        let full_iri_option = match node.kind() {
            "full_iri" => Some(trim_full_iri(node_text(&node, doc.rope()))),
            "simple_iri" | "abbreviated_iri" => {
                let iri = node_text(&node, doc.rope());
                Some(
                    doc.abbreviated_iri_to_full_iri(&iri)
                        .unwrap_or(iri.to_string()),
                )
            }
            _ => None,
        };

        Ok(if let Some(full_iri) = full_iri_option {
            let locations = workspace
                .internal_documents()
                .flat_map(|doc| {
                    doc.references(&full_iri, params.context.include_declaration)
                        .into_iter()
                        .filter_map(|range| {
                            range
                                .into_lsp(doc.rope(), self.encoding())
                                .inspect_log()
                                .ok()
                        })
                        .map(|range| Location {
                            uri: Url::from_file_path(&doc.path)
                                .expect("File path should be a valid URL"),
                            range,
                        })
                })
                .collect_vec();
            Some(locations)
        } else {
            None
        })
    }

    async fn prepare_rename(
        &self,
        params: TextDocumentPositionParams,
    ) -> Result<Option<PrepareRenameResponse>> {
        let url = params.text_document.uri;

        let sync = self.read_sync().await;
        let (doc, _) = sync.get_internal_document(&url)?;
        let pos: Position = Position::from_lsp(params.position, doc.rope(), self.encoding())?;

        fn node_range(position: Position, doc: &InternalDocument) -> Option<Range> {
            debug!("prepare_rename try {position:?}");
            let node = doc
                .tree()
                .root_node()
                .named_descendant_for_point_range(position.into(), position.into())?;

            // This excludes prefix declaration, import and annotation target IRIs
            match node.parent()?.kind() {
                "datatype_iri"
                | "class_iri"
                | "annotation_property_iri"
                | "ontology_iri"
                | "data_property_iri"
                | "version_iri"
                | "object_property_iri"
                | "annotation_property_iri_annotated_list"
                | "individual_iri" => {}
                _ => return None,
            }

            match node.kind() {
                "full_iri" => {
                    let range: Range = node.range().into();
                    let range = Range {
                        start: range.start.moved_right(1, doc.rope()),
                        end: range.end.moved_left(1, doc.rope()),
                    };
                    Some(range)
                }
                "simple_iri" => {
                    let range: Range = node.range().into();
                    Some(range)
                }
                "abbreviated_iri" => {
                    let range: Range = node.range().into();
                    let text = node_text(&node, doc.rope()).to_string();
                    let col_offset = text
                        .find(':')
                        .expect("abbreviated_iri to contain at least one :")
                        + 1;
                    let range = Range {
                        // The column offset will never be that big
                        #[allow(clippy::cast_possible_truncation)]
                        start: range.start.moved_right(col_offset as u32, doc.rope()),
                        ..range
                    };
                    Some(range)
                }
                _ => None,
            }
        }

        let range = node_range(pos, doc)
            .or_else(|| {
                // we need to check one position left of the position because renames should work when the cursor is at end (inclusive) of a word
                // For example: ThisIsSomeIri| other text
                //                           ^
                //                       Cursor
                debug!("prepare rename try one position left");
                let position = pos.moved_left(1, doc.rope());
                node_range(position, doc)
            })
            .map(|range| {
                Ok(PrepareRenameResponse::Range(
                    range.into_lsp(doc.rope(), self.encoding())?,
                ))
            })
            .and_then(|r| r.inspect_err(|e: &Error| error!("{e}")).ok());
        debug!("prepare rename found range {range:?}");

        Ok(range)
    }

    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        debug!("rename {params:?}");
        let url = params.text_document_position.text_document.uri;
        let new_name = params.new_name;

        let sync = self.read_sync().await;
        let (doc, workspace) = sync.get_internal_document(&url)?;

        let pos: Position = Position::from_lsp(
            params.text_document_position.position,
            doc.rope(),
            self.encoding(),
        )?;

        let old_and_new_iri = if let Some(x) = rename_helper(pos, doc, new_name.clone())? {
            Some(x)
        } else {
            // we need to check one position left of the position because renames should work when the cursor is at end (inclusive) of a word
            // For example: ThisIsSomeIri| other text
            //                           ^
            //                       Cursor
            debug!("prepare rename try one position left");
            let position = pos.moved_left(1, doc.rope());
            rename_helper(position, doc, new_name)?
        };

        if let Some((full_iri, new_iri, iri_kind, original)) = old_and_new_iri {
            debug!("renaming resolved iris from {full_iri:?} to {new_iri:?}");
            let changes = workspace
                .internal_documents()
                .map(|doc| {
                    let edits = doc
                        .rename_edits(&full_iri, new_iri.as_ref(), &iri_kind, &original)
                        .into_iter()
                        .filter_map(|(range, str)| {
                            range
                                .into_lsp(doc.rope(), self.encoding())
                                .inspect_log()
                                .ok()
                                .map(|range| TextEdit {
                                    range,
                                    new_text: str,
                                })
                        })
                        .collect_vec();
                    (doc.uri.clone(), edits)
                })
                .collect();

            return Ok(Some(WorkspaceEdit {
                changes: Some(changes),
                document_changes: None,
                change_annotations: None,
            }));
        }
        Ok(None)
    }

    async fn shutdown(&self) -> Result<()> {
        info!("Shutdown");
        // TODO
        // let mut workspaces = self.workspaces.write();
        // workspaces.clear();
        Ok(())
    }
}

fn single_path_response(path: &Path) -> GotoDefinitionResponse {
    GotoDefinitionResponse::Scalar(Location {
        uri: Url::from_file_path(path).expect("path should be valid url"),
        range: lsp_types::Range {
            start: lsp_types::Position {
                line: 0,
                character: 0,
            },
            end: lsp_types::Position {
                line: 0,
                character: 0,
            },
        },
    })
}

type IriKindName = Option<(String, Option<String>, String, String)>;

fn rename_helper(
    position: Position,
    doc: &InternalDocument,
    new_name: String,
) -> Result<IriKindName> {
    let node = doc
        .tree()
        .root_node()
        .named_descendant_for_point_range(position.into(), position.into())
        .ok_or(Error::PositionOutOfBounds(position))?;

    match node.kind() {
        "full_iri" => {
            let iri_kind = node
                .parent()
                .expect("full_iri to have a parent")
                .kind()
                .to_string();
            let iri = trim_full_iri(node_text(&node, doc.rope()));
            Ok(Some((
                iri.clone(),
                Some(new_name.clone()),
                iri_kind,
                new_name,
            )))
        }
        "simple_iri" => {
            let iri = node_text(&node, doc.rope());
            let iri_kind = node
                .parent()
                .expect("simple_iri to have a parent")
                .kind()
                .to_string();
            Ok(Some((
                doc.abbreviated_iri_to_full_iri(&iri)
                    .unwrap_or(iri.to_string()),
                doc.abbreviated_iri_to_full_iri(&new_name),
                iri_kind,
                new_name,
            )))
        }
        "abbreviated_iri" => {
            let iri_kind = node
                .parent()
                .expect("abbreviated_iri to have a parent")
                .kind()
                .to_string();
            let iri = node_text(&node, doc.rope()).to_string();
            let (prefix, _) = iri
                .split_once(':')
                .expect("abbreviated_iri to contain at least one :");
            Ok(Some((
                doc.abbreviated_iri_to_full_iri(&iri).unwrap_or(iri.clone()),
                doc.abbreviated_iri_to_full_iri(&format!("{prefix}:{new_name}")),
                iri_kind,
                format!("{prefix}:{new_name}"),
            )))
        }
        _ => Ok(None),
    }
}

impl Backend {
    async fn read_sync(&self) -> RwLockReadGuard<'_, SyncBackend> {
        self.sync.read().await
    }
    async fn write_sync(&self) -> RwLockWriteGuard<'_, SyncBackend> {
        self.sync.write().await
    }

    fn encoding(&self) -> &PositionEncodingKind {
        self.position_encoding
            .get()
            .expect("position should be set")
    }
}

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
mod tests;
pub mod web;
mod workspace;

use debugging::timeit;
use error::{Error, ResultExt, ResultIterator};
use itertools::Itertools;
use log::{debug, error, info};
use pos::Position;
use range::Range;
use std::collections::HashMap;
use std::sync::{Arc, LazyLock};
use tokio::sync::{OnceCell, RwLock, RwLockReadGuard, RwLockWriteGuard};
use tokio::task::{self};
use tower_lsp::jsonrpc::Result;
// There are too many LSP types
#[allow(clippy::wildcard_imports)]
use tower_lsp::lsp_types::{self, *};
use tower_lsp::{Client, LanguageServer};
use tree_sitter_c2rust::Language;
use workspace::{node_text, trim_full_iri, InternalDocument, Workspace};

use crate::sync_backend::SyncBackend;
use crate::web::HttpClient;

// Constants

pub static LANGUAGE: LazyLock<Language> = LazyLock::new(|| tree_sitter_owl_ms::LANGUAGE.into());

// Model

pub struct Backend {
    pub client: Client,
    pub http_client: Arc<dyn HttpClient>,
    position_encoding: OnceCell<PositionEncodingKind>,
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
        }
    }
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

        Ok(InitializeResult {
            server_info: Some(ServerInfo {
                name: "owl-ms-language-server".to_string(),
                version: None,
            }),
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::INCREMENTAL,
                )),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                document_formatting_provider: Some(OneOf::Left(true)),
                position_encoding: Some(
                    self.position_encoding
                        .get()
                        .expect("encoding should be set")
                        .clone(),
                ),
                inlay_hint_provider: Some(OneOf::Left(true)),
                definition_provider: Some(OneOf::Left(true)),
                code_action_provider: Some(CodeActionProviderCapability::Simple(true)),
                completion_provider: Some(CompletionOptions {
                    ..Default::default()
                }),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensOptions(
                        SemanticTokensOptions {
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
                        },
                    ),
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
            },
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
            let url = params.text_document.uri;
            info!("Did open {url} ...",);

            let mut sync = self.write_sync().await;

            let workspace = sync.get_or_insert_workspace_mut(&url);
            let internal_document = InternalDocument::new(
                url.clone(),
                params.text_document.version,
                params.text_document.text,
            );

            let diagnostics = internal_document
                .diagnostics()
                .iter()
                .map(|(range, msg)| {
                    Ok(Diagnostic {
                        range: range.into_lsp(internal_document.rope(), self.encoding())?,
                        severity: Some(DiagnosticSeverity::ERROR),
                        code: None,
                        code_description: None,
                        source: Some("owl language server".to_string()),
                        message: msg.clone(),
                        related_information: None,
                        tags: None,
                        data: None,
                    })
                })
                .filter_and_log()
                .collect_vec();

            let client = self.client.clone();
            let url_cloned = url.clone();

            client
                .publish_diagnostics(url_cloned, diagnostics, Some(internal_document.version))
                .await;

            let doc = workspace.insert_internal_document(internal_document);
            let path = doc.path.clone();

            let handle = InternalDocument::load_dependencies(&path, &self.sync, &self.http_client);

            #[cfg(test)]
            {
                // TODO is this fine?
                drop(sync);
                handle.await.unwrap();
            }
            #[cfg(not(test))]
            {
                workspace.index_handles.push(handle);
            }

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

            let mut sync = self.write_sync().await;
            let (document, workspace) = sync.take_internal_document(&url)?;

            let new_document = document.edit(&params, self.encoding())?;

            let document = workspace.insert_internal_document(new_document);

            let diagnostics = document
                .diagnostics()
                .iter()
                .map(|(range, msg)| {
                    Ok(Diagnostic {
                        range: range.into_lsp(document.rope(), self.encoding())?,
                        severity: Some(DiagnosticSeverity::ERROR),
                        code: None,
                        code_description: None,
                        source: Some("owl language server".to_string()),
                        message: msg.clone(),
                        related_information: None,
                        tags: None,
                        data: None,
                    })
                })
                .filter_and_log()
                .collect_vec();
            let client = self.client.clone();
            let version = Some(document.version);
            task::spawn(async move {
                client.publish_diagnostics(url, diagnostics, version).await;
            });
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

        // TODO just send the diff
        let text = doc.formatted(if tab_size == 0 { 4 } else { tab_size }, 80);

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

        if ["full_iri", "simple_iri", "abbreviated_iri"].contains(&leaf_node.kind()) {
            let iri = trim_full_iri(node_text(&leaf_node, doc.rope()));
            let iri = doc.abbreviated_iri_to_full_iri(&iri).unwrap_or(iri);

            debug!("Try goto definition of {iri}");

            let frame_info = Workspace::get_frame_info_recursive(workspace, &iri, doc);

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
            return Ok(None);
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
                .filter_map(|(full, maybe_full_iri, frame)| {
                    let iri = doc.full_iri_to_abbreviated_iri(&maybe_full_iri).unwrap_or(
                        // This means it was not a full iri
                        maybe_full_iri.clone(),
                    );

                    if iri == partial_text {
                        None
                    } else {
                        Some(CompletionItem {
                            label: full,
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
            #[allow(deprecated)] // All fields need to be specified
            infos
                .iter()
                .flat_map(|info| {
                    info.definitions.iter().map(|def| {
                        Ok(SymbolInformation {
                            name: info.label().unwrap_or_else(|| {
                                doc.full_iri_to_abbreviated_iri(&info.iri)
                                    .unwrap_or(info.iri.clone())
                            }),
                            kind: info.frame_type.into(),
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
            .flat_map(|workspace| {
                let ws = workspace;
                ws.internal_documents()
                    .flat_map(workspace::InternalDocument::all_frame_infos)
                    .collect_vec()
            })
            .collect_vec();

        let symbols = all_frame_infos
            .iter()
            .filter(|fi| {
                fi.iri.contains(query.as_str())
                    || fi
                        .annotations
                        .values()
                        .any(|v| v.iter().any(|l| l.contains(query.as_str())))
            })
            .flat_map(|fi| {
                #[allow(deprecated)] // All fields need to be specified
                fi.definitions.iter().map(|definition| {
                    let url = &definition.uri;
                    let location = sync
                        .get_internal_document(url)
                        .map(|(doc, _)| definition.clone().into_lsp(doc.rope(), self.encoding()))
                        .and_then(|r| r.inspect_err(|e| error!("{e}")))
                        .unwrap_or(Location {
                            uri: url.clone(),
                            range: lsp_types::Range::default(),
                        });

                    SymbolInformation {
                        name: fi.iri.clone(),
                        kind: fi.frame_type.into(),
                        tags: None,
                        deprecated: None,
                        location,
                        container_name: None,
                    }
                })
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
                    doc.references(&full_iri, !params.context.include_declaration)
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

mod catalog;
mod consts;
pub mod debugging;
mod error;
mod pos;
mod queries;
mod range;
pub mod rope_provider;
#[cfg(test)]
mod tests;
mod web;
mod workspace;

use debugging::timeit;
use error::{Error, Result as MyResult, ResultExt, RwLockExt};
use itertools::Itertools;
use log::{debug, error, info, warn};
use once_cell::sync::Lazy;
use parking_lot::RwLock;
use pos::Position;
use queries::{ALL_QUERIES, NODE_TYPES};
use range::Range;
use std::collections::HashMap;
use std::sync::Arc;
use std::time::Duration;
use tokio::task::{self};
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{self, *};
use tower_lsp::{Client, LanguageServer};
use tree_sitter_c2rust::Language;
use web::{HttpClient, UreqClient};
use workspace::{node_text, trim_full_iri, InternalDocument, Workspace};

// Constants

pub static LANGUAGE: Lazy<Language> = Lazy::new(|| tree_sitter_owl_ms::LANGUAGE.into());

// Model

pub struct Backend {
    pub client: Client,
    position_encoding: RwLock<PositionEncodingKind>,
    workspaces: RwLock<Vec<Arc<RwLock<Workspace>>>>,
    pub http_client: Arc<dyn HttpClient>,
}

impl Backend {
    /// Creates a new [`Backend`] with a Ureq http client and UTF16 encoding.
    /// The workspaces are created empty.
    pub fn new(client: Client) -> Self {
        Backend {
            client,
            position_encoding: PositionEncodingKind::UTF16.into(),
            workspaces: RwLock::new(vec![]),
            http_client: Arc::new(UreqClient),
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
        debug!("Client capabilities:\n{:#?}", params.capabilities);
        debug!("Client locale:\n{:#?}", params.locale);
        debug!("Client info:\n{:#?}", params.client_info);

        let encodings = params
            .capabilities
            .general
            .and_then(|g| g.position_encodings)
            .unwrap_or_default();

        let mut position_encoding = self.position_encoding.write();
        *position_encoding = if encodings.contains(&PositionEncodingKind::UTF8) {
            PositionEncodingKind::UTF8
        } else {
            PositionEncodingKind::UTF16
        };

        let mut workspaces = self.workspaces.write();
        *workspaces = params
            .workspace_folders
            .unwrap_or_default()
            .iter()
            .map(|wf| Arc::new(RwLock::new(Workspace::new(wf.clone()))))
            .collect();

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
                position_encoding: Some(position_encoding.clone()),
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
        let workspace_paths = self
            .workspaces
            .read()
            .iter()
            .map(|w| {
                let w = w.read();
                w.workspace_folder
                    .uri
                    .to_file_path()
                    .expect("Valid filepath")
                    .display()
                    .to_string()
            })
            .collect_vec();

        info!("Initialized languag server with workspaces: {workspace_paths:?}");
    }

    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        info!("formatting {params:#?}");
        let url = params.text_document.uri;

        let tab_size = params.options.tab_size;

        let doc = self.get_internal_document(&url)?;
        let doc = doc.read();

        // TODO just send the diff
        let text = doc.formatted(if tab_size == 0 { 4 } else { tab_size }, 80);

        let range: Range = doc.tree.root_node().range().into();

        return Ok(Some(vec![TextEdit {
            range: range.into_lsp(&doc.rope, &self.position_encoding.read()),
            new_text: text,
        }]));
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        async {
            let url = params.text_document.uri;
            info!("Opend file {url}",);
            let workspace = self.find_workspace(&url)?;
            let internal_document = InternalDocument::new(
                url.clone(),
                params.text_document.version,
                params.text_document.text,
            );

            let diagnostics = internal_document
                .diagnostics
                .iter()
                .map(|(range, msg)| Diagnostic {
                    range: range.into_lsp(&internal_document.rope, &self.position_encoding.read()),
                    severity: Some(DiagnosticSeverity::ERROR),
                    code: None,
                    code_description: None,
                    source: Some("owl language server".to_string()),
                    message: msg.clone(),
                    related_information: None,
                    tags: None,
                    data: None,
                })
                .collect_vec();

            let client = self.client.clone();
            let a = tokio::spawn(async move {
                client
                    .publish_diagnostics(url.clone(), diagnostics, Some(internal_document.version))
                    .await;
            });

            let document = Workspace::insert_internal_document(workspace, internal_document);

            let http_client = self.http_client.clone();
            let b = tokio::spawn(async move {
                resolve_imports(document, &*http_client)
                    .await
                    .log_if_error();
            });

            tokio::try_join!(a, b)?;

            Ok(())
        }
        .await
        .log_if_error();
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        (|| {
            debug!(
                "did_change at {} with version {}",
                params
                    .text_document
                    .uri
                    .path_segments()
                    .ok_or(Error::InvalidUrl(params.text_document.uri.clone()))?
                    .next_back()
                    .ok_or(Error::InvalidUrl(params.text_document.uri.clone()))?,
                params.text_document.version
            );

            let url = &params.text_document.uri;
            let document = self.get_internal_document(url)?;

            let mut document = document.write();
            document.edit(&params, &self.position_encoding.read());

            let uri = params.text_document.uri.clone();
            let diagnostics = document
                .diagnostics
                .iter()
                .map(|(range, msg)| Diagnostic {
                    range: range.into_lsp(&document.rope, &self.position_encoding.read()),
                    severity: Some(DiagnosticSeverity::ERROR),
                    code: None,
                    code_description: None,
                    source: Some("owl language server".to_string()),
                    message: msg.clone(),
                    related_information: None,
                    tags: None,
                    data: None,
                })
                .collect_vec();
            let client = self.client.clone();
            let version = Some(document.version);
            task::spawn(async move {
                client.publish_diagnostics(uri, diagnostics, version).await;
            });
            Ok(())
        })()
        .log_if_error();
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        (|| {
            debug!(
                "did_close at {}",
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

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let url = params.text_document_position_params.text_document.uri;
        info!(
            "Hover at {:?} in file {url}",
            params.text_document_position_params.position
        );

        let document = self.get_internal_document(&url)?;

        let doc = document.read();
        let pos: Position = Position::from_lsp(
            &params.text_document_position_params.position,
            &doc.rope,
            &self.position_encoding.read(),
        );
        let node = doc
            .tree
            .root_node()
            .named_descendant_for_point_range(pos.into(), pos.into())
            .ok_or(Error::PositionOutOfBounds(pos))?;

        let info = doc.try_get_workspace()?.read().node_info(&node, &doc);

        Ok(if info.is_empty() {
            None
        } else {
            // Transitive into
            let range: Range = node.range().into();
            Some(Hover {
                contents: HoverContents::Scalar(MarkedString::String(info)),
                range: Some(range.into_lsp(&doc.rope, &self.position_encoding.read())),
            })
        })
    }

    async fn inlay_hint(&self, params: InlayHintParams) -> Result<Option<Vec<InlayHint>>> {
        let url = params.text_document.uri;

        let document = self.get_internal_document(&url)?;

        let document = document.read();
        let range = Range::from_lsp(
            &params.range,
            &document.rope,
            &self.position_encoding.read(),
        );
        debug!(
            "inlay_hint at {}:{range}",
            url.path_segments()
                .ok_or(Error::InvalidUrl(url.clone()))?
                .next_back()
                .ok_or(Error::InvalidUrl(url.clone()))?
        );

        let hints =
            document.inlay_hint(range, &*self.http_client, &self.position_encoding.read())?;

        Ok(Some(hints))
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let url = params.text_document_position_params.text_document.uri;
        debug!("goto_definition at {url}");

        let doc = self.get_internal_document(&url)?;
        let doc = doc.read();
        let pos: Position = Position::from_lsp(
            &params.text_document_position_params.position,
            &doc.rope,
            &self.position_encoding.read(),
        );

        let leaf_node = doc
            .tree
            .root_node()
            .named_descendant_for_point_range(pos.into(), pos.into())
            .ok_or(Error::PositionOutOfBounds(pos))?;

        if ["full_iri", "simple_iri", "abbreviated_iri"].contains(&leaf_node.kind()) {
            let iri = trim_full_iri(node_text(&leaf_node, &doc.rope));
            let iri = doc.abbreviated_iri_to_full_iri(iri.clone()).unwrap_or(iri);

            debug!("Try goto definition of {}", iri);

            let frame_info = Workspace::get_frame_info_recursive(
                doc.try_get_workspace()?,
                &iri,
                &doc,
                &*self.http_client,
            );

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
                    .map(|l| {
                        l.clone()
                            .into_lsp(&doc.rope, &__self.position_encoding.read())
                    })
                    .collect_vec();

                return Ok(Some(GotoDefinitionResponse::Array(locations)));
            }
            return Ok(None);
        } else {
            Ok(None)
        }
    }

    async fn code_action(&self, params: CodeActionParams) -> Result<Option<CodeActionResponse>> {
        debug!("code_action at {}", params.text_document.uri);
        let url = params.text_document.uri;
        let doc = self.get_internal_document(&url)?;
        let doc = doc.read();
        let end: Position = doc.tree.root_node().range().end_point.into();

        Ok(Some(vec![CodeActionOrCommand::CodeAction(CodeAction {
            title: "add class".to_string(),
            edit: Some(WorkspaceEdit {
                changes: Some(HashMap::from([(
                    url,
                    vec![TextEdit {
                        range: lsp_types::Range {
                            start: end.into_lsp(&doc.rope, &self.position_encoding.read()),
                            end: end.into_lsp(&doc.rope, &self.position_encoding.read()),
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
            self.position_encoding.read()
        );
        let url = params.text_document_position.text_document.uri;
        let doc = self.get_internal_document(&url)?;
        let doc = doc.read();
        let pos: Position = Position::from_lsp(
            &params.text_document_position.position,
            &doc.rope,
            &self.position_encoding.read(),
        );

        let kws = timeit("try_keywords_at_position", || {
            doc.try_keywords_at_position(pos)
        });

        debug!("The resultingn kws are {kws:#?}");

        let pos_one_left = pos.moved_left(1, &doc.rope);

        let keywords_completion_items = kws.into_iter().map(|keyword| CompletionItem {
            label: keyword,
            kind: Some(CompletionItemKind::KEYWORD),
            ..Default::default()
        });

        let mut items = vec![];

        let node = doc
            .tree
            .root_node()
            .named_descendant_for_point_range(pos_one_left.into(), pos_one_left.into())
            .expect("The pos to be in at least one node");

        // Generate the list of iris that can be inserted.
        let partial_text = node_text(&node, &doc.rope).to_string();

        if node.kind() == "simple_iri" {
            let workspace = doc.try_get_workspace()?;
            let workspace = workspace.read();
            let iris: Vec<CompletionItem> = workspace
                .search_frame(&partial_text)
                .iter()
                .filter_map(|(full_iri, frame)| {
                    let iri = doc
                        .full_iri_to_abbreviated_iri(full_iri.clone())
                        .unwrap_or(format!("<{full_iri}>"));

                    if iri == partial_text {
                        None
                    } else {
                        Some(CompletionItem {
                            label: iri,
                            kind: Some(CompletionItemKind::REFERENCE),
                            detail: Some(frame.info_display(&workspace)),
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

        Ok(Some(CompletionResponse::Array(items)))
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        debug!("semantic_tokens_full at {}", params.text_document.uri);
        let url = params.text_document.uri;
        let doc = self.get_internal_document(&url)?;
        let doc = doc.read();

        let tokens = doc.sematic_tokens(None, &self.position_encoding.read());

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
        let doc = self.get_internal_document(&url)?;
        let doc = doc.read();
        let range = Range::from_lsp(&params.range, &doc.rope, &self.position_encoding.read());
        let tokens = doc.sematic_tokens(Some(range), &self.position_encoding.read());

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
        let doc = self.get_internal_document(&url)?;
        let infos = doc.read().get_all_frame_infos();
        return Ok(Some(DocumentSymbolResponse::Flat(
            #[allow(deprecated)] // All fields need to be specified
            infos
                .iter()
                .flat_map(|info| {
                    info.definitions.iter().map(|def| SymbolInformation {
                        name: info.iri.clone(),
                        kind: info.frame_type.into(),
                        tags: None,
                        deprecated: None,
                        location: Location {
                            uri: url.clone(),
                            range: def
                                .range
                                .into_lsp(&doc.read().rope, &self.position_encoding.read()),
                        },
                        container_name: None,
                    })
                })
                .collect_vec(),
        )));
    }

    async fn symbol(
        &self,
        params: WorkspaceSymbolParams,
    ) -> Result<Option<Vec<SymbolInformation>>> {
        let query = params.query;
        info!("symbol with query: {query}");

        let workspaces = self.workspaces.read();
        let all_frame_infos = workspaces
            .iter()
            .flat_map(|workspace| {
                let ws = workspace.read();
                ws.internal_documents
                    .iter()
                    .flat_map(|doc| doc.read().get_all_frame_infos())
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
                    let location = self
                        .get_internal_document(url)
                        .map(|doc| {
                            let doc = doc.read();
                            definition
                                .clone()
                                .into_lsp(&doc.rope, &self.position_encoding.read())
                        })
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
        let (full_iri_option, workspace) = {
            let url = params.text_document_position.text_document.uri;

            let doc = self.get_internal_document(&url)?;
            let doc = doc.read();
            let pos: Position = Position::from_lsp(
                &params.text_document_position.position,
                &doc.rope,
                &self.position_encoding.read(),
            );

            let node = doc
                .tree
                .root_node()
                .named_descendant_for_point_range(pos.into(), pos.into())
                .ok_or(Error::PositionOutOfBounds(pos))?;

            let full_iri_option = match node.kind() {
                "full_iri" => Some(trim_full_iri(node_text(&node, &doc.rope))),
                "simple_iri" | "abbreviated_iri" => {
                    let iri = node_text(&node, &doc.rope);
                    Some(
                        doc.abbreviated_iri_to_full_iri(iri.to_string())
                            .unwrap_or(iri.to_string()),
                    )
                }
                _ => None,
            };
            let workspace = doc.try_get_workspace()?;
            (full_iri_option, workspace)
        };

        Ok(if let Some(full_iri) = full_iri_option {
            let workspace = workspace.read();
            let locations = workspace
                .internal_documents
                .iter()
                .flat_map(|doc| {
                    let doc = doc.value().read();
                    doc.query(&ALL_QUERIES.iri_query)
                        .into_iter()
                        .filter_map(|m| {
                            let (iri, range) = match &m.captures[..] {
                                [iri_capture] => (
                                    match iri_capture.node.kind.as_str() {
                                        "full_iri" => trim_full_iri(iri_capture.node.text.clone()),
                                        "simple_iri" | "abbreviated_iri" => doc
                                            .abbreviated_iri_to_full_iri(
                                                iri_capture.node.text.clone(),
                                            )
                                            .unwrap_or(iri_capture.node.text.clone()),

                                        _ => unreachable!(),
                                    },
                                    iri_capture.node.range,
                                ),
                                _ => unreachable!(),
                            };
                            if iri == full_iri {
                                Some(Location {
                                    uri: doc.uri.clone(),
                                    range: range
                                        .into_lsp(&doc.rope, &self.position_encoding.read()),
                                })
                            } else {
                                None
                            }
                        })
                        .collect_vec()
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

        let doc = self.get_internal_document(&url)?;
        let doc = doc.read();
        let pos: Position =
            Position::from_lsp(&params.position, &doc.rope, &self.position_encoding.read());

        fn node_range(position: Position, doc: &InternalDocument) -> Option<Range> {
            debug!("prepare_rename try {position:?}");
            let node = doc
                .tree
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
                        start: range.start.moved_right(1, &doc.rope),
                        end: range.end.moved_left(1, &doc.rope),
                    };
                    Some(range)
                }
                "simple_iri" => {
                    let range: Range = node.range().into();
                    Some(range)
                }
                "abbreviated_iri" => {
                    let range: Range = node.range().into();
                    let text = node_text(&node, &doc.rope).to_string();
                    let col_offset = text
                        .find(':')
                        .expect("abbreviated_iri to contain at least one :")
                        + 1;
                    let range = Range {
                        start: range.start.moved_right(col_offset as u32, &doc.rope),
                        ..range
                    };
                    Some(range)
                }
                _ => None,
            }
        }

        let range = node_range(pos, &doc)
            .or_else(|| {
                // we need to check one position left of the position because renames should work when the cursor is at end (inclusive) of a word
                // For example: ThisIsSomeIri| other text
                //                           ^
                //                       Cursor
                debug!("prepare rename try one position left");
                let position = pos.moved_left(1, &doc.rope);
                node_range(position, &doc)
            })
            .map(|range| {
                PrepareRenameResponse::Range(
                    range.into_lsp(&doc.rope, &self.position_encoding.read()),
                )
            });
        debug!("prepare rename found range {range:?}");

        Ok(range)
    }

    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        debug!("rename {params:?}");
        let url = params.text_document_position.text_document.uri;
        let new_name = params.new_name;

        let doc = self.get_internal_document(&url)?;

        let doc = doc.read_timeout()?;

        let pos: Position = Position::from_lsp(
            &params.text_document_position.position,
            &doc.rope,
            &self.position_encoding.read_recursive(),
        );

        fn rename_helper(
            position: Position,
            doc: &InternalDocument,
            new_name: String,
        ) -> Result<Option<(String, Option<String>, String, String)>> {
            let node = doc
                .tree
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
                    let iri = trim_full_iri(node_text(&node, &doc.rope));
                    Ok(Some((
                        iri.clone(),
                        Some(new_name.clone()),
                        iri_kind,
                        new_name,
                    )))
                }
                "simple_iri" => {
                    let iri = node_text(&node, &doc.rope);
                    let iri_kind = node
                        .parent()
                        .expect("simple_iri to have a parent")
                        .kind()
                        .to_string();
                    Ok(Some((
                        doc.abbreviated_iri_to_full_iri(iri.to_string())
                            .unwrap_or(iri.to_string()),
                        doc.abbreviated_iri_to_full_iri(new_name.clone()),
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
                    let iri = node_text(&node, &doc.rope).to_string();
                    let (prefix, _) = iri
                        .split_once(':')
                        .expect("abbreviated_iri to contain at least one :");
                    Ok(Some((
                        doc.abbreviated_iri_to_full_iri(iri.clone())
                            .unwrap_or(iri.clone()),
                        doc.abbreviated_iri_to_full_iri(format!("{prefix}:{new_name}")),
                        iri_kind,
                        format!("{prefix}:{new_name}"),
                    )))
                }
                _ => Ok(None),
            }
        }

        let old_and_new_iri = match rename_helper(pos, &doc, new_name.clone())? {
            Some(x) => Some(x),
            None => {
                // we need to check one position left of the position because renames should work when the cursor is at end (inclusive) of a word
                // For example: ThisIsSomeIri| other text
                //                           ^
                //                       Cursor
                debug!("prepare rename try one position left");
                let position = pos.moved_left(1, &doc.rope);
                rename_helper(position, &doc, new_name)?
            }
        };

        let workspace = doc.try_get_workspace()?;
        let workspace = workspace.read_timeout()?;
        drop(doc);
        drop(url);

        if let Some((full_iri, new_iri, iri_kind, original)) = old_and_new_iri {
            debug!("renaming resolved iris from {full_iri:?} to {new_iri:?}");
            let changes = workspace
                .internal_documents
                .iter()
                .map(|doc| {
                    let doc = doc
                        .value()
                        .try_read_recursive_for(Duration::from_secs(1))
                        .expect("a read lock to work");
                    let edits = doc
                        .query(&ALL_QUERIES.iri_query)
                        .into_iter()
                        .filter_map(|m| {
                            let (iri, range, parent_kind) = match &m.captures[..] {
                                [iri_capture] => (
                                    match iri_capture.node.kind.as_str() {
                                        "full_iri" => trim_full_iri(iri_capture.node.text.clone()),
                                        "simple_iri" | "abbreviated_iri" => doc
                                            .abbreviated_iri_to_full_iri(
                                                iri_capture.node.text.clone(),
                                            )
                                            .unwrap_or(iri_capture.node.text.clone()),

                                        _ => unreachable!(),
                                    },
                                    iri_capture.node.range,
                                    doc.node_by_id(iri_capture.node.id)
                                        .expect("the node id to be valid")
                                        .parent()
                                        .expect(
                                            "the iri node to have a parent of a specific iri kind",
                                        )
                                        .kind(),
                                ),
                                _ => unreachable!(),
                            };
                            if iri == full_iri && iri_kind == parent_kind {
                                Some(TextEdit {
                                    range: range.into_lsp(
                                        &doc.rope,
                                        &self.position_encoding.read_recursive(),
                                    ),
                                    new_text: new_iri
                                        .clone()
                                        .map(|new_iri| {
                                            doc.full_iri_to_abbreviated_iri(new_iri.clone())
                                                .unwrap_or(format!("<{}>", new_iri))
                                        })
                                        .unwrap_or(original.clone()),
                                })
                            } else {
                                None
                            }
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
        Ok(())
    }
}

impl Backend {
    /// This will find a workspace or create one for a given url
    fn find_workspace(&self, url: &Url) -> MyResult<Arc<RwLock<Workspace>>> {
        debug!("find workspace");
        let mut workspaces = self
            .workspaces
            .try_upgradable_read_for(Duration::from_secs(5))
            .ok_or(Error::LockTimeout(5))?;

        // TODO there are problems when the workspace is changing
        // - A document could be in its own workspace
        // - Then a catalog file is created or modified that would place a document in that workspace
        // - Because of the early return the document can never move to the new workspace
        let maybe_workspace = workspaces.iter().find(|workspace| {
            let workspace = workspace.read();
            workspace.internal_documents.contains_key(url)
                || workspace.catalogs.iter().any(|catalog| {
                    match &url
                        .to_file_path()
                        .inspect_err(|_| error!("Url is not a filepath {url}"))
                    {
                        Ok(path) => catalog.contains(path),
                        Err(_) => false,
                    }
                })
        });

        Ok(match maybe_workspace {
            None => {
                let mut file_path = url.to_file_path().expect("URL should be a filepath");
                file_path.pop();
                warn!("Workspace for {url} could not be found. Could the entry in catalog-v001.xml be missing? Creating a new one at {}", file_path.display());
                let workspace_folder = WorkspaceFolder {
                    // The workspace folder IS the single file. This is not ideal but should work for now.
                    uri: Url::from_file_path(file_path).expect("Valid URL from filepath"),
                    name: "Single File".into(),
                };
                debug!("try workspaces write...");
                let workspace = Workspace::new(workspace_folder.clone());

                // workspace_folder
                workspaces.with_upgraded(|w| {
                    debug!("locked workspace");

                    let ws = Arc::new(RwLock::new(workspace));
                    w.push(ws.clone());
                    ws
                })
            }
            Some(w) => w.clone(),
        })
    }

    /// Convinience function to fetch internal document
    fn get_internal_document(&self, url: &Url) -> MyResult<Arc<RwLock<InternalDocument>>> {
        let workspace = self.find_workspace(url)?;
        let workspace = workspace.read_timeout()?;
        workspace.get_internal_document(url)
    }
}

// TODO maybe move this into document or workspace
/// This will try to fetch the imports of a document
async fn resolve_imports(
    document: Arc<RwLock<InternalDocument>>,
    http_client: &dyn HttpClient,
) -> MyResult<()> {
    let document = document.read();
    debug!("Resolve imports for {}", document.uri);
    let urls = document
        .query(&ALL_QUERIES.import_query)
        .iter()
        .filter_map(|match_| match &match_.captures[..] {
            [iri] => Url::parse(&trim_full_iri(&iri.node.text)[..]).ok(),
            _ => unimplemented!(),
        })
        .collect_vec();

    for url in urls {
        Workspace::resolve_url_to_document(document.try_get_workspace()?, &url, http_client)
            .inspect_err(|e| error!("Resolve imports error: {e:?}"))
            .ok();
    }
    Ok(())
}

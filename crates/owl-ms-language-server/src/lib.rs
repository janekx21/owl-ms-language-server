mod catalog;
mod consts;
pub mod debugging;
mod position;
mod queries;
mod range;
pub mod rope_provider;
#[cfg(test)]
mod tests;
mod web;
mod workspace;

use debugging::timeit;
use itertools::Itertools;
use log::{debug, error, info, warn};
use once_cell::sync::Lazy;
use parking_lot::{MappedRwLockWriteGuard, RwLock, RwLockWriteGuard};
use position::Position;
use queries::{ALL_QUERIES, NODE_TYPES};
use range::Range;
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::Mutex;
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
    position_encoding: Mutex<PositionEncodingKind>,
    workspaces: RwLock<Vec<Workspace>>,
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

        let encodings = params
            .capabilities
            .general
            .and_then(|g| g.position_encodings)
            .unwrap_or(vec![PositionEncodingKind::UTF16]);

        let encoding = if encodings.contains(&PositionEncodingKind::UTF8) {
            PositionEncodingKind::UTF8
        } else {
            PositionEncodingKind::UTF16
        };

        let mut position_encoding = self.position_encoding.lock().await;
        *position_encoding = encoding.clone();

        let mut workspaces = self.workspaces.write();
        *workspaces = params
            .workspace_folders
            .unwrap_or_default()
            .iter()
            .map(|wf| Workspace::new(wf.clone()))
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
                position_encoding: Some(encoding),
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

        let workspace = self.find_workspace(&url);
        if let Some(doc) = workspace.internal_documents.get(&url) {
            let doc = doc.read();

            // TODO just send the diff
            let text = doc.formatted(if tab_size == 0 { 4 } else { tab_size }, 80);

            let range: Range = doc.tree.root_node().range().into();

            return Ok(Some(vec![TextEdit {
                range: range.into(),
                new_text: text,
            }]));
        }

        Ok(Some(vec![]))
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let url = params.text_document.uri;
        info!("Opend file {url}",);

        let workspace = self.find_workspace(&url);
        let internal_document = InternalDocument::new(
            url.clone(),
            params.text_document.version,
            params.text_document.text,
        );

        self.client
            .publish_diagnostics(
                url.clone(),
                internal_document.diagnostics.clone(),
                Some(internal_document.version),
            )
            .await;

        let document = workspace.insert_internal_document(internal_document);

        self.resolve_imports(document, &workspace).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        debug!(
            "did_change at {} with version {}",
            params
                .text_document
                .uri
                .path_segments()
                .unwrap()
                .next_back()
                .unwrap(),
            params.text_document.version
        );

        let url = &params.text_document.uri;
        let workspace = self.find_workspace(url);

        if let Some(document) = workspace
            .internal_documents
            .get_mut(&params.text_document.uri)
        {
            let mut document = document.write();
            document.edit(&params);

            let uri = params.text_document.uri.clone();
            let diagnostics = document.diagnostics.clone();
            let client = self.client.clone();
            let version = Some(document.version);
            task::spawn(async move {
                client.publish_diagnostics(uri, diagnostics, version).await;
            });
        };
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        debug!(
            "did_close at {}",
            params
                .text_document
                .uri
                .path_segments()
                .unwrap()
                .next_back()
                .unwrap(),
        );
        // We do not close yet :> because of refences
        // TODO should data be deleted if a file is closed?
        // let workspace = self.find_workspace(&params.text_document.uri).await;
        // workspace.document_map.remove(&params.text_document.uri);
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let url = params.text_document_position_params.text_document.uri;
        info!(
            "Hover at {:?} in file {url}",
            params.text_document_position_params.position
        );

        let workspace = self.find_workspace(&url);

        Ok(workspace.internal_documents.get(&url).and_then(|document| {
            let document = document.read();
            let pos: Position = params.text_document_position_params.position.into();
            let node = document
                .tree
                .root_node()
                .named_descendant_for_point_range(pos.into(), pos.into())
                .unwrap();
            let info = workspace.node_info(&node, &document);

            if info.is_empty() {
                None
            } else {
                // Transitive into
                let range: Range = node.range().into();
                Some(Hover {
                    contents: HoverContents::Scalar(MarkedString::String(info)),
                    range: Some(range.into()),
                })
            }
        }))
    }

    async fn inlay_hint(&self, params: InlayHintParams) -> Result<Option<Vec<InlayHint>>> {
        let url = params.text_document.uri;
        let range = params.range.into();

        debug!(
            "inlay_hint at {}:{range}",
            url.path_segments().unwrap().next_back().unwrap()
        );

        let workspace = self.find_workspace(&url);
        let document = if let Some(doc) = workspace.internal_documents.get(&url) {
            doc
        } else {
            return Err(tower_lsp::jsonrpc::Error::invalid_params(
                "Document with that URL not found",
            ));
        };

        let document = document.read();

        let hints = document.inlay_hint(range, &workspace, &*self.http_client);

        Ok(Some(hints))
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = params.text_document_position_params.text_document.uri;
        debug!("goto_definition at {}", uri);
        let workspace = self.find_workspace(&uri);
        let maybe_doc = workspace.internal_documents.get(&uri);
        if let Some(doc) = maybe_doc {
            let doc = doc.read();
            let pos: Position = params.text_document_position_params.position.into();

            let leaf_node = doc
                .tree
                .root_node()
                .named_descendant_for_point_range(pos.into(), pos.into())
                .unwrap();

            if ["full_iri", "simple_iri", "abbreviated_iri"].contains(&leaf_node.kind()) {
                let iri = trim_full_iri(node_text(&leaf_node, &doc.rope));
                let iri = doc.abbreviated_iri_to_full_iri(iri.clone()).unwrap_or(iri);

                debug!("Try goto definition of {}", iri);

                // TODO #16 is this now correct?
                let frame_info = workspace.get_frame_info_recursive(&iri, &doc, &*self.http_client);

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
                        .map(|l| l.clone().into())
                        .collect_vec();

                    return Ok(Some(GotoDefinitionResponse::Array(locations)));
                }
                return Ok(None);
            } else {
                Ok(None)
            }
        } else {
            Err(tower_lsp::jsonrpc::Error::new(
                tower_lsp::jsonrpc::ErrorCode::InvalidParams,
            ))
        }
    }

    async fn code_action(&self, params: CodeActionParams) -> Result<Option<CodeActionResponse>> {
        debug!("code_action at {}", params.text_document.uri);
        let url = params.text_document.uri;
        let workspace = self.find_workspace(&url);
        let doc = workspace
            .internal_documents
            .get(&url)
            .ok_or(tower_lsp::jsonrpc::Error::invalid_request())?;
        let doc = doc.read();
        let end: Position = doc.tree.root_node().range().end_point.into();

        Ok(Some(vec![CodeActionOrCommand::CodeAction(CodeAction {
            title: "add class".to_string(),
            edit: Some(WorkspaceEdit {
                changes: Some(HashMap::from([(
                    url,
                    vec![TextEdit {
                        range: lsp_types::Range {
                            start: end.into(),
                            end: end.into(),
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
            "completion at {}",
            params.text_document_position.text_document.uri
        );
        let url = params.text_document_position.text_document.uri;
        let workspace = self.find_workspace(&url);
        let doc = workspace.internal_documents.get(&url);
        let pos: Position = params.text_document_position.position.into();

        Ok(doc.map(|doc| {
            let doc = doc.read();

            let kws = timeit("try_keywords_at_position", || {
                doc.try_keywords_at_position(pos)
            });

            debug!("The resultingn kws are {kws:#?}");

            let mut pos_one_left = pos.clone();
            pos_one_left.sub_character(1);

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
                let iris: Vec<CompletionItem> = workspace
                    .search_frame(&partial_text)
                    .iter()
                    .map(|(full_iri, frame)| {
                        let iri = doc
                            .full_iri_to_abbreviated_iri(full_iri.clone())
                            .unwrap_or(format!("<{full_iri}>"));

                        CompletionItem {
                            label: iri,
                            kind: Some(CompletionItemKind::REFERENCE),
                            detail: Some(frame.info_display(&workspace)),
                            // TODO #29 add details from the frame
                            ..Default::default()
                        }
                    })
                    // TODO #29 add items for simple iri, abbriviated iri and full iri
                    // Take the shortest one maybe
                    .collect();
                items.extend(iris);
            }

            items.extend(keywords_completion_items);

            CompletionResponse::Array(items)
        }))
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        debug!("semantic_tokens_full at {}", params.text_document.uri);
        let uri = params.text_document.uri;
        let workspace = self.find_workspace(&uri);
        if let Some(doc) = workspace.internal_documents.get(&uri) {
            let doc = doc.read();

            let tokens = doc.sematic_tokens(None);

            return Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
                result_id: None,
                data: tokens,
            })));
        }
        Ok(None)
    }

    async fn semantic_tokens_range(
        &self,
        params: SemanticTokensRangeParams,
    ) -> Result<Option<SemanticTokensRangeResult>> {
        debug!("semantic_tokens_range at {}", params.text_document.uri);
        let uri = params.text_document.uri;
        let workspace = self.find_workspace(&uri);
        if let Some(doc) = workspace.internal_documents.get(&uri) {
            let doc = doc.read();
            let range: Range = params.range.into();
            let tokens = doc.sematic_tokens(Some(range));

            return Ok(Some(SemanticTokensRangeResult::Tokens(SemanticTokens {
                result_id: None,
                data: tokens,
            })));
        }
        Ok(None)
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let url = params.text_document.uri;
        let workspace = self.find_workspace(&url);
        if let Some(doc) = workspace.internal_documents.get(&url) {
            let infos = doc.read().get_all_frame_infos();
            drop(doc);
            return Ok(Some(DocumentSymbolResponse::Flat(
                #[allow(deprecated)] // All fields need to be specified
                infos
                    .iter()
                    .map(|info| SymbolInformation {
                        name: info.iri.clone(),
                        kind: info.frame_type.into(),
                        tags: None,
                        deprecated: None,
                        location: Location {
                            uri: url.clone(),
                            range: info.definitions.first().unwrap().range.into(), //TODO remove unwrap
                        },
                        container_name: None,
                    })
                    .collect_vec(),
            )));
        }
        Ok(None)
    }

    async fn symbol(
        &self,
        params: WorkspaceSymbolParams,
    ) -> Result<Option<Vec<SymbolInformation>>> {
        let query = params.query;
        info!("symbol with query:{query}");
        let workspaces = self.workspaces.read();
        let all_frame_infos = workspaces
            .iter()
            .flat_map(|workspace| workspace.internal_documents.iter())
            .flat_map(|doc| doc.read().get_all_frame_infos())
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
                fi.definitions.iter().map(|definition| SymbolInformation {
                    name: fi.iri.clone(),
                    kind: fi.frame_type.into(),
                    tags: None,
                    deprecated: None,
                    location: definition.clone().into(),
                    container_name: None,
                })
            })
            .collect_vec();

        Ok(Some(symbols))
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let url = params.text_document_position.text_document.uri;
        let position: Position = params.text_document_position.position.into();

        let workspace = self.find_workspace(&url);
        if let Some(doc) = workspace.internal_documents.get(&url) {
            let doc = doc.read();
            let node = doc
                .tree
                .root_node()
                .named_descendant_for_point_range(position.into(), position.into())
                .unwrap();

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

            drop(doc);
            drop(url);

            if let Some(full_iri) = full_iri_option {
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
                                            "full_iri" => {
                                                trim_full_iri(iri_capture.node.text.clone())
                                            }
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
                                        range: range.into(),
                                    })
                                } else {
                                    None
                                }
                            })
                            .collect_vec()
                    })
                    .collect_vec();
                return Ok(Some(locations));
            }
        }
        Ok(None)
    }

    async fn prepare_rename(
        &self,
        params: TextDocumentPositionParams,
    ) -> Result<Option<PrepareRenameResponse>> {
        let url = params.text_document.uri;
        let position: Position = params.position.into();

        let workspace = self.find_workspace(&url);
        let x = if let Some(doc) = workspace.internal_documents.get(&url) {
            let doc = doc.read();

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
                        let mut range: Range = node.range().into();
                        range.start.add_character(1);
                        range.end.sub_character(1);
                        Some(range)
                    }
                    "simple_iri" => {
                        let range: Range = node.range().into();
                        Some(range)
                    }
                    "abbreviated_iri" => {
                        let mut range: Range = node.range().into();
                        let text = node_text(&node, &doc.rope).to_string();
                        let col_offset = text.find(':').unwrap() + 1;
                        range.start.add_character(col_offset as u32);
                        Some(range)
                    }
                    _ => None,
                }
            }

            let range = node_range(position, &doc)
                .or_else(|| {
                    // we need to check one position left of the position because renames should work when the cursor is at end (inclusive) of a word
                    // For example: ThisIsSomeIri| other text
                    //                           ^
                    //                       Cursor
                    debug!("prepare rename try one position left");
                    let mut position = position.clone();
                    position.sub_character(1);
                    node_range(position, &doc)
                })
                .map(|range| PrepareRenameResponse::Range(range.into()));
            debug!("prepare rename found range {range:?}");

            Ok(range)
        } else {
            // TODO #24 this should be some document not found error
            Ok(None)
        };
        x
    }

    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        let url = params.text_document_position.text_document.uri;
        let position: Position = params.text_document_position.position.into();
        let new_name = params.new_name;

        debug!("rename {url}:{position:?} with {new_name}");

        let workspace = self.find_workspace(&url);
        if let Some(doc) = workspace.internal_documents.get(&url) {
            let doc = doc.read();

            fn rename_helper(
                position: Position,
                doc: &InternalDocument,
                new_name: String,
            ) -> Option<(String, Option<String>, String, String)> {
                let node = doc
                    .tree
                    .root_node()
                    .named_descendant_for_point_range(position.into(), position.into())
                    .unwrap();

                match node.kind() {
                    "full_iri" => {
                        let iri_kind = node.parent().unwrap().kind().to_string();
                        let iri = trim_full_iri(node_text(&node, &doc.rope));
                        Some((iri.clone(), Some(new_name.clone()), iri_kind, new_name))
                    }
                    "simple_iri" => {
                        let iri = node_text(&node, &doc.rope);
                        let iri_kind = node.parent().unwrap().kind().to_string();
                        Some((
                            doc.abbreviated_iri_to_full_iri(iri.to_string())
                                .unwrap_or(iri.to_string()),
                            doc.abbreviated_iri_to_full_iri(new_name.clone()),
                            iri_kind,
                            new_name,
                        ))
                    }
                    "abbreviated_iri" => {
                        let iri_kind = node.parent().unwrap().kind().to_string();
                        let iri = node_text(&node, &doc.rope).to_string();
                        let (prefix, _) = iri.split_once(':').unwrap();
                        Some((
                            doc.abbreviated_iri_to_full_iri(iri.clone())
                                .unwrap_or(iri.clone()),
                            doc.abbreviated_iri_to_full_iri(format!("{prefix}:{new_name}")),
                            iri_kind,
                            format!("{prefix}:{new_name}"),
                        ))
                    }
                    _ => None,
                }
            }

            let old_and_new_iri = rename_helper(position, &doc, new_name.clone()).or_else(|| {
                // we need to check one position left of the position because renames should work when the cursor is at end (inclusive) of a word
                // For example: ThisIsSomeIri| other text
                //                           ^
                //                       Cursor
                debug!("prepare rename try one position left");
                let mut position = position.clone();
                position.sub_character(1);
                rename_helper(position, &doc, new_name)
            });

            drop(doc);
            drop(url);

            if let Some((full_iri, new_iri, iri_kind, original)) = old_and_new_iri {
                debug!("renaming resolved iris from {full_iri:?} to {new_iri:?}");
                let changes = workspace
                    .internal_documents
                    .iter()
                    .map(|doc| {
                        let doc = doc.value().read();
                        let edits = doc
                            .query(&ALL_QUERIES.iri_query)
                            .into_iter()
                            .filter_map(|m| {
                                let (iri, range, parent_kind) = match &m.captures[..] {
                                    [iri_capture] => (
                                        match iri_capture.node.kind.as_str() {
                                            "full_iri" => {
                                                trim_full_iri(iri_capture.node.text.clone())
                                            }
                                            "simple_iri" | "abbreviated_iri" => doc
                                                .abbreviated_iri_to_full_iri(
                                                    iri_capture.node.text.clone(),
                                                )
                                                .unwrap_or(iri_capture.node.text.clone()),

                                            _ => unreachable!(),
                                        },
                                        iri_capture.node.range,
                                        doc.node_by_id(iri_capture.node.id)
                                            .unwrap()
                                            .parent()
                                            .unwrap()
                                            .kind(),
                                    ),
                                    _ => unreachable!(),
                                };
                                if iri == full_iri && iri_kind == parent_kind {
                                    Some(TextEdit {
                                        range: range.into(),
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
    fn find_workspace<'a>(&'a self, url: &Url) -> MappedRwLockWriteGuard<'a, Workspace> {
        let mut workspaces = self.workspaces.write();

        // TODO there are problems when the workspace is changing
        // - A document could be in its own workspace
        // - Then a catalog file is created or modified that would place a document in that workspace
        // - Because of the early return the document can never move to the new workspace
        let maybe_workspace = workspaces.iter().find(|workspace| {
            workspace.internal_documents.contains_key(url)
                || workspace
                    .catalogs
                    .iter()
                    .any(|catalog| catalog.contains(&url.to_file_path().unwrap()))
        });

        let folder = match maybe_workspace {
            None => {
                let mut file_path = url.to_file_path().expect("URL should be a filepath");
                file_path.pop();
                warn!("Workspace for {url} could not be found. Could the entry in catalog-v001.xml be missing? Creating a new one at {}", file_path.display());
                let workspace_folder = WorkspaceFolder {
                    // The workspace folder IS the single file. This is not ideal but should work for now.
                    uri: Url::from_file_path(file_path).expect("Valid URL from filepath"),
                    name: "Single File".into(),
                };
                let workspace = Workspace::new(workspace_folder.clone());
                workspaces.push(workspace);
                workspace_folder
            }
            Some(w) => w.workspace_folder.clone(),
        };

        RwLockWriteGuard::map(workspaces, |ws| {
            ws.iter_mut()
                .find(|w| w.workspace_folder == folder)
                .expect("The file to be located in a workspace, but it was not.")
        })
    }

    // TODO maybe move this into document or workspace
    /// This will try to fetch the imports of a document
    async fn resolve_imports(
        &self,
        document: Arc<RwLock<InternalDocument>>,
        workspace: &Workspace,
    ) {
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
            workspace
                .resolve_url_to_document(&url, &*self.http_client)
                .inspect_err(|e| error!("Resolve imports error: {e:?}"))
                .ok();
        }
    }
}

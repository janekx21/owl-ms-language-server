mod catalog;
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
use rope_provider::RopeProvider;
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::Mutex;
use tokio::task::{self};
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{self, *};
use tower_lsp::{Client, LanguageServer};
use tree_sitter::{Language, Node, Point, Query, QueryCursor, Tree, TreeCursor};
use tree_sitter_owl_ms::language;
use web::{HttpClient, UreqClient};
use workspace::{node_text, trim_full_iri, InternalDocument, Workspace};

// Constants

pub static LANGUAGE: Lazy<Language> = Lazy::new(language);

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
                            ..Default::default()
                        },
                    ),
                ),
                workspace_symbol_provider: Some(OneOf::Right(WorkspaceSymbolOptions {
                    work_done_progress_options: WorkDoneProgressOptions {
                        work_done_progress: Some(false),
                    },
                    resolve_provider: Some(false),
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
                .last()
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

        Ok(workspace.internal_documents.get(&url).map(|document| {
            let document = document.read();
            let pos = params.text_document_position_params.position.into();
            let node = deepest_named_node_at_pos(&document.tree, pos);
            let info = workspace.node_info(&node, &document, &*self.http_client);
            let range: Range = node.range().into();
            Hover {
                contents: HoverContents::Scalar(MarkedString::String(info)),
                range: Some(range.into()),
            }
        }))
    }

    async fn inlay_hint(&self, params: InlayHintParams) -> Result<Option<Vec<InlayHint>>> {
        let url = params.text_document.uri;
        let range = params.range.into();

        debug!(
            "inlay_hint at {}:{range}",
            url.path_segments().unwrap().last().unwrap()
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

            let leaf_node = deepest_named_node_at_pos(&doc.tree, pos);
            if ["full_iri", "simple_iri", "abbreviated_iri"].contains(&leaf_node.kind()) {
                let iri = trim_full_iri(node_text(&leaf_node, &doc.rope));
                let iri = doc.abbreviated_iri_to_full_iri(iri);

                debug!("Try goto definition of {}", iri);

                let frame_info = workspace.get_frame_info(&iri);

                if let Some(frame_info) = frame_info {
                    let locations = frame_info
                        .definitions
                        .iter()
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
            // generate keyword list that can be applied. Note that this is missing some keywords because of limitations in the grammar.
            let mut cursor = doc.tree.walk();
            cursor_goto_pos(&mut cursor, pos);
            while cursor.node().is_error() {
                if !cursor.goto_parent() {
                    break; // we reached the root
                }
            }
            let parent_kind = cursor.node().kind();
            let possible_children = NODE_TYPES
                .get(parent_kind)
                .map(|static_node| static_node.children.types.clone())
                .unwrap_or_default();

            let keywords = possible_children
                .iter()
                .filter_map(|child| node_type_to_keyword(child.type_.as_ref()))
                .map(|keyword| CompletionItem {
                    label: keyword,
                    ..Default::default()
                });

            let mut items = vec![];

            // Generate the list of iris that can be inserted.
            let child_node_types: Vec<String> = possible_children
                .iter()
                .filter_map(|child| node_type_to_keyword(child.type_.as_ref()))
                .collect();
            debug!("parent kind {}", parent_kind);
            debug!("child node type {:?}", child_node_types);

            let partial_text = node_text(&cursor.node(), &doc.rope).to_string();

            if parent_kind == "simple_iri" {
                let iris: Vec<CompletionItem> = workspace
                    .search_frame(&partial_text)
                    .iter()
                    .map(|(iri, _frame)| CompletionItem {
                        label: iri.clone(),
                        // TODO #29 add details from the frame
                        ..Default::default()
                    })
                    .collect();
                items.extend(iris);
            }

            items.extend(keywords);

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
            let query_source = tree_sitter_owl_ms::HIGHLIGHTS_QUERY;

            let query = Query::new(*LANGUAGE, query_source).expect("valid query expect");
            let mut query_cursor = QueryCursor::new();
            let matches =
                query_cursor.matches(&query, doc.tree.root_node(), RopeProvider::new(&doc.rope));

            let mut tokens = vec![];
            let mut last_start = Point { row: 0, column: 0 };

            let mut nodes = matches
                .flat_map(|m| m.captures)
                .map(|c| {
                    (
                        c.node,
                        treesitter_highlight_capture_into_semantic_token_type_index(
                            query.capture_names()[c.index as usize].as_str(),
                        ),
                    )
                })
                .collect::<Vec<(Node, u32)>>();
            // node start poins need to be stricly in order, because the delta might otherwise negativly overflow
            // TODO is this needed? are query matches in order?
            nodes.sort_unstable_by_key(|(n, _)| n.start_byte());
            for (node, type_index) in nodes {
                let start = node.range().start_point;
                let delta_line = (start.row - last_start.row) as u32;
                let delta_start = if delta_line == 0 {
                    (start.column - last_start.column) as u32 // same line
                } else {
                    start.column as u32 // some other line
                };
                let length = node_text(&node, &doc.rope).len_chars() as u32;

                let token = SemanticToken {
                    delta_line,
                    delta_start,
                    length,
                    token_type: type_index,
                    token_modifiers_bitset: 0,
                };

                last_start = node.start_position();
                tokens.push(token);
            }

            return Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
                result_id: None,
                data: tokens,
            })));
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

// Functions

fn treesitter_highlight_capture_into_semantic_token_type_index(str: &str) -> u32 {
    match str {
        "punctuation.bracket" => 21,   // SemanticTokenType::OPERATOR,
        "punctuation.delimiter" => 21, // SemanticTokenType::OPERATOR,
        "keyword" => 15,               // SemanticTokenType::KEYWORD,
        "operator" => 21,              // SemanticTokenType::OPERATOR,
        "variable.buildin" => 8,       // SemanticTokenType::VARIABLE,
        "string" => 18,                // SemanticTokenType::STRING,
        "number" => 19,                //SemanticTokenType::NUMBER,
        "constant.builtin" => 8,       // SemanticTokenType::VARIABLE,
        "variable" => 8,               //SemanticTokenType::VARIABLE,
        _ => todo!("highlight capture {} not implemented", str),
    }
}

// Implementations for Structs

fn deepest_named_node_at_pos(tree: &Tree, pos: Position) -> Node {
    let mut cursor = tree.walk();
    loop {
        let is_leaf = cursor.goto_first_child_for_point(pos.into()).is_none();
        if is_leaf {
            break;
        }
    }
    while !cursor.node().is_named() {
        if !cursor.goto_parent() {
            break;
        }
    }
    cursor.node()
}

fn cursor_goto_pos(cursor: &mut TreeCursor, pos: Position) {
    loop {
        let is_leaf = cursor.goto_first_child_for_point(pos.into()).is_none();
        if is_leaf {
            break;
        }
    }
    while !cursor.node().is_named() {
        if !cursor.goto_parent() {
            break;
        }
    }
}

// TODO this could be even better, if the grammar would consider all possible sub properties like "sub_class_of" for every frame type. This can be done by splitting a buch of rules up or returning a list of possible keywords for node types.
fn node_type_to_keyword(_type: &str) -> Option<String> {
    match _type {
        "ontology" => Some("Ontology:".to_string()),
        "import" => Some("Import:".to_string()),
        "class_frame" => Some("Class:".to_string()),
        "datatype_frame" => Some("Datatype:".to_string()),
        "object_property_frame" => Some("ObjectProperty:".to_string()),
        "annotation_property_frame" => Some("AnnotationProperty:".to_string()),
        "data_property_frame" => Some("DataProperty:".to_string()),
        "individual_frame" => Some("Individual:".to_string()),
        "annotation" => Some("Annotations:".to_string()),
        "datatype_equavalent_to" => Some("EquivalentTo:".to_string()),
        "sub_class_of" => Some("SubClassOf:".to_string()),
        "equavalent_to" => Some("EquivalentTo:".to_string()),
        "disjoint_with" => Some("DisjointWith:".to_string()),
        "disjoint_union_of" => Some("DisjointUnionOf:".to_string()),
        "has_key" => Some("HasKey:".to_string()),
        _ => None,
    }
}

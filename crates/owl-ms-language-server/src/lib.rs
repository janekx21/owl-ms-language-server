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

use itertools::Itertools;
use log::{debug, error, info, warn};
use once_cell::sync::Lazy;
use parking_lot::{MappedRwLockWriteGuard, RwLock, RwLockWriteGuard};
use position::Position;
use queries::{Rule, ALL_QUERIES, GRAMMAR, NODE_TYPES};
use range::Range;
use rope_provider::RopeProvider;
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::Mutex;
use tokio::task::{self};
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{self, *};
use tower_lsp::{Client, LanguageServer};
use tree_sitter::{
    InputEdit, Language, Node, Point, Query, QueryCursor, StreamingIterator, Tree, TreeCursor,
};
use web::{HttpClient, UreqClient};
use workspace::{lock_global_parser, node_text, trim_full_iri, InternalDocument, Workspace};

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

                // TODO #16 is this now correct?
                let frame_info = workspace.get_frame_info_recursive(&iri, &doc, &*self.http_client);

                if let Some(frame_info) = frame_info {
                    let locations = frame_info
                        .definitions
                        .iter()
                        .sorted_by_key(|l| {
                            if l.range == Range::ZERO {
                                100000 // No range? Then put this at the end
                            } else {
                                l.range.start.line
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

            let kws = try_keywords_at_position(&doc, pos);

            info!("The resultingn kws are {kws:#?}");

            // generate keyword list that can be applied. Note that this is missing some keywords because of limitations in the grammar.
            // let mut cursor = doc.tree.walk();
            // cursor_goto_pos(&mut cursor, pos);
            // while cursor.node().is_error() {
            //     if !cursor.goto_parent() {
            //         break; // we reached the root
            //     }
            // }

            // info!("Cursor node {:#?}", cursor.node());

            let pos_one_left = Position {
                line: pos.line,
                character: pos.character.checked_sub(1).unwrap_or_default(),
            };

            let node = doc
                .tree
                .root_node()
                .named_descendant_for_point_range(pos_one_left.into(), pos_one_left.into())
                .expect("The pos to be in at least one node");

            // // Goto prev siblin or ancestor that is not an error
            // while node.is_error() || node.kind() == "comment" {
            //     debug!("- Node is {node:#?}");
            //     if let Some(ps) = node.prev_named_sibling() {
            //         debug!("  - Go to sibling");
            //         node = ps;
            //     } else if let Some(p) = node.parent() {
            //         debug!("  - Go to parent");
            //         node = p;
            //     } else {
            //         break; // root reached
            //     }
            // }

            // // Goto last named decendent in that prev siblin
            // while node.named_child_count() > 1 {
            //     node = node.named_child(node.named_child_count() - 1).unwrap();
            // }

            // // Goto parent if the node is a keyword
            // match node.kind() {
            //     k if k.starts_with("keyword_") => {
            //         node = node.parent().unwrap();
            //     }
            //     _ => {}
            // }

            // // The node should now be the named node before the error

            // info!("Cursor node 2 (no err) {:#?} \n {}", node, node.to_sexp());

            // let mut node_kinds = vec![node.kind()];

            // match node.kind() {
            //     "class_frame" => {
            //         node_kinds.push("ontology");
            //     }
            //     "characteristics" => {
            //         node_kinds.push(node.parent().unwrap().kind());
            //         node_kinds.push(node.parent().unwrap().parent().unwrap().kind());
            //     }
            //     _ => {}
            // }

            // info!("Therefore nodekinds is {:#?}", node_kinds);

            // let mut possible_children_kinds = vec![];

            // for node_kind in node_kinds {
            //     if let Some(sn) = NODE_TYPES.get(node_kind) {
            //         for ele in &sn.children.types {
            //             possible_children_kinds.push(ele.type_.clone());
            //         }
            //     }
            // }

            // info!(
            //     "The resulting possible children kinds are {:#?}",
            //     possible_children_kinds
            // );

            // let keywords = possible_children_kinds
            // .iter()
            // .flat_map(|child_kind| node_kind_keywords(child_kind).map(|k| k.to_string()))
            let keywords_completion_items = kws.into_iter().map(|keyword| CompletionItem {
                label: keyword,
                kind: Some(CompletionItemKind::KEYWORD),
                ..Default::default()
            });

            let mut items = vec![];

            // Generate the list of iris that can be inserted.
            let partial_text = node_text(&node, &doc.rope).to_string();

            if node.kind() == "simple_iri" {
                let iris: Vec<CompletionItem> = workspace
                    .search_frame(&partial_text)
                    .iter()
                    .map(|(iri, _frame)| CompletionItem {
                        label: iri.clone(),
                        kind: Some(CompletionItemKind::REFERENCE),
                        // TODO #29 add details from the frame
                        ..Default::default()
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
            let query_source = tree_sitter_owl_ms::HIGHLIGHTS_QUERY;

            let query = Query::new(&LANGUAGE, query_source).expect("valid query expect");
            let mut query_cursor = QueryCursor::new();
            let matches =
                query_cursor.matches(&query, doc.tree.root_node(), RopeProvider::new(&doc.rope));

            let mut tokens = vec![];
            let mut last_start = Point { row: 0, column: 0 };

            let mut nodes = matches
                .map_deref(|m| m.captures)
                .flatten()
                .map(|c| {
                    (
                        c.node,
                        treesitter_highlight_capture_into_semantic_token_type_index(
                            query.capture_names()[c.index as usize],
                        ),
                    )
                })
                .collect_vec();

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
        "number" => 19,                // SemanticTokenType::NUMBER,
        "constant.builtin" => 8,       // SemanticTokenType::VARIABLE,
        "variable" => 8,               // SemanticTokenType::VARIABLE,
        "comment" => 17,               // SemanticTokenType::COMMENT,
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
fn node_kind_keywords(_type: &str) -> Option<&str> {
    match _type {
        // Top
        "prefix_declaration" => Some("Prefix:"),
        "ontology" => Some("Ontology:"),
        "import" => Some("Import:"),

        // Frametypes
        "class_frame" => Some("Class:"),
        "datatype_frame" => Some("Datatype:"),
        "object_property_frame" => Some("ObjectProperty:"),
        "annotation_property_frame" => Some("AnnotationProperty:"),
        "data_property_frame" => Some("DataProperty:"),
        "individual_frame" => Some("Individual:"),

        // Global
        "annotation" => Some("Annotations:"),

        // Class Frame
        "sub_class_of" => Some("SubClassOf:"),
        "class_equavalent_to" => Some("EquivalentTo:"),
        "class_disjoint_with" => Some("DisjointWith:"),
        "disjoint_union_of" => Some("DisjointUnionOf:"),
        "equivalent_to" => Some("EquivalentTo:"),
        "has_key" => Some("HasKey:"),

        // Datatype Frame
        "datatype_equavalent_to" => Some("EquivalentTo:"),

        // Object Property Frame
        "domain" => Some("Domain:"),
        "range" => Some("Range:"),
        "sub_property_of" => Some("SubPropertyOf:"),
        "object_property_equivalent_to" => Some("EquivalentTo:"),
        "object_property_disjoint_with" => Some("DisjointWith:"),
        "inverse_of" => Some("InverseOf:"),
        "characteristics" => Some("Characteristics:"),
        "sub_property_chain" => Some("SubPropertyChain:"),

        // _object_property_characteristic Keywords
        "keyword_functional" => Some("Functional"),
        "keyword_inverse_functional" => Some("InverseFunctional"),
        "keyword_reflexive" => Some("Reflexive"),
        "keyword_irreflexive" => Some("Irreflexive"),
        "keyword_symmetric" => Some("Symmetric"),
        "keyword_asymmetric" => Some("Asymmetric"),
        "keyword_transitive" => Some("Transitive"),

        // TODO extend!
        _ => None,
    }
}

fn try_keywords_at_position(document: &InternalDocument, cursor: Position) -> Vec<String> {
    let mut parser = lock_global_parser();
    let rope = document.rope.clone();
    let tree = document.tree.clone();

    let cursor_char_index = rope.line_to_char(cursor.line as usize) + cursor.character as usize;

    // let mut cursor_one_left = cursor.clone();
    // cursor_one_left.character -= 1;

    // let curor_node = tree
    //     .root_node()
    //     .descendant_for_point_range(cursor_one_left.into(), cursor_one_left.into())
    //     .unwrap();

    let line = rope.line(cursor.line as usize).to_string();
    let partial = word_before_character(cursor.character as usize, &line);

    // let partial = node_text(&curor_node, &rope).to_string();
    // let partial = curor_node.utf8_text(source_code.as_bytes()).unwrap();

    debug!("Cursor node text is {:?}", partial);

    let grammar = &GRAMMAR;
    let keywords = grammar
        .rules
        .iter()
        .filter_map(|item| match item {
            (rule_name, Rule::String { value }) if rule_name.starts_with("keyword_") => Some(value),
            _ => None,
        })
        .collect_vec();

    // let keywords = vec!["Class:", "DataClass:", "Functional"];
    let kws = keywords
        .iter()
        .filter(|k| k.starts_with(&partial))
        .collect_vec();

    debug!("Checking {} keywords", kws.len());

    kws.iter()
        .filter_map(|kw| {
            let mut rope_version = rope.clone();
            let change = kw[partial.len()..].to_string() + " a";


            let mut tree = tree.clone(); // This is fast

            // Must come before the rope is changed!
            let cursor_byte_index = rope_version.char_to_byte(cursor_char_index);

            rope_version.insert(cursor_char_index, &change);

            // Must come after rope changed!
            let new_end_byte = cursor_byte_index + change.len();
            let new_end_line = cursor.line as usize;
            let new_end_character =
                rope_version.byte_to_char(new_end_byte) - rope_version.line_to_char(new_end_line);

            let edit = InputEdit {
                // Old range is just a zero size range
                start_byte: cursor_char_index,
                start_position: cursor.into(),
                old_end_byte: cursor_char_index,
                old_end_position: cursor.into(),

                new_end_byte,
                new_end_position: Point {
                    row: new_end_line,
                    column: new_end_character,
                },
            };
            tree.edit(&edit);

            let rope_provider = RopeProvider::new(&rope_version);
            let new_tree = parser
                .parse_with_options(
                    &mut |byte_idx, _| rope_provider.chunk_callback(byte_idx),
                    Some(&tree),
                    None,
                )
                .expect("language to be set, no timeout to be used, no cancelation flag");

            debug!(
                "A possible source code version for {kw} (change is {change}) with rope version {rope_version}\nNew tree {:?}", new_tree.root_node().to_sexp());

            let mut cursor_one_left = cursor.to_owned();
            cursor_one_left.character = cursor_one_left.character.saturating_sub(1);
            let cursor_node_version = new_tree
                .root_node()
                .named_descendant_for_point_range(cursor_one_left.into(), cursor_one_left.into())
                .unwrap();

            debug!("{cursor_node_version:#?} is {}", cursor_node_version.kind());

            if cursor_node_version.kind().starts_with("keyword_")
                && !cursor_node_version.parent().unwrap().is_error()
            {
                debug!("Found possible keyword {kw}!");
                Some(kw.to_string())
            } else {
                debug!("{kw} is not possible");
                None
            }
        })
        .collect_vec()
}

/// Returns the word before the [`character`] position in the [`line`]
///
/// # Examples
///
/// ```
/// let word = owl_ms_language_server::word_before_character(25, "This is a line with multi words");
/// assert_eq!(word, "multi");
/// ```
pub fn word_before_character(character: usize, line: &str) -> String {
    line[..character]
        .chars()
        .rev()
        .take_while(|c| c.is_alphabetic())
        .collect_vec()
        .iter()
        .rev()
        .collect::<String>()
}

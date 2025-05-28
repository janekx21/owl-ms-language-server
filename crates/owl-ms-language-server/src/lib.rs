mod catalog;
pub mod debugging;
mod position;
mod queries;
mod range;
pub mod rope_provider;
#[cfg(test)]
mod tests;
mod workspace;

use core::panic;
use debugging::timeit;
use itertools::Itertools;
use log::{debug, error, info, warn};
use once_cell::sync::Lazy;
use position::Position;
use queries::{ALL_QUERIES, NODE_TYPES};
use range::Range;
use rope_provider::RopeProvider;
use std::collections::HashMap;
use tokio::sync::{MappedMutexGuard, Mutex, MutexGuard};
use tokio::task::{self};
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{self, *};
use tower_lsp::{Client, LanguageServer};
use tree_sitter::{InputEdit, Language, Node, Parser, Point, Query, QueryCursor, Tree, TreeCursor};
use tree_sitter_owl_ms::language;
use workspace::{gen_diagnostics, node_text, Document, Workspace};

// Constants

pub static LANGUAGE: Lazy<Language> = Lazy::new(language);

// Model
pub struct Backend {
    pub client: Client,
    parser: Mutex<Parser>, // stateful because of resume behavior after fail, timeout or cancellation
    position_encoding: Mutex<PositionEncodingKind>,
    workspaces: Mutex<Vec<Workspace>>,
}

impl Backend {
    /// Creates a new [`Backend`].
    pub fn new(client: Client, parser: Parser) -> Self {
        Backend {
            client,
            parser: Mutex::new(parser),
            position_encoding: PositionEncodingKind::UTF16.into(),
            workspaces: Mutex::new(vec![]),
        }
    }

    async fn lock_workspaces(&self) -> MappedMutexGuard<Vec<Workspace>> {
        MutexGuard::map(self.workspaces.lock().await, |w| w)
    }
}

/// This is the main language server implamentation. It is the entry point for all requests to the language server.
#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        self.client
            .log_message(MessageType::INFO, "initialize called.")
            .await;

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

        let mut pg = self.position_encoding.lock().await;
        *pg = encoding.clone();

        {
            let mut parser = self.parser.lock().await;

            let mut wf = self.workspaces.lock().await;
            *wf = params
                .workspace_folders
                .unwrap_or_default()
                .iter()
                .map(|wf| {
                    let w = Workspace::new(wf.clone());
                    w.load_catalog_documents(&mut parser);
                    w
                })
                .collect();
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
        let workspaces = self.workspaces.lock().await;
        let workspace_paths = workspaces
            .iter()
            .map(|w| {
                w.workspace_folder
                    .uri
                    .to_file_path()
                    .expect("Valid filepath")
                    .display()
                    .to_string()
            })
            .join(", ");
        info!(
            "Initialized Languag Server with workspaces: {}",
            if workspace_paths.is_empty() {
                "No Workspace".into()
            } else {
                workspace_paths
            }
        );
        self.client
            .log_message(MessageType::INFO, "server initialized!")
            .await;
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let url = params.text_document.uri;

        let workspace = self.find_workspace(&url).await;

        debug!("Found workspace! Folder: {:#?}", workspace.workspace_folder);

        debug!(
            "did_open at {} with version {}",
            url.clone(),
            params.text_document.version.clone()
        );

        let mut parser_guard = self.parser.lock().await;

        let document = Document::new(
            url.clone(),
            params.text_document.version,
            params.text_document.text,
            &mut parser_guard,
        );

        self.client
            .publish_diagnostics(
                url.clone(),
                document.diagnostics.clone(),
                Some(document.version),
            )
            .await;

        let document = workspace.insert_document(document);

        debug!("Inserted document");

        self.resolve_imports(&document, &workspace, &mut parser_guard)
            .await;
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
        let workspace = self.find_workspace(url).await;

        if let Some(mut document) = workspace.document_map.get_mut(&params.text_document.uri) {
            if document.version >= params.text_document.version {
                return; // no change needed
            }

            if params
                .content_changes
                .iter()
                .any(|change| change.range.is_none())
            {
                // Change the whole file
                panic!("Whole file changes are not supported yet");
            }

            // This range is relative to the *old* document not the new one
            let change_ranges = params
                .content_changes
                .iter()
                .rev() // See https://github.com/helix-editor/helix/blob/0815b52e0959e21ec792ea41d508a050b552f850/helix-core/src/syntax.rs#L1293C1-L1297C26
                .map(|change| {
                    if let Some(range) = change.range {
                        let old_range: Range = range.into();
                        let start_char = document
                            .rope
                            .try_line_to_char(old_range.start.line as usize)
                            .expect("line_idx out of bounds")
                            + (old_range.start.character as usize);

                        let old_end_char = document
                            .rope
                            .try_line_to_char(old_range.end.line as usize)
                            .expect("line_idx out of bounds")
                            + (old_range.end.character as usize); // exclusive

                        // must come before the rope is changed!
                        let old_end_byte = document.rope.char_to_byte(old_end_char);

                        // rope replace
                        timeit("rope operations", || {
                            document.rope.remove(start_char..old_end_char);
                            document.rope.insert(start_char, &change.text);
                        });

                        // this must come after the rope was changed!
                        let start_byte = document.rope.char_to_byte(start_char);
                        let new_end_byte = start_byte + change.text.len();
                        let new_end_line = document.rope.byte_to_line(new_end_byte);
                        let new_end_character = document.rope.byte_to_char(new_end_byte)
                            - document.rope.line_to_char(new_end_line);

                        let edit = InputEdit {
                            start_byte,
                            old_end_byte,
                            new_end_byte: start_byte + change.text.len(),
                            start_position: old_range.start.into(),
                            old_end_position: old_range.end.into(),
                            new_end_position: Point {
                                row: new_end_line,
                                column: new_end_character,
                            },
                        };
                        timeit("tree edit", || document.tree.edit(&edit));

                        document.version = params.text_document.version;

                        let new_range = Range {
                            start: edit.start_position.into(),
                            end: edit.new_end_position.into(),
                        };

                        (old_range, edit, new_range)
                    } else {
                        unreachable!("Change should have range {:#?}", change);
                    }
                })
                .collect::<Vec<(Range, InputEdit, Range)>>();

            debug!("Change ranges {:#?}", change_ranges);

            let rope_provider = RopeProvider::new(&document.rope);

            let tree = {
                let mut parser_guard = self.parser.lock().await;
                timeit("parsing", || {
                    parser_guard
                        .parse_with(
                            &mut |byte_idx, _| rope_provider.chunk_callback(byte_idx),
                            Some(&document.tree),
                        )
                        .expect("language to be set, no timeout to be used, no cancelation flag")
                })
            };
            debug!("New tree {}", tree.root_node().to_sexp());
            document.tree = tree;

            for (_, _, new_range) in change_ranges.iter() {
                // TODO #32 prune
                // document
                //     .frame_infos
                //     .retain(|k, v| !range_overlaps(&v.range.into(), &range));

                let frame_infos = timeit("gen_class_iri_label_map", || {
                    document.gen_frame_infos(Some(new_range))
                });

                document.frame_infos.extend(frame_infos);
            }

            // TODO #30 prune
            // Remove all old diagnostics with an overlapping range. They will need to be recreated
            // for (_, _, old_range) in change_ranges.iter() {
            //     document
            //         .diagnostics
            //         .retain(|d| !lines_overlap(&d.range.into(), old_range));
            // }
            // Move all other diagnostics
            // for diagnostic in &mut document.diagnostics {
            //     for (new_range, edit, old_range) in change_ranges.iter() {
            //         let mut range_to_end = *old_range;
            //         range_to_end.end = Position {
            //             line: u32::MAX,
            //             character: u32::MAX,
            //         };
            //         if lines_overlap(&diagnostic.range.into(), &range_to_end) {
            //             debug!("old {} -> new {}", old_range, new_range);
            //             let delta = new_range.end.line as i32 - old_range.end.line as i32;
            //             diagnostic.range.start.line =
            //                 (diagnostic.range.start.line as i32 + delta) as u32;
            //             diagnostic.range.start.line =
            //                 (diagnostic.range.end.line as i32 + delta) as u32;
            //         }
            //     }
            // }
            // for (_, _, _) in change_ranges.iter() {
            //     let cursor = document.tree.walk();
            //     // TODO #30
            //     // while range_exclusive_inside(new_range, &cursor.node().range().into()) {
            //     //     if cursor
            //     //         .goto_first_child_for_point(new_range.start.into())
            //     //         .is_none()
            //     //     {
            //     //         break;
            //     //     }
            //     // }
            //     // cursor.goto_parent();
            //     let node_that_has_change = cursor.node();
            //     drop(cursor);
            //     // while range_overlaps(&ts_range_to_lsp_range(cursor.node().range()), &range) {}
            //     // document.diagnostics =
            //     let additional_diagnostics = timeit("did_change > gen_diagnostics", || {
            //         gen_diagnostics(&node_that_has_change)
            //     })
            //     .into_iter();
            //     // .filter(|d| lines_overlap(&d.range.into(), new_range)); // should be exclusive to other diagnostics
            //     document.diagnostics.extend(additional_diagnostics);
            // }

            // TODO #30 replace with above
            document.diagnostics = timeit("did_change > gen_diagnostics", || {
                gen_diagnostics(&document.tree.root_node())
            });

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
        debug!(
            "hover at {:?} in file {}",
            params.text_document_position_params.position, &url
        );

        let workspace = self.find_workspace(&url).await;

        Ok(workspace.document_map.get(&url).map(|document| {
            let pos = params.text_document_position_params.position.into();
            let node = deepest_named_node_at_pos(&document.tree, pos);
            let info = workspace.node_info(&node, &document);
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

        let mut parser = self.parser.lock().await;

        debug!(
            "inlay_hint at {}:{range}",
            url.path_segments().unwrap().last().unwrap()
        );

        let workspace = self.find_workspace(&url).await;
        let document = if let Some(doc) = workspace.document_map.get(&url) {
            doc
        } else {
            return Err(tower_lsp::jsonrpc::Error::invalid_params(
                "Document with that URL not found",
            ));
        };

        let annotations =
            document.query_with_imports(&ALL_QUERIES.annotation_query, &workspace, &mut parser);

        let matches = document.query_range(&ALL_QUERIES.iri_query, range);
        let hints = matches
            .into_iter()
            .flat_map(|match_| match_.captures)
            .filter_map(|capture| {
                let iri = capture.node.text;

                let label = annotations
                    .iter()
                    .filter_map(|m| match &m.captures[..] {
                        [frame_iri, annoation_iri, literal] => {
                            if frame_iri.node.text == iri && annoation_iri.node.text == "rdfs:label"
                            {
                                Some(literal.node.text_trimmed())
                            } else {
                                None
                            }
                        }
                        _ => unreachable!(),
                    })
                    .join(", ");

                if label.is_empty() {
                    None
                } else {
                    Some(InlayHint {
                        position: capture.node.range.end.into(),
                        label: InlayHintLabel::String(label),
                        kind: None,
                        text_edits: None,
                        tooltip: None,
                        padding_left: Some(true),
                        padding_right: None,
                        data: None,
                    })
                }

                // workspace.get_frame_info(&iri).map(|fi| InlayHint {
                //     position: capture.node.range.end.into(),
                //     label: InlayHintLabel::String(fi.label()),
                //     kind: None,
                //     text_edits: None,
                //     tooltip: None,
                //     padding_left: Some(true),
                //     padding_right: None,
                //     data: None,
                // })
            })
            .collect_vec();
        Ok(Some(hints))
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        debug!(
            "goto_definition at {}",
            params.text_document_position_params.text_document.uri
        );
        let uri = params.text_document_position_params.text_document.uri;
        let workspace = self.find_workspace(&uri).await;
        let maybe_doc = workspace.document_map.get(&uri);
        if let Some(doc) = maybe_doc {
            let pos: Position = params.text_document_position_params.position.into();

            let leaf_node = deepest_named_node_at_pos(&doc.tree, pos);
            if ["full_iri", "simple_iri", "abbreviated_iri"].contains(&leaf_node.kind()) {
                let iri = node_text(&leaf_node, &doc.rope).to_string();

                debug!("Try goto definition of {}", iri);

                let frame_info = workspace.get_frame_info(&iri);

                debug!("Found frame info {:#?}", frame_info);

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
        let workspace = self.find_workspace(&url).await;
        let doc = workspace.document_map.get(&url).unwrap();
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
        let workspace = self.find_workspace(&url).await;
        let doc = workspace.document_map.get(&url);
        let pos: Position = params.text_document_position.position.into();

        Ok(doc.map(|doc| {
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
        let workspace = self.find_workspace(&uri).await;
        if let Some(doc) = workspace.document_map.get(&uri) {
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
        let workspaces = self.workspaces.lock().await;
        let c = workspaces
            .iter()
            .flat_map(|w| {
                w.document_map.iter()
                // .map(|d| d.value().clone())
                // .flat_map(|d| d.frame_infos.iter().map(|i| i.value())) //.iter().map(|i| i.value()))
            })
            .collect_vec();
        let d = c.iter().flat_map(|a| a.frame_infos.iter()).collect_vec();

        info!(
            "All frame infos: {:#?}",
            d.iter().map(|v| v.value()).collect_vec()
        );

        let symbols = d
            .iter()
            .filter(|i| i.iri.contains(query.as_str()))
            .flat_map(|fi| {
                fi.definitions.iter().map(|definition| SymbolInformation {
                    name: fi.iri.clone(),
                    kind: fi.frame_type.into(),
                    tags: Some(vec![]), // TODO #38 add support for depricated entities
                    deprecated: None,
                    location: definition.clone().into(),
                    container_name: None,
                })
            })
            .collect_vec();

        // .flat_map(|d| d.value().frame_infos.iter());
        Ok(Some(symbols))
    }

    async fn shutdown(&self) -> Result<()> {
        info!("Shutdown");
        Ok(())
    }
}

impl Backend {
    /// LSP clients can open multiple workspaces at a time. To find the one that a particular file is located in
    /// this functions searches in all openend workspaces.
    async fn has_document(&self, url: &Url) -> bool {
        let workspaces = self.lock_workspaces().await;
        workspaces.iter().any(|w| w.document_map.contains_key(url))
    }

    async fn find_workspace<'a>(&'a self, url: &Url) -> MappedMutexGuard<'a, Workspace> {
        let mut workspaces = self.workspaces.lock().await;

        let maybe_workspace = workspaces.iter().find(|workspace| {
            workspace.document_map.contains_key(url)
                || workspace
                    .catalogs
                    .iter()
                    .any(|catalog| catalog.contains(&url.to_file_path().unwrap()))
        });

        if maybe_workspace.is_none() {
            info!("Workspace for {url} could not be found. Creating a new one.");
            // Create a workspace that is a single file
            let mut file_path = url.to_file_path().expect("URL should be a filepath");
            file_path.pop();
            let workspace = Workspace::new(WorkspaceFolder {
                uri: Url::from_file_path(file_path.clone()).expect("Valid URL from filepath"), // TODO do i need the parent folder fot that?
                name: "Single File".into(),
            });
            workspaces.push(workspace);
        }

        MutexGuard::map(workspaces, |f| {
            f.iter_mut()
                .find(|w| w.could_contain(url))
                .expect("The file to be located in a workspace, but it was not.")
        })
    }

    async fn resolve_imports(
        &self,
        document: &Document,
        workspace: &Workspace,
        parser: &mut Parser,
    ) {
        let urls = document
            .query(&ALL_QUERIES.import_query)
            .iter()
            .filter_map(|match_| match &match_.captures[..] {
                [iri] => {
                    Url::parse(iri.node.text.trim_end_matches(">").trim_start_matches("<")).ok()
                }
                _ => unimplemented!(),
            })
            .collect_vec();

        for url in urls {
            info!("Resolving url {url}");
            let _ = workspace
                .resolve_url_to_document(&url, parser)
                .inspect_err(|e| error!("Error while resolving imports: {e}"));
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

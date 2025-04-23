mod catalog;
mod debugging;
mod position;
mod range;
mod rope_provider;
#[cfg(test)]
mod tests;
mod workspace;

use clap::Parser as ClapParser;
use dashmap::DashMap;
use debugging::timeit;
use itertools::Itertools;
use log::{debug, error, info, LevelFilter};
use once_cell::sync::Lazy;
use position::Position;
use range::{range_exclusive_inside, range_overlaps, Range};
use rope_provider::RopeProvider;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt::Display;
use std::path::Path;
use std::{env, fs};
use tokio::sync::{MappedMutexGuard, Mutex, MutexGuard};
use tokio::task::{self};
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{self, *};
use tower_lsp::{Client, LanguageServer, LspService, Server};
use tree_sitter::{InputEdit, Language, Node, Parser, Point, Query, QueryCursor, Tree, TreeCursor};
use tree_sitter_owl_ms::language;
use workspace::{gen_diagnostics, gen_iri_info_map, node_text, Document, Workspace};

// Constants

static FULL_REPLACE_THRESHOLD: usize = 10;

static LANGUAGE: Lazy<Language> = Lazy::new(language);
static NODE_TYPES: Lazy<DashMap<String, StaticNode>> = Lazy::new(|| {
    let node_types: Vec<StaticNode> =
        serde_json::from_str(tree_sitter_owl_ms::NODE_TYPES).expect("valid node types");

    DashMap::<String, StaticNode>::from_iter(
        node_types
            .iter()
            .map(|node| (node._type.clone(), (*node).clone())),
    )
});

// Model

/// A language server for the owl 2 manchester syntax language
#[derive(ClapParser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Use stdin and stdout for communication
    #[arg(long, default_value_t = true)]
    stdio: bool,
}

struct Backend {
    client: Client,
    parser: Mutex<Parser>, // stateful because of resume behavior after fail, timeout or cancellation
    position_encoding: Mutex<PositionEncodingKind>,
    workspaces: Mutex<Vec<Workspace>>,
}

impl Backend {
    /// Creates a new [`Backend`].
    fn new(client: Client, parser: Parser) -> Self {
        Backend {
            client,
            parser: Mutex::new(parser),
            // document_map: DashMap::new(),
            position_encoding: PositionEncodingKind::UTF16.into(),
            // workspace_folders: Mutex::new(vec![]),
            // catalogs: Mutex::new(vec![]),
            // frame_infos: DashMap::new(),
            workspaces: Mutex::new(vec![]),
        }
    }

    async fn lock_workspaces(&self) -> MappedMutexGuard<Vec<Workspace>> {
        MutexGuard::map(self.workspaces.lock().await, |w| w)
    }
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[serde(rename_all = "camelCase")]
struct StaticNode {
    #[serde(rename = "type")]
    _type: String,
    named: bool,
    #[serde(default)]
    children: StaticNodeChildren,
}

#[derive(Serialize, Deserialize, Default, Clone, Debug)]
#[serde(rename_all = "camelCase")]
struct StaticNodeChildren {
    multiple: bool,
    required: bool,
    types: Vec<StaticNode>,
}

/// taken from https://www.w3.org/TR/owl2-syntax/#Entity_Declarations_and_Typing
#[derive(Clone, Copy, Eq, PartialEq, Debug)]
enum FrameType {
    Class,
    DataType,
    ObjectProperty,
    DataProperty,
    AnnotationProperty,
    Individual,
    Ontology,
    Invalid, // The frame type of an IRI that has no valid frame
}

#[tokio::main]
async fn main() {
    let _ = Args::parse();

    let mut log_file_path = env::temp_dir();
    log_file_path.push("owl-ms-lanugage-server.log");
    let log_file_path = log_file_path.as_path();
    simple_logging::log_to_file(log_file_path, LevelFilter::Debug).unwrap_or_else(|_| {
        panic!(
            "Logging file could not be created at {}",
            log_file_path.to_str().unwrap_or("[invalid unicode]")
        )
    });

    std::panic::set_hook(Box::new(|info| {
        error!("paniced with {}", info);
    }));

    let mut parser = Parser::new();
    parser.set_language(*LANGUAGE).unwrap();

    let (service, socket) = LspService::new(|client| Backend::new(client, parser));

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();
    Server::new(stdin, stdout, socket).serve(service).await;
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

        let mut wf = self.workspaces.lock().await;
        *wf = params
            .workspace_folders
            .unwrap_or_default()
            .iter()
            .map(|wf| Workspace::new(wf.clone()))
            .collect();

        // let mut catalogs_guard = self.catalogs.lock().await;
        // for folder in wf.iter() {
        //     let read_catalogs = read_catalog(folder.uri.clone());
        //     catalogs_guard.extend(read_catalogs);
        // }

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

            let change_ranges = params
                .content_changes
                .iter()
                .map(|change| {
                    if let Some(range) = change.range {
                        let range: Range = range.into();
                        let start_char = document
                            .rope
                            .try_line_to_char(range.start.line as usize)
                            .expect("line_idx out of bounds")
                            + (range.start.character as usize);

                        let old_end_char = document
                            .rope
                            .try_line_to_char(range.end.line as usize)
                            .expect("line_idx out of bounds")
                            + (range.end.character as usize); // exclusive

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
                            start_position: range.start.into(),
                            old_end_position: range.end.into(),
                            new_end_position: Point {
                                row: new_end_line,
                                column: new_end_character,
                            },
                        };
                        timeit("tree edit", || document.tree.edit(&edit));

                        document.version = params.text_document.version;
                        range
                    } else {
                        document.tree.root_node().range().into()
                    }
                })
                .collect::<Vec<Range>>();

            let mut parser_guard = self.parser.lock().await;
            parser_guard.reset();
            let tree = timeit("parsing", || {
                parser_guard
                    .parse(document.rope.to_string(), Some(&document.tree))
                    .expect("language to be set, no timeout to be used, no cancelation flag")
            });
            document.tree = tree;

            let use_full_replace = params.content_changes.len() > FULL_REPLACE_THRESHOLD;
            if use_full_replace {
                document.frame_infos = gen_iri_info_map(&document.tree, &document.rope, None);

                let diagnostics = gen_diagnostics(&document.tree.root_node());
                document.diagnostics = diagnostics.clone();

                self.client
                    .publish_diagnostics(params.text_document.uri, diagnostics, None)
                    .await;
                return;
            }

            for range in change_ranges.iter() {
                let iri_info_map = timeit("gen_class_iri_label_map", || {
                    gen_iri_info_map(&document.tree, &document.rope, Some(range))
                });

                // TODO prune
                // document
                //     .iri_info_map
                //     .retain(|k, v| !range_overlaps(&v.range.into(), &range));

                document.frame_infos.extend(iri_info_map);
            }

            for range in change_ranges.iter() {
                // diagnostics
                // TODO this does not work correctly
                document
                    .diagnostics
                    .retain(|d| !range_overlaps(&d.range.into(), range));

                let mut cursor = document.tree.walk();

                while range_exclusive_inside(range, &cursor.node().range().into()) {
                    if cursor
                        .goto_first_child_for_point(range.start.into())
                        .is_none()
                    {
                        break;
                    }
                }
                cursor.goto_parent();
                let node_that_has_change = cursor.node();
                drop(cursor);
                // while range_overlaps(&ts_range_to_lsp_range(cursor.node().range()), &range) {}
                // document.diagnostics =
                let additional_diagnostics = timeit("did_change > gen_diagnostics", || {
                    gen_diagnostics(&node_that_has_change)
                })
                .into_iter()
                .filter(|d| range_overlaps(&d.range.into(), range)); // should be exclusive to other diagnostics

                document.diagnostics.extend(additional_diagnostics);
            }

            let uri = params.text_document.uri.clone();
            let d = document.diagnostics.clone();
            let c = self.client.clone();
            task::spawn(async move {
                c.publish_diagnostics(uri, d, None).await;
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
        let workspace = self.find_workspace(&params.text_document.uri).await;
        workspace.document_map.remove(&params.text_document.uri);
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let url = params.text_document_position_params.text_document.uri;
        debug!(
            "hover at {:?} in file {}",
            params.text_document_position_params.position, &url
        );

        self.client.log_message(MessageType::INFO, "hover").await;

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
        debug!("inlay_hint at {}", params.text_document.uri);

        let url = params.text_document.uri;
        let workspace = self.find_workspace(&url).await;

        // let mut combined_iri_info_map: DashMap<Iri, FrameInfo> = DashMap::new();
        // for doc in self.document_map.iter() {
        //     combined_iri_info_map.extend(doc.value().iri_info_map.clone());
        // }

        Ok(workspace.document_map.get(&url).map(|document| {
            let full_iri_query =
                Query::new(*LANGUAGE, "[(full_iri) (simple_iri) (abbreviated_iri)]@iri").unwrap();

            let range: Range = params.range.into();

            let mut query_cursor = QueryCursor::new();
            query_cursor.set_point_range(range.into());
            query_cursor
                .matches(
                    &full_iri_query,
                    document.tree.root_node(),
                    RopeProvider::new(&document.rope),
                )
                .map(|match_| match_.captures[0])
                .filter_map(|capture| {
                    let iri = &node_text(&capture.node, &document.rope).to_string();
                    workspace.get_frame_info(iri).map(|info| {
                        let node = capture.node;
                        // inlay hints are at the end of the iri
                        let end: Position = node.end_position().into();

                        // TODO write a tooltip
                        let label = info.label();
                        InlayHint {
                            position: end.into(),
                            label: InlayHintLabel::String(label),
                            kind: None,
                            text_edits: None,
                            tooltip: None,
                            padding_left: Some(true),
                            padding_right: None,
                            data: None,
                        }
                    })
                })
                .collect::<Vec<InlayHint>>()
        }))
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
                let parent_kind = leaf_node.parent().unwrap().kind();
                let iri = node_text(&leaf_node, &doc.rope);

                // TODO other frame types
                let query = match parent_kind {
                    "class_iri" => Some(format!(
                        "(class_frame
                            . (class_iri)@iri
                            (#eq? @iri \"{}\")
                        )@frame",
                        iri
                    )),
                    "annotation_property_iri" => Some(format!(
                        "(annotation_property_frame
                            . (annotation_property_iri)@iri
                            (#eq? @iri \"{}\")
                        )@frame",
                        iri
                    )),
                    "object_property_iri" => Some(format!(
                        "(object_property_frame
                            . (object_property_iri)@iri
                            (#eq? @iri \"{}\")
                        )@frame",
                        iri
                    )),
                    "data_property_iri" => Some(format!(
                        "(data_property_frame
                            . (data_property_iri)@iri
                            (#eq? @iri \"{}\")
                        )@frame",
                        iri
                    )),
                    "individual_iri" => Some(format!(
                        "(individual_frame
                            . (individual_iri)@iri
                            (#eq? @iri \"{}\")
                        )@frame",
                        iri
                    )),
                    "datatype_iri" => Some(format!(
                        "(datatype_frame
                            . (datatype_iri)@iri
                            (#eq? @iri \"{}\")
                        )@frame",
                        iri
                    )),
                    _ => None,
                };
                debug!("{:?}", query);

                Ok(query.and_then(|query_text| {
                    let definition_query = Query::new(*LANGUAGE, query_text.as_str())
                        .expect("valid query syntax expect");

                    let root = doc.tree.root_node();
                    let mut query_cursor = QueryCursor::new();

                    let rp = RopeProvider::new(&doc.rope);
                    let mut matches = query_cursor.matches(&definition_query, root, rp);

                    matches.next().map(|m| {
                        let range: Range = m.captures[0].node.range().into();
                        GotoDefinitionResponse::Scalar(Location {
                            uri,
                            range: range.into(),
                        })
                    })
                }))
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
        // let mut combined_iri_info_map: DashMap<Iri, FrameInfo> = DashMap::new();
        // for doc in wor.document_map.iter() {
        //     combined_iri_info_map.extend(doc.value().iri_info_map.clone());
        // }
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
                .filter_map(|child| node_type_to_keyword(child._type.as_ref()))
                .map(|keyword| CompletionItem {
                    label: keyword,
                    ..Default::default()
                });

            let mut items = vec![
                // CompletionItem {
                //     label: parent_kind.to_string(),
                //     ..Default::default()
                // }
            ];

            // Generate the list of iris that can be inserted.
            let child_node_types: Vec<String> = possible_children
                .iter()
                .filter_map(|child| node_type_to_keyword(child._type.as_ref()))
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

            // debug!(
            //     "matches {:?}",
            //     matches
            //         .flat_map(|m| m.captures)
            //         .map(|c| c.index)
            //         .collect::<Vec<u32>>()
            // );

            //treesitter_highlight_capture_into_sematic_token_type

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

                // debug!("token {:?}", token);

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

    // async fn semantic_tokens_full_delta(
    //     &self,
    //     params: SemanticTokensDeltaParams,
    // ) -> Result<Option<SemanticTokensFullDeltaResult>> {
    //     let _ = params;
    //     debug!("Got a textDocument/semanticTokens/full/delta request, but it is not implemented");
    //     Ok(None)
    // }

    // async fn semantic_tokens_range(
    //     &self,
    //     params: SemanticTokensRangeParams,
    // ) -> Result<Option<SemanticTokensRangeResult>> {
    //     Ok(Some(SemanticTokensRangeResult::Tokens(SemanticTokens {
    //         result_id: None,
    //         data: vec![
    //             SemanticToken {
    //                 delta_line: 0,
    //                 delta_start: 0,
    //                 length: 50,
    //                 token_type: 0, // index into types
    //                 token_modifiers_bitset: 0,
    //             },
    //             SemanticToken {
    //                 delta_line: 0,
    //                 delta_start: 0,
    //                 length: 50,
    //                 token_type: 1,
    //                 token_modifiers_bitset: 0,
    //             },
    //         ],
    //     })))
    // }

    async fn shutdown(&self) -> Result<()> {
        debug!("shutdown");
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
        let mut guard = self.workspaces.lock().await;
        let found = guard.iter().any(|w| w.could_contain(url));
        if !found {
            // Create a workspace that is a single file
            let mut file_path = url.to_file_path().expect("URL should be a filepath");
            file_path.pop();
            let workspace = Workspace::new(WorkspaceFolder {
                uri: Url::from_file_path(file_path.clone()).expect("Valid URL from filepath"), // TODO do i need the parent folder fot that?
                name: "Single File".into(),
            });
            guard.push(workspace);
        }
        MutexGuard::map(guard, |f| {
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
        let urls = query_document(document, &IMPORT_QUERY)
            .iter()
            .flat_map(|match_| &match_.captures)
            .map(|capture| node_text(&capture.node, &document.rope).to_string())
            .filter_map(|iri_text| {
                Url::parse(iri_text.trim_end_matches(">").trim_start_matches("<")).ok()
            })
            .collect_vec();

        // let workspace = self.find_workspace(&document.uri).await;

        for url in urls {
            // TODO #8 filepath
            if workspace.document_map.contains_key(&url) {
                debug!("This document is already in the backend. URL: {}", url);
            } else {
                debug!("Try to resolve URL {}", url);
                // Find uri in catalog
                debug!("{:?}", workspace.catalogs);
                for c in &workspace.catalogs {
                    for u in &c.uri {
                        debug!("url {}  u.name {}", url, u.name);
                        if u.name == url.to_string() {
                            // This is an import of a local file!
                            let path = Path::new(&c.locaton).parent().unwrap().join(&u.uri);
                            debug!(
                                "Found matching local file {:?} loading from path {}",
                                u,
                                path.display()
                            );
                            let ontology_text = fs::read_to_string(path).unwrap();
                            let document = Document::new(url.clone(), -1, ontology_text, parser);
                            workspace.insert_document(document);
                        }
                    }
                }

                // debug!("Resolving import of {}", url.clone());
                // let ontology_text = ureq::get(url.to_string())
                //     .call()
                //     .unwrap()
                //     .body_mut()
                //     .read_to_string()
                //     .unwrap();

                // debug!(
                //     "Inserted imported document URL: {}, number of iris: {}, number of diagnostics: {}",
                //     url,
                //     document.iri_info_map.iter().count(),
                //     document.diagnostics.len()
                // );
                // }
                // } else {
                // warn!("The imported URL type is not supported. URL: {}", url);
                // }

                // debug!("Resolved import {} to\n{}", url, resolved_import);
            }
        }
    }
}

static IMPORT_QUERY: Lazy<Query> = Lazy::new(|| {
    Query::new(
        *LANGUAGE,
        "(import [(full_iri) (simple_iri) (abbreviated_iri)]@iri)",
    )
    .unwrap()
});

fn query_document<'a>(
    document: &'a Document,
    // source: &str,
    query: &'a Query,
) -> Vec<UnwrappedQueryMatch<'a>> {
    // let query = Query::new(*LANGUAGE, source).unwrap();
    // let query_ref: &'a Query = &query;

    let mut query_cursor = QueryCursor::new();
    let rope_provider: RopeProvider<'a> = RopeProvider::new(&document.rope);
    query_cursor
        .matches(query, document.tree.root_node(), rope_provider)
        .map(|m| UnwrappedQueryMatch::<'a> {
            _pattern_index: m.pattern_index,
            _id: m.id(),
            captures: m
                .captures
                .iter()
                .map(|c| UnwrappedQueryCapture::<'a> {
                    node: c.node,
                    _index: c.index,
                })
                .collect_vec(),
        })
        .collect_vec()
}

#[derive(Debug)]
struct UnwrappedQueryMatch<'a> {
    _pattern_index: usize,
    captures: Vec<UnwrappedQueryCapture<'a>>,
    _id: u32,
}

#[derive(Debug)]
struct UnwrappedQueryCapture<'a> {
    node: Node<'a>,
    _index: u32,
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

// fn semantic_token_type_into_index(type_: SemanticTokenType) -> u32 {
//     match type_ {
//         SemanticTokenType::NAMESPACE => 0,
//         SemanticTokenType::TYPE => 1,
//         SemanticTokenType::CLASS => 2,
//         SemanticTokenType::ENUM => 3,
//         SemanticTokenType::INTERFACE => 4,
//         SemanticTokenType::STRUCT => 5,
//         SemanticTokenType::TYPE_PARAMETER => 6,
//         SemanticTokenType::PARAMETER => 7,
//         SemanticTokenType::VARIABLE => 8,
//         SemanticTokenType::PROPERTY => 9,
//         SemanticTokenType::ENUM_MEMBER => 10,
//         SemanticTokenType::EVENT => 11,
//         SemanticTokenType::FUNCTION => 12,
//         SemanticTokenType::METHOD => 13,
//         SemanticTokenType::MACRO => 14,
//         SemanticTokenType::KEYWORD => 15,
//         SemanticTokenType::MODIFIER => 16,
//         SemanticTokenType::COMMENT => 17,
//         SemanticTokenType::STRING => 18,
//         SemanticTokenType::NUMBER => 19,
//         SemanticTokenType::REGEXP => 20,
//         SemanticTokenType::OPERATOR => 21,
//         SemanticTokenType::DECORATOR => 22,
//     }
// }

// static SEMANTIC_TOKEN_TYPES: Lazy<HashMap<usize, SemanticTokenType>> = Lazy::new(|| {
//     let tokens = vec![
//         SemanticTokenType::NAMESPACE,
//         SemanticTokenType::TYPE,
//         SemanticTokenType::CLASS,
//         SemanticTokenType::ENUM,
//         SemanticTokenType::INTERFACE,
//         SemanticTokenType::STRUCT,
//         SemanticTokenType::TYPE_PARAMETER,
//         SemanticTokenType::PARAMETER,
//         SemanticTokenType::VARIABLE,
//         SemanticTokenType::PROPERTY,
//         SemanticTokenType::ENUM_MEMBER,
//         SemanticTokenType::EVENT,
//         SemanticTokenType::FUNCTION,
//         SemanticTokenType::METHOD,
//         SemanticTokenType::MACRO,
//         SemanticTokenType::KEYWORD,
//         SemanticTokenType::MODIFIER,
//         SemanticTokenType::COMMENT,
//         SemanticTokenType::STRING,
//         SemanticTokenType::NUMBER,
//         SemanticTokenType::REGEXP,
//         SemanticTokenType::OPERATOR,
//         SemanticTokenType::DECORATOR,
//     ];
//     let mut hash_map = HashMap::new();

//     for (index, item) in tokens.iter().enumerate() {
//         hash_map.insert(index, item);
//     }
//     hash_map.shrink_to_fit();
// });

// Implementations for Structs

// fn global_resolve_iri(workspace: &Workspace, iri: &String) -> String {
//     // let mut combined_iri_info_map: DashMap<Iri, FrameInfo> = DashMap::new();
//     // for doc in backend.document_map.iter() {
//     //     combined_iri_info_map.extend(doc.value().iri_info_map.clone());
//     // }

//     workspace
//         .get_frame_info(iri)
//         .and_then(|iri_info| iri_info.label())
//         .unwrap_or(iri.clone())
// }

fn _log_to_client(client: &Client, msg: String) -> task::JoinHandle<()> {
    let c = client.clone();
    task::spawn(async move {
        c.log_message(MessageType::INFO, msg).await;
    })
}

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

impl Display for FrameType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = match self {
            FrameType::Class => "Class",
            FrameType::DataType => "Data Type",
            FrameType::ObjectProperty => "Object Property",
            FrameType::DataProperty => "Data Property",
            FrameType::AnnotationProperty => "Annotation Property",
            FrameType::Individual => "Named Individual",
            FrameType::Ontology => "Ontology",
            FrameType::Invalid => "Invalid Frame Type",
        };
        write!(f, "{name}")
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
        "disjoint_union_of" => Some("DisjointUnionOf".to_string()),
        "has_key" => Some("HasKey:".to_string()),
        _ => None,
    }
}

#![feature(async_closure, iter_intersperse)]
use dashmap::DashMap;
use log::{error, info, log, LevelFilter};
use once_cell::sync::Lazy;
use ropey::{Rope, RopeSlice};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt::Display;
use tokio::sync::Mutex;
use tokio::task;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{self, *};
use tower_lsp::{Client, LanguageServer, LspService, Server};
use tree_sitter::{
    InputEdit, Language, Node, Parser, Point, Query, QueryCursor, TextProvider, Tree, TreeCursor,
};
use tree_sitter_owl_ms::language;

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

// static FRAME_TYPES: [&str; 6] = [
//     "datatype_frame",
//     "class_frame",
//     "object_property_frame",
//     "data_property_frame",
//     "annotation_property_frame",
//     "individual_frame",
// ];

// static FRAME_QUERY_SOURCE: Lazy<String> = Lazy::new(|| {
//     FRAME_TYPES
//         .iter()
//         .map(|ft| format!("({ft})"))
//         .intersperse(" ".to_string())
//         .collect::<String>()
// });

type Iri = String;

#[derive(Clone, Copy)]
struct Position {
    line: u32,
    character: u32,
}

impl From<tower_lsp::lsp_types::Position> for Position {
    fn from(value: tower_lsp::lsp_types::Position) -> Self {
        Position {
            line: value.line,
            character: value.character,
        }
    }
}

impl Into<tower_lsp::lsp_types::Position> for Position {
    fn into(self) -> tower_lsp::lsp_types::Position {
        tower_lsp::lsp_types::Position {
            line: self.line,
            character: self.character,
        }
    }
}

impl From<tree_sitter::Point> for Position {
    fn from(value: tree_sitter::Point) -> Self {
        Position {
            line: value.row as u32,
            character: value.column as u32,
        }
    }
}

impl Into<tree_sitter::Point> for Position {
    fn into(self) -> tree_sitter::Point {
        tree_sitter::Point {
            row: self.line as usize,
            column: self.character as usize,
        }
    }
}

#[derive(Clone, Copy)]
struct Range {
    start: Position,
    end: Position,
}

impl From<tower_lsp::lsp_types::Range> for Range {
    fn from(value: tower_lsp::lsp_types::Range) -> Self {
        Range {
            start: value.start.into(),
            end: value.end.into(),
        }
    }
}

impl Into<tower_lsp::lsp_types::Range> for Range {
    fn into(self) -> tower_lsp::lsp_types::Range {
        tower_lsp::lsp_types::Range {
            start: self.start.into(),
            end: self.end.into(),
        }
    }
}

impl From<tree_sitter::Range> for Range {
    fn from(value: tree_sitter::Range) -> Self {
        Range {
            start: value.start_point.into(),
            end: value.end_point.into(),
        }
    }
}

impl Into<std::ops::Range<Point>> for Range {
    fn into(self) -> std::ops::Range<Point> {
        self.start.into()..self.end.into()
    }
}

// TODO
// impl Into<tree_sitter::Range> for Range {
//     fn into(self) -> tree_sitter::Range {
//         tower_lsp::lsp_types::Range {
//             start: self.start.into(),
//             end: self.end.into(),
//         }
//     }
// }

/// taken from https://www.w3.org/TR/owl2-syntax/#Entity_Declarations_and_Typing
#[derive(Clone, Copy)]
enum IriType {
    Class,
    DataType,
    ObjectProperty,
    DataProperty,
    AnnotationProperty,
    Individual,
    Ontology,
}

#[derive(Clone)]
struct IriInfo {
    annotations: HashMap<Iri, ResolvedIri>,
    tipe: IriType,
}

impl IriInfo {
    fn label(&self) -> Option<String> {
        self.annotations
            .get("rdfs:label")
            .map(|resolved| resolved.value.clone())
    }
}

#[derive(Clone)]
struct ResolvedIri {
    value: String,
    range: Range,
}

struct Backend {
    client: Client,
    parser: Mutex<Parser>, // stateful because of resume behavior after fail, timeout or cancellation
    document_map: DashMap<Url, Document>, // async hash map
    position_encoding: Mutex<PositionEncodingKind>,
}

struct Document {
    tree: Tree,
    rope: Rope,
    version: i32,
    iri_info_map: DashMap<Iri, IriInfo>,
    diagnostics: Vec<Diagnostic>,
}

impl Document {
    fn resolve_iri(&self, iri: &String) -> String {
        self.iri_info_map
            .get(iri)
            .and_then(|iri_info| iri_info.label())
            .unwrap_or(iri.clone())
    }
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[serde(rename_all = "camelCase")]
struct StaticNode {
    #[serde(rename = "type")]
    _type: String,
    named: bool,
    // fields: Vec<??> // TODO when needed
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

#[tokio::main]
async fn main() {
    simple_logging::log_to_file("lanugage-server.log", LevelFilter::Trace)
        .expect("logging to work");
    std::panic::set_hook(Box::new(|info| {
        error!("paniced with {}", info);
    }));

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let mut parser = Parser::new();
    parser.set_language(*LANGUAGE).unwrap();

    let (service, socket) = LspService::new(|client| Backend {
        client,
        parser: Mutex::new(parser),
        document_map: DashMap::new(),
        position_encoding: PositionEncodingKind::UTF16.into(),
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        self.client
            .log_message(MessageType::INFO, "initialize called")
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

        info!("initialize");
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
                ..Default::default()
            },
        })
    }

    async fn initialized(&self, _params: InitializedParams) {
        info!("initialized");
        self.client
            .log_message(MessageType::INFO, "server initialized!")
            .await;
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        info!(
            "did_open at {} with version {}",
            params.text_document.uri, params.text_document.version
        );

        let mut parser_guard = self.parser.lock().await;
        parser_guard.reset();

        let tree = timeit("did_open > inital parse", || {
            parser_guard
                .parse(&params.text_document.text, None)
                .expect("language to be set, no timeout to be used, no cancelation flag")
        });

        let rope = Rope::from(params.text_document.text);
        let iri_info_map = gen_iri_info_map(&tree, &rope, None);
        let diagnostics = timeit("did_open > gen_diagnostics", || {
            gen_diagnostics(&tree.root_node())
        });

        self.client
            .publish_diagnostics(params.text_document.uri.clone(), diagnostics.clone(), None)
            .await;

        self.document_map.insert(
            params.text_document.uri.clone(),
            Document {
                version: params.text_document.version,
                tree,
                rope,
                iri_info_map,
                diagnostics,
            },
        );
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        info!(
            "did_change at {} with version {}",
            params
                .text_document
                .uri
                .path_segments()
                .unwrap()
                .last()
                .unwrap(),
            params.text_document.version
        );

        if let Some(mut document) = self.document_map.get_mut(&params.text_document.uri) {
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
                            old_end_position: position_to_point(range.end),
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
                document.iri_info_map = gen_iri_info_map(&document.tree, &document.rope, None);

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

                document.iri_info_map.extend(iri_info_map);
            }

            for range in change_ranges.iter() {
                // diagnostics
                // TODO this does not work correctly
                document
                    .diagnostics
                    .retain(|d| !range_overlaps(&d.range.into(), range));

                let mut cursor = document.tree.walk();

                while range_exclusive_inside(range, &ts_range_to_lsp_range(cursor.node().range())) {
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
                .filter(|d| range_overlaps(&d.range.into(), &range)); // should be exclusive to other diagnostics

                document.diagnostics.extend(additional_diagnostics);

                // OK?
                let uri = params.text_document.uri.clone();
                let d = document.diagnostics.clone();
                let c = self.client.clone();
                task::spawn(async move {
                    c.publish_diagnostics(uri, d, None).await;
                });
            }
        }

        // TODO
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        info!(
            "hover at {:?} in file {}",
            params.text_document_position_params.position,
            params.text_document_position_params.text_document.uri
        );

        self.client.log_message(MessageType::INFO, "hover").await;

        Ok(self
            .document_map
            .get(&params.text_document_position_params.text_document.uri)
            .map(|document| {
                let pos = params.text_document_position_params.position.into();
                let node = deepest_named_node_at_pos(&document.tree, pos);
                let info = node_info(&node, &document);
                let range: Range = node.range().into();
                Hover {
                    contents: HoverContents::Scalar(MarkedString::String(info)),
                    range: Some(range.into()),
                }
            }))
    }

    async fn inlay_hint(&self, params: InlayHintParams) -> Result<Option<Vec<InlayHint>>> {
        info!("inlay_hint at {}", params.text_document.uri);

        Ok(self
            .document_map
            .get(&params.text_document.uri)
            .map(|document| {
                let iri_label_map = &document.iri_info_map;

                let full_iri_query = Query::new(*LANGUAGE, "(full_iri)@iri").unwrap();

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
                        iri_label_map.get(iri).and_then(|info| {
                            let node = capture.node;
                            let end: Position = node.end_position().into(); // inlay hints are at the end of the iri

                            // TODO write a tooltip
                            info.label().map(trim_string_value).map(|label| InlayHint {
                                position: end.into(),
                                label: InlayHintLabel::String(label),
                                kind: None,
                                text_edits: None,
                                tooltip: None,
                                padding_left: Some(true),
                                padding_right: None,
                                data: None,
                            })
                        })
                    })
                    .collect::<Vec<InlayHint>>()
            }))
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = params.text_document_position_params.text_document.uri;
        if let Some(doc) = self.document_map.get(&uri) {
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
                    // TODO this is not working :/
                    // "datatype_iri" => Some(format!(
                    //     "(datatype_frame
                    //         . (datatype_iri)@iri
                    //         (#eq? @iri \"{}\")
                    //     )@frame",
                    //     iri
                    // )),
                    _ => None,
                };

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
        let url = params.text_document.uri;
        let doc = self.document_map.get(&url).unwrap();
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
        let url = params.text_document_position.text_document.uri;
        let doc = self.document_map.get(&url);
        let pos: Position = params.text_document_position.position.into();
        Ok(doc.map(|doc| {
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

            items.extend(keywords);

            CompletionResponse::Array(items)
        }))
    }

    async fn shutdown(&self) -> Result<()> {
        info!("shutdown");
        Ok(())
    }
}

// TODO only do this in a range of changed content
fn gen_iri_info_map(tree: &Tree, rope: &Rope, range: Option<&Range>) -> DashMap<Iri, IriInfo> {
    info!("generating iri info map");

    // TODO check frame [(datatype_frame) (class_frame) (object_property_frame) (data_property_frame) (annotation_property_frame) (individual_frame)]
    // the typed literal is for the string type
    let frame_source = "
        (_
            . (_ [(full_iri) (simple_iri) (abbreviated_iri)]@frame_iri)
            (annotation
                (annotation_property_iri)@iri
                [
                    (string_literal_no_language)
                    (string_literal_with_language)
                    (typed_literal)
                ]@literal))";

    let frame_query = Query::new(*LANGUAGE, frame_source).expect("valid query expect");
    let mut query_cursor = QueryCursor::new();

    if let Some(range) = range {
        query_cursor.set_point_range((*range).into());
    }

    let matches = query_cursor.matches(&frame_query, tree.root_node(), RopeProvider::new(rope));

    let infos = DashMap::<Iri, IriInfo>::new();

    for m in matches {
        let mut capture_texts = m.captures.iter().map(|c| node_text(&c.node, rope));
        let frame_iri = capture_texts.next().unwrap().to_string();
        let annotation_iri = capture_texts.next().unwrap().to_string();
        let literal = capture_texts.next().unwrap().to_string();

        let parent_node = m.captures[0].node.parent().unwrap();
        let entity = match parent_node.kind() {
            "class_iri" => IriType::Class,
            "datatype_iri" => IriType::DataType,
            "annotation_property_iri" => IriType::AnnotationProperty,
            "individual_iri" => IriType::Individual,
            "ontology_iri" => IriType::Ontology,
            "data_property_iri" => IriType::DataProperty,
            "object_property_iri" => IriType::ObjectProperty,
            kind => {
                error!("implement {kind}");
                IriType::Class
            }
        };

        if !infos.contains_key(&frame_iri) {
            infos.insert(
                frame_iri.clone(),
                IriInfo {
                    annotations: HashMap::new(),
                    tipe: entity,
                },
            );
        }

        let mut info = infos.get_mut(&frame_iri).unwrap();
        info.annotations.insert(
            annotation_iri.clone(),
            ResolvedIri {
                value: literal.clone(),
                range: parent_node.range().into(),
            },
        );
    }
    infos
}

// Thanks to the helix team
// https://github.com/helix-editor/helix/blob/master/helix-core/src/syntax.rs#L1747
pub struct ChunksBytes<'a> {
    chunks: ropey::iter::Chunks<'a>,
}
impl<'a> Iterator for ChunksBytes<'a> {
    type Item = &'a [u8];
    fn next(&mut self) -> Option<Self::Item> {
        self.chunks.next().map(str::as_bytes)
    }
}

struct RopeProvider<'a>(pub RopeSlice<'a>);
impl<'a> TextProvider<'a> for RopeProvider<'a> {
    type I = ChunksBytes<'a>;

    fn text(&mut self, node: Node) -> Self::I {
        let fragment = self.0.byte_slice(node.start_byte()..node.end_byte());
        ChunksBytes {
            chunks: fragment.chunks(),
        }
    }
}
impl<'a> RopeProvider<'a> {
    fn new(value: &'a Rope) -> Self {
        RopeProvider(value.slice(..))
    }
}
impl<'a> From<&'a Rope> for RopeProvider<'a> {
    fn from(value: &'a Rope) -> Self {
        RopeProvider::new(value)
    }
}

fn gen_diagnostics(node: &Node) -> Vec<Diagnostic> {
    let mut cursor = node.walk();
    let mut diagnostics = Vec::<Diagnostic>::new();

    loop {
        let node = cursor.node();

        if node.is_error() {
            // log
            let range: Range = cursor.node().range().into();

            // root has no parents so use itself
            let parent_kind = node.parent().unwrap_or(node).kind();

            if let Some(static_node) = NODE_TYPES.get(parent_kind) {
                let valid_children: String = static_node
                    .children
                    .types
                    .iter()
                    .map(|sn| node_type_to_string(&sn._type))
                    .intersperse(", ".to_string())
                    .collect();

                // let text = node.utf8_text(bytes).unwrap();
                let parent = node_type_to_string(parent_kind);
                let msg = format!("Syntax Error. expected {valid_children} inside {parent}");

                diagnostics.push(Diagnostic {
                    range: range.into(),
                    severity: Some(DiagnosticSeverity::ERROR),
                    code: None,
                    code_description: None,
                    source: Some("owl language server".to_string()),
                    message: msg.to_string(),
                    related_information: None,
                    tags: None,
                    data: None,
                });
            }
            // move along
            while !cursor.goto_next_sibling() {
                // move out
                if !cursor.goto_parent() {
                    // this node has no parent, its the root
                    return diagnostics;
                }
            }
        } else if node.has_error() {
            // move in
            let has_child = cursor.goto_first_child(); // should alwayes work

            if !has_child {
                while !cursor.goto_next_sibling() {
                    // move out
                    if !cursor.goto_parent() {
                        // this node has no parent, its the root
                        return diagnostics;
                    }
                }
            }
        } else {
            // move along
            while !cursor.goto_next_sibling() {
                // move out
                if !cursor.goto_parent() {
                    // this node has no parent, its the root
                    return diagnostics;
                }
            }
        }
    }
}

fn _log_to_client(client: &Client, msg: String) -> task::JoinHandle<()> {
    let c = client.clone();
    task::spawn(async move {
        c.log_message(MessageType::INFO, msg).await;
    })
}

fn node_type_to_string(node_type: &str) -> String {
    node_type
        .split_terminator('_')
        .map(capitilize_string)
        .intersperse(" ".to_string())
        .collect()
}

fn capitilize_string(s: &str) -> String {
    let mut c = s.chars();
    match c.next() {
        None => String::new(),
        Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
    }
}

/// only looks at the lines
fn range_overlaps(a: &Range, b: &Range) -> bool {
    (a.start.line <= b.end.line && a.end.line >= b.start.line)
        || (b.start.line <= a.end.line && b.end.line >= a.start.line)
}

/// is one range "inner" inside the range "outer"
fn range_exclusive_inside(inner: &Range, outer: &Range) -> bool {
    inner.start.line > outer.start.line && inner.end.line < outer.end.line
}

fn ts_range_to_lsp_range(range: tree_sitter::Range) -> Range {
    Range {
        start: point_to_positon(range.start_point),
        end: point_to_positon(range.end_point),
    }
}

fn position_to_point(position: Position) -> Point {
    Point {
        row: position.line as usize,
        column: position.character as usize,
    }
}

fn point_to_positon(point: Point) -> Position {
    Position {
        line: point.row as u32,
        character: point.column as u32,
    }
}

fn node_text<'a>(node: &Node, rope: &'a Rope) -> ropey::RopeSlice<'a> {
    rope.byte_slice(node.start_byte()..node.end_byte())
}

#[test]
fn test_parser() {
    let mut parser = Parser::new();
    parser.set_language(*LANGUAGE).unwrap();

    let source_code = "Ontology: Foobar";
    let tree = parser.parse(source_code, None).unwrap();

    assert_eq!(
        tree.root_node().to_sexp(),
        "(source_file (ontology (ontology_iri (simple_iri))))"
    );
}

#[test]
fn test_class_annotation_query() {
    use tree_sitter::QueryMatch;

    let mut parser = Parser::new();
    parser.set_language(*LANGUAGE).unwrap();

    let source_code = "
Ontology: <http://foo.bar>
    Class: <http://foo.bar/0>
        Annotations:
            rdfs:label \"Fizz\"
";
    let tree = parser.parse(source_code, None).unwrap();

    let query_source = "
                            (class_frame (class_iri (full_iri)@iri)\
                                (annotation\
                                    (annotation_property_iri (abbreviated_iri)@abbriviated-iri)\
                                    (string_literal_no_language)@literal))";

    let class_frame_query = Query::new(*LANGUAGE, query_source).unwrap();
    let root = tree.root_node();
    let mut query_cursor = QueryCursor::new();
    let bytes: &[u8] = source_code.as_bytes();
    let matches = query_cursor
        .matches(&class_frame_query, root, bytes)
        .collect::<Vec<QueryMatch>>();

    println!("{}", root.to_sexp());

    assert_eq!(matches.len(), 1);

    assert_eq!(
        matches[0].captures[1]
            .node
            .utf8_text(bytes)
            .expect("valid utf-8"),
        "rdfs:label"
    );
}

fn timeit<F: FnMut() -> T, T>(name: &str, mut f: F) -> T {
    use std::time::Instant;
    let start = Instant::now();
    let result = f();
    let end = Instant::now();
    let duration = end.duration_since(start);
    info!("⏲ {} took {:?}", name, duration);
    result
}

fn deepest_named_node_at_pos(tree: &Tree, pos: Position) -> Node {
    let mut cursor = tree.walk();
    loop {
        let is_leaf = cursor
            .goto_first_child_for_point(position_to_point(pos))
            .is_none();
        if is_leaf {
            break;
        }
    }
    while !cursor.node().is_named() {
        if !cursor.goto_parent() {
            break;
        }
    }
    return cursor.node();
}

fn cursor_goto_pos(cursor: &mut TreeCursor, pos: Position) {
    loop {
        let is_leaf = cursor
            .goto_first_child_for_point(position_to_point(pos))
            .is_none();
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

fn node_info(node: &Node, doc: &Document) -> String {
    match node.kind() {
        "class_frame" | "annotation_property_frame" => {
            let iri = &node
                .named_child(0)
                .map(|c| node_text(&c, &doc.rope).to_string());

            if let Some(iri) = iri {
                iri_info(iri, doc)
            } else {
                "Class Frame\nNo iri was found".to_string()
            }
        }
        "full_iri" | "simple_iri" | "abbreviated_iri" => {
            let iri = node_text(node, &doc.rope).to_string();
            iri_info(&iri, doc)
        }
        "class_iri" => iri_info(&node_text(node, &doc.rope).to_string(), doc),

        _ => format!("generic node named {}", node.kind()),
    }
}

fn iri_info(iri: &String, doc: &Document) -> String {
    if let Some(info) = doc.iri_info_map.get(iri) {
        let entity = info.tipe;
        let label = info
            .label()
            .map(trim_string_value)
            .unwrap_or("(no label)".to_string());

        let annotations = info
            .annotations
            .iter()
            .map(|(iri, v)| {
                let label = doc.resolve_iri(iri);
                let label = trim_string_value(label);
                let v = trim_string_value(v.value.clone());
                format!("`{label}`: {v}")
            })
            .intersperse("\n".to_string())
            .collect::<String>();

        format!("{entity} **{label}**\n\n{annotations}")
    } else {
        "No info found on iri".to_string()
    }
}

fn trim_string_value(value: String) -> String {
    value
        .trim_start_matches('"')
        .trim_end_matches("@en")
        .trim_end_matches("@de")
        .trim_end_matches("^^xsd:string") // typed literal with type string
        .trim_end_matches('"')
        .replace("\\\"", "\"")
        .trim()
        .to_string()
}

impl Display for IriType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = match self {
            IriType::Class => "Class",
            IriType::DataType => "Data Type",
            IriType::ObjectProperty => "Object Property",
            IriType::DataProperty => "Data Property",
            IriType::AnnotationProperty => "Annotation Property",
            IriType::Individual => "Named Individual",
            IriType::Ontology => "Ontology",
        };
        write!(f, "{name}")
    }
}

fn node_type_to_keyword(_type: &str) -> Option<String> {
    match _type {
        "annotation_property_frame" => Some("AnnotationProperty:".to_string()),
        "class_frame" => Some("Class:".to_string()),
        "annotation" => Some("Annotations:".to_string()),
        _ => None,
    }
}

#![feature(async_closure, iter_intersperse)]
use dashmap::DashMap;
use log::{error, info, trace, LevelFilter};
use once_cell::sync::Lazy;
use ropey::{Rope, RopeSlice};
use serde::{Deserialize, Serialize};
use tokio::sync::Mutex;
use tokio::task;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};
use tree_sitter::{
    InputEdit, Language, Node, Parser, Point, Query, QueryCursor, TextProvider, Tree,
};
use tree_sitter_owl_ms::language;

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

type Iri = String;

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
    class_iri_label_map: DashMap<Iri, String>,
    diagnostics: Vec<Diagnostic>,
}

#[derive(Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
struct StaticNode {
    #[serde(rename = "type")]
    _type: String,
    named: bool,
    // fields: Vec<??> // TODO when needed
    #[serde(default)]
    children: StaticNodeChildren,
}

#[derive(Serialize, Deserialize, Default, Clone)]
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

        let tree = parser_guard
            .parse(&params.text_document.text, None)
            .expect("language to be set, no timeout to be used, no cancelation flag");

        let rope = Rope::from(params.text_document.text);
        let class_iri_label_map = gen_class_iri_label_map(&tree, &rope);
        let diagnostics = timeit("gen_diagnostics inside did_open", || {
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
                class_iri_label_map,
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

            for change in params.content_changes {
                if let Some(range) = change.range {
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
                        start_position: position_to_point(range.start),
                        old_end_position: position_to_point(range.end),
                        new_end_position: Point {
                            row: new_end_line,
                            column: new_end_character,
                        },
                    };
                    timeit("tree edit", || document.tree.edit(&edit));

                    let mut parser_guard = self.parser.lock().await;
                    parser_guard.reset();

                    let tree =
                        timeit("parsing", || {
                            parser_guard
                        .parse(document.rope.to_string(), Some(&document.tree))
                        .expect("language to be set, no timeout to be used, no cancelation flag")
                        });

                    document.class_iri_label_map = timeit("gen_class_iri_label_map", || {
                        gen_class_iri_label_map(&tree, &document.rope)
                    });
                    document.tree = tree;
                    document.version = params.text_document.version;

                    // diagnostics
                    // TODO this does not work correctly
                    document
                        .diagnostics
                        .retain(|d| !range_overlaps(&d.range, &range));

                    let mut cursor = document.tree.walk();

                    while range_exclusive_inside(
                        &range,
                        &ts_range_to_lsp_range(cursor.node().range()),
                    ) {
                        trace!("move in to find error");
                        if cursor
                            .goto_first_child_for_point(edit.start_position)
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
                    let additional_diagnostics =
                        timeit("gen_diagnostics inside did_change", || {
                            gen_diagnostics(&node_that_has_change)
                        })
                        .into_iter()
                        .filter(|d| range_overlaps(&d.range, &range)); // should be exclusive to other diagnostics

                    document.diagnostics.extend(additional_diagnostics);

                    // OK?
                    let uri = params.text_document.uri.clone();
                    let d = document.diagnostics.clone();
                    let c = self.client.clone();
                    task::spawn(async move {
                        c.publish_diagnostics(uri, d, None).await;
                    });
                } else {
                    todo!("full replace");
                }
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

        Ok(
            if let Some(document) = self
                .document_map
                .get(&params.text_document_position_params.text_document.uri)
            {
                let position = params.text_document_position_params.position;
                let byte_offset = document.rope.char_to_byte(
                    document.rope.line_to_char(position.line as usize)
                        + (position.character as usize),
                );

                let mut cursor = document.tree.root_node().walk();
                for _ in 0..20 {
                    cursor.goto_first_child_for_byte(byte_offset); // brute force the recursive decent
                }
                let node = cursor.node();
                Some(Hover {
                    contents: HoverContents::Scalar(MarkedString::String(node.kind().to_string())),
                    range: None,
                })
            } else {
                None
            },
        )
    }

    async fn inlay_hint(&self, params: InlayHintParams) -> Result<Option<Vec<InlayHint>>> {
        info!("inlay_hint at {}", params.text_document.uri);

        if let Some(document) = self.document_map.get(&params.text_document.uri) {
            let iri_label_map = &document.class_iri_label_map;
            let inline_values = {
                let full_iri_query = Query::new(*LANGUAGE, "(class_iri (full_iri)@iri)").unwrap();
                let root = document.tree.root_node();
                let mut query_cursor = QueryCursor::new();
                query_cursor.set_point_range(
                    position_to_point(params.range.start)..position_to_point(params.range.end),
                );
                let s = document.rope.to_string();
                let bytes: &[u8] = s.as_bytes();
                let matches = query_cursor.matches(&full_iri_query, root, bytes);
                matches
                    .map(|match_| match_.captures[0])
                    .filter_map(|capture| {
                        let iri = node_text(&capture.node, &document.rope).to_string();

                        iri_label_map.get(&iri).map(|label| {
                            let node = capture.node;
                            // let start = node.start_position();
                            let end = node.end_position();

                            InlayHint {
                                position: point_to_positon(end),
                                label: InlayHintLabel::String(label.to_owned()),
                                kind: None,
                                text_edits: None,
                                tooltip: Some(InlayHintTooltip::String("Hey there".into())),
                                padding_left: None, // TOOD test what is possible
                                padding_right: None,
                                data: None,
                            }
                        })
                    })
                    .collect::<Vec<InlayHint>>()
            };

            Ok(Some(inline_values))
        } else {
            Ok(None)
        }
    }

    async fn shutdown(&self) -> Result<()> {
        info!("shutdown");
        Ok(())
    }
}

fn gen_class_iri_label_map(tree: &Tree, rope: &Rope) -> DashMap<Iri, String> {
    // the typed literal is for the string type
    let class_frame_source = "
                            (class_frame (class_iri (full_iri)@iri)\
                                (annotation\
                                    (annotation_property_iri (abbreviated_iri)@abbriviated-iri)\
                                    [\
                                        (string_literal_no_language)\
                                        (string_literal_with_language)\
                                        (typed_literal)\
                                    ]@literal))";

    let class_frame_query = Query::new(*LANGUAGE, class_frame_source).unwrap();
    let root = tree.root_node();
    let mut query_cursor = QueryCursor::new();

    let rp = RopeProvider(rope.slice(..));
    let matches = query_cursor.matches(&class_frame_query, root, rp);
    matches
        .filter(|m| node_text(&m.captures[1].node, rope) == "rdfs:label")
        .map(|m| {
            (
                node_text(&m.captures[0].node, rope).to_string(),
                node_text(&m.captures[2].node, rope).to_string(),
            )
        })
        .collect::<DashMap<Iri, String>>()
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

fn gen_diagnostics(root: &Node) -> Vec<Diagnostic> {
    let mut cursor = root.walk();
    let mut diagnostics = Vec::<Diagnostic>::new();

    loop {
        let node = cursor.node();

        if node.is_error() {
            // log
            let range = cursor.node().range();

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
                    range: Range {
                        start: point_to_positon(range.start_point),
                        end: point_to_positon(range.end_point),
                    },
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

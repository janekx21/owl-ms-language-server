use std::collections::HashMap;
use std::fmt::Debug;

use dashmap::DashMap;
use ropey::Rope;
use tokio::sync::Mutex;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};
use tree_sitter::{InputEdit, Language, Parser, Point, Query, QueryCursor, QueryMatch, Tree};

extern "C" {
    fn tree_sitter_owl_ms() -> Language;
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
    // TODO version
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
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
                inline_value_provider: Some(OneOf::Left(true)),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "server initialized!")
            .await;
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let mut parser_guard = self.parser.lock().await;
        parser_guard.reset();
        if let Some(tree) = parser_guard.parse(params.text_document.text.to_owned(), None) {
            self.document_map.insert(
                params.text_document.uri,
                Document {
                    tree,
                    rope: Rope::from(params.text_document.text),
                },
            );
        }
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        if let Some(mut document) = self.document_map.get_mut(&params.text_document.uri) {
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
                    document.rope.remove(start_char..old_end_char);
                    document.rope.insert(start_char, &change.text);

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
                    document.tree.edit(&edit);

                    let mut parser_guard = self.parser.lock().await;
                    parser_guard.reset();
                    if let Some(tree) =
                        parser_guard.parse(document.rope.to_string(), Some(&document.tree))
                    {
                        document.tree = tree;
                    };
                } else {
                    todo!("full replace");
                }
            }
        }
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
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
        let language = unsafe { tree_sitter_owl_ms() };
        if let Some(document) = self.document_map.get(&params.text_document.uri) {
            let iri_label_map = {
                let class_frame_source = "
                            (class_frame (class_iri (full_iri)@iri)\
                                (annotation\
                                    (annotation_property_iri (abbreviated_iri)@abbriviated-iri)\
                                    [\
                                        (string_literal_no_language)\
                                        (string_literal_with_language)\
                                        (typed_literal)\
                                    ]@literal))";
                // TODO do i need typed_literal?

                let class_frame_query = Query::new(language, class_frame_source).unwrap();
                let root = document.tree.root_node();
                let mut query_cursor = QueryCursor::new();
                let s = document.rope.to_string();
                let bytes: &[u8] = s.as_bytes();
                let matches = query_cursor.matches(&class_frame_query, root, bytes);
                matches
                    .filter(|m| {
                        m.captures[1].node.utf8_text(bytes).expect("valid utf-8") == "rdfs:label"
                    })
                    .map(|m| {
                        (
                            m.captures[0].node.utf8_text(bytes).unwrap().to_owned(),
                            m.captures[2].node.utf8_text(bytes).unwrap().to_owned(),
                        )
                    })
                    .collect::<HashMap<String, String>>()
            };

            let inline_values = {
                let language = unsafe { tree_sitter_owl_ms() };
                let full_iri_query = Query::new(language, "(class_iri (full_iri)@iri)").unwrap();
                let root = document.tree.root_node();
                let mut query_cursor = QueryCursor::new();
                let s = document.rope.to_string();
                let bytes: &[u8] = s.as_bytes();
                let matches = query_cursor.matches(&full_iri_query, root, bytes);
                matches
                    .map(|match_| match_.captures[0])
                    .filter_map(|capture| {
                        let iri = capture.node.utf8_text(bytes).unwrap().to_string();

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

            self.client
                .log_message(MessageType::INFO, format!("{inline_values:?}"))
                .await;

            self.client
                .show_message(MessageType::INFO, format!("{inline_values:?}"))
                .await;
            // for match_ in  {
            //     for ele in match_.captures.iter() {
            //         ele.node;
            //     }
            // }

            Ok(Some(inline_values))
        } else {
            Ok(None)
        }
    }

    async fn inline_value(&self, params: InlineValueParams) -> Result<Option<Vec<InlineValue>>> {
        if let Some(document) = self.document_map.get(&params.text_document.uri) {
            let inline_values = {
                let language = unsafe { tree_sitter_owl_ms() };
                let full_iri_query = Query::new(language, "(full_iri)").unwrap();
                let root = document.tree.root_node();
                let mut query_cursor = QueryCursor::new();
                let s = document.rope.to_string();
                let bytes: &[u8] = s.as_bytes();
                let matches = query_cursor.matches(&full_iri_query, root, bytes);
                matches
                    .flat_map(|match_| match_.captures.iter())
                    .map(|capture| {
                        let node = capture.node;
                        let start = node.start_position();
                        let end = node.end_position();
                        InlineValue::Text(InlineValueText {
                            range: Range {
                                start: point_to_positon(start),
                                end: point_to_positon(end),
                            },
                            text: format!("INLINE kind: {} index: {}", node.kind(), capture.index),
                        })
                    })
                    .collect::<Vec<InlineValue>>()
            };

            self.client
                .log_message(MessageType::INFO, format!("{inline_values:?}"))
                .await;

            self.client
                .show_message(MessageType::INFO, format!("{inline_values:?}"))
                .await;
            // for match_ in  {
            //     for ele in match_.captures.iter() {
            //         ele.node;
            //     }
            // }

            // TODO query for some full_iri
            // TODO query for the rdfs:label of that iris frame
            // TODO append the inline value
            Ok(Some(inline_values))
        } else {
            Ok(None)
        }
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
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

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let language = unsafe { tree_sitter_owl_ms() };
    let mut parser = Parser::new();
    parser.set_language(language).unwrap();

    let (service, socket) = LspService::new(|client| Backend {
        client,
        parser: Mutex::new(parser),
        document_map: DashMap::new(),
        position_encoding: PositionEncodingKind::UTF16.into(),
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}

#[test]
fn test_parser() {
    let language = unsafe { tree_sitter_owl_ms() };
    let mut parser = Parser::new();
    parser.set_language(language).unwrap();

    let source_code = "Ontology: Foobar";
    let tree = parser.parse(source_code, None).unwrap();

    assert_eq!(
        tree.root_node().to_sexp(),
        "(source_file (ontology (ontology_iri (simple_iri))))"
    );
}

#[test]
fn test_class_annotation_query() {
    let language = unsafe { tree_sitter_owl_ms() };
    let mut parser = Parser::new();
    parser.set_language(language).unwrap();

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

    let class_frame_query = Query::new(language, query_source).unwrap();
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

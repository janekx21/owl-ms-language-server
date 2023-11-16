use dashmap::DashMap;
use ropey::Rope;
use std::cell::Cell;
use std::collections::HashMap;
use std::fmt::Pointer;
use std::ops::Index;
use std::sync::{Arc, Mutex};
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};
use tree_sitter::{InputEdit, Language, Parser, Point, Tree};

extern "C" {
    fn tree_sitter_owl_ms() -> Language;
}

// #[derive(Debug)]
struct Backend {
    client: Client,
    parser: Mutex<Parser>,
    document_map: DashMap<Url, Document>,
}

struct Document {
    tree: Tree,
    rope: Rope,
    // version
    // uri
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
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
                position_encoding: Some(PositionEncodingKind::UTF8),
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
        let mut parser_guard = self.parser.lock().unwrap();
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
                    // let (chunk, chunk_byte_idx, chunk_char_idx, chunk_line_idx) =
                    // let tuple = document.rope.chunk_at_line_break(range.start.line as usize);
                    self.client
                        .log_message(
                            MessageType::INFO,
                            format!("lines is {}", document.rope.len_lines()),
                        )
                        .await;

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

                    self.client
                        .log_message(MessageType::INFO, format!("{start_char}..{old_end_char}"))
                        .await;

                    // must come before the document is changed!
                    let old_end_byte = document.rope.char_to_byte(old_end_char);

                    document.rope.remove(start_char..old_end_char);
                    document.rope.insert(start_char, &change.text);

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

                    {
                        let mut parser_guard = self.parser.lock().unwrap(); // TODO remove unwrap
                        if let Some(tree) =
                            parser_guard.parse(document.rope.to_string(), Some(&document.tree))
                        {
                            document.tree = tree;
                        };
                    }

                    self.client
                        .log_message(
                            MessageType::INFO,
                            format!("tree is {}", document.tree.root_node().to_sexp()),
                        )
                        .await;
                } else {
                    todo!("full replace");
                }
            }
        }
        // if let Some(tree) = parser_guard.parse(params.text_document.text, None) {
        //     println!("{}", tree.root_node().to_sexp());
        //     trees_guard.insert(params.text_document.uri, tree);
        // }
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        println!("{:?}", params.text_document_position_params);
        Ok(Some(Hover {
            contents: HoverContents::Scalar(MarkedString::String("Hello World".to_string())),
            range: None,
        }))
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }
}

fn position_to_byte_offset(position: Position, text: &String) -> usize {
    let mut line = 0;
    let mut character = 0;
    for (index, char) in text.char_indices() {
        if char == '\n' {
            line += 1;
            character = 0;
        } else {
            character += 1;
        }
        if line == position.line && character == position.character {
            return index;
        }
    }
    panic!("Position out of bounds")
}

fn position_to_point(position: Position) -> Point {
    Point {
        row: position.line as usize,
        column: position.character as usize,
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

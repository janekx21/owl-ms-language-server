use crate::*;

#[test]
fn test_parse() {
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
fn test_parse_datatype() {
    let mut parser = Parser::new();
    parser.set_language(*LANGUAGE).unwrap();

    let source_code = "Ontology: o\nDatatype: d";
    let tree = parser.parse(source_code, None).unwrap();

    assert_eq!(
        tree.root_node().to_sexp(),
        "(source_file (ontology (ontology_iri (simple_iri)) (datatype_frame (datatype_iri (simple_iri)))))"
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

/// This tests if the "did_change" feature works on the lsp. It takes the document DEF and adds two changes resolving in ABCDEFGHI.
#[tokio::test]
async fn test_language_server_did_change() {
    let mut parser = Parser::new();
    parser.set_language(*LANGUAGE).unwrap();

    let (service, _) = LspService::new(|client| Backend {
        client,
        parser: Mutex::new(parser),
        document_map: DashMap::new(),
        position_encoding: PositionEncodingKind::UTF16.into(),
    });

    let result = service
        .inner()
        .initialize(InitializeParams {
            ..Default::default()
        })
        .await;
    assert!(result.is_ok(), "initialize returned {:#?}", result);

    let url = Url::parse("file://foo.omn").expect("valid url");
    service
        .inner()
        .did_open(DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri: url.clone(),
                language_id: "owl2md".to_string(),
                version: 0,
                text: "DEF".to_string(),
            },
        })
        .await;
    service
        .inner()
        .did_change(DidChangeTextDocumentParams {
            text_document: VersionedTextDocumentIdentifier {
                uri: url.clone(),
                version: 2,
            },
            content_changes: vec![
                TextDocumentContentChangeEvent {
                    range: Some(
                        Range {
                            start: (Position {
                                line: 0,
                                character: 0,
                            }),
                            end: Position {
                                line: 0,
                                character: 0,
                            },
                        }
                        .into(),
                    ),
                    range_length: None,
                    text: "ABC".to_string(),
                },
                TextDocumentContentChangeEvent {
                    range: Some(
                        Range {
                            start: (Position {
                                line: 0,
                                character: 6,
                            }),
                            end: Position {
                                line: 0,
                                character: 6,
                            },
                        }
                        .into(),
                    ),
                    range_length: None,
                    text: "GHI".to_string(),
                },
            ],
        })
        .await;

    let doc = service
        .inner()
        .document_map
        .get(&url.clone())
        .expect("found the document");
    let doc_content = doc.rope.to_string();

    assert_eq!(doc_content, "ABCDEFGHI");
}

use crate::{catalog::Catalog, web::StaticClient, *};
use anyhow::anyhow;
use pretty_assertions::assert_eq;
use std::{fs, path::Path};
use tempdir::{self, TempDir};
use test_log::test;
use tower_lsp::LspService;
use tree_sitter::Parser;
use tree_sitter::QueryMatch;

/// This module contains tests.
/// Each test function name is in the form of `<function>_<thing>_<condition>_<expectation>`.

/////////////////////////
// Tree sitter tests
/////////////////////////

#[test]
fn parse_ontology_should_work() {
    // Arrange
    let mut parser = arrange_parser();

    let source_code = "Ontology: Foobar";

    // Act
    let tree = parser.parse(source_code, None).unwrap();

    // Assert
    assert_eq!(
        tree.root_node().to_sexp(),
        "(source_file (ontology (ontology_iri (simple_iri))))"
    );
}

#[test]
fn parse_ontology_with_datatype_should_work() {
    // Arrange
    let mut parser = arrange_parser();

    let source_code = "Ontology: o\nDatatype: d";
    // Act
    let tree = parser.parse(source_code, None).unwrap();

    // Assert
    assert_eq!(
        tree.root_node().to_sexp(),
        "(source_file (ontology (ontology_iri (simple_iri)) (datatype_frame (datatype_iri (simple_iri)))))"
    );
}

#[test]
fn query_ontology_for_class_annotation_should_yield_all_class_annotations() {
    // Arrange
    let mut parser = arrange_parser();
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

    // Act
    let matches = query_cursor
        .matches(&class_frame_query, root, bytes)
        .collect::<Vec<QueryMatch>>();

    // Assert
    info!("{}", root.to_sexp());
    assert_eq!(matches.len(), 1);
    assert_eq!(
        matches[0].captures[1]
            .node
            .utf8_text(bytes)
            .expect("valid utf-8"),
        "rdfs:label"
    );
}

#[test]
/// This test should initilize all queries and therefore check if they are valid
fn deref_all_queries_should_be_valid() {
    let _ = *ALL_QUERIES;
}

///////////////////////////////////////////
// Backend Tests
//////////////////////////////////////////

#[test(tokio::test)]
async fn backend_did_open_should_create_document() {
    // Arrange
    let service = arrange_backend(None, vec![]).await;

    let url = Url::parse("file:///tmp/foo.omn").expect("valid url");

    // Act

    service
        .inner()
        .did_open(DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri: url.clone(),
                language_id: "owl2md".to_string(),
                version: 0,
                text: "abc".to_string(),
            },
        })
        .await;

    // Assert
    let workspaces = service.inner().workspaces.read();
    let workspace = workspaces.iter().exactly_one().unwrap();

    let docs = workspace.internal_documents.iter().collect_vec();

    let doc = docs
        .iter()
        .exactly_one()
        .map_err(|_| anyhow!("Should be exactly one"))
        .unwrap()
        .value()
        .read();

    assert_eq!(doc.uri, url);
    assert_eq!(doc.version, 0);
    assert_eq!(doc.rope.to_string(), "abc");
    assert!(doc.tree.root_node().is_error());
}

/// This tests if the "did_change" feature works on the lsp. It takes the document DEF and adds two changes resolving in ABCDEFGHI.
#[test(tokio::test)]
async fn backend_did_change_should_update_internal_rope() {
    // Arrange
    let service = arrange_backend(None, vec![]).await;

    let url = Url::parse("file:///tmp/foo.omn").expect("valid url");
    //                              ^^^ 2 for the scheme, 1 for the root

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

    // Act
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
                                character: 3,
                            }),
                            end: Position {
                                line: 0,
                                character: 3,
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

    // Assert
    let workspace = service.inner().find_workspace(&url).await;

    let doc = workspace
        .internal_documents
        .get(&url.clone())
        .expect("found the document");
    let doc = doc.read();
    let doc_content = doc.rope.to_string();

    assert_eq!(doc_content, "ABCDEFGHI");
}

#[test(tokio::test)]
async fn backend_hover_on_class_should_show_class_info() {
    // Arrange
    let service = arrange_backend(None, vec![]).await;

    let url = Url::parse("file:///tmp/foo.omn").expect("valid url");

    let ontology = r#"
        Ontology: HoverOnto
            Class: Janek
                Annotations:
                    rdfs:label "Janek der Coder"@de
    "#;
    service
        .inner()
        .did_open(DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri: url.clone(),
                language_id: "owl2md".to_string(),
                version: 0,
                text: ontology.to_string(),
            },
        })
        .await;

    // Act
    let hover_result = service
        .inner()
        .hover(HoverParams {
            text_document_position_params: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri: url.clone() },
                position: Position {
                    line: 2,
                    character: 21,
                }
                .into(),
            },
            work_done_progress_params: WorkDoneProgressParams {
                work_done_token: None,
            },
        })
        .await
        .unwrap();

    // Assert
    let hover_result = hover_result.unwrap();
    let range = hover_result.range.unwrap();
    assert_eq!(
        range,
        // Range of the "Janek" in the ontology
        Range {
            start: Position {
                line: 2,
                character: 19
            },
            end: Position {
                line: 2,
                character: 24
            }
        }
        .into()
    );

    let contents = match hover_result.contents {
        HoverContents::Scalar(MarkedString::String(str)) => str,
        _ => panic!("Did not think of that"),
    };
    assert_eq!(
        contents,
        "Class **Janek der Coder**\n\n---\n`rdfs:label`: Janek der Coder"
    );
}

async fn arrange_multi_file_ontology() -> (LspService<Backend>, TempDir) {
    let tmp_dir = arrange_workspace_folders(|dir| {
        vec![
            WorkspaceMember::Folder {
                name: "ontology-a".into(),
                children: vec![
                    WorkspaceMember::CatalogFile(
                        Catalog::new(dir.join("catalog-v001.xml").to_str().unwrap())
                            .with_uri("http://ontology-a.org/a1.omn", "a1.omn")
                            .with_uri("http://ontology-a.org/a2.omn", "a2.omn"),
                    ),
                    WorkspaceMember::OmnFile {
                        name: "a2.omn".into(),
                        content: r#"
                        Ontology: <http://ontology-a.org/a2.omn>
                            Class: ClassA2
                                Annotations:
                                    rdfs:label "Some class in A2",
                                    test "test"
                    "#
                        .into(),
                    },
                ],
            },
            WorkspaceMember::Folder {
                name: "ontology-b".into(),
                children: vec![
                    WorkspaceMember::CatalogFile(
                        Catalog::new(dir.join("catalog-v001.xml").to_str().unwrap())
                            .with_uri("http://ontology-b.org/b1.omn", "b1.omn")
                            .with_uri("http://ontology-b.org/b2.omn", "b2.omn"),
                    ),
                    WorkspaceMember::OmnFile {
                        name: "b2.omn".into(),
                        content: r#"
                        Ontology: <http://ontology-b.org/b2.omn>
                            Class: ClassB2
                                Annotations:
                                    rdfs:label "Some class in B2"
                    "#
                        .into(),
                    },
                ],
            },
        ]
    });

    let service = arrange_backend(
        Some(WorkspaceFolder {
            uri: Url::from_directory_path(tmp_dir.path()).unwrap(),
            name: "test wosrkpace".into(),
        }),
        vec![],
    )
    .await;

    let url = Url::from_file_path(tmp_dir.path().join("ontology-a").join("a1.omn")).unwrap();

    let ontology = r#"
        Ontology: <http://ontology-a.org/a1.omn>
            Import: <http://ontology-a.org/a2.omn>
            Class: ClassA1
                Annotations:
                    rdfs:label "Some class in A1"
                SubClassOf: ClassA2,ClassB2

    "#;

    service
        .inner()
        .did_open(DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri: url.clone(),
                language_id: "owl2md".to_string(),
                version: 0,
                text: ontology.to_string(),
            },
        })
        .await;

    (service, tmp_dir)
}

#[test(tokio::test)]
async fn backend_hover_in_multi_file_ontology_should_work() {
    // Arrange
    let (service, tmp_dir) = arrange_multi_file_ontology().await;

    let url = Url::from_file_path(tmp_dir.path().join("ontology-a").join("a1.omn")).unwrap();

    // Act
    // TODO check if the arrange did not create diagnostics
    let hover_result = service
        .inner()
        .hover(HoverParams {
            text_document_position_params: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri: url.clone() },
                position: Position {
                    line: 6,
                    character: 31,
                }
                .into(),
            },
            work_done_progress_params: WorkDoneProgressParams {
                work_done_token: None,
            },
        })
        .await
        .unwrap();

    // Assert
    let hover_result = hover_result.unwrap();
    let range = hover_result.range.unwrap();
    assert_eq!(
        range,
        // Range of the "Janek" in the ontology
        Range {
            start: Position {
                line: 6,
                character: 28
            },
            end: Position {
                line: 6,
                character: 35
            }
        }
        .into()
    );

    let contents = match hover_result.contents {
        HoverContents::Scalar(MarkedString::String(str)) => str,
        _ => panic!("Did not think of that"),
    };
    assert!(contents.contains("test"));
    assert!(contents.contains("Some class in A2"));
}

#[test(tokio::test)]
async fn backend_hover_in_multi_file_ontology_on_not_imported_iri_should_not_work() {
    // Arrange
    let (service, tmp_dir) = arrange_multi_file_ontology().await;

    let url = Url::from_file_path(tmp_dir.path().join("ontology-a").join("a1.omn")).unwrap();

    // Act
    // TODO check if the arrange did not create diagnostics
    let hover_result = service
        .inner()
        .hover(HoverParams {
            text_document_position_params: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri: url.clone() },
                position: Position {
                    line: 6,
                    character: 38,
                }
                .into(),
            },
            work_done_progress_params: WorkDoneProgressParams {
                work_done_token: None,
            },
        })
        .await
        .unwrap();

    // Assert
    let hover_result = hover_result.unwrap();

    let contents = match hover_result.contents {
        HoverContents::Scalar(MarkedString::String(str)) => str,
        _ => panic!("Did not think of that"),
    };
    assert!(contents.contains("ClassB2"));
    assert!(!contents.contains("Some class"));
    assert!(!contents.contains("b2"));
}

#[test(tokio::test)]
async fn backend_hover_on_external_iri_should_show_external_info() {
    // Arrange
    let tmp_dir = arrange_workspace_folders(|dir| {
        vec![WorkspaceMember::Folder {
            name: "ontology-a".into(),
            children: vec![WorkspaceMember::CatalogFile(
                Catalog::new(dir.join("catalog-v001.xml").to_str().unwrap())
                    .with_uri("http://ontology-a.org/a1", "a1.omn")
                    .with_uri("http://ontology-a.org/a2", "http://ontology-a.org/a2.owx"),
            )],
        }]
    });

    let service = arrange_backend(
        Some(WorkspaceFolder {
            uri: Url::from_directory_path(tmp_dir.path()).unwrap(),
            name: "test wosrkpace".into(),
        }),
        vec![(
            "http://ontology-a.org/a2.owx",
            r##"
                <?xml version="1.0"?>
                <Ontology xmlns="http://www.w3.org/2002/07/owl#"
                     xml:base="http://foo.org/a"
                     xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                     xmlns:xml="http://www.w3.org/XML/1998/namespace"
                     xmlns:xsd="http://www.w3.org/2001/XMLSchema#"
                     xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#"
                     ontologyIRI="http://ontology-a.org/a2.owx">

                    <Declaration>
                        <Class IRI="#ClassA2"/>
                    </Declaration>
    
                    <AnnotationAssertion>
                        <AnnotationProperty abbreviatedIRI="rdfs:label"/>
                        <IRI>#ClassA2</IRI>
                        <Literal>Some class in A2</Literal>
                    </AnnotationAssertion>

                </Ontology>
            "##,
        )],
    )
    .await;

    let url = Url::from_file_path(tmp_dir.path().join("ontology-a").join("a1.omn")).unwrap();

    let ontology = r#"
        Ontology: <http://ontology-a.org/a1>
            Import: <http://ontology-a.org/a2>
            Class: ClassA1
                Annotations:
                    rdfs:label "Some class in A1"
                SubClassOf: ClassA2,ClassB2

    "#;

    service
        .inner()
        .did_open(DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri: url.clone(),
                language_id: "owl2md".to_string(),
                version: 0,
                text: ontology.to_string(),
            },
        })
        .await;

    let url = Url::from_file_path(tmp_dir.path().join("ontology-a").join("a1.omn")).unwrap();

    // Act
    // TODO check if the arrange did not create diagnostics
    let hover_result = service
        .inner()
        .hover(HoverParams {
            text_document_position_params: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri: url.clone() },
                position: Position {
                    line: 6,
                    character: 32,
                }
                .into(),
            },
            work_done_progress_params: WorkDoneProgressParams {
                work_done_token: None,
            },
        })
        .await
        .unwrap();

    // Assert
    let hover_result = hover_result.unwrap();

    let contents = match hover_result.contents {
        HoverContents::Scalar(MarkedString::String(str)) => str,
        _ => panic!("Did not think of that"),
    };
    info!("contents={contents}");
    assert!(!contents.contains("ClassA2"));
    assert!(contents.contains("Some class in A2"));
}

#[test(tokio::test)]
async fn backend_import_resolve_should_load_documents() {
    // Arrange

    let tmp_dir = arrange_workspace_folders(|dir| {
        vec![
            WorkspaceMember::CatalogFile(
                Catalog::new(dir.join("catalog-v001.xml").to_str().unwrap())
                    .with_uri("http://external.org/shared.omn", "foobaronto.omn")
                    .with_uri("http://foobar.org/ontology/", "foo.omn"),
            ),
            WorkspaceMember::OmnFile {
                name: "foobaronto.omn".into(),
                content: "".into(),
            },
        ]
    });

    let service = arrange_backend(
        Some(WorkspaceFolder {
            uri: Url::from_directory_path(tmp_dir.path()).unwrap(),
            name: "test workspace".into(),
        }),
        vec![],
    )
    .await;

    let file_url = Url::from_file_path(tmp_dir.path().join("foo.omn")).expect("valid url");
    let ontology = r#"
        Ontology: <http://foobar.org/ontology/>
        Import: <http://external.org/shared.omn>
    "#;

    // Act
    service
        .inner()
        .did_open(DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri: file_url.clone(),
                language_id: "owl2md".to_string(),
                version: 0,
                text: ontology.to_string(),
            },
        })
        .await;

    // Assert

    let workspaces = service.inner().workspaces.read();
    assert_eq!(workspaces.len(), 1, "all files should be in one workspace");
    let workspace = workspaces.first().unwrap();
    info!(" Workspace documents {:#?}", workspace.internal_documents);
    let document_count = workspace.internal_documents.iter().count();
    assert_eq!(document_count, 2);
}

#[test(tokio::test)]
async fn backend_did_change_should_remove_old_infos() {
    // Arrange
    let service = arrange_backend(None, vec![]).await;

    let ontology_url = Url::from_file_path("/tmp/file.omn").unwrap();

    let ontology = r#"Ontology: <http://a.b/multi-file>
Class: class-in-first-file
    Annotations: rdfs:label "This class is in the first file"
"#;

    service
        .inner()
        .did_open(DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri: ontology_url.clone(),
                language_id: "owl2md".to_string(),
                version: 0,
                text: ontology.to_string(),
            },
        })
        .await;

    // Act

    service
        .inner()
        .did_change(DidChangeTextDocumentParams {
            text_document: VersionedTextDocumentIdentifier {
                uri: ontology_url.clone(),
                version: 1,
            },
            content_changes: vec![TextDocumentContentChangeEvent {
                text: "".into(),
                range: Some(
                    Range {
                        start: Position {
                            line: 2,
                            character: 0,
                        },
                        end: Position {
                            line: 2,
                            character: 61,
                        },
                    }
                    .into(),
                ),
                range_length: None,
            }],
        })
        .await;

    // Assert

    let workspaces = service.inner().workspaces.read();
    let workspace = workspaces.iter().exactly_one().unwrap();
    let document = workspace
        .internal_documents
        .iter()
        .exactly_one()
        .unwrap_or_else(|_| panic!("Multiple documents"));
    assert_eq!(
        document.read().rope.to_string(),
        r#"Ontology: <http://a.b/multi-file>
Class: class-in-first-file

"#
    );
    // let frame = document.frame_infos.get("class-in-first-file").unwrap();
    // TODO #32 assert_eq!(frame.annotations.get("rdfs:label"), None);
}

#[test(tokio::test)]
async fn backend_workspace_symbols_should_work() {
    // Arrange

    let tmp_dir = arrange_workspace_folders(|dir| {
        vec![
            WorkspaceMember::CatalogFile(
                Catalog::new(dir.join("catalog.xml").to_str().unwrap())
                    .with_uri("http://foo.org/a.omn", "a.omn"),
            ),
            WorkspaceMember::OmnFile {
                name: "a.omn".into(),
                content: r#"
                Ontology: <http://foo.org/a>
                    Class: some-class
                        Annotations:
                            rdfs:label "Some class"
                "#
                .into(),
            },
            WorkspaceMember::OmnFile {
                name: "b.omn".into(),
                content: r#"
                Ontology: <http://foo.org/b>
                    Import: <http://foo.org/a.omn>
                    Class: some-other-class
                        Annotations:
                            rdfs:label "Some other class"
                "#
                .into(),
            },
        ]
    });

    let service = arrange_backend(
        Some(WorkspaceFolder {
            uri: Url::from_directory_path(tmp_dir.path()).unwrap(),
            name: "foo".into(),
        }),
        vec![],
    )
    .await;

    let url = Url::from_file_path(tmp_dir.path().join("c.omn")).unwrap();

    let ontology = r#"
        Ontology: <http://foo.org/c>
            Import: <http://foo.org/a.omn>
            Class: some-other-class-at-c
                Annotations:
                    rdfs:label "Some other class at c"
    "#;
    service
        .inner()
        .did_open(DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri: url.clone(),
                language_id: "owl2md".to_string(),
                version: 0,
                text: ontology.to_string(),
            },
        })
        .await;

    // Act

    let result = service
        .inner()
        .symbol(WorkspaceSymbolParams {
            partial_result_params: PartialResultParams {
                partial_result_token: None,
            },
            work_done_progress_params: WorkDoneProgressParams {
                work_done_token: None,
            },
            query: "some".to_string(),
        })
        .await;

    // Assert

    let symbols = result
        .expect("Symbols should not throw errors")
        .expect("Symbols should contain something");

    assert_eq!(symbols.len(), 2);
}

#[test(tokio::test)]
async fn backend_did_open_should_load_external_documents_via_http() {
    // Arrange

    let tmp_dir = arrange_workspace_folders(|dir| {
        vec![
            WorkspaceMember::CatalogFile(
                Catalog::new(dir.join("catalog-v001.xml").to_str().unwrap())
                    .with_uri("http://foo.org/a.omn", "http://foo.org/version/file.owl")
                    .with_uri("http://foo.org/c.omn", "c.omn"),
            ),
            WorkspaceMember::OmnFile {
                name: "a.omn".into(),
                content: r#"
                Ontology: <http://foo.org/a>
                    Class: some-class
                        Annotations:
                            rdfs:label "Some class"
                "#
                .into(),
            },
        ]
    });

    let service = arrange_backend(
        Some(WorkspaceFolder {
            uri: Url::from_directory_path(tmp_dir.path()).unwrap(),
            name: "foo".into(),
        }),
        vec![(
            "http://foo.org/version/file.owl",
            r##"
                <?xml version="1.0"?>
                <Ontology xmlns="http://www.w3.org/2002/07/owl#"
                     xml:base="http://foo.org/a"
                     xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                     xmlns:xml="http://www.w3.org/XML/1998/namespace"
                     xmlns:xsd="http://www.w3.org/2001/XMLSchema#"
                     xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#"
                     ontologyIRI="http://foo.org/a">

                    <Declaration>
                        <Class IRI="#SomeOtherClass"/>
                    </Declaration>
    
                    <AnnotationAssertion>
                        <AnnotationProperty abbreviatedIRI="rdfs:label"/>
                        <IRI>#SomeOtherClass</IRI>
                        <Literal>Some other class at a</Literal>
                    </AnnotationAssertion>

                </Ontology>
            "##,
        )],
    )
    .await;

    let url = Url::from_file_path(tmp_dir.path().join("c.omn")).unwrap();

    let ontology = r#"
        Ontology: <http://foo.org/c>
            Import: <http://foo.org/a.omn>
            Class: some-other-class-at-c
                Annotations:
                    rdfs:label "Some other class at c"
    "#;

    // Act

    service
        .inner()
        .did_open(DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri: url.clone(),
                language_id: "owl2md".to_string(),
                version: 0,
                text: ontology.to_string(),
            },
        })
        .await;

    // Assert

    let workspaces = service.inner().workspaces.read();
    let workspace = workspaces
        .iter()
        .exactly_one()
        .expect("Only one workspace should be crated");

    assert_eq!(
        workspace.internal_documents.len(),
        1,
        "One internal document should be loaded. It was given"
    );
    assert_eq!(
        workspace.external_documents.len(),
        1,
        "One external document should be loaded"
    );
}

#[test(tokio::test)]
async fn backend_did_open_should_load_external_documents_via_file() {
    // Arrange

    let tmp_dir = arrange_workspace_folders(|dir| {
        vec![
            WorkspaceMember::CatalogFile(
                Catalog::new(dir.join("catalog-v001.xml").to_str().unwrap())
                    .with_uri("http://foo.org/a", "a.owx")
                    .with_uri("http://foo.org/c", "c.omn"),
            ),
            WorkspaceMember::OwxFile {
                name: "a.owx".into(),
                content: r##"
                    <?xml version="1.0"?>
                    <Ontology xmlns="http://www.w3.org/2002/07/owl#"
                         xml:base="http://foo.org/a"
                         xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                         xmlns:xml="http://www.w3.org/XML/1998/namespace"
                         xmlns:xsd="http://www.w3.org/2001/XMLSchema#"
                         xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#"
                         ontologyIRI="http://foo.org/a">

                        <Declaration>
                            <Class IRI="#SomeOtherClass"/>
                        </Declaration>
    
                        <AnnotationAssertion>
                            <AnnotationProperty abbreviatedIRI="rdfs:label"/>
                            <IRI>#SomeOtherClass</IRI>
                            <Literal>Some other class at a</Literal>
                        </AnnotationAssertion>

                    </Ontology>
                "##
                .into(),
            },
        ]
    });

    let service = arrange_backend(
        Some(WorkspaceFolder {
            uri: Url::from_directory_path(tmp_dir.path()).unwrap(),
            name: "foo".into(),
        }),
        vec![],
    )
    .await;

    let url = Url::from_file_path(tmp_dir.path().join("c.omn")).unwrap();

    let ontology = r#"
        Ontology: <http://foo.org/c>
            Import: <http://foo.org/a>
            Class: some-other-class-at-c
                Annotations:
                    rdfs:label "Some other class at c"
    "#;

    // Act

    service
        .inner()
        .did_open(DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri: url.clone(),
                language_id: "owl2md".to_string(),
                version: 0,
                text: ontology.to_string(),
            },
        })
        .await;

    // Assert

    let workspaces = service.inner().workspaces.read();
    let workspace = workspaces
        .iter()
        .exactly_one()
        .expect("Only one workspace should be crated");

    assert_eq!(
        workspace.internal_documents.len(),
        1,
        "One internal document should be loaded. It was given"
    );
    assert_eq!(
        workspace.external_documents.len(),
        1,
        "One external document should be loaded"
    );
}

//////////////////////////
// Arrange
//////////////////////////

#[derive(Debug, Clone)]
enum WorkspaceMember {
    Folder {
        name: String,
        children: Vec<WorkspaceMember>,
    },
    CatalogFile(Catalog),
    OmnFile {
        name: String,
        content: String,
    },
    OwxFile {
        name: String,
        content: String,
    },
}

/// Create a tmp workspace for testing
/// Example:
/// ```
/// vec![
///     WorkspaceMember::CatalogFile(
///         Catalog::new(dir.join("catalog-v001.xml").to_str().unwrap())
///             .with_uri("http://external.org/shared.omn", "foobaronto.omn")
///             .with_uri("http://foobar.org/ontology/", "foo.omn"),
///     ),
///     WorkspaceMember::OmnFile {
///         name: "foobaronto.omn".into(),
///         content: "".into(),
///     },
/// ]
/// ```
fn arrange_workspace_folders<F>(root_member_generator: F) -> TempDir
where
    F: Fn(&Path) -> Vec<WorkspaceMember>,
{
    let dir = TempDir::new("owl-ms-test").unwrap();

    let root_members = root_member_generator(dir.path());
    for member in root_members.clone() {
        arrange_workspace_member(member, dir.path());
    }

    info!("Arranged Workspace Member {:#?}", root_members);

    dir
}
fn arrange_workspace_member(member: WorkspaceMember, path: &Path) {
    match member {
        WorkspaceMember::Folder { name, children } => {
            let inner_path = path.join(name);
            fs::create_dir(inner_path.clone()).unwrap();
            for child in children {
                arrange_workspace_member(child, &inner_path);
            }
        }
        WorkspaceMember::CatalogFile(catalog) => {
            let content = quick_xml::se::to_string::<Catalog>(&catalog).unwrap();
            fs::write(path.join("catalog-v001.xml"), content).unwrap();
        }
        WorkspaceMember::OmnFile { name, content } => {
            fs::write(path.join(name), content).unwrap();
        }
        WorkspaceMember::OwxFile { name, content } => {
            fs::write(path.join(name), content).unwrap();
        }
    }
}

fn arrange_parser() -> Parser {
    let mut parser = Parser::new();
    parser.set_language(*LANGUAGE).unwrap();
    parser
}

async fn arrange_init_backend(
    service: &LspService<Backend>,
    workspacefolder: Option<WorkspaceFolder>,
) {
    let result = service
        .inner()
        .initialize(InitializeParams {
            workspace_folders: workspacefolder.map(|w| vec![w]),
            ..Default::default()
        })
        .await;
    assert!(result.is_ok(), "Initialize returned {:#?}", result);

    service.inner().initialized(InitializedParams {}).await;
}

async fn arrange_backend(
    workspace_folder: Option<WorkspaceFolder>,
    data: Vec<(&str, &str)>,
) -> LspService<Backend> {
    let (service, _) = LspService::new(|client| {
        let mut backend = Backend::new(client);
        backend.http_client = Arc::new(StaticClient {
            data: data
                .iter()
                .map(|(k, v)| (k.to_string(), v.to_string()))
                .collect(),
        });
        backend
    });
    arrange_init_backend(&service, workspace_folder).await;
    service
}

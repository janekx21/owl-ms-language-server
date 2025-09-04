use crate::{catalog::Catalog, range::range_overlaps, web::StaticClient, *};
use anyhow::anyhow;
use indoc::indoc;
use position::Position;
use pretty_assertions::{assert_eq, assert_ne};
use std::{any, fs, path::Path};
use tempdir::{self, TempDir};
use test_log::test;
use tower_lsp::LspService;
use tree_sitter_c2rust::{ParseOptions, Parser};

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
    assert_eq!(tree.root_node().has_error(), false);
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

    let dir = TempDir::new("owl-ms-test").unwrap();
    let url = Url::from_file_path(dir.path().join("foo.omn")).unwrap();

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

    let dir = TempDir::new("owl-ms-test").unwrap();
    let ontology_url = Url::from_file_path(dir.path().join("file.omn")).unwrap();

    service
        .inner()
        .did_open(DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri: ontology_url.clone(),
                language_id: "owl2md".to_string(),
                version: 0,
                text: "DE😊F".to_string(),
            },
        })
        .await;

    // Act
    service
        .inner()
        .did_change(DidChangeTextDocumentParams {
            text_document: VersionedTextDocumentIdentifier {
                uri: ontology_url.clone(),
                version: 2,
            },
            content_changes: vec![
                TextDocumentContentChangeEvent {
                    range: Some(
                        Range {
                            start: (Position::new(0, 0)),
                            end: Position::new(0, 0),
                        }
                        .into(),
                    ),
                    range_length: None,
                    text: "A😊BC".to_string(),
                },
                TextDocumentContentChangeEvent {
                    range: Some(
                        Range {
                            start: (Position::new(
                                0,
                                // Changes depend on each other in order. By the time that the first
                                // change is applied the line ends at charater 6 not 3.
                                14,
                            )),
                            end: Position::new(0, 14),
                        }
                        .into(),
                    ),
                    range_length: None,
                    text: "GH😊I".to_string(),
                },
            ],
        })
        .await;

    // Assert
    let workspace = service.inner().find_workspace(&ontology_url);

    let doc = workspace
        .internal_documents
        .get(&ontology_url.clone())
        .expect("found the document");
    let doc = doc.read();
    let doc_content = doc.rope.to_string();

    assert_eq!(doc_content, "A😊BCDE😊FGH😊I");
}

#[test(tokio::test)]
async fn backend_hover_on_class_should_show_class_info() {
    // Arrange
    let service = arrange_backend(None, vec![]).await;

    let dir = TempDir::new("owl-ms-test").unwrap();
    let url = Url::from_file_path(dir.path().join("file.omn")).unwrap();

    let ontology = r#"
        Prefix: rdfs: <http://www.w3.org/2000/01/rdf-schema#>
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
                position: Position::new(3, 21).into(),
            },
            work_done_progress_params: WorkDoneProgressParams {
                work_done_token: None,
            },
        })
        .await
        .unwrap();

    // Assert
    assert_empty_diagnostics(&service);
    let hover_result = hover_result.unwrap();
    let range = hover_result.range.unwrap();
    assert_eq!(
        range,
        // Range of the "Janek" in the ontology
        Range {
            start: Position::new(3, 19),
            end: Position::new(3, 24)
        }
        .into()
    );

    let contents = match hover_result.contents {
        HoverContents::Scalar(MarkedString::String(str)) => str,
        _ => panic!("Did not think of that"),
    };

    assert!(contents.contains("Class"));
    assert!(contents.contains("**Janek der Coder**"));
    assert!(contents.contains("`label`: Janek der Coder"));
    assert!(contents.contains("IRI: Janek"));
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
                        Prefix: #comment
                            : #comment
                            <http://ontology-a.org/ontology#> #comment
                        Prefix: #comment
                            rdfs: #comment
                            <http://www.w3.org/2000/01/rdf-schema#> #comment
                        Ontology: #comment
                            <http://ontology-a.org/a2.omn> #comment
                            Class: #comment
                                ClassA2 #comment
                                Annotations: #comment
                                    rdfs:label #comment
                                    "Some class in A2", #comment
                                    test #comment
                                    "test" #comment
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
                        Prefix: : <http://ontology-a.org/ontology#>
                        Prefix: rdfs: <http://www.w3.org/2000/01/rdf-schema#>
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
        Prefix: : <http://ontology-a.org/ontology#>
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
    let hover_result = service
        .inner()
        .hover(HoverParams {
            text_document_position_params: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri: url.clone() },
                position: Position::new(7, 31).into(),
            },
            work_done_progress_params: WorkDoneProgressParams {
                work_done_token: None,
            },
        })
        .await
        .unwrap();

    // Assert
    assert_empty_diagnostics(&service);
    let hover_result = hover_result.unwrap();
    let range = hover_result.range.unwrap();
    assert_eq!(
        range,
        // Range of the "Janek" in the ontology
        Range {
            start: Position::new(7, 28),
            end: Position::new(7, 35)
        }
        .into()
    );

    let contents = match hover_result.contents {
        HoverContents::Scalar(MarkedString::String(str)) => str,
        _ => panic!("Did not think of that"),
    };
    info!("{:#?}", service.inner().workspaces.read());
    info!("{contents}");
    assert!(contents.contains("Some class in A2"));
    assert!(contents.contains("test"));
}

#[test(tokio::test)]
async fn backend_hover_in_multi_file_ontology_on_not_imported_iri_should_not_work() {
    // Arrange
    let (service, tmp_dir) = arrange_multi_file_ontology().await;

    let url = Url::from_file_path(tmp_dir.path().join("ontology-a").join("a1.omn")).unwrap();

    // Act
    let hover_result = service
        .inner()
        .hover(HoverParams {
            text_document_position_params: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri: url.clone() },
                position: Position::new(7, 38).into(),
            },
            work_done_progress_params: WorkDoneProgressParams {
                work_done_token: None,
            },
        })
        .await
        .unwrap();

    // Assert
    assert_empty_diagnostics(&service);
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
async fn backend_hover_on_external_simple_iri_should_show_external_info() {
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
                     xml:base="http://foo.org/ontology"
                     xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                     xmlns:xml="http://www.w3.org/XML/1998/namespace"
                     xmlns:xsd="http://www.w3.org/2001/XMLSchema#"
                     xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#"
                     ontologyIRI="http://foo.org/ontology#">

                     <Prefix name="" IRI="http://foo.org/ontology#"/>
                     <Prefix name="rdfs" IRI="http://www.w3.org/2000/01/rdf-schema#"/>

                    <Declaration>
                        <Class IRI="ClassA2"/>
                    </Declaration>
    
                    <AnnotationAssertion>
                        <AnnotationProperty abbreviatedIRI="rdfs:label"/>
                        <IRI>ClassA2</IRI>
                        <Literal>Some class in A2</Literal>
                    </AnnotationAssertion>

                </Ontology>
            "##,
        )],
    )
    .await;

    let url = Url::from_file_path(tmp_dir.path().join("ontology-a").join("a1.omn")).unwrap();

    let ontology = r#"
        Prefix: : <http://foo.org/ontology#>
        Prefix: rdfs: <http://www.w3.org/2000/01/rdf-schema#>
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
    let hover_result = service
        .inner()
        .hover(HoverParams {
            text_document_position_params: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri: url.clone() },
                position: Position::new(8, 32).into(),
            },
            work_done_progress_params: WorkDoneProgressParams {
                work_done_token: None,
            },
        })
        .await
        .unwrap();

    // Assert
    assert_empty_diagnostics(&service);
    let hover_result = hover_result.unwrap();

    let contents = match hover_result.contents {
        HoverContents::Scalar(MarkedString::String(str)) => str,
        _ => panic!("Did not think of that"),
    };
    info!("{:#?}", service.inner().workspaces.read());
    info!("contents={contents}");
    assert!(!contents.contains("**ClassA2**"));
    assert!(contents.contains("Some class in A2"));
}

#[test(tokio::test)]
async fn backend_hover_on_external_full_iri_should_show_external_info() {
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

                     <Prefix name="rdfs" IRI="http://www.w3.org/2000/01/rdf-schema#"/>

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
        Prefix: rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        Ontology: <http://ontology-a.org/a1>
            Import: <http://ontology-a.org/a2>
            Class: ClassA1
                Annotations:
                    rdfs:label "Some class in A1"
                SubClassOf: <http://ontology-a.org/a2.owx#ClassA2>,ClassB2

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
    let hover_result = service
        .inner()
        .hover(HoverParams {
            text_document_position_params: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri: url.clone() },
                position: Position::new(7, 32).into(),
            },
            work_done_progress_params: WorkDoneProgressParams {
                work_done_token: None,
            },
        })
        .await
        .unwrap();

    // Assert
    assert_empty_diagnostics(&service);
    let hover_result = hover_result.unwrap();

    let contents = match hover_result.contents {
        HoverContents::Scalar(MarkedString::String(str)) => str,
        _ => panic!("Did not think of that"),
    };
    info!("contents={contents}");
    assert!(!contents.contains("**ClassA2**"));
    assert!(contents.contains("Some class in A2"));
    assert!(contents.contains("IRI: http://ontology-a.org/a2.owx#ClassA2"));
}
#[test(tokio::test)]
async fn backend_hover_on_external_rdf_document_at_simple_iri_should_show_external_info() {
    // Arrange
    let tmp_dir = arrange_workspace_folders(|dir| {
        vec![WorkspaceMember::Folder {
            name: "ontology-a".into(),
            children: vec![WorkspaceMember::CatalogFile(
                Catalog::new(dir.join("catalog-v001.xml").to_str().unwrap())
                    .with_uri("http://ontology-a.org/a1", "a1.omn")
                    .with_uri("http://ontology-a.org/a2", "http://ontology-a.org/a2.rdf"),
            )],
        }]
    });

    let service = arrange_backend(
        Some(WorkspaceFolder {
            uri: Url::from_directory_path(tmp_dir.path()).unwrap(),
            name: "test wosrkpace".into(),
        }),
        vec![(
            "http://ontology-a.org/a2.rdf",
            r##"
                <?xml version="1.0"?>
                <rdf:RDF xmlns="http://www.w3.org/2002/07/owl#"
                     xml:base="http://www.w3.org/2002/07/owl"
                     xmlns:owl="http://www.w3.org/2002/07/owl#"
                     xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                     xmlns:xml="http://www.w3.org/XML/1998/namespace"
                     xmlns:xsd="http://www.w3.org/2001/XMLSchema#"
                     xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#">
                    <Ontology rdf:about="http://ontology-a.org/a2.rdf"/>

                    <!-- http://ontology-a.org/a2#ClassA2 -->

                    <Class rdf:about="http://foo.org/ontology#ClassA2">
                        <rdfs:label>Some class in A2</rdfs:label>
                    </Class>
                </rdf:RDF>
            "##,
        )],
    )
    .await;

    let url = Url::from_file_path(tmp_dir.path().join("ontology-a").join("a1.omn")).unwrap();

    let ontology = r#"
        Prefix: : <http://foo.org/ontology#>
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
    let hover_result = service
        .inner()
        .hover(HoverParams {
            text_document_position_params: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri: url.clone() },
                position: Position::new(7, 32).into(),
            },
            work_done_progress_params: WorkDoneProgressParams {
                work_done_token: None,
            },
        })
        .await
        .unwrap();

    // Assert
    assert_empty_diagnostics(&service);
    let hover_result = hover_result.unwrap();

    let contents = match hover_result.contents {
        HoverContents::Scalar(MarkedString::String(str)) => str,
        _ => panic!("Did not think of that"),
    };
    info!("{:#?}", service.inner().workspaces.read());
    info!("contents={contents}");
    assert!(!contents.contains("**ClassA2**"));
    assert!(contents.contains("Some class in A2"));
}

#[test(tokio::test)]
#[ignore = "use internal document test first"] // TODO reactivate
async fn backend_formatting_on_file_should_correctly_format() {
    // Arrange

    let source = indoc! {"
    Prefix:    a:    <http://a.b/c/>

    Prefix:    b:    <http://a.b/b/>
    Ontology:   foo    ver
    Class:    A    SubClassOf: Y    Annotations:   rdfs:label    \"Y\"    EquivalentTo:    Y

                 DisjointWith:    Y   DisjointUnionOf:    Y,Z    HasKey:    Y


     
    Datatype:     B
               EquivalentTo:    Y
    DataProperty:    C
    ObjectProperty:    D
    AnnotationProperty:    E
    Individual:    F
    Class: C
    SubClassOf: p some (A and B)
    SubClassOf: inverse p some (A and B)
    SubClassOf: inverse p some A and B
    "};

    let target = indoc! {"
    Prefix: a: <http://a.b/c/>
    Prefix: b: <http://a.b/b/>

    Ontology: foo ver


    Class: A
        SubClassOf: Y
        Annotations: rdfs:label \"Y\"
        EquivalentTo: Y
        DisjointWith: Y
        DisjointUnionOf: Y, Z
        HasKey: Y

    Datatype: B
        EquivalentTo: Y

    DataProperty: C


    ObjectProperty: D


    AnnotationProperty: E


    Individual: F


    Class: C
        SubClassOf: p some ( A and B )
        SubClassOf: inverse p some ( A and B )
        SubClassOf: inverse p some A and B
    "};

    let tmp_dir = arrange_workspace_folders(|_| vec![]);

    let service = arrange_backend(None, vec![]).await;

    let url = Url::from_file_path(tmp_dir.path().join("main.omn")).unwrap();
    service
        .inner()
        .did_open(DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri: url.clone(),
                language_id: "owl-ms".to_string(),
                version: 0,
                text: source.to_string(),
            },
        })
        .await;

    // Act
    let result = service
        .inner()
        .formatting(DocumentFormattingParams {
            text_document: TextDocumentIdentifier { uri: url.clone() },
            work_done_progress_params: WorkDoneProgressParams {
                work_done_token: None,
            },
            options: FormattingOptions {
                tab_size: 4,
                insert_spaces: true,
                ..FormattingOptions::default()
            },
        })
        .await
        .unwrap();

    // Assert
    assert_empty_diagnostics(&service);
    let edits = result.unwrap();

    let any_overlaps = edits
        .iter()
        .tuple_combinations()
        .any(|(a, b)| range_overlaps(&a.range.into(), &b.range.into()));
    assert!(!any_overlaps);

    service
        .inner()
        .did_change(DidChangeTextDocumentParams {
            text_document: VersionedTextDocumentIdentifier {
                uri: url.clone(),
                version: 1,
            },
            content_changes: edits
                .iter()
                .sorted_by_key(|e| e.range.start)
                .rev()
                .map(|e| TextDocumentContentChangeEvent {
                    range: Some(e.range),
                    range_length: None,
                    text: e.new_text.clone(),
                })
                .collect(),
        })
        .await;

    let ws = service.inner().find_workspace(&url);
    // let ws = workspaces.iter().exactly_one().unwrap();
    let doc = ws.internal_documents.get(&url).unwrap();
    let doc = doc.read();
    assert_eq!(doc.diagnostics, vec![], "doc:\n{}", doc.rope.to_string());
    assert_eq!(doc.rope.to_string(), target);
}

#[test(tokio::test)]
async fn backend_inlay_hint_on_external_simple_iri_should_show_iri() {
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
                     ontologyIRI="http://foo.org/ontology#">

                     <Prefix name="" IRI="http://foo.org/ontology#"/>
                     <Prefix name="rdfs" IRI="http://www.w3.org/2000/01/rdf-schema#"/>

                    <Declaration>
                        <Class IRI="ClassA2"/>
                    </Declaration>
    
                    <AnnotationAssertion>
                        <AnnotationProperty abbreviatedIRI="rdfs:label"/>
                        <IRI>ClassA2</IRI>
                        <Literal>Some class in A2</Literal>
                    </AnnotationAssertion>

                </Ontology>
            "##,
        )],
    )
    .await;

    let url = Url::from_file_path(tmp_dir.path().join("ontology-a").join("a1.omn")).unwrap();

    let ontology = r#"
        Prefix: : <http://foo.org/ontology#>
        Prefix: rdfs: <http://www.w3.org/2000/01/rdf-schema#>
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
    let result = service
        .inner()
        .inlay_hint(InlayHintParams {
            work_done_progress_params: WorkDoneProgressParams {
                work_done_token: None,
            },
            text_document: TextDocumentIdentifier { uri: url.clone() },
            range: Range {
                start: Position::new(0, 0),
                end: Position::new(999, 0),
            }
            .into(),
        })
        .await
        .unwrap();

    // Assert
    assert_empty_diagnostics(&service);
    let result = result.unwrap();

    info!("result={result:#?}");
    assert_eq!(result.len(), 2); // no rdfs:label inlay hint anymore

    assert!(result.iter().any(|x| match &x.label {
        InlayHintLabel::String(a) => a.contains("Some class in A1"),
        InlayHintLabel::LabelParts(_) => unreachable!(),
    }));

    assert!(result.iter().any(|x| match &x.label {
        InlayHintLabel::String(a) => {
            a.contains("Some class in A2")
        }
        InlayHintLabel::LabelParts(_) => unreachable!(),
    }));
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
    assert_empty_diagnostics(&service);
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

    let dir = TempDir::new("owl-ms-test").unwrap();
    let ontology_url = Url::from_file_path(dir.path().join("file.omn")).unwrap();

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
                        start: Position::new(2, 0),
                        end: Position::new(2, 61),
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
    assert_empty_diagnostics(&service);
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
                    .with_uri("http://foo.org/a.omn", "http://foo.org/version/file.owx")
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
            "http://foo.org/version/file.owx",
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

    assert_empty_diagnostics(&service);
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

    assert_empty_diagnostics(&service);
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
async fn backend_completion_should_work_for_keyword_class() {
    let ontology = indoc! {r#"
        Ontology: <http://foo.org/a>

            <PARTIAL>

            Class: c
    "#};
    backend_completion_test_helper("Cl", "Class:", ontology).await;
}

#[test(tokio::test)]
async fn backend_completion_should_work_for_keyword_class_at_the_end() {
    let ontology = indoc! {r#"
        Ontology: <http://foo.org/a> Version

        <PARTIAL>
    "#};
    backend_completion_test_helper("Cl", "Class:", ontology).await;
}

#[test(tokio::test)]
async fn backend_completion_should_work_for_keyword_datatype() {
    let ontology = indoc! {r#"
        Ontology: <http://foo.org/a>

            <PARTIAL>

            Class: c
    "#};
    backend_completion_test_helper("Datat", "Datatype:", ontology).await;
}

#[test(tokio::test)]
async fn backend_completion_should_work_for_keyword_integer() {
    let ontology = indoc! {r#"
        Ontology: <http://foo.org/a>

            Datatype: Foo
                EquivalentTo: (<PARTIAL>

            Class: c
    "#};
    backend_completion_test_helper("in", "integer", ontology).await;
}

#[test(tokio::test)]
async fn backend_completion_should_work_for_keyword_some() {
    let ontology = indoc! {r#"
        Ontology: <http://foo.org/a>

            Ontology:
            	Class: Person
            		SubClassOf: hasAge <PARTIAL> 1 and hasAge only not NegInt

            Class: c
    "#};
    backend_completion_test_helper("so", "some", ontology).await;
}

#[test(tokio::test)]
async fn backend_completion_should_work_for_keyword_functionnal() {
    let ontology = indoc! {r#"
        Ontology: <http://foo.org/a>

            Ontology:
            	ObjectProperty: Thing
            		Characteristics: <PARTIAL>

            Class: c
    "#};
    backend_completion_test_helper("Fu", "Functional", ontology).await;
}

async fn backend_completion_test_helper(partial: &str, full: &str, ontology: &str) {
    // Arrange

    let tmp_dir = arrange_workspace_folders(|_| vec![]);
    let service = arrange_backend(
        Some(WorkspaceFolder {
            uri: Url::from_directory_path(tmp_dir.path()).unwrap(),
            name: "foo".into(),
        }),
        vec![],
    )
    .await;

    let url = Url::from_file_path(tmp_dir.path().join("a.omn")).unwrap();

    let pos = ontology
        .lines()
        .enumerate()
        .find_map(|(li, line)| {
            line.find("<PARTIAL>")
                .map(|ci| Position::new(li as u32, ci as u32))
        })
        .expect("Should contain <PARTIAL> str");

    let ontology = ontology.replace("<PARTIAL>", partial);

    let mut pos = pos.to_owned();
    pos.add_character(partial.len() as u32);

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
        .completion(CompletionParams {
            text_document_position: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri: url },
                position: pos.into(),
            },
            work_done_progress_params: WorkDoneProgressParams {
                work_done_token: None,
            },
            partial_result_params: PartialResultParams {
                partial_result_token: None,
            },
            context: None,
        })
        .await;

    // Assert

    let result = result.expect("Result shoud not produce error");
    let result = result.expect("Result should contain completion response");

    match result {
        CompletionResponse::Array(completion_items) => {
            let completion_items = completion_items
                .iter()
                .filter(|i| i.kind == Some(CompletionItemKind::KEYWORD))
                .collect_vec();
            assert_eq!(completion_items.len(), 1);

            assert_eq!(completion_items[0].label, full);
        }
        CompletionResponse::List(_) => unimplemented!(),
    }
}

#[test(tokio::test)]
async fn backend_completion_should_work_for_keywords() {
    // Arrange

    let tmp_dir = arrange_workspace_folders(|_| vec![]);
    let service = arrange_backend(
        Some(WorkspaceFolder {
            uri: Url::from_directory_path(tmp_dir.path()).unwrap(),
            name: "foo".into(),
        }),
        vec![],
    )
    .await;

    let url = Url::from_file_path(tmp_dir.path().join("a.omn")).unwrap();

    let ontology = indoc! {r#"
        Ontology: <http://foo.org/a>

            Cl
        
            Class: some-other-class-at-c
                Annotations:
                    rdfs:label "Some other class at c"
    "#};

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
        .completion(CompletionParams {
            text_document_position: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri: url },
                position: Position::new(2, 6).into(),
            },
            work_done_progress_params: WorkDoneProgressParams {
                work_done_token: None,
            },
            partial_result_params: PartialResultParams {
                partial_result_token: None,
            },
            context: None,
        })
        .await;

    // Assert

    let result = result.expect("Result shoud not produce error");
    let result = result.expect("Result should contain completion response");

    match result {
        CompletionResponse::Array(completion_items) => {
            assert_eq!(completion_items.len(), 1);

            assert_eq!(completion_items[0].label, "Class:");
        }
        CompletionResponse::List(_) => unimplemented!(),
    }
}

#[test(tokio::test)]
async fn backend_references_in_multi_file_ontology_should_work() {
    // Arrange
    let (service, tmp_dir) = arrange_multi_file_ontology().await;

    let url = Url::from_file_path(tmp_dir.path().join("ontology-a").join("a1.omn")).unwrap();

    // Act
    let result = service
        .inner()
        .references(ReferenceParams {
            text_document_position: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri: url.clone() },
                position: Position::new(7, 31).into(),
            },
            work_done_progress_params: WorkDoneProgressParams {
                work_done_token: None,
            },
            partial_result_params: PartialResultParams {
                partial_result_token: None,
            },
            context: ReferenceContext {
                include_declaration: true,
            },
        })
        .await
        .unwrap();

    // Assert
    assert_empty_diagnostics(&service);
    let url2 = Url::from_file_path(tmp_dir.path().join("ontology-a").join("a2.omn")).unwrap();
    let result = result.unwrap();
    info!("{result:#?}");
    assert_eq!(result.len(), 2);
    assert!(result.iter().any(|l| l.uri == url));
    assert!(result.iter().any(|l| l.uri == url2));
}

#[test(tokio::test)]
async fn backend_goto_definition_in_multi_file_ontology_should_work() {
    // Arrange
    let (service, tmp_dir) = arrange_multi_file_ontology().await;

    let url = Url::from_file_path(tmp_dir.path().join("ontology-a").join("a1.omn")).unwrap();

    // Act
    let result = service
        .inner()
        .goto_definition(GotoDefinitionParams {
            text_document_position_params: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri: url.clone() },
                position: Position::new(7, 31).into(),
            },
            work_done_progress_params: WorkDoneProgressParams {
                work_done_token: None,
            },
            partial_result_params: PartialResultParams {
                partial_result_token: None,
            },
        })
        .await
        .unwrap();

    // Assert
    assert_empty_diagnostics(&service);
    info!("{:#?}", service.inner().workspaces.read());
    let url2 = Url::from_file_path(tmp_dir.path().join("ontology-a").join("a2.omn")).unwrap();
    let result = result.unwrap();
    match result {
        GotoDefinitionResponse::Array(locations) => {
            let location = locations.into_iter().exactly_one().unwrap();
            assert_eq!(location.uri, url2);
        }
        _ => todo!(),
    }
}

#[test(tokio::test)]
async fn backend_document_symbols_in_multi_file_ontology_should_just_show_symbols_from_active_file()
{
    // Arrange
    let (service, tmp_dir) = arrange_multi_file_ontology().await;

    let url = Url::from_file_path(tmp_dir.path().join("ontology-a").join("a2.omn")).unwrap();

    // Act
    let result = service
        .inner()
        .document_symbol(DocumentSymbolParams {
            text_document: TextDocumentIdentifier { uri: url.clone() },
            work_done_progress_params: WorkDoneProgressParams {
                work_done_token: None,
            },
            partial_result_params: PartialResultParams {
                partial_result_token: None,
            },
        })
        .await
        .unwrap();

    // Assert
    assert_empty_diagnostics(&service);
    match result.unwrap() {
        DocumentSymbolResponse::Flat(symbol_informations) => {
            let info = symbol_informations.iter().exactly_one().unwrap();
            assert_eq!(info.name, "http://ontology-a.org/ontology#ClassA2");
        }
        DocumentSymbolResponse::Nested(_document_symbols) => todo!(),
    }
}

#[test(tokio::test)]
async fn backend_rename_simple_iri_should_work() {
    let ontology = indoc! {"
        Prefix: : <https://example.com/ontology#>
        Prefix: o: <https://example.com/ontology#>

        Ontology:
        Class: B
            SubClassOf: o:B, B, <https://example.com/ontology#B>
    "};
    let new_ontology = indoc! {"
        Prefix: : <https://example.com/ontology#>
        Prefix: o: <https://example.com/ontology#>

        Ontology:
        Class: beta
            SubClassOf: beta, beta, beta
    "};

    backend_rename_helper(ontology, new_ontology, Position::new(4, 7), "beta").await;
}

#[test(tokio::test)]
async fn backend_rename_at_end_should_work() {
    let ontology = indoc! {"
        Prefix: : <https://example.com/ontology#>

        Ontology:
        Class: B
    "};
    let new_ontology = indoc! {"
        Prefix: : <https://example.com/ontology#>

        Ontology:
        Class: beta
    "};

    backend_rename_helper(ontology, new_ontology, Position::new(3, 8), "beta").await;
}

#[test(tokio::test)]
async fn backend_rename_prefixless_simple_iri_should_work() {
    let ontology = indoc! {"
        Ontology:
        Class: B
            SubClassOf: B
    "};
    let new_ontology = indoc! {"
        Ontology:
        Class: beta
            SubClassOf: beta
    "};

    backend_rename_helper(ontology, new_ontology, Position::new(1, 7), "beta").await;
}

#[test(tokio::test)]
async fn backend_rename_unknown_abbriviated_iri_should_work() {
    let ontology = indoc! {"
        Ontology:
        Class: unknown:B
            SubClassOf: unknown:B
    "};
    let new_ontology = indoc! {"
        Ontology:
        Class: unknown:beta
            SubClassOf: unknown:beta
    "};

    backend_rename_helper(ontology, new_ontology, Position::new(1, 7), "beta").await;
}

#[test(tokio::test)]
async fn backend_rename_full_iri_should_shorten() {
    let ontology = indoc! {"
        Prefix: : <https://example.com/ontology#>
        Prefix: o: <https://example.com/ontology#>

        Ontology:
        Class: <https://example.com/ontology#B>
            SubClassOf: o:B, B, <https://example.com/ontology#B>
    "};
    let new_ontology = indoc! {"
        Prefix: : <https://example.com/ontology#>
        Prefix: o: <https://example.com/ontology#>

        Ontology:
        Class: beta
            SubClassOf: beta, beta, beta
    "};

    backend_rename_helper(
        ontology,
        new_ontology,
        Position::new(4, 7),
        "https://example.com/ontology#beta",
    )
    .await;
}

#[test(tokio::test)]
async fn backend_rename_abbriviated_iri_should_work_for_matching() {
    let ontology = indoc! {"
        Prefix: o: <https://example.com/ontology#>

        Ontology:
        Class: o:B
            SubClassOf: o:B, B, <https://example.com/ontology#B>
    "};
    let new_ontology = indoc! {"
        Prefix: o: <https://example.com/ontology#>

        Ontology:
        Class: o:beta
            SubClassOf: o:beta, B, o:beta
    "};

    backend_rename_helper(ontology, new_ontology, Position::new(3, 9), "beta").await;
}

#[test(tokio::test)]
async fn backend_rename_full_iri_should_work_for_matching() {
    let ontology = indoc! {"
        Ontology:
        Class: <https://example.com/ontology#B>
            SubClassOf: o:B, B, <https://example.com/ontology#B>
    "};
    let new_ontology = indoc! {"
        Ontology:
        Class: <https://example.com/ontology#beta>
            SubClassOf: o:B, B, <https://example.com/ontology#beta>
    "};

    backend_rename_helper(
        ontology,
        new_ontology,
        Position::new(1, 9),
        "https://example.com/ontology#beta",
    )
    .await;
}

#[test(tokio::test)]
async fn backend_rename_full_iri_should_not_shorten() {
    let ontology = indoc! {"
        Prefix: o: <https://example.com/ontology#B>

        Ontology:
        Class: <https://example.com/ontology#B>
            SubClassOf: o:B, B, <https://example.com/ontology#B>
    "};
    let new_ontology = indoc! {"
        Prefix: o: <https://example.com/ontology#B>

        Ontology:
        Class: <https://example.com/things#beta>
            SubClassOf: o:B, B, <https://example.com/things#beta>
    "};

    backend_rename_helper(
        ontology,
        new_ontology,
        Position::new(3, 9),
        "https://example.com/things#beta",
    )
    .await;
}

async fn backend_rename_helper(
    old_ontology: &str,
    new_ontology: &str,
    position: Position,
    new_name: &str,
) {
    // Arrange
    let service = arrange_backend(None, vec![]).await;

    let dir = TempDir::new("owl-ms-test").unwrap();
    let url = Url::from_file_path(dir.path().join("foo.omn")).unwrap();

    service
        .inner()
        .did_open(DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri: url.clone(),
                language_id: "owl2md".to_string(),
                version: 0,
                text: old_ontology.to_string(),
            },
        })
        .await;

    // Act
    let result = service
        .inner()
        .rename(RenameParams {
            text_document_position: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri: url.clone() },
                position: position.into(),
            },
            new_name: new_name.to_string(),
            work_done_progress_params: WorkDoneProgressParams {
                work_done_token: None,
            },
        })
        .await;

    // Assert
    assert_empty_diagnostics(&service);
    let result = result.unwrap();
    let result = result.unwrap();

    let changes = result.changes.unwrap();
    for (url, edits) in changes {
        service
            .inner()
            .did_change(DidChangeTextDocumentParams {
                text_document: VersionedTextDocumentIdentifier {
                    uri: url.clone(),
                    version: 1,
                },
                content_changes: edits
                    .iter()
                    .sorted_by_key(|e| e.range.start)
                    .rev()
                    .map(|e| TextDocumentContentChangeEvent {
                        range: Some(e.range),
                        range_length: None,
                        text: e.new_text.clone(),
                    })
                    .collect(),
            })
            .await;
    }

    let workspaces = service.inner().workspaces.read();
    let workspace = workspaces.iter().exactly_one().unwrap();
    let doc = workspace.internal_documents.get(&url).unwrap();
    let doc = doc.read();
    let doc_content = doc.rope.to_string();

    assert_eq!(doc_content, new_ontology);
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
    parser.set_language(&LANGUAGE).unwrap();
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

fn assert_empty_diagnostics(service: &LspService<Backend>) {
    let workspaces = service.inner().workspaces.read();
    for workspace in workspaces.iter() {
        for doc in workspace.internal_documents.iter() {
            let doc = doc.value().read();
            assert_eq!(doc.diagnostics, vec![], "rope:\n{}", doc.rope.to_string());
        }
    }
}

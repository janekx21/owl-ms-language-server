use std::{fs::File, io::BufWriter};
use tempdir::{self, TempDir};

use quick_xml::de::from_str;
use test_log::test;

use crate::{
    catalog::{Catalog, CatalogUri},
    *,
};

#[test]
fn test_parse() {
    let mut parser = arrange_parser();

    let source_code = "Ontology: Foobar";
    let tree = parser.parse(source_code, None).unwrap();

    assert_eq!(
        tree.root_node().to_sexp(),
        "(source_file (ontology (ontology_iri (simple_iri))))"
    );
}

#[test]
fn test_parse_datatype() {
    let mut parser = arrange_parser();

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
#[test(tokio::test)]
async fn test_language_server_did_change() {
    // Arrange
    let (service, _) = LspService::new(|client| Backend::new(client, arrange_parser()));

    let result = service
        .inner()
        .initialize(InitializeParams {
            ..Default::default()
        })
        .await;
    assert!(result.is_ok(), "initialize returned {:#?}", result);

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

    // Assert
    let workspace = service.inner().find_workspace(&url).await;

    let doc = workspace
        .document_map
        .get(&url.clone())
        .expect("found the document");
    let doc_content = doc.rope.to_string();

    assert_eq!(doc_content, "ABCDEFGHI");
}

#[test]
fn test_parsing_catalog() {
    let xml = r#"
        <?xml version="1.0" encoding="UTF-8" standalone="no"?>
        <catalog prefer="public" xmlns="urn:oasis:names:tc:entity:xmlns:xml:catalog">
            <uri id="User Entered Import Resolution" name="http://openenergy-platform.org/ontology/oeo/dev/imports/cco-extracted.owl" uri="imports/cco-extracted.owl"/>
            <uri id="Imports Wizard Entry" name="http://openenergy-platform.org/ontology/oeo/" uri="oeo.omn"/>
            <uri id="Imports Wizard Entry" name="http://openenergy-platform.org/ontology/oeo/dev/oeo-physical.omn" uri="edits/oeo-physical.omn"/>
            <uri id="Imports Wizard Entry" name="http://openenergy-platform.org/ontology/oeo/dev/oeo-physical-axioms.owl" uri="edits/oeo-physical-axioms.owl"/>
            <uri id="Imports Wizard Entry" name="http://openenergy-platform.org/ontology/oeo/dev/oeo-sector.omn" uri="edits/oeo-sector.omn"/>
            <uri id="Imports Wizard Entry" name="http://openenergy-platform.org/ontology/oeo/dev/oeo-social.omn" uri="edits/oeo-social.omn"/>
            <uri id="Imports Wizard Entry" name="http://openenergy-platform.org/ontology/oeo/dev/oeo-model.omn" uri="edits/oeo-model.omn"/>
            <uri id="Imports Wizard Entry" name="http://openenergy-platform.org/ontology/oeo/dev/oeo-import-edits.owl" uri="edits/oeo-import-edits.owl"/>
            <uri id="Imports Wizard Entry" name="http://openenergy-platform.org/ontology/oeo/dev/oeo-shared.omn" uri="edits/oeo-shared.omn"/>
            <uri id="Imports Wizard Entry" name="http://openenergy-platform.org/ontology/oeo/dev/oeo-shared-axioms.omn" uri="edits/oeo-shared-axioms.omn"/>
            <uri id="Imports Wizard Entry" name="http://openenergy-platform.org/ontology/oeo/dev/imports/iao-annotation-module.owl" uri="imports/iao-annotation-module.owl"/>
            <uri id="Imports Wizard Entry" name="http://openenergy-platform.org/ontology/oeo/dev/imports/iao-extracted.owl" uri="imports/iao-extracted.owl"/>
            <uri id="Imports Wizard Entry" name="http://openenergy-platform.org/ontology/oeo/dev/imports/iao-minimal-module.owl" uri="imports/iao-minimal-module.owl"/>
            <uri id="Imports Wizard Entry" name="http://openenergy-platform.org/ontology/oeo/dev/imports/omo-extracted.owl" uri="imports/omo-extracted.owl"/>
            <uri id="Imports Wizard Entry" name="http://openenergy-platform.org/ontology/oeo/dev/imports/ro-extracted.owl" uri="imports/ro-extracted.owl"/>
            <uri id="Imports Wizard Entry" name="http://openenergy-platform.org/ontology/oeo/dev/imports/uo-extracted.owl" uri="imports/uo-extracted.owl"/>
        	<uri id="Imports Wizard Entry" name="http://openenergy-platform.org/ontology/oeo/dev/imports/stato-extracted.owl" uri="imports/stato-extracted.owl"/>
        	<uri id="Imports Wizard Entry" name="http://openenergy-platform.org/ontology/oeo/dev/imports/meno-extracted.owl" uri="imports/meno-extracted.owl"/>
            <uri id="Imports Wizard Entry" name="http://purl.obolibrary.org/obo/bfo/2.0/bfo.owl" uri="http://purl.obolibrary.org/obo/bfo.owl"/>
        </catalog>
    "#;

    let catalog: Catalog = from_str(xml).unwrap();

    assert_eq!(catalog.uri.len(), 19);
    assert_eq!(catalog.uri[0].uri, "imports/cco-extracted.owl".to_string());
}

#[test(tokio::test)]
async fn test_import_resolve() {
    // Arrange
    let parser = arrange_parser();

    let dir = TempDir::new("owl-ms-test").unwrap();
    let file_path = dir.path().join("foobaronto.omn");
    File::create(file_path.clone()).unwrap(); // no content for now

    let catalog_path = dir.path().join("catalog-v001.xml");
    let mut catalog_file = File::create(catalog_path).unwrap();

    let catalog = Catalog {
        uri: vec![CatalogUri {
            _id: "Testing".into(),
            name: "http://external.org/shared.omn".into(),
            uri: file_path.file_name().unwrap().to_str().unwrap().to_string(),
        }],
        locaton: dir.path().join("catalog.xml").to_str().unwrap().to_string(),
    };

    let buffer = BufWriter::new(catalog_file);
    quick_xml::se::to_writer::<File, Catalog>(&mut catalog_file, &catalog);

    println!("starting service...");

    let (service, _) = LspService::new(|client| Backend {
        client,
        parser: Mutex::new(parser),
        position_encoding: PositionEncodingKind::UTF16.into(),
        workspaces: Mutex::new(vec![Workspace {
            workspace_folder: vec![],
            document_map: DashMap::new(),
            catalogs: vec![Catalog {
                uri: vec![CatalogUri {
                    _id: "Testing".into(),
                    name: "http://external.org/shared.omn".into(),
                    uri: file_path.file_name().unwrap().to_str().unwrap().to_string(),
                }],
                locaton: dir.path().join("catalog.xml").to_str().unwrap().to_string(),
            }],
        }]),
    });

    arrange_init_backend(
        &service,
        Some(WorkspaceFolder {
            uri: Url::from_directory_path(dir.path()).unwrap(),
            name: "bar".into(),
        }),
    )
    .await;

    let url = Url::from_file_path(dir.path().join("foo.omn")).expect("valid url");

    let ontology = r#"
Ontology: <http://foobar.org/ontology/> <http://foobar.org/ontology.omn>
Import: <http://external.org/shared.omn>
    "#;

    // Act
    println!("open file...");
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

    println!("file opend");

    // Assert

    let workspaces = service.inner().lock_workspaces().await;
    assert_eq!(workspaces.len(), 1);
    let workspace = workspaces.first().unwrap();
    println!(" Workspace documents {:#?}", workspace.document_map);
    let document_count = workspace.document_map.iter().count();
    assert_eq!(document_count, 2);
}

#[test]
fn test_path_segment() {
    // Arrange
    let url = Url::parse("file://example.com/this/is/a/path.html").unwrap();
    let mut url = url.clone();

    // Act
    url.path_segments_mut()
        .expect("The provided url was a cannot-be-a-base url")
        .pop();

    // Assert
    assert_eq!(url.as_str(), "file://example.com/this/is/a");
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
    println!(
        "service started. {:?}",
        service.inner().lock_workspaces().await
    );

    let result = service
        .inner()
        .initialize(InitializeParams {
            workspace_folders: workspacefolder.map(|w| vec![w]),
            ..Default::default()
        })
        .await;
    assert!(result.is_ok(), "initialize returned {:#?}", result);

    service.inner().initialized(InitializedParams {}).await;
}

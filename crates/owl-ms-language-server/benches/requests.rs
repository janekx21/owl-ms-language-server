use criterion::{criterion_group, criterion_main, Criterion};
use owl_ms_language_server::{web::HttpClient, Backend};
use std::{collections::HashMap, hint::black_box};
use tempdir::TempDir;
use tokio::runtime::Runtime;
#[allow(clippy::wildcard_imports)]
use tower_lsp::{lsp_types::*, LanguageServer, LspService};

/// A static HTTP client that returns dummy data for tests
#[derive(Debug)]
struct StaticClient {
    data: HashMap<String, String>,
}

// We cant reuse the test_helpers sadly
impl HttpClient for StaticClient {
    fn get(&self, url: &str) -> owl_ms_language_server::web::Result<String> {
        Ok(self
            .data
            .get(url)
            .unwrap_or(&"dummy".to_string())
            .to_string())
    }
}

/// Sample ontology for benchmarking. Contains multiple classes, properties, and references.
const BENCHMARK_ONTOLOGY: &str = r#"
Prefix: rdfs: <http://www.w3.org/2000/01/rdf-schema#>
Prefix: owl: <http://www.w3.org/2002/07/owl#>
Prefix: ex: <http://example.org/>

Ontology: <http://example.org/benchmark>

    Class: Person
        Annotations:
            rdfs:label "Person"@en,
            rdfs:comment "A human being"@en
        SubClassOf: owl:Thing

    Class: Employee
        Annotations:
            rdfs:label "Employee"@en
        SubClassOf: Person

    Class: Manager
        Annotations:
            rdfs:label "Manager"@en
        SubClassOf: Employee

    Class: Department
        Annotations:
            rdfs:label "Department"@en

    Class: Project
        Annotations:
            rdfs:label "Project"@en

    ObjectProperty: worksIn
        Annotations:
            rdfs:label "works in"@en
        Domain: Employee
        Range: Department

    ObjectProperty: manages
        Annotations:
            rdfs:label "manages"@en
        Domain: Manager
        Range: Department

    ObjectProperty: worksOn
        Annotations:
            rdfs:label "works on"@en
        Domain: Employee
        Range: Project

    DataProperty: hasName
        Annotations:
            rdfs:label "has name"@en
        Domain: Person
        Range: xsd:string

    DataProperty: hasAge
        Annotations:
            rdfs:label "has age"@en
        Domain: Person
        Range: xsd:integer

    Individual: JohnDoe
        Annotations:
            rdfs:label "John Doe"@en
        Types: Employee
        Facts: worksIn SalesDept, hasName "John Doe"

    Individual: JaneSmith
        Annotations:
            rdfs:label "Jane Smith"@en
        Types: Manager
        Facts: manages EngineeringDept

    Individual: SalesDept
        Types: Department

    Individual: EngineeringDept
        Types: Department

    AnnotationProperty: rdfs:label
    AnnotationProperty: rdfs:comment
"#;

async fn setup_backend() -> (LspService<Backend>, Url, TempDir) {
    let http_client = Box::new(StaticClient {
        data: [
            ("http://www.w3.org/1999/02/22-rdf-syntax-ns#", "dummy"),
            ("http://www.w3.org/2002/07/owl#", "dummy"),
            ("http://www.w3.org/2000/01/rdf-schema#", "dummy"),
        ]
        .into_iter()
        .map(|(k, v)| (k.to_string(), v.to_string()))
        .collect(),
    });

    let (service, _) = LspService::new(|client| Backend::new(client, http_client));

    // Initialize the backend
    let result = service
        .inner()
        .initialize(InitializeParams {
            workspace_folders: None,
            capabilities: ClientCapabilities {
                general: Some(GeneralClientCapabilities {
                    // TODO benchmark UTF-16 later
                    position_encodings: Some(vec![PositionEncodingKind::UTF8]),
                    ..Default::default()
                }),
                ..Default::default()
            },
            ..Default::default()
        })
        .await;
    assert!(result.is_ok());
    service.inner().initialized(InitializedParams {}).await;

    // Create a temp directory and open a document
    let dir = TempDir::new("owl-ms-bench").unwrap();
    let url = Url::from_file_path(dir.path().join("benchmark.omn")).unwrap();

    service
        .inner()
        .did_open(DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri: url.clone(),
                language_id: "owl2md".to_string(),
                version: 0,
                text: BENCHMARK_ONTOLOGY.to_string(),
            },
        })
        .await;

    (service, url, dir)
}

fn bench_hover(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let (service, url, _dir) = rt.block_on(setup_backend());

    c.bench_function("hover", |b| {
        b.iter(|| {
            rt.block_on(async {
                black_box(
                    service
                        .inner()
                        .hover(HoverParams {
                            text_document_position_params: TextDocumentPositionParams {
                                text_document: TextDocumentIdentifier { uri: url.clone() },
                                position: Position::new(8, 12), // On "Person" class
                            },
                            work_done_progress_params: WorkDoneProgressParams {
                                work_done_token: None,
                            },
                        })
                        .await,
                )
            })
        });
    });
}

fn bench_goto_definition(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let (service, url, _dir) = rt.block_on(setup_backend());

    c.bench_function("goto_definition", |b| {
        b.iter(|| {
            rt.block_on(async {
                black_box(
                    service
                        .inner()
                        .goto_definition(GotoDefinitionParams {
                            text_document_position_params: TextDocumentPositionParams {
                                text_document: TextDocumentIdentifier { uri: url.clone() },
                                position: Position::new(18, 20), // On "Person" in SubClassOf
                            },
                            work_done_progress_params: WorkDoneProgressParams {
                                work_done_token: None,
                            },
                            partial_result_params: PartialResultParams {
                                partial_result_token: None,
                            },
                        })
                        .await,
                )
            })
        });
    });
}

fn bench_completion(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let (service, url, _dir) = rt.block_on(setup_backend());

    c.bench_function("completion", |b| {
        b.iter(|| {
            rt.block_on(async {
                black_box(
                    service
                        .inner()
                        .completion(CompletionParams {
                            text_document_position: TextDocumentPositionParams {
                                text_document: TextDocumentIdentifier { uri: url.clone() },
                                position: Position::new(18, 20), // After "SubClassOf: "
                            },
                            work_done_progress_params: WorkDoneProgressParams {
                                work_done_token: None,
                            },
                            partial_result_params: PartialResultParams {
                                partial_result_token: None,
                            },
                            context: None,
                        })
                        .await,
                )
            })
        });
    });
}

fn bench_references(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let (service, url, _dir) = rt.block_on(setup_backend());

    c.bench_function("references", |b| {
        b.iter(|| {
            rt.block_on(async {
                black_box(
                    service
                        .inner()
                        .references(ReferenceParams {
                            text_document_position: TextDocumentPositionParams {
                                text_document: TextDocumentIdentifier { uri: url.clone() },
                                position: Position::new(8, 12), // On "Person" class definition
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
                        .await,
                )
            })
        });
    });
}

fn bench_prepare_rename(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let (service, url, _dir) = rt.block_on(setup_backend());

    c.bench_function("prepare_rename", |b| {
        b.iter(|| {
            rt.block_on(async {
                black_box(
                    service
                        .inner()
                        .prepare_rename(TextDocumentPositionParams {
                            text_document: TextDocumentIdentifier { uri: url.clone() },
                            position: Position::new(8, 12), // On "Person" class
                        })
                        .await,
                )
            })
        });
    });
}

fn bench_rename(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let (service, url, _dir) = rt.block_on(setup_backend());

    c.bench_function("rename", |b| {
        b.iter(|| {
            rt.block_on(async {
                black_box(
                    service
                        .inner()
                        .rename(RenameParams {
                            text_document_position: TextDocumentPositionParams {
                                text_document: TextDocumentIdentifier { uri: url.clone() },
                                position: Position::new(8, 12), // On "Person" class
                            },
                            new_name: "Human".to_string(),
                            work_done_progress_params: WorkDoneProgressParams {
                                work_done_token: None,
                            },
                        })
                        .await,
                )
            })
        });
    });
}

fn bench_document_symbol(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let (service, url, _dir) = rt.block_on(setup_backend());

    c.bench_function("document_symbol", |b| {
        b.iter(|| {
            rt.block_on(async {
                black_box(
                    service
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
                        .await,
                )
            })
        });
    });
}

fn bench_formatting(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let (service, url, _dir) = rt.block_on(setup_backend());

    c.bench_function("formatting", |b| {
        b.iter(|| {
            rt.block_on(async {
                black_box(
                    service
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
                        .await,
                )
            })
        });
    });
}

fn bench_semantic_tokens_full(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let (service, url, _dir) = rt.block_on(setup_backend());

    c.bench_function("semantic_tokens_full", |b| {
        b.iter(|| {
            rt.block_on(async {
                black_box(
                    service
                        .inner()
                        .semantic_tokens_full(SemanticTokensParams {
                            text_document: TextDocumentIdentifier { uri: url.clone() },
                            work_done_progress_params: WorkDoneProgressParams {
                                work_done_token: None,
                            },
                            partial_result_params: PartialResultParams {
                                partial_result_token: None,
                            },
                        })
                        .await,
                )
            })
        });
    });
}

fn bench_semantic_tokens_range(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let (service, url, _dir) = rt.block_on(setup_backend());

    c.bench_function("semantic_tokens_range", |b| {
        b.iter(|| {
            rt.block_on(async {
                black_box(
                    service
                        .inner()
                        .semantic_tokens_range(SemanticTokensRangeParams {
                            text_document: TextDocumentIdentifier { uri: url.clone() },
                            range: Range {
                                start: Position::new(0, 0),
                                end: Position::new(20, 0),
                            },
                            work_done_progress_params: WorkDoneProgressParams {
                                work_done_token: None,
                            },
                            partial_result_params: PartialResultParams {
                                partial_result_token: None,
                            },
                        })
                        .await,
                )
            })
        });
    });
}

fn bench_code_action(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let (service, url, _dir) = rt.block_on(setup_backend());

    c.bench_function("code_action", |b| {
        b.iter(|| {
            rt.block_on(async {
                black_box(
                    service
                        .inner()
                        .code_action(CodeActionParams {
                            text_document: TextDocumentIdentifier { uri: url.clone() },
                            range: Range {
                                start: Position::new(8, 0),
                                end: Position::new(8, 10),
                            },
                            context: tower_lsp::lsp_types::CodeActionContext {
                                diagnostics: vec![],
                                only: None,
                                trigger_kind: None,
                            },
                            work_done_progress_params: WorkDoneProgressParams {
                                work_done_token: None,
                            },
                            partial_result_params: PartialResultParams {
                                partial_result_token: None,
                            },
                        })
                        .await,
                )
            })
        });
    });
}

fn bench_inlay_hint(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let (service, url, _dir) = rt.block_on(setup_backend());

    c.bench_function("inlay_hint", |b| {
        b.iter(|| {
            rt.block_on(async {
                black_box(
                    service
                        .inner()
                        .inlay_hint(InlayHintParams {
                            text_document: TextDocumentIdentifier { uri: url.clone() },
                            range: Range {
                                start: Position::new(0, 0),
                                end: Position::new(50, 0),
                            },
                            work_done_progress_params: WorkDoneProgressParams {
                                work_done_token: None,
                            },
                        })
                        .await,
                )
            })
        });
    });
}

fn bench_symbol(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let (service, _url, _dir) = rt.block_on(setup_backend());

    c.bench_function("symbol", |b| {
        b.iter(|| {
            rt.block_on(async {
                black_box(
                    service
                        .inner()
                        .symbol(WorkspaceSymbolParams {
                            query: "Person".to_string(),
                            work_done_progress_params: WorkDoneProgressParams {
                                work_done_token: None,
                            },
                            partial_result_params: PartialResultParams {
                                partial_result_token: None,
                            },
                        })
                        .await,
                )
            })
        });
    });
}

/// Ontology with a partial keyword for keyword completion benchmark
const KEYWORD_COMPLETION_ONTOLOGY: &str = r"
Ontology: <http://example.org/benchmark>

    Cl
";

fn bench_completion_keywords(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();

    // Set up backend with keyword completion ontology
    let (service, url, _dir) = rt.block_on(async {
        let http_client = Box::new(StaticClient {
            data: [
                ("http://www.w3.org/1999/02/22-rdf-syntax-ns#", "dummy"),
                ("http://www.w3.org/2002/07/owl#", "dummy"),
                ("http://www.w3.org/2000/01/rdf-schema#", "dummy"),
            ]
            .into_iter()
            .map(|(k, v)| (k.to_string(), v.to_string()))
            .collect(),
        });

        let (service, _) = LspService::new(|client| Backend::new(client, http_client));

        let result = service
            .inner()
            .initialize(InitializeParams {
                workspace_folders: None,
                capabilities: ClientCapabilities {
                    general: Some(GeneralClientCapabilities {
                        position_encodings: Some(vec![PositionEncodingKind::UTF8]),
                        ..Default::default()
                    }),
                    ..Default::default()
                },
                ..Default::default()
            })
            .await;
        assert!(result.is_ok());
        service.inner().initialized(InitializedParams {}).await;

        let dir = TempDir::new("owl-ms-bench").unwrap();
        let url = Url::from_file_path(dir.path().join("keyword.omn")).unwrap();

        service
            .inner()
            .did_open(DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: url.clone(),
                    language_id: "owl2md".to_string(),
                    version: 0,
                    text: KEYWORD_COMPLETION_ONTOLOGY.to_string(),
                },
            })
            .await;

        (service, url, dir)
    });

    c.bench_function("completion_keywords", |b| {
        b.iter(|| {
            rt.block_on(async {
                black_box(
                    service
                        .inner()
                        .completion(CompletionParams {
                            text_document_position: TextDocumentPositionParams {
                                text_document: TextDocumentIdentifier { uri: url.clone() },
                                position: Position::new(3, 6), // After "Cl" - triggers keyword completion
                            },
                            work_done_progress_params: WorkDoneProgressParams {
                                work_done_token: None,
                            },
                            partial_result_params: PartialResultParams {
                                partial_result_token: None,
                            },
                            context: None,
                        })
                        .await,
                )
            })
        });
    });
}

criterion_group!(
    benches,
    bench_hover,
    bench_goto_definition,
    bench_completion,
    bench_completion_keywords,
    bench_references,
    bench_prepare_rename,
    bench_rename,
    bench_document_symbol,
    bench_formatting,
    bench_semantic_tokens_full,
    bench_semantic_tokens_range,
    bench_code_action,
    bench_inlay_hint,
    bench_symbol,
);
criterion_main!(benches);

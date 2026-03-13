use criterion::{criterion_group, criterion_main, BatchSize, BenchmarkId, Criterion, Throughput};
use owl_ms_language_server::{clear_caches, web::HttpClient, Backend};
use std::{collections::HashMap, hint::black_box, time::Duration};
use tempdir::TempDir;
use tokio::runtime::Runtime;
#[allow(clippy::wildcard_imports)]
use tower_lsp::{lsp_types::*, LanguageServer, LspService};

/// A static HTTP client that returns dummy data for tests
#[derive(Debug)]
struct StaticClient {
    data: HashMap<String, String>,
}

impl HttpClient for StaticClient {
    fn get(&self, url: &str) -> owl_ms_language_server::web::Result<String> {
        Ok(self
            .data
            .get(url)
            .unwrap_or(&"dummy".to_string())
            .to_string())
    }
}

/// Simple seeded pseudo-random number generator (LCG)
struct SeededRng {
    state: u64,
}

impl SeededRng {
    fn new(seed: u64) -> Self {
        Self { state: seed }
    }

    fn next(&mut self) -> u64 {
        // LCG parameters from Numerical Recipes
        self.state = self.state.wrapping_mul(6364136223846793005).wrapping_add(1);
        self.state
    }

    fn next_usize(&mut self, max: usize) -> usize {
        (self.next() as usize) % max
    }

    fn choose<'a, T>(&mut self, items: &'a [T]) -> &'a T {
        &items[self.next_usize(items.len())]
    }
}

/// Benchmark sizes: 10 steps from ~100 to ~20000 lines
const BENCHMARK_SIZES: [usize; 10] = [100, 500, 1000, 2000, 4000, 6000, 8000, 12000, 16000, 20000];

/// Fixed seed for reproducible ontology generation
const RNG_SEED: u64 = 42;

/// Returns appropriate sample size for a given ontology size.
/// Larger ontologies need fewer samples because setup (including indexing) takes longer.
fn sample_size_for(ontology_size: usize) -> usize {
    match ontology_size {
        0..=500 => 100,
        501..=2000 => 50,
        2001..=6000 => 20,
        _ => 10,
    }
}

/// Generate an ontology with approximately `target_lines` lines.
/// Uses a fixed seed for reproducibility.
fn generate_ontology(target_lines: usize) -> String {
    let mut rng = SeededRng::new(RNG_SEED);
    let mut lines = Vec::with_capacity(target_lines + 100);

    // Prefixes and ontology header (~5 lines)
    lines.push("Prefix: rdfs: <http://www.w3.org/2000/01/rdf-schema#>".to_string());
    lines.push("Prefix: owl: <http://www.w3.org/2002/07/owl#>".to_string());
    lines.push("Prefix: xsd: <http://www.w3.org/2001/XMLSchema#>".to_string());
    lines.push("Prefix: ex: <http://example.org/>".to_string());
    lines.push(String::new());
    lines.push("Ontology: <http://example.org/generated>".to_string());
    lines.push(String::new());

    // Track created entities for cross-references
    let mut classes: Vec<String> = vec!["owl:Thing".to_string()];
    let mut object_properties: Vec<String> = Vec::new();
    let mut data_properties: Vec<String> = Vec::new();
    let mut annotation_properties: Vec<String> =
        vec!["rdfs:label".to_string(), "rdfs:comment".to_string()];
    let mut individuals: Vec<String> = Vec::new();

    let characteristics = [
        "Functional",
        "InverseFunctional",
        "Transitive",
        "Symmetric",
        "Asymmetric",
        "Reflexive",
        "Irreflexive",
    ];
    let data_types = [
        "xsd:string",
        "xsd:integer",
        "xsd:boolean",
        "xsd:dateTime",
        "xsd:decimal",
    ];

    let mut entity_counter = 0;

    while lines.len() < target_lines {
        let frame_type = rng.next_usize(6);
        entity_counter += 1;

        match frame_type {
            0 => {
                // Class frame (~4-8 lines each)
                let class_name = format!("Class{entity_counter}");
                lines.push(format!("    Class: {class_name}"));
                lines.push("        Annotations:".to_string());
                lines.push(format!("            rdfs:label \"{class_name} Label\"@en"));

                if rng.next_usize(2) == 0 {
                    lines.push(format!(
                        "            rdfs:comment \"Description of {class_name}\"@en"
                    ));
                }

                // SubClassOf
                if !classes.is_empty() && rng.next_usize(3) > 0 {
                    let parent = rng.choose(&classes).clone();
                    lines.push(format!("        SubClassOf: {parent}"));
                }

                // EquivalentTo (occasionally)
                if classes.len() > 2 && rng.next_usize(5) == 0 {
                    let equiv = rng.choose(&classes).clone();
                    if equiv != class_name {
                        lines.push(format!("        EquivalentTo: {equiv}"));
                    }
                }

                // DisjointWith (occasionally)
                if classes.len() > 3 && rng.next_usize(6) == 0 {
                    let disjoint = rng.choose(&classes).clone();
                    if disjoint != class_name && disjoint != "owl:Thing" {
                        lines.push(format!("        DisjointWith: {disjoint}"));
                    }
                }

                lines.push(String::new());
                classes.push(class_name);
            }
            1 => {
                // ObjectProperty frame (~4-7 lines each)
                let prop_name = format!("objectProp{entity_counter}");
                lines.push(format!("    ObjectProperty: {prop_name}"));
                lines.push("        Annotations:".to_string());
                lines.push(format!("            rdfs:label \"{prop_name} label\"@en"));

                // Domain
                if !classes.is_empty() && rng.next_usize(2) == 0 {
                    let domain = rng.choose(&classes).clone();
                    lines.push(format!("        Domain: {domain}"));
                }

                // Range
                if !classes.is_empty() && rng.next_usize(2) == 0 {
                    let range = rng.choose(&classes).clone();
                    lines.push(format!("        Range: {range}"));
                }

                // Characteristics
                if rng.next_usize(3) == 0 {
                    let char = rng.choose(&characteristics);
                    lines.push(format!("        Characteristics: {char}"));
                }

                // SubPropertyOf
                if !object_properties.is_empty() && rng.next_usize(4) == 0 {
                    let parent = rng.choose(&object_properties).clone();
                    lines.push(format!("        SubPropertyOf: {parent}"));
                }

                lines.push(String::new());
                object_properties.push(prop_name);
            }
            2 => {
                // DataProperty frame (~4-6 lines each)
                let prop_name = format!("dataProp{entity_counter}");
                lines.push(format!("    DataProperty: {prop_name}"));
                lines.push("        Annotations:".to_string());
                lines.push(format!("            rdfs:label \"{prop_name} label\"@en"));

                // Domain
                if !classes.is_empty() && rng.next_usize(2) == 0 {
                    let domain = rng.choose(&classes).clone();
                    lines.push(format!("        Domain: {domain}"));
                }

                // Range
                let range = rng.choose(&data_types);
                lines.push(format!("        Range: {range}"));

                // Functional
                if rng.next_usize(3) == 0 {
                    lines.push("        Characteristics: Functional".to_string());
                }

                lines.push(String::new());
                data_properties.push(prop_name);
            }
            3 => {
                // Individual frame (~4-8 lines each)
                let ind_name = format!("individual{entity_counter}");
                lines.push(format!("    Individual: {ind_name}"));
                lines.push("        Annotations:".to_string());
                lines.push(format!("            rdfs:label \"{ind_name} Label\"@en"));

                // Types
                if !classes.is_empty() {
                    let type_class = rng.choose(&classes).clone();
                    if type_class != "owl:Thing" {
                        lines.push(format!("        Types: {type_class}"));
                    }
                }

                // Facts (object property assertions)
                if !object_properties.is_empty()
                    && !individuals.is_empty()
                    && rng.next_usize(2) == 0
                {
                    let prop = rng.choose(&object_properties).clone();
                    let target = rng.choose(&individuals).clone();
                    lines.push(format!("        Facts: {prop} {target}"));
                }

                // Facts (data property assertions)
                if !data_properties.is_empty() && rng.next_usize(2) == 0 {
                    let prop = rng.choose(&data_properties).clone();
                    let value = match rng.next_usize(3) {
                        0 => format!("\"Value{}\"", rng.next_usize(1000)),
                        1 => format!("{}", rng.next_usize(1000)),
                        _ => "true".to_string(),
                    };
                    lines.push(format!("        Facts: {prop} {value}"));
                }

                // SameAs (occasionally)
                if individuals.len() > 2 && rng.next_usize(8) == 0 {
                    let same = rng.choose(&individuals).clone();
                    if same != ind_name {
                        lines.push(format!("        SameAs: {same}"));
                    }
                }

                lines.push(String::new());
                individuals.push(ind_name);
            }
            4 => {
                // AnnotationProperty frame (~2-3 lines each)
                let prop_name = format!("annotProp{entity_counter}");
                lines.push(format!("    AnnotationProperty: {prop_name}"));
                lines.push("        Annotations:".to_string());
                lines.push(format!("            rdfs:label \"{prop_name}\"@en"));
                lines.push(String::new());
                annotation_properties.push(prop_name);
            }
            _ => {
                // Datatype frame (~3-4 lines each)
                let dt_name = format!("CustomDatatype{entity_counter}");
                lines.push(format!("    Datatype: {dt_name}"));
                lines.push("        Annotations:".to_string());
                lines.push(format!("            rdfs:label \"{dt_name}\"@en"));
                lines.push(String::new());
            }
        }
    }

    // Add annotation property declarations at the end
    lines.push("    AnnotationProperty: rdfs:label".to_string());
    lines.push("    AnnotationProperty: rdfs:comment".to_string());

    lines.join("\n")
}

/// Count approximate lines in generated ontology
fn count_lines(ontology: &str) -> usize {
    ontology.lines().count()
}

async fn setup_backend_with_ontology(ontology: String) -> (LspService<Backend>, Url, TempDir) {
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
    let url = Url::from_file_path(dir.path().join("benchmark.omn")).unwrap();

    service
        .inner()
        .did_open(DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri: url.clone(),
                language_id: "owl2md".to_string(),
                version: 0,
                text: ontology,
            },
        })
        .await;

    // Wait for background indexing to complete before benchmarking
    service.inner().wait_for_indexing().await;

    (service, url, dir)
}

fn bench_hover(c: &mut Criterion) {
    let mut group = c.benchmark_group("hover");

    for &size in &BENCHMARK_SIZES {
        group.throughput(Throughput::Elements(size as u64));
        group.sample_size(sample_size_for(size));
        group.warm_up_time(Duration::from_millis(100));
        group.measurement_time(Duration::from_millis(500));

        group.bench_with_input(BenchmarkId::from_parameter(size), &size, |b, &size| {
            b.iter_batched(
                || {
                    let rt = Runtime::new().unwrap();
                    let ontology = generate_ontology(size);
                    let line_count = count_lines(&ontology);
                    let (service, url, dir) = rt.block_on(setup_backend_with_ontology(ontology));
                    // Position in middle of document on a class
                    let target_line = (line_count / 4).max(7) as u32;
                    (rt, service, url, dir, target_line)
                },
                |(rt, service, url, _dir, target_line)| {
                    rt.block_on(async {
                        black_box(
                            service
                                .inner()
                                .hover(HoverParams {
                                    text_document_position_params: TextDocumentPositionParams {
                                        text_document: TextDocumentIdentifier { uri: url.clone() },
                                        position: Position::new(target_line, 12),
                                    },
                                    work_done_progress_params: WorkDoneProgressParams {
                                        work_done_token: None,
                                    },
                                })
                                .await,
                        )
                    })
                },
                BatchSize::PerIteration,
            );
        });
    }
    group.finish();
    clear_caches();
}

fn bench_goto_definition(c: &mut Criterion) {
    let mut group = c.benchmark_group("goto_definition");

    for &size in &BENCHMARK_SIZES {
        group.throughput(Throughput::Elements(size as u64));
        group.sample_size(sample_size_for(size));
        group.warm_up_time(Duration::from_millis(100));
        group.measurement_time(Duration::from_millis(500));

        group.bench_with_input(BenchmarkId::from_parameter(size), &size, |b, &size| {
            b.iter_batched(
                || {
                    let rt = Runtime::new().unwrap();
                    let ontology = generate_ontology(size);
                    let line_count = count_lines(&ontology);
                    let (service, url, dir) = rt.block_on(setup_backend_with_ontology(ontology));
                    let target_line = (line_count / 3).max(10) as u32;
                    (rt, service, url, dir, target_line)
                },
                |(rt, service, url, _dir, target_line)| {
                    rt.block_on(async {
                        black_box(
                            service
                                .inner()
                                .goto_definition(GotoDefinitionParams {
                                    text_document_position_params: TextDocumentPositionParams {
                                        text_document: TextDocumentIdentifier { uri: url.clone() },
                                        position: Position::new(target_line, 20),
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
                },
                BatchSize::PerIteration,
            );
        });
    }
    group.finish();
    clear_caches();
}

fn bench_completion(c: &mut Criterion) {
    let mut group = c.benchmark_group("completion");

    for &size in &BENCHMARK_SIZES {
        group.throughput(Throughput::Elements(size as u64));
        group.sample_size(sample_size_for(size));
        group.warm_up_time(Duration::from_millis(100));
        group.measurement_time(Duration::from_millis(500));

        group.bench_with_input(BenchmarkId::from_parameter(size), &size, |b, &size| {
            b.iter_batched(
                || {
                    let rt = Runtime::new().unwrap();
                    let ontology = generate_ontology(size);
                    let line_count = count_lines(&ontology);
                    let (service, url, dir) = rt.block_on(setup_backend_with_ontology(ontology));
                    let target_line = (line_count / 3).max(10) as u32;
                    (rt, service, url, dir, target_line)
                },
                |(rt, service, url, _dir, target_line)| {
                    rt.block_on(async {
                        black_box(
                            service
                                .inner()
                                .completion(CompletionParams {
                                    text_document_position: TextDocumentPositionParams {
                                        text_document: TextDocumentIdentifier { uri: url.clone() },
                                        position: Position::new(target_line, 20),
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
                },
                BatchSize::PerIteration,
            );
        });
    }
    group.finish();
    clear_caches();
}

fn bench_references(c: &mut Criterion) {
    let mut group = c.benchmark_group("references");

    for &size in &BENCHMARK_SIZES {
        group.throughput(Throughput::Elements(size as u64));
        group.sample_size(sample_size_for(size));
        group.warm_up_time(Duration::from_millis(100));
        group.measurement_time(Duration::from_millis(500));

        group.bench_with_input(BenchmarkId::from_parameter(size), &size, |b, &size| {
            b.iter_batched(
                || {
                    let rt = Runtime::new().unwrap();
                    let ontology = generate_ontology(size);
                    let line_count = count_lines(&ontology);
                    let (service, url, dir) = rt.block_on(setup_backend_with_ontology(ontology));
                    let target_line = (line_count / 4).max(7) as u32;
                    (rt, service, url, dir, target_line)
                },
                |(rt, service, url, _dir, target_line)| {
                    rt.block_on(async {
                        black_box(
                            service
                                .inner()
                                .references(ReferenceParams {
                                    text_document_position: TextDocumentPositionParams {
                                        text_document: TextDocumentIdentifier { uri: url.clone() },
                                        position: Position::new(target_line, 12),
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
                },
                BatchSize::PerIteration,
            );
        });
    }
    group.finish();
    clear_caches();
}

fn bench_prepare_rename(c: &mut Criterion) {
    let mut group = c.benchmark_group("prepare_rename");

    for &size in &BENCHMARK_SIZES {
        group.throughput(Throughput::Elements(size as u64));
        group.sample_size(sample_size_for(size));
        group.warm_up_time(Duration::from_millis(100));
        group.measurement_time(Duration::from_millis(500));

        group.bench_with_input(BenchmarkId::from_parameter(size), &size, |b, &size| {
            b.iter_batched(
                || {
                    let rt = Runtime::new().unwrap();
                    let ontology = generate_ontology(size);
                    let line_count = count_lines(&ontology);
                    let (service, url, dir) = rt.block_on(setup_backend_with_ontology(ontology));
                    let target_line = (line_count / 4).max(7) as u32;
                    (rt, service, url, dir, target_line)
                },
                |(rt, service, url, _dir, target_line)| {
                    rt.block_on(async {
                        black_box(
                            service
                                .inner()
                                .prepare_rename(TextDocumentPositionParams {
                                    text_document: TextDocumentIdentifier { uri: url.clone() },
                                    position: Position::new(target_line, 12),
                                })
                                .await,
                        )
                    })
                },
                BatchSize::PerIteration,
            );
        });
    }
    group.finish();
    clear_caches();
}

fn bench_rename(c: &mut Criterion) {
    let mut group = c.benchmark_group("rename");

    for &size in &BENCHMARK_SIZES {
        group.throughput(Throughput::Elements(size as u64));
        group.sample_size(sample_size_for(size));
        group.warm_up_time(Duration::from_millis(100));
        group.measurement_time(Duration::from_millis(500));

        group.bench_with_input(BenchmarkId::from_parameter(size), &size, |b, &size| {
            b.iter_batched(
                || {
                    let rt = Runtime::new().unwrap();
                    let ontology = generate_ontology(size);
                    let line_count = count_lines(&ontology);
                    let (service, url, dir) = rt.block_on(setup_backend_with_ontology(ontology));
                    let target_line = (line_count / 4).max(7) as u32;
                    (rt, service, url, dir, target_line)
                },
                |(rt, service, url, _dir, target_line)| {
                    rt.block_on(async {
                        black_box(
                            service
                                .inner()
                                .rename(RenameParams {
                                    text_document_position: TextDocumentPositionParams {
                                        text_document: TextDocumentIdentifier { uri: url.clone() },
                                        position: Position::new(target_line, 12),
                                    },
                                    new_name: "RenamedEntity".to_string(),
                                    work_done_progress_params: WorkDoneProgressParams {
                                        work_done_token: None,
                                    },
                                })
                                .await,
                        )
                    })
                },
                BatchSize::PerIteration,
            );
        });
    }
    group.finish();
    clear_caches();
}

fn bench_document_symbol(c: &mut Criterion) {
    let mut group = c.benchmark_group("document_symbol");

    for &size in &BENCHMARK_SIZES {
        group.throughput(Throughput::Elements(size as u64));
        group.sample_size(sample_size_for(size));
        group.warm_up_time(Duration::from_millis(100));
        group.measurement_time(Duration::from_millis(500));

        group.bench_with_input(BenchmarkId::from_parameter(size), &size, |b, &size| {
            b.iter_batched(
                || {
                    let rt = Runtime::new().unwrap();
                    let ontology = generate_ontology(size);
                    let (service, url, dir) = rt.block_on(setup_backend_with_ontology(ontology));
                    (rt, service, url, dir)
                },
                |(rt, service, url, _dir)| {
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
                },
                BatchSize::PerIteration,
            );
        });
    }
    group.finish();
    clear_caches();
}

fn bench_formatting(c: &mut Criterion) {
    let mut group = c.benchmark_group("formatting");

    for &size in &BENCHMARK_SIZES {
        group.throughput(Throughput::Elements(size as u64));
        group.sample_size(sample_size_for(size));
        group.warm_up_time(Duration::from_millis(100));
        group.measurement_time(Duration::from_millis(500));

        group.bench_with_input(BenchmarkId::from_parameter(size), &size, |b, &size| {
            b.iter_batched(
                || {
                    let rt = Runtime::new().unwrap();
                    let ontology = generate_ontology(size);
                    let (service, url, dir) = rt.block_on(setup_backend_with_ontology(ontology));
                    (rt, service, url, dir)
                },
                |(rt, service, url, _dir)| {
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
                },
                BatchSize::PerIteration,
            );
        });
    }
    group.finish();
    clear_caches();
}

fn bench_semantic_tokens_full(c: &mut Criterion) {
    let mut group = c.benchmark_group("semantic_tokens_full");

    for &size in &BENCHMARK_SIZES {
        group.throughput(Throughput::Elements(size as u64));
        group.sample_size(sample_size_for(size));
        group.warm_up_time(Duration::from_millis(100));
        group.measurement_time(Duration::from_millis(500));

        group.bench_with_input(BenchmarkId::from_parameter(size), &size, |b, &size| {
            b.iter_batched(
                || {
                    let rt = Runtime::new().unwrap();
                    let ontology = generate_ontology(size);
                    let (service, url, dir) = rt.block_on(setup_backend_with_ontology(ontology));
                    (rt, service, url, dir)
                },
                |(rt, service, url, _dir)| {
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
                },
                BatchSize::PerIteration,
            );
        });
    }
    group.finish();
    clear_caches();
}

fn bench_semantic_tokens_range(c: &mut Criterion) {
    let mut group = c.benchmark_group("semantic_tokens_range");

    for &size in &BENCHMARK_SIZES {
        group.throughput(Throughput::Elements(size as u64));
        group.sample_size(sample_size_for(size));
        group.warm_up_time(Duration::from_millis(100));
        group.measurement_time(Duration::from_millis(500));

        group.bench_with_input(BenchmarkId::from_parameter(size), &size, |b, &size| {
            b.iter_batched(
                || {
                    let rt = Runtime::new().unwrap();
                    let ontology = generate_ontology(size);
                    let line_count = count_lines(&ontology);
                    let (service, url, dir) = rt.block_on(setup_backend_with_ontology(ontology));
                    // Request tokens for first quarter of document
                    let end_line = (line_count / 4).max(20) as u32;
                    (rt, service, url, dir, end_line)
                },
                |(rt, service, url, _dir, end_line)| {
                    rt.block_on(async {
                        black_box(
                            service
                                .inner()
                                .semantic_tokens_range(SemanticTokensRangeParams {
                                    text_document: TextDocumentIdentifier { uri: url.clone() },
                                    range: Range {
                                        start: Position::new(0, 0),
                                        end: Position::new(end_line, 0),
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
                },
                BatchSize::PerIteration,
            );
        });
    }
    group.finish();
    clear_caches();
}

fn bench_code_action(c: &mut Criterion) {
    let mut group = c.benchmark_group("code_action");

    for &size in &BENCHMARK_SIZES {
        group.throughput(Throughput::Elements(size as u64));
        group.sample_size(sample_size_for(size));
        group.warm_up_time(Duration::from_millis(100));
        group.measurement_time(Duration::from_millis(500));

        group.bench_with_input(BenchmarkId::from_parameter(size), &size, |b, &size| {
            b.iter_batched(
                || {
                    let rt = Runtime::new().unwrap();
                    let ontology = generate_ontology(size);
                    let line_count = count_lines(&ontology);
                    let (service, url, dir) = rt.block_on(setup_backend_with_ontology(ontology));
                    let target_line = (line_count / 4).max(7) as u32;
                    (rt, service, url, dir, target_line)
                },
                |(rt, service, url, _dir, target_line)| {
                    rt.block_on(async {
                        black_box(
                            service
                                .inner()
                                .code_action(CodeActionParams {
                                    text_document: TextDocumentIdentifier { uri: url.clone() },
                                    range: Range {
                                        start: Position::new(target_line, 0),
                                        end: Position::new(target_line, 10),
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
                },
                BatchSize::PerIteration,
            );
        });
    }
    group.finish();
    clear_caches();
}

fn bench_inlay_hint(c: &mut Criterion) {
    let mut group = c.benchmark_group("inlay_hint");

    for &size in &BENCHMARK_SIZES {
        group.throughput(Throughput::Elements(size as u64));
        group.sample_size(sample_size_for(size));
        group.warm_up_time(Duration::from_millis(100));
        group.measurement_time(Duration::from_millis(500));

        group.bench_with_input(BenchmarkId::from_parameter(size), &size, |b, &size| {
            b.iter_batched(
                || {
                    let rt = Runtime::new().unwrap();
                    let ontology = generate_ontology(size);
                    let line_count = count_lines(&ontology);
                    let (service, url, dir) = rt.block_on(setup_backend_with_ontology(ontology));
                    (rt, service, url, dir, line_count as u32)
                },
                |(rt, service, url, _dir, line_count)| {
                    rt.block_on(async {
                        black_box(
                            service
                                .inner()
                                .inlay_hint(InlayHintParams {
                                    text_document: TextDocumentIdentifier { uri: url.clone() },
                                    range: Range {
                                        start: Position::new(0, 0),
                                        end: Position::new(line_count, 0),
                                    },
                                    work_done_progress_params: WorkDoneProgressParams {
                                        work_done_token: None,
                                    },
                                })
                                .await,
                        )
                    })
                },
                BatchSize::PerIteration,
            );
        });
    }
    group.finish();
    clear_caches();
}

fn bench_symbol(c: &mut Criterion) {
    let mut group = c.benchmark_group("symbol");

    for &size in &BENCHMARK_SIZES {
        group.throughput(Throughput::Elements(size as u64));
        group.sample_size(sample_size_for(size));
        group.warm_up_time(Duration::from_millis(100));
        group.measurement_time(Duration::from_millis(500));

        group.bench_with_input(BenchmarkId::from_parameter(size), &size, |b, &size| {
            b.iter_batched(
                || {
                    let rt = Runtime::new().unwrap();
                    let ontology = generate_ontology(size);
                    let (service, url, dir) = rt.block_on(setup_backend_with_ontology(ontology));
                    (rt, service, url, dir)
                },
                |(rt, service, _url, _dir)| {
                    rt.block_on(async {
                        black_box(
                            service
                                .inner()
                                .symbol(WorkspaceSymbolParams {
                                    query: "Class".to_string(),
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
                },
                BatchSize::PerIteration,
            );
        });
    }
    group.finish();
    clear_caches();
}

/// Ontology with a partial keyword for keyword completion benchmark
fn generate_keyword_completion_ontology(target_lines: usize) -> String {
    let base = generate_ontology(target_lines);
    // Append partial keyword at end
    format!("{base}\n    Cl")
}

fn bench_completion_keywords(c: &mut Criterion) {
    let mut group = c.benchmark_group("completion_keywords");

    for &size in &BENCHMARK_SIZES {
        group.throughput(Throughput::Elements(size as u64));
        group.sample_size(sample_size_for(size));
        group.warm_up_time(Duration::from_millis(100));
        group.measurement_time(Duration::from_millis(500));

        group.bench_with_input(BenchmarkId::from_parameter(size), &size, |b, &size| {
            b.iter_batched(
                || {
                    let rt = Runtime::new().unwrap();
                    let ontology = generate_keyword_completion_ontology(size);
                    let line_count = count_lines(&ontology);
                    let (service, url, dir) = rt.block_on(setup_backend_with_ontology(ontology));
                    // Position on the "Cl" partial keyword at the end
                    let target_line = (line_count - 1) as u32;
                    (rt, service, url, dir, target_line)
                },
                |(rt, service, url, _dir, target_line)| {
                    rt.block_on(async {
                        black_box(
                            service
                                .inner()
                                .completion(CompletionParams {
                                    text_document_position: TextDocumentPositionParams {
                                        text_document: TextDocumentIdentifier { uri: url.clone() },
                                        position: Position::new(target_line, 6),
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
                },
                BatchSize::PerIteration,
            );
        });
    }
    group.finish();
    clear_caches();
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

use criterion::{criterion_group, criterion_main, BatchSize, BenchmarkId, Criterion, Throughput};
use owl_ms_language_server::{clear_caches, web::HttpClient, Backend};
#[cfg(unix)]
use pprof::criterion::{Output, PProfProfiler};
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
        self.state = self
            .state
            .wrapping_mul(6_364_136_223_846_793_005)
            .wrapping_add(1);
        self.state
    }

    fn next_usize(&mut self, max: usize) -> usize {
        (usize::try_from(self.next()).unwrap_or_default()) % max
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
                generate_class(&mut rng, &mut lines, &mut classes, entity_counter);
            }
            1 => {
                generate_object_property(
                    &mut rng,
                    &mut lines,
                    &classes,
                    &mut object_properties,
                    characteristics,
                    entity_counter,
                );
            }
            2 => {
                generate_data_property(
                    &mut rng,
                    &mut lines,
                    &classes,
                    &mut data_properties,
                    data_types,
                    entity_counter,
                );
            }
            3 => {
                generate_individual(
                    &mut rng,
                    &mut lines,
                    &classes,
                    &object_properties,
                    &data_properties,
                    &mut individuals,
                    entity_counter,
                );
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

fn generate_object_property(
    rng: &mut SeededRng,
    lines: &mut Vec<String>,
    classes: &Vec<String>,
    object_properties: &mut Vec<String>,
    characteristics: [&'static str; 7],
    entity_counter: i32,
) {
    // ObjectProperty frame (~4-7 lines each)
    let prop_name = format!("objectProp{entity_counter}");
    lines.push(format!("    ObjectProperty: {prop_name}"));
    lines.push("        Annotations:".to_string());
    lines.push(format!("            rdfs:label \"{prop_name} label\"@en"));

    // Domain
    if !classes.is_empty() && rng.next_usize(2) == 0 {
        let domain = rng.choose(classes).clone();
        lines.push(format!("        Domain: {domain}"));
    }

    // Range
    if !classes.is_empty() && rng.next_usize(2) == 0 {
        let range = rng.choose(classes).clone();
        lines.push(format!("        Range: {range}"));
    }

    // Characteristics
    if rng.next_usize(3) == 0 {
        let char = rng.choose(&characteristics);
        lines.push(format!("        Characteristics: {char}"));
    }

    // SubPropertyOf
    if !object_properties.is_empty() && rng.next_usize(4) == 0 {
        let parent = rng.choose(&*object_properties).clone();
        lines.push(format!("        SubPropertyOf: {parent}"));
    }

    lines.push(String::new());
    object_properties.push(prop_name);
}

fn generate_data_property(
    rng: &mut SeededRng,
    lines: &mut Vec<String>,
    classes: &Vec<String>,
    data_properties: &mut Vec<String>,
    data_types: [&'static str; 5],
    entity_counter: i32,
) {
    // DataProperty frame (~4-6 lines each)
    let prop_name = format!("dataProp{entity_counter}");
    lines.push(format!("    DataProperty: {prop_name}"));
    lines.push("        Annotations:".to_string());
    lines.push(format!("            rdfs:label \"{prop_name} label\"@en"));

    // Domain
    if !classes.is_empty() && rng.next_usize(2) == 0 {
        let domain = rng.choose(classes).clone();
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

fn generate_individual(
    rng: &mut SeededRng,
    lines: &mut Vec<String>,
    classes: &Vec<String>,
    object_properties: &Vec<String>,
    data_properties: &Vec<String>,
    individuals: &mut Vec<String>,
    entity_counter: i32,
) {
    // Individual frame (~4-8 lines each)
    let ind_name = format!("individual{entity_counter}");
    lines.push(format!("    Individual: {ind_name}"));
    lines.push("        Annotations:".to_string());
    lines.push(format!("            rdfs:label \"{ind_name} Label\"@en"));

    // Types
    if !classes.is_empty() {
        let type_class = rng.choose(classes).clone();
        if type_class != "owl:Thing" {
            lines.push(format!("        Types: {type_class}"));
        }
    }

    // Facts (object property assertions)
    if !object_properties.is_empty() && !individuals.is_empty() && rng.next_usize(2) == 0 {
        let prop = rng.choose(object_properties).clone();
        let target = rng.choose(&*individuals).clone();
        lines.push(format!("        Facts: {prop} {target}"));
    }

    // Facts (data property assertions)
    if !data_properties.is_empty() && rng.next_usize(2) == 0 {
        let prop = rng.choose(data_properties).clone();
        let value = match rng.next_usize(3) {
            0 => format!("\"Value{}\"", rng.next_usize(1000)),
            1 => format!("{}", rng.next_usize(1000)),
            _ => "true".to_string(),
        };
        lines.push(format!("        Facts: {prop} {value}"));
    }

    // SameAs (occasionally)
    if individuals.len() > 2 && rng.next_usize(8) == 0 {
        let same = rng.choose(&*individuals).clone();
        if same != ind_name {
            lines.push(format!("        SameAs: {same}"));
        }
    }

    lines.push(String::new());
    individuals.push(ind_name);
}

fn generate_class(
    rng: &mut SeededRng,
    lines: &mut Vec<String>,
    classes: &mut Vec<String>,
    entity_counter: i32,
) {
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
        let parent = rng.choose(&*classes).clone();
        lines.push(format!("        SubClassOf: {parent}"));
    }

    // EquivalentTo (occasionally)
    if classes.len() > 2 && rng.next_usize(5) == 0 {
        let equiv = rng.choose(&*classes).clone();
        if equiv != class_name {
            lines.push(format!("        EquivalentTo: {equiv}"));
        }
    }

    // DisjointWith (occasionally)
    if classes.len() > 3 && rng.next_usize(6) == 0 {
        let disjoint = rng.choose(&*classes).clone();
        if disjoint != class_name && disjoint != "owl:Thing" {
            lines.push(format!("        DisjointWith: {disjoint}"));
        }
    }

    lines.push(String::new());
    classes.push(class_name);
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

fn setup_with_size(size: usize) -> (Runtime, LspService<Backend>, Url, TempDir, u32) {
    let rt = Runtime::new().unwrap();
    let ontology = generate_ontology(size);
    let line_count = count_lines(&ontology);
    let (service, url, dir) = rt.block_on(setup_backend_with_ontology(ontology));
    let target_line = (line_count / 4).max(7) as u32;
    (rt, service, url, dir, target_line)
}

fn setup_with_onto(ontology: String) -> (Runtime, LspService<Backend>, Url, TempDir, usize) {
    let rt = Runtime::new().unwrap();
    let line_count = count_lines(&ontology);
    let (service, url, dir) = rt.block_on(setup_backend_with_ontology(ontology));
    (rt, service, url, dir, line_count)
}

///////////////// Benchmarks

/// Ontology with a partial keyword for keyword completion benchmark
fn generate_hover_ontology(target_lines: usize) -> String {
    let base = generate_ontology(target_lines);
    // Append partial keyword at end
    format!("{base}\n    Class: X\n    SubClassOf: Class12")
}

fn bench_hover(c: &mut Criterion) {
    let mut group = c.benchmark_group("hover");

    for &size in &BENCHMARK_SIZES {
        group.throughput(Throughput::Elements(size as u64));
        // group.sample_size(sample_size_for(size));
        group.warm_up_time(Duration::from_millis(100));
        group.measurement_time(Duration::from_millis(500));
        let ontology = generate_hover_ontology(size);
        let setup = setup_with_onto(ontology);

        group.bench_with_input(BenchmarkId::from_parameter(size), &size, |b, &size| {
            b.iter(|| {
                let (rt, service, url, dir, line_count) = &setup;
                let result = rt.block_on(async {
                    black_box(
                        service
                            .inner()
                            .hover(HoverParams {
                                text_document_position_params: TextDocumentPositionParams {
                                    text_document: TextDocumentIdentifier { uri: url.clone() },
                                    position: Position::new(*line_count as u32 - 1, 18),
                                },
                                work_done_progress_params: WorkDoneProgressParams {
                                    work_done_token: None,
                                },
                            })
                            .await,
                    )
                });
                // black_box(result.as_ref().unwrap().as_ref().unwrap());
                (result, rt, service, dir)
            });
        });
    }
    group.finish();
    clear_caches();
}

/// Ontology with a partial keyword for keyword completion benchmark
fn generate_goto_definition_ontology(target_lines: usize) -> String {
    let base = generate_ontology(target_lines);
    // Append partial keyword at end
    format!("{base}\n    Class: X\n    SubClassOf: Class12")
}

fn bench_goto_definition(c: &mut Criterion) {
    let mut group = c.benchmark_group("goto_definition");

    for &size in &BENCHMARK_SIZES {
        group.throughput(Throughput::Elements(size as u64));
        // group.sample_size(sample_size_for(size));
        group.warm_up_time(Duration::from_millis(100));
        group.measurement_time(Duration::from_millis(500));
        let ontology = generate_goto_definition_ontology(size);
        let setup = setup_with_onto(ontology);

        group.bench_with_input(BenchmarkId::from_parameter(size), &size, |b, &size| {
            b.iter(|| {
                let (rt, service, url, dir, line_count) = &setup;
                let result = rt.block_on(async {
                    black_box(
                        service
                            .inner()
                            .goto_definition(GotoDefinitionParams {
                                text_document_position_params: TextDocumentPositionParams {
                                    text_document: TextDocumentIdentifier { uri: url.clone() },
                                    position: Position::new(*line_count as u32 - 1, 18),
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
                });
                (result, rt, service, dir)
            });
        });
    }
    group.finish();
    clear_caches();
}

/// Ontology with a partial keyword for keyword completion benchmark
fn generate_completion_ontology(target_lines: usize) -> String {
    let base = generate_ontology(target_lines);
    // Append partial keyword at end
    format!("{base}\n    Class: X\n    SubClassOf: ")
}

fn bench_completion(c: &mut Criterion) {
    let mut group = c.benchmark_group("completion");

    for &size in &BENCHMARK_SIZES {
        group.throughput(Throughput::Elements(size as u64));
        // group.sample_size(sample_size_for(size));
        group.warm_up_time(Duration::from_millis(100));
        group.measurement_time(Duration::from_millis(500));

        let ontology = generate_completion_ontology(size);
        let setup = &setup_with_onto(ontology);

        group.bench_with_input(BenchmarkId::from_parameter(size), &size, |b, &size| {
            b.iter(|| {
                let (rt, service, url, dir, line_count) = setup;
                let result = rt.block_on(async {
                    black_box(
                        service
                            .inner()
                            .completion(CompletionParams {
                                text_document_position: TextDocumentPositionParams {
                                    text_document: TextDocumentIdentifier { uri: url.clone() },
                                    position: Position::new(*line_count as u32 - 1, 16),
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
                });
                (result, rt, service, dir)
            });
        });
    }
    group.finish();
    clear_caches();
}

/// Ontology with a partial keyword for keyword completion benchmark
fn generate_keyword_completion_ontology(target_lines: usize) -> String {
    let base = generate_ontology(target_lines);
    // Append partial keyword at end
    format!("{base}\n    An")
}

fn bench_completion_keywords(c: &mut Criterion) {
    let mut group = c.benchmark_group("completion_keywords");

    for &size in &BENCHMARK_SIZES {
        group.throughput(Throughput::Elements(size as u64));
        // group.sample_size(sample_size_for(size));
        group.warm_up_time(Duration::from_millis(100));
        let onto = generate_keyword_completion_ontology(size);
        let setup = &setup_with_onto(onto);
        group.measurement_time(Duration::from_millis(500));

        group.bench_with_input(BenchmarkId::from_parameter(size), &size, |b, &size| {
            b.iter(|| {
                let (rt, service, url, dir, line_count) = &setup;
                let result = rt.block_on(async {
                    black_box(
                        service
                            .inner()
                            .completion(CompletionParams {
                                text_document_position: TextDocumentPositionParams {
                                    text_document: TextDocumentIdentifier { uri: url.clone() },
                                    position: Position::new(*line_count as u32 - 1, 6),
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
                });
                (result, rt, service, dir)
            });
        });
    }
    group.finish();
    clear_caches();
}

/// Ontology with a partial keyword for keyword completion benchmark
fn generate_reference_ontology(target_lines: usize) -> String {
    let base = generate_ontology(target_lines);
    // Append partial keyword at end
    format!("{base}\n    Class: X\n    SubClassOf: Class12")
}

fn bench_references(c: &mut Criterion) {
    let mut group = c.benchmark_group("references");

    for &size in &BENCHMARK_SIZES {
        group.throughput(Throughput::Elements(size as u64));
        // group.sample_size(sample_size_for(size));
        group.warm_up_time(Duration::from_millis(100));
        group.measurement_time(Duration::from_millis(500));
        let ontology = generate_reference_ontology(size);
        let setup = setup_with_onto(ontology);

        group.bench_with_input(BenchmarkId::from_parameter(size), &size, |b, &size| {
            b.iter(|| {
                let (rt, service, url, dir, line_count) = &setup;
                let result = rt.block_on(async {
                    black_box(
                        service
                            .inner()
                            .references(ReferenceParams {
                                text_document_position: TextDocumentPositionParams {
                                    text_document: TextDocumentIdentifier { uri: url.clone() },
                                    position: Position::new(*line_count as u32 - 1, 18),
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
                });
                (result, rt, service, dir)
            });
        });
    }
    group.finish();
    clear_caches();
}

fn bench_prepare_rename(c: &mut Criterion) {
    let mut group = c.benchmark_group("prepare_rename");

    for &size in &BENCHMARK_SIZES {
        group.throughput(Throughput::Elements(size as u64));
        // group.sample_size(sample_size_for(size));
        group.warm_up_time(Duration::from_millis(100));
        group.measurement_time(Duration::from_millis(500));
        let ontology = generate_rename_ontology(size);
        let setup = setup_with_onto(ontology);

        group.bench_with_input(BenchmarkId::from_parameter(size), &size, |b, &size| {
            b.iter(|| {
                let (rt, service, url, dir, line_count) = &setup;
                let result = rt.block_on(async {
                    black_box(
                        service
                            .inner()
                            .prepare_rename(TextDocumentPositionParams {
                                text_document: TextDocumentIdentifier { uri: url.clone() },
                                position: Position::new(*line_count as u32 - 1, 18),
                            })
                            .await,
                    )
                });
                (result, rt, service, dir)
            });
        });
    }
    group.finish();
    clear_caches();
}

/// Ontology with a partial keyword for keyword completion benchmark
fn generate_rename_ontology(target_lines: usize) -> String {
    let base = generate_ontology(target_lines);
    // Append partial keyword at end
    format!("{base}\n    Class: X\n    SubClassOf: Class12")
}

fn bench_rename(c: &mut Criterion) {
    let mut group = c.benchmark_group("rename");

    for &size in &BENCHMARK_SIZES {
        group.throughput(Throughput::Elements(size as u64));
        group.sample_size(10); // TODO remove
                               // group.sample_size(sample_size_for(size));
        group.warm_up_time(Duration::from_millis(100));
        group.measurement_time(Duration::from_millis(500));
        let ontology = generate_rename_ontology(size);
        let setup = setup_with_onto(ontology);

        group.bench_with_input(BenchmarkId::from_parameter(size), &size, |b, &size| {
            b.iter(|| {
                let (rt, service, url, dir, line_count) = &setup;
                let result = rt.block_on(async {
                    black_box(
                        service
                            .inner()
                            .rename(RenameParams {
                                text_document_position: TextDocumentPositionParams {
                                    text_document: TextDocumentIdentifier { uri: url.clone() },
                                    position: Position::new(*line_count as u32 - 1, 18),
                                },
                                new_name: "RenamedEntity".to_string(),
                                work_done_progress_params: WorkDoneProgressParams {
                                    work_done_token: None,
                                },
                            })
                            .await,
                    )
                });

                let edit = result.as_ref().unwrap().as_ref().unwrap();
                let changes = &edit.changes;
                let x = changes.as_ref().unwrap();
                assert!(!x.is_empty(), "Result was empty!");

                (result, rt, service, dir)
            });
        });
    }
    group.finish();
    clear_caches();
}

fn bench_document_symbol(c: &mut Criterion) {
    let mut group = c.benchmark_group("document_symbol");

    for &size in &BENCHMARK_SIZES {
        group.throughput(Throughput::Elements(size as u64));
        // group.sample_size(sample_size_for(size));
        group.warm_up_time(Duration::from_millis(100));
        group.measurement_time(Duration::from_millis(500));
        let setup = setup_with_size(size);

        group.bench_with_input(BenchmarkId::from_parameter(size), &size, |b, &size| {
            b.iter(|| {
                let (rt, service, url, dir, _) = &setup;
                let result = rt.block_on(async {
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
                });
                (result, rt, service, dir)
            });
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
                |(rt, service, url, dir)| {
                    let result = rt.block_on(async {
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
                    });
                    (result, rt, service, dir)
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
        group.warm_up_time(Duration::from_millis(100));
        group.measurement_time(Duration::from_millis(500));
        let setup = setup_with_size(size);

        group.bench_with_input(BenchmarkId::from_parameter(size), &size, |b, _size| {
            b.iter(|| {
                let (rt, service, url, dir, _) = &setup;
                let result = rt.block_on(async {
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
                });
                (result, rt, service, dir)
            });
        });
    }
    group.finish();
    clear_caches();
}

fn bench_semantic_tokens_range(c: &mut Criterion) {
    let mut group = c.benchmark_group("semantic_tokens_range");

    for &size in &BENCHMARK_SIZES {
        group.throughput(Throughput::Elements(size as u64));
        // group.sample_size(sample_size_for(size));
        group.warm_up_time(Duration::from_millis(100));
        group.measurement_time(Duration::from_millis(500));
        let setup = setup_with_size(size);

        group.bench_with_input(BenchmarkId::from_parameter(size), &size, |b, &size| {
            b.iter(|| {
                let (rt, service, url, dir, _) = &setup;
                let result = rt.block_on(async {
                    black_box(
                        service
                            .inner()
                            .semantic_tokens_range(SemanticTokensRangeParams {
                                text_document: TextDocumentIdentifier { uri: url.clone() },
                                range: Range {
                                    start: Position::new(50, 0),
                                    end: Position::new(90, 0),
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
                });
                (result, rt, service, dir)
            });
        });
    }
    group.finish();
    clear_caches();
}

fn bench_code_action(c: &mut Criterion) {
    let mut group = c.benchmark_group("code_action");

    for &size in &BENCHMARK_SIZES {
        group.throughput(Throughput::Elements(size as u64));
        // group.sample_size(sample_size_for(size));
        // group.warm_up_time(Duration::from_millis(100));
        // group.measurement_time(Duration::from_millis(500));
        let setup = setup_with_size(size);

        group.bench_with_input(BenchmarkId::from_parameter(size), &size, |b, &size| {
            b.iter(|| {
                let (rt, service, url, dir, line_count) = &setup;

                let result = rt.block_on(async {
                    black_box(
                        service
                            .inner()
                            .code_action(CodeActionParams {
                                text_document: TextDocumentIdentifier { uri: url.clone() },
                                range: Range {
                                    start: Position::new(*line_count, 0),
                                    end: Position::new(*line_count, 10),
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
                });
            });
        });
        drop(setup);
    }
    group.finish();
    clear_caches();
}

fn bench_inlay_hint(c: &mut Criterion) {
    let mut group = c.benchmark_group("inlay_hint");

    for &size in &BENCHMARK_SIZES {
        group.throughput(Throughput::Elements(size as u64));
        group.warm_up_time(Duration::from_millis(100));
        // group.measurement_time(Duration::from_millis(500));
        let setup = setup_with_size(size);

        group.bench_with_input(BenchmarkId::from_parameter(size), &size, |b, &size| {
            b.iter(|| {
                let (rt, service, url, dir, line_count) = &setup;
                let result = rt.block_on(async {
                    black_box(
                        service
                            .inner()
                            .inlay_hint(InlayHintParams {
                                text_document: TextDocumentIdentifier { uri: url.clone() },
                                range: Range {
                                    start: Position::new(50, 0),
                                    end: Position::new(90, 0),
                                },
                                work_done_progress_params: WorkDoneProgressParams {
                                    work_done_token: None,
                                },
                            })
                            .await,
                    )
                });
                (result, rt, service, dir)
            });
        });
    }
    group.finish();
    clear_caches();
}

fn bench_symbol(c: &mut Criterion) {
    let mut group = c.benchmark_group("symbol");

    for &size in &BENCHMARK_SIZES {
        group.throughput(Throughput::Elements(size as u64));
        group.warm_up_time(Duration::from_millis(100));
        group.measurement_time(Duration::from_millis(500));
        let setup = setup_with_size(size);

        group.bench_with_input(BenchmarkId::from_parameter(size), &size, |b, &size| {
            b.iter(|| {
                let (rt, service, url, dir, _) = &setup;
                let result = rt.block_on(async {
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
                });
                (result, rt, service, url, dir)
            });
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
    bench_inlay_hint,
    bench_symbol,
);

// Profiling benchmarks group
#[cfg(unix)]
criterion_group!(
    name = profiling;
    config = Criterion::default().with_profiler(PProfProfiler::new(100, Output::Flamegraph(None)));
    targets = bench_code_action
);

criterion_main!(benches, profiling);

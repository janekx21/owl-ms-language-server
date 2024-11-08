// use crate::Backend;
use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use dashmap::DashMap;
use std::{sync::Mutex, time::Duration};
use tower_lsp::{
    lsp_types::{
        DidOpenTextDocumentParams, InitializeParams, PositionEncodingKind, TextDocumentItem, Url,
    },
    LspService,
};
use tree_sitter::Parser;
use tree_sitter_owl_ms::language;

fn parse_helper(source_code: &String, parser: &mut Parser) {
    parser.reset();
    parser.parse(source_code, None).unwrap();
}

fn parse_bench(c: &mut Criterion) {
    c.bench_function("parse_bench", |b| {
        b.iter_batched_ref(
            || {
                let source_code = "
Ontology: <http://foo.bar>
    Class: <http://foo.bar/0>
        Annotations:
            rdfs:label \"Fizz\"
";

                let mut parser = Parser::new();
                parser.set_language(language()).unwrap();
                (source_code.to_string(), parser)
            },
            |(source, parser)| parse_helper(source, parser),
            criterion::BatchSize::SmallInput,
        )
    });
}

fn ontology_size_bench(c: &mut Criterion) {
    let mut group = c.benchmark_group("ontology_size_bench");
    for size in (1..20).map(|i| i * 100) {
        group.throughput(Throughput::Elements(size as u64));
        group.bench_with_input(BenchmarkId::from_parameter(size), &size, |b, &size| {
            b.iter_batched_ref(
                || {
                    let mut source_code = "Ontology: <http://foo.bar>".to_string();
                    source_code.push_str(
                        "Class: <http://foo.bar/0> Annotations: rdfs:label \"Fizz\" "
                            .repeat(size)
                            .as_str(),
                    );

                    let mut parser = Parser::new();
                    parser.set_language(language()).unwrap();
                    (source_code.to_string(), parser)
                },
                |(source, parser)| parse_helper(source, parser),
                criterion::BatchSize::SmallInput,
            )
        });
    }
    group.finish();
}

// fn change_bench(c: &mut Criterion) {
//     let mut group = c.benchmark_group("from_elem");
//     for size in (1..20).map(|i| i * 100) {
//         group.throughput(Throughput::Elements(size as u64));
//         group.bench_with_input(BenchmarkId::from_parameter(size), &size, |b, &size| {
//             b.iter_batched_ref(
//                 || {
//                     let mut source_code = "Ontology: <http://foo.bar>".to_string();
//                     source_code.push_str(
//                         "Class: <http://foo.bar/0> Annotations: rdfs:label \"Fizz\" "
//                             .repeat(size)
//                             .as_str(),
//                     );

//                     let mut parser = Parser::new();
//                     parser.set_language(language()).unwrap();

//                     let (service, _) = LspService::new(|client| Backend {
//                         client,
//                         parser: Mutex::new(parser),
//                         document_map: DashMap::new(),
//                         position_encoding: PositionEncodingKind::UTF8.into(),
//                     });

//                     let result = service
//                         .inner()
//                         .initialize(InitializeParams {
//                             ..Default::default()
//                         })
//                         .await;
//                     assert!(result.is_ok(), "initialize returned {:#?}", result);

//                     let url = Url::parse("file://foo.omn").expect("valid url");
//                     service
//                         .inner()
//                         .did_open(DidOpenTextDocumentParams {
//                             text_document: TextDocumentItem {
//                                 uri: url.clone(),
//                                 language_id: "owl2md".to_string(),
//                                 version: 0,
//                                 text: "DEF".to_string(),
//                             },
//                         })
//                         .await;
//                 },
//                 |(source, parser)| did_change(source, parser),
//                 criterion::BatchSize::SmallInput,
//             )
//         });
//     }
//     group.finish();
// }

// async fn did_change_helper(service: LspService) {
//     service
//         .inner()
//         .did_change(DidChangeTextDocumentParams {
//             text_document: VersionedTextDocumentIdentifier {
//                 uri: url.clone(),
//                 version: 2,
//             },
//             content_changes: vec![
//                 TextDocumentContentChangeEvent {
//                     range: Some(
//                         Range {
//                             start: (Position {
//                                 line: 0,
//                                 character: 0,
//                             }),
//                             end: Position {
//                                 line: 0,
//                                 character: 0,
//                             },
//                         }
//                         .into(),
//                     ),
//                     range_length: None,
//                     text: "ABC".to_string(),
//                 },
//                 TextDocumentContentChangeEvent {
//                     range: Some(
//                         Range {
//                             start: (Position {
//                                 line: 0,
//                                 character: 6,
//                             }),
//                             end: Position {
//                                 line: 0,
//                                 character: 6,
//                             },
//                         }
//                         .into(),
//                     ),
//                     range_length: None,
//                     text: "GHI".to_string(),
//                 },
//             ],
//         })
//         .await;
// }

criterion_group!(
    name = long_bench;
    config = Criterion::default().measurement_time(Duration::from_secs(30));
    targets = ontology_size_bench
);
criterion_group!(benches, parse_bench);
criterion_main!(benches, long_bench);

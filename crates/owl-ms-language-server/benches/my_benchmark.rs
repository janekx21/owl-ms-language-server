use std::{hint::black_box, time::Duration};

// use crate::Backend;
use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use owl_ms_language_server::{rope_provider::RopeProvider, LANGUAGE};
use ropey::Rope;
use tree_sitter_c2rust::{InputEdit, Parser, Point, Query, QueryCursor, StreamingIterator, Tree};

fn parse_helper(source_code: &String, parser: &mut Parser) {
    parser.reset();
    parser.parse(source_code, None).unwrap();
}

fn re_parse_helper(source_code: &Rope, parser: &mut Parser, old_tree: &Tree) {
    let rope_provider = RopeProvider::new(source_code);
    parser.reset();
    parser
        .parse_with_options(
            &mut |byte_idx, _| rope_provider.chunk_callback(byte_idx),
            Some(old_tree),
            None,
        )
        .unwrap();
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
                parser.set_language(&LANGUAGE).unwrap();
                (source_code.to_string(), parser)
            },
            |(source, parser)| parse_helper(source, parser),
            criterion::BatchSize::SmallInput,
        )
    });
}

fn ontology_size_bench(c: &mut Criterion) {
    let mut group = c.benchmark_group("ontology_size_bench");
    for size in (1..10).map(|i| i * 100) {
        group.throughput(Throughput::Elements(size as u64));
        group.bench_with_input(BenchmarkId::from_parameter(size), &size, |b, &size| {
            b.iter_batched_ref(
                || {
                    let mut source_code = "Ontology: <http://foo.bar>\n".to_string();
                    source_code.push_str(
                        "Class: <http://foo.bar/0> Annotations: rdfs:label \"Fizz\" "
                            .repeat(size)
                            .as_str(),
                    );

                    let mut parser = Parser::new();
                    parser.set_language(&LANGUAGE).unwrap();
                    (source_code.to_string(), parser)
                },
                |(source, parser)| parse_helper(source, parser),
                criterion::BatchSize::SmallInput,
            )
        });
    }
    group.finish();
}

fn ontology_change_bench(c: &mut Criterion) {
    let mut group = c.benchmark_group("ontology_change_bench");
    for size in (1..100).map(|i| i * 100) {
        group.throughput(Throughput::Elements(size as u64));
        group.sample_size(1000);
        group.warm_up_time(Duration::from_millis(100));
        // group.measurement_time(Duration::from_millis(5000));
        group.bench_with_input(BenchmarkId::from_parameter(size), &size, |b, &size| {
            b.iter_batched_ref(
                || {
                    let mut source_code = "Ontology: <http://foo.bar>\n".to_string();
                    source_code.push_str(
                        "Class: <http://foo.bar/0>\nAnnotations: rdfs:label \"Fizz\"\n"
                            .repeat(size)
                            .as_str(),
                    );

                    let mut parser = Parser::new();
                    parser.set_language(&LANGUAGE).unwrap();
                    let mut rope = Rope::from_str(source_code.as_str());
                    let rope_provider = RopeProvider::new(&rope);
                    let mut old_tree = parser
                        .parse_with_options(&mut |i, _| rope_provider.chunk_callback(i), None, None)
                        .unwrap();

                    let edit = InputEdit {
                        start_byte: 0,
                        old_end_byte: 0,
                        new_end_byte: 10,
                        start_position: Point { row: 0, column: 0 },
                        old_end_position: Point { row: 0, column: 0 },
                        new_end_position: Point { row: 10, column: 0 },
                    };
                    old_tree.edit(&edit);

                    rope.insert(0, "0123456789");

                    (rope, parser, old_tree)
                },
                |(source, parser, old_tree)| re_parse_helper(source, parser, old_tree),
                criterion::BatchSize::SmallInput,
            )
        });
    }
    group.finish();
}

fn ontology_query_bench(c: &mut Criterion) {
    let mut group = c.benchmark_group("ontology_query_bench");
    for size in (1..10).map(|i| i * 100) {
        group.throughput(Throughput::Elements(size as u64));
        group.sample_size(10);
        group.warm_up_time(Duration::from_millis(10));
        group.measurement_time(Duration::from_millis(100));

        group.bench_with_input(BenchmarkId::from_parameter(size), &size, |b, &size| {
            b.iter_batched_ref(
                || {
                    let mut source_code = "Ontology: <http://foo.bar>\n".to_string();
                    source_code.push_str(
                        "Class: <http://foo.bar/0>\nAnnotations: rdfs:label \"Fizz\"\n"
                            .repeat(size)
                            .as_str(),
                    );

                    let mut parser = Parser::new();
                    parser.set_language(&LANGUAGE).unwrap();
                    let rope = Rope::from_str(source_code.as_str());
                    let rope_provider = RopeProvider::new(&rope);
                    let tree = parser
                        .parse_with_options(&mut |i, _| rope_provider.chunk_callback(i), None, None)
                        .unwrap();

                    let qc = QueryCursor::new();
                    let query =
                        Query::new(&LANGUAGE, "[(full_iri) (simple_iri) (abbreviated_iri)]@iri")
                            .unwrap();
                    (rope, tree, query, qc)
                },
                |(rope, tree, query, qc)| {
                    let mut matches = qc.matches(query, tree.root_node(), RopeProvider::new(rope));

                    while let Some(item) = matches.next() {
                        black_box(item);
                    }
                },
                criterion::BatchSize::SmallInput,
            )
        });
    }
    group.finish();
}

// criterion_group!(
//     name = long_bench;
//     config = Criterion::default().measurement_time(Duration::from_secs(60));
//     targets = ontology_size_bench
// );
criterion_group!(
    benches,
    parse_bench,
    ontology_change_bench,
    ontology_size_bench,
    ontology_query_bench
);
criterion_main!(benches); //, long_bench

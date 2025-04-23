// use crate::Backend;
use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use tree_sitter::{Parser, Tree};
use tree_sitter_owl_ms::language;

fn parse_helper(source_code: &String, parser: &mut Parser) {
    parser.reset();
    parser.parse(source_code, None).unwrap();
}

fn re_parse_helper(source_code: &String, parser: &mut Parser, old_tree: &Tree) {
    parser.parse(source_code, Some(old_tree)).unwrap();
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

fn ontology_change_bench(c: &mut Criterion) {
    let mut group = c.benchmark_group("ontology_change_bench");
    for size in (1..10).map(|i| i * 1000) {
        group.throughput(Throughput::Elements(size as u64));
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
                    parser.set_language(language()).unwrap();
                    let old_tree = parser.parse(source_code.clone(), None).unwrap();
                    // println!("{}", old_tree.root_node().to_sexp());

                    // let edit = InputEdit {
                    //     start_byte: 29usize,
                    //     old_end_byte: 30usize,
                    //     new_end_byte: 30usize,
                    //     start_position: Point { row: 1, column: 0 },
                    //     old_end_position: Point { row: 1, column: 1 },
                    //     new_end_position: Point { row: 1, column: 1 },
                    // };
                    // old_tree.edit(&edit);
                    (source_code.to_string(), parser, old_tree)
                },
                |(source, parser, old_tree)| re_parse_helper(source, parser, old_tree),
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
    ontology_size_bench
);
criterion_main!(benches); //, long_bench

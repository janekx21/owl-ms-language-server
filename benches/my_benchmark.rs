use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use tree_sitter::{Language, Parser};

extern "C" {
    fn tree_sitter_owl_ms() -> Language;
}

fn bench_parse(source_code: &String, parser: &mut Parser) {
    parser.reset();
    parser.parse(source_code, None).unwrap();
}

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("parse", |b| {
        b.iter_batched_ref(
            || {
                let source_code = "
Ontology: <http://foo.bar>
    Class: <http://foo.bar/0>
        Annotations:
            rdfs:label \"Fizz\"
";

                let language = unsafe { tree_sitter_owl_ms() };
                let mut parser = Parser::new();
                parser.set_language(language).unwrap();
                (source_code.to_string(), parser)
            },
            |(source, parser)| bench_parse(source, parser),
            criterion::BatchSize::SmallInput,
        )
    });
}

fn from_elem(c: &mut Criterion) {
    let mut group = c.benchmark_group("from_elem");
    for size in (1..10).map(|i| i * 100) {
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

                    let language = unsafe { tree_sitter_owl_ms() };
                    let mut parser = Parser::new();
                    parser.set_language(language).unwrap();
                    (source_code.to_string(), parser)
                },
                |(source, parser)| bench_parse(source, parser),
                criterion::BatchSize::SmallInput,
            )
        });
    }
    group.finish();
}

criterion_group!(benches, criterion_benchmark, from_elem);
criterion_main!(benches);

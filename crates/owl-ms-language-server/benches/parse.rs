use std::{
    cell::RefCell,
    hint::black_box,
    rc::Rc,
    time::{Duration, Instant},
};

// use crate::Backend;
use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use itertools::Itertools;
use owl_ms_language_server::{
    apply_change_to_rope_and_tree, rope_provider::RopeProvider, LANGUAGE,
};
use ropey::{str_utils::byte_to_char_idx, Rope};
use tower_lsp::lsp_types::{Position, PositionEncodingKind, Range, TextDocumentContentChangeEvent};
use tree_sitter_c2rust::{
    InputEdit, ParseOptions, Parser, Point, Query, QueryCursor, StreamingIterator, Tree, TreeCursor,
};

mod requests;

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
                parser.set_language(&LANGUAGE).unwrap();
                (source_code.to_string(), parser)
            },
            |(source, parser)| parse_helper(source, parser),
            criterion::BatchSize::SmallInput,
        );
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
            );
        });
    }
    group.finish();
}

fn ontology_change_bench(c: &mut Criterion) {
    let mut group = c.benchmark_group("ontology_change_bench");
    for size in (0..20).map(|i| i * 2000) {
        let model = {
            let ontology = requests::generate_ontology(size + 20);
            let parser = Rc::new(RefCell::new(Parser::new()));
            parser.borrow_mut().set_language(&LANGUAGE).unwrap();
            let mut rope = Rope::from_str(ontology.as_str());
            let rope_provider = RopeProvider::new(&rope);
            let mut old_tree = parser
                .borrow_mut()
                .parse_with_options(&mut |i, _| rope_provider.chunk_callback(i), None, None)
                .unwrap();

            // panic!("{}", rope);
            // let text = "Prefix: : <http://invalid/onto#> ";
            // let start_byte = rope.line_to_byte(line);
            // assert!(start_byte == 0);
            // assert_eq!(rope.char(start_byte - 1), '\n');

            // let edit = InputEdit {
            //     start_byte: start_byte,
            //     old_end_byte: start_byte,
            //     new_end_byte: start_byte + text.len(),
            //     start_position: Point {
            //         row: line,
            //         column: 0,
            //     },
            //     old_end_position: Point {
            //         row: line,
            //         column: 0,
            //     },
            //     new_end_position: Point {
            //         row: line,
            //         column: text.len(),
            //     },
            // };
            // old_tree.edit(&edit);
            // rope.insert(start_byte, text);

            // How the 0 ontology looks like
            // ```owl-ms
            // Prefix: rdfs: <http://www.w3.org/2000/01/rdf-schema#>
            // Prefix: owl: <http://www.w3.org/2000/07/owl#>
            // Prefix: xsd: <http://www.w3.org/2001/XMLSchema#>
            // Prefix: ex: <http://example.org/>
            //
            // Ontology: <http://example.org/generated>
            //
            //     AnnotationProperty: rdfs:label
            //     AnnotationProperty: rdfs:comment
            // ```

            let line = ontology.lines().count() - 2; //           let line = 8; // ontology.lines().count() - 1;
            let text = "        Annotations: rdfs:label \"Foo Bar\"\n";

            apply_change_to_rope_and_tree(
                &PositionEncodingKind::UTF8,
                &mut old_tree,
                &mut rope,
                &TextDocumentContentChangeEvent {
                    range: Some(Range {
                        start: Position {
                            line: line.try_into().unwrap(),
                            character: 0,
                        },
                        end: Position {
                            line: line.try_into().unwrap(),
                            character: 0,
                        },
                    }),
                    text: text.to_string(),
                    range_length: None,
                },
            )
            .unwrap();

            // TODO try this one old_tree.changed_ranges(other)
            // log_changed_nodes(&old_tree);
            // log_all_changes(&old_tree);
            // panic!("{rope}");
            // TODO hier weiter machen. Also wir haben als letztes versucht das auf O(1) zu bekommen

            (rope, old_tree, parser)
        };

        group.throughput(Throughput::Elements(size as u64));
        group.warm_up_time(Duration::from_millis(100));
        group.measurement_time(Duration::from_millis(500));
        // group.measurement_time(Duration::from_millis(5000));
        group.bench_with_input(BenchmarkId::from_parameter(size), &size, |b, &_size| {
            // let m = model.clone();
            // b.iter_custom(move |iters| {
            //     let (rope, old_tree, parser) = m.clone();
            //     let start = Instant::now();
            //     for _i in 0..iters {
            //         black_box({
            //             let rope_provider = RopeProvider::new(&rope);
            //             // parser.reset();
            //             let tree = parser
            //                 .borrow_mut()
            //                 .parse_with_options(
            //                     &mut |byte_idx, _| rope_provider.chunk_callback(byte_idx),
            //                     Some(&old_tree),
            //                     None,
            //                 )
            //                 .unwrap();

            //             // let changes = old_tree.changed_ranges(&tree).collect_vec();
            //             // eprintln!("Changes {changes:#?}");

            //             assert!(
            //                 // false &&
            //                 !tree.root_node().has_error(),
            //                 "tree has error:\n{:#?}\n ------ in rope:\n{}",
            //                 tree.root_node().to_sexp(),
            //                 rope
            //             );
            //             tree
            //         });
            //     }
            //     start.elapsed()
            // });

            b.iter_batched(
                || {
                    // let mut parser = Parser::new();
                    // parser.set_language(&LANGUAGE).unwrap();
                    model.clone()
                },
                |(rope, old_tree, parser)| {
                    black_box({
                        let rope_provider = RopeProvider::new(&rope);
                        // parser.reset();
                        let tree = parser
                            .borrow_mut()
                            .parse_with_options(
                                &mut |byte_idx, _| rope_provider.chunk_callback(byte_idx),
                                Some(&old_tree),
                                None,
                            )
                            .unwrap();

                        // let changes = old_tree.changed_ranges(&tree).collect_vec();
                        // eprintln!("Changes {changes:#?}");

                        assert!(
                            // false &&
                            !tree.root_node().has_error(),
                            "tree has error:\n{:#?}\n ------ in rope:\n{}",
                            tree.root_node().to_sexp(),
                            rope
                        );
                        tree
                    });
                    (rope, parser, old_tree)
                },
                criterion::BatchSize::SmallInput,
            );
        });
    }
    group.finish();
}

fn log_changed_nodes(tree: &Tree) {
    eprintln!("Changed Nodes:\n");
    let mut cursor = tree.walk();
    log_changed_nodes_recursive(&mut cursor, 0);
}

fn log_changed_nodes_recursive(cursor: &mut TreeCursor, depth: usize) {
    let node = cursor.node();
    if node.has_changes() {
        eprintln!(
            "[+] {}{} [{}-{}] changed",
            "  ".repeat(depth),
            node.kind(),
            node.start_byte(),
            node.end_byte(),
        );
    } else {
        eprintln!(
            "[ ] {}{} [{}-{}] unchanged",
            "  ".repeat(depth),
            node.kind(),
            node.start_byte(),
            node.end_byte(),
        );
    }
    if cursor.goto_first_child() {
        loop {
            log_changed_nodes_recursive(cursor, depth + 1);
            if !cursor.goto_next_sibling() {
                break;
            }
        }
        cursor.goto_parent();
    }
}

// fn log_all_changes(tree: &Tree) {
//     eprintln!("All Changes:\n");
//     let mut cursor = tree.walk();
//     if cursor.goto_first_child() {
//         loop {
//             let node = cursor.node();
//             eprintln!(
//                 "frame: {} [{}-{}] changed={}",
//                 node.kind(),
//                 node.start_byte(),
//                 node.end_byte(),
//                 node.has_changes()
//             );
//             if !cursor.goto_next_sibling() {
//                 break;
//             }
//         }
//         // cursor.goto_parent();
//     }
// }

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
            );
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

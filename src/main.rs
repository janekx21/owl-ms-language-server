use tree_sitter::{Language, Parser};

extern "C" {
    fn tree_sitter_owl_ms() -> Language;
}

fn main() {
    println!("Hello, world!");
}

#[test]
fn test_parser() {
    let language = unsafe { tree_sitter_owl_ms() };
    let mut parser = Parser::new();
    parser.set_language(language).unwrap();

    let source_code = "Ontology: Foobar";
    let tree = parser.parse(source_code, None).unwrap();

    assert_eq!(
        tree.root_node().to_sexp(),
        "(source_file (ontology (ontology_iri (simple_iri))))"
    );
}

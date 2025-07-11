use dashmap::DashMap;
use once_cell::sync::Lazy;
use serde::{Deserialize, Serialize};
use tree_sitter::Query;

use crate::LANGUAGE;

pub static NODE_TYPES: Lazy<DashMap<String, StaticNode>> = Lazy::new(|| {
    let node_types: Vec<StaticNode> =
        serde_json::from_str(tree_sitter_owl_ms::NODE_TYPES).expect("valid node types");

    DashMap::<String, StaticNode>::from_iter(
        node_types
            .iter()
            .map(|node| (node.type_.clone(), (*node).clone())),
    )
});

#[derive(Serialize, Deserialize, Clone, Debug)]
#[serde(rename_all = "camelCase")]
pub struct StaticNode {
    #[serde(rename = "type")]
    pub type_: String,
    pub named: bool,
    #[serde(default)]
    pub children: StaticNodeChildren,
}

#[derive(Serialize, Deserialize, Default, Clone, Debug)]
#[serde(rename_all = "camelCase")]
pub struct StaticNodeChildren {
    pub multiple: bool,
    pub required: bool,
    pub types: Vec<StaticNode>,
}

pub struct AllQueries {
    pub import_query: Query,
    pub iri_query: Query,
    pub annotation_query: Query,
    pub frame_query: Query,
    pub ontology_id: Query,
    pub prefix: Query,
}

pub static ALL_QUERIES: Lazy<AllQueries> = Lazy::new(|| AllQueries {
    import_query: Query::new(
        &LANGUAGE,
        "(import [(full_iri) (simple_iri) (abbreviated_iri)]@iri)",
    )
    .unwrap(),
    iri_query: Query::new(&LANGUAGE, "[(full_iri) (simple_iri) (abbreviated_iri)]@iri").unwrap(),
    annotation_query: Query::new(
        &LANGUAGE,
        "
        (_ iri: (_)@frame_iri
            (annotation
                (annotation_property_iri)@iri
                [
                    (string_literal_no_language)
                    (string_literal_with_language)
                    (typed_literal)
                ]@literal))
        ",
    )
    .unwrap(),
    frame_query: Query::new(
        &LANGUAGE,
        "
            [
                (datatype_frame (datatype_iri)@frame_iri)
                (class_frame (class_iri)@frame_iri)
                (object_property_frame (object_property_iri)@frame_iri)
                (data_property_frame (data_property_iri)@frame_iri)
                (annotation_property_frame (annotation_property_iri)@frame_iri)
                (individual_frame (individual_iri)@frame_iri)
            ]@frame
        ",
    )
    .unwrap(),
    ontology_id: Query::new(
        &LANGUAGE,
        "
            (ontology (ontology_iri)@iri)
        ",
    )
    .unwrap(),
    prefix: Query::new(
        &LANGUAGE,
        "
            (prefix_declaration (prefix_name)@name (full_iri)@iri)
        ",
    )
    .unwrap(),
});

#[cfg(test)]
mod tests {
    use crate::workspace::lock_global_parser;

    use super::*;
    use pretty_assertions::assert_eq;
    use test_log::test;
    use tree_sitter::{QueryCursor, StreamingIterator};

    #[test]
    fn query_frame_query() {
        // Arrange
        let text = r#"
            
                Ontology:
                    Class: A
                        Annotations: rdfs:label "This class is in the first file"

                        SubClassOf: class-in-other-file
        "#;
        let mut parser_guard = lock_global_parser();
        let tree = parser_guard.parse(text, None).unwrap();
        let mut query_cursor = QueryCursor::new();

        // Act
        let q = &ALL_QUERIES.frame_query;
        let matches = query_cursor.matches(q, tree.root_node(), text.as_bytes());

        // Assert
        assert_eq!(matches.count(), 1);
    }
}

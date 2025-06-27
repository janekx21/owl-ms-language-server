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
    pub frame_info_query: Query,
    pub ontology_id: Query,
    pub prefix: Query,
}

pub static ALL_QUERIES: Lazy<AllQueries> = Lazy::new(|| AllQueries {
    import_query: Query::new(
        *LANGUAGE,
        "(import [(full_iri) (simple_iri) (abbreviated_iri)]@iri)",
    )
    .unwrap(),
    iri_query: Query::new(*LANGUAGE, "[(full_iri) (simple_iri) (abbreviated_iri)]@iri").unwrap(),
    annotation_query: Query::new(
        *LANGUAGE,
        "
        (_ . (_ . [(full_iri) (simple_iri) (abbreviated_iri)]@frame_iri)
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
    // TODO check frame [(datatype_frame) (class_frame) (object_property_frame) (data_property_frame) (annotation_property_frame) (individual_frame)]
    // the typed literal is for the string type
    frame_info_query: Query::new(
        *LANGUAGE,
        "
        (_ . (_ . [(full_iri) (simple_iri) (abbreviated_iri)]@frame_iri)
            (annotation
                (annotation_property_iri)@iri
                [
                    (string_literal_no_language)
                    (string_literal_with_language)
                    (typed_literal)
                ]@literal))

        (prefix_declaration (prefix_name)@prefix_name (full_iri)@iri)

        ([
            (datatype_frame)
            (class_frame)
            (object_property_frame)
            (data_property_frame)
            (annotation_property_frame)
            (individual_frame)
        ] . (_ [(full_iri) (simple_iri) (abbreviated_iri)]@frame_iri))@frame
        ",
    )
    .unwrap(),
    ontology_id: Query::new(
        *LANGUAGE,
        "
            (ontology (ontology_iri)@iri)
        ",
    )
    .unwrap(),
    prefix: Query::new(
        *LANGUAGE,
        "
            (prefix_declaration (prefix_name)@name (full_iri)@iri)
        ",
    )
    .unwrap(),
});

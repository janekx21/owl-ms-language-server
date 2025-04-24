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
}

pub static ALL_QUERIES: Lazy<AllQueries> = Lazy::new(|| AllQueries {
    import_query: Query::new(
        *LANGUAGE,
        "(import [(full_iri) (simple_iri) (abbreviated_iri)]@iri)",
    )
    .unwrap(),
});

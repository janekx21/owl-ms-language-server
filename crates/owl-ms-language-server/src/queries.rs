use std::{collections::HashMap, sync::LazyLock};

use dashmap::DashMap;
use serde::{Deserialize, Serialize};
use tree_sitter_c2rust::Query;

use crate::LANGUAGE;

pub static NODE_TYPES: LazyLock<DashMap<String, StaticNode>> = LazyLock::new(|| {
    let node_types: Vec<StaticNode> =
        serde_json::from_str(tree_sitter_owl_ms::NODE_TYPES).expect("valid node types");

    node_types
        .iter()
        .map(|node| (node.type_.clone(), (*node).clone()))
        .collect::<DashMap<String, StaticNode>>()
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

pub static GRAMMAR: LazyLock<Grammar> = LazyLock::new(|| {
    serde_json::from_str(tree_sitter_owl_ms::GRAMMAR).expect("valid grammar json")
});

pub static KEYWORDS: LazyLock<Vec<String>> = LazyLock::new(|| {
    GRAMMAR
        .rules
        .iter()
        .filter_map(|item| match item {
            (rule_name, Rule::String { value }) if rule_name.starts_with("keyword_") => {
                Some(value.clone())
            }
            _ => None,
        })
        .collect()
});

pub struct AllQueries {
    pub import_query: Query,
    pub iri_query: Query,
    pub annotation_query: Query,
    pub frame_query: Query,
    pub prefix: Query,
}

// All queries are in one struct for easy testing. Invalid ones are detected by unit tests.
pub static ALL_QUERIES: LazyLock<AllQueries> = LazyLock::new(|| AllQueries {
    import_query: Query::new(
        &LANGUAGE,
        "(import [(full_iri) (simple_iri) (abbreviated_iri)]@iri)",
    )
    .expect("valid query"),
    iri_query: Query::new(&LANGUAGE, "[(full_iri) (simple_iri) (abbreviated_iri)]@iri")
        .expect("valid query"),
    annotation_query: Query::new(
        &LANGUAGE,
        "
        (_ iri: (_)@frame_iri
            (annotations
                (annotation
                    (annotation_property_iri)@iri
                    [
                        (string_literal_no_language)
                        (string_literal_with_language)
                        (typed_literal)
                    ]@literal)))
        ",
    )
    .expect("valid query"),
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
    .expect("valid query"),
    prefix: Query::new(
        &LANGUAGE,
        "
            (prefix_declaration (prefix_name)@name (full_iri)@iri)
        ",
    )
    .expect("valid query"),
});

/// Tree-sitter grammar specification
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Grammar {
    /// JSON Schema reference
    #[serde(rename = "$schema", skip_serializing_if = "Option::is_none")]
    pub schema: Option<String>,

    /// The name of the grammar
    pub name: String,

    /// The name of the parent grammar
    #[serde(skip_serializing_if = "Option::is_none")]
    pub inherits: Option<String>,

    /// Grammar rules
    pub rules: HashMap<String, Rule>,

    /// Extra rules
    #[serde(skip_serializing_if = "Option::is_none")]
    pub extras: Option<Vec<Rule>>,

    /// Precedence definitions
    #[serde(skip_serializing_if = "Option::is_none")]
    pub precedences: Option<Vec<Vec<PrecedenceItem>>>,

    /// Reserved words
    #[serde(skip_serializing_if = "Option::is_none")]
    pub reserved: Option<HashMap<String, Vec<Rule>>>,

    /// External rules
    #[serde(skip_serializing_if = "Option::is_none")]
    pub externals: Option<Vec<Rule>>,

    /// Inline rules
    #[serde(skip_serializing_if = "Option::is_none")]
    pub inline: Option<Vec<String>>,

    /// Conflicts
    #[serde(skip_serializing_if = "Option::is_none")]
    pub conflicts: Option<Vec<Vec<String>>>,

    /// Word rule
    #[serde(skip_serializing_if = "Option::is_none")]
    pub word: Option<String>,

    /// Supertypes
    #[serde(skip_serializing_if = "Option::is_none")]
    pub supertypes: Option<Vec<String>>,
}

/// Precedence item (can be string or symbol rule)
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum PrecedenceItem {
    String(String),
    Symbol(SymbolRule),
}

/// Rule types
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum Rule {
    #[serde(rename = "BLANK")]
    Blank,

    #[serde(rename = "STRING")]
    String { value: String },

    #[serde(rename = "PATTERN")]
    Pattern {
        value: String,
        #[serde(skip_serializing_if = "Option::is_none")]
        flags: Option<String>,
    },

    #[serde(rename = "SYMBOL")]
    Symbol { name: String },

    #[serde(rename = "SEQ")]
    Seq { members: Vec<Rule> },

    #[serde(rename = "CHOICE")]
    Choice { members: Vec<Rule> },

    #[serde(rename = "ALIAS")]
    Alias {
        value: String,
        named: bool,
        content: Box<Rule>,
    },

    #[serde(rename = "REPEAT")]
    Repeat { content: Box<Rule> },

    #[serde(rename = "REPEAT1")]
    Repeat1 { content: Box<Rule> },

    #[serde(rename = "TOKEN")]
    Token { content: Box<Rule> },

    #[serde(rename = "IMMEDIATE_TOKEN")]
    ImmediateToken { content: Box<Rule> },

    #[serde(rename = "FIELD")]
    Field { name: String, content: Box<Rule> },

    #[serde(rename = "PREC")]
    Prec {
        value: PrecValue,
        content: Box<Rule>,
    },

    #[serde(rename = "PREC_LEFT")]
    PrecLeft {
        value: PrecValue,
        content: Box<Rule>,
    },

    #[serde(rename = "PREC_RIGHT")]
    PrecRight {
        value: PrecValue,
        content: Box<Rule>,
    },

    #[serde(rename = "PREC_DYNAMIC")]
    PrecDynamic {
        value: PrecValue,
        content: Box<Rule>,
    },
}

/// Precedence value (can be integer or string)
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum PrecValue {
    Integer(i32),
    String(String),
}

/// Symbol rule type (used in precedence items)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SymbolRule {
    #[serde(rename = "type")]
    pub rule_type: String, // Should be "SYMBOL"
    pub name: String,
}

pub fn treesitter_highlight_capture_into_semantic_token_type_index(str: &str) -> u32 {
    match str {
        "keyword" => 15, // SemanticTokenType::KEYWORD,
        "operator" | "punctuation.delimiter" | "punctuation.bracket" => 21, // SemanticTokenType::OPERATOR,
        "variable.buildin" | "constant.builtin" | "variable" => 8, // SemanticTokenType::VARIABLE,
        "string" => 18,                                            // SemanticTokenType::STRING,
        "number" => 19,                                            // SemanticTokenType::NUMBER,
        "comment" => 17,                                           // SemanticTokenType::COMMENT,
        _ => todo!("highlight capture {} not implemented", str),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::workspace::lock_global_parser;
    use pretty_assertions::assert_eq;
    use test_log::test;
    use tree_sitter_c2rust::{QueryCursor, StreamingIterator};

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
        let tree = parser_guard.parse(text, None).expect("valid query");
        let mut query_cursor = QueryCursor::new();

        // Act
        let q = &ALL_QUERIES.frame_query;
        let matches = query_cursor.matches(q, tree.root_node(), text.as_bytes());

        // Assert
        assert_eq!(matches.count(), 1);
    }

    #[test]
    fn test_basic_grammar_deserialization() {
        let json = r#"
        {
            "name": "test_grammar",
            "rules": {
                "start": {
                    "type": "SYMBOL",
                    "name": "expression"
                },
                "expression": {
                    "type": "STRING",
                    "value": "hello"
                }
            }
        }
        "#;

        let grammar: Grammar = serde_json::from_str(json).expect("valid query");
        assert_eq!(grammar.name, "test_grammar");
        assert_eq!(grammar.rules.len(), 2);
    }

    #[test]
    fn test_complex_rule_deserialization() {
        let json = r#"
        {
            "type": "SEQ",
            "members": [
                {
                    "type": "STRING",
                    "value": "if"
                },
                {
                    "type": "SYMBOL",
                    "name": "condition"
                }
            ]
        }
        "#;

        let rule: Rule = serde_json::from_str(json).expect("valid query");
        match rule {
            Rule::Seq { members } => {
                assert_eq!(members.len(), 2);
            }
            _ => panic!("Expected Seq rule"),
        }
    }

    #[test]
    fn from_str_node_types_should_be_valid() {
        let json = tree_sitter_owl_ms::GRAMMAR;

        let grammar: Grammar = serde_json::from_str(json).expect("valid query");
        assert_eq!(grammar.name, "owl_ms");
    }

    #[test]
    fn keywords_clone_should_return_all_keywords() {
        let kws = KEYWORDS.clone();

        assert!(!kws.is_empty());
    }
}

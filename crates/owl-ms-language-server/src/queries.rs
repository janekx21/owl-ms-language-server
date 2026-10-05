use std::{collections::HashMap, sync::LazyLock};

use serde::{Deserialize, Serialize};
use tree_sitter_c2rust::Query;

use crate::LANGUAGE_OMN;

pub static NODE_TYPES: LazyLock<HashMap<String, StaticNode>> = LazyLock::new(|| {
    let node_types: Vec<StaticNode> =
        serde_json::from_str(tree_sitter_owl_ms::NODE_TYPES).expect("valid node types");

    node_types
        .iter()
        .map(|node| (node.type_.clone(), (*node).clone()))
        .collect::<HashMap<String, StaticNode>>()
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

/// Maps keyword rule names to keyword texts
/// Example: "keyword functional" => "Functional:"
pub static KEYWORDS_MAP: LazyLock<HashMap<String, String>> = LazyLock::new(|| {
    GRAMMAR
        .rules
        .iter()
        .filter_map(|item| match item {
            (rule_name, Rule::String { value }) if rule_name.starts_with("keyword_") => {
                Some((rule_name.clone(), value.clone()))
            }
            _ => None,
        })
        .collect()
});

pub struct AllQueries {
    pub import_query: Query,
    pub iri_query_all: Query,
    pub iri_query_references: Query,
    pub annotation_query: Query,
    pub frame_query: Query,
    pub prefix: Query,
    pub ontology: Query,
}

// All queries are in one struct for easy testing. Invalid ones are detected by unit tests.
pub static ALL_QUERIES: LazyLock<AllQueries> = LazyLock::new(|| AllQueries {
    import_query: Query::new(&LANGUAGE_OMN, "(import (iri)@iri)").expect("valid query"),
    iri_query_all: Query::new(&LANGUAGE_OMN, "(iri)@iri").expect("valid query"),
    // Just IRIs that are references to frames
    iri_query_references: Query::new(
        &LANGUAGE_OMN,
        "
        [
          (datatype_iri (_)@iri)
          (class_iri (_)@iri)
          (annotation_property_iri (_)@iri)
          (data_property_iri (_)@iri)
          (object_property_iri (_)@iri)
          (individual_iri (_)@iri)
        ] 
        ",
    )
    .expect("valid query"),
    annotation_query: Query::new(
        &LANGUAGE_OMN,
        "
        (_ iri: (_)@frame_iri
            (annotations
                (annotation
                    (annotation_property_iri)@iri
                    [
                        (string_literal_no_language
                            value: (_) @literal)
                        (string_literal_with_language
                            value: (_) @literal
                            language: (_) @language)
                        (typed_literal
                            value: (_) @literal
                            datatype: (_) @datatype
                            )

                        (integer_literal) @literal
                        (decimal_literal) @literal
                        (floating_point_literal) @literal
                    ])))@frame
        ",
    )
    .expect("valid query"),
    frame_query: Query::new(
        &LANGUAGE_OMN,
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
        &LANGUAGE_OMN,
        "
            (prefix_declaration (prefix_name)@name (full_iri)@iri)
        ",
    )
    .expect("valid query"),
    ontology: Query::new(
        &LANGUAGE_OMN,
        "
            (ontology iri: (_)@iri version_iri: (_)@version_iri ? )
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

// We use syntax highlighting from helix documentation
// https://docs.helix-editor.com/themes.html#scopes
//
//
// These keys match tree-sitter scopes.
//
// When determining styling for a highlight, the longest matching theme key will be used. For example, if the highlight is function.builtin.static, the key function.builtin will be used instead of function.
//
// We use a similar set of scopes as Sublime Text. See also TextMate scopes.
//
//     attribute - Class attributes, HTML tag attributes
//
//     type - Types
//         builtin - Primitive types provided by the language (int, usize)
//         parameter - Generic type parameters (T)
//         enum
//             variant
//
//     constructor
//
//     constant (TODO: constant.other.placeholder for %v)
//         builtin Special constants provided by the language (true, false, nil etc)
//             boolean
//         character
//             escape
//         numeric (numbers)
//             integer
//             float
//
//     string (TODO: string.quoted.{single, double}, string.raw/.unquoted)?
//         regexp - Regular expressions
//         special
//             path
//             url
//             symbol - Erlang/Elixir atoms, Ruby symbols, Clojure keywords
//
//     comment - Code comments
//         line - Single line comments (//)
//             documentation - Line documentation comments (e.g. /// in Rust)
//         block - Block comments (e.g. (/* */)
//             documentation - Block documentation comments (e.g. /** */ in Rust)
//         unused - Unused variables and patterns, e.g. _ and _foo

//     variable - Variables
//         builtin - Reserved language variables (self, this, super, etc.)
//         parameter - Function parameters
//         other
//             member - Fields of composite data types (e.g. structs, unions)
//                 private - Private fields that use a unique syntax (currently just ECMAScript-based languages)
//
//     label - .class, #id in CSS, etc.
//
//     punctuation
//         delimiter - Commas, colons
//         bracket - Parentheses, angle brackets, etc.
//         special - String interpolation brackets.
//
//     keyword
//         control
//             conditional - if, else
//             repeat - for, while, loop
//             import - import, export
//             return
//             exception
//         operator - or, in
//         directive - Preprocessor directives (#if in C)
//         function - fn, func
//         storage - Keywords describing how things are stored
//             type - The type of something, class, function, var, let, etc.
//             modifier - Storage modifiers like static, mut, const, ref, etc.
//
//     operator - ||, +=, >
//
//     function
//         builtin
//         method
//             private - Private methods that use a unique syntax (currently just ECMAScript-based languages)
//         macro
//         special (preprocessor in C)
//
//     tag - Tags (e.g. <body> in HTML)
//         builtin
//
//     namespace
//
//     special - derive in Rust, etc.
//
//     markup
//         heading
//             marker
//             1, 2, 3, 4, 5, 6 - heading text for h1 through h6
//         list
//             unnumbered
//             numbered
//             checked
//             unchecked
//         bold
//         italic
//         strikethrough
//         link
//             url - URLs pointed to by links
//             label - non-URL link references
//             text - URL and image descriptions in links
//         quote
//         raw
//             inline
//             block
//
//     diff - version control changes
//         plus - additions
//             gutter - gutter indicator
//         minus - deletions
//             gutter - gutter indicator
//         delta - modifications
//             moved - renamed or moved files/changes
//             conflict - merge conflicts
//             gutter - gutter indicator

/// Convert a tree-sitter capture into a semantic token index. These reference the helix highlight names.
pub fn treesitter_highlight_capture_into_semantic_token_type_index(str: &str) -> u32 {
    match str {
        "namespace" => 0, // SemanticTokenType::NAMESPACE,
        "type" => 1,      // SemanticTokenType::TYPE,
        // constants, subjects, local names and blank node labels
        "variable" | "variable.builtin" | "constant" | "constant.builtin" | "label" => 8, // SemanticTokenType::VARIABLE,
        "variable.other.member" => 9, // SemanticTokenType::PROPERTY,
        // `@prefix`, `@base`, `BASE`, `PREFIX`, `GRAPH`, `a`, `true`, `false`
        "keyword" | "keyword.directive" | "constant.builtin.boolean" => 15, // SemanticTokenType::KEYWORD,
        "comment" | "comment.line" => 17, // SemanticTokenType::COMMENT,
        // IRIs, strings and their escape sequences
        "string" | "string.special" | "string.special.url" | "constant.character.escape" => 18, // SemanticTokenType::STRING,
        "number" | "constant.numeric.integer" | "constant.numeric.float" => 19, // SemanticTokenType::NUMBER,
        "operator" | "punctuation.delimiter" | "punctuation.bracket" | "punctuation.special" => 21, // SemanticTokenType::OPERATOR,
        // language tags (@en, @fr-CA)
        "attribute" => 22, // SemanticTokenType::DECORATOR,
        _ => todo!("highlight capture {} not implemented", str),
    }
}

#[cfg(test)]
mod tests {
    use crate::manchester::GLOBAL_OMN_PARSER;

    use super::*;
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

        let tree = GLOBAL_OMN_PARSER
            .with(|parser| parser.borrow_mut().parse(text, None).expect("valid query"));

        let mut query_cursor = QueryCursor::new();

        // Act
        let q = &ALL_QUERIES.frame_query;
        let matches = query_cursor.matches(q, tree.root_node(), text.as_bytes());

        // Assert
        assert_eq!(matches.count(), 1);
    }

    #[test]
    fn query_ontology() {
        // Arrange
        let text = "
            Ontology: OntologyID
        ";
        let tree = GLOBAL_OMN_PARSER
            .with(|parser| parser.borrow_mut().parse(text, None).expect("valid query"));

        let mut query_cursor = QueryCursor::new();

        // Act
        let q = &ALL_QUERIES.ontology;
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
        let kws = KEYWORDS_MAP.clone();

        assert!(!kws.is_empty());
    }
}

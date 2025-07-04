module.exports = grammar({
  name: "owl_ms",
  conflicts: ($) => [
    [$.datatype_frame],
    [$.data_property_iri, $.object_property_iri],
    [$.datatype_iri, $.class_iri],
  ],
  extras: ($) => [/[ \t\n\r]/, $.comment],
  rules: {
    // My rules
    source_file: ($) => $._ontology_document,
    comment: _ => token(seq('#', /.*/)), // https://github.com/tree-sitter/tree-sitter-rust/blob/master/grammar.js

    // https://www.w3.org/TR/owl2-manchester-syntax/

    // 2.1 IRIs, Integers, Literals, and Entities
    _iri: ($) => choice($.full_iri, $.abbreviated_iri, $.simple_iri),
    full_iri: ($) => seq("<", $._iri_rfc3987, ">"),
    abbreviated_iri: ($) => $._pname_ln,
    simple_iri: ($) => $._pn_local,
    prefix_name: ($) => /([A-Za-z][A-Za-z0-9_\-\.]*)?:/,

    _datatype: ($) =>
      choice($.datatype_iri, "integer", "decimal", "float", "string"),

    // Iri Types
    datatype_iri: ($) => $._iri,
    class_iri: ($) => $._iri,
    annotation_property_iri: ($) => $._iri,
    ontology_iri: ($) => $._iri,
    data_property_iri: ($) => $._iri,
    version_iri: ($) => $._iri,
    object_property_iri: ($) => $._iri,
    annotation_property_iri_annotated_list: ($) => $._iri,
    individual_iri: ($) => $._iri,

    _individual: ($) => choice($.individual_iri, $.node_id),
    node_id: ($) => seq("_:", $._pn_local),

    non_negative_integer: ($) => choice($._zero, $._positive_integer),
    _positive_integer: ($) => seq($._non_zero, repeat($._digit)),
    _digits: ($) => repeat1($._digit),
    _digit: ($) => choice($._zero, $._non_zero),
    _non_zero: ($) => /[1-9]/,
    _zero: ($) => "0",

    _literal: ($) =>
      choice(
        $.typed_literal,
        $.string_literal_no_language,
        $.string_literal_with_language,
        $.integer_literal,
        $.decimal_literal,
        $.floating_point_literal,
      ),
    typed_literal: ($) => seq($._lexial_value, "^^", $._datatype),
    string_literal_no_language: ($) => $._quoted_string,
    string_literal_with_language: ($) => seq($._quoted_string, $._language_tag),
    integer_literal: ($) => seq(optional(choice("+", "-")), $._digits),
    decimal_literal: ($) =>
      seq(optional(choice("+", "-")), $._digits, ".", $._digits),
    floating_point_literal: ($) =>
      seq(
        optional(choice("+", "-")),
        choice(
          seq($._digits, optional(seq(".", $._digits)), optional($._exponent)),
          seq(".", $._digits, optional($._exponent)),
        ),
        choice("f", "F"),
      ),

    _exponent: ($) =>
      seq(choice("e", "E"), optional(choice("+", "-")), $._digits),

    _quoted_string: ($) => /"([^"\\]|\\\\|\\")*"/,
    _language_tag: ($) => /@[a-zA-Z\-]+/, // TODO make more strict https://www.rfc-editor.org/rfc/bcp/bcp47.txt
    _lexial_value: ($) => $._quoted_string,

    // 2.2 Ontologies and Annotations
    _ontology_document: ($) => seq(repeat($.prefix_declaration), $.ontology),
    prefix_declaration: ($) => seq("Prefix:", $.prefix_name, $.full_iri),

    ontology: ($) =>
      seq(
        "Ontology:",
        optional(seq($.ontology_iri, optional($.version_iri))),
        repeat($.import),
        repeat($._annotations),
        repeat($._frame),
      ),

    import: ($) => seq("Import:", $._iri),

    _frame: ($) =>
      choice(
        $.datatype_frame,
        $.class_frame,
        $.object_property_frame,
        $.data_property_frame,
        $.annotation_property_frame,
        $.individual_frame,
        $.misc,
      ),

    _annotations: ($) => seq("Annotations:", $._annotation_annotated_list),
    annotation: ($) => seq($.annotation_property_iri, $._annotation_target),
    _annotation_target: ($) => choice($._iri, $._literal, $.node_id),

    // 2.3  Property and Datatype Expressions
    _object_property_expression: ($) =>
      choice($.object_property_iri, $._inverse_object_property),
    _inverse_object_property: ($) => seq("inverse", $.object_property_iri),

    _data_property_expression: ($) => $.data_property_iri,

    data_range: ($) => sep1($._data_conjunction, "or"),
    _data_conjunction: ($) => sep1($._data_primary, "and"),
    _data_primary: ($) => seq(optional("not"), $._data_atomic),
    _data_atomic: ($) =>
      choice(
        $._datatype,
        seq("{", $._literal_list, "}"),
        $.datatype_restriction,
        seq("(", $.data_range, ")"),
      ),
    datatype_restriction: ($) =>
      seq(
        $._datatype,
        "[",
        sep1(seq($._facet, $._restriction_value), ","),
        "]",
      ),

    _facet: ($) =>
      choice(
        "length",
        "minLength",
        "maxLength",
        "pattern",
        "langRange",
        "<=",
        "<",
        ">=",
        ">",
      ),

    _restriction_value: ($) => $._literal,

    // 2.4 Descriptions
    description: ($) => sep1($._conjunction, "or"),

    _conjunction: ($) =>
      choice(
        seq(
          $.class_iri,
          "that",
          optional("not"),
          $._restriction,
          repeat(seq("and", optional("not"), $._restriction)),
        ),
        sep1($._primary, "and"),
      ),

    _primary: ($) => seq(optional("not"), choice($._restriction, $._atomic)),

    _restriction: ($) =>
      choice(
        seq($._data_property_expression, "some", $._primary),
        seq($._data_property_expression, "some", $._data_primary),
        seq($._data_property_expression, "only", $._primary),
        seq($._data_property_expression, "only", $._data_primary),
        seq($._data_property_expression, "Self"),
        seq(
          $._data_property_expression,
          "min",
          $.non_negative_integer,
          optional($._primary),
        ),
        seq(
          $._data_property_expression,
          "min",
          $.non_negative_integer,
          optional($._data_primary),
        ),
        seq(
          $._data_property_expression,
          "max",
          $.non_negative_integer,
          optional($._primary),
        ),
        seq(
          $._data_property_expression,
          "max",
          $.non_negative_integer,
          optional($._data_primary),
        ),
        seq(
          $._data_property_expression,
          "exactly",
          $.non_negative_integer,
          optional($._primary),
        ),
        seq(
          $._data_property_expression,
          "exactly",
          $.non_negative_integer,
          optional($._data_primary),
        ),
        seq($._object_property_expression, "value", $._individual),
        seq($._data_property_expression, "value", $._literal),
      ),

    _atomic: ($) =>
      choice(
        $.class_iri,
        seq("{", $._individual_list, "}"),
        seq("(", $.description, ")"),
      ),

    // 2.5 Frames and Miscellaneous

    datatype_frame: ($) =>
      seq(
        "Datatype:",
        field("iri",$._datatype),
        repeat($._annotations),
        optional($.datatype_equavalent_to),
        repeat($._annotations),
      ),

    datatype_equavalent_to: ($) =>
      seq("EquivalentTo:", optional($._annotations), $.data_range),

    class_frame: ($) =>
      seq(
        "Class:",
        field("iri", $.class_iri),
        repeat(
          choice(
            $._annotations,
            $.sub_class_of,
            $.equivalent_to,
            $.disjoint_with,
            $.disjoint_union_of,
            $.has_key,
          ),
        ),
      ),

    sub_class_of: ($) => seq("SubClassOf:", $._description_annotated_list),
    equivalent_to: ($) => seq("EquivalentTo:", $._description_annotated_list),
    disjoint_with: ($) => seq("DisjointWith:", $._description_annotated_list),
    disjoint_union_of: ($) =>
      seq("DisjointUnionOf:", optional($._annotations), $._description_2list),
    has_key: ($) =>
      seq(
        "HasKey:",
        optional($._annotations),
        repeat1(
          choice($._object_property_expression, $._data_property_expression),
        ),
      ),

    object_property_frame: ($) =>
      seq(
        "ObjectProperty:",
        field("iri", $.object_property_iri),
        repeat(
          choice(
            $._annotations,
            seq("Domain:", $._description_annotated_list),
            seq("Range:", $._description_annotated_list),
            seq("SubPropertyOf:", $._object_property_expression_annotated_list),
            seq("EquivalentTo:", $._object_property_expression_annotated_list),
            seq("DisjointWith:", $._object_property_expression_annotated_list),
            seq("InverseOf:", $._object_property_expression_annotated_list),
            seq(
              "Characteristics:",
              $._object_property_characteristic_annotated_list,
            ),
            seq(
              "SubPropertyChain:",
              optional($._annotations),
              sep1($._object_property_expression, "o"),
            ),
          ),
        ),
      ),

    _object_property_characteristic: ($) =>
      choice(
        "Functional",
        "InverseFunctional",
        "Reflexive",
        "Irreflexive",
        "Symmetric",
        "Asymmetric",
        "Transitive",
      ),

    data_property_frame: ($) =>
      seq(
        "DataProperty:",
        field("iri", $.data_property_iri),
        repeat(
          choice(
            $._annotations,
            seq("Domain:", $._description_annotated_list),
            seq("Range:", $._data_range_annotated_list),
            seq("Characteristics:", optional($._annotations), "Functional"),
            seq("SubPropertyOf:", $._data_property_expression_annotated_list),
            seq("EquivalentTo:", $._data_property_expression_annotated_list),
            seq("DisjointWith:", $._data_property_expression_annotated_list),
          ),
        ),
      ),

    annotation_property_frame: ($) =>
      seq(
        "AnnotationProperty:",
        field("iri",$.annotation_property_iri),
        repeat(
          choice(
            $._annotations,
            seq("Domain:", $._iri_annotated_list),
            seq("Range:", $._iri_annotated_list),
            seq("SubPropertyOf:", $._annotation_property_iri_annotated_list),
          ),
        ),
      ),

    individual_frame: ($) =>
      seq(
        "Individual:",
        field("iri", $._individual),
        repeat(
          choice(
            $._annotations,
            seq("Types:", $._description_annotated_list),
            seq("Facts:", $._fact_annotated_list),
            seq("SameAs:", $._individual_annotated_list),
            seq("DifferentFrom:", $._individual_annotated_list),
          ),
        ),
      ),

    _fact: ($) =>
      seq(
        optional("not"),
        choice($._object_property_fact, $._data_property_fact),
      ),
    _object_property_fact: ($) => seq($.object_property_iri, $._individual),
    _data_property_fact: ($) => seq($.data_property_iri, $._literal),

    misc: ($) =>
      choice(
        seq(
          "EquivalentClasses:",
          optional($._annotations),
          $._description_2list,
        ), // optional annotations is not to spec. spec wrong?
        seq("DisjointClasses:", optional($._annotations), $._description_2list),
        seq(
          "EquivalentProperties:",
          optional($._annotations),
          $._object_property_2list,
        ),
        seq(
          "DisjointProperties:",
          optional($._annotations),
          $._object_property_2list,
        ),
        seq(
          "EquivalentProperties:",
          optional($._annotations),
          $._data_property_2list,
        ),
        seq(
          "DisjointProperties:",
          optional($._annotations),
          $._data_property_2list,
        ),
        seq("SameIndividual:", optional($._annotations), $._individual_2list),
        seq(
          "DifferentIndividuals:",
          optional($._annotations),
          $._individual_2list,
        ),
      ),

    // Annotated Lists
    _description_annotated_list: ($) =>
      annotated_list($._annotations, $.description),
    _annotation_annotated_list: ($) =>
      annotated_list($._annotations, $.annotation),
    _object_property_expression_annotated_list: ($) =>
      annotated_list($._annotations, $._object_property_expression),
    _object_property_characteristic_annotated_list: ($) =>
      annotated_list($._annotations, $._object_property_characteristic),
    _data_range_annotated_list: ($) =>
      annotated_list($._annotations, $.data_range),
    _data_property_expression_annotated_list: ($) =>
      annotated_list($._annotations, $._data_property_expression),
    _iri_annotated_list: ($) => annotated_list($._annotations, $._iri),
    _annotation_property_iri_annotated_list: ($) =>
      annotated_list($._annotations, $.annotation_property_iri),
    _individual_annotated_list: ($) =>
      annotated_list($._annotations, $._individual),
    _fact_annotated_list: ($) => annotated_list($._annotations, $._fact),

    // List2
    _description_2list: ($) =>
      seq($.description, ",", sep1($.description, ",")),
    _object_property_2list: ($) =>
      seq(
        $._object_property_expression,
        ",",
        sep1($._object_property_expression, ","),
      ), // This is not correct to the grammar. Grammar is missing "objectProperty" Is the grammar wrong?
    // resolved using https://github.com/spechub/Hets/blob/master/OWL2/ParseMS.hs
    _data_property_2list: ($) =>
      seq(
        $._data_property_expression,
        ",",
        sep1($._data_property_expression, ","),
      ), // same as with object_property
    _individual_2list: ($) => seq($._individual, ",", sep1($._individual, ",")),

    // List
    _individual_list: ($) => sep1($._individual, ","),
    _literal_list: ($) => sep1($._literal, ","),

    // IRI [RFC 3987]
    // TODO finish this rfc3987 URL rule
    _iri_rfc3987: ($) =>
      token(seq(
        /[A-Za-z][A-Za-z0-9+\-\.]*/,
        ":",
        seq("//",
          seq(
            optional(seq(/[A-Za-z0-9_\-\.\~:%]*/, "@")),
            /[A-Za-z0-9_\-\.\~:%]*/,
            optional(seq(":", /[0-9]*/)),
          ),
          repeat1(seq("/", /[A-Za-z0-9_\-\.\~:%]*/))),
        optional(/\?[A-Za-z0-9_\-\.\~\/\?]*/),
        optional(/\#[A-Za-z0-9_\-\.\~\/\?]*/),
      )),

    // https://www.w3.org/TR/2008/REC-rdf-sparql-query-20080115/
    // TODO make more strict
    _pn_local: ($) => /[A-Za-z0-9_\-\.]+/,
    _pname_ln: ($) => /[A-Za-z0-9_\-\.]*:[A-Za-z0-9_\-\.]+/,
    // _pn_prefix: $ => /[A-Za-z0-9_\-\.]+/,
    // _pname_ln: $ => seq(optional($._pn_prefix), ':', $._pn_local),
  },
});

// Util Functions

function sep1(rule, separator) {
  return seq(rule, repeat(seq(separator, rule)));
}

function annotated_list(annotations, nt) {
  return sep1(seq(optional(annotations), nt), ",");
}

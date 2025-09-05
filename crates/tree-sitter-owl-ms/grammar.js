module.exports = grammar({
  name: 'owl_ms',
  conflicts: $ => [
    [$.datatype_frame],
    [$.data_property_iri, $.object_property_iri],
    [$.datatype_iri, $.class_iri],
  ],
  extras: $ => [/[ \t\n\r]/, $.comment],
  rules: {
    // My rules
    source_file: $ => $._ontology_document,
    comment: _ => token(seq('#', /.*/)), // https://github.com/tree-sitter/tree-sitter-rust/blob/master/grammar.js

    // https://www.w3.org/TR/owl2-manchester-syntax/

    // 2.1 IRIs, Integers, Literals, and Entities
    _iri: $ => choice($.full_iri, $.abbreviated_iri, $.simple_iri),
    full_iri: $ => token(seq('<', iri_rfc3987(), '>')),
    abbreviated_iri: $ => $._pname_ln,
    simple_iri: $ => $._pn_local,
    prefix_name: $ => /([A-Za-z][A-Za-z0-9_\-\.]*)?:/,

    _datatype: $ =>
      choice(
        $.keyword_integer,
        $.keyword_decimal,
        $.keyword_float,
        $.keyword_string,
        $.datatype_iri,
      ),

    // Iri Types
    datatype_iri: $ => $._iri,
    class_iri: $ => $._iri,
    annotation_property_iri: $ => $._iri,
    ontology_iri: $ => $._iri,
    data_property_iri: $ => $._iri,
    version_iri: $ => $._iri,
    object_property_iri: $ => $._iri,
    annotation_property_iri_annotated_list: $ => $._iri,
    individual_iri: $ => $._iri,

    _individual: $ => choice($.individual_iri, $.node_id),
    node_id: $ => seq('_:', $._pn_local),

    non_negative_integer: $ => choice($._zero, $._positive_integer),
    _positive_integer: $ => seq($._non_zero, repeat($._digit)),
    _digits: $ => repeat1($._digit),
    _digit: $ => choice($._zero, $._non_zero),
    _non_zero: $ => /[1-9]/,
    _zero: $ => '0',

    _literal: $ =>
      choice(
        $.typed_literal,
        $.string_literal_no_language,
        $.string_literal_with_language,
        $.integer_literal,
        $.decimal_literal,
        $.floating_point_literal,
      ),
    typed_literal: $ => seq($._lexial_value, '^^', $._datatype),
    string_literal_no_language: $ => $._quoted_string,
    string_literal_with_language: $ => seq($._quoted_string, $._language_tag),
    integer_literal: $ => seq(optional(choice('+', '-')), $._digits),
    decimal_literal: $ =>
      seq(optional(choice('+', '-')), $._digits, '.', $._digits),
    floating_point_literal: $ =>
      seq(
        optional(choice('+', '-')),
        choice(
          seq($._digits, optional(seq('.', $._digits)), optional($._exponent)),
          seq('.', $._digits, optional($._exponent)),
        ),
        choice('f', 'F'),
      ),

    _exponent: $ =>
      seq(choice('e', 'E'), optional(choice('+', '-')), $._digits),

    _quoted_string: $ => /"([^"\\]|\\\\|\\")*"/,
    _language_tag: $ => /@[a-zA-Z\-]+/, // TODO make more strict https://www.rfc-editor.org/rfc/bcp/bcp47.txt
    _lexial_value: $ => $._quoted_string,

    // 2.2 Ontologies and Annotations
    _ontology_document: $ => seq(repeat($.prefix_declaration), $.ontology),
    prefix_declaration: $ => seq($.keyword_prefix, $.prefix_name, $.full_iri),

    ontology: $ =>
      seq(
        $.keyword_ontology,
        optional(
          seq(
            field('iri', $.ontology_iri),
            optional(field('version_iri', $.version_iri)),
          ),
        ),
        field('import', repeat($.import)),
        field('annotations', repeat($.annotations)),
        field('frame', repeat($._frame)),
      ),

    import: $ => seq($.keyword_import, $._iri),

    _frame: $ =>
      choice(
        $.datatype_frame,
        $.class_frame,
        $.object_property_frame,
        $.data_property_frame,
        $.annotation_property_frame,
        $.individual_frame,
        $.misc,
      ),

    annotations: $ => seq($.keyword_annotations, $._annotation_annotated_list),

    annotation: $ => seq($.annotation_property_iri, $._annotation_target),
    _annotation_target: $ => choice($._iri, $._literal, $.node_id),

    // 2.3  Property and Datatype Expressions
    _object_property_expression: $ =>
      choice($.object_property_iri, $._inverse_object_property),
    _inverse_object_property: $ =>
      seq($.keyword_inverse, $.object_property_iri),

    _data_property_expression: $ => $.data_property_iri,

    data_range: $ => sep1($._data_conjunction, 'or'),
    _data_conjunction: $ => sep1($._data_primary, 'and'),
    _data_primary: $ => seq(optional('not'), $._data_atomic),
    _data_atomic: $ =>
      choice(
        $._datatype,
        seq('{', $._literal_list, '}'),
        $.datatype_restriction,
        seq('(', $.data_range, ')'),
      ),
    datatype_restriction: $ =>
      seq(
        $._datatype,
        '[',
        sep1(seq($._facet, $._restriction_value), ','),
        ']',
      ),

    _facet: $ =>
      choice(
        $.keyword_length,
        $.keyword_min_length,
        $.keyword_max_length,
        $.keyword_pattern,
        $.keyword_lang_range,
        '<=',
        '<',
        '>=',
        '>',
      ),

    _restriction_value: $ => $._literal,

    // 2.4 Descriptions
    description: $ => sep1($._conjunction, 'or'),

    _conjunction: $ =>
      choice(
        seq(
          $.class_iri,
          'that',
          optional('not'),
          $._restriction,
          repeat(seq('and', optional('not'), $._restriction)),
        ),
        sep1($._primary, 'and'),
      ),

    _primary: $ => seq(optional('not'), choice($._restriction, $._atomic)),

    _restriction: $ =>
      choice(
        seq($._object_property_expression, $.keyword_some, $._primary),
        seq($._data_property_expression, $.keyword_some, $._data_primary),
        seq($._object_property_expression, $.keyword_only, $._primary),
        seq($._data_property_expression, $.keyword_only, $._data_primary),
        seq($._object_property_expression, $.keyword_self),
        seq(
          $._object_property_expression,
          'min',
          $.non_negative_integer,
          optional($._primary),
        ),
        seq(
          $._data_property_expression,
          'min',
          $.non_negative_integer,
          optional($._data_primary),
        ),
        seq(
          $._object_property_expression,
          'max',
          $.non_negative_integer,
          optional($._primary),
        ),
        seq(
          $._data_property_expression,
          'max',
          $.non_negative_integer,
          optional($._data_primary),
        ),
        seq(
          $._object_property_expression,
          'exactly',
          $.non_negative_integer,
          optional($._primary),
        ),
        seq(
          $._data_property_expression,
          'exactly',
          $.non_negative_integer,
          optional($._data_primary),
        ),
        seq($._object_property_expression, $.keyword_value, $._individual),
        seq($._data_property_expression, $.keyword_value, $._literal),
      ),

    _atomic: $ =>
      choice(
        $.class_iri,
        seq('{', $._individual_list, '}'),
        seq('(', $.description, ')'),
      ),

    // 2.5 Frames and Miscellaneous

    datatype_frame: $ =>
      seq(
        $.keyword_datatype,
        field('iri', $._datatype),
        repeat($.annotations),
        optional($.datatype_equavalent_to),
        repeat($.annotations),
      ),

    datatype_equavalent_to: $ =>
      seq($.keyword_equivalent_to, optional($.annotations), $.data_range),

    class_frame: $ =>
      seq(
        $.keyword_class,
        field('iri', $.class_iri),
        repeat(
          choice(
            $.annotations,
            $.sub_class_of,
            $.class_equivalent_to,
            $.class_disjoint_with,
            $.disjoint_union_of,
            $.has_key,
          ),
        ),
      ),

    sub_class_of: $ =>
      seq($.keyword_sub_class_of, $._description_annotated_list),

    class_equivalent_to: $ =>
      seq($.keyword_equivalent_to, $._description_annotated_list),

    class_disjoint_with: $ =>
      seq($.keyword_disjoint_with, $._description_annotated_list),

    disjoint_union_of: $ =>
      seq(
        $.keyword_disjoint_union_of,
        optional($.annotations),
        $._description_2list,
      ),

    has_key: $ =>
      seq(
        $.keyword_has_key,
        optional($.annotations),
        repeat1(
          choice($._object_property_expression, $._data_property_expression),
        ),
      ),

    object_property_frame: $ =>
      seq(
        $.keyword_object_property,
        field('iri', $.object_property_iri),
        repeat(
          choice(
            $.annotations,
            $.domain,
            $.range,
            $.sub_property_of,
            $.object_property_equivalent_to,
            $.object_property_disjoint_with,
            $.inverse_of,
            $.characteristics,
            $.sub_property_chain,
          ),
        ),
      ),

    domain: $ => seq($.keyword_domain, $._description_annotated_list),

    range: $ => seq($.keyword_range, $._description_annotated_list),

    sub_property_of: $ =>
      seq(
        $.keyword_sub_property_of,
        $._object_property_expression_annotated_list,
      ),

    object_property_equivalent_to: $ =>
      seq(
        $.keyword_equivalent_to,
        $._object_property_expression_annotated_list,
      ),

    object_property_disjoint_with: $ =>
      seq(
        $.keyword_disjoint_with,
        $._object_property_expression_annotated_list,
      ),

    inverse_of: $ =>
      seq($.keyword_inverse_of, $._object_property_expression_annotated_list),

    characteristics: $ =>
      seq(
        $.keyword_characteristics,
        $._object_property_characteristic_annotated_list,
      ),
    sub_property_chain: $ =>
      seq(
        $.keyword_sub_property_chain,
        optional($.annotations),
        sep1($._object_property_expression, 'o'),
      ),

    _object_property_characteristic: $ =>
      choice(
        $.keyword_functional,
        $.keyword_inverse_functional,
        $.keyword_reflexive,
        $.keyword_irreflexive,
        $.keyword_symmetric,
        $.keyword_asymmetric,
        $.keyword_transitive,
      ),

    data_property_frame: $ =>
      seq(
        $.keyword_data_property,
        field('iri', $.data_property_iri),
        repeat(
          choice(
            $.annotations,
            $.data_property_domain,
            $.data_property_range,
            $.data_property_characteristics,
            $.data_property_sub_property_of,
            $.data_property_equivalent_to,
            $.data_property_disjoint_with,
          ),
        ),
      ),

    // TODO hier weiter machen mit den keywords!

    data_property_domain: $ =>
      seq($.keyword_domain, $._description_annotated_list),
    data_property_range: $ =>
      seq($.keyword_range, $._data_range_annotated_list),

    data_property_characteristics: $ =>
      seq(
        $.keyword_characteristics,
        optional($.annotations),
        $.keyword_functional,
      ),
    data_property_sub_property_of: $ =>
      seq(
        $.keyword_sub_property_of,
        $._data_property_expression_annotated_list,
      ),
    data_property_equivalent_to: $ =>
      seq($.keyword_equivalent_to, $._data_property_expression_annotated_list),
    data_property_disjoint_with: $ =>
      seq($.keyword_disjoint_with, $._data_property_expression_annotated_list),

    annotation_property_frame: $ =>
      seq(
        $.keyword_annotation_property,
        field('iri', $.annotation_property_iri),
        repeat(
          choice(
            $.annotations,
            $.annotation_property_domin,
            $.annotation_property_range,
            $.annotation_property_sub_property_of,
          ),
        ),
      ),

    annotation_property_domin: $ =>
      seq($.keyword_domain, $._iri_annotated_list),
    annotation_property_range: $ => seq($.keyword_range, $._iri_annotated_list),
    annotation_property_sub_property_of: $ =>
      seq($.keyword_sub_property_of, $._annotation_property_iri_annotated_list),

    individual_frame: $ =>
      seq(
        $.keyword_individual,
        field('iri', $._individual),
        repeat(
          choice(
            $.annotations,
            $.individual_types,
            $.individual_facts,
            $.individual_same_as,
            $.individual_different_from,
          ),
        ),
      ),

    individual_types: $ => seq($.keyword_types, $._description_annotated_list),

    individual_facts: $ => seq($.keyword_facts, $._fact_annotated_list),

    individual_same_as: $ =>
      seq($.keyword_same_as, $._individual_annotated_list),

    individual_different_from: $ =>
      seq($.keyword_different_from, $._individual_annotated_list),

    fact: $ =>
      seq(
        optional('not'),
        choice($._object_property_fact, $._data_property_fact),
      ),
    _object_property_fact: $ => seq($.object_property_iri, $._individual),
    _data_property_fact: $ => seq($.data_property_iri, $._literal),

    // TODO make misc hidden and add rules for choices
    misc: $ =>
      choice(
        seq(
          $.keyword_equivalent_classes,
          optional($.annotations),
          $._description_2list,
        ), // optional annotations is not to spec. spec wrong?
        seq(
          $.keyword_disjoint_classes,
          optional($.annotations),
          $._description_2list,
        ),
        seq(
          $.keyword_equivalent_properties,
          optional($.annotations),
          $._object_property_2list,
        ),
        seq(
          $.keyword_disjoint_properties,
          optional($.annotations),
          $._object_property_2list,
        ),
        seq(
          $.keyword_equivalent_properties,
          optional($.annotations),
          $._data_property_2list,
        ),
        seq(
          $.keyword_disjoint_properties,
          optional($.annotations),
          $._data_property_2list,
        ),
        seq(
          $.keyword_same_individual,
          optional($.annotations),
          $._individual_2list,
        ),
        seq(
          $.keyword_different_individuals,
          optional($.annotations),
          $._individual_2list,
        ),
      ),

    // Annotated Lists
    _description_annotated_list: $ =>
      annotated_list($.annotations, $.description),
    _annotation_annotated_list: $ =>
      annotated_list($.annotations, $.annotation),
    _object_property_expression_annotated_list: $ =>
      annotated_list($.annotations, $._object_property_expression),
    _object_property_characteristic_annotated_list: $ =>
      annotated_list($.annotations, $._object_property_characteristic),
    _data_range_annotated_list: $ =>
      annotated_list($.annotations, $.data_range),
    _data_property_expression_annotated_list: $ =>
      annotated_list($.annotations, $._data_property_expression),
    _iri_annotated_list: $ => annotated_list($.annotations, $._iri),
    _annotation_property_iri_annotated_list: $ =>
      annotated_list($.annotations, $.annotation_property_iri),
    _individual_annotated_list: $ =>
      annotated_list($.annotations, $._individual),
    _fact_annotated_list: $ => annotated_list($.annotations, $.fact),

    // List2
    _description_2list: $ => seq($.description, ',', sep1($.description, ',')),
    _object_property_2list: $ =>
      seq(
        $._object_property_expression,
        ',',
        sep1($._object_property_expression, ','),
      ), // This is not correct to the grammar. Grammar is missing "objectProperty" Is the grammar wrong?
    // resolved using https://github.com/spechub/Hets/blob/master/OWL2/ParseMS.hs
    _data_property_2list: $ =>
      seq(
        $._data_property_expression,
        ',',
        sep1($._data_property_expression, ','),
      ), // same as with object_property
    _individual_2list: $ => seq($._individual, ',', sep1($._individual, ',')),

    // List
    _individual_list: $ => sep1($._individual, ','),
    _literal_list: $ => sep1($._literal, ','),

    // IRI [RFC 3987]
    // TODO finish this rfc3987 URL rule

    // https://www.w3.org/TR/2008/REC-rdf-sparql-query-20080115/
    // TODO make more strict
    // _pn_local: $ => /[A-Za-z0-9_\-\.]+/,
    _pname_ln: $ => token(seq(pname_ns(), pn_local())), // old= /[A-Za-z0-9_\-\.]*:[A-Za-z0-9_\-\.]+/,
    // _pn_prefix: $ => /[A-Za-z0-9_\-\.]+/,
    // _pname_ln: $ => seq(optional($._pn_prefix), ':', $._pn_local),

    _pn_local: $ => token(pn_local()),

    // Keywords

    keyword_prefix: $ => 'Prefix:',
    keyword_ontology: $ => 'Ontology:',
    keyword_integer: $ => 'integer',
    keyword_decimal: $ => 'decimal',
    keyword_float: $ => 'float',
    keyword_string: $ => 'string',
    keyword_import: $ => 'Import:',
    keyword_annotations: $ => 'Annotations:',
    keyword_inverse: $ => 'inverse',
    keyword_length: $ => 'length',
    keyword_min_length: $ => 'minLength',
    keyword_max_length: $ => 'maxLength',
    keyword_pattern: $ => 'pattern',
    keyword_lang_range: $ => 'langRange',
    keyword_some: $ => 'some',
    keyword_only: $ => 'only',
    keyword_self: $ => 'Self',
    keyword_value: $ => 'value',
    keyword_datatype: $ => 'Datatype:',
    keyword_equivalent_to: $ => 'EquivalentTo:',
    keyword_class: $ => 'Class:',
    keyword_sub_class_of: $ => 'SubClassOf:',
    keyword_disjoint_with: $ => 'DisjointWith:',
    keyword_disjoint_union_of: $ => 'DisjointUnionOf:',
    keyword_has_key: $ => 'HasKey:',
    keyword_object_property: $ => 'ObjectProperty:',
    keyword_domain: $ => 'Domain:',
    keyword_range: $ => 'Range:',
    keyword_sub_property_of: $ => 'SubPropertyOf:',
    keyword_inverse_of: $ => 'InverseOf:',
    keyword_sub_property_chain: $ => 'SubPropertyChain:',
    keyword_functional: $ => 'Functional',
    keyword_inverse_functional: $ => 'InverseFunctional',
    keyword_reflexive: $ => 'Reflexive',
    keyword_irreflexive: $ => 'Irreflexive',
    keyword_symmetric: $ => 'Symmetric',
    keyword_asymmetric: $ => 'Asymmetric',
    keyword_transitive: $ => 'Transitive',
    keyword_data_property: $ => 'DataProperty:',
    keyword_characteristics: $ => 'Characteristics:',
    keyword_annotation_property: $ => 'AnnotationProperty:',
    keyword_individual: $ => 'Individual:',
    keyword_types: $ => 'Types:',
    keyword_facts: $ => 'Facts:',
    keyword_same_as: $ => 'SameAs:',
    keyword_equivalent_classes: $ => 'EquivalentClasses:',
    keyword_disjoint_classes: $ => 'DisjointClasses:',
    keyword_equivalent_properties: $ => 'EquivalentProperties:',
    keyword_disjoint_properties: $ => 'DisjointProperties:',
    keyword_same_individual: $ => 'SameIndividual:',
    keyword_different_individuals: $ => 'DifferentIndividuals:',
    keyword_different_from: $ => 'DifferentFrom:',
  },
})

// Util Functions

function sep1(rule, separator) {
  return seq(rule, repeat(seq(separator, rule)))
}

function annotated_list(annotations, nt) {
  return sep1(seq(optional(annotations), nt), ',')
}

function pn_chars_base() {
  return choice(
    /[A-Z]/,
    /[a-z]/,
    /[\u00C0-\u00D6]/,
    /[\u00D8-\u00F6]/,
    /[\u00F8-\u02FF]/,
    /[\u0370-\u037D]/,
    /[\u037F-\u1FFF]/,
    /[\u200C-\u200D]/,
    /[\u2070-\u218F]/,
    /[\u2C00-\u2FEF]/,
    /[\u3001-\uD7FF]/,
    /[\uF900-\uFDCF]/,
    /[\uFDF0-\uFFFD]/,
    // /[\u10000-\uEFFFF]/,
  )
}

///([A-Z])|([a-z])|([\u00C0-\u00D6])|([\u00D8-\u00F6])|([\u00F8-\u02FF])|([\u0370-\u037D])|([\u037F-\u1FFF])|([\u200C-\u200D])|([\u2070-\u218F])|([\u2C00-\u2FEF])|([\u3001-\uD7FF])|([\uF900-\uFDCF])|([\uFDF0-\uFFFD])|([\u10000-\uEFFFF])/

function pn_chars_u() {
  return choice(pn_chars_base(), '_')
}

function pn_chars() {
  return choice(
    pn_chars_u(),
    '-',
    /[0-9]/,
    /\u00B7/,
    /[\u0300-\u036F]/,
    /[\u203F-\u2040]/,
  )
}

function pn_local() {
  return seq(
    choice(pn_chars_u(), /[0-9]/),
    optional(
      seq(
        repeat(
          choice(
            pn_chars(),

            '.',
          ),
        ),
        pn_chars(),
      ),
    ),
  )
}

function pname_ns() {
  return seq(optional(pn_prefix()), ':')
}

function pn_prefix() {
  return seq(
    pn_chars_base(),
    optional(seq(repeat(choice(pn_chars(), '.')), pn_chars())),
  )
}

function iri_rfc3987() {
  return seq(
    /[A-Za-z][A-Za-z0-9+\-\.]*/,
    ':',
    seq(
      '//',
      seq(
        optional(seq(/[A-Za-z0-9_\-\.\~:%]*/, '@')),
        /[A-Za-z0-9_\-\.\~:%]*/,
        optional(seq(':', /[0-9]*/)),
      ),
      repeat1(seq('/', /[A-Za-z0-9_\-\.\~:%]*/)),
    ),
    optional(/\?[A-Za-z0-9_\-\.\~\/\?]*/),
    optional(/\#[A-Za-z0-9_\-\.\~\/\?]*/),
  )
}

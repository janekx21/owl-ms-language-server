"{" @punctuation.bracket
"}" @punctuation.bracket
"(" @punctuation.bracket
")" @punctuation.bracket
"[" @punctuation.bracket
"]" @punctuation.bracket

"," @punctuation.delimiter
; "o" @punctuation.delimiter

[
  (keyword_ontology)
  (keyword_prefix)
  (keyword_integer)
  (keyword_decimal)
  (keyword_float)
  (keyword_string)
  (keyword_import)
  (keyword_annotations)
  (keyword_datatype)
  (keyword_equivalent_to)
  (keyword_class)
  (keyword_sub_class_of)
  (keyword_equivalent_to)
  (keyword_disjoint_with)
  (keyword_disjoint_union_of)
  (keyword_has_key)
  (keyword_object_property)
  (keyword_domain)
  (keyword_range)
  (keyword_sub_property_of)
  (keyword_equivalent_to)
  (keyword_disjoint_with)
  (keyword_inverse_of)
  (keyword_sub_property_chain)
  (keyword_functional)
  (keyword_inverse_functional)
  (keyword_reflexive)
  (keyword_irreflexive)
  (keyword_symmetric)
  (keyword_asymmetric)
  (keyword_transitive)
  (keyword_data_property)
  (keyword_characteristics)
  (keyword_annotation_property)
  (keyword_individual)
  (keyword_types)
  (keyword_facts)
  (keyword_same_as)
  (keyword_equivalent_classes)
  (keyword_disjoint_classes)
  (keyword_equivalent_properties)
  (keyword_disjoint_properties)
  (keyword_same_individual)
  (keyword_different_individuals)
  (keyword_different_from)
] @keyword


[
  (keyword_inverse)
  (keyword_length)
  (keyword_min_length)
  (keyword_max_length)
  (keyword_pattern)
  (keyword_lang_range)
  (keyword_some)
  (keyword_only)
  (keyword_value)
] @operator

"or" @operator
"and" @operator
"not" @operator
"<=" @operator
"<" @operator
">=" @operator
">" @operator
"that" @operator
"min" @operator
"max" @operator
"exactly" @operator

(keyword_self) @variable.buildin

(string_literal_no_language) @string
(string_literal_with_language) @string
(integer_literal) @number
(decimal_literal) @number
(floating_point_literal) @number
(non_negative_integer) @number
(typed_literal) @constant.builtin

(full_iri) @variable
(abbreviated_iri) @variable
(simple_iri) @variable
(prefix_name) @variable

(comment) @comment

; queries/highlights.scm for tree-sitter-turtle
;
; Ordered general -> specific.

; ---------------------------------------------------------------------------
; Comments
; ---------------------------------------------------------------------------

(comment) @comment.line

; ---------------------------------------------------------------------------
; Keywords / directives
; ---------------------------------------------------------------------------

["@prefix" "@base" "BASE" "PREFIX"] @keyword.directive
"GRAPH" @keyword

; `a` is shorthand for rdf:type
(predicate "a" @keyword)

; ---------------------------------------------------------------------------
; IRIs and prefixed names
; ---------------------------------------------------------------------------

(iri_reference) @string.special.url

(namespace (pn_prefix) @namespace)
(namespace ":" @punctuation.delimiter)

(prefixed_name (pn_local) @constant)

; Subjects
(subject (prefixed_name) @constant)
(subject (iri_reference) @constant)

; Predicates
(predicate (prefixed_name) @variable.other.member)
(predicate (iri_reference) @variable.other.member)

; Objects of `a` are classes
(property
  (predicate "a")
  (object_list
    [(prefixed_name) (iri_reference)] @type))

; Named graph labels
(graph
  label: [(prefixed_name) (iri_reference)] @type)

; ---------------------------------------------------------------------------
; Blank nodes
; ---------------------------------------------------------------------------

(blank_node_label) @label
(anon) @label

; ---------------------------------------------------------------------------
; Literals
; ---------------------------------------------------------------------------

(string) @string
(echar) @constant.character.escape

(lang_tag) @attribute

(rdf_literal
  datatype: [(prefixed_name) (iri_reference)] @type)
(rdf_literal "^^" @operator)

(integer) @constant.numeric.integer
(decimal) @constant.numeric.float
(double) @constant.numeric.float

(boolean_literal) @constant.builtin.boolean

; ---------------------------------------------------------------------------
; Punctuation
; ---------------------------------------------------------------------------

["." ";" ","] @punctuation.delimiter
["(" ")" "[" "]" "{" "}"] @punctuation.bracket

; highlights.scm
;
; Highlighting query for the OWL 2 Functional-Style Syntax tree-sitter
; grammar. Unlike the sibling Manchester Syntax grammar, this grammar
; does not wrap each keyword in its own named `keyword_*` rule -- every
; OWL 2 functional-syntax construct (e.g. `SubClassOf`, `Declaration`,
; `ObjectSomeValuesFrom`) is written as a literal anonymous token, so
; those tokens are matched here directly by their quoted text.

"(" @punctuation.bracket
")" @punctuation.bracket

"=" @punctuation.delimiter
"^^" @punctuation.special

; ---------------------------------------------------------------------
; Document / prefix / declaration structure
; ---------------------------------------------------------------------

[
  "Ontology"
  "Prefix"
  "Import"
  "Declaration"
  "Annotation"
] @keyword

; ---------------------------------------------------------------------
; Entity types (Section 5, Section 5.8 Entity)
; ---------------------------------------------------------------------

[
  "Class"
  "Datatype"
  "ObjectProperty"
  "DataProperty"
  "AnnotationProperty"
  "NamedIndividual"
] @keyword

; ---------------------------------------------------------------------
; Class expression axioms (Section 9.1)
; ---------------------------------------------------------------------

[
  "SubClassOf"
  "EquivalentClasses"
  "DisjointClasses"
  "DisjointUnion"
] @keyword

; ---------------------------------------------------------------------
; Object property axioms (Section 9.2)
; ---------------------------------------------------------------------

[
  "SubObjectPropertyOf"
  "ObjectPropertyChain"
  "EquivalentObjectProperties"
  "DisjointObjectProperties"
  "InverseObjectProperties"
  "ObjectPropertyDomain"
  "ObjectPropertyRange"
  "FunctionalObjectProperty"
  "InverseFunctionalObjectProperty"
  "ReflexiveObjectProperty"
  "IrreflexiveObjectProperty"
  "SymmetricObjectProperty"
  "AsymmetricObjectProperty"
  "TransitiveObjectProperty"
] @keyword

; ---------------------------------------------------------------------
; Data property axioms (Section 9.3)
; ---------------------------------------------------------------------

[
  "SubDataPropertyOf"
  "EquivalentDataProperties"
  "DisjointDataProperties"
  "DataPropertyDomain"
  "DataPropertyRange"
  "FunctionalDataProperty"
] @keyword

; ---------------------------------------------------------------------
; Datatype definitions and keys (Section 9.4, 9.5)
; ---------------------------------------------------------------------

[
  "DatatypeDefinition"
  "HasKey"
] @keyword

; ---------------------------------------------------------------------
; Assertions / facts (Section 9.6)
; ---------------------------------------------------------------------

[
  "SameIndividual"
  "DifferentIndividuals"
  "ClassAssertion"
  "ObjectPropertyAssertion"
  "NegativeObjectPropertyAssertion"
  "DataPropertyAssertion"
  "NegativeDataPropertyAssertion"
] @keyword

; ---------------------------------------------------------------------
; Annotation axioms (Section 10.2)
; ---------------------------------------------------------------------

[
  "AnnotationAssertion"
  "SubAnnotationPropertyOf"
  "AnnotationPropertyDomain"
  "AnnotationPropertyRange"
] @keyword

; ---------------------------------------------------------------------
; Class / data range constructors (Section 7, Section 8) -- these play
; the role that infix operators (and/or/not/some/only/min/max/exactly)
; play in the Manchester Syntax, so they are highlighted as operators
; rather than plain keywords.
; ---------------------------------------------------------------------

[
  "ObjectIntersectionOf"
  "ObjectUnionOf"
  "ObjectComplementOf"
  "ObjectOneOf"
  "ObjectSomeValuesFrom"
  "ObjectAllValuesFrom"
  "ObjectHasValue"
  "ObjectHasSelf"
  "ObjectMinCardinality"
  "ObjectMaxCardinality"
  "ObjectExactCardinality"
  "ObjectInverseOf"
  "DataIntersectionOf"
  "DataUnionOf"
  "DataComplementOf"
  "DataOneOf"
  "DatatypeRestriction"
  "DataSomeValuesFrom"
  "DataAllValuesFrom"
  "DataHasValue"
  "DataMinCardinality"
  "DataMaxCardinality"
  "DataExactCardinality"
] @operator

; ---------------------------------------------------------------------
; Literals
; ---------------------------------------------------------------------

(stringLiteralNoLanguage) @string
(stringLiteralWithLanguage) @string
(languageTag) @string.special
(typedLiteral) @constant.builtin
(nonNegativeInteger) @number

; ---------------------------------------------------------------------
; IRIs, prefixes, and anonymous individuals
; ---------------------------------------------------------------------

(fullIRI) @variable
(abbreviatedIRI) @variable
(prefixName) @variable
(nodeID) @variable.builtin

; ---------------------------------------------------------------------
; Comments
; ---------------------------------------------------------------------

(comment) @comment

/**
 * grammar.js
 *
 * A tree-sitter grammar for the OWL 2 Web Ontology Language
 * Functional-Style Syntax, as defined in:
 *
 *   OWL 2 Web Ontology Language
 *   Structural Specification and Functional-Style Syntax (Second Edition)
 *   W3C Recommendation 11 December 2012
 *   http://www.w3.org/TR/2012/REC-owl2-syntax-20121211/
 *
 * The rule names below closely follow the BNF grammar given in
 * Section 13 ("Appendix: Complete Grammar") of the specification, so
 * that the specification can be used directly as a reference while
 * reading or extending this grammar.
 *
 * Notes on departures from the normative BNF:
 *  - Whitespace and `#`-comments (Section 2.2) are handled generically
 *    via `extras`, rather than being threaded explicitly through every
 *    production, since tree-sitter deals with this at the lexer level.
 *  - `fullIRI`, `prefixName`, `abbreviatedIRI`, `quotedString`,
 *    `languageTag` and `nodeID` are implemented as regular expressions
 *    that approximate (rather than exactly reproduce) the referenced
 *    external grammars (RFC 3987 IRIs, SPARQL PNAME_NS/PNAME_LN,
 *    BCP 47 language tags, SPARQL BLANK_NODE_LABEL). This is standard
 *    practice for tree-sitter grammars, which favor practical,
 *    unambiguous lexing over byte-for-byte conformance.
 */

module.exports = grammar({
  name: 'owl_functional',

  extras: $ => [/[ \t\r\n]/, $.comment],

  word: $ => $._bare_word,

  rules: {
    // ------------------------------------------------------------------
    // 3.7 / 13.1  Ontology documents
    // ------------------------------------------------------------------

    // ontologyDocument := { prefixDeclaration } Ontology
    ontologyDocument: $ => seq(repeat($.prefixDeclaration), $.Ontology),

    // prefixDeclaration := 'Prefix' '(' prefixName '=' fullIRI ')'
    prefixDeclaration: $ =>
      seq('Prefix', '(', $.prefixName, '=', $.fullIRI, ')'),

    // Ontology :=
    //   'Ontology' '(' [ ontologyIRI [ versionIRI ] ]
    //      directlyImportsDocuments
    //      ontologyAnnotations
    //      axioms
    //   ')'
    Ontology: $ =>
      seq(
        'Ontology',
        '(',
        optional(
          seq(
            alias($.IRI, $.ontologyIRI),
            optional(alias($.IRI, $.versionIRI)),
          ),
        ),
        // directlyImportsDocuments := { 'Import' '(' IRI ')' }
        repeat($.Import),
        // ontologyAnnotations := { Annotation }
        repeat($.Annotation),
        // axioms := { Axiom }
        repeat($.Axiom),
        ')',
      ),

    Import: $ => seq('Import', '(', $.IRI, ')'),

    // ------------------------------------------------------------------
    // 2.3 / 2.4 / 13.1  Lexical building blocks
    // ------------------------------------------------------------------

    // nonNegativeInteger := a nonempty finite sequence of digits 0-9
    nonNegativeInteger: $ => /[0-9]+/,

    // quotedString := chars where " and \ occur only as \" or \\,
    //                 enclosed in a pair of " characters
    quotedString: $ => /"([^"\\]|\\.)*"/,

    // languageTag := '@' followed by a BCP 47 langtag
    languageTag: $ => /@[A-Za-z]+(-[A-Za-z0-9]+)*/,

    // nodeID := a SPARQL BLANK_NODE_LABEL
    nodeID: $ => /_:[A-Za-z0-9_\u00C0-\uFFFF][A-Za-z0-9_.\-\u00C0-\uFFFF]*/,

    // fullIRI := an RFC 3987 IRI enclosed in '<' '>'
    fullIRI: $ => /<[^\s<>"{}|\^`\\]*>/,

    // prefixName := a SPARQL PNAME_NS (possibly-empty prefix, then ':')
    prefixName: $ => /([A-Za-z_][A-Za-z0-9_.\-]*)?:/,

    // abbreviatedIRI := a SPARQL PNAME_LN (prefixName followed by a
    //                   local name, with no intervening whitespace)
    abbreviatedIRI: $ =>
      /([A-Za-z_][A-Za-z0-9_.\-]*)?:[A-Za-z_][A-Za-z0-9_.\-]*/,

    // IRI := fullIRI | abbreviatedIRI
    IRI: $ => choice($.fullIRI, $.abbreviatedIRI),

    // used only to satisfy tree-sitter's `word` requirement; not part
    // of the normative grammar. Matches the keyword-like tokens used
    // throughout the functional-style syntax (e.g. 'SubClassOf').
    _bare_word: $ => /[A-Za-z][A-Za-z0-9]*/,

    // # comments, per Section 2.2
    comment: $ => /#[^\n\r]*/,

    // ------------------------------------------------------------------
    // 5.8 / 13.1  Declarations
    // ------------------------------------------------------------------

    // Declaration := 'Declaration' '(' axiomAnnotations Entity ')'
    Declaration: $ =>
      seq('Declaration', '(', repeat($.Annotation), $.Entity, ')'),

    // Entity :=
    //   'Class' '(' Class ')' |
    //   'Datatype' '(' Datatype ')' |
    //   'ObjectProperty' '(' ObjectProperty ')' |
    //   'DataProperty' '(' DataProperty ')' |
    //   'AnnotationProperty' '(' AnnotationProperty ')' |
    //   'NamedIndividual' '(' NamedIndividual ')'
    Entity: $ =>
      choice(
        seq('Class', '(', $.Class, ')'),
        seq('Datatype', '(', $.Datatype, ')'),
        seq('ObjectProperty', '(', $.ObjectProperty, ')'),
        seq('DataProperty', '(', $.DataProperty, ')'),
        seq('AnnotationProperty', '(', $.AnnotationProperty, ')'),
        seq('NamedIndividual', '(', $.NamedIndividual, ')'),
      ),

    // ------------------------------------------------------------------
    // 10  Annotations
    // ------------------------------------------------------------------

    // AnnotationSubject := IRI | AnonymousIndividual
    AnnotationSubject: $ => choice($.IRI, $.AnonymousIndividual),

    // AnnotationValue := AnonymousIndividual | IRI | Literal
    AnnotationValue: $ => choice($.AnonymousIndividual, $.IRI, $.Literal),

    // Annotation := 'Annotation' '(' annotationAnnotations
    //                 AnnotationProperty AnnotationValue ')'
    Annotation: $ =>
      seq(
        'Annotation',
        '(',
        repeat($.Annotation),
        $.AnnotationProperty,
        $.AnnotationValue,
        ')',
      ),

    // AnnotationAxiom := AnnotationAssertion | SubAnnotationPropertyOf |
    //                    AnnotationPropertyDomain | AnnotationPropertyRange
    AnnotationAxiom: $ =>
      choice(
        $.AnnotationAssertion,
        $.SubAnnotationPropertyOf,
        $.AnnotationPropertyDomain,
        $.AnnotationPropertyRange,
      ),

    // AnnotationAssertion := 'AnnotationAssertion' '(' axiomAnnotations
    //   AnnotationProperty AnnotationSubject AnnotationValue ')'
    AnnotationAssertion: $ =>
      seq(
        'AnnotationAssertion',
        '(',
        repeat($.Annotation),
        $.AnnotationProperty,
        $.AnnotationSubject,
        $.AnnotationValue,
        ')',
      ),

    // SubAnnotationPropertyOf := 'SubAnnotationPropertyOf' '('
    //   axiomAnnotations subAnnotationProperty superAnnotationProperty ')'
    SubAnnotationPropertyOf: $ =>
      seq(
        'SubAnnotationPropertyOf',
        '(',
        repeat($.Annotation),
        alias($.AnnotationProperty, $.subAnnotationProperty),
        alias($.AnnotationProperty, $.superAnnotationProperty),
        ')',
      ),

    // AnnotationPropertyDomain := 'AnnotationPropertyDomain' '('
    //   axiomAnnotations AnnotationProperty IRI ')'
    AnnotationPropertyDomain: $ =>
      seq(
        'AnnotationPropertyDomain',
        '(',
        repeat($.Annotation),
        $.AnnotationProperty,
        $.IRI,
        ')',
      ),

    // AnnotationPropertyRange := 'AnnotationPropertyRange' '('
    //   axiomAnnotations AnnotationProperty IRI ')'
    AnnotationPropertyRange: $ =>
      seq(
        'AnnotationPropertyRange',
        '(',
        repeat($.Annotation),
        $.AnnotationProperty,
        $.IRI,
        ')',
      ),

    // ------------------------------------------------------------------
    // 5  Entities, Literals, and Anonymous Individuals
    // ------------------------------------------------------------------

    // Class := IRI
    Class: $ => $.IRI,
    // Datatype := IRI
    Datatype: $ => $.IRI,
    // ObjectProperty := IRI
    ObjectProperty: $ => $.IRI,
    // DataProperty := IRI
    DataProperty: $ => $.IRI,
    // AnnotationProperty := IRI
    AnnotationProperty: $ => $.IRI,

    // Individual := NamedIndividual | AnonymousIndividual
    Individual: $ => choice($.NamedIndividual, $.AnonymousIndividual),
    // NamedIndividual := IRI
    NamedIndividual: $ => $.IRI,
    // AnonymousIndividual := nodeID
    AnonymousIndividual: $ => $.nodeID,

    // Literal := typedLiteral | stringLiteralNoLanguage
    //          | stringLiteralWithLanguage
    Literal: $ =>
      choice(
        $.typedLiteral,
        $.stringLiteralWithLanguage,
        $.stringLiteralNoLanguage,
      ),

    // typedLiteral := lexicalForm '^^' Datatype
    typedLiteral: $ => seq($.lexicalForm, '^^', $.Datatype),
    // lexicalForm := quotedString
    lexicalForm: $ => $.quotedString,
    // stringLiteralNoLanguage := quotedString
    stringLiteralNoLanguage: $ => $.quotedString,
    // stringLiteralWithLanguage := quotedString languageTag
    stringLiteralWithLanguage: $ => seq($.quotedString, $.languageTag),

    // ------------------------------------------------------------------
    // 6  Property Expressions
    // ------------------------------------------------------------------

    // ObjectPropertyExpression := ObjectProperty | InverseObjectProperty
    ObjectPropertyExpression: $ =>
      choice($.ObjectProperty, $.InverseObjectProperty),

    // InverseObjectProperty := 'ObjectInverseOf' '(' ObjectProperty ')'
    InverseObjectProperty: $ =>
      seq('ObjectInverseOf', '(', $.ObjectProperty, ')'),

    // DataPropertyExpression := DataProperty
    DataPropertyExpression: $ => $.DataProperty,

    // ------------------------------------------------------------------
    // 7  Data Ranges
    // ------------------------------------------------------------------

    // DataRange :=
    //   Datatype | DataIntersectionOf | DataUnionOf | DataComplementOf
    //   | DataOneOf | DatatypeRestriction
    DataRange: $ =>
      choice(
        $.Datatype,
        $.DataIntersectionOf,
        $.DataUnionOf,
        $.DataComplementOf,
        $.DataOneOf,
        $.DatatypeRestriction,
      ),

    // DataIntersectionOf := 'DataIntersectionOf' '(' DataRange DataRange
    //                        { DataRange } ')'
    DataIntersectionOf: $ =>
      seq(
        'DataIntersectionOf',
        '(',
        $.DataRange,
        $.DataRange,
        repeat($.DataRange),
        ')',
      ),

    // DataUnionOf := 'DataUnionOf' '(' DataRange DataRange { DataRange } ')'
    DataUnionOf: $ =>
      seq(
        'DataUnionOf',
        '(',
        $.DataRange,
        $.DataRange,
        repeat($.DataRange),
        ')',
      ),

    // DataComplementOf := 'DataComplementOf' '(' DataRange ')'
    DataComplementOf: $ => seq('DataComplementOf', '(', $.DataRange, ')'),

    // DataOneOf := 'DataOneOf' '(' Literal { Literal } ')'
    DataOneOf: $ => seq('DataOneOf', '(', $.Literal, repeat($.Literal), ')'),

    // DatatypeRestriction := 'DatatypeRestriction' '(' Datatype
    //   constrainingFacet restrictionValue
    //   { constrainingFacet restrictionValue } ')'
    DatatypeRestriction: $ =>
      seq(
        'DatatypeRestriction',
        '(',
        $.Datatype,
        $.constrainingFacet,
        $.restrictionValue,
        repeat(seq($.constrainingFacet, $.restrictionValue)),
        ')',
      ),
    // constrainingFacet := IRI
    constrainingFacet: $ => $.IRI,
    // restrictionValue := Literal
    restrictionValue: $ => $.Literal,

    // ------------------------------------------------------------------
    // 8  Class Expressions
    // ------------------------------------------------------------------

    // ClassExpression :=
    //   Class
    //   | ObjectIntersectionOf | ObjectUnionOf | ObjectComplementOf | ObjectOneOf
    //   | ObjectSomeValuesFrom | ObjectAllValuesFrom | ObjectHasValue | ObjectHasSelf
    //   | ObjectMinCardinality | ObjectMaxCardinality | ObjectExactCardinality
    //   | DataSomeValuesFrom | DataAllValuesFrom | DataHasValue
    //   | DataMinCardinality | DataMaxCardinality | DataExactCardinality
    ClassExpression: $ =>
      choice(
        $.Class,
        $.ObjectIntersectionOf,
        $.ObjectUnionOf,
        $.ObjectComplementOf,
        $.ObjectOneOf,
        $.ObjectSomeValuesFrom,
        $.ObjectAllValuesFrom,
        $.ObjectHasValue,
        $.ObjectHasSelf,
        $.ObjectMinCardinality,
        $.ObjectMaxCardinality,
        $.ObjectExactCardinality,
        $.DataSomeValuesFrom,
        $.DataAllValuesFrom,
        $.DataHasValue,
        $.DataMinCardinality,
        $.DataMaxCardinality,
        $.DataExactCardinality,
      ),

    // 8.1.1  ObjectIntersectionOf := 'ObjectIntersectionOf' '('
    //          ClassExpression ClassExpression { ClassExpression } ')'
    ObjectIntersectionOf: $ =>
      seq(
        'ObjectIntersectionOf',
        '(',
        $.ClassExpression,
        $.ClassExpression,
        repeat($.ClassExpression),
        ')',
      ),

    // 8.1.2  ObjectUnionOf := 'ObjectUnionOf' '('
    //          ClassExpression ClassExpression { ClassExpression } ')'
    ObjectUnionOf: $ =>
      seq(
        'ObjectUnionOf',
        '(',
        $.ClassExpression,
        $.ClassExpression,
        repeat($.ClassExpression),
        ')',
      ),

    // 8.1.3  ObjectComplementOf := 'ObjectComplementOf' '(' ClassExpression ')'
    ObjectComplementOf: $ =>
      seq('ObjectComplementOf', '(', $.ClassExpression, ')'),

    // 8.1.4  ObjectOneOf := 'ObjectOneOf' '(' Individual { Individual } ')'
    ObjectOneOf: $ =>
      seq('ObjectOneOf', '(', $.Individual, repeat($.Individual), ')'),

    // 8.2.1  ObjectSomeValuesFrom := 'ObjectSomeValuesFrom' '('
    //          ObjectPropertyExpression ClassExpression ')'
    ObjectSomeValuesFrom: $ =>
      seq(
        'ObjectSomeValuesFrom',
        '(',
        $.ObjectPropertyExpression,
        $.ClassExpression,
        ')',
      ),

    // 8.2.2  ObjectAllValuesFrom := 'ObjectAllValuesFrom' '('
    //          ObjectPropertyExpression ClassExpression ')'
    ObjectAllValuesFrom: $ =>
      seq(
        'ObjectAllValuesFrom',
        '(',
        $.ObjectPropertyExpression,
        $.ClassExpression,
        ')',
      ),

    // 8.2.3  ObjectHasValue := 'ObjectHasValue' '('
    //          ObjectPropertyExpression Individual ')'
    ObjectHasValue: $ =>
      seq('ObjectHasValue', '(', $.ObjectPropertyExpression, $.Individual, ')'),

    // 8.2.4  ObjectHasSelf := 'ObjectHasSelf' '(' ObjectPropertyExpression ')'
    ObjectHasSelf: $ =>
      seq('ObjectHasSelf', '(', $.ObjectPropertyExpression, ')'),

    // 8.3.1  ObjectMinCardinality := 'ObjectMinCardinality' '('
    //          nonNegativeInteger ObjectPropertyExpression [ ClassExpression ] ')'
    ObjectMinCardinality: $ =>
      seq(
        'ObjectMinCardinality',
        '(',
        $.nonNegativeInteger,
        $.ObjectPropertyExpression,
        optional($.ClassExpression),
        ')',
      ),

    // 8.3.2  ObjectMaxCardinality := 'ObjectMaxCardinality' '('
    //          nonNegativeInteger ObjectPropertyExpression [ ClassExpression ] ')'
    ObjectMaxCardinality: $ =>
      seq(
        'ObjectMaxCardinality',
        '(',
        $.nonNegativeInteger,
        $.ObjectPropertyExpression,
        optional($.ClassExpression),
        ')',
      ),

    // 8.3.3  ObjectExactCardinality := 'ObjectExactCardinality' '('
    //          nonNegativeInteger ObjectPropertyExpression [ ClassExpression ] ')'
    ObjectExactCardinality: $ =>
      seq(
        'ObjectExactCardinality',
        '(',
        $.nonNegativeInteger,
        $.ObjectPropertyExpression,
        optional($.ClassExpression),
        ')',
      ),

    // 8.4.1  DataSomeValuesFrom := 'DataSomeValuesFrom' '('
    //          DataPropertyExpression { DataPropertyExpression } DataRange ')'
    DataSomeValuesFrom: $ =>
      seq(
        'DataSomeValuesFrom',
        '(',
        $.DataPropertyExpression,
        repeat($.DataPropertyExpression),
        $.DataRange,
        ')',
      ),

    // 8.4.2  DataAllValuesFrom := 'DataAllValuesFrom' '('
    //          DataPropertyExpression { DataPropertyExpression } DataRange ')'
    DataAllValuesFrom: $ =>
      seq(
        'DataAllValuesFrom',
        '(',
        $.DataPropertyExpression,
        repeat($.DataPropertyExpression),
        $.DataRange,
        ')',
      ),

    // 8.4.3  DataHasValue := 'DataHasValue' '(' DataPropertyExpression Literal ')'
    DataHasValue: $ =>
      seq('DataHasValue', '(', $.DataPropertyExpression, $.Literal, ')'),

    // 8.5.1  DataMinCardinality := 'DataMinCardinality' '('
    //          nonNegativeInteger DataPropertyExpression [ DataRange ] ')'
    DataMinCardinality: $ =>
      seq(
        'DataMinCardinality',
        '(',
        $.nonNegativeInteger,
        $.DataPropertyExpression,
        optional($.DataRange),
        ')',
      ),

    // 8.5.2  DataMaxCardinality := 'DataMaxCardinality' '('
    //          nonNegativeInteger DataPropertyExpression [ DataRange ] ')'
    DataMaxCardinality: $ =>
      seq(
        'DataMaxCardinality',
        '(',
        $.nonNegativeInteger,
        $.DataPropertyExpression,
        optional($.DataRange),
        ')',
      ),

    // 8.5.3  DataExactCardinality := 'DataExactCardinality' '('
    //          nonNegativeInteger DataPropertyExpression [ DataRange ] ')'
    DataExactCardinality: $ =>
      seq(
        'DataExactCardinality',
        '(',
        $.nonNegativeInteger,
        $.DataPropertyExpression,
        optional($.DataRange),
        ')',
      ),

    // ------------------------------------------------------------------
    // 9  Axioms
    // ------------------------------------------------------------------

    // Axiom := Declaration | ClassAxiom | ObjectPropertyAxiom |
    //          DataPropertyAxiom | DatatypeDefinition | HasKey |
    //          Assertion | AnnotationAxiom
    Axiom: $ =>
      choice(
        $.Declaration,
        $.ClassAxiom,
        $.ObjectPropertyAxiom,
        $.DataPropertyAxiom,
        $.DatatypeDefinition,
        $.HasKey,
        $.Assertion,
        $.AnnotationAxiom,
      ),

    // ---- 9.1  Class Expression Axioms --------------------------------

    // ClassAxiom := SubClassOf | EquivalentClasses | DisjointClasses | DisjointUnion
    ClassAxiom: $ =>
      choice(
        $.SubClassOf,
        $.EquivalentClasses,
        $.DisjointClasses,
        $.DisjointUnion,
      ),

    // SubClassOf := 'SubClassOf' '(' axiomAnnotations
    //   subClassExpression superClassExpression ')'
    SubClassOf: $ =>
      seq(
        'SubClassOf',
        '(',
        repeat($.Annotation),
        alias($.ClassExpression, $.subClassExpression),
        alias($.ClassExpression, $.superClassExpression),
        ')',
      ),

    // EquivalentClasses := 'EquivalentClasses' '(' axiomAnnotations
    //   ClassExpression ClassExpression { ClassExpression } ')'
    EquivalentClasses: $ =>
      seq(
        'EquivalentClasses',
        '(',
        repeat($.Annotation),
        $.ClassExpression,
        $.ClassExpression,
        repeat($.ClassExpression),
        ')',
      ),

    // DisjointClasses := 'DisjointClasses' '(' axiomAnnotations
    //   ClassExpression ClassExpression { ClassExpression } ')'
    DisjointClasses: $ =>
      seq(
        'DisjointClasses',
        '(',
        repeat($.Annotation),
        $.ClassExpression,
        $.ClassExpression,
        repeat($.ClassExpression),
        ')',
      ),

    // DisjointUnion := 'DisjointUnion' '(' axiomAnnotations
    //   Class disjointClassExpressions ')'
    DisjointUnion: $ =>
      seq(
        'DisjointUnion',
        '(',
        repeat($.Annotation),
        $.Class,
        $.disjointClassExpressions,
        ')',
      ),
    // disjointClassExpressions := ClassExpression ClassExpression { ClassExpression }
    disjointClassExpressions: $ =>
      seq($.ClassExpression, $.ClassExpression, repeat($.ClassExpression)),

    // ---- 9.2  Object Property Axioms ---------------------------------

    // ObjectPropertyAxiom :=
    //   SubObjectPropertyOf | EquivalentObjectProperties |
    //   DisjointObjectProperties | InverseObjectProperties |
    //   ObjectPropertyDomain | ObjectPropertyRange |
    //   FunctionalObjectProperty | InverseFunctionalObjectProperty |
    //   ReflexiveObjectProperty | IrreflexiveObjectProperty |
    //   SymmetricObjectProperty | AsymmetricObjectProperty |
    //   TransitiveObjectProperty
    ObjectPropertyAxiom: $ =>
      choice(
        $.SubObjectPropertyOf,
        $.EquivalentObjectProperties,
        $.DisjointObjectProperties,
        $.InverseObjectProperties,
        $.ObjectPropertyDomain,
        $.ObjectPropertyRange,
        $.FunctionalObjectProperty,
        $.InverseFunctionalObjectProperty,
        $.ReflexiveObjectProperty,
        $.IrreflexiveObjectProperty,
        $.SymmetricObjectProperty,
        $.AsymmetricObjectProperty,
        $.TransitiveObjectProperty,
      ),

    // SubObjectPropertyOf := 'SubObjectPropertyOf' '(' axiomAnnotations
    //   subObjectPropertyExpression superObjectPropertyExpression ')'
    SubObjectPropertyOf: $ =>
      seq(
        'SubObjectPropertyOf',
        '(',
        repeat($.Annotation),
        $.subObjectPropertyExpression,
        alias($.ObjectPropertyExpression, $.superObjectPropertyExpression),
        ')',
      ),
    // subObjectPropertyExpression := ObjectPropertyExpression | propertyExpressionChain
    subObjectPropertyExpression: $ =>
      choice($.ObjectPropertyExpression, $.propertyExpressionChain),
    // propertyExpressionChain := 'ObjectPropertyChain' '('
    //   ObjectPropertyExpression ObjectPropertyExpression
    //   { ObjectPropertyExpression } ')'
    propertyExpressionChain: $ =>
      seq(
        'ObjectPropertyChain',
        '(',
        $.ObjectPropertyExpression,
        $.ObjectPropertyExpression,
        repeat($.ObjectPropertyExpression),
        ')',
      ),

    // EquivalentObjectProperties := 'EquivalentObjectProperties' '('
    //   axiomAnnotations ObjectPropertyExpression ObjectPropertyExpression
    //   { ObjectPropertyExpression } ')'
    EquivalentObjectProperties: $ =>
      seq(
        'EquivalentObjectProperties',
        '(',
        repeat($.Annotation),
        $.ObjectPropertyExpression,
        $.ObjectPropertyExpression,
        repeat($.ObjectPropertyExpression),
        ')',
      ),

    // DisjointObjectProperties := 'DisjointObjectProperties' '('
    //   axiomAnnotations ObjectPropertyExpression ObjectPropertyExpression
    //   { ObjectPropertyExpression } ')'
    DisjointObjectProperties: $ =>
      seq(
        'DisjointObjectProperties',
        '(',
        repeat($.Annotation),
        $.ObjectPropertyExpression,
        $.ObjectPropertyExpression,
        repeat($.ObjectPropertyExpression),
        ')',
      ),

    // ObjectPropertyDomain := 'ObjectPropertyDomain' '(' axiomAnnotations
    //   ObjectPropertyExpression ClassExpression ')'
    ObjectPropertyDomain: $ =>
      seq(
        'ObjectPropertyDomain',
        '(',
        repeat($.Annotation),
        $.ObjectPropertyExpression,
        $.ClassExpression,
        ')',
      ),

    // ObjectPropertyRange := 'ObjectPropertyRange' '(' axiomAnnotations
    //   ObjectPropertyExpression ClassExpression ')'
    ObjectPropertyRange: $ =>
      seq(
        'ObjectPropertyRange',
        '(',
        repeat($.Annotation),
        $.ObjectPropertyExpression,
        $.ClassExpression,
        ')',
      ),

    // InverseObjectProperties := 'InverseObjectProperties' '('
    //   axiomAnnotations ObjectPropertyExpression ObjectPropertyExpression ')'
    InverseObjectProperties: $ =>
      seq(
        'InverseObjectProperties',
        '(',
        repeat($.Annotation),
        $.ObjectPropertyExpression,
        $.ObjectPropertyExpression,
        ')',
      ),

    // FunctionalObjectProperty := 'FunctionalObjectProperty' '('
    //   axiomAnnotations ObjectPropertyExpression ')'
    FunctionalObjectProperty: $ =>
      seq(
        'FunctionalObjectProperty',
        '(',
        repeat($.Annotation),
        $.ObjectPropertyExpression,
        ')',
      ),

    // InverseFunctionalObjectProperty := 'InverseFunctionalObjectProperty' '('
    //   axiomAnnotations ObjectPropertyExpression ')'
    InverseFunctionalObjectProperty: $ =>
      seq(
        'InverseFunctionalObjectProperty',
        '(',
        repeat($.Annotation),
        $.ObjectPropertyExpression,
        ')',
      ),

    // ReflexiveObjectProperty := 'ReflexiveObjectProperty' '('
    //   axiomAnnotations ObjectPropertyExpression ')'
    ReflexiveObjectProperty: $ =>
      seq(
        'ReflexiveObjectProperty',
        '(',
        repeat($.Annotation),
        $.ObjectPropertyExpression,
        ')',
      ),

    // IrreflexiveObjectProperty := 'IrreflexiveObjectProperty' '('
    //   axiomAnnotations ObjectPropertyExpression ')'
    IrreflexiveObjectProperty: $ =>
      seq(
        'IrreflexiveObjectProperty',
        '(',
        repeat($.Annotation),
        $.ObjectPropertyExpression,
        ')',
      ),

    // SymmetricObjectProperty := 'SymmetricObjectProperty' '('
    //   axiomAnnotations ObjectPropertyExpression ')'
    SymmetricObjectProperty: $ =>
      seq(
        'SymmetricObjectProperty',
        '(',
        repeat($.Annotation),
        $.ObjectPropertyExpression,
        ')',
      ),

    // AsymmetricObjectProperty := 'AsymmetricObjectProperty' '('
    //   axiomAnnotations ObjectPropertyExpression ')'
    AsymmetricObjectProperty: $ =>
      seq(
        'AsymmetricObjectProperty',
        '(',
        repeat($.Annotation),
        $.ObjectPropertyExpression,
        ')',
      ),

    // TransitiveObjectProperty := 'TransitiveObjectProperty' '('
    //   axiomAnnotations ObjectPropertyExpression ')'
    TransitiveObjectProperty: $ =>
      seq(
        'TransitiveObjectProperty',
        '(',
        repeat($.Annotation),
        $.ObjectPropertyExpression,
        ')',
      ),

    // ---- 9.3  Data Property Axioms ------------------------------------

    // DataPropertyAxiom :=
    //   SubDataPropertyOf | EquivalentDataProperties | DisjointDataProperties |
    //   DataPropertyDomain | DataPropertyRange | FunctionalDataProperty
    DataPropertyAxiom: $ =>
      choice(
        $.SubDataPropertyOf,
        $.EquivalentDataProperties,
        $.DisjointDataProperties,
        $.DataPropertyDomain,
        $.DataPropertyRange,
        $.FunctionalDataProperty,
      ),

    // SubDataPropertyOf := 'SubDataPropertyOf' '(' axiomAnnotations
    //   subDataPropertyExpression superDataPropertyExpression ')'
    SubDataPropertyOf: $ =>
      seq(
        'SubDataPropertyOf',
        '(',
        repeat($.Annotation),
        alias($.DataPropertyExpression, $.subDataPropertyExpression),
        alias($.DataPropertyExpression, $.superDataPropertyExpression),
        ')',
      ),

    // EquivalentDataProperties := 'EquivalentDataProperties' '('
    //   axiomAnnotations DataPropertyExpression DataPropertyExpression
    //   { DataPropertyExpression } ')'
    EquivalentDataProperties: $ =>
      seq(
        'EquivalentDataProperties',
        '(',
        repeat($.Annotation),
        $.DataPropertyExpression,
        $.DataPropertyExpression,
        repeat($.DataPropertyExpression),
        ')',
      ),

    // DisjointDataProperties := 'DisjointDataProperties' '('
    //   axiomAnnotations DataPropertyExpression DataPropertyExpression
    //   { DataPropertyExpression } ')'
    DisjointDataProperties: $ =>
      seq(
        'DisjointDataProperties',
        '(',
        repeat($.Annotation),
        $.DataPropertyExpression,
        $.DataPropertyExpression,
        repeat($.DataPropertyExpression),
        ')',
      ),

    // DataPropertyDomain := 'DataPropertyDomain' '(' axiomAnnotations
    //   DataPropertyExpression ClassExpression ')'
    DataPropertyDomain: $ =>
      seq(
        'DataPropertyDomain',
        '(',
        repeat($.Annotation),
        $.DataPropertyExpression,
        $.ClassExpression,
        ')',
      ),

    // DataPropertyRange := 'DataPropertyRange' '(' axiomAnnotations
    //   DataPropertyExpression DataRange ')'
    DataPropertyRange: $ =>
      seq(
        'DataPropertyRange',
        '(',
        repeat($.Annotation),
        $.DataPropertyExpression,
        $.DataRange,
        ')',
      ),

    // FunctionalDataProperty := 'FunctionalDataProperty' '('
    //   axiomAnnotations DataPropertyExpression ')'
    FunctionalDataProperty: $ =>
      seq(
        'FunctionalDataProperty',
        '(',
        repeat($.Annotation),
        $.DataPropertyExpression,
        ')',
      ),

    // ---- 9.4  Datatype Definitions -------------------------------------

    // DatatypeDefinition := 'DatatypeDefinition' '(' axiomAnnotations
    //   Datatype DataRange ')'
    DatatypeDefinition: $ =>
      seq(
        'DatatypeDefinition',
        '(',
        repeat($.Annotation),
        $.Datatype,
        $.DataRange,
        ')',
      ),

    // ---- 9.5  Keys ------------------------------------------------------

    // HasKey := 'HasKey' '(' axiomAnnotations ClassExpression
    //   '(' { ObjectPropertyExpression } ')'
    //   '(' { DataPropertyExpression } ')' ')'
    HasKey: $ =>
      seq(
        'HasKey',
        '(',
        repeat($.Annotation),
        $.ClassExpression,
        '(',
        repeat($.ObjectPropertyExpression),
        ')',
        '(',
        repeat($.DataPropertyExpression),
        ')',
        ')',
      ),

    // ---- 9.6  Assertions --------------------------------------------

    // Assertion :=
    //   SameIndividual | DifferentIndividuals | ClassAssertion |
    //   ObjectPropertyAssertion | NegativeObjectPropertyAssertion |
    //   DataPropertyAssertion | NegativeDataPropertyAssertion
    Assertion: $ =>
      choice(
        $.SameIndividual,
        $.DifferentIndividuals,
        $.ClassAssertion,
        $.ObjectPropertyAssertion,
        $.NegativeObjectPropertyAssertion,
        $.DataPropertyAssertion,
        $.NegativeDataPropertyAssertion,
      ),

    // sourceIndividual := Individual
    sourceIndividual: $ => $.Individual,
    // targetIndividual := Individual
    targetIndividual: $ => $.Individual,
    // targetValue := Literal
    targetValue: $ => $.Literal,

    // SameIndividual := 'SameIndividual' '(' axiomAnnotations
    //   Individual Individual { Individual } ')'
    SameIndividual: $ =>
      seq(
        'SameIndividual',
        '(',
        repeat($.Annotation),
        $.Individual,
        $.Individual,
        repeat($.Individual),
        ')',
      ),

    // DifferentIndividuals := 'DifferentIndividuals' '(' axiomAnnotations
    //   Individual Individual { Individual } ')'
    DifferentIndividuals: $ =>
      seq(
        'DifferentIndividuals',
        '(',
        repeat($.Annotation),
        $.Individual,
        $.Individual,
        repeat($.Individual),
        ')',
      ),

    // ClassAssertion := 'ClassAssertion' '(' axiomAnnotations
    //   ClassExpression Individual ')'
    ClassAssertion: $ =>
      seq(
        'ClassAssertion',
        '(',
        repeat($.Annotation),
        $.ClassExpression,
        $.Individual,
        ')',
      ),

    // ObjectPropertyAssertion := 'ObjectPropertyAssertion' '(' axiomAnnotations
    //   ObjectPropertyExpression sourceIndividual targetIndividual ')'
    ObjectPropertyAssertion: $ =>
      seq(
        'ObjectPropertyAssertion',
        '(',
        repeat($.Annotation),
        $.ObjectPropertyExpression,
        alias($.Individual, $.sourceIndividual),
        alias($.Individual, $.targetIndividual),
        ')',
      ),

    // NegativeObjectPropertyAssertion := 'NegativeObjectPropertyAssertion' '('
    //   axiomAnnotations ObjectPropertyExpression sourceIndividual
    //   targetIndividual ')'
    NegativeObjectPropertyAssertion: $ =>
      seq(
        'NegativeObjectPropertyAssertion',
        '(',
        repeat($.Annotation),
        $.ObjectPropertyExpression,
        alias($.Individual, $.sourceIndividual),
        alias($.Individual, $.targetIndividual),
        ')',
      ),

    // DataPropertyAssertion := 'DataPropertyAssertion' '(' axiomAnnotations
    //   DataPropertyExpression sourceIndividual targetValue ')'
    DataPropertyAssertion: $ =>
      seq(
        'DataPropertyAssertion',
        '(',
        repeat($.Annotation),
        $.DataPropertyExpression,
        alias($.Individual, $.sourceIndividual),
        alias($.Literal, $.targetValue),
        ')',
      ),

    // NegativeDataPropertyAssertion := 'NegativeDataPropertyAssertion' '('
    //   axiomAnnotations DataPropertyExpression sourceIndividual targetValue ')'
    NegativeDataPropertyAssertion: $ =>
      seq(
        'NegativeDataPropertyAssertion',
        '(',
        repeat($.Annotation),
        $.DataPropertyExpression,
        alias($.Individual, $.sourceIndividual),
        alias($.Literal, $.targetValue),
        ')',
      ),
  },
})

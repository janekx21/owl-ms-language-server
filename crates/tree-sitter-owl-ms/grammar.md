source: https://www.w3.org/TR/owl2-manchester-syntax/

## 2 The Grammar

| Construct            | Syntax          | Example                  |
| -------------------- | --------------- | ------------------------ |
| non-terminal symbols | boldface        | ClassExpression          |
| terminal symbols     | single quoted   | 'PropertyRange'          |
| zero or more         | curly braces    | { ClassExpression }      |
| zero or one          | square brackets | [ ClassExpression ]      |
| alternative          | vertical bar    | Assertion \| Declaration |
| grouping             | parentheses     | dataPropertyExpression ) |

```
<NT>List ::= <NT> { ',' <NT> }
<NT>2List ::= <NT> ',' <NT>List
<NT>AnnotatedList ::= [annotations] <NT> { ',' [annotations] <NT> }
```

The infix notation for descriptions is ambiguous as stated. This ambiguity is resolved in the usual way, with later productions in the description grammar below binding more tightly, so, for example, `p some a and p only b` is parsed as `(p some a) and (p only b)`.

## 2.1 IRIs, Integers, Literals, and Entities

```
fullIRI := an IRI as defined in [RFC 3987], enclosed in a pair of < (U+3C) and > (U+3E) characters
prefixName := a finite sequence of characters matching the PNAME_NS production of [SPARQL] and not matching any of the keyword terminals of the syntax
abbreviatedIRI := a finite sequence of characters matching the PNAME_LN production of [SPARQL]
simpleIRI := a finite sequence of characters matching the PN_LOCAL production of [SPARQL] and not matching any of the keyword terminals of the syntax
IRI := fullIRI | abbreviatedIRI | simpleIRI

nonNegativeInteger ::= zero | positiveInteger
positiveInteger ::= nonZero { digit }
digits ::= digit { digit }
digit ::= zero | nonZero
nonZero := '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
zero ::= '0'

classIRI ::= IRI
Datatype ::= datatypeIRI | 'integer' | 'decimal' | 'float' | 'string'
datatypeIRI ::= IRI
objectPropertyIRI ::= IRI
dataPropertyIRI ::= IRI
annotationPropertyIRI ::= IRI
individual ::= individualIRI | nodeID
individualIRI ::= IRI
nodeID := a finite sequence of characters matching the BLANK_NODE_LABEL production of [SPARQL]

literal ::= typedLiteral | stringLiteralNoLanguage | stringLiteralWithLanguage | integerLiteral | decimalLiteral | floatingPointLiteral
typedLiteral ::= lexicalValue '^^' Datatype
stringLiteralNoLanguage ::= quotedString
stringLiteralWithLanguage ::= quotedString languageTag
languageTag := @ (U+40) followed a nonempty sequence of characters matching the langtag production from [BCP 47]
lexicalValue ::= quotedString
quotedString := a finite sequence of characters in which " (U+22) and \ (U+5C) occur only in pairs of the form \" (U+5C, U+22) and \\ (U+5C, U+5C), enclosed in a pair of " (U+22) characters
floatingPointLiteral ::= [ '+' | '-'] ( digits ['.'digits] [exponent] | '.' digits[exponent]) ( 'f' | 'F' )
exponent ::= ('e' | 'E') ['+' | '-'] digits
decimalLiteral ::= ['+' | '-'] digits '.' digits
integerLiteral ::= ['+' | '-'] digits

entity ::= 'Datatype' '(' Datatype ')' | 'Class' '(' classIRI ')' | 'ObjectProperty' '(' objectPropertyIRI ')' | 'DataProperty' '('dataPropertyIRI ')' | 'AnnotationProperty' '(' annotationPropertyIRI ')' | 'NamedIndividual' '(' individualIRI ')'
```

## 2.2 Ontologies and Annotations

```
annotations ::= 'Annotations:' annotationAnnotatedList
annotation ::= annotationPropertyIRI annotationTarget
annotationTarget ::= nodeID | IRI | literal

ontologyDocument ::=  { prefixDeclaration } ontology
prefixDeclaration ::= 'Prefix:' prefixName fullIRI
ontology ::= 'Ontology:' [ ontologyIRI [ versionIRI ] ] { import } { annotations } { frame }
ontologyIRI ::= IRI
versionIRI ::= IRI
import ::= 'Import:' IRI
frame ::= datatypeFrame | classFrame | objectPropertyFrame | dataPropertyFrame | annotationPropertyFrame | individualFrame | misc
```

## 2.3 Property and Datatype Expressions

```
objectPropertyExpression ::= objectPropertyIRI | inverseObjectProperty
inverseObjectProperty ::= 'inverse' objectPropertyIRI
dataPropertyExpression ::= dataPropertyIRI

dataRange ::= dataConjunction 'or' dataConjunction { 'or' dataConjunction }
         | dataConjunction
dataConjunction ::= dataPrimary 'and' dataPrimary { 'and' dataPrimary }
         | dataPrimary
dataPrimary ::= [ 'not' ] dataAtomic
dataAtomic ::= Datatype
         | '{' literalList '}'
         | datatypeRestriction | '(' dataRange ')'
datatypeRestriction ::= Datatype '[' facet restrictionValue { ',' facet restrictionValue } ']'
facet ::= 'length' | 'minLength' | 'maxLength' | 'pattern' | 'langRange' | '<=' | '<' | '>=' | '>'
restrictionValue ::= literal
```

## 2.4 Descriptions

```
description ::= conjunction 'or' conjunction { 'or' conjunction }
         | conjunction
conjunction ::= classIRI 'that' [ 'not' ] restriction { 'and' [ 'not' ] restriction }
         | primary 'and' primary { 'and' primary }
         | primary
primary ::= [ 'not' ] ( restriction | atomic )
restriction ::= objectPropertyExpression 'some' primary
         | objectPropertyExpression 'only' primary
         | objectPropertyExpression 'value' individual
         | objectPropertyExpression 'Self'
         | objectPropertyExpression 'min' nonNegativeInteger [ primary ]
         | objectPropertyExpression 'max' nonNegativeInteger [ primary ]
         | objectPropertyExpression 'exactly' nonNegativeInteger [ primary ]
         | dataPropertyExpression 'some' dataPrimary
         | dataPropertyExpression 'only' dataPrimary
         | dataPropertyExpression 'value' literal
         | dataPropertyExpression 'min' nonNegativeInteger [ dataPrimary ]
         | dataPropertyExpression 'max' nonNegativeInteger [ dataPrimary ]
         | dataPropertyExpression 'exactly' nonNegativeInteger [ dataPrimary ]
atomic ::= classIRI
         | '{' individualList '}'
         | '(' description ')'
```

## 2.5 Frames and Miscellaneous

```
datatypeFrame ::= 'Datatype:' Datatype
       { 'Annotations:'     annotationAnnotatedList }
       [ 'EquivalentTo:' annotations dataRange ]
       { 'Annotations:'     annotationAnnotatedList }

classFrame ::= 'Class:' classIRI
       { 'Annotations:'     annotationAnnotatedList
       | 'SubClassOf:'      descriptionAnnotatedList
       | 'EquivalentTo:'    descriptionAnnotatedList
       | 'DisjointWith:'    descriptionAnnotatedList
       | 'DisjointUnionOf:' annotations description2List }
       | 'HasKey:' annotations ( objectPropertyExpression | dataPropertyExpression )
                                        { objectPropertyExpression | dataPropertyExpression }


objectPropertyFrame ::= 'ObjectProperty:' objectPropertyIRI
       { 'Annotations:'     annotationAnnotatedList
       | 'Domain:'          descriptionAnnotatedList
       | 'Range:'           descriptionAnnotatedList
       | 'Characteristics:' objectPropertyCharacteristicAnnotatedList
       | 'SubPropertyOf:'   objectPropertyExpressionAnnotatedList
       | 'EquivalentTo:'    objectPropertyExpressionAnnotatedList
       | 'DisjointWith:'    objectPropertyExpressionAnnotatedList
       | 'InverseOf:'        objectPropertyExpressionAnnotatedList
       | 'SubPropertyChain:' annotations objectPropertyExpression 'o' objectPropertyExpression
                                                { 'o' objectPropertyExpression } }

objectPropertyCharacteristic ::= 'Functional' | 'InverseFunctional'
             | 'Reflexive' | 'Irreflexive' | 'Symmetric' | 'Asymmetric' | 'Transitive'

dataPropertyFrame ::= 'DataProperty:' dataPropertyIRI
       { 'Annotations:'     annotationAnnotatedList
       | 'Domain:'          descriptionAnnotatedList
       | 'Range:'           dataRangeAnnotatedList
       | 'Characteristics:' annotations 'Functional'
       | 'SubPropertyOf:'   dataPropertyExpressionAnnotatedList
       | 'EquivalentTo:'    dataPropertyExpressionAnnotatedList
       | 'DisjointWith:'    dataPropertyExpressionAnnotatedList }

annotationPropertyFrame ::= 'AnnotationProperty:' annotationPropertyIRI
       { 'Annotations:'     annotationAnnotatedList }
       | 'Domain:'          IRIAnnotatedList
       | 'Range:'           IRIAnnotatedList
       | 'SubPropertyOf:'   annotationPropertyIRIAnnotatedList

individualFrame ::= 'Individual:' individual
       { 'Annotations:'   annotationAnnotatedList
       | 'Types:'         descriptionAnnotatedList
       | 'Facts:'         factAnnotatedList
       | 'SameAs:'        individualAnnotatedList
       | 'DifferentFrom:' individualAnnotatedList }

fact ::= [ 'not' ] (objectPropertyFact | dataPropertyFact)
objectPropertyFact ::= objectPropertyIRI individual
dataPropertyFact ::= dataPropertyIRI literal

misc ::= 'EquivalentClasses:' annotations description2List
       | 'DisjointClasses:' annotations description2List
       | 'EquivalentProperties:' annotations objectProperty2List
       | 'DisjointProperties:' annotations objectProperty2List
       | 'EquivalentProperties:' annotations dataProperty2List
       | 'DisjointProperties:' annotations dataProperty2List
       | 'SameIndividual:' annotations individual2List
       | 'DifferentIndividuals:' annotations individual2List
```

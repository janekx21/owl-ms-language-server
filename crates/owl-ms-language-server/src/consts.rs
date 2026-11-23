use crate::{
    iri::Iri,
    iri::ToIriUnchecked,
    workspace::{Annotation, FrameInfo, FrameType},
};
use indoc::indoc;
use language::Language;

// TODO these constants should be of type Iri, because the amount of into() calls at runtime can be ommited and maybe even a faster eq comparison
// Build in xsd datatypes
pub const STRING_IRI: &str = "http://www.w3.org/2001/XMLSchema#string";
pub const INTEGER_IRI: &str = "http://www.w3.org/2001/XMLSchema#integer";
pub const DECIMAL_IRI: &str = "http://www.w3.org/2001/XMLSchema#decimal";
pub const FLOAT_IRI: &str = "http://www.w3.org/2001/XMLSchema#float";

pub const LABEL_IRI: &str = "http://www.w3.org/2000/01/rdf-schema#label";
pub const COMMENT_IRI: &str = "http://www.w3.org/2000/01/rdf-schema#comment";

macro_rules! fixed_infos {
    (
        $(
            $iri:expr => {
                frame_type: $frame_type:expr,
                annotations: [
                    $( $ann_iri:expr => $string_value:expr, $language:expr $(, datatype: $datatype:expr)? );* $(;)?
                ]
            }
        ),* $(,)?
    ) => {
        pub fn get_fixed_infos(iri: &Iri) -> Vec<FrameInfo> {
            match iri.as_str() {
                $(
                    $iri => vec![FrameInfo {
                        iri: $iri.to_iri_unchecked(),
                        annotations: vec![
                            $(
                                Annotation {
                                    frame_iri: $iri.to_iri_unchecked(),
                                    iri: $ann_iri.to_iri_unchecked(),
                                    string_value: $string_value.to_string(),
                                    language: Some($language),
                                    datatype: fixed_infos!(@datatype $($datatype)?),
                                }
                            ),*
                        ]
                        .into_iter()
                        .collect(),
                        frame_type: $frame_type,
                        definitions: vec![],
                    }],
                )*
                _ => vec![],
            }
        }
    };

    (@datatype) => { STRING_IRI.to_iri_unchecked() };
    (@datatype $datatype:expr) => { $datatype };
}

fixed_infos! {
    "http://www.w3.org/2000/01/rdf-schema#label" => {
        frame_type: FrameType::AnnotationProperty,
        annotations: [
            LABEL_IRI => "label", Language::En;
        ]
    },
    "http://www.w3.org/2000/01/rdf-schema#comment" => {
        frame_type: FrameType::AnnotationProperty,
        annotations: [
            LABEL_IRI => "comment", Language::En;
        ]
    },

    "http://www.w3.org/2001/XMLSchema#string" =>{
    frame_type: FrameType::DataType,
        annotations:
            [COMMENT_IRI => "Primitive datatype: any finite-length sequence of characters.", Language::En]},

    "http://www.w3.org/2001/XMLSchema#normalizedString" =>{
    frame_type: FrameType::DataType,
        annotations:
            [COMMENT_IRI => "Represents white space normalized strings and do not contain carriage return (#xD), line feed (#xA) nor tab (#x9) characters.", Language::En]},

    "http://www.w3.org/2001/XMLSchema#token" =>{
    frame_type: FrameType::DataType,
        annotations:
            [COMMENT_IRI => "Represents tokenized strings and do not contain carriage return (#xD), line feed (#xA), tab (#x9), leading/trailing whitespace and internal whitespace runs are collapsed to a single space.", Language::En]},

    "http://www.w3.org/2001/XMLSchema#language" =>{
    frame_type: FrameType::DataType,
        annotations:
            [COMMENT_IRI => "Natural language identifiers as defined by RFC 1766 (e.g. 'en', 'en-GB').", Language::En]},

    "http://www.w3.org/2001/XMLSchema#NMTOKEN" =>{
    frame_type: FrameType::DataType,
        annotations:
            [COMMENT_IRI => "An XML 'Nmtoken'. One or more XML name characters.", Language::En]},

    "http://www.w3.org/2001/XMLSchema#Name" =>{
    frame_type: FrameType::DataType,
        annotations:
            [COMMENT_IRI => "An XML 'Name'. Starts with a name-start character, followed by name characters.", Language::En]},

    "http://www.w3.org/2001/XMLSchema#NCName" =>{
    frame_type: FrameType::DataType,
        annotations:
                [COMMENT_IRI => "A 'non-colonized' name. An XML Name that excludes the colon character, used for unprefixed identifiers.", Language::En]},

    "http://www.w3.org/2001/XMLSchema#boolean" =>{
    frame_type: FrameType::DataType,
        annotations:
            [COMMENT_IRI => "Primitive datatype: the two-value logic space {true, false}, lexically true/false/1/0.", Language::En]},

    "http://www.w3.org/2001/XMLSchema#decimal" =>{
    frame_type: FrameType::DataType,
        annotations:
            [COMMENT_IRI => "Primitive datatype: arbitrary-precision decimal numbers (e.g. -1.23, 12678967.543233, +100000.00, 210).", Language::En]},

    "http://www.w3.org/2001/XMLSchema#float" =>{
    frame_type: FrameType::DataType,
        annotations:
            [COMMENT_IRI => "Primitive datatype: IEEE 754-1985 single-precision 32-bit floating point numbers (e.g. -1E4, 1267.43233E12, 12.78e-2, 12, INF). The special values positive and negative zero, positive and negative infinity and not-a-number have lexical representations 0, -0, INF, -INF and NaN, respectively. ", Language::En]},

    "http://www.w3.org/2001/XMLSchema#double" =>{
    frame_type: FrameType::DataType,
        annotations:
            [COMMENT_IRI => "Primitive datatype: IEEE 754-1985 double-precision 64-bit floating point numbers (e.g. -1E4, 1267.43233E12, 12.78e-2, 12 and INF). The special values positive and negative zero, positive and negative infinity and not-a-number have lexical representations 0, -0, INF, -INF and NaN, respectively. ", Language::En]},

    "http://www.w3.org/2001/XMLSchema#integer" =>{
    frame_type: FrameType::DataType,
        annotations:
            [COMMENT_IRI => "Derived from xsd:decimal: decimal numbers with no fractional part.", Language::En]},

    "http://www.w3.org/2001/XMLSchema#nonPositiveInteger" =>{
    frame_type: FrameType::DataType,
        annotations:
            [COMMENT_IRI => "Derived from xsd:integer: integers less than or equal to zero.", Language::En]},

    "http://www.w3.org/2001/XMLSchema#negativeInteger" =>{
    frame_type: FrameType::DataType,
        annotations:
            [COMMENT_IRI => "Derived from xsd:nonPositiveInteger: integers strictly less than zero.", Language::En]},

    "http://www.w3.org/2001/XMLSchema#long" =>{
    frame_type: FrameType::DataType,
        annotations:
            [COMMENT_IRI => "Derived from xsd:integer: integers representable in 64 signed bits.", Language::En]},

    "http://www.w3.org/2001/XMLSchema#int" =>{
    frame_type: FrameType::DataType,
        annotations:
            [COMMENT_IRI => "Derived from xsd:long: integers representable in 32 signed bits.", Language::En]},

    "http://www.w3.org/2001/XMLSchema#short" =>{
    frame_type: FrameType::DataType,
        annotations:
            [COMMENT_IRI => "Derived from xsd:int: integers representable in 16 signed bits.", Language::En]},

    "http://www.w3.org/2001/XMLSchema#byte" =>{
    frame_type: FrameType::DataType,
        annotations:
            [COMMENT_IRI => "Derived from xsd:short: integers representable in 8 signed bits.", Language::En]},

    "http://www.w3.org/2001/XMLSchema#nonNegativeInteger" =>{
    frame_type: FrameType::DataType,
        annotations:
            [COMMENT_IRI => "Derived from xsd:integer: integers greater than or equal to zero.", Language::En]},

    "http://www.w3.org/2001/XMLSchema#unsignedLong" =>{
    frame_type: FrameType::DataType,
        annotations:
            [COMMENT_IRI => "Derived from xsd:nonNegativeInteger: integers representable in 64 unsigned bits.", Language::En]},

    "http://www.w3.org/2001/XMLSchema#unsignedInt" =>{
    frame_type: FrameType::DataType,
        annotations:
            [COMMENT_IRI => "Derived from xsd:unsignedLong: integers representable in 32 unsigned bits.", Language::En]},

    "http://www.w3.org/2001/XMLSchema#unsignedShort" =>{
    frame_type: FrameType::DataType,
        annotations:
            [COMMENT_IRI => "Derived from xsd:unsignedInt: integers representable in 16 unsigned bits.", Language::En]},

    "http://www.w3.org/2001/XMLSchema#unsignedByte" =>{
    frame_type: FrameType::DataType,
        annotations:
            [COMMENT_IRI => "Derived from xsd:unsignedShort: integers representable in 8 unsigned bits.", Language::En]},

    "http://www.w3.org/2001/XMLSchema#positiveInteger" =>{
    frame_type: FrameType::DataType,
        annotations:
            [COMMENT_IRI => "Derived from xsd:nonNegativeInteger: integers strictly greater than zero.", Language::En]},

    "http://www.w3.org/2001/XMLSchema#dateTime" =>{
    frame_type: FrameType::DataType,
        annotations:
            [COMMENT_IRI => "Primitive datatype: a specific instant combining date and time-of-day with ISO 8601 extended format CCYY-MM-DDThh:mm:ss (e.g. 2026-08-31T14:30:00, 1999-05-31T13:20:00-05:00). May be immediately followed by a Time Zone using Coordinated Universal Time (UTC).", Language::En]},

    "http://www.w3.org/2001/XMLSchema#time" =>{
    frame_type: FrameType::DataType,
        annotations:
            [COMMENT_IRI => "Primitive datatype: an instant of time that recurs every day (e.g. 14:30:00, 13:20:00-05:00). May be immediately followed by a Time Zone using Coordinated Universal Time (UTC).", Language::En]},

    "http://www.w3.org/2001/XMLSchema#date" =>{
    frame_type: FrameType::DataType,
        annotations:
            [COMMENT_IRI => "Primitive datatype: a calendar date CCYY-MM-DD (e.g. 2026-08-31).", Language::En]},

    "http://www.w3.org/2001/XMLSchema#gYearMonth" =>{
    frame_type: FrameType::DataType,
        annotations:
            [COMMENT_IRI => "Primitive datatype: a specific month of a specific year CCYY-MM (e.g. 2026-08).", Language::En]},

    "http://www.w3.org/2001/XMLSchema#gYear" =>{
    frame_type: FrameType::DataType,
        annotations:
            [COMMENT_IRI => "Primitive datatype: a specific calendar year CCYY (e.g. 2026).", Language::En]},

    "http://www.w3.org/2001/XMLSchema#gMonthDay" =>{
    frame_type: FrameType::DataType,
        annotations:
            [COMMENT_IRI => "Primitive datatype: a recurring day of a specific month --MM-DD (e.g. --08-31).", Language::En]},

    "http://www.w3.org/2001/XMLSchema#gDay" =>{
    frame_type: FrameType::DataType,
        annotations:
            [COMMENT_IRI => "Primitive datatype: a recurring day of the month ---DD (e.g. ---31).", Language::En]},

    "http://www.w3.org/2001/XMLSchema#gMonth" =>{
    frame_type: FrameType::DataType,
        annotations:
            [COMMENT_IRI => "Primitive datatype: a recurring month --MM-- (e.g. --08--).", Language::En]},

    "http://www.w3.org/2001/XMLSchema#hexBinary" =>{
    frame_type: FrameType::DataType,
        annotations:
            [COMMENT_IRI => "Primitive datatype: arbitrary binary data encoded as hexadecimal digits [0-9a-fA-F] (e.g. 0FB7, fa02).", Language::En]},

    "http://www.w3.org/2001/XMLSchema#base64Binary" =>{
    frame_type: FrameType::DataType,
        annotations:
            [COMMENT_IRI => "Primitive datatype: Base64-encoded arbitrary binary data encoded per RFC 2045.", Language::En]},

    "http://www.w3.org/2001/XMLSchema#anyURI" =>{
    frame_type: FrameType::DataType,
        annotations:
            [COMMENT_IRI => "Primitive datatype: a Uniform Resource Identifier Reference (URI) or Internationalized Resource Identifier (IRI), per RFC 2396 amended by RFC 2732. Can be absolute or relative, and may have an optional fragment identifier.", Language::En]},

}

#[allow(clippy::too_many_lines)]
pub fn omn_keyword_hover_info(kind: &str) -> String {
    match kind{
            "keyword_prefix" => indoc! {"
                `Prefix:`

                *Top-level keyword*

                ---

                Declare a namespace prefix for use in abbriviated IRIs.

                A prefix always comes before the ontology. Abbreviated IRIs without a prefix part are treated as if they have the empty prefix `:`.

                Example:
                ```owl-ms
                Prefix: rdfs: <http://www.w3.org/2000/01/rdf-schema#>
                Prefix: : <http://example.com/ontology/>

                Ontology: ...
                ```

                [Specification](https://www.w3.org/TR/owl2-manchester-syntax/#IRIs.2C_Integers.2C_Literals.2C_and_Entities)
            "}.to_string(),
            "keyword_ontology" => indoc! {"
               `Ontology:`

               *Top-level keyword* 

                ---

                Defines an ontology with its IRI and optional version IRI.

                
                OWL 2 provides several built-in annotation properties for ontology annotations. The usage of these annotation properties on entities other than ontologies is discouraged.

                - The `owl:priorVersion` annotation property specifies the IRI of a prior version of the containing ontology.
                - The `owl:backwardCompatibleWith` annotation property specifies the IRI of a prior version of the containing ontology that is compatible with the current version of the containing ontology.
                - The `owl:incompatibleWith` annotation property specifies the IRI of a prior version of the containing ontology that is incompatible with the current version of the containing ontology.

                Example:
                ```owl-ms
                Ontology: <http://example.com/ontology/physical/> <http://example.com/ontology/v1.2.3/physical.omn>
                ```

                [Specification](https://www.w3.org/TR/owl2-syntax/#Ontologies)
            "}.to_string(),
            "keyword_integer" => indoc!{"
                `integer`

                *Facet* 

                ---
                *Built-in alias for* `xsd:integer`. A datatype for integer values.

                [Specification](https://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/#integer)"
                }.to_string(),
            "keyword_decimal" => indoc!{"
                *Facet* `decimal`

                ---
                *Built-in alias for* `xsd:decimal`. A datatype for decimal numbers with arbitrary precision.

                [Specification](https://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/#decimal)"
            }.to_string(),
            "keyword_float" => indoc!{"
                `float`
                
                *Facet* 

                ---
                *Built-in alias for* `xsd:float`. A datatype for 32-bit floating-point numbers.

                [Specification](https://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/#float)"
            }.to_string(),
            "keyword_string" => indoc!{"
                `string`

                *Facet* 

                ---
                *Built-in alias for* `xsd:string`. A datatype for character strings.

                [Specification](https://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/#string)"
            }.to_string(),
            "keyword_import" => indoc!{"
                `Import:`

                *Top-level keyword* 

                ---
                
                Imports another ontology into the current ontology.

                Import other ontologies in order to gain access to their entities, expressions, and axioms. This provides the basic facility for ontology modularization. The IRIs of the imported ontology do not automaticly join the default namespace.

                Example:
                ```owl-ms
                Ontology: <http://www.example.com/importing-ontology>

                    Import: <http://www.example.com/my/2.0>
                ```

                [Specification](https://www.w3.org/TR/owl2-syntax/#Imports)
            "}.to_string(),
            "keyword_annotations" => indoc!{"
                `Annotations:`

                *Keyword* 

                ---

                Defines annotation assertions for an entity.

                Ontologies, axioms, and annotations themselves can be annotated using annotations. They consist of an annotation property and an annotation value, where the latter can be anonymous individuals, IRIs, and literals.

                
                The annotation properties with the IRIs listed below are available in OWL 2 as built-in annotation properties with a predefined semantics:
                - The `rdfs:label` annotation property can be used to provide an IRI with a human-readable label.
                - The `rdfs:comment` annotation property can be used to provide an IRI with a human-readable comment.
                - The `rdfs:seeAlso` annotation property can be used to provide an IRI with another IRI such that the latter provides additional information about the former.
                - The `rdfs:isDefinedBy` annotation property can be used to provide an IRI with another IRI such that the latter provides information about the definition of the former; the way in which this information is provided is not described by this specification.
                - An annotation with the `owl:deprecated` annotation property and the value equal to `\"true\"^^xsd:boolean` can be used to specify that an IRI is deprecated.
                - The `owl:versionInfo` annotation property can be used to provide an IRI with a string that describes the IRI's version.
                - The `owl:priorVersion` annotation property is described in more detail in Section 3.5.
                - The `owl:backwardCompatibleWith` annotation property is described in more detail in Section 3.5.
                - The `owl:incompatibleWith` annotation property is described in more detail in Section 3.5.

                Example:
                ```owl-ms
                Class: OEO0042

                    Annotations: 
                        <http://example.com/o/CH_0000118> \"CO2\"@de,
                        Annotations: rdfs:comment \"this is an annotated annotation\"
                            rdfs:label \"greenhouse gas\",
                        rdfs:label \"Treibhausgas\"@de
                ```

                [Specification](https://www.w3.org/TR/owl2-syntax/#Annotations)
            "}.to_string(),
            "keyword_inverse" => indoc!{"
                `inverse`

                *Object property expression* 

                ---

                An inverse property expression connects an individual i with j if and only if j is connected to i by the current object property.

                [Specification](https://www.w3.org/TR/2012/REC-owl2-syntax-20121211/#Inverse_Object_Properties)"
            }.to_string(),
            "keyword_length" => indoc!{"
                `length`

                *Facet* 

                ---

                Built-in alias for `xsd:length`. Constrains the exact length of a datatype value. Must be a non-negative integer.
            
                [Specification](https://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/#rf-length)"}.to_string(),
            "keyword_min_length" => indoc!{"
                `minLength`

                *Facet* 

                ---

                Built-in alias for `xsd:minLength`. Constrains the minimum length of a datatype value. Must be a non-negative integer.

                [Specification](https://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/#rf-minLength)"}.to_string(),
            "keyword_max_length" => indoc!{"
                `maxLength`

                *Facet* 

                ---

                Built-in alias for `xsd:maxLength`. Constrains the maximum length of a datatype value. Must be a non-negative integer.

                [Specification](https://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/#rf-maxLength)"}.to_string(),
            "keyword_pattern" => indoc!{"
                `pattern`

                *Facet* 

                ---

                Built-in alias for `xsd:pattern`. Constrains datatype values to match a regular expression pattern.
                
                [Specification](https://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/#rf-pattern)
            "
            }.to_string(),
            "keyword_lang_range" => indoc!{"
                `langRange`

                *Facet* 

                ---

                Built-in alias for rdf:langRange. Constrains literal values to specific language ranges"
            }.to_string(),
            "keyword_some" => indoc!{"
                `some`

                *Class expression* 

                ---

                The existential class expression `ope some ce` describes a class of individuals that are connected by the object property expression `ope` to at least one individual of the class `ce`.
                The existential class expression `dp some dr` describes a class of individuals that are connected by the data property `dp` to at least one literal in `dr`.

                Example:
                ```owl-ms
                Class: Parent
                    SubClassOf: hasChild some Person
                
                Class: Adult
                    SubClassOf: hasAge some integer[>= 18]
                ```

                Specifications:
                - [Existential Quantification over Object Properties](https://www.w3.org/TR/owl2-syntax/#Existential_Quantification)
                - [Existential Quantification over Data Properties](https://www.w3.org/TR/owl2-syntax/#Existential_Quantification_2)

            "}.to_string(),
            "keyword_only" => indoc!{"
                `only`

                *Class expression* 

                ---

                The universal class expression `ope only ce` describes a class of individuals that are connected by the object property expression `ope` only to individuals of the class `ce`.
                The universal class expression `dp only dr` describes a class of individuals that are connected by the data property `dp` only to literals in `dr`.

                Example:
                ```owl-ms
                Class: VegetarianPizza
                    SubClassOf: hasTopping only (VegetableTopping or CheeseTopping)

                Class: Adult
                    SubClassOf: hasAge only integer
                ```

                Specifications:
                - [Universal Quantification over Object Properties](https://www.w3.org/TR/owl2-syntax/#Universal_Quantification)
                - [Universal Quantification over Data Properties](https://www.w3.org/TR/owl2-syntax/#Universal_Quantification_2)
            "}.to_string(),
            "keyword_self" => indoc! {"
                `Self`

                *Class expression* 

                ---

                The self-restriction `ope self` describes a class of individuals that are connected by the object property expression `ope` to themselves.

                Example:
                ```owl-ms
                Class: Person
                    SubClassOf: likes Self
                ```

                [Specification](https://www.w3.org/TR/owl2-syntax/#Self-Restriction)
            "}.to_string(),
            "keyword_value" => indoc!{"
                `value`

                *Class expression* 

                ---

                The has-value class expression `ope value i` describes a class of individuals that are connected by the object property expression `ope` to the individual `i`.
                The has-value class expression `dp value l` describes a class of individuals that are connected by the data property `dp` to the literal `l`.

                Example:
                ```owl-ms
                Class: Alices
                    SubClassOf: hasName value \"Alice\"@en
                Class: InitProcess
                    SubClassOf: hasPID value 1
                ```

                Specifications:
                - [Individual Value Restriction](https://www.w3.org/TR/owl2-syntax/#Individual_Value_Restriction)
                - [Literal Value Restriction](https://www.w3.org/TR/owl2-syntax/#Literal_Value_Restriction)
            "}.to_string(),

            "keyword_datatype" => indoc!{"
                `Datatype:`

                *Frame*

                ---
                
                Declares a custom datatype.
                    
                Example:
                ```owl-ms
            	Datatype: NegInt
            		EquivalentTo:
            			integer[< 0]
                ```

                [Specification](https://www.w3.org/TR/owl2-syntax/#Datatypes)
            "}.to_string(),
            "keyword_equivalent_to" => indoc!{"
                `EquivalentTo:`

                *Axiom* 

                ---

                Declares that the current frame entity is logically equivalent to another.

                Example:
                ```owl-ms
                Class: Child
                    EquivalentTo: Person and hasAge some integer[< 18]
                Datatype: PositiveInteger
                    EquivalentTo: integer[>= 0]
                ObjectProperty: hasParent
                    EquivalentTo: isParentOf inverse isChildOf
                DataProperty: hasAge
                    EquivalentTo: isAgeOf
                ```

                Specifications:
                - [Equivalent Classes](https://www.w3.org/TR/owl2-syntax/#Equivalent_Classes)
                - [Equivalent Datatypes](https://www.w3.org/TR/owl2-syntax/#Datatype_Definitions)
                - [Equivalent Object Properties](https://www.w3.org/TR/owl2-syntax/#Equivalent_Object_Properties)
                - [Equivalent Data Properties](https://www.w3.org/TR/owl2-syntax/#Equivalent_Data_Properties)
            "}.to_string(),
            "keyword_class" => indoc!{"
                `Class:`

                *Frame*

                ---

                Declares a class in the ontology.

                Classes can be understood as sets of individuals.

                For example, classes `Child` and `Person` can be used to represent the set of all children and persons, respectively, in the application domain.
                ```owl-ms
                Class: Person
            	Class: Child
            	    SubClassOf: Person
                ```

                [Specification](https://www.w3.org/TR/owl2-syntax/#Classes)
            "}.to_string(),
            "keyword_sub_class_of" => indoc!{"
                `SubClassOf:`

                *Axiom*

                ---

                States that the current class is a subclass of all listed classes.

                Example:
                ```owl-ms
                Class: Child
                    SubClassOf: Person
                ```

                [Specification](https://www.w3.org/TR/owl2-syntax/#Subclass_Axioms)
            "}.to_string(),
            "keyword_disjoint_with" => indoc!{"
                `DisjointWith:`

                *Axiom*

                ---

                States that the current class is disjoint with the listed classes.

                Example:
                ```owl-ms
                Class: Child
                    DisjointWith: Adult
                ```

                [Specification](https://www.w3.org/TR/owl2-syntax/#Disjoint_Classes)
            "}.to_string(),
            "keyword_disjoint_union_of" => indoc! {"
                `DisjointUnionOf:`

                *Axiom*

                ---

                States a class as the disjoint union of the listed classes.

                Example:
                ```owl-ms
                Class: Party
                    DisjointUnionOf: Person, PrivateCompany, PublicOrganization
                ```

                [Specification](https://www.w3.org/TR/owl2-syntax/#Disjoint_Union_of_Class_Expressions)
            "}.to_string(),
            "keyword_has_key" => indoc!{"
                `HasKey:`

                *Axiom*

                ---

                States that each individual in the current class has a combination of values for the listed properties that uniquely identifies them.

                Example:
                ```owl-ms
                Class: Person
                    HasKey: hasSocialSecurityNumber
                ```

                [Specification](https://www.w3.org/TR/owl2-syntax/#Keys)
            "}.to_string(),
            "keyword_object_property" => indoc!{"
                `ObjectProperty:`

                *Frame*

                ---

                Declares a property that connects individuals to individuals.

                Example:
                ```owl-ms
                ObjectProperty: HasEnergy
                    Domain: 
                        ArtificialObject
                    Range: 
                        Energy
                    InverseOf: 
                        IsEnergyParticipantOf
                ```

                [Specification](https://www.w3.org/TR/owl2-syntax/#Object_Properties)
            "}.to_string(),
            "keyword_domain" => indoc!{"
                `Domain:`

                *Axiom*

                ---

                States that the range of the current object property is the specified class expression.
                States that the range of the current data property is the specified datarange.

                Example:
                ```owl-ms
                ObjectProperty: hasParent
                    Domain: Person
                DataProperty: hasAge
                    Domain: integer[>= 0]
                ```

                Specifications:
                - [Object Property Domain](https://www.w3.org/TR/owl2-syntax/#Object_Property_Domain)
                - [Data Property Domain](https://www.w3.org/TR/owl2-syntax/#Data_Property_Domain)
            "}.to_string(),
            "keyword_range" => indoc!{"
                `Range:`

                *Axiom*

                ---

                States that the range of the current object property is the specified class expression.
                States that the range of the current data property is the specified datarange.

                Example:
                ```owl-ms
                ObjectProperty: hasParent
                    Range: Person
                DataProperty: hasAge
                    Range: integer[>= 0]
                ```

                Specifications:
                - [Object Property Range](https://www.w3.org/TR/owl2-syntax/#Object_Property_Range)
                - [Data Property Range](https://www.w3.org/TR/owl2-syntax/#Data_Property_Range)
            "}.to_string(),
            "keyword_sub_property_of" => indoc!{"
                `SubPropertyOf:`

                *Axiom*

                ---

                States that the current property is a subproperty of the listed object properties. If p is a subproperty of q, then if an individual x is connected to an individual y by p, then x is also connected to y by q.

                Example:
                ```owl-ms
                ObjectProperty: hasDog
                    SubPropertyOf: hasPet
                DataProperty: hasLastName
                    SubPropertyOf: hasName
                ```

                Specifications:
                - [Object Subproperties](https://www.w3.org/TR/owl2-syntax/#Object_Subproperties)
                - [Data Subproperties](https://www.w3.org/TR/owl2-syntax/#Data_Subproperties)
            "}.to_string(),
            "keyword_inverse_of" => indoc!{"
                `InverseOf:`

                *Axiom*

                ---

                States that the current object property is the inverse of the listed object properties.

                Example:
                ```owl-ms
                ObjectProperty: hasParent
                    InverseOf: isParentOf
                ```

                [Specification](https://www.w3.org/TR/owl2-syntax/#Inverse_Object_Properties)
            "}.to_string(),
            "keyword_sub_property_chain" => indoc!{"
                `SubPropertyChainOf:`

                *Axiom*

                ---

                States that, if an individual x is connected by a sequence of the listed object property expressions to an individual y, then x is also connected to y by the current object property.

                Example:
                ```owl-ms
                ObjectProperty: hasGrandParent
                    SubPropertyChainOf: hasParent o hasParent
                ```

                [Specification](https://www.w3.org/TR/owl2-syntax/#Object_Subproperties)
            "}.to_string(),
            "keyword_functional" => indoc!{"
                `Functional`

                *Object property characteristic*

                ---

                States that the current object property is functional, meaning that each individual can be connected to at most one distinct individual by this property.

                Example:
                ```owl-ms
                ObjectProperty: hasFather
                    Characteristic: Functional
                ```

                [Specification](https://www.w3.org/TR/owl2-syntax/#Functional_Object_Properties)
            "}.to_string(),
            "keyword_inverse_functional" => indoc!{"
                `InverseFunctional`

                *Object property characteristic*

                ---

                States that the current object property is inverse functional, meaning that for each individual x there can be at most one individual y such that y is connected to x by this property.

                Example:
                ```owl-ms
                ObjectProperty: fatherOf
                    Characteristic: InverseFunctional
                ```

                [Specification](https://www.w3.org/TR/owl2-syntax/#Inverse-Functional_Object_Properties)
            "}.to_string(),
            "keyword_reflexive" => indoc!{"
                `Reflexive`

                *Object property characteristic*

                ---

                States that the current object property connects each individual to itself.

                Example:
                ```owl-ms
                ObjectProperty: knows
                    Characteristic: Reflexive
                ```

                [Specification](https://www.w3.org/TR/owl2-syntax/#Reflexive_Object_Properties)
            "}.to_string(),
            "keyword_irreflexive" => indoc!{"
                `Irreflexive`

                *Object property characteristic*

                ---

                States that the current object property does not connect any individual to itself.

                Example:
                ```owl-ms
                ObjectProperty: marriedTo
                    Characteristic: Irreflexive
                ```

                [Specification](https://www.w3.org/TR/owl2-syntax/#Irreflexive_Object_Properties)
            "}.to_string(),
            "keyword_symmetric" => indoc!{"
                `Symmetric`

                *Object property characteristic*

                ---

                States that if an individual x is connected to an individual y by the current object property, then y is also connected to x by this property.

                Example:
                ```owl-ms
                ObjectProperty: isSiblingOf
                    Characteristic: Symmetric
                ```

                [Specification](https://www.w3.org/TR/owl2-syntax/#Symmetric_Object_Properties)
            "}.to_string(),
            "keyword_asymmetric" => indoc!{"
                `Asymmetric`

                *Object property characteristic*

                ---

                States that if an individual x is connected to an individual y by the current object property, then y cannot be connected to x by this property.

                Example:
                ```owl-ms
                ObjectProperty: isParentOf
                    Characteristic: Asymmetric
                ```

                [Specification](https://www.w3.org/TR/owl2-syntax/#Asymmetric_Object_Properties)
            "}.to_string(),
            "keyword_transitive" => indoc!{"
                `Transitive`

                *Object property characteristic*

                ---

                States that if an individual x is connected to an individual y by the current object property, and y is connected to an individual z by this property, then x is also connected to z by this property.

                Example:
                ```owl-ms
                ObjectProperty: isAncestorOf
                    Characteristic: Transitive
                ```

                [Specification](https://www.w3.org/TR/owl2-syntax/#Transitive_Object_Properties)
            "}.to_string(),
            "keyword_data_property" => indoc!{"
                `DataProperty:`

                *Frame*

                ---

                Declares a property that connects individuals to literal values.

                Example:
                ```owl-ms
                DataProperty: hasAge
                    Domain: Person
                    Range: xsd:integer
                ```

                [Specification](https://www.w3.org/TR/owl2-syntax/#Data_Properties)
            "}.to_string(),
            "keyword_characteristics" => indoc!{"
                `Characteristics:`

                *Axiom*

                ---

                States the characteristics of the current object property.

                Example:
                ```owl-ms
                ObjectProperty: fatherOf
                    Characteristics: InverseFunctional, Irreflexive
                ```

                [Specification](https://www.w3.org/TR/owl2-syntax/#Object_Property_Axioms)
            "}.to_string(),
            "keyword_annotation_property" => indoc!{"
                `AnnotationProperty:`

                *Frame*

                ---
                
                Declares a property used for annotations (metadata).

                Example:
                ```owl-ms
                AnnotationProperty: rdfs:label
                    Domain: owl:Thing
                    Range: xsd:string
                ```

                [Specification](https://www.w3.org/TR/owl2-syntax/#Annotation_Properties)
            "}.to_string(),
            "keyword_individual" => indoc! {"
                `Individual:`

                *Frame*

                ---

                Declares a named or anonymous individual in the ontology. Individuals which use the blank node (start with `_:`) are considered anonymous. Their identifier is then only available within the current ontology.

                Example:
                ```owl-ms
                Individual: john
                    Types: Person
                    Facts: hasAge 25
                Individual: _:genid1
                    Types: Person
                    Facts: hasAge 30
                ```

                [Specification](https://www.w3.org/TR/owl2-syntax/#Individuals)
            "}.to_string(),
            "keyword_types" => indoc!{"
                `Types:`

                *Axiom*

                ---

                States that the current individual belongs to the listed classes.

                Example:
                ```owl-ms
                Individual: john
                    Types: Person
                ```

                [Specification](https://www.w3.org/TR/owl2-syntax/#Class_Assertions)
            "}.to_string(),
            "keyword_facts" => indoc!{"
                `Facts:`

                *Axiom*

                ---

                States that the current individual has the specified property values.

                Example:
                ```owl-ms
                Individual: john
                    Facts: hasAge 25, hasMother mary
                ```

                [Specification](https://www.w3.org/TR/owl2-syntax/#Positive_Object_Property_Assertions)
            "}.to_string(),
            "keyword_same_as" => indoc!{"
                `SameAs:`

                *Axiom*

                ---

                States that the current individual is the same as the listed individuals.

                Example:
                ```owl-ms
                Individual: john
                    SameAs: john_doe, j_doe
                ```

                [Specification](https://www.w3.org/TR/owl2-syntax/#Individual_Equality)
            "}.to_string(),
            "keyword_equivalent_classes" => indoc!{"
                `EquivalentClasses:`

                *Axiom*

                ---

                States logical equivalence between multiple classes.

                Example:
                ```owl-ms
                EquivalentClasses: Person, People
                ```
                [Specification](https://www.w3.org/TR/owl2-syntax/#Equivalent_Classes)
            "}.to_string(),
            "keyword_disjoint_classes" => indoc!{"
                `DisjointClasses:`

                *Axiom*

                ---

                States that multiple classes are pairwise disjoint.

                Example:
                ```owl-ms
                DisjointClasses: Person, Animal, Plant
                ```

                [Specification](https://www.w3.org/TR/owl2-syntax/#Disjoint_Classes)
            "}.to_string(),
            "keyword_equivalent_properties" => indoc!{"
                `EquivalentProperties:`

                *Axiom*

                ---

                States logical equivalence between multiple properties.

                Example:
                ```owl-ms
                EquivalentProperties: hasBrother, hasMaleSibling
                ```

                Specifications:
                - [Equivalent Object Properties](https://www.w3.org/TR/owl2-syntax/#Equivalent_Object_Properties)
                - [Equivalent Data Properties](https://www.w3.org/TR/owl2-syntax/#Equivalent_Data_Properties)
            "}.to_string(),
            "keyword_disjoint_properties" => indoc!{"
                `DisjointProperties:`

                *Axiom*

                ---

                States that multiple properties are pairwise disjoint.

                Example:
                ```owl-ms
                DisjointProperties: hasFather, hasMother
                ```

                Specifications:
                - [Disjoint Object Properties](https://www.w3.org/TR/owl2-syntax/#Disjoint_Object_Properties)
                - [Disjoint Data Properties](https://www.w3.org/TR/owl2-syntax/#Disjoint_Data_Properties)
            "}.to_string(),
            "keyword_same_individual" => indoc!{"
                `SameIndividual:`

                *Axiom*

                ---

                States that multiple individuals are the same entity.

                Example:
                ```owl-ms
                SameIndividual: john, john_doe
                ```

                [Specification](https://www.w3.org/TR/owl2-syntax/#Individual_Equality)
            "}.to_string(),
            "keyword_different_individuals" => indoc!{"
                `DifferentIndividuals:`

                *Axiom*

                ---

                States that multiple individuals are pairwise distinct entities. This axiom can be used to axiomatize the unique name assumption — the assumption that all different individual names denote different individuals.
            
                Example:
                ```owl-ms
                DifferentIndividuals: john, jane, jack
                ```

                [Specification](https://www.w3.org/TR/owl2-syntax/#Individual_Inequality)
            "}.to_string(),
            "keyword_different_from"=> indoc!{"
                `DifferentFrom:`

                *Axiom*

                ---

                States that an individual is distinct from another specific individual.

                Example:
                ```owl-ms
                Individual: john
                    DifferentFrom: jane, jack
                ```

                [Specification](https://www.w3.org/TR/owl2-syntax/#Individual_Inequality)
            "}.to_string(),
            _ => String::new(),
        }
}

/// Hover documentation for OWL 2 Functional-Style Syntax keywords.
///
/// `kind` is the kind of the anonymous keyword node in the `owl_fn` tree-sitter
/// grammar, i.e. the literal string in the grammar, e.g. `"SubClassOf"`.
#[allow(clippy::too_many_lines)]
pub fn ofn_keyword_hover_info(kind: &str) -> String {
    match kind {
        // ------------------------------------------------------------------
        // Ontology document
        // ------------------------------------------------------------------
        "Prefix" => indoc! {r"
            `Prefix( name=<iri> )`

            *Top-level keyword*

            ---

            Declares a namespace prefix for use in abbreviated IRIs.

            Prefix declarations always come before the `Ontology`. Abbreviated IRIs without a prefix part use the empty prefix `:`.

            Example:
            ```owl-fs
            Prefix(rdfs:=<http://www.w3.org/2000/01/rdf-schema#>)
            Prefix(:=<http://example.com/ontology/>)

            Ontology(...)
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#IRIs)
        "}.to_string(),
        "Ontology" => indoc! {r#"
            `Ontology( [ ontologyIRI [ versionIRI ] ] imports annotations axioms )`

            *Top-level keyword*

            ---

            Defines an ontology with an optional ontology IRI and version IRI, followed by imports, ontology annotations and axioms.

            Example:
            ```owl-fs
            Ontology(<http://example.com/ontology/physical/> <http://example.com/ontology/v1.2.3/physical.ofn>
                Import(<http://example.com/ontology/base/>)
                Annotation(rdfs:label "Physical ontology")

                Declaration(Class(:Person))
            )
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Ontologies)
        "#}.to_string(),
        "Import" => indoc! {r"
            `Import( ontologyIRI )`

            *Ontology element*

            ---

            Imports another ontology into the current ontology, giving access to its entities, expressions and axioms. This is the basic facility for ontology modularization.

            Example:
            ```owl-fs
            Ontology(<http://www.example.com/importing-ontology>
                Import(<http://www.example.com/my/2.0>)
            )
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Imports)
        "}.to_string(),

        // ------------------------------------------------------------------
        // Declarations
        // ------------------------------------------------------------------
        "Declaration" => indoc! {r"
            `Declaration( entity )`

            *Axiom*

            ---

            Declares an entity (class, datatype, property or named individual) as part of the ontology's signature. The entity is given by a wrapped `Class`, `Datatype`, `ObjectProperty`, `DataProperty`, `AnnotationProperty` or `NamedIndividual`.

            Example:
            ```owl-fs
            Declaration(Class(:Person))
            Declaration(ObjectProperty(:hasParent))
            Declaration(NamedIndividual(:john))
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Axioms)
        "}.to_string(),
        "Class" => indoc! {r"
            `Class( iri )`

            *Entity*

            ---

            Refers to a class, usually inside a `Declaration`. Classes can be understood as sets of individuals.

            Example:
            ```owl-fs
            Declaration(Class(:Person))
            Declaration(Class(:Child))
            SubClassOf(:Child :Person)
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Classes)
        "}.to_string(),
        "Datatype" => indoc! {r#"
            `Datatype( iri )`

            *Entity*

            ---

            Refers to a datatype, usually inside a `Declaration`. Custom datatypes are defined with `DatatypeDefinition`.

            Example:
            ```owl-fs
            Declaration(Datatype(:NegInt))
            DatatypeDefinition(:NegInt
                DatatypeRestriction(xsd:integer xsd:maxExclusive "0"^^xsd:integer)
            )
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Datatypes)
        "#}.to_string(),
        "ObjectProperty" => indoc! {r"
            `ObjectProperty( iri )`

            *Entity*

            ---

            Refers to an object property, which connects individuals to individuals.

            Example:
            ```owl-fs
            Declaration(ObjectProperty(:hasEnergy))
            ObjectPropertyDomain(:hasEnergy :ArtificialObject)
            ObjectPropertyRange(:hasEnergy :Energy)
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Object_Properties)
        "}.to_string(),
        "DataProperty" => indoc! {r"
            `DataProperty( iri )`

            *Entity*

            ---

            Refers to a data property, which connects individuals to literal values.

            Example:
            ```owl-fs
            Declaration(DataProperty(:hasAge))
            DataPropertyDomain(:hasAge :Person)
            DataPropertyRange(:hasAge xsd:integer)
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Data_Properties)
        "}.to_string(),
        "AnnotationProperty" => indoc! {r"
            `AnnotationProperty( iri )`

            *Entity*

            ---

            Refers to an annotation property, used for annotations (metadata).

            Example:
            ```owl-fs
            Declaration(AnnotationProperty(:definition))
            AnnotationPropertyDomain(:definition owl:Thing)
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Annotation_Properties)
        "}.to_string(),
        "NamedIndividual" => indoc! {r"
            `NamedIndividual( iri )`

            *Entity*

            ---

            Refers to a named individual, i.e. an individual identified by an IRI. Anonymous individuals are written as blank nodes (`_:genid1`) and are only visible within the current ontology.

            Example:
            ```owl-fs
            Declaration(NamedIndividual(:john))
            ClassAssertion(:Person :john)
            ClassAssertion(:Person _:genid1)
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Individuals)
        "}.to_string(),

        // ------------------------------------------------------------------
        // Annotations
        // ------------------------------------------------------------------
        "Annotation" => indoc! {r#"
            `Annotation( [ annotations ] property value )`

            *Annotation*

            ---

            Annotates an ontology, an axiom, or another annotation. It consists of an annotation property and an annotation value (an IRI, a literal or an anonymous individual). Annotations may themselves be annotated.

            Built-in annotation properties include `rdfs:label`, `rdfs:comment`, `rdfs:seeAlso`, `rdfs:isDefinedBy`, `owl:deprecated`, `owl:versionInfo`, `owl:priorVersion`, `owl:backwardCompatibleWith` and `owl:incompatibleWith`.

            Example:
            ```owl-fs
            Ontology(<http://example.com/o>
                Annotation(rdfs:comment "An ontology annotation")
            )

            SubClassOf(
                Annotation(rdfs:comment "An axiom annotation")
                :Child :Person
            )
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Annotations)
        "#}.to_string(),
        "AnnotationAssertion" => indoc! {r#"
            `AnnotationAssertion( [ annotations ] property subject value )`

            *Annotation axiom*

            ---

            Attaches an annotation to an IRI or an anonymous individual.

            Example:
            ```owl-fs
            AnnotationAssertion(rdfs:label :OEO0042 "greenhouse gas")
            AnnotationAssertion(rdfs:label :OEO0042 "Treibhausgas"@de)
            AnnotationAssertion(
                Annotation(rdfs:comment "this is an annotated annotation")
                rdfs:label :OEO0042 "CO2"
            )
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Annotation_Assertion)
        "#}.to_string(),
        "SubAnnotationPropertyOf" => indoc! {r"
            `SubAnnotationPropertyOf( sub super )`

            *Annotation axiom*

            ---

            States that the first annotation property is a subproperty of the second.

            Example:
            ```owl-fs
            SubAnnotationPropertyOf(:preferredLabel rdfs:label)
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Annotation_Subproperties)
        "}.to_string(),
        "AnnotationPropertyDomain" => indoc! {r"
            `AnnotationPropertyDomain( property iri )`

            *Annotation axiom*

            ---

            States that the domain of an annotation property is the given IRI. Annotation property domains carry no logical semantics.

            Example:
            ```owl-fs
            AnnotationPropertyDomain(:definition :Concept)
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Annotation_Property_Domain)
        "}.to_string(),
        "AnnotationPropertyRange" => indoc! {r"
            `AnnotationPropertyRange( property iri )`

            *Annotation axiom*

            ---

            States that the range of an annotation property is the given IRI. Annotation property ranges carry no logical semantics.

            Example:
            ```owl-fs
            AnnotationPropertyRange(:definition xsd:string)
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Annotation_Property_Range)
        "}.to_string(),

        // ------------------------------------------------------------------
        // Property expressions
        // ------------------------------------------------------------------
        "ObjectInverseOf" => indoc! {r"
            `ObjectInverseOf( op )`

            *Object property expression*

            ---

            The inverse object property expression connects an individual i with j if and only if j is connected to i by `op`.

            Example:
            ```owl-fs
            ObjectSomeValuesFrom(ObjectInverseOf(:hasChild) :Person)
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Inverse_Object_Properties)
        "}.to_string(),

        // ------------------------------------------------------------------
        // Data ranges
        // ------------------------------------------------------------------
        "DataIntersectionOf" => indoc! {r"
            `DataIntersectionOf( dr1 dr2 ... drn )`

            *Data range*

            ---

            Contains exactly the literals that are in all of the listed data ranges. Requires at least two data ranges.

            Example:
            ```owl-fs
            DataIntersectionOf(xsd:nonNegativeInteger xsd:nonPositiveInteger)
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Intersection_of_Data_Ranges)
        "}.to_string(),
        "DataUnionOf" => indoc! {r"
            `DataUnionOf( dr1 dr2 ... drn )`

            *Data range*

            ---

            Contains exactly the literals that are in at least one of the listed data ranges. Requires at least two data ranges.

            Example:
            ```owl-fs
            DataUnionOf(xsd:string xsd:integer)
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Union_of_Data_Ranges)
        "}.to_string(),
        "DataComplementOf" => indoc! {r"
            `DataComplementOf( dr )`

            *Data range*

            ---

            Contains all literals of the data domain that are not in the given data range.

            Example:
            ```owl-fs
            DataComplementOf(xsd:positiveInteger)
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Complement_of_Data_Ranges)
        "}.to_string(),
        "DataOneOf" => indoc! {r#"
            `DataOneOf( lt1 ... ltn )`

            *Data range*

            ---

            Contains exactly the listed literals.

            Example:
            ```owl-fs
            DataOneOf("Peter" "1"^^xsd:integer)
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Enumeration_of_Literals)
        "#}.to_string(),
        "DatatypeRestriction" => indoc! {r#"
            `DatatypeRestriction( dt facet1 value1 ... facetn valuen )`

            *Data range*

            ---

            Restricts a datatype by one or more pairs of a constraining facet and a restriction value. Facets are IRIs such as `xsd:minInclusive`, `xsd:maxExclusive`, `xsd:length`, `xsd:minLength`, `xsd:maxLength`, `xsd:pattern` and `rdf:langRange`.

            Example:
            ```owl-fs
            DatatypeRestriction(xsd:integer
                xsd:minInclusive "5"^^xsd:integer
                xsd:maxExclusive "10"^^xsd:integer
            )
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Datatype_Restrictions)
        "#}.to_string(),

        // ------------------------------------------------------------------
        // Class expressions
        // ------------------------------------------------------------------
        "ObjectIntersectionOf" => indoc! {r"
            `ObjectIntersectionOf( ce1 ce2 ... cen )`

            *Class expression*

            ---

            Contains the individuals that are instances of all of the listed class expressions. Requires at least two class expressions.

            Example:
            ```owl-fs
            ObjectIntersectionOf(:Person :Female)
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Intersection_of_Class_Expressions)
        "}.to_string(),
        "ObjectUnionOf" => indoc! {r"
            `ObjectUnionOf( ce1 ce2 ... cen )`

            *Class expression*

            ---

            Contains the individuals that are instances of at least one of the listed class expressions. Requires at least two class expressions.

            Example:
            ```owl-fs
            ObjectUnionOf(:Man :Woman)
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Union_of_Class_Expressions)
        "}.to_string(),
        "ObjectComplementOf" => indoc! {r"
            `ObjectComplementOf( ce )`

            *Class expression*

            ---

            Contains the individuals that are not instances of the given class expression.

            Example:
            ```owl-fs
            ObjectComplementOf(:Man)
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Complement_of_Class_Expressions)
        "}.to_string(),
        "ObjectOneOf" => indoc! {r"
            `ObjectOneOf( a1 ... an )`

            *Class expression*

            ---

            Contains exactly the listed individuals.

            Example:
            ```owl-fs
            ObjectOneOf(:john :mary)
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Enumeration_of_Individuals)
        "}.to_string(),
        "ObjectSomeValuesFrom" => indoc! {r"
            `ObjectSomeValuesFrom( ope ce )`

            *Class expression*

            ---

            The existential restriction contains the individuals that are connected by `ope` to at least one instance of `ce`.

            Example:
            ```owl-fs
            SubClassOf(:Parent ObjectSomeValuesFrom(:hasChild :Person))
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Existential_Quantification)
        "}.to_string(),
        "ObjectAllValuesFrom" => indoc! {r"
            `ObjectAllValuesFrom( ope ce )`

            *Class expression*

            ---

            The universal restriction contains the individuals that are connected by `ope` only to instances of `ce`.

            Example:
            ```owl-fs
            SubClassOf(:VegetarianPizza
                ObjectAllValuesFrom(:hasTopping
                    ObjectUnionOf(:VegetableTopping :CheeseTopping)))
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Universal_Quantification)
        "}.to_string(),
        "ObjectHasValue" => indoc! {r"
            `ObjectHasValue( ope a )`

            *Class expression*

            ---

            The has-value restriction contains the individuals that are connected by `ope` to the individual `a`.

            Example:
            ```owl-fs
            SubClassOf(:Citizen ObjectHasValue(:livesIn :Germany))
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Individual_Value_Restriction)
        "}.to_string(),
        "ObjectHasSelf" => indoc! {r"
            `ObjectHasSelf( ope )`

            *Class expression*

            ---

            The self-restriction contains the individuals that are connected by `ope` to themselves.

            Example:
            ```owl-fs
            SubClassOf(:Narcissist ObjectHasSelf(:loves))
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Self-Restriction)
        "}.to_string(),
        "ObjectMinCardinality" => indoc! {r"
            `ObjectMinCardinality( n ope [ ce ] )`

            *Class expression*

            ---

            Contains the individuals that are connected by `ope` to at least `n` different individuals (optionally restricted to instances of `ce`).

            Example:
            ```owl-fs
            SubClassOf(:Parent ObjectMinCardinality(1 :hasChild :Person))
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Minimum_Cardinality)
        "}.to_string(),
        "ObjectMaxCardinality" => indoc! {r"
            `ObjectMaxCardinality( n ope [ ce ] )`

            *Class expression*

            ---

            Contains the individuals that are connected by `ope` to at most `n` different individuals (optionally restricted to instances of `ce`).

            Example:
            ```owl-fs
            SubClassOf(:Person ObjectMaxCardinality(2 :hasParent))
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Maximum_Cardinality)
        "}.to_string(),
        "ObjectExactCardinality" => indoc! {r"
            `ObjectExactCardinality( n ope [ ce ] )`

            *Class expression*

            ---

            Contains the individuals that are connected by `ope` to exactly `n` different individuals (optionally restricted to instances of `ce`).

            Example:
            ```owl-fs
            SubClassOf(:Person ObjectExactCardinality(2 :hasParent :Person))
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Exact_Cardinality)
        "}.to_string(),
        "DataSomeValuesFrom" => indoc! {r#"
            `DataSomeValuesFrom( dpe1 ... dpen dr )`

            *Class expression*

            ---

            The existential restriction contains the individuals that are connected by the data property to at least one literal in `dr`. Several data properties may be listed before the data range.

            Example:
            ```owl-fs
            SubClassOf(:Adult
                DataSomeValuesFrom(:hasAge
                    DatatypeRestriction(xsd:integer xsd:minInclusive "18"^^xsd:integer)))
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Existential_Quantification_2)
        "#}.to_string(),
        "DataAllValuesFrom" => indoc! {r"
            `DataAllValuesFrom( dpe1 ... dpen dr )`

            *Class expression*

            ---

            The universal restriction contains the individuals that are connected by the data property only to literals in `dr`. Several data properties may be listed before the data range.

            Example:
            ```owl-fs
            SubClassOf(:Adult DataAllValuesFrom(:hasAge xsd:integer))
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Universal_Quantification_2)
        "}.to_string(),
        "DataHasValue" => indoc! {r#"
            `DataHasValue( dpe lt )`

            *Class expression*

            ---

            The has-value restriction contains the individuals that are connected by the data property `dpe` to the literal `lt`.

            Example:
            ```owl-fs
            SubClassOf(:Alices DataHasValue(:hasName "Alice"@en))
            SubClassOf(:InitProcess DataHasValue(:hasPID "1"^^xsd:integer))
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Literal_Value_Restriction)
        "#}.to_string(),
        "DataMinCardinality" => indoc! {r"
            `DataMinCardinality( n dpe [ dr ] )`

            *Class expression*

            ---

            Contains the individuals that are connected by `dpe` to at least `n` different literals (optionally in `dr`).

            Example:
            ```owl-fs
            SubClassOf(:Person DataMinCardinality(1 :hasName xsd:string))
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Minimum_Cardinality_2)
        "}.to_string(),
        "DataMaxCardinality" => indoc! {r"
            `DataMaxCardinality( n dpe [ dr ] )`

            *Class expression*

            ---

            Contains the individuals that are connected by `dpe` to at most `n` different literals (optionally in `dr`).

            Example:
            ```owl-fs
            SubClassOf(:Person DataMaxCardinality(1 :hasBirthDate xsd:dateTime))
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Maximum_Cardinality_2)
        "}.to_string(),
        "DataExactCardinality" => indoc! {r"
            `DataExactCardinality( n dpe [ dr ] )`

            *Class expression*

            ---

            Contains the individuals that are connected by `dpe` to exactly `n` different literals (optionally in `dr`).

            Example:
            ```owl-fs
            SubClassOf(:Person DataExactCardinality(1 :hasSSN xsd:string))
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Exact_Cardinality_2)
        "}.to_string(),

        // ------------------------------------------------------------------
        // Class axioms
        // ------------------------------------------------------------------
        "SubClassOf" => indoc! {r"
            `SubClassOf( [ annotations ] sub super )`

            *Axiom*

            ---

            States that every instance of the first class expression is also an instance of the second.

            Example:
            ```owl-fs
            SubClassOf(:Child :Person)
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Subclass_Axioms)
        "}.to_string(),
        "EquivalentClasses" => indoc! {r#"
            `EquivalentClasses( [ annotations ] ce1 ce2 ... cen )`

            *Axiom*

            ---

            States that all listed class expressions have exactly the same instances. Requires at least two class expressions.

            Example:
            ```owl-fs
            EquivalentClasses(:Child
                ObjectIntersectionOf(:Person
                    DataSomeValuesFrom(:hasAge
                        DatatypeRestriction(xsd:integer xsd:maxExclusive "18"^^xsd:integer))))
            EquivalentClasses(:Person :Human)
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Equivalent_Classes)
        "#}.to_string(),
        "DisjointClasses" => indoc! {r"
            `DisjointClasses( [ annotations ] ce1 ce2 ... cen )`

            *Axiom*

            ---

            States that the listed class expressions are pairwise disjoint, i.e. no individual is an instance of more than one of them. Requires at least two class expressions.

            Example:
            ```owl-fs
            DisjointClasses(:Child :Adult)
            DisjointClasses(:Person :Animal :Plant)
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Disjoint_Classes)
        "}.to_string(),
        "DisjointUnion" => indoc! {r"
            `DisjointUnion( [ annotations ] C ce1 ce2 ... cen )`

            *Axiom*

            ---

            States that the class `C` is the union of the listed class expressions, and that these are pairwise disjoint. Requires at least two class expressions.

            Example:
            ```owl-fs
            DisjointUnion(:Party :Person :PrivateCompany :PublicOrganization)
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Disjoint_Union_of_Class_Expressions)
        "}.to_string(),

        // ------------------------------------------------------------------
        // Object property axioms
        // ------------------------------------------------------------------
        "SubObjectPropertyOf" => indoc! {r"
            `SubObjectPropertyOf( [ annotations ] sub super )`

            *Axiom*

            ---

            States that the first object property expression (or property chain) is a subproperty of the second: if `x` is connected to `y` by the sub property, then `x` is also connected to `y` by the super property.

            Example:
            ```owl-fs
            SubObjectPropertyOf(:hasDog :hasPet)
            SubObjectPropertyOf(ObjectPropertyChain(:hasParent :hasParent) :hasGrandparent)
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Object_Subproperties)
        "}.to_string(),
        "ObjectPropertyChain" => indoc! {r"
            `ObjectPropertyChain( ope1 ope2 ... open )`

            *Property expression chain*

            ---

            A sequence of object property expressions that can only be used as the first argument of `SubObjectPropertyOf`. If `x` is connected to `y` through the whole chain, then `x` is connected to `y` by the super property. Requires at least two object property expressions.

            Example:
            ```owl-fs
            SubObjectPropertyOf(ObjectPropertyChain(:hasMother :hasSister) :hasAunt)
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Object_Subproperties)
        "}.to_string(),
        "EquivalentObjectProperties" => indoc! {r"
            `EquivalentObjectProperties( [ annotations ] ope1 ope2 ... open )`

            *Axiom*

            ---

            States that all listed object property expressions connect exactly the same pairs of individuals. Requires at least two expressions.

            Example:
            ```owl-fs
            EquivalentObjectProperties(:hasBrother :hasMaleSibling)
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Equivalent_Object_Properties)
        "}.to_string(),
        "DisjointObjectProperties" => indoc! {r"
            `DisjointObjectProperties( [ annotations ] ope1 ope2 ... open )`

            *Axiom*

            ---

            States that the listed object property expressions are pairwise disjoint: no two individuals are connected by more than one of them. Requires at least two expressions.

            Example:
            ```owl-fs
            DisjointObjectProperties(:hasFather :hasMother)
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Disjoint_Object_Properties)
        "}.to_string(),
        "InverseObjectProperties" => indoc! {r"
            `InverseObjectProperties( [ annotations ] ope1 ope2 )`

            *Axiom*

            ---

            States that two object property expressions are inverses of each other.

            Example:
            ```owl-fs
            InverseObjectProperties(:hasParent :isParentOf)
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Inverse_Object_Properties_2)
        "}.to_string(),
        "ObjectPropertyDomain" => indoc! {r"
            `ObjectPropertyDomain( [ annotations ] ope ce )`

            *Axiom*

            ---

            States that every individual that has an outgoing connection via the object property expression is an instance of the given class expression.

            Example:
            ```owl-fs
            ObjectPropertyDomain(:hasParent :Person)
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Object_Property_Domain)
        "}.to_string(),
        "ObjectPropertyRange" => indoc! {r"
            `ObjectPropertyRange( [ annotations ] ope ce )`

            *Axiom*

            ---

            States that every individual that is the target of a connection via the object property expression is an instance of the given class expression.

            Example:
            ```owl-fs
            ObjectPropertyRange(:hasParent :Person)
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Object_Property_Range)
        "}.to_string(),
        "FunctionalObjectProperty" => indoc! {r"
            `FunctionalObjectProperty( [ annotations ] ope )`

            *Axiom*

            ---

            States that each individual can be connected by the object property expression to at most one distinct individual.

            Example:
            ```owl-fs
            FunctionalObjectProperty(:hasFather)
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Functional_Object_Properties)
        "}.to_string(),
        "InverseFunctionalObjectProperty" => indoc! {r"
            `InverseFunctionalObjectProperty( [ annotations ] ope )`

            *Axiom*

            ---

            States that for each individual `x` there is at most one individual `y` that is connected to `x` by the object property expression.

            Example:
            ```owl-fs
            InverseFunctionalObjectProperty(:fatherOf)
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Inverse-Functional_Object_Properties)
        "}.to_string(),
        "ReflexiveObjectProperty" => indoc! {r"
            `ReflexiveObjectProperty( [ annotations ] ope )`

            *Axiom*

            ---

            States that the object property expression connects each individual to itself.

            Example:
            ```owl-fs
            ReflexiveObjectProperty(:knows)
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Reflexive_Object_Properties)
        "}.to_string(),
        "IrreflexiveObjectProperty" => indoc! {r"
            `IrreflexiveObjectProperty( [ annotations ] ope )`

            *Axiom*

            ---

            States that the object property expression does not connect any individual to itself.

            Example:
            ```owl-fs
            IrreflexiveObjectProperty(:marriedTo)
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Irreflexive_Object_Properties)
        "}.to_string(),
        "SymmetricObjectProperty" => indoc! {r"
            `SymmetricObjectProperty( [ annotations ] ope )`

            *Axiom*

            ---

            States that if `x` is connected to `y` by the object property expression, then `y` is also connected to `x`.

            Example:
            ```owl-fs
            SymmetricObjectProperty(:isSiblingOf)
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Symmetric_Object_Properties)
        "}.to_string(),
        "AsymmetricObjectProperty" => indoc! {r"
            `AsymmetricObjectProperty( [ annotations ] ope )`

            *Axiom*

            ---

            States that if `x` is connected to `y` by the object property expression, then `y` cannot be connected to `x`.

            Example:
            ```owl-fs
            AsymmetricObjectProperty(:isParentOf)
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Asymmetric_Object_Properties)
        "}.to_string(),
        "TransitiveObjectProperty" => indoc! {r"
            `TransitiveObjectProperty( [ annotations ] ope )`

            *Axiom*

            ---

            States that if `x` is connected to `y` and `y` is connected to `z` by the object property expression, then `x` is also connected to `z`.

            Example:
            ```owl-fs
            TransitiveObjectProperty(:isAncestorOf)
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Transitive_Object_Properties)
        "}.to_string(),

        // ------------------------------------------------------------------
        // Data property axioms
        // ------------------------------------------------------------------
        "SubDataPropertyOf" => indoc! {r"
            `SubDataPropertyOf( [ annotations ] sub super )`

            *Axiom*

            ---

            States that the first data property is a subproperty of the second: if `x` is connected to literal `l` by the sub property, then `x` is also connected to `l` by the super property.

            Example:
            ```owl-fs
            SubDataPropertyOf(:hasLastName :hasName)
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Data_Subproperties)
        "}.to_string(),
        "EquivalentDataProperties" => indoc! {r"
            `EquivalentDataProperties( [ annotations ] dpe1 dpe2 ... dpen )`

            *Axiom*

            ---

            States that all listed data properties connect exactly the same individuals to the same literals. Requires at least two properties.

            Example:
            ```owl-fs
            EquivalentDataProperties(:hasSurname :hasFamilyName)
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Equivalent_Data_Properties)
        "}.to_string(),
        "DisjointDataProperties" => indoc! {r"
            `DisjointDataProperties( [ annotations ] dpe1 dpe2 ... dpen )`

            *Axiom*

            ---

            States that the listed data properties are pairwise disjoint: no individual is connected to the same literal by more than one of them. Requires at least two properties.

            Example:
            ```owl-fs
            DisjointDataProperties(:hasName :hasAddress)
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Disjoint_Data_Properties)
        "}.to_string(),
        "DataPropertyDomain" => indoc! {r"
            `DataPropertyDomain( [ annotations ] dpe ce )`

            *Axiom*

            ---

            States that every individual that has an outgoing connection via the data property is an instance of the given class expression. Note that the domain of a data property is a class expression, not a data range.

            Example:
            ```owl-fs
            DataPropertyDomain(:hasAge :Person)
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Data_Property_Domain)
        "}.to_string(),
        "DataPropertyRange" => indoc! {r#"
            `DataPropertyRange( [ annotations ] dpe dr )`

            *Axiom*

            ---

            States that all literals that are the target of the data property are in the given data range.

            Example:
            ```owl-fs
            DataPropertyRange(:hasAge
                DatatypeRestriction(xsd:integer xsd:minInclusive "0"^^xsd:integer))
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Data_Property_Range)
        "#}.to_string(),
        "FunctionalDataProperty" => indoc! {r"
            `FunctionalDataProperty( [ annotations ] dpe )`

            *Axiom*

            ---

            States that each individual can be connected by the data property to at most one distinct literal.

            Example:
            ```owl-fs
            FunctionalDataProperty(:hasAge)
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Functional_Data_Properties)
        "}.to_string(),

        // ------------------------------------------------------------------
        // Datatype definitions, keys
        // ------------------------------------------------------------------
        "DatatypeDefinition" => indoc! {r#"
            `DatatypeDefinition( [ annotations ] dt dr )`

            *Axiom*

            ---

            Defines the datatype `dt` as being equivalent to the data range `dr`.

            Example:
            ```owl-fs
            DatatypeDefinition(:SSN
                DatatypeRestriction(xsd:string xsd:pattern "[0-9]{3}-[0-9]{2}-[0-9]{4}"))
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Datatype_Definitions)
        "#}.to_string(),
        "HasKey" => indoc! {r"
            `HasKey( [ annotations ] ce ( ope1 ... open ) ( dpe1 ... dpen ) )`

            *Axiom*

            ---

            States that each named instance of the class expression is uniquely identified by the combination of values of the listed properties. The properties are given as two parenthesized lists: first object property expressions, then data properties.

            Example:
            ```owl-fs
            HasKey(:Person () (:hasSSN))
            HasKey(:Country (:hasCapital) (:hasIsoCode))
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Keys)
        "}.to_string(),

        // ------------------------------------------------------------------
        // Assertions
        // ------------------------------------------------------------------
        "SameIndividual" => indoc! {r"
            `SameIndividual( [ annotations ] a1 a2 ... an )`

            *Axiom*

            ---

            States that all listed individuals are the same entity. Requires at least two individuals.

            Example:
            ```owl-fs
            SameIndividual(:john :john_doe :j_doe)
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Individual_Equality)
        "}.to_string(),
        "DifferentIndividuals" => indoc! {r"
            `DifferentIndividuals( [ annotations ] a1 a2 ... an )`

            *Axiom*

            ---

            States that the listed individuals are pairwise distinct. This axiom can be used to axiomatize the unique name assumption. Requires at least two individuals.

            Example:
            ```owl-fs
            DifferentIndividuals(:john :jane :jack)
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Individual_Inequality)
        "}.to_string(),
        "ClassAssertion" => indoc! {r"
            `ClassAssertion( [ annotations ] ce a )`

            *Axiom*

            ---

            States that the individual `a` is an instance of the class expression `ce`.

            Example:
            ```owl-fs
            ClassAssertion(:Person :john)
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Class_Assertions)
        "}.to_string(),
        "ObjectPropertyAssertion" => indoc! {r"
            `ObjectPropertyAssertion( [ annotations ] ope source target )`

            *Axiom*

            ---

            States that the source individual is connected to the target individual by the object property expression.

            Example:
            ```owl-fs
            ObjectPropertyAssertion(:hasMother :john :mary)
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Positive_Object_Property_Assertions)
        "}.to_string(),
        "NegativeObjectPropertyAssertion" => indoc! {r"
            `NegativeObjectPropertyAssertion( [ annotations ] ope source target )`

            *Axiom*

            ---

            States that the source individual is **not** connected to the target individual by the object property expression.

            Example:
            ```owl-fs
            NegativeObjectPropertyAssertion(:hasSon :peter :chris)
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Negative_Object_Property_Assertions)
        "}.to_string(),
        "DataPropertyAssertion" => indoc! {r#"
            `DataPropertyAssertion( [ annotations ] dpe source value )`

            *Axiom*

            ---

            States that the source individual is connected to the literal `value` by the data property.

            Example:
            ```owl-fs
            DataPropertyAssertion(:hasAge :john "25"^^xsd:integer)
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Positive_Data_Property_Assertions)
        "#}.to_string(),
        "NegativeDataPropertyAssertion" => indoc! {r#"
            `NegativeDataPropertyAssertion( [ annotations ] dpe source value )`

            *Axiom*

            ---

            States that the source individual is **not** connected to the literal `value` by the data property.

            Example:
            ```owl-fs
            NegativeDataPropertyAssertion(:hasAge :john "53"^^xsd:integer)
            ```

            [Specification](https://www.w3.org/TR/owl2-syntax/#Negative_Data_Property_Assertions)
        "#}.to_string(),

        _ => String::new(),
    }
}

pub fn child_keywords_for_kind(kind: &str) -> &[(&'static str, &'static str)] {
    match kind {
        "class_frame" => &[
            ("Class Frame", "    Annotations:"),
            ("Class Frame", "    SubClassOf:"),
            ("Class Frame", "    EquivalentTo:"),
            ("Class Frame", "    DisjointWith:"),
            ("Class Frame", "    DisjointUnionOf:"),
            ("Class Frame", "    HasKey:"),
        ],
        "datatype_frame" => &[
            ("Datatype Frame", "    Annotations:"),
            ("Datatype Frame", "    EquivalentTo:"),
        ],
        "object_property_frame" => &[
            ("Object Property Frame", "    Annotations:"),
            ("Object Property Frame", "    Domain:"),
            ("Object Property Frame", "    Range:"),
            ("Object Property Frame", "    SubPropertyOf:"),
            ("Object Property Frame", "    EquivalentTo:"),
            ("Object Property Frame", "    DisjointWith:"),
            ("Object Property Frame", "    InverseOf:"),
            ("Object Property Frame", "    Characteristics:"),
            ("Object Property Frame", "    SubPropertyChain:"),
        ],
        "data_property_frame" => &[
            ("Data Property Frame", "    Annotations:"),
            ("Data Property Frame", "    Domain:"),
            ("Data Property Frame", "    Range:"),
            ("Data Property Frame", "    Characteristics:"),
            ("Data Property Frame", "    SubPropertyOf:"),
            ("Data Property Frame", "    EquivalentTo:"),
            ("Data Property Frame", "    DisjointWith:"),
        ],
        "annotation_property_frame" => &[
            ("Annotation Property Frame", "    Annotations:"),
            ("Annotation Property Frame", "    Domain:"),
            ("Annotation Property Frame", "    Range:"),
            ("Annotation Property Frame", "    SubPropertyOf:"),
        ],
        "individual_frame" => &[
            ("Individual Frame", "    Annotations:"),
            ("Individual Frame", "    Types:"),
            ("Individual Frame", "    Facts:"),
            ("Individual Frame", "    SameAs:"),
            ("Individual Frame", "    DifferentFrom:"),
        ],
        "ontology" => &[
            ("Ontology", "Annotations:"),
            ("Ontology", "Import:"),
            ("Ontology", "Class:"),
            ("Ontology", "Datatype:"),
            ("Ontology", "ObjectProperty:"),
            ("Ontology", "DataProperty:"),
            ("Ontology", "AnnotationProperty:"),
            ("Ontology", "Individual:"),
        ],
        _ => {
            &[]
            // cant do action here
        }
    }
}

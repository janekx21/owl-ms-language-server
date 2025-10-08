use crate::workspace::{FrameInfo, FrameType, Iri};
use indoc::indoc;

pub fn get_fixed_infos(iri: &Iri) -> Vec<FrameInfo> {
    match &iri[..] {
        "http://www.w3.org/2000/01/rdf-schema#label" => vec![FrameInfo {
            iri: "http://www.w3.org/2000/01/rdf-schema#label".to_string(),
            annotations: vec![(
                "http://www.w3.org/2000/01/rdf-schema#label".to_string(),
                vec!["label".to_string()],
            )]
            .into_iter()
            .collect(),
            frame_type: FrameType::AnnotationProperty,
            definitions: vec![],
        }],
        "http://www.w3.org/2000/01/rdf-schema#comment" => vec![FrameInfo {
            iri: "http://www.w3.org/2000/01/rdf-schema#comment".to_string(),
            annotations: vec![(
                "http://www.w3.org/2000/01/rdf-schema#label".to_string(),
                vec!["comment".to_string()],
            )]
            .into_iter()
            .collect(),
            frame_type: FrameType::AnnotationProperty,
            definitions: vec![],
        }],
        _ => vec![],
    }
}

pub fn keyword_hover_info(kind: &str) -> String {
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
                `self`

                *Class expression* 

                ---

                The self-restriction `ope self` describes a class of individuals that are connected by the object property expression `ope` to themselves.

                Example:
                ```owl-ms
                Class: Person
                    SubClassOf: likes self
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
                    EquivalentTo: isParentOf inverseOf isChildOf
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
            _ => "".into(),
        }
}

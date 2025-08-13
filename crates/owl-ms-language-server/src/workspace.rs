use crate::catalog::CatalogUri;
use crate::position::Position;
use crate::queries::{self, treesitter_highlight_capture_into_semantic_token_type_index};
use crate::web::HttpClient;
use crate::{
    catalog::Catalog, debugging::timeit, queries::ALL_QUERIES, range::Range,
    rope_provider::RopeProvider, LANGUAGE, NODE_TYPES,
};
use anyhow::Result;
use anyhow::{anyhow, Context};
use cached::proc_macro::cached;
use cached::SizedCache;
use core::fmt;
use dashmap::DashMap;
use horned_owl::io::ParserConfiguration;
use horned_owl::model::Component::*;
use horned_owl::model::{ArcStr, Build};
use horned_owl::ontology::iri_mapped::ArcIRIMappedOntology;
use horned_owl::ontology::set::SetOntology;
use indoc::indoc;
use itertools::Itertools;
use log::{debug, error, info, trace, warn};
use once_cell::sync::Lazy;
use parking_lot::{Mutex, MutexGuard, RwLock};
use ropey::Rope;
use std::fmt::Debug;
use std::hash::{DefaultHasher, Hash, Hasher};
use std::iter::once;
use std::ops::Deref;
use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    fs,
    path::PathBuf,
    sync::Arc,
};
use tower_lsp::lsp_types::{
    Diagnostic, DiagnosticSeverity, DidChangeTextDocumentParams, InlayHint, InlayHintLabel,
    SemanticToken, SymbolKind, Url, WorkspaceFolder,
};
use tree_sitter_c2rust::{
    InputEdit, Node, Parser, Point, Query, QueryCursor, StreamingIterator, Tree,
};

static GLOBAL_PARSER: Lazy<Mutex<Parser>> = Lazy::new(|| {
    let mut parser = Parser::new();
    parser.set_language(&LANGUAGE).unwrap();
    parser.set_logger(Some(Box::new(|type_, str| match type_ {
        tree_sitter_c2rust::LogType::Parse => trace!(target: "tree-sitter-parse", "{}", str),
        tree_sitter_c2rust::LogType::Lex => trace!(target: "tree-sitter-lex", "{}", str),
    })));

    Mutex::new(parser)
});

pub fn lock_global_parser() -> MutexGuard<'static, Parser> {
    (*GLOBAL_PARSER).lock()
}

static GLOBAL_BUILD_ARC: Lazy<Mutex<Build<ArcStr>>> = Lazy::new(|| {
    let build = Build::new_arc();
    Mutex::new(build)
});

pub fn lock_global_build_arc() -> MutexGuard<'static, Build<ArcStr>> {
    (*GLOBAL_BUILD_ARC).lock()
}

#[derive(Debug)]
pub struct Workspace {
    /// Maps an URL to a document that can be internal or external
    pub internal_documents: DashMap<Url, Arc<RwLock<InternalDocument>>>,
    pub external_documents: DashMap<Url, Arc<RwLock<ExternalDocument>>>,
    pub workspace_folder: WorkspaceFolder,
    pub catalogs: Vec<Catalog>,
}

impl Workspace {
    pub fn new(workspace_folder: WorkspaceFolder) -> Self {
        let catalogs = Catalog::load_catalogs_recursive(workspace_folder.uri.clone());
        info!(
            "New workspace {} at {} with catalogs {catalogs:?}",
            workspace_folder.name, workspace_folder.uri
        );
        Workspace {
            internal_documents: DashMap::new(),
            external_documents: DashMap::new(),
            workspace_folder,
            catalogs,
        }
    }

    pub fn insert_internal_document(
        &self,
        document: InternalDocument,
    ) -> Arc<RwLock<InternalDocument>> {
        debug!(
            "Insert internal document {} length is {}",
            document.uri,
            document.rope.len_chars()
        );
        let uri = document.uri.clone();
        let arc = Arc::new(RwLock::new(document));
        self.internal_documents.insert(uri, arc.clone());
        arc
    }
    pub fn insert_external_document(
        &self,
        document: ExternalDocument,
    ) -> Arc<RwLock<ExternalDocument>> {
        debug!(
            "Insert external document {} length is {}",
            document.uri,
            document.text.len()
        );
        let uri = document.uri.clone();
        let arc = Arc::new(RwLock::new(document));
        self.external_documents.insert(uri, arc.clone());
        arc
    }

    // TODO #28 maybe return a reference?
    /// This searches in the frames of internal documents
    pub fn search_frame(&self, partial_text: &str) -> Vec<(Iri, FrameInfo)> {
        self.internal_documents
            .iter()
            .flat_map(|dm| {
                let dm = &*dm.value().read();
                dm.get_all_frame_infos()
                    .iter()
                    .filter(|item| item.iri.contains(partial_text))
                    .map(|kv| (kv.iri.clone(), kv.clone()))
                    .collect_vec()
            })
            .collect_vec()
    }

    /// This finds a frame info in the internal and external documents.
    ///
    /// - `iri` should be a full iri
    pub fn get_frame_info(&self, iri: &Iri) -> Option<FrameInfo> {
        let external_infos = self.external_documents.iter().flat_map(|doc| {
            let mut doc = doc.write();
            doc.get_frame_info(iri)
        });

        let internal_infos = self.internal_documents.iter().filter_map(|dm| {
            let dm = dm.value().read();
            dm.get_frame_info(iri)
        });

        internal_infos
            .chain(external_infos)
            .chain(get_fixed_infos(iri))
            .tree_reduce(FrameInfo::merge)
    }

    pub fn get_frame_info_recursive(
        &self,
        iri: &Iri,
        doc: &InternalDocument,
        http_client: &dyn HttpClient,
    ) -> Option<FrameInfo> {
        doc.reachable_docs_recusive(self, http_client)
            .iter()
            .filter_map(|url| {
                if let Ok(doc) = self.resolve_url_to_document(url, http_client) {
                    match &doc {
                        DocumentReference::Internal(rw_lock) => rw_lock.read().get_frame_info(iri),
                        DocumentReference::External(rw_lock) => rw_lock.write().get_frame_info(iri),
                    }
                } else {
                    None
                }
            })
            .chain(get_fixed_infos(iri))
            .tree_reduce(FrameInfo::merge)
    }

    pub fn node_info(&self, node: &Node, doc: &InternalDocument) -> String {
        match node.kind() {
            "class_frame" | "annotation_property_frame" | "class_iri" => {
                // Goto first named child and repeat
                if let Some(iri_node) = &node.named_child(0) {
                    self.node_info(iri_node, doc)
                } else {
                    "Class Frame\nNo iri was found".to_string()
                }
            }
            "full_iri" => {
                let iri = trim_full_iri(node_text(node, &doc.rope));

                self.get_frame_info(&iri)
                    .map(|fi| fi.info_display(self))
                    .unwrap_or(iri)
            }
            "simple_iri" | "abbreviated_iri" => {
                let iri = node_text(node, &doc.rope);
                debug!("Getting node info for {iri} at doc {}", doc.uri);
                let iri = doc.abbreviated_iri_to_full_iri(iri.to_string());
                self.get_frame_info(&iri)
                    .map(|fi| fi.info_display(self))
                    .unwrap_or(iri)
            }

            // Keyword hover information
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

    fn find_catalog_uri(&self, url: &Url) -> Option<(&Catalog, &CatalogUri)> {
        let url_string = url.to_string();

        for catalog in &self.catalogs {
            for catalog_uri in catalog.all_catalog_uris() {
                if catalog_uri.name == url_string {
                    return Some((catalog, catalog_uri));
                }
            }
        }
        None
    }

    pub fn get_document_by_url(&self, url: &Url) -> Option<DocumentReference> {
        if let Some(doc) = self.internal_documents.get(url) {
            // Document is loaded already
            return Some(DocumentReference::Internal(doc.clone()));
        }
        if let Some(doc) = self.external_documents.get(url) {
            // Document is loaded already
            return Some(DocumentReference::External(doc.clone()));
        }
        None
    }

    /// Resolves a URL (file or http/https protocol) to a document that is inserted into this workspace
    pub fn resolve_url_to_document(
        &self,
        url: &Url,
        client: &dyn HttpClient,
    ) -> Result<DocumentReference> {
        if let Some(doc) = self.get_document_by_url(url) {
            return Ok(doc);
        }

        let (catalog, catalog_uri) = if let Some(a) = self.find_catalog_uri(url) {
            a
        } else {
            warn!("Url {url} could not be found in any catalog");
            let document_text = client.get(url.as_str()).context("Http client request")?;
            let document = ExternalDocument::new(document_text, url.clone())
                .context("External document creation")?;
            let doc = self.insert_external_document(document);
            return Ok(DocumentReference::External(doc));
        };

        let url_from_catalog = Url::parse(&catalog_uri.uri);

        match url_from_catalog {
            Ok(url) => {
                if let Some(doc) = self.get_document_by_url(&url) {
                    return Ok(doc);
                }

                match url.to_file_path() {
                    Ok(path) => {
                        // This is an abolute file path url
                        self.resolve_path_to_document(path)
                    }
                    Err(_) => {
                        // This is an external url
                        let document_text =
                            client.get(url.as_str()).context("Http client request")?;
                        let document = ExternalDocument::new(document_text, url)
                            .context("External document creation")?;
                        let doc = self.insert_external_document(document);
                        Ok(DocumentReference::External(doc))
                    }
                }
            }
            Err(_) => {
                // This is a relative file path
                let path = catalog.parent_folder().join(&catalog_uri.uri);
                let url =
                    Url::from_file_path(&path).map_err(|_| anyhow!("Url is not a file path"))?;
                if let Some(doc) = self.get_document_by_url(&url) {
                    return Ok(doc);
                }

                self.resolve_path_to_document(path)
            }
        }
    }

    fn resolve_path_to_document(&self, path: PathBuf) -> Result<DocumentReference> {
        let (document_text, document_url) = load_file_from_disk(path.clone())?;

        match path
            .extension()
            .and_then(|s| s.to_str())
            .unwrap_or_default()
        {
            "omn" => {
                let document = InternalDocument::new(document_url, -1, document_text);
                let doc = self.insert_internal_document(document);
                Ok(DocumentReference::Internal(doc))
            }
            "owl" | "owx" => {
                let document = ExternalDocument::new(document_text, document_url)?;
                let doc = self.insert_external_document(document);
                Ok(DocumentReference::External(doc))
            }
            ext => Err(anyhow!("The extention {ext} is not supported")),
        }
    }
}

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

fn load_file_from_disk(path: PathBuf) -> Result<(String, Url)> {
    info!("Loading file from disk {}", path.display());

    Ok((
        fs::read_to_string(&path)?,
        Url::from_file_path(&path).map_err(|_| anyhow!("Url is not a file path"))?,
    ))
}

#[derive(Debug)]
pub enum DocumentReference {
    // Not boxing this is fine because the size ratio is just about 1.6
    Internal(Arc<RwLock<InternalDocument>>),
    External(Arc<RwLock<ExternalDocument>>),
}

impl DocumentReference {}

#[derive(Debug)]
pub struct InternalDocument {
    pub uri: Url,
    pub tree: Tree,
    pub rope: Rope,
    pub version: i32,
    pub diagnostics: Vec<Diagnostic>,
}

impl core::hash::Hash for InternalDocument {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.uri.hash(state);
        self.version.hash(state);
    }
}
impl Eq for InternalDocument {}
impl PartialEq for InternalDocument {
    fn eq(&self, other: &Self) -> bool {
        self.rope == other.rope
    }
}

impl InternalDocument {
    pub fn new(uri: Url, version: i32, text: String) -> InternalDocument {
        let tree = timeit("create_document / parse", || {
            lock_global_parser()
                .parse(&text, None)
                .expect("language to be set, no timeout to be used, no cancelation flag")
        });

        let rope = Rope::from(text);
        let diagnostics = timeit("create_document / gen_diagnostics", || {
            gen_diagnostics(&tree.root_node())
        });

        InternalDocument {
            uri,
            version,
            tree,
            rope,
            diagnostics,
        }
    }

    pub fn query(&self, query: &Query) -> Vec<UnwrappedQueryMatch> {
        self.query_helper(query, None)
    }

    pub fn query_range(&self, query: &Query, range: Range) -> Vec<UnwrappedQueryMatch> {
        self.query_helper(query, Some(range))
    }

    pub fn query_with_imports(
        &self,
        query: &Query,
        workspace: &Workspace,
        http_client: &dyn HttpClient,
    ) -> Vec<UnwrappedQueryMatch> {
        // Resolve the documents that are imported here
        // This also contains itself!
        let docs = self
            .reachable_docs_recusive(workspace, http_client)
            .iter()
            .filter_map(|url| {
                workspace
                    .resolve_url_to_document(url, http_client)
                    .inspect_err(|e| error!("{e:?}"))
                    .ok()
            })
            .collect_vec();

        docs.iter()
            .flat_map(|doc| match doc {
                DocumentReference::Internal(doc) => doc.read().query(query),
                DocumentReference::External(_) => {
                    warn!("Query in external document not supported");
                    vec![]
                }
            })
            .collect_vec()
    }

    fn reachable_docs_recusive(
        &self,
        workspace: &Workspace,
        http_client: &dyn HttpClient,
    ) -> Vec<Url> {
        let mut set: HashSet<Url> = HashSet::new();
        self.reachable_docs_recursive_helper(workspace, &mut set, http_client);
        set.into_iter().collect_vec()
    }

    fn reachable_docs_recursive_helper(
        &self,
        workspace: &Workspace,
        result: &mut HashSet<Url>,
        http_client: &dyn HttpClient,
    ) {
        if result.contains(&self.uri) {
            // Do nothing
            return;
        }

        result.insert(self.uri.clone());

        let docs = self
            .imports()
            .iter()
            .filter_map(|url| {
                workspace
                    .resolve_url_to_document(url, http_client)
                    .inspect_err(|e| error!("{e:?}"))
                    .ok()
            })
            .collect_vec();

        for doc in docs {
            match doc {
                DocumentReference::Internal(internal_document) => internal_document
                    .read()
                    .reachable_docs_recursive_helper(workspace, result, http_client),
                DocumentReference::External(e) => {
                    e.read()
                        .reachable_docs_recursive_helper(workspace, result, http_client)
                }
            };
        }
    }

    fn imports(&self) -> Vec<Url> {
        self.query(&ALL_QUERIES.import_query)
            .iter()
            .filter_map(|m| match &m.captures[..] {
                [iri] => Url::parse(&trim_full_iri(&iri.node.text)[..])
                    .inspect_err(|e| warn!("Url could not be parsed {e:#}"))
                    .ok(),
                _ => unimplemented!(),
            })
            .collect_vec()
    }

    pub fn query_helper(&self, query: &Query, range: Option<Range>) -> Vec<UnwrappedQueryMatch> {
        let mut query_cursor = QueryCursor::new();
        if let Some(range) = range {
            query_cursor.set_point_range(range.into());
        }
        let rope_provider = RopeProvider::new(&self.rope);

        query_cursor
            .matches(query, self.tree.root_node(), rope_provider)
            .map_deref(|m| UnwrappedQueryMatch {
                _pattern_index: m.pattern_index,
                _id: m.id(),
                captures: m
                    .captures
                    .iter()
                    .sorted_by_key(|c| c.index)
                    .map(|c| UnwrappedQueryCapture {
                        node: UnwrappedNode {
                            id: c.node.id(),
                            text: node_text(&c.node, &self.rope).to_string(),
                            range: c.node.range().into(),
                            kind: c.node.kind().into(),
                        },
                        _index: c.index,
                    })
                    .collect_vec(),
            })
            .collect_vec()
    }

    pub fn edit(&mut self, params: &DidChangeTextDocumentParams) {
        if self.version >= params.text_document.version {
            return; // no change needed
        }

        if params
            .content_changes
            .iter()
            .any(|change| change.range.is_none())
        {
            // Change the whole file
            panic!("Whole file changes are not supported yet");
        }

        // This range is relative to the *old* document not the new one
        let change_ranges = params
            .content_changes
            .iter()
            .rev() // See https://github.com/helix-editor/helix/blob/0815b52e0959e21ec792ea41d508a050b552f850/helix-core/src/syntax.rs#L1293C1-L1297C26
            .map(|change| {
                if let Some(range) = change.range {
                    let old_range: Range = range.into();
                    let start_char = self
                        .rope
                        .try_line_to_char(old_range.start.line as usize)
                        .expect("line_idx out of bounds")
                        + (old_range.start.character as usize);

                    let old_end_char = self
                        .rope
                        .try_line_to_char(old_range.end.line as usize)
                        .expect("line_idx out of bounds")
                        + (old_range.end.character as usize); // exclusive

                    // must come before the rope is changed!
                    let old_end_byte = self.rope.char_to_byte(old_end_char);

                    // rope replace
                    timeit("rope operations", || {
                        self.rope.remove(start_char..old_end_char);
                        self.rope.insert(start_char, &change.text);
                    });

                    // this must come after the rope was changed!
                    let start_byte = self.rope.char_to_byte(start_char);
                    let new_end_byte = start_byte + change.text.len();
                    let new_end_line = self.rope.byte_to_line(new_end_byte);
                    let new_end_character =
                        self.rope.byte_to_char(new_end_byte) - self.rope.line_to_char(new_end_line);

                    let edit = InputEdit {
                        start_byte,
                        old_end_byte,
                        new_end_byte: start_byte + change.text.len(),
                        start_position: old_range.start.into(),
                        old_end_position: old_range.end.into(),
                        new_end_position: Point {
                            row: new_end_line,
                            column: new_end_character,
                        },
                    };
                    timeit("tree edit", || self.tree.edit(&edit));

                    self.version = params.text_document.version;

                    let new_range = Range {
                        start: edit.start_position.into(),
                        end: edit.new_end_position.into(),
                    };

                    (old_range, edit, new_range)
                } else {
                    unreachable!("Change should have range {:#?}", change);
                }
            })
            .collect::<Vec<(Range, InputEdit, Range)>>();

        // debug!("Change ranges {:#?}", change_ranges);

        let rope_provider = RopeProvider::new(&self.rope);

        let tree = {
            let mut parser_guard = lock_global_parser();
            timeit("parsing", || {
                parser_guard
                    .parse_with_options(
                        &mut |byte_idx, _| rope_provider.chunk_callback(byte_idx),
                        Some(&self.tree),
                        None,
                    )
                    .expect("language to be set, no timeout to be used, no cancelation flag")
            })
        };
        // debug!("New tree {}", tree.root_node().to_sexp());
        self.tree = tree;

        // TODO #30 prune
        // Remove all old diagnostics with an overlapping range. They will need to be recreated
        // for (_, _, old_range) in change_ranges.iter() {
        //     document
        //         .diagnostics
        //         .retain(|d| !lines_overlap(&d.range.into(), old_range));
        // }
        // Move all other diagnostics
        // for diagnostic in &mut document.diagnostics {
        //     for (new_range, edit, old_range) in change_ranges.iter() {
        //         let mut range_to_end = *old_range;
        //         range_to_end.end = Position {
        //             line: u32::MAX,
        //             character: u32::MAX,
        //         };
        //         if lines_overlap(&diagnostic.range.into(), &range_to_end) {
        //             debug!("old {} -> new {}", old_range, new_range);
        //             let delta = new_range.end.line as i32 - old_range.end.line as i32;
        //             diagnostic.range.start.line =
        //                 (diagnostic.range.start.line as i32 + delta) as u32;
        //             diagnostic.range.start.line =
        //                 (diagnostic.range.end.line as i32 + delta) as u32;
        //         }
        //     }
        // }
        // for (_, _, _) in change_ranges.iter() {
        //     let cursor = document.tree.walk();
        //     // TODO #30
        //     // while range_exclusive_inside(new_range, &cursor.node().range().into()) {
        //     //     if cursor
        //     //         .goto_first_child_for_point(new_range.start.into())
        //     //         .is_none()
        //     //     {
        //     //         break;
        //     //     }
        //     // }
        //     // cursor.goto_parent();
        //     let node_that_has_change = cursor.node();
        //     drop(cursor);
        //     // while range_overlaps(&ts_range_to_lsp_range(cursor.node().range()), &range) {}
        //     // document.diagnostics =
        //     let additional_diagnostics = timeit("did_change > gen_diagnostics", || {
        //         gen_diagnostics(&node_that_has_change)
        //     })
        //     .into_iter();
        //     // .filter(|d| lines_overlap(&d.range.into(), new_range)); // should be exclusive to other diagnostics
        //     document.diagnostics.extend(additional_diagnostics);
        // }

        // TODO #30 replace with above
        self.diagnostics = timeit("did_change > gen_diagnostics", || {
            gen_diagnostics(&self.tree.root_node())
        });
    }

    pub fn abbreviated_iri_to_full_iri(&self, abbriviated_iri: String) -> String {
        let prefixes = self.prefixes();
        if let Some((prefix, simple_iri)) = abbriviated_iri.split_once(':') {
            if let Some(resolved_prefix) = prefixes.get(prefix) {
                resolved_prefix.clone() + simple_iri
            } else {
                abbriviated_iri.clone()
            }
        } else {
            // Simple IRIs get a free colon prependet
            // ref: https://www.w3.org/TR/owl2-manchester-syntax/#IRIs.2C_Integers.2C_Literals.2C_and_Entities
            if let Some(resolved_prefix) = prefixes.get("") {
                resolved_prefix.clone() + &abbriviated_iri
            } else {
                abbriviated_iri.clone()
            }
        }
    }

    pub fn full_iri_to_abbreviated_iri(&self, full_iri: String) -> Option<String> {
        self.prefixes()
            .into_iter()
            .find_map(|(prefix, url)| match full_iri.split_once(&url) {
                Some(("", post)) if prefix.is_empty() => Some(post.to_string()),
                Some(("", post)) => Some(prefix + ":" + post),
                Some(_) => None,
                None => None,
            })
    }

    pub fn ontology_id(&self) -> Option<Iri> {
        let result = self.query(&ALL_QUERIES.ontology_id);
        result
            .iter()
            .exactly_one()
            .ok()
            .map(|m| match &m.captures[..] {
                [iri] => trim_full_iri(&iri.node.text),
                _ => unreachable!(),
            })
    }

    /// Returns the prefixes of a document (without colon :)
    ///
    /// Some prefixes should always be defined
    ///
    /// ```owl-ms
    /// Prefix: rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    /// Prefix: rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    /// Prefix: xsd: <http://www.w3.org/2001/XMLSchema#>
    /// Prefix: owl: <http://www.w3.org/2002/07/owl#>
    /// ```
    pub fn prefixes(&self) -> HashMap<String, String> {
        prefixes_helper(self)
    }

    pub fn inlay_hint(
        &self,
        range: Range,
        workspace: &Workspace,
        http_client: &dyn HttpClient,
    ) -> Vec<InlayHint> {
        self.query_range(&ALL_QUERIES.iri_query, range)
            .into_iter()
            .flat_map(|match_| match_.captures)
            .filter_map(|capture| {
                let iri = trim_full_iri(capture.node.text);
                let iri = self.abbreviated_iri_to_full_iri(iri);

                let label = workspace
                    .get_frame_info_recursive(&iri, self, http_client)
                    .and_then(|frame_info| frame_info.label())
                    .unwrap_or_default();

                if label.is_empty() {
                    None
                } else {
                    Some(InlayHint {
                        position: capture.node.range.end.into(),
                        label: InlayHintLabel::String(label),
                        kind: None,
                        text_edits: None,
                        tooltip: None,
                        padding_left: Some(true),
                        padding_right: None,
                        data: None,
                    })
                }
            })
            .collect_vec()
    }

    pub fn get_frame_info(&self, iri: &Iri) -> Option<FrameInfo> {
        get_frame_info_helper(self, iri)
    }

    pub fn get_all_frame_infos(&self) -> Vec<FrameInfo> {
        document_all_frame_infos(self)
            .values()
            .cloned()
            .collect_vec()
    }

    pub fn try_keywords_at_position(&self, cursor: Position) -> Vec<String> {
        let mut parser = lock_global_parser();
        let rope = self.rope.clone();
        let tree = self.tree.clone();

        let cursor_char_index = rope.line_to_char(cursor.line as usize) + cursor.character as usize;

        let line = rope.line(cursor.line as usize).to_string();
        let partial = word_before_character(cursor.character as usize, &line);

        debug!("Cursor node text is {:?}", partial);

        let keywords = &*queries::KEYWORDS;

        let kws = keywords
            .iter()
            .filter(|k| k.starts_with(&partial))
            .collect_vec();

        debug!("Checking {} keywords", kws.len());

        kws.iter()
            .filter_map(|kw| {
                let mut rope_version = rope.clone();
                let change = kw[partial.len()..].to_string() + " a";

                let mut tree = tree.clone(); // This is fast

                // Must come before the rope is changed!
                let cursor_byte_index = rope_version.char_to_byte(cursor_char_index);

                rope_version.insert(cursor_char_index, &change);

                // Must come after rope changed!
                let new_end_byte = cursor_byte_index + change.len();
                let new_end_line = cursor.line as usize;
                let new_end_character = rope_version.byte_to_char(new_end_byte)
                    - rope_version.line_to_char(new_end_line);

                let edit = InputEdit {
                    // Old range is just a zero size range
                    start_byte: cursor_char_index,
                    start_position: cursor.into(),
                    old_end_byte: cursor_char_index,
                    old_end_position: cursor.into(),

                    new_end_byte,
                    new_end_position: Point {
                        row: new_end_line,
                        column: new_end_character,
                    },
                };
                tree.edit(&edit);

                let rope_provider = RopeProvider::new(&rope_version);
                let new_tree = parser
                    .parse_with_options(
                        &mut |byte_idx, _| rope_provider.chunk_callback(byte_idx),
                        Some(&tree),
                        None,
                    )
                    .expect("language to be set, no timeout to be used, no cancelation flag");

                // debug!(
                // "A possible source code version for {kw} (change is {change}) with rope version {rope_version}\nNew tree {:?}", new_tree.root_node().to_sexp());

                let mut cursor_one_left = cursor.to_owned();
                cursor_one_left.character = cursor_one_left.character.saturating_sub(1);
                let cursor_node_version = new_tree
                    .root_node()
                    .named_descendant_for_point_range(
                        cursor_one_left.into(),
                        cursor_one_left.into(),
                    )
                    .unwrap();

                debug!("{cursor_node_version:#?} is {}", cursor_node_version.kind());

                if cursor_node_version.kind().starts_with("keyword_")
                    && !cursor_node_version.parent().unwrap().is_error()
                {
                    debug!("Found possible keyword {kw}!");
                    Some(kw.to_string())
                } else {
                    debug!("{kw} is not possible");
                    None
                }
            })
            .collect_vec()
    }

    pub fn sematic_tokens(&self, range: Option<Range>) -> Vec<SemanticToken> {
        let doc = self;
        let query_source = tree_sitter_owl_ms::HIGHLIGHTS_QUERY;

        let query = Query::new(&LANGUAGE, query_source).expect("valid query expect");
        let mut query_cursor = QueryCursor::new();
        if let Some(range) = range {
            query_cursor.set_point_range(range.into());
        }
        let matches =
            query_cursor.matches(&query, doc.tree.root_node(), RopeProvider::new(&doc.rope));

        let mut tokens = vec![];
        let mut last_start = Point { row: 0, column: 0 };

        let mut nodes = matches
            .map_deref(|m| m.captures)
            .flatten()
            .map(|c| {
                (
                    c.node,
                    treesitter_highlight_capture_into_semantic_token_type_index(
                        query.capture_names()[c.index as usize],
                    ),
                )
            })
            .collect_vec();

        // node start poins need to be stricly in order, because the delta might otherwise negativly overflow
        // TODO is this needed? are query matches in order?
        nodes.sort_unstable_by_key(|(n, _)| n.start_byte());
        for (node, type_index) in nodes {
            let start = node.range().start_point;
            let delta_line = (start.row - last_start.row) as u32;
            let delta_start = if delta_line == 0 {
                (start.column - last_start.column) as u32 // same line
            } else {
                start.column as u32 // some other line
            };
            let length = node_text(&node, &doc.rope).len_chars() as u32;

            let token = SemanticToken {
                delta_line,
                delta_start,
                length,
                token_type: type_index,
                token_modifiers_bitset: 0,
            };

            last_start = node.start_position();
            tokens.push(token);
        }
        tokens
    }
}

/// Returns the word before the [`character`] position in the [`line`]
pub fn word_before_character(character: usize, line: &str) -> String {
    line[..character]
        .chars()
        .rev()
        .take_while(|c| c.is_alphabetic())
        .collect_vec()
        .iter()
        .rev()
        .collect::<String>()
}

#[cached(
    size = 20,
    key = "u64",
    convert = r#"{
            let mut hasher = DefaultHasher::new();
            doc.hash(&mut hasher);
            hasher.finish()
     } "#
)]
fn document_all_frame_infos(doc: &InternalDocument) -> HashMap<Iri, FrameInfo> {
    let mut infos: HashMap<String, FrameInfo> = HashMap::new();

    for ele in document_annotations(doc)
        .into_iter()
        .map(|(frame_iri, annoation_iri, literal)| FrameInfo {
            iri: frame_iri.clone(),
            annotations: HashMap::from([(annoation_iri, vec![literal])]),
            frame_type: FrameType::Unknown,
            definitions: Vec::new(),
        })
    {
        if let Some(fi) = infos.get_mut(&ele.iri) {
            fi.extend(ele);
        } else {
            infos.insert(ele.iri.clone(), ele);
        }
    }

    for ele in document_definitions(doc)
        .into_iter()
        .map(|(frame_iri, range, kind)| FrameInfo {
            iri: frame_iri.clone(),
            annotations: HashMap::new(),
            frame_type: FrameType::parse(&kind),
            definitions: vec![Location {
                uri: doc.uri.clone(),
                range,
            }],
        })
    {
        if let Some(fi) = infos.get_mut(&ele.iri) {
            fi.extend(ele);
        } else {
            infos.insert(ele.iri.clone(), ele);
        }
    }

    infos
}

#[cached(
    size = 200,
    key = "u64",
    convert = r#"{
            let mut hasher = DefaultHasher::new();
            doc.hash(&mut hasher);
            iri.hash(&mut hasher);
            hasher.finish()
     } "#
)]
fn get_frame_info_helper(doc: &InternalDocument, iri: &Iri) -> Option<FrameInfo> {
    document_all_frame_infos(doc).get(iri).cloned()
    // let annotation_infos = timeit("Annotation infos", || {
    //     let a = document_annotations(doc);
    //     info!("Annotations {a:#?}");
    //     a.get_key_value(iri)
    //         .map(|(frame_iri, (annoation_iri, literal))| FrameInfo {
    //             iri: frame_iri.clone(),
    //             annotations: HashMap::from([(annoation_iri.clone(), vec![literal.clone()])]),
    //             frame_type: FrameType::Unknown,
    //             definitions: Vec::new(),
    //         })
    // });

    // let definition_infos = timeit("Definition infos", || {
    //     let a = document_definitions(doc);
    //     info!("Definitions {a:#?}");
    //     a.get_key_value(iri)
    //         .map(|(frame_iri, (range, kind))| FrameInfo {
    //             iri: frame_iri.clone(),
    //             annotations: HashMap::new(),
    //             frame_type: FrameType::parse(&kind),
    //             definitions: vec![Location {
    //                 uri: doc.uri.clone(),
    //                 range: range.clone(),
    //             }],
    //         })
    // });

    // annotation_infos
    //     .into_iter()
    //     .chain(definition_infos)
    //     .tree_reduce(FrameInfo::merge)
}

fn document_annotations(doc: &InternalDocument) -> Vec<(String, String, String)> {
    doc.query(&ALL_QUERIES.annotation_query)
        .iter()
        .map(|m| match &m.captures[..] {
            [frame_iri, annoation_iri, literal] => {
                let frame_iri =
                    doc.abbreviated_iri_to_full_iri(trim_full_iri(frame_iri.node.text.clone()));
                let annoation_iri =
                    doc.abbreviated_iri_to_full_iri(trim_full_iri(annoation_iri.node.text.clone()));
                let literal = trim_string_value(&literal.node.text);

                (frame_iri, annoation_iri, literal)
            }
            _ => unreachable!(),
        })
        .collect_vec()
}

fn document_definitions(doc: &InternalDocument) -> Vec<(String, Range, String)> {
    doc.query(&ALL_QUERIES.frame_query)
        .iter()
        .map(|m| match &m.captures[..] {
            [frame_iri, frame] => {
                let frame_iri =
                    doc.abbreviated_iri_to_full_iri(trim_full_iri(frame_iri.node.text.clone()));

                (frame_iri, frame.node.range, frame.node.kind.clone())
            }
            _ => unreachable!(),
        })
        .collect()
}

/// External documents are ontologies that are not expected to change in any way.
pub struct ExternalDocument {
    pub uri: Url,
    pub text: String,
    pub ontology: ArcIRIMappedOntology,
}

impl Hash for ExternalDocument {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.uri.hash(state);
    }
}

impl fmt::Debug for ExternalDocument {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ExternalDocument")
            .field("uri", &self.uri)
            .field("text.len()", &self.text.len())
            .finish()
    }
}

impl ExternalDocument {
    /// The ontology type is currently determent by the url extention
    pub fn new(text: String, url: Url) -> Result<ExternalDocument> {
        let b = horned_owl::model::Build::new_arc();
        let mut buffer = text.as_bytes();

        match url.path().rsplit_once(".") {
            // TODO parse both
            Some((_, "owx")) => {
                let (ontology, _) = horned_owl::io::owx::reader::read_with_build(&mut buffer, &b)
                    .map_err(horned_to_anyhow)?;

                Ok(ExternalDocument {
                    uri: url,
                    text,
                    ontology,
                })
            }
            // For owl files and files without extention
            _ => {
                let (ontology, _) = horned_owl::io::rdf::reader::read_with_build(
                    &mut buffer,
                    &b,
                    ParserConfiguration::default(),
                )
                .map_err(horned_to_anyhow)?;

                let ontology = SetOntology::from_index(ontology.i().clone());

                let ontology = ArcIRIMappedOntology::from(ontology);

                Ok(ExternalDocument {
                    uri: url,
                    text,
                    ontology,
                })
            }
        }
    }

    fn imports(&self) -> Vec<Url> {
        self.ontology
            .iter()
            .filter_map(|ac| match &ac.component {
                horned_owl::model::Component::Import(import) => Some(Url::parse(import.0.deref())),
                _ => None,
            })
            .filter_map(|r| r.ok())
            .collect_vec()
    }

    fn reachable_documents(&self) -> Vec<Url> {
        self.imports()
    }

    fn reachable_docs_recursive_helper(
        &self,
        workspace: &Workspace,
        result: &mut HashSet<Url>,
        http_client: &dyn HttpClient,
    ) {
        if result.contains(&self.uri) {
            // Do nothing
            return;
        }

        result.insert(self.uri.clone());

        let docs = self
            .imports()
            .iter()
            .filter_map(|url| {
                workspace
                    .resolve_url_to_document(url, http_client)
                    .inspect_err(|e| error!("{e:?}"))
                    .ok()
            })
            .collect_vec();

        for doc in docs {
            match doc {
                DocumentReference::Internal(internal_document) => internal_document
                    .read()
                    .reachable_docs_recursive_helper(workspace, result, http_client),
                DocumentReference::External(e) => {
                    e.read()
                        .reachable_docs_recursive_helper(workspace, result, http_client)
                }
            };
        }
    }

    pub fn get_frame_info(&mut self, iri: &Iri) -> Option<FrameInfo> {
        get_frame_info_helper_ex(self, iri)
    }
}

#[cached(
    size = 200,
    key = "u64",
    convert = r#"{
            let mut hasher = DefaultHasher::new();
            doc.hash(&mut hasher);
            iri.hash(&mut hasher);
            hasher.finish()
     } "#
)]
fn get_frame_info_helper_ex(doc: &mut ExternalDocument, iri: &Iri) -> Option<FrameInfo> {
    let iri_mapped_ontology = &mut doc.ontology;

    let build = lock_global_build_arc();
    let iri = build.iri(iri.clone());

    iri_mapped_ontology
        .components_for_iri(&iri)
        .filter_map(|c| match &c.component {
            AnnotationAssertion(aa) => match &aa.subject {
                horned_owl::model::AnnotationSubject::IRI(iri_) if iri_ == &iri => {
                    Some(aa.ann.clone())
                }
                _ => None,
            },
            _ => None,
        })
        .filter_map(|annotation| match annotation.av {
            horned_owl::model::AnnotationValue::Literal(horned_owl::model::Literal::Simple {
                literal,
            }) => {
                let annotations = once((annotation.ap.0.to_string(), vec![literal])).collect();
                Some(FrameInfo {
                    iri: iri.to_string(),
                    annotations,
                    frame_type: FrameType::Class,
                    definitions: vec![Location {
                        uri: doc.uri.clone(),
                        range: Range::ZERO,
                    }],
                })
            }
            _ => None,
        })
        .tree_reduce(FrameInfo::merge)
}

fn horned_to_anyhow(e: horned_owl::error::HornedError) -> anyhow::Error {
    match e {
        horned_owl::error::HornedError::IOError(error) => {
            anyhow!("IO Error: {error}")
        }
        horned_owl::error::HornedError::ParserError(error, location) => {
            anyhow!("Parsing Error: {error} {location}")
        }
        horned_owl::error::HornedError::ValidityError(msg, location) => {
            anyhow!("Validity Error: {msg} at {location}")
        }
        horned_owl::error::HornedError::CommandError(msg) => {
            anyhow!("Command Error: {msg}")
        }
    }
}

/// This is a version of a query match that has no reference to the tree or cursor
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct UnwrappedQueryMatch {
    _pattern_index: usize,
    pub captures: Vec<UnwrappedQueryCapture>,
    _id: u32,
}

/// This is a version of a query capture that has no reference to the tree or cursor
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct UnwrappedQueryCapture {
    pub node: UnwrappedNode,
    _index: u32,
}

/// This is a version of a node that has no reference to the tree
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct UnwrappedNode {
    /// Id's of a changed tree stay the same. So you can search for up to date information that way
    pub id: usize,
    /// This informtion can be outdated
    pub text: String,
    /// This informtion can be outdated
    pub range: Range,
    pub kind: String,
}

/// This represents informations about a frame.
/// For example the following frame has information.
/// ```owl-ms
/// Class: PizzaThing
///     Annotations: rdfs:label "Pizza"
/// ```
/// Then the [`FrameInfo`] contains the label "Pizza" and the frame type "Class".
#[derive(Clone, Debug)]
pub struct FrameInfo {
    pub iri: Iri,
    pub annotations: HashMap<Iri, Vec<String>>,
    pub frame_type: FrameType,
    pub definitions: Vec<Location>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Location {
    uri: Url,
    pub range: Range,
}

impl From<Location> for tower_lsp::lsp_types::Location {
    fn from(val: Location) -> Self {
        tower_lsp::lsp_types::Location {
            uri: val.uri,
            range: val.range.into(),
        }
    }
}

impl From<tower_lsp::lsp_types::Location> for Location {
    fn from(value: tower_lsp::lsp_types::Location) -> Self {
        Location {
            uri: value.uri,
            range: value.range.into(),
        }
    }
}

impl FrameInfo {
    fn merge(a: FrameInfo, b: FrameInfo) -> FrameInfo {
        let mut c = a.clone();
        c.extend(b);
        c
    }

    fn extend(&mut self, b: FrameInfo) {
        for (key_a, values_a) in b.annotations {
            if let Some(values_b) = self.annotations.get_mut(&key_a) {
                values_b.extend(values_a);
            } else {
                self.annotations.insert(key_a, values_a);
            }
        }
        self.definitions.extend(b.definitions);
        self.frame_type = match (self.frame_type, b.frame_type) {
            (a, b) if a == b => a,
            (FrameType::Unknown, b) => b,
            (a, FrameType::Unknown) => a,
            _ => FrameType::Invalid, // a != b and not one of them is unknown => conflict
        };
    }

    pub fn label(&self) -> Option<String> {
        self.annoation_display(&"http://www.w3.org/2000/01/rdf-schema#label".to_string())
    }

    pub fn annoation_display(&self, iri: &Iri) -> Option<String> {
        self.annotations
            .get(iri)
            // TODO #20 make this more usable by providing multiple lines with indentation
            .map(|resolved| {
                resolved
                    .iter()
                    .map(|s| trim_string_value(s))
                    .unique()
                    .join(", ")
            })
    }

    pub fn info_display(&self, workspace: &Workspace) -> String {
        let entity = self.frame_type;
        let label = self
            .label()
            .unwrap_or(trim_url_before_last(&self.iri).to_string());

        let annotations = self
            .annotations
            .keys()
            .map(|iri| {
                let iri_label = workspace
                    .get_frame_info(iri)
                    .map(|fi| {
                        fi.label()
                            .unwrap_or_else(|| trim_url_before_last(&fi.iri).to_string())
                    })
                    .unwrap_or(iri.clone());
                // TODO #28 use values directly
                let mut annoation_display = self.annoation_display(iri).unwrap_or(iri.clone());

                // If this is a multiline string then give it some space to work whith
                if annoation_display.contains("\n") {
                    annoation_display = format!("\n{annoation_display}\n\n");
                }

                format!("- `{iri_label}`: {annoation_display}")
            })
            .join("\n");

        format!(
            "{entity} **{label}**\n\n---\n{annotations}\n\nIRI: {}",
            self.iri
        )
    }
}

fn trim_url_before_last(iri: &str) -> &str {
    iri.rsplit_once(['/', '#']).map(|(_, b)| b).unwrap_or(iri)
}

fn trim_string_value(value: &str) -> String {
    value
        .trim_start_matches('"')
        .trim_end_matches("@en")
        .trim_end_matches("@de")
        .trim_end_matches("@pt")
        .trim_end_matches("^^xsd:string") // typed literal with type string
        .trim_end_matches('"')
        .replace("\\\"", "\"")
        .trim()
        .to_string()
}

// TODO maybe use Arc<String>
type Iri = String;

pub fn node_text<'a>(node: &Node, rope: &'a Rope) -> ropey::RopeSlice<'a> {
    rope.byte_slice(node.start_byte()..node.end_byte())
}

/// Generate the diagnostics for a single node, walking recusivly down to every child and every syntax error within
pub fn gen_diagnostics(node: &Node) -> Vec<Diagnostic> {
    let mut cursor = node.walk();
    let mut diagnostics = Vec::<Diagnostic>::new();

    loop {
        let node = cursor.node();

        if node.is_error() {
            // log
            let range: Range = cursor.node().range().into();

            // root has no parents so use itself
            let parent_kind = node.parent().unwrap_or(node).kind();

            if let Some(static_node) = NODE_TYPES.get(parent_kind) {
                let valid_children: String = Itertools::intersperse(
                    static_node
                        .children
                        .types
                        .iter()
                        .map(|sn| node_type_to_string(&sn.type_)),
                    ", ".to_string(),
                )
                .collect();

                let parent = node_type_to_string(parent_kind);
                let msg = format!("Syntax Error. expected {valid_children} inside {parent}");

                diagnostics.push(Diagnostic {
                    range: range.into(),
                    severity: Some(DiagnosticSeverity::ERROR),
                    code: None,
                    code_description: None,
                    source: Some("owl language server".to_string()),
                    message: msg.to_string(),
                    related_information: None,
                    tags: None,
                    data: None,
                });
            }
            // move along
            while !cursor.goto_next_sibling() {
                // move out
                if !cursor.goto_parent() {
                    // this node has no parent, its the root
                    return diagnostics;
                }
            }
        } else if node.has_error() {
            // move in
            let has_child = cursor.goto_first_child(); // should alwayes work

            if !has_child {
                while !cursor.goto_next_sibling() {
                    // move out
                    if !cursor.goto_parent() {
                        // this node has no parent, its the root
                        return diagnostics;
                    }
                }
            }
        } else {
            // move along
            while !cursor.goto_next_sibling() {
                // move out
                if !cursor.goto_parent() {
                    // this node has no parent, its the root
                    return diagnostics;
                }
            }
        }
    }
}

fn node_type_to_string(node_type: &str) -> String {
    Itertools::intersperse(
        node_type.split_terminator('_').map(capitilize_string),
        " ".to_string(),
    )
    .collect()
}

fn capitilize_string(s: &str) -> String {
    let mut c = s.chars();
    match c.next() {
        None => String::new(),
        Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
    }
}

/// taken from https://www.w3.org/TR/owl2-syntax/#Entity_Declarations_and_Typing
#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum FrameType {
    Class,
    DataType,
    ObjectProperty,
    DataProperty,
    AnnotationProperty,
    Individual,
    Ontology,
    Invalid, // The frame type of an IRI that has no valid frame (this can be because of conflicts)
    Unknown, // The frame type of an IRI that has no frame at all (can be overriten)
}

impl FrameType {
    pub fn parse(kind: &str) -> FrameType {
        match kind {
            "class_iri" => FrameType::Class,
            "datatype_iri" => FrameType::DataType,
            "annotation_property_iri" => FrameType::AnnotationProperty,
            "individual_iri" => FrameType::Individual,
            "ontology_iri" => FrameType::Ontology,
            "data_property_iri" => FrameType::DataProperty,
            "object_property_iri" => FrameType::ObjectProperty,
            "class_frame" => FrameType::Class,
            "datatype_frame" => FrameType::DataType,
            "annotation_property_frame" => FrameType::AnnotationProperty,
            "individual_frame" => FrameType::Individual,
            "ontology_frame" => FrameType::Ontology,
            "data_property_frame" => FrameType::DataProperty,
            "object_property_frame" => FrameType::ObjectProperty,
            kind => {
                error!("Implement {kind}");
                FrameType::Invalid
            }
        }
    }
}

impl From<FrameType> for tower_lsp::lsp_types::SymbolKind {
    fn from(val: FrameType) -> Self {
        match val {
            FrameType::Class => SymbolKind::CLASS,
            FrameType::DataType => SymbolKind::STRUCT,
            FrameType::ObjectProperty => SymbolKind::PROPERTY,
            FrameType::DataProperty => SymbolKind::PROPERTY,
            FrameType::AnnotationProperty => SymbolKind::PROPERTY,
            FrameType::Individual => SymbolKind::OBJECT,
            FrameType::Ontology => SymbolKind::MODULE,
            FrameType::Invalid => SymbolKind::NULL,
            FrameType::Unknown => SymbolKind::NULL,
        }
    }
}

impl Display for FrameType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = match self {
            FrameType::Class => "Class",
            FrameType::DataType => "Data Type",
            FrameType::ObjectProperty => "Object Property",
            FrameType::DataProperty => "Data Property",
            FrameType::AnnotationProperty => "Annotation Property",
            FrameType::Individual => "Named Individual",
            FrameType::Ontology => "Ontology",
            FrameType::Invalid => "Invalid Frame Type",
            FrameType::Unknown => "Unknown Frame Type",
        };
        write!(f, "{name}")
    }
}

/// Takes an IRI in any form and removed the <> symbols
pub fn trim_full_iri<T: ToString>(untrimmed_iri: T) -> Iri {
    untrimmed_iri
        .to_string()
        .trim_end_matches(">")
        .trim_start_matches("<")
        .to_string()
}

// Horned owl has no default here. Lets keep it out for now.
// static STANDART_PREFIX_NAMES: [(&str, &str); 4] = [
//     ("rdf:", "http://www.w3.org/1999/02/22-rdf-syntax-ns#"),
//     ("rdfs:", "http://www.w3.org/2000/01/rdf-schema#"),
//     ("owl:", "http://www.w3.org/2002/07/owl#"),
//     ("xsd:", "http://www.w3.org/2001/XMLSchema#"),
// ];

#[cached(
    ty = "SizedCache<u64, HashMap<String, String>>",
    create = "{ SizedCache::with_size(20) }",
    convert = r#"{
            let mut hasher = DefaultHasher::new();
            doc.hash(&mut hasher);
            hasher.finish()
     } "#
)]
fn prefixes_helper(doc: &InternalDocument) -> HashMap<String, String> {
    doc.query(&ALL_QUERIES.prefix)
        .into_iter()
        .map(|m| match &m.captures[..] {
            [name, iri] => (
                name.node.text.trim_end_matches(':').to_string(),
                trim_full_iri(iri.node.text.clone()),
            ),
            _ => unreachable!(),
        })
        // Horned owl has no default here. Lets keep it out for now.
        // .chain(
        //     STANDART_PREFIX_NAMES
        //         .iter()
        //         .map(|(a, b)| (a.to_string(), b.to_string())),
        // )
        .unique()
        .collect()
}

#[cfg(test)]
mod tests {
    use crate::position::Position;

    use super::*;
    use pretty_assertions::assert_eq;
    use test_log::test;

    #[test]
    fn internal_document_ontology_id_should_return_none() {
        let doc = InternalDocument::new(Url::parse("http://foo").unwrap(), -1, "Ontology: ".into());

        let iri = doc.ontology_id();

        assert_eq!(iri, None);
    }

    #[test]
    fn internal_document_ontology_id_should_return_some() {
        let doc = InternalDocument::new(
            Url::parse("http://foo/bar").unwrap(),
            -1,
            "Ontology: <http://foo/bar>
            Class: Foo"
                .into(),
        );

        let iri = doc.ontology_id();

        info!("{}", doc.tree.root_node().to_sexp());

        assert_eq!(iri, Some("http://foo/bar".to_string()));
    }

    #[test]
    fn internal_document_abbreviated_iri_to_full_iri_should_convert_abbriviated_iri() {
        let doc = InternalDocument::new(
            Url::parse("http://this/is/not/relevant/1").unwrap(),
            -1,
            "
                Prefix: owl: <http://www.w3.org/2002/07/owl#>
                Prefix: ja: <http://www.semanticweb.org/janek/ontologies/2025/5/untitled-ontology-3/>
            "
            .into(),
        );

        let full_iri = doc.abbreviated_iri_to_full_iri("owl:Nothing".into());
        let full_iri_2 = doc.abbreviated_iri_to_full_iri("ja:Janek".into());

        assert_eq!(full_iri, "http://www.w3.org/2002/07/owl#Nothing");
        assert_eq!(
            full_iri_2,
            "http://www.semanticweb.org/janek/ontologies/2025/5/untitled-ontology-3/Janek"
        );
    }

    #[test]
    fn internal_document_abbreviated_iri_to_full_iri_should_convert_simple_iri() {
        let doc = InternalDocument::new(
            Url::parse("http://this/is/not/relevant/2").unwrap(),
            -1,
            "
                Prefix: : <http://www.w3.org/2002/07/owl#>
            "
            .into(),
        );

        let full_iri = doc.abbreviated_iri_to_full_iri(":Nothing".into());
        let full_iri_2 = doc.abbreviated_iri_to_full_iri("Nothing".into());

        assert_eq!(full_iri, "http://www.w3.org/2002/07/owl#Nothing");
        assert_eq!(full_iri_2, "http://www.w3.org/2002/07/owl#Nothing");
    }

    #[test]
    fn internal_document_prefix_should_return_all_prefixes() {
        let doc = InternalDocument::new(
            Url::parse("http://www.w3.org/2002/07/owl").unwrap(),
            -1,
            "
                Prefix: : <http://www.semanticweb.org/janek/ontologies/2025/5/untitled-ontology-3/>
                Prefix: owl: <http://www.w3.org/2002/07/owl#>
                Prefix: rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                Prefix: rdfs: <http://www.w3.org/2000/01/rdf-schema#>
                Prefix: xml: <http://www.w3.org/XML/1998/namespace>
                Prefix: xsd: <http://www.w3.org/2001/XMLSchema#>

                Ontology: <http://www.semanticweb.org/janek/ontologies/2025/5/untitled-ontology-3>
            "
            .into(),
        );

        let prefixes = doc.prefixes().into_iter().sorted().collect_vec();

        assert_eq!(
            prefixes,
            vec![
                (
                    "".into(),
                    "http://www.semanticweb.org/janek/ontologies/2025/5/untitled-ontology-3/"
                        .into()
                ),
                ("owl".into(), "http://www.w3.org/2002/07/owl#".into()),
                (
                    "rdf".into(),
                    "http://www.w3.org/1999/02/22-rdf-syntax-ns#".into()
                ),
                (
                    "rdfs".into(),
                    "http://www.w3.org/2000/01/rdf-schema#".into()
                ),
                ("xml".into(), "http://www.w3.org/XML/1998/namespace".into()),
                ("xsd".into(), "http://www.w3.org/2001/XMLSchema#".into())
            ]
        );
    }

    #[test]
    fn internal_document_get_frame_info_should_show_definitions() {
        // Arrange
        let doc = InternalDocument::new(
            Url::parse("http://foo/14329076").unwrap(),
            -1,
            r#"
                Ontology:
                    Class: A
                        Annotations: rdfs:label "This class is in the first file"

                        SubClassOf: class-in-other-file
             "#
            .into(),
        );

        // Act
        let info = doc.get_frame_info(&"A".to_string());

        // Assert
        info!("{doc:#?}");
        let info = info.unwrap();

        assert_eq!(info.iri, "A".to_string());
        assert_eq!(
            info.definitions,
            vec![Location {
                uri: "http://foo/14329076".parse().unwrap(),
                range: Range {
                    start: Position {
                        line: 2,
                        character: 20
                    },
                    end: Position {
                        line: 5,
                        character: 55
                    },
                }
            }]
        );
    }

    #[test]
    fn external_document_new_given_owl_text_does_parse_ontology() {
        // Arrange
        let ontology_text = r#"
        <?xml version="1.0"?>
        <Ontology xmlns="http://www.w3.org/2002/07/owl#" xml:base="http://www.example.com/iri" ontologyIRI="http://www.example.com/iri">
            <Declaration>
                <Class IRI="https://www.example.com/o1"/>
            </Declaration>
        </Ontology>
        "#
        .to_string();

        // Act
        let external_doc = ExternalDocument::new(
            ontology_text.clone(),
            Url::parse("https://example.com/onto.owx").unwrap(),
        );

        // Assert
        let doc = external_doc.unwrap();
        assert_eq!(doc.text, ontology_text);

        doc.ontology.iter().for_each(|ac| match &ac.component {
            horned_owl::model::Component::DeclareClass(declare_class) => {
                let iri = &(declare_class.0).0;
                assert_eq!(&iri[..], "https://www.example.com/o1");
            }
            horned_owl::model::Component::OntologyID(ontology_id) => {
                let iri = ontology_id.iri.clone().unwrap();
                assert_eq!(&iri[..], "http://www.example.com/iri");
            }
            _ => {}
        });
    }

    #[test]
    fn external_document_reachable_documents_given_imports_does_return_imports() {
        // Arrange
        let owl_ontology_text = r#"
            <?xml version="1.0"?>
            <Ontology xmlns="http://www.w3.org/2002/07/owl#" xml:base="http://www.example.com/iri" ontologyIRI="http://www.example.com/iri">
                <Import>file:///abosulte/file</Import>
                <Import>http://www.example.com/other-property</Import>
                <Declaration>
                    <Class IRI="https://www.example.com/o9"/>
                </Declaration>
            </Ontology>
        "#
    .to_string();
        let external_doc = ExternalDocument::new(
            owl_ontology_text.clone(),
            Url::parse("https://example.com/onto.owx").unwrap(),
        )
        .unwrap();

        // Act
        let urls = external_doc.reachable_documents();

        // Assert
        assert!(urls.contains(&Url::parse("http://www.example.com/other-property").unwrap()));
        assert!(urls.contains(&Url::parse("file:///abosulte/file").unwrap()));
    }

    #[test]
    fn word_before_character_should_find_word() {
        let word = word_before_character(25, "This is a line with multi words");
        assert_eq!(word, "multi");
    }

    #[test]
    fn full_iri_to_abbreviated_iri_should_work_for_simple_iris() {
        let doc = InternalDocument::new(
            Url::parse("http://this/is/not/relevant/3").unwrap(),
            -1,
            "
                Prefix: owl: <http://www.w3.org/2002/07/owl#>
            "
            .into(),
        );

        let abbr_iri =
            doc.full_iri_to_abbreviated_iri("http://www.w3.org/2002/07/owl#Thing".into());

        assert_eq!(abbr_iri, Some("owl:Thing".to_string()));
    }

    #[test]
    fn full_iri_to_abbreviated_iri_should_work_for_simple_iris_with_empty_prefix() {
        let doc = InternalDocument::new(
            Url::parse("http://this/is/not/relevant/4").unwrap(),
            -1,
            "
                Prefix: : <http://www.w3.org/2002/07/owl#>
            "
            .into(),
        );

        let abbr_iri =
            doc.full_iri_to_abbreviated_iri("http://www.w3.org/2002/07/owl#Thing".into());

        assert_eq!(abbr_iri, Some("Thing".to_string()));
    }
}

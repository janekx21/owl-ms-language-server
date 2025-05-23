use std::{
    collections::{HashMap, HashSet},
    ffi::OsStr,
    fmt::Display,
    fs,
    ops::Deref,
    path::PathBuf,
};

use anyhow::anyhow;
use anyhow::Result;
use dashmap::DashMap;
use horned_owl::{curie::PrefixMapping, ontology::set::SetOntology};
use itertools::Itertools;
use log::{debug, error, info, warn};
use ropey::Rope;
use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, SymbolKind, Url, WorkspaceFolder};
use tree_sitter::{Node, Parser, Query, QueryCursor, Tree};

use crate::{
    catalog::Catalog, debugging::timeit, queries::ALL_QUERIES, range::Range,
    rope_provider::RopeProvider, NODE_TYPES,
};

#[derive(Debug)]
pub struct Workspace {
    pub internal_document_map: DashMap<Url, InternalDocument>,
    pub external_document_map: DashMap<Url, ExternalDocument>,
    pub workspace_folder: WorkspaceFolder,
    pub catalogs: Vec<Catalog>,
}

impl Workspace {
    pub fn new(workspace_folder: WorkspaceFolder) -> Self {
        let catalogs = Catalog::load_catalogs_recursive(workspace_folder.uri.clone());
        info!(
            "New workspace at {} with catalogs {:?}",
            workspace_folder.uri, catalogs
        );
        Workspace {
            internal_document_map: DashMap::new(),
            external_document_map: DashMap::new(),
            workspace_folder,
            catalogs,
        }
    }

    /// Returns a bool that specifies if the workspace url is a base for the provided url and therefore
    /// the workspace could contain that url
    pub fn could_contain(&self, url: &Url) -> bool {
        let fp_folder = self
            .workspace_folder
            .uri
            .to_file_path()
            .expect("valid filepath");
        let fp_file = url.to_file_path().expect("valid filepath");
        fp_file.starts_with(fp_folder)
    }

    pub fn insert_document(
        &self,
        document: InternalDocument,
    ) -> dashmap::mapref::one::Ref<Url, InternalDocument> {
        let uri = document.uri.clone();
        self.internal_document_map.insert(uri.clone(), document);
        self.internal_document_map.get(&uri).unwrap()
    }

    // TODO #28 maybe return a reference?
    pub fn search_frame(&self, partial_text: &str) -> Vec<(Iri, FrameInfo)> {
        self.internal_document_map
            .iter()
            .flat_map(|dm| {
                dm.frame_infos
                    .iter()
                    .filter(|item| item.key().contains(partial_text))
                    .map(|kv| (kv.key().clone(), kv.value().clone()))
                    .collect_vec()
            })
            .collect_vec()
    }

    pub fn get_frame_info(&self, iri: &Iri) -> Option<FrameInfo> {
        self.internal_document_map
            .iter()
            .filter_map(|dm| dm.frame_infos.get(iri).map(|v| v.value().clone()))
            .tree_reduce(FrameInfo::merge)
    }

    pub fn node_info(&self, node: &Node, doc: &InternalDocument) -> String {
        match node.kind() {
            "class_frame" | "annotation_property_frame" => {
                let iri = &node
                    .named_child(0)
                    .map(|c| node_text(&c, &doc.rope).to_string());

                if let Some(iri) = iri {
                    self.get_frame_info(iri)
                        .map(|fi| fi.info_display(self))
                        .unwrap_or(iri.clone())
                    // iri_info_display(iri, self)
                } else {
                    "Class Frame\nNo iri was found".to_string()
                }
            }
            "full_iri" | "simple_iri" | "abbreviated_iri" => {
                let iri = node_text(node, &doc.rope).to_string();
                self.get_frame_info(&iri)
                    .map(|fi| fi.info_display(self))
                    .unwrap_or(iri)
                // iri_info_display(&iri, backend)
            }
            "class_iri" => {
                // iri_info_display(&node_text(node, &doc.rope).to_string(), backend)
                let iri = node_text(node, &doc.rope).to_string();
                self.get_frame_info(&iri)
                    .map(|fi| fi.info_display(self))
                    .unwrap_or(iri)
            }

            _ => format!("generic node named {}", node.kind()),
        }
    }

    pub fn query(&self, query: &Query) -> Vec<UnwrappedQueryMatch> {
        self.internal_document_map
            .iter()
            .flat_map(|doc| doc.query(query))
            .collect_vec()
    }

    pub fn resolve_url_to_document(
        &self,
        url: &Url,
        parser: &mut Parser,
    ) -> Result<dashmap::mapref::one::Ref<'_, Url, InternalDocument>> {
        if let Some(doc) = self.internal_document_map.get(url) {
            // Document is loaded already
            return Ok(doc);
        }

        for catalog in &self.catalogs {
            for catalog_uri in catalog.all_catalog_uris() {
                if catalog_uri.name == url.to_string() {
                    let file_or_external_url = Url::parse(&catalog_uri.name);

                    let (document_text, document_url) = match file_or_external_url {
                        Ok(url) => match url.to_file_path() {
                            Ok(path) => {
                                // This is an abolute file path url
                                load_file_from_disk(path)?
                            }
                            Err(_) => {
                                // This is an external url
                                let text = ureq::get(url.to_string())
                                    .call()?
                                    .body_mut()
                                    .read_to_string()?;
                                (text, url)
                            }
                        },
                        Err(_) => {
                            // This is a relative file path
                            let path = catalog.parent_folder().join(&catalog_uri.uri);

                            info!("Loaded file from disk at {}", path.display());
                            load_file_from_disk(path)?
                        }
                    };

                    let document = InternalDocument::new(document_url, -1, document_text, parser);
                    return Ok(self.insert_document(document));
                }
            }
        }
        Err(anyhow!(
            "The document at {url} could not be found in the workspace"
        ))
    }
}

fn load_file_from_disk(path: PathBuf) -> Result<(String, Url)> {
    if path.extension() != Some(OsStr::new("omn")) {
        warn!("Non omn files can not be loaded. path {}", path.display());
        return Err(anyhow!(
            "Non omn files can not be loaded. path {}",
            path.display()
        ));
    }

    info!("Loading file from disk {}", path.display());

    Ok((
        fs::read_to_string(&path)?,
        Url::from_file_path(&path).map_err(|_| anyhow!("Url is not a file path"))?,
    ))
}

#[derive(Debug)]
pub enum Document {
    Internal(Box<InternalDocument>),
    External(Box<ExternalDocument>),
}
#[derive(Debug, PartialEq, Eq)]
pub enum OwlDialect {
    Unknown,
    Omn,
    Owl,
}

#[derive(Debug)]
pub struct InternalDocument {
    pub uri: Url,
    pub tree: Tree,
    pub rope: Rope,
    pub version: i32,
    pub diagnostics: Vec<Diagnostic>,
    pub frame_infos: DashMap<Iri, FrameInfo>,

    /// This can differ from the url file extention, so we need to track it
    pub owl_dialect: OwlDialect,
}

impl InternalDocument {
    pub fn new(uri: Url, version: i32, text: String, parser: &mut Parser) -> InternalDocument {
        let owl_dialect = match &uri.path() {
            x if x.ends_with(".owl") => OwlDialect::Owl,
            x if x.ends_with(".omn") => OwlDialect::Omn,
            _ => OwlDialect::Unknown,
        };

        if owl_dialect == OwlDialect::Omn {
            let tree = timeit("create_document / parse", || {
                parser
                    .parse(&text, None)
                    .expect("language to be set, no timeout to be used, no cancelation flag")
            });

            let rope = Rope::from(text);
            let diagnostics = timeit("create_document / gen_diagnostics", || {
                gen_diagnostics(&tree.root_node())
            });
            let mut document = InternalDocument {
                owl_dialect,
                uri,
                version,
                tree,
                rope,
                frame_infos: DashMap::new(),
                diagnostics,
            };
            document.frame_infos = document.gen_frame_infos(None);
            document
        } else {
            warn!("Only omn files are supported");
            InternalDocument {
                owl_dialect,
                uri,
                version,
                tree: parser
                    .parse(&text, None)
                    .expect("language to be set, no timeout to be used, no cancelation flag"),
                rope: Rope::from(text),

                frame_infos: DashMap::new(),
                diagnostics: vec![],
            }
        }
    }

    /// Generate an async hash map that resolves IRIs to infos about that IRI
    pub fn gen_frame_infos(&self, range: Option<&Range>) -> DashMap<Iri, FrameInfo> {
        if self.owl_dialect != OwlDialect::Omn {
            error!("Only omn files are supported for frame infos");
            return DashMap::new();
        }
        debug!("Generating frame infos");

        let tree = &self.tree;
        let rope = &self.rope;

        let mut query_cursor = QueryCursor::new();

        // if let Some(range) = range {
        //     query_cursor.set_point_range((*range).into());
        // }

        let matches = query_cursor.matches(
            &ALL_QUERIES.frame_info_query,
            tree.root_node(),
            RopeProvider::new(rope),
        );

        let infos = DashMap::<Iri, FrameInfo>::new();

        for m in matches {
            info!("Found match {:#?}", m);
            match m.pattern_index {
                0 => {
                    let mut capture_texts = m.captures.iter().map(|c| node_text(&c.node, rope));
                    let frame_iri = capture_texts.next().unwrap().to_string();
                    let annotation_iri = capture_texts.next().unwrap().to_string();
                    let literal = capture_texts.next().unwrap().to_string();

                    let parent_node = m.captures[0].node.parent().unwrap();

                    let frame_type = FrameType::parse(parent_node.kind());

                    debug!("Found frame {}", frame_iri);

                    if !infos.contains_key(&frame_iri) {
                        infos.insert(
                            frame_iri.clone(),
                            FrameInfo {
                                iri: frame_iri.clone(),
                                annotations: HashMap::new(),
                                frame_type,
                                definitions: vec![Location {
                                    uri: self.uri.clone(),
                                    // This node should be the total frame
                                    range: parent_node
                                        .parent()
                                        .unwrap_or(parent_node)
                                        .range()
                                        .into(),
                                }],
                            },
                        );
                    }

                    let mut info = infos.get_mut(&frame_iri).unwrap();

                    if let Some(vec) = info.annotations.get_mut(&annotation_iri) {
                        vec.push(literal);
                    } else {
                        info.annotations
                            .insert(annotation_iri.clone(), vec![literal]);
                    }
                }
                1 => {
                    let mut capture_texts = m.captures.iter().map(|c| node_text(&c.node, rope));
                    let prefix_name = capture_texts.next().unwrap().to_string();
                    let iri = capture_texts.next().unwrap().to_string();

                    // TODO requst prefix url to cache the ontology there
                    debug!("Prefix named {} with iri {}", prefix_name, iri);
                }
                2 => {
                    let mut capture_texts = m.captures.iter().map(|c| node_text(&c.node, rope));
                    let frame_iri = capture_texts.next().unwrap().to_string();

                    let specific_iri_node = m.captures[0].node.parent().unwrap();

                    let frame_type = FrameType::parse(specific_iri_node.kind());

                    let frame_node = m.captures[1].node;

                    debug!("Found frame {}", frame_iri);
                    if !infos.contains_key(&frame_iri) {
                        infos.insert(
                            frame_iri.clone(),
                            FrameInfo {
                                iri: frame_iri.clone(),
                                annotations: HashMap::new(),
                                frame_type,
                                definitions: vec![Location {
                                    uri: self.uri.clone(),
                                    range: frame_node.range().into(),
                                }],
                            },
                        );
                    }
                }
                i => todo!("pattern index {} not implemented", i),
            }
        }
        infos
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
        parser: &mut Parser,
    ) -> Vec<UnwrappedQueryMatch> {
        // Resolve the documents that are imported here
        // This also contains itself!
        let docs = self
            .reachable_docs_recusive(workspace, parser)
            .iter()
            .filter_map(|url| {
                workspace
                    .resolve_url_to_document(url, parser)
                    .inspect_err(|e| error!("{e}"))
                    .ok()
            })
            .collect_vec();

        info!("Query in documents with additional {}", docs.len());

        docs.iter().flat_map(|doc| doc.query(query)).collect_vec()
    }

    fn reachable_docs_recusive(&self, workspace: &Workspace, parser: &mut Parser) -> Vec<Url> {
        let mut set: HashSet<Url> = HashSet::new();
        self.reachable_docs_recursive_helper(workspace, parser, &mut set);
        set.into_iter().collect_vec()
    }

    fn reachable_docs_recursive_helper(
        &self,
        workspace: &Workspace,
        parser: &mut Parser,
        result: &mut HashSet<Url>,
    ) {
        if result.contains(&self.uri) {
            // Do nothing
            return;
        }

        result.insert(self.uri.clone());

        let docs = self
            .reachable_documents()
            .iter()
            .filter_map(|url| {
                workspace
                    .resolve_url_to_document(url, parser)
                    .inspect_err(|e| error!("{e}"))
                    .ok()
            })
            .collect_vec();

        for doc in docs {
            doc.reachable_docs_recursive_helper(workspace, parser, result);
        }
    }

    fn reachable_documents(&self) -> Vec<Url> {
        self.query(&ALL_QUERIES.import_query)
            .iter()
            .filter_map(|m| match &m.captures[..] {
                [iri] => {
                    Url::parse(iri.node.text.trim_end_matches(">").trim_start_matches("<")).ok()
                }
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
            .map(|m| UnwrappedQueryMatch {
                _pattern_index: m.pattern_index,
                _id: m.id(),
                captures: m
                    .captures
                    .iter()
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
}

#[derive(Debug)]
pub struct ExternalDocument {
    pub uri: Url,
    pub text: String,
    pub ontology: (SetOntology<String>, PrefixMapping),
    /// This can differ from the url file extention, so we need to track it
    pub owl_dialect: OwlDialect,
}

impl ExternalDocument {
    pub fn new(text: String, url: Url) -> Result<ExternalDocument> {
        let b = horned_owl::model::Build::new_string();
        let mut buffer = text.as_bytes();
        let ontology =
            horned_owl::io::owx::reader::read_with_build(&mut buffer, &b).map_err(|e| match e {
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
            })?;

        Ok(ExternalDocument {
            uri: url,
            text,
            ontology,
            owl_dialect: OwlDialect::Unknown,
        })
    }

    fn reachable_documents(&self) -> Vec<Url> {
        self.ontology
            .0
            .iter()
            .filter_map(|ac| match &ac.component {
                horned_owl::model::Component::Import(import) => Some(Url::parse(import.0.deref())),
                _ => None,
            })
            .filter_map(|r| r.ok())
            .collect_vec()
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

impl UnwrappedNode {
    pub fn text_trimmed(&self) -> String {
        trim_string_value(&self.text)
    }
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

#[derive(Clone, Debug)]
pub struct Location {
    uri: Url,
    range: Range,
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
        let mut annotations = b.annotations.clone();
        for (key_a, values_a) in a.annotations {
            if let Some(values_b) = annotations.get_mut(&key_a) {
                values_b.extend(values_a);
            } else {
                annotations.insert(key_a, values_a);
            }
        }
        let definitions = b
            .definitions
            .iter()
            .chain(a.definitions.iter())
            .cloned()
            .collect_vec();
        FrameInfo {
            iri: a.iri,
            frame_type: if a.frame_type == b.frame_type {
                a.frame_type
            } else {
                FrameType::Invalid
            },
            annotations,
            definitions,
        }
    }

    pub fn label(&self) -> String {
        self.annotations
            .get("rdfs:label")
            // TODO #20 make this more usable by providing multiple lines with indentation
            .map(|resolved| resolved.iter().map(trim_string_value).join(","))
            .unwrap_or("(rdfs:label missing)".into())
    }

    pub fn annoation_display(&self, iri: &Iri) -> Option<String> {
        self.annotations
            .get(iri)
            // TODO #20 make this more usable by providing multiple lines with indentation
            .map(|resolved| resolved.iter().map(trim_string_value).join(","))
    }

    pub fn info_display(&self, workspace: &Workspace) -> String {
        let entity = self.frame_type;
        let label = self.label();

        let annotations = self
            .annotations
            .keys()
            .map(|iri| {
                let iri_label = workspace
                    .get_frame_info(iri)
                    .map(|fi| fi.label())
                    .unwrap_or(iri.clone());
                // TODO #28 use values directly
                let annoation_display = self.annoation_display(iri).unwrap_or(iri.clone());
                format!("`{iri_label}`: {annoation_display}")
            })
            .join("  \n");

        format!("{entity} **{label}**\n\n---\n{annotations}")
    }
}

fn trim_string_value(value: &String) -> String {
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
    Invalid, // The frame type of an IRI that has no valid frame
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
        };
        write!(f, "{name}")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_log::test;

    #[test]
    fn external_document_new_given_text_does_parse_ontology() {
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
            Url::parse("https://example.com/onto").unwrap(),
        );

        // Assert
        let doc = external_doc.unwrap();
        assert_eq!(doc.text, ontology_text);
        let set_onto = &doc.ontology.0;
        assert_eq!(
            set_onto
                .i()
                .the_ontology_id_or_default()
                .iri
                .unwrap()
                .deref(),
            "http://www.example.com/iri"
        );
    }

    #[test]
    fn external_document_reachable_documents_given_imports_does_return_imports() {
        // Arrange
        let ontology_text = r#"
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
            ontology_text.clone(),
            Url::parse("https://example.com/onto").unwrap(),
        )
        .unwrap();

        // Act
        let urls = external_doc.reachable_documents();

        // Assert
        assert_eq!(
            urls,
            vec![
                Url::parse("http://www.example.com/other-property").unwrap(),
                Url::parse("file:///abosulte/file").unwrap()
            ]
        );
    }
}

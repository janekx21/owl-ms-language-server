use std::{
    collections::{HashMap, HashSet},
    ffi::OsStr,
    fmt::Display,
    fs,
};

use anyhow::anyhow;
use anyhow::Result;
use dashmap::DashMap;
use itertools::Itertools;
use log::{debug, error, info, warn};
use ropey::Rope;
use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, SymbolKind, Url, WorkspaceFolder};
use tree_sitter::{Node, Parser, Query, QueryCursor, Tree};

use crate::{
    catalog::{Catalog, CatalogUri},
    debugging::timeit,
    queries::ALL_QUERIES,
    range::Range,
    rope_provider::RopeProvider,
    NODE_TYPES,
};

#[derive(Debug)]
pub struct Workspace {
    pub document_map: DashMap<Url, Document>,
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
            document_map: DashMap::new(),
            workspace_folder,
            catalogs,
        }
    }

    pub fn load_catalog_documents(&self, parser: &mut Parser) {
        for catalog in &self.catalogs {
            for catalog_uri in catalog.all_catalog_uris() {
                match self.load_catalog_uri(catalog, catalog_uri, parser) {
                    Ok(_) => {}
                    Err(e) => {
                        error!("Load catalog documents error: {e}");
                    }
                }
            }
        }
    }

    fn load_catalog_uri(
        &self,
        catalog: &Catalog,
        catalog_uri: &CatalogUri,
        parser: &mut Parser,
    ) -> Result<()> {
        let path = catalog.parent_folder().join(catalog_uri.uri.clone());
        let file_url = Url::from_file_path(path).map_err(|_| anyhow!("Path is not absolute"))?;
        self.resolve_url_to_document(&file_url, parser)
            .ok_or(anyhow!("Could not resolve to document"))?;

        Ok(())
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

    pub fn insert_document(&self, document: Document) -> dashmap::mapref::one::Ref<Url, Document> {
        let uri = document.uri.clone();
        self.document_map.insert(uri.clone(), document);
        self.document_map.get(&uri).unwrap()
    }

    // TODO #28 maybe return a reference?
    pub fn search_frame(&self, partial_text: &str) -> Vec<(Iri, FrameInfo)> {
        self.document_map
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
        self.document_map
            .iter()
            .filter_map(|dm| dm.frame_infos.get(iri).map(|v| v.value().clone()))
            .tree_reduce(FrameInfo::merge)
    }

    pub fn node_info(&self, node: &Node, doc: &Document) -> String {
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
        self.document_map
            .iter()
            .flat_map(|doc| doc.query(query))
            .collect_vec()
    }

    pub fn resolve_url_to_document(
        &self,
        url: &Url,
        parser: &mut Parser,
    ) -> Option<dashmap::mapref::one::Ref<'_, Url, Document>> {
        if let Some(doc) = self.document_map.get(url) {
            // Document is loaded already
            return Some(doc);
        }

        for catalog in &self.catalogs {
            for catalog_uri in catalog.all_catalog_uris() {
                if catalog_uri.name == url.to_string() {
                    // Load document from disk
                    let path = catalog.parent_folder().join(&catalog_uri.uri);

                    if path.extension() != Some(OsStr::new("omn")) {
                        warn!("Non omn files can not be loaded. path {}", path.display());
                        return None;
                    }

                    return if let Ok(ontology_text) = fs::read_to_string(&path) {
                        let document = Document::new(
                            Url::from_file_path(&path).unwrap(),
                            -1,
                            ontology_text,
                            parser,
                        );
                        info!("Loaded file from disk at {}", path.display());
                        Some(self.insert_document(document))
                    } else {
                        error!("File could not be loaded at {}", path.display());
                        None
                    };
                }
            }
        }
        error!("The document at {url} could not be found in the workspace");
        None
    }
}

#[derive(Debug)]
pub struct Document {
    pub uri: Url,
    pub tree: Tree,
    pub rope: Rope,
    pub version: i32,
    pub diagnostics: Vec<Diagnostic>,
    pub frame_infos: DashMap<Iri, FrameInfo>,
}

impl Document {
    /// Arguments:
    /// - `uri` This must be a file url
    pub fn new(uri: Url, version: i32, text: String, parser: &mut Parser) -> Document {
        let tree = timeit("create_document / parse", || {
            parser
                .parse(&text, None)
                .expect("language to be set, no timeout to be used, no cancelation flag")
        });

        let rope = Rope::from(text);
        let diagnostics = timeit("create_document / gen_diagnostics", || {
            gen_diagnostics(&tree.root_node())
        });

        let mut document = Document {
            uri,
            version,
            tree,
            rope,
            frame_infos: DashMap::new(),
            diagnostics,
        };
        document.frame_infos = document.gen_frame_infos(None);
        document
    }

    /// Generate an async hash map that resolves IRIs to infos about that IRI
    pub fn gen_frame_infos(&self, range: Option<&Range>) -> DashMap<Iri, FrameInfo> {
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
            .filter_map(|url| workspace.resolve_url_to_document(url, parser))
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
            .filter_map(|url| workspace.resolve_url_to_document(url, parser))
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

impl Into<tower_lsp::lsp_types::Location> for Location {
    fn into(self) -> tower_lsp::lsp_types::Location {
        tower_lsp::lsp_types::Location {
            uri: self.uri,
            range: self.range.into(),
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

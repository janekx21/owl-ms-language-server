use crate::catalog::CatalogUri;
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
use itertools::Itertools;
use log::{debug, error, info, trace, warn};
use once_cell::sync::Lazy;
use parking_lot::{Mutex, MutexGuard, RwLock};
use ropey::Rope;
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
    SymbolKind, Url, WorkspaceFolder,
};
use tree_sitter::{InputEdit, Node, Parser, Point, Query, QueryCursor, Tree};

static GLOBAL_PARSER: Lazy<Mutex<Parser>> = Lazy::new(|| {
    let mut parser = Parser::new();
    parser.set_language(*LANGUAGE).unwrap();
    parser.set_logger(Some(Box::new(|type_, str| match type_ {
        tree_sitter::LogType::Parse => trace!(target: "tree-sitter-parse", "{}", str),
        tree_sitter::LogType::Lex => trace!(target: "tree-sitter-lex", "{}", str),
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
            "Insert internal document {} with {:?} length is {}",
            document.uri,
            document.owl_dialect,
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
            "Insert external document {} with {:?} length is {}",
            document.uri,
            document.owl_dialect,
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
                dm.frame_infos
                    .iter()
                    .filter(|item| item.key().contains(partial_text))
                    .map(|kv| (kv.key().clone(), kv.value().clone()))
                    .collect_vec()
            })
            .collect_vec()
    }

    /// This finds a frame info in the internal and external documents.
    ///
    /// - `iri` should be a full iri
    pub fn get_frame_info(&self, iri: &Iri) -> Option<FrameInfo> {
        debug!("Getting frame info for {iri}");

        let external_infos = self.external_documents.iter().flat_map(|doc| {
            let mut doc = doc.write();
            doc.get_frame_info(iri)
        });

        // TODO #37 replace this with a query
        let internal_infos = self.internal_documents.iter().filter_map(|dm| {
            let dm = dm.value().read();

            dm.get_frame_info(iri)
            // dm.frame_infos.get(iri).map(|v| v.value().clone())
        });

        let fixed_infos = match &iri[..] {
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
        };

        internal_infos
            .chain(external_infos)
            .chain(fixed_infos)
            .tree_reduce(FrameInfo::merge)
    }

    pub fn node_info(
        &self,
        node: &Node,
        doc: &InternalDocument,
        http_client: &dyn HttpClient,
    ) -> String {
        match node.kind() {
            "class_frame" | "annotation_property_frame" | "class_iri" => {
                // Goto first named child and repeat
                if let Some(iri_node) = &node.named_child(0) {
                    self.node_info(iri_node, doc, http_client)
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

            _ => format!("generic node named {}", node.kind()),
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

#[derive(Debug, PartialEq, Eq)]
pub enum OwlDialect {
    Unknown,
    Omn,
    Owl,
    Rdf,
}

#[derive(Debug)]
pub struct InternalDocument {
    pub uri: Url,
    pub tree: Tree,
    pub rope: Rope,
    pub version: i32,
    pub diagnostics: Vec<Diagnostic>,
    #[deprecated = "Will not be filled with data. Query the AST directly with the query functions"]
    pub frame_infos: DashMap<Iri, FrameInfo>,

    /// This can differ from the url file extention, so we need to track it
    pub owl_dialect: OwlDialect,
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
        let owl_dialect = match &uri.path() {
            x if x.ends_with(".owl") => OwlDialect::Owl,
            x if x.ends_with(".omn") => OwlDialect::Omn,
            _ => OwlDialect::Unknown,
        };

        if owl_dialect == OwlDialect::Omn {
            let tree = timeit("create_document / parse", || {
                lock_global_parser()
                    .parse(&text, None)
                    .expect("language to be set, no timeout to be used, no cancelation flag")
            });

            let rope = Rope::from(text);
            let diagnostics = timeit("create_document / gen_diagnostics", || {
                gen_diagnostics(&tree.root_node())
            });
            let mut document = InternalDocument {
                uri,
                owl_dialect,
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
                uri,
                owl_dialect,
                version,
                tree: lock_global_parser()
                    .parse(&text, None)
                    .expect("language to be set, no timeout to be used, no cancelation flag"),
                rope: Rope::from(text),

                frame_infos: DashMap::new(),
                diagnostics: vec![],
            }
        }
    }

    /// Generate an async hash map that resolves IRIs to infos about that IRI
    #[deprecated]
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
            // info!("Found match {:#?}", m);
            match m.pattern_index {
                0 => {
                    let mut capture_texts = m.captures.iter().map(|c| node_text(&c.node, rope));
                    let frame_iri = capture_texts.next().unwrap().to_string();
                    let annotation_iri = trim_full_iri(capture_texts.next().unwrap());
                    let annotation_iri = self.abbreviated_iri_to_full_iri(annotation_iri);
                    let literal = capture_texts.next().unwrap().to_string();

                    let parent_node = m.captures[0].node.parent().unwrap();

                    let frame_type = FrameType::parse(parent_node.kind());

                    // debug!("Found frame {}", frame_iri);

                    // Make this IRI absolute
                    let frame_iri = if frame_iri.starts_with("<") {
                        trim_full_iri(frame_iri)
                    } else {
                        self.abbreviated_iri_to_full_iri(frame_iri)
                    };

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
                    let frame_iri = capture_texts.next().unwrap().to_string();

                    let specific_iri_node = m.captures[0].node.parent().unwrap();

                    let frame_type = FrameType::parse(specific_iri_node.kind());

                    let frame_node = m.captures[1].node;

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

        info!("Query in documents with additional {}", docs.len());

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
        debug!("Add reachable {} (internal)", self.uri);

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

        debug!("Change ranges {:#?}", change_ranges);

        let rope_provider = RopeProvider::new(&self.rope);

        let tree = {
            let mut parser_guard = lock_global_parser();
            timeit("parsing", || {
                parser_guard
                    .parse_with(
                        &mut |byte_idx, _| rope_provider.chunk_callback(byte_idx),
                        Some(&self.tree),
                    )
                    .expect("language to be set, no timeout to be used, no cancelation flag")
            })
        };
        debug!("New tree {}", tree.root_node().to_sexp());
        self.tree = tree;

        for (_, _, new_range) in change_ranges.iter() {
            // TODO #32 prune
            // document
            //     .frame_infos
            //     .retain(|k, v| !range_overlaps(&v.range.into(), &range));

            let frame_infos = timeit("gen_class_iri_label_map", || {
                self.gen_frame_infos(Some(new_range))
            });

            self.frame_infos.extend(frame_infos);
        }

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
        timeit("abbreviated_iri_to_full_iri", || {
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

            // prefixes
            //     .into_iter()
            //     .filter_map(|(name, iri)| {
            //         abbriviated_iri.split_once(&name).and_then(|(pre, post)|
            //     // pre must be empty because the prefix is at the start
            //     if pre.is_empty() {
            //         Some(iri + post)
            //     } else {
            //         None
            //     }
            //     )
            //     })
            //     .next()
            //     .unwrap_or(abbriviated_iri) // Will return the simple IRI with colon
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

    /// Returns the prefixes of a document
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
        timeit("prefixes", || prefixes_helper(self))
    }

    pub fn inlay_hint(
        &self,
        range: Range,
        workspace: &Workspace,
        http_client: &dyn HttpClient,
    ) -> Vec<InlayHint> {
        // let annotations = timeit("Annotation query", || {
        //     self.query_with_imports(&ALL_QUERIES.annotation_query, workspace, http_client)
        // });

        let hints = self
            .query_range(&ALL_QUERIES.iri_query, range)
            .into_iter()
            .flat_map(|match_| match_.captures)
            .filter_map(|capture| {
                let iri = trim_full_iri(capture.node.text);
                let iri = self.abbreviated_iri_to_full_iri(iri);

                let label =
                    // annotations
                    // .iter()
                    // .filter_map(|m| match &m.captures[..] {
                    //     [frame_iri, annoation_iri, literal] => {
                    //         // TODO no full iri :<

                    //         let annoation_iri = self.abbreviated_iri_to_full_iri(trim_full_iri(
                    //             annoation_iri.node.text.clone(),
                    //         ));
                    //         if frame_iri.node.text == iri
                    //             && annoation_iri == "http://www.w3.org/2000/01/rdf-schema#label"
                    //         {
                    //             Some(literal.node.text_trimmed())
                    //         } else {
                    //             None
                    //         }
                    //     }
                    //     _ => unreachable!(),
                    // })
                    // .chain(
                        // Chain the frame info label from the workspace
                        timeit("Workspace get frame info", || {
                            workspace.get_frame_info(&iri)
                        })
                        .inspect(|fi| debug!("Found frame info {fi:#?}"))
                        .map(|frame_info| frame_info.label())
                        .unwrap_or_default();
                // .into_iter()
                // )
                // .unique()
                // .join(", ");

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
            .collect_vec();
        hints
    }

    pub fn get_frame_info(&self, iri: &Iri) -> Option<FrameInfo> {
        // TODO query
        self.frame_infos.get(iri).map(|v| v.value().clone())
    }
}

pub struct ExternalDocument {
    pub uri: Url,
    pub text: String,
    pub ontology: ArcIRIMappedOntology,
    /// This can differ from the url file extention, so we need to track it
    pub owl_dialect: OwlDialect,
}

impl Hash for ExternalDocument {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.uri.hash(state);
    }
}

impl fmt::Debug for ExternalDocument {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "ExternalDocument {{uri: {}, text.len(): {}}}, ontology: ..., owl_dialect: {:?}}}",
            self.uri,
            self.text.len(),
            self.owl_dialect
        )
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
                    owl_dialect: OwlDialect::Owl,
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
                    owl_dialect: OwlDialect::Rdf,
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
        debug!("Add reachable {} (external)", self.uri);

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

    pub fn abbreviated_iri_to_full_iri(&self, abbriviated_iri: String) -> String {
        format!(
            "{}#{}",
            self.uri.to_string().trim_end_matches('#'),
            abbriviated_iri
        )
    }

    pub fn get_frame_info(&mut self, iri: &Iri) -> Vec<FrameInfo> {
        timeit("Get frame infos", || get_frame_info_helper(self, iri))

        //     let iri_mapped_ontology = &mut self.ontology;

        //     let build = lock_global_build_arc();
        //     let iri = build.iri(iri.clone());

        //     iri_mapped_ontology
        //         .components_for_iri(&iri)
        //         .filter_map(|c| match &c.component {
        //             AnnotationAssertion(aa) => match &aa.subject {
        //                 horned_owl::model::AnnotationSubject::IRI(iri_) if iri_ == &iri => {
        //                     Some(aa.ann.clone())
        //                 }
        //                 _ => None,
        //             },
        //             _ => None,
        //         })
        //         .filter_map(|annotation| match annotation.av {
        //             horned_owl::model::AnnotationValue::Literal(
        //                 horned_owl::model::Literal::Simple { literal },
        //             ) => {
        //                 let annotations = once((annotation.ap.0.to_string(), vec![literal])).collect();
        //                 Some(FrameInfo {
        //                     iri: iri.to_string(),
        //                     annotations,
        //                     frame_type: FrameType::Class,
        //                     definitions: vec![],
        //                 })
        //             }
        //             _ => None,
        //         })
        //         .collect_vec()
    }
}

#[cached(
    ty = "SizedCache<u64, Vec<FrameInfo>>",
    create = "{ SizedCache::with_size(200) }",
    convert = r#"{
            let mut hasher = DefaultHasher::new();
            doc.hash(&mut hasher);
            iri.hash(&mut hasher);
            hasher.finish()
     } "#
)]
fn get_frame_info_helper(doc: &mut ExternalDocument, iri: &Iri) -> Vec<FrameInfo> {
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
                    definitions: vec![],
                })
            }
            _ => None,
        })
        .collect_vec()
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
        self.annoation_display(&"http://www.w3.org/2000/01/rdf-schema#label".to_string())
            .unwrap_or(format!("{} (no label)", self.iri))
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
    fn external_document_abbreviated_iri_to_full_iri_should_convert_simple_iri() {
        let doc = ExternalDocument::new(
            "".into(),
            Url::parse("http://www.w3.org/2002/07/owl#").unwrap(),
        )
        .unwrap();

        let full_iri = doc.abbreviated_iri_to_full_iri("Nothing".into());

        assert_eq!(full_iri, "http://www.w3.org/2002/07/owl#Nothing");
    }

    #[test]
    fn external_document_abbreviated_iri_to_full_iri_should_convert_simple_iri_with_missing_fragment(
    ) {
        let doc = ExternalDocument::new(
            "".into(),
            Url::parse("http://www.w3.org/2002/07/owl").unwrap(),
        )
        .unwrap();

        let full_iri = doc.abbreviated_iri_to_full_iri("Nothing".into());

        assert_eq!(full_iri, "http://www.w3.org/2002/07/owl#Nothing");
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
}

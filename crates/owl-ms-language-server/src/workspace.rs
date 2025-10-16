use crate::catalog::CatalogUri;
use crate::consts::{get_fixed_infos, keyword_hover_info};
use crate::error::{Error, Result, ResultExt, ResultIterator, RwLockExt};
use crate::pos::Position;
use crate::queries::{self, treesitter_highlight_capture_into_semantic_token_type_index};
use crate::web::HttpClient;
use crate::{
    catalog::Catalog, debugging::timeit, queries::ALL_QUERIES, range::Range,
    rope_provider::RopeProvider, LANGUAGE, NODE_TYPES,
};
use cached::proc_macro::cached;
use cached::SizedCache;
use dashmap::DashMap;
use horned_owl::model::Component::AnnotationAssertion;
use horned_owl::model::{AnnotationSubject, AnnotationValue, ArcStr, Build, Literal};
use horned_owl::ontology::set::SetOntology;
use itertools::Itertools;
use log::{debug, error, info, trace, warn};
use parking_lot::{Mutex, MutexGuard, RwLock};
use pretty::RcDoc;
use ropey::Rope;
use sophia::api::graph::{Graph, MutableGraph};
use sophia::api::prelude::Any;
use sophia::api::source::TripleSource;
use sophia::api::term::{BnodeId, LanguageTag, SimpleTerm, Term};
use sophia::api::MownStr;
use sophia::inmem::graph::LightGraph;
use sophia::iri::IriRef;
use std::fmt::Debug;
use std::hash::{DefaultHasher, Hash, Hasher};
use std::iter::once;
use std::path::Path;
use std::sync::{LazyLock, Weak};
use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    fs,
    path::PathBuf,
    sync::Arc,
};
use tower_lsp::lsp_types::{
    DidChangeTextDocumentParams, InlayHint, InlayHintLabel, PositionEncodingKind, SemanticToken,
    SymbolKind, Url, WorkspaceFolder,
};
use tree_sitter_c2rust::{InputEdit, Node, Parser, Query, QueryCursor, StreamingIterator, Tree};

static GLOBAL_PARSER: LazyLock<Mutex<Parser>> = LazyLock::new(|| {
    let mut parser = Parser::new();
    parser
        .set_language(&LANGUAGE)
        .expect("the language to be valid");
    parser.set_logger(Some(Box::new(|type_, str| match type_ {
        tree_sitter_c2rust::LogType::Parse => trace!(target: "tree-sitter-parse", "{str}"),
        tree_sitter_c2rust::LogType::Lex => trace!(target: "tree-sitter-lex", "{str}"),
    })));

    Mutex::new(parser)
});

pub fn lock_global_parser() -> MutexGuard<'static, Parser> {
    (*GLOBAL_PARSER).lock()
}

static GLOBAL_BUILD_ARC: LazyLock<Mutex<Build<ArcStr>>> = LazyLock::new(|| {
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
    pub folder: WorkspaceFolder,
    pub catalogs: Vec<Catalog>,
}

impl Workspace {
    pub fn new(workspace_folder: WorkspaceFolder) -> Self {
        let catalogs = Catalog::load_catalogs_recursive(&workspace_folder.uri);
        info!(
            "New workspace {} at {} with catalogs {catalogs:?}",
            workspace_folder.name, workspace_folder.uri
        );
        Workspace {
            internal_documents: DashMap::new(),
            external_documents: DashMap::new(),
            folder: workspace_folder,
            catalogs,
        }
    }

    pub fn insert_internal_document(
        workspace: &Arc<RwLock<Workspace>>,
        document: InternalDocument,
    ) -> Arc<RwLock<InternalDocument>> {
        debug!(
            "Insert internal document {} length is {}",
            document.uri,
            document.rope.len_chars()
        );
        let uri = document.uri.clone();
        let arc = Arc::new(RwLock::new(document));
        let mut doc = arc.write();
        doc.workspace = Arc::downgrade(workspace);
        let workspace = workspace.read();
        workspace.internal_documents.insert(uri, arc.clone());
        arc.clone()
    }

    pub fn get_internal_document(&self, url: &Url) -> Result<Arc<RwLock<InternalDocument>>> {
        self.internal_documents
            .get(url)
            .map(|d| d.clone())
            .ok_or(Error::DocumentNotFound(url.clone()))
    }

    pub fn insert_external_document(
        &self,
        document: ExternalDocument,
    ) -> Arc<RwLock<ExternalDocument>> {
        debug!(
            "Insert external document {} length is {} into workspace at {}",
            document.uri,
            document.text.len(),
            self.folder.uri
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
                dm.all_frame_infos()
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
        debug!(
            "getting workspace frame info for {iri} on {}",
            self.folder.uri
        );
        let external_infos = self.external_documents.iter().filter_map(|doc| {
            let mut doc = doc.write();
            doc.get_frame_info(iri)
        });

        let internal_infos = self.internal_documents.iter().filter_map(|dm| {
            let dm = dm.value().read();
            dm.frame_info_by_iri(iri)
        });

        internal_infos
            .chain(external_infos)
            .chain(get_fixed_infos(iri))
            .tree_reduce(FrameInfo::merge)
    }

    pub fn get_frame_info_recursive(
        workspace: &Arc<RwLock<Workspace>>,
        iri: &Iri,
        doc: &InternalDocument,
        http_client: &dyn HttpClient,
    ) -> Option<FrameInfo> {
        timeit("rechable docs", || doc.reachable_docs_recusive(http_client))
            .iter()
            .filter_map(|url| {
                if let Ok(doc) = timeit("\tresolve document", || {
                    Workspace::resolve_url_to_document(workspace, url, http_client)
                }) {
                    match &doc {
                        DocumentReference::Internal(rw_lock) => {
                            timeit("\t|-> get frame info internal", || {
                                rw_lock.read().frame_info_by_iri(iri)
                            })
                        }
                        DocumentReference::External(rw_lock) => {
                            timeit("\t|-> get frame info external", || {
                                rw_lock.write().get_frame_info(iri)
                            })
                        }
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
                let iri = doc
                    .abbreviated_iri_to_full_iri(&iri)
                    .unwrap_or(iri.to_string());
                self.get_frame_info(&iri)
                    .map(|fi| fi.info_display(self))
                    .unwrap_or(iri)
            }
            kind => keyword_hover_info(kind),
        }
    }

    pub fn find_catalog_uri(&self, url: &Url) -> Option<(&Catalog, &CatalogUri)> {
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
        workspace: &Arc<RwLock<Workspace>>,
        url: &Url,
        client: &dyn HttpClient,
    ) -> Result<DocumentReference> {
        let w = workspace.read();
        if let Some(doc) = w.get_document_by_url(url) {
            return Ok(doc);
        }

        let ws = workspace.read();
        let Some((catalog, catalog_uri)) = ws.find_catalog_uri(url) else {
            warn!("Url {url} could not be found in any catalog");
            let document_text = client.get(url.as_str())?;
            let document = ExternalDocument::new(document_text, url.clone())?;
            let doc = workspace.read().insert_external_document(document);
            return Ok(DocumentReference::External(doc));
        };

        let url_from_catalog = Url::parse(&catalog_uri.uri);

        if let Ok(url) = url_from_catalog {
            if let Some(doc) = workspace.read().get_document_by_url(&url) {
                return Ok(doc);
            }

            if let Ok(path) = url.to_file_path() {
                // This is an abolute file path url
                Workspace::resolve_path_to_document(workspace, &path)
            } else {
                // This is an external url
                let document_text = client.get(url.as_str())?;
                let document = ExternalDocument::new(document_text, url)?;
                let doc = workspace.read().insert_external_document(document);
                Ok(DocumentReference::External(doc))
            }
        } else {
            // This is a relative file path
            let path = catalog.parent_folder().join(&catalog_uri.uri);
            let url =
                Url::from_file_path(&path).map_err(|()| Error::InvalidFilePath(path.clone()))?;
            if let Some(doc) = workspace.read().get_document_by_url(&url) {
                return Ok(doc);
            }

            Workspace::resolve_path_to_document(workspace, &path)
        }
    }

    /// Resolves a URL (file or http/https protocol) to a document that is inserted into this workspace
    pub fn resolve_prefix_url_to_document(
        workspace: &Arc<RwLock<Workspace>>,
        url: &Url,
        client: &dyn HttpClient,
    ) -> Result<DocumentReference> {
        let w = workspace.read();
        if let Some(doc) = w.get_document_by_url(url) {
            return Ok(doc);
        }

        let document_text = client.get(url.as_str())?;
        let document = ExternalDocument::new(document_text, url.clone())?;
        let document = w.insert_external_document(document);
        Ok(DocumentReference::External(document))
    }

    fn resolve_path_to_document(
        workspace: &Arc<RwLock<Workspace>>,
        path: &Path,
    ) -> Result<DocumentReference> {
        let (document_text, document_url) = load_file_from_disk(path.to_path_buf())?;

        match path
            .extension()
            .and_then(|s| s.to_str())
            .unwrap_or_default()
        {
            "omn" => {
                let document = InternalDocument::new(document_url, -1, document_text);
                let doc = Workspace::insert_internal_document(workspace, document);
                Ok(DocumentReference::Internal(doc))
            }
            "owl" | "owx" => {
                let document = ExternalDocument::new(document_text, document_url)?;
                let workspace = workspace.read_timeout()?;
                let doc = workspace.insert_external_document(document);
                Ok(DocumentReference::External(doc))
            }
            ext => Err(Error::DocumentNotSupported(ext.to_string())),
        }
    }
}

fn load_file_from_disk(path: PathBuf) -> Result<(String, Url)> {
    info!("Loading file from disk {}", path.display());

    Ok((
        fs::read_to_string(&path)?,
        Url::from_file_path(&path).map_err(|()| Error::InvalidFilePath(path))?,
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
    pub diagnostics: Vec<(Range, String)>, // Not using the LSP type
    pub workspace: Weak<RwLock<Workspace>>,
}

impl core::hash::Hash for InternalDocument {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.uri.hash(state);
        Hash::hash(&self.version, state);
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
            workspace: Weak::new(),
        }
    }

    pub fn formatted(&self, tab_size: u32, ruler_width: usize) -> String {
        let root = self.tree.root_node();
        let doc = to_doc(&root, &self.rope, tab_size);
        debug!("doc:\n{doc:#?}");
        doc.pretty(ruler_width).to_string()
    }

    pub fn query(&self, query: &Query) -> Vec<UnwrappedQueryMatch> {
        self.query_helper(query, None)
    }

    pub fn query_range(&self, query: &Query, range: Range) -> Vec<UnwrappedQueryMatch> {
        self.query_helper(query, Some(range))
    }

    pub fn node_by_id(&self, id: usize) -> Option<Node<'_>> {
        let mut w = self.tree.walk();
        loop {
            if w.node().id() == id {
                return Some(w.node());
            }

            // In order triversal
            if !w.goto_first_child() {
                while !w.goto_next_sibling() {
                    if !w.goto_parent() {
                        return None;
                    }
                }
            }
        }
    }

    fn reachable_docs_recusive(&self, http_client: &dyn HttpClient) -> Vec<Url> {
        let mut set: HashSet<Url> = HashSet::new();
        self.reachable_docs_recursive_helper(&mut set, http_client)
            .log_if_error();
        set.into_iter().collect_vec()
    }

    fn reachable_docs_recursive_helper(
        &self,
        result: &mut HashSet<Url>,
        http_client: &dyn HttpClient,
    ) -> Result<()> {
        if result.contains(&self.uri) {
            // Do nothing
            return Ok(());
        }

        result.insert(self.uri.clone());

        let docs = timeit("\timports query", || self.imports())
            .iter()
            .filter_map(|url| {
                timeit("\t resolve url to doc", || {
                    Workspace::resolve_url_to_document(&self.try_get_workspace()?, url, http_client)
                })
                .inspect_log()
                .ok()
            })
            .collect_vec();

        for doc in docs {
            match doc {
                DocumentReference::Internal(internal_document) => internal_document
                    .read()
                    .reachable_docs_recursive_helper(result, http_client)?,
                DocumentReference::External(e) => e.read().reachable_docs_recursive_helper(
                    &self.try_get_workspace()?,
                    result,
                    http_client,
                )?,
            }
        }
        Ok(())
    }

    pub fn imports(&self) -> Vec<Url> {
        imports_helper(self)
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
                        index: c.index,
                    })
                    .collect_vec(),
            })
            .collect_vec()
    }

    pub fn edit(
        &mut self,
        params: &DidChangeTextDocumentParams,
        enconding: &PositionEncodingKind,
    ) -> Result<()> {
        if self.version >= params.text_document.version {
            return Ok(()); // no change needed
        }

        if params
            .content_changes
            .iter()
            .any(|change| change.range.is_none())
        {
            // Change the whole file
            return Err(Error::LspFeatureNotSupported(
                "Whole file (null range) change event",
            ));
        }

        debug!("content changes {:#?}", params.content_changes);

        // This range is relative to the *old* document not the new one
        for change in &params.content_changes {
            let range = change.range.expect("range to be defined");
            // LSP ranges are in bytes when encoding is utf-8!!!
            let old_range: Range = Range::from_lsp(&range, &self.rope, enconding)?;
            let start_byte = old_range.start.byte_index(&self.rope);
            let old_end_byte = old_range.end.byte_index(&self.rope);

            // must come before the rope is changed!
            let start_char = self.rope.try_byte_to_char(start_byte)?;
            let old_end_char = self.rope.try_byte_to_char(old_end_byte)?;

            debug!(
                "change range in chars {start_byte}..{old_end_byte} og range {range:?} and text {}",
                change.text
            );

            // rope replace
            self.rope.try_remove(start_char..old_end_char)?;
            self.rope.try_insert(start_char, &change.text)?;

            // this must come after the rope was changed!
            let new_end_byte = start_byte + change.text.len();
            let new_end_position = Position::new_from_byte_index(&self.rope, new_end_byte);

            let edit = InputEdit {
                start_byte,
                old_end_byte,
                new_end_byte,
                start_position: old_range.start.into(),
                old_end_position: old_range.end.into(),
                new_end_position: new_end_position.into(),
            };
            timeit("tree edit", || self.tree.edit(&edit));

            self.version = params.text_document.version;
        }

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
        self.tree = tree;

        // TODO #30 prune diagnostics with
        // Remove all old diagnostics with an overlapping range. They will need to be recreated
        // Move all other diagnostics

        self.diagnostics = timeit("did_change > gen_diagnostics", || {
            gen_diagnostics(&self.tree.root_node())
        });

        Ok(())
    }

    pub fn abbreviated_iri_to_full_iri(&self, abbriviated_iri: &str) -> Option<String> {
        let prefixes = self.prefixes();
        if let Some((prefix, simple_iri)) = abbriviated_iri.split_once(':') {
            prefixes
                .get(prefix)
                .map(|resolved_prefix| resolved_prefix.clone() + simple_iri)
        } else {
            // Simple IRIs get a free colon prependet
            // ref: https://www.w3.org/TR/owl2-manchester-syntax/#IRIs.2C_Integers.2C_Literals.2C_and_Entities
            prefixes
                .get("")
                .map(|resolved_prefix| resolved_prefix.clone() + abbriviated_iri)
        }
    }

    pub fn full_iri_to_abbreviated_iri(&self, full_iri: &str) -> Option<String> {
        self.prefixes()
            .into_iter()
            .filter_map(|(prefix, url)| match full_iri.split_once(&url) {
                Some(("", post)) if prefix.is_empty() => Some(post.to_string()),
                Some(("", post)) => Some(prefix + ":" + post),
                Some(_) | None => None,
            })
            .sorted_by_key(String::len) // short IRI's are preferred
            .next()
    }

    /// Returns the prefixes of a document (without colon :) in a prefix name to iri map.
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

    pub fn try_get_workspace(&self) -> Result<Arc<RwLock<Workspace>>> {
        self.workspace
            .upgrade()
            .ok_or(Error::WorkspaceNotFound(self.uri.clone()))
    }

    pub fn inlay_hint(
        &self,
        range: Range,
        http_client: &dyn HttpClient,
        enconding: &PositionEncodingKind,
    ) -> Result<Vec<InlayHint>> {
        let workspace = self.try_get_workspace()?;
        Ok(self
            .query_range(&ALL_QUERIES.iri_query, range)
            .into_iter()
            .flat_map(|match_| match_.captures)
            .map(|capture| {
                let iri = trim_full_iri(capture.node.text);
                let iri = self.abbreviated_iri_to_full_iri(&iri).unwrap_or(iri);

                let label = timeit("get frame info recursive", || {
                    Workspace::get_frame_info_recursive(&workspace, &iri, self, http_client)
                })
                .and_then(|frame_info| frame_info.label())
                .unwrap_or_default();

                let mut label_normalized = label.clone().to_lowercase();
                label_normalized.retain(char::is_alphanumeric);

                let same = iri.to_lowercase().contains(&label_normalized);

                if label.is_empty() || same {
                    Ok(None)
                } else {
                    Ok(Some(InlayHint {
                        position: capture.node.range.end.into_lsp(&self.rope, enconding)?,
                        label: InlayHintLabel::String(label),
                        kind: None,
                        text_edits: None,
                        tooltip: None,
                        padding_left: Some(true),
                        padding_right: None,
                        data: None,
                    }))
                }
            })
            .filter_and_log()
            .flatten()
            .collect())
    }

    pub fn frame_info_by_iri(&self, iri: &Iri) -> Option<FrameInfo> {
        get_frame_info_helper(self, iri)
    }

    pub fn all_frame_infos(&self) -> Vec<FrameInfo> {
        document_all_frame_infos(self)
            .values()
            .cloned()
            .collect_vec()
    }

    pub fn try_keywords_at_position(&self, cursor: Position) -> Vec<String> {
        let mut parser = lock_global_parser();
        let rope = self.rope.clone();
        let tree = self.tree.clone();

        let line = rope
            .get_line(cursor.line() as usize)
            .map(|s| s.to_string())
            .unwrap_or_default();
        let partial = word_before_character(cursor.character_byte() as usize, &line);

        debug!("Cursor node text is {partial:?}");

        let keywords = &*queries::KEYWORDS;

        let kws = keywords
            .iter()
            .filter(|k| k.starts_with(&partial))
            .collect_vec();

        debug!("Checking {} keywords", kws.len());

        kws.iter()
            .map(|kw| {
                let mut rope_version = rope.clone();
                let change = kw[partial.len()..].to_string() + " a";

                let mut tree = tree.clone(); // This is fast

                // Must come before the rope is changed!
                let cursor_byte_index = cursor.byte_index(&rope_version);

                rope_version.insert(cursor.char_index(&rope_version), &change);

                // Must come after rope changed!
                let new_end_byte = cursor_byte_index + change.len();
                let new_end_position = Position::new_from_byte_index(&rope_version, new_end_byte);

                let edit = InputEdit {
                    // Old range is just a zero size range
                    start_byte: cursor_byte_index,
                    start_position: cursor.into(),
                    old_end_byte: cursor_byte_index,
                    old_end_position: cursor.into(),

                    new_end_byte,
                    new_end_position: new_end_position.into(),
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

                let cursor_one_left = cursor.moved_left(1, &rope);
                let cursor_node_version = new_tree
                    .root_node()
                    .named_descendant_for_point_range(
                        cursor_one_left.into(),
                        cursor_one_left.into(),
                    )
                    .ok_or(Error::PositionOutOfBounds(cursor_one_left))?;

                debug!("{cursor_node_version:#?} is {}", cursor_node_version.kind());

                if cursor_node_version.kind().starts_with("keyword_")
                    && !cursor_node_version
                        .parent()
                        .expect("keyword to have parent")
                        .is_error()
                {
                    debug!("Found possible keyword {kw}!");
                    Ok(Some((*kw).to_string()))
                } else {
                    debug!("{kw} is not possible");
                    Ok(None)
                }
            })
            .filter_map_ok(|x| x)
            .filter_and_log()
            .collect_vec()
    }

    pub fn sematic_tokens(
        &self,
        range: Option<Range>,
        encoding: &PositionEncodingKind,
    ) -> Result<Vec<SemanticToken>> {
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

        let mut last_line = 0;
        let mut last_character = 0; // the indexing is encoding dependent
        for (node, type_index) in nodes {
            let range: Range = node.range().into();
            let range = range.into_lsp(&self.rope, encoding)?;
            let start = range.start;

            let delta_line = start.line - last_line;
            let delta_start = if delta_line == 0 {
                start.character - last_character // same line
            } else {
                start.character // some other line
            };

            if range.start.line != range.end.line {
                return Err(Error::LspFeatureNotSupported("Multi line highlights"));
            }
            let length = range.end.character - range.start.character;

            let token = SemanticToken {
                delta_line,
                delta_start,
                length,
                token_type: type_index,
                token_modifiers_bitset: 0,
            };

            last_line = start.line;
            last_character = start.character;
            tokens.push(token);
        }
        Ok(tokens)
    }
}

/// Returns the word before the [`character`] position in the [`line`]
pub fn word_before_character(byte_index: usize, line: &str) -> String {
    line.get(..byte_index)
        .map(|s| {
            s.chars()
                .rev()
                .take_while(|c| c.is_alphabetic())
                .collect_vec()
                .iter()
                .rev()
                .collect()
        })
        .unwrap_or_default()
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
fn imports_helper(doc: &InternalDocument) -> Vec<Url> {
    doc.query(&ALL_QUERIES.import_query)
        .iter()
        .filter_map(|m| match &m.captures[..] {
            [iri] => Url::parse(&trim_full_iri(iri.node.text.clone())[..]).ok(),
            _ => unimplemented!(),
        })
        .collect_vec()
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
    let mut frame_infos: HashMap<String, FrameInfo> = HashMap::new();

    // First we collect the annotations
    for frame_info in
        document_annotations(doc)
            .into_iter()
            .map(|(frame_iri, annoation_iri, literal)| FrameInfo {
                iri: frame_iri.clone(),
                annotations: HashMap::from([(annoation_iri, vec![literal])]),
                frame_type: FrameType::Unknown,
                definitions: Vec::new(),
            })
    {
        if let Some(frame_info_mut) = frame_infos.get_mut(&frame_info.iri) {
            // Merge the frame info for the same IRI
            frame_info_mut.extend(frame_info);
        } else {
            frame_infos.insert(frame_info.iri.clone(), frame_info);
        }
    }

    // Second we collect the location and frame type (definitions)
    for frame_info in document_definitions(doc)
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
        if let Some(frame_info_mut) = frame_infos.get_mut(&frame_info.iri) {
            // Merge the frame info for the same IRI
            frame_info_mut.extend(frame_info);
        } else {
            frame_infos.insert(frame_info.iri.clone(), frame_info);
        }
    }

    frame_infos
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
}

fn document_annotations(doc: &InternalDocument) -> Vec<(String, String, String)> {
    doc.query(&ALL_QUERIES.annotation_query)
        .iter()
        .map(|m| match &m.captures[..] {
            [frame_iri, annoation_iri, literal] => {
                let iri = trim_full_iri(frame_iri.node.text.clone());
                let frame_iri = doc.abbreviated_iri_to_full_iri(&iri).unwrap_or(iri);
                let iri = trim_full_iri(annoation_iri.node.text.clone());
                let annoation_iri = doc.abbreviated_iri_to_full_iri(&iri).unwrap_or(iri);
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
                let iri = trim_full_iri(frame_iri.node.text.clone());
                let frame_iri = doc.abbreviated_iri_to_full_iri(&iri).unwrap_or(iri);

                (frame_iri, frame.node.range, frame.node.kind.clone())
            }
            _ => unreachable!(),
        })
        .collect()
}

/// External documents are ontologies that are not expected to change in any way.
#[derive(Debug)]
pub struct ExternalDocument {
    pub uri: Url,
    pub text: String,
    pub graph: InfoGraph,
}

#[derive(Debug)]
pub struct InfoGraph(LightGraph);

impl From<SetOntology<ArcStr>> for InfoGraph {
    fn from(value: SetOntology<ArcStr>) -> Self {
        let mut graph = LightGraph::new();

        let ontology_iri = &value.iter().find_map(|ac| match &ac.component {
            horned_owl::model::Component::OntologyID(horned_owl::model::OntologyID {
                iri: Some(id),
                viri: _,
            }) => Some(SimpleTerm::Iri(IriRef::new(MownStr::from_ref(id)).unwrap())),
            _ => None,
        });

        for ac in &value {
            match &ac.component {
                AnnotationAssertion(aa) => match &aa.subject {
                    AnnotationSubject::IRI(iri) => {
                        let subject = SimpleTerm::Iri(IriRef::new(MownStr::from_ref(iri)).unwrap());
                        let predicate =
                            SimpleTerm::Iri(IriRef::new(MownStr::from_ref(&aa.ann.ap)).unwrap());
                        let object = match &aa.ann.av {
                            AnnotationValue::Literal(literal) => match literal {
                                Literal::Simple { literal } => SimpleTerm::LiteralDatatype(
                                    literal.clone().into(),
                                    IriRef::new_unchecked(MownStr::from_ref(
                                        "http://www.w3.org/2001/XMLSchema#string",
                                    )),
                                ),
                                Literal::Language { literal, lang } => SimpleTerm::LiteralLanguage(
                                    literal.clone().into(),
                                    LanguageTag::new(lang.clone().into()).unwrap(),
                                ),
                                Literal::Datatype {
                                    literal,
                                    datatype_iri,
                                } => SimpleTerm::LiteralDatatype(
                                    literal.clone().into(),
                                    IriRef::new(MownStr::from_ref(datatype_iri)).unwrap(),
                                ),
                            },
                            AnnotationValue::IRI(iri) => {
                                SimpleTerm::Iri(IriRef::new(MownStr::from_ref(iri)).unwrap())
                            }
                            AnnotationValue::AnonymousIndividual(anonymous_individual) => {
                                SimpleTerm::BlankNode(
                                    BnodeId::new(MownStr::from_ref(anonymous_individual)).unwrap(),
                                )
                            }
                        };

                        if graph.insert(subject, predicate, object).is_err() {
                            // This sould not happen :>
                            error!("The term index is full");
                        }
                    }
                    AnnotationSubject::AnonymousIndividual(_) => {
                        // TODO support anonymous individual
                    }
                },
                horned_owl::model::Component::Import(horned_owl::model::Import(iri)) => {
                    if let Some(subject) = ontology_iri {
                        let predicate = SimpleTerm::Iri(IriRef::new_unchecked(MownStr::from_ref(
                            "http://www.w3.org/2002/07/owl#imports",
                        )));
                        let object = SimpleTerm::Iri(IriRef::new(MownStr::from_ref(iri)).unwrap());
                        graph.insert(subject, predicate, object).unwrap();
                    }
                }
                _ => (),
            }
        }

        Self(graph)
    }
}

impl Hash for ExternalDocument {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.uri.hash(state);
    }
}

impl ExternalDocument {
    /// The ontology type is currently determent by the url extention
    pub fn new(text: String, url: Url) -> Result<ExternalDocument> {
        debug!("try creating external document {url}\n{text}");
        let builder = lock_global_build_arc();

        // Try parsing different styles
        // - first owx then rdf
        // This could try more styles in the future

        debug!("try owx...");
        let mut buffer = text.as_bytes();
        let doc = horned_owl::io::owx::reader::read_with_build::<ArcStr, SetOntology<ArcStr>, _>(
            &mut buffer,
            &builder,
        )
        .map_err(Error::HornedOwl)
        .map(|(ontology, _)| {
            let graph: InfoGraph = ontology.into();

            // TODO maybe try the other parser...
            // if graph.0.triples().count() == 0 {
            //     return Err(Error::DocumentEmpty(url.clone()));
            // }

            ExternalDocument {
                uri: url.clone(),
                text: text.clone(),
                graph,
            }
        })
        .or_else(|e| {
            warn!("owx failed with {e}");
            debug!("try rdf...");

            let graph = sophia::xml::parser::parse_str(&text)
                .collect_triples::<LightGraph>()
                .map_err(|e| Error::Sophia(format!("{e}")))
                .map(|g| ExternalDocument {
                    uri: url,
                    text,
                    graph: InfoGraph(g),
                });
            graph
        });

        debug!(
            "parsing worked! {} turtle:\n{}",
            doc.as_ref().unwrap().uri,
            {
                use sophia::api::serializer::Stringifier;
                use sophia::api::serializer::TripleSerializer;
                use sophia::turtle::serializer::nt::NtSerializer;
                let mut nt_stringifier = NtSerializer::new_stringifier();
                let graph = &doc.as_ref().unwrap().graph.0;
                let str = nt_stringifier.serialize_graph(&graph).unwrap().as_str();
                str.to_string()
            }
        );
        doc
    }

    fn imports(&self) -> Vec<Url> {
        let graph = &self.graph.0;
        let iris = graph
            .triples_matching(
                Any,
                |p: SimpleTerm<'_>| {
                    p.iri()
                        .is_some_and(|iri| iri.as_str() == "http://www.w3.org/2002/07/owl#imports")
                },
                Any,
            )
            .inspect(|a| debug!("inspect={a:?}"))
            .flatten()
            .filter_map(|[_, _, o]| o.iri())
            .flat_map(|iri| Url::parse(&iri))
            .unique()
            .collect_vec();

        debug!("iris={iris:#?}");

        iris
    }

    fn reachable_docs_recursive_helper(
        &self,
        workspace: &Arc<RwLock<Workspace>>,
        result: &mut HashSet<Url>,
        http_client: &dyn HttpClient,
    ) -> Result<()> {
        if result.contains(&self.uri) {
            // Do nothing
            return Ok(());
        }

        result.insert(self.uri.clone());

        let docs = self
            .imports()
            .iter()
            .filter_map(|url| {
                Workspace::resolve_url_to_document(workspace, url, http_client)
                    .inspect_log()
                    .ok()
            })
            .collect_vec();

        for doc in docs {
            match doc {
                DocumentReference::Internal(internal_document) => internal_document
                    .read()
                    .reachable_docs_recursive_helper(result, http_client)?,
                DocumentReference::External(e) => {
                    e.read()
                        .reachable_docs_recursive_helper(workspace, result, http_client)?;
                }
            }
        }
        Ok(())
    }

    pub fn get_frame_info(&mut self, iri: &Iri) -> Option<FrameInfo> {
        debug!("getting frame info for {iri} on {}", self.uri);
        let o = get_frame_info_helper_ex(self, iri);
        debug!("found {o:#?}");
        o
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
    let graph = &doc.graph.0;

    graph
        .triples_matching(
            |s: SimpleTerm| s.iri().is_some_and(|subject| subject.as_str() == iri),
            Any,
            Any,
        )
        .flatten()
        .map(|[_, p, o]| {
            let ps = p.iri().unwrap();
            let os = o
                .lexical_form()
                .map_or_else(|| o.iri().unwrap().to_string(), |l| l.to_string());

            FrameInfo {
                iri: iri.clone(),
                annotations: once((format!("{ps}"), vec![format!("{os}")])).collect(),
                frame_type: FrameType::Unknown,
                definitions: vec![Location {
                    uri: doc.uri.clone(),
                    range: Range::ZERO,
                }],
            }
        })
        .tree_reduce(FrameInfo::merge)
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
    pub index: u32,
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
    pub uri: Url,
    pub range: Range,
}

impl Location {
    pub fn into_lsp(
        self,
        rope: &Rope,
        encoding: &PositionEncodingKind,
    ) -> Result<tower_lsp::lsp_types::Location> {
        Ok(tower_lsp::lsp_types::Location {
            uri: self.uri.clone(),
            range: self.range.into_lsp(rope, encoding)?,
        })
    }
}

impl FrameInfo {
    fn merge(a: FrameInfo, b: FrameInfo) -> FrameInfo {
        let mut c = a;
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
        self.definitions.dedup();
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

        debug!("frame annotations {:#?}", self.annotations);

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
                if annoation_display.contains('\n') {
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
    iri.rsplit_once(['/', '#']).map_or(iri, |(_, b)| b)
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
pub type Iri = String;

pub fn node_text(node: &Node, rope: &Rope) -> String {
    rope.get_byte_slice(node.start_byte()..node.end_byte())
        .map_or(String::new(), |rs| rs.to_string())
}

/// Generate the diagnostics for a single node, walking recusivly down to every child and every syntax error within
pub fn gen_diagnostics(node: &Node) -> Vec<(Range, String)> {
    let mut cursor = node.walk();
    let mut diagnostics = Vec::<(Range, String)>::new();

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

                diagnostics.push((range, msg.to_string()));
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

/// taken from <https://www.w3.org/TR/owl2-syntax/#Entity_Declarations_and_Typing>
#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum FrameType {
    Class,
    DataType,
    ObjectProperty,
    DataProperty,
    AnnotationProperty,
    Individual,
    Ontology,

    /// The frame type of an IRI that has no valid frame (this can be because of conflicts)
    Invalid,
    /// The frame type of an IRI that has no frame at all (can be overriten)
    Unknown,
}

impl FrameType {
    pub fn parse(kind: &str) -> FrameType {
        match kind {
            "datatype_iri" | "datatype_frame" => FrameType::DataType,
            "annotation_property_iri" | "annotation_property_frame" => {
                FrameType::AnnotationProperty
            }
            "individual_iri" | "individual_frame" => FrameType::Individual,
            "ontology_iri" | "ontology_frame" => FrameType::Ontology,
            "data_property_iri" | "data_property_frame" => FrameType::DataProperty,
            "object_property_iri" | "object_property_frame" => FrameType::ObjectProperty,
            "class_frame" | "class_iri" => FrameType::Class,
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
            FrameType::ObjectProperty | FrameType::DataProperty | FrameType::AnnotationProperty => {
                SymbolKind::PROPERTY
            }
            FrameType::Individual => SymbolKind::OBJECT,
            FrameType::Ontology => SymbolKind::MODULE,
            FrameType::Invalid | FrameType::Unknown => SymbolKind::NULL,
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
pub fn trim_full_iri(untrimmed_iri: String) -> Iri {
    let iri = untrimmed_iri;
    iri.trim_end_matches('>')
        .trim_start_matches('<')
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

fn to_doc(node: &Node, rope: &Rope, tab_size: u32) -> RcDoc<'static, ()> {
    // I do not target 32 systems
    #[allow(clippy::cast_possible_wrap)]
    let nest_depth = tab_size as isize;
    let text = node_text(node, rope);
    debug!(
        "to_doc for {text} that is {} at {:?}",
        node.kind(),
        node.range()
    );
    let mut cursor = node.walk();

    // So if this node as an error child then the translation into RcDoc could exclude that error node.
    // Therefore lets not translate it at all.
    if node.children(&mut cursor).any(|child| child.is_error()) {
        return RcDoc::text(text);
    }

    match node.kind() {
        "source_file" => {
            source_file_to_doc(node, rope, tab_size)
        },
        "ontology" =>
            ontology_to_doc(node, rope, tab_size, nest_depth)
        ,
        "prefix_declaration" | "import" | "annotation" => RcDoc::intersperse(
            node.children(&mut cursor)
                .map(|n| to_doc(&n, rope, tab_size)),
            RcDoc::line(),
        )
        .nest(nest_depth)
        .group(),
        "annotations"
        // class
        | "sub_class_of" | "class_equivalent_to" | "class_disjoint_with" | "disjoint_union_of" | "has_key"
        // datatype
        | "datatype_equavalent_to"
        // individual
        | "individual_facts" | "individual_same_as" | "individual_different_from" | "individual_types"
        // annotation property
        | "annotation_property_domin" | "annotation_property_range" | "annotation_property_sub_property_of"
        // data property
        | "data_property_domain" | "data_property_range" | "data_property_characteristics" | "data_property_sub_property_of" | "data_property_equivalent_to" | "data_property_disjoint_with"
        // object property
        |"domain" |"range" |"sub_property_of" |"object_property_equivalent_to" |"object_property_disjoint_with" |"inverse_of" |"characteristics" |"sub_property_chain"
        // misc
        |"equivalent_classes" |"disjoint_classes" |"equivalent_object_properties" |"disjoint_object_properties" |"equivalent_data_properties" |"disjoint_data_properties" |"same_individual" |"different_individuals"
         => {
            nesting_property_with_keyword_to_frame(node, rope, tab_size, nest_depth)
        },
        "description"
         => {
             let subs=node.children(&mut cursor).chunk_by(|n| n.kind()=="or").into_iter().map(|(is_or, chunks)|{
                 if is_or {
                     RcDoc::line().append(RcDoc::text("or").append(RcDoc::space()))
                 } else {
                     let conjunction_node = chunks.exactly_one().unwrap_or_else(|_| unreachable!("chunk should contain exacly one seperator node"));
                     to_doc(&conjunction_node, rope, tab_size)
                 }
             }).collect_vec();
            RcDoc::concat(subs)
        },
        "conjunction"
         => {
             let subs=node.children(&mut cursor).chunk_by(|n| n.kind()=="and").into_iter().map(|(is_or, chunks)|{
                 if is_or {
                     RcDoc::line().append(RcDoc::text("and").append(RcDoc::space()))
                 } else {
                     RcDoc::intersperse(chunks.map(|n| to_doc(&n, rope, tab_size)), RcDoc::space())
                 }
             }).collect_vec();
            RcDoc::concat(subs)
        },
        "primary"=>{
            RcDoc::intersperse(node.children(&mut cursor).map(|n|to_doc(&n, rope, tab_size)), RcDoc::space())
        },
        "nested_description"
         => {
            RcDoc::text("(").append(RcDoc::line()).append(
                to_doc(&node.named_child(0).expect("open parenthese to have sibling"), rope, tab_size)
            ).nest(nest_depth).append(RcDoc::line()).append(")")
        },
        "class_frame"
        | "datatype_frame"
        | "data_property_frame"
        | "object_property_frame"
        | "annotation_property_frame"
        | "individual_frame"
         => frame_to_doc(node, rope, tab_size, nest_depth),
        _ => RcDoc::text(text), // this applies also to "ERROR" nodes!
    }
}

fn nesting_property_with_keyword_to_frame(
    node: &Node,
    rope: &Rope,
    tab_size: u32,
    nest_depth: isize,
) -> RcDoc<'static> {
    let mut cursor = node.walk();
    let mut docs = vec![];

    // This should be the keyword
    if let Some(child) = node.child(0) {
        docs.push(to_doc(&child, rope, tab_size).append(RcDoc::line()));
    }

    for (is_seperator, chunk) in &node
        .children(&mut cursor)
        .skip(1)
        .chunk_by(|x| x.kind() == "," || x.kind() == "o")
    {
        if is_seperator {
            let n = &chunk
                .exactly_one()
                .unwrap_or_else(|_| unreachable!("chunk should contain exacly one seperator node"));

            if n.kind() == "o" {
                docs.push(RcDoc::text(" o").append(RcDoc::line()));
            } else {
                docs.push(RcDoc::text(",").append(RcDoc::line()));
            }
        } else {
            docs.push(RcDoc::intersperse(
                chunk.map(|n| to_doc(&n, rope, tab_size)),
                RcDoc::line(),
            ));
        }
    }

    RcDoc::concat(docs).nest(nest_depth).group()
}

fn source_file_to_doc(node: &Node, rope: &Rope, tab_size: u32) -> RcDoc<'static, ()> {
    let mut cursor = node.walk();
    let prefix_docs = node
        .children_by_field_name("prefix", &mut cursor)
        .map(|n| to_doc(&n, rope, tab_size))
        .collect_vec();
    let ontology_doc = node
        .child_by_field_name("ontology")
        .map_or(RcDoc::nil(), |n| to_doc(&n, rope, tab_size));
    if prefix_docs.is_empty() {
        ontology_doc
    } else {
        RcDoc::intersperse(
            [
                RcDoc::intersperse(prefix_docs, RcDoc::hardline()),
                ontology_doc,
            ],
            RcDoc::hardline().append(RcDoc::hardline()),
        )
    }
}

fn ontology_to_doc(node: &Node, rope: &Rope, tab_size: u32, nest_depth: isize) -> RcDoc<'static> {
    let mut cursor = node.walk();
    RcDoc::intersperse(
        [
            RcDoc::text("Ontology:")
                .append(RcDoc::line())
                .append(RcDoc::intersperse(
                    node.child_by_field_name("iri")
                        .into_iter()
                        .map(|n| to_doc(&n, rope, tab_size))
                        .chain(
                            node.child_by_field_name("version_iri")
                                .into_iter()
                                .map(|n| to_doc(&n, rope, tab_size)),
                        ),
                    RcDoc::line(),
                ))
                .nest(nest_depth)
                .group(),
            // imports
            RcDoc::intersperse(
                node.children_by_field_name("import", &mut cursor.clone())
                    .map(|n| to_doc(&n, rope, tab_size).append(RcDoc::hardline())),
                RcDoc::nil(),
            ),
            // annotations
            RcDoc::intersperse(
                node.children_by_field_name("annotations", &mut cursor.clone())
                    .map(|n| to_doc(&n, rope, tab_size).append(RcDoc::hardline())),
                RcDoc::nil(),
            ),
            // frames
            RcDoc::intersperse(
                node.children_by_field_name("frame", &mut cursor)
                    .map(|n| to_doc(&n, rope, tab_size).append(RcDoc::hardline())),
                RcDoc::hardline(),
            ),
        ],
        RcDoc::hardline(),
    )
}

fn frame_to_doc(node: &Node, rope: &Rope, tab_size: u32, nest_depth: isize) -> RcDoc<'static> {
    let mut cursor = node.walk();
    node.child(0)
        .map_or(RcDoc::nil(), |n| to_doc(&n, rope, tab_size))
        .append(RcDoc::line())
        .append(
            node.child(1)
                .map_or(RcDoc::nil(), |n| to_doc(&n, rope, tab_size)),
        )
        .nest(nest_depth)
        .group()
        .append(RcDoc::hardline())
        .append(RcDoc::intersperse(
            node.children(&mut cursor)
                .skip(2)
                .map(|n| to_doc(&n, rope, tab_size)),
            RcDoc::hardline(),
        ))
        .nest(nest_depth)
        .group()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::pos::Position;
    use indoc::indoc;
    use pretty_assertions::assert_eq;
    use test_log::test;

    #[test]
    fn internal_document_formatted_should_format_correctly() {
        let doc = InternalDocument::new(
            Url::parse("http://formatted").unwrap(),
            -1,
            indoc! {"
                Prefix:  a:  <http://a/a>  Prefix:  a:  <http://a/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa>    Ontology:   a   v   Import:    <http://a/a>    Import:    <http://a/a> Annotations:    rdfs:label     \"a\"    Class:   a   SubClassOf:   b,e,f SubClassOf:   cccccccccccccccccccccccc,ddddddddddddddddddddd,eeeeeeeeeee   Class:   a     SubClassOf: a    Annotations:   rdfs:label    \"Y\"    EquivalentTo:    a   ,   a DisjointWith:    a  ,  a   DisjointUnionOf:  Annotations: y 12, a 2    a,a    HasKey:    a
            "}
            .into(),
        );

        info!("sexp:\n{}", doc.tree.root_node().to_sexp());

        let result = doc.formatted(4, 35);

        assert_eq!(
            result,
            indoc! {"
                Prefix: a: <http://a/a>
                Prefix:
                    a:
                    <http://a/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa>

                Ontology: a v
                Import: <http://a/a>
                Import: <http://a/a>

                Annotations: rdfs:label \"a\"

                Class: a
                    SubClassOf: b, e, f
                    SubClassOf:
                        cccccccccccccccccccccccc,
                        ddddddddddddddddddddd,
                        eeeeeeeeeee

                Class: a
                    SubClassOf: a
                    Annotations: rdfs:label \"Y\"
                    EquivalentTo: a, a
                    DisjointWith: a, a
                    DisjointUnionOf:
                        Annotations: y 12, a 2
                        a,
                        a
                    HasKey: a
            "}
        );
    }

    #[test]
    fn internal_document_formatted_with_description_should_format_correctly() {
        let doc = InternalDocument::new(
            Url::parse("http://formatted").unwrap(),
            -1,
            indoc! {r"
                Ontology:a
                Class: a
                SubClassOf:   (aaaaaaaa and bbbbbb)    or   (bbbb and hasRel some (ccccccc or ddddddd or eeeeeeeee))
            "}
            .into(),
        );

        info!("sexp:\n{}", doc.tree.root_node().to_sexp());

        let result = doc.formatted(4, 35);

        assert_eq!(
            result,
            indoc! {r"
                Ontology: a


                Class: a
                    SubClassOf:
                        (
                            aaaaaaaa
                            and bbbbbb
                        )
                        or (
                            bbbb
                            and hasRel some (
                                ccccccc
                                or ddddddd
                                or eeeeeeeee
                            )
                        )
            "}
        );
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

        let full_iri = doc.abbreviated_iri_to_full_iri("owl:Nothing");
        let full_iri_2 = doc.abbreviated_iri_to_full_iri("ja:Janek");

        assert_eq!(
            full_iri,
            Some("http://www.w3.org/2002/07/owl#Nothing".to_string())
        );
        assert_eq!(
            full_iri_2,
            Some(
                "http://www.semanticweb.org/janek/ontologies/2025/5/untitled-ontology-3/Janek"
                    .to_string()
            )
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

        let full_iri = doc.abbreviated_iri_to_full_iri(":Nothing");
        let full_iri_2 = doc.abbreviated_iri_to_full_iri("Nothing");

        assert_eq!(
            full_iri,
            Some("http://www.w3.org/2002/07/owl#Nothing".to_string())
        );
        assert_eq!(
            full_iri_2,
            Some("http://www.w3.org/2002/07/owl#Nothing".to_string())
        );
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
                    String::new(),
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
        let info = doc.frame_info_by_iri(&"A".to_string());

        // Assert
        info!("{doc:#?}");
        let info = info.unwrap();

        assert_eq!(info.iri, "A".to_string());
        assert_eq!(
            info.definitions,
            vec![Location {
                uri: "http://foo/14329076".parse().unwrap(),
                range: Range {
                    start: Position::new(2, 20),
                    end: Position::new(5, 55),
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
        let urls = external_doc.imports();

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

        let abbr_iri = doc.full_iri_to_abbreviated_iri("http://www.w3.org/2002/07/owl#Thing");

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

        let abbr_iri = doc.full_iri_to_abbreviated_iri("http://www.w3.org/2002/07/owl#Thing");

        assert_eq!(abbr_iri, Some("Thing".to_string()));
    }
}

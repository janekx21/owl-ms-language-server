use crate::catalog::CatalogUri;
use crate::consts::{
    get_fixed_infos, keyword_hover_info, LABEL_IRI, STRING_IRI,
};
use crate::error::{Error, Result, ResultExt, ResultIterator};
use crate::functional::InternalOfnDocument;
use crate::iri::{trim_tags, Iri};
use crate::manchester::InternalOmnDocument;
use crate::pos::Position;
use crate::queries::NODE_TYPES;
use crate::range::{Change, RangeBox};
use crate::web::{url_to_filename, HttpClient};
use crate::{
    catalog::Catalog, debugging::timeit, queries::ALL_QUERIES, range::Range,
    rope_provider::RopeProvider, LANGUAGE_OMN,
};
use crate::iri::ToIri;
use cached::{cached, Cached};
use enum_dispatch::enum_dispatch;
use horned_owl::model::Component::{self, AnnotationAssertion};
use horned_owl::model::{
    AnnotationProperty, AnnotationSubject, AnnotationValue, ArcStr, Build, DataProperty, Datatype,
    DeclareAnnotationProperty, DeclareClass, DeclareDataProperty, DeclareDatatype,
    DeclareNamedIndividual, DeclareObjectProperty, Literal, NamedIndividual, ObjectProperty,
};
use horned_owl::ontology::set::SetOntology;
use itertools::Itertools;
use log::{debug, error, info, trace, warn};
use rayon::iter::{IntoParallelRefMutIterator, ParallelIterator};
use ropey::{Rope, RopeSlice};
use sophia::api::graph::{Graph, MutableGraph};
use sophia::api::ns::Namespace;
use sophia::api::prelude::Any;
use sophia::api::source::TripleSource;
use sophia::api::term::{BnodeId, LanguageTag, SimpleTerm, Term};
use sophia::api::MownStr;
use sophia::inmem::graph::LightGraph;
use sophia::iri::resolve::Oxiri;
use sophia::iri::IriRef;
use std::fmt::Debug;
use std::hash::{DefaultHasher, Hash, Hasher};
use std::iter::once;
use std::path::Path;
use std::string::ToString;
use std::sync::{Arc, LazyLock, Mutex, MutexGuard};
use std::time::{Duration, SystemTime};
use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    fs,
    path::PathBuf,
};
use tokio::task::JoinHandle;
use tower_lsp::lsp_types::{
    self, DiagnosticSeverity, DidChangeTextDocumentParams, InlayHint, InlayHintLabel,
    PositionEncodingKind, SymbolKind, TextDocumentContentChangeEvent, Url, WorkspaceFolder,
};
use tree_sitter_c2rust::{InputEdit, Node, Parser, Query, QueryCursor, StreamingIterator, Tree};


static GLOBAL_BUILD_ARC: LazyLock<Mutex<Build<ArcStr>>> = LazyLock::new(|| {
    let build = Build::new_arc();
    Mutex::new(build)
});

pub fn lock_global_build_arc() -> MutexGuard<'static, Build<ArcStr>> {
    (*GLOBAL_BUILD_ARC)
        .lock()
        .expect("the horned owl builder should not panic")
}

/// Clears all global caches used by the workspace.
/// This is useful for benchmarks to prevent memory accumulation across iterations.
pub fn clear_caches() {
    {
        let mut cache = GET_FRAME_INFO_HELPER_EX.write();
        cache.cache_clear();
    }
    if let Ok(mut gab) = GLOBAL_BUILD_ARC.lock() {
        *gab = Build::new_arc();
    }
}

/// Document container
#[derive(Debug)]
pub struct Workspace {
    /// Maps a Path/URL to a document that can only be internal
    internal_documents: HashMap<PathBuf, InternalDocument>,
    /// Maps a Path/URL to a document that can only be external
    external_documents: HashMap<Url, ExternalDocument>,
    folder: WorkspaceFolder,
    catalogs: Vec<Catalog>,
    pub index_handles: Vec<JoinHandle<()>>,
}

/// This enum is a proxy for dispatching [`OntologyDocument`] methods
#[enum_dispatch(OntologyDocument)]
#[derive(Debug, PartialEq, Eq)]
pub enum InternalDocument {
    InternalOmn(InternalOmnDocument),
    InternalOfn(InternalOfnDocument),
}

/// The read/write internal document trait.
/// Every ontology document that the user can edit should implement this trait.
/// Tree state and querie methods are not included here, because they are document specific.
#[enum_dispatch]
pub trait OntologyDocument {
    /// The file path of this text file
    fn path(&self) -> &Path;

    /// The file url of this text file
    fn uri(&self) -> &Url;

    /// LSP version. Gets incremented when editing. Not to be confused with the ontology version.
    fn version(&self) -> i32;

    /// Text content stored as a rope
    fn rope(&self) -> &Rope;

    // Frame infos
    fn frame_infos(&self) -> Vec<&FrameInfo>;
    fn find_frame_info(&self, iri: &Iri) -> Option<FrameInfo>;

    // Analytics
    fn ontology_iri(&self) -> Option<Iri>;
    fn version_iri(&self) -> Option<Iri>;
    fn definitions(&self) -> Vec<&RangeBox<IriDefinition>>;
    fn references(&self) -> Vec<&RangeBox<Iri>>;
    /// OWL ontolgies that are imported by this ontology.
    /// There are also indirect imports, but they need to be resolved in the workspace,
    /// because a single document has no context of the workspace.
    fn directly_imports(&self) -> Vec<&Url>;
    /// This includes prefixes, references and imports
    fn directly_references_doc(&self) -> Vec<&Url>;

    /// Errors/Diagnostics that are created by this document alone. This
    /// includes e.g. syntax errors.
    fn local_diagnostics(&self) -> &[Diagnostic];

    /// Map of IRIs and where to find them
    fn iri_locations(&self) -> HashMap<&Iri, &Vec<RangeBox<()>>>;

    // IRI conversions
    /// Taking a (relative) abbreviated or simple IRI and resolving the (absolute) full IRI.
    /// The reverse of [`full_iri_to_abbreviated_iri`].
    fn abbreviated_iri_to_full_iri(&self, iri: &Iri) -> Option<Iri>;
    /// Taking a (absolute) full IRI and by prefixing it making it shorter into a (relative)
    /// abbriviated or simple IRI.
    /// The reverse of [`abbreviated_iri_to_full_iri`].
    fn full_iri_to_abbreviated_iri(&self, full_iri: &Iri) -> Option<String>;

    fn full_iri_to_shorter_iri(&self, full_iri: &Iri) -> String {
        self.full_iri_to_abbreviated_iri(full_iri)
            .unwrap_or_else(|| {
                if full_iri.as_str().contains("://") {
                    format!("<{full_iri}>")
                } else {
                    full_iri.to_string()
                }
            })
    }

    /// Prefix map with key prefix and value prefix target
    fn prefixes(&self) -> HashMap<String, String>;

    // Document-level position queries (grammar-agnostic)
    /// The range of the document content.
    fn range(&self) -> Range;
    /// Takes a positon and returns an IRI and a range when one is found at that position.
    /// It does not include the '<' '>' chars of a full iri.
    fn iri_at(&self, pos: Position) -> Option<RangeBox<IriAtPosition>>;

    /// The range of an IRI that can be renamed. Does not include '<' '>' or prefixes.
    /// So it should trigger like this:
    /// ```txt
    /// foo:bar    --->    "bar"
    ///   ^
    ///  Cursor
    /// ```
    fn rename_range(&self, pos: Position) -> Option<Range>;
    /// Generate informations for renaming
    fn rename_info_at(&self, pos: Position, new_name: &str) -> Result<Option<RenameInfo>>;
    /// Takes a [`RenameInfo`] and returns the edits that will get performed by the rename
    fn rename_edits(&self, rename_info: &RenameInfo) -> Vec<RangeBox<String>>;

    /// Get actions for creating keywords at a position
    fn keyword_actions_at(&self, pos: Position) -> Vec<KeywordAction>;

    /// Get all iris that are in a range
    fn all_iris_in_range(&self, range: Range) -> Vec<RangeBox<Iri>>;

    // Requests
    /// A formatted version of the document
    fn formatted(&self, options: &FormattingSettings) -> String;
    /// Hover information at a position
    fn hover(&self, pos: Position) -> Option<HoverResult>;
    /// All highlights
    fn highlights(&self, range: Range) -> Highlights;
    /// A keyword completion
    fn get_keyword_competions_at(&self, pos: Position) -> Vec<String>;
    /// An IRI completion at a position
    fn get_iri_completions_at(
        &self,
        pos: Position,
        workspace: &Workspace,
    ) -> Vec<(String, String, String)>; // TODO can this be done in another way???

    /// Searches for a declared IRI
    fn find_iri_references(&self, full_iri: &Iri, include_declaration: bool) -> Vec<Range> {
        if include_declaration {
            self.references()
                .into_iter()
                .filter(|rb| rb.value() == full_iri)
                .map(RangeBox::range)
                .copied()
                .collect()
        } else {
            debug!("def {:#?}", self.definitions());
            let exclude = self
                .definitions()
                .into_iter()
                .filter(|rb| &rb.value().iri == full_iri)
                .map(RangeBox::range)
                .copied()
                .collect_vec();

            debug!("Exclude {exclude:#?}");

            debug!("ref {:#?}", self.references());
            // TODO this could be more accurate by including the iri range of the definition in the IriDefinition struct
            // TODO drops intra frame references
            self.references()
                .into_iter()
                .filter(|rb| rb.value() == full_iri)
                .map(RangeBox::range)
                .filter(|r| !exclude.iter().any(|r2| r2.overlaps(r)))
                .copied()
                .collect()
        }
    }

    fn statistic(&self) -> String;
}

pub(crate) enum HoverResult {
    Iri { iri: Iri, range: Range },
    Keyword { text: String, range: Range },
}

pub struct IriAtPosition {
    pub full_iri: Iri,
    pub is_import: bool,
    /// `None` for import IRIs and positions where no frame context was found
    pub frame_type: Option<FrameType>,
}

/// Some info for renaming
#[derive(Debug)]
pub struct RenameInfo {
    /// The IRI of the thing that gets renamed
    pub full_iri: Iri,
    /// The IRI that the thing is getting renamed to
    pub new_iri: Option<Iri>,
    /// The frame type of the thing that gets renamed
    pub frame_type: FrameType,
    /// The text that actualy gets inserted.
    /// This can e.g. be an abbreviated IRI
    pub original: String,
}

pub struct KeywordAction {
    pub parent_name: String,
    pub new_text: String,
    pub range: Range,
}

pub type Highlights = Vec<RangeBox<u32>>;

#[derive(Debug)]
pub enum Lang {
    Omn,
    Ofn,
}

impl InternalDocument {
    pub fn new(url: Url, version: i32, text: String, lang: &Lang) -> InternalDocument {
        match lang {
            Lang::Omn => {
                InternalDocument::InternalOmn(InternalOmnDocument::new(url, version, text))
            }
            Lang::Ofn => InternalDocument::InternalOfn(InternalOfnDocument::new(url, version, text)),
        }
    }

    pub fn edit(
        self,
        params: DidChangeTextDocumentParams,
        encoding: &PositionEncodingKind,
    ) -> Result<InternalDocument> {
        match self {
            InternalDocument::InternalOmn(doc) => doc
                .edit_inner(params, encoding)
                .map(InternalDocument::InternalOmn),
            InternalDocument::InternalOfn(doc) => doc
                .edit_inner(params, encoding)
                .map(InternalDocument::InternalOfn),
        }
    }
}

impl Display for Workspace {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Workspace {} at {}", self.folder.name, self.folder.uri)
    }
}

impl Workspace {
    pub fn new(workspace_folder: WorkspaceFolder) -> Self {
        let catalogs = Catalog::load_catalogs_recursive(&workspace_folder.uri);
        info!(
            "New workspace {} at {} with catalogs {catalogs:?}",
            workspace_folder.name, workspace_folder.uri
        );
        Workspace {
            internal_documents: HashMap::new(),
            external_documents: HashMap::new(),
            folder: workspace_folder,
            catalogs,
            index_handles: Vec::new(),
        }
    }

    /// Inserts an internal document into the workspace and returns a reference to it.
    /// This will replace the document with the same URL if there was one.
    pub fn insert_internal_document(&mut self, document: InternalDocument) -> &InternalDocument {
        debug!("Insert internal document {document:?}");
        let path = document.path().to_path_buf();
        self.internal_documents.insert(path.clone(), document);
        self.internal_documents
            .get(&path)
            .expect("document should be present")
    }

    pub fn get_internal_document(&self, path: &Path) -> Result<&InternalDocument> {
        self.internal_documents
            .get(path)
            .ok_or(Error::InternalDocumentNotFound(path.to_path_buf()))
    }

    pub fn take_internal_document(&mut self, path: &Path) -> Result<InternalDocument> {
        self.internal_documents
            .remove(path)
            .ok_or(Error::InternalDocumentNotFound(path.to_path_buf()))
    }

    pub fn contains_internal_document(&self, path: &Path) -> bool {
        self.internal_documents.contains_key(path)
    }

    pub fn internal_documents(
        &self,
    ) -> std::collections::hash_map::Values<'_, PathBuf, InternalDocument> {
        self.internal_documents.values()
    }

    pub fn insert_external_document(&mut self, document: ExternalDocument) -> &ExternalDocument {
        debug!(
            "Insert external document {} length is {} into workspace at {}",
            document.uri,
            document.text.len(),
            self.folder.uri
        );
        let uri = document.uri.clone();
        self.external_documents.insert(uri.clone(), document);
        self.external_documents
            .get(&uri)
            .expect("external document should exist")
    }

    // #[cfg(test)]
    pub fn external_documents(
        &self,
    ) -> std::collections::hash_map::Values<'_, Url, ExternalDocument> {
        self.external_documents.values()
    }

    pub fn catalog_contains_url(&self, url: &Url) -> bool {
        self.catalogs.iter().any(|catalog| {
            match &url
                .to_file_path()
                .inspect_err(|()| error!("Url is not a filepath {url}"))
            {
                Ok(path) => catalog.contains(path),
                Err(()) => false,
            }
        })
    }

    pub fn workspace_folder_is_base_of_url(&self, url: &Url) -> bool {
        self.folder.uri.make_relative(url).is_some()
    }

    pub fn all_frame_infos(&self) -> impl Iterator<Item = &FrameInfo> {
        self.internal_documents()
            .flat_map(OntologyDocument::frame_infos)
    }

    /// Returns the path for the cache folder
    pub fn shared_cache_folder_path(&self) -> PathBuf {
        if let Some(dir) = dirs::cache_dir() {
            // Well all projects can even share a cache dir
            dir.join("owl-ms-language-server")
        } else {
            // If the cache folder can not be accessed then lets just use a local folder
            self.folder
                .uri
                .to_file_path()
                .expect("Workspace folder url should be file path")
                .join(".owl")
        }
    }

    // TODO #28 maybe return a reference?
    /// This searches in the frames of internal documents (case-insensitive)
    pub fn search_frame(&self, partial_text: &str) -> Vec<(String, Iri, FrameInfo)> {
        let partial_lower = partial_text.to_lowercase();
        self.internal_documents
            .values()
            .flat_map(|doc| {
                doc.frame_infos()
                    .into_iter()
                    .flat_map(
                        |item| -> Box<dyn Iterator<Item = (String, Iri, FrameInfo)>> {
                            if item.iri.as_str().to_lowercase().contains(&partial_lower) {
                                Box::new(once((item.iri.to_string(), item.iri.clone(), item.clone())))
                            } else {
                                Box::new(
                                    item.annotations
                                        .iter()
                                        .filter(|annotation| {
                                            annotation
                                                .string_value
                                                .to_lowercase()
                                                .starts_with(&partial_lower)
                                        })
                                        .map(|annotation| {
                                            (annotation.iri.to_string(), item.iri.clone(), item.clone())
                                        }),
                                )
                            }
                        },
                    )
                    .collect_vec()
            })
            .collect_vec()

        // TODO search in external frames
    }

    /// This finds a frame info in the internal and external documents.
    ///
    /// - `iri` should be a full iri
    pub fn get_frame_info(&self, iri: &Iri) -> Option<FrameInfo> {
        debug!(
            "getting workspace frame info for {iri} on {}",
            self.folder.uri
        );
        let external_infos = self
            .external_documents
            .values()
            .filter_map(|doc| doc.get_frame_info(iri));

        let internal_infos = self
            .internal_documents
            .values()
            .filter_map(|dm| dm.find_frame_info(iri));

        internal_infos
            .chain(external_infos)
            .chain(get_fixed_infos(iri))
            .tree_reduce(FrameInfo::merge)
    }

    /// This no longer uses a document. Because the reachable documents would be callec way to often. Now it just takes the slice of reachable documents directly. Generate it using `reachable_docs_recursive`.
    pub fn get_frame_info_recursive(
        workspace: &Workspace,
        iri: &Iri,
        reachable_docs: &[Url],
    ) -> Option<FrameInfo> {
        reachable_docs
            .iter()
            .filter_map(|url| {
                if let Some(doc) = workspace.document_by_url(url) {
                    match &doc {
                        DocumentReference::Internal(doc) => doc.find_frame_info(iri),
                        DocumentReference::External(doc) => doc.get_frame_info(iri),
                    }
                } else {
                    warn!("Doc {url} not in worksapce");
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
                let iri = trim_full_iri_rope_slice(node_text(node, doc.rope()));

                self.get_frame_info(&iri.to_iri())
                    .map(|fi| fi.info_display(self))
                    .unwrap_or(iri.to_string())
            }
            "simple_iri" | "abbreviated_iri" => {
                let iri = node_text(node, doc.rope()).to_string();
                let iri = doc
                    .abbreviated_iri_to_full_iri(&iri.to_iri())
                    .unwrap_or(iri.into());
                debug!("Getting node info for {iri} at doc {}", doc.uri());
                self.get_frame_info(&iri)
                    .map(|fi| fi.info_display(self))
                    .unwrap_or(iri.to_string())
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
    /// Does no loading. Just returns the document when found.
    pub fn document_by_url(&'_ self, url: &Url) -> Option<DocumentReference<'_>> {
        if let Some(path) = self.url_to_path_with_catalog(url) {
            if let Some(doc) = self.internal_documents.get(&path) {
                // Document is loaded already
                return Some(DocumentReference::Internal(doc));
            }
        }

        // TODO maybe change this
        // Lets try to find the doc in internal docs
        if let Some(doc) = self
            .internal_documents
            .values()
            .find(|doc| doc.uri() == url)
        {
            return Some(DocumentReference::Internal(doc));
        }

        if let Some(doc) = self.external_documents.get(url) {
            // Document is loaded already
            return Some(DocumentReference::External(doc));
        }
        None
    }

    // TODO can this be done without two calls?
    /// Resolves/Loads a URL (file or http/https protocol) to a document that is inserted into this workspace
    /// Locks workspace for read
    pub async fn resolve_url_to_document(
        workspace: &Workspace,
        url: &Url,
        http_client: Arc<dyn HttpClient>,
    ) -> Result<Option<Document>> {
        if workspace.document_by_url(url).is_some() {
            return Ok(None);
        }

        if let Ok(Some(doc)) = read_cached_doc(workspace, url).inspect_log() {
            debug!("Document found in web cache {url}");
            return Ok(Some(doc));
        }

        warn!("Document NOT found in web cache {url}");

        // TODO maybe use workspace.url_to_path_with_catalog(url)

        let Some((catalog, catalog_uri)) = workspace.find_catalog_uri(url) else {
            warn!("Url {url} could not be found in any catalog");

            let url_copy = url.clone();
            let document_text =
                tokio::task::spawn_blocking(move || http_client.get(url_copy.as_str()))
                    .await
                    .expect("Should not be canceled")?;

            let document = timeit("external doc new", || {
                ExternalDocument::new(document_text, url.clone())
            })?;

            timeit("cache_doc", || cache_doc(workspace, &document));

            return Ok(Some(Document::External(document)));
        };

        if let Ok(real_url) = Url::parse(&catalog_uri.uri) {
            if workspace.document_by_url(&real_url).is_some() {
                return Ok(None);
            }

            if let Ok(path) = real_url.to_file_path() {
                // This is an absolute file path url
                Ok(Some(Workspace::resolve_path_to_document(
                    &path,
                    url.clone(),
                )?))
            } else {
                // This is an external url
                let document_text = http_client.get(real_url.as_str())?;
                // TODO maybe use url or just the requested url
                // let document = ExternalDocument::new(document_text, url)?;
                let document = ExternalDocument::new(document_text, url.clone())?;

                cache_doc(workspace, &document);

                Ok(Some(Document::External(document)))
            }
        } else {
            // The catalog uri is most likely a relative file path, so lets try that
            let path = catalog.parent_folder().join(&catalog_uri.uri);
            let path_url =
                Url::from_file_path(&path).map_err(|()| Error::InvalidFilePath(path.clone()))?;
            if workspace.document_by_url(&path_url).is_some() {
                return Ok(None);
            }

            Ok(Some(Workspace::resolve_path_to_document(
                &path,
                url.clone(),
            )?))
        }
    }

    /// Convert an URL that is in the catalog into the file path
    pub fn url_to_path_with_catalog(&self, url: &Url) -> Option<PathBuf> {
        if let Some((catalog, catalog_uri)) = self.find_catalog_uri(url) {
            if let Ok(url) = Url::parse(&catalog_uri.uri) {
                url.to_file_path().ok()
            } else {
                // The catalog uri is most likely a relative file path, so lets try that

                let path = catalog.parent_folder().join(&catalog_uri.uri);
                Some(path)
            }
        } else {
            None
        }
    }

    fn resolve_path_to_document(path: &Path, original_url: Url) -> Result<Document> {
        // I think I don't care about the URL that is the path to the file.
        // Let's ignore it and use the original URL instead.
        let (document_text, path_url) = load_file_from_disk(path.to_path_buf())?;

        match path
            .extension()
            .and_then(|s| s.to_str())
            .unwrap_or_default()
        {
            "omn" => {
                let document = InternalOmnDocument::new_with_path(
                    original_url,
                    -1,
                    document_text,
                    path.to_path_buf(),
                );
                Ok(Document::Internal(document.into()))
            }
            "ofn" => {
                let document = InternalOfnDocument::new_with_path(
                    original_url,
                    -1,
                    document_text,
                    path.to_path_buf(),
                );
                Ok(Document::Internal(document.into()))
            }
            "owl" | "owx" => {
                let document = ExternalDocument::new(document_text, path_url)?;
                Ok(Document::External(document))
            }
            ext => Err(Error::DocumentNotSupported(ext.to_string())),
        }
    }

    // pub fn diagnostics(&self) {
    //     for (path, doc) in &self.internal_documents {
    //         doc.stage2.all_frame_infos;
    //     }
    // }
}

fn load_file_from_disk(path: PathBuf) -> Result<(String, Url)> {
    info!("Loading file from disk {}", path.display());

    Ok((
        fs::read_to_string(&path)?,
        Url::from_file_path(&path).map_err(|()| Error::InvalidFilePath(path))?,
    ))
}

fn read_cached_doc(workspace: &Workspace, url: &Url) -> Result<Option<Document>> {
    if cfg!(test) {
        // Do not cache in tests
        return Ok(None);
    }

    let owl_dir = workspace.shared_cache_folder_path();
    let web_cache = owl_dir.join("web_cache");
    let file_name = url_to_filename(url.as_ref());

    debug!("try read cached doc {}", file_name.display());

    let mut cache_valid = true;
    if let Ok(file) = fs::File::open(web_cache.join(&file_name)) {
        let modified_time = file.metadata()?.modified()?;
        if modified_time + Duration::from_secs(60 * 60 * 24 * 30) < SystemTime::now() {
            // invalidate cache
            warn!("Cached document is stale (older then 30 days) {url}");
            cache_valid = false;
        }
    }
    if !cache_valid {
        fs::remove_file(web_cache.join(&file_name))?;
        return Ok(None); // No error, just refetch
    }

    if let Ok(some) = fs::read(web_cache.join(file_name)) {
        let text = String::from_utf8(some).expect("Cached file should be valid UTF-8");
        let doc = ExternalDocument::new(text, url.clone())?;
        Ok(Some(Document::External(doc)))
    } else {
        Ok(None)
    }
}

fn cache_doc(workspace: &Workspace, doc: &ExternalDocument) {
    if cfg!(test) {
        // Do not cache in tests
        return;
    }

    let file_name = url_to_filename(doc.uri.as_ref());
    let owl_dir = workspace.shared_cache_folder_path();

    if let Err(err) = fs::create_dir_all(&owl_dir) {
        error!("Dir create Error: {err}");
    }
    if let Err(err) = fs::write(owl_dir.join(".gitignore"), "web_cache") {
        error!("File write Error: {err}");
    }

    let web_cache = owl_dir.join("web_cache");
    if let Err(err) = fs::create_dir_all(web_cache.clone()) {
        error!("Dir create Error: {err}");
    }
    if let Err(err) = fs::write(web_cache.join(file_name), &doc.text) {
        error!("Web cache Error: {err}");
    } else {
        debug!("Added {} to web cache", doc.uri);
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum DocumentReference<'a> {
    // Not boxing this is fine because the size ratio is just about 1.6
    Internal(&'a InternalDocument),
    External(&'a ExternalDocument),
}

#[derive(Debug)]
#[allow(clippy::large_enum_variant)] // Not boxing this is fine because the size ratio is just about 1.6
pub enum Document {
    Internal(InternalDocument),
    External(ExternalDocument),
}

/// Internal omn ontologies are OMN files on disk.
/// Text -> Parsed -> Queried -> Analyzed -> ``InternalDocument``

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct DocumentId {
    /// File location
    pub path: PathBuf,
    /// URL and location where this document was loaded from
    pub uri: Url,
    pub version: i32,
}


pub fn edit_vec_rb<T: Send>(changes: &[Change], items: &mut Vec<RangeBox<T>>) {
    items.par_iter_mut().for_each(|x| x.edit(changes.iter()));
}

pub fn retain_vec_rb<T>(post_change_ranges: &[Range], items: &mut Vec<RangeBox<T>>) {
    retain_vec_rb_on_remove(post_change_ranges, items, |_| {});
}

pub fn retain_vec_rb_on_remove<T, F: FnMut(&RangeBox<T>)>(
    post_change_ranges: &[Range],
    items: &mut Vec<RangeBox<T>>,
    mut on_remove: F,
) {
    items.retain(|range_box| {
        for sc in post_change_ranges {
            if range_box.range().overlaps(sc) || range_box.range().is_zero() {
                on_remove(range_box);
                return false;
            }
        }
        true
    });
}

pub fn extend_vec_rb<T, F: FnMut(Range) -> Vec<RangeBox<T>>>(
    post_change_ranges: &[Range],
    items: &mut Vec<RangeBox<T>>,
    mut gen: F,
) {
    for di in post_change_ranges {
        let additional_items = gen(*di);

        items.extend(additional_items);
    }
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct IriDefinition {
    pub iri: Iri,
    pub kind: FrameType,
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct Diagnostic {
    pub range: Range,
    pub kind: DiagnosticKind,
}

impl Diagnostic {
    pub fn label(&self) -> String {
        match &self.kind {
            DiagnosticKind::MissingIri(iri) => format!("Iri {iri} used but not defined"),
            DiagnosticKind::SyntaxError {
                valid_children,
                parent,
                msg,
            } => {
                format!(
                    "Syntax Error {msg}. Expected\n  {valid_children}\ninside or after\n  {parent}"
                )
            }
        }
    }

    pub fn into_lsp_diagnostic(
        self,
        rope: &Rope,
        encoding: &PositionEncodingKind,
    ) -> Result<lsp_types::Diagnostic> {
        Ok(lsp_types::Diagnostic {
            range: self.range.into_lsp(rope, encoding)?,
            severity: Some(DiagnosticSeverity::ERROR),
            code: None,
            code_description: None,
            source: Some("owl language server".to_string()),
            message: self.label(),
            related_information: None,
            tags: None,
            data: None,
        })
    }
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub enum DiagnosticKind {
    MissingIri(Iri),
    SyntaxError {
        valid_children: String, // add some nicer info here if needed
        parent: String,
        msg: String, // Eg. "UNEXPECTED '\n'"
    },
}

/// Take this document, generate the diagnostics in workspace context and send the results via the client.
pub async fn publish_lsp_diagnostics(
    doc: &InternalDocument,
    workspace: &Workspace,
    encoding: &PositionEncodingKind,
    client: &tower_lsp::Client,
) {
    let diagnostics = diagnostics(doc, workspace)
        .iter()
        .map(|diagnostic| diagnostic.clone().into_lsp_diagnostic(doc.rope(), encoding))
        .filter_and_log()
        .collect_vec();

    // TODO create diagnostics for files that depend on this file
    debug!(
        "Publish diagnostics for {} {:#?}",
        doc.path().display(),
        diagnostics
    );

    client
        .publish_diagnostics(doc.uri().clone(), diagnostics, Some(doc.version()))
        .await;
}

pub fn diagnostics(doc: &InternalDocument, workspace: &Workspace) -> Vec<Diagnostic> {
    let workspace_diagnostics = timeit("semantic errors", || semantic_errors(doc, workspace));
    doc.local_diagnostics()
        .iter()
        .cloned()
        .chain(workspace_diagnostics)
        .collect_vec()
}

pub fn inlay_hint(
    doc: &InternalDocument,
    range: Range,
    encoding: &PositionEncodingKind,
    workspace: &Workspace,
) -> Vec<InlayHint> {
    let reachable_docs = reachable_docs_recursive(doc, workspace, true);
    debug!("Reachable docs for {} are {reachable_docs:#?}", doc.uri());
    doc.all_iris_in_range(range)
        .into_iter()
        .map(|rb| {
            let iri = rb.value().clone();
            let end = rb.range().end;

            let label = Workspace::get_frame_info_recursive(workspace, &iri, &reachable_docs)
                .ok_or(Error::FrameInfoNotFound(iri.to_string()))?
                .label(workspace)
                .unwrap_or_default();

            trace!("Found {label} for {iri}");

            let mut label_normalized = label.clone().to_lowercase();
            label_normalized.retain(char::is_alphanumeric);

            let same = iri.as_str().to_lowercase().contains(&label_normalized);

            if label.is_empty() || same {
                Ok(None)
            } else {
                Ok(Some(InlayHint {
                    position: end.into_lsp(doc.rope(), encoding)?,
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
        .collect()
}

/// Returns all document URL's that can be reached (imports, prefixes, ...) from this internal document.
/// Does not load anything.
pub fn reachable_docs_recursive(
    doc: &InternalDocument,
    workspace: &Workspace,
    include_prefix: bool,
) -> Vec<Url> {
    reachable_docs_recursive_cached(doc, workspace, include_prefix)
}

fn reachable_docs_recursive_helper(
    doc: &InternalDocument,
    result: &mut HashSet<Url>,
    workspace: &Workspace,
    include_prefix: bool,
) -> Result<()> {
    if result.contains(doc.uri()) {
        // Do nothing
        return Ok(());
    }

    result.insert(doc.uri().clone());

    let urls = if include_prefix {
        doc.directly_references_doc()
    } else {
        doc.directly_imports()
    };

    let docs = urls
        .iter()
        .inspect(|u| trace!("{u}"))
        .filter_map(|url| {
            workspace.document_by_url(url)
            // TODO maybe reactivate but for now lets not log here
            // .ok_or(Error::DocumentNotLoaded(url.clone())) //                    Workspace::resolve_url_to_document(&doc.try_get_workspace()?, &url)
            // .inspect_log()
            // .ok()
        })
        .collect_vec();

    // Ignore these debugging traces
    trace!("reachable docs recursive step tries:");
    trace!("but gets just:");
    for r in &docs {
        trace!("{r:?}");
    }
    trace!("internal documents loaded:");
    for p in workspace.internal_documents.keys() {
        trace!("{}", p.display());
    }
    trace!("external documents loaded:");
    for u in workspace.external_documents.keys() {
        trace!("{u}");
    }

    for doc in docs {
        match doc {
            DocumentReference::Internal(next_doc) => {
                reachable_docs_recursive_helper(next_doc, result, workspace, include_prefix)?;
            }
            DocumentReference::External(external_document) => {
                external_document.reachable_docs_recursive_helper(
                    workspace,
                    result,
                    0,
                    include_prefix,
                )?;
            }
        }
    }
    Ok(())
}

const RANGE_GROW: u32 = 1;

/// This is a combination of syntax and text changes
pub fn post_change_ranges(
    changes: &[Change],
    parsed_document: &ParsedDocument,
    old_tree: &Tree,
) -> Vec<Range> {
    old_tree
        .changed_ranges(&parsed_document.tree)
        .map(Range::from)
        .chain(changes.iter().map(Change::range_after_change))
        // Increase the range of a change by one char so that adjacent nodes are checked
        .map(|r| Range {
            start: r.start.moved_left(RANGE_GROW, &parsed_document.rope),
            end: r.end.moved_right(RANGE_GROW, &parsed_document.rope),
        })
        .collect_vec()
}

pub fn changes_from_lsp(
    params: DidChangeTextDocumentParams,
    encoding: &PositionEncodingKind,
    rope: &Rope,
) -> Vec<Change> {
    params
        .content_changes
        .into_iter()
        .map(|TextDocumentContentChangeEvent { range, text, .. }| {
            Range::from_lsp(&range.expect("range to be defined"), rope, encoding)
                .map(|range| Change { range, text })
        })
        .filter_and_log()
        .collect_vec()
}

impl ParsedDocument {
    pub fn edit_parsed_document<'a>(
        self,
        changes: impl Iterator<Item = &'a Change>,
        parser: &mut Parser,
    ) -> Result<(Self, Tree)> {
        let ParsedDocument { tree, rope, .. } = self;
        let mut old_tree = tree;
        let mut new_rope = rope;

        for change in changes {
            apply_change_to_rope_and_tree(&mut old_tree, &mut new_rope, change)?;
        }

        let rope_provider = RopeProvider::new(&new_rope);
        let new_tree = 
            // This takes a long time 190ms/16k lines
            timeit("document.edit / parsing", || {
                parser
                    .parse_with_options(
                        &mut |byte_idx, _| rope_provider.chunk_callback(byte_idx),
                        Some(&old_tree),
                        None,
                    )
                    .expect("language to be set, no timeout to be used, no cancellation flag")
            });
        
        let parsed_document = ParsedDocument {
            tree: new_tree,
            rope: new_rope,
        };
        Ok((parsed_document, old_tree))
    }

    /// Generate the diagnostics for a single node, walking recursively down to every child and every syntax error within
    pub fn syntax_errors(self: &ParsedDocument) -> Vec<Diagnostic> {
        let mut cursor = self.tree.root_node().walk();
        let mut diagnostics = Vec::<Diagnostic>::new();

        loop {
            let node = cursor.node();

            if node.is_error() {
                let leaf = first_leaf(node);
                let range: Range = cursor.node().range().into();

                let prev_node = node.prev_sibling().unwrap_or(node.parent().unwrap_or(node));

                let mut prev = if let Some(prev_sib) = node.prev_sibling() {
                    let prev_sibs_last_leaf = get_last_leaf(prev_sib);
                    prev_sibs_last_leaf
                } else {
                    prev_node
                };

                let mut parent_chain = vec![prev.kind()];
                while let Some(par) = prev.parent() {
                    parent_chain.push(par.kind());
                    prev = par;
                }

                let possible_nodes = possible_nodes(node)
                    .iter()
                    .map(ToString::to_string)
                    .collect();
                debug!("Possible nodes: {possible_nodes:#?}");
                debug!("Parent chain: {parent_chain:#?}");

                let parent_chain_children = parent_chain
                    .iter()
                    .flat_map(|kind| {
                        NODE_TYPES
                            .get(&(*kind).to_string())
                            .iter()
                            .flat_map(|t| {
                                t.children
                                    .types
                                    .iter()
                                    .map(|c| c.type_.clone())
                                    .collect_vec()
                            })
                            .collect_vec()
                    })
                    .collect::<HashSet<_>>();

                let valid_children = parent_chain_children.intersection(&possible_nodes);

                let valid_children: String = valid_children
                    .map(|nt| node_type_to_string(nt))
                    .sorted_unstable()
                    .join(", ");

                let parent = parent_chain
                    .iter()
                    .map(|parent_kind| node_type_to_string(parent_kind))
                    .join(" < ");

                diagnostics.push(Diagnostic {
                    range,
                    kind: DiagnosticKind::SyntaxError {
                        valid_children,
                        parent,
                        msg: leaf
                            .to_string()
                            .trim_start_matches('(')
                            .trim_end_matches(')')
                            .to_string(),
                    },
                });

                // move along
                while !cursor.goto_next_sibling() {
                    // move out
                    if !cursor.goto_parent() {
                        // this node has no parent, it's the root
                        return diagnostics;
                    }
                }
            } else if node.has_error() {
                // move in
                let has_child = cursor.goto_first_child(); // should always work

                if !has_child {
                    while !cursor.goto_next_sibling() {
                        // move out
                        if !cursor.goto_parent() {
                            // this node has no parent, it's the root
                            return diagnostics;
                        }
                    }
                }
            } else {
                // move along
                while !cursor.goto_next_sibling() {
                    // move out
                    if !cursor.goto_parent() {
                        // this node has no parent, it's the root
                        return diagnostics;
                    }
                }
            }
        }
    }
}

fn first_leaf(node: Node<'_>) -> Node<'_> {
    let mut current = node;
    while let Some(child) = current.child(0) {
        current = child;
    }
    current
}

fn possible_nodes(node: Node<'_>) -> HashSet<&'static str> {
    if !node.is_error() {
        return HashSet::new();
    }

    let state = first_leaf(node).parse_state();

    let names = LANGUAGE_OMN
        .lookahead_iterator(state)
        .map(|mut it| it.iter_names().collect())
        .unwrap_or_default();

    names
}

fn get_last_leaf(node: Node<'_>) -> Node<'_> {
    debug!("Get last leaf of {node}");
    if node.child_count() > 0 {
        if let Some(c) = node.child(node.child_count() - 1) {
            debug!("Goto child {c} with index {}", node.child_count() - 1);
            get_last_leaf(c)
        } else {
            node
        }
    } else {
        debug!("End on {node}");
        node
    }
}

// for debugging
// fn print_node(node: Node<'_>, depth: usize) {
//     debug!(
//         "{}- {} ({}) {} {} {node:?}",
//         " ".repeat(depth),
//         node.kind(),
//         node.grammar_name(),
//         node.parse_state(),
//         node.next_parse_state()
//     );
//     for i in 0..node.child_count() {
//         print_node(node.child(i).unwrap(), depth + 1);
//     }
// }

/// .
///
/// # Panics
///
/// Panics if change has no range.
///
/// # Errors
///
/// This function will return an error if the change is out of range.
pub fn apply_change_to_rope_and_tree(
    new_tree: &mut Tree,
    new_rope: &mut Rope, // This rope is always in UTF-8
    Change {
        range: old_range,
        text,
    }: &Change,
) -> Result<()> {
    // let range = range.expect("range to be defined");
    // let old_range = range: Range = Range::from_lsp(&range, &*new_rope, encoding)?;
    let start_byte = old_range.start.byte_index(&*new_rope);
    let old_end_byte = old_range.end.byte_index(&*new_rope);
    let start_char = new_rope.try_byte_to_char(start_byte)?;
    let old_end_char = new_rope.try_byte_to_char(old_end_byte)?;
    debug!("change range in bytes {start_byte}..{old_end_byte} and text {text}");
    new_rope.try_remove(start_char..old_end_char)?;
    new_rope.try_insert(start_char, text)?;
    let new_end_byte = start_byte + text.len();
    let new_end_position = Position::new_from_byte_index(&*new_rope, new_end_byte);
    let edit = InputEdit {
        start_byte,
        old_end_byte,
        new_end_byte,
        start_position: old_range.start.into(),
        old_end_position: old_range.end.into(),
        new_end_position: new_end_position.into(),
    };
    timeit("tree edit", || new_tree.edit(&edit));
    Ok(())
}

#[derive(Debug)]
pub struct FormattingSettings {
    pub tab_size: u32,
    pub ruler_width: u32,
    pub order_frames: bool,
}

/// An internal document that has no semantic analysis. Just text and syntax tree.
#[derive(Debug, Clone)]
pub struct ParsedDocument {
    tree: Tree,
    rope: Rope,
}

impl ParsedDocument {
    pub fn new(tree: Tree, rope: Rope) -> Self {
        Self { tree, rope }
    }

    pub fn rope(&self) -> &Rope {
        &self.rope
    }

    pub fn tree(&self) -> &Tree {
        &self.tree
    }
}

impl ParsedDocument {
    pub fn query(&'_ self, query: &Query) -> Vec<UnwrappedQueryMatch<'_>> {
        query_helper(self, query, None)
    }

    pub fn query_range(&'_ self, query: &Query, range: Range) -> Vec<UnwrappedQueryMatch<'_>> {
        query_helper(self, query, Some(range))
    }

    /// Returns the ontology IRI if possible and the version IRI if possible.
    pub fn ontology_id(&self) -> Option<RangeBox<OntologyId>> {
        match &self.query(&ALL_QUERIES.ontology)[..] {
            [] => None,
            [ontology] => match &ontology.captures[..] {
                [] => None,
                // This should be a full IRI so lets trim it
                [iri_capture] => Some(RangeBox::new(
                    (trim_full_iri_rope_slice(iri_capture.node.text).to_iri(), None),
                    iri_capture.node.range,
                )),
                [iri_capture, version_iri_capture] => Some(RangeBox::new(
                    (
                        trim_full_iri_rope_slice(iri_capture.node.text).to_iri(),
                        Some(trim_full_iri_rope_slice(version_iri_capture.node.text).to_iri()),
                    ),
                    Range::new(
                        iri_capture.node.range.start,
                        version_iri_capture.node.range.end,
                    ),
                )),
                _ => unreachable!("The query has only one capture"),
            },
            _ => unreachable!("the grammar only parses ontology zero or one time"),
        }
    }

    /// Returns the prefixes of a document (without colon `:`) in a prefix name to iri map.
    ///
    /// Some prefixes should always be defined
    ///
    /// ```owl-ms
    /// Prefix: rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    /// Prefix: rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    /// Prefix: xsd: <http://www.w3.org/2001/XMLSchema#>
    /// Prefix: owl: <http://www.w3.org/2002/07/owl#>
    /// ```
    pub fn prefixes(&self) -> HashMap<String, RangeBox<String>> {
        self.query(&ALL_QUERIES.prefix)
            .into_iter()
            .map(|m| match &m.captures[..] {
                [name_capture, iri_capture] => (
                    name_capture.node.text.to_string().trim_end_matches(':').to_string(),
                    RangeBox::new(
                        trim_full_iri_rope_slice(iri_capture.node.text).to_string(),
                        Range::new(name_capture.node.range.start, iri_capture.node.range.end),
                    ),
                ),
                _ => unreachable!(),
            })
            // Horned owl has no default here. Let's keep it out for now.
            // .chain(
            //     STANDARD_PREFIX_NAMES
            //         .iter()
            //         .map(|(a, b)| (a.to_string(), b.to_string())),
            // )
            .unique_by(|(k, v)| (k.clone(), v.value().clone()))
            .collect()
    }

    pub fn prefixes_in_range(&self, range: Range) -> HashMap<String, RangeBox<String>> {
        self.query_range(&ALL_QUERIES.prefix, range)
            .into_iter()
            .map(|m| match &m.captures[..] {
                [name_capture, iri_capture] => (
                    name_capture.node.text.to_string().trim_end_matches(':').to_string(),
                    RangeBox::new(
                    trim_full_iri_rope_slice(iri_capture.node.text).to_string(),
                        Range::new(name_capture.node.range.start, iri_capture.node.range.end),
                    ),
                ),
                _ => unreachable!(),
            })
            .unique_by(|(k, v)| (k.clone(), v.value().clone()))
            .collect()
    }

    pub fn imports(&self) -> Vec<RangeBox<Iri>> {
        self.query(&ALL_QUERIES.import_query)
            .iter()
            .filter_map(|m| match &m.captures[..] {
                [iri_capture] => Oxiri::parse(trim_full_iri_rope_slice(iri_capture.node.text).to_string())
                    .ok()
                    .map(|iri_| RangeBox::new(iri_.as_str().to_iri(), iri_capture.node.range)),
                _ => unimplemented!(),
            })
            .collect_vec()
    }

    pub fn imports_in_range(&self, range: Range) -> Vec<RangeBox<Iri>> {
        self.query_range(&ALL_QUERIES.import_query, range)
            .iter()
            .filter_map(|m| match &m.captures[..] {
                [iri_capture] => Oxiri::parse(trim_full_iri_rope_slice(iri_capture.node.text).to_string())
                    .ok()
                    .map(|iri_| RangeBox::new(iri_.as_str().to_iri(), iri_capture.node.range)),
                _ => unimplemented!(),
            })
            .collect_vec()
    }
}

// TODO consumes a lot of memory
fn query_helper<'a>(
    stage1: &'a ParsedDocument,
    query: &Query,
    range: Option<Range>,
) -> Vec<UnwrappedQueryMatch<'a>> {
    let mut query_cursor = QueryCursor::new();
    if let Some(range) = range {
        if range != Range::FULL_RANGE {
            query_cursor.set_point_range(range.into());
        }
    }
    let rope_provider = RopeProvider::new(&stage1.rope);

    query_cursor
        .matches(query, stage1.tree.root_node(), rope_provider)
        .map_deref(|m| UnwrappedQueryMatch {
            pattern_index: m.pattern_index,
            _id: m.id(),
            captures: m
                .captures
                .iter()
                .sorted_by_cached_key(|c| c.index) // TODO Does this use less memory now?
                .map(|c| UnwrappedQueryCapture {
                    node: UnwrappedNode {
                        id: c.node.id(),
                        text: node_text(&c.node, &stage1.rope),
                        range: c.node.range().into(),
                        kind: c.node.kind(),
                        parent_kind: c.node.parent().map(|p| p.kind()),
                    },
                    index: c.index,
                })
                .collect_vec(),
        })
        .collect_vec()
}

pub type OntologyId = (Iri, Option<Iri>);


pub fn capture_by_name<'a>(
    query: &'a Query,
    captures: &'a [UnwrappedQueryCapture],
    name: &'static str,
) -> Option<&'a UnwrappedQueryCapture<'a>> {
    captures.iter().find(|c| {
        c.index
            == query
                .capture_index_for_name(name)
                .expect("capture name should be valid")
    })
}

pub fn build_iri_locations(references: &Vec<RangeBox<Iri>>) -> HashMap<Iri, Vec<RangeBox<()>>> {
    let mut iri_locations: HashMap<Iri, Vec<RangeBox<()>>> =
        HashMap::with_capacity(references.len());
    for rb in references {
        iri_locations
            .entry(rb.value().clone())
            .or_default()
            .push(RangeBox::new((), *rb.range()));
    }
    iri_locations
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

/// External documents are ontologies that are not expected to change in any way.
#[derive(Debug)]
pub struct ExternalDocument {
    pub uri: Url,
    pub text: String,
    pub graph: InfoGraph,
    reachable_urls: Vec<Url>,
    imports: Vec<Url>,
    definitions: HashSet<Iri>,
}

impl PartialEq for ExternalDocument {
    fn eq(&self, other: &Self) -> bool {
        self.uri == other.uri
    }
}

impl Eq for ExternalDocument {}

#[derive(Debug)]
pub struct InfoGraph(LightGraph, GraphName);

type GraphName = String;

impl From<SetOntology<ArcStr>> for InfoGraph {
    fn from(value: SetOntology<ArcStr>) -> Self {
        let mut graph = LightGraph::new();

        let ontology_iri = &value.iter().find_map(|ac| match &ac.component {
            Component::OntologyID(horned_owl::model::OntologyID {
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
                                    IriRef::new_unchecked(MownStr::from_ref(STRING_IRI)),
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
                            // This should not happen :>
                            error!("The term index is full");
                        }
                    }
                    AnnotationSubject::AnonymousIndividual(_) => {
                        // TODO support anonymous individual
                    }
                },
                Component::Import(horned_owl::model::Import(iri)) => {
                    if let Some(subject) = ontology_iri {
                        let predicate = SimpleTerm::Iri(IriRef::new_unchecked(MownStr::from_ref(
                            "http://www.w3.org/2002/07/owl#imports",
                        )));
                        let object = SimpleTerm::Iri(
                            IriRef::new(MownStr::from_ref(iri)).expect("valid IRI"),
                        );
                        graph
                            .insert(subject, predicate, object)
                            .expect("graph should not be full");
                    }
                }
                Component::DeclareClass(DeclareClass(horned_owl::model::Class(iri)))
                | Component::DeclareDatatype(DeclareDatatype(Datatype(iri)))
                | Component::DeclareObjectProperty(DeclareObjectProperty(ObjectProperty(iri)))
                | Component::DeclareAnnotationProperty(DeclareAnnotationProperty(
                    AnnotationProperty(iri),
                ))
                | Component::DeclareDataProperty(DeclareDataProperty(DataProperty(iri)))
                | Component::DeclareNamedIndividual(DeclareNamedIndividual(NamedIndividual(iri))) =>
                {
                    let subject = SimpleTerm::Iri(
                        IriRef::new(MownStr::from_ref(iri)).expect("Class IRI should be valid IRI"),
                    );
                    let predicate =
                        SimpleTerm::Iri(IriRef::new_unchecked(MownStr::from_ref(IRI_RDF_TYPE)));

                    let object = SimpleTerm::Iri(IriRef::new_unchecked(MownStr::from_ref(
                        "http://www.w3.org/2002/07/owl#Class",
                    )));
                    graph
                        .insert(subject, predicate, object)
                        .expect("graph should not be full");
                }
                _ => (),
            }
        }

        let graph_name = ontology_iri
            .as_ref()
            .and_then(|s| s.iri().clone())
            .map_or("???".into(), |i| i.to_string()); //TODO what default graph name?

        Self(graph, graph_name)
    }
}

const IRI_RDF_TYPE: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type";

impl Hash for ExternalDocument {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.uri.hash(state);
    }
}

impl ExternalDocument {
    /// The ontology type is currently determent by tial and error
    pub fn new(text: String, url: Url) -> Result<ExternalDocument> {
        debug!("try creating external document... {url}");

        // Try parsing different styles
        // This could try more styles in the future

        debug!("try rdf...");
        let builder = lock_global_build_arc();
        let doc = ExternalDocument::try_parse_rdf(&text)
            .or_else(|e| {
                warn!("rdf failed with {e}");
                debug!("try rdf...");
                ExternalDocument::try_parse_owx(&text, &builder)
            })
            .map(|graph| {
                let imports = ExternalDocument::gen_imports(&graph).collect_vec();

                let reachable_urls =
                    ExternalDocument::gen_reachable_urls(&url, &graph, &imports).collect_vec();

                ExternalDocument {
                    reachable_urls,
                    definitions: ExternalDocument::gen_definitions(&graph),
                    imports,
                    graph,
                    text,
                    uri: url,
                }
            });

        if let Ok(doc) = &doc {
            debug!("parsing worked! {}", doc.uri);
        }
        doc
    }

    fn try_parse_rdf(text: &str) -> Result<InfoGraph> {
        sophia::xml::parser::parse_str(text)
            .collect_triples::<LightGraph>()
            .map_err(|e| Error::Sophia(format!("{e}")))
            .map(|g| {
                // Find Match for: x <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/2002/07/owl#Ontology>.
                let owl = Namespace::new_unchecked("http://www.w3.org/2002/07/owl#");
                let owl_ontology = owl.get("Ontology").unwrap();
                let ontology_id = &g
                    .triples_matching(Any, [sophia::api::ns::rdf::type_], [owl_ontology])
                    .flatten()
                    .next()
                    .and_then(|[s, _, _]| s.iri())
                    .map_or("???".into(), |i| i.to_string()); //TODO default case

                InfoGraph(g, ontology_id.clone())
            })
    }

    fn try_parse_owx(text: &str, builder: &Build<ArcStr>) -> Result<InfoGraph> {
        // let builder = lock_global_build_arc().await;
        let mut buffer = text.as_bytes();
        let (ontology, _) = horned_owl::io::owx::reader::read_with_build::<
            ArcStr,
            SetOntology<ArcStr>,
            _,
        >(&mut buffer, builder)?;
        let graph: InfoGraph = ontology.into();

        Ok(graph)
    }

    fn gen_imports(graph: &InfoGraph) -> Box<dyn Iterator<Item = Url> + '_> {
        let graph = &graph.0;

        // `graph.triples_matching` will exceed the stack size (Stack Overflow) on large graphs
        let iris = graph
            .triples()
            .flatten()
            .filter(|[_, p, _]| {
                p.iri()
                    .is_some_and(|iri| iri.as_str() == "http://www.w3.org/2002/07/owl#imports")
            })
            .filter_map(|[_, _, o]| o.iri())
            .flat_map(|iri| Url::parse(&iri))
            .unique();

        Box::new(iris)
    }

    pub fn imports(&self) -> &Vec<Url> {
        &self.imports
    }

    pub fn definitions(&self) -> &HashSet<Iri> {
        debug!("Definitions of {} are {:#?}", self.uri, self.definitions);
        &self.definitions
    }

    pub fn gen_definitions(graph: &InfoGraph) -> HashSet<Iri> {
        let mut hash_set = HashSet::new();
        graph
            .0
            .triples()
            .flatten()
            .filter_map(|[s, p, o]| {
                p.iri().map(|iri| {
                    debug!("{s:?} {iri:?} {o:?}");
                    if let Some(subject_iri) = s.iri() {
                        if iri.as_str() == IRI_RDF_TYPE {
                            hash_set.insert(subject_iri.as_str().to_iri());
                        }
                    }
                })
            })
            .collect_vec();
        hash_set
    }

    // Because external documents most likely relate to other external ones and because there are many of them in a graph the depth should be limited
    fn reachable_docs_recursive_helper(
        &self,
        workspace: &Workspace,
        result: &mut HashSet<Url>,
        depth: u32,
        include_prefix: bool,
    ) -> Result<()> {
        if depth >= 1 {
            // Do nothing max depth reached
            return Ok(());
        }
        if result.contains(&self.uri) {
            // Do nothing
            return Ok(());
        }

        let urls = self.reachable_urls();

        result.insert(self.uri.clone());

        // TODO shitty child urls :<
        let docs = urls.iter().filter_map(|url| {
            workspace.document_by_url(url)
            // TODO maybe reactivate but for now lets not log here
            // .ok_or(Error::DocumentNotLoaded(url.clone()))
            // Workspace::resolve_url_to_document(&self.try_get_workspace()?, &url)
            // .inspect_log()
            // .ok()
        });

        for doc in docs {
            match doc {
                DocumentReference::Internal(internal_document) => {
                    reachable_docs_recursive_helper(
                        internal_document,
                        result,
                        workspace,
                        include_prefix,
                    )?;
                }
                DocumentReference::External(external_document) => {
                    external_document.reachable_docs_recursive_helper(
                        workspace,
                        result,
                        0,
                        include_prefix,
                    )?;
                }
            }
        }

        Ok(())
    }

    fn gen_reachable_urls<'a>(
        uri: &'a Url,
        graph: &'a InfoGraph,
        imports: &'a [Url],
    ) -> Box<dyn Iterator<Item = Url> + 'a> {
        // TODO this is not that stable yet
        let child_urls = graph
            .0
            .iris()
            .filter_map(std::result::Result::ok)
            .filter_map(|term| term.iri())
            .unique()
            .filter_map(|iri| Url::parse(&iri).ok())
            // Filter out IRI's that point to this ontology
            .filter(|url| {
                // This should be faster than url.make_relative, because url contains a serialized version
                !url.to_string()
                    .starts_with(uri.to_string().trim_end_matches('#'))
            })
            .filter(|url| !url.to_string().contains(&graph.1))
            .map(iri_to_onology_url)
            .unique();

        Box::new(imports.iter().cloned().chain(child_urls).unique())
    }

    pub fn reachable_urls(&self) -> &Vec<Url> {
        &self.reachable_urls
    }

    pub fn get_frame_info(&self, iri: &Iri) -> Option<FrameInfo> {
        let x = get_frame_info_helper_ex(self, iri);
        trace!("frame info for {iri} is {x:#?}");
        x
    }
}

pub fn iri_to_parent_url(iri: &Iri) -> Option<Url> {
    let url = Url::parse(iri).ok()?;
    Some(iri_to_onology_url(url))
}

/// Convert some IRI (here in URL type) into a URL where the IRI can be fetched from
pub fn iri_to_onology_url(mut url: Url) -> Url {
    if url.fragment().is_some() {
        url.set_fragment(Some(""));
    } else if let Ok(mut seg) = url.path_segments_mut() {
        // TODO check for obo ontology
        // See https://obofoundry.org/principles/fp-003-uris.html

        seg.pop();
    }
    url
}

#[cached(
    size = 2000,
    key = "u64",
    convert = r#"{
            let mut hasher = DefaultHasher::new();
            doc.hash(&mut hasher);
            iri.hash(&mut hasher);
            hasher.finish()
     } "#
)]
fn get_frame_info_helper_ex(doc: &ExternalDocument, iri: &Iri) -> Option<FrameInfo> {
    let graph = &doc.graph.0;

    graph
        .triples_matching(
            // TODO maybe convert the other way around(as_str)?
            |s: SimpleTerm| s.iri().is_some_and(|subject| subject.as_str() == iri.as_str()),
            Any,
            Any,
        )
        .flatten()
        .map(|[s, p, o]| {
            let ann = simple_term_to_annotation(o);
            FrameInfo {
                iri: iri.clone(),
                annotations: vec![Annotation {
                    frame_iri: simple_term_literal(s).into(),
                    iri: simple_term_literal(p).into(),
                    string_value: ann.0,
                    language: ann.2,
                    datatype: ann.1.into(),
                }],
                frame_type: FrameType::Unknown,
                definitions: vec![Location {
                    uri: doc.uri.clone(),
                    range: Range::ZERO,
                }],
            }
        })
        .tree_reduce(FrameInfo::merge)
}

fn simple_term_to_annotation(
    simple_term: &SimpleTerm,
) -> (String, String, Option<language::Language>) {
    // TODO some string iri datatypes are not correct here
    match simple_term {
        SimpleTerm::Iri(iri_ref) => (iri_ref.to_string(), STRING_IRI.to_string(), None), // TODO string_iri is not the correct one here right?
        SimpleTerm::BlankNode(bnode_id) => (bnode_id.to_string(), STRING_IRI.to_string(), None),
        SimpleTerm::LiteralDatatype(mown_str, iri_ref) => {
            (mown_str.to_string(), iri_ref.to_string(), None)
        }

        SimpleTerm::LiteralLanguage(mown_str, language_tag) => (
            mown_str.to_string(),
            STRING_IRI.to_string(),
            language::Language::try_from(language_tag.to_string()).ok(),
        ),
        SimpleTerm::Triple(_) => ("TODO triple".into(), STRING_IRI.into(), None),
        SimpleTerm::Variable(var_name) => (var_name.to_string(), STRING_IRI.into(), None),
    }
}

fn simple_term_literal(simple_term: &SimpleTerm) -> String {
    match simple_term {
        SimpleTerm::Iri(iri_ref) => iri_ref.to_string(),
        SimpleTerm::BlankNode(bnode_id) => bnode_id.to_string(),
        SimpleTerm::LiteralDatatype(mown_str, _) => mown_str.to_string(),

        SimpleTerm::LiteralLanguage(mown_str, _) => {
            format!("{mown_str}")
        }
        SimpleTerm::Triple(_) => "TODO triple".into(),
        SimpleTerm::Variable(var_name) => var_name.to_string(),
    }
}

/// This is a version of a query match that has no reference to the tree or cursor
#[derive(Debug, PartialEq, Eq)]
pub struct UnwrappedQueryMatch<'a> {
    pattern_index: usize,
    _id: u32,
    pub captures: Vec<UnwrappedQueryCapture<'a>>,
}

/// This is a version of a query capture that has no reference to the tree or cursor
#[derive(Debug, PartialEq, Eq)]
pub struct UnwrappedQueryCapture<'a> {
    pub node: UnwrappedNode<'a>,
    pub index: u32,
}

/// This is a version of a node that has no reference to the tree
#[derive(Debug, PartialEq, Eq)]
pub struct UnwrappedNode<'a> {
    /// ID's of a changed tree stay the same. So you can search for up-to-date information that way
    pub id: usize,
    /// This information can be outdated
    pub text: RopeSlice<'a>,
    /// This information can be outdated
    pub range: Range,
    pub kind: &'static str,
    pub parent_kind: Option<&'static str>,
}

/// This represents information about a frame.
/// For example the following frame has information.
/// ```owl-ms
/// Class: PizzaThing
///     Annotations: rdfs:label "Pizza"
/// ```
/// Then the [`FrameInfo`] contains the label "Pizza" and the frame type "Class".
#[derive(Clone, Debug)]
pub struct FrameInfo {
    pub iri: Iri,
    pub annotations: Vec<Annotation>,
    pub frame_type: FrameType,
    pub definitions: Vec<Location>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Annotation {
    pub frame_iri: Iri,
    pub iri: Iri,
    pub string_value: String,
    pub language: Option<language::Language>,
    pub datatype: Iri,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
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

    pub fn extend(&mut self, b: FrameInfo) {
        self.annotations.extend(b.annotations);
        self.annotations.sort_unstable();
        self.annotations.dedup();
        self.definitions.extend(b.definitions);
        self.definitions.sort_unstable();
        self.definitions.dedup();
        self.frame_type = match (self.frame_type, b.frame_type) {
            (a, b) if a == b => a,
            (FrameType::Unknown, b) => b,
            (a, FrameType::Unknown) => a,
            _ => FrameType::Invalid,
        };
    }

    pub fn label(&self, workspace: &Workspace) -> Option<String> {
        self.annotation_display(&LABEL_IRI.into(), workspace)
    }

    pub fn annotation_display(&self, iri: &Iri, workspace: &Workspace) -> Option<String> {
        let joined = self
            .annotations
            .iter()
            .filter(|annotation| &annotation.iri == iri)
            .map(|annotation| {
                if let Some(language) = &annotation.language {
                    format!("{} @ {}", &annotation.string_value, &language.name())
                } else if annotation.datatype != STRING_IRI.into() && iri != &LABEL_IRI.into()
                // Endless recursion if label is a valid iri
                {
                    let datatype_label = workspace
                        .get_frame_info(&annotation.datatype)
                        .map(|fi| {
                            fi.label(workspace)
                                .unwrap_or_else(|| trim_url_before_last(&fi.iri).to_string())
                        })
                        .unwrap_or(annotation.datatype.to_string());

                    format!("{} ^^ {}", annotation.string_value, datatype_label)
                } else {
                    annotation.string_value.clone()
                }
            })
            .join(", ");

        if joined.is_empty() {
            None
        } else {
            Some(joined)
        }
    }

    pub fn info_display(&self, workspace: &Workspace) -> String {
        let entity = self.frame_type;
        let label = self
            .label(workspace)
            .unwrap_or(trim_url_before_last(&self.iri).to_string());

        debug!("info display / frame annotations {:#?}", self.annotations);

        let annotations = self
            .annotations
            .iter()
            // This should be unique, because they get joined in `annotation_display`
            .unique_by(|a| &a.iri)
            .map(|annotation| {
                let iri_label = workspace
                    .get_frame_info(&annotation.iri)
                    .map(|fi| {
                        fi.label(workspace)
                            .unwrap_or_else(|| trim_url_before_last(&fi.iri).to_string())
                    })
                    .unwrap_or(annotation.iri.to_string());
                // TODO #28 use values directly
                let mut annotation_display = self
                    .annotation_display(&annotation.iri, workspace)
                    .unwrap_or(annotation.iri.to_string());

                // If this is a multiline string then give it some space to work with
                if annotation_display.contains('\n') {
                    annotation_display = format!("\n{annotation_display}\n\n");
                }

                format!("- `{iri_label}`: {annotation_display}")
            })
            .join("\n");

        format!(
            "{entity} **{label}**\n\n---\n{annotations}\n\nIRI: {}",
            self.iri
        )
    }

    /// This is a quick and dirty matcher that returns a match score from `0` to ``usize::MAX``
    pub fn matches(&self, query: &str) -> usize {
        let mut sum = 0usize;
        if self.iri.contains(query) {
            sum += 5000;
        }
        for annotation in &self.annotations {
            if annotation.string_value.contains(query) {
                if annotation.iri == LABEL_IRI.into() {
                    sum += 1000;

                    if let Some((l, r)) = annotation.string_value.split_once(query) {
                        // Starts with query
                        if l.is_empty() {
                            sum += 100;
                        }
                        // Ends with query
                        if r.is_empty() {
                            sum += 10;
                        }

                        // Query found at exact word boundary
                        if r.starts_with(' ') && l.ends_with(' ') {
                            sum += 10;
                        }

                        // Chars not matching query
                        sum = sum.saturating_sub(l.len() * 10);
                        sum = sum.saturating_sub(r.len() * 10);
                        sum += 1;
                    }
                } else {
                    sum += 1;
                }
            }
        }
        sum
    }
}

fn trim_url_before_last(iri: &str) -> &str {
    iri.rsplit_once(['/', '#']).map_or(iri, |(_, b)| b)
}

pub fn trim_string_value(value: &str) -> String {
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

pub fn node_text<'a>(node: &Node, rope: &'a Rope) -> RopeSlice<'a> {
    rope.get_byte_slice(node.start_byte()..node.end_byte()).expect("tree and rope should be synced")
}

fn semantic_errors(doc: &InternalDocument, workspace: &Workspace) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();

    let uses: &HashSet<Iri> = &doc
        .references()
        .iter()
        .map(|rb| rb.value().clone())
        .collect();

    let mut defines: HashSet<Iri> = doc
        .definitions()
        .iter()
        .map(|rb| rb.value().iri.clone())
        .collect();

    debug!(
        "semantic_errors / doc {:?} uses {uses:#?} defines {defines:#?}",
        doc.uri()
    );

    let imports_recursive = timeit("semantic errors  reachable", || {
        reachable_docs_recursive(doc, workspace, false)
    });
    debug!("Imports recursive {} {:#?}", doc.uri(), imports_recursive);

    for url in imports_recursive {
        if let Some(doc) = workspace.document_by_url(&url) {
            match doc {
                DocumentReference::Internal(internal_document) => {
                    defines.extend(
                        internal_document
                            .definitions()
                            .iter()
                            .map(|rb| rb.value().iri.clone()),
                    );
                }
                DocumentReference::External(external_document) => {
                    defines.extend(external_document.definitions().clone());
                }
            }
        }
    }

    // TODO this is a quick fix for now. The correct way will be not not include prefixes in the used Iris
    let prefixes = doc.prefixes();
    let prefix_iris: HashSet<&str> = prefixes.values().map(String::as_str).collect();
    let import_iris: Vec<&str> = doc
        .directly_imports()
        .into_iter()
        .map(Url::as_str)
        .collect();

    let ontology_iri = doc.ontology_iri();
    let version_iri = doc.version_iri();

    for diff in uses
        .difference(&defines)
        .filter(|iri| !prefix_iris.contains(&iri.as_str()))
        .filter(|iri| !import_iris.contains(&iri.as_str()))
    {
        // Skip ontology and version IRIs
        if let Some(ref iri) = ontology_iri {
            if diff == iri {
                continue;
            }
        }
        if let Some(ref version) = version_iri {
            if diff == version {
                continue;
            }
        }

        if let Some(vec) = doc.iri_locations().get(diff) {
            for ele in *vec {
                diagnostics.push(Diagnostic {
                    range: *ele.range(),
                    kind: DiagnosticKind::MissingIri(diff.clone()),
                });
            }
        }
    }

    diagnostics
}

fn node_type_to_string(node_type: &str) -> String {
    node_type
        .trim_start_matches("keyword_")
        .split_terminator('_')
        .map(capitalize_string)
        .join(" ")
}

fn capitalize_string(s: &str) -> String {
    let mut c = s.chars();
    match c.next() {
        None => String::new(),
        Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
    }
}

/// taken from <https://www.w3.org/TR/owl2-syntax/#Entity_Declarations_and_Typing>
#[derive(Clone, Copy, Eq, PartialEq, Debug, Hash)]
pub enum FrameType {
    Class,
    DataType,
    ObjectProperty,
    DataProperty,
    AnnotationProperty,
    Individual,
    Ontology,

    /// The frame type of IRI that has no valid frame (this can be because of conflicts)
    Invalid,
    /// The frame type of IRI that has no frame at all (can be overridden)
    Unknown,
}

impl FrameType {
    pub fn parse(kind: &str) -> FrameType {
        match kind {
            "datatype_iri" | "datatype_frame" => FrameType::DataType,
            "annotation_property_iri"
            | "annotation_property_frame"
            | "annotation_property_iri_annotated_list" => FrameType::AnnotationProperty,
            "individual_iri" | "individual_frame" => FrameType::Individual,
            "ontology_iri" | "ontology_frame" | "version_iri" => FrameType::Ontology,
            "data_property_iri" | "data_property_frame" => FrameType::DataProperty,
            "object_property_iri" | "object_property_frame" => FrameType::ObjectProperty,
            "class_frame" | "class_iri" => FrameType::Class,
            kind => {
                error!("FrameType parse failed: Implement frame type for {kind}");
                FrameType::Invalid
            }
        }
    }

    pub fn to_definition(self) -> Option<String> {
        match self {
            FrameType::Class => Some("Class:".to_string()),
            FrameType::DataType => Some("Datatype:".to_string()),
            FrameType::ObjectProperty => Some("ObjectProperty:".to_string()),
            FrameType::DataProperty => Some("DataProperty:".to_string()),
            FrameType::AnnotationProperty => Some("AnnotationProperty:".to_string()),
            FrameType::Individual => Some("Individual:".to_string()),
            FrameType::Ontology => Some("Ontology:".to_string()),
            FrameType::Invalid | FrameType::Unknown => None,
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
pub fn trim_full_iri(untrimmed_iri: &str) -> Iri {
    trim_tags(untrimmed_iri).into()
}
 
/// Takes an IRI in any form and removed the <> symbols
pub fn trim_full_iri_rope_slice(slice: RopeSlice<'_>) -> RopeSlice<'_> {
    if slice.len_chars() == 0 {
        return slice
    }

    let slice  = slice.get_char(0).map_or_else(|| slice,|c|
        if c == '<' {
           slice.slice(1..) 
        } else {
            slice
        }

    );

    let slice =  slice.get_char(slice.len_chars()-1).map_or_else(|| slice, |c|  {
        if c == '>' {
           slice.slice(..slice.len_chars()-1) 
        } else {
            slice
        } } );


    slice
}
 

// Horned owl has no default here. Let's keep it out for now.
// static STANDARD_PREFIX_NAMES: [(&str, &str); 4] = [
//     ("rdf:", "http://www.w3.org/1999/02/22-rdf-syntax-ns#"),
//     ("rdfs:", "http://www.w3.org/2000/01/rdf-schema#"),
//     ("owl:", "http://www.w3.org/2002/07/owl#"),
//     ("xsd:", "http://www.w3.org/2001/XMLSchema#"),
// ];

// This can not be cached, because some dependencies are maybe not loaded.
// Therefore the result could change indepenent of the document.
fn reachable_docs_recursive_cached(
    doc: &InternalDocument,
    workspace: &Workspace,
    include_prefix: bool,
) -> Vec<Url> {
    let mut set: HashSet<Url> = HashSet::new();
    reachable_docs_recursive_helper(doc, &mut set, workspace, include_prefix).log_if_error();
    set.into_iter().collect_vec()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::pos::Position;
    use indoc::indoc;
    use pretty_assertions::assert_eq;
    use tempdir::TempDir;
    use test_log::test;

    /// Well the file:/// syntax is not valid for all OS's, that's why generating a random file URL is easier.
    struct TmpUrl {
        url: Url,
        _tmp_dir: TempDir,
    }

    impl TmpUrl {
        fn new() -> Self {
            let tmp_dir = TempDir::new("owl-ms-test").unwrap();
            let url = Url::from_file_path(tmp_dir.path().join("file.omn")).unwrap();
            Self {
                url,
                _tmp_dir: tmp_dir,
            }
        }

        fn url(&self) -> Url {
            self.url.clone()
        }
    }

    #[test(tokio::test)]
    async fn internal_document_formatted_should_format_correctly() {
        let tmp_url = TmpUrl::new();
        let doc = InternalOmnDocument::new(
            tmp_url.url(),
            -1,
            indoc! {"
                Prefix:  a:  <http://a/a>  Prefix:  a:  <http://a/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa>    Ontology:   a   v   Import:    <http://a/a>    Import:    <http://a/a> Annotations:    rdfs:label     \"a\"    Class:   a   SubClassOf:   b,e,f SubClassOf:   cccccccccccccccccccccccc,ddddddddddddddddddddd,eeeeeeeeeee   Class:   a     SubClassOf: a    Annotations:   rdfs:label    \"Y\"    EquivalentTo:    a   ,   a DisjointWith:    a  ,  a   DisjointUnionOf:  Annotations: y 12, a 2    a,a    HasKey:    a
            "}
            .into(),
        );

        info!("sexp:\n{}", doc.tree().root_node().to_sexp());

        let result = doc.formatted(&FormattingSettings {
            tab_size: 4,
            ruler_width: 35,
            order_frames: true,
        });

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

    #[test(tokio::test)]
    async fn internal_document_formatted_with_description_should_format_correctly() {
        let tmp_url = TmpUrl::new();
        let doc = InternalOmnDocument::new(
            tmp_url.url(),
            -1,
            indoc! {r"
                Ontology:a
                Class: a
                SubClassOf:   (aaaaaaaa and bbbbbb)    or   (bbbb and hasRel some (ccccccc or ddddddd or eeeeeeeee))
            "}
            .into(),
        );

        info!("sexp:\n{}", doc.tree().root_node().to_sexp());

        let result = doc.formatted(&FormattingSettings {
            tab_size: 4,
            ruler_width: 35,
            order_frames: true,
        });

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

    #[test(tokio::test)]
    async fn internal_document_formatted_without_frame_order_should_format_correctly() {
        let tmp_url = TmpUrl::new();
        let doc = InternalOmnDocument::new(
            tmp_url.url(),
            -1,
            indoc! {r"
                Ontology:a
                Class: a
                AnnotationProperty: a
            "}
            .into(),
        );

        let result = doc.formatted(&FormattingSettings {
            tab_size: 4,
            ruler_width: 35,
            order_frames: false,
        });

        assert_eq!(
            result,
            indoc! {r"
                Ontology: a

                
                Class: a

                
                AnnotationProperty: a

            "}
        );
    }

    #[test(tokio::test)]
    async fn internal_document_abbreviated_iri_to_full_iri_should_convert_abbreviated_iri() {
        let tmp_url = TmpUrl::new();
        let doc = InternalOmnDocument::new(
            tmp_url.url(),
            -1,
            "
                Prefix: owl: <http://www.w3.org/2002/07/owl#>
                Prefix: ja: <http://www.semanticweb.org/janek/ontologies/2025/5/untitled-ontology-3/>
            "
            .into(),
        );

        let full_iri = doc.abbreviated_iri_to_full_iri(&"owl:Nothing".into());
        let full_iri_2 = doc.abbreviated_iri_to_full_iri(&"ja:Janek".into());

        assert_eq!(
            full_iri,
            Some("http://www.w3.org/2002/07/owl#Nothing".into())
        );
        assert_eq!(
            full_iri_2,
            Some(
                "http://www.semanticweb.org/janek/ontologies/2025/5/untitled-ontology-3/Janek"
                    .into()
            )
        );
    }

    #[test]
    fn internal_document_abbreviated_iri_to_full_iri_should_convert_simple_iri() {
        let tmp_url = TmpUrl::new();
        let doc = InternalOmnDocument::new(
            tmp_url.url(),
            -1,
            "
                Prefix: : <http://www.w3.org/2002/07/owl#>
            "
            .into(),
        );

        let full_iri = doc.abbreviated_iri_to_full_iri(&":Nothing".into());
        let full_iri_2 = doc.abbreviated_iri_to_full_iri(&"Nothing".into());

        assert_eq!(
            full_iri,
            Some("http://www.w3.org/2002/07/owl#Nothing".into())
        );
        assert_eq!(
            full_iri_2,
            Some("http://www.w3.org/2002/07/owl#Nothing".into())
        );
    }

    #[test]
    fn internal_document_prefix_should_return_all_prefixes() {
        let tmp_url = TmpUrl::new();
        let doc = InternalOmnDocument::new(
            tmp_url.url(),
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

        let prefixes = doc
            .prefixes()
            .iter()
            .map(|(a, b)| (a.clone(), b.clone()))
            .sorted()
            .collect_vec();

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
        let tmp_url = TmpUrl::new();
        let doc = InternalOmnDocument::new(
            tmp_url.url(),
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
        let info = doc.find_frame_info(&"A".into());

        // Assert
        info!("{doc:#?}");
        let info = info.unwrap();

        assert_eq!(info.iri, "A".into());
        assert_eq!(
            info.definitions,
            vec![Location {
                uri: tmp_url.url(),
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
        let tmp_url = TmpUrl::new();
        let owl_ontology_text = format!(
            r#"
            <?xml version="1.0"?>
            <Ontology xmlns="http://www.w3.org/2002/07/owl#" xml:base="http://www.example.com/iri" ontologyIRI="http://www.example.com/iri">
                <Import>{}</Import>
                <Import>http://www.example.com/other-property</Import>
                <Declaration>
                    <Class IRI="https://www.example.com/o9"/>
                </Declaration>
            </Ontology>
        "#,
            tmp_url.url()
        );

        let external_doc = ExternalDocument::new(
            owl_ontology_text.clone(),
            Url::parse("https://example.com/onto.owx").unwrap(),
        )
        .unwrap();

        // Act
        let urls = external_doc.imports();

        // Assert
        assert!(urls.contains(&Url::parse("http://www.example.com/other-property").unwrap()));
        assert!(urls.contains(&tmp_url.url()));
    }

    #[test]
    fn word_before_character_should_find_word() {
        let word = word_before_character(25, "This is a line with multi words");
        assert_eq!(word, "multi");
    }

    #[test]
    fn full_iri_to_abbreviated_iri_should_work_for_simple_iris() {
        let tmp_url = TmpUrl::new();
        let doc = InternalOmnDocument::new(
            tmp_url.url(),
            -1,
            "
                Prefix: owl: <http://www.w3.org/2002/07/owl#>
            "
            .into(),
        );

        let abbr_iri = doc.full_iri_to_abbreviated_iri(&"http://www.w3.org/2002/07/owl#Thing".into());

        assert_eq!(abbr_iri, Some("owl:Thing".to_string()));
    }

    #[test]
    fn full_iri_to_abbreviated_iri_should_work_for_simple_iris_with_empty_prefix() {
        let tmp_url = TmpUrl::new();
        let doc = InternalOmnDocument::new(
            tmp_url.url(),
            -1,
            "
                Prefix: : <http://www.w3.org/2002/07/owl#>
            "
            .into(),
        );

        let abbr_iri = doc.full_iri_to_abbreviated_iri(&"http://www.w3.org/2002/07/owl#Thing".into());

        assert_eq!(abbr_iri, Some("Thing".to_string()));
    }
}

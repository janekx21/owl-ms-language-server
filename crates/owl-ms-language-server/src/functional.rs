use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    path::{Path, PathBuf},
    sync::LazyLock,
};

use itertools::Itertools;
use log::{debug, trace};
use ropey::Rope;
use tower_lsp::lsp_types::{DidChangeTextDocumentParams, PositionEncodingKind, Url};
use tree_sitter_c2rust::{Language, Parser, Query, QueryCursor, StreamingIterator};

use crate::{
    debugging::timeit,
    error::{Error, Result},
    iri::{Iri, TryToIri},
    pos::Position,
    queries::{
        first_leaf, get_last_leaf, possible_nodes,
        treesitter_highlight_capture_into_semantic_token_type_index, OFN_ALL_QUERIES,
        OFN_NODE_TYPES,
    },
    range::{LineRange, Range, RangeBox},
    rope_provider::RopeProvider,
    workspace::{
        changes_from_lsp, trim_full_iri_rope_slice, Annotation, Diagnostic, DiagnosticKind,
        DocumentId, FormattingSettings, FrameInfo, FrameType, Highlights, HoverResult,
        IriAtPosition, IriDefinition, KeywordAction, OntologyDocument, ParsedDocument, RenameInfo,
        Workspace,
    },
};

pub static LANGUAGE_OFN: LazyLock<Language> = LazyLock::new(|| tree_sitter_owl_fn::LANGUAGE.into());

thread_local! {
    static OFN_PARSER: LazyLock<RefCell<Parser>> = LazyLock::new(|| {
        let mut parser = Parser::new();
        parser
            .set_language(&LANGUAGE_OFN)
            .expect("the language to be valid");
        parser.set_logger(Some(Box::new(|type_, str| match type_ {
            tree_sitter_c2rust::LogType::Parse => trace!(target: "ofn tree-sitter-parse", "{str}"),
            tree_sitter_c2rust::LogType::Lex => trace!(target: "ofn tree-sitter-lex", "{str}"),
        })));

        RefCell::new(parser)
    });
}

#[derive(Debug)]
pub struct InternalOfnDocument {
    id: DocumentId,
    parsed_document: ParsedDocument,
    prefixes: HashMap<String, RangeBox<String>>,
    definitions: Vec<RangeBox<IriDefinition>>,
    references: Vec<RangeBox<Iri>>,
    imports: Vec<Iri>,
    syntax_errors: Vec<Diagnostic>,
}

impl InternalOfnDocument {
    pub fn new(uri: Url, version: i32, text: String) -> Self {
        // TODO what should happen with non local files?
        let path = uri.to_file_path().expect("URL should be a file path");
        Self::new_with_path(uri, version, text, path)
    }

    pub fn new_with_path(uri: Url, version: i32, text: String, path: PathBuf) -> Self {
        let id = DocumentId { path, uri, version };

        let tree = timeit("create_document (ofn) / parse", || {
            OFN_PARSER.with(|parser| {
                parser
                    .borrow_mut() // This shoud exit scope after the with
                    .parse(&text, None)
                    .expect("language to be set, no timeout to be used, no cancellation flag")
            })
        });

        let rope = Rope::from(text);
        let parsed_document = ParsedDocument::new(tree, rope);

        let prefixes = gen_prefixes(&parsed_document);
        let definitions = gen_definitions(&parsed_document, &prefixes);
        let imports = gen_imports(&parsed_document);
        let syntax_errors = gen_syntax_errors(&parsed_document);
        let references = gen_references(&parsed_document, &prefixes);

        Self {
            id,
            parsed_document,
            prefixes,
            definitions,
            references,
            imports,
            syntax_errors,
        }
    }

    pub fn edit_inner(
        self, // TODO #30 do a mut instead so the analytics do not get dropped
        params: DidChangeTextDocumentParams,
        encoding: &PositionEncodingKind,
    ) -> Result<Self> {
        let new_version = params.text_document.version;
        if self.version() >= new_version {
            return Ok(self); // no change needed
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

        let InternalOfnDocument {
            id,
            parsed_document,
            prefixes: _,    // regenreated for now
            definitions: _, // regenreated for now
            imports: _,
            syntax_errors: _,
            references: _,
        } = self;

        let changes = changes_from_lsp(params, encoding, parsed_document.rope());

        // Note that these ranges are in the pre edit form
        for change in &changes {
            debug!("Updating changed range (pre edit) {change:?}");
        }

        let (parsed_document, _old_tree) = OFN_PARSER.with(|parser| {
            parsed_document.edit_parsed_document(changes.iter(), &mut parser.borrow_mut())
        })?;

        // Increment ID
        let id = DocumentId {
            version: new_version,
            ..id
        };

        let prefixes = gen_prefixes(&parsed_document);
        let definitions = gen_definitions(&parsed_document, &prefixes);
        let imports = gen_imports(&parsed_document);
        let syntax_errors = gen_syntax_errors(&parsed_document);
        let references = gen_references(&parsed_document, &prefixes);

        let doc = Self {
            id,
            parsed_document,
            prefixes,
            definitions,
            references,
            imports,
            syntax_errors,
        };

        Ok(doc)
    }
}

fn gen_references(
    parsed_document: &ParsedDocument,
    prefixes: &HashMap<String, RangeBox<String>>,
) -> Vec<RangeBox<Iri>> {
    parsed_document
        .query(&OFN_ALL_QUERIES.iri_query_all)
        .into_iter()
        .filter_map(|m| {
            let iri_c = m.captures.first().expect("should be one");
            trim_full_iri_rope_slice(iri_c.node.text)
                .try_to_iri()
                .ok()
                .map(|iri| {
                    let iri = iri.try_to_full(prefixes).unwrap_or(iri);
                    RangeBox::new(iri, iri_c.node.range)
                })
        })
        .collect()
}

fn gen_prefixes(parsed_document: &ParsedDocument) -> HashMap<String, RangeBox<String>> {
    let mut prefixes = HashMap::new();
    for ele in parsed_document.query(&OFN_ALL_QUERIES.prefix) {
        let (prefix_name_c, full_iri_c) = ele
            .captures
            .into_iter()
            .next_tuple()
            .expect("should match query");

        let mut name = prefix_name_c.node.text.to_string();
        if name.ends_with(':') {
            name.pop();
        }

        let iri = trim_full_iri_rope_slice(full_iri_c.node.text).to_string();
        // TODO range is zero!
        prefixes.insert(name, RangeBox::new(iri, Range::ZERO));
    }
    prefixes
}

fn gen_definitions(
    parsed_document: &ParsedDocument,
    prefixes: &HashMap<String, RangeBox<String>>,
) -> Vec<RangeBox<IriDefinition>> {
    parsed_document
        .query(&OFN_ALL_QUERIES.definition)
        .into_iter()
        .filter_map(|m| {
            let iri_c = m.captures.first().expect("should be one");
            trim_full_iri_rope_slice(iri_c.node.text)
                .try_to_iri()
                .ok()
                .map(|iri| {
                    let iri = iri.try_to_full(prefixes).unwrap_or(iri);
                    RangeBox::new(
                        IriDefinition {
                            iri,
                            kind: FrameType::parse(
                                iri_c.node.parent_kind.expect("iri should have parent"),
                            ),
                        },
                        iri_c.node.range,
                    )
                })
        })
        .collect()
}

fn gen_imports(parsed_document: &ParsedDocument) -> Vec<Iri> {
    parsed_document
        .query(&OFN_ALL_QUERIES.import)
        .into_iter()
        .filter_map(|m| {
            let iri_c = m.captures.iter().exactly_one().expect("should be one");
            trim_full_iri_rope_slice(iri_c.node.text).try_to_iri().ok()
        })
        .collect()
}

fn gen_syntax_errors(parsed_document: &ParsedDocument) -> Vec<Diagnostic> {
    syntax_errors(parsed_document)
}

/// Generate the diagnostics for a single node, walking recursively down to every child and every syntax error within
pub fn syntax_errors(parsed_document: &ParsedDocument) -> Vec<Diagnostic> {
    let mut cursor = parsed_document.tree().root_node().walk();
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
                    OFN_NODE_TYPES
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
                // .map(|nt| node_type_to_string(nt)) // TODO!
                .sorted_unstable()
                .join(", ");

            let parent = parent_chain
                .iter()
                // .map(|parent_kind| node_type_to_string(parent_kind)) // TODO!
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

impl PartialEq for InternalOfnDocument {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for InternalOfnDocument {}

impl OntologyDocument for InternalOfnDocument {
    fn path(&self) -> &Path {
        self.id.path.as_path()
    }

    fn uri(&self) -> &Url {
        &self.id.uri
    }

    fn version(&self) -> i32 {
        self.id.version
    }
    fn rope(&self) -> &Rope {
        self.parsed_document.rope()
    }

    fn frame_infos(&self) -> Vec<&FrameInfo> {
        // self.definitions
        //     .iter()
        //     .map(|d| FrameInfo {
        //         iri: d.value().iri.clone(),
        //         annotations: vec![],
        //         frame_type: d.value().kind,
        //         definitions: vec![crate::workspace::Location {
        //             uri: self.uri().clone(),
        //             range: *d.range(),
        //         }],
        //     })
        //     // .tree_reduce(FrameInfo::merge)
        //     .collect()
        vec![]
    }

    fn find_frame_info(&self, iri: &Iri) -> Option<FrameInfo> {
        self.definitions
            .iter()
            .filter(|d| &d.value().iri == iri)
            .map(|d| FrameInfo {
                iri: d.value().iri.clone(),
                annotations: vec![],
                frame_type: d.value().kind,
                definitions: vec![crate::workspace::Location {
                    uri: self.uri().clone(),
                    range: *d.range(),
                }],
            })
            .tree_reduce(FrameInfo::merge)
    }

    fn ontology_iri(&self) -> Option<Iri> {
        None // TODO
    }

    fn version_iri(&self) -> Option<Iri> {
        None // TODO
    }

    fn definitions(&self) -> Vec<&RangeBox<IriDefinition>> {
        self.definitions.iter().collect()
    }

    fn references(&self) -> Vec<&RangeBox<Iri>> {
        self.references.iter().collect()
    }

    fn directly_imports(&self) -> Vec<&Iri> {
        self.imports.iter().collect()
    }

    fn directly_references_doc(&self) -> Vec<&Iri> {
        self.imports.iter().collect()
    }

    fn local_diagnostics(&self) -> &[Diagnostic] {
        &self.syntax_errors
    }

    fn iri_locations(&self) -> HashMap<&Iri, &Vec<RangeBox<()>>> {
        HashMap::new() // TODO
    }

    fn abbreviated_iri_to_full_iri(&self, iri: &Iri) -> Option<Iri> {
        iri.try_to_full(&self.prefixes)
    }

    fn full_iri_to_abbreviated_iri(&self, full_iri: &Iri) -> Option<String> {
        full_iri.try_to_abbriviated(&self.prefixes).map(|s| {
            if s.contains(':') {
                s
            } else {
                format!(":{s}") // We have to prefix!
            }
        })
    }

    fn prefixes(&self) -> HashMap<String, String> {
        HashMap::new()
    }

    fn range(&self) -> Range {
        self.parsed_document.tree().root_node().range().into()
    }

    fn iri_at(&self, pos: Position) -> Option<RangeBox<IriAtPosition>> {
        self.references
            .iter()
            .find(|r| r.range().contains(pos))
            .map(|i| {
                RangeBox::new(
                    IriAtPosition {
                        full_iri: i.value().clone(),
                        is_import: false,   // TODO!
                        is_ontology: false, // TODO!
                        frame_type: self
                            .definitions
                            .iter()
                            .find(|d| &d.value().iri == i.value())
                            .map(|d| d.value().kind),
                    },
                    *i.range(),
                )
            })
    }

    fn rename_range(&self, _pos: Position) -> Option<Range> {
        None // TODO
    }

    fn rename_info_at(&self, _pos: Position, _new_name: &str) -> Result<Option<RenameInfo>> {
        Ok(None) // TODO
    }

    fn rename_edits(&self, rename_info: &RenameInfo) -> Vec<RangeBox<String>> {
        match rename_info {
            RenameInfo::Iri(iri_rename_info) => self
                .references
                .iter()
                .filter_map(|iri_box| {
                    if iri_box.value() == &iri_rename_info.full_iri {
                        let value = iri_rename_info
                            .new_iri
                            .as_ref()
                            .and_then(|i| self.full_iri_to_abbreviated_iri(i))
                            .unwrap_or(iri_rename_info.original.clone());

                        Some(RangeBox::new(value, *iri_box.range()))
                    } else {
                        None
                    }
                })
                .collect(),
            RenameInfo::Prefix(_, _) => vec![], // TODO
        }
    }

    fn keyword_actions_at(&self, _pos: Position) -> Vec<KeywordAction> {
        vec![] // TODO
    }

    fn all_iris_in_range(&self, _range: Range) -> Vec<RangeBox<Iri>> {
        vec![] // TODO
    }

    fn formatted(&self, _options: &FormattingSettings) -> String {
        self.parsed_document.rope().to_string() // TODO
    }

    fn hover(&self, pos: Position) -> Option<HoverResult> {
        // TODO optimize
        // TODO include ontology iri
        self.definitions
            .iter()
            .find(|d| d.range().contains(pos))
            .map(|d| HoverResult::Iri {
                iri: d.value().iri.clone(),
                range: *d.range(),
            })
    }

    fn highlights(&self, range: Range) -> Highlights {
        let query_source = tree_sitter_owl_fn::HIGHLIGHTS_QUERY;
        let query = Query::new(&LANGUAGE_OFN, query_source).expect("valid query expect");
        let mut query_cursor = QueryCursor::new();
        if range != Range::FULL_RANGE {
            query_cursor.set_point_range(range.into());
        }
        let matches = query_cursor.matches(
            &query,
            self.parsed_document.tree().root_node(),
            RopeProvider::new(self.rope()),
        );

        let nodes = matches
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

        nodes
            .iter()
            .map(|(node, type_index)| RangeBox::new(*type_index, node.range().into()))
            .collect()
    }

    fn get_keyword_competions_at(&self, _pos: Position) -> Vec<String> {
        vec![] // TODO
    }

    fn get_iri_completions_at(
        &self,
        _pos: Position,
        _workspace: &Workspace,
    ) -> Vec<(String, String, String, bool)> {
        vec![] // TODO
    }

    fn statistic(&self) -> String {
        "Not implemented".into()
    }

    fn add_annotation_edits(&self, _annotation: Annotation) -> Vec<RangeBox<String>> {
        vec![]
    }

    fn folding_range(&self) -> Vec<LineRange> {
        vec![]
    }
}

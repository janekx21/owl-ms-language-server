use std::collections::HashMap;

use dashmap::DashMap;
use itertools::Itertools;
use log::{debug, error, info};
use ropey::Rope;
use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, Url, WorkspaceFolder};
use tree_sitter::{Node, Parser, Query, QueryCursor, Tree};

use crate::{
    catalog::Catalog, debugging::timeit, range::Range, rope_provider::RopeProvider, FrameType,
    LANGUAGE, NODE_TYPES,
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
        info!("New workspace at {}", workspace_folder.uri);
        Workspace {
            document_map: DashMap::new(),
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
    pub fn new(uri: Url, version: i32, text: String, parser: &mut Parser) -> Document {
        parser.reset();

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
        document.frame_infos = gen_iri_info_map(&document, None);
        document
    }
}

/// This represents informations about a frame.
/// For example the following frame has information.
/// ```
/// Class: PizzaThing
///     Annotations: rdfs:label "Pizza"
/// ```
/// Then the [`FrameInfo`] contains the label "Pizza" and the frame type "Class".
#[derive(Clone, Debug)]
pub struct FrameInfo {
    pub annotations: HashMap<Iri, Vec<String>>,
    pub frame_type: FrameType,
    pub definitions: Vec<(Url, Range)>,
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
            frame_type: if a.frame_type != b.frame_type {
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
        .trim_end_matches("@pl")
        .trim_end_matches("^^xsd:string") // typed literal with type string
        .trim_end_matches('"')
        .replace("\\\"", "\"")
        .trim()
        .to_string()
}

// #[derive(Clone, Debug)]
// pub struct ResolvedIri {
//     pub value: String,
// pub _origin_doucment_range: Range,
// }

type Iri = String;

/// Generate an async hash map that resolves IRIs to infos about that IRI
pub fn gen_iri_info_map(document: &Document, range: Option<&Range>) -> DashMap<Iri, FrameInfo> {
    debug!("generating iri info map");

    let tree = &document.tree;
    let rope = &document.rope;

    // TODO check frame [(datatype_frame) (class_frame) (object_property_frame) (data_property_frame) (annotation_property_frame) (individual_frame)]
    // the typed literal is for the string type
    let frame_source = "
        (_
            . (_ [(full_iri) (simple_iri) (abbreviated_iri)]@frame_iri)
            (annotation
                (annotation_property_iri)@iri
                [
                    (string_literal_no_language)
                    (string_literal_with_language)
                    (typed_literal)
                ]@literal))
        
        (prefix_declaration (prefix_name)@prefix_name (full_iri)@iri)
        ";

    let frame_query = Query::new(*LANGUAGE, frame_source).expect("valid query expect");
    let mut query_cursor = QueryCursor::new();

    if let Some(range) = range {
        query_cursor.set_point_range((*range).into());
    }

    let matches = query_cursor.matches(&frame_query, tree.root_node(), RopeProvider::new(rope));

    let infos = DashMap::<Iri, FrameInfo>::new();

    for m in matches {
        match m.pattern_index {
            0 => {
                let mut capture_texts = m.captures.iter().map(|c| node_text(&c.node, rope));
                let frame_iri = capture_texts.next().unwrap().to_string();
                let annotation_iri = capture_texts.next().unwrap().to_string();
                let literal = capture_texts.next().unwrap().to_string();

                let parent_node = m.captures[0].node.parent().unwrap();
                let frame_type = match parent_node.kind() {
                    "class_iri" => FrameType::Class,
                    "datatype_iri" => FrameType::DataType,
                    "annotation_property_iri" => FrameType::AnnotationProperty,
                    "individual_iri" => FrameType::Individual,
                    "ontology_iri" => FrameType::Ontology,
                    "data_property_iri" => FrameType::DataProperty,
                    "object_property_iri" => FrameType::ObjectProperty,
                    kind => {
                        error!("implement {kind}");
                        FrameType::Class
                    }
                };

                if !infos.contains_key(&frame_iri) {
                    infos.insert(
                        frame_iri.clone(),
                        FrameInfo {
                            annotations: HashMap::new(),
                            frame_type,
                            // TODO the range could be wrong. maybe one parent up
                            definitions: vec![(
                                document.uri.clone(),
                                parent_node.parent().unwrap_or(parent_node).range().into(),
                            )],
                        },
                    );
                }

                let mut info = infos.get_mut(&frame_iri).unwrap();
                // let resolved_iri = ResolvedIri {
                //     value: literal.clone(),
                // _origin_doucment_range: parent_node.range().into(),
                // };

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
                debug!("prefix named {} with iri {}", prefix_name, iri);
            }
            i => todo!("pattern index {} not implemented", i),
        }
    }
    infos
}

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
                        .map(|sn| node_type_to_string(&sn._type)),
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

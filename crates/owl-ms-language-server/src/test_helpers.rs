use crate::{catalog::Catalog, web::HttpClient, workspace, Backend, LANGUAGE};
use itertools::Itertools;
use log::info;
use std::{collections::HashMap, fs, path::Path};
use tempdir::TempDir;
use tower_lsp::{
    lsp_types::{
        ClientCapabilities, GeneralClientCapabilities, InitializeParams, InitializedParams,
        PositionEncodingKind, WorkspaceFolder,
    },
    LanguageServer, LspService,
};
use tree_sitter_c2rust::Parser;

/// The setup is for setting up test fixture stuff. No test data or otherwise test dependent stuff is set up here.
pub fn setup() {
    // Do nothing for now
}

#[derive(Debug, Clone)]
pub enum WorkspaceMember {
    Folder {
        name: String,
        children: Vec<WorkspaceMember>,
    },
    CatalogFile(Catalog),
    OmnFile {
        name: String,
        content: String,
    },
    OwxFile {
        name: String,
        content: String,
    },
}

/// Create a tmp workspace for testing
/// Example:
/// ```
/// vec![
///     WorkspaceMember::CatalogFile(
///         Catalog::new(dir.join("catalog-v001.xml").to_str().unwrap())
///             .with_uri("http://external.org/shared.omn", "foobaronto.omn")
///             .with_uri("http://foobar.org/ontology/", "foo.omn"),
///     ),
///     WorkspaceMember::OmnFile {
///         name: "foobaronto.omn".into(),
///         content: "".into(),
///     },
/// ]
/// ```
pub fn arrange_workspace_folders<F>(root_member_generator: F) -> TempDir
where
    F: Fn(&Path) -> Vec<WorkspaceMember>,
{
    let dir = TempDir::new("owl-ms-test").unwrap();

    let root_members = root_member_generator(dir.path());
    for member in root_members.clone() {
        arrange_workspace_member(member, dir.path());
    }

    info!("Arranged Workspace Member {:#?}", root_members);

    dir
}

fn arrange_workspace_member(member: WorkspaceMember, path: &Path) {
    match member {
        WorkspaceMember::Folder { name, children } => {
            let inner_path = path.join(name);
            fs::create_dir(inner_path.clone()).unwrap();
            for child in children {
                arrange_workspace_member(child, &inner_path);
            }
        }
        WorkspaceMember::CatalogFile(catalog) => {
            let content = quick_xml::se::to_string::<Catalog>(&catalog).unwrap();
            fs::write(path.join("catalog-v001.xml"), content).unwrap();
        }
        WorkspaceMember::OmnFile { name, content } => {
            fs::write(path.join(name), content).unwrap();
        }
        WorkspaceMember::OwxFile { name, content } => {
            fs::write(path.join(name), content).unwrap();
        }
    }
}

pub fn arrange_parser() -> Parser {
    let mut parser = Parser::new();
    parser.set_language(&LANGUAGE).unwrap();
    parser
}

pub async fn arrange_init_backend(
    service: &LspService<Backend>,
    workspacefolder: Option<WorkspaceFolder>,
) {
    let result = service
        .inner()
        .initialize(InitializeParams {
            workspace_folders: workspacefolder.map(|w| vec![w]),
            capabilities: ClientCapabilities {
                general: Some(GeneralClientCapabilities {
                    position_encodings: Some(vec![PositionEncodingKind::UTF8]),
                    ..Default::default()
                }),
                ..Default::default()
            },
            ..Default::default()
        })
        .await;
    assert!(result.is_ok(), "Initialize returned {:#?}", result);

    service.inner().initialized(InitializedParams {}).await;
}

pub async fn arrange_backend(
    workspace_folder: Option<WorkspaceFolder>,
    data: Vec<(&str, &str)>,
) -> LspService<Backend> {
    let http_client = Box::new(StaticClient {
        data: [
            // Defaults for tests
            ("http://www.w3.org/1999/02/22-rdf-syntax-ns#", "dummy"),
            ("http://www.w3.org/2002/07/owl#", "dummy"),
            ("http://www.w3.org/2000/01/rdf-schema#", "dummy"),
        ]
        .into_iter()
        .chain(data.into_iter())
        .map(|(k, v)| (k.to_string(), v.to_string()))
        .collect(),
    });

    let (service, _) = LspService::new(|client| Backend::new(client, http_client));

    arrange_init_backend(&service, workspace_folder).await;
    service
}

#[allow(dead_code)]
pub async fn assert_empty_diagnostics(service: &LspService<Backend>) {
    let sync = service.inner().read_sync().await;
    let workspaces = sync.workspaces();
    for workspace in workspaces.iter() {
        for doc in workspace.internal_documents() {
            // TODO change tests to define all used IRIs
            // Filter out the not defined IRIs for now
            let diagnostics = doc
                .diagnostics(workspace)
                .into_iter()
                // .filter(|d| !d.label.contains("not defined"))
                .collect_vec();

            assert_eq!(diagnostics, vec![], "rope:\n{}", doc.rope());
        }
    }
}

pub async fn service_diagnostics(service: &LspService<Backend>) -> Vec<workspace::Diagnostic> {
    let sync = service.inner().read_sync().await;
    let workspaces = sync.workspaces();
    workspaces
        .iter()
        .flat_map(|workspace| {
            workspace
                .internal_documents()
                .flat_map(|doc| doc.diagnostics(workspace))
        })
        .collect_vec()
}

#[derive(Debug)]
pub struct StaticClient {
    pub data: HashMap<String, String>,
}

impl HttpClient for StaticClient {
    fn get(&self, url: &str) -> crate::web::Result<String> {
        info!("Resolving {url} in static client");
        self.data.get(url).cloned().ok_or(crate::web::Error::Web(
            url.to_string(),
            "Static client did not define that URL and date",
        ))
    }
}

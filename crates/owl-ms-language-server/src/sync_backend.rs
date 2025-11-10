use log::{debug, warn};
use tower_lsp::lsp_types::{Url, WorkspaceFolder};

use crate::{
    error::{Error, Result as MyResult},
    workspace::{InternalDocument, Workspace},
};

#[derive(Default, Debug)]
pub struct SyncBackend {
    workspaces: Vec<Workspace>,
    // TODO hmmmmmmmmmmmm
    // parsers: Parsers,
}

// #[derive(Default)]
// pub struct Parsers {
//     build: horned_owl::model::Build<ArcStr>,
//     omn_parser: Parser,
// }

impl SyncBackend {
    pub fn push_workspace(&mut self, workspace: Workspace) {
        self.workspaces.push(workspace);
    }

    pub fn workspaces(&self) -> &Vec<Workspace> {
        &self.workspaces
    }

    /// This will find a workspace or create one for a given url
    pub fn get_or_insert_workspace_mut(&mut self, url: &Url) -> &mut Workspace {
        debug!("get or insert workspace");
        // TODO there are problems when the workspace is changing
        // - A document could be in its own workspace
        // - Then a catalog file is created or modified that would place a document in that workspace
        // - Because of the early return the document can never move to the new workspace
        let path = url.to_file_path().unwrap(); // TODO
        let maybe_index = self.workspaces.iter().position(|workspace| {
            workspace.contains_internal_document(&path)
                || workspace.catalog_contains_url(url)
                || workspace.workspace_folder_is_base_of_url(url)
        });

        let index = if let Some(idx) = maybe_index {
            idx
        } else {
            let mut file_path = url.to_file_path().expect("URL should be a filepath");
            file_path.pop();

            warn!("Workspace for {url} could not be found. Could the entry in catalog-v001.xml be missing? Creating a new one at {}", file_path.display());
            let workspace_folder = WorkspaceFolder {
                // The workspace folder IS the single file. This is not ideal but should work for now.
                uri: Url::from_file_path(file_path).expect("Valid URL from filepath"),
                name: "Single File".into(),
            };
            let workspace = Workspace::new(workspace_folder.clone());

            self.workspaces.push(workspace);
            self.workspaces.len() - 1
        };

        &mut self.workspaces[index]
    }

    /// This will find a workspace or create one for a given url
    pub fn get_workspace(&self, url: &Url) -> Option<&Workspace> {
        debug!("get workspace");
        let path = url.to_file_path().unwrap(); // TODO
                                                // TODO there are problems when the workspace is changing
                                                // - A document could be in its own workspace
                                                // - Then a catalog file is created or modified that would place a document in that workspace
                                                // - Because of the early return the document can never move to the new workspace
        self.workspaces.iter().find(|workspace| {
            workspace.contains_internal_document(&path)
                || workspace.catalog_contains_url(url)
                || workspace.workspace_folder_is_base_of_url(url)
        })
    }

    /// Convinience function to fetch internal document
    pub fn get_internal_document(&self, url: &Url) -> MyResult<(&InternalDocument, &Workspace)> {
        let workspace = self
            .get_workspace(url)
            .ok_or(Error::DocumentNotFound(url.clone()))?;
        let path = url.to_file_path().unwrap(); // TODO
        Ok((workspace.get_internal_document(&path)?, workspace))
    }
    /// Convinience function to fetch internal document
    pub fn take_internal_document(
        &mut self,
        url: &Url,
    ) -> MyResult<(InternalDocument, &mut Workspace)> {
        let workspace = self.get_or_insert_workspace_mut(url);
        let path = url.to_file_path().unwrap(); // TODO
        Ok((workspace.take_internal_document(&path)?, workspace))
    }
}

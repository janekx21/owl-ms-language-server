use std::{
    fs::read_to_string,
    path::{Path, PathBuf},
};

use itertools::Itertools;
use log::info;
use quick_xml::de::from_str;
use serde::{Deserialize, Serialize};
use tower_lsp::lsp_types::Url;
use walkdir::WalkDir;

#[derive(Deserialize, Serialize, Debug, Clone)]
#[serde(rename_all = "kebab-case")]
pub struct Catalog {
    #[serde(default)]
    uri: Vec<CatalogUri>,
    #[serde(default)]
    group: Vec<CatalogGroup>,

    /// Warning! This is the path to the catalog file NOT its parent folder.
    #[serde(skip)]
    locaton: String,
}

impl Catalog {
    #[cfg(test)]
    pub fn new(location: &str) -> Catalog {
        Catalog {
            uri: vec![],
            group: vec![],
            locaton: location.into(),
        }
    }

    #[cfg(test)]
    pub fn with_uri(self, name: &str, uri: &str) -> Catalog {
        let mut c = self.clone();
        c.uri.push(CatalogUri {
            _id: "Constructed in a builder".into(),
            name: name.into(),
            uri: uri.into(),
        });
        c
    }
}

#[derive(Deserialize, Serialize, Debug, Clone)]
#[serde(rename_all = "kebab-case")]
pub struct CatalogUri {
    /// Non unique name of item
    #[serde(rename = "@id")]
    pub _id: String,

    /// Full URL of the ontology. This will be used in OMN import statements
    #[serde(rename = "@name")]
    pub name: String,

    /// Relative file path of the backing ontology file
    #[serde(rename = "@uri")]
    pub uri: String,
}

#[derive(Deserialize, Serialize, Debug, Clone)]
#[serde(rename_all = "kebab-case")]
pub struct CatalogGroup {
    pub uri: Vec<CatalogUri>,
}

impl Catalog {
    pub fn load_catalogs_recursive(uri: Url) -> Vec<Catalog> {
        let path = uri
            .to_file_path()
            .unwrap_or_else(|_| panic!("Provided url should be a file path but was {}", uri));
        WalkDir::new(path)
            .into_iter()
            .filter_map(|e| e.ok())
            .filter(|entry| entry.file_type().is_file())
            .filter(|entry| entry.file_name().to_str() == "catalog-v001.xml".into())
            .filter_map(|entry| read_to_string(entry.path()).ok().map(|s| (s, entry)))
            .filter_map(|(xml, entry)| from_str(xml.as_str()).ok().map(|c: Catalog| (c, entry)))
            .map(|(mut catalog, entry)| {
                catalog.locaton = entry.path().to_str().unwrap().to_string();
                info!("Found catalog {:?}", catalog);
                catalog
            })
            .collect_vec()
    }

    pub fn all_catalog_uris(&self) -> impl Iterator<Item = &CatalogUri> {
        self.uri
            .iter()
            .chain(self.group.iter().flat_map(|g| &g.uri))
    }

    /// Takes a path to a document and determins if the item is inside this catalog
    pub fn contains(&self, path: &PathBuf) -> bool {
        for catalog_uri in self.all_catalog_uris() {
            let catalog_item_path = self.parent_folder().join(&catalog_uri.uri);
            if &catalog_item_path == path {
                return true;
            }
        }
        false
    }

    pub fn parent_folder(&self) -> &Path {
        Path::new(self.locaton.as_str()).parent().unwrap()
    }
}

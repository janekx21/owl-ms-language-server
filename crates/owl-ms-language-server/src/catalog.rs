use std::{fs::read_to_string, path::Path};

use itertools::Itertools;
use log::info;
use quick_xml::de::from_str;
use serde::{Deserialize, Serialize};
use tower_lsp::lsp_types::Url;
use walkdir::WalkDir;

#[derive(Deserialize, Serialize, Debug, Clone)]
#[serde(rename_all = "kebab-case")]
pub struct Catalog {
    pub uri: Vec<CatalogUri>,

    #[serde(skip)]
    /// Warning! This is the path to the catalog file NOT its parent folder.
    pub locaton: String,
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

    pub fn parent_folder(&self) -> &Path {
        Path::new(self.locaton.as_str()).parent().unwrap()
    }
}

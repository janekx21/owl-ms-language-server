use std::fs::read_to_string;

use log::debug;
use quick_xml::de::from_str;
use serde::Deserialize;
use tower_lsp::lsp_types::Url;
use walkdir::WalkDir;

#[derive(Deserialize, Debug, Clone)]
#[serde(rename_all = "kebab-case")]
pub struct Catalog {
    pub uri: Vec<CatalogUri>,

    #[serde(skip)]
    pub locaton: String,
}

#[derive(Deserialize, Debug, Clone)]
#[serde(rename_all = "kebab-case")]
pub struct CatalogUri {
    #[serde(rename = "@id")]
    pub _id: String, // Non unique name of item
    #[serde(rename = "@name")]
    pub name: String, // Full URL of the ontology. This will be used in OMN import statements
    #[serde(rename = "@uri")]
    pub uri: String, // Relative file path of the backing ontology file
}

impl Catalog {
    pub fn load_catalogs(uri: Url) -> Vec<Catalog> {
        let path = uri
            .to_file_path()
            .unwrap_or_else(|_| panic!("Provided url should be a file path but was {}", uri));
        let walker = WalkDir::new(path);
        let mut catalogs: Vec<Catalog> = vec![];
        for entry in walker.into_iter().filter_map(|e| e.ok()) {
            if entry.file_type().is_file()
                && entry.file_name().to_str() == "catalog-v001.xml".into()
            {
                // Possible catalog file. Lets parse it

                let path = entry.path().iter().as_path();
                let xml = read_to_string(path).expect("Catalog file should be readable");
                let mut catalog: Catalog = from_str(xml.as_str()).unwrap();
                catalog.locaton = path.to_str().unwrap().to_string();
                debug!("Found catalog {:?}", catalog);
                catalogs.push(catalog);
            };
        }
        catalogs
    }
}

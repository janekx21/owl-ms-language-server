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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn catalog_deserialize_should_work() {
        let xml = r#"
            <?xml version="1.0" encoding="UTF-8" standalone="no"?>
            <catalog prefer="public" xmlns="urn:oasis:names:tc:entity:xmlns:xml:catalog">
                <uri id="Imports Wizard Entry" name="http://openenergy-platform.org/ontology/oeo/dev/oeo-import-edits.owl" uri="oeo-import-edits.owl"/>
                <uri id="Imports Wizard Entry" name="http://purl.obolibrary.org/obo/bfo/2.0/bfo.owl" uri="https://raw.githubusercontent.com/BFO-ontology/BFO/v2.0/bfo.owl"/>
                <uri id="Imports Wizard Entry" name="http://openenergy-platform.org/ontology/oeo/dev/oeo-model.omn" uri="oeo-model.omn"/>
                <uri id="Imports Wizard Entry" name="http://openenergy-platform.org/ontology/oeo/dev/oeo-physical.omn" uri="oeo-physical.omn"/>
                <uri id="Imports Wizard Entry" name="http://openenergy-platform.org/ontology/oeo/dev/oeo-sector.omn" uri="oeo-sector.omn"/>
                <uri id="Imports Wizard Entry" name="http://openenergy-platform.org/ontology/oeo/dev/oeo-social.omn" uri="oeo-social.omn"/>
                <uri id="Imports Wizard Entry" name="http://openenergy-platform.org/ontology/oeo/dev/oeo-shared.omn" uri="oeo-shared.omn"/>
                <uri id="Imports Wizard Entry" name="http://openenergy-platform.org/ontology/oeo/dev/imports/iao-extracted.owl" uri="../imports/iao-extracted.owl"/>
                <uri id="Imports Wizard Entry" name="http://openenergy-platform.org/ontology/oeo/dev/imports/ro-extracted.owl" uri="../imports/ro-extracted.owl"/>
                <uri id="Imports Wizard Entry" name="http://openenergy-platform.org/ontology/oeo/dev/imports/uo-extracted.owl" uri="../imports/uo-extracted.owl"/>
                <uri id="Imports Wizard Entry" name="http://openenergy-platform.org/ontology/oeo/dev/imports/stato-extracted.owl" uri="../imports/stato-extracted.owl"/>
                <uri id="Imports Wizard Entry" name="http://openenergy-platform.org/ontology/oeo/dev/imports/cco-extracted.owl" uri="../imports/cco-extracted.owl"/>                                      
                <uri id="Imports Wizard Entry" name="http://openenergy-platform.org/ontology/oeo/dev/imports/meno-extracted.owl" uri="../imports/meno-extracted.owl"/>
                <group id="Folder Repository, directory=, recursive=true, Auto-Update=true, version=2" prefer="public" xml:base="">
                    <uri id="Automatically generated entry, Timestamp=1740265509439" name="http://openenergy-platform.org/ontology/oeo/oeo-import-edits.owl" uri="oeo-import-edits.owl"/>
                    <uri id="Automatically generated entry, Timestamp=1740265509439" name="http://openenergy-platform.org/ontology/oeo/oeo-physical-axioms/" uri="oeo-physical-axioms.owl"/>
                </group>
            </catalog>
        "#;

        let catalog: Catalog = from_str(xml).unwrap();

        assert_eq!(catalog.all_catalog_uris().count(), 15);
        assert_eq!(
            catalog.all_catalog_uris().next().unwrap().uri,
            "oeo-import-edits.owl".to_string()
        );
        assert_eq!(
            catalog.all_catalog_uris().last().unwrap().uri,
            "oeo-physical-axioms.owl".to_string()
        );
    }

    #[test]
    fn catalog_deserialize_without_group_should_work() {
        // Arrange
        let xml = r#"
            <?xml version="1.0" encoding="UTF-8" standalone="no"?>
            <catalog prefer="public" xmlns="urn:oasis:names:tc:entity:xmlns:xml:catalog">
                <uri id="Imports Wizard Entry" name="http://openenergy-platform.org/ontology/oeo/dev/oeo-import-edits.owl" uri="oeo-import-edits.owl"/>
                <uri id="Imports Wizard Entry" name="http://purl.obolibrary.org/obo/bfo/2.0/bfo.owl" uri="https://raw.githubusercontent.com/BFO-ontology/BFO/v2.0/bfo.owl"/>
                <uri id="Imports Wizard Entry" name="http://openenergy-platform.org/ontology/oeo/dev/oeo-model.omn" uri="oeo-model.omn"/>
                <uri id="Imports Wizard Entry" name="http://openenergy-platform.org/ontology/oeo/dev/oeo-physical.omn" uri="oeo-physical.omn"/>
                <uri id="Imports Wizard Entry" name="http://openenergy-platform.org/ontology/oeo/dev/oeo-sector.omn" uri="oeo-sector.omn"/>
                <uri id="Imports Wizard Entry" name="http://openenergy-platform.org/ontology/oeo/dev/oeo-social.omn" uri="oeo-social.omn"/>
                <uri id="Imports Wizard Entry" name="http://openenergy-platform.org/ontology/oeo/dev/oeo-shared.omn" uri="oeo-shared.omn"/>
                <uri id="Imports Wizard Entry" name="http://openenergy-platform.org/ontology/oeo/dev/imports/iao-extracted.owl" uri="../imports/iao-extracted.owl"/>
                <uri id="Imports Wizard Entry" name="http://openenergy-platform.org/ontology/oeo/dev/imports/ro-extracted.owl" uri="../imports/ro-extracted.owl"/>
                <uri id="Imports Wizard Entry" name="http://openenergy-platform.org/ontology/oeo/dev/imports/uo-extracted.owl" uri="../imports/uo-extracted.owl"/>
                <uri id="Imports Wizard Entry" name="http://openenergy-platform.org/ontology/oeo/dev/imports/stato-extracted.owl" uri="../imports/stato-extracted.owl"/>
                <uri id="Imports Wizard Entry" name="http://openenergy-platform.org/ontology/oeo/dev/imports/cco-extracted.owl" uri="../imports/cco-extracted.owl"/>                                      
                <uri id="Imports Wizard Entry" name="http://openenergy-platform.org/ontology/oeo/dev/imports/meno-extracted.owl" uri="../imports/meno-extracted.owl"/>
            </catalog>
        "#;

        // Act
        let catalog: Catalog = from_str(xml).unwrap();

        // Assert
        assert_eq!(catalog.all_catalog_uris().count(), 13);
    }

    #[test]
    fn catalog_serialize_with_empty_catalog_should_work() {
        // Arrange
        let value = Catalog::new("testing");

        // Act
        let xml = quick_xml::se::to_string(&value).unwrap();

        //Assert
        assert_eq!(xml, "<Catalog/>");
    }
}

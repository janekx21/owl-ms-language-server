use log::{debug, error};
use ureq::http::StatusCode;

use crate::error::Result;

/// Trait for simple http get requests. It can be mocked with the static client.
pub trait HttpClient: Send + Sync {
    fn get(&self, url: &str) -> Result<String>;
}

pub struct UreqClient;

impl HttpClient for UreqClient {
    fn get(&self, url: &str) -> Result<String> {
        let mut response = ureq::get(url)
            .header("Accept", "application/rdf+xml")
            .call()?;
        if response.status() == StatusCode::NOT_ACCEPTABLE {
            error!("not acceptable {url}");
            return Err(crate::error::Error::DocumentNotSupported("rdf".to_string()));
        }
        debug!("ontology request {url} got {:#?}", response.headers());
        Ok(response.body_mut().read_to_string()?)
    }
}

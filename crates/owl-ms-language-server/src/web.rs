use crate::error::Result;
use dashmap::DashMap;
use log::info;

/// Trait for simple http get requests. It can be mocked with the static client.
pub trait HttpClient: Send + Sync {
    fn get(&self, url: &str) -> Result<String>;
}

pub struct UreqClient;

impl HttpClient for UreqClient {
    fn get(&self, url: &str) -> Result<String> {
        let mut response = ureq::get(url).call()?;
        Ok(response.body_mut().read_to_string()?)
    }
}

pub struct StaticClient {
    pub data: DashMap<String, String>,
}

impl HttpClient for StaticClient {
    fn get(&self, url: &str) -> Result<String> {
        info!("Resolving {url} in static client");
        Ok(self
            .data
            .get(url)
            .unwrap_or_else(|| panic!("the url {url} should be defined"))
            .to_string())
    }
}

use crate::error::Result;

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

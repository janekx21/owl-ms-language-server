use itertools::Itertools;
use log::{debug, error};
use std::{collections::HashMap, path::PathBuf, sync::Mutex, time::Duration};
use thiserror::Error;
use ureq::{http::StatusCode, Agent};

/// Trait for simple http get requests. It can be mocked with the static client.
pub trait HttpClient: Send + Sync + std::fmt::Debug {
    /// # Errors
    ///
    /// This function will return an error if the get request is not successfull.
    fn get(&self, url: &str) -> Result<String>;
}

#[derive(Error, Debug)]
pub enum Error {
    #[error("Ureq Error: {0}")]
    Ureq(#[from] ureq::Error),
    #[error("The request to {0} could not be fulfilled because: {1}")]
    Web(String, &'static str), // Url and reason
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub struct UreqClient {
    agent: Agent,
    state: Mutex<ClientState>,
}

#[derive(Debug, Default)]
struct ClientState {
    retry_count: HashMap<String, usize>,
    cache: HashMap<String, String>,
}

impl Default for UreqClient {
    fn default() -> Self {
        Self {
            agent: Agent::config_builder()
                .timeout_global(Some(Duration::from_secs(5)))
                .build()
                .into(),
            state: Mutex::default(),
        }
    }
}

impl HttpClient for UreqClient {
    fn get(&self, url: &str) -> Result<String> {
        // Some common ontologies that are statictly included in the binary
        match url {
            "http://www.w3.org/2000/01/rdf-schema#" => {
                return Ok(include_str!("../static/rdfs.owl").to_string())
            }
            "http://www.w3.org/2002/07/owl#" => {
                return Ok(include_str!("../static/owl.owl").to_string())
            }
            "http://www.w3.org/1999/02/22-rdf-syntax-ns#" => {
                return Ok(include_str!("../static/rdf.owl").to_string())
            }
            "http://purl.org/dc/elements/1.1/" => {
                return Ok(include_str!("../static/dc.owl").to_string())
            }
            _ => {}
        }

        let mut state = self.state.lock().expect("Client should not panic");

        if let Some(value) = state.cache.get(url) {
            debug!("Found value in cache");
            return Ok(value.clone());
        }
        if let Some(count) = state.retry_count.get_mut(url) {
            // TODO more sensible numbers :>
            if count >= &mut 1 {
                return Err(Error::Web(url.into(), "too many requests"));
            }
            *count += 1;
        } else {
            state.retry_count.insert(url.into(), 1);
        }

        let mut response = self
            .agent
            .get(url)
            .header("Accept", "application/rdf+xml")
            .call()?;

        if response.status() == StatusCode::NOT_ACCEPTABLE {
            error!("not acceptable {url}");
            return Err(Error::Web(url.to_string(), "Not acceptable"));
        }

        // "content-type" can be something like "text/html; charset=UTF-8". So lets look for substrings.
        if response
            .headers()
            .get("Content-Type")
            .and_then(|v| v.to_str().ok())
            .is_some_and(|v| v.split(';').map(str::trim).contains("text/html"))
        {
            return Err(Error::Web(
                url.to_string(),
                "Content type not supported (got html)",
            ));
        }

        let read_to_string = response.body_mut().read_to_string()?;
        state.cache.insert(url.into(), read_to_string.clone());
        Ok(read_to_string)
    }
}

/// This client is simply offline
#[derive(Debug)]
pub struct OfflineClient;

impl HttpClient for OfflineClient {
    fn get(&self, url: &str) -> Result<String> {
        Err(Error::Web(url.to_string(), "You are offline"))
    }
}

/// This is a simple URL to Path escape. Changing this will break the web cache.
#[must_use]
pub fn url_to_filename(url: &str) -> PathBuf {
    url.replace('/', "_slash_")
        .replace(':', "_colon_")
        .replace('#', "_hash_")
        .chars()
        .map(|c| if c.is_alphanumeric() { c } else { '_' })
        .chain(".cache".chars())
        .collect::<String>()
        .into()
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    #[test]
    fn test_simple_url() {
        let result = url_to_filename("https://example.com");
        assert_eq!(
            result,
            PathBuf::from("https_colon__slash__slash_example_com.cache")
        );
    }

    #[test]
    fn test_url_with_path() {
        let result = url_to_filename("https://example.com/path/to/page");
        assert_eq!(
            result,
            PathBuf::from(
                "https_colon__slash__slash_example_com_slash_path_slash_to_slash_page.cache"
            )
        );
    }

    #[test]
    fn test_url_with_port() {
        let result = url_to_filename("http://localhost:8080/api");
        assert_eq!(
            result,
            PathBuf::from("http_colon__slash__slash_localhost_colon_8080_slash_api.cache")
        );
    }

    #[test]
    fn test_url_with_query_params() {
        let result = url_to_filename("https://api.example.com?key=value&id=123");
        assert_eq!(
            result,
            PathBuf::from("https_colon__slash__slash_api_example_com_key_value_id_123.cache")
        );
    }

    #[test]
    fn test_url_with_hash() {
        let result = url_to_filename("https://example.com/page#section");
        assert_eq!(
            result,
            PathBuf::from("https_colon__slash__slash_example_com_slash_page_hash_section.cache")
        );
    }

    #[test]
    fn test_url_with_special_chars() {
        let result = url_to_filename("https://example.com/path@file!name");
        assert_eq!(
            result,
            PathBuf::from("https_colon__slash__slash_example_com_slash_path_file_name.cache")
        );
    }

    #[test]
    fn test_url_with_multiple_special_chars() {
        let result = url_to_filename("https://example.com/path?q=test&sort=asc#top");
        assert_eq!(
            result,
            PathBuf::from(
                "https_colon__slash__slash_example_com_slash_path_q_test_sort_asc_hash_top.cache"
            )
        );
    }

    #[test]
    fn test_ftp_url() {
        let result = url_to_filename("ftp://files.example.org:21/download");
        assert_eq!(
            result,
            PathBuf::from(
                "ftp_colon__slash__slash_files_example_org_colon_21_slash_download.cache"
            )
        );
    }

    #[test]
    fn test_empty_string() {
        let result = url_to_filename("");
        assert_eq!(result, PathBuf::from(".cache"));
    }

    #[test]
    fn test_url_with_dashes_and_underscores() {
        let result = url_to_filename("https://my-site.com/my_page");
        assert_eq!(
            result,
            PathBuf::from("https_colon__slash__slash_my_site_com_slash_my_page.cache")
        );
    }

    #[test]
    fn test_cache_extension_always_added() {
        let result = url_to_filename("simple");
        assert!(result.to_string_lossy().ends_with(".cache"));
    }

    #[test]
    fn test_url_with_encoded_chars() {
        let result = url_to_filename("https://example.com/path%20with%20spaces");
        assert_eq!(
            result,
            PathBuf::from("https_colon__slash__slash_example_com_slash_path_20with_20spaces.cache")
        );
    }
}

use dashmap::DashMap;
use itertools::Itertools;
use log::{debug, error};
use parking_lot::{Mutex, RwLock};
use std::{
    fs,
    ops::Deref,
    path::Path,
    sync::{Arc, LazyLock},
    time::Duration,
};
use tokio::sync::OnceCell;
use ureq::{http::StatusCode, Agent};

use crate::error::{Error, Result};

/// Trait for simple http get requests. It can be mocked with the static client.
pub trait HttpClient: Send + Sync + std::fmt::Debug {
    fn get(&self, url: &str) -> Result<String>;
}

#[derive(Debug)]
pub struct UreqClient;

pub static RETRY_COUNT: LazyLock<DashMap<String, usize>> = LazyLock::new(DashMap::new);

pub static CACHE: LazyLock<DashMap<String, String>> = LazyLock::new(DashMap::new);

pub static AGENT: LazyLock<Agent> = LazyLock::new(|| {
    let config = Agent::config_builder()
        .timeout_global(Some(Duration::from_secs(5)))
        .build();

    config.into()
});

pub static MUTEX: Mutex<()> = Mutex::new(());

pub static HTTP_CLIENT: RwLock<Option<Arc<dyn HttpClient>>> = RwLock::new(None);

/// .
///
/// # Panics
///
/// Panics if http client is not initilized.
pub fn global_http_client() -> Arc<dyn HttpClient> {
    HTTP_CLIENT
        .read_recursive()
        .as_ref()
        .expect("global http client should be initialized")
        .clone()
}

/// .
///
/// # Panics
///
/// Panics if the global http client is intitialised.
pub fn set_global_http_client(http_client: Box<dyn HttpClient>) {
    let mut hc = HTTP_CLIENT.write();
    *hc = Some(http_client.into());
}

impl HttpClient for UreqClient {
    fn get(&self, url: &str) -> Result<String> {
        let _lock = MUTEX.lock();

        if let Some(value) = CACHE.get(url) {
            debug!("Found value in cache");
            return Ok(value.value().clone());
        }

        if let Some(mut count) = RETRY_COUNT.get_mut(url) {
            if count.value() > &1 {
                return Err(Error::Web(url.into(), "too many requests"));
            }
            let c = count.value_mut();
            *c += 1;
        } else {
            RETRY_COUNT.insert(url.into(), 1);
        }

        let mut response = AGENT
            .get(url)
            .header("Accept", "application/rdf+xml")
            .call()?;

        if response.status() == StatusCode::NOT_ACCEPTABLE {
            error!("not acceptable {url}");
            return Err(Error::Web(url.to_string(), "Not acceptable"));
        }
        // "content-type": "text/html; charset=UTF-8
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

        debug!("ontology request {url} got {:#?}", response.headers());
        let read_to_string = response.body_mut().read_to_string()?;
        fs::write(
            Path::new("/tmp/owl-ms-web-cache").join(
                url.replace('/', " (slash) ")
                    .replace(':', " (colon) ")
                    .replace('#', " (hash) "),
            ),
            read_to_string.clone(),
        )
        .unwrap();
        CACHE.insert(url.into(), read_to_string.clone());
        Ok(read_to_string)
    }
}

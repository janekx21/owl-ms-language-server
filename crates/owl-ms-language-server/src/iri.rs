use std::{
    collections::BTreeSet,
    fmt::Display,
    ops::Deref,
    sync::{Arc, LazyLock},
};

use tokio::sync::RwLock;

#[derive(Debug, Default)]
pub struct ReusedIriSet(RwLock<BTreeSet<Iri>>);

impl ReusedIriSet {
    fn new() -> Self {
        Self(RwLock::new(BTreeSet::new()))
    }

    pub fn iri(&self, a: &str) -> Iri {
        let iri = Iri(Arc::from(a));
        {
            let cache = self.0.blocking_read();
            cache.get(&iri).map(|x| x.clone())
        }
        .unwrap_or_else(|| {
            let mut cache = self.0.blocking_write();
            cache.insert(iri.clone());
            iri
        })
    }
}

static REUSE_IRI: LazyLock<ReusedIriSet> = LazyLock::new(|| ReusedIriSet::new());

pub fn create_iri(iri_str: &str) -> Iri {
    REUSE_IRI.iri(iri_str)
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
pub struct Iri(Arc<str>);

impl Display for Iri {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Iri {
    pub fn as_str(&self) -> &str {
        &self.0
    }

    pub fn ref_count(&self) -> usize {
        Arc::strong_count(&self.0)
    }
}

pub trait ToIri {
    fn to_iri(&self) -> Iri;
}

impl ToIri for &str {
    fn to_iri(&self) -> Iri {
        create_iri(&self.to_string())
    }
}

impl ToIri for String {
    fn to_iri(&self) -> Iri {
        create_iri(&self)
    }
}

impl Into<Iri> for String {
    fn into(self) -> Iri {
        create_iri(&self)
    }
}

impl Deref for Iri {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.as_str()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_log::test;

    #[test]
    fn reused_iri_set_create_iri_should_work() {
        let iri = "http://example.com/onto#132".to_iri();
        let _iri2 = "http://example.com/onto#132".to_iri();
        let _iri3 = "http://example.com/onto#132".to_iri();

        assert!(iri.ref_count() == 3);
    }
}

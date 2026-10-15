use itertools::Itertools;
use lasso::{Spur, ThreadedRodeo};
use ropey::RopeSlice;
use std::{borrow::Borrow, collections::HashMap, fmt::Display, ops::Deref, sync::LazyLock};

use crate::range::RangeBox;

#[derive(Debug, Default)]
pub struct ReusedIriSet(ThreadedRodeo<Spur>);

impl ReusedIriSet {
    fn new() -> Self {
        Self(ThreadedRodeo::new())
    }

    pub fn iri(&self, s: &str) -> Iri {
        let key = self.0.get_or_intern(s);
        Iri(key)
    }

    pub fn str(&self, i: &Iri) -> &str {
        self.0.resolve(&i.0)
    }

    pub fn used(&self) -> usize {
        self.0.len()
    }

    // TODO do this for the url as well
}

pub static REUSE_IRI: LazyLock<ReusedIriSet> = LazyLock::new(ReusedIriSet::new);

// TODO
// Iri could maybe be split into absolute(full iri) and relative(simple and abbriviated iri) versions

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
pub struct Iri(Spur);

impl Display for Iri {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl Iri {
    pub fn as_str(&self) -> &str {
        REUSE_IRI.str(self)
    }

    pub fn try_to_full(&self, prefixes: &HashMap<String, RangeBox<String>>) -> Option<Self> {
        if let Some((prefix, simple_iri)) = self.as_str().split_once(':') {
            prefixes.get(prefix).map(|resolved_prefix| {
                [resolved_prefix.value().as_str(), simple_iri]
                    .join("")
                    .to_iri()
            })
        } else {
            // Simple IRIs get a free colon prepended
            // ref: https://www.w3.org/TR/owl2-manchester-syntax/#IRIs.2C_Integers.2C_Literals.2C_and_Entities
            prefixes.get("").map(|resolved_prefix| {
                [resolved_prefix.value().as_str(), self.as_str()]
                    .join("")
                    .to_iri()
            })
        }
    }

    pub fn try_to_abbriviated(
        &self,
        prefixes: &HashMap<String, RangeBox<String>>,
    ) -> Option<String> {
        prefixes
            .iter()
            .map(|(k, v)| (k.clone(), v.value().clone()))
            .filter_map(
                |(prefix, url)| match self.as_str().split_once(url.as_str()) {
                    Some(("", post)) if prefix.is_empty() => Some(post.to_string()),
                    Some(("", post)) => Some(prefix + ":" + post),
                    Some(_) | None => None,
                },
            )
            .sorted_by_key(String::len)
            .next()
    }
}

pub trait ToIri {
    fn to_iri(&self) -> Iri;
}

impl ToIri for &str {
    fn to_iri(&self) -> Iri {
        REUSE_IRI.iri(self)
    }
}

impl ToIri for String {
    fn to_iri(&self) -> Iri {
        REUSE_IRI.iri(self)
    }
}

impl ToIri for RopeSlice<'_> {
    fn to_iri(&self) -> Iri {
        if let Some(str) = self.as_str() {
            // Does not allocate a String
            REUSE_IRI.iri(str)
        } else {
            REUSE_IRI.iri(&self.to_string())
        }
    }
}

impl From<String> for Iri {
    fn from(val: String) -> Self {
        REUSE_IRI.iri(&val)
    }
}

impl From<&str> for Iri {
    fn from(val: &str) -> Self {
        REUSE_IRI.iri(val)
    }
}

impl Deref for Iri {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.as_str()
    }
}
impl Borrow<str> for Iri {
    fn borrow(&self) -> &str {
        REUSE_IRI.str(self)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_log::test;

    #[test]
    fn reused_iri_set_create_iri_should_work() {
        let _iri = "http://example.com/onto#132".to_iri();
        let _iri2 = "http://example.com/onto#132".to_iri();
        let _iri3 = "http://example.com/onto#132".to_iri();

        // This is 4, because the resuse set holds one of the strong reference counts
        assert_eq!(REUSE_IRI.0.len(), 1);
    }
}

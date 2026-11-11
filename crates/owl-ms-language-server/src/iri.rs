use core::fmt;
use itertools::Itertools;
use lasso::{Spur, ThreadedRodeo};
use log::{debug, warn};
use ropey::RopeSlice;
use sophia::{
    api::MownStr,
    iri::{
        resolve::{IriParseError, OxiriRef},
        IriRef,
    },
};
use std::{
    borrow::Borrow, collections::HashMap, fmt::Display, ops::Deref, str::FromStr, sync::LazyLock,
};
use tower_lsp::lsp_types::Url;

use crate::range::RangeBox;

#[derive(Debug, Default)]
pub struct ReusedIriSet(ThreadedRodeo<Spur>);

impl ReusedIriSet {
    fn new() -> Self {
        Self(ThreadedRodeo::new())
    }

    fn iri(&self, s: &str) -> Iri {
        let key = self.0.get_or_intern(s);
        Iri(key)
    }

    fn str(&self, i: &Iri) -> &str {
        self.0.resolve(&i.0)
    }

    pub fn used(&self) -> usize {
        self.0.len()
    }
}

pub static REUSE_IRI: LazyLock<ReusedIriSet> = LazyLock::new(ReusedIriSet::new);

// TODO
// Iri could maybe be split into absolute(full iri) and relative(simple and abbriviated iri) versions

/// Absolute or relative IRI. Validated on creation. Space optimized with reuse set.
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
pub struct Iri(Spur);

impl Display for Iri {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl fmt::Debug for Iri {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Iri({self})")
    }
}

impl Iri {
    pub fn as_str(&self) -> &str {
        REUSE_IRI.str(self)
    }

    pub fn try_to_full(&self, prefixes: &HashMap<String, RangeBox<String>>) -> Option<Self> {
        if let Some((prefix, simple_iri)) = self.as_str().split_once(':') {
            prefixes.get(prefix).and_then(|resolved_prefix| {
                [resolved_prefix.value().as_str(), simple_iri]
                    .join("")
                    .parse()
                    .ok()
            })
        } else {
            // Simple IRIs get a free colon prepended
            // ref: https://www.w3.org/TR/owl2-manchester-syntax/#IRIs.2C_Integers.2C_Literals.2C_and_Entities
            prefixes.get("").and_then(|resolved_prefix| {
                [resolved_prefix.value().as_str(), self.as_str()]
                    .join("")
                    .parse()
                    .ok()
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

    /// This normalizes the IRI into a URL
    pub fn to_url(&self) -> Option<Url> {
        debug!("IRI to URL {}", self.as_str());
        Url::parse(self.as_str())
            .inspect_err(|e| warn!("Url Parse Error: {e}"))
            .ok()
    }
}

impl FromStr for Iri {
    type Err = IriParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // Remove empty fragments
        let s = match s.rsplit_once('#') {
            Some((iri, "")) => iri,
            None | Some(_) => s,
        };

        match s.split_once("://") {
            // This is an abbriviated IRI. Lets not parse it.
            // TODO!!!! this lets absolute iris trhou
            None => Ok(REUSE_IRI.iri(s)),

            // This is a full IRI. Lets parse it.
            Some(_) => match OxiriRef::parse(s) {
                Ok(_) => Ok(REUSE_IRI.iri(s)),
                Err(e) => Err(e),
            },
        }
    }
}

pub trait ToIriUnchecked {
    /// This does panic if the IRI is invalid!
    fn to_iri_unchecked(&self) -> Iri;
}

impl ToIriUnchecked for &str {
    #[cfg(test)]
    fn to_iri_unchecked(&self) -> Iri {
        self.parse().expect("unchecked iri should still be valid")
    }
    #[cfg(not(test))]
    fn to_iri_unchecked(&self) -> Iri {
        REUSE_IRI.iri(self)
    }
}

pub trait TryToIri {
    fn try_to_iri(&self) -> Result<Iri, IriParseError>;
}

// impl ToIri for String {
//     fn to_iri(&self) -> Iri {
//         REUSE_IRI.iri(self)
//     }
// }

impl TryToIri for RopeSlice<'_> {
    fn try_to_iri(&self) -> Result<Iri, IriParseError> {
        if let Some(str) = self.as_str() {
            // Does not allocate a String
            str.parse()
        } else {
            self.to_string().parse()
        }
    }
}

// impl From<String> for Iri {
//     fn from(val: String) -> Self {
//         REUSE_IRI.iri(&val)
//     }
// }

impl From<Url> for Iri {
    fn from(val: Url) -> Self {
        val.as_str().parse().expect("url is valid iri")
    }
}

impl From<IriRef<MownStr<'_>>> for Iri {
    fn from(val: IriRef<MownStr<'_>>) -> Self {
        val.as_str().parse().expect("url is valid iri")
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

impl TryFrom<Iri> for Url {
    type Error = ();
    fn try_from(value: Iri) -> Result<Self, Self::Error> {
        Url::parse(value.as_str()).map_err(|_| ())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_log::test;

    #[test]
    fn reused_iri_set_create_iri_should_work() {
        let _iri = "http://example.com/onto#132".to_iri_unchecked();
        let _iri2 = "http://example.com/onto#132".to_iri_unchecked();
        let _iri3 = "http://example.com/onto#132".to_iri_unchecked();

        // This is 4, because the resuse set holds one of the strong reference counts
        assert_eq!(REUSE_IRI.0.len(), 1);
    }

    #[test]
    fn iri_is_url() {
        let u = Url::parse("http://jänek.de/öntology").unwrap();
        assert_eq!(&u.to_string(), "http://xn--jnek-loa.de/%C3%B6ntology");
    }
}

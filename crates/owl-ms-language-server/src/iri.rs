use lasso::{Spur, ThreadedRodeo};
use std::{borrow::Borrow, fmt::Display, ops::Deref, sync::LazyLock};

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

    // TODO do this for the url as well
}

static REUSE_IRI: LazyLock<ReusedIriSet> = LazyLock::new(ReusedIriSet::new);

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

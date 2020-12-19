use std::fmt;
use automerge_protocol as amp;

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum PathElement {
    Key(String),
    Index(u32),
}

impl PathElement {
    pub(crate) fn to_request_key(&self) -> amp::RequestKey {
        match self {
            PathElement::Key(s) => amp::RequestKey::Str(s.into()),
            PathElement::Index(i) => amp::RequestKey::Num(*i as u64),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Path(Vec<PathElement>);

impl Path {
    pub fn root() -> Path {
        Path(Vec::new())
    }

    pub fn index(mut self, index: u32) -> Self {
        self.0.push(PathElement::Index(index));
        self
    }

    pub fn key<S: Into<String>>(mut self, key: S) -> Path {
        self.0.push(PathElement::Key(key.into()));
        self
    }

    pub fn parent(&self) -> Self {
        if self.0.is_empty() {
            Path(Vec::new())
        } else {
            let mut new_path = self.0.clone();
            new_path.pop();
            Path(new_path)
        }
    }

    /// Get the final component of the path, if any
    pub(crate) fn name(&self) -> Option<&PathElement> {
        self.0.last()
    }

    pub(crate) fn elements(self) -> Vec<PathElement> {
        self.0
    }
}

impl fmt::Display for PathElement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PathElement::Key(k) => write!(f, "{}", k),
            PathElement::Index(i) => write!(f, "{}", i),
        }
    }
}

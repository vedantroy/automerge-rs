use crate::{ElementID, Key, OpID};
use std::cmp::{Ordering, PartialOrd};

impl PartialOrd for Key {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Key {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Key::Map(a), Key::Map(b)) => a.cmp(b),
            (Key::Seq(a), Key::Seq(b)) => a.cmp(b),
            (Key::Map(_), _) => Ordering::Less,
            (_, Key::Map(_)) => Ordering::Greater,
        }
    }
}

impl From<OpID> for Key {
    fn from(id: OpID) -> Self {
        Key::Seq(ElementID::ID(id))
    }
}

impl From<&OpID> for Key {
    fn from(id: &OpID) -> Self {
        Key::Seq(ElementID::ID(id.clone()))
    }
}

impl From<ElementID> for Key {
    fn from(id: ElementID) -> Self {
        Key::Seq(id)
    }
}

impl<S> From<S> for Key
where
    S: AsRef<str>,
{
    fn from(s: S) -> Self {
        Key::Map(s.as_ref().to_string())
    }
}

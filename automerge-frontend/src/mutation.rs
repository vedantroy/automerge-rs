use crate::state_tree::{StateTree, PathTarget};
use crate::error::InvalidChangeRequest;
use crate::value::Value;
use crate::{Path, PathElement};
use automerge_protocol as amp;

pub trait MutableDocument {
    fn value_at_path(&self, path: &Path) -> Option<Value>;
    fn add_change(&mut self, change: LocalChange) -> Result<(), InvalidChangeRequest>;
}

pub(crate) enum LocalOperation {
    Set(Value),
    Delete,
    Increment(u32),
    Insert(Value),
}

pub struct LocalChange {
    path: Path,
    operation: LocalOperation,
}

impl LocalChange {
    /// Set the value at `path` to `value`
    pub fn set(path: Path, value: Value) -> LocalChange {
        LocalChange {
            path,
            operation: LocalOperation::Set(value),
        }
    }

    /// Delete the entry at `path`
    pub fn delete(path: Path) -> LocalChange {
        LocalChange {
            path,
            operation: LocalOperation::Delete,
        }
    }

    /// Increment the counter at `path` by 1
    pub fn increment(path: Path) -> LocalChange {
        LocalChange {
            path,
            operation: LocalOperation::Increment(1),
        }
    }

    /// Increment the counter at path by a (possibly negative) amount `by`
    pub fn increment_by(path: Path, by: u32) -> LocalChange {
        LocalChange {
            path,
            operation: LocalOperation::Increment(by),
        }
    }

    pub fn insert(path: Path, value: Value) -> LocalChange {
        LocalChange {
            path,
            operation: LocalOperation::Insert(value),
        }
    }
}

/// `MutationTracker` is used as the context in which a mutation closure is
/// applied. The mutation tracker implements `MutableDocument`, which is how it
/// captures the changes that the mutation closure is making.
///
/// For each operation in the mutation closure the `MutationTracker` generates
/// a diff and immediately applies it to the `StateTree` it is constructed
/// with. It also adds the change to a set of operations. This set of operations
/// is used to generate a `ChangeRequest` once the closure is completed.
pub struct MutationTracker {
    pub state: StateTree,
    pub ops: Vec<amp::Op>,
}

impl MutationTracker {
    pub(crate) fn new(state_tree: StateTree) -> MutationTracker {
        MutationTracker {
            state: state_tree,
            ops: Vec::new(),
        }
    }

    pub fn ops(&self) -> Option<Vec<amp::Op>> {
        if !self.ops.is_empty() {
            Some(self.ops.clone())
        } else {
            None
        }
    }

    /// If the `value` is a map, individually assign each k,v in it to a key in
    /// the root object
    fn wrap_root_assignment(&mut self, value: &Value) -> Result<(), InvalidChangeRequest> {
        match value {
            Value::Map(kvs, amp::MapType::Map) => {
                for (k, v) in kvs.iter() {
                    self.add_change(LocalChange::set(Path::root().key(k), v.clone()))?;
                }
                Ok(())
            }
            _ => Err(InvalidChangeRequest::CannotSetNonMapObjectAsRoot {
                value: value.clone(),
            }),
        }
    }
}

impl MutableDocument for MutationTracker {
    fn value_at_path(&self, path: &Path) -> Option<Value> {
        self.state.resolve_path(path).map(|r| r.default_value())
    }

    fn add_change(&mut self, change: LocalChange) -> Result<(), InvalidChangeRequest> {
        match &change.operation {
            LocalOperation::Set(value) => {
                if let Some(name) = change.path.name() {
                    if let Some(parent) = self.state.resolve_path(&change.path.parent()) {
                        match (name, parent.target()) {
                            (PathElement::Key(k), PathTarget::Map(maptarget)) => {
                                self.state = maptarget.set_key(k, value);
                                Ok(())
                            },
                            (PathElement::Key(k), PathTarget::Table{set_key, ..}) => {
                                self.state = set_key(k, value);
                                Ok(())
                            },
                            // In this case we are trying to modify a key in something which is not
                            // an object or a table, so the path does not exist
                            (PathElement::Key(_), _) => {
                                Err(InvalidChangeRequest::NoSuchPathError { path: change.path })
                            },
                            (PathElement::Index(i), PathTarget::List{insert, ..}) => {
                                self.state = insert(*i, value);
                                Ok(())
                            },
                            (PathElement::Index(i), PathTarget::Text{insert, ..}) => {
                                match value {
                                    Value::Primitive(amp::ScalarValue::Str(s)) => {
                                        if s.len() == 1 {
                                            self.state = insert(*i, s.chars().next().unwrap());
                                            Ok(())
                                        } else {
                                            Err(InvalidChangeRequest::InsertNonTextInTextObject{
                                                path: change.path.clone(),
                                                object: value.clone(),
                                            })
                                        }
                                    },
                                    _ => Err(InvalidChangeRequest::InsertNonTextInTextObject{
                                        path: change.path.clone(),
                                        object: value.clone(),
                                    })
                                }
                            },
                            (PathElement::Index(_), _) => Err(InvalidChangeRequest::InsertWithNonSequencePath{
                                path: change.path.clone()
                            })
                        }
                    } else {
                        Err(InvalidChangeRequest::NoSuchPathError { path: change.path })
                    }
                } else {
                    self.wrap_root_assignment(value)
                }
            }
            LocalOperation::Delete => {
                if let Some(pr) = self.state.resolve_path(&change.path) {
                    self.state = pr.update.delete();
                    Ok(())
                } else {
                    Err(InvalidChangeRequest::NoSuchPathError { path: change.path })
                }
            }
            LocalOperation::Increment(by) => {
                if let Some(pr) = self.state.resolve_path(&change.path) {
                    match pr.update {
                        PathTarget::Counter{increment, ..} => {
                            self.state = increment(*by);
                            Ok(())
                        },
                        _ => Err(InvalidChangeRequest::IncrementForNonCounterObject{path: change.path.clone()})
                    }
                } else {
                    Err(InvalidChangeRequest::NoSuchPathError { path: change.path })
                }
            }
            LocalOperation::Insert(value) => {
                let index = match change.path.name() {
                    Some(PathElement::Index(i)) => i,
                    _ => {
                        return Err(InvalidChangeRequest::InsertWithNonSequencePath {
                            path: change.path,
                        })
                    }
                };
                if let Some(parent) = self.state.resolve_path(&change.path.parent()) {
                    match (parent.update, value) {
                        (PathTarget::List{insert,..}, val) => {
                            self.state = insert(*index, val);
                            Ok(())
                        },
                        (PathTarget::Text{insert, ..}, val) => {
                            match val {
                                Value::Primitive(amp::ScalarValue::Str(s)) => {
                                    if s.len() == 1 {
                                        self.state = insert(*index, s.chars().next().unwrap());
                                        Ok(())
                                    } else {
                                        Err(InvalidChangeRequest::InsertNonTextInTextObject{path: change.path, object: value.clone()})
                                    }
                                },
                                _ => Err(InvalidChangeRequest::InsertNonTextInTextObject{path: change.path, object: value.clone()})
                            }
                        },
                        _ => {
                            Err(InvalidChangeRequest::NoSuchPathError{path: change.path.clone()})
                        }
                    }
                } else {
                    Err(InvalidChangeRequest::NoSuchPathError{path: change.path.clone()})
                }
            }
        }
    }
}

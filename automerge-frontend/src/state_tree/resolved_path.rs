use super::focus::Focus;
use super::{
    random_op_id, DiffApplicationResult, LocalStateChange, MultiChar, MultiValue, StateTree,
    StateTreeComposite, StateTreeList, StateTreeMap, StateTreeTable, StateTreeText, StateTreeValue,
};
use crate::path::PathElement;
use crate::Value;
use automerge_protocol as amp;
use im::hashmap;
use std::convert::TryInto;

pub enum ResolvedPath {
    Root(ResolvedRoot),
    Map(ResolvedMap),
    Table(ResolvedTable),
    List(ResolvedList),
    Text(ResolvedText),
    Character(ResolvedChar),
    Counter(ResolvedCounter),
    Primitive(ResolvedPrimitive),
}

impl ResolvedPath {
    pub fn default_value(&self) -> Value {
        match self {
            ResolvedPath::Map(maptarget) => maptarget.multivalue.default_value().unwrap(),
            ResolvedPath::Root(root) => root.root.value(),
            ResolvedPath::Table(tabletarget) => tabletarget.multivalue.default_value().unwrap(),
            ResolvedPath::List(listtarget) => listtarget.multivalue.default_value().unwrap(),
            ResolvedPath::Text(texttarget) => texttarget.multivalue.default_value().unwrap(),
            ResolvedPath::Counter(countertarget) => {
                countertarget.multivalue.default_value().unwrap()
            }
            ResolvedPath::Primitive(p) => p.multivalue.default_value().unwrap(),
            ResolvedPath::Character(ctarget) => ctarget.multivalue.default_value().unwrap(),
        }
    }

    pub fn values(&self) -> std::collections::HashMap<amp::OpID, Value> {
        match self {
            ResolvedPath::Map(maptarget) => maptarget.multivalue.values(),
            ResolvedPath::Root(root) => {
                let mut result = std::collections::HashMap::new();
                result.insert(random_op_id(), root.root.value());
                result
            }
            ResolvedPath::Table(tabletarget) => tabletarget.multivalue.values(),
            ResolvedPath::List(listtarget) => listtarget.multivalue.values(),
            ResolvedPath::Text(texttarget) => texttarget.multivalue.values(),
            ResolvedPath::Counter(countertarget) => countertarget.multivalue.values(),
            ResolvedPath::Primitive(p) => p.multivalue.values(),
            ResolvedPath::Character(ctarget) => ctarget.multivalue.values(),
        }
    }

    pub fn object_id(&self) -> Option<amp::ObjectID> {
        match self {
            ResolvedPath::Map(maptarget) => Some(maptarget.value.object_id.clone()),
            ResolvedPath::Root(_) => Some(amp::ObjectID::Root),
            ResolvedPath::Table(tabletarget) => Some(tabletarget.value.object_id.clone()),
            ResolvedPath::List(listtarget) => Some(listtarget.value.object_id.clone()),
            ResolvedPath::Text(texttarget) => Some(texttarget.value.object_id.clone()),
            ResolvedPath::Counter(_) => None,
            ResolvedPath::Primitive(_) => None,
            ResolvedPath::Character(_) => None,
        }
    }
}

pub struct ResolvedRoot {
    pub(super) root: StateTree,
}

impl ResolvedRoot {
    pub(crate) fn set_key(&self, key: &str, value: &Value) -> LocalStateChange {
        let newvalue = MultiValue::new_from_value(
            amp::ObjectID::Root,
            &PathElement::Key(key.to_string()),
            value,
            false,
        );
        let new_state = self.root.update(key.to_string(), newvalue.diffapp());
        LocalStateChange {
            new_state,
            new_ops: newvalue.ops,
        }
    }

    pub(crate) fn delete_key(&self, key: &str) -> LocalStateChange {
        LocalStateChange {
            new_state: self.root.remove(key),
            new_ops: vec![amp::Op {
                action: amp::OpType::Del,
                obj: amp::ObjectID::Root.to_string(),
                key: amp::RequestKey::Str(key.to_string()),
                child: None,
                value: None,
                datatype: None,
                insert: false,
            }],
        }
    }
}

pub struct ResolvedCounter {
    pub(super) current_value: i64,
    pub(super) multivalue: MultiValue,
    pub(super) containing_object_id: amp::ObjectID,
    pub(super) key_in_container: PathElement,
    pub(super) focus: Box<Focus>,
}

impl ResolvedCounter {
    pub(crate) fn increment(&self, by: i64) -> LocalStateChange {
        let diffapp = DiffApplicationResult::pure(self.multivalue.update_default(
            StateTreeValue::Leaf(amp::ScalarValue::Counter(self.current_value + by)),
        ));
        let new_state = self.focus.update(diffapp);
        LocalStateChange {
            new_state,
            new_ops: vec![amp::Op {
                action: amp::OpType::Inc,
                obj: self.containing_object_id.to_string(),
                key: (&self.key_in_container).into(),
                child: None,
                value: Some(amp::ScalarValue::Int(by)),
                datatype: Some(amp::DataType::Counter),
                insert: false,
            }],
        }
    }
}

pub struct ResolvedMap {
    pub(super) value: StateTreeMap,
    pub(super) multivalue: MultiValue,
    pub(super) focus: Box<Focus>,
}

impl ResolvedMap {
    pub(crate) fn set_key(&self, key: &str, value: &Value) -> LocalStateChange {
        let newvalue = MultiValue::new_from_value(
            self.value.object_id.clone(),
            &PathElement::Key(key.to_string()),
            value,
            false,
        );
        let diffapp = newvalue.diffapp().and_then(|v| {
            let new_value = self.value.update(key.to_string(), v);
            let new_composite = StateTreeComposite::Map(new_value);
            let new_mv = self
                .multivalue
                .update_default(StateTreeValue::Internal(new_composite.clone()));
            DiffApplicationResult::pure(new_mv).with_updates(Some(
                im::HashMap::new().update(self.value.object_id.clone(), new_composite),
            ))
        });
        LocalStateChange {
            new_state: self.focus.update(diffapp),
            new_ops: newvalue.ops,
        }
    }

    pub(crate) fn delete_key(&self, key: &str) -> LocalStateChange {
        let new_value = self.value.without(key);
        let new_composite = StateTreeComposite::Map(new_value);
        let new_mv = self
            .multivalue
            .update_default(StateTreeValue::Internal(new_composite));
        let diffapp = DiffApplicationResult::pure(new_mv);
        LocalStateChange {
            new_state: self.focus.update(diffapp),
            new_ops: vec![amp::Op {
                action: amp::OpType::Del,
                obj: self.value.object_id.to_string(),
                key: amp::RequestKey::Str(key.to_string()),
                child: None,
                value: None,
                datatype: None,
                insert: false,
            }],
        }
    }
}

pub struct ResolvedTable {
    pub(super) value: StateTreeTable,
    pub(super) multivalue: MultiValue,
    pub(super) focus: Box<Focus>,
}

impl ResolvedTable {
    pub(crate) fn set_key(&self, key: &str, value: &Value) -> LocalStateChange {
        let newvalue = MultiValue::new_from_value(
            self.value.object_id.clone(),
            &PathElement::Key(key.to_string()),
            value,
            false,
        );
        let diffapp = newvalue.diffapp().and_then(|v| {
            let new_value = self.value.update(key.to_string(), v);
            let new_composite = StateTreeComposite::Table(new_value);
            let new_mv = self
                .multivalue
                .update_default(StateTreeValue::Internal(new_composite.clone()));
            DiffApplicationResult::pure(new_mv).with_updates(Some(
                hashmap!(self.value.object_id.clone() => new_composite),
            ))
        });
        LocalStateChange {
            new_state: self.focus.update(diffapp),
            new_ops: newvalue.ops,
        }
    }

    pub(crate) fn delete_key(&self, key: &str) -> LocalStateChange {
        let new_value = self.value.without(key);
        let new_composite = StateTreeComposite::Table(new_value);
        let new_mv = self
            .multivalue
            .update_default(StateTreeValue::Internal(new_composite));
        let diffapp = DiffApplicationResult::pure(new_mv);
        LocalStateChange {
            new_state: self.focus.update(diffapp),
            new_ops: vec![amp::Op {
                action: amp::OpType::Del,
                obj: self.value.object_id.to_string(),
                key: amp::RequestKey::Str(key.to_string()),
                child: None,
                value: None,
                datatype: None,
                insert: false,
            }],
        }
    }
}

pub struct ResolvedText {
    pub(super) value: StateTreeText,
    pub(super) multivalue: MultiValue,
    pub(super) update: Box<dyn Fn(DiffApplicationResult<MultiValue>) -> StateTree>,
}

impl ResolvedText {
    pub(crate) fn insert(&self, index: u32, c: char) -> LocalStateChange {
        let mut new_chars = self.value.chars.clone();
        new_chars.insert(index.try_into().unwrap(), MultiChar::new_from_char(c));
        let updated = StateTreeComposite::Text(StateTreeText {
            object_id: self.value.object_id.clone(),
            chars: new_chars,
        });
        let mv = self
            .multivalue
            .update_default(StateTreeValue::Internal(updated.clone()));
        let diffapp = DiffApplicationResult::pure(mv)
            .with_updates(Some(hashmap!(self.value.object_id.clone() => updated)));
        LocalStateChange {
            new_state: (self.update)(diffapp),
            new_ops: vec![amp::Op {
                action: amp::OpType::Set,
                obj: self.value.object_id.to_string(),
                key: amp::RequestKey::Num(index.try_into().unwrap()),
                child: None,
                value: Some(amp::ScalarValue::Str(c.to_string())),
                datatype: None,
                insert: true,
            }],
        }
    }

    pub(crate) fn set(&self, index: u32, c: char) -> LocalStateChange {
        let mut new_chars = self.value.chars.clone();
        new_chars.set(index.try_into().unwrap(), MultiChar::new_from_char(c));
        let updated = StateTreeComposite::Text(StateTreeText {
            object_id: self.value.object_id.clone(),
            chars: new_chars,
        });
        let mv = self
            .multivalue
            .update_default(StateTreeValue::Internal(updated.clone()));
        let diffapp = DiffApplicationResult::pure(mv)
            .with_updates(Some(hashmap!(self.value.object_id.clone() => updated)));
        let new_state = (self.update)(diffapp);
        LocalStateChange {
            new_state,
            new_ops: vec![amp::Op {
                action: amp::OpType::Set,
                obj: self.value.object_id.to_string(),
                key: amp::RequestKey::Num(index.try_into().unwrap()),
                child: None,
                value: Some(amp::ScalarValue::Str(c.to_string())),
                datatype: None,
                insert: false,
            }],
        }
    }

    pub(crate) fn remove(&self, index: u32) -> LocalStateChange {
        let mut new_chars = self.value.chars.clone();
        new_chars.remove(index.try_into().unwrap());
        let updated = StateTreeComposite::Text(StateTreeText {
            object_id: self.value.object_id.clone(),
            chars: new_chars,
        });
        let mv = self
            .multivalue
            .update_default(StateTreeValue::Internal(updated.clone()));
        let diffapp = DiffApplicationResult::pure(mv)
            .with_updates(Some(hashmap!(self.value.object_id.clone() => updated)));
        let new_state = (self.update)(diffapp);
        LocalStateChange {
            new_state,
            new_ops: vec![amp::Op {
                action: amp::OpType::Del,
                obj: self.value.object_id.to_string(),
                key: amp::RequestKey::Num(index.try_into().unwrap()),
                child: None,
                value: None,
                datatype: None,
                insert: false,
            }],
        }
    }
}

pub struct ResolvedList {
    pub(super) value: StateTreeList,
    pub(super) multivalue: MultiValue,
    pub(super) focus: Box<Focus>,
}

impl ResolvedList {
    pub(crate) fn set(&self, index: u32, v: &Value) -> LocalStateChange {
        let newvalue = MultiValue::new_from_value(
            self.value.object_id.clone(),
            &PathElement::Index(index),
            v,
            false,
        );
        let diffapp = newvalue.diffapp().and_then(|v| {
            let new_value = StateTreeComposite::List(self.value.set(index.try_into().unwrap(), v));
            let mv = self
                .multivalue
                .update_default(StateTreeValue::Internal(new_value.clone()));
            DiffApplicationResult::pure(mv)
                .with_updates(Some(hashmap!(self.value.object_id.clone() => new_value)))
        });
        let new_state = self.focus.update(diffapp);
        LocalStateChange {
            new_state,
            new_ops: newvalue.ops,
        }
    }

    pub(crate) fn insert(&self, index: u32, v: &Value) -> LocalStateChange {
        let newvalue = MultiValue::new_from_value(
            self.value.object_id.clone(),
            &PathElement::Index(index),
            v,
            false,
        );
        let diffapp = newvalue.diffapp().and_then(|v| {
            let new_value =
                StateTreeComposite::List(self.value.insert(index.try_into().unwrap(), v));
            let mv = self
                .multivalue
                .update_default(StateTreeValue::Internal(new_value.clone()));
            DiffApplicationResult::pure(mv)
                .with_updates(Some(hashmap!(self.value.object_id.clone() => new_value)))
        });
        LocalStateChange {
            new_state: self.focus.update(diffapp),
            new_ops: newvalue.ops,
        }
    }

    pub(crate) fn remove(&self, index: u32) -> LocalStateChange {
        let new_value = StateTreeComposite::List(self.value.remove(index.try_into().unwrap()));
        let mv = self
            .multivalue
            .update_default(StateTreeValue::Internal(new_value.clone()));
        let diffapp = DiffApplicationResult::pure(mv)
            .with_updates(Some(hashmap!(self.value.object_id.clone() => new_value)));
        LocalStateChange {
            new_state: self.focus.update(diffapp),
            new_ops: vec![amp::Op {
                action: amp::OpType::Del,
                obj: self.value.object_id.to_string(),
                key: amp::RequestKey::Num(index as u64),
                child: None,
                value: None,
                datatype: None,
                insert: false,
            }],
        }
    }
}

pub struct ResolvedChar {
    pub(super) multivalue: MultiValue,
}

pub struct ResolvedPrimitive {
    pub(super) multivalue: MultiValue,
}

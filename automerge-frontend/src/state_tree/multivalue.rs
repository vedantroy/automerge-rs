use automerge_protocol as amp;
use im::hashmap;

use super::{
    new_object_id, random_op_id, value_to_datatype, DiffApplicationResult, StateTreeComposite,
    StateTreeList, StateTreeMap, StateTreeTable, StateTreeText, StateTreeValue,
};
use crate::error;
use crate::path::PathElement;
use crate::value::Value;
use std::convert::TryInto;

/// A set of conflicting values for the same key, indexed by OpID
// TODO make the type system aware that this always has at least one value
#[derive(Debug, Clone)]
pub(super) struct MultiValue(im::HashMap<amp::OpID, StateTreeValue>);

impl MultiValue {
    pub(super) fn new() -> MultiValue {
        MultiValue(im::HashMap::new())
    }

    pub(super) fn new_from_statetree_value(value: StateTreeValue) -> MultiValue {
        MultiValue(hashmap! {random_op_id() => value})
    }

    pub(super) fn new_from_value(
        parent_id: amp::ObjectID,
        key: &PathElement,
        value: &Value,
        insert: bool,
    ) -> NewValue<MultiValue> {
        match value {
            Value::Map(props, amp::MapType::Map) => {
                let map_id = new_object_id();
                let make_op = amp::Op {
                    action: amp::OpType::MakeMap,
                    obj: parent_id.to_string(),
                    key: key.to_request_key(),
                    child: Some(map_id.to_string()),
                    value: None,
                    datatype: None,
                    insert,
                };
                let map = im::HashMap::new();
                let newvalue: NewValue<im::HashMap<String, MultiValue>> =
                    NewValue::init(map, make_op);
                props
                    .iter()
                    .fold(newvalue, |newvalue_so_far, (key, value)| {
                        newvalue_so_far.and_then(|m| {
                            MultiValue::new_from_value(
                                map_id.clone(),
                                &PathElement::Key(key.to_string()),
                                value,
                                false,
                            )
                            .map(|v| m.update(key.to_string(), v))
                        })
                    })
                    .map(|map| {
                        MultiValue::new_from_statetree_value(StateTreeValue::Composite(
                            StateTreeComposite::Map(StateTreeMap {
                                object_id: map_id.clone(),
                                props: map,
                            }),
                        ))
                    })
            }
            Value::Map(props, amp::MapType::Table) => {
                let table_id = new_object_id();
                let table = im::HashMap::new();
                let make_op = amp::Op {
                    action: amp::OpType::MakeTable,
                    obj: parent_id.to_string(),
                    key: key.to_request_key(),
                    child: Some(table_id.to_string()),
                    value: None,
                    datatype: None,
                    insert,
                };
                let newvalue: NewValue<im::HashMap<String, MultiValue>> =
                    NewValue::init(table, make_op);
                props
                    .iter()
                    .fold(newvalue, |newvalue_so_far, (key, value)| {
                        newvalue_so_far.and_then(|t| {
                            MultiValue::new_from_value(
                                table_id.clone(),
                                &PathElement::Key(key.to_string()),
                                value,
                                false,
                            )
                            .map(|v| t.update(key.to_string(), v))
                        })
                    })
                    .map(|table| {
                        MultiValue::new_from_statetree_value(StateTreeValue::Composite(
                            StateTreeComposite::Table(StateTreeTable {
                                object_id: table_id.clone(),
                                props: table,
                            }),
                        ))
                    })
            }
            Value::Sequence(vals) => {
                let list_id = new_object_id();
                let elems = im::Vector::new();
                let make_op = amp::Op {
                    action: amp::OpType::MakeList,
                    obj: parent_id.to_string(),
                    key: key.to_request_key(),
                    child: Some(list_id.to_string()),
                    value: None,
                    datatype: None,
                    insert,
                };
                let newvalue: NewValue<im::Vector<MultiValue>> = NewValue::init(elems, make_op);
                vals.iter()
                    .enumerate()
                    .fold(newvalue, |newvalue_so_far, (index, elem)| {
                        newvalue_so_far.and_then(|l| {
                            MultiValue::new_from_value(
                                list_id.clone(),
                                &PathElement::Index(index.try_into().unwrap()),
                                elem,
                                true,
                            )
                            .map(|e| {
                                let mut new_l = l.clone();
                                new_l.push_back(e);
                                new_l
                            })
                        })
                    })
                    .map(|elems| {
                        MultiValue(im::HashMap::new().update(
                            random_op_id(),
                            StateTreeValue::Composite(StateTreeComposite::List(StateTreeList {
                                object_id: list_id.clone(),
                                elements: elems,
                            })),
                        ))
                    })
            }
            Value::Text(chars) => {
                let text_id = new_object_id();
                let make_op = amp::Op {
                    action: amp::OpType::MakeText,
                    obj: parent_id.to_string(),
                    key: key.to_request_key(),
                    child: Some(text_id.to_string()),
                    value: None,
                    datatype: None,
                    insert,
                };
                let newvalue: NewValue<im::Vector<MultiChar>> =
                    NewValue::init(im::Vector::new(), make_op);
                chars
                    .iter()
                    .enumerate()
                    .fold(newvalue, |newvalue_so_far, (index, c)| {
                        newvalue_so_far.and_then(|newval_chars| {
                            NewValue::init(
                                MultiChar::new_from_char(*c),
                                amp::Op {
                                    action: amp::OpType::Set,
                                    obj: text_id.to_string(),
                                    key: amp::RequestKey::Num(index as u64),
                                    child: None,
                                    value: Some(amp::ScalarValue::Str(c.to_string())),
                                    datatype: None,
                                    insert: true,
                                },
                            )
                            .map(|newchar| {
                                let mut chars = newval_chars.clone();
                                chars.push_back(newchar);
                                chars
                            })
                        })
                    })
                    .map(|newchars| {
                        MultiValue::new_from_statetree_value(StateTreeValue::Composite(
                            StateTreeComposite::Text(StateTreeText {
                                object_id: text_id.clone(),
                                chars: newchars,
                            }),
                        ))
                    })
            }
            Value::Primitive(v) => NewValue::init(
                MultiValue::new_from_statetree_value(StateTreeValue::Leaf(v.clone())),
                amp::Op {
                    action: amp::OpType::Set,
                    obj: parent_id.to_string(),
                    key: key.to_request_key(),
                    child: None,
                    value: Some(v.clone()),
                    datatype: Some(value_to_datatype(v)),
                    insert,
                },
            ),
        }
    }

    pub(super) fn apply_diff(
        &self,
        diff: &std::collections::HashMap<amp::OpID, amp::Diff>,
    ) -> Result<DiffApplicationResult<MultiValue>, error::InvalidPatch> {
        let mut result = DiffApplicationResult::pure(self.0.clone());
        for (opid, subdiff) in diff.iter() {
            let application_result = if let Some(existing_value) = self.0.get(opid) {
                match existing_value {
                    StateTreeValue::Leaf(_) => StateTreeValue::new_from_diff(subdiff),
                    StateTreeValue::Composite(composite) => Ok(composite
                        .apply_diff(subdiff)?
                        .map(StateTreeValue::Composite)),
                }
            } else {
                StateTreeValue::new_from_diff(subdiff)
            }?;
            result = result
                .map(|u| u.update(opid.clone(), application_result.value().clone()))
                .with_updates(application_result.index_updates());
        }
        Ok(result.map(MultiValue))
    }

    pub(super) fn default_statetree_value(&self) -> Option<StateTreeValue> {
        self.0.get(&self.default_opid()).cloned()
    }

    pub(super) fn default_value(&self) -> Option<Value> {
        self.default_statetree_value().map(|sv| sv.value())
    }

    pub(super) fn default_opid(&self) -> amp::OpID {
        let mut opids: Vec<&amp::OpID> = self.0.keys().collect();
        opids.sort();
        opids.reverse();
        opids.first().cloned().unwrap().clone()
    }

    pub(super) fn ordered_statetree_values(&self) -> Vec<(amp::OpID, StateTreeValue)> {
        let mut opids: Vec<amp::OpID> = self.0.keys().cloned().collect();
        opids.sort();
        opids.reverse();
        opids
            .iter()
            .map(|oid| (oid.clone(), self.0.get(oid).unwrap().clone()))
            .collect()
    }

    pub(super) fn update_default(&self, val: StateTreeValue) -> MultiValue {
        MultiValue(self.0.update(self.default_opid(), val))
    }

    pub(super) fn values(&self) -> std::collections::HashMap<amp::OpID, Value> {
        self.ordered_statetree_values()
            .into_iter()
            .map(|(oid, value)| (oid, value.value()))
            .collect()
    }
}

pub(super) struct NewValue<T> {
    value: T,
    ops: Vec<amp::Op>,
    index_updates: im::HashMap<amp::ObjectID, StateTreeComposite>,
}

impl<T> NewValue<T> {
    pub(super) fn diffapp(&self) -> DiffApplicationResult<T>
    where
        T: Clone,
    {
        DiffApplicationResult::pure(self.value.clone())
            .with_updates(Some(self.index_updates.clone()))
    }

    pub(super) fn ops(self) -> Vec<amp::Op> {
        self.ops
    }

    fn init(t: T, op: amp::Op) -> NewValue<T> {
        NewValue {
            value: t,
            ops: vec![op],
            index_updates: im::HashMap::new(),
        }
    }

    fn and_then<F, G>(&self, f: F) -> NewValue<G>
    where
        T: Clone,
        F: Fn(T) -> NewValue<G>,
    {
        let newvalue = (f)(self.value.clone());
        let mut newops = self.ops.clone();
        newops.extend(newvalue.ops);
        NewValue {
            value: newvalue.value,
            ops: newops,
            index_updates: newvalue.index_updates.union(self.index_updates.clone()),
        }
    }

    fn map<F, G>(self, f: F) -> NewValue<G>
    where
        F: Fn(T) -> G,
    {
        NewValue {
            value: f(self.value),
            ops: self.ops,
            index_updates: self.index_updates,
        }
    }
}

/// This struct exists to constrain the values of a text type to just containing
/// sequences of chars
#[derive(Debug, Clone)]
pub(super) struct MultiChar(im::HashMap<amp::OpID, char>);

impl MultiChar {
    pub(super) fn new() -> MultiChar {
        MultiChar(im::HashMap::new())
    }

    pub(super) fn new_from_char(c: char) -> MultiChar {
        MultiChar(im::HashMap::new().update(random_op_id(), c))
    }

    pub(super) fn apply_diff(
        &self,
        parent_object_id: &amp::ObjectID,
        diff: &std::collections::HashMap<amp::OpID, amp::Diff>,
    ) -> Result<MultiChar, error::InvalidPatch> {
        let mut new_values = self.0.clone();
        for (opid, subdiff) in diff.iter() {
            match subdiff {
                amp::Diff::Value(amp::ScalarValue::Str(s)) => {
                    if s.len() != 1 {
                        return Err(error::InvalidPatch::InsertNonTextInTextObject {
                            object_id: parent_object_id.clone(),
                            diff: subdiff.clone(),
                        });
                    } else {
                        new_values = new_values.update(opid.clone(), s.chars().next().unwrap());
                    }
                }
                _ => {
                    return Err(error::InvalidPatch::InsertNonTextInTextObject {
                        object_id: parent_object_id.clone(),
                        diff: subdiff.clone(),
                    });
                }
            }
        }
        Ok(MultiChar(new_values))
    }

    pub(super) fn default_char(&self) -> Option<char> {
        let mut opids: Vec<&amp::OpID> = self.0.keys().collect();
        opids.sort();
        opids.reverse();
        opids.first().map(|oid| *self.0.get(*oid).unwrap())
    }
}

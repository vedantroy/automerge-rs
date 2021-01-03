use automerge_protocol as amp;

use super::{
    new_object_id, random_op_id, value_to_datatype, StateTreeChange, StateTreeComposite,
    StateTreeList, StateTreeMap, StateTreeTable, StateTreeText, StateTreeValue,
};
use crate::error;
use crate::path::PathElement;
use crate::value::Value;
use std::convert::TryInto;
use std::iter::Iterator;

/// A set of conflicting values for the same key, indexed by OpID
#[derive(Debug, Clone)]
pub(super) struct MultiValue {
    winning_value: (amp::OpID, StateTreeValue),
    conflicts: im::HashMap<amp::OpID, StateTreeValue>,
}

impl MultiValue {
    pub(super) fn new_from_statetree_value(opid: amp::OpID, value: StateTreeValue) -> MultiValue {
        MultiValue {
            winning_value: (opid, value),
            conflicts: im::HashMap::new(),
        }
    }

    pub fn new_from_diff(
        opid: amp::OpID,
        diff: &amp::Diff,
    ) -> Result<StateTreeChange<MultiValue>, error::InvalidPatch> {
        StateTreeValue::new_from_diff(diff)?.fallible_map(move |value| {
            Ok(MultiValue {
                winning_value: (opid, value),
                conflicts: im::HashMap::new(),
            })
        })
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
                        MultiValue::new_from_statetree_value(
                            random_op_id(),
                            StateTreeValue::Composite(StateTreeComposite::Map(StateTreeMap {
                                object_id: map_id.clone(),
                                props: map,
                            })),
                        )
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
                        MultiValue::new_from_statetree_value(
                            random_op_id(),
                            StateTreeValue::Composite(StateTreeComposite::Table(StateTreeTable {
                                object_id: table_id.clone(),
                                props: table,
                            })),
                        )
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
                        MultiValue::new_from_statetree_value(
                            random_op_id(),
                            StateTreeValue::Composite(StateTreeComposite::List(StateTreeList {
                                object_id: list_id.clone(),
                                elements: elems,
                            })),
                        )
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
                        MultiValue::new_from_statetree_value(
                            random_op_id(),
                            StateTreeValue::Composite(StateTreeComposite::Text(StateTreeText {
                                object_id: text_id.clone(),
                                chars: newchars,
                            })),
                        )
                    })
            }
            Value::Primitive(v) => NewValue::init(
                MultiValue::new_from_statetree_value(
                    random_op_id(),
                    StateTreeValue::Leaf(v.clone()),
                ),
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
        opid: &amp::OpID,
        subdiff: &amp::Diff,
    ) -> Result<StateTreeChange<MultiValue>, error::InvalidPatch> {
        let current = self.tree_values();
        let update_for_opid = if let Some(existing_value) = current.get(opid) {
            match existing_value {
                StateTreeValue::Leaf(_) => StateTreeValue::new_from_diff(subdiff),
                StateTreeValue::Composite(composite) => composite
                    .apply_diff(subdiff)
                    .map(|value| value.map(StateTreeValue::Composite)),
            }
        } else {
            StateTreeValue::new_from_diff(subdiff)
        }?;
        update_for_opid.fallible_map(|update| {
            Self::multivalue_from_opids_and_values(current.update(opid.clone(), update))
        })
    }

    pub(super) fn apply_diff_iter<'a, 'b, I>(
        &'a self,
        diff: &mut I,
    ) -> Result<StateTreeChange<MultiValue>, error::InvalidPatch>
    where
        I: Iterator<Item = (&'b amp::OpID, &'b amp::Diff)>,
    {
        let init = Ok(StateTreeChange::pure(self.tree_values()));
        let updated = diff.fold(init, move |updated_so_far, (opid, subdiff)| {
            //let result_so_far = result_so_far?;
            updated_so_far?.fallible_and_then(|updated| {
                let update_for_opid = if let Some(existing_value) = updated.get(opid) {
                    match existing_value {
                        StateTreeValue::Leaf(_) => StateTreeValue::new_from_diff(subdiff),
                        StateTreeValue::Composite(composite) => composite
                            .apply_diff(subdiff)
                            .map(|value| value.map(StateTreeValue::Composite)),
                    }
                } else {
                    StateTreeValue::new_from_diff(subdiff)
                }?;
                Ok(update_for_opid.map(|u| updated.update(opid.clone(), u)))
            })
        })?;
        updated.fallible_map(Self::multivalue_from_opids_and_values)
    }

    pub(super) fn default_statetree_value(&self) -> StateTreeValue {
        self.winning_value.1.clone()
    }

    pub(super) fn default_value(&self) -> Value {
        self.winning_value.1.value()
    }

    pub(super) fn default_opid(&self) -> amp::OpID {
        self.winning_value.0.clone()
    }

    pub(super) fn update_default(&self, val: StateTreeValue) -> MultiValue {
        MultiValue {
            winning_value: (self.winning_value.0.clone(), val),
            conflicts: self.conflicts.clone(),
        }
    }

    fn tree_values(&self) -> im::HashMap<amp::OpID, StateTreeValue> {
        self.conflicts
            .update(self.winning_value.0.clone(), self.winning_value.1.clone())
    }

    pub(super) fn values(&self) -> std::collections::HashMap<amp::OpID, Value> {
        self.tree_values()
            .iter()
            .map(|(opid, v)| (opid.clone(), v.value()))
            .collect()
    }

    fn multivalue_from_opids_and_values<I>(
        opids_and_values: I,
    ) -> Result<MultiValue, error::InvalidPatch>
    where
        I: IntoIterator<Item = (amp::OpID, StateTreeValue)>,
    {
        let mut opids_and_values_vec: Vec<(amp::OpID, StateTreeValue)> =
            opids_and_values.into_iter().collect();
        opids_and_values_vec.sort_by(|(o1, _), (o2, _)| o1.cmp(o2));
        opids_and_values_vec.reverse();
        //updates_vec.sort_by_key(|(o, _)| o.clone());
        match opids_and_values_vec.split_first() {
            Some(((opid, value), rest)) => Ok(MultiValue {
                winning_value: (opid.clone(), value.clone()),
                conflicts: rest.into(),
            }),
            None => Err(error::InvalidPatch::DiffCreatedObjectWithNoValue),
        }
    }
}

pub(super) struct NewValue<T> {
    value: T,
    ops: Vec<amp::Op>,
    index_updates: im::HashMap<amp::ObjectID, StateTreeComposite>,
}

impl<T> NewValue<T> {
    pub(super) fn state_tree_change(&self) -> StateTreeChange<T>
    where
        T: Clone,
    {
        StateTreeChange::pure(self.value.clone()).with_updates(Some(self.index_updates.clone()))
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

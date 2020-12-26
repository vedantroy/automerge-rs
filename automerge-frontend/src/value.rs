use crate::PathElement;
use automerge_protocol as amp;
use serde::Serialize;
use std::{borrow::Borrow, collections::HashMap};
use std::convert::TryInto;

#[derive(Serialize, Clone, Debug, PartialEq)]
pub struct Conflicts(HashMap<amp::OpID, Value>);

impl From<HashMap<amp::OpID, Value>> for Conflicts {
    fn from(hmap: HashMap<amp::OpID, Value>) -> Self {
        Conflicts(hmap)
    }
}

#[derive(Serialize, Clone, Debug, PartialEq)]
#[serde(untagged)]
pub enum Value {
    Map(HashMap<String, Value>, amp::MapType),
    Sequence(Vec<Value>),
    Text(Vec<char>),
    Primitive(amp::ScalarValue),
}

impl From<amp::ScalarValue> for Value {
    fn from(val: amp::ScalarValue) -> Self {
        Value::Primitive(val)
    }
}

impl From<&amp::ScalarValue> for Value {
    fn from(val: &amp::ScalarValue) -> Self {
        val.clone().into()
    }
}

impl From<&str> for Value {
    fn from(s: &str) -> Self {
        Value::Primitive(amp::ScalarValue::Str(s.to_string()))
    }
}

impl<T> From<Vec<T>> for Value
where
    T: Into<Value>,
{
    fn from(v: Vec<T>) -> Self {
        Value::Sequence(v.into_iter().map(|t| t.into()).collect())
    }
}

impl<T, K> From<HashMap<K, T>> for Value
where
    T: Into<Value>,
    K: Borrow<str>,
{
    fn from(h: HashMap<K, T>) -> Self {
        Value::Map(
            h.into_iter()
                .map(|(k, v)| (k.borrow().to_string(), v.into()))
                .collect(),
            amp::MapType::Map,
        )
    }
}

impl Value {
    pub fn from_json(json: &serde_json::Value) -> Value {
        match json {
            serde_json::Value::Object(kvs) => {
                let result: HashMap<String, Value> = kvs
                    .iter()
                    .map(|(k, v)| (k.clone(), Value::from_json(v)))
                    .collect();
                Value::Map(result, amp::MapType::Map)
            }
            serde_json::Value::Array(vs) => {
                Value::Sequence(vs.iter().map(Value::from_json).collect())
            }
            serde_json::Value::String(s) => Value::Primitive(amp::ScalarValue::Str(s.clone())),
            serde_json::Value::Number(n) => {
                Value::Primitive(amp::ScalarValue::F64(n.as_f64().unwrap_or(0.0)))
            }
            serde_json::Value::Bool(b) => Value::Primitive(amp::ScalarValue::Boolean(*b)),
            serde_json::Value::Null => Value::Primitive(amp::ScalarValue::Null),
        }
    }

    pub fn to_json(&self) -> serde_json::Value {
        match self {
            Value::Map(map, _) => {
                let result: serde_json::map::Map<String, serde_json::Value> =
                    map.iter().map(|(k, v)| (k.clone(), v.to_json())).collect();
                serde_json::Value::Object(result)
            }
            Value::Sequence(elements) => {
                serde_json::Value::Array(elements.iter().map(|v| v.to_json()).collect())
            }
            Value::Text(chars) => serde_json::Value::String(chars.iter().collect()),
            Value::Primitive(v) => match v {
                amp::ScalarValue::F64(n) => serde_json::Value::Number(
                    serde_json::Number::from_f64(*n).unwrap_or_else(|| serde_json::Number::from(0)),
                ),
                amp::ScalarValue::F32(n) => serde_json::Value::Number(
                    serde_json::Number::from_f64(f64::from(*n))
                        .unwrap_or_else(|| serde_json::Number::from(0)),
                ),
                amp::ScalarValue::Uint(n) => {
                    serde_json::Value::Number(serde_json::Number::from(*n))
                }
                amp::ScalarValue::Int(n) => serde_json::Value::Number(serde_json::Number::from(*n)),
                amp::ScalarValue::Str(s) => serde_json::Value::String(s.to_string()),
                amp::ScalarValue::Boolean(b) => serde_json::Value::Bool(*b),
                amp::ScalarValue::Counter(c) => {
                    serde_json::Value::Number(serde_json::Number::from(*c))
                }
                amp::ScalarValue::Timestamp(t) => {
                    serde_json::Value::Number(serde_json::Number::from(*t))
                }
                amp::ScalarValue::Null => serde_json::Value::Null,
            },
        }
    }
}

/// Convert a value to a vector of op requests that will create said value.
///
/// #Arguments
///
/// * parent_object - The ID of the "parent" object, i.e the object that will
///                   contain the newly created object
/// * key           - The property that the newly created object will populate
///                   within the parent object.
///
///
/// Returns a vector of the op requests which will create this value
pub(crate) fn value_to_op_requests(
    parent_object: amp::ObjectID,
    key: &PathElement,
    v: &Value,
    insert: bool,
) -> Vec<amp::Op> {
    match v {
        Value::Sequence(vs) => {
            let list_id = new_object_id();
            let make_op = amp::Op {
                action: amp::OpType::MakeList,
                obj: parent_object.to_string(),
                key: key.to_request_key(),
                child: Some(list_id.to_string()),
                value: None,
                datatype: None,
                insert,
            };
            let child_requests_nested: Vec<Vec<amp::Op>> = vs
                .iter()
                .enumerate()
                .map(|(index, v)| {
                    value_to_op_requests(list_id.clone(), &PathElement::Index(index.try_into().unwrap()), v, true)
                })
                .collect();
            let child_requests: Vec<amp::Op> = child_requests_nested.into_iter().flatten().collect();
            let mut result = vec![make_op];
            result.extend(child_requests);
            result
        }
        Value::Text(chars) => {
            let text_id = new_object_id();
            let make_op = amp::Op {
                action: amp::OpType::MakeText,
                obj: parent_object.to_string(),
                key: key.to_request_key(),
                child: Some(text_id.to_string()),
                value: None,
                datatype: None,
                insert,
            };
            let insert_ops: Vec<amp::Op> = chars
                .iter()
                .enumerate()
                .map(|(i, c)| amp::Op {
                    action: amp::OpType::Set,
                    obj: text_id.to_string(),
                    key: amp::RequestKey::Num(i as u64),
                    child: None,
                    value: Some(amp::ScalarValue::Str(c.to_string())),
                    datatype: None,
                    insert: true,
                })
                .collect();
            let mut ops = vec![make_op];
            ops.extend(insert_ops.into_iter());
            ops
        }
        Value::Map(kvs, map_type) => {
            let make_action = match map_type {
                amp::MapType::Map => amp::OpType::MakeMap,
                amp::MapType::Table => amp::OpType::MakeTable,
            };
            let map_id = new_object_id();
            let make_op = amp::Op {
                action: make_action,
                obj: parent_object.to_string(),
                key: key.to_request_key(),
                child: Some(map_id.to_string()),
                value: None,
                datatype: None,
                insert,
            };
            let child_requests_nested: HashMap<String, Vec<amp::Op>> = kvs
                .iter()
                .map(|(k, v)| {
                    (
                        k.clone(),
                        value_to_op_requests(map_id.clone(), &PathElement::Key(k.clone()), v, false),
                    )
                })
                .collect();
            let mut result = vec![make_op];
            let child_requests: Vec<amp::Op> = child_requests_nested
                .iter()
                .flat_map(|(_, o)| o)
                .cloned()
                .collect();
            result.extend(child_requests);
            result
        }
        Value::Primitive(prim_value) => {
            let ops = vec![amp::Op {
                action: amp::OpType::Set,
                obj: parent_object.to_string(),
                key: key.to_request_key(),
                child: None,
                value: Some(prim_value.clone()),
                datatype: Some(value_to_datatype(prim_value)),
                insert,
            }];
            ops
        }
    }
}

fn new_object_id() -> amp::ObjectID {
    amp::ObjectID::ID(random_op_id())
}

pub(crate) fn random_op_id() -> amp::OpID {
    amp::OpID::new(1, &amp::ActorID::random())
}

fn value_to_datatype(value: &amp::ScalarValue) -> amp::DataType {
    match value {
        amp::ScalarValue::Counter(_) => amp::DataType::Counter,
        amp::ScalarValue::Timestamp(_) => amp::DataType::Timestamp,
        _ => amp::DataType::Undefined,
    }
}

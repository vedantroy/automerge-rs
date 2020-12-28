use crate::error;
use crate::Value;
use crate::{Path, PathElement};
use automerge_protocol as amp;
use im::hashmap;
use std::convert::TryInto;

mod focus;

pub(crate) struct LocalStateChange {
    pub new_state: StateTree,
    pub new_ops: Vec<amp::Op>,
}

#[derive(Debug, Clone)]
pub(crate) struct StateTree {
    root_map: im::HashMap<String, MultiValue>,
    object_index: im::HashMap<amp::ObjectID, StateTreeComposite>,
}

/// Represents a set of possible values for a particular path
trait Register {
    /// All the values in this register (i.e the conflicting values)
    fn values(&self) -> std::collections::HashMap<amp::OpID, Value>;
    /// The value which "wins"
    fn default_value(&self) -> Value;
}

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

pub struct ResolvedRoot {
    root: StateTree,
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
    current_value: i64,
    multivalue: MultiValue,
    containing_object_id: amp::ObjectID,
    key_in_container: PathElement,
    focus: Box<focus::Focus>,
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
    value: StateTreeMap,
    multivalue: MultiValue,
    focus: Box<focus::Focus>,
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
    value: StateTreeTable,
    multivalue: MultiValue,
    focus: Box<focus::Focus>,
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
    value: StateTreeText,
    multivalue: MultiValue,
    update: Box<dyn Fn(DiffApplicationResult<MultiValue>) -> StateTree>,
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
    value: StateTreeList,
    multivalue: MultiValue,
    focus: Box<focus::Focus>,
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
    multivalue: MultiValue,
}

pub struct ResolvedPrimitive {
    multivalue: MultiValue,
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

impl StateTree {
    pub fn new() -> StateTree {
        StateTree {
            root_map: im::HashMap::new(),
            object_index: im::HashMap::new(),
        }
    }

    pub fn apply_diff(&self, diff: &amp::Diff) -> Result<StateTree, error::InvalidPatch> {
        match diff {
            amp::Diff::Map(mapdiff) => {
                let amp::MapDiff {
                    object_id,
                    obj_type,
                    props: _props,
                } = mapdiff;
                if *object_id != amp::ObjectID::Root {
                    Err(error::InvalidPatch::PatchDidNotBeginAtRoot)
                } else if *obj_type != amp::MapType::Map {
                    Err(error::InvalidPatch::MismatchingObjectType {
                        object_id: amp::ObjectID::Root,
                        patch_expected_type: Some(amp::ObjType::map()),
                        actual_type: Some(amp::ObjType::Map(*obj_type)),
                    })
                } else {
                    self.apply_map_diff(diff)
                }
            }
            _ => Err(error::InvalidPatch::PatchDidNotBeginAtRoot),
        }
    }

    fn apply_map_diff(&self, diff: &amp::Diff) -> Result<StateTree, error::InvalidPatch> {
        let application_result = StateTreeComposite::Map(StateTreeMap {
            object_id: amp::ObjectID::Root,
            props: self.root_map.clone(),
        })
        .apply_diff(diff)?;
        match application_result.value() {
            StateTreeComposite::Map(StateTreeMap { props: values, .. }) => {
                let new_object_index = match application_result.index_updates() {
                    None => self.object_index.clone(),
                    Some(updates) => updates.union(self.object_index.clone()),
                };
                Ok(StateTree {
                    root_map: values.clone(),
                    object_index: new_object_index,
                })
            }
            _ => panic!("Bad type returned from apply_diff"),
        }
    }

    fn update(&self, k: String, diffapp: DiffApplicationResult<MultiValue>) -> StateTree {
        let new_index = match diffapp.index_updates() {
            Some(u) => u.union(self.object_index.clone()),
            None => self.object_index.clone(),
        };
        StateTree {
            root_map: self.root_map.update(k, diffapp.value().clone()),
            object_index: new_index,
        }
    }

    fn remove(&self, k: &str) -> StateTree {
        let mut new_root = self.root_map.clone();
        new_root.remove(k);
        StateTree {
            root_map: new_root,
            object_index: self.object_index.clone(),
        }
    }

    pub(crate) fn resolve_path(&self, path: &Path) -> Option<ResolvedPath> {
        if path.is_root() {
            return Some(ResolvedPath::Root(ResolvedRoot { root: self.clone() }));
        }
        let mut stack = path.clone().elements();
        stack.reverse();
        if let Some(PathElement::Key(k)) = stack.pop() {
            let mut parent_object_id = amp::ObjectID::Root.clone();
            let mut key_in_container = PathElement::Key(k.clone());
            let first_obj = self.root_map.get(&k);
            if let Some(o) = first_obj {
                let mut focus = Box::new(focus::Focus::new_root(self.clone(), k.clone()));
                let mut current_obj: MultiValue = o.clone();
                while let Some(next_elem) = stack.pop() {
                    match next_elem {
                        PathElement::Key(k) => {
                            key_in_container = PathElement::Key(k.clone());
                            match current_obj.default_statetree_value().unwrap() {
                                StateTreeValue::Internal(StateTreeComposite::Map(map)) => {
                                    if let Some(target) = map.props.get(&k) {
                                        let new_focus = focus::Focus::new_map(
                                            focus.clone(),
                                            map.clone(),
                                            k.clone(),
                                            target.clone(),
                                        );
                                        focus = Box::new(new_focus);
                                        parent_object_id = map.object_id.clone();
                                        current_obj = target.clone();
                                    } else {
                                        return None;
                                    }
                                }
                                StateTreeValue::Internal(StateTreeComposite::Table(table)) => {
                                    if let Some(target) = table.props.get(&k) {
                                        let new_focus = focus::Focus::new_table(
                                            focus.clone(),
                                            table.clone(),
                                            k.clone(),
                                            target.clone(),
                                        );
                                        focus = Box::new(new_focus);
                                        parent_object_id = table.object_id.clone();
                                        current_obj = target.clone();
                                    } else {
                                        return None;
                                    }
                                }
                                _ => return None,
                            }
                        }
                        PathElement::Index(i) => {
                            key_in_container = PathElement::Index(i);
                            match current_obj.default_statetree_value().unwrap() {
                                StateTreeValue::Internal(StateTreeComposite::List(list)) => {
                                    let index = i.try_into().unwrap();
                                    if let Some(target) = list.elements.get(index) {
                                        parent_object_id = list.object_id.clone();
                                        current_obj = target.clone();
                                        let new_focus = focus::Focus::new_list(
                                            focus.clone(),
                                            list,
                                            index,
                                            current_obj.clone(),
                                        );
                                        focus = Box::new(new_focus);
                                    } else {
                                        return None;
                                    }
                                }
                                StateTreeValue::Internal(StateTreeComposite::Text(
                                    StateTreeText { chars, .. },
                                )) => {
                                    if chars.get(i as usize).is_some() {
                                        if stack.is_empty() {
                                            return Some(ResolvedPath::Character(ResolvedChar {
                                                multivalue: current_obj,
                                            }));
                                        } else {
                                            return None;
                                        }
                                    } else {
                                        return None;
                                    };
                                }
                                _ => return None,
                            }
                        }
                    };
                }
                let resolved_path = match current_obj.default_statetree_value().unwrap() {
                    StateTreeValue::Leaf(v) => match v {
                        amp::ScalarValue::Counter(v) => ResolvedPath::Counter(ResolvedCounter {
                            containing_object_id: parent_object_id,
                            key_in_container,
                            current_value: v,
                            multivalue: current_obj,
                            focus,
                        }),
                        _ => ResolvedPath::Primitive(ResolvedPrimitive {
                            multivalue: current_obj,
                        }),
                    },
                    StateTreeValue::Internal(composite) => match composite {
                        StateTreeComposite::Map(m) => ResolvedPath::Map(ResolvedMap {
                            value: m,
                            multivalue: current_obj,
                            focus,
                        }),
                        StateTreeComposite::Table(t) => ResolvedPath::Table(ResolvedTable {
                            value: t,
                            multivalue: current_obj,
                            focus,
                        }),
                        StateTreeComposite::List(l) => ResolvedPath::List(ResolvedList {
                            value: l,
                            multivalue: current_obj,
                            focus,
                        }),
                        StateTreeComposite::Text(t) => ResolvedPath::Text(ResolvedText {
                            multivalue: current_obj,
                            value: t,
                            update: Box::new(move |d| focus.update(d)),
                        }),
                    },
                };
                Some(resolved_path)
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn value(&self) -> Value {
        StateTreeValue::Internal(StateTreeComposite::Map(StateTreeMap {
            object_id: amp::ObjectID::Root,
            props: self.root_map.clone(),
        }))
        .value()
    }
}

/// Applying a diff returns two things
/// - The updated object T (either a StateTreeComposite or a [MultiValue])
/// - The updates to the object ID index
#[derive(Clone)]
pub struct DiffApplicationResult<T>((T, Option<im::HashMap<amp::ObjectID, StateTreeComposite>>));

impl<T> DiffApplicationResult<T>
where
    T: Clone,
{
    fn pure(value: T) -> DiffApplicationResult<T> {
        DiffApplicationResult((value, None))
    }

    fn value(&self) -> &T {
        &self.0 .0
    }

    fn index_updates(&self) -> Option<im::HashMap<amp::ObjectID, StateTreeComposite>> {
        self.0 .1.clone()
    }

    fn map<F, G>(self, f: F) -> DiffApplicationResult<G>
    where
        F: Fn(T) -> G,
        G: Clone,
    {
        DiffApplicationResult((f(self.0 .0), self.0 .1))
    }

    fn and_then<F, G>(self, f: F) -> DiffApplicationResult<G>
    where
        F: Fn(T) -> DiffApplicationResult<G>,
        G: Clone,
    {
        let diff = f(self.0 .0.clone());
        self.with_updates(diff.0 .1.clone())
            .map(|_| diff.0 .0.clone())
    }

    fn with_updates(
        self,
        updates: Option<im::HashMap<amp::ObjectID, StateTreeComposite>>,
    ) -> DiffApplicationResult<T> {
        match (updates, self.0 .1) {
            (Some(updates), Some(existing_updates)) => {
                DiffApplicationResult((self.0 .0, Some(updates.union(existing_updates))))
            }
            (Some(updates), None) => DiffApplicationResult((self.0 .0, Some(updates))),
            (None, Some(existing_updates)) => {
                DiffApplicationResult((self.0 .0, Some(existing_updates)))
            }
            (None, None) => DiffApplicationResult((self.0 .0, None)),
        }
    }
}

struct NewValue<T> {
    value: T,
    ops: Vec<amp::Op>,
    index_updates: im::HashMap<amp::ObjectID, StateTreeComposite>,
}

impl<T> NewValue<T> {
    fn diffapp(&self) -> DiffApplicationResult<T>
    where
        T: Clone,
    {
        DiffApplicationResult::pure(self.value.clone())
            .with_updates(Some(self.index_updates.clone()))
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

/// A set of conflicting values for the same key, indexed by OpID
// TODO make the type system aware that this always has at least one value
#[derive(Debug, Clone)]
struct MultiValue(im::HashMap<amp::OpID, StateTreeValue>);

impl MultiValue {
    fn new() -> MultiValue {
        MultiValue(im::HashMap::new())
    }

    fn new_from_statetree_value(value: StateTreeValue) -> MultiValue {
        MultiValue(hashmap! {random_op_id() => value})
    }

    fn new_from_diff(
        diff: &std::collections::HashMap<amp::OpID, amp::Diff>,
    ) -> Result<DiffApplicationResult<MultiValue>, error::InvalidPatch> {
        let result = MultiValue::new();
        result.apply_diff(diff)
    }

    fn new_from_value(
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
                        MultiValue::new_from_statetree_value(StateTreeValue::Internal(
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
                        MultiValue::new_from_statetree_value(StateTreeValue::Internal(
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
                            StateTreeValue::Internal(StateTreeComposite::List(StateTreeList {
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
                        MultiValue::new_from_statetree_value(StateTreeValue::Internal(
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

    fn apply_diff(
        &self,
        diff: &std::collections::HashMap<amp::OpID, amp::Diff>,
    ) -> Result<DiffApplicationResult<MultiValue>, error::InvalidPatch> {
        let mut result = DiffApplicationResult::pure(self.0.clone());
        for (opid, subdiff) in diff.iter() {
            let application_result = if let Some(existing_value) = self.0.get(opid) {
                match existing_value {
                    StateTreeValue::Leaf(_) => StateTreeValue::new_from_diff(subdiff),
                    StateTreeValue::Internal(composite) => {
                        Ok(composite.apply_diff(subdiff)?.map(StateTreeValue::Internal))
                    }
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

    fn default_statetree_value(&self) -> Option<StateTreeValue> {
        self.0.get(&self.default_opid()).cloned()
    }

    fn default_value(&self) -> Option<Value> {
        self.default_statetree_value().map(|sv| sv.value())
    }

    fn default_opid(&self) -> amp::OpID {
        let mut opids: Vec<&amp::OpID> = self.0.keys().collect();
        opids.sort();
        opids.reverse();
        opids.first().cloned().unwrap().clone()
    }

    fn ordered_statetree_values(&self) -> Vec<(amp::OpID, StateTreeValue)> {
        let mut opids: Vec<amp::OpID> = self.0.keys().cloned().collect();
        opids.sort();
        opids.reverse();
        opids
            .iter()
            .map(|oid| (oid.clone(), self.0.get(oid).unwrap().clone()))
            .collect()
    }

    fn update_default(&self, val: StateTreeValue) -> MultiValue {
        MultiValue(self.0.update(self.default_opid(), val))
    }
}

impl Register for MultiValue {
    fn values(&self) -> std::collections::HashMap<amp::OpID, Value> {
        self.ordered_statetree_values()
            .into_iter()
            .map(|(oid, value)| (oid, value.value()))
            .collect()
    }

    fn default_value(&self) -> Value {
        self.default_value().unwrap()
    }
}

/// This struct exists to constrain the values of a text type to just containing
/// sequences of chars
#[derive(Debug, Clone)]
struct MultiChar(im::HashMap<amp::OpID, char>);

impl MultiChar {
    fn new() -> MultiChar {
        MultiChar(im::HashMap::new())
    }

    fn new_from_char(c: char) -> MultiChar {
        MultiChar(im::HashMap::new().update(random_op_id(), c))
    }

    fn apply_diff(
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

    fn default_char(&self) -> Option<char> {
        let mut opids: Vec<&amp::OpID> = self.0.keys().collect();
        opids.sort();
        opids.reverse();
        opids.first().map(|oid| *self.0.get(*oid).unwrap())
    }
}

impl Register for MultiChar {
    fn values(&self) -> std::collections::HashMap<amp::OpID, Value> {
        let mut opids: Vec<amp::OpID> = self.0.keys().cloned().collect();
        opids.sort();
        opids.reverse();
        opids
            .into_iter()
            .map(|oid| {
                (
                    oid.clone(),
                    Value::Primitive(amp::ScalarValue::Str(
                        self.0.get(&oid).unwrap().clone().into(),
                    )),
                )
            })
            .collect()
    }

    fn default_value(&self) -> Value {
        Value::Primitive(amp::ScalarValue::Str(self.default_char().unwrap().into()))
    }
}

/// A node in the state tree is either a leaf node containing a scalarvalue,
/// or an internal composite type
#[derive(Debug, Clone)]
enum StateTreeValue {
    Leaf(amp::ScalarValue),
    Internal(StateTreeComposite),
}

impl StateTreeValue {
    fn new_from_diff(
        diff: &amp::Diff,
    ) -> Result<DiffApplicationResult<StateTreeValue>, error::InvalidPatch> {
        match diff {
            amp::Diff::Value(v) => Ok(DiffApplicationResult::pure(StateTreeValue::Leaf(v.clone()))),
            amp::Diff::Map(amp::MapDiff {
                object_id,
                obj_type,
                props,
                ..
            }) => {
                let prop_diffs_vec: Result<Vec<(String, DiffApplicationResult<MultiValue>)>, error::InvalidPatch> = props.iter().map::<Result<(String, DiffApplicationResult<MultiValue>), error::InvalidPatch>, _>(|(prop,  prop_diff)| {
                    let conflicts = MultiValue::new_from_diff(prop_diff)?;
                    Ok((prop.clone(), conflicts))
                }).collect();
                let prop_diff_results: im::HashMap<String, DiffApplicationResult<MultiValue>> =
                    prop_diffs_vec?.into();
                let prop_diffs: im::HashMap<String, MultiValue> = prop_diff_results
                    .iter()
                    .map(|(k, v)| (k.clone(), v.value().clone()))
                    .collect();
                let composite = match obj_type {
                    amp::MapType::Map => StateTreeComposite::Map(StateTreeMap {
                        object_id: object_id.clone(),
                        props: prop_diffs,
                    }),
                    amp::MapType::Table => StateTreeComposite::Table(StateTreeTable {
                        object_id: object_id.clone(),
                        props: prop_diffs,
                    }),
                };
                let prop_updates: im::HashMap<amp::ObjectID, StateTreeComposite> =
                    prop_diff_results.values().fold(
                        im::HashMap::new().update(object_id.clone(), composite.clone()),
                        |acc, conflict_result| match (acc, conflict_result.index_updates()) {
                            (acc, Some(update)) => update.union(acc),
                            (acc, None) => acc,
                        },
                    );
                let value = StateTreeValue::Internal(composite);
                Ok(DiffApplicationResult::pure(value).with_updates(Some(prop_updates)))
            }
            //TODO the following code is copied from StateTreeComposite::apply_diff, there should
            //be a way to abstract this behaviour
            amp::Diff::Seq(amp::SeqDiff {
                object_id,
                obj_type,
                edits,
                props,
                ..
            }) => match obj_type {
                amp::SequenceType::Text => {
                    let mut char_conflicts = im::Vector::new();
                    for edit in edits.iter() {
                        match edit {
                            amp::DiffEdit::Remove { index } => {
                                char_conflicts.remove(*index);
                            }
                            amp::DiffEdit::Insert { index } => {
                                char_conflicts.insert(*index, MultiChar::new());
                            }
                        };
                    }
                    for (index, prop_diff) in props {
                        if let Some(char) = char_conflicts.get(*index) {
                            let new_char = char.apply_diff(object_id, prop_diff)?;
                            char_conflicts = char_conflicts.update(*index, new_char)
                        } else {
                            return Err(error::InvalidPatch::InvalidIndex {
                                object_id: object_id.clone(),
                                index: *index,
                            });
                        }
                    }
                    let composite = StateTreeComposite::Text(StateTreeText {
                        object_id: object_id.clone(),
                        chars: char_conflicts,
                    });
                    let value = StateTreeValue::Internal(composite.clone());
                    let updates = im::HashMap::new().update(object_id.clone(), composite);
                    Ok(DiffApplicationResult::pure(value).with_updates(Some(updates)))
                }
                amp::SequenceType::List => {
                    let mut new_elements = im::Vector::new();
                    for edit in edits.iter() {
                        match edit {
                            amp::DiffEdit::Remove { index } => {
                                new_elements.remove(*index);
                            }
                            amp::DiffEdit::Insert { index } => {
                                if (*index) == new_elements.len() {
                                    new_elements.push_back(MultiValue::new())
                                } else {
                                    new_elements.insert(*index, MultiValue::new())
                                }
                            }
                        };
                    }
                    let mut object_index_updates = im::HashMap::new();
                    for (index, prop_diff) in props {
                        if let Some(node) = new_elements.get(*index) {
                            let node_result = node.apply_diff(prop_diff)?;
                            if let Some(updates) = node_result.index_updates() {
                                object_index_updates = object_index_updates.union(updates);
                            }
                            new_elements = new_elements.update(*index, node_result.value().clone())
                        } else {
                            return Err(error::InvalidPatch::InvalidIndex {
                                object_id: object_id.clone(),
                                index: *index,
                            });
                        }
                    }
                    let composite = StateTreeComposite::List(StateTreeList {
                        object_id: object_id.clone(),
                        elements: new_elements,
                    });
                    object_index_updates =
                        object_index_updates.update(object_id.clone(), composite.clone());
                    Ok(
                        DiffApplicationResult::pure(StateTreeValue::Internal(composite))
                            .with_updates(Some(object_index_updates)),
                    )
                }
            },
            amp::Diff::Unchanged(..) => Err(error::InvalidPatch::UnchangedDiffForNonExistentObject),
        }
    }

    fn value(&self) -> Value {
        match self {
            StateTreeValue::Leaf(p) => p.into(),
            StateTreeValue::Internal(composite) => composite.value(),
        }
    }
}

#[derive(Debug, Clone)]
struct StateTreeMap {
    object_id: amp::ObjectID,
    props: im::HashMap<String, MultiValue>,
}

impl StateTreeMap {
    fn update(&self, key: String, value: MultiValue) -> StateTreeMap {
        StateTreeMap {
            object_id: self.object_id.clone(),
            props: self.props.update(key, value),
        }
    }

    fn without(&self, key: &str) -> StateTreeMap {
        StateTreeMap {
            object_id: self.object_id.clone(),
            props: self.props.without(key),
        }
    }
}

#[derive(Debug, Clone)]
struct StateTreeTable {
    object_id: amp::ObjectID,
    props: im::HashMap<String, MultiValue>,
}

impl StateTreeTable {
    fn update(&self, key: String, value: MultiValue) -> StateTreeTable {
        StateTreeTable {
            object_id: self.object_id.clone(),
            props: self.props.update(key, value),
        }
    }

    fn without(&self, key: &str) -> StateTreeTable {
        StateTreeTable {
            object_id: self.object_id.clone(),
            props: self.props.without(key),
        }
    }
}

#[derive(Debug, Clone)]
struct StateTreeText {
    object_id: amp::ObjectID,
    chars: im::Vector<MultiChar>,
}
#[derive(Debug, Clone)]
struct StateTreeList {
    object_id: amp::ObjectID,
    elements: im::Vector<MultiValue>,
}

impl StateTreeList {
    fn remove(&self, index: usize) -> StateTreeList {
        let mut new_elems = self.elements.clone();
        new_elems.remove(index);
        StateTreeList {
            object_id: self.object_id.clone(),
            elements: new_elems,
        }
    }

    fn set(&self, index: usize, value: MultiValue) -> StateTreeList {
        StateTreeList {
            object_id: self.object_id.clone(),
            elements: self.elements.update(index, value),
        }
    }

    fn insert(&self, index: usize, value: MultiValue) -> StateTreeList {
        let mut new_elems = self.elements.clone();
        new_elems.insert(index, value);
        StateTreeList {
            object_id: self.object_id.clone(),
            elements: new_elems,
        }
    }
}

#[derive(Debug, Clone)]
enum StateTreeComposite {
    Map(StateTreeMap),
    Table(StateTreeTable),
    Text(StateTreeText),
    List(StateTreeList),
}

impl StateTreeComposite {
    fn apply_diff(
        &self,
        diff: &amp::Diff,
    ) -> Result<DiffApplicationResult<StateTreeComposite>, error::InvalidPatch> {
        if diff_object_id(diff) != Some(self.object_id()) {
            return Err(error::InvalidPatch::MismatchingObjectIDs {
                patch_expected_id: diff_object_id(diff),
                actual_id: self.object_id(),
            });
        };
        match diff {
            amp::Diff::Map(amp::MapDiff {
                obj_type,
                props: prop_diffs,
                ..
            }) => match self {
                StateTreeComposite::Map(StateTreeMap {
                    object_id: this_obj_id,
                    props: current_props,
                }) => {
                    if *obj_type != amp::MapType::Map {
                        Err(error::InvalidPatch::MismatchingObjectType {
                            object_id: this_obj_id.clone(),
                            patch_expected_type: Some(amp::ObjType::Map(*obj_type)),
                            actual_type: Some(self.obj_type()),
                        })
                    } else {
                        let mut new_props = current_props.clone();
                        let mut object_index_updates = im::HashMap::new();
                        for (prop, prop_diff) in prop_diffs.iter() {
                            if prop_diff.is_empty() {
                                new_props = new_props.without(prop)
                            } else {
                                let node =
                                    new_props.get(prop).cloned().unwrap_or_else(MultiValue::new);
                                let node_result = node.apply_diff(prop_diff)?;
                                if let Some(updates) = node_result.index_updates() {
                                    object_index_updates = updates.union(object_index_updates);
                                }
                                new_props =
                                    new_props.update(prop.clone(), node_result.value().clone())
                            }
                        }
                        let new_map = StateTreeComposite::Map(StateTreeMap {
                            object_id: this_obj_id.clone(),
                            props: new_props,
                        });
                        object_index_updates =
                            object_index_updates.update(this_obj_id.clone(), new_map.clone());
                        Ok(DiffApplicationResult::pure(new_map)
                            .with_updates(Some(object_index_updates)))
                    }
                }
                StateTreeComposite::Table(StateTreeTable {
                    object_id: this_obj_id,
                    props: current_props,
                }) => {
                    if *obj_type != amp::MapType::Table {
                        Err(error::InvalidPatch::MismatchingObjectType {
                            object_id: this_obj_id.clone(),
                            patch_expected_type: Some(amp::ObjType::Map(*obj_type)),
                            actual_type: Some(self.obj_type()),
                        })
                    } else {
                        let mut new_props = current_props.clone();
                        let mut object_index_updates = im::HashMap::new();
                        for (prop, prop_diff) in prop_diffs.iter() {
                            if prop_diffs.is_empty() {
                                new_props = new_props.without(prop)
                            } else {
                                let node =
                                    new_props.get(prop).cloned().unwrap_or_else(MultiValue::new);
                                let node_result = node.apply_diff(prop_diff)?;
                                new_props =
                                    new_props.update(prop.clone(), node_result.value().clone());
                                if let Some(update) = node_result.index_updates() {
                                    object_index_updates = update.union(object_index_updates);
                                }
                            }
                        }
                        let composite = StateTreeComposite::Table(StateTreeTable {
                            object_id: this_obj_id.clone(),
                            props: new_props,
                        });
                        object_index_updates =
                            object_index_updates.update(this_obj_id.clone(), composite.clone());
                        Ok(DiffApplicationResult::pure(composite)
                            .with_updates(Some(object_index_updates)))
                    }
                }
                _ => Err(error::InvalidPatch::MismatchingObjectType {
                    object_id: self.object_id(),
                    patch_expected_type: diff_object_type(diff),
                    actual_type: Some(self.obj_type()),
                }),
            },
            amp::Diff::Seq(amp::SeqDiff {
                edits,
                props: new_props,
                obj_type,
                ..
            }) => {
                println!("Diff: {:?}", diff);
                println!("Self: {:?}", self);
                match self {
                    StateTreeComposite::List(StateTreeList {
                        object_id: this_obj_id,
                        elements: current_elements,
                    }) => {
                        println!("Applying list diff");
                        if *obj_type != amp::SequenceType::List {
                            Err(error::InvalidPatch::MismatchingObjectType {
                                object_id: this_obj_id.clone(),
                                patch_expected_type: Some(amp::ObjType::Sequence(*obj_type)),
                                actual_type: Some(self.obj_type()),
                            })
                        } else {
                            let mut new_elements = current_elements.clone();
                            for edit in edits.iter() {
                                match edit {
                                    amp::DiffEdit::Remove { index } => {
                                        new_elements.remove(*index);
                                    }
                                    amp::DiffEdit::Insert { index } => {
                                        if (*index) == new_elements.len() {
                                            new_elements.push_back(MultiValue::new())
                                        } else {
                                            new_elements.insert(*index, MultiValue::new())
                                        }
                                    }
                                };
                            }
                            let mut object_index_updates = im::HashMap::new();
                            for (index, prop_diff) in new_props {
                                if let Some(node) = new_elements.get(*index) {
                                    let node_result = node.apply_diff(prop_diff)?;
                                    new_elements =
                                        new_elements.update(*index, node_result.value().clone());
                                    if let Some(update) = node_result.index_updates() {
                                        object_index_updates = update.union(object_index_updates);
                                    }
                                } else {
                                    return Err(error::InvalidPatch::InvalidIndex {
                                        object_id: this_obj_id.clone(),
                                        index: *index,
                                    });
                                }
                            }
                            let composite = StateTreeComposite::List(StateTreeList {
                                object_id: this_obj_id.clone(),
                                elements: new_elements,
                            });
                            object_index_updates =
                                object_index_updates.update(this_obj_id.clone(), composite.clone());
                            Ok(DiffApplicationResult::pure(composite)
                                .with_updates(Some(object_index_updates)))
                        }
                    }
                    StateTreeComposite::Text(StateTreeText {
                        object_id: this_obj_id,
                        chars: current_chars,
                    }) => {
                        if *obj_type != amp::SequenceType::Text {
                            Err(error::InvalidPatch::MismatchingObjectType {
                                object_id: this_obj_id.clone(),
                                patch_expected_type: Some(amp::ObjType::Sequence(*obj_type)),
                                actual_type: Some(self.obj_type()),
                            })
                        } else {
                            let mut new_chars = current_chars.clone();
                            for edit in edits.iter() {
                                match edit {
                                    amp::DiffEdit::Remove { index } => {
                                        new_chars.remove(*index);
                                    }
                                    amp::DiffEdit::Insert { index } => {
                                        new_chars.insert(*index, MultiChar::new());
                                    }
                                };
                            }
                            for (index, prop_diff) in new_props {
                                if let Some(char) = new_chars.get(*index) {
                                    let new_char = char.apply_diff(this_obj_id, prop_diff)?;
                                    new_chars = new_chars.update(*index, new_char)
                                } else {
                                    return Err(error::InvalidPatch::InvalidIndex {
                                        object_id: this_obj_id.clone(),
                                        index: *index,
                                    });
                                }
                            }
                            let composite = StateTreeComposite::Text(StateTreeText {
                                object_id: this_obj_id.clone(),
                                chars: new_chars,
                            });
                            let object_index_updates =
                                im::HashMap::new().update(this_obj_id.clone(), composite.clone());
                            Ok(DiffApplicationResult::pure(composite)
                                .with_updates(Some(object_index_updates)))
                        }
                    }
                    _ => Err(error::InvalidPatch::MismatchingObjectType {
                        object_id: self.object_id(),
                        patch_expected_type: diff_object_type(diff),
                        actual_type: Some(self.obj_type()),
                    }),
                }
            }
            amp::Diff::Unchanged(..) => Ok(DiffApplicationResult::pure(self.clone())),
            amp::Diff::Value(..) => {
                panic!("SHould never be called")
            }
        }
    }

    fn obj_type(&self) -> amp::ObjType {
        match self {
            Self::Map(..) => amp::ObjType::map(),
            Self::Table(..) => amp::ObjType::table(),
            Self::Text(..) => amp::ObjType::text(),
            Self::List(..) => amp::ObjType::list(),
        }
    }

    fn object_id(&self) -> amp::ObjectID {
        match self {
            Self::Map(StateTreeMap { object_id, .. }) => object_id.clone(),
            Self::Table(StateTreeTable { object_id, .. }) => object_id.clone(),
            Self::Text(StateTreeText { object_id, .. }) => object_id.clone(),
            Self::List(StateTreeList { object_id, .. }) => object_id.clone(),
        }
    }

    fn value(&self) -> Value {
        match self {
            Self::Map(StateTreeMap { props, .. }) => Value::Map(
                props
                    .iter()
                    .filter_map(|(k, v)| v.default_value().map(|v| (k.clone(), v)))
                    .collect(),
                amp::MapType::Map,
            ),
            Self::Table(StateTreeTable { props, .. }) => Value::Map(
                props
                    .iter()
                    .filter_map(|(k, v)| v.default_value().map(|v| (k.clone(), v)))
                    .collect(),
                amp::MapType::Table,
            ),
            Self::List(StateTreeList {
                elements: elems, ..
            }) => Value::Sequence(elems.iter().filter_map(|e| e.default_value()).collect()),
            Self::Text(StateTreeText { chars, .. }) => {
                Value::Text(chars.iter().filter_map(|c| c.default_char()).collect())
            }
        }
    }
}

/// Helper method to get the object type of an amp::Diff
fn diff_object_type(diff: &amp::Diff) -> Option<amp::ObjType> {
    match diff {
        amp::Diff::Map(mapdiff) => Some(amp::ObjType::Map(mapdiff.obj_type)),
        amp::Diff::Seq(seqdiff) => Some(amp::ObjType::Sequence(seqdiff.obj_type)),
        amp::Diff::Unchanged(udiff) => Some(udiff.obj_type),
        amp::Diff::Value(..) => None,
    }
}

/// Helper method to get the object ID of an amp::Diff
fn diff_object_id(diff: &amp::Diff) -> Option<amp::ObjectID> {
    match diff {
        amp::Diff::Map(mapdiff) => Some(mapdiff.object_id.clone()),
        amp::Diff::Seq(seqdiff) => Some(seqdiff.object_id.clone()),
        amp::Diff::Unchanged(udiff) => Some(udiff.object_id.clone()),
        amp::Diff::Value(..) => None,
    }
}

fn new_object_id() -> amp::ObjectID {
    amp::ObjectID::ID(random_op_id())
}

pub fn random_op_id() -> amp::OpID {
    amp::OpID::new(1, &amp::ActorID::random())
}

fn value_to_datatype(value: &amp::ScalarValue) -> amp::DataType {
    match value {
        amp::ScalarValue::Counter(_) => amp::DataType::Counter,
        amp::ScalarValue::Timestamp(_) => amp::DataType::Timestamp,
        _ => amp::DataType::Undefined,
    }
}

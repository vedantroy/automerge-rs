use crate::error;
use crate::Value;
use crate::{Path, PathElement};
use automerge_protocol as amp;
use std::collections::HashMap;
use std::convert::TryInto;

mod focus;
mod multivalue;
mod resolved_path;
use multivalue::{MultiChar, MultiValue};
pub use resolved_path::ResolvedPath;
use resolved_path::{
    ResolvedChar, ResolvedCounter, ResolvedList, ResolvedMap, ResolvedPrimitive, ResolvedRoot,
    ResolvedTable, ResolvedText,
};

pub(crate) struct LocalStateChange {
    pub new_state: StateTree,
    pub new_ops: Vec<amp::Op>,
}

#[derive(Debug, Clone)]
pub(crate) struct StateTree {
    root_map: im::HashMap<String, MultiValue>,
    object_index: im::HashMap<amp::ObjectID, StateTreeComposite>,
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

    pub(crate) fn resolve_path(&self, path: &Path) -> Option<resolved_path::ResolvedPath> {
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
                                StateTreeValue::Composite(StateTreeComposite::Map(map)) => {
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
                                StateTreeValue::Composite(StateTreeComposite::Table(table)) => {
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
                                StateTreeValue::Composite(StateTreeComposite::List(list)) => {
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
                                StateTreeValue::Composite(StateTreeComposite::Text(
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
                    StateTreeValue::Composite(composite) => match composite {
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
        StateTreeValue::Composite(StateTreeComposite::Map(StateTreeMap {
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

/// A node in the state tree is either a leaf node containing a scalarvalue,
/// or an internal composite type (e.g a Map or a List)
#[derive(Debug, Clone)]
enum StateTreeValue {
    Leaf(amp::ScalarValue),
    Composite(StateTreeComposite),
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
                ..
            }) => match obj_type {
                amp::MapType::Map => StateTreeComposite::Map(StateTreeMap {
                    object_id: object_id.clone(),
                    props: im::HashMap::new(),
                }),
                amp::MapType::Table => StateTreeComposite::Table(StateTreeTable {
                    object_id: object_id.clone(),
                    props: im::HashMap::new(),
                }),
            }
            .apply_diff(diff)
            .map(|d| d.map(StateTreeValue::Composite)),
            amp::Diff::Seq(amp::SeqDiff {
                object_id,
                obj_type,
                ..
            }) => match obj_type {
                amp::SequenceType::Text => StateTreeComposite::Text(StateTreeText {
                    object_id: object_id.clone(),
                    chars: im::Vector::new(),
                }),
                amp::SequenceType::List => StateTreeComposite::List(StateTreeList {
                    object_id: object_id.clone(),
                    elements: im::Vector::new(),
                }),
            }
            .apply_diff(diff)
            .map(|d| d.map(StateTreeValue::Composite)),
            amp::Diff::Unchanged(..) => Err(error::InvalidPatch::UnchangedDiffForNonExistentObject),
        }
    }

    fn value(&self) -> Value {
        match self {
            StateTreeValue::Leaf(p) => p.into(),
            StateTreeValue::Composite(composite) => composite.value(),
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

    fn apply_diff(
        &self,
        prop_diffs: &HashMap<String, HashMap<amp::OpID, amp::Diff>>,
    ) -> Result<DiffApplicationResult<StateTreeMap>, error::InvalidPatch> {
        let mut new_props = self.props.clone();
        let mut object_index_updates = im::HashMap::new();
        for (prop, prop_diff) in prop_diffs.iter() {
            if prop_diff.is_empty() {
                new_props = new_props.without(prop)
            } else {
                let node = new_props.get(prop).cloned().unwrap_or_else(MultiValue::new);
                let node_result = node.apply_diff(prop_diff)?;
                if let Some(updates) = node_result.index_updates() {
                    object_index_updates = updates.union(object_index_updates);
                }
                new_props = new_props.update(prop.clone(), node_result.value().clone())
            }
        }
        let new_map = StateTreeMap {
            object_id: self.object_id.clone(),
            props: new_props,
        };
        object_index_updates = object_index_updates.update(
            self.object_id.clone(),
            StateTreeComposite::Map(new_map.clone()),
        );
        Ok(DiffApplicationResult::pure(new_map).with_updates(Some(object_index_updates)))
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

    fn apply_diff(
        &self,
        prop_diffs: &HashMap<String, HashMap<amp::OpID, amp::Diff>>,
    ) -> Result<DiffApplicationResult<StateTreeTable>, error::InvalidPatch> {
        let mut new_props = self.props.clone();
        let mut object_index_updates = im::HashMap::new();
        for (prop, prop_diff) in prop_diffs.iter() {
            if prop_diff.is_empty() {
                new_props = new_props.without(prop)
            } else {
                let node = new_props.get(prop).cloned().unwrap_or_else(MultiValue::new);
                let node_result = node.apply_diff(prop_diff)?;
                if let Some(updates) = node_result.index_updates() {
                    object_index_updates = updates.union(object_index_updates);
                }
                new_props = new_props.update(prop.clone(), node_result.value().clone())
            }
        }
        let new_table = StateTreeTable {
            object_id: self.object_id.clone(),
            props: new_props,
        };
        object_index_updates = object_index_updates.update(
            self.object_id.clone(),
            StateTreeComposite::Table(new_table.clone()),
        );
        Ok(DiffApplicationResult::pure(new_table).with_updates(Some(object_index_updates)))
    }
}

#[derive(Debug, Clone)]
struct StateTreeText {
    object_id: amp::ObjectID,
    chars: im::Vector<MultiChar>,
}

impl StateTreeText {
    fn apply_diff(
        &self,
        edits: &[amp::DiffEdit],
        props: &HashMap<usize, HashMap<amp::OpID, amp::Diff>>,
    ) -> Result<DiffApplicationResult<StateTreeText>, error::InvalidPatch> {
        let mut new_chars = self.chars.clone();
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
        for (index, prop_diff) in props {
            if let Some(char) = new_chars.get(*index) {
                let new_char = char.apply_diff(&self.object_id, prop_diff)?;
                new_chars = new_chars.update(*index, new_char)
            } else {
                return Err(error::InvalidPatch::InvalidIndex {
                    object_id: self.object_id.clone(),
                    index: *index,
                });
            }
        }
        let text = StateTreeText {
            object_id: self.object_id.clone(),
            chars: new_chars,
        };
        let object_index_updates = im::HashMap::new().update(
            self.object_id.clone(),
            StateTreeComposite::Text(text.clone()),
        );
        Ok(DiffApplicationResult::pure(text).with_updates(Some(object_index_updates)))
    }
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

    fn apply_diff(
        &self,
        edits: &[amp::DiffEdit],
        new_props: &HashMap<usize, HashMap<amp::OpID, amp::Diff>>,
    ) -> Result<DiffApplicationResult<StateTreeList>, error::InvalidPatch> {
        let mut new_elements = self.elements.clone();
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
                new_elements = new_elements.update(*index, node_result.value().clone());
                if let Some(update) = node_result.index_updates() {
                    object_index_updates = update.union(object_index_updates);
                }
            } else {
                return Err(error::InvalidPatch::InvalidIndex {
                    object_id: self.object_id.clone(),
                    index: *index,
                });
            }
        }
        let composite = StateTreeList {
            object_id: self.object_id.clone(),
            elements: new_elements,
        };
        object_index_updates = object_index_updates.update(
            self.object_id.clone(),
            StateTreeComposite::List(composite.clone()),
        );
        Ok(DiffApplicationResult::pure(composite).with_updates(Some(object_index_updates)))
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
                StateTreeComposite::Map(map) => {
                    if *obj_type != amp::MapType::Map {
                        Err(error::InvalidPatch::MismatchingObjectType {
                            object_id: map.object_id.clone(),
                            patch_expected_type: Some(amp::ObjType::Map(*obj_type)),
                            actual_type: Some(self.obj_type()),
                        })
                    } else {
                        map.apply_diff(prop_diffs)
                            .map(|d| d.map(StateTreeComposite::Map))
                    }
                }
                StateTreeComposite::Table(table) => {
                    if *obj_type != amp::MapType::Table {
                        Err(error::InvalidPatch::MismatchingObjectType {
                            object_id: table.object_id.clone(),
                            patch_expected_type: Some(amp::ObjType::Map(*obj_type)),
                            actual_type: Some(self.obj_type()),
                        })
                    } else {
                        table
                            .apply_diff(prop_diffs)
                            .map(|d| d.map(StateTreeComposite::Table))
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
            }) => match self {
                StateTreeComposite::List(list) => {
                    if *obj_type != amp::SequenceType::List {
                        Err(error::InvalidPatch::MismatchingObjectType {
                            object_id: list.object_id.clone(),
                            patch_expected_type: Some(amp::ObjType::Sequence(*obj_type)),
                            actual_type: Some(self.obj_type()),
                        })
                    } else {
                        list.apply_diff(edits, new_props)
                            .map(|d| d.map(StateTreeComposite::List))
                    }
                }
                StateTreeComposite::Text(text) => {
                    if *obj_type != amp::SequenceType::Text {
                        Err(error::InvalidPatch::MismatchingObjectType {
                            object_id: text.object_id.clone(),
                            patch_expected_type: Some(amp::ObjType::Sequence(*obj_type)),
                            actual_type: Some(self.obj_type()),
                        })
                    } else {
                        text.apply_diff(edits, new_props)
                            .map(|d| d.map(StateTreeComposite::Text))
                    }
                }
                _ => Err(error::InvalidPatch::MismatchingObjectType {
                    object_id: self.object_id(),
                    patch_expected_type: diff_object_type(diff),
                    actual_type: Some(self.obj_type()),
                }),
            },
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

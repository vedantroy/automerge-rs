use crate::error;
use crate::Value;
use crate::{Path, PathElement};
/// The state of the frontend is more complicated than just the current resolved
/// value. We need to tag every value in the state with the OpID that created it.
/// and maintain paralell trees of conflicting values.
use automerge_protocol as amp;
use std::convert::TryInto;
use im::hashmap;

mod focus;

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

pub enum MutationTarget {
    Map(MapTarget),
    Table(TableTarget),
    List(ListTarget),
    Text(TextTarget),
    Character(CharTarget),
    Counter(CounterTarget),
    Primitive(PrimitiveTarget),
}

pub struct CounterTarget {
    current_value: i64,
    multivalue: MultiValue,
    update: Box<dyn Fn(DiffApplicationResult<MultiValue>) -> StateTree>,
    delete: Box<dyn Fn() -> StateTree>,
}

impl CounterTarget {
    pub fn increment(&self, by: i64) -> StateTree {
        let diffapp = DiffApplicationResult::pure(self.multivalue.update_default(StateTreeValue::Leaf(amp::ScalarValue::Counter(self.current_value + by))));
        (self.update)(diffapp)
    }

    pub fn delete(&self) -> StateTree {
        (self.delete)()
    }
}

pub struct MapTarget {
    value: StateTreeMap,
    multivalue: MultiValue,
    update: Box<dyn Fn(DiffApplicationResult<MultiValue>) -> StateTree>,
    delete: Box<dyn Fn() -> StateTree>,
}

impl MapTarget {
    pub fn replace(&self, value: &Value) -> StateTree {
        (self.update)(MultiValue::new_from_value(value))
    }

    pub fn set_key(&self, key: &str, value: &Value) -> StateTree {
        let diffapp = MultiValue::new_from_value(value).and_then(|v| {
            let new_value = self.value.update(key.to_string(), v);
            let new_composite = StateTreeComposite::Map(new_value);
            let new_mv = self
                .multivalue
                .update_default(StateTreeValue::Internal(new_composite.clone()));
            DiffApplicationResult::pure(new_mv).with_updates(Some(
                im::HashMap::new().update(self.value.object_id.clone(), new_composite),
            ))
        });
        (self.update)(diffapp)
    }

    pub fn delete(&self) -> StateTree {
        (self.delete)()
    }
}

pub struct TableTarget {
    value: StateTreeTable,
    multivalue: MultiValue,
    update: Box<dyn Fn(DiffApplicationResult<MultiValue>) -> StateTree>,
    delete: Box<dyn Fn() -> StateTree>,
}

impl TableTarget {
    pub fn replace(&self, value: &Value) -> StateTree {
        (self.update)(MultiValue::new_from_value(value))
    }

    pub fn set_key(&self, key: &str, value: &Value) -> StateTree {
        let diffapp = MultiValue::new_from_value(value).and_then(|v| {
            let new_value = self.value.update(key.to_string(), v);
            let new_composite = StateTreeComposite::Table(new_value);
            let new_mv = self
                .multivalue
                .update_default(StateTreeValue::Internal(new_composite.clone()));
            DiffApplicationResult::pure(new_mv).with_updates(Some(
                hashmap!(self.value.object_id.clone() => new_composite)
            ))
        });
        (self.update)(diffapp)
    }

    pub fn delete(&self) -> StateTree {
        (self.delete)()
    }
}

pub struct TextTarget {
    value: StateTreeText,
    multivalue: MultiValue,
    update: Box<dyn Fn(DiffApplicationResult<MultiValue>) -> StateTree>,
    delete: Box<dyn Fn() -> StateTree>,
}

impl TextTarget {
    pub fn insert(&self, index: u32, c: char) -> StateTree {
        let mut new_chars = self.value.chars.clone();
        new_chars.insert(index.try_into().unwrap(), MultiChar::new_from_char(c));
        let updated =
            StateTreeComposite::Text(StateTreeText {
                object_id: self.value.object_id.clone(),
                chars: new_chars,
            });
        let mv = self.multivalue.update_default(
            StateTreeValue::Internal(updated.clone()),
        );
        let diffapp = DiffApplicationResult::pure(mv)
            .with_updates(Some(
                hashmap!(self.value.object_id.clone() => updated.clone()),
            ));
        (self.update)(diffapp)
    }

    pub fn set(&self, index: u32, c: char) -> StateTree {
        let mut new_chars = self.value.chars.clone();
        new_chars.set(index.try_into().unwrap(), MultiChar::new_from_char(c));
        let updated =
            StateTreeComposite::Text(StateTreeText {
                object_id: self.value.object_id.clone(),
                chars: new_chars,
            });
        let mv = self.multivalue.update_default(
            StateTreeValue::Internal(updated.clone()),
        );
        let diffapp = DiffApplicationResult::pure(mv)
            .with_updates(Some(
                hashmap!(self.value.object_id.clone() => updated.clone())
            ));
        (self.update)(diffapp)
    }

    pub fn remove(&self, index: u32) -> StateTree {
        let mut new_chars = self.value.chars.clone();
        new_chars.remove(index.try_into().unwrap());
        let updated =
            StateTreeComposite::Text(StateTreeText {
                object_id: self.value.object_id.clone(),
                chars: new_chars,
            });
        let mv = self.multivalue.update_default(
            StateTreeValue::Internal(updated.clone()),
        );
        let diffapp = DiffApplicationResult::pure(mv)
            .with_updates(Some(
                hashmap!(self.value.object_id.clone() => updated.clone())
            ));
        (self.update)(diffapp)
    }

    pub fn delete(&self) -> StateTree {
        (self.delete)()
    }
}

pub struct ListTarget {
    value: StateTreeList,
    multivalue: MultiValue,
    update: Box<dyn Fn(DiffApplicationResult<MultiValue>) -> StateTree>,
    delete: Box<dyn Fn() -> StateTree>,
}

impl ListTarget {
    pub fn set(&self, index: u32, v: &Value) -> StateTree {
        let diffapp = MultiValue::new_from_value(v).and_then(|v| {
            let new_value = StateTreeComposite::List(self.value.set(index.try_into().unwrap(), v));
            let mv = self.multivalue.update_default(StateTreeValue::Internal(new_value.clone()));
            DiffApplicationResult::pure(mv).with_updates(Some(hashmap!(self.value.object_id.clone() => new_value)))
        });
        (self.update)(diffapp)
    }

    pub fn insert(&self, index: u32, v: &Value) -> StateTree {
        let diffapp = MultiValue::new_from_value(v).and_then(|v| {
            let new_value = StateTreeComposite::List(self.value.insert(index.try_into().unwrap(), v));
            let mv = self.multivalue.update_default(StateTreeValue::Internal(new_value.clone()));
            DiffApplicationResult::pure(mv).with_updates(Some(hashmap!(self.value.object_id.clone() => new_value.clone())))
        });
        (self.update)(diffapp)
    }

    pub fn remove(&self, index: u32) -> StateTree {
        let new_value = StateTreeComposite::List(self.value.remove(index.try_into().unwrap()));
        let mv = self.multivalue.update_default(StateTreeValue::Internal(new_value.clone()));
        let diffapp = DiffApplicationResult::pure(mv).with_updates(Some(hashmap!(self.value.object_id.clone() => new_value.clone())));
        (self.update)(diffapp)
    }

    pub fn delete(&self) -> StateTree {
        (self.delete)()
    }
}


pub struct CharTarget {
    update: Box<dyn Fn(DiffApplicationResult<MultiChar>) -> StateTree>,
    delete: Box<dyn Fn() -> StateTree>,
}

impl CharTarget {
    pub fn set(&self, c: char) -> StateTree {
        (self.update)(DiffApplicationResult::pure(MultiChar::new_from_char(c)))
    }

    pub fn delete(&self) -> StateTree {
        (self.delete)()
    }
}

pub struct PrimitiveTarget {
    update: Box<dyn Fn(DiffApplicationResult<MultiValue>) -> StateTree>,
    delete: Box<dyn Fn() -> StateTree>,
}

impl PrimitiveTarget {
    pub fn set(&self, v: &Value) -> StateTree {
        let diffapp = MultiValue::new_from_value(v);
        (self.update)(diffapp)
    }

    pub fn delete(&self) -> StateTree {
        (self.delete)()
    }
}

impl MutationTarget {
    pub fn delete(&self) -> StateTree {
        match self {
            Self::Map(maptarget) => maptarget.delete(),
            Self::Text(text) => text.delete(),
            Self::Primitive(prim) => prim.delete(),
            Self::Character(chartarget) => chartarget.delete(),
            Self::Table(table) => table.delete(),
            Self::List(list) => list.delete(),
            Self::Counter(counter) => counter.delete()
        }
    }
}

pub struct PathResolution<'a> {
    path: &'a Path,
    pub containing_object_id: amp::ObjectID,
    pub object_id: Option<amp::ObjectID>,
    register: Box<dyn Register>,
    mutation_target: MutationTarget,
}

impl<'a> PathResolution<'a> {
    pub fn default_value(&self) -> Value {
        self.register.default_value()
    }

    pub fn values(&self) -> std::collections::HashMap<amp::OpID, Value> {
        self.register.values()
    }

    pub fn mutations(&self) -> &MutationTarget {
        &self.mutation_target
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
                    return Err(error::InvalidPatch::MismatchingObjectType {
                        object_id: amp::ObjectID::Root,
                        patch_expected_type: Some(amp::ObjType::map()),
                        actual_type: Some(amp::ObjType::Map(*obj_type)),
                    });
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

    pub(crate) fn resolve_path<'b>(&self, path: &'b Path) -> Option<PathResolution<'b>> {
        let mut stack = path.clone().elements();
        stack.reverse();
        if let Some(PathElement::Key(k)) = stack.pop() {
            let mut parent_object_id = amp::ObjectID::Root.clone();
            let first_obj = self.root_map.get(&k);
            if let Some(o) = first_obj {

                let root_for_delete = self.clone();
                let key_for_delete = k.clone();
                let mut delete: Box<dyn Fn() -> StateTree> = Box::new(move || {
                    let mut root_for_delete = root_for_delete.clone();
                    root_for_delete.root_map.remove(&key_for_delete);
                    root_for_delete
                });
                
                let root_for_update = self.clone();
                let key_for_update = k.clone();
                let mut update: Box<dyn Fn(DiffApplicationResult<MultiValue>) -> StateTree> =
                    Box::new(move |diff_result| {
                        root_for_update.update(key_for_update.clone(), diff_result)
                    });
                let mut current_obj: MultiValue = o.clone();
                while let Some(next_elem) = stack.pop() {
                    match next_elem {
                        PathElement::Key(k) => {
                            match current_obj.default_statetree_value().unwrap() {
                                StateTreeValue::Internal(StateTreeComposite::Map(
                                    StateTreeMap {
                                        object_id: oid,
                                        props,
                                    },
                                )) => {
                                    if let Some(target) = props.get(&k) {
                                        let props_for_update = props.clone();
                                        let oid_for_update = oid.clone();
                                        let new_update: Box<
                                            dyn Fn(
                                                DiffApplicationResult<MultiValue>,
                                            )
                                                -> StateTree,
                                        > = Box::new(move |c| {
                                            let diffapp = c.and_then(|v| {
                                                let oid = oid_for_update.clone();
                                                let updated =
                                                    StateTreeComposite::Map(StateTreeMap {
                                                        object_id: oid,
                                                        props: props_for_update.update(k, v),
                                                    });
                                                DiffApplicationResult::pure(
                                                    current_obj.update_default(
                                                        StateTreeValue::Internal(updated),
                                                    ),
                                                )
                                                .with_updates(Some(
                                                    im::HashMap::new().update(oid, updated.clone()),
                                                ))
                                            });
                                            update(diffapp)
                                        });
                                        delete = Box::new(|| {
                                            let new_val = current_obj.update_default(
                                                StateTreeValue::Internal(StateTreeComposite::Map(
                                                    StateTreeMap {
                                                        object_id: oid,
                                                        props: props.without(&k),
                                                    },
                                                )),
                                            );
                                            update(DiffApplicationResult::pure(new_val))
                                        });
                                        update = new_update;
                                        parent_object_id = oid.clone();
                                        current_obj = target.clone();
                                    } else {
                                        return None;
                                    }
                                }
                                StateTreeValue::Internal(StateTreeComposite::Table(
                                    StateTreeTable {
                                        object_id: oid,
                                        props,
                                    },
                                )) => {
                                    if let Some(target) = props.get(&k) {
                                        let new_update: Box<
                                            dyn Fn(
                                                DiffApplicationResult<MultiValue>,
                                            )
                                                -> StateTree,
                                        > = Box::new(|c| {
                                            let diffapp = c.and_then(|v| {
                                                let updated =
                                                    StateTreeComposite::Table(StateTreeTable {
                                                        object_id: oid,
                                                        props: props.update(k, v),
                                                    });
                                                DiffApplicationResult::pure(
                                                    current_obj.update_default(
                                                        StateTreeValue::Internal(updated),
                                                    ),
                                                )
                                                .with_updates(Some(
                                                    im::HashMap::new().update(oid, updated.clone()),
                                                ))
                                            });
                                            update(diffapp)
                                        });
                                        delete = Box::new(|| {
                                            let new_val = current_obj.update_default(
                                                StateTreeValue::Internal(
                                                    StateTreeComposite::Table(StateTreeTable {
                                                        object_id: oid,
                                                        props: props.without(&k),
                                                    }),
                                                ),
                                            );
                                            update(DiffApplicationResult::pure(new_val))
                                        });
                                        update = new_update;
                                        parent_object_id = oid.clone();
                                        current_obj = target.clone();
                                    } else {
                                        return None;
                                    }
                                }
                                _ => return None,
                            }
                        }
                        PathElement::Index(i) => {
                            match current_obj.default_statetree_value().unwrap() {
                                StateTreeValue::Internal(StateTreeComposite::List(
                                    StateTreeList {
                                        object_id: oid,
                                        elements: elems,
                                    },
                                )) => {
                                    if let Some(target) = elems.get(i as usize) {
                                        parent_object_id = oid.clone();
                                        current_obj = target.clone();
                                        let new_update: Box<
                                            dyn Fn(
                                                DiffApplicationResult<MultiValue>,
                                            )
                                                -> StateTree,
                                        > = Box::new(|c| {
                                            let diffapp = c.and_then(|v| {
                                                let updated =
                                                    StateTreeComposite::List(StateTreeList {
                                                        object_id: oid,
                                                        elements: elems
                                                            .update(i.try_into().unwrap(), v),
                                                    });
                                                DiffApplicationResult::pure(
                                                    current_obj.update_default(
                                                        StateTreeValue::Internal(updated),
                                                    ),
                                                )
                                                .with_updates(Some(
                                                    im::HashMap::new().update(oid, updated.clone()),
                                                ))
                                            });
                                            update(diffapp)
                                        });
                                        update = new_update;
                                        delete = Box::new(|| {
                                            let new_elems = elems.clone();
                                            new_elems.remove(i as usize);
                                            let new_val = current_obj.update_default(
                                                StateTreeValue::Internal(StateTreeComposite::List(
                                                    StateTreeList {
                                                        object_id: oid,
                                                        elements: new_elems,
                                                    },
                                                )),
                                            );
                                            update(DiffApplicationResult::pure(new_val))
                                        })
                                    } else {
                                        return None;
                                    }
                                }
                                StateTreeValue::Internal(StateTreeComposite::Text(
                                    StateTreeText {
                                        object_id: oid,
                                        chars,
                                    },
                                )) => {
                                    if let Some(target) = chars.get(i as usize) {
                                        parent_object_id = oid.clone();
                                        if stack.is_empty() {
                                            let new_update: Box<dyn Fn(DiffApplicationResult<MultiChar>) -> StateTree> = Box::new(|d| {
                                                let diffapp = d.and_then(|c| {
                                                    let new_chars = chars.update(i.try_into().unwrap(), c);
                                                    let new_value = StateTreeComposite::Text(StateTreeText{
                                                        object_id: oid,
                                                        chars: new_chars,
                                                    });
                                                    let mv = current_obj.update_default(StateTreeValue::Internal(new_value));
                                                    DiffApplicationResult::pure(mv).with_updates(Some(im::HashMap::new().update(oid, new_value.clone())))
                                                });
                                                update(diffapp)
                                            });
                                            let new_delete: Box<dyn Fn() -> StateTree> = Box::new(|| {
                                                let new_chars = chars.clone();
                                                new_chars.remove(i.try_into().unwrap());
                                                let new_val = StateTreeValue::Internal(StateTreeComposite::Text(StateTreeText{
                                                    object_id: oid,
                                                    chars: new_chars,
                                                }));
                                                update(DiffApplicationResult::pure(current_obj.update_default(new_val)))
                                            });
                                            return Some(PathResolution {
                                                path: &path,
                                                containing_object_id: parent_object_id,
                                                object_id: None,
                                                register: Box::new(target.clone()),
                                                mutation_target: MutationTarget::Character(CharTarget {
                                                    update: new_update,
                                                    delete: new_delete,
                                                }),
                                            });
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
                };
                let target = match current_obj.default_statetree_value().unwrap() {
                    StateTreeValue::Leaf(v) => match v {
                        amp::ScalarValue::Counter(v) => MutationTarget::Counter(CounterTarget{
                            current_value: v,
                            multivalue: current_obj.clone(),
                            update,
                            delete,
                        }),
                        _ => MutationTarget::Primitive(PrimitiveTarget{
                            update,
                            delete,
                        }),
                    },
                    StateTreeValue::Internal(composite) => match composite {
                        StateTreeComposite::Map(m) => MutationTarget::Map(MapTarget{
                            value: m,
                            multivalue: current_obj.clone(),
                            update,
                            delete,
                        }),
                        StateTreeComposite::Table(t) => MutationTarget::Table(TableTarget{
                            value: t,
                            multivalue: current_obj.clone(),
                            update,
                            delete,
                        }),
                        StateTreeComposite::List(l) => MutationTarget::List(ListTarget{
                            value: l,
                            multivalue: current_obj.clone(),
                            update,
                            delete,
                        }),
                        StateTreeComposite::Text(_) => {
                            panic!("Unexpectedly reached text object")
                        }
                    }
                };
                Some(PathResolution {
                    path: &path,
                    containing_object_id: parent_object_id,
                    object_id: current_obj
                        .default_statetree_value()
                        .and_then(|o| o.object_id()),
                    register: Box::new(current_obj.clone()),
                    mutation_target: target,
                })
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

    pub fn value_for_object_id(&self, oid: &amp::ObjectID) -> Option<Value> {
        self.object_index.get(oid).map(|o| o.value())
    }

    pub fn with_new_value<'a, 'b>(
        &'a self,
        v: &'b Value,
        update: Box<dyn Fn(MultiValue) -> StateTree>,
    ) -> (StateTree, MultiValue) {
        let result = MultiValue::new_from_value(v);
        let new_tree = StateTree {
            root_map: update(result.value().clone()).root_map,
            object_index: result
                .index_updates()
                .map(|u| u.union(self.object_index.clone()))
                .unwrap_or_else(|| self.object_index.clone()),
        };
        (new_tree, result.value().clone())
    }
}

/// Applying a diff returns two things
/// - The updated object T (either a StateTreeComposite or a [MultiValue])
/// - The updates to the object ID index
#[derive(Clone)]
pub struct DiffApplicationResult<T>((T, Option<im::HashMap<amp::ObjectID, StateTreeComposite>>));

impl<T> DiffApplicationResult<T> where T: Clone {
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
        G: Clone
    {
        DiffApplicationResult((f(self.0 .0), self.0 .1))
    }

    fn and_then<F, G>(self, f: F) -> DiffApplicationResult<G>
    where
        F: Fn(T) -> DiffApplicationResult<G>,
        G: Clone
    {
        let diff = f(self.0 .0.clone());
        self.with_updates(diff.0 .1.clone()).map(|_| diff.0 .0.clone())
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

    /// Combines two diffapplication results. This does two things:
    /// 1. Use `f` to choose a new contained value
    /// 2. Take the union of the index updates of the diffapplication result, updates from `other`
    ///    will take precedence in the case of duplicate object IDs
    fn union_with<U, V, F>(&self, other: DiffApplicationResult<U>, f: F) -> DiffApplicationResult<V>
    where
        F: Fn(T, U) -> V,
    {
        let new_contained = f(self.0 .0.clone(), other.0 .0);
        let new_index_updates = match (self.0 .1.clone(), other.0 .1) {
            (Some(u1), Some(u2)) => Some(u2.union(u1)),
            (None, Some(u2)) => Some(u2),
            (Some(u1), None) => Some(u1),
            (None, None) => None,
        };
        DiffApplicationResult((new_contained, new_index_updates))
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

    fn new_from_diff(
        diff: &std::collections::HashMap<amp::OpID, amp::Diff>,
    ) -> Result<DiffApplicationResult<MultiValue>, error::InvalidPatch> {
        let result = MultiValue::new();
        result.apply_diff(diff)
    }

    fn new_from_value(value: &Value) -> DiffApplicationResult<MultiValue> {
        match value {
            Value::Map(props, amp::MapType::Map) => {
                let map = im::HashMap::new();
                let diffapp = DiffApplicationResult((map, None));
                props
                    .iter()
                    .fold(diffapp, |diffapp_so_far, (key, value)| {
                        let val_diffapp = MultiValue::new_from_value(value);
                        diffapp_so_far
                            .union_with(val_diffapp, |map, val| map.update(key.clone(), val))
                    })
                    .map(|map| {
                        MultiValue(im::HashMap::new().update(
                            random_op_id(),
                            StateTreeValue::Internal(StateTreeComposite::Map(StateTreeMap {
                                object_id: new_object_id(),
                                props: map,
                            })),
                        ))
                    })
            }
            Value::Map(props, amp::MapType::Table) => {
                let map = im::HashMap::new();
                let diffapp = DiffApplicationResult((map, None));
                props
                    .iter()
                    .fold(diffapp, |diffapp_so_far, (key, value)| {
                        let val_diffapp = MultiValue::new_from_value(value);
                        diffapp_so_far
                            .union_with(val_diffapp, |map, val| map.update(key.clone(), val))
                    })
                    .map(|map| {
                        MultiValue(im::HashMap::new().update(
                            random_op_id(),
                            StateTreeValue::Internal(StateTreeComposite::Table(StateTreeTable {
                                object_id: new_object_id(),
                                props: map,
                            })),
                        ))
                    })
            }
            Value::Sequence(vals) => {
                let elems = im::Vector::new();
                let diffapp = DiffApplicationResult((elems, None));
                vals.iter()
                    .fold(diffapp, |diffapp_so_far, elem| {
                        let elem_diffapp = MultiValue::new_from_value(elem);
                        diffapp_so_far.union_with(elem_diffapp, |mut elems_so_far, elem| {
                            elems_so_far.push_back(elem);
                            elems_so_far
                        })
                    })
                    .map(|elems| {
                        MultiValue(im::HashMap::new().update(
                            random_op_id(),
                            StateTreeValue::Internal(StateTreeComposite::List(StateTreeList {
                                object_id: new_object_id(),
                                elements: elems,
                            })),
                        ))
                    })
            }
            Value::Text(chars) => {
                let obj_id = new_object_id();
                let val = StateTreeComposite::Text(StateTreeText {
                    object_id: new_object_id(),
                    chars: chars
                        .iter()
                        .map(|c| MultiChar::new_from_char(c.clone()))
                        .collect(),
                });
                let mv = MultiValue(
                    im::HashMap::new().update(random_op_id(), StateTreeValue::Internal(val.clone())),
                );
                DiffApplicationResult((mv, Some(im::HashMap::new().update(obj_id, val))))
            }
            Value::Primitive(v) => DiffApplicationResult((
                MultiValue(
                    im::HashMap::new().update(random_op_id(), StateTreeValue::Leaf(v.clone())),
                ),
                None,
            )),
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
                    StateTreeValue::Internal(composite) => Ok(composite
                        .apply_diff(subdiff)?
                        .map(|c| StateTreeValue::Internal(c))),
                }
            } else {
                StateTreeValue::new_from_diff(subdiff)
            }?;
            result = result
                .map(|u| u.update(opid.clone(), application_result.value().clone()))
                .with_updates(application_result.index_updates());
        }
        Ok(result.map(|new_values| MultiValue(new_values)))
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
        opids.first().map(|oid| self.0.get(oid).unwrap().clone())
    }

    fn default_opid(&self) -> amp::OpID {
        let mut opids: Vec<&amp::OpID> = self.0.keys().collect();
        opids.sort();
        opids.reverse();
        (*opids.first().unwrap()).clone()
    }

    fn update_default(&self, new_char: char) -> MultiChar {
        MultiChar(self.0.update(self.default_opid(), new_char))
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
            amp::Diff::Value(v) => {
                return Ok(DiffApplicationResult::pure(StateTreeValue::Leaf(v.clone())))
            }
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

    fn object_id(&self) -> Option<amp::ObjectID> {
        match self {
            StateTreeValue::Leaf(_) => None,
            StateTreeValue::Internal(composite) => Some(composite.object_id()),
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
        StateTreeList{
            object_id: self.object_id.clone(),
            elements: new_elems,
        }
    }

    fn set(&self, index: usize, value: MultiValue) -> StateTreeList {
        StateTreeList{
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
                            patch_expected_type: Some(amp::ObjType::Map(obj_type.clone())),
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
                            patch_expected_type: Some(amp::ObjType::Map(obj_type.clone())),
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
                                patch_expected_type: Some(amp::ObjType::Sequence(obj_type.clone())),
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
                                patch_expected_type: Some(amp::ObjType::Sequence(obj_type.clone())),
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

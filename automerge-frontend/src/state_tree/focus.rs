use super::{DiffApplicationResult, MultiValue, StateTree, StateTreeMap, StateTreeComposite, StateTreeValue, StateTreeTable, StateTreeList};
use im::hashmap;

#[derive(Clone)]
pub(crate) struct Focus(FocusInner);

impl Focus {
    pub(super) fn update(&self, diffapp: DiffApplicationResult<MultiValue>) -> StateTree {
        match &self.0 {
            FocusInner::Root(root) => root.update(diffapp),
            FocusInner::Map(mapfocus) => mapfocus.update(diffapp),
            FocusInner::Table(tablefocus) => tablefocus.update(diffapp),
            FocusInner::List(listfocus) => listfocus.update(diffapp),
        }
    }

    pub(crate) fn delete(&self) -> StateTree {
        match &self.0 {
            FocusInner::Root(root) => root.delete(),
            FocusInner::Map(mapfocus) => mapfocus.delete(),
            FocusInner::Table(tablefocus) => tablefocus.delete(),
            FocusInner::List(listfocus) => listfocus.delete(),
        }
    }

    pub fn new_root(root_tree: StateTree, key: String) -> Focus{
        Focus(FocusInner::Root(RootFocus{
            root: root_tree,
            key,
        }))
    }

    pub(super) fn new_map(parent: Box<Focus>, map: StateTreeMap, key: String, multivalue: MultiValue) -> Focus{
        Focus(FocusInner::Map(MapFocus{
            parent_focus: parent,
            key,
            map,
            multivalue,
        }))
    }

    pub(super) fn new_table(parent: Box<Focus>, table: StateTreeTable, key: String, multivalue: MultiValue) -> Focus {
        Focus(FocusInner::Table(TableFocus{
            parent_focus: parent,
            key,
            table,
            multivalue,
        }))
    }

    pub(super) fn new_list(parent: Box<Focus>, list: StateTreeList, index: usize, multivalue: MultiValue) -> Focus {
        Focus(FocusInner::List(ListFocus{
            parent_focus: parent,
            index,
            list,
            multivalue,
        }))
    }
}

#[derive(Clone)]
enum FocusInner {
    Root(RootFocus),
    Map(MapFocus),
    Table(TableFocus),
    List(ListFocus),
}


#[derive(Clone)]
struct RootFocus {
    root: StateTree,
    key: String,
}


impl RootFocus {
    fn update(&self, diffapp: DiffApplicationResult<MultiValue>) -> StateTree {
        self.root.update(self.key.clone(), diffapp)
    }

    fn delete(&self) -> StateTree {
        self.root.remove(&self.key)
    }
}

#[derive(Clone)]
struct MapFocus{
    parent_focus: Box<Focus>, 
    key: String,
    map: StateTreeMap,
    multivalue: MultiValue,
}

impl MapFocus {
    fn update(&self, diffapp: DiffApplicationResult<MultiValue>) -> StateTree {
        let new_diffapp = diffapp.and_then(|v| {
            let updated =
                StateTreeComposite::Map(StateTreeMap {
                    object_id: self.map.object_id.clone(),
                    props: self.map.props.update(self.key.clone(), v),
                });
            DiffApplicationResult::pure(
                self.multivalue.update_default(
                    StateTreeValue::Internal(updated.clone()),
                ),
            )
            .with_updates(Some(
                hashmap!(self.map.object_id.clone() => updated)
            ))
        });
        self.parent_focus.update(new_diffapp)
    }

    fn delete(&self) -> StateTree {
        let new_val = self.multivalue.update_default(
            StateTreeValue::Internal(StateTreeComposite::Map(
                StateTreeMap {
                    object_id: self.map.object_id.clone(),
                    props: self.map.props.without(&self.key),
                },
            )),
        );
        self.parent_focus.update(DiffApplicationResult::pure(new_val))
    }
}

#[derive(Clone)]
struct TableFocus{
    parent_focus: Box<Focus>, 
    key: String,
    table: StateTreeTable,
    multivalue: MultiValue,
}

impl TableFocus {
    fn update(&self, diffapp: DiffApplicationResult<MultiValue>) -> StateTree {
        let new_diffapp = diffapp.and_then(|v| {
            let updated =
                StateTreeComposite::Table(StateTreeTable {
                    object_id: self.table.object_id.clone(),
                    props: self.table.props.update(self.key.clone(), v),
                });
            DiffApplicationResult::pure(
                self.multivalue.update_default(
                    StateTreeValue::Internal(updated.clone()),
                ),
            )
            .with_updates(Some(
                hashmap!(self.table.object_id.clone() => updated)
            ))
        });
        self.parent_focus.update(new_diffapp)
    }

    fn delete(&self) -> StateTree {
        let new_val = self.multivalue.update_default(
            StateTreeValue::Internal(StateTreeComposite::Table(
                StateTreeTable {
                    object_id: self.table.object_id.clone(),
                    props: self.table.props.without(&self.key),
                },
            )),
        );
        self.parent_focus.update(DiffApplicationResult::pure(new_val))
    }
}

#[derive(Clone)]
struct ListFocus {
    parent_focus: Box<Focus>,
    index: usize,
    list: StateTreeList,
    multivalue: MultiValue,
}

impl ListFocus {
    fn update(&self, diffapp: DiffApplicationResult<MultiValue>) -> StateTree {
        let new_diffapp = diffapp.and_then(|v| {
            let updated =
                StateTreeComposite::List(StateTreeList {
                    object_id: self.list.object_id.clone(),
                    elements: self.list.elements
                        .update(self.index, v),
                });
            DiffApplicationResult::pure(
                self.multivalue.update_default(
                    StateTreeValue::Internal(updated.clone()),
                ),
            )
            .with_updates(Some(
                hashmap!(self.list.object_id.clone() => updated)
            ))
        });
        self.parent_focus.update(new_diffapp)
    }

    fn delete(&self) -> StateTree {
        let mut new_elems = self.list.elements.clone();
        new_elems.remove(self.index);
        let new_val = self.multivalue.update_default(
            StateTreeValue::Internal(StateTreeComposite::List(
                StateTreeList {
                    object_id: self.list.object_id.clone(),
                    elements: new_elems,
                },
            )),
        );
        self.parent_focus.update(DiffApplicationResult::pure(new_val))
    }
}

pub enum MutationTarget {
    Map(MapTarget),
    Table {
        replace: Box<dyn FnOnce(&Value) -> StateTree>,
        set_key: Box<dyn FnOnce(&str, &Value) -> StateTree>,
        delete: Box<dyn FnOnce() -> StateTree>,
    },
    List {
        replace: Box<dyn FnOnce(&Value) -> StateTree>,
        insert: Box<dyn FnOnce(u32, &Value) -> StateTree>,
        delete: Box<dyn FnOnce() -> StateTree>,
    },
    Text {
        replace: Box<dyn FnOnce(&Value) -> StateTree>,
        insert: Box<dyn FnOnce(u32, char) -> StateTree>,
        delete: Box<dyn FnOnce() -> StateTree>,
    },
    Character {
        replace: Box<dyn FnOnce(char) -> StateTree>,
        delete: Box<dyn FnOnce() -> StateTree>,
    },
    Counter {
        increment: Box<dyn FnOnce(u32) -> StateTree>,
        delete: Box<dyn FnOnce() -> StateTree>,
    },
    Primitive {
        replace: Box<dyn FnOnce(&Value) -> StateTree>,
        delete: Box<dyn FnOnce() -> StateTree>,
    },
}

pub struct MapTarget {
    root: StateTree,
    value: StateTreeMap,
    multivalue: MultiValue,
    update: Box<dyn FnOnce(DiffApplicationResult<MultiValue>) -> StateTree>,
    delete: Box<dyn FnOnce() -> StateTree>,
}

impl MapTarget {
    pub fn replace(&self, value: &Value) -> StateTree {
        (self.update)(MultiValue::new_from_value(value))
    }

    pub fn set_key(&self, key: &str, value: &Value) -> StateTree {
        let diffapp = MultiValue::new_from_value(value).and_then(|v| {
            let new_value = self.value.update(key.to_string(), v);
            let new_composite = StateTreeComposite::Map(new_value);
            let new_mv = self.multivalue.update_default(StateTreeValue::Internal(new_composite));
            DiffApplicationResult::pure(new_mv).with_updates(Some(im::HashMap::new().update(self.value.object_id, new_composite)))
        });
        (self.update)(diffapp)
    }

    pub fn delete(&self) -> StateTree {
        (self.delete)()
    }
}

impl MutationTarget {
    pub fn delete(&self) -> StateTree {
        match self {
            Self::Map { delete, .. }
            | Self::Table { delete, .. }
            | Self::List { delete, .. }
            | Self::Text { delete, .. }
            | Self::Character { delete, .. }
            | Self::Counter { delete, .. }
            | Self::Primitive { delete, .. } => delete(),
        }
    }
}

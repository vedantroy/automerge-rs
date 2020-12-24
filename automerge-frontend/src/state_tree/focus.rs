use super::{DiffApplicationResult, MultiValue, StateTree, StateTreeMap};

enum Focus {
    MapFocus{
        parent_focus: Box<Focus>, 
        value: StateTreeMap
    }
}

impl Focus {
    fn update(&self, diffapp: DiffApplicationResult<MultiValue>) -> StateTree {
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
}

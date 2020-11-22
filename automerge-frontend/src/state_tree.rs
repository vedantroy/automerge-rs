/// The state of the frontend is more complicated than just the current resolved
/// value. We need to tag every value in the state with the OpID that created it.
/// and maintain paralell trees of conflicting values.

use automerge_protocol as amp;
use crate::error;

/// The root node is special in that it doesn't require an OpID to create
#[derive(Debug, Clone)]
pub(crate) struct StateTreeRoot {
    root_map: im::HashMap<String, Conflicts>,
}

/// A set of conflicting values for the same key, indexed by OpID
#[derive(Debug, Clone)]
struct Conflicts(im::HashMap<amp::OpID, StateTreeValue>);

impl Conflicts {
    fn new() -> Conflicts {
        Conflicts(im::HashMap::new())
    }

    fn new_from_diff(diff: &std::collections::HashMap<amp::OpID, amp::Diff>) -> Result<Conflicts, error::InvalidPatch> {
        let result = Conflicts::new();
        result.apply_diff(diff)
    }

    fn apply_diff(&self, diff: &std::collections::HashMap<amp::OpID, amp::Diff>) -> Result<Conflicts, error::InvalidPatch> {
        let mut new_values = self.0.clone();
        for (opid, subdiff) in diff.iter() {
            let new_value = if let Some(existing_value) = self.0.get(opid) {
                match existing_value {
                    StateTreeValue::Leaf(_) => StateTreeValue::new_from_diff(subdiff)?,
                    StateTreeValue::Internal(composite) => StateTreeValue::Internal(composite.apply_diff(subdiff)?),
                }
            } else {
                StateTreeValue::new_from_diff(subdiff)?
            };
            new_values = new_values.update(opid.clone(), new_value);
        };
        Ok(Conflicts(new_values))
    }
}

/// This struct exists to constrain the values of a text type to just containing
/// sequences of chars
#[derive(Debug, Clone)]
struct CharConflicts(im::HashMap<amp::OpID, char>);

impl CharConflicts {
    fn new() -> CharConflicts {
        CharConflicts(im::HashMap::new())
    }

    fn apply_diff(&self, parent_object_id: &amp::ObjectID, diff: &std::collections::HashMap<amp::OpID, amp::Diff>) -> Result<CharConflicts, error::InvalidPatch> {
        let mut new_values = self.0.clone();
        for (opid, subdiff) in diff.iter() {
            match subdiff {
                amp::Diff::Value(amp::ScalarValue::Str(s)) => {
                    if s.len() != 1 {
                        return Err(error::InvalidPatch::InsertNonTextInTextObject{
                            object_id: parent_object_id.clone(),
                            diff: subdiff.clone(),
                        });
                    } else {
                        new_values = new_values.update(opid.clone(), s.chars().next().unwrap());
                    }
                },
                _ => {
                    return Err(error::InvalidPatch::InsertNonTextInTextObject{
                        object_id: parent_object_id.clone(),
                        diff: subdiff.clone(),
                    });
                }
            }
        };
        Ok(CharConflicts(new_values))
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

    fn new_from_diff(diff: &amp::Diff) -> Result<StateTreeValue, error::InvalidPatch> {
        match diff {
            amp::Diff::Value(v) => return Ok(StateTreeValue::Leaf(v.clone())),
            amp::Diff::Map(amp::MapDiff{object_id, obj_type, props, ..}) => {
                let prop_diffs_vec: Result<Vec<(String, Conflicts)>, error::InvalidPatch> = props.iter().map::<Result<(String, Conflicts), error::InvalidPatch>, _>(|(prop,  prop_diff)| {
                    let conflicts = Conflicts::new_from_diff(prop_diff)?;
                    Ok((prop.clone(), conflicts))
                }).collect();
                let prop_diffs: im::HashMap<String, Conflicts> = prop_diffs_vec?.into();
                match obj_type {
                    amp::MapType::Map => Ok(StateTreeValue::Internal(StateTreeComposite::Map(object_id.clone(), prop_diffs))),
                    amp::MapType::Table => Ok(StateTreeValue::Internal(StateTreeComposite::Table(object_id.clone(), prop_diffs))),
                }
            }
            //TODO the following code is copied from StateTreeComposite::apply_diff, there should
            //be a way to abstract this behaviour
            amp::Diff::Seq(amp::SeqDiff{object_id, obj_type, edits, props, ..}) => match obj_type {
                amp::SequenceType::Text => {
                    let mut char_conflicts = im::Vector::new();
                    for edit in edits.iter() {
                        match edit {
                            amp::DiffEdit::Remove{ index } => {
                                char_conflicts.remove(*index);
                            },
                            amp::DiffEdit::Insert{ index } => {
                                char_conflicts.insert(*index, CharConflicts::new());
                            },
                        };
                    };
                    for (index, prop_diff) in props {
                        if let Some(char) = char_conflicts.get(*index) {
                            let new_char = char.apply_diff(object_id, prop_diff)?;
                            char_conflicts = char_conflicts.update(*index, new_char)
                        } else {
                            return Err(error::InvalidPatch::InvalidIndex{
                                object_id: object_id.clone(),
                                index: *index,
                            });
                        }
                    };
                    Ok(StateTreeValue::Internal(StateTreeComposite::Text(object_id.clone(), char_conflicts)))
                }
                amp::SequenceType::List => {
                    let mut new_elements = im::Vector::new();
                    for edit in edits.iter() {
                        match edit {
                            amp::DiffEdit::Remove{ index } => {
                                new_elements.remove(*index);
                            },
                            amp::DiffEdit::Insert{ index } => {
                                if (*index) == new_elements.len() {
                                    new_elements.push_back(Conflicts::new())
                                } else {
                                    new_elements.insert(*index, Conflicts::new())
                                }
                            },
                        };
                    };
                    for (index, prop_diff) in props {
                        if let Some(node) = new_elements.get(*index) {
                            let new_node = node.apply_diff(prop_diff)?;
                            new_elements = new_elements.update(*index, new_node)
                        } else {
                            return Err(error::InvalidPatch::InvalidIndex{
                                object_id: object_id.clone(),
                                index: *index,
                            });
                        }
                    };
                    Ok(StateTreeValue::Internal(StateTreeComposite::List(object_id.clone(), new_elements)))
                }
            },
            amp::Diff::Unchanged(..) => Err(error::InvalidPatch::UnchangedDiffForNonExistentObject),
        }
    }

}

#[derive(Debug, Clone)]
enum StateTreeComposite {
    Map(amp::ObjectID, im::HashMap<String, Conflicts>),
    Table(amp::ObjectID, im::HashMap<String, Conflicts>),
    Text(amp::ObjectID, im::Vector<CharConflicts>),
    List(amp::ObjectID, im::Vector<Conflicts>),
}

impl StateTreeComposite {
    fn apply_diff(&self, diff: &amp::Diff) -> Result<StateTreeComposite, error::InvalidPatch> {
        if diff_object_id(diff) != Some(self.object_id()) {
            return Err(error::InvalidPatch::MismatchingObjectIDs{
                patch_expected_id: diff_object_id(diff),
                actual_id: self.object_id(),
            })
        };
        match diff {
            amp::Diff::Map(amp::MapDiff{obj_type, props: prop_diffs, ..}) => {
                match self {
                    StateTreeComposite::Map(this_obj_id, current_props) => {
                        if *obj_type != amp::MapType::Map {
                            Err(error::InvalidPatch::MismatchingObjectType{
                                object_id: this_obj_id.clone(),
                                patch_expected_type: Some(amp::ObjType::Map(obj_type.clone())),
                                actual_type: Some(self.obj_type()),
                            })
                        } else {
                            let mut new_props = current_props.clone();
                            for (prop, prop_diff) in prop_diffs.iter() {
                                if prop_diffs.is_empty() {
                                    new_props = new_props.without(prop)
                                } else {
                                    let node = new_props.get(prop).cloned().unwrap_or_else(Conflicts::new);
                                    let updated_node = node.apply_diff(prop_diff)?; 
                                    new_props = new_props.update(prop.clone(), updated_node)
                                }
                            }
                            Ok(StateTreeComposite::Map(this_obj_id.clone(), new_props))
                        }
                    },
                    StateTreeComposite::Table(this_obj_id, current_props) => {
                        if *obj_type != amp::MapType::Table {
                            Err(error::InvalidPatch::MismatchingObjectType{
                                object_id: this_obj_id.clone(),
                                patch_expected_type: Some(amp::ObjType::Map(obj_type.clone())),
                                actual_type: Some(self.obj_type()),
                            })
                        } else {
                            let mut new_props = current_props.clone();
                            for (prop, prop_diff) in prop_diffs.iter() {
                                if prop_diffs.is_empty() {
                                    new_props = new_props.without(prop)
                                } else {
                                    let node = new_props.get(prop).cloned().unwrap_or_else(Conflicts::new);
                                    let updated_node = node.apply_diff(prop_diff)?; 
                                    new_props = new_props.update(prop.clone(), updated_node)
                                }
                            }
                            Ok(StateTreeComposite::Table(this_obj_id.clone(), new_props))
                        }
                    },
                    _ => {
                        Err(error::InvalidPatch::MismatchingObjectType{
                            object_id: self.object_id(),
                            patch_expected_type: diff_object_type(diff),
                            actual_type: Some(self.obj_type()),
                        })
                    }
                }
            },
            amp::Diff::Seq(amp::SeqDiff{edits, props: new_props, obj_type, ..}) => {
                println!("Diff: {:?}", diff);
                println!("Self: {:?}", self);
                match self {
                    StateTreeComposite::List(this_obj_id, current_elements) => {
                        println!("Applying list diff");
                        if *obj_type != amp::SequenceType::List {
                            Err(error::InvalidPatch::MismatchingObjectType{
                                object_id: this_obj_id.clone(),
                                patch_expected_type: Some(amp::ObjType::Sequence(obj_type.clone())),
                                actual_type: Some(self.obj_type()),
                            })
                        } else {
                            let mut new_elements = current_elements.clone();
                            for edit in edits.iter() {
                                match edit {
                                    amp::DiffEdit::Remove{ index } => {
                                        new_elements.remove(*index);
                                    },
                                    amp::DiffEdit::Insert{ index } => {
                                        if (*index) == new_elements.len() {
                                            new_elements.push_back(Conflicts::new())
                                        } else {
                                            new_elements.insert(*index, Conflicts::new())
                                        }
                                    },
                                };
                            };
                            for (index, prop_diff) in new_props {
                                if let Some(node) = new_elements.get(*index) {
                                    let new_node = node.apply_diff(prop_diff)?;
                                    new_elements = new_elements.update(*index, new_node)
                                } else {
                                    return Err(error::InvalidPatch::InvalidIndex{
                                        object_id: this_obj_id.clone(),
                                        index: *index,
                                    });
                                }
                            };
                            println!("New elements: {:?}", new_elements);
                            Ok(StateTreeComposite::List(this_obj_id.clone(), new_elements))
                        }
                    },
                    StateTreeComposite::Text(this_obj_id, current_chars) => {
                        if *obj_type != amp::SequenceType::Text {
                            Err(error::InvalidPatch::MismatchingObjectType{
                                object_id: this_obj_id.clone(),
                                patch_expected_type: Some(amp::ObjType::Sequence(obj_type.clone())),
                                actual_type: Some(self.obj_type()),
                            })
                        } else {
                            let mut new_chars = current_chars.clone();
                            for edit in edits.iter() {
                                match edit {
                                    amp::DiffEdit::Remove{ index } => {
                                        new_chars.remove(*index);
                                    },
                                    amp::DiffEdit::Insert{ index } => {
                                        new_chars.insert(*index, CharConflicts::new());
                                    },
                                };
                            };
                            for (index, prop_diff) in new_props {
                                if let Some(char) = new_chars.get(*index) {
                                    let new_char = char.apply_diff(this_obj_id, prop_diff)?;
                                    new_chars = new_chars.update(*index, new_char)
                                } else {
                                    return Err(error::InvalidPatch::InvalidIndex{
                                        object_id: this_obj_id.clone(),
                                        index: *index,
                                    });
                                }
                            };
                            Ok(StateTreeComposite::Text(this_obj_id.clone(), new_chars))
                        }
                    },
                    _ => {
                        Err(error::InvalidPatch::MismatchingObjectType{
                            object_id: self.object_id(),
                            patch_expected_type: diff_object_type(diff),
                            actual_type: Some(self.obj_type()),
                        })
                    }
                }
            },
            amp::Diff::Unchanged(..) => {
                Ok(self.clone())
            },
            amp::Diff::Value(..) => {
                panic!("SHould never be called")
            },
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
            Self::Map(object_id, ..) => object_id.clone(),
            Self::Table(object_id, ..) => object_id.clone(),
            Self::Text(object_id, ..) => object_id.clone(),
            Self::List(object_id, ..) => object_id.clone(),
        }
    }


}


impl StateTreeRoot {
    pub fn new() -> StateTreeRoot {
        StateTreeRoot{
            root_map: im::HashMap::new(),
        }
    }

    pub fn apply_diff(&self, diff: &amp::Diff) -> Result<StateTreeRoot, error::InvalidPatch> {
        match diff {
            amp::Diff::Map(mapdiff) => {
                let amp::MapDiff{object_id, obj_type, props: _props} = mapdiff;
                if *object_id != amp::ObjectID::Root {
                    Err(error::InvalidPatch::PatchDidNotBeginAtRoot)
                } else if *obj_type != amp::MapType::Map {
                    return Err(error::InvalidPatch::MismatchingObjectType{
                        object_id: amp::ObjectID::Root,
                        patch_expected_type: Some(amp::ObjType::map()),
                        actual_type: Some(amp::ObjType::Map(*obj_type))
                    })
                } else {
                    Ok(StateTreeRoot{root_map: apply_map_diff(&self.root_map, diff)?})
                }
            },
            _ => Err(error::InvalidPatch::PatchDidNotBeginAtRoot)
        }
    }
}

fn apply_map_diff(current_state: &im::HashMap<String, Conflicts>, diff: &amp::Diff) -> Result<im::HashMap<String, Conflicts>, error::InvalidPatch> {
    let updated = StateTreeComposite::Map(amp::ObjectID::Root, current_state.clone()).apply_diff(diff)?;
    match updated {
        StateTreeComposite::Map(_, values) => Ok(values),
        _ => panic!("Bad type returned from apply_diff")
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

extern crate automerge_backend;
use automerge_backend::Backend;
use automerge_backend::Change;
use automerge_protocol as protocol;
use automerge_protocol as amp;
use automerge_protocol::{
    ActorID, ChangeHash, DataType, Diff, DiffEdit, ElementID, MapDiff, MapType, ObjectID, Op,
    Patch, SeqDiff, SequenceType, UncompressedChange,
};
use maplit::hashmap;
use std::collections::HashSet;
use std::convert::TryInto;

#[test]
fn test_apply_local_change() {
    let actor: ActorID = "eb738e04ef8848ce8b77309b6c7f7e39".try_into().unwrap();
    let change_request = UncompressedChange {
        actor_id: actor.clone(),
        time: 0,
        message: None,
        seq: 1,
        deps: Vec::new(),
        start_op: 1,
        operations: vec![Op {
            action: protocol::OpType::Set,
            value: Some("magpie".into()),
            datatype: Some(DataType::Undefined),
            key: "bird".into(),
            obj: ObjectID::Root.to_string(),
            insert: false,
            pred: Vec::new(),
        }],
    };

    let mut backend = Backend::init();
    let patch = backend.apply_local_change(change_request).unwrap().0;

    let changes = backend.get_changes(&[]);
    let expected_change = UncompressedChange {
        actor_id: actor.clone(),
        seq: 1,
        start_op: 1,
        time: changes[0].time,
        message: None,
        deps: Vec::new(),
        operations: vec![Op {
            action: amp::OpType::Set,
            value: Some("magpie".into()),
            datatype: Some(DataType::Undefined),
            obj: ObjectID::Root.to_string(),
            key: "bird".into(),
            pred: Vec::new(),
            insert: false,
        }],
    }
    .try_into()
    .unwrap();
    assert_eq!(changes[0], &expected_change);

    let expected_patch = Patch {
        actor: Some(actor.clone()),
        max_op: 1,
        seq: Some(1),
        clock: hashmap! {
            actor => 1,
        },
        deps: Vec::new(),
        diffs: Some(Diff::Map(MapDiff {
            object_id: ObjectID::Root,
            obj_type: MapType::Map,
            props: hashmap! {
                "bird".into() => hashmap!{
                    "1@eb738e04ef8848ce8b77309b6c7f7e39".try_into().unwrap() => Diff::Value("magpie".into())
                }
            },
        })),
    };
    assert_eq!(patch, expected_patch);
}

#[test]
fn test_error_on_duplicate_requests() {
    let actor: ActorID = "37704788917a499cb0206fa8519ac4d9".try_into().unwrap();
    let change_request1 = UncompressedChange {
        actor_id: actor.clone(),
        seq: 1,
        message: None,
        time: 0,
        deps: Vec::new(),
        start_op: 1,
        operations: vec![Op {
            action: protocol::OpType::Set,
            obj: ObjectID::Root.to_string(),
            key: "bird".into(),
            value: Some("magpie".into()),
            datatype: Some(DataType::Undefined),
            insert: false,
            pred: Vec::new(),
        }],
    };

    let change_request2 = UncompressedChange {
        actor_id: actor,
        seq: 2,
        message: None,
        time: 0,
        deps: Vec::new(),
        start_op: 2,
        operations: vec![Op {
            action: protocol::OpType::Set,
            obj: ObjectID::Root.to_string(),
            key: "bird".into(),
            value: Some("jay".into()),
            insert: false,
            datatype: Some(DataType::Undefined),
            pred: Vec::new(),
        }],
    };
    let mut backend = Backend::init();
    backend.apply_local_change(change_request1.clone()).unwrap();
    backend.apply_local_change(change_request2.clone()).unwrap();
    assert!(backend.apply_local_change(change_request1).is_err());
    assert!(backend.apply_local_change(change_request2).is_err());
}

#[test]
fn test_handle_concurrent_frontend_and_backend_changes() {
    let actor: ActorID = "cb55260e9d7e457886a4fc73fd949202".try_into().unwrap();
    let local1 = UncompressedChange {
        actor_id: actor.clone(),
        seq: 1,
        time: 0,
        deps: Vec::new(),
        message: None,
        start_op: 1,
        operations: vec![Op {
            action: protocol::OpType::Set,
            obj: ObjectID::Root.to_string(),
            key: "bird".into(),
            value: Some("magpie".into()),
            datatype: Some(DataType::Undefined),
            insert: false,
            pred: Vec::new(),
        }],
    };

    let local2 = UncompressedChange {
        actor_id: actor.clone(),
        seq: 2,
        start_op: 2,
        time: 0,
        deps: Vec::new(),
        message: None,
        operations: vec![Op {
            action: protocol::OpType::Set,
            obj: ObjectID::Root.to_string(),
            key: "bird".into(),
            value: Some("jay".into()),
            datatype: Some(DataType::Undefined),
            insert: false,
            pred: vec![actor.op_id_at(1)],
        }],
    };
    let remote_actor: ActorID = "6d48a01318644eed90455d2cb68ac657".try_into().unwrap();
    let remote1 = UncompressedChange {
        actor_id: remote_actor.clone(),
        seq: 1,
        start_op: 1,
        time: 0,
        deps: Vec::new(),
        message: None,
        operations: vec![Op {
            action: protocol::OpType::Set,
            value: Some("goldfish".into()),
            datatype: Some(DataType::Undefined),
            obj: ObjectID::Root.to_string(),
            key: "fish".into(),
            pred: Vec::new(),
            insert: false,
        }],
    }
    .try_into()
    .unwrap();

    let mut expected_change1 = UncompressedChange {
        actor_id: actor.clone(),
        seq: 1,
        start_op: 1,
        time: 0,
        message: None,
        deps: Vec::new(),
        operations: vec![Op {
            action: protocol::OpType::Set,
            value: Some("magpie".into()),
            datatype: Some(DataType::Undefined),
            obj: ObjectID::Root.to_string(),
            key: "bird".into(),
            pred: Vec::new(),
            insert: false,
        }],
    };

    let mut expected_change2 = UncompressedChange {
        actor_id: remote_actor,
        seq: 1,
        start_op: 1,
        time: 0,
        message: None,
        deps: Vec::new(),
        operations: vec![Op {
            action: protocol::OpType::Set,
            value: Some("goldfish".into()),
            datatype: Some(DataType::Undefined),
            key: "fish".into(),
            obj: ObjectID::Root.to_string(),
            pred: Vec::new(),
            insert: false,
        }],
    };

    let mut expected_change3 = UncompressedChange {
        actor_id: actor.clone(),
        seq: 2,
        start_op: 2,
        time: 0,
        message: None,
        deps: Vec::new(),
        operations: vec![Op {
            action: protocol::OpType::Set,
            value: Some("jay".into()),
            datatype: Some(DataType::Undefined),
            obj: ObjectID::Root.to_string(),
            key: "bird".into(),
            pred: vec![actor.op_id_at(1)],
            insert: false,
        }],
    };
    let mut backend = Backend::init();
    backend.apply_local_change(local1).unwrap();
    let backend_after_first = backend.clone();
    let changes1 = backend_after_first.get_changes(&[]);
    let change01 = changes1.get(0).unwrap();

    backend.apply_changes(vec![remote1]).unwrap();
    let backend_after_second = backend.clone();
    let changes2 = backend_after_second.get_changes(&[change01.hash]);
    let change12 = *changes2.get(0).unwrap();

    backend.apply_local_change(local2).unwrap();
    let changes3 = backend.get_changes(&[change01.hash, change12.hash]);
    let change23 = changes3.get(0).unwrap();

    expected_change1.time = change01.time;
    expected_change2.time = change12.time;
    expected_change3.time = change23.time;
    expected_change3.deps = vec![change01.hash];

    assert_eq!(change01, &&expected_change1.try_into().unwrap());
    assert_eq!(change12, &expected_change2.try_into().unwrap());
    assert_changes_equal(change23.decode(), expected_change3.clone());
    assert_eq!(change23, &&expected_change3.try_into().unwrap());
}

#[test]
fn test_transform_list_indexes_into_element_ids() {
    let actor: ActorID = "8f389df8fecb4ddc989102321af3578e".try_into().unwrap();
    let remote_actor: ActorID = "9ba21574dc44411b8ce37bc6037a9687".try_into().unwrap();
    let remote1: Change = UncompressedChange {
        actor_id: remote_actor.clone(),
        seq: 1,
        start_op: 1,
        time: 0,
        message: None,
        deps: Vec::new(),
        operations: vec![Op {
            action: protocol::OpType::MakeList,
            value: None,
            datatype: Some(DataType::Undefined),
            key: "birds".into(),
            obj: ObjectID::Root.to_string(),
            pred: Vec::new(),
            insert: false,
        }],
    }
    .try_into()
    .unwrap();

    let remote2: Change = UncompressedChange {
        actor_id: remote_actor.clone(),
        seq: 2,
        start_op: 2,
        time: 0,
        message: None,
        deps: vec![remote1.hash],
        operations: vec![Op {
            action: protocol::OpType::Set,
            value: Some("magpie".into()),
            datatype: Some(DataType::Undefined),
            obj: ObjectID::from(remote_actor.op_id_at(1)).to_string(),
            key: ElementID::Head.into(),
            insert: true,
            pred: Vec::new(),
        }],
    }
    .try_into()
    .unwrap();

    let local1 = UncompressedChange {
        actor_id: actor.clone(),
        seq: 1,
        message: None,
        time: 0,
        deps: vec![remote1.hash],
        start_op: 2,
        operations: vec![Op {
            obj: ObjectID::from(remote_actor.op_id_at(1)).to_string(),
            action: protocol::OpType::Set,
            value: Some("goldfinch".into()),
            key: ElementID::Head.into(),
            datatype: Some(DataType::Undefined),
            insert: true,
            pred: Vec::new(),
        }],
    };
    let local2 = UncompressedChange {
        actor_id: actor.clone(),
        seq: 2,
        message: None,
        deps: Vec::new(),
        time: 0,
        start_op: 3,
        operations: vec![Op {
            obj: ObjectID::from(remote_actor.op_id_at(1)).to_string(),
            action: protocol::OpType::Set,
            value: Some("wagtail".into()),
            key: actor.op_id_at(2).into(),
            insert: true,
            pred: Vec::new(),
            datatype: Some(DataType::Undefined),
        }],
    };

    let local3 = UncompressedChange {
        actor_id: actor.clone(),
        seq: 3,
        message: None,
        deps: vec![remote2.hash],
        time: 0,
        start_op: 4,
        operations: vec![
            Op {
                obj: ObjectID::from(remote_actor.op_id_at(1)).to_string(),
                action: protocol::OpType::Set,
                key: remote_actor.op_id_at(2).into(),
                value: Some("Magpie".into()),
                insert: false,
                datatype: Some(DataType::Undefined),
                pred: vec![remote_actor.op_id_at(2)],
            },
            Op {
                obj: ObjectID::from(remote_actor.op_id_at(1)).to_string(),
                action: protocol::OpType::Set,
                key: actor.op_id_at(2).into(),
                value: Some("Goldfinch".into()),
                insert: false,
                datatype: Some(DataType::Undefined),
                pred: vec![actor.op_id_at(2)],
            },
        ],
    };

    let mut expected_change1 = UncompressedChange {
        actor_id: actor.clone(),
        seq: 1,
        start_op: 2,
        time: 0,
        message: None,
        deps: vec![remote1.hash],
        operations: vec![Op {
            obj: ObjectID::from(remote_actor.op_id_at(1)).to_string(),
            action: protocol::OpType::Set,
            value: Some("goldfinch".into()),
            datatype: Some(DataType::Undefined),
            key: ElementID::Head.into(),
            insert: true,
            pred: Vec::new(),
        }],
    };
    let mut expected_change2 = UncompressedChange {
        actor_id: actor.clone(),
        seq: 2,
        start_op: 3,
        time: 0,
        message: None,
        deps: Vec::new(),
        operations: vec![Op {
            obj: ObjectID::from(remote_actor.op_id_at(1)).to_string(),
            action: protocol::OpType::Set,
            value: Some("wagtail".into()),
            datatype: Some(DataType::Undefined),
            key: actor.op_id_at(2).into(),
            insert: true,
            pred: Vec::new(),
        }],
    };
    let mut expected_change3 = UncompressedChange {
        actor_id: actor.clone(),
        seq: 3,
        start_op: 4,
        time: 0,
        message: None,
        deps: Vec::new(),
        operations: vec![
            Op {
                obj: ObjectID::from(remote_actor.op_id_at(1)).to_string(),
                action: protocol::OpType::Set,
                value: Some("Magpie".into()),
                datatype: Some(DataType::Undefined),
                key: remote_actor.op_id_at(2).into(),
                pred: vec![remote_actor.op_id_at(2)],
                insert: false,
            },
            Op {
                obj: ObjectID::from(remote_actor.op_id_at(1)).to_string(),
                action: protocol::OpType::Set,
                value: Some("Goldfinch".into()),
                datatype: Some(DataType::Undefined),
                key: actor.op_id_at(2).into(),
                pred: vec![actor.op_id_at(2)],
                insert: false,
            },
        ],
    };

    let mut backend = Backend::init();
    backend.apply_changes(vec![remote1.clone()]).unwrap();
    backend.apply_local_change(local1).unwrap();
    let backend_after_first = backend.clone();
    let changes1 = backend_after_first.get_changes(&[remote1.hash]);
    let change12 = *changes1.get(0).unwrap();

    backend.apply_changes(vec![remote2.clone()]).unwrap();
    backend.apply_local_change(local2).unwrap();
    let backend_after_second = backend.clone();
    let changes2 = backend_after_second.get_changes(&[remote2.hash, change12.hash]);
    let change23 = *changes2.get(0).unwrap();

    backend.apply_local_change(local3).unwrap();
    let changes3 = backend.get_changes(&[remote2.hash, change23.hash]);
    let change34 = changes3.get(0).unwrap().decode();

    expected_change1.time = change12.time;
    expected_change2.time = change23.time;
    expected_change2.deps = vec![change12.hash];
    expected_change3.time = change34.time;
    expected_change3.deps = vec![remote2.hash, change23.hash];

    assert_changes_equal(change34, expected_change3);
    assert_eq!(change12, &expected_change1.try_into().unwrap());
    assert_changes_equal(change23.decode(), expected_change2.clone());
    assert_eq!(change23, &expected_change2.try_into().unwrap());
}

#[test]
fn test_handle_list_insertion_and_deletion_in_same_change() {
    let actor: ActorID = "0723d2a1940744868ffd6b294ada813f".try_into().unwrap();
    let local1 = UncompressedChange {
        actor_id: actor.clone(),
        seq: 1,
        message: None,
        time: 0,
        deps: Vec::new(),
        start_op: 1,
        operations: vec![Op {
            obj: ObjectID::Root.to_string(),
            action: protocol::OpType::MakeList,
            key: "birds".into(),
            datatype: None,
            value: None,
            insert: false,
            pred: Vec::new(),
        }],
    };

    let local2 = UncompressedChange {
        actor_id: actor.clone(),
        seq: 2,
        message: None,
        time: 0,
        deps: Vec::new(),
        start_op: 2,
        operations: vec![
            Op {
                obj: ObjectID::from(actor.op_id_at(1)).to_string(),
                action: protocol::OpType::Set,
                key: ElementID::Head.into(),
                insert: true,
                value: Some("magpie".into()),
                datatype: Some(DataType::Undefined),
                pred: Vec::new(),
            },
            Op {
                obj: ObjectID::from(actor.op_id_at(1)).to_string(),
                action: protocol::OpType::Del,
                key: actor.op_id_at(2).into(),
                insert: false,
                value: None,
                datatype: None,
                pred: vec![actor.op_id_at(2)],
            },
        ],
    };

    let mut expected_patch = Patch {
        actor: Some(actor.clone()),
        seq: Some(2),
        max_op: 3,
        clock: hashmap! {
            actor.clone() => 2
        },
        deps: Vec::new(),
        diffs: Some(Diff::Map(MapDiff {
            object_id: ObjectID::Root,
            obj_type: MapType::Map,
            props: hashmap! {
                "birds".into() => hashmap!{
                    actor.op_id_at(1) => Diff::Seq(SeqDiff{
                        object_id: ObjectID::from(actor.op_id_at(1)),
                        obj_type: SequenceType::List,
                        edits: vec![
                            DiffEdit::Insert{index: 0, elem_id: actor.op_id_at(2).into()},
                            DiffEdit::Remove{index: 0},
                        ],
                        props: hashmap!{},
                    })
                }
            },
        })),
    };

    let mut backend = Backend::init();
    backend.apply_local_change(local1).unwrap();
    let patch = backend.apply_local_change(local2).unwrap().0;
    expected_patch.deps = patch.deps.clone();
    assert_eq!(patch, expected_patch);

    let changes = backend.get_changes(&[]);
    assert_eq!(changes.len(), 2);
    let change1 = changes[0].clone();
    let change2 = changes[1].clone();

    let expected_change1 = UncompressedChange {
        actor_id: actor.clone(),
        seq: 1,
        start_op: 1,
        time: change1.time,
        message: None,
        deps: Vec::new(),
        operations: vec![Op {
            obj: ObjectID::Root.to_string(),
            action: protocol::OpType::MakeList,
            value: None,
            datatype: None,
            key: "birds".into(),
            insert: false,
            pred: Vec::new(),
        }],
    }
    .try_into()
    .unwrap();

    let expected_change2 = UncompressedChange {
        actor_id: actor.clone(),
        seq: 2,
        start_op: 2,
        time: change2.time,
        message: None,
        deps: vec![change1.hash],
        operations: vec![
            Op {
                obj: ObjectID::from(actor.op_id_at(1)).to_string(),
                action: protocol::OpType::Set,
                value: Some("magpie".into()),
                datatype: Some(DataType::Undefined),
                key: ElementID::Head.into(),
                insert: true,
                pred: Vec::new(),
            },
            Op {
                obj: ObjectID::from(actor.op_id_at(1)).to_string(),
                action: protocol::OpType::Del,
                value: None,
                datatype: None,
                key: actor.op_id_at(2).into(),
                pred: vec![actor.op_id_at(2)],
                insert: false,
            },
        ],
    }
    .try_into()
    .unwrap();

    assert_eq!(change1, expected_change1);
    assert_eq!(change2, expected_change2);
}

/// Asserts that the changes are equal without respect to order of the hashes
/// in the change dependencies
fn assert_changes_equal(mut change1: UncompressedChange, change2: UncompressedChange) {
    let change2_clone = change2.clone();
    let deps1: HashSet<&ChangeHash> = change1.deps.iter().collect();
    let deps2: HashSet<&ChangeHash> = change2.deps.iter().collect();
    assert_eq!(
        deps1, deps2,
        "The two changes did not have equal dependencies, left: {:?}, right: {:?}",
        deps1, deps2
    );
    change1.deps = change2.deps;
    assert_eq!(change1, change2_clone)
}

use automerge_backend::Backend;
use automerge_frontend::{Frontend, InvalidChangeRequest, InvalidPatch, LocalChange, Path, Value};
use automerge_protocol as amp;
use maplit::hashmap;

fn random_op_id() -> amp::OpID {
    amp::OpID::new(1, &amp::ActorID::random())
}

#[test]
fn use_version_and_sequence_number_from_backend() {
    let mut doc = Frontend::new();
    let remote_actor1 = amp::ActorID::random();
    let remote_actor2 = amp::ActorID::random();

    // This is a remote patch
    let patch = amp::Patch {
        actor: None,
        seq: None,
        clock: hashmap! {
            doc.actor_id.clone() => 4,
            remote_actor1 => 11,
            remote_actor2 => 41,
        },
        deps: Vec::new(),
        diffs: Some(amp::Diff::Map(amp::MapDiff {
            object_id: amp::ObjectID::Root,
            obj_type: amp::MapType::Map,
            props: hashmap! {
                "blackbirds".into() => hashmap!{
                    random_op_id() => amp::Diff::Value(amp::ScalarValue::F64(24.0))
                }
            },
        })),
        max_op: 4,
    };

    // There were no in flight requests so the doc state should be reconciled
    // and should reflect the above patch
    doc.apply_patch(patch).unwrap();

    // Now apply a local patch, this will move the doc into the "waiting for
    // in flight requests" state, which should reflect the change just made.
    let req = doc
        .change::<_, InvalidChangeRequest>(None, |doc| {
            doc.add_change(LocalChange::set(
                Path::root().key("partridges"),
                Value::Primitive(amp::ScalarValue::Int(1)),
            ))?;
            Ok(())
        })
        .unwrap()
        .unwrap();

    let expected_change_request = amp::UncompressedChange {
        actor_id: doc.actor_id,
        seq: 5,
        start_op: 5,
        time: req.time,
        message: None,
        deps: Vec::new(),
        operations: vec![amp::Op {
            action: amp::OpType::Set(amp::ScalarValue::Int(1)),
            obj: amp::ObjectID::Root,
            key: "partridges".into(),
            insert: false,
            pred: Vec::new(),
        }],
        extra_bytes: Vec::new(),
    };

    assert_eq!(req, expected_change_request);
}

#[test]
fn remove_pending_requests_once_handled() {
    let mut doc = Frontend::new();

    // First we add two local changes
    let _req1 = doc
        .change::<_, InvalidChangeRequest>(None, |doc| {
            doc.add_change(LocalChange::set(
                Path::root().key("blackbirds"),
                amp::ScalarValue::Int(24).into(),
            ))?;
            Ok(())
        })
        .unwrap()
        .unwrap();

    let _req2 = doc
        .change::<_, InvalidChangeRequest>(None, |doc| {
            doc.add_change(LocalChange::set(
                Path::root().key("partridges"),
                amp::ScalarValue::Int(1).into(),
            ))?;
            Ok(())
        })
        .unwrap()
        .unwrap();

    // The doc is waiting for those changes to be applied
    assert_eq!(doc.in_flight_requests(), vec![1, 2]);

    // Apply a patch corresponding (via actor ID and seq) to the first change
    doc.apply_patch(amp::Patch {
        actor: Some(doc.actor_id.clone()),
        seq: Some(1),
        clock: hashmap! {
            doc.actor_id.clone() => 1,
        },
        max_op: 4,
        deps: Vec::new(),
        diffs: Some(amp::Diff::Map(amp::MapDiff {
            object_id: amp::ObjectID::Root,
            obj_type: amp::MapType::Map,
            props: hashmap! {
                "blackbirds".into() => hashmap!{
                    random_op_id() => amp::Diff::Value(amp::ScalarValue::Int(24))
                }
            },
        })),
    })
    .unwrap();

    // The doc state should still reflect both local changes as we're still
    // waiting  for the last in flight request to be fulfilled
    assert_eq!(
        doc.state(),
        &Into::<Value>::into(hashmap! {
            "blackbirds".to_string() => amp::ScalarValue::Int(24),
            "partridges".to_string() => amp::ScalarValue::Int(1),
        })
    );
    assert_eq!(doc.in_flight_requests(), vec![2]);

    // Apply a patch corresponding (via actor ID and seq) to the second change
    doc.apply_patch(amp::Patch {
        actor: Some(doc.actor_id.clone()),
        seq: Some(2),
        clock: hashmap! {
            doc.actor_id.clone() => 2,
        },
        max_op: 5,
        deps: Vec::new(),
        diffs: Some(amp::Diff::Map(amp::MapDiff {
            object_id: amp::ObjectID::Root,
            obj_type: amp::MapType::Map,
            props: hashmap! {
                "partridges".into() => hashmap!{
                    random_op_id() => amp::Diff::Value(amp::ScalarValue::Int(1))
                }
            },
        })),
    })
    .unwrap();

    // The doc state should have switched to reconciled
    assert!(doc.in_flight_requests().is_empty());

    // The doc state should still reflect the local changes as they have now
    // been reconciled
    assert_eq!(
        doc.state(),
        &Into::<Value>::into(hashmap! {
            "blackbirds".to_string() => amp::ScalarValue::Int(24),
            "partridges".to_string() => amp::ScalarValue::Int(1),
        })
    );

    assert_eq!(doc.seq, 2);
}

#[test]
fn leave_request_queue_unchanged_on_remote_changes() {
    let remote = amp::ActorID::random();
    let mut doc = Frontend::new();
    // Enqueue a local change, moving the document into the "waiting for in
    // flight requests" state
    let _req1 = doc
        .change::<_, InvalidChangeRequest>(None, |doc| {
            doc.add_change(LocalChange::set(
                Path::root().key("blackbirds"),
                amp::ScalarValue::Int(24).into(),
            ))?;
            Ok(())
        })
        .unwrap()
        .unwrap();

    // The document is now waiting for the above request
    assert_eq!(doc.in_flight_requests(), vec![1]);

    // Apply a remote patch (due to actor ID and seq missing)
    doc.apply_patch(amp::Patch {
        actor: None,
        seq: None,
        max_op: 10,
        clock: hashmap! {
            remote.clone() => 1,
        },
        deps: Vec::new(),
        diffs: Some(amp::Diff::Map(amp::MapDiff {
            object_id: amp::ObjectID::Root,
            obj_type: amp::MapType::Map,
            props: hashmap! {
                "pheasants".into() => hashmap!{
                    random_op_id() => amp::Diff::Value(amp::ScalarValue::Int(2))
                }
            },
        })),
    })
    .unwrap();

    // The doc state should reflect outstanding in flight request and not the
    // remote patch (because we're still waiting for in flight requests)
    assert_eq!(
        doc.state(),
        &Into::<Value>::into(hashmap! {
            "blackbirds".to_string() => amp::ScalarValue::Int(24),
        })
    );
    assert_eq!(doc.in_flight_requests(), vec![1]);

    // Now apply a patch corresponding to the outstanding in flight request
    doc.apply_patch(amp::Patch {
        actor: Some(doc.actor_id.clone()),
        seq: Some(1),
        clock: hashmap! {
            doc.actor_id.clone() => 2,
            remote => 1,
        },
        max_op: 11,
        deps: Vec::new(),
        diffs: Some(amp::Diff::Map(amp::MapDiff {
            object_id: amp::ObjectID::Root,
            obj_type: amp::MapType::Map,
            props: hashmap! {
                "blackbirds".into() => hashmap!{
                    random_op_id() => amp::Diff::Value(amp::ScalarValue::Int(24))
                }
            },
        })),
    })
    .unwrap();

    // The doc state should now reflect both the local and remote changes
    // as the doc is now reconciled (all in flight requests have received a
    // patch)
    assert_eq!(
        doc.state(),
        &Into::<Value>::into(hashmap! {
            "blackbirds".to_string() => amp::ScalarValue::Int(24),
            "pheasants".to_string() => amp::ScalarValue::Int(2),
        })
    );

    assert!(doc.in_flight_requests().is_empty());
    assert_eq!(doc.seq, 2);
}

#[test]
fn dont_allow_out_of_order_request_patches() {
    let mut doc = Frontend::new();
    let _req1 = doc
        .change::<_, InvalidChangeRequest>(None, |doc| {
            doc.add_change(LocalChange::set(
                Path::root().key("blackbirds"),
                amp::ScalarValue::Int(24).into(),
            ))?;
            Ok(())
        })
        .unwrap()
        .unwrap();

    let result = doc.apply_patch(amp::Patch {
        actor: Some(doc.actor_id.clone()),
        seq: Some(2),
        max_op: 8,
        clock: hashmap! {
            doc.actor_id.clone() => 2,
        },
        deps: Vec::new(),
        diffs: Some(amp::Diff::Map(amp::MapDiff {
            object_id: amp::ObjectID::Root,
            obj_type: amp::MapType::Map,
            props: hashmap! {
                "partridges".to_string() => hashmap!{
                    random_op_id() => amp::Diff::Value(amp::ScalarValue::Int(1))
                }
            },
        })),
    });

    assert_eq!(
        result,
        Err(InvalidPatch::MismatchedSequenceNumber {
            expected: 1,
            actual: 2
        })
    );
}

#[test]
fn handle_concurrent_insertions_into_lists() {
    let mut doc = Frontend::new();
    let _req1 = doc
        .change::<_, InvalidChangeRequest>(None, |doc| {
            doc.add_change(LocalChange::set(
                Path::root().key("birds"),
                vec!["goldfinch"].into(),
            ))?;
            Ok(())
        })
        .unwrap()
        .unwrap();

    let birds_id = doc.get_object_id(&Path::root().key("birds")).unwrap();

    // Apply the corresponding backend patch for the above state, document
    // shoudl be reconciled after this
    doc.apply_patch(amp::Patch {
        actor: Some(doc.actor_id.clone()),
        seq: Some(1),
        max_op: 1,
        clock: hashmap! {
            doc.actor_id.clone() => 1,
        },
        deps: Vec::new(),
        diffs: Some(amp::Diff::Map(amp::MapDiff {
            object_id: amp::ObjectID::Root,
            obj_type: amp::MapType::Map,
            props: hashmap! {
                "birds".to_string() => hashmap!{
                    doc.actor_id.op_id_at(1) => amp::Diff::Seq(amp::SeqDiff{
                        object_id: birds_id.clone(),
                        obj_type: amp::SequenceType::List,
                        edits: vec![amp::DiffEdit::Insert{ index: 0, elem_id: doc.actor_id.op_id_at(1).into() }],
                        props: hashmap!{
                            0 => hashmap!{
                                random_op_id() => amp::Diff::Value("goldfinch".into())
                            }
                        }
                    })
                }
            },
        })),
    })
    .unwrap();

    assert_eq!(
        doc.state(),
        &Into::<Value>::into(hashmap! {"birds".to_string() => vec!["goldfinch"]})
    );
    assert!(doc.in_flight_requests().is_empty());

    // Now add another change which updates the same list, this results in an
    // in flight reuest
    let _req2 = doc
        .change::<_, InvalidChangeRequest>(None, |doc| {
            doc.add_change(LocalChange::insert(
                Path::root().key("birds").index(0),
                "chaffinch".into(),
            ))?;
            doc.add_change(LocalChange::insert(
                Path::root().key("birds").index(2),
                "greenfinch".into(),
            ))?;
            Ok(())
        })
        .unwrap()
        .unwrap();

    assert_eq!(
        doc.state(),
        &Into::<Value>::into(
            hashmap! {"birds".to_string() => vec!["chaffinch", "goldfinch", "greenfinch"]}
        )
    );

    let remote = amp::ActorID::random();

    // Apply a patch which does not take effect because we're still waiting
    // for the in flight requests to be responded to
    doc.apply_patch(amp::Patch {
        clock: hashmap! {
            doc.actor_id.clone() => 1,
            remote.clone() => 1,
        },
        max_op: 3,
        actor: None,
        seq: None,
        deps: Vec::new(),
        diffs: Some(amp::Diff::Map(amp::MapDiff {
            object_id: amp::ObjectID::Root,
            obj_type: amp::MapType::Map,
            props: hashmap! {
                "birds".into() => hashmap!{
                    doc.actor_id.op_id_at(1) => amp::Diff::Seq(amp::SeqDiff{
                        object_id: birds_id.clone(),
                        obj_type: amp::SequenceType::List,
                        edits: vec![amp::DiffEdit::Insert{ index: 1, elem_id: remote.op_id_at(1).into()}],
                        props: hashmap!{
                            1 => hashmap!{
                                remote.op_id_at(1) => amp::Diff::Value("bullfinch".into())
                            }
                        }
                    })
                }
            },
        })),
    })
    .unwrap();

    // Check that the doc state hasn't been updated yet
    assert_eq!(
        doc.state(),
        &Into::<Value>::into(
            hashmap! {"birds".to_string() => vec!["chaffinch", "goldfinch", "greenfinch"]}
        )
    );

    // Now apply a patch acknowledging the in flight request
    doc.apply_patch(amp::Patch {
        actor: Some(doc.actor_id.clone()),
        seq: Some(2),
        max_op: 3,
        clock: hashmap!{
            doc.actor_id.clone() => 2,
            remote => 1,
        },
        deps: Vec::new(),
        diffs: Some(amp::Diff::Map(amp::MapDiff{
            object_id: amp::ObjectID::Root,
            obj_type: amp::MapType::Map,
            props: hashmap!{
                "birds".to_string() => hashmap!{
                    doc.actor_id.op_id_at(1) => amp::Diff::Seq(amp::SeqDiff{
                        object_id: birds_id,
                        obj_type: amp::SequenceType::List,
                        edits: vec![
                            amp::DiffEdit::Insert { index: 0, elem_id: doc.actor_id.op_id_at(2).into() },
                            amp::DiffEdit::Insert{ index: 2, elem_id: doc.actor_id.op_id_at(3).into() },
                        ],
                        props: hashmap!{
                            0 => hashmap!{
                                doc.actor_id.op_id_at(2) => amp::Diff::Value("chaffinch".into()),
                            },
                            2 => hashmap!{
                                doc.actor_id.op_id_at(3) => amp::Diff::Value("greenfinch".into()),
                            }
                        }
                    })
                }
            }
        }))
    }).unwrap();

    assert!(doc.in_flight_requests().is_empty());
    assert_eq!(
        doc.state(),
        &Into::<Value>::into(
            hashmap! {"birds".to_string() => vec!["chaffinch", "goldfinch", "greenfinch", "bullfinch"]}
        )
    )
}

#[test]
fn allow_interleaving_of_patches_and_changes() {
    let mut doc = Frontend::new();
    let req1 = doc
        .change::<_, InvalidChangeRequest>(None, |doc| {
            doc.add_change(LocalChange::set(
                Path::root().key("number"),
                amp::ScalarValue::Int(1).into(),
            ))?;
            Ok(())
        })
        .unwrap()
        .unwrap();

    let req2 = doc
        .change::<_, InvalidChangeRequest>(None, |doc| {
            doc.add_change(LocalChange::set(
                Path::root().key("number"),
                amp::ScalarValue::Int(2).into(),
            ))?;
            Ok(())
        })
        .unwrap()
        .unwrap();

    assert_eq!(
        req1,
        amp::UncompressedChange {
            actor_id: doc.actor_id.clone(),
            seq: 1,
            start_op: 1,
            message: None,
            time: req1.time,
            deps: Vec::new(),
            operations: vec![amp::Op {
                action: amp::OpType::Set(amp::ScalarValue::Int(1)),
                obj: amp::ObjectID::Root,
                key: "number".into(),
                insert: false,
                pred: Vec::new(),
            }],
            extra_bytes: Vec::new(),
        }
    );

    assert_eq!(
        req2,
        amp::UncompressedChange {
            actor_id: doc.actor_id.clone(),
            seq: 2,
            start_op: 2,
            message: None,
            time: req2.time,
            deps: Vec::new(),
            operations: vec![amp::Op {
                action: amp::OpType::Set(amp::ScalarValue::Int(2)),
                obj: amp::ObjectID::Root,
                key: "number".into(),
                insert: false,
                pred: vec![doc.actor_id.op_id_at(1)],
            }],
            extra_bytes: Vec::new(),
        }
    );

    let mut backend = Backend::init();
    let (patch1, _) = backend.apply_local_change(req1).unwrap();
    doc.apply_patch(patch1).unwrap();

    let req3 = doc
        .change::<_, InvalidChangeRequest>(None, |doc| {
            doc.add_change(LocalChange::set(
                Path::root().key("number"),
                amp::ScalarValue::Int(3).into(),
            ))?;
            Ok(())
        })
        .unwrap()
        .unwrap();

    assert_eq!(
        req3,
        amp::UncompressedChange {
            actor_id: doc.actor_id.clone(),
            seq: 3,
            start_op: 3,
            message: None,
            time: req3.time,
            deps: Vec::new(),
            operations: vec![amp::Op {
                action: amp::OpType::Set(amp::ScalarValue::Int(3)),
                obj: amp::ObjectID::Root,
                key: "number".into(),
                insert: false,
                pred: vec![doc.actor_id.op_id_at(2)],
            }],
            extra_bytes: Vec::new(),
        }
    );
}

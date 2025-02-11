use std::time::Duration;

use automerge::{Backend, Frontend, InvalidChangeRequest, LocalChange, Path, Primitive, Value};
use automerge_backend::SyncState;
use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn sync(
    a: &mut Backend,
    b: &mut Backend,
    a_sync_state: &mut SyncState,
    b_sync_state: &mut SyncState,
) {
    const MAX_ITER: u32 = 10;
    #[allow(unused_assignments)]
    let mut a_to_b_msg = None;
    #[allow(unused_assignments)]
    let mut b_to_a_msg = None;
    let mut i = 0;
    loop {
        a_to_b_msg = a.generate_sync_message(a_sync_state);

        if let Some(message) = a_to_b_msg.clone() {
            let _patch = b.receive_sync_message(b_sync_state, message).unwrap();
        }

        b_to_a_msg = b.generate_sync_message(b_sync_state);

        if let Some(message) = b_to_a_msg.clone() {
            let _patch = a.receive_sync_message(a_sync_state, message).unwrap();
        }

        i += 1;
        if i > MAX_ITER {
            panic!(
       "Did not synchronize within {} iterations. Do you have a bug causing an infinite loop?",MAX_ITER
      )
        }
        if a_to_b_msg.is_none() && b_to_a_msg.is_none() {
            break;
        }
    }
}

fn sync_per_change(count: u32, sync_interval: u32) {
    let mut n1 = Backend::init();
    let mut n2 = Backend::init();
    let mut s1 = SyncState::default();
    let mut s2 = SyncState::default();

    let mut f1 = Frontend::new_with_timestamper(Box::new(|| None));

    let change = f1
        .change::<_, _, InvalidChangeRequest>(None, |d| {
            d.add_change(LocalChange::set(
                Path::root().key("n"),
                Value::Sequence(vec![]),
            ))?;
            Ok(())
        })
        .unwrap()
        .1
        .unwrap();
    let (patch, _) = n1.apply_local_change(change).unwrap();
    f1.apply_patch(patch).unwrap();

    for i in 0..count {
        let change = f1
            .change::<_, _, InvalidChangeRequest>(None, |d| {
                d.add_change(LocalChange::insert(
                    Path::root().key("n").index(i),
                    Value::Primitive(Primitive::Uint(i as u64)),
                ))?;
                Ok(())
            })
            .unwrap()
            .1
            .unwrap();
        let (patch, _) = n1.apply_local_change(change).unwrap();
        f1.apply_patch(patch).unwrap();

        if i % sync_interval == sync_interval - 1 {
            sync(&mut n1, &mut n2, &mut s1, &mut s2);
        }
    }
}

pub fn sync_with_changes_matrix(c: &mut Criterion) {
    let mut group = c.benchmark_group("Sync with changes");

    for count in [1000, 2000, 5000, 10_000].iter() {
        for interval in [1, 10, 100, 1000, 10_000].iter().rev() {
            if interval <= count {
                group.throughput(criterion::Throughput::Elements((count / interval) as u64));
                group.bench_function(
                    format!(
                        "{} changes, syncing every {}, {} syncs total",
                        count,
                        interval,
                        count / interval
                    ),
                    |b| {
                        b.iter(|| {
                            #[allow(clippy::unit_arg)]
                            black_box(sync_per_change(*count, *interval))
                        })
                    },
                );
            }
        }
    }

    group.finish();
}

criterion_group! {
    name = benches;
    config = Criterion::default().sample_size(10).measurement_time(Duration::from_secs(30));
    targets = sync_with_changes_matrix
}
criterion_main!(benches);

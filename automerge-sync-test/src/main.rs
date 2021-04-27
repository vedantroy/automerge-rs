use std::time::{Duration, Instant};

use automerge::{Backend, Frontend, InvalidChangeRequest, LocalChange, Path, Primitive, Value};
use automerge_backend::SyncState;
use criterion::black_box;

fn sync(
    a: &mut Backend,
    b: &mut Backend,
    a_sync_state: &mut SyncState,
    b_sync_state: &mut SyncState,
) -> (Duration, Duration) {
    const MAX_ITER: u32 = 10;
    #[allow(unused_assignments)]
    let mut a_to_b_msg = None;
    #[allow(unused_assignments)]
    let mut b_to_a_msg = None;
    let mut i = 0;
    let mut gen_duration = Duration::default();
    let mut recv_duration = Duration::default();
    loop {
        let start = Instant::now();
        a_to_b_msg = a.generate_sync_message(a_sync_state);
        gen_duration += start.elapsed();

        if let Some(message) = a_to_b_msg.clone() {
            let start = Instant::now();
            let _patch = b.receive_sync_message(b_sync_state, message).unwrap();
            recv_duration += start.elapsed();
        }

        let start = Instant::now();
        b_to_a_msg = b.generate_sync_message(b_sync_state);
        gen_duration += start.elapsed();

        if let Some(message) = b_to_a_msg.clone() {
            let start = Instant::now();
            let _patch = a.receive_sync_message(a_sync_state, message).unwrap();
            recv_duration += start.elapsed();
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
    (gen_duration, recv_duration)
}

fn sync_per_change(count: u32, sync_interval: u32) -> (Duration, Duration, Duration) {
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

    let mut sync_duration = Duration::default();
    let mut gen_duration = Duration::default();
    let mut recv_duration = Duration::default();

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
            let start = Instant::now();
            let (gen_dur, recv_dur) = sync(&mut n1, &mut n2, &mut s1, &mut s2);
            sync_duration += start.elapsed();
            gen_duration += gen_dur;
            recv_duration += recv_dur;
        }
    }
    (sync_duration, gen_duration, recv_duration)
}

fn sync_matrix() {
    for count in [1000, 2000, 5000, 10_000].iter() {
        for interval in [1, 10, 100, 1000, 10_000, 100_000].iter().rev() {
            if interval <= count {
                let start = Instant::now();
                let (s, g, r) = black_box(sync_per_change(*count, *interval));
                println!(
                    "{:>6} changes, syncing every {:>6}, {:>6} syncs total, total time {:?}, syncing time {:?} {:?} {:?}",
                    count,
                    interval,
                    count / interval,
                    start.elapsed(),
                    s,
                    g,
                    r,
                );
            }
        }
    }
}

fn main() {
    sync_matrix()
}

use automerge::{Backend, Frontend, LocalChange, Path, Primitive, Value};
use rand::{distributions::Alphanumeric, thread_rng, Rng};

fn main() {
    for frontends_count in &[2, 5, 10] {
        for changes_count in &[10, 100, 200] {
            for sync_prob in &[0.2, 0.5, 0.8] {
                let backend = measure_text(*frontends_count, *changes_count, *sync_prob);

                println!("frontends: {}", frontends_count);
                println!("changes: {}", changes_count);
                println!("sync prob: {}", sync_prob);
                println!();

                println!("text");
                let old = backend.save().unwrap_or_default().len();
                let new = backend.new_save().unwrap_or_default().len();
                println!("old save: {}", old);
                println!("new save: {}", new);
                println!("ratio: {:.4}", new as f64 / old as f64);
                println!();

                let backend = measure_map(*frontends_count, *changes_count, *sync_prob);
                println!("map");
                let old = backend.save().unwrap_or_default().len();
                let new = backend.new_save().unwrap_or_default().len();
                println!("old save: {}", old);
                println!("new save: {}", new);
                println!("ratio: {:.4}", new as f64 / old as f64);
                println!();
                println!();
            }
        }
    }
}

fn measure_map(frontends_count: usize, changes_count: usize, sync_prob: f64) -> Backend {
    let mut frontends = (0..frontends_count)
        .map(|_| Frontend::new())
        .collect::<Vec<_>>();
    let mut b = automerge::Backend::init();

    for _ in 0..changes_count {
        let f_i = thread_rng().gen_range(0, frontends.len());
        let f = &mut frontends[f_i];

        if thread_rng().gen_bool(sync_prob) {
            let patch = b.get_patch().unwrap();
            f.apply_patch(patch).unwrap();
        }

        let random_key: String = rand::thread_rng()
            .sample_iter(&Alphanumeric)
            .take(3)
            .map(char::from)
            .collect();
        let random_string: String = rand::thread_rng()
            .sample_iter(&Alphanumeric)
            .take(10)
            .map(char::from)
            .collect();

        let ((), change) = f
            .change::<_, _, std::convert::Infallible>(None, |d| {
                d.add_change(LocalChange::set(
                    Path::root().key(random_key),
                    Value::Primitive(Primitive::Str(random_string)),
                ))
                .unwrap();
                Ok(())
            })
            .unwrap();
        b.apply_local_change(change.unwrap()).unwrap();
    }

    b
}

fn measure_text(frontends_count: usize, changes_count: usize, sync_prob: f64) -> Backend {
    let mut frontends = (0..frontends_count)
        .map(|_| Frontend::new())
        .collect::<Vec<_>>();
    let mut b = automerge::Backend::init();

    for _ in 0..changes_count {
        let f_i = thread_rng().gen_range(0, frontends.len());
        let f = &mut frontends[f_i];

        if thread_rng().gen_bool(sync_prob) {
            let patch = b.get_patch().unwrap();
            f.apply_patch(patch).unwrap();
        }

        let random_string: Vec<String> = rand::thread_rng()
            .sample_iter(&Alphanumeric)
            .take(thread_rng().gen_range(2, 20))
            .map(char::from)
            .map(|c| c.to_string())
            .collect();

        let ((), change) = f
            .change::<_, _, std::convert::Infallible>(None, |d| {
                let start = if let Some(Value::Text(c)) = d.value_at_path(&Path::root().key("text"))
                {
                    c.len()
                } else {
                    d.add_change(LocalChange::set(
                        Path::root().key("text"),
                        Value::Text(Vec::new()),
                    ))
                    .unwrap();
                    0
                };
                for (i, item) in random_string.iter().enumerate() {
                    d.add_change(LocalChange::insert(
                        Path::root().key("text").index(start as u32 + i as u32),
                        Value::Primitive(Primitive::Str(item.clone())),
                    ))
                    .unwrap();
                }

                for i in 0..thread_rng().gen_range(0, 5) {
                    if start > 10 {
                        d.add_change(LocalChange::delete(
                            Path::root()
                                .key("text")
                                .index(thread_rng().gen_range(0, start - i - 1) as u32),
                        ))
                        .unwrap();
                    }
                }

                Ok(())
            })
            .unwrap();
        b.apply_local_change(change.unwrap()).unwrap();
    }

    b
}

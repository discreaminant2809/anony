mod threading;

use std::{
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
    num::NonZeroUsize,
    sync::{Arc, Mutex},
};

use anony::r#struct;

use crate::threading::ThreadPool;

fn main() {
    let o1 = r#struct! {
        name: "foo".to_owned(),
        age: 123,
    };

    let gpa = 4.0;
    let o2 = r#struct! {
        name: "foo".to_owned(),
        gpa,
    };

    println!("{:?}. {:?}", o1, o2);

    let serialized = serde_json::ser::to_string(&o1).unwrap();
    println!("{serialized}");

    let name = o1.name;
    let age = o1.age;

    assert_eq!((&name[..], age), ("foo", 123));

    let o3 = r#struct! {
        x: 3,
        y: "4".to_owned(),
    };
    let mut o4 = o3.clone();
    assert_eq!(o3, o4);
    o4.x += 1;
    assert!(o3 < o4);

    let o5 = r#struct! {
        x: 3,
        y: "4".to_owned(),
    };
    // assert_eq!(o3, o5); // Compile error! They have entirely different types

    let mut hasher = DefaultHasher::new();
    o5.hash(&mut hasher);
    println!("Hash value of o5: {}", hasher.finish());

    // let's use the ThreadPool we've implemented with the assistance of `r#struct!` macro!
    let mut sum = Arc::new(Mutex::new(0_u32));
    let mut thread_pool = ThreadPool::new(NonZeroUsize::new(10).unwrap()).unwrap();

    for i in 0..100 {
        let sum = sum.clone();
        thread_pool.spawn(move || {
            const COUNT: usize = 100;
            let start = i * COUNT;
            for j in start..start + COUNT {
                *sum.lock().unwrap() += j as u32;
            }
        });
    }

    thread_pool.shutdown();

    assert_eq!(
        *Arc::get_mut(&mut sum).unwrap().get_mut().unwrap(),
        49_995_000
    );
}

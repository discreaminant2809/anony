mod threading;

use std::{
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
    num::NonZeroUsize,
    sync::{Arc, Mutex},
};

use anony::{r#struct, tuple};

use crate::threading::ThreadPool;

fn main() {
    demonstrate_struct();
    demonstrate_tuple();
}

fn demonstrate_struct() {
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

fn demonstrate_tuple() {
    let o1 = tuple!(12.3, 45.6);
    println!("{o1:?}");
    let o1 = tuple!(12.3);
    println!("{o1:?}");
    let o1 = tuple!();
    println!("{o1:?}");

    let v = vec![2, 4, 6];
    let o1 = tuple!("132", v,);
    let v = o1.1;
    assert_eq!(v, [2, 4, 6]);

    let mut hasher = DefaultHasher::new();
    // Compile error!
    // (
    //     2,
    //     "fgh",
    //     [4, 2, -1],
    //     2,
    //     "fgh",
    //     [4, 2, -1],
    //     2,
    //     "fgh",
    //     [4, 2, -1],
    //     2,
    //     "fgh",
    //     [4, 2, -1],
    //     "3",
    // )
    //     .hash(&mut hasher);
    tuple!(
        2,
        "fgh",
        [4, 2, -1],
        2,
        "fgh",
        [4, 2, -1],
        2,
        "fgh",
        [4, 2, -1],
        2,
        "fgh",
        [4, 2, -1],
        "3"
    )
    .hash(&mut hasher);
    println!("Hash value of o5: {}", hasher.finish());

    for window in (0..100)
        .map(|i| tuple!(i, 0))
        .collect::<Vec<_>>()
        .windows(2)
    {
        assert!(window[0].0 < window[1].0);
    }

    let o1 = tuple!(
        2,
        "fgh",
        [4, 2, -1],
        2,
        "fgh",
        [4, 2, -1],
        2,
        "fgh",
        [4, 2, -1],
        2,
        "fgh",
        [4, 2, -1],
        "fgh",
        [4, 2, -1],
        2,
        "fgh",
        [4, 2, -1],
        "3"
    );
    // Compile error if you uncomment the below!
    // let o1 = (
    //     2,
    //     "fgh",
    //     [4, 2, -1],
    //     2,
    //     "fgh",
    //     [4, 2, -1],
    //     2,
    //     "fgh",
    //     [4, 2, -1],
    //     2,
    //     "fgh",
    //     [4, 2, -1],
    //     "fgh",
    //     [4, 2, -1],
    //     2,
    //     "fgh",
    //     [4, 2, -1],
    //     "3",
    // );
    let serialized = serde_json::ser::to_string(&o1).unwrap();
    println!("{serialized}");

    // And they're the only main advantages of it.
    // For 99.99% of the time, just use a normal tuple.
}

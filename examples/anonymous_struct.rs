use std::{
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
};

use anony::r#struct;

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
}

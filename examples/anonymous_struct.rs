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
    assert_eq!(serialized, r#"{"name":"foo","age":123}"#);

    let name = o1.name;
    let age = o1.age;

    assert_eq!((&name[..], age), ("foo", 123));
}

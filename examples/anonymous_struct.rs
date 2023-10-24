use anony::r#struct;

fn main() {
    let o1 = r#struct! {
        name: "foo".to_owned(),
        age: 123,
    };

    let o2 = r#struct! {
        name: "foo".to_owned(),
        gpa: 4.0,
        // fut: async {}
    };

    println!("{:?}. {:?}", o1, o2);

    let serialized = serde_json::ser::to_string(&o1).unwrap();
    println!("{serialized}");
}
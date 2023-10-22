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

    let _greater = {
        let closure = || {
            // let _ = Inner(|| todo!());
            o1.name.clone()
        };

        {
            enum Anonymous<F: Fn() -> String> {
                Inner(Inner<F>)
            }
    
            struct Inner<F: Fn() -> String>(F);
    
            impl<F: Fn() -> String> Greet for Anonymous<F> {
                fn great(&self) -> String {
                    match self {
                        Anonymous::Inner(inner) => inner.0(),
                    }
                }
            }
    
            Anonymous::Inner(Inner(closure))
        }
    };

    // let _ = greater.0;
}

trait Greet {
    fn great(&self) -> String;
}
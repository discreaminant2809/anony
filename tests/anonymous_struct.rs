use std::pin::pin;

use anony::r#struct;

#[test]
#[ignore = "test correct references"]
fn must_use_correct_references() {
    let x = pin!(r#struct! {});
    let x = x.into_ref();
    x.project_ref();
    let x = pin!(r#struct! {});
    x.project_mut();

    let x = pin!(r#struct! {
        a: 1,
    });
    let x = x.into_ref();
    x.project_ref();
    let x = pin!(r#struct! {
        a: 1,
    });
    x.project_mut();

    let x = pin!(r#struct! {
        a: 1,
        b: 2,
    });
    let x = x.into_ref();
    x.project_ref();
    let x = pin!(r#struct! {
        a: 1,
        b: 2,
    });
    x.project_mut();
}

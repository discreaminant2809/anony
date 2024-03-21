use std::pin::pin;

use anony::tuple;

#[test]
#[ignore = "test correct references"]
fn must_use_correct_references() {
    let x = pin!(tuple!());
    let x = x.into_ref();
    x.project_ref();
    let x = pin!(tuple!());
    x.project_mut();

    let x = pin!(tuple!(1));
    let x = x.into_ref();
    x.project_ref();
    let x = pin!(tuple!(1));
    x.project_mut();

    let x = pin!(tuple!(1, 2));
    let x = x.into_ref();
    x.project_ref();
    let x = pin!(tuple!(1, 2));
    x.project_mut();
}

use std::{
    future::{pending, Future},
    time::Duration,
};

use anony::join;
use tokio::{select, spawn, time::sleep};

#[tokio::test]
async fn join_duration() {
    let fut = join!(
        sleep(Duration::from_secs(3)),
        sleep(Duration::from_secs(3)),
        // This's the longest, so the whole future should be ended in 4 seconds
        sleep(Duration::from_secs(4)),
        sleep(Duration::from_secs(1)),
    );

    select! {
        _ = fut => {

        }

        _ = sleep(Duration::from_secs_f64(4.1)) => {
            panic!("too long")
        }
    }
}

#[tokio::test]
async fn join_result() {
    let fut = join!(spawn(async { 3 }), spawn(async { "123" }));

    assert!(matches!(fut.await, (Ok(3), Ok("123"))))
}

#[test]
#[ignore = "testing trait implementation"]
fn unpin_impl() {
    use std::future::{pending, ready};
    fn assert_val_impl_unpin(_val: &impl Unpin) {}

    assert_val_impl_unpin(&join!(
        pending::<Vec<()>>(),
        // the output is `!Unpin`, but the `join!` must be `Unpin` also since `Ready` is `Unpin`
        ready(async {}),
    ))
}

#[test]
#[ignore = "testing return type of `join!`"]
fn output_type() {
    fn assert_output_type<F: Future<Output = T>, T>(_x: &F) {}

    assert_output_type::<_, (Vec<()>, ())>(&join!(
        pending::<Vec<()>>(),
        sleep(Duration::default())
    ));

    assert_output_type::<_, ()>(&join!());
    assert_output_type::<_, (i32,)>(&join!(async { 2 }));
    assert_output_type::<_, (i32,)>(&async { tokio::join!(async { 2 }) }); // `tokio`'s one is the same as us
    assert_output_type::<_, (i32,)>(&async { futures::join!(async { 2 }) }); // the same for `futures`'s
}

#![allow(deprecated)]

use std::{
    cell::Cell,
    future::{Future, IntoFuture, pending},
    marker::PhantomPinned,
    pin::{Pin, pin},
    rc::Rc,
    task::{Context, Poll},
    time::Duration,
};

use anony::join_cyclic;
use futures::task::{noop_waker, noop_waker_ref};
use tokio::{select, spawn, task::spawn_blocking, time::sleep};

async fn assert_sleep_correct_dur(
    fut: impl IntoFuture,
    desired_dur_secs_f64: f64,
    delta_secs_f64: f64,
) {
    let test_fut_is_later = Cell::new(false);

    let lower_bound_sleeper = async {
        sleep(Duration::from_secs_f64(
            desired_dur_secs_f64 - delta_secs_f64,
        ))
        .await;
        test_fut_is_later.set(true);
    };

    let in_bound_sleeper = async {
        fut.await;
        assert!(test_fut_is_later.get(), "too early");
    };

    select! {
        _ = async { futures::join!(lower_bound_sleeper, in_bound_sleeper) } => {}

        _ = sleep(Duration::from_secs_f64(desired_dur_secs_f64 + delta_secs_f64)) => {
            // we failed!
            panic!("too late");
        }
    }
}

#[tokio::test]
async fn join_duration() {
    let fut = join_cyclic!(
        sleep(Duration::from_millis(300)),
        sleep(Duration::from_millis(300)),
        // This's the longest, so the whole future should be ended in (approximately) 0.04 seconds.
        async {
            sleep(Duration::from_millis(200)).await;
            sleep(Duration::from_millis(200)).await;
        },
        sleep(Duration::from_millis(100)),
    );

    assert_sleep_correct_dur(fut, 0.4, 0.01).await;
}

#[tokio::test]
async fn join_result() {
    let fut = join_cyclic!(
        spawn(async { 3 }),
        spawn(async { "123" }),
        spawn_blocking(|| panic!())
    );

    assert!(matches!(fut.await, (Ok(3), Ok("123"), Err(_))))
}

#[tokio::test]
async fn join_1_ary() {
    let fut = async {
        sleep(Duration::from_millis(1)).await;
        12
    };

    assert_eq!(join_cyclic!(fut).await, (12,));
    // join_cyclic!(async { Join::Inner(async {}).await }).await;
}

#[test]
fn should_poll_all_and_in_correct_ord() {
    thread_local! {
        // This number will keep incrementing for each controlled future being polled
        // We use it to test whether the polling routine is in the correct sequence
        static POLL_NUMBER: Cell<usize> = const { Cell::new(0) };
    }

    #[derive(Clone, Default)]
    struct ControlledFuture {
        should_complete: Rc<Cell<bool>>,
        has_polled_me: Rc<Cell<bool>>,
        poll_number: Rc<Cell<usize>>,
    }

    impl Future for ControlledFuture {
        type Output = ();

        fn poll(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<Self::Output> {
            self.has_polled_me.set(true);
            self.poll_number
                .set(POLL_NUMBER.replace(POLL_NUMBER.get() + 1));
            if self.should_complete.get() {
                Poll::Ready(())
            } else {
                Poll::Pending
            }
        }
    }

    let waker = noop_waker();
    let mut cx = Context::from_waker(&waker);
    let monitor: (
        ControlledFuture,
        ControlledFuture,
        ControlledFuture,
        ControlledFuture,
    ) = Default::default();
    let mut joint = pin!(join_cyclic!(
        monitor.0.clone(),
        monitor.1.clone(),
        monitor.2.clone(),
        monitor.3.clone(),
    ));

    // #1: assume that no one has completed yet
    assert!(joint.as_mut().poll(&mut cx).is_pending());
    assert!(monitor.0.has_polled_me.take()); // to set the value to `false` and assert that the previous is `true` in one line
    assert!(monitor.1.has_polled_me.take());
    assert!(monitor.2.has_polled_me.take());
    assert!(monitor.3.has_polled_me.take());
    assert_eq!(monitor.0.poll_number.get(), 0); // `join_cyclic!` will shirt 1 future to be polled first!
    assert_eq!(monitor.1.poll_number.get(), 1);
    assert_eq!(monitor.2.poll_number.get(), 2);
    assert_eq!(monitor.3.poll_number.get(), 3);

    // #1.5: let the `join_cyclic` cycles around the first-poller!
    assert!(joint.as_mut().poll(&mut cx).is_pending());
    assert_eq!(monitor.0.poll_number.get(), 7);
    assert_eq!(monitor.1.poll_number.get(), 4); // the 2nd one got polled first instead now!
    assert_eq!(monitor.2.poll_number.get(), 5);
    assert_eq!(monitor.3.poll_number.get(), 6);

    assert!(joint.as_mut().poll(&mut cx).is_pending());
    assert_eq!(monitor.0.poll_number.get(), 10);
    assert_eq!(monitor.1.poll_number.get(), 11);
    assert_eq!(monitor.2.poll_number.get(), 8); // now the 3rd one!
    assert_eq!(monitor.3.poll_number.get(), 9);

    assert!(joint.as_mut().poll(&mut cx).is_pending());
    assert_eq!(monitor.0.poll_number.get(), 13);
    assert_eq!(monitor.1.poll_number.get(), 14);
    assert_eq!(monitor.2.poll_number.get(), 15);
    assert_eq!(monitor.3.poll_number.get(), 12);

    // #2: assume that one of these futures has completed, while the others haven't
    monitor.2.should_complete.set(true);
    assert!(joint.as_mut().poll(&mut cx).is_pending());
    assert!(monitor.0.has_polled_me.take());
    assert!(monitor.1.has_polled_me.take());
    assert!(monitor.2.has_polled_me.take());
    assert!(monitor.3.has_polled_me.take());
    assert_eq!(monitor.0.poll_number.get(), 16);
    assert_eq!(monitor.1.poll_number.get(), 17);
    assert_eq!(monitor.2.poll_number.get(), 18);
    assert_eq!(monitor.3.poll_number.get(), 19);

    // #3: assume that all of these futures have completed
    monitor.0.should_complete.set(true);
    monitor.1.should_complete.set(true);
    monitor.3.should_complete.set(true);
    assert!(joint.as_mut().poll(&mut cx).is_ready());
    assert!(monitor.0.has_polled_me.take());
    assert!(monitor.1.has_polled_me.take());
    assert!(!monitor.2.has_polled_me.take()); // it has completed in the #2, which `MaybeDone` is in the `Ready` state,
    // in which the wrapped future won't actually be polled again
    assert!(monitor.3.has_polled_me.take());
    assert_eq!(monitor.0.poll_number.get(), 22);
    assert_eq!(monitor.1.poll_number.get(), 20);
    assert_eq!(monitor.2.poll_number.get(), 18); // since it didn't get polled, the poll number stays the same
    assert_eq!(monitor.3.poll_number.get(), 21);

    // #4: poll after completion!
    assert!(
        std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| joint.poll(&mut cx))).is_err()
    );
}

#[test]
#[should_panic = "`join_cyclic!` future polled after completion"]
fn panic_correct_msg() {
    let mut cx = Context::from_waker(noop_waker_ref());
    let fut = join_cyclic!(async {}, async {});
    let mut fut = pin!(fut);

    let _ = fut.as_mut().poll(&mut cx);
    let _ = fut.poll(&mut cx); // panic here
}

#[test]
#[ignore = "testing trait implementation"]
fn unpin_impl() {
    use std::future::{pending, ready};
    fn assert_val_impl_unpin(_val: &impl Unpin) {}

    assert_val_impl_unpin(&join_cyclic!(
        pending::<Vec<()>>(),
        // the output is `!Unpin`, but the `join!` must still be `Unpin` also since `Ready` is `Unpin` regardless of the output
        ready(PhantomPinned),
    ))
}

#[test]
#[ignore = "testing return type of `join_cyclic!`"]
fn output_type() {
    fn assert_output_type<T>(_x: &impl Future<Output = T>) {}

    assert_output_type::<(Vec<()>, ())>(&join_cyclic!(
        pending::<Vec<()>>(),
        sleep(Duration::default())
    ));

    assert_output_type::<()>(&join_cyclic!());
    assert_output_type::<(i32,)>(&join_cyclic!(async { 2 }));
    // assert_output_type::<_, (i32,)>(&async { tokio::join!(async { 2 }) }); // `tokio`'s one is the same as us
    // assert_output_type::<_, (i32,)>(&async { futures::join!(async { 2 }) }); // the same for `futures`'s
}

#[test]
#[ignore = "testing `IntoFuture` acceptance"]
#[allow(unused_must_use)]
fn into_future_acceptance() {
    fn _f(into_fut: impl std::future::IntoFuture + Copy) {
        join_cyclic!(into_fut);
        join_cyclic!(into_fut, into_fut);
        join_cyclic!(into_fut, into_fut, into_fut);
    }
}

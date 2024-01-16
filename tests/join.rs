use std::{
    cell::Cell,
    future::{pending, Future},
    marker::PhantomPinned,
    pin::{pin, Pin},
    rc::Rc,
    task::{Context, Poll},
    time::Duration,
};

use anony::join;
use futures::FutureExt;
use noop_waker::noop_waker;
use tokio::{select, spawn, task::spawn_blocking, time::sleep};

#[tokio::test]
async fn join_duration() {
    let fut = join!(
        sleep(Duration::from_secs(3)),
        sleep(Duration::from_secs(3)),
        // This's the longest, so the whole future should be ended in (approximately) 4 seconds
        sleep(Duration::from_secs(4)),
        sleep(Duration::from_secs(1)),
    );

    select! {
        _ = fut => {
            // we passed!
        }

        _ = sleep(Duration::from_secs_f64(4.1)) => {
            // we failed!
            panic!("too long");
        }
    }
}

#[tokio::test]
async fn join_result() {
    let fut = join!(
        spawn(async { 3 }),
        spawn(async { "123" }),
        spawn_blocking(|| panic!())
    );

    assert!(matches!(fut.await, (Ok(3), Ok("123"), Err(_))))
}

#[tokio::test]
async fn should_poll_all() {
    #[derive(Clone, Default)]
    struct ControlledFuture {
        should_complete: Rc<Cell<bool>>,
        has_polled_me: Rc<Cell<bool>>,
    }

    impl Future for ControlledFuture {
        type Output = ();

        fn poll(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<Self::Output> {
            self.has_polled_me.set(true);
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
    let mut joint = pin!(join!(
        monitor.0.clone(),
        monitor.1.clone(),
        monitor.2.clone(),
        monitor.3.clone()
    ));

    // #1: assume that no one has completed yet
    assert!(joint.as_mut().poll(&mut cx).is_pending()); // our joint future is `Unpin`!
    assert!(monitor.0.has_polled_me.take()); // to set the value to `false` and assert that the previous is `true` in one line
    assert!(monitor.1.has_polled_me.take());
    assert!(monitor.2.has_polled_me.take());
    assert!(monitor.3.has_polled_me.take());

    // #2: assume that one of these futures has completed, while the others haven't
    monitor.2.should_complete.set(true);
    assert!(joint.as_mut().poll(&mut cx).is_pending());
    assert!(monitor.0.has_polled_me.take());
    assert!(monitor.1.has_polled_me.take());
    assert!(monitor.2.has_polled_me.take());
    assert!(monitor.3.has_polled_me.take());

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

    // #4: poll after completion!
    assert!(
        std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| joint.poll_unpin(&mut cx)))
            .is_err()
    );
}

#[test]
#[ignore = "testing trait implementation"]
fn unpin_impl() {
    use std::future::{pending, ready};
    fn assert_val_impl_unpin(_val: &impl Unpin) {}

    assert_val_impl_unpin(&join!(
        pending::<Vec<()>>(),
        // the output is `!Unpin`, but the `join!` must still be `Unpin` also since `Ready` is `Unpin` regardless of the output
        ready(PhantomPinned),
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
    // assert_output_type::<_, (i32,)>(&async { tokio::join!(async { 2 }) }); // `tokio`'s one is the same as us
    // assert_output_type::<_, (i32,)>(&async { futures::join!(async { 2 }) }); // the same for `futures`'s
}

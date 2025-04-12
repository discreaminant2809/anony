#![allow(deprecated)]

use std::{
    cell::Cell,
    future::{Future, IntoFuture},
    marker::PhantomPinned,
    ops::ControlFlow,
    pin::{Pin, pin},
    rc::Rc,
    task::{Context, Poll},
    time::Duration,
};

use futures::{
    FutureExt,
    task::{noop_waker, noop_waker_ref},
};
use tokio::{select, time::sleep};

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

/// In this test (and the 2 more tests below), we need to test for the correctness of not only the `join` part,
/// but also the `try` part.
#[tokio::test]
async fn join_duration_option() {
    async fn no_short_circuit() {
        let fut = anony::try_join!(
            sleep(Duration::from_millis(300)).map(Some),
            sleep(Duration::from_millis(300)).map(Some),
            // This's the longest, and no one return `None`
            // so the whole future should be ended in (approximately) 0.04 seconds.
            async {
                sleep(Duration::from_millis(200)).await;
                sleep(Duration::from_millis(200)).await;
                Some(())
            },
            sleep(Duration::from_millis(100)).map(Some),
        );

        assert_sleep_correct_dur(fut, 0.4, 0.01).await;
    }

    async fn short_circuit() {
        let fut = anony::try_join!(
            sleep(Duration::from_millis(300)).map(Some),
            sleep(Duration::from_millis(300)).map(Some),
            // The whole future short-circuited since this branch return `None`.
            // The whole future should be ended in (approximately) 0.04 seconds.
            async {
                sleep(Duration::from_millis(250)).await;
                None::<()>
            },
            // ...even though this's the longest
            async {
                sleep(Duration::from_millis(200)).await;
                sleep(Duration::from_millis(200)).await;
                Some(())
            },
            sleep(Duration::from_millis(100)).map(Some),
        );

        assert_sleep_correct_dur(fut, 0.25, 0.01).await;
    }

    futures::join!(no_short_circuit(), short_circuit());
}

#[tokio::test]
async fn join_duration_result() {
    async fn no_short_circuit() {
        let fut = anony::try_join!(
            sleep(Duration::from_millis(300)).map(Ok::<_, &str>),
            sleep(Duration::from_millis(300)).map(Ok),
            // This's the longest, and no one return `None`
            // so the whole future should be ended in (approximately) 4 seconds
            async {
                sleep(Duration::from_millis(200)).await;
                sleep(Duration::from_millis(200)).await;
                Ok(())
            },
            sleep(Duration::from_millis(100)).map(Ok),
        );

        assert_sleep_correct_dur(fut, 0.4, 0.01).await;
    }

    async fn short_circuit() {
        let fut = anony::try_join!(
            sleep(Duration::from_millis(300)).map(Ok),
            sleep(Duration::from_millis(300)).map(Ok),
            // The whole future short-circuited since this branch return `Err`
            async {
                sleep(Duration::from_millis(250)).await;
                Err::<(), _>("123")
            },
            // ...even though this's the longest
            async {
                sleep(Duration::from_millis(200)).await;
                sleep(Duration::from_millis(200)).await;
                Ok(())
            },
            sleep(Duration::from_millis(100)).map(Ok),
        );

        assert_sleep_correct_dur(fut, 0.25, 0.01).await;
    }

    futures::join!(no_short_circuit(), short_circuit());
}

#[tokio::test]
async fn join_duration_control_flow() {
    async fn no_short_circuit() {
        let fut = anony::try_join!(
            sleep(Duration::from_millis(300)).map(ControlFlow::<i32>::Continue),
            sleep(Duration::from_millis(300)).map(ControlFlow::Continue),
            // This's the longest, and no one return `Break`
            // so the whole future should be ended in (approximately) 4 seconds
            async {
                sleep(Duration::from_millis(200)).await;
                sleep(Duration::from_millis(200)).await;
                ControlFlow::Continue(())
            },
            sleep(Duration::from_millis(100)).map(ControlFlow::Continue),
        );

        assert_sleep_correct_dur(fut, 0.4, 0.01).await;
    }

    async fn short_circuit() {
        let fut = anony::try_join!(
            sleep(Duration::from_millis(300)).map(ControlFlow::Continue),
            sleep(Duration::from_millis(300)).map(ControlFlow::Continue),
            // The whole future short-circuited since this branch return `Break`
            async {
                sleep(Duration::from_millis(250)).await;
                ControlFlow::<_, ()>::Break(())
            },
            // ...even though this's the longest
            async {
                sleep(Duration::from_millis(200)).await;
                sleep(Duration::from_millis(200)).await;
                ControlFlow::Continue(())
            },
            sleep(Duration::from_millis(100)).map(ControlFlow::Continue),
        );

        assert_sleep_correct_dur(fut, 0.25, 0.01).await;
    }

    futures::join!(no_short_circuit(), short_circuit());
}

#[tokio::test]
async fn join_1_ary() {
    // Option
    let fut = async { Some(12) };
    assert_eq!(anony::try_join!(fut).await, Some((12,)));
    let fut = async { None::<i32> };
    assert_eq!(anony::try_join!(fut).await, None);

    // Result
    let fut = async { Ok::<_, &str>(12) };
    assert_eq!(anony::try_join!(fut).await, Ok((12,)));
    let fut = async { Err::<i32, _>("345") };
    assert_eq!(anony::try_join!(fut).await, Err("345"));

    // ControlFlow
    let fut = async { ControlFlow::Continue::<&str, _>(12) };
    assert_eq!(anony::try_join!(fut).await, ControlFlow::Continue((12,)));
    let fut = async { ControlFlow::Break::<_>("123") };
    assert_eq!(anony::try_join!(fut).await, ControlFlow::Break("123"));
}

#[tokio::test]
async fn join_unit() {
    assert_eq!(
        {
            async fn f() -> Option<()> {
                anony::try_join!().await
            }
            f()
        }
        .await,
        Some(())
    );
    assert_eq!(
        {
            async fn f() -> Result<(), usize> {
                anony::try_join!().await
            }
            f()
        }
        .await,
        Ok(())
    );
    assert_eq!(
        {
            async fn f() -> ControlFlow<usize> {
                anony::try_join!().await
            }
            f()
        }
        .await,
        ControlFlow::Continue(())
    );
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
        type Output = Option<()>;

        fn poll(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<Self::Output> {
            self.has_polled_me.set(true);
            self.poll_number
                .set(POLL_NUMBER.replace(POLL_NUMBER.get() + 1));
            if self.should_complete.get() {
                Poll::Ready(Some(()))
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
    let mut joint = pin!(anony::try_join!(
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
    assert_eq!(monitor.0.poll_number.get(), 0); // `join!` will always poll the first future first!
    assert_eq!(monitor.1.poll_number.get(), 1);
    assert_eq!(monitor.2.poll_number.get(), 2);
    assert_eq!(monitor.3.poll_number.get(), 3);

    // #2: assume that one of these futures has completed, while the others haven't
    monitor.2.should_complete.set(true);
    assert!(joint.as_mut().poll(&mut cx).is_pending());
    assert!(monitor.0.has_polled_me.take());
    assert!(monitor.1.has_polled_me.take());
    assert!(monitor.2.has_polled_me.take());
    assert!(monitor.3.has_polled_me.take());
    assert_eq!(monitor.0.poll_number.get(), 4); // it should be consistent
    assert_eq!(monitor.1.poll_number.get(), 5);
    assert_eq!(monitor.2.poll_number.get(), 6);
    assert_eq!(monitor.3.poll_number.get(), 7);

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
    assert_eq!(monitor.0.poll_number.get(), 8);
    assert_eq!(monitor.1.poll_number.get(), 9);
    assert_eq!(monitor.2.poll_number.get(), 6); // since it didn't get polled, the poll number stays the same
    assert_eq!(monitor.3.poll_number.get(), 10);

    // #4: poll after completion!
    assert!(
        std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| joint.poll(&mut cx))).is_err()
    );
}

#[test]
#[should_panic = "`try_join!` future polled after completion"]
fn panic_correct_msg() {
    let mut cx = Context::from_waker(noop_waker_ref());
    let fut = anony::try_join!(async { Some(()) }, async { Some(()) });
    let mut fut = pin!(fut);

    let _ = fut.as_mut().poll(&mut cx);
    let _ = fut.poll(&mut cx); // panic here
}

#[test]
#[ignore = "testing trait implementation"]
fn unpin_impl() {
    use std::future::{pending, ready};

    fn assert_val_impl_unpin(_: &impl Unpin) {}

    assert_val_impl_unpin(&anony::try_join!(
        pending::<Result<Vec<()>, &str>>(),
        // the output is `!Unpin`, but the `join!` must still be `Unpin` also since `Ready` is `Unpin` regardless of the output
        ready(Ok(PhantomPinned)),
    ));
}

#[test]
#[ignore = "testing return type"]
fn output_type() {
    use std::future::pending;

    fn assert_output_type<T>(_x: &impl Future<Output = T>) {}

    assert_output_type::<Option<(Vec<()>, ())>>(&anony::try_join!(
        pending::<Option<Vec<()>>>(),
        sleep(Duration::default()).map(Some)
    ));

    assert_output_type::<ControlFlow<i64>>(&anony::try_join!());
    assert_output_type::<Result<(i32,), String>>(&anony::try_join!(async { Ok(2) }));
    // assert_output_type::<Result<(i32,), String>>(&async { tokio::try_join!(async { Ok(2) }) }); // `tokio`'s one is the same as us
    // assert_output_type::<Result<(i32,), String>>(&async { futures::try_join!(async { Ok(2) }) });
    // // the same for `futures`'s
}

#[test]
#[ignore = "testing `IntoFuture` acceptance"]
#[allow(unused_must_use)]
fn into_future_acceptance() {
    fn _f(into_fut: impl std::future::IntoFuture<Output = Option<()>> + Copy) {
        anony::try_join!(into_fut);
        anony::try_join!(into_fut, into_fut);
        anony::try_join!(into_fut, into_fut, into_fut);
    }
}

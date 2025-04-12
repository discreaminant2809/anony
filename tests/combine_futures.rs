#![forbid(unsafe_code)]

use anony::{combine_futures, combine_futures_cyclic};
use futures::executor::block_on;

fn _syntax_test() {
    macro_rules! pure_break {
        ($macro_name:ident) => {
            let fut_expr0 = async { 3 };
            let condition = false;

            let _fut = ::anony::$macro_name! {
                break async {2},

                let 2 = async {2} else => break 2 => continue,

                let x = fut_expr0 => continue x,

                if let x @ -1 = async {-1} => break {
                    x
                } else if condition => continue {
                    0
                } else if condition => break {
                    0
                } else => break {
                    0
                }

                match async{ Some(2) } {
                    Some(x) if x == 0 => break x,
                    Some(x) => continue { x }
                    None => break 0,
                }
            };
        };
    }

    pure_break!(combine_futures);
    pure_break!(combine_futures_cyclic);

    fn assert_static<T: 'static>(_: &T) {}

    macro_rules! no_pure_break {
        ($macro_name:ident) => {
            let fut_expr0 = async { 3 };
            let condition = false;
            let coefficient = -8;
            let fut = ::anony::$macro_name! {
                // removing it will cause error in the `assert_static`, since the captured variable is captured by reference.
                move

                continue async {2},

                let 2 = async {2} else => break 2 => continue,

                let x = fut_expr0 => continue x,

                if let x @ -1 = async {-1} => break {
                    x
                } else if condition => continue {
                    0
                } else if let false = condition => break {
                    0
                } else => break {
                    0
                }

                if async { true } => continue {
                    "abc"
                } else => break {
                    dbg!(0)
                }

                match async{ Some(2) } {
                    Some(x) if x == 0 => break x,
                    Some(x) => continue { x }
                    None => break 0,
                }

                |a, _, c: i32, d, _: &str, e| (a + c + d + e) * coefficient,
            };

            assert_static(&fut);
        }
    }

    no_pure_break!(combine_futures);
    no_pure_break!(combine_futures_cyclic);
}

fn _unpin_test(futs: (impl Future + Unpin + Copy, impl Future + Unpin + Copy)) {
    fn assert_val_impl_unpin(_: &impl Unpin) {}

    let fut = combine_futures! { continue futs.0, continue futs.1 };
    assert_val_impl_unpin(&fut);

    let fut = combine_futures_cyclic! { continue futs.0, continue futs.1 };
    assert_val_impl_unpin(&fut);
}

#[test]
fn empty() {
    // Use `block_on` instead of `tokio::test` for miri.
    block_on(async {
        let () = combine_futures! {}.await;
        let () = combine_futures_cyclic! {}.await;

        assert_eq!(
            combine_futures! {
                || 2
            }
            .await,
            2
        );

        assert_eq!(
            combine_futures_cyclic! {
                || 2
            }
            .await,
            2
        );
    })
}

#[test]
fn branch_let() {
    block_on(async {
        assert_eq!(
            combine_futures! {
                let x = async { 2 } => continue x,
            }
            .await,
            (2,)
        );
        assert_eq!(
            combine_futures_cyclic! {
                let x = async { 2 } => continue x,
            }
            .await,
            (2,)
        );

        assert_eq!(
            combine_futures! {
                let x = async { 2 } => break x,
            }
            .await,
            2,
        );
        assert_eq!(
            combine_futures_cyclic! {
                let x = async { 2 } => break x,
            }
            .await,
            2,
        );

        assert_eq!(
            combine_futures! {
                continue async { 2 },
            }
            .await,
            (2,)
        );
        assert_eq!(
            combine_futures_cyclic! {
                continue async { 2 },
            }
            .await,
            (2,)
        );

        assert_eq!(
            combine_futures! {
                break async { 2 },
            }
            .await,
            2,
        );
        assert_eq!(
            combine_futures_cyclic! {
                break async { 2 },
            }
            .await,
            2,
        );
    })
}

#[test]
fn branch_if() {
    block_on(async {
        assert_eq!(
            combine_futures! {
                if async { true } => continue {
                    2
                } else => break {
                    (3,)
                }
            }
            .await,
            (2,)
        );
        assert_eq!(
            combine_futures_cyclic! {
                if async { true } => continue {
                    2
                } else => break {
                    (3,)
                }
            }
            .await,
            (2,)
        );

        assert_eq!(
            combine_futures! {
                if async { false } => continue {
                    2
                } else => break {
                    (3,)
                }
            }
            .await,
            (3,)
        );
        assert_eq!(
            combine_futures_cyclic! {
                if async { false } => continue {
                    2
                } else => break {
                    (3,)
                }
            }
            .await,
            (3,)
        );

        assert_eq!(
            combine_futures! {
                if async { false } => break {
                    1
                } else if true => break {
                    2
                } else => break {
                    3
                }
            }
            .await,
            2,
        );
        assert_eq!(
            combine_futures_cyclic! {
                if async { false } => break {
                    1
                } else if true => break {
                    2
                } else => break {
                    3
                }
            }
            .await,
            2,
        );

        combine_futures! {
            if let Some(sub) = async { 1_u8.checked_sub(2) } => break {
                Ok(sub)
            } else => break {
                Err(())
            }
        }
        .await
        .unwrap_err();
        combine_futures_cyclic! {
            if let Some(sub) = async { 1_u8.checked_sub(2) } => break {
                Ok(sub)
            } else => break {
                Err(())
            }
        }
        .await
        .unwrap_err();
    })
}

#[test]
fn branch_match() {
    block_on(async {
        assert!(
            combine_futures! {
                match async { 2 } {
                    0..3 => break true,
                    _ => break false,
                }
            }
            .await,
        );
        assert!(
            !combine_futures_cyclic! {
                match async { 3 } {
                    0..3 => break true,
                    _ => break false,
                }
            }
            .await,
        );

        async fn read_db(key: &str) -> Result<String, ()> {
            if key == "qwerty" {
                Ok("my secret".into())
            } else {
                Err(())
            }
        }

        combine_futures! {
            match read_db("qwerty") {
                Ok(s) => break {
                    assert_eq!(s, "my secret");
                }
                Err(_) => break unreachable!(),
            }
        };
        combine_futures_cyclic! {
            match read_db("qwerty") {
                Ok(s) => break {
                    assert_eq!(s, "my secret");
                }
                Err(_) => break unreachable!(),
            }
        };
    })
}

// Basically delays the completion, and yield other futures.
async fn lagger<F: Future>(fut: F) -> F::Output {
    use std::task::Poll;

    let mut should_done = false;
    std::future::poll_fn(move |cx| {
        if std::mem::replace(&mut should_done, true) {
            Poll::Ready(())
        } else {
            cx.waker().wake_by_ref();
            Poll::Pending
        }
    })
    .await;

    fut.await
}

#[test]
fn multi_branches() {
    block_on(async {
        assert_eq!(
            combine_futures! {
                continue lagger(async { 0 }),
                continue async { 1 },
                continue async { 2 },
            }
            .await,
            (0, 1, 2)
        );
        assert_eq!(
            combine_futures_cyclic! {
                continue lagger(async { 0 }),
                continue async { 1 },
                continue async { 2 },
            }
            .await,
            (0, 1, 2)
        );

        assert_eq!(
            combine_futures! {
                continue lagger(async { 1 }),
                continue async { 2 },
                |a, b| a + b
            }
            .await,
            3
        );
        assert_eq!(
            combine_futures_cyclic! {
                continue lagger(async { 1 }),
                continue async { 2 },
                |a, b| a + b
            }
            .await,
            3
        );

        assert_eq!(
            combine_futures! {
                continue lagger(async { 0 }),
                break    async { 1 }, // The output is not dictated by this pure-break branch
                continue async { "2" },
            }
            .await,
            1,
        );
        assert_eq!(
            combine_futures_cyclic! {
                continue lagger(async { 0 }),
                break    async { 1 },
                continue async { "2" },
            }
            .await,
            1,
        );

        assert_eq!(
            combine_futures! {
                continue async { 0 },
                break    lagger(async { 1 }),
                continue async { "2" },
            }
            .await,
            1,
        );
        assert_eq!(
            combine_futures_cyclic! {
                continue async { 0 },
                break    lagger(async { 1 }),
                continue async { "2" },
            }
            .await,
            1,
        );

        async fn async_checked_sub(a: u8, b: u8) -> Option<u8> {
            a.checked_sub(b)
        }

        assert_eq!(
            combine_futures! {
                match async_checked_sub(4, 2) {
                    Some(a) => continue a,
                    None => break None,
                }
                match async_checked_sub(100, 99) {
                    Some(b) => continue b,
                    None => break None,
                }
                |a, b| Some((a, b))
            }
            .await,
            Some((2, 1))
        );
        assert_eq!(
            combine_futures_cyclic! {
                match async_checked_sub(4, 2) {
                    Some(a) => continue a,
                    None => break None,
                }
                match async_checked_sub(100, 99) {
                    Some(b) => continue b,
                    None => break None,
                }
                |a, b| Some((a, b))
            }
            .await,
            Some((2, 1))
        );
    })
}

#[test]
fn poll_order_non_cyclic() {
    block_on(async {
        use std::future::ready;

        let mut poll_count = 0;
        combine_futures! {
            move // we have our own copy of `poll_count`
            let _ = ready(()) => continue {
                assert_eq!(poll_count, 0);
                poll_count += 1;
            }
            let _ = ready(()) => continue {
                assert_eq!(poll_count, 1);
                poll_count += 1;
            }
            let _ = ready(()) => continue {
                assert_eq!(poll_count, 2);
                poll_count += 1;
            }
        }
        .await;
        // Respawn it, to make sure
        combine_futures! {
            move // we have our own copy of `poll_count`
            let _ = ready(()) => continue {
                assert_eq!(poll_count, 0);
                poll_count += 1;
            }
            let _ = ready(()) => continue {
                assert_eq!(poll_count, 1);
                poll_count += 1;
            }
            let _ = ready(()) => continue {
                assert_eq!(poll_count, 2);
                poll_count += 1;
            }
        }
        .await;

        // Sadly no order is guaranteed.
        combine_futures_cyclic! {
            // No `move`. `poll_count` is affected.
            let _ = ready(()) => continue {
                poll_count += 1;
            }
            let _ = ready(()) => continue {
                poll_count += 1;
            }
            let _ = ready(()) => continue {
                poll_count += 1;
            }
        }
        .await;

        assert_eq!(poll_count, 3);
    })
}

#[test]
fn poll_after_completion() {
    fn must_panic_after_a_poll(fut: impl Future) {
        use std::panic::{AssertUnwindSafe, catch_unwind};
        use std::task::{Context, Waker};

        let mut cx = Context::from_waker(Waker::noop());
        let mut fut = std::pin::pin!(fut);

        assert!(fut.as_mut().poll(&mut cx).is_ready());

        assert!(
            // Must panic
            // We don't use `#[should_panic]` since we don't want to catch every single panic in this test
            catch_unwind(AssertUnwindSafe(|| {
                let _ = fut.as_mut().poll(&mut cx);
            }))
            .is_err()
        );
    }

    must_panic_after_a_poll(combine_futures! {
        continue async {},
        continue async {},
    });
    must_panic_after_a_poll(combine_futures_cyclic! {
        continue async {},
        continue async {},
    });
}

#[test]
fn poll_cyclic_must_wrap() {
    use std::future::pending;
    use std::task::{Context, Waker};

    let mut fut = std::pin::pin!(combine_futures_cyclic! {
        break pending::<()>(),
        break pending::<()>(),
    });
    let mut cx = Context::from_waker(Waker::noop());

    // Only 2 branches, so `u8`.
    // In this case, the wrap-around should happen about 1000/256 times.
    for _ in 0..1000 {
        let _ = fut.as_mut().poll(&mut cx);
    }
}

use std::{array, future::poll_fn, hint::black_box, task::Poll};

use criterion::{criterion_group, criterion_main, Criterion};
use futures_concurrency::future::TryJoin;
use tokio::runtime::Runtime;

// We only test the short-circuit case (when futures return `Err`)

pub fn bench_join_no_ret(c: &mut Criterion) {
    let mut group = c.benchmark_group("join");

    async fn unstable_future(n: usize, err_when: Option<usize>) -> Result<(), ()> {
        for i in 0..n {
            // I used 2 versions of `yield_now`, and both yield different results

            // Result for this one (from best to worst):
            // - anony::try_join
            // - futures::try_join
            // - tokio::try_join
            // - anony::try_join_cyclic
            // - futures_concurrency::future::Join::try_join
            // tokio::task::yield_now().await;

            // Result for the 2nd (from best to worst):
            // - anony::try_join_cyclic
            // - tokio::try_join
            // - anony::try_join
            // - futures::try_join
            // - futures_concurrency::future::Join::try_join
            let mut ready = false;
            poll_fn(|cx| {
                if std::mem::replace(&mut ready, true) {
                    Poll::Ready(())
                } else {
                    cx.waker().wake_by_ref();
                    Poll::Pending
                }
            })
            .await;

            if err_when.is_some_and(|err_when| err_when == i) {
                return Err(());
            }
        }

        Ok(())
    }

    group.bench_function("anony::try_join", |b| {
        let mut b = b.to_async(Runtime::new().unwrap());
        b.iter(|| async {
            black_box(
                anony::try_join!(
                    black_box(unstable_future(20, None)),
                    black_box(unstable_future(50, None)),
                    black_box(unstable_future(30, Some(25))),
                )
                .await,
            )
        });
    });
    group.bench_function("anony::try_join_cyclic", |b| {
        let mut b = b.to_async(Runtime::new().unwrap());
        b.iter(|| async {
            black_box(
                anony::try_join_cyclic!(
                    black_box(unstable_future(20, None)),
                    black_box(unstable_future(50, None)),
                    black_box(unstable_future(30, Some(25))),
                )
                .await,
            )
        });
    });
    group.bench_function("futures::try_join", |b| {
        let mut b = b.to_async(Runtime::new().unwrap());
        b.iter(|| async {
            black_box(futures::try_join!(
                black_box(unstable_future(20, None)),
                black_box(unstable_future(50, None)),
                black_box(unstable_future(30, Some(25))),
            ))
        });
    });
    group.bench_function("tokio::try_join", |b| {
        let mut b = b.to_async(Runtime::new().unwrap());
        b.iter(|| async {
            black_box(tokio::try_join!(
                black_box(unstable_future(20, None)),
                black_box(unstable_future(50, None)),
                black_box(unstable_future(30, Some(25))),
            ))
        });
    });
    group.bench_function("futures_concurrency::future::Join::try_join", |b| {
        let mut b = b.to_async(Runtime::new().unwrap());
        b.iter(|| async {
            black_box(
                (
                    black_box(unstable_future(20, None)),
                    black_box(unstable_future(50, None)),
                    black_box(unstable_future(30, Some(25))),
                )
                    .try_join()
                    .await,
            )
        });
    });

    group.finish();
}

pub fn bench_join_ret(c: &mut Criterion) {
    let mut group = c.benchmark_group("join");

    #[inline(never)]
    async fn unstable_future(n: usize, err_when: Option<usize>) -> Result<[usize; 10_000], ()> {
        for i in 0..n {
            // I used 2 versions of `yield_now`, and both yield different results

            // Result for this one (from best to worst):
            // - anony::try_join
            // - anony::try_join_cyclic
            // - futures::try_join
            // - tokio::try_join
            // - futures_concurrency::future::Join::try_join
            // tokio::task::yield_now().await;

            // Result for the 2nd (from best to worst):
            // - anony::try_join
            // - anony::try_join_cyclic
            // - futures::try_join
            // - tokio::try_join
            // - futures_concurrency::future::Join::try_join
            let mut ready = false;
            poll_fn(|cx| {
                if std::mem::replace(&mut ready, true) {
                    Poll::Ready(())
                } else {
                    cx.waker().wake_by_ref();
                    Poll::Pending
                }
            })
            .await;

            if err_when.is_some_and(|err_when| err_when == i) {
                return Err(());
            }
        }

        Ok(array::from_fn(|i| i))
    }

    group.bench_function("anony::try_join", |b| {
        let mut b = b.to_async(Runtime::new().unwrap());
        b.iter(|| async {
            black_box(
                anony::try_join!(
                    black_box(unstable_future(30, None)),
                    black_box(unstable_future(30, Some(25))),
                    black_box(unstable_future(30, None)),
                )
                .await,
            )
        });
    });
    group.bench_function("anony::try_join_cyclic", |b| {
        let mut b = b.to_async(Runtime::new().unwrap());
        b.iter(|| async {
            black_box(
                anony::try_join_cyclic!(
                    black_box(unstable_future(30, None)),
                    black_box(unstable_future(30, Some(25))),
                    black_box(unstable_future(30, None)),
                )
                .await,
            )
        });
    });
    group.bench_function("futures::try_join", |b| {
        let mut b = b.to_async(Runtime::new().unwrap());
        b.iter(|| async {
            black_box(futures::try_join!(
                black_box(unstable_future(30, None)),
                black_box(unstable_future(30, Some(25))),
                black_box(unstable_future(30, None)),
            ))
        });
    });
    group.bench_function("tokio::try_join", |b| {
        let mut b = b.to_async(Runtime::new().unwrap());
        b.iter(|| async {
            black_box(tokio::try_join!(
                black_box(unstable_future(30, None)),
                black_box(unstable_future(30, Some(25))),
                black_box(unstable_future(30, None)),
            ))
        });
    });
    group.bench_function("futures_concurrency::future::Join::try_join", |b| {
        let mut b = b.to_async(Runtime::new().unwrap());
        b.iter(|| async {
            black_box(
                (
                    black_box(unstable_future(30, None)),
                    black_box(unstable_future(30, Some(25))),
                    black_box(unstable_future(30, None)),
                )
                    .try_join()
                    .await,
            )
        });
    });

    group.finish();
}

criterion_group!(benches, bench_join_no_ret, bench_join_ret);
criterion_main!(benches);

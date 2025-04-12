#![allow(deprecated)]

use std::{array, future::poll_fn, hint::black_box, task::Poll};

use criterion::{Criterion, criterion_group, criterion_main};
use futures_concurrency::future::Join;
use tokio::runtime::Runtime;

pub fn bench_join_no_ret(c: &mut Criterion) {
    let mut group = c.benchmark_group("join");

    async fn unstable_future(n: usize) {
        for _ in 0..n {
            // I used 2 versions of `yield_now`, and both yield different results

            // Result for this one (from best to worst):
            // - anony::join
            // - futures::join
            // - anony::join_cyclic
            // - tokio::join
            // - futures_concurrency::future::Join::join
            tokio::task::yield_now().await;

            // Result for the 2nd (from best to worst):
            // - anony::join (sub 1μs!)
            // - anony::join_cyclic
            // - tokio::join
            // - futures::join
            // - futures_concurrency::future::Join::join
            // let mut ready = false;
            // poll_fn(|cx| {
            //     if std::mem::replace(&mut ready, true) {
            //         Poll::Ready(())
            //     } else {
            //         cx.waker().wake_by_ref();
            //         Poll::Pending
            //     }
            // })
            // .await
        }
    }

    group.bench_function("anony::join", |b| {
        let mut b = b.to_async(Runtime::new().unwrap());
        b.iter(|| async {
            black_box(
                anony::join!(
                    black_box(unstable_future(20)),
                    black_box(unstable_future(50)),
                    black_box(unstable_future(30)),
                )
                .await,
            )
        });
    });
    group.bench_function("anony::join_cyclic", |b| {
        let mut b = b.to_async(Runtime::new().unwrap());
        b.iter(|| async {
            black_box(
                anony::join_cyclic!(
                    black_box(unstable_future(20)),
                    black_box(unstable_future(50)),
                    black_box(unstable_future(30)),
                )
                .await,
            )
        });
    });
    group.bench_function("futures::join", |b| {
        let mut b = b.to_async(Runtime::new().unwrap());
        b.iter(|| async {
            black_box(futures::join!(
                black_box(unstable_future(20)),
                black_box(unstable_future(50)),
                black_box(unstable_future(30)),
            ))
        });
    });
    group.bench_function("tokio::join", |b| {
        let mut b = b.to_async(Runtime::new().unwrap());
        b.iter(|| async {
            black_box(tokio::join!(
                black_box(unstable_future(20)),
                black_box(unstable_future(50)),
                black_box(unstable_future(30)),
            ))
        });
    });
    group.bench_function("futures_concurrency::future::Join::join", |b| {
        let mut b = b.to_async(Runtime::new().unwrap());
        b.iter(|| async {
            black_box(
                (
                    black_box(unstable_future(20)),
                    black_box(unstable_future(50)),
                    black_box(unstable_future(30)),
                )
                    .join()
                    .await,
            )
        });
    });

    group.finish();
}

pub fn bench_join_ret(c: &mut Criterion) {
    let mut group = c.benchmark_group("join");

    #[inline(never)]
    async fn unstable_future(n: usize) -> [usize; 10_000] {
        for _ in 0..n {
            // I used 2 versions of `yield_now`, and both yield different results

            // Result for this one (from best to worst):
            // - tokio::join
            // - anony::join_cyclic
            // - futures::join
            // - anony::join
            // - futures_concurrency::future::Join::join
            // tokio::task::yield_now().await;

            // Result for the 2nd (from best to worst):
            // - tokio::join
            // - anony::join_cyclic
            // - futures::join
            // - anony::join
            // - futures_concurrency::future::Join::join
            let mut ready = false;
            poll_fn(|cx| {
                if std::mem::replace(&mut ready, true) {
                    Poll::Ready(())
                } else {
                    cx.waker().wake_by_ref();
                    Poll::Pending
                }
            })
            .await
        }

        array::from_fn(|i| i)
    }

    group.bench_function("anony::join", |b| {
        let mut b = b.to_async(Runtime::new().unwrap());
        b.iter(|| async {
            black_box(
                anony::join!(
                    black_box(unstable_future(30)),
                    black_box(unstable_future(30)),
                    black_box(unstable_future(30)),
                )
                .await,
            )
        });
    });
    group.bench_function("anony::join_cyclic", |b| {
        let mut b = b.to_async(Runtime::new().unwrap());
        b.iter(|| async {
            black_box(
                anony::join_cyclic!(
                    black_box(unstable_future(30)),
                    black_box(unstable_future(30)),
                    black_box(unstable_future(30)),
                )
                .await,
            )
        });
    });
    group.bench_function("futures::join", |b| {
        let mut b = b.to_async(Runtime::new().unwrap());
        b.iter(|| async {
            black_box(futures::join!(
                black_box(unstable_future(30)),
                black_box(unstable_future(30)),
                black_box(unstable_future(30)),
            ))
        });
    });
    group.bench_function("tokio::join", |b| {
        let mut b = b.to_async(Runtime::new().unwrap());
        b.iter(|| async {
            black_box(tokio::join!(
                black_box(unstable_future(30)),
                black_box(unstable_future(30)),
                black_box(unstable_future(30)),
            ))
        });
    });
    group.bench_function("futures_concurrency::future::Join::join", |b| {
        let mut b = b.to_async(Runtime::new().unwrap());
        b.iter(|| async {
            black_box(
                (
                    black_box(unstable_future(30)),
                    black_box(unstable_future(30)),
                    black_box(unstable_future(30)),
                )
                    .join()
                    .await,
            )
        });
    });

    group.finish();
}

criterion_group!(benches, bench_join_no_ret, bench_join_ret);
criterion_main!(benches);

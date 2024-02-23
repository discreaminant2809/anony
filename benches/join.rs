use std::{hint::black_box, time::Duration};

use criterion::{criterion_group, criterion_main, Criterion};
use tokio::runtime::Runtime;

// Result (runtime): anony::join < futures::join < anony::join_cyclic < tokio::join
pub fn bench_join_no_sleep(c: &mut Criterion) {
    let mut group = c.benchmark_group("join");

    async fn unstable_future(n: usize) {
        for _ in 0..n {
            tokio::task::yield_now().await;
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

    group.finish();
}

// Result: there is no significant differences
pub fn bench_join_sleep(c: &mut Criterion) {
    let mut group = c.benchmark_group("join");

    async fn sleepy_future(n: usize) {
        for dur in (0..n).map(|i| Duration::from_millis((50 + i) as _)) {
            tokio::time::sleep(dur).await;
        }
    }

    group.bench_function("anony::join", |b| {
        let mut b = b.to_async(Runtime::new().unwrap());
        b.iter(|| async {
            black_box(
                anony::join!(
                    black_box(sleepy_future(1)),
                    black_box(sleepy_future(3)),
                    black_box(sleepy_future(2)),
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
                    black_box(sleepy_future(1)),
                    black_box(sleepy_future(3)),
                    black_box(sleepy_future(2)),
                )
                .await,
            )
        });
    });
    group.bench_function("futures::join", |b| {
        let mut b = b.to_async(Runtime::new().unwrap());
        b.iter(|| async {
            black_box(futures::join!(
                black_box(sleepy_future(1)),
                black_box(sleepy_future(3)),
                black_box(sleepy_future(2)),
            ))
        });
    });
    group.bench_function("tokio::join", |b| {
        let mut b = b.to_async(Runtime::new().unwrap());
        b.iter(|| async {
            black_box(tokio::join!(
                black_box(sleepy_future(1)),
                black_box(sleepy_future(3)),
                black_box(sleepy_future(2)),
            ))
        });
    });

    group.finish();
}

criterion_group!(benches, bench_join_no_sleep, bench_join_sleep);
criterion_main!(benches);

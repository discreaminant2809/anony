#![allow(clippy::size_of_ref, deprecated)]

use std::future::Future;
use std::mem::{size_of, size_of_val};

use futures::AsyncReadExt;
use futures_concurrency::future::Join;
use tokio::time::sleep;

#[tokio::main]
async fn main() {
    // Comparing Sizes of all `join!`s //

    let anony_join = anony::join!(async { 2 }, async { "a" }, sleep(Default::default()));
    let futures_join =
        async { futures::join!(async { 2 }, async { "a" }, sleep(Default::default())) };
    let tokio_join = async { tokio::join!(async { 2 }, async { "a" }, sleep(Default::default())) };
    let fc_join = (async { 2 }, async { "a" }, sleep(Default::default())).join();

    // These will be diverging!
    // On my arm64, ...
    dbg!(size_of_val(&anony_join)); // This's 144 bytes
    dbg!(size_of_val(&futures_join)); // This's 176 bytes
    dbg!(size_of_val(&tokio_join)); // This's 168 bytes
    dbg!(size_of_val(&fc_join)); // This's 216 bytes

    println!();

    let anony_join = async {
        futures::io::repeat(2).read_exact(&mut []).await.unwrap();
        anony::join!(async { 2 }, async { "a" }).await;
    };
    let futures_join = async {
        futures::io::repeat(2).read_exact(&mut []).await.unwrap();
        futures::join!(async { 2 }, async { "a" });
    };
    let tokio_join = async {
        futures::io::repeat(2).read_exact(&mut []).await.unwrap();
        tokio::join!(async { 2 }, async { "a" });
    };
    let fc_join = async {
        futures::io::repeat(2).read_exact(&mut []).await.unwrap();
        (async { 2 }, async { "a" }).join().await;
    };

    // On my arm64, ...
    dbg!(size_of_val(&anony_join)); // This's 40 bytes
    dbg!(size_of_val(&futures_join)); // This's 56 bytes
    dbg!(size_of_val(&tokio_join)); // This's 56 bytes
    dbg!(size_of_val(&fc_join)); // This's 88 bytes

    println!();

    let anony_join = anony::join!();
    let futures_join = async { futures::join!() };
    let tokio_join = async { tokio::join!() };
    let fc_join = ().join().await;

    // On my arm64, ...
    dbg!(size_of_val(&anony_join)); // This's 0 bytes
    dbg!(size_of_val(&futures_join)); // This's 1 byte
    dbg!(size_of_val(&tokio_join)); // This's 2 bytes
    dbg!(size_of_val(&fc_join)); // This's 0 bytes

    println!();

    // Comparing Sizes of the 2 Possible `MaybeDone` Implementations //

    dbg!(size_of_val(&async { 2 }));
    dbg!(size_of_val(&async { "a" }));
    dbg!(size_of_val(&sleep(Default::default())));
    dbg!(size_of_fut_output(&async { 2 }));
    dbg!(size_of_fut_output(&async { "a" }));
    dbg!(size_of_fut_output(&sleep(Default::default())));

    // All of the following have the same size!
    dbg!(size_of_val(&MaybeDone1::Pending(async { 2 })));
    dbg!(size_of_val(&MaybeDone1::Pending(async { "a" })));
    dbg!(size_of_val(&MaybeDone1::Pending(sleep(Default::default()))));
    dbg!(size_of_val(&MaybeDone2::Pending(async { 2 })));
    dbg!(size_of_val(&MaybeDone2::Pending(async { "a" })));
    dbg!(size_of_val(&MaybeDone2::Pending(sleep(Default::default()))));
    // You should be able to conclude that the 2 approaches yield the same size,
    // but mine is slightly advantegous since we don't need to use `unreachable!`
}

fn size_of_fut_output<F: Future>(_x: &F) -> usize {
    size_of::<F::Output>()
}

// This is what I use
#[allow(dead_code)]
enum MaybeDone1<F: Future> {
    Pending(F),
    Ready(Option<F::Output>),
}

// This is what most does
#[allow(dead_code)]
enum MaybeDone2<F: Future> {
    Pending(F),
    Ready(F::Output),
    NothingLeft,
}

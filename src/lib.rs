//! Provides various anonymous type constructs
//!
//! ## Macros
//!
//! * [`struct!`]: creates an instance of an anonymous struct.
//!
//! ```rust
//! use anony::r#struct;
//!
//! let items = vec![1, 3, 5];
//!
//! let x = r#struct! {
//!     color: "Red".to_owned(),
//!     // move the `items` variable to the struct
//!     items
//! };
//!
//! assert_eq!(x.color, "Red");
//! assert_eq!(x.items, [1, 3, 5]);
//! ```
//!
//! * [`join!`] and [`join_cyclic!`]: join multiple futures. Require `future` feature.
//!
//! ```rust
//! # futures::executor::block_on(async {
//! use anony::join;
//!
//! assert_eq!(join!(async { 2 }, async { "123" }).await, (2, "123"));
//! # });
//! ```
//!
//! ## Features
//!
//! * `serde`: derives [`Serialize`] for anonymous structs. [serde] crate and its `derive` feature must exist in your crate.
//! * `future`: enables [`Future`] anonymous types, such as [`join!`].
//!
//! [`Serialize`]: https://docs.rs/serde/latest/serde/ser/trait.Serialize.html
//! [`Future`]: std::future::Future
//! [serde]: https://docs.rs/serde/latest/serde/index.html

#![deny(missing_docs)]
#![cfg_attr(docsrs, feature(doc_cfg))]

mod anonymous_struct;
#[cfg(feature = "future")]
mod join;

use proc_macro as pm;
use proc_macro2 as pm2;

/// Creates an instance of an anonymous struct.
///
/// **Note**: if two instances are created from two different `r#struct!`s, they will guarantee belong to two differences anonymous structs
/// even if they have exactly the same set of fields (both names and types). You can clone an instance instead to get the same
/// fields and type for the cloned instance.
///
/// # Examples
///
/// Like how an instance of a normal struct is constructed, you can do the same with this macro:
/// ```
/// use anony::r#struct;
///
/// let address = "123 St. SW";
///
/// let o1 = r#struct! {
///     name: "Alice",
///     age: 28,
///     address,
/// };
///
/// assert_eq!(o1.name, "Alice");
/// assert_eq!(o1.age, 28);
/// assert_eq!(o1.address, "123 St. SW");
///
/// // other anonymous constructs are allowed too!
/// let _o2 = r#struct! {
///     closure: || 3,
///     future: async {
///         let x = "Hello, world!".to_owned();
///         std::future::ready(()).await;
///         x.len()
///     },
///     anonymous: r#struct! {
///         f1: 3.4,
///         f2: Box::new("123"),
///     },
/// };
/// ```
/// You can move fields one by one:
/// ```
/// use anony::r#struct;
///
/// let address = "123 St. SW".to_owned();
///
/// let o1 = r#struct! {
///     name: "Alice".to_owned(),
///     age: 28,
///     address,
/// };
///
/// let name = o1.name;
/// let age = o1.age;
/// let address = o1.address;
///
/// assert_eq!(name, "Alice");
/// assert_eq!(age, 28);
/// assert_eq!(address, "123 St. SW");
/// ```
/// Pinning projection (use `project_ref` for `Pin<&_>` and `project_mut` for `Pin<&mut _>`, like you use `pin-project` crate).
/// The struct created by `project_ref` (not `project_mut`) is implemented [`Clone`] and [`Copy`]:
/// ```
/// use std::pin::pin;
/// use std::future::Future;
/// use std::task::Context;
/// use std::task::Poll;
/// use anony::r#struct;
/// use noop_waker::noop_waker;
///
/// let o1 = r#struct! {
///     fut: async {
///         let s = "10011001001";
///         s.matches("1").count()
///     }
/// };
///
/// let o1 = pin!(o1);
/// let waker = noop_waker();
/// let mut cx = Context::from_waker(&waker);
///
/// // Project to the `fut` field
/// assert_eq!(o1.project_mut().fut.poll(&mut cx), Poll::Ready(5));
/// ```
///
/// # Derived traits
///
/// This struct implements the following if all of its fields are implemented them:
/// * All traits in [`std::cmp`]
/// * [`Debug`]
/// * [`Hash`]
/// * [`Clone`] and [`Copy`] (the cloned instance is guaranteed to have the same type as the source)
/// * [`Serialize`] (`serde` feature required)
///
/// [`Serialize`]: https://docs.rs/serde/latest/serde/ser/trait.Serialize.html
/// [`Debug`]: std::fmt::Debug
/// [`Hash`]: std::hash::Hash
#[proc_macro]
pub fn r#struct(token_stream: pm::TokenStream) -> pm::TokenStream {
    anonymous_struct::imp(token_stream)
        .unwrap_or_else(|e| e.into_compile_error())
        .into()
}

/// Returns a future that "joins" multiple futures that will be completed concurrently.
///
/// It's output is a tuple of input futures' outputs.
///
/// It is more efficient than awaiting futures like this: `(fut1.await, fut2.await, fut3.await)`, since these futures will be resolved
/// **sequencially** (`fut1` must be done first before awaiting `fut2`, and `fut3`). `join!(fut1, fut2, fut3).await` will poll every futures
/// on getting polled, which makes them concurrently awaited.
///
/// This future will always poll the first input future first, which is similar to the [futures]'s one.
/// For example, `join!(fut1, fut2, fut3)` always polls `fut1` first on being polled.
/// If fairness is your concern, consider using [`join_cyclic!`], which is less efficient but fairer.
///
/// [futures]: https://docs.rs/futures/latest/futures/macro.join.html
///
/// # Possible differences from other implementations
///
/// `join!`:
///
/// * returns an instance of an anonymous type implemented [`Future`](std::future::Future)
/// instead of requiring it to be inside an `async`. You will be warned if you neither
/// `.await`, [`poll`](std::future::Future::poll), nor return it.
///
/// * the returned future (generally) has smaller size
///
/// * the returned future is [`Unpin`] if all of the input futures are [`Unpin`].
///
/// # Examples
///
/// ```rust
/// # futures::executor::block_on(async {
/// use anony::join;
///
/// let a = async { 1 };
/// let b = async { 2 };
/// let c = async { 3 };
/// assert_eq!(join!(a, b, c).await, (1, 2, 3));
/// # });
/// ```
#[proc_macro]
#[cfg(feature = "future")]
#[cfg_attr(docsrs, doc(cfg(feature = "future")))]
pub fn join(token_stream: pm::TokenStream) -> pm::TokenStream {
    join::imp(token_stream, false)
        .unwrap_or_else(|e| e.into_compile_error())
        .into()
}

/// Returns a future that "joins" multiple futures that will be completed concurrently, using cycling polling strategy.
///
/// It's output is a tuple of input futures' outputs.
///
/// It is more efficient than awaiting futures like this: `(fut1.await, fut2.await, fut3.await)`, since these futures will be resolved
/// **sequencially** (`fut1` must be done first before awaiting `fut2`, and `fut3`). `join!(fut1, fut2, fut3).await` will poll every futures
/// on getting polled, which makes them concurrently awaited.
///
/// This future will cycle the first future to be polled for each time it is polled, which is similar to the [tokio]'s one.
/// For example, `join!(fut1, fut2, fut3)` polls `fut1` first for the first time being polled, then it polls 'fut2' for the second time,
/// then `fut3` will be the first, then it rolls back to `fut1`, and so on. This strategy ensure fairness as it reduces the chance that
/// heavy futures may make other futures stuck.
/// If fairness is not your concern, consider using [`join!`], which is less fairer but more efficient.
///
/// [tokio]: https://docs.rs/tokio/latest/tokio/macro.join.html
///
/// # Possible differences from other implementations
///
/// `join_cyclic!`:
///
/// * returns an instance of an anonymous type implemented [`Future`](std::future::Future)
/// instead of requiring it to be inside an `async`. You will be warned if you neither
/// `.await`, [`poll`](std::future::Future::poll), nor return it.
///
/// * the returned future (generally) has smaller size
///
/// * the returned future is [`Unpin`] if all of the input futures are [`Unpin`].
///
/// # Examples
///
/// ```rust
/// # futures::executor::block_on(async {
/// use anony::join_cyclic;
///
/// let a = async { 1 };
/// let b = async { 2 };
/// let c = async { 3 };
/// assert_eq!(join_cyclic!(a, b, c).await, (1, 2, 3));
/// # });
/// ```
#[proc_macro]
#[cfg(feature = "future")]
#[cfg_attr(docsrs, doc(cfg(feature = "future")))]
pub fn join_cyclic(token_stream: pm::TokenStream) -> pm::TokenStream {
    join::imp(token_stream, true)
        .unwrap_or_else(|e| e.into_compile_error())
        .into()
}

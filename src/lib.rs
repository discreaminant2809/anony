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
//! * [`tuple!`]: creates an instance of an anonymous tuple.
//!
//! ```rust
//! use anony::tuple;
//!
//! let items = vec![1, 3, 5];
//!
//! let x = tuple!("Red".to_owned(), items);
//!
//! assert_eq!(x.0, "Red");
//! assert_eq!(x.1, [1, 3, 5]);
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
//! * [`try_join!`] and [`try_join_cyclic!`]: join multiple futures and short-circuit on "break" value. Require `future` feature.
//!
//! ```rust
//! # futures::executor::block_on(async {
//! use anony::try_join;
//!
//! assert_eq!(try_join!(async { Some(2) }, async { Some("123") }).await, Some((2, "123")));
//! assert_eq!(try_join!(async { Some(2) }, async { None::<i32> }).await, None);
//! # });
//! ```
//!
//! ## Example Macro Expansions
//!
//! <https://github.com/discreaminant2809/anony/blob/master/examples/expansions.rs>
//!
//! ## Features
//!
//! * `serde`: derives [`Serialize`] for anonymous structs and tuples.
//! [serde] crate, and its `derive` feature in case of [`struct!`], must exist in your crate.
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
mod tuple;
mod utils;

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
/// # Implemented traits
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

/// Create an instance of an anonymous tuple.
///
/// # Practicality
///
/// For 99.999999999999% of use cases, just use a normal tuple. This macro's only advantages are that it supports pinning projection
/// and it implements common traits for any arbitrary arities, not just limited up to 12-ary or 16-ary tuple. That's it!
///
/// # Examples
///
/// Like how an instance of a normal tuple is constructed, you can do the same with this macro:
/// ```rust
/// use anony::tuple;
///
/// let x = tuple!(123, "456");
///
/// assert_eq!(x.0, 123);
/// assert_eq!(x.1, "456");
/// ```
/// You can move fields one by one:
/// ```rust
/// use anony::tuple;
///
/// let address = "123 St. SW".to_owned();
///
/// let o1 = tuple!("Alice".to_owned(), 28, address);
///
/// let name = o1.0;
/// let age = o1.1;
/// let address = o1.2;
///
/// assert_eq!(name, "Alice");
/// assert_eq!(age, 28);
/// assert_eq!(address, "123 St. SW");
/// ```
/// Pinning projection (use `project_ref` for `Pin<&_>` and `project_mut` for `Pin<&mut _>`, like you use `pin-project` crate).
/// They return normal tuples
/// ```rust
/// use std::pin::pin;
/// use std::future::Future;
/// use std::task::Context;
/// use std::task::Poll;
/// use anony::tuple;
/// use noop_waker::noop_waker;
///
/// let o1 = tuple!(async {
///     let s = "10011001001";
///     s.matches("1").count()
/// });
///
/// let o1 = pin!(o1);
/// let waker = noop_waker();
/// let mut cx = Context::from_waker(&waker);
///
/// // Project to the `fut` field
/// assert_eq!(o1.project_mut().0.poll(&mut cx), Poll::Ready(5));
/// ```
/// Convert to a normal tuple
/// ```rust
/// use anony::tuple;
///
/// let x = tuple!(1, 2);
/// let y: (_, _) = x.into();
/// assert_eq!(y, (1, 2));
/// ```
///
/// # Implemented traits
///
/// This struct implements the following if all of its fields implement them:
/// * All traits in [`std::cmp`]
/// * [`Debug`]
/// * [`Hash`]
/// * [`Clone`] and [`Copy`] (the cloned instance is guaranteed to have the same type as the source)
/// * [`Into`], since normal tuples implement [`From`] their equivalent anonymouns tuples
/// * [`Serialize`] (`serde` feature required)
///
/// [`Serialize`]: https://docs.rs/serde/latest/serde/ser/trait.Serialize.html
/// [`Debug`]: std::fmt::Debug
/// [`Hash`]: std::hash::Hash
#[proc_macro]
pub fn tuple(token_stream: pm::TokenStream) -> pm::TokenStream {
    tuple::imp(token_stream)
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
/// * `join!` returns an instance of an anonymous type implemented [`Future`](std::future::Future)
/// instead of requiring it to be inside an `async`. You will be warned if you neither
/// `.await`, [`poll`](std::future::Future::poll), nor return it.
///
/// * input futures are required to implement [`IntoFuture`](std::future::IntoFuture).
///
/// * the returned future (generally) has smaller size and is (generally) faster.
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
/// * `join_cyclic!` returns an instance of an anonymous type implemented [`Future`](std::future::Future)
/// instead of requiring it to be inside an `async`. You will be warned if you neither
/// `.await`, [`poll`](std::future::Future::poll), nor return it.
///
/// * input futures are required to implement [`IntoFuture`](std::future::IntoFuture).
///
/// * the returned future (generally) has smaller size and is (generally) faster.
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

/// Returns a future that "joins" multiple futures that will be completed concurrently. May short-circuit.
///
/// It is similar to [`join!`], except it resolves to "continue" value if all the futures resolve to "continue" value,
/// and resolves to "break" value if one of the futures resolves to "break" value. The "continue" and the "break" value
/// are dependent on the output type of all the futures.
///
/// Here's a basic overview of possible return types and return values:
///
/// | `(F0::Output, F1::Output, ...)` | `TryJoin::Output` | "Continue" value | "Break" value | Note
/// |-----------|-------------|-|-|-|
/// | `(Option<T0>, Option<T1>, ...)`  | `Option<(T0, T1, ...)>` | `Some` | `None` |
/// | `(Result<T0, E>, Result<T1, E>, ...)`  | `Result<(T0, T1, ...), E>` | `Ok` | `Err` | All errors must exactly be the same |
/// | `(ControlFlow<B, C0>, ControlFlow<B, C1>, ...)`  | `ControlFlow<B, (C0, C1, ...)>` | `Continue` | `Break` |
///
/// Formally, the trait bound is as the following, in term of [`Try`](std::ops::Try) and [`Residual`](std::ops::Residual) traits:
/// ```ignore
/// impl<
///         F0: Future,
///         F1: Future,
///         ...
///         FN: Future,
///         R: Residual<(
///             <F0::Output as Try>::Output,
///             <F1::Output as Try>::Output,
///             ...
///             <FN::Output as Try>::Output,
///         )>,
///     > Future for TryJoin<F0, F1, ..., FN, R>
/// where
///     F0::Output: Try<Residual = R>,
///     F1::Output: Try<Residual = R>,
///     ...
///     FN::Output: Try<Residual = R>,
/// {
///     type Output = R::TryType;
/// }
/// ```
///
/// It means that in theory, you can use `Poll<Result<T, E>>` and `Poll<Option<Result<T, E>>>` and mix up
/// with other futures returning `Result<T, E>`! However, to prevent such mess, the two types are NOT allowed
/// as of now. It may be lifted later when many unstable `try_*` methods/functions are stabilized.
///
/// If the standard library add more types implementing [`Try`](std::ops::Try) or [`Residual`](std::ops::Residual),
/// this macro will NOT be aware of it. You can only be waiting till this crate is updated, or both traits are stabilized.
///
/// # DISCLAIMER
/// This macro does NOT use nightly or beta channel. It is usable on stable release.
///
/// # Possible differences from other implementations
///
/// * `try_join!` returns an instance of an anonymous type implemented [`Future`](std::future::Future)
/// instead of requiring it to be inside an `async`. You will be warned if you neither
/// `.await`, [`poll`](std::future::Future::poll), nor return it.
///
/// * input futures are required to implement [`IntoFuture`](std::future::IntoFuture), and their outputs can be more than just
/// [`Result`] (see the first section above for the supported types).
///
/// * the returned future (generally) has smaller size and is (generally) faster.
///
/// * the returned future is [`Unpin`] if all of the input futures are [`Unpin`].
///
/// # Examples
/// ```
/// # futures::executor::block_on(async {
/// use anony::try_join;
///
/// let a = async { Some(1) };
/// let b = async { Some(2) };
/// let c = async { Some(3) };
/// assert_eq!(try_join!(a, b, c).await, Some((1, 2, 3)));
///
/// let a = async { Ok(4) };
/// let b = async { Err::<(), _>("5") };
/// assert_eq!(try_join!(a, b).await, Err("5"));
///
/// let a = async { Some("6") };
/// let b = async { None::<&str> };
/// let c = async { Some("7") };
/// assert_eq!(try_join!(a, b, c).await, None);
/// # });
/// ```
#[proc_macro]
#[cfg(feature = "future")]
#[cfg_attr(docsrs, doc(cfg(feature = "future")))]
pub fn try_join(token_stream: pm::TokenStream) -> pm::TokenStream {
    join::imp_try(token_stream, false)
        .unwrap_or_else(|e| e.into_compile_error())
        .into()
}

/// Returns a future that "joins" multiple futures that will be completed concurrently, using cycling polling strategy.
/// May short-circuit.
///
/// It is similar to [`join_cyclic!`], except it resolves to "continue" value if all the futures resolve to "continue" value,
/// and resolves to "break" value if one of the futures resolves to "break" value. The "continue" and the "break" value
/// are dependent on the output type of all the futures.
///
/// Here's a basic overview of possible return types and return values:
///
/// | `(F0::Output, F1::Output, ...)` | `TryJoinCyclic::Output` | "Continue" value | "Break" value | Note
/// |-----------|-------------|-|-|-|
/// | `(Option<T0>, Option<T1>, ...)`  | `Option<(T0, T1, ...)>` | `Some` | `None` |
/// | `(Result<T0, E>, Result<T1, E>, ...)`  | `Result<(T0, T1, ...), E>` | `Ok` | `Err` | All errors must exactly be the same |
/// | `(ControlFlow<B, C0>, ControlFlow<B, C1>, ...)`  | `ControlFlow<B, (C0, C1, ...)>` | `Continue` | `Break` |
///
/// Formally, the trait bound is as the following, in term of [`Try`](std::ops::Try) and [`Residual`](std::ops::Residual) traits:
/// ```ignore
/// impl<
///         F0: Future,
///         F1: Future,
///         ...
///         FN: Future,
///         R: Residual<(
///             <F0::Output as Try>::Output,
///             <F1::Output as Try>::Output,
///             ...
///             <FN::Output as Try>::Output,
///         )>,
///     > Future for TryJoinCyclic<F0, F1, ..., FN, R>
/// where
///     F0::Output: Try<Residual = R>,
///     F1::Output: Try<Residual = R>,
///     ...
///     FN::Output: Try<Residual = R>,
/// {
///     type Output = R::TryType;
/// }
/// ```
///
/// It means that in theory, you can use `Poll<Result<T, E>>` and `Poll<Option<Result<T, E>>>` and mix up
/// with other futures returning `Result<T, E>`! However, to prevent such mess, the two types are NOT allowed
/// as of now. It may be lifted later when many unstable `try_*` methods/functions are stabilized.
///
/// If the standard library add more types implementing [`Try`](std::ops::Try) or [`Residual`](std::ops::Residual),
/// this macro will NOT be aware of it. You can only be waiting till this crate is updated, or both traits are stabilized.
///
/// # DISCLAIMER
/// This macro does NOT use nightly or beta channel. It is usable on stable release.
///
/// # Possible differences from other implementations
///
/// * `try_join_cyclic!` returns an instance of an anonymous type implemented [`Future`](std::future::Future)
/// instead of requiring it to be inside an `async`. You will be warned if you neither
/// `.await`, [`poll`](std::future::Future::poll), nor return it.
///
/// * input futures are required to implement [`IntoFuture`](std::future::IntoFuture), and their outputs can be more than just
/// [`Result`] (see the first section above for the supported types).
///
/// * the returned future (generally) has smaller size and is (generally) faster.
///
/// * the returned future is [`Unpin`] if all of the input futures are [`Unpin`].
///
/// # Examples
/// ```
/// # futures::executor::block_on(async {
/// use anony::try_join_cyclic;
///
/// let a = async { Some(1) };
/// let b = async { Some(2) };
/// let c = async { Some(3) };
/// assert_eq!(try_join_cyclic!(a, b, c).await, Some((1, 2, 3)));
///
/// let a = async { Ok(4) };
/// let b = async { Err::<(), _>("5") };
/// assert_eq!(try_join_cyclic!(a, b).await, Err("5"));
///
/// let a = async { Some("6") };
/// let b = async { None::<&str> };
/// let c = async { Some("7") };
/// assert_eq!(try_join_cyclic!(a, b, c).await, None);
/// # });
/// ```
#[proc_macro]
#[cfg(feature = "future")]
#[cfg_attr(docsrs, doc(cfg(feature = "future")))]
pub fn try_join_cyclic(token_stream: pm::TokenStream) -> pm::TokenStream {
    join::imp_try(token_stream, true)
        .unwrap_or_else(|e| e.into_compile_error())
        .into()
}

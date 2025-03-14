//! Provides various constructs for anonymous types.
//!
//! # Macros
//!
//! * [`struct!`]: Creates an instance of an anonymous struct.
//!
//! ```rust
//! use anony::r#struct;
//!
//! let items = vec![1, 3, 5];
//!
//! let x = r#struct! {
//!     name: "discreaminant".to_owned(),
//!     // Move the `items` variable into the struct
//!     items,
//! };
//!
//! assert_eq!(x.name, "discreaminant");
//! assert_eq!(x.items, [1, 3, 5]);
//! ```
//!
//! * [`tuple!`]: Creates an instance of an anonymous tuple.
//!
//! ```rust
//! use anony::tuple;
//!
//! let items = vec![1, 3, 5];
//!
//! let x = tuple!("discreaminant".to_owned(), items);
//!
//! assert_eq!(x.0, "discreaminant");
//! assert_eq!(x.1, [1, 3, 5]);
//! ```
//!
//! * [`join!`] and [`join_cyclic!`]: Join multiple futures.
//!   Requires the `future` feature.
//!
//! ```rust
//! # futures::executor::block_on(async {
//! use anony::join;
//!
//! assert_eq!(join!(async { 2 }, async { "123" }).await, (2, "123"));
//! # });
//! ```
//!
//! * [`try_join!`] and [`try_join_cyclic!`]: Join multiple futures, short-circuiting on a "break" value.
//!   Requires the `future` feature.
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
//! # Example Macro Expansions
//!
//! <https://github.com/discreaminant2809/anony/tree/master/examples/expansions>
//!
//! # Features
//!
//! * `serde`: Derives [`Serialize`] for anonymous structs and tuples.
//!   The [serde] crate must be included in your dependencies.
//! * `future`: Enables [`Future`] anonymous types, such as [`join!`].
//!
//! # Nightly
//!
//! Add this to your dependencies:
//!
//! ```toml
//! anony = { git = "https://github.com/discreaminant2809/anony.git", branch = "nightly" }
//! ```
//!
//! [`Future`]: https://doc.rust-lang.org/core/future/trait.Future.html
//! [`Serialize`]: https://docs.rs/serde/latest/serde/ser/trait.Serialize.html
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
/// **Note**: If two instances are created from two different `r#struct!` macros, they are guaranteed to belong to two distinct anonymous structs,
/// even if they have exactly the same set of fields (both names and types). To obtain an instance with the same fields and type, you can clone an existing instance instead.
///
/// # Examples
///
/// Similar to how an instance of a normal struct is constructed, you can use this macro in the same way:
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
/// // Other anonymous constructs are also allowed!
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
///
/// You can move fields individually:
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
///
/// You can use `project_ref` for `Pin<&_>` and `project_mut` for `Pin<&mut _>`, similar to how you use the `pin-project` crate.
/// The struct created by `project_ref` (but not `project_mut`) implements [`Clone`] and [`Copy`]:
///
/// ```
/// use std::pin::pin;
/// use std::future::Future;
/// use std::task::Context;
/// use std::task::Poll;
/// use anony::r#struct;
/// use futures::task::noop_waker;
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
/// # Implemented Traits
///
/// This struct implements the following traits if all of its fields also implement them:
///
/// * All traits in [`std::cmp`]
/// * [`Debug`]
///   
///   **Behavior:** The struct's name is hidden; only field names and values are debugged.
///
/// * [`Hash`]
/// * [`Clone`] and [`Copy`] (the cloned instance is guaranteed to have the same type as the source)
/// * [`Serialize`] (requires the `serde` feature)
///
///   **Behavior:** The struct's name is hidden; only field names and values are serialized.
///   As a result, some data formats may not work, such as XML as it requires the struct's name as a tag.
///
/// # Example Expansions
///
/// <https://github.com/discreaminant2809/anony/blob/master/examples/expansions/anonymous_struct.rs>
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

/// Creates an instance of an anonymous tuple.
///
/// # Practicality
///
/// In 99.999999999999% of use cases, just use a normal tuple.
/// The only advantages of this macro are:
/// - It supports pinning projection.
/// - It implements common traits for arbitrary arities, not just limited to 12-ary or 16-ary tuples.  
///
/// That's it!
///
/// # Examples
///
/// Similar to how a normal tuple is constructed, you can use this macro in the same way:
///
/// ```rust
/// use anony::tuple;
///
/// let x = tuple!(123, "456");
///
/// assert_eq!(x.0, 123);
/// assert_eq!(x.1, "456");
/// ```
///
/// You can move fields individually:
///
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
///
/// Use `project_ref` for `Pin<&_>` and `project_mut` for `Pin<&mut _>`, similar to the `pin-project` crate.
/// These functions return normal tuples:
///
/// ```rust
/// use std::pin::pin;
/// use std::future::Future;
/// use std::task::Context;
/// use std::task::Poll;
/// use anony::tuple;
/// use futures::task::noop_waker;
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
/// // Project to the first field
/// assert_eq!(o1.project_mut().0.poll(&mut cx), Poll::Ready(5));
/// ```
///
/// ## Conversion to a Normal Tuple
///
/// ```rust
/// use anony::tuple;
///
/// let x = tuple!(1, 2);
/// let y = x.into_tuple();
/// assert_eq!(y, (1, 2));
/// ```
///
/// # Implemented Traits
///
/// This tuple implements the following traits if all of its fields implement them:
///
/// * All traits in [`std::cmp`]
/// * [`Debug`]
///   
///   **Behavior:** Debugged as a normal tuple.
///
/// * [`Hash`]
/// * [`Clone`] and [`Copy`] (the cloned instance is guaranteed to have the same type as the source)
/// * [`Serialize`] (requires the `serde` feature)
///
///   **Behavior:** Serialized as a normal tuple.
///
/// # Example Expansions
///
/// <https://github.com/discreaminant2809/anony/blob/master/examples/expansions/tuple.rs>
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

/// Returns a future that "joins" multiple futures, allowing them to be completed concurrently.
/// Its output is a tuple containing the outputs of the input futures.
///
/// This approach is more efficient than awaiting futures sequentially, such as `(fut1.await, fut2.await, fut3.await)`,
/// where each future must complete before the next one starts. In contrast, `join!(fut1, fut2, fut3).await` polls all futures
/// simultaneously when it is polled, enabling concurrent execution.
///
/// This future always polls the first input future first, similar to the [`futures`]' implementation.
/// For example, `join!(fut1, fut2, fut3)` always polls `fut1` first when polled.
/// If fairness is a concern, consider using [`join_cyclic!`], which is fairer but less efficient.
///
/// [`futures`]: https://docs.rs/futures/latest/futures/macro.join.html
///
/// # Possible Differences from Other Implementations
///
/// * `join!` returns an instance of an anonymous type that implements [`Future`](std::future::Future),
///   instead of requiring it to be inside an `async` block. You will receive a warning if you neither
///   `.await`, [`poll`](std::future::Future::poll), nor return it.
///
/// * Input futures must implement [`IntoFuture`](std::future::IntoFuture).
///
/// * The returned future is (generally) smaller in size and more efficient.
///
/// * The returned future is [`Unpin`] if all input futures are [`Unpin`].
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
///
/// If you want to run one or more futures while performing other tasks,
/// this macro helps! Note that the additional tasks must come after all other futures you want to run:
///
/// ```rust
/// # #[tokio::main]
/// # async fn main() {
/// use anony::join;
/// use tokio::time::sleep;
/// use std::time::Duration;
///
/// async fn read_db() -> String {
///     sleep(Duration::from_secs(1)).await;
///     "My secret".into()
/// }
///
/// let (secret_value, _) = join!(read_db(), async {
///     // Perform other tasks here, either asynchronous or just blocking...
///     let a = 1;
///     let b = 2;
///     assert_eq!(a + b, 3);
/// }).await;
///
/// assert_eq!(secret_value, "My secret");
/// # }
/// ```
///
/// # Example Expansions
///
/// <https://github.com/discreaminant2809/anony/blob/master/examples/expansions/join.rs>
#[proc_macro]
#[cfg(feature = "future")]
#[cfg_attr(docsrs, doc(cfg(feature = "future")))]
pub fn join(token_stream: pm::TokenStream) -> pm::TokenStream {
    join::imp(token_stream, false, false)
        .unwrap_or_else(|e| e.into_compile_error())
        .into()
}

/// Returns a future that "joins" multiple futures using a cyclic polling strategy, allowing them to be completed concurrently.
/// Its output is a tuple containing the outputs of the input futures.
///
/// **Usage Note**: If you are unsure whether to use this macro or [`join!`], the latter is generally recommended.
///
/// This approach is more efficient than awaiting futures sequentially, such as `(fut1.await, fut2.await, fut3.await)`,
/// where each future must complete before the next one starts. In contrast, `join!(fut1, fut2, fut3).await`
/// polls all futures simultaneously, allowing them to be awaited concurrently.
///
/// Unlike [`join!`], which always polls the first future first, this macro cycles the order of polling, similar to [`tokio`]’s implementation.
/// For example, given `join_cyclic!(fut1, fut2, fut3)`:
/// - On the first poll, `fut1` is polled first.
/// - On the second poll, `fut2` is polled first.
/// - On the third poll, `fut3` is polled first.
/// - The cycle then repeats, ensuring that no future dominates execution.
///
/// This strategy promotes fairness by reducing the likelihood that heavier futures block others.
/// If fairness is not a concern, consider using [`join!`], which is less fair but more efficient.
///
/// [`tokio`]: https://docs.rs/tokio/latest/tokio/macro.join.html
///
/// # Possible Differences from Other Implementations
///
/// * `join_cyclic!` returns an instance of an anonymous type that implements [`Future`](std::future::Future),
///   instead of requiring it to be inside an `async` block. You will receive a warning if you neither
///   `.await`, [`poll`](std::future::Future::poll), nor return it.
///
/// * Input futures must implement [`IntoFuture`](std::future::IntoFuture).
///
/// * The returned future is (generally) smaller in size and more efficient.
///
/// * The returned future is [`Unpin`] if all input futures are [`Unpin`].
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
///
/// If you want to run one or more futures while performing other tasks,
/// this macro helps! Note that the additional tasks must come after all other futures you want to run:
///
/// ```rust
/// # #[tokio::main]
/// # async fn main() {
/// use anony::join_cyclic;
/// use tokio::time::sleep;
/// use std::time::Duration;
///
/// async fn read_db() -> String {
///     sleep(Duration::from_secs(1)).await;
///     "My secret".into()
/// }
///
/// let (secret_value, _) = join_cyclic!(read_db(), async {
///     // Perform other tasks here, either asynchronous or just blocking...
///     let a = 1;
///     let b = 2;
///     assert_eq!(a + b, 3);
/// }).await;
///
/// assert_eq!(secret_value, "My secret");
/// # }
/// ```
///
/// # Example Expansions
///
/// <https://github.com/discreaminant2809/anony/blob/master/examples/expansions/join_cyclic.rs>
#[proc_macro]
#[cfg(feature = "future")]
#[cfg_attr(docsrs, doc(cfg(feature = "future")))]
pub fn join_cyclic(token_stream: pm::TokenStream) -> pm::TokenStream {
    join::imp(token_stream, false, true)
        .unwrap_or_else(|e| e.into_compile_error())
        .into()
}

/// Returns a future that "joins" multiple futures, allowing them to be completed concurrently. May short-circuit.
///
/// This macro is similar to [`join!`], except that it resolves to a "continue" value if all futures resolve to "continue" values,
/// and it resolves to a "break" value if one of the futures resolves to a "break" value.
/// The "continue" and "break" values depend on the output types of the futures.
///
/// Here's a basic overview of possible return types and values:
///
/// | `(F0::Output, F1::Output, ...)` | `TryJoin::Output` | "Continue" Value | "Break" Value | Note |
/// |----------------------------------|-------------------|------------------|---------------|------|
/// | `(Option<T0>, Option<T1>, ...)` | `Option<(T0, T1, ...)>` | `Some` | `None` | |
/// | `(Result<T0, E>, Result<T1, E>, ...)` | `Result<(T0, T1, ...), E>` | `Ok` | `Err` | All errors must exactly be the same |
/// | `(ControlFlow<B, C0>, ControlFlow<B, C1>, ...)` | `ControlFlow<B, (C0, C1, ...)>` | `Continue` | `Break` | |
///
/// Formally, the trait bound is defined using [`Try`](std::ops::Try) and [`Residual`](std::ops::Residual) traits:
///
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
/// This means that, in theory, you could use `Poll<Result<T, E>>` and `Poll<Option<Result<T, E>>>`
/// together with other futures returning `Result<T, E>`!
/// However, to prevent such mess, these types are **currently not allowed**.
/// This restriction may be lifted once more unstable `try_*` methods and functions are stabilized.
///
/// Additionally, if the standard library introduces new types implementing [`Try`](std::ops::Try) or
/// [`Residual`](std::ops::Residual), this macro **will not** automatically support them.
/// You will need to wait for this crate to be updated, or both traits to be stabilized.
///
/// # Disclaimer
///
/// This macro **does not** require the Rust nightly or beta channels.
/// It is fully **usable on the stable release** of Rust.
///
/// # Possible Differences from Other Implementations
///
/// * `try_join!` returns an instance of an anonymous type that implements [`Future`](std::future::Future),
///   instead of requiring it to be inside an `async` block. You will receive a warning if you neither
///   `.await`, [`poll`](std::future::Future::poll), nor return it.
///
/// * Input futures must implement [`IntoFuture`](std::future::IntoFuture),
///   and their outputs are not limited to [`Result`] types (see the table above for supported types).
///
/// * The returned future is (generally) smaller in size and more efficient.
///
/// * The returned future is [`Unpin`] if all input futures are [`Unpin`].
///
/// # Examples
///
/// ```rust
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
///
/// If you want to run one or more futures while performing other tasks,
/// this macro helps! Note that the additional tasks must come **after** all other futures you want to run:
///
/// ```rust
/// # use std::error::Error;
/// # #[tokio::main]
/// # async fn main() -> Result<(), Box<dyn Error>> {
/// use anony::try_join;
/// use tokio::time::sleep;
/// use std::time::Duration;
///
/// async fn read_db() -> Result<String, Box<dyn Error>> {
///     sleep(Duration::from_secs(1)).await;
///     Ok("My secret".into())
/// }
///
/// let (secret_value, _) = try_join!(read_db(), async {
///     // Perform other tasks here, either asynchronous or just blocking...
///     let a = 1;
///     let b = 2;
///     assert_eq!(a + b, 3);
///     Ok(())
/// }).await?;
///
/// assert_eq!(secret_value, "My secret");
/// # Ok(())
/// # }
/// ```
///
/// # Example Expansions
///
/// <https://github.com/discreaminant2809/anony/blob/master/examples/expansions/try_join.rs>
#[proc_macro]
#[cfg(feature = "future")]
#[cfg_attr(docsrs, doc(cfg(feature = "future")))]
pub fn try_join(token_stream: pm::TokenStream) -> pm::TokenStream {
    join::imp(token_stream, true, false)
        .unwrap_or_else(|e| e.into_compile_error())
        .into()
}

/// Returns a future that "joins" multiple futures using cycling polling strategy, allowing them to be completed concurrently.
/// May short-circuit.
///
/// **Usage note**: If you are unsure whether to use this macro or [`try_join!`], the latter is generally recommended.
///
/// This macro is similar to [`join_cyclic!`], except that it resolves to a "continue" value if all futures resolve to "continue" values,
/// and it resolves to a "break" value if one of the futures resolves to a "break" value.
/// The "continue" and "break" values depend on the output types of the futures.
///
/// Here's a basic overview of possible return types and values:
///
/// | `(F0::Output, F1::Output, ...)` | `TryJoinCyclic::Output` | "Continue" Value | "Break" Value | Note
/// |-----------|-------------|-|-|-|
/// | `(Option<T0>, Option<T1>, ...)`  | `Option<(T0, T1, ...)>` | `Some` | `None` |
/// | `(Result<T0, E>, Result<T1, E>, ...)`  | `Result<(T0, T1, ...), E>` | `Ok` | `Err` | All errors must exactly be the same |
/// | `(ControlFlow<B, C0>, ControlFlow<B, C1>, ...)`  | `ControlFlow<B, (C0, C1, ...)>` | `Continue` | `Break` |
///
/// Formally, the trait bound is defined using [`Try`](std::ops::Try) and [`Residual`](std::ops::Residual) traits:
///
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
/// This means that, in theory, you could use `Poll<Result<T, E>>` and `Poll<Option<Result<T, E>>>`
/// together with other futures returning `Result<T, E>`!
/// However, to prevent such mess, these types are **currently not allowed**.
/// This restriction may be lifted once more unstable `try_*` methods and functions are stabilized.
///
/// Additionally, if the standard library introduces new types implementing [`Try`](std::ops::Try) or
/// [`Residual`](std::ops::Residual), this macro **will not** automatically support them.
/// You will need to wait for this crate to be updated, or both traits to be stabilized.
///
/// # Disclaimer
///
/// This macro **does not** require the Rust nightly or beta channels.
/// It is fully **usable on the stable release** of Rust.
///
/// # Possible Differences from Other Implementations
///
/// * `try_join_cyclic!` returns an instance of an anonymous type that implements [`Future`](std::future::Future),
///   instead of requiring it to be inside an `async` block. You will receive a warning if you neither
///   `.await`, [`poll`](std::future::Future::poll), nor return it.
///
/// * Input futures must implement [`IntoFuture`](std::future::IntoFuture),
///   and their outputs are not limited to [`Result`] types (see the table above for supported types).
///
/// * The returned future is (generally) smaller in size and more efficient.
///
/// * The returned future is [`Unpin`] if all input futures are [`Unpin`].
///
/// # Examples
/// ```rust
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
///
/// If you want to run one or more futures while performing other tasks,
/// this macro helps! Note that the additional tasks must come **after** all other futures you want to run:
///
/// ```rust
/// # use std::error::Error;
/// # #[tokio::main]
/// # async fn main() -> Result<(), Box<dyn Error>> {
/// use anony::try_join_cyclic;
/// use tokio::time::sleep;
/// use std::time::Duration;
///
/// async fn read_db() -> Result<String, Box<dyn Error>> {
///     sleep(Duration::from_secs(1)).await;
///     Ok("My secret".into())
/// }
///
/// let (secret_value, _) = try_join_cyclic!(read_db(), async {
///     // Perform other tasks here, either asynchronous or just blocking...
///     let a = 1;
///     let b = 2;
///     assert_eq!(a + b, 3);
///     Ok(())
/// }).await?;
///
/// assert_eq!(secret_value, "My secret");
/// # Ok(())
/// # }
/// ```
///
/// # Example Expansions
///
/// <https://github.com/discreaminant2809/anony/blob/master/examples/expansions/try_join_cyclic.rs>
#[proc_macro]
#[cfg(feature = "future")]
#[cfg_attr(docsrs, doc(cfg(feature = "future")))]
pub fn try_join_cyclic(token_stream: pm::TokenStream) -> pm::TokenStream {
    join::imp(token_stream, true, true)
        .unwrap_or_else(|e| e.into_compile_error())
        .into()
}

//! Provides various anonymous type constructs
//!
//! ## Macros
//!
//! * `r#struct`: creates an instance of an anonymous struct
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
//! // Since all of the fields implement `Debug`, the type of the instance implements it also!
//! assert_eq!(format!("{x:?}"), r#" { color: "Red", items: [1, 3, 5] }"#);
//! ```
//!
//! ## Features
//!
//! * `serde`: derives `serde`'s traits for anonymous structs. `serde` crate and its `derive` feature must exist in your crate

#![deny(missing_docs)]

mod anonymous_struct;

use proc_macro as pm;
use proc_macro2 as pm2;

/// Creates an instance of an anonymous struct.
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
/// You can move fields one by one
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
/// Pinning projection (use `project` method, like you use `pin-project` crate)
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
/// assert_eq!(o1.project().fut.poll(&mut cx), Poll::Ready(5));
/// ```
///
/// # Derived traits
///
/// This struct implements the following if all of its fields are implemented them:
/// * All traits in [`std::cmp`]
/// * [`Debug`]
/// * [`Hash`]
/// * [`Clone`]
/// * [`Copy`]
/// * `Serialize` (require `serde` feature)
#[proc_macro]
pub fn r#struct(token_stream: pm::TokenStream) -> pm::TokenStream {
    anonymous_struct::imp(token_stream)
        .unwrap_or_else(|e| e.into_compile_error())
        .into()
}

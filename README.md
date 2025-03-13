# anony

Provides various constructs for anonymous types.

## Macros

* [`struct!`]: Creates an instance of an anonymous struct.

```rust
use anony::r#struct;

let items = vec![1, 3, 5];

let x = r#struct! {
    name: "discreaminant".to_owned(),
    // Move the `items` variable into the struct
    items,
};

assert_eq!(x.name, "discreaminant");
assert_eq!(x.items, [1, 3, 5]);
```

* [`tuple!`]: Creates an instance of an anonymous tuple.

```rust
use anony::tuple;

let items = vec![1, 3, 5];

let x = tuple!("discreaminant".to_owned(), items);

assert_eq!(x.0, "discreaminant");
assert_eq!(x.1, [1, 3, 5]);
```

* [`join!`] and [`join_cyclic!`]: Join multiple futures.
  Requires the `future` feature.

```rust
use anony::join;

assert_eq!(join!(async { 2 }, async { "123" }).await, (2, "123"));
```

* [`try_join!`] and [`try_join_cyclic!`]: Join multiple futures, short-circuiting on a "break" value.
  Requires the `future` feature.

```rust
use anony::try_join;

assert_eq!(try_join!(async { Some(2) }, async { Some("123") }).await, Some((2, "123")));
assert_eq!(try_join!(async { Some(2) }, async { None::<i32> }).await, None);
```

## Example Macro Expansions

<https://github.com/discreaminant2809/anony/tree/master/examples/expansions>

## Features

* `serde`: Derives [`Serialize`] for anonymous structs and tuples.
  The [serde] crate must be included in your dependencies.
* `future`: Enables [`Future`] anonymous types, such as [`join!`].

## Nightly

Add this to your dependencies:

```toml
anony = { git = "https://github.com/discreaminant2809/anony.git", branch = "nightly" }
```

[`Future`]: https://doc.rust-lang.org/core/future/trait.Future.html
[`Serialize`]: https://docs.rs/serde/latest/serde/ser/trait.Serialize.html
[serde]: https://docs.rs/serde/latest/serde/index.html

[`struct!`]: https://docs.rs/anony/latest/anony/macro.struct.html
[`tuple!`]: https://docs.rs/anony/latest/anony/macro.tuple.html
[`join!`]: https://docs.rs/anony/latest/anony/macro.join.html
[`join_cyclic!`]: https://docs.rs/anony/latest/anony/macro.join_cyclic.html
[`try_join!`]: https://docs.rs/anony/latest/anony/macro.try_join.html
[`try_join_cyclic!`]: https://docs.rs/anony/latest/anony/macro.try_join_cyclic.html

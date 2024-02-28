# Provides various anonymous type constructs

## Macros

* [`struct!`]: creates an instance of an anonymous struct.

```rust
use anony::r#struct;

let items = vec![1, 3, 5];

let x = r#struct! {
    color: "Red".to_owned(),
    // move the `items` variable to the struct
    items
};

assert_eq!(x.color, "Red");
assert_eq!(x.items, [1, 3, 5]);
```

* [`join!`] and [`join_cyclic!`]: join multiple futures. Require `future` feature.

```rust
use anony::join;

assert_eq!(join!(async { 2 }, async { "123" }).await, (2, "123"));
```

## Example Macro Expansions

<https://github.com/discreaminant2809/anony/blob/master/examples/expansions.rs>

## Features

* `serde`: derives [`Serialize`] for anonymous structs. [serde] crate and its `derive` feature must exist in your crate.
* `future`: enables [`Future`] anonymous types, such as [`join!`].

[`struct!`]: https://docs.rs/anony/latest/anony/macro.struct.html
[`join!`]: https://docs.rs/anony/latest/anony/macro.join.html
[`join_cyclic!`]: https://docs.rs/anony/latest/anony/macro.join_cyclic.html
[`Serialize`]: https://docs.rs/serde/latest/serde/ser/trait.Serialize.html
[`Future`]: https://doc.rust-lang.org/core/future/trait.Future.html
[serde]: https://docs.rs/serde/latest/serde/index.html

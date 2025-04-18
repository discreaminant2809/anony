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

* [`combine_futures!`] and [`combine_futures_cyclic!`]:
  General-purpose future concurrency combinators. Requires the `future` feature.

```rust
let mut s = String::new();

let fut = combine_futures! {
    let ten = async { 10 } => continue {
        s.write_fmt(format_args!("{ten}"));
        ten
    }
    let zero = async { "0" } => continue s.push_str(zero),
};

assert_eq!(fut.await, (10, ()));
assert_eq!(s, "100");
```

```rust
let mut x = 2;

let fut = combine_futures_cyclic! {
    move
    match async { Some(1) } {
        Some(x) if x % 2 == 0 => continue,
        _ => break {
            x += 1;
            x
        }
    }
    continue async {},
    |_, _| -1
};

assert_eq!(fut.await, 3);
assert_eq!(x, 2);
```

## Example Macro Expansions

<https://github.com/discreaminant2809/anony/tree/master/examples/expansions>

## Features

* `serde`: Derives [`Serialize`] for anonymous structs and tuples.
  The [serde] crate must be included in your dependencies.
* `future`: Enables [`Future`] anonymous types, such as [`combine_futures!`].

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
[`combine_futures!`]: https://docs.rs/anony/latest/anony/macro.combine_futures.html
[`combine_futures_cyclic!`]: https://docs.rs/anony/latest/anony/macro.combine_futures_cyclic.html

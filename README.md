# Provides various anonymous type constructs

## Macros

* `r#struct`: creates an instance of an anonymous struct

```rust
use anony::r#struct;

let items = vec![1, 3, 5];

let x = r#struct! {
    color: "Red".to_owned(),
    // move the `items` variable to the struct
    items
};

// Since all of the fields implement `Debug`, the type of the instance implements it also!
assert_eq!(format!("{x:?}"), r#" { color: "Red", items: [1, 3, 5] }"#);
```

## Features

* `serde`: derives `serde`'s traits for anonymous structs. `serde` crate and its `derive` feature must exist in your crate
* `future`: allow anonymous types implementing `Future`, such as `join!`

## Todos

* "Functional traits": quickly create an instance of a struct implementing a trait having exactly one required method
* `join!` and `join_cyclic!`: awaits multiple futures concurrently

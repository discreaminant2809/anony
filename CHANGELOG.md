# Changelog

## 0.6.1

### Fixed

- Hygience fix.
- `combine_futures_cyclic` now panics with a correct message.

## 0.6.0

### Added

- `combine_futures(_cyclic)` macro, a generalization of various future combinatorics, such as `join!` and `select!`.

### Deprecated

- `(try_)join(_cyclic)` macro.

## 0.5.3

### Added

- For each macro, added a link to their corresponding example expansions.

### Changed

- Major documentation change.
- `(try_)join_cyclic`'s internals.

## 0.5.2

### Added

- implement `Default` for types created by `r#struct!` and `tuple!`.

### Changed

- `(try_)join_cyclic`'s internals.

### Fixed

- Hygience fix.

## 0.5.1

### Added

- `#[track_caller]` in case of `Result` for `try_join(_cyclic)!`.
- `categories` and `keywords` in `Cargo.toml`.

### Changed

- `serde` feature now does not require `serde`'s `derive` feature anymore, and the struct's name is hidden for `r#struct!`.
- `#[must_use]` message when the future is unused.
- Method `into_tuple` instead of `From` implementation for `tuple!`.

### Fixed

- Fixed `project_ref` method that should receive a `Pin<&Self>` instead in case of empty struct.

## 0.5.0

### Added

- `try_join` and `try_join_cyclic!`
- `tuple!`

### Removed

- `Todos` in `README.md`

### Fixed

- join_cyclic! now correctly panics with "\`join_cyclic!\` future polled after completion"
- Other minor fixes

## 0.4.2

### Changed

- `join` and `join_cyclic!` require `IntoFuture` instead of `Future` for all future arguments
- Optimize `join` and `join_cyclic!`

## 0.4.1

### Added

- More examples, including example expansions
  
### Changed

- Slightly more efficient polling logic of `join_cyclic!`
- Better documentation

## 0.4.0

### Added

- `join` and `join_cyclic!` which can be used to concurrently await multiple futures

## 0.3.1

### Added

- Immutable pinning projection for `r#struct!`

### Changed

- Mutable pinning projection method's name is changed from `project` to `project_mut`

## 0.3.0

### Added

- Pinning projection for `r#struct!`

## 0.2.0

### Removed

- `into_inner` method of the anonymous struct generated by `r#struct!`

## 0.1.0

### Added

- This crate

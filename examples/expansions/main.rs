#![allow(
    unused,
    unsafe_op_in_unsafe_fn,
    clippy::match_single_binding,
    dropping_references,
    irrefutable_let_patterns,
    dropping_copy_types,
    clippy::unit_arg,
    clippy::redundant_closure_call,
    clippy::redundant_pattern_matching,
    deprecated
)]

mod anonymous_struct;
mod combine_futures;
mod join;
mod join_cyclic;
mod try_join;
mod try_join_cyclic;
mod tuple;

fn main() {}

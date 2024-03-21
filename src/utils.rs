use std::fmt::Display;

use quote::format_ident;
use syn::{Ident, Index};

pub(crate) fn i_generics(prefix: impl Display, n: usize) -> Vec<Ident> {
    (0..n).map(|i| format_ident!("{prefix}{i}")).collect()
}

pub(crate) fn tuple_indices(n: usize) -> Vec<Index> {
    (0..n).map(Index::from).collect()
}

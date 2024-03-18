use quote::format_ident;
use syn::{Ident, Index};

pub(crate) fn t_generics(n: usize) -> Vec<Ident> {
    (0..n).map(|i| format_ident!("T{i}")).collect()
}

pub(crate) fn tuple_indices(n: usize) -> Vec<Index> {
    (0..n).map(Index::from).collect()
}

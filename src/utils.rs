use std::fmt::Display;

use quote::format_ident;
use syn::{parse::Parse, Expr, Token};
use syn::{Ident, Index};

pub(crate) fn i_idents(prefix: impl Display, n: usize) -> Vec<Ident> {
    (0..n).map(|i| format_ident!("{prefix}{i}")).collect()
}

pub(crate) fn tuple_indices(n: usize) -> Vec<Index> {
    (0..n).map(Index::from).collect()
}

pub(crate) enum TupleLikeTypeInput {
    Direct {
        values: Vec<Expr>,
    },
    #[allow(unused)]
    Typing {
        arity: usize,
    },
}

impl Parse for TupleLikeTypeInput {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        if input.peek(Token![type]) && input.peek2(Token![;]) {
            // Parse as `Typing`
            input.parse::<Token![type]>()?;
            input.parse::<Token![;]>()?;
            let arity: syn::LitInt = input.parse()?;

            return Ok(Self::Typing {
                arity: arity.base10_parse()?,
            });
        }

        // Parse as `Direct`
        let values = input.parse_terminated(Expr::parse, Token![,])?;
        Ok(Self::Direct {
            values: values.into_iter().collect(),
        })
    }
}

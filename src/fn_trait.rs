use pm2::Span;
use syn::{ItemImpl, Type};
use quote::quote;

use crate::{pm, pm2};

#[allow(dead_code)]
pub(crate) fn imp(tt: pm::TokenStream) -> syn::Result<pm2::TokenStream> {
    let item_impl = syn::parse::<ItemImpl>(tt)?;
    item_impl.trait_.ok_or_else(|| {
        syn::Error::new(Span::call_site(), "missing which trait")
    })?;

    match *item_impl.self_ty {
        Type::Infer(_) => {}
        _ => return Err(syn::Error::new(
            Span::call_site(),
            "cannot specify type for the anonymous struct\nuse `_` instead"
        ))
    }

    Ok(quote!())
}
use quote::quote;
use syntax::Input;

use crate::pm2;

mod syntax;

pub(crate) fn imp(tt: crate::pm::TokenStream, is_cyclic: bool) -> syn::Result<pm2::TokenStream> {
    let input = syn::parse::<Input>(tt)?;

    if input.continue_collector.is_some() {
        let mut always_breaks_error = syn::Error::new_spanned(
            input.continue_collector.as_ref().unwrap(),
            "cannot specify continue collector because branches above always breaks",
        );
        let mut any_always_breaks = false;

        for branch in &input.branches {
            if branch.always_breaks() {
                any_always_breaks = true;
                always_breaks_error.combine(syn::Error::new_spanned(
                    branch,
                    "this branch always breaks, which causes the error",
                ));
            }
        }

        if any_always_breaks {
            return Err(always_breaks_error);
        }
    }

    // TODO: Generate the macro

    Ok(quote! {{}})
}

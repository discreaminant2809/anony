#![cfg(feature = "future")]

use quote::quote;
use syn::{parse::Parse, punctuated::Punctuated, Expr, Token};

use crate::pm2;

pub(crate) fn imp_join(tt: crate::pm::TokenStream) -> syn::Result<pm2::TokenStream> {
    // The implementation here exists even before my `r#struct!` macro, so the code here doesn't really match with `r#struct!`!
    // However, their `phylosophies` are consistent: open a new scope, "capture" all varriables with tuple matching,
    // then open another scope, define structs here and finally move all of these variables to create a new anonymous struct

    let exprs = parse_exprs(tt)?;

    let must_use = quote!(
        #[must_use = "unlike other `join!` implementations, this one returns a `Future` that must be explicitly `.await`ed or polled"]
    );

    if exprs.is_empty() {
        return Ok(quote!({
            #must_use
            struct Join;

            impl ::core::future::Future for Join {
                type Output = ();

                #[inline]
                fn poll(
                    self: ::core::pin::Pin<&mut Self>,
                    _cx: &mut ::core::task::Context<'_>
                ) -> ::core::task::Poll<<Self as ::core::future::Future>::Output> {
                    ::core::task::Poll::Ready(())
                }
            }

            Join
        }));
    }

    let captured_futs_input = exprs.iter();

    let fut_generics =
        (0..exprs.len()).map(|i| pm2::Ident::new(&format!("F{i}"), pm2::Span::call_site()));

    let fut_generics_at_impl = fut_generics.clone();

    let fut_generics_bounded_at_decl = fut_generics.clone();

    let fut_generics_bounded_at_impl = fut_generics_bounded_at_decl.clone();

    let fut_generics_at_maybe_done = fut_generics.clone();

    let outputs = fut_generics.clone();

    let maybe_done_vars = (0..exprs.len())
        .map(|i| pm2::Ident::new(&format!("maybe_done{i}"), pm2::Span::call_site()));

    let maybe_done_vars_at_destructuring = maybe_done_vars.clone();

    let maybe_done_vars_at_polling = maybe_done_vars.clone();

    let o_matching =
        (0..exprs.len()).map(|i| pm2::Ident::new(&format!("o{i}"), pm2::Span::call_site()));

    let o_return = o_matching.clone();

    let maybe_done_vars_at_take_outputs = maybe_done_vars.clone();

    // let unpin_generics_bounded_at_impl = fut_generics.clone();
    // let unpin_generics_at_impl = unpin_generics_bounded_at_impl.clone();

    let futs_to_maybe_done = (0..exprs.len()).map(|i| {
        let i = syn::Index::from(i);
        quote!(futs.#i)
    });

    Ok(quote!({
        // futures in the macro input
        let futs = (#(#captured_futs_input),* ,);

        // Open another scope so that the futures in the input can't access anything within it
        {
            // Put a "ghost" `#[pin_project]` macro to help know which one is structurally pinned
            // #[pin_project]
            enum MaybeDone<F: ::core::future::Future> {
                Pending(
                    // #[pin]
                    F
                ),
                // It might seem inefficient... but the compiler can optimize the layout
                Ready(::core::option::Option<<F as ::core::future::Future>::Output>),
            }

            // Only the wrapped future is considered, not its output, since the former is structurally pinned, while the latter isn't
            impl<F: ::core::future::Future + ::core::marker::Unpin> ::core::marker::Unpin for MaybeDone<F> {}

            impl<F: ::core::future::Future> MaybeDone<F> {
                fn poll(mut self: ::core::pin::Pin<&mut Self>, cx: &mut ::core::task::Context<'_>) -> bool {
                    // SAFETY: pinning projection
                    match unsafe {
                        ::core::pin::Pin::get_unchecked_mut(::core::pin::Pin::as_mut(&mut self))
                    } {
                        Self::Pending(fut) => {
                            // SAFETY: pinning projection. `fut` is structurally pinned
                            match ::core::future::Future::poll(unsafe {
                                ::core::pin::Pin::new_unchecked(fut)
                            }, cx) {
                                ::core::task::Poll::Ready(o) => {
                                    ::core::pin::Pin::set(&mut self, Self::Ready(::core::option::Option::Some(o)));
                                    true
                                }
                                _ => false
                            }
                        }
                        _ => true
                    }
                }

                fn take_output(self: ::core::pin::Pin<&mut Self>) -> ::core::option::Option<<F as ::core::future::Future>::Output> {
                    // SAFETY: pinning projection
                    match unsafe {
                        ::core::pin::Pin::get_unchecked_mut(self)
                    } {
                        Self::Pending(_) => {
                            ::core::option::Option::None
                        }
                        // the output is NOT structurally pinned
                        Self::Ready(o) => ::core::option::Option::take(o)
                    }
                }
            }

            #must_use
            // #[pin_project]
            enum Join<#(#fut_generics_bounded_at_decl: ::core::future::Future),*> {
                // We encapsulate fields using enum variant!
                // To access these fields, we must pattern matching with this variant, which requires us to write `Join::Inner`...
                // wait... we can't even specify the struct's name to begin with!
                // Therefore, we have effectively made these fields "private"!
                Inner(#(
                    // Collectively, `Join` only requires all wrapped futures to be `Unpin`,
                    // and don't care whether their outputs are `Unpin` or not
                    // #[pin]
                    MaybeDone<#fut_generics_at_maybe_done>
                ),*)
            }

            impl<#(#fut_generics_bounded_at_impl: ::core::future::Future),*> ::core::future::Future for Join<#(#fut_generics_at_impl),*> {
                // we include the additional comma in the end so that we should return an 1-ary tuple on having just 1 future to join
                // so that it consistently with 1-ary tuple's construct.
                type Output = (#(<#outputs as ::core::future::Future>::Output),* ,);

                fn poll(
                    self: ::core::pin::Pin<&mut Self>,
                    cx: &mut ::core::task::Context<'_>
                ) -> ::core::task::Poll<<Self as ::core::future::Future>::Output> {
                    // SAFETY: pinning projection! All `Maybedone`s are structurally pinned
                    let Self::Inner(#(#maybe_done_vars_at_destructuring),*) = unsafe {
                        ::core::pin::Pin::get_unchecked_mut(self)
                    };

                    if !unsafe {
                        #(
                            MaybeDone::poll(::core::pin::Pin::new_unchecked(#maybe_done_vars_at_polling), cx)
                        )&* // only use a single ampersand here, since we must poll all of these futures and not short-circuit any
                    }{
                        return ::core::task::Poll::Pending;
                    }

                    unsafe {
                        match (#(MaybeDone::take_output(::core::pin::Pin::new_unchecked(#maybe_done_vars_at_take_outputs))),* ,) {
                            (#(::core::option::Option::Some(#o_matching)),* ,) => ::core::task::Poll::Ready((#(#o_return),* ,)),
                            _ => ::core::panic!("`join!` future polled after completion")
                        }
                    }
                }
            }

            Join::Inner(#(MaybeDone::Pending(#futs_to_maybe_done)),*)
        }
    }))
}

fn parse_exprs(tt: crate::pm::TokenStream) -> syn::Result<Punctuated<Expr, Token![,]>> {
    struct Exprs(Punctuated<Expr, Token![,]>);

    impl Parse for Exprs {
        fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
            input.call(Punctuated::parse_terminated).map(Exprs)
        }
    }

    syn::parse::<Exprs>(tt).map(|exprs| exprs.0)
}

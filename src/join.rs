#![cfg(feature = "future")]

use quote::{format_ident, quote};
use syn::{parse::Parse, punctuated::Punctuated, Expr, Token};

use crate::pm2;

pub(crate) fn imp(tt: crate::pm::TokenStream, is_cyclic: bool) -> syn::Result<pm2::TokenStream> {
    // The implementation here exists even before my `r#struct!` macro, so the code here doesn't really match with `r#struct!`!
    // However, their `phylosophies` are consistent: open a new scope, "capture" all varriables with tuple matching,
    // then open another scope, define structs here and finally move all of these variables to create a new anonymous struct

    let exprs = parse_exprs(tt)?;

    let must_use = quote!(
        #[must_use = "unlike other `join!` implementations, this one returns a `Future` that must be explicitly `.await`ed or polled"]
    );
    let join_ty_at_decl = if is_cyclic {
        quote!(JoinCyclic)
    } else {
        quote!(Join)
    };
    let join_ty_at_impl = join_ty_at_decl.clone();
    let join_ty_at_ret = join_ty_at_decl.clone();

    if exprs.is_empty() {
        return Ok(quote!({
            #must_use
            struct #join_ty_at_decl;

            impl ::core::future::Future for #join_ty_at_impl {
                type Output = ();

                #[inline]
                fn poll(
                    self: ::core::pin::Pin<&mut Self>,
                    _cx: &mut ::core::task::Context<'_>
                ) -> ::core::task::Poll<<Self as ::core::future::Future>::Output> {
                    ::core::task::Poll::Ready(())
                }
            }

            #join_ty_at_ret
        }));
    } else if exprs.len() == 1 {
        let expr = exprs.into_iter().next().unwrap();
        return Ok(quote!({
            let fut = #expr;

            {
                #must_use
                #[repr(transparent)]
                enum #join_ty_at_decl<F> {
                    Inner(F)
                }

                impl<F: ::core::future::Future> ::core::future::Future for #join_ty_at_impl<F> {
                    type Output = (<F as ::core::future::Future>::Output,);

                    #[inline]
                    fn poll(
                        self: ::core::pin::Pin<&mut Self>,
                        cx: &mut ::core::task::Context<'_>
                    ) -> ::core::task::Poll<<Self as ::core::future::Future>::Output> {
                        // SAFETY: pinning projection. The wrapped future is structurally pinned
                        ::core::future::Future::poll(unsafe {
                            self.map_unchecked_mut(|this| {
                                let Self::Inner(fut) = this;
                                fut
                            })
                        }, cx).map(|o| (o,))
                    }
                }

                #join_ty_at_ret::Inner(fut)
            }
        }));
    }

    let fut_count = exprs.len();

    let captured_futs_input = exprs.iter();

    let fut_generics = (0..exprs.len()).map(|i| format_ident!("F{i}"));

    let fut_generics_at_impl = fut_generics.clone();

    let fut_generics_bounded_at_decl = fut_generics.clone();

    let fut_generics_bounded_at_impl = fut_generics_bounded_at_decl.clone();

    let fut_generics_at_maybe_done = fut_generics.clone();

    let outputs = fut_generics.clone();

    let maybe_done_vars = (0..exprs.len()).map(|i| format_ident!("maybe_done{i}"));

    let maybe_done_vars_at_destructuring = maybe_done_vars.clone();

    let maybe_done_vars_at_polling = maybe_done_vars.clone();

    let (skip_next_time_ty, skip_next_time_var, skip_next_time_init) = if is_cyclic {
        (quote!(usize), quote!(skip_next_time), quote!(0))
    } else {
        (quote!(), quote!(), quote!())
    };

    let polling_strategy = if is_cyclic {
        // For `join_cyclic!`, we will use `tokio`'s approach
        // See https://docs.rs/tokio/latest/src/tokio/macros/join.rs.html#57-166
        // Tokio's code is licensed under the MIT License: https://github.com/tokio-rs/tokio/blob/master/LICENSE
        quote!(
            const COUNT: usize = #fut_count;
            let mut done = true;
            let mut to_run = COUNT;
            let mut to_skip = ::core::mem::replace(
                skip_next_time,
                // `COUNT` is always > 1 since we've guarded the `0` and `1` cases explicitly (for more efficient logics)
                // so `clippy::modulo_one` won't be triggered
                (*skip_next_time + 1) % COUNT
            );

            loop {
                #(
                    if to_skip > 0 {
                        to_skip -= 1;
                    } else {
                        // We alter the logic a bit, since we guarantee that `to_run` starts with a number > 1
                        // because we initialize it with `COUNT`.
                        // By doing this, we reduce the number of checks
                        done &= MaybeDone::poll(::core::pin::Pin::new_unchecked(#maybe_done_vars_at_polling), cx);

                        if to_run <= 1 { // if we are the last one...
                            break done;
                        }
                        to_run -= 1;
                    }
                )*
            }
        )
    } else {
        quote!(
            #(
                MaybeDone::poll(::core::pin::Pin::new_unchecked(#maybe_done_vars_at_polling), cx)
            )&* // only use a single ampersand here, since we must poll all of these futures and not short-circuit any
        )
    };

    let o_matching = (0..exprs.len()).map(|i| format_ident!("o{i}"));

    let o_return = o_matching.clone();

    let maybe_done_vars_at_take_outputs = maybe_done_vars.clone();

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
            // We strictly adhere to the invariant regarding structurally pinned fields
            // If we don't add this, the future's output type is considered also, which shouldn't
            // since the output is NOT structurally pinned
            impl<F: ::core::future::Future + ::core::marker::Unpin> ::core::marker::Unpin for MaybeDone<F> {}

            impl<F: ::core::future::Future> MaybeDone<F> {
                // This method is only used to poll and check the status of the wrapped future. It isn't for getting the result!
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
            enum #join_ty_at_decl<#(#fut_generics_bounded_at_decl: ::core::future::Future),*> {
                // We encapsulate fields using enum variant!
                // To access these fields, we must pattern matching with this variant, which requires us to write `Join::Inner`...
                // wait... we can't even specify the struct's name to begin with!
                // Therefore, we have effectively made these fields "private"!
                Inner(
                    #(
                        // Collectively, `Join` only requires all wrapped futures to be `Unpin`,
                        // and don't care whether their outputs are `Unpin` or not
                        // #[pin]
                        MaybeDone<#fut_generics_at_maybe_done>
                    ),*,
                    #skip_next_time_ty
                )
            }

            impl<#(#fut_generics_bounded_at_impl: ::core::future::Future),*> ::core::future::Future for #join_ty_at_impl<#(#fut_generics_at_impl),*> {
                // we include the additional comma in the end
                // previously we should return an 1-ary tuple on having just 1 future to join so that it consistently with 1-ary tuple's construct.
                // However, it is not neccessary anymore! We generate a different implementation for 1-ary `join!``
                type Output = (#(<#outputs as ::core::future::Future>::Output),* ,);

                fn poll(
                    self: ::core::pin::Pin<&mut Self>,
                    cx: &mut ::core::task::Context<'_>
                ) -> ::core::task::Poll<<Self as ::core::future::Future>::Output> {
                    // SAFETY: pinning projection! All `Maybedone`s are structurally pinned, while `skip_next_time` is NOT
                    let Self::Inner(#(#maybe_done_vars_at_destructuring),*, #skip_next_time_var) = unsafe {
                        ::core::pin::Pin::get_unchecked_mut(self)
                    };

                    if !unsafe {
                        #polling_strategy
                    } {
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

            #join_ty_at_ret::Inner(#(MaybeDone::Pending(#futs_to_maybe_done)),*, #skip_next_time_init)
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

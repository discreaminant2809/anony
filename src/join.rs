#![cfg(feature = "future")]

use quote::{format_ident, quote, ToTokens};
use syn::{parse::Parse, punctuated::Punctuated, Expr, Ident, Token};

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
    let join_ty_at_ret: pm2::TokenStream = join_ty_at_decl.clone();
    let neccessary_import = neccessary_import();

    if exprs.is_empty() {
        return Ok(quote!({
            #neccessary_import

            #must_use
            struct #join_ty_at_decl;

            impl Future for #join_ty_at_impl {
                type Output = ();

                #[inline]
                fn poll(
                    self: Pin<&mut Self>,
                    _cx: &mut Context<'_>
                ) -> Poll<<Self as Future>::Output> {
                    Poll::Ready(())
                }
            }

            #join_ty_at_ret
        }));
    }

    if exprs.len() == 1 {
        let expr = exprs.into_iter().next().unwrap();
        return Ok(quote!({
            let fut = #expr;

            {
                #neccessary_import

                #must_use
                #[repr(transparent)]
                enum #join_ty_at_decl<F: Future> {
                    Inner(F)
                }

                impl<F: Future> Future for #join_ty_at_impl<F> {
                    type Output = (<F as Future>::Output,);

                    #[inline]
                    fn poll(
                        self: Pin<&mut Self>,
                        cx: &mut Context<'_>
                    ) -> Poll<<Self as Future>::Output> {
                        // SAFETY: pinning projection. The wrapped future is structurally pinned
                        Future::poll(unsafe {
                            Pin::map_unchecked_mut(self, |this| {
                                let Self::Inner(fut) = this;
                                fut
                            })
                        }, cx).map(|o| (o,))
                    }
                }

                use ::core::future::IntoFuture;
                #join_ty_at_ret::Inner(IntoFuture::into_future(fut))
            }
        }));
    }

    let fut_count = exprs.len();

    let captured_futs_input = exprs.iter();

    let fut_generics = (0..exprs.len()).map(|i| format_ident!("F{i}"));

    let fut_generics_at_impl_ty = fut_generics.clone();

    let fut_generics_bounded_at_decl = fut_generics.clone();

    let bounded_generics_at_decl = quote!(
        <#(#fut_generics_bounded_at_decl: Future),*>
    );
    let bounded_generics_at_impl = bounded_generics_at_decl.clone();

    let fut_generics_at_maybe_done = fut_generics.clone();
    let outputs = fut_generics.clone();
    let maybe_done_vars = (0..exprs.len()).map(|i| format_ident!("maybe_done{i}"));
    let maybe_done_vars_at_destructuring = maybe_done_vars.clone();

    let (skip_next_time_ty, skip_next_time_var, skip_next_time_init) = if is_cyclic {
        (quote!(usize), quote!(skip_next_time), quote!(0))
    } else {
        (quote!(), quote!(), quote!())
    };

    let do_sth_w_done = maybe_done_vars
        .clone()
        .map(|maybe_done_var| quote!(done &= MaybeDone::poll(Pin::new_unchecked(&mut *#maybe_done_var), cx);));
    let polling_strategy = if is_cyclic {
        cyclical_poll(fut_count, do_sth_w_done)
    } else {
        quote!(
            let mut done = true;
            #(#do_sth_w_done)*
            done
        )
    };

    let maybe_done_vars_at_take_outputs = maybe_done_vars.clone();

    let futs_to_maybe_done = (0..exprs.len()).map(|i| {
        let i = syn::Index::from(i);
        quote!(futs.#i)
    });

    let maybe_done_force_take_output = maybe_done_force_take_output(quote!(<F as Future>::Output));

    let take_output = take_output(
        if is_cyclic { "join_cyclic" } else { "join" },
        maybe_done_vars_at_take_outputs,
        quote!(),
    );

    let future_impl = future_impl(
        bounded_generics_at_impl,
        quote!(#join_ty_at_impl<#(#fut_generics_at_impl_ty),*>),
        quote!((#(<#outputs as Future>::Output),* ,)),
        quote!(),
        quote!(
            // SAFETY: pinning projection! All `Maybedone`s are structurally pinned, while `skip_next_time` is NOT
            let Self::Inner(#(#maybe_done_vars_at_destructuring),*, #skip_next_time_var) = unsafe {
                Pin::get_unchecked_mut(self)
            };

            if !unsafe {
                #polling_strategy
            } {
                return Poll::Pending;
            }

            #take_output
        ),
    );

    Ok(quote!({
        // futures in the macro input
        let futs = (#(#captured_futs_input),* ,);

        // Open another scope so that the futures in the input can't access anything within it
        {
            #neccessary_import
            use ::core::option::Option::{self, Some, None}; // the `neccessary_import` hasn't imported this
            use ::core::hint::unreachable_unchecked;

            // Put a "ghost" `#[pin_project]` macro to help know which one is structurally pinned
            // #[pin_project]
            enum MaybeDone<F: Future> {
                Pending(
                    // #[pin]
                    F
                ),
                // It might seem inefficient... but the compiler can optimize the layout
                Ready(Option<<F as Future>::Output>),
            }

            // Only the wrapped future is considered, not its output, since the former is structurally pinned, while the latter isn't
            // We strictly adhere to the invariant regarding structurally pinned fields
            // If we don't add this, the future's output type is considered also, which shouldn't
            // since the output is NOT structurally pinned
            use ::core::marker::Unpin;
            impl<F: Future + Unpin> Unpin for MaybeDone<F> {}

            impl<F: Future> MaybeDone<F> {
                // This method is only used to poll and check the status of the wrapped future. It isn't for getting the result!
                fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> bool {
                    // SAFETY: pinning projection
                    match unsafe {
                        Pin::get_unchecked_mut(Pin::as_mut(&mut self))
                    } {
                        Self::Pending(fut) => {
                            // SAFETY: pinning projection. `fut` is structurally pinned
                            match Future::poll(unsafe {
                                Pin::new_unchecked(fut)
                            }, cx) {
                                Poll::Ready(o) => {
                                    Pin::set(&mut self, Self::Ready(Some(o)));
                                    true
                                }
                                _ => false
                            }
                        }
                        _ => true
                    }
                }

                #maybe_done_force_take_output
            }

            #must_use
            // #[pin_project]
            enum #join_ty_at_decl #bounded_generics_at_decl {
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

            #future_impl

            use ::core::future::IntoFuture;
            #join_ty_at_ret::Inner(
                #(
                    MaybeDone::Pending(IntoFuture::into_future(#futs_to_maybe_done))
                ),*,
                #skip_next_time_init
            )
        }
    }))
}

pub(crate) fn imp_try(
    tt: crate::pm::TokenStream,
    is_cyclic: bool,
) -> syn::Result<pm2::TokenStream> {
    let exprs = parse_exprs(tt)?;

    let must_use = quote!(
        #[must_use = "unlike other `try_join!` implementations, this one returns a `Future` that must be explicitly `.await`ed or polled"]
    );
    let join_ty_at_decl = if is_cyclic {
        quote!(TryJoinCyclic)
    } else {
        quote!(TryJoin)
    };
    let join_ty_at_impl = join_ty_at_decl.clone();
    let join_ty_at_ret = join_ty_at_decl.clone();
    let try_trait_and_import = try_trait_and_import();
    let neccessary_import = neccessary_import();

    if exprs.is_empty() {
        return Ok(quote!({
            #neccessary_import
            use ::core::marker::PhantomData;

            #try_trait_and_import

            #must_use
            #[repr(transparent)]
            enum #join_ty_at_decl<T: Try<Output = ()>> {
                Inner(PhantomData<T>),
            };

            impl<T: Try<Output = ()>> Future for #join_ty_at_impl<T> {
                type Output = T;

                #[inline]
                fn poll(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<<Self as Future>::Output> {
                    Poll::Ready(Try::from_output(()))
                }
            }

            #join_ty_at_ret::Inner(PhantomData)
        }));
    }

    if exprs.len() == 1 {
        let expr = exprs.into_iter().next().unwrap();
        return Ok(quote!({
            let fut = #expr;

            {
                #neccessary_import

                #try_trait_and_import

                #must_use
                #[repr(transparent)]
                enum #join_ty_at_decl<F> {
                    Inner(F),
                }

                impl<F: Future, R: Residual<(<<F as Future>::Output as Try>::Output,)>> Future for #join_ty_at_impl<F>
                where
                    <F as Future>::Output: Try<Residual = R>
                {
                    type Output =
                        <R as Residual<(<<F as Future>::Output as Try>::Output,)>>::TryType;

                    #[inline]
                    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<<Self as Future>::Output> {
                        // SAFETY: pinning projection. The wrapped future is structurally pinned
                        Future::poll(
                            unsafe {
                                Pin::map_unchecked_mut(self, |this| {
                                    let Self::Inner(fut) = this;
                                    fut
                                })
                            },
                            cx
                        )
                        .map(|o| match Try::branch(o) {
                            ControlFlow::Continue(c) => Try::from_output((c,)),
                            ControlFlow::Break(b) => Try::from_residual(b),
                        })
                    }
                }

                use ::core::future::IntoFuture;
                #join_ty_at_ret::Inner(IntoFuture::into_future(fut))
            }
        }));
    }

    let fut_count = exprs.len();

    let captured_futs_input = exprs.iter();

    let fut_generics = (0..exprs.len()).map(|i| format_ident!("F{i}"));

    let fut_generics_at_impl_ty = fut_generics.clone();

    let fut_generics_bounded_at_decl = fut_generics.clone();
    let fut_generics_bounded_at_where = fut_generics.clone();
    let fut_generics_bounded_at_bound = fut_generics.clone();
    let residual_trait_at_decl = quote!(
        Residual<(#(
            <<#fut_generics_bounded_at_bound as Future>::Output as Try>::Output
        ),*,)>
    );
    let residual_trait_at_output_type = residual_trait_at_decl.clone();
    let bounded_generics_at_decl = quote!(
        <#(#fut_generics_bounded_at_decl: Future),*, R: #residual_trait_at_decl>
    );
    let bounded_generics_at_impl = bounded_generics_at_decl.clone();
    let where_clause_at_decl = quote!(
        where
            #(
                <#fut_generics_bounded_at_where as Future>::Output: Try<Residual = R>
            ),*
    );
    let where_clause_at_impl = where_clause_at_decl.clone();

    let fut_generics_at_maybe_done = fut_generics.clone();
    // let outputs = fut_generics.clone();
    let maybe_done_vars = (0..exprs.len()).map(|i| format_ident!("maybe_done{i}"));
    let maybe_done_vars_at_destructuring = maybe_done_vars.clone();

    let (skip_next_time_ty, skip_next_time_var, skip_next_time_init) = if is_cyclic {
        (quote!(usize), quote!(skip_next_time), quote!(0))
    } else {
        (quote!(), quote!(), quote!())
    };

    let do_sth_w_done = maybe_done_vars.clone().map(|maybe_done_var| {
        quote!(match Pin::new_unchecked(&mut *#maybe_done_var).poll(cx) {
            ControlFlow::Continue(ready) => done &= ready,
            ControlFlow::Break(r) => return Poll::Ready(Try::from_residual(r)),
        })
    });
    let polling_strategy = if is_cyclic {
        cyclical_poll(fut_count, do_sth_w_done)
    } else {
        quote!(
            let mut done = true;
            #(#do_sth_w_done)*
            done
        )
    };

    let maybe_done_vars_at_take_outputs = maybe_done_vars.clone();

    let futs_to_maybe_done = (0..exprs.len()).map(|i| {
        let i = syn::Index::from(i);
        quote!(futs.#i)
    });

    let maybe_done_force_take_output =
        maybe_done_force_take_output(quote!(<<F as Future>::Output as Try>::Output));

    let take_output = take_output(
        if is_cyclic {
            "try_join_cyclic"
        } else {
            "try_join"
        },
        maybe_done_vars_at_take_outputs,
        quote!(Try::from_output),
    );

    let future_impl = future_impl(
        bounded_generics_at_impl,
        quote!(#join_ty_at_impl<#(#fut_generics_at_impl_ty),*, R>),
        quote!(<R as #residual_trait_at_output_type>::TryType),
        where_clause_at_impl,
        quote!(
            // SAFETY: pinning projection! All `Maybedone`s are structurally pinned, while `skip_next_time` is NOT
            let Self::Inner(#(#maybe_done_vars_at_destructuring),*, #skip_next_time_var) = unsafe {
                Pin::get_unchecked_mut(self)
            };

            if !unsafe {
                #polling_strategy
            } {
                return Poll::Pending;
            }

            #take_output
        ),
    );

    Ok(quote!({
        // futures in the macro input
        let futs = (#(#captured_futs_input),* ,);

        // Open another scope so that the futures in the input can't access anything within it
        {
            #neccessary_import
            use ::core::hint::unreachable_unchecked;

            #try_trait_and_import

            // Put a "ghost" `#[pin_project]` macro to help know which one is structurally pinned
            // #[pin_project]
            enum MaybeDone<F: Future>
            where
                F::Output: Try,
            {
                Pending(
                    // #[pin]
                    F
                ),
                // It might seem inefficient... but the compiler can optimize the layout
                Ready(Option<<<F as Future>::Output as Try>::Output>),
            }

            // Only the wrapped future is considered, not its output, since the former is structurally pinned, while the latter isn't
            // We strictly adhere to the invariant regarding structurally pinned fields
            // If we don't add this, the future's output type is considered also, which shouldn't
            // since the output is NOT structurally pinned
            use ::core::marker::Unpin;
            impl<F: Future + Unpin> Unpin for MaybeDone<F>
            where
                <F as Future>::Output: Try,
            {}

            impl<F: Future> MaybeDone<F>
            where
                <F as Future>::Output: Try,
            {
                // This method is only used to poll and check the status of the wrapped future. It isn't for getting the result!
                fn poll(
                    mut self: Pin<&mut Self>,
                    cx: &mut Context<'_>,
                ) -> ControlFlow<<F::Output as Try>::Residual, bool> {
                    // SAFETY: pinning projection
                    unsafe {
                        match Pin::get_unchecked_mut(Pin::as_mut(&mut self)) {
                            MaybeDone::Pending(fut) => match Future::poll(Pin::new_unchecked(fut), cx) {
                                Poll::Ready(o) => match Try::branch(o) {
                                    ControlFlow::Continue(c) => {
                                        Pin::set(&mut self, Self::Ready(Some(c)));
                                        ControlFlow::Continue(true)
                                    }
                                    ControlFlow::Break(b) => ControlFlow::Break(b),
                                },
                                Poll::Pending => ControlFlow::Continue(false),
                            },
                            MaybeDone::Ready(_) => ControlFlow::Continue(true),
                        }
                    }
                }

                #maybe_done_force_take_output
            }

            #must_use
            // #[pin_project]
            enum #join_ty_at_decl #bounded_generics_at_decl #where_clause_at_decl {
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

            #future_impl

            use ::core::future::IntoFuture;
            #join_ty_at_ret::Inner(
                #(
                    MaybeDone::Pending(IntoFuture::into_future(#futs_to_maybe_done))
                ),*,
                #skip_next_time_init
            )
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

/// Option, Result, and ControlFlow are imported already
fn try_trait_and_import() -> pm2::TokenStream {
    quote!(
        use ::core::ops::ControlFlow;
        use ::core::option::Option::{self, None, Some};
        use ::core::result::Result::{self, Err, Ok};

        trait Try {
            type Output;
            type Residual;

            fn from_residual(residual: Self::Residual) -> Self;
            fn from_output(output: Self::Output) -> Self;

            fn branch(self) -> ControlFlow<Self::Residual, Self::Output>;
        }

        trait Residual<O> {
            type TryType: Try<Output = O, Residual = Self>;
        }

        // Option<T>
        struct OptionResidual;

        impl<T> Try for Option<T> {
            type Output = T;

            type Residual = OptionResidual;

            #[inline]
            fn from_residual(_residual: Self::Residual) -> Self {
                None
            }

            #[inline]
            fn from_output(output: Self::Output) -> Self {
                Some(output)
            }

            #[inline]
            fn branch(self) -> ControlFlow<Self::Residual, Self::Output> {
                match self {
                    Some(val) => ControlFlow::Continue(val),
                    None => ControlFlow::Break(OptionResidual),
                }
            }
        }

        impl<T> Residual<T> for OptionResidual {
            type TryType = Option<T>;
        }

        // Result<T, E>
        #[repr(transparent)]
        struct ResultResidual<E>(E);

        impl<T, E> Try for Result<T, E> {
            type Output = T;

            type Residual = ResultResidual<E>;

            #[inline]
            fn from_residual(residual: Self::Residual) -> Self {
                Err(residual.0)
            }

            #[inline]
            fn from_output(output: Self::Output) -> Self {
                Ok(output)
            }

            #[inline]
            fn branch(self) -> ControlFlow<Self::Residual, Self::Output> {
                match self {
                    Ok(val) => ControlFlow::Continue(val),
                    Err(e) => ControlFlow::Break(ResultResidual(e)),
                }
            }
        }

        impl<T, E> Residual<T> for ResultResidual<E> {
            type TryType = Result<T, E>;
        }

        // ControlFlow<B, C>
        #[repr(transparent)]
        struct ControlFlowResidual<B>(B);

        impl<B, C> Try for ControlFlow<B, C> {
            type Output = C;

            type Residual = ControlFlowResidual<B>;

            #[inline]
            fn from_residual(residual: Self::Residual) -> Self {
                ControlFlow::Break(residual.0)
            }

            #[inline]
            fn from_output(output: Self::Output) -> Self {
                ControlFlow::Continue(output)
            }

            #[inline]
            fn branch(self) -> ControlFlow<Self::Residual, Self::Output> {
                match self {
                    ControlFlow::Continue(c) => ControlFlow::Continue(c),
                    ControlFlow::Break(b) => ControlFlow::Break(ControlFlowResidual(b)),
                }
            }
        }

        impl<B, C> Residual<C> for ControlFlowResidual<B> {
            type TryType = ControlFlow<B, C>;
        }
    )
}

/// Nothing is imported by default
fn maybe_done_force_take_output(ret_ty: impl ToTokens) -> pm2::TokenStream {
    quote!(
        // SAFETY: the caller must only call it when `self` is `Self::Ready(Some)`
        unsafe fn force_take_output(self: Pin<&mut Self>) -> #ret_ty {
            // SAFETY: pinning projection
            match Pin::get_unchecked_mut(self) {
                Self::Pending(_) => unreachable_unchecked(),
                // SAFETY: the output is NOT structurally pinned
                Self::Ready(o) => o.take().unwrap_unchecked(),
            }
        }
    )
}

fn take_output(
    macro_name: &str,
    maybe_done_vars_at_take_outputs: impl IntoIterator<Item = Ident>,
    mapper: impl ToTokens,
) -> pm2::TokenStream {
    let maybe_done_vars_at_take_outputs = maybe_done_vars_at_take_outputs.into_iter();
    quote!(
        unsafe {
            // We only need to check the first MaybeDone since all the MaybeDone::Ready are either all Some or all None
            match maybe_done0 {
                MaybeDone::Ready(Some(_)) => Poll::Ready(#mapper((
                    // SAFETY: they're at `MaybeDone::Ready(Some)` variant
                    #(MaybeDone::force_take_output(Pin::new_unchecked(#maybe_done_vars_at_take_outputs))),*
                ))),
                // SAFETY: they have been done. It leads to a more efficient codegen
                MaybeDone::Pending(_) => unreachable_unchecked(),
                _ => ::core::panic!("`{}!` future polled after completion", #macro_name)
            }
        }
    )
}

/// Includes: Future, Pin, Context, Poll
fn neccessary_import() -> pm2::TokenStream {
    quote!(
        use ::core::future::Future;
        use ::core::pin::Pin;
        use ::core::task::{Context, Poll};
    )
}

fn future_impl(
    impl_bound: impl ToTokens,
    ty: impl ToTokens,
    output_ty: impl ToTokens,
    where_clause: impl ToTokens,
    poll_impl: impl ToTokens,
) -> pm2::TokenStream {
    quote!(
        impl #impl_bound Future for #ty #where_clause {
            type Output = #output_ty;

            fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<<Self as Future>::Output> {
                #poll_impl
            }
        }
    )
}

fn cyclical_poll<T: ToTokens>(
    fut_count: impl ToTokens,
    do_sth_w_done: impl IntoIterator<Item = T>,
) -> pm2::TokenStream {
    let do_sth_w_done = do_sth_w_done.into_iter();
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
                    #do_sth_w_done

                    if to_run <= 1 { // if we are the last one...
                        break done;
                    }
                    to_run -= 1;
                }
            )*
        }
    )
}

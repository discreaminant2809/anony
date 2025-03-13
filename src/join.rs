use crate::utils::TupleLikeTypeInput;
use crate::{pm2, utils};
use quote::{quote, ToTokens};
use syn::{Expr, Ident};

pub(crate) fn imp(
    tt: crate::pm::TokenStream,
    is_try: bool,
    is_cyclic: bool,
) -> syn::Result<pm2::TokenStream> {
    match syn::parse::<utils::TupleLikeTypeInput>(tt)? {
        TupleLikeTypeInput::Direct { values } if !is_try => Ok(imp_as_direct(&values, is_cyclic)),
        TupleLikeTypeInput::Direct { values } => Ok(imp_try_as_direct(&values, is_cyclic)),
        TupleLikeTypeInput::Typing { arity: _ } => Err(syn::Error::new(
            pm2::Span::call_site(),
            "typing is not supported yet",
        )),
    }
}

fn imp_as_direct(futs: &[Expr], is_cyclic: bool) -> pm2::TokenStream {
    let join_ty = if is_cyclic {
        quote!(JoinCyclic)
    } else {
        quote!(Join)
    };
    let neccessary_import = neccessary_import();
    let must_use = must_use();

    if futs.is_empty() {
        let future_impl = future_impl(
            quote!(),
            &join_ty,
            quote!(()),
            quote!(),
            quote!(Poll::Ready(())),
        );
        return quote!({
            #neccessary_import

            #must_use
            struct #join_ty;

            #future_impl

            #join_ty
        });
    }

    if futs.len() == 1 {
        let fut = &futs[0];
        let future_impl = future_impl(
            quote!(F: Future),
            quote!(#join_ty<F>),
            quote!((F::Output,)),
            quote!(),
            quote!(
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
                .map(|o| (o,))
            ),
        );

        return quote!(match #fut { fut => {
            #neccessary_import

            #must_use
            #[repr(transparent)]
            enum #join_ty<F: Future> {
                // We encapsulate fields using enum variant!
                // To access these fields, we must pattern matching with this variant, which requires us to write `Join::Inner`...
                // wait... we can't even specify the struct's name to begin with!
                // Therefore, we have effectively made these fields "private"!
                Inner(F)
            }

            #future_impl

            use ::core::future::IntoFuture;
            #join_ty::Inner(IntoFuture::into_future(fut))
        }});
    }

    let indices = utils::tuple_indices(futs.len());
    let generics = utils::i_idents('F', futs.len());
    let maybe_done_force_take_output = maybe_done_force_take_output(quote!(F::Output));
    let maybe_done_vars = utils::i_idents("maybe_done", generics.len());
    let [skip_next_time_ty, skip_next_time_var, skip_next_time_init] =
        skip_next_time(is_cyclic, futs.len() as _);
    let take_output = take_output(
        if is_cyclic { "join_cyclic" } else { "join" },
        &maybe_done_vars,
        quote!(),
    );
    let fut_count = pm2::Literal::usize_unsuffixed(futs.len());
    let polling_strategy = if is_cyclic {
        let indices = (0..futs.len()).map(syn::Index::from);
        quote!(
            const COUNT: #skip_next_time_ty = #fut_count;
            let to_skip = ::core::mem::replace(
                skip_next_time,
                // `COUNT` is always > 1 since we've guarded the `0` and `1` cases explicitly (for more efficient logics)
                // so `clippy::modulo_one` won't be triggered
                (*skip_next_time + 1) % COUNT
            );

            use ::core::iter::Iterator;
            // DO NOT use `Iterator::any` or `Iterator::all`. They're short-circuit, and we require polling ALL.
            Iterator::fold(
                Iterator::chain(to_skip..COUNT, 0..to_skip),
                true,
                |done, i| match i {
                    #(#indices => MaybeDone::poll(Pin::new_unchecked(&mut *#maybe_done_vars), cx) && done,)*
                    // SAFETY: we will never reach it since the indices are carefully restricted.
                    _ => unsafe { unreachable_unchecked() },
                },
            )
        )
    } else {
        quote!(
            #(MaybeDone::poll(Pin::new_unchecked(#maybe_done_vars), cx))&*
        )
    };
    let future_impl = future_impl(
        quote!(#(#generics: Future),*),
        quote!(#join_ty<#(#generics),*>),
        quote!((#(#generics::Output,)*)),
        quote!(),
        quote!(
            // SAFETY: pinning projection! All `Maybedone`s are structurally pinned, while `skip_next_time` is NOT
            let Self::Inner(#(#maybe_done_vars),*, #skip_next_time_var) = unsafe { Pin::get_unchecked_mut(self) };

            if !unsafe { #polling_strategy } {
                return Poll::Pending;
            }

            #take_output
        ),
    );

    quote!(match (#(#futs,)*) { futs => {
        #neccessary_import
        use ::core::option::Option::{self, Some, None}; // the `neccessary_import` hasn't imported this

        // Put a "ghost" `#[pin_project]` macro to help know which one is structurally pinned
        // #[pin_project]
        enum MaybeDone<F: Future> {
            Pending(
                // #[pin]
                F
            ),
            // It might seem inefficient... but the compiler can optimize the layout
            Ready(Option<F::Output>),
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
                unsafe {
                    // SAFETY: pinning projection
                    match Pin::get_unchecked_mut(Pin::as_mut(&mut self)) {
                        Self::Pending(fut) => {
                            // SAFETY: pinning projection. `fut` is structurally pinned
                            match Future::poll(Pin::new_unchecked(fut), cx) {
                                Poll::Ready(o) => {
                                    Pin::set(&mut self, Self::Ready(Some(o)));
                                    true
                                }
                                _ => false,
                            }
                        }
                        _ => true,
                    }
                }
            }

            #maybe_done_force_take_output
        }

        #must_use
        enum #join_ty<#(#generics: Future),*> {
            Inner(
                #(
                    MaybeDone<#generics>,
                )*
                #skip_next_time_ty
            )
        }

        #future_impl

        use ::core::future::IntoFuture;
        #join_ty::Inner(
            #(
                MaybeDone::Pending(IntoFuture::into_future(futs.#indices))
            ),*,
            #skip_next_time_init
        )
    }})
}

fn imp_try_as_direct(futs: &[Expr], is_cyclic: bool) -> pm2::TokenStream {
    let join_ty = if is_cyclic {
        quote!(TryJoinCyclic)
    } else {
        quote!(TryJoin)
    };
    let neccessary_import = neccessary_import();
    let try_trait_and_import = try_trait_and_import();
    let must_use = must_use();

    if futs.is_empty() {
        let bound = quote!(T: Try<Output = ()>);
        let where_clause = quote!();

        let future_impl = future_impl(
            &bound,
            quote!(#join_ty<T>),
            quote!(T),
            &where_clause,
            quote!(Poll::Ready(Try::from_output(()))),
        );

        return quote!({
            #neccessary_import
            #try_trait_and_import
            use ::core::marker::PhantomData;

            #must_use
            struct #join_ty<#bound>(PhantomData<T>) where #where_clause;

            #future_impl

            #join_ty(PhantomData)
        });
    }

    if futs.len() == 1 {
        let fut = &futs[0];
        let future_impl = future_impl(
            quote!(F: Future, R: Residual<(<F::Output as Try>::Output,)>),
            quote!(#join_ty<F, R>),
            quote!(R::TryType),
            quote!(F::Output: Try<Residual = R>),
            quote!(
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
            ),
        );

        return quote!(match #fut { fut => {
            #neccessary_import
            #try_trait_and_import

            #must_use
            #[repr(transparent)]
            enum #join_ty<F: Future, R: Residual<(<F::Output as Try>::Output,)>>
            where
                F::Output: Try<Residual = R>
            {
                // We encapsulate fields using enum variant!
                // To access these fields, we must pattern matching with this variant, which requires us to write `Join::Inner`...
                // wait... we can't even specify the struct's name to begin with!
                // Therefore, we have effectively made these fields "private"!
                Inner(F)
            }

            #future_impl

            use ::core::future::IntoFuture;
            #join_ty::Inner(IntoFuture::into_future(fut))
        }});
    }

    let indices = utils::tuple_indices(futs.len());
    let generics = utils::i_idents('F', futs.len());
    let maybe_done_force_take_output =
        maybe_done_force_take_output(quote!(<F::Output as Try>::Output));
    let maybe_done_vars = utils::i_idents("maybe_done", generics.len());
    let [skip_next_time_ty, skip_next_time_var, skip_next_time_init] =
        skip_next_time(is_cyclic, futs.len() as _);
    let take_output = take_output(
        if is_cyclic {
            "try_join_cyclic"
        } else {
            "try_join"
        },
        &maybe_done_vars,
        quote!(Try::from_output),
    );
    let fut_count = pm2::Literal::usize_unsuffixed(futs.len());
    let polling_strategy = if is_cyclic {
        let indices = (0..futs.len()).map(syn::Index::from);
        quote!(
            const COUNT: #skip_next_time_ty = #fut_count;
            let to_skip = ::core::mem::replace(
                skip_next_time,
                // `COUNT` is always > 1 since we've guarded the `0` and `1` cases explicitly (for more efficient logics)
                // so `clippy::modulo_one` won't be triggered
                (*skip_next_time + 1) % COUNT
            );

            use ::core::iter::Iterator;
            // DO NOT use `Iterator::any` or `Iterator::all`. They're short-circuit, and we require polling ALL.
            // However, we do short-circuit on break value, since it signifies the completion with short-citcuit completion.
            // As long as we don't cause UB, `Future::poll` doesn't require us to specify behaviors after yielding `Pending::Ready`,
            // so we don't need to set the wrapped futures to be done on short-circuit for performance.
            // However, this behavior may be changed in the future
            match Iterator::try_fold(
                &mut Iterator::chain(to_skip..COUNT, 0..to_skip),
                true,
                |done, i| match i {
                    #(
                        #indices => ControlFlow::Continue(
                            MaybeDone::poll(Pin::new_unchecked(&mut *#maybe_done_vars), cx)? && done
                        ),
                    )*
                    // SAFETY: we will never reach it since the indices are carefully restricted.
                    _ => unsafe { unreachable_unchecked() },
                }
            ) {
                ControlFlow::Break(r) => return Poll::Ready(Try::from_residual(r)),
                ControlFlow::Continue(done) => done,
            }
        )
    } else {
        quote!(
            let mut done = true;
            #(
                match MaybeDone::poll(Pin::new_unchecked(&mut *#maybe_done_vars), cx) {
                    ControlFlow::Continue(ready) => done &= ready,
                    ControlFlow::Break(r) => return Poll::Ready(Try::from_residual(r)),
                }
            )*
            done
        )
    };
    let future_impl = future_impl(
        quote!(
            #(#generics: Future,)*
            R: Residual<(
                #(<#generics::Output as Try>::Output,)*
            )>,
        ),
        quote!(#join_ty<#(#generics),*, R>),
        quote!(R::TryType),
        quote!(
            #(#generics::Output: Try<Residual = R>),*
        ),
        quote!(
            // SAFETY: pinning projection! All `Maybedone`s are structurally pinned, while `skip_next_time` is NOT
            let Self::Inner(#(#maybe_done_vars),*, #skip_next_time_var) = unsafe { Pin::get_unchecked_mut(self) };

            if !unsafe { #polling_strategy } {
                return Poll::Pending;
            }

            #take_output
        ),
    );

    quote!(match (#(#futs,)*) { futs => {
        #neccessary_import

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
            // Note that this is just an ad-hoc `MaybeDone` that's just to serve the macro, or the future specifically.
            // A correct implementation should store `Option<F::Output>` instead (mark as Ready despite short-circuit).
            // We don't do that here. But what if the future is polled again after short-circuit?
            // When the future is short-circuit, it also counts as "completed the future," and
            // `Future` doesn't require any behaviors (except UBs) if we poll it again after completion.
            // It means that we can just leave the future behaves whatever we want after completion; for this case, it's
            // that the wrapped completed futures may be polled again, and panic with a completely different message than
            // when all are completed. There's no risk of UB, since we always make sure that the code that take all outputs
            // are only reachable when all the `MaybeDone`s are `Ready`.
            // However, this behavior may be changed in the future, so the caller shouldn't rely on it.
            Ready(Option<<F::Output as Try>::Output>),
        }

        // Only the wrapped future is considered, not its output, since the former is structurally pinned, while the latter isn't
        // We strictly adhere to the invariant regarding structurally pinned fields
        // If we don't add this, the future's output type is considered also, which shouldn't
        // since the output is NOT structurally pinned
        use ::core::marker::Unpin;
        impl<F: Future + Unpin> Unpin for MaybeDone<F>
        where
            F::Output: Try,
        {}

        impl<F: Future> MaybeDone<F>
        where
            F::Output: Try,
        {
            // This method is only used to poll and check the status of the wrapped future. It isn't for getting the result!
            fn poll(
                mut self: Pin<&mut Self>,
                cx: &mut Context<'_>,
            ) -> ControlFlow<<F::Output as Try>::Residual, bool> {
                unsafe {
                    // SAFETY: pinning projection
                    match Pin::get_unchecked_mut(Pin::as_mut(&mut self)) {
                        // SAFETY: pinning projection. `fut` is structurally pinned
                        MaybeDone::Pending(fut) => match Future::poll(Pin::new_unchecked(fut), cx) {
                            Poll::Ready(o) => match Try::branch(o) {
                                ControlFlow::Continue(c) => {
                                    Pin::set(&mut self, Self::Ready(Some(c)));
                                    ControlFlow::Continue(true)
                                }
                                // As explained above, we don't need to set the `MaybeDone` to `Ready`
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
        enum #join_ty<
            #(#generics: Future,)*
            R: Residual<(
                #(<#generics::Output as Try>::Output,)*
            )>,
        >
        where
            #(
                #generics::Output: Try<Residual = R>
            ),*
        {
            Inner(
                #(
                    // #[pin]
                    MaybeDone<#generics>,
                )*
                #skip_next_time_ty
            )
        }

        #future_impl

        use ::core::future::IntoFuture;
        #join_ty::Inner(
            #(
                MaybeDone::Pending(IntoFuture::into_future(futs.#indices))
            ),*,
            #skip_next_time_init
        )
    }})
}

fn must_use() -> pm2::TokenStream {
    quote!(
        #[must_use = "unlike other implementations, this one returns a `Future` that should be explicitly `.await`ed or polled"]
    )
}

fn try_trait_and_import() -> pm2::TokenStream {
    quote!(
        use ::core::ops::ControlFlow;
        use ::core::option::Option::{self, None, Some};
        use ::core::result::Result::{self, Err, Ok};

        trait Try {
            type Output;
            type Residual;

            // We don't need `FromResidual` since it is just for implicit error conversion
            // which is not relevant as we can only require all errors to be the same
            // due to "unconstrained type parameters"
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
            #[track_caller]
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

fn maybe_done_force_take_output(ret_ty: impl ToTokens) -> pm2::TokenStream {
    quote!(
        // SAFETY: the caller must only call it when `self` is `Self::Ready`
        unsafe fn force_take_output(self: Pin<&mut Self>) -> Option<#ret_ty> {
            // SAFETY: pinning projection
            match Pin::get_unchecked_mut(self) {
                // SAFETY: by the time it's called, the future must've been done.
                Self::Pending(_) => unreachable_unchecked(),
                // SAFETY: the output is NOT structurally pinned
                Self::Ready(o) => o.take(),
            }
        }
    )
}

fn take_output(
    macro_name: &str,
    maybe_done_vars: &[Ident],
    mapper: impl ToTokens,
) -> pm2::TokenStream {
    let outputs = utils::i_idents("o", maybe_done_vars.len());
    let nones = std::iter::repeat_with(|| quote!(None)).take(maybe_done_vars.len());

    quote!(
        unsafe {
            // We only need to check the first MaybeDone since all the MaybeDone::Ready are either all Some or all None
            match (
                #(
                    MaybeDone::force_take_output(Pin::new_unchecked(#maybe_done_vars)),
                )*
            ) {
                (#(Some(#outputs),)*) => Poll::Ready(#mapper((#(#outputs,)*))),
                (#(#nones,)*) => ::core::panic!("`{}!` future polled after completion", #macro_name),
                // SAFETY: they have either not been done, or all done (no mixed states). It leads to a more efficient codegen
                _ => unreachable_unchecked(),
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
        // All primitive types in Rust are actually NOT keywords!
        // They can be polluted by something like `struct usize;`, which is actually permitted but causes errors
        // See more:
        use ::core::primitive::bool;
        use ::core::hint::unreachable_unchecked;
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
        impl<#impl_bound> Future for #ty where #where_clause {
            type Output = #output_ty;

            fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
                #poll_impl
            }
        }
    )
}

fn skip_next_time(is_cyclic: bool, n: u128) -> [pm2::TokenStream; 3] {
    if !is_cyclic {
        return Default::default();
    }

    let skip_next_time_ty = if n <= u8::MAX as _ {
        quote!(::core::primitive::u8)
    } else if n <= u16::MAX as _ {
        quote!(::core::primitive::u16)
    } else if n <= u32::MAX as _ {
        quote!(::core::primitive::u32)
    } else if n <= u64::MAX as _ {
        quote!(::core::primitive::u64)
    } else {
        quote!(::core::primitive::u128)
    };

    [skip_next_time_ty, quote!(skip_next_time), quote!(0)]
}

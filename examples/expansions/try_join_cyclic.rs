// This function exists since an empty `try_join(_cyclic)` may cause an error.
fn expect_output(_f: impl std::future::Future<Output = Option<()>>) {}

// Separate it so that you can focus on the main logic, since all `try_join(_cyclic)` expansions here have this
// The actual expansion will inline it instead
// We inline the whole `Try` and `Residual` implementation in the actual expansion
// Prior knowledge of the Try trait is recommended to understand
macro_rules! try_trait {
    () => {
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

        #[repr(transparent)]
        struct ResultResidual<E>(E);

        impl<T, E> Try for Result<T, E> {
            type Output = T;
            type Residual = ResultResidual<E>;

            #[inline]
            // See https://doc.rust-lang.org/src/core/result.rs.html#1954-1962 to see why we use `#[track_caller]` here
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
    };
}

fn expansion() {
    // No future at all
    let fut_0 = anony::try_join_cyclic!();
    expect_output(fut_0);
    let fut_0 = {
        use ::core::future::Future;
        use ::core::marker::PhantomData;
        use ::core::ops::ControlFlow;
        use ::core::option::Option::{self, None, Some};
        use ::core::pin::Pin;
        use ::core::result::Result::{self, Err, Ok};
        use ::core::task::{Context, Poll};

        // See the macro declaration above
        try_trait!();

        #[must_use = "unlike other implementations, this one returns a `Future` that should be explicitly `.await`ed or polled"]
        #[repr(transparent)]
        enum TryJoinCyclic<T: Try<Output = ()>> {
            Inner(PhantomData<T>),
        }

        impl<T: Try<Output = ()>> Future for TryJoinCyclic<T> {
            type Output = T;
            #[inline]
            fn poll(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<<Self as Future>::Output> {
                Poll::Ready(Try::from_output(()))
            }
        }

        TryJoinCyclic::Inner(PhantomData)
    };
    expect_output(fut_0);

    // Just 1 future
    let _fut_1 = anony::try_join_cyclic!(async { Some(()) });
    let _fut_1 = match async { Some(()) } {
        fut => {
            use ::core::future::Future;
            use ::core::ops::ControlFlow;
            use ::core::option::Option::{self, None, Some};
            use ::core::pin::Pin;
            use ::core::result::Result::{self, Err, Ok};
            use ::core::task::{Context, Poll};

            try_trait!();

            #[must_use = "unlike other implementations, this one returns a `Future` that should be explicitly `.await`ed or polled"]
            #[repr(transparent)]
            enum TryJoinCyclic<F: Future, R: Residual<(<F::Output as Try>::Output,)>>
            where
                F::Output: Try<Residual = R>,
            {
                Inner(F),
            }

            impl<F: Future, R: Residual<(<F::Output as Try>::Output,)>> Future for TryJoinCyclic<F, R>
            where
                F::Output: Try<Residual = R>,
            {
                type Output = R::TryType;
                fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
                    Future::poll(
                        unsafe {
                            Pin::map_unchecked_mut(self, |this| {
                                let Self::Inner(fut) = this;
                                fut
                            })
                        },
                        cx,
                    )
                    .map(|o| match Try::branch(o) {
                        ControlFlow::Continue(c) => Try::from_output((c,)),
                        ControlFlow::Break(b) => Try::from_residual(b),
                    })
                }
            }

            use ::core::future::IntoFuture;
            TryJoinCyclic::Inner(IntoFuture::into_future(fut))
        }
    };

    let _fut_3 =
        anony::try_join_cyclic!(async { Some(()) }, async { Some(()) }, async { Some(()) });
    let _fut_3 = match (async { Some(()) }, async { Some(()) }, async { Some(()) }) {
        futs => {
            use ::core::future::Future;
            use ::core::hint::unreachable_unchecked;
            use ::core::ops::ControlFlow;
            use ::core::option::Option::{self, None, Some};
            use ::core::pin::Pin;
            use ::core::primitive::bool;
            use ::core::result::Result::{self, Err, Ok};
            use ::core::task::{Context, Poll};

            try_trait!();

            enum MaybeDone<F: Future>
            where
                F::Output: Try,
            {
                Pending(F),
                Ready(Option<<F::Output as Try>::Output>),
            }

            use ::core::marker::Unpin;
            impl<F: Future + Unpin> Unpin for MaybeDone<F> where F::Output: Try {}

            impl<F: Future> MaybeDone<F>
            where
                F::Output: Try,
            {
                fn poll(
                    mut self: Pin<&mut Self>,
                    cx: &mut Context<'_>,
                ) -> ControlFlow<<F::Output as Try>::Residual, bool> {
                    unsafe {
                        match Pin::get_unchecked_mut(Pin::as_mut(&mut self)) {
                            MaybeDone::Pending(fut) => {
                                match Future::poll(Pin::new_unchecked(fut), cx) {
                                    Poll::Ready(o) => match Try::branch(o) {
                                        ControlFlow::Continue(c) => {
                                            Pin::set(&mut self, Self::Ready(Some(c)));
                                            ControlFlow::Continue(true)
                                        }
                                        ControlFlow::Break(b) => ControlFlow::Break(b),
                                    },
                                    Poll::Pending => ControlFlow::Continue(false),
                                }
                            }
                            MaybeDone::Ready(_) => ControlFlow::Continue(true),
                        }
                    }
                }

                unsafe fn force_take_output(
                    self: Pin<&mut Self>,
                ) -> Option<<F::Output as Try>::Output> {
                    match Pin::get_unchecked_mut(self) {
                        Self::Pending(_) => unreachable_unchecked(),
                        Self::Ready(o) => o.take(),
                    }
                }
            }

            #[must_use = "unlike other implementations, this one returns a `Future` that should be explicitly `.await`ed or polled"]
            enum TryJoinCyclic<
                F0: Future,
                F1: Future,
                F2: Future,
                R: Residual<(
                    <F0::Output as Try>::Output,
                    <F1::Output as Try>::Output,
                    <F2::Output as Try>::Output,
                )>,
            >
            where
                F0::Output: Try<Residual = R>,
                F1::Output: Try<Residual = R>,
                F2::Output: Try<Residual = R>,
            {
                Inner(
                    MaybeDone<F0>,
                    MaybeDone<F1>,
                    MaybeDone<F2>,
                    ::core::primitive::u8,
                ),
            }

            impl<
                    F0: Future,
                    F1: Future,
                    F2: Future,
                    R: Residual<(
                        <F0::Output as Try>::Output,
                        <F1::Output as Try>::Output,
                        <F2::Output as Try>::Output,
                    )>,
                > Future for TryJoinCyclic<F0, F1, F2, R>
            where
                F0::Output: Try<Residual = R>,
                F1::Output: Try<Residual = R>,
                F2::Output: Try<Residual = R>,
            {
                type Output = R::TryType;
                fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
                    let Self::Inner(maybe_done0, maybe_done1, maybe_done2, skip_next_time) =
                        unsafe { Pin::get_unchecked_mut(self) };

                    if !unsafe {
                        const COUNT: ::core::primitive::u8 = 3;
                        let to_skip =
                            ::core::mem::replace(skip_next_time, (*skip_next_time + 1) % COUNT);

                        use ::core::iter::Iterator;
                        match Iterator::try_fold(
                            &mut Iterator::chain(to_skip..COUNT, 0..to_skip),
                            true,
                            |done, i| match i {
                                0 => ControlFlow::Continue(
                                    MaybeDone::poll(Pin::new_unchecked(&mut *maybe_done0), cx)?
                                        && done,
                                ),
                                1 => ControlFlow::Continue(
                                    MaybeDone::poll(Pin::new_unchecked(&mut *maybe_done1), cx)?
                                        && done,
                                ),
                                2 => ControlFlow::Continue(
                                    MaybeDone::poll(Pin::new_unchecked(&mut *maybe_done2), cx)?
                                        && done,
                                ),
                                _ => unsafe { unreachable_unchecked() },
                            },
                        ) {
                            ControlFlow::Break(r) => return Poll::Ready(Try::from_residual(r)),
                            ControlFlow::Continue(done) => done,
                        }
                    } {
                        return Poll::Pending;
                    }

                    unsafe {
                        match (
                            MaybeDone::force_take_output(Pin::new_unchecked(maybe_done0)),
                            MaybeDone::force_take_output(Pin::new_unchecked(maybe_done1)),
                            MaybeDone::force_take_output(Pin::new_unchecked(maybe_done2)),
                        ) {
                            (Some(o0), Some(o1), Some(o2)) => {
                                Poll::Ready(Try::from_output((o0, o1, o2)))
                            }
                            (None, None, None) => ::core::panic!(
                                "`{}!` future polled after completion",
                                "try_join_cyclic"
                            ),
                            _ => unreachable_unchecked(),
                        }
                    }
                }
            }

            use ::core::future::IntoFuture;
            TryJoinCyclic::Inner(
                MaybeDone::Pending(IntoFuture::into_future(futs.0)),
                MaybeDone::Pending(IntoFuture::into_future(futs.1)),
                MaybeDone::Pending(IntoFuture::into_future(futs.2)),
                0,
            )
        }
    };
}

#[allow(clippy::double_parens)]
fn expansion() {
    // Before we move on we should know that our `join!` has two efficient expansions for no future and 1 future

    // No future at all
    let _fut0 = anony::join!();
    // In this case the struct is zero size!
    // Note that we add `#[must_use]` to warn that it is neither futures nor tokio - our version returns a future instead
    let _fut0 = {
        use ::core::future::Future;
        use ::core::pin::Pin;
        use ::core::task::{Context, Poll};

        #[must_use = "unlike other implementations, this one returns a `Future` that should be explicitly `.await`ed or polled"]
        struct Join;

        impl Future for Join {
            type Output = ();
            fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
                Poll::Ready(())
            }
        }
        Join
    };

    // Just one future
    let _fut1 = anony::join!(async { 13 });
    // In this case no `MaybeDone`s are generated
    let _fut1 = match async { 13 } {
        fut => {
            use ::core::future::Future;
            use ::core::pin::Pin;
            use ::core::task::{Context, Poll};

            #[must_use = "unlike other implementations, this one returns a `Future` that should be explicitly `.await`ed or polled"]
            #[repr(transparent)]
            enum Join<F: Future> {
                Inner(F),
            }

            impl<F: Future> Future for Join<F> {
                type Output = (F::Output,);
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
                    .map(|o| (o,))
                }
            }

            use ::core::future::IntoFuture;
            Join::Inner(IntoFuture::into_future(fut))
        }
    };

    // Base case
    let _fut_n = anony::join!(async { 134 }, async { "144" }, std::future::pending::<()>());
    let _fut_n = match (async { 134 }, async { "144" }, std::future::pending::<()>()) {
        futs => {
            use ::core::future::Future;
            use ::core::option::Option::{self, None, Some};
            use ::core::pin::Pin;
            use ::core::primitive::bool;
            use ::core::task::{Context, Poll};

            #[inline(always)]
            unsafe fn unreachable_unchecked() -> ! {
                ::core::unreachable!("`unreachable_unchecked` reached at runtime")
            }

            enum MaybeDone<F: Future> {
                Pending(F),
                Ready(Option<F::Output>),
            }

            use ::core::marker::Unpin;
            impl<F: Future + Unpin> Unpin for MaybeDone<F> {}

            impl<F: Future> MaybeDone<F> {
                fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> bool {
                    unsafe {
                        match Pin::get_unchecked_mut(Pin::as_mut(&mut self)) {
                            Self::Pending(fut) => match Future::poll(Pin::new_unchecked(fut), cx) {
                                Poll::Ready(o) => {
                                    Pin::set(&mut self, Self::Ready(Some(o)));
                                    true
                                }
                                _ => false,
                            },
                            _ => true,
                        }
                    }
                }

                unsafe fn force_take_output(self: Pin<&mut Self>) -> Option<F::Output> {
                    match Pin::get_unchecked_mut(self) {
                        Self::Pending(_) => unreachable_unchecked(),
                        Self::Ready(o) => o.take(),
                    }
                }
            }

            #[must_use = "unlike other implementations, this one returns a `Future` that should be explicitly `.await`ed or polled"]
            enum Join<F0: Future, F1: Future, F2: Future> {
                Inner(MaybeDone<F0>, MaybeDone<F1>, MaybeDone<F2>),
            }

            impl<F0: Future, F1: Future, F2: Future> Future for Join<F0, F1, F2> {
                type Output = (F0::Output, F1::Output, F2::Output);
                fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
                    let Self::Inner(maybe_done0, maybe_done1, maybe_done2) =
                        unsafe { Pin::get_unchecked_mut(self) };

                    if !unsafe {
                        MaybeDone::poll(Pin::new_unchecked(maybe_done0), cx)
                            & MaybeDone::poll(Pin::new_unchecked(maybe_done1), cx)
                            & MaybeDone::poll(Pin::new_unchecked(maybe_done2), cx)
                    } {
                        return Poll::Pending;
                    }

                    unsafe {
                        match (
                            MaybeDone::force_take_output(Pin::new_unchecked(maybe_done0)),
                            MaybeDone::force_take_output(Pin::new_unchecked(maybe_done1)),
                            MaybeDone::force_take_output(Pin::new_unchecked(maybe_done2)),
                        ) {
                            (Some(o0), Some(o1), Some(o2)) => Poll::Ready(((o0, o1, o2))),
                            (None, None, None) => {
                                ::core::panic!("`{}!` future polled after completion", "join")
                            }
                            _ => unreachable_unchecked(),
                        }
                    }
                }
            }

            use ::core::future::IntoFuture;
            Join::Inner(
                MaybeDone::Pending(IntoFuture::into_future(futs.0)),
                MaybeDone::Pending(IntoFuture::into_future(futs.1)),
                MaybeDone::Pending(IntoFuture::into_future(futs.2)),
            )
        }
    };
}

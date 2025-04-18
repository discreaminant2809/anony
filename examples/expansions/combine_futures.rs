use anony::combine_futures;

fn predicate() -> bool {
    true
}

fn zero_branch_case() {
    let _fut = combine_futures! {};
    let _fut = ({
        use ::core::future::{Future, IntoFuture};
        use ::core::marker::Unpin;
        use ::core::num::Wrapping;
        use ::core::ops::{ControlFlow, FnMut};
        use ::core::option::Option;
        use ::core::pin::Pin;
        use ::core::task::{Context, Poll};
        enum CombineFutures<S: FnMut(&mut Context<'_>) -> Poll<O>, O> {
            Inner(S),
        }
        impl<S: FnMut(&mut Context<'_>) -> Poll<O>, O> CombineFutures<S, O> {
            fn new(selector: S) -> Self {
                Self::Inner(selector)
            }
        }
        impl<S: FnMut(&mut Context<'_>) -> Poll<O>, O> Unpin for CombineFutures<S, O> {}

        impl<S: FnMut(&mut Context<'_>) -> Poll<O>, O> Future for CombineFutures<S, O> {
            type Output = O;
            fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
                let Self::Inner(selector) = unsafe { Pin::get_unchecked_mut(self) };
                selector(cx)
            }
        }
        CombineFutures::new
    })(|__cx| {
        let mut __done = true;
        if __done {
            let () = () else {
                unsafe { ::core::hint::unreachable_unchecked() }
            };
            match () {
                () => {
                    return ::core::task::Poll::Ready((|| ())());
                }
                () => ::core::panic!("`{}!` future polled after completion`", "combine_futures"),
                _ => unsafe { ::core::hint::unreachable_unchecked() },
            }
        }
        ::core::task::Poll::Pending
    });
}

fn one_branch_case() {
    let _fut = combine_futures! {
        break async { 0 },
    };
    let _fut = ({
        use ::core::future::{Future, IntoFuture};
        use ::core::marker::Unpin;
        use ::core::num::Wrapping;
        use ::core::ops::{ControlFlow, FnMut};
        use ::core::option::Option;
        use ::core::pin::Pin;
        use ::core::task::{Context, Poll};
        enum CombineFutures<F0: Future, S: FnMut(&mut F0, &mut Context<'_>) -> Poll<O>, O> {
            Inner(F0, S),
        }
        impl<F0: Future, S: FnMut(&mut F0, &mut Context<'_>) -> Poll<O>, O> CombineFutures<F0, S, O> {
            fn new(__fut0: impl IntoFuture<IntoFuture = F0>, selector: S) -> Self {
                Self::Inner(IntoFuture::into_future(__fut0), selector)
            }
        }
        impl<F0: Future + Unpin, S: FnMut(&mut F0, &mut Context<'_>) -> Poll<O>, O> Unpin
            for CombineFutures<F0, S, O>
        {
        }

        impl<F0: Future, S: FnMut(&mut F0, &mut Context<'_>) -> Poll<O>, O> Future
            for CombineFutures<F0, S, O>
        {
            type Output = O;
            fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
                let Self::Inner(__fut0, selector) = unsafe { Pin::get_unchecked_mut(self) };
                selector(__fut0, cx)
            }
        }
        CombineFutures::new
    })(async { 0 }, |__fut0, __cx| {
        let mut __done = true;
        'poll_scope: {
            if let ::core::task::Poll::Ready(__o) = ::core::future::Future::poll(
                unsafe { ::core::pin::Pin::new_unchecked(__fut0) },
                __cx,
            ) {
                return ::core::task::Poll::Ready((|| __o)());
            } else {
                __done = false;
            }
        }
        ::core::task::Poll::Pending
    });
}

fn general_case_pure_break() {
    let fut_expr0 = async { 3 };
    let condition = false;
    let _fut = combine_futures! {
        break async {2},

        let 2 = async {2} else => continue => break 2,

        let x = fut_expr0 => continue x,

        if let x @ -1 = async {-1} => break {
            x
        } else if condition => continue {
            println!("Hello");
            if predicate() {
                return 2;
            }

            1
        } else if let true = condition => break {
            0
        } else => break {
            0
        }

        if async { true } => break {
            0
        } else => break {
            dbg!(0)
        }

        match async{ Some(2) } {
            Some(x) if x == 0 => break x,
            Some(_x) => continue,
            None => break 0,
        }
    };

    let fut_expr0 = async { 3 };
    let condition = false;
    let _fut = ({
        use ::core::future::{Future, IntoFuture};
        use ::core::marker::Unpin;
        use ::core::num::Wrapping;
        use ::core::ops::{ControlFlow, FnMut};
        use ::core::option::Option;
        use ::core::pin::Pin;
        use ::core::task::{Context, Poll};
        enum CombineFutures<
            F0: Future,
            F1: Future,
            F2: Future,
            F3: Future,
            F4: Future,
            F5: Future,
            S: FnMut(
                &mut F0,
                &mut ControlFlow<(), F1>,
                &mut ControlFlow<(), F2>,
                &mut ControlFlow<(), F3>,
                &mut F4,
                &mut ControlFlow<(), F5>,
                &mut Context<'_>,
            ) -> Poll<O>,
            O,
        > {
            Inner(
                F0,
                ControlFlow<(), F1>,
                ControlFlow<(), F2>,
                ControlFlow<(), F3>,
                F4,
                ControlFlow<(), F5>,
                S,
            ),
        }
        impl<
            F0: Future,
            F1: Future,
            F2: Future,
            F3: Future,
            F4: Future,
            F5: Future,
            S: FnMut(
                &mut F0,
                &mut ControlFlow<(), F1>,
                &mut ControlFlow<(), F2>,
                &mut ControlFlow<(), F3>,
                &mut F4,
                &mut ControlFlow<(), F5>,
                &mut Context<'_>,
            ) -> Poll<O>,
            O,
        > CombineFutures<F0, F1, F2, F3, F4, F5, S, O>
        {
            fn new(
                __fut0: impl IntoFuture<IntoFuture = F0>,
                __fut1: impl IntoFuture<IntoFuture = F1>,
                __fut2: impl IntoFuture<IntoFuture = F2>,
                __fut3: impl IntoFuture<IntoFuture = F3>,
                __fut4: impl IntoFuture<IntoFuture = F4>,
                __fut5: impl IntoFuture<IntoFuture = F5>,
                selector: S,
            ) -> Self {
                Self::Inner(
                    IntoFuture::into_future(__fut0),
                    ControlFlow::Continue(IntoFuture::into_future(__fut1)),
                    ControlFlow::Continue(IntoFuture::into_future(__fut2)),
                    ControlFlow::Continue(IntoFuture::into_future(__fut3)),
                    IntoFuture::into_future(__fut4),
                    ControlFlow::Continue(IntoFuture::into_future(__fut5)),
                    selector,
                )
            }
        }
        impl<
            F0: Future + Unpin,
            F1: Future + Unpin,
            F2: Future + Unpin,
            F3: Future + Unpin,
            F4: Future + Unpin,
            F5: Future + Unpin,
            S: FnMut(
                &mut F0,
                &mut ControlFlow<(), F1>,
                &mut ControlFlow<(), F2>,
                &mut ControlFlow<(), F3>,
                &mut F4,
                &mut ControlFlow<(), F5>,
                &mut Context<'_>,
            ) -> Poll<O>,
            O,
        > Unpin for CombineFutures<F0, F1, F2, F3, F4, F5, S, O>
        {
        }

        impl<
            F0: Future,
            F1: Future,
            F2: Future,
            F3: Future,
            F4: Future,
            F5: Future,
            S: FnMut(
                &mut F0,
                &mut ControlFlow<(), F1>,
                &mut ControlFlow<(), F2>,
                &mut ControlFlow<(), F3>,
                &mut F4,
                &mut ControlFlow<(), F5>,
                &mut Context<'_>,
            ) -> Poll<O>,
            O,
        > Future for CombineFutures<F0, F1, F2, F3, F4, F5, S, O>
        {
            type Output = O;
            fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
                let Self::Inner(__fut0, __fut1, __fut2, __fut3, __fut4, __fut5, selector) =
                    unsafe { Pin::get_unchecked_mut(self) };
                selector(__fut0, __fut1, __fut2, __fut3, __fut4, __fut5, cx)
            }
        }
        CombineFutures::new
    })(
        async { 2 },
        async { 2 },
        fut_expr0,
        async { -1 },
        async { true },
        async { Some(2) },
        |__fut0, __fut1, __fut2, __fut3, __fut4, __fut5, __cx| {
            let mut __done = true;
            'poll_scope: {
                if let ::core::task::Poll::Ready(__o) = ::core::future::Future::poll(
                    unsafe { ::core::pin::Pin::new_unchecked(__fut0) },
                    __cx,
                ) {
                    return ::core::task::Poll::Ready((|| __o)());
                } else {
                    __done = false;
                }
            }
            'poll_scope: {
                if let ::core::ops::ControlFlow::Continue(__fut_inner) = __fut1 {
                    if let ::core::task::Poll::Ready(__o) = ::core::future::Future::poll(
                        unsafe { ::core::pin::Pin::new_unchecked(__fut_inner) },
                        __cx,
                    ) {
                        let 2 = __o else {
                            ::core::mem::drop((|| {})());
                            *__fut1 = ::core::ops::ControlFlow::Break(());
                            break 'poll_scope;
                        };
                        return ::core::task::Poll::Ready((|| 2)());
                    } else {
                        __done = false;
                    }
                }
            }
            'poll_scope: {
                if let ::core::ops::ControlFlow::Continue(__fut_inner) = __fut2 {
                    if let ::core::task::Poll::Ready(__o) = ::core::future::Future::poll(
                        unsafe { ::core::pin::Pin::new_unchecked(__fut_inner) },
                        __cx,
                    ) {
                        let x = __o;
                        ::core::mem::drop((|| x)());
                        *__fut2 = ::core::ops::ControlFlow::Break(());
                        break 'poll_scope;
                    } else {
                        __done = false;
                    }
                }
            }
            'poll_scope: {
                if let ::core::ops::ControlFlow::Continue(__fut_inner) = __fut3 {
                    if let ::core::task::Poll::Ready(__o) = ::core::future::Future::poll(
                        unsafe { ::core::pin::Pin::new_unchecked(__fut_inner) },
                        __cx,
                    ) {
                        if let x @ -1 = __o {
                            return ::core::task::Poll::Ready((|| x)());
                        } else if (|| condition)() {
                            ::core::mem::drop((|| {
                                println!("Hello");
                                if predicate() {
                                    return 2;
                                }
                                1
                            })());
                            *__fut3 = ::core::ops::ControlFlow::Break(());
                            break 'poll_scope;
                        } else if let true = (|| condition)() {
                            return ::core::task::Poll::Ready((|| 0)());
                        } else {
                            return ::core::task::Poll::Ready((|| 0)());
                        }
                    } else {
                        __done = false;
                    }
                }
            }
            'poll_scope: {
                if let ::core::task::Poll::Ready(__o) = ::core::future::Future::poll(
                    unsafe { ::core::pin::Pin::new_unchecked(__fut4) },
                    __cx,
                ) {
                    if __o {
                        return ::core::task::Poll::Ready((|| 0)());
                    } else {
                        return ::core::task::Poll::Ready((|| dbg!(0))());
                    }
                } else {
                    __done = false;
                }
            }
            'poll_scope: {
                if let ::core::ops::ControlFlow::Continue(__fut_inner) = __fut5 {
                    if let ::core::task::Poll::Ready(__o) = ::core::future::Future::poll(
                        unsafe { ::core::pin::Pin::new_unchecked(__fut_inner) },
                        __cx,
                    ) {
                        match __o {
                            Some(x) if (|| x == 0)() => {
                                return ::core::task::Poll::Ready((|| x)());
                            }
                            Some(_x) => {
                                ::core::mem::drop((|| {})());
                                *__fut5 = ::core::ops::ControlFlow::Break(());
                                break 'poll_scope;
                            }
                            None => {
                                return ::core::task::Poll::Ready((|| 0)());
                            }
                        }
                    } else {
                        __done = false;
                    }
                }
            }
            ::core::task::Poll::Pending
        },
    );
}

fn general_case_no_pure_break() {
    let fut_expr0 = async { 3 };
    let condition = false;
    let coefficient = -8;
    let fut = combine_futures! {
        move

        continue async {2},

        let 2 = async {2} else => continue => break 2,

        let x = fut_expr0 => continue x,

        if let x @ -1 = async {-1} => break {
            x
        } else if condition => continue {
            println!("Hello");
            if predicate() {
                return 2;
            }

            1
        } else if let None = condition.then_some("abc") => break {
            0
        } else => break {
            0
        }

        if async { true } => continue {
            "abc"
        } else => break {
            dbg!(0)
        }

        match async{ Some(2) } {
            Some(x) if x == 0 => break x,
            Some(x) => continue { x }
            None => break 0,
        }

        |a, _, c: i32, d, _: &str, e| (a + c + d + e) * coefficient,
    };

    let fut_expr0 = async { 3 };
    let condition = false;
    let coefficient = -8;
    let fut = ({
        use ::core::future::{Future, IntoFuture};
        use ::core::marker::Unpin;
        use ::core::num::Wrapping;
        use ::core::ops::{ControlFlow, FnMut};
        use ::core::option::Option;
        use ::core::pin::Pin;
        use ::core::task::{Context, Poll};
        enum CombineFutures<
            F0: Future,
            F1: Future,
            F2: Future,
            F3: Future,
            F4: Future,
            F5: Future,
            C0,
            C1,
            C2,
            C3,
            C4,
            C5,
            S: FnMut(
                &mut ControlFlow<Option<C0>, F0>,
                &mut ControlFlow<Option<C1>, F1>,
                &mut ControlFlow<Option<C2>, F2>,
                &mut ControlFlow<Option<C3>, F3>,
                &mut ControlFlow<Option<C4>, F4>,
                &mut ControlFlow<Option<C5>, F5>,
                &mut Context<'_>,
            ) -> Poll<O>,
            O,
        > {
            Inner(
                ControlFlow<Option<C0>, F0>,
                ControlFlow<Option<C1>, F1>,
                ControlFlow<Option<C2>, F2>,
                ControlFlow<Option<C3>, F3>,
                ControlFlow<Option<C4>, F4>,
                ControlFlow<Option<C5>, F5>,
                S,
            ),
        }
        impl<
            F0: Future,
            F1: Future,
            F2: Future,
            F3: Future,
            F4: Future,
            F5: Future,
            C0,
            C1,
            C2,
            C3,
            C4,
            C5,
            S: FnMut(
                &mut ControlFlow<Option<C0>, F0>,
                &mut ControlFlow<Option<C1>, F1>,
                &mut ControlFlow<Option<C2>, F2>,
                &mut ControlFlow<Option<C3>, F3>,
                &mut ControlFlow<Option<C4>, F4>,
                &mut ControlFlow<Option<C5>, F5>,
                &mut Context<'_>,
            ) -> Poll<O>,
            O,
        > CombineFutures<F0, F1, F2, F3, F4, F5, C0, C1, C2, C3, C4, C5, S, O>
        {
            fn new(
                __fut0: impl IntoFuture<IntoFuture = F0>,
                __fut1: impl IntoFuture<IntoFuture = F1>,
                __fut2: impl IntoFuture<IntoFuture = F2>,
                __fut3: impl IntoFuture<IntoFuture = F3>,
                __fut4: impl IntoFuture<IntoFuture = F4>,
                __fut5: impl IntoFuture<IntoFuture = F5>,
                selector: S,
            ) -> Self {
                Self::Inner(
                    ControlFlow::Continue(IntoFuture::into_future(__fut0)),
                    ControlFlow::Continue(IntoFuture::into_future(__fut1)),
                    ControlFlow::Continue(IntoFuture::into_future(__fut2)),
                    ControlFlow::Continue(IntoFuture::into_future(__fut3)),
                    ControlFlow::Continue(IntoFuture::into_future(__fut4)),
                    ControlFlow::Continue(IntoFuture::into_future(__fut5)),
                    selector,
                )
            }
        }
        impl<
            F0: Future + Unpin,
            F1: Future + Unpin,
            F2: Future + Unpin,
            F3: Future + Unpin,
            F4: Future + Unpin,
            F5: Future + Unpin,
            C0,
            C1,
            C2,
            C3,
            C4,
            C5,
            S: FnMut(
                &mut ControlFlow<Option<C0>, F0>,
                &mut ControlFlow<Option<C1>, F1>,
                &mut ControlFlow<Option<C2>, F2>,
                &mut ControlFlow<Option<C3>, F3>,
                &mut ControlFlow<Option<C4>, F4>,
                &mut ControlFlow<Option<C5>, F5>,
                &mut Context<'_>,
            ) -> Poll<O>,
            O,
        > Unpin for CombineFutures<F0, F1, F2, F3, F4, F5, C0, C1, C2, C3, C4, C5, S, O>
        {
        }

        impl<
            F0: Future,
            F1: Future,
            F2: Future,
            F3: Future,
            F4: Future,
            F5: Future,
            C0,
            C1,
            C2,
            C3,
            C4,
            C5,
            S: FnMut(
                &mut ControlFlow<Option<C0>, F0>,
                &mut ControlFlow<Option<C1>, F1>,
                &mut ControlFlow<Option<C2>, F2>,
                &mut ControlFlow<Option<C3>, F3>,
                &mut ControlFlow<Option<C4>, F4>,
                &mut ControlFlow<Option<C5>, F5>,
                &mut Context<'_>,
            ) -> Poll<O>,
            O,
        > Future for CombineFutures<F0, F1, F2, F3, F4, F5, C0, C1, C2, C3, C4, C5, S, O>
        {
            type Output = O;
            fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
                let Self::Inner(__fut0, __fut1, __fut2, __fut3, __fut4, __fut5, selector) =
                    unsafe { Pin::get_unchecked_mut(self) };
                selector(__fut0, __fut1, __fut2, __fut3, __fut4, __fut5, cx)
            }
        }
        CombineFutures::new
    })(
        async { 2 },
        async { 2 },
        fut_expr0,
        async { -1 },
        async { true },
        async { Some(2) },
        move |__fut0, __fut1, __fut2, __fut3, __fut4, __fut5, __cx| {
            let mut __done = true;
            'poll_scope: {
                if let ::core::ops::ControlFlow::Continue(__fut_inner) = __fut0 {
                    if let ::core::task::Poll::Ready(__o) = ::core::future::Future::poll(
                        unsafe { ::core::pin::Pin::new_unchecked(__fut_inner) },
                        __cx,
                    ) {
                        *__fut0 = ::core::ops::ControlFlow::Break(::core::option::Option::Some(
                            (|| __o)(),
                        ));
                        break 'poll_scope;
                    } else {
                        __done = false;
                    }
                }
            }
            'poll_scope: {
                if let ::core::ops::ControlFlow::Continue(__fut_inner) = __fut1 {
                    if let ::core::task::Poll::Ready(__o) = ::core::future::Future::poll(
                        unsafe { ::core::pin::Pin::new_unchecked(__fut_inner) },
                        __cx,
                    ) {
                        let 2 = __o else {
                            *__fut1 = ::core::ops::ControlFlow::Break(
                                ::core::option::Option::Some((|| {})()),
                            );
                            break 'poll_scope;
                        };
                        return ::core::task::Poll::Ready((|| 2)());
                    } else {
                        __done = false;
                    }
                }
            }
            'poll_scope: {
                if let ::core::ops::ControlFlow::Continue(__fut_inner) = __fut2 {
                    if let ::core::task::Poll::Ready(__o) = ::core::future::Future::poll(
                        unsafe { ::core::pin::Pin::new_unchecked(__fut_inner) },
                        __cx,
                    ) {
                        let x = __o;
                        *__fut2 =
                            ::core::ops::ControlFlow::Break(::core::option::Option::Some((|| x)()));
                        break 'poll_scope;
                    } else {
                        __done = false;
                    }
                }
            }
            'poll_scope: {
                if let ::core::ops::ControlFlow::Continue(__fut_inner) = __fut3 {
                    if let ::core::task::Poll::Ready(__o) = ::core::future::Future::poll(
                        unsafe { ::core::pin::Pin::new_unchecked(__fut_inner) },
                        __cx,
                    ) {
                        if let x @ -1 = __o {
                            return ::core::task::Poll::Ready((|| x)());
                        } else if (|| condition)() {
                            *__fut3 = ::core::ops::ControlFlow::Break(
                                ::core::option::Option::Some((|| {
                                    println!("Hello");
                                    if predicate() {
                                        return 2;
                                    }
                                    1
                                })()),
                            );
                            break 'poll_scope;
                        } else if let None = (|| condition.then_some("abc"))() {
                            return ::core::task::Poll::Ready((|| 0)());
                        } else {
                            return ::core::task::Poll::Ready((|| 0)());
                        }
                    } else {
                        __done = false;
                    }
                }
            }
            'poll_scope: {
                if let ::core::ops::ControlFlow::Continue(__fut_inner) = __fut4 {
                    if let ::core::task::Poll::Ready(__o) = ::core::future::Future::poll(
                        unsafe { ::core::pin::Pin::new_unchecked(__fut_inner) },
                        __cx,
                    ) {
                        if __o {
                            *__fut4 = ::core::ops::ControlFlow::Break(
                                ::core::option::Option::Some((|| "abc")()),
                            );
                            break 'poll_scope;
                        } else {
                            return ::core::task::Poll::Ready((|| dbg!(0))());
                        }
                    } else {
                        __done = false;
                    }
                }
            }
            'poll_scope: {
                if let ::core::ops::ControlFlow::Continue(__fut_inner) = __fut5 {
                    if let ::core::task::Poll::Ready(__o) = ::core::future::Future::poll(
                        unsafe { ::core::pin::Pin::new_unchecked(__fut_inner) },
                        __cx,
                    ) {
                        match __o {
                            Some(x) if (|| x == 0)() => {
                                return ::core::task::Poll::Ready((|| x)());
                            }
                            Some(x) => {
                                *__fut5 = ::core::ops::ControlFlow::Break(
                                    ::core::option::Option::Some((|| x)()),
                                );
                                break 'poll_scope;
                            }
                            None => {
                                return ::core::task::Poll::Ready((|| 0)());
                            }
                        }
                    } else {
                        __done = false;
                    }
                }
            }
            if __done {
                let (
                    ::core::ops::ControlFlow::Break(__o0),
                    ::core::ops::ControlFlow::Break(__o1),
                    ::core::ops::ControlFlow::Break(__o2),
                    ::core::ops::ControlFlow::Break(__o3),
                    ::core::ops::ControlFlow::Break(__o4),
                    ::core::ops::ControlFlow::Break(__o5),
                ) = (__fut0, __fut1, __fut2, __fut3, __fut4, __fut5)
                else {
                    unsafe { ::core::hint::unreachable_unchecked() }
                };
                match (
                    ::core::option::Option::take(__o0),
                    ::core::option::Option::take(__o1),
                    ::core::option::Option::take(__o2),
                    ::core::option::Option::take(__o3),
                    ::core::option::Option::take(__o4),
                    ::core::option::Option::take(__o5),
                ) {
                    (
                        ::core::option::Option::Some(__o0),
                        ::core::option::Option::Some(__o1),
                        ::core::option::Option::Some(__o2),
                        ::core::option::Option::Some(__o3),
                        ::core::option::Option::Some(__o4),
                        ::core::option::Option::Some(__o5),
                    ) => {
                        return ::core::task::Poll::Ready((|a, _, c: i32, d, _: &str, e| {
                            (a + c + d + e) * coefficient
                        })(
                            __o0, __o1, __o2, __o3, __o4, __o5
                        ));
                    }
                    (None, None, None, None, None, None) => {
                        ::core::panic!("`{}!` future polled after completion`", "combine_futures")
                    }
                    _ => unsafe { ::core::hint::unreachable_unchecked() },
                }
            }
            ::core::task::Poll::Pending
        },
    );
}

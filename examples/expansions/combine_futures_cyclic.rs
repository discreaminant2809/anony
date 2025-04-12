use anony::combine_futures_cyclic;

fn predicate() -> bool {
    true
}

fn zero_branch_case() {
    let _fut = combine_futures_cyclic! {};
    let _fut = ({
        use ::core::future::{Future, IntoFuture};
        use ::core::marker::Unpin;
        use ::core::ops::ControlFlow;
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
    })(|cx| {
        let mut done = true;
        if done {
            let () = () else {
                unsafe { ::core::hint::unreachable_unchecked() }
            };
            match () {
                () => {
                    return ::core::task::Poll::Ready((|| ())());
                }
                () => ::core::panic!("`combine_futures!` future polled after completion`"),
                _ => unsafe { ::core::hint::unreachable_unchecked() },
            }
        }
        ::core::task::Poll::Pending
    });
}

fn one_branch_case() {
    let _fut = combine_futures_cyclic! {
        break async { 0 },
    };
    let _fut = ({
        use ::core::future::{Future, IntoFuture};
        use ::core::marker::Unpin;
        use ::core::ops::ControlFlow;
        use ::core::option::Option;
        use ::core::pin::Pin;
        use ::core::task::{Context, Poll};
        enum CombineFutures<F0: Future, S: FnMut(&mut F0, &mut Context<'_>) -> Poll<O>, O> {
            Inner(F0, S),
        }
        impl<F0: Future, S: FnMut(&mut F0, &mut Context<'_>) -> Poll<O>, O> CombineFutures<F0, S, O> {
            fn new(fut0: impl IntoFuture<IntoFuture = F0>, selector: S) -> Self {
                Self::Inner(IntoFuture::into_future(fut0), selector)
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
                let Self::Inner(fut0, selector) = unsafe { Pin::get_unchecked_mut(self) };
                selector(fut0, cx)
            }
        }
        CombineFutures::new
    })(async { 0 }, |fut0, cx| {
        let mut done = true;
        'poll_scope: {
            if let ::core::task::Poll::Ready(o) =
                ::core::future::Future::poll(unsafe { ::core::pin::Pin::new_unchecked(fut0) }, cx)
            {
                return ::core::task::Poll::Ready((|| o)());
            } else {
                done = false;
            }
        }
        ::core::task::Poll::Pending
    });
}

fn general_case_pure_break() {
    let fut_expr0 = async { 3 };
    let condition = false;
    let _fut = combine_futures_cyclic! {
        break async {2},

        let 2 = async {2} else => break 2 => continue,

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

        if async { true } => continue {
            "abc"
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
    let _fut = combine_futures_cyclic! {
        break async {2},

        let 2 = async {2} else => break 2 => continue,

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

        if async { true } => continue {
            "abc"
        } else => break {
            dbg!(0)
        }

        match async{ Some(2) } {
            Some(x) if x == 0 => break x,
            Some(_x) => continue,
            None => break 0,
        }
    };
}

fn general_case_no_pure_break() {
    let fut_expr0 = async { 3 };
    let condition = false;
    let coefficient = -8;
    let fut = combine_futures_cyclic! {
        move

        continue async {2},

        let 2 = async {2} else => break 2 => continue,

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
    let fut = combine_futures_cyclic! {
        move

        continue async {2},

        let 2 = async {2} else => break 2 => continue,

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
}

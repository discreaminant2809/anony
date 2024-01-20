#[allow(unused)]
fn _struct_expansion() {
    // This is an original code
    let courses = vec![
        "Introduction to Programming",
        "Rust Programming Language",
        "Web Programming",
    ];
    let _o1 = anony::r#struct! {
        name: "foo".to_owned(),
        age: 23,
        height: 180,
        courses,
    };

    // This is its expansion
    let courses = vec![
        "Introduction to Programming",
        "Rust Programming Language",
        "Web Programming",
    ];
    let _o1 = {
        // We don't have to capture `courses` here. It cac't access to our structs below with just a mere identifier
        // The reason we use tuple matching instead of multiple `let`s is because the
        let (name, age, height) = ("foo".to_owned(), 23, 180);
        // Open another scope so that the captured values can't access the struct
        {
            #[::core::prelude::v1::derive(
                ::core::cmp::PartialEq,
                ::core::cmp::Eq,
                ::core::cmp::PartialOrd,
                ::core::cmp::Ord,
                ::core::hash::Hash,
                ::core::clone::Clone,
                ::core::marker::Copy
            )]
            #[::core::prelude::v1::derive(::serde::Serialize)] // if `serde` feature is enabled
            struct Struct<T0, T1, T2, T3> {
                name: T0,
                age: T1,
                height: T2,
                courses: T3,
            }
            struct StructProjMut<'a, T0, T1, T2, T3> {
                name: ::core::pin::Pin<&'a mut T0>,
                age: ::core::pin::Pin<&'a mut T1>,
                height: ::core::pin::Pin<&'a mut T2>,
                courses: ::core::pin::Pin<&'a mut T3>,
            }
            struct StructProjRef<'a, T0, T1, T2, T3> {
                name: ::core::pin::Pin<&'a T0>,
                age: ::core::pin::Pin<&'a T1>,
                height: ::core::pin::Pin<&'a T2>,
                courses: ::core::pin::Pin<&'a T3>,
            }
            impl<T0, T1, T2, T3> ::core::clone::Clone for StructProjRef<'_, T0, T1, T2, T3> {
                #[inline]
                fn clone(&self) -> Self {
                    *self
                }
            }
            impl<T0, T1, T2, T3> ::core::marker::Copy for StructProjRef<'_, T0, T1, T2, T3> {}

            impl<T0, T1, T2, T3> Struct<T0, T1, T2, T3> {
                fn project_mut(
                    self: ::core::pin::Pin<&mut Self>,
                ) -> StructProjMut<'_, T0, T1, T2, T3> {
                    // SAFETY: just a classic pinning projection! We guarantee that
                    // (see https://doc.rust-lang.org/std/pin/index.html#pinning-is-structural-for-field):
                    // 1. The anonymous struct is only Unpin if all the fields are Unpin (guaranteed by the `auto` impl)
                    // 2. We don't provide a destructor for this type
                    // 3. The same as the 2nd point
                    // 4. We provide no operations leading to data being moved
                    unsafe {
                        let this = self.get_unchecked_mut();
                        StructProjMut {
                            name: ::core::pin::Pin::new_unchecked(&mut this.name),
                            age: ::core::pin::Pin::new_unchecked(&mut this.age),
                            height: ::core::pin::Pin::new_unchecked(&mut this.height),
                            courses: ::core::pin::Pin::new_unchecked(&mut this.courses),
                        }
                    }
                }
                fn project_ref(self: ::core::pin::Pin<&Self>) -> StructProjRef<'_, T0, T1, T2, T3> {
                    let this = self.get_ref();
                    unsafe {
                        StructProjRef {
                            name: ::core::pin::Pin::new_unchecked(&this.name),
                            age: ::core::pin::Pin::new_unchecked(&this.age),
                            height: ::core::pin::Pin::new_unchecked(&this.height),
                            courses: ::core::pin::Pin::new_unchecked(&this.courses),
                        }
                    }
                }
            }

            // We don't derive this trait, since the struct's name will get debugged also
            impl<
                    T0: ::core::fmt::Debug,
                    T1: ::core::fmt::Debug,
                    T2: ::core::fmt::Debug,
                    T3: ::core::fmt::Debug,
                > ::core::fmt::Debug for Struct<T0, T1, T2, T3>
            {
                fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                    f.debug_struct("") // The name is concealed, but there is a single space before the opening curly bracket
                        .field(::core::stringify!(name), &self.name)
                        .field(::core::stringify!(age), &self.age)
                        .field(::core::stringify!(height), &self.height)
                        .field(::core::stringify!(courses), &self.courses)
                        .finish()
                }
            }
            Struct {
                name,
                age,
                height,
                courses,
            }
        }
    };
}

#[allow(dead_code)]
fn _join_expansion() {
    // Before we move on we should know that our `join!` has two efficient expansions for no future and 1 future

    // No future at all
    let _fut0 = anony::join!();
    // In this case the struct is zero size!
    // Note that we add `#[must_use]` to warn that it is neither futures nor tokio - our version returns a future instead
    let _fut0 = {
        #[must_use = "unlike other `join!` implementations, this one returns a `Future` that must be explicitly `.await`ed or polled"]
        struct Join;

        impl ::core::future::Future for Join {
            type Output = ();
            #[inline]
            fn poll(
                self: ::core::pin::Pin<&mut Self>,
                _cx: &mut ::core::task::Context<'_>,
            ) -> ::core::task::Poll<<Self as ::core::future::Future>::Output> {
                ::core::task::Poll::Ready(())
            }
        }
        Join
    };

    // Just one future
    let _fut1 = anony::join!(async { 13 });
    // In this case no `MaybeDone`s are generated
    let _fut1 = {
        let fut = async { 13 };
        {
            #[must_use = "unlike other `join!` implementations, this one returns a `Future` that must be explicitly `.await`ed or polled"]
            #[repr(transparent)]
            enum Join<F> {
                Inner(F),
            }
            impl<F: ::core::future::Future> ::core::future::Future for Join<F> {
                type Output = (<F as ::core::future::Future>::Output,);
                #[inline]
                fn poll(
                    self: ::core::pin::Pin<&mut Self>,
                    cx: &mut ::core::task::Context<'_>,
                ) -> ::core::task::Poll<<Self as ::core::future::Future>::Output> {
                    ::core::future::Future::poll(
                        unsafe {
                            self.map_unchecked_mut(|this| {
                                let Self::Inner(fut) = this;
                                fut
                            })
                        },
                        cx,
                    )
                    .map(|o| (o,))
                }
            }
            Join::Inner(fut)
        }
    };

    // Base case
    let _fut_n = anony::join!(async { 134 }, async { "144" }, std::future::pending::<()>());
    let _fut_n = {
        let futs = (async { 134 }, async { "144" }, std::future::pending::<()>());
        // Open another scope so that the futures in the input can't access anything within it
        {
            // Put a "ghost" `#[pin_project]` macro to help know which one is structurally pinned
            // #[pin_project]
            enum MaybeDone<F: ::core::future::Future> {
                Pending(
                    // #[pin]
                    F,
                ),
                // It might seem inefficient... but the compiler can optimize the layout
                // See the `conparisons` example for this regard
                Ready(::core::option::Option<<F as ::core::future::Future>::Output>),
            }
            // Only the wrapped future is considered, not its output, since the former is structurally pinned, while the latter isn't
            // We strictly adhere to the invariant regarding structurally pinned fields
            // If we don't add this, the future's output type is considered also, which shouldn't
            // since the output is NOT structurally pinned
            impl<F: ::core::future::Future + ::core::marker::Unpin> ::core::marker::Unpin for MaybeDone<F> {}

            impl<F: ::core::future::Future> MaybeDone<F> {
                // This method is only used to poll and check the status of the wrapped future. It isn't for getting the result!
                fn poll(
                    mut self: ::core::pin::Pin<&mut Self>,
                    cx: &mut ::core::task::Context<'_>,
                ) -> bool {
                    // SAFETY: pinning projection
                    match unsafe {
                        ::core::pin::Pin::get_unchecked_mut(::core::pin::Pin::as_mut(&mut self))
                    } {
                        Self::Pending(fut) => {
                            // SAFETY: pinning projection. `fut` is structurally pinned
                            match ::core::future::Future::poll(
                                unsafe { ::core::pin::Pin::new_unchecked(fut) },
                                cx,
                            ) {
                                ::core::task::Poll::Ready(o) => {
                                    ::core::pin::Pin::set(
                                        &mut self,
                                        Self::Ready(::core::option::Option::Some(o)),
                                    );
                                    true
                                }
                                _ => false,
                            }
                        }
                        _ => true,
                    }
                }
                fn take_output(
                    self: ::core::pin::Pin<&mut Self>,
                ) -> ::core::option::Option<<F as ::core::future::Future>::Output> {
                    // SAFETY: pinning projection
                    match unsafe { ::core::pin::Pin::get_unchecked_mut(self) } {
                        Self::Pending(_) => ::core::option::Option::None,
                        // the output is NOT structurally pinned
                        Self::Ready(o) => ::core::option::Option::take(o),
                    }
                }
            }

            // Now, our spotlight!
            #[must_use = "unlike other `join!` implementations, this one returns a `Future` that must be explicitly `.await`ed or polled"]
            // #[pin_project]
            enum Join<
                F0: ::core::future::Future,
                F1: ::core::future::Future,
                F2: ::core::future::Future,
            > {
                // We encapsulate fields using enum variant!
                // To access these fields, we must pattern matching with this variant, which requires us to write `Join::Inner`...
                // wait... we can't even specify the struct's name to begin with!
                // Therefore, we have effectively made these fields "private"!
                //
                // Collectively, `Join` only requires all wrapped futures to be `Unpin`,
                // and don't care whether their outputs are `Unpin` or not
                Inner(
                    // #[pin]
                    MaybeDone<F0>,
                    // #[pin]
                    MaybeDone<F1>,
                    // #[pin]
                    MaybeDone<F2>,
                ),
            }
            impl<
                    F0: ::core::future::Future,
                    F1: ::core::future::Future,
                    F2: ::core::future::Future,
                > ::core::future::Future for Join<F0, F1, F2>
            {
                // we include the additional comma in the end
                // previously we should return an 1-ary tuple on having just 1 future to join so that it consistently with 1-ary tuple's construct.
                // However, it is not neccessary anymore! We generate a different implementation for 1-ary `join!``
                type Output = (
                    <F0 as ::core::future::Future>::Output,
                    <F1 as ::core::future::Future>::Output,
                    <F2 as ::core::future::Future>::Output,
                );
                fn poll(
                    self: ::core::pin::Pin<&mut Self>,
                    cx: &mut ::core::task::Context<'_>,
                ) -> ::core::task::Poll<<Self as ::core::future::Future>::Output> {
                    // SAFETY: pinning projection! All `Maybedone`s are structurally pinned
                    let Self::Inner(maybe_done0, maybe_done1, maybe_done2) =
                        unsafe { ::core::pin::Pin::get_unchecked_mut(self) };

                    if !unsafe {
                        // only use single ampersand here, since we must poll all of these futures and not short-circuit any
                        MaybeDone::poll(::core::pin::Pin::new_unchecked(maybe_done0), cx)
                            & MaybeDone::poll(::core::pin::Pin::new_unchecked(maybe_done1), cx)
                            & MaybeDone::poll(::core::pin::Pin::new_unchecked(maybe_done2), cx)
                    } {
                        return ::core::task::Poll::Pending;
                    }

                    unsafe {
                        match (
                            MaybeDone::take_output(::core::pin::Pin::new_unchecked(maybe_done0)),
                            MaybeDone::take_output(::core::pin::Pin::new_unchecked(maybe_done1)),
                            MaybeDone::take_output(::core::pin::Pin::new_unchecked(maybe_done2)),
                        ) {
                            (
                                ::core::option::Option::Some(o0),
                                ::core::option::Option::Some(o1),
                                ::core::option::Option::Some(o2),
                            ) => ::core::task::Poll::Ready((o0, o1, o2)),
                            _ => ::core::panic!("`join!` future polled after completion"),
                        }
                    }
                }
            }

            // Finally...
            Join::Inner(
                MaybeDone::Pending(futs.0),
                MaybeDone::Pending(futs.1),
                MaybeDone::Pending(futs.2),
            )
        }
    };
}

#[allow(dead_code)]
fn _join_cyclic_expansion() {
    // Before we move on we should know that our `join_cyclic!` has two efficient expansions for no future and 1 future

    // No future at all
    let _fut0 = anony::join_cyclic!();
    // For no future and 1 future, the expansions are the same as `join!`, and only differ in the struct's name
    let _fut0 = {
        #[must_use = "unlike other `join!` implementations, this one returns a `Future` that must be explicitly `.await`ed or polled"]
        struct JoinCyclic;

        impl ::core::future::Future for JoinCyclic {
            type Output = ();
            #[inline]
            fn poll(
                self: ::core::pin::Pin<&mut Self>,
                _cx: &mut ::core::task::Context<'_>,
            ) -> ::core::task::Poll<<Self as ::core::future::Future>::Output> {
                ::core::task::Poll::Ready(())
            }
        }
        JoinCyclic
    };

    // Just one future
    let _fut1 = anony::join_cyclic!(async { 13 });
    let _fut1 = {
        let fut = async { 13 };
        {
            #[must_use = "unlike other `join!` implementations, this one returns a `Future` that must be explicitly `.await`ed or polled"]
            #[repr(transparent)]
            enum JoinCyclic<F> {
                Inner(F),
            }
            impl<F: ::core::future::Future> ::core::future::Future for JoinCyclic<F> {
                type Output = (<F as ::core::future::Future>::Output,);
                #[inline]
                fn poll(
                    self: ::core::pin::Pin<&mut Self>,
                    cx: &mut ::core::task::Context<'_>,
                ) -> ::core::task::Poll<<Self as ::core::future::Future>::Output> {
                    ::core::future::Future::poll(
                        unsafe {
                            self.map_unchecked_mut(|this| {
                                let Self::Inner(fut) = this;
                                fut
                            })
                        },
                        cx,
                    )
                    .map(|o| (o,))
                }
            }
            JoinCyclic::Inner(fut)
        }
    };

    // Base case
    let _fut_n = anony::join_cyclic!(async { 134 }, async { "144" }, std::future::pending::<()>());
    // The only difference is in the base case. Specifically, the polling logic
    let _fut_n = {
        let futs = (async { 134 }, async { "144" }, std::future::pending::<()>());
        {
            // #[pin_project]
            enum MaybeDone<F: ::core::future::Future> {
                Pending(
                    // #[pin]
                    F,
                ),
                Ready(::core::option::Option<<F as ::core::future::Future>::Output>),
            }
            impl<F: ::core::future::Future + ::core::marker::Unpin> ::core::marker::Unpin for MaybeDone<F> {}

            impl<F: ::core::future::Future> MaybeDone<F> {
                fn poll(
                    mut self: ::core::pin::Pin<&mut Self>,
                    cx: &mut ::core::task::Context<'_>,
                ) -> bool {
                    match unsafe {
                        ::core::pin::Pin::get_unchecked_mut(::core::pin::Pin::as_mut(&mut self))
                    } {
                        Self::Pending(fut) => {
                            match ::core::future::Future::poll(
                                unsafe { ::core::pin::Pin::new_unchecked(fut) },
                                cx,
                            ) {
                                ::core::task::Poll::Ready(o) => {
                                    ::core::pin::Pin::set(
                                        &mut self,
                                        Self::Ready(::core::option::Option::Some(o)),
                                    );
                                    true
                                }
                                _ => false,
                            }
                        }
                        _ => true,
                    }
                }
                fn take_output(
                    self: ::core::pin::Pin<&mut Self>,
                ) -> ::core::option::Option<<F as ::core::future::Future>::Output> {
                    match unsafe { ::core::pin::Pin::get_unchecked_mut(self) } {
                        Self::Pending(_) => ::core::option::Option::None,
                        Self::Ready(o) => ::core::option::Option::take(o),
                    }
                }
            }

            // #[pin_project]
            #[must_use = "unlike other `join!` implementations, this one returns a `Future` that must be explicitly `.await`ed or polled"]
            enum JoinCyclic<
                F0: ::core::future::Future,
                F1: ::core::future::Future,
                F2: ::core::future::Future,
            > {
                Inner(
                    // #[pin]
                    MaybeDone<F0>,
                    // #[pin]
                    MaybeDone<F1>,
                    // #[pin]
                    MaybeDone<F2>,
                    // This is NOT structurally pinned!
                    usize,
                ),
            }
            impl<
                    F0: ::core::future::Future,
                    F1: ::core::future::Future,
                    F2: ::core::future::Future,
                > ::core::future::Future for JoinCyclic<F0, F1, F2>
            {
                type Output = (
                    <F0 as ::core::future::Future>::Output,
                    <F1 as ::core::future::Future>::Output,
                    <F2 as ::core::future::Future>::Output,
                );
                fn poll(
                    self: ::core::pin::Pin<&mut Self>,
                    cx: &mut ::core::task::Context<'_>,
                ) -> ::core::task::Poll<<Self as ::core::future::Future>::Output> {
                    let Self::Inner(maybe_done0, maybe_done1, maybe_done2, skip_next_time) =
                        unsafe { ::core::pin::Pin::get_unchecked_mut(self) };

                    // For `join_cyclic!`, we will use `tokio`'s approach
                    // See https://docs.rs/tokio/latest/src/tokio/macros/join.rs.html#57-166
                    // Tokio's code is licensed under the MIT License: https://github.com/tokio-rs/tokio/blob/master/LICENSE
                    if !unsafe {
                        const COUNT: usize = 3usize;
                        let mut done = true;
                        let mut to_run = COUNT;
                        let mut to_skip = ::core::mem::replace(
                            skip_next_time,
                            // `COUNT` is always > 1 since we've guarded the `0` and `1` cases explicitly (for more efficient logics)
                            // so `clippy::modulo_one` won't be triggered
                            (*skip_next_time + 1) % COUNT,
                        );

                        loop {
                            if to_skip > 0 {
                                to_skip -= 1;
                            } else {
                                // We alter the logic a bit, since we guarantee that `to_run` starts with a number > 1
                                // because we initialize it with `COUNT`.
                                // By doing this, we reduce the number of checks
                                done &= MaybeDone::poll(
                                    ::core::pin::Pin::new_unchecked(maybe_done0),
                                    cx,
                                );
                                if to_run <= 1 {
                                    // if we are the last one...
                                    break done;
                                }
                                to_run -= 1;
                            }

                            // The same for the rest
                            if to_skip > 0 {
                                to_skip -= 1;
                            } else {
                                done &= MaybeDone::poll(
                                    ::core::pin::Pin::new_unchecked(maybe_done1),
                                    cx,
                                );
                                if to_run <= 1 {
                                    break done;
                                }
                                to_run -= 1;
                            }

                            if to_skip > 0 {
                                to_skip -= 1;
                            } else {
                                done &= MaybeDone::poll(
                                    ::core::pin::Pin::new_unchecked(maybe_done2),
                                    cx,
                                );
                                if to_run <= 1 {
                                    break done;
                                }
                                to_run -= 1;
                            }
                        }
                    } {
                        return ::core::task::Poll::Pending;
                    }

                    unsafe {
                        match (
                            MaybeDone::take_output(::core::pin::Pin::new_unchecked(maybe_done0)),
                            MaybeDone::take_output(::core::pin::Pin::new_unchecked(maybe_done1)),
                            MaybeDone::take_output(::core::pin::Pin::new_unchecked(maybe_done2)),
                        ) {
                            (
                                ::core::option::Option::Some(o0),
                                ::core::option::Option::Some(o1),
                                ::core::option::Option::Some(o2),
                            ) => ::core::task::Poll::Ready((o0, o1, o2)),
                            _ => ::core::panic!("`join!` future polled after completion"),
                        }
                    }
                }
            }

            JoinCyclic::Inner(
                MaybeDone::Pending(futs.0),
                MaybeDone::Pending(futs.1),
                MaybeDone::Pending(futs.2),
                0,
            )
        }
    };
}

fn main() {}

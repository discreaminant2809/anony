#[allow(unused)]
fn _struct_expansion() {
    let x = anony::r#struct! {};
    let x = {
        #[::core::prelude::v1::derive(
            ::core::cmp::PartialEq,
            ::core::cmp::Eq,
            ::core::cmp::PartialOrd,
            ::core::cmp::Ord,
            ::core::hash::Hash,
            ::core::clone::Clone,
            ::core::marker::Copy
        )]
        #[::core::prelude::v1::derive(::serde::Serialize)]
        struct Struct;

        struct StructProjMut<'a>(::core::marker::PhantomData<::core::pin::Pin<&'a mut Struct>>);

        #[derive(Clone, Copy)]
        struct StructProjRef<'a>(::core::marker::PhantomData<::core::pin::Pin<&'a Struct>>);

        impl Struct {
            #[inline]
            fn project_mut(self: ::core::pin::Pin<&mut Self>) -> StructProjMut<'_> {
                StructProjMut(::core::marker::PhantomData)
            }
            #[inline]
            fn project_ref(self: ::core::pin::Pin<&mut Self>) -> StructProjRef<'_> {
                StructProjRef(::core::marker::PhantomData)
            }
        }
        impl ::core::fmt::Debug for Struct {
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                ::core::fmt::Formatter::write_str(f, "{}")
            }
        }
        Struct
    };

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

#[allow(unused_imports, unused, clippy::match_single_binding)]
fn _tuple_expansion() {
    // The expansions below are basically the same as `r#struct!`, so there is nothing worth explaining.
    let _x = anony::tuple!();
    let _x = {
        use ::core::clone::Clone;
        use ::core::marker::Copy;
        use ::core::pin::Pin;
        #[::core::prelude::v1::derive(
            ::core::cmp::PartialEq,
            ::core::cmp::Eq,
            ::core::cmp::PartialOrd,
            ::core::cmp::Ord,
            ::core::hash::Hash,
            Clone,
            Copy
        )]
        struct Tuple;

        impl Tuple {
            fn project_ref(self: Pin<&Self>) {}

            fn project_mut(self: Pin<&mut Self>) {}
        }
        use ::core::fmt::Debug;
        impl Debug for Tuple {
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                f.write_str("()")
            }
        }
        use ::core::convert::From;
        impl From<Tuple> for () {
            #[inline]
            fn from(x: Tuple) -> Self {}
        }
        use ::core::result::Result;
        use ::serde::{ser::SerializeTuple, Serialize, Serializer};
        impl Serialize for Tuple {
            fn serialize<S: Serializer>(
                &self,
                serializer: S,
            ) -> Result<<S as Serializer>::Ok, <S as Serializer>::Error> {
                let mut ser_tuple = Serializer::serialize_tuple(serializer, 0usize)?;
                SerializeTuple::end(ser_tuple)
            }
        }
        Tuple
    };

    let _x = anony::tuple!(2, "3");
    let _x = match (2, "3") {
        inputs => {
            use ::core::clone::Clone;
            use ::core::marker::Copy;
            use ::core::pin::Pin;
            #[::core::prelude::v1::derive(
                ::core::cmp::PartialEq,
                ::core::cmp::Eq,
                ::core::cmp::PartialOrd,
                ::core::cmp::Ord,
                ::core::hash::Hash,
                Clone,
                Copy
            )]
            struct Tuple<T0, T1>(T0, T1);

            impl<T0, T1> Tuple<T0, T1> {
                fn project_ref(self: Pin<&Self>) -> (Pin<&'_ T0>, Pin<&'_ T1>) {
                    let this = Pin::get_ref(self);
                    unsafe { (Pin::new_unchecked(&this.0), Pin::new_unchecked(&this.1)) }
                }
                fn project_mut(self: Pin<&mut Self>) -> (Pin<&'_ mut T0>, Pin<&'_ mut T1>) {
                    unsafe {
                        let this = Pin::get_unchecked_mut(self);
                        (
                            Pin::new_unchecked(&mut this.0),
                            Pin::new_unchecked(&mut this.1),
                        )
                    }
                }
            }
            use ::core::fmt::Debug;
            impl<T0: Debug, T1: Debug> Debug for Tuple<T0, T1> {
                fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                    f.debug_tuple("").field(&self.0).field(&self.1).finish()
                }
            }
            use ::core::convert::From;
            impl<T0, T1> From<Tuple<T0, T1>> for (T0, T1) {
                #[inline]
                fn from(x: Tuple<T0, T1>) -> Self {
                    (x.0, x.1)
                }
            }
            use ::core::result::Result;
            use ::serde::{ser::SerializeTuple, Serialize, Serializer};
            impl<T0: Serialize, T1: Serialize> Serialize for Tuple<T0, T1> {
                fn serialize<S: Serializer>(
                    &self,
                    serializer: S,
                ) -> Result<<S as Serializer>::Ok, <S as Serializer>::Error> {
                    let mut ser_tuple: <S as Serializer>::SerializeTuple =
                        Serializer::serialize_tuple(serializer, 2usize)?;
                    SerializeTuple::serialize_element(&mut ser_tuple, &self.0)?;
                    SerializeTuple::serialize_element(&mut ser_tuple, &self.1)?;
                    SerializeTuple::end(ser_tuple)
                }
            }
            Tuple(inputs.0, inputs.1)
        }
    };
}

#[allow(dead_code, unused_imports, unused_parens, clippy::double_parens)]
fn _join_expansion() {
    // Before we move on we should know that our `join!` has two efficient expansions for no future and 1 future

    // No future at all
    let _fut0 = anony::join!();
    // In this case the struct is zero size!
    // Note that we add `#[must_use]` to warn that it is neither futures nor tokio - our version returns a future instead
    let _fut0 = {
        use ::core::future::Future;
        use ::core::pin::Pin;
        use ::core::task::{Context, Poll};
        #[must_use = "unlike other `join!` implementations, this one returns a `Future` that must be explicitly `.await`ed or polled"]
        struct Join;

        impl Future for Join {
            type Output = ();
            #[inline]
            fn poll(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<<Self as Future>::Output> {
                Poll::Ready(())
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
            use ::core::future::Future;
            use ::core::pin::Pin;
            use ::core::task::{Context, Poll};
            #[must_use = "unlike other `join!` implementations, this one returns a `Future` that must be explicitly `.await`ed or polled"]
            #[repr(transparent)]
            enum Join<F: Future> {
                Inner(F),
            }
            impl<F: Future> Future for Join<F> {
                type Output = (<F as Future>::Output,);
                #[inline]
                fn poll(
                    self: Pin<&mut Self>,
                    cx: &mut Context<'_>,
                ) -> Poll<<Self as Future>::Output> {
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
    let _fut_n = {
        let futs = (async { 134 }, async { "144" }, std::future::pending::<()>());
        // Open another scope so that the futures in the input can't access anything within it
        {
            use ::core::future::Future;
            use ::core::hint::unreachable_unchecked;
            use ::core::option::Option::{self, None, Some};
            use ::core::pin::Pin;
            use ::core::task::{Context, Poll};

            // Put a "ghost" `#[pin_project]` macro to help know which one is structurally pinned
            // #[pin_project]
            enum MaybeDone<F: Future> {
                Pending(
                    // #[pin]
                    F,
                ),
                // It might seem inefficient... but the compiler can optimize the layout
                // See the `conparisons` example for this regard
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
                    match unsafe { Pin::get_unchecked_mut(Pin::as_mut(&mut self)) } {
                        Self::Pending(fut) => {
                            // SAFETY: pinning projection. `fut` is structurally pinned
                            match Future::poll(unsafe { Pin::new_unchecked(fut) }, cx) {
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

                // SAFETY: the caller must only call it when `self` is `Self::Ready(Some)`
                unsafe fn force_take_output(self: Pin<&mut Self>) -> <F as Future>::Output {
                    // SAFETY: pinning projection
                    match Pin::get_unchecked_mut(self) {
                        Self::Pending(_) => unreachable_unchecked(),
                        // SAFETY: the output is NOT structurally pinned
                        Self::Ready(o) => o.take().unwrap_unchecked(),
                    }
                }
            }

            // Now, our spotlight!
            #[must_use = "unlike other `join!` implementations, this one returns a `Future` that must be explicitly `.await`ed or polled"]
            // #[pin_project]
            enum Join<F0: Future, F1: Future, F2: Future> {
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

            impl<F0: Future, F1: Future, F2: Future> Future for Join<F0, F1, F2> {
                type Output = (
                    <F0 as Future>::Output,
                    <F1 as Future>::Output,
                    <F2 as Future>::Output,
                );
                fn poll(
                    self: Pin<&mut Self>,
                    cx: &mut Context<'_>,
                ) -> Poll<<Self as Future>::Output> {
                    // SAFETY: pinning projection! All `Maybedone`s are structurally pinned
                    let Self::Inner(maybe_done0, maybe_done1, maybe_done2) =
                        unsafe { Pin::get_unchecked_mut(self) };
                    if !unsafe {
                        let mut done = true;
                        done &= MaybeDone::poll(Pin::new_unchecked(&mut *maybe_done0), cx);
                        done &= MaybeDone::poll(Pin::new_unchecked(&mut *maybe_done1), cx);
                        done &= MaybeDone::poll(Pin::new_unchecked(&mut *maybe_done2), cx);
                        done
                    } {
                        return Poll::Pending;
                    }
                    unsafe {
                        // We only need to check the first MaybeDone since all the MaybeDone::Ready are either all Some or all None
                        match maybe_done0 {
                            MaybeDone::Ready(Some(_)) => Poll::Ready(
                                ((
                                    // SAFETY: they're at `MaybeDone::Ready(Some)` variant
                                    MaybeDone::force_take_output(Pin::new_unchecked(maybe_done0)),
                                    MaybeDone::force_take_output(Pin::new_unchecked(maybe_done1)),
                                    MaybeDone::force_take_output(Pin::new_unchecked(maybe_done2)),
                                )),
                            ),
                            // SAFETY: they have been done. It leads to a more efficient codegen
                            MaybeDone::Pending(_) => unreachable_unchecked(),
                            _ => ::core::panic!("`{}!` future polled after completion", "join"),
                        }
                    }
                }
            }

            // Finally...
            use ::core::future::IntoFuture;
            Join::Inner(
                MaybeDone::Pending(IntoFuture::into_future(futs.0)),
                MaybeDone::Pending(IntoFuture::into_future(futs.1)),
                MaybeDone::Pending(IntoFuture::into_future(futs.2)),
            )
        }
    };
}

#[allow(dead_code, unused_imports, unused_parens, clippy::double_parens)]
fn _join_cyclic_expansion() {
    // Before we move on we should know that our `join_cyclic!` has two efficient expansions for no future and 1 future

    // No future at all
    let _fut0 = anony::join_cyclic!();
    // For no future and 1 future, the expansions are the same as `join!`, and only differ in the struct's name
    let _fut0 = {
        use ::core::future::Future;
        use ::core::pin::Pin;
        use ::core::task::{Context, Poll};
        #[must_use = "unlike other `join!` implementations, this one returns a `Future` that must be explicitly `.await`ed or polled"]
        struct JoinCyclic;

        impl Future for JoinCyclic {
            type Output = ();
            #[inline]
            fn poll(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<<Self as Future>::Output> {
                Poll::Ready(())
            }
        }
        JoinCyclic
    };

    // Just one future
    let _fut1 = anony::join_cyclic!(async { 13 });
    let _fut1 = {
        let fut = async { 13 };
        {
            use ::core::future::Future;
            use ::core::pin::Pin;
            use ::core::task::{Context, Poll};
            #[must_use = "unlike other `join!` implementations, this one returns a `Future` that must be explicitly `.await`ed or polled"]
            #[repr(transparent)]
            enum JoinCyclic<F: Future> {
                Inner(F),
            }
            impl<F: Future> Future for JoinCyclic<F> {
                type Output = (<F as Future>::Output,);
                #[inline]
                fn poll(
                    self: Pin<&mut Self>,
                    cx: &mut Context<'_>,
                ) -> Poll<<Self as Future>::Output> {
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
            JoinCyclic::Inner(IntoFuture::into_future(fut))
        }
    };

    // Base case
    let _fut_n = anony::join_cyclic!(async { 134 }, async { "144" }, std::future::pending::<()>());
    // The only difference is in the base case. Specifically, the polling logic
    let _fut_n = {
        let futs = (async { 134 }, async { "144" }, std::future::pending::<()>());
        {
            use ::core::future::Future;
            use ::core::hint::unreachable_unchecked;
            use ::core::option::Option::{self, None, Some};
            use ::core::pin::Pin;
            use ::core::task::{Context, Poll};

            // #[pin_project]
            enum MaybeDone<F: Future> {
                Pending(
                    // #[pin]
                    F,
                ),
                Ready(Option<<F as Future>::Output>),
            }
            use ::core::marker::Unpin;
            impl<F: Future + Unpin> Unpin for MaybeDone<F> {}

            impl<F: Future> MaybeDone<F> {
                fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> bool {
                    match unsafe { Pin::get_unchecked_mut(Pin::as_mut(&mut self)) } {
                        Self::Pending(fut) => {
                            match Future::poll(unsafe { Pin::new_unchecked(fut) }, cx) {
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

                // SAFETY: the caller must only call it when `self` is `Self::Ready(Some)`
                unsafe fn force_take_output(self: Pin<&mut Self>) -> <F as Future>::Output {
                    // SAFETY: pinning projection
                    match Pin::get_unchecked_mut(self) {
                        Self::Pending(_) => unreachable_unchecked(),
                        // SAFETY: the output is NOT structurally pinned
                        Self::Ready(o) => o.take().unwrap_unchecked(),
                    }
                }
            }

            // #[pin_project]
            #[must_use = "unlike other `join!` implementations, this one returns a `Future` that must be explicitly `.await`ed or polled"]
            enum JoinCyclic<F0: Future, F1: Future, F2: Future> {
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
            impl<F0: Future, F1: Future, F2: Future> Future for JoinCyclic<F0, F1, F2> {
                type Output = (
                    <F0 as Future>::Output,
                    <F1 as Future>::Output,
                    <F2 as Future>::Output,
                );
                fn poll(
                    self: Pin<&mut Self>,
                    cx: &mut Context<'_>,
                ) -> Poll<<Self as Future>::Output> {
                    let Self::Inner(maybe_done0, maybe_done1, maybe_done2, skip_next_time) =
                        unsafe { Pin::get_unchecked_mut(self) };

                    // For `join_cyclic!`, we will use `tokio`'s approach
                    // See https://docs.rs/tokio/latest/src/tokio/macros/join.rs.html#57-166
                    // Tokio's code is licensed under the MIT License: https://github.com/tokio-rs/tokio/blob/master/LICENSE
                    if !unsafe {
                        const COUNT: usize = 3usize;
                        let mut done = true;
                        let mut to_run = COUNT;
                        let mut to_skip =
                            ::core::mem::replace(skip_next_time, (*skip_next_time + 1) % COUNT);
                        loop {
                            if to_skip > 0 {
                                to_skip -= 1;
                            } else {
                                // We alter the logic a bit, since we guarantee that `to_run` starts with a number > 1
                                // because we initialize it with `COUNT`.
                                // By doing this, we reduce the number of checks
                                done &= MaybeDone::poll(Pin::new_unchecked(&mut *maybe_done0), cx);
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
                                done &= MaybeDone::poll(Pin::new_unchecked(&mut *maybe_done1), cx);
                                if to_run <= 1 {
                                    break done;
                                }
                                to_run -= 1;
                            }

                            if to_skip > 0 {
                                to_skip -= 1;
                            } else {
                                done &= MaybeDone::poll(Pin::new_unchecked(&mut *maybe_done2), cx);
                                if to_run <= 1 {
                                    break done;
                                }
                                to_run -= 1;
                            }
                        }
                    } {
                        return Poll::Pending;
                    }
                    unsafe {
                        // We only need to check the first MaybeDone since all the MaybeDone::Ready are either all Some or all None
                        match maybe_done0 {
                            MaybeDone::Ready(Some(_)) => Poll::Ready(
                                ((
                                    // SAFETY: they're at `MaybeDone::Ready(Some)` variant
                                    MaybeDone::force_take_output(Pin::new_unchecked(maybe_done0)),
                                    MaybeDone::force_take_output(Pin::new_unchecked(maybe_done1)),
                                    MaybeDone::force_take_output(Pin::new_unchecked(maybe_done2)),
                                )),
                            ),
                            // SAFETY: they have been done. It leads to a more efficient codegen
                            MaybeDone::Pending(_) => unreachable_unchecked(),
                            _ => ::core::panic!(
                                "`{}!` future polled after completion",
                                "join_cyclic"
                            ),
                        }
                    }
                }
            }
            use ::core::future::IntoFuture;
            JoinCyclic::Inner(
                MaybeDone::Pending(IntoFuture::into_future(futs.0)),
                MaybeDone::Pending(IntoFuture::into_future(futs.1)),
                MaybeDone::Pending(IntoFuture::into_future(futs.2)),
                0,
            )
        }
    };
}

// This function exists since an empty
fn expect_output(_f: impl std::future::Future<Output = Option<()>>) {}

// Separate it so that you can focus on the main logic, since all `try_join(_cyclic)` expansions here have this
// The actual expansion will inline it instead
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

#[allow(dead_code, unused_imports, unused_parens, clippy::double_parens)]
fn _try_join_expansion() {
    // No future at all
    let fut_0 = anony::try_join!();
    expect_output(fut_0);
    let fut_0 = {
        use ::core::future::Future;
        use ::core::marker::PhantomData;
        use ::core::ops::ControlFlow;
        use ::core::option::Option::{self, None, Some};
        use ::core::pin::Pin;
        use ::core::result::Result::{self, Err, Ok};
        use ::core::task::{Context, Poll};

        // See the explanation above
        // We inline the whole `Try` and `Residual` implementation here in an actual expansion
        try_trait!();

        #[must_use = "unlike other `try_join!` implementations, this one returns a `Future` that must be explicitly `.await`ed or polled"]
        #[repr(transparent)]
        enum TryJoin<T: Try<Output = ()>> {
            Inner(PhantomData<T>),
        }
        impl<T: Try<Output = ()>> Future for TryJoin<T> {
            type Output = T;
            #[inline]
            fn poll(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<<Self as Future>::Output> {
                Poll::Ready(Try::from_output(()))
            }
        }
        TryJoin::Inner(PhantomData)
    };
    expect_output(fut_0);

    // Just 1 future
    let _fut_1 = anony::try_join!(async { Some(()) });
    let _fut_1 = {
        let fut = async { Some(()) };
        {
            use ::core::future::Future;
            use ::core::ops::ControlFlow;
            use ::core::option::Option::{self, None, Some};
            use ::core::pin::Pin;
            use ::core::result::Result::{self, Err, Ok};
            use ::core::task::{Context, Poll};

            try_trait!();

            #[must_use = "unlike other `try_join!` implementations, this one returns a `Future` that must be explicitly `.await`ed or polled"]
            #[repr(transparent)]
            enum TryJoin<F> {
                Inner(F),
            }
            impl<F: Future, R: Residual<(<<F as Future>::Output as Try>::Output,)>> Future for TryJoin<F>
            where
                <F as Future>::Output: Try<Residual = R>,
            {
                type Output = <R as Residual<(<<F as Future>::Output as Try>::Output,)>>::TryType;
                #[inline]
                fn poll(
                    self: Pin<&mut Self>,
                    cx: &mut Context<'_>,
                ) -> Poll<<Self as Future>::Output> {
                    Future::poll(
                        unsafe {
                            Pin::map_unchecked_mut(self, |this| {
                                let Self::Inner(fut) = this;
                                fut
                            })
                        },
                        cx,
                    )
                    // Things get a little bit complicated
                    .map(|o| match Try::branch(o) {
                        ControlFlow::Continue(c) => Try::from_output((c,)),
                        ControlFlow::Break(b) => Try::from_residual(b),
                    })
                }
            }
            use ::core::future::IntoFuture;
            TryJoin::Inner(IntoFuture::into_future(fut))
        }
    };

    // Base case
    let _fut_3 = anony::try_join!(async { Some(()) }, async { Some(()) }, async { Some(()) });
    let _fut_3 = {
        let futs = (async { Some(()) }, async { Some(()) }, async { Some(()) });
        {
            use ::core::future::Future;
            use ::core::hint::unreachable_unchecked;
            use ::core::ops::ControlFlow;
            use ::core::option::Option::{self, None, Some};
            use ::core::pin::Pin;
            use ::core::result::Result::{self, Err, Ok};
            use ::core::task::{Context, Poll};

            try_trait!();

            // #[pin_project]
            enum MaybeDone<F: Future>
            where
                F::Output: Try,
            {
                Pending(
                    // #[pin]
                    F,
                ),
                // Don't be confused with `Future::Output` and `Try::Output`!
                Ready(Option<<<F as Future>::Output as Try>::Output>),
            }
            use ::core::marker::Unpin;
            impl<F: Future + Unpin> Unpin for MaybeDone<F> where <F as Future>::Output: Try {}

            impl<F: Future> MaybeDone<F>
            where
                <F as Future>::Output: Try,
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

                // SAFETY: the caller must only call it when `self` is `Self::Ready(Some)`
                unsafe fn force_take_output(
                    self: Pin<&mut Self>,
                ) -> <<F as Future>::Output as Try>::Output {
                    // SAFETY: pinning projection
                    match Pin::get_unchecked_mut(self) {
                        Self::Pending(_) => unreachable_unchecked(),
                        // SAFETY: the output is NOT structurally pinned
                        Self::Ready(o) => o.take().unwrap_unchecked(),
                    }
                }
            }

            // #[pin_project]
            #[must_use = "unlike other `try_join!` implementations, this one returns a `Future` that must be explicitly `.await`ed or polled"]
            enum TryJoin<
                F0: Future,
                F1: Future,
                F2: Future,
                R: Residual<(
                    <<F0 as Future>::Output as Try>::Output,
                    <<F1 as Future>::Output as Try>::Output,
                    <<F2 as Future>::Output as Try>::Output,
                )>,
            >
            where
                <F0 as Future>::Output: Try<Residual = R>,
                <F1 as Future>::Output: Try<Residual = R>,
                <F2 as Future>::Output: Try<Residual = R>,
            {
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
                    F0: Future,
                    F1: Future,
                    F2: Future,
                    R: Residual<(
                        <<F0 as Future>::Output as Try>::Output,
                        <<F1 as Future>::Output as Try>::Output,
                        <<F2 as Future>::Output as Try>::Output,
                    )>,
                > Future for TryJoin<F0, F1, F2, R>
            where
                <F0 as Future>::Output: Try<Residual = R>,
                <F1 as Future>::Output: Try<Residual = R>,
                <F2 as Future>::Output: Try<Residual = R>,
            {
                type Output = <R as Residual<(
                    <<F0 as Future>::Output as Try>::Output,
                    <<F1 as Future>::Output as Try>::Output,
                    <<F2 as Future>::Output as Try>::Output,
                )>>::TryType;

                fn poll(
                    self: Pin<&mut Self>,
                    cx: &mut Context<'_>,
                ) -> Poll<<Self as Future>::Output> {
                    // SAFETY: pinning projection! All `Maybedone`s are structurally pinned
                    let Self::Inner(maybe_done0, maybe_done1, maybe_done2) =
                        unsafe { Pin::get_unchecked_mut(self) };
                    if !unsafe {
                        let mut done = true;
                        match MaybeDone::poll(Pin::new_unchecked(&mut *maybe_done0), cx) {
                            ControlFlow::Continue(ready) => done &= ready,
                            ControlFlow::Break(r) => return Poll::Ready(Try::from_residual(r)),
                        }
                        match MaybeDone::poll(Pin::new_unchecked(&mut *maybe_done1), cx) {
                            ControlFlow::Continue(ready) => done &= ready,
                            ControlFlow::Break(r) => return Poll::Ready(Try::from_residual(r)),
                        }
                        match MaybeDone::poll(Pin::new_unchecked(&mut *maybe_done2), cx) {
                            ControlFlow::Continue(ready) => done &= ready,
                            ControlFlow::Break(r) => return Poll::Ready(Try::from_residual(r)),
                        }
                        done
                    } {
                        return Poll::Pending;
                    }
                    unsafe {
                        // We only need to check the first MaybeDone since all the MaybeDone::Ready are either all Some or all None
                        match maybe_done0 {
                            MaybeDone::Ready(Some(_)) => Poll::Ready(Try::from_output((
                                MaybeDone::force_take_output(Pin::new_unchecked(maybe_done0)),
                                MaybeDone::force_take_output(Pin::new_unchecked(maybe_done1)),
                                MaybeDone::force_take_output(Pin::new_unchecked(maybe_done2)),
                            ))),
                            MaybeDone::Pending(_) => unreachable_unchecked(),
                            _ => ::core::panic!("`{}!` future polled after completion", "try_join"),
                        }
                    }
                }
            }

            use ::core::future::IntoFuture;
            TryJoin::Inner(
                MaybeDone::Pending(IntoFuture::into_future(futs.0)),
                MaybeDone::Pending(IntoFuture::into_future(futs.1)),
                MaybeDone::Pending(IntoFuture::into_future(futs.2)),
            )
        }
    };
}

#[allow(dead_code, unused_imports, unused_parens, clippy::double_parens)]
fn _try_join_cyclic_expansion() {
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

        // See the explanation above
        // We inline the whole `Try` and `Residual` implementation here in an actual expansion
        try_trait!();

        #[must_use = "unlike other `try_join!` implementations, this one returns a `Future` that must be explicitly `.await`ed or polled"]
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
    let _fut_1 = {
        let fut = async { Some(()) };
        {
            use ::core::future::Future;
            use ::core::ops::ControlFlow;
            use ::core::option::Option::{self, None, Some};
            use ::core::pin::Pin;
            use ::core::result::Result::{self, Err, Ok};
            use ::core::task::{Context, Poll};

            try_trait!();

            #[must_use = "unlike other `try_join!` implementations, this one returns a `Future` that must be explicitly `.await`ed or polled"]
            #[repr(transparent)]
            enum TryJoinCyclic<F> {
                Inner(F),
            }
            impl<F: Future, R: Residual<(<<F as Future>::Output as Try>::Output,)>> Future for TryJoinCyclic<F>
            where
                <F as Future>::Output: Try<Residual = R>,
            {
                type Output = <R as Residual<(<<F as Future>::Output as Try>::Output,)>>::TryType;
                #[inline]
                fn poll(
                    self: Pin<&mut Self>,
                    cx: &mut Context<'_>,
                ) -> Poll<<Self as Future>::Output> {
                    Future::poll(
                        unsafe {
                            Pin::map_unchecked_mut(self, |this| {
                                let Self::Inner(fut) = this;
                                fut
                            })
                        },
                        cx,
                    )
                    // Things get a little bit complicated
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
    let _f_3 = {
        let futs = (async { Some(()) }, async { Some(()) }, async { Some(()) });
        {
            use ::core::future::Future;
            use ::core::hint::unreachable_unchecked;
            use ::core::ops::ControlFlow;
            use ::core::option::Option::{self, None, Some};
            use ::core::pin::Pin;
            use ::core::result::Result::{self, Err, Ok};
            use ::core::task::{Context, Poll};

            try_trait!();

            // #[pin_project]
            enum MaybeDone<F: Future>
            where
                F::Output: Try,
            {
                Pending(
                    // #[pin]
                    F,
                ),
                // Don't be confused with `Future::Output` and `Try::Output`!
                Ready(Option<<<F as Future>::Output as Try>::Output>),
            }
            use ::core::marker::Unpin;
            impl<F: Future + Unpin> Unpin for MaybeDone<F> where <F as Future>::Output: Try {}

            impl<F: Future> MaybeDone<F>
            where
                <F as Future>::Output: Try,
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

                // SAFETY: the caller must only call it when `self` is `Self::Ready(Some)`
                unsafe fn force_take_output(
                    self: Pin<&mut Self>,
                ) -> <<F as Future>::Output as Try>::Output {
                    // SAFETY: pinning projection
                    match Pin::get_unchecked_mut(self) {
                        Self::Pending(_) => unreachable_unchecked(),
                        // SAFETY: the output is NOT structurally pinned
                        Self::Ready(o) => o.take().unwrap_unchecked(),
                    }
                }
            }

            // #[pin_project]
            #[must_use = "unlike other `try_join!` implementations, this one returns a `Future` that must be explicitly `.await`ed or polled"]
            enum TryJoinCyclic<
                F0: Future,
                F1: Future,
                F2: Future,
                R: Residual<(
                    <<F0 as Future>::Output as Try>::Output,
                    <<F1 as Future>::Output as Try>::Output,
                    <<F2 as Future>::Output as Try>::Output,
                )>,
            >
            where
                <F0 as Future>::Output: Try<Residual = R>,
                <F1 as Future>::Output: Try<Residual = R>,
                <F2 as Future>::Output: Try<Residual = R>,
            {
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
                    F0: Future,
                    F1: Future,
                    F2: Future,
                    R: Residual<(
                        <<F0 as Future>::Output as Try>::Output,
                        <<F1 as Future>::Output as Try>::Output,
                        <<F2 as Future>::Output as Try>::Output,
                    )>,
                > Future for TryJoinCyclic<F0, F1, F2, R>
            where
                <F0 as Future>::Output: Try<Residual = R>,
                <F1 as Future>::Output: Try<Residual = R>,
                <F2 as Future>::Output: Try<Residual = R>,
            {
                type Output = <R as Residual<(
                    <<F0 as Future>::Output as Try>::Output,
                    <<F1 as Future>::Output as Try>::Output,
                    <<F2 as Future>::Output as Try>::Output,
                )>>::TryType;
                fn poll(
                    self: Pin<&mut Self>,
                    cx: &mut Context<'_>,
                ) -> Poll<<Self as Future>::Output> {
                    let Self::Inner(maybe_done0, maybe_done1, maybe_done2, skip_next_time) =
                        unsafe { Pin::get_unchecked_mut(self) };

                    // For `join_cyclic!`, we will use `tokio`'s approach
                    // See https://docs.rs/tokio/latest/src/tokio/macros/join.rs.html#57-166
                    // Tokio's code is licensed under the MIT License: https://github.com/tokio-rs/tokio/blob/master/LICENSE
                    if !unsafe {
                        const COUNT: usize = 3usize;
                        let mut done = true;
                        let mut to_run = COUNT;
                        let mut to_skip =
                            ::core::mem::replace(skip_next_time, (*skip_next_time + 1) % COUNT);
                        loop {
                            if to_skip > 0 {
                                to_skip -= 1;
                            } else {
                                // We alter the logic a bit, since we guarantee that `to_run` starts with a number > 1
                                // because we initialize it with `COUNT`.
                                // By doing this, we reduce the number of checks
                                match MaybeDone::poll(Pin::new_unchecked(&mut *maybe_done0), cx) {
                                    ControlFlow::Continue(ready) => done &= ready,
                                    ControlFlow::Break(r) => {
                                        return Poll::Ready(Try::from_residual(r))
                                    }
                                }
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
                                match MaybeDone::poll(Pin::new_unchecked(&mut *maybe_done1), cx) {
                                    ControlFlow::Continue(ready) => done &= ready,
                                    ControlFlow::Break(r) => {
                                        return Poll::Ready(Try::from_residual(r))
                                    }
                                }
                                if to_run <= 1 {
                                    break done;
                                }
                                to_run -= 1;
                            }
                            if to_skip > 0 {
                                to_skip -= 1;
                            } else {
                                match MaybeDone::poll(Pin::new_unchecked(&mut *maybe_done2), cx) {
                                    ControlFlow::Continue(ready) => done &= ready,
                                    ControlFlow::Break(r) => {
                                        return Poll::Ready(Try::from_residual(r))
                                    }
                                }
                                if to_run <= 1 {
                                    break done;
                                }
                                to_run -= 1;
                            }
                        }
                    } {
                        return Poll::Pending;
                    }

                    unsafe {
                        // We only need to check the first MaybeDone since all the MaybeDone::Ready are either all Some or all None
                        match maybe_done0 {
                            MaybeDone::Ready(Some(_)) => Poll::Ready(Try::from_output((
                                MaybeDone::force_take_output(Pin::new_unchecked(maybe_done0)),
                                MaybeDone::force_take_output(Pin::new_unchecked(maybe_done1)),
                                MaybeDone::force_take_output(Pin::new_unchecked(maybe_done2)),
                            ))),
                            MaybeDone::Pending(_) => unreachable_unchecked(),
                            _ => ::core::panic!(
                                "`{}!` future polled after completion",
                                "try_join_cyclic"
                            ),
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

fn main() {}

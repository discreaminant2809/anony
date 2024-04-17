// Assume `debug_assertions` is enabled.

#![allow(clippy::match_single_binding, unused)]

fn _struct_expansion() {
    let x = anony::r#struct! {};
    let x = {
        use ::core::clone::Clone;
        use ::core::marker::Copy;
        use ::core::marker::PhantomData;
        use ::core::pin::Pin;
        use ::core::stringify;
        #[::core::prelude::v1::derive(
            ::core::cmp::PartialEq,
            ::core::cmp::Eq,
            ::core::cmp::PartialOrd,
            ::core::cmp::Ord,
            ::core::hash::Hash,
            Clone,
            Copy
        )]
        struct Struct;

        struct StructProjRef<'a>(PhantomData<Pin<&'a Struct>>);

        struct StructProjMut<'a>(PhantomData<Pin<&'a mut Struct>>);

        impl Clone for StructProjRef<'_> {
            #[inline]
            fn clone(&self) -> Self {
                *self
            }
        }
        impl Copy for StructProjRef<'_> {}

        impl Struct {
            fn project_ref(self: Pin<&Self>) -> StructProjRef<'_> {
                StructProjRef(PhantomData)
            }
            fn project_mut(self: Pin<&mut Self>) -> StructProjMut<'_> {
                StructProjMut(PhantomData)
            }
        }
        use ::core::fmt::Debug;
        impl Debug for Struct {
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                ::core::fmt::Formatter::write_str(f, "{}")
            }
        }
        use ::core::result::Result;
        use ::serde::{ser::SerializeStruct, Serialize, Serializer};
        impl Serialize for Struct {
            fn serialize<S: Serializer>(
                &self,
                serializer: S,
            ) -> Result<<S as Serializer>::Ok, <S as Serializer>::Error> {
                let mut serializer = Serializer::serialize_struct(serializer, "", 0usize)?;
                SerializeStruct::end(serializer)
            }
        }
        Struct
    };

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

    let courses = vec![
        "Introduction to Programming",
        "Rust Programming Language",
        "Web Programming",
    ];
    let _o1 = match ("foo".to_owned(), 23, 180, courses) {
        (name, age, height, courses) => {
            use ::core::clone::Clone;
            use ::core::marker::Copy;
            use ::core::pin::Pin;
            use ::core::stringify;

            #[::core::prelude::v1::derive(
                ::core::cmp::PartialEq,
                ::core::cmp::Eq,
                ::core::cmp::PartialOrd,
                ::core::cmp::Ord,
                ::core::hash::Hash,
                Clone,
                Copy
            )]
            struct Struct<T0, T1, T2, T3> {
                name: T0,
                age: T1,
                height: T2,
                courses: T3,
            }

            struct StructProjRef<'a, T0, T1, T2, T3> {
                name: Pin<&'a T0>,
                age: Pin<&'a T1>,
                height: Pin<&'a T2>,
                courses: Pin<&'a T3>,
            }

            struct StructProjMut<'a, T0, T1, T2, T3> {
                name: Pin<&'a mut T0>,
                age: Pin<&'a mut T1>,
                height: Pin<&'a mut T2>,
                courses: Pin<&'a mut T3>,
            }

            impl<T0, T1, T2, T3> Clone for StructProjRef<'_, T0, T1, T2, T3> {
                #[inline]
                fn clone(&self) -> Self {
                    *self
                }
            }

            impl<T0, T1, T2, T3> Copy for StructProjRef<'_, T0, T1, T2, T3> {}

            impl<T0, T1, T2, T3> Struct<T0, T1, T2, T3> {
                fn project_ref(self: Pin<&Self>) -> StructProjRef<'_, T0, T1, T2, T3> {
                    let this = Pin::get_ref(self);
                    unsafe {
                        StructProjRef {
                            name: Pin::new_unchecked(&this.name),
                            age: Pin::new_unchecked(&this.age),
                            height: Pin::new_unchecked(&this.height),
                            courses: Pin::new_unchecked(&this.courses),
                        }
                    }
                }

                fn project_mut(self: Pin<&mut Self>) -> StructProjMut<'_, T0, T1, T2, T3> {
                    unsafe {
                        let this = Pin::get_unchecked_mut(self);
                        StructProjMut {
                            name: Pin::new_unchecked(&mut this.name),
                            age: Pin::new_unchecked(&mut this.age),
                            height: Pin::new_unchecked(&mut this.height),
                            courses: Pin::new_unchecked(&mut this.courses),
                        }
                    }
                }
            }

            use ::core::fmt::Debug;
            impl<T0: Debug, T1: Debug, T2: Debug, T3: Debug> Debug for Struct<T0, T1, T2, T3> {
                fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                    f.debug_struct("")
                        .field(stringify!(name), &self.name)
                        .field(stringify!(age), &self.age)
                        .field(stringify!(height), &self.height)
                        .field(stringify!(courses), &self.courses)
                        .finish()
                }
            }

            use ::core::result::Result;
            use ::serde::{ser::SerializeStruct, Serialize, Serializer};
            impl<T0: Serialize, T1: Serialize, T2: Serialize, T3: Serialize> Serialize
                for Struct<T0, T1, T2, T3>
            {
                fn serialize<S: Serializer>(
                    &self,
                    serializer: S,
                ) -> Result<<S as Serializer>::Ok, <S as Serializer>::Error> {
                    let mut serializer = Serializer::serialize_struct(serializer, "", 4usize)?;
                    SerializeStruct::serialize_field(
                        &mut serializer,
                        stringify!(name),
                        &self.name,
                    )?;
                    SerializeStruct::serialize_field(&mut serializer, stringify!(age), &self.age)?;
                    SerializeStruct::serialize_field(
                        &mut serializer,
                        stringify!(height),
                        &self.height,
                    )?;
                    SerializeStruct::serialize_field(
                        &mut serializer,
                        stringify!(courses),
                        &self.courses,
                    )?;
                    SerializeStruct::end(serializer)
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

            fn into_tuple(self) {}
        }

        use ::core::fmt::Debug;
        impl Debug for Tuple {
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                f.write_str("()")
            }
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

                fn into_tuple(self) -> (T0, T1) {
                    (self.0, self.1)
                }
            }

            use ::core::fmt::Debug;
            impl<T0: Debug, T1: Debug> Debug for Tuple<T0, T1> {
                fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                    f.debug_tuple("").field(&self.0).field(&self.1).finish()
                }
            }

            use ::core::result::Result;
            use ::serde::{ser::SerializeTuple, Serialize, Serializer};
            impl<T0: Serialize, T1: Serialize> Serialize for Tuple<T0, T1> {
                fn serialize<S: Serializer>(
                    &self,
                    serializer: S,
                ) -> Result<<S as Serializer>::Ok, <S as Serializer>::Error> {
                    let mut ser_tuple = Serializer::serialize_tuple(serializer, 2usize)?;
                    SerializeTuple::serialize_element(&mut ser_tuple, &self.0)?;
                    SerializeTuple::serialize_element(&mut ser_tuple, &self.1)?;
                    SerializeTuple::end(ser_tuple)
                }
            }

            Tuple(inputs.0, inputs.1)
        }
    };
}

#[allow(clippy::double_parens)]
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

#[allow(clippy::double_parens)]
fn _join_cyclic_expansion() {
    // Before we move on we should know that our `join_cyclic!` has two efficient expansions for no future and 1 future

    // No future at all
    let _fut0 = anony::join_cyclic!();
    // For no future and 1 future, the expansions are the same as `join!`, and only differ in the struct's name
    let _fut0 = {
        use ::core::future::Future;
        use ::core::pin::Pin;
        use ::core::task::{Context, Poll};

        #[must_use = "unlike other implementations, this one returns a `Future` that should be explicitly `.await`ed or polled"]
        struct JoinCyclic;

        impl Future for JoinCyclic {
            type Output = ();
            fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
                Poll::Ready(())
            }
        }

        JoinCyclic
    };

    // Just one future
    let _fut1 = anony::join_cyclic!(async { 13 });
    let _fut1 = match async { 13 } {
        fut => {
            use ::core::future::Future;
            use ::core::pin::Pin;
            use ::core::task::{Context, Poll};

            #[must_use = "unlike other implementations, this one returns a `Future` that should be explicitly `.await`ed or polled"]
            #[repr(transparent)]
            enum JoinCyclic<F: Future> {
                Inner(F),
            }

            impl<F: Future> Future for JoinCyclic<F> {
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
            JoinCyclic::Inner(IntoFuture::into_future(fut))
        }
    };

    // Base case
    let _fut_n = anony::join_cyclic!(async { 134 }, async { "144" }, std::future::pending::<()>());
    // The only difference is in the base case. Specifically, the polling logic
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
            enum JoinCyclic<F0: Future, F1: Future, F2: Future> {
                Inner(
                    MaybeDone<F0>,
                    MaybeDone<F1>,
                    MaybeDone<F2>,
                    ::core::primitive::u8,
                ),
            }

            impl<F0: Future, F1: Future, F2: Future> Future for JoinCyclic<F0, F1, F2> {
                type Output = (F0::Output, F1::Output, F2::Output);
                fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
                    let Self::Inner(maybe_done0, maybe_done1, maybe_done2, skip_next_time) =
                        unsafe { Pin::get_unchecked_mut(self) };

                    if !unsafe {
                        const COUNT: ::core::primitive::u8 = 3;
                        let mut done = true;
                        let to_skip =
                            ::core::mem::replace(skip_next_time, (*skip_next_time + 1) % COUNT);

                        use ::core::iter::Iterator;
                        Iterator::for_each(
                            Iterator::chain(to_skip..COUNT, 0..to_skip),
                            |i| match i {
                                0 => {
                                    done &=
                                        MaybeDone::poll(Pin::new_unchecked(&mut *maybe_done0), cx)
                                }
                                1 => {
                                    done &=
                                        MaybeDone::poll(Pin::new_unchecked(&mut *maybe_done1), cx)
                                }
                                2 => {
                                    done &=
                                        MaybeDone::poll(Pin::new_unchecked(&mut *maybe_done2), cx)
                                }
                                _ => unsafe { unreachable_unchecked() },
                            },
                        );

                        done
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
                            (None, None, None) => ::core::panic!(
                                "`{}!` future polled after completion",
                                "join_cyclic"
                            ),
                            _ => unreachable_unchecked(),
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

fn _try_join_expansion() {
    // No future at all
    let fut_0 = anony::try_join!();
    expect_output(fut_0);
    let fut_0 = {
        use ::core::future::Future;
        use ::core::ops::ControlFlow;
        use ::core::option::Option::{self, None, Some};
        use ::core::pin::Pin;
        use ::core::result::Result::{self, Err, Ok};
        use ::core::task::{Context, Poll};

        try_trait!();

        use ::core::marker::PhantomData;
        #[must_use = "unlike other implementations, this one returns a `Future` that should be explicitly `.await`ed or polled"]
        struct TryJoin<T: Try<Output = ()>>(PhantomData<T>);

        impl<T: Try<Output = ()>> Future for TryJoin<T> {
            type Output = T;
            fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
                Poll::Ready(Try::from_output(()))
            }
        }

        TryJoin(PhantomData)
    };
    expect_output(fut_0);

    // Just 1 future
    let _fut_1 = anony::try_join!(async { Some(()) });
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
            enum TryJoin<F: Future, R: Residual<(<F::Output as Try>::Output,)>>
            where
                F::Output: Try<Residual = R>,
            {
                Inner(F),
            }

            impl<F: Future, R: Residual<(<F::Output as Try>::Output,)>> Future for TryJoin<F, R>
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
            TryJoin::Inner(IntoFuture::into_future(fut))
        }
    };

    // Base case
    let _fut_3 = anony::try_join!(async { Some(()) }, async { Some(()) }, async { Some(()) });
    let _fut_3 = match (async { Some(()) }, async { Some(()) }, async { Some(()) }) {
        futs => {
            use ::core::future::Future;
            use ::core::ops::ControlFlow;
            use ::core::option::Option::{self, None, Some};
            use ::core::pin::Pin;
            use ::core::primitive::bool;
            use ::core::result::Result::{self, Err, Ok};
            use ::core::task::{Context, Poll};

            try_trait!();

            #[inline(always)]
            unsafe fn unreachable_unchecked() -> ! {
                ::core::unreachable!("`unreachable_unchecked` reached at runtime")
            }

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
            enum TryJoin<
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
                Inner(MaybeDone<F0>, MaybeDone<F1>, MaybeDone<F2>),
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
                > Future for TryJoin<F0, F1, F2, R>
            where
                F0::Output: Try<Residual = R>,
                F1::Output: Try<Residual = R>,
                F2::Output: Try<Residual = R>,
            {
                type Output = R::TryType;
                fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
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
                        match (
                            MaybeDone::force_take_output(Pin::new_unchecked(maybe_done0)),
                            MaybeDone::force_take_output(Pin::new_unchecked(maybe_done1)),
                            MaybeDone::force_take_output(Pin::new_unchecked(maybe_done2)),
                        ) {
                            (Some(o0), Some(o1), Some(o2)) => {
                                Poll::Ready(Try::from_output((o0, o1, o2)))
                            }
                            (None, None, None) => {
                                ::core::panic!("`{}!` future polled after completion", "try_join")
                            }
                            _ => unreachable_unchecked(),
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

#[allow(clippy::blocks_in_conditions)]
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
            use ::core::ops::ControlFlow;
            use ::core::option::Option::{self, None, Some};
            use ::core::pin::Pin;
            use ::core::primitive::bool;
            use ::core::result::Result::{self, Err, Ok};
            use ::core::task::{Context, Poll};

            try_trait!();

            #[inline(always)]
            unsafe fn unreachable_unchecked() -> ! {
                ::core::unreachable!("`unreachable_unchecked` reached at runtime")
            }

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
                        let mut done = true;
                        let to_skip =
                            ::core::mem::replace(skip_next_time, (*skip_next_time + 1) % COUNT);

                        use ::core::iter::Iterator;
                        if let ControlFlow::Break(r) = Iterator::try_for_each(
                            &mut Iterator::chain(to_skip..COUNT, 0..to_skip),
                            |i| {
                                match i {
                                    0 => {
                                        done &= MaybeDone::poll(
                                            Pin::new_unchecked(&mut *maybe_done0),
                                            cx,
                                        )?
                                    }
                                    1 => {
                                        done &= MaybeDone::poll(
                                            Pin::new_unchecked(&mut *maybe_done1),
                                            cx,
                                        )?
                                    }
                                    2 => {
                                        done &= MaybeDone::poll(
                                            Pin::new_unchecked(&mut *maybe_done2),
                                            cx,
                                        )?
                                    }
                                    _ => unsafe { unreachable_unchecked() },
                                }

                                ControlFlow::Continue(())
                            },
                        ) {
                            return Poll::Ready(Try::from_residual(r));
                        }

                        done
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

fn main() {}

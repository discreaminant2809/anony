fn expansion() {
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

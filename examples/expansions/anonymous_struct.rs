fn expansion() {
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

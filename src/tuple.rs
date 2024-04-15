use quote::quote;
use syn::{Expr, Ident, Index};

use crate::{
    pm, pm2,
    utils::{self, TupleLikeTypeInput},
};

pub(crate) fn imp(tt: pm::TokenStream) -> syn::Result<pm2::TokenStream> {
    match syn::parse::<TupleLikeTypeInput>(tt)? {
        TupleLikeTypeInput::Direct { values } => Ok(imp_as_direct(&values)),
        TupleLikeTypeInput::Typing { arity: _ } => Err(syn::Error::new(
            pm2::Span::call_site(),
            "typing is not supported yet",
        )),
    }
}

fn imp_as_direct(values: &[Expr]) -> pm2::TokenStream {
    let generics = utils::i_idents('T', values.len());
    let indices = utils::tuple_indices(values.len());
    let core_part = imp_core_part(&generics, &indices);
    if values.is_empty() {
        quote!({
            #core_part
            Tuple
        })
    } else {
        quote!(
            match (#(#values,)*) { inputs => {
                #core_part
                Tuple(#(inputs.#indices),*)
            }}
        )
    }
}

fn imp_core_part(generics: &[Ident], indices: &[Index]) -> pm2::TokenStream {
    assert_eq!(generics.len(), indices.len());
    let n = generics.len();
    let derive_serde = if cfg!(feature = "serde") {
        quote!(
            // We manually implement the trait since we wanna treat it as a tuple, not a tuple struct
            use ::serde::{Serialize, Serializer, ser::SerializeTuple};
            use ::core::result::Result;
            impl<#(#generics: Serialize),*> Serialize for Tuple<#(#generics),*> {
                fn serialize<S: Serializer>(&self, serializer: S) -> Result<<S as Serializer>::Ok, <S as Serializer>::Error> {
                    let mut ser_tuple = Serializer::serialize_tuple(serializer, #n)?;
                    #(
                        SerializeTuple::serialize_element(&mut ser_tuple, &self.#indices)?;
                    )*
                    SerializeTuple::end(ser_tuple)
                }
            }
        )
    } else {
        quote!()
    };

    // Separate case to avoid warning related to the unit type
    if n == 0 {
        quote!(
            use ::core::pin::Pin;
            use ::core::clone::Clone;
            use ::core::marker::Copy;

            #[::core::prelude::v1::derive(
                ::core::cmp::PartialEq, ::core::cmp::Eq, ::core::cmp::PartialOrd, ::core::cmp::Ord,
                ::core::hash::Hash,
                ::core::default::Default,
                Clone, Copy,
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

            #derive_serde
        )
    } else {
        quote!(
            use ::core::pin::Pin;
            use ::core::clone::Clone;
            use ::core::marker::Copy;

            #[::core::prelude::v1::derive(
                ::core::cmp::PartialEq, ::core::cmp::Eq, ::core::cmp::PartialOrd, ::core::cmp::Ord,
                ::core::hash::Hash,
                ::core::default::Default,
                Clone, Copy,
            )]
            struct Tuple<#(#generics),*>(#(#generics),*);

            impl<#(#generics),*> Tuple<#(#generics),*> {
                fn project_ref(self: Pin<&Self>) -> (#(Pin<&'_ #generics>,)*) {
                    let this = Pin::get_ref(self); // this method is SAFE!
                    // SAFETY: just a classic pinning projection! We guarantee that (see https://doc.rust-lang.org/std/pin/index.html#pinning-is-structural-for-field):
                    // 1. The anonymous struct is only Unpin if all the fields are Unpin (guaranteed by the `auto` impl)
                    // 2. We don't provide a destructor for this type
                    // 3. The same as the 2nd point
                    // 4. We provide no operations leading to data being moved
                    unsafe {
                        (
                            #(
                                Pin::new_unchecked(&this.#indices),
                            )*
                        )
                    }
                }

                fn project_mut(self: Pin<&mut Self>) -> (#(Pin<&'_ mut #generics>,)*) {
                    // SAFETY: see above
                    unsafe {
                        let this = Pin::get_unchecked_mut(self);
                        (
                            #(
                                Pin::new_unchecked(&mut this.#indices),
                            )*
                        )
                    }
                }

                fn into_tuple(self) -> (#(#generics,)*) {
                    (#(self.#indices,)*)
                }
            }

            use ::core::fmt::Debug;
            impl<#(#generics: Debug),*> Debug for Tuple<#(#generics),*> {
                fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                    f.debug_tuple("") // The name is concealed.
                        #(
                            .field(&self.#indices)
                        )*
                        .finish()
                }
            }

            #derive_serde
        )
    }
}

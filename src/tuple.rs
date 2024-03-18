use quote::quote;
use syn::{parse::Parse, Expr, Index, LitInt, Token};

use crate::{pm, pm2, utils};

pub(crate) fn imp(tt: pm::TokenStream) -> syn::Result<pm2::TokenStream> {
    match syn::parse::<Input>(tt)? {
        Input::Direct { values } => Ok(imp_as_direct(&values)),
        Input::Typing { arity } => imp_as_typing(arity),
    }
}

fn imp_as_direct(values: &[Expr]) -> pm2::TokenStream {
    let (core_part, indices) = imp_core_part(values.len());
    if values.is_empty() {
        quote!({
            #core_part

            Tuple
        })
    } else {
        quote!(
            match (#(#values),*,) { inputs => {
                #core_part

                Tuple(#(inputs.#indices),*)
            }}
        )
    }
}

fn imp_as_typing(arity: usize) -> syn::Result<pm2::TokenStream> {
    let _ = arity;
    Err(syn::Error::new(
        pm2::Span::call_site(),
        "typing is not supported yet",
    ))
}

fn imp_core_part(n: usize) -> (pm2::TokenStream, Vec<Index>) {
    let t_generics = utils::t_generics(n);
    let indices = utils::tuple_indices(n);
    let derive_serde = if cfg!(feature = "serde") {
        quote!(
            // We manually implement the trait since we wanna treat it as a tuple, not a tuple struct
            use ::serde::{Serialize, Serializer, ser::SerializeTuple};
            use ::core::result::Result;
            impl<#(#t_generics: Serialize),*> Serialize for Tuple<#(#t_generics),*> {
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

    if n == 0 {
        let part = quote!(
            use ::core::pin::Pin;
            use ::core::clone::Clone;
            use ::core::marker::Copy;

            #[::core::prelude::v1::derive(
                ::core::cmp::PartialEq, ::core::cmp::Eq, ::core::cmp::PartialOrd, ::core::cmp::Ord,
                ::core::hash::Hash,
                Clone, Copy,
            )]
            struct Tuple;

            impl Tuple {
                fn project_ref(self: Pin<&Self>) {}

                fn project_mut(self: Pin<&mut Self>) {}
            }

            use ::core::fmt::Debug;
            impl<#(#t_generics: Debug),*> Debug for Tuple<#(#t_generics),*> {
                fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                    f.write_str("()")
                }
            }

            #derive_serde
        );

        return (part, vec![]);
    }

    let part = quote!(
        use ::core::pin::Pin;
        use ::core::clone::Clone;
        use ::core::marker::Copy;

        #[::core::prelude::v1::derive(
            ::core::cmp::PartialEq, ::core::cmp::Eq, ::core::cmp::PartialOrd, ::core::cmp::Ord,
            ::core::hash::Hash,
            Clone, Copy,
        )]
        struct Tuple<#(#t_generics),*>(#(#t_generics),*);

        impl<#(#t_generics),*> Tuple<#(#t_generics),*> {
            fn project_ref(self: Pin<&Self>) -> (#(Pin<&'_ #t_generics>),*,) {
                let this = Pin::get_ref(self); // this method is SAFE!
                // SAFETY: just a classic pinning projection! We guarantee that (see https://doc.rust-lang.org/std/pin/index.html#pinning-is-structural-for-field):
                // 1. The anonymous struct is only Unpin if all the fields are Unpin (guaranteed by the `auto` impl)
                // 2. We don't provide a destructor for this type
                // 3. The same as the 2nd point
                // 4. We provide no operations leading to data being moved
                unsafe {
                    (
                        #(
                            Pin::new_unchecked(&this.#indices)
                        ),*,
                    )
                }
            }

            fn project_mut(self: Pin<&mut Self>) -> (#(Pin<&'_ mut #t_generics>),*,) {
                // SAFETY: see above
                unsafe {
                    let this = Pin::get_unchecked_mut(self);
                    (
                        #(
                            Pin::new_unchecked(&mut this.#indices)
                        ),*,
                    )
                }
            }
        }

        use ::core::fmt::Debug;
        impl<#(#t_generics: Debug),*> Debug for Tuple<#(#t_generics),*> {
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                f.debug_tuple("") // The name is concealed.
                    #(
                        .field(&self.#indices)
                    )*
                    .finish()
            }
        }

        #derive_serde
    );

    (part, indices)
}

enum Input {
    Direct { values: Vec<Expr> },
    Typing { arity: usize },
}

impl Parse for Input {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        if input.peek(Token![type]) {
            // Parse as `Typing`
            input.parse::<Token![type]>()?;
            input.parse::<Token![,]>()?;
            let arity: LitInt = input.parse()?;

            if !input.is_empty() {
                let lookahead = input.lookahead1();
                if !lookahead.peek(Token![,]) {
                    return Err(lookahead.error());
                }
                input.parse::<Token![,]>()?;
            }

            return Ok(Self::Typing {
                arity: arity.base10_parse()?,
            });
        }

        // Parse as `Direct`
        let values = input.parse_terminated(Expr::parse, Token![,])?;
        Ok(Self::Direct {
            values: values.into_iter().collect(),
        })
    }
}

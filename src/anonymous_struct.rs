use crate::{pm, pm2, utils};
use quote::quote;
use syn::{parse::Parse, Expr, Ident, Token};

struct Input {
    field_names: Vec<Ident>,
    values: Vec<Expr>,
}

struct AnonymousField {
    name: Ident,
    value: Expr,
}

impl Parse for Input {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let (field_names, values) = input
            .parse_terminated(AnonymousField::parse, Token![,])?
            .into_iter()
            .map(|field| (field.name, field.value))
            .unzip();

        Ok(Self {
            field_names,
            values,
        })
    }
}

impl Parse for AnonymousField {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let name: Ident = input.parse()?;
        let value = match input.parse::<Token!(:)>() {
            Ok(_) => input.parse()?,
            Err(_) => Expr::Path(syn::ExprPath {
                attrs: vec![],
                qself: None,
                path: name.clone().into(),
            }),
        };

        Ok(Self { name, value })
    }
}

pub(crate) fn imp(tt: pm::TokenStream) -> syn::Result<pm2::TokenStream> {
    let input = syn::parse::<Input>(tt)?;
    Ok(imp_as_direct(&input.field_names, &input.values))
}

// The code is restructured like this so that we can implement "typing" easily in the future.
fn imp_as_direct(field_names: &[Ident], values: &[Expr]) -> pm2::TokenStream {
    let generics = utils::i_idents("T", field_names.len());

    let core_part = imp_core_part(&generics, field_names);
    if field_names.is_empty() {
        quote!({
            #core_part
            Struct
        })
    } else {
        quote!(
            match (#(#values),*,) { (#(#field_names),*,) => {
                #core_part
                Struct {
                    #(#field_names,)*
                }
            }}
        )
    }
}

fn imp_core_part(generics: &[Ident], field_names: &[Ident]) -> pm2::TokenStream {
    let n = generics.len();
    let derive_serde = if cfg!(feature = "serde") {
        quote!(
            use ::serde::{Serialize, Serializer, ser::SerializeStruct};
            use ::core::result::Result;
            impl<#(#generics: Serialize),*> Serialize for Struct<#(#generics),*> {
                fn serialize<S: Serializer>(&self, serializer: S) -> Result<<S as Serializer>::Ok, <S as Serializer>::Error> {
                    let mut serializer = Serializer::serialize_struct(serializer, "", #n)?;
                    #(
                        SerializeStruct::serialize_field(&mut serializer, stringify!(#field_names), &self.#field_names)?;
                    )*
                    SerializeStruct::end(serializer)
                }
            }
        )
    } else {
        quote!()
    };

    // Separate case to avoid warning related to the unit type
    if n == 0 {
        return quote!(
            use ::core::pin::Pin;
            use ::core::clone::Clone;
            use ::core::marker::Copy;
            use ::core::marker::PhantomData;
            use ::core::stringify;

            #[::core::prelude::v1::derive(
                ::core::cmp::PartialEq, ::core::cmp::Eq, ::core::cmp::PartialOrd, ::core::cmp::Ord,
                ::core::hash::Hash,
                ::core::default::Default,
                Clone, Copy,
            )]
            struct Struct;

            struct StructProjRef<'a>(PhantomData<Pin<&'a Struct>>);

            struct StructProjMut<'a>(PhantomData<Pin<&'a mut Struct>>);

            impl<#(#generics),*> Clone for StructProjRef<'_, #(#generics),*> {
                #[inline]
                fn clone(&self) -> Self {
                    *self
                }
            }

            impl<#(#generics),*> Copy for StructProjRef<'_, #(#generics),*> {}

            impl<#(#generics),*> Struct<#(#generics),*> {
                fn project_ref(self: Pin<&Self>) -> StructProjRef<'_, #(#generics),*> {
                    StructProjRef(PhantomData)
                }

                fn project_mut(self: Pin<&mut Self>) -> StructProjMut<'_, #(#generics),*> {
                    StructProjMut(PhantomData)
                }
            }

            use ::core::fmt::Debug;
            impl<#(#generics: Debug),*> Debug for Struct<#(#generics),*> {
                fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                    ::core::fmt::Formatter::write_str(f, "{}")
                }
            }

            #derive_serde
        );
    }

    quote!(
        use ::core::pin::Pin;
        use ::core::clone::Clone;
        use ::core::marker::Copy;
        use ::core::stringify;

        #[::core::prelude::v1::derive(
            ::core::cmp::PartialEq, ::core::cmp::Eq, ::core::cmp::PartialOrd, ::core::cmp::Ord,
            ::core::hash::Hash,
            ::core::default::Default,
            Clone, Copy,
        )]
        struct Struct<#(#generics),*> {
            #(
                #field_names: #generics,
            )*
        }

        struct StructProjRef<'a, #(#generics),*> {
            #(
                #field_names: Pin<&'a #generics>,
            )*
        }

        struct StructProjMut<'a, #(#generics),*> {
            #(
                #field_names: Pin<&'a mut #generics>,
            )*
        }

        impl<#(#generics),*> Clone for StructProjRef<'_, #(#generics),*> {
            #[inline]
            fn clone(&self) -> Self {
                *self
            }
        }

        impl<#(#generics),*> Copy for StructProjRef<'_, #(#generics),*> {}

        impl<#(#generics),*> Struct<#(#generics),*> {
            fn project_ref(self: Pin<&Self>) -> StructProjRef<'_, #(#generics),*> {
                let this = Pin::get_ref(self); // this method is SAFE!

                // SAFETY: just a classic pinning projection! We guarantee that (see https://doc.rust-lang.org/std/pin/index.html#pinning-is-structural-for-field):
                // 1. The anonymous struct is only Unpin if all the fields are Unpin (guaranteed by the `auto` impl)
                // 2. We don't provide a destructor for this type
                // 3. The same as the 2nd point
                // 4. We provide no operations leading to data being moved
                unsafe {
                    StructProjRef {
                        #(
                            #field_names: Pin::new_unchecked(&this.#field_names),
                        )*
                    }
                }
            }

            fn project_mut(self: Pin<&mut Self>) -> StructProjMut<'_, #(#generics),*> {
                // SAFETY: see above
                unsafe {
                    let this = Pin::get_unchecked_mut(self);
                    StructProjMut {
                        #(
                            #field_names: Pin::new_unchecked(&mut this.#field_names),
                        )*
                    }
                }
            }
        }

        use ::core::fmt::Debug;
        impl<#(#generics: Debug),*> Debug for Struct<#(#generics),*> {
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                f.debug_struct("") // The name is concealed, but there is a single space before the opening curly bracket
                    #(
                        .field(stringify!(#field_names), &self.#field_names)
                    )*
                    .finish()
            }
        }

        #derive_serde
    )
}

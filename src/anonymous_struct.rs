use crate::{pm, pm2};
use quote::{format_ident, quote};
use syn::{parse::Parse, Expr, Ident, Token};

struct Input {
    anonymous_fields: Vec<AnonymousField>,
}

struct AnonymousField {
    name: Ident,
    value: Option<Expr>,
}

impl Parse for Input {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        std::iter::from_fn(|| {
            let ret = (!input.is_empty()).then(|| input.parse::<AnonymousField>());
            if let Err(e) = input.parse::<Token!(,)>() {
                if !input.is_empty() {
                    return Some(Err(e));
                }
            }

            ret
        })
        .collect::<syn::Result<Vec<_>>>()
        .map(|anonymous_fields| Self { anonymous_fields })
    }
}

impl Parse for AnonymousField {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let name: Ident = input.parse()?;
        let value = match input.parse::<Token!(:)>() {
            Ok(_) => Some(input.parse()?),
            Err(_) => None,
        };

        Ok(Self { name, value })
    }
}

pub(crate) fn imp(tt: pm::TokenStream) -> syn::Result<pm2::TokenStream> {
    let input = syn::parse::<Input>(tt)?;
    let anonymous_fields = input.anonymous_fields;

    #[cfg(feature = "serde")]
    let derive_serde = quote!(#[::core::prelude::v1::derive(::serde::Serialize)]);
    #[cfg(not(feature = "serde"))]
    let derive_serde = quote!();

    // Handle this case since `()` matching causes `#[warn(clippy::let_unit_value)]`
    if anonymous_fields.is_empty() {
        return Ok(quote!({
            #[::core::prelude::v1::derive(
                ::core::cmp::PartialEq, ::core::cmp::Eq, ::core::cmp::PartialOrd, ::core::cmp::Ord,
                ::core::hash::Hash,
                ::core::clone::Clone, ::core::marker::Copy,
            )]
            #derive_serde
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
        }));
    }

    // `anon` crate uses the fields' names for fields' type directly, which `#[forbid]` will kill it
    // and uppercasing them is not really efficient so we use `T[n]` instead
    fn t_i(i: usize) -> Ident {
        format_ident!("T{i}")
    }

    // base iterators of everything here! Most of the below just clone from the two here!
    let generics = (0..anonymous_fields.len()).map(t_i);
    let names = anonymous_fields.iter().map(|field| &field.name);

    let anony_proj_generics = generics.clone();
    let anony_proj_field_decls = names.clone().enumerate().map(|(i, name)| {
        let ty = t_i(i);
        quote!(#name: ::core::pin::Pin<&'a mut #ty>)
    });
    let anony_proj_ref_generics = generics.clone();
    let anony_proj_ref_field_decls = names.clone().enumerate().map(|(i, name)| {
        let ty = t_i(i);
        quote!(#name: ::core::pin::Pin<&'a #ty>)
    });

    let impl_generics_left = generics.clone();
    let impl_generics_right = generics.clone();

    let anony_proj_ret_generics = generics.clone();
    let anony_proj_name_inits = names.clone();
    let anony_proj_ref_ret_generics = generics.clone();
    let anony_proj_ref_name_inits = names.clone();
    let anony_proj_ref_clone_generics_left = generics.clone();
    let anony_proj_ref_clone_generics_right = generics.clone();
    let anony_proj_ref_copy_generics_left = generics.clone();
    let anony_proj_ref_copy_generics_right = generics.clone();

    let field_decls = names.clone().enumerate().map(|(i, name)| {
        let ty = t_i(i);
        quote!(#name: #ty)
    });

    let names_let = anonymous_fields
        .iter()
        .filter(|field| field.value.is_some())
        .map(|field| &field.name);
    let name_inits = names.clone();
    let exprs = anonymous_fields
        .iter()
        .filter_map(|field| field.value.as_ref());

    let debug_generics_left = generics.clone();
    let debug_generics_right = generics.clone();
    let debug_idents = names.clone();

    Ok(quote!({
        let (#(#names_let),*) = (#(#exprs),*);

        // Open another scope so that the captured values can't access the struct
        {
            // deriving `Default` is useless, since we can't get the type to call the method on
            #[::core::prelude::v1::derive(
                ::core::cmp::PartialEq, ::core::cmp::Eq, ::core::cmp::PartialOrd, ::core::cmp::Ord,
                ::core::hash::Hash,
                ::core::clone::Clone, ::core::marker::Copy,
            )]
            #derive_serde
            struct Struct<#(#generics),*> {
                #(
                    #field_decls
                ),*
            }

            struct StructProjMut<'a, #(#anony_proj_generics),*> {
                #(
                    #anony_proj_field_decls
                ),*
            }

            struct StructProjRef<'a, #(#anony_proj_ref_generics),*> {
                #(
                    #anony_proj_ref_field_decls
                ),*
            }

            impl<#(#anony_proj_ref_clone_generics_left),*> ::core::clone::Clone for StructProjRef<'_, #(#anony_proj_ref_clone_generics_right),*> {
                #[inline]
                fn clone(&self) -> Self {
                    *self
                }
            }

            impl<#(#anony_proj_ref_copy_generics_left),*> ::core::marker::Copy for StructProjRef<'_, #(#anony_proj_ref_copy_generics_right),*> {}

            impl<#(#impl_generics_left),*> Struct<#(#impl_generics_right),*> {
                fn project_mut(self: ::core::pin::Pin<&mut Self>) -> StructProjMut<'_, #(#anony_proj_ret_generics),*> {
                    // SAFETY: just a classic pinning projection! We guarantee that (see https://doc.rust-lang.org/std/pin/index.html#pinning-is-structural-for-field):
                    // 1. The anonymous struct is only Unpin if all the fields are Unpin (guaranteed by the `auto` impl)
                    // 2. We don't provide a destructor for this type
                    // 3. The same as the 2nd point
                    // 4. We provide no operations leading to data being moved
                    unsafe {
                        let this = self.get_unchecked_mut();
                        StructProjMut {
                            #(
                                #anony_proj_name_inits: ::core::pin::Pin::new_unchecked(&mut this.#anony_proj_name_inits)
                            ),*
                        }
                    }
                }

                fn project_ref(self: ::core::pin::Pin<&Self>) -> StructProjRef<'_, #(#anony_proj_ref_ret_generics),*> {
                    let this = self.get_ref(); // this method is SAFE!

                    // SAFETY: see the `project_mut` method
                    unsafe {
                        StructProjRef {
                            #(
                                #anony_proj_ref_name_inits: ::core::pin::Pin::new_unchecked(&this.#anony_proj_ref_name_inits)
                            ),*
                        }
                    }
                }
            }

            impl<#(#debug_generics_left: ::core::fmt::Debug),*> ::core::fmt::Debug for Struct<#(#debug_generics_right),*> {
                fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                    f.debug_struct("") // The name is concealed, but there is a single space before the opening curly bracket
                        #(
                            .field(::core::stringify!(#debug_idents), &self.#debug_idents)
                        )*
                        .finish()
                }
            }

            Struct {
                #(#name_inits),*
            }
        }
    }))
}

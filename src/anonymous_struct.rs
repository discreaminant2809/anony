use crate::{pm, pm2};
use itertools::Itertools;
use quote::{format_ident, quote};
use syn::{parse::Parse, Expr, ExprPath, Ident, Token};

struct Input {
    anonymous_fields: Vec<AnonymousField>,
}

struct AnonymousField {
    name: Ident,
    value: Expr,
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
            Ok(_) => input.parse()?,
            Err(_) => Expr::Path(ExprPath {
                attrs: Default::default(),
                qself: Default::default(),
                path: name.clone().into(),
            }),
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
            struct Anony;

            impl ::core::fmt::Debug for Anony {
                fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                    ::core::fmt::Formatter::write_str(f, "{}")
                }
            }

            Anony
        }));
    }

    // `anon` crate uses the fields' names for fields' type directly, which `#[forbid]` will kill it
    // and uppercasing them is not really efficient so we use `T[n]` instead
    fn t_i(i: usize) -> Ident {
        format_ident!("T{i}")
    }
    let generics = (0..anonymous_fields.len()).map(t_i);
    let field_decls =
        anonymous_fields
            .iter()
            .enumerate()
            .map(|(i, AnonymousField { name, .. })| {
                let ty = t_i(i);
                quote!(#name: #ty)
            });
    let names_let = anonymous_fields.iter().map(|field| &field.name);
    let name_inits = names_let.clone();
    let exprs = anonymous_fields.iter().map(|field| &field.value);

    let generics_impl_left = generics.clone();
    let generics_impl_right = generics.clone();
    let generics_into_inner = generics.clone();
    let ret_idents = names_let.clone();

    let debug_generics_left = generics.clone();
    let debug_generics_right = generics.clone();
    let debug_each_field = names_let.clone().map(|debug_ident| {
        quote!(
            ::core::fmt::Formatter::write_str(f, ::core::stringify!(#debug_ident))?;
            ::core::fmt::Formatter::write_str(f, ": ")?;
            ::core::fmt::Debug::fmt(&self.#debug_ident, f)?;
        )
    });
    let debug_no_alt = Itertools::intersperse_with(
        debug_each_field.clone(),
        || quote!(::core::fmt::Formatter::write_str(f, ", ")?;),
    );
    let debug_alt = Itertools::intersperse_with(
        debug_each_field.clone(),
        || quote!(::core::fmt::Formatter::write_str(f, ",\n    ")?;),
    );

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
            struct Anony<#(#generics),*> {
                #(
                    #field_decls
                ),*
            }

            #[automatically_derived]
            impl<#(#generics_impl_left),*> Anony<#(#generics_impl_right),*> {
                fn into_inner(self) -> (#(#generics_into_inner),*) {
                    (#(self.#ret_idents),*)
                }
            }

            #[automatically_derived]
            impl<#(#debug_generics_left: ::core::fmt::Debug),*> ::core::fmt::Debug for Anony<#(#debug_generics_right),*> {
                fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                    if ::core::fmt::Formatter::alternate(f) {
                        ::core::fmt::Formatter::write_str(f, "{\n    ")?;
                        #(#debug_alt)*
                        ::core::fmt::Formatter::write_str(f, ",\n}")
                    } else {
                        ::core::fmt::Formatter::write_str(f, "{ ")?;
                        #(#debug_no_alt)*
                        ::core::fmt::Formatter::write_str(f, " }")
                    }
                }
            }

            Anony {
                #(#name_inits),*
            }
        }
    }))
}

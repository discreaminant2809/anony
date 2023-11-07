use crate::{pm, pm2};
use quote::quote;
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
        let anonymous_fields = std::iter::from_fn(|| {
            let ret = (!input.is_empty()).then(|| input.parse::<AnonymousField>());
            if let Err(e) = input.parse::<Token!(,)>() {
                if !input.is_empty() {
                    return Some(Err(e));
                }
            }

            ret
        })
        .collect::<syn::Result<Vec<_>>>()?;

        Ok(Input { anonymous_fields })
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

    fn t_i(i: usize) -> Ident {
        Ident::new(&format!("T{i}"), pm2::Span::call_site())
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
    let destruct_idents = names_let.clone();
    let ret_idents = names_let.clone();

    #[cfg(feature = "serde")]
    let derive_serde = quote!(#[::core::prelude::v1::derive(::serde::Serialize)]);
    #[cfg(not(feature = "serde"))]
    let derive_serde = pm2::TokenStream::new();

    Ok(quote!({
        let (#(#names_let),*) = (#(#exprs),*);

        {
            // deriving `Default` is useless, since we can't get the type to call the method on
            #[::core::prelude::v1::derive(
                ::core::cmp::PartialEq, ::core::cmp::Eq, ::core::cmp::PartialOrd, ::core::cmp::Ord,
                ::core::fmt::Debug,
                ::core::hash::Hash,
                ::core::clone::Clone, ::core::marker::Copy,
            )]
            #derive_serde
            struct Anonymous<#(#generics),*> {
                #(
                    #field_decls
                ),*
            }

            impl<#(#generics_impl_left),*> Anonymous<#(#generics_impl_right),*> {
                fn into_inner(self) -> (#(#generics_into_inner),*) {
                    let Anonymous { #(#destruct_idents),* } = self;
                    (#(#ret_idents),*)
                }
            }

            Anonymous {
                #(
                    #name_inits
                ),*
            }
        }
    }))
}

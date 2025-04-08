use derive_quote_to_tokens::ToTokens;
use syn::{Token, parse::Parse};

#[derive(ToTokens)]
pub enum CfToken {
    Break(Token![break]),
    Continue(Token![continue]),
}

impl CfToken {
    pub fn is_break(&self) -> bool {
        matches!(self, Self::Break(_))
    }

    #[allow(dead_code)]
    pub fn is_continue(&self) -> bool {
        matches!(self, Self::Continue(_))
    }
}

impl Parse for CfToken {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        if input.peek(Token![break]) {
            input.parse::<Token![break]>().map(Self::Break)
        } else if input.peek(Token![continue]) {
            input.parse::<Token![continue]>().map(Self::Continue)
        } else {
            Err(input.error("expected `break` or `continue`"))
        }
    }
}

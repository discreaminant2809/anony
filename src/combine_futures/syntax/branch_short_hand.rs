use derive_quote_to_tokens::ToTokens;
use syn::{Expr, Token, parse::Parse};

use super::{CfToken, parse_expr_followed_by_comma};

#[derive(ToTokens)]
pub struct BranchShortHand {
    pub control_flow: CfToken,
    pub fut_expr: Expr,
    pub comma: Option<Token![,]>,
}

impl BranchShortHand {
    pub fn pure_break(&self) -> bool {
        matches!(self.control_flow, CfToken::Break(_))
    }
}

impl Parse for BranchShortHand {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        // We can't omit the expression since it's this branch's future expression,
        // so no `parse_cf_directive_followed_by_comma`.
        let control_flow = input.parse()?;
        let (fut_expr, comma) = parse_expr_followed_by_comma(input)?;

        Ok(Self {
            control_flow,
            fut_expr,
            comma,
        })
    }
}

use derive_quote_to_tokens::ToTokens;
use syn::{Expr, Pat, Token, parse::Parse};

use super::{CfToken, parse_cf_directive_followed_by_comma};

#[derive(ToTokens)]
pub struct BranchLet {
    pub let_token: Token![let],
    pub pat: Pat,
    pub eq_token: Token![=],
    pub fut_expr: Expr,
    pub else_arm: Option<BranchLetElseArm>,
    pub fat_arrow_token: Token![=>],
    pub control_flow: CfToken,
    pub body: Option<Expr>,
    pub comma: Option<Token![,]>,
}

#[derive(ToTokens)]
pub struct BranchLetElseArm {
    pub else_token: Token![else],
    pub fat_arrow_token: Token![=>],
    pub control_flow: CfToken,
    pub body: Option<Expr>,
}

impl BranchLet {
    pub fn pure_break(&self) -> bool {
        !matches!(
            self.else_arm,
            Some(BranchLetElseArm {
                control_flow: CfToken::Continue(_),
                ..
            })
        ) && matches!(self.control_flow, CfToken::Break(_))
    }
}

impl Parse for BranchLet {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        // See https://docs.rs/syn/latest/src/syn/expr.rs.html#2193-2214

        let let_token = input.parse()?;
        let pat = Pat::parse_multi_with_leading_vert(input)?;
        let eq_token = input.parse()?;
        let fut_expr = input.parse()?;
        let else_arm = input
            .parse::<Token![else]>()
            .ok()
            .map(|else_token| -> syn::Result<_> {
                Ok(BranchLetElseArm {
                    else_token,
                    fat_arrow_token: input.parse()?,
                    control_flow: input.parse()?,
                    body: (!input.peek(Token![=>]))
                        .then(|| input.parse())
                        .transpose()?,
                })
            })
            .transpose()?;
        let fat_arrow_token = input.parse()?;
        let (control_flow, body, comma) = parse_cf_directive_followed_by_comma(input)?;

        Ok(Self {
            let_token,
            pat,
            eq_token,
            fut_expr,
            else_arm,
            fat_arrow_token,
            control_flow,
            body,
            comma,
        })
    }
}

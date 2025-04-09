use derive_quote_to_tokens::ToTokens;
use syn::{Expr, Pat, Token, parse::Parse, token::Brace};

use super::{CfToken, parse_cf_directive_followed_by_comma};

pub struct BranchMatch {
    pub match_token: Token![match],
    pub fut_expr: Expr,
    pub brace_token: Brace,
    pub arms: Vec<BranchMatchArm>,
}

#[derive(ToTokens)]
pub struct BranchMatchArm {
    pub pat: Pat,
    pub guard: Option<BranchMatchArmGuard>,
    pub fat_arrow_token: Token![=>],
    pub control_flow: CfToken,
    pub body: Option<Expr>,
    pub comma: Option<Token![,]>,
}

#[derive(ToTokens)]
pub struct BranchMatchArmGuard {
    pub if_token: Token![if],
    pub expr: Expr,
}

impl BranchMatch {
    pub fn pure_break(&self) -> bool {
        self.arms.iter().all(BranchMatchArm::pure_break)
    }
}

impl BranchMatchArm {
    fn pure_break(&self) -> bool {
        matches!(self.control_flow, CfToken::Break(_))
    }
}

impl Parse for BranchMatch {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        // See https://docs.rs/syn/2.0.100/src/syn/expr.rs.html#2333-2355

        let match_token: Token![match] = input.parse()?;
        let fut_expr = Expr::parse_without_eager_brace(input)?;

        let arms_content;
        let brace_token = syn::braced!(arms_content in input);
        let arms = std::iter::from_fn(|| {
            (!arms_content.is_empty()).then(|| arms_content.parse::<BranchMatchArm>())
        })
        .collect::<Result<Vec<_>, _>>()?;

        Ok(Self {
            match_token,
            fut_expr,
            brace_token,
            arms,
        })
    }
}

impl quote::ToTokens for BranchMatch {
    fn to_tokens(&self, tokens: &mut crate::pm2::TokenStream) {
        self.match_token.to_tokens(tokens);
        self.fut_expr.to_tokens(tokens);
        self.brace_token.surround(tokens, |tokens| {
            for arm in &self.arms {
                arm.to_tokens(tokens);
            }
        });
    }
}

impl Parse for BranchMatchArm {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let pat = Pat::parse_multi_with_leading_vert(input)?;
        let guard = input.peek(Token![if]).then(|| input.parse()).transpose()?;
        let fat_arrow_token = input.parse()?;
        let (control_flow, body, comma) = parse_cf_directive_followed_by_comma(input)?;

        Ok(Self {
            pat,
            guard,
            fat_arrow_token,
            control_flow,
            body,
            comma,
        })
    }
}

impl Parse for BranchMatchArmGuard {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let if_token = input.parse()?;
        let expr = input.parse()?;

        Ok(Self { if_token, expr })
    }
}

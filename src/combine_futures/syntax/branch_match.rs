use syn::{Expr, Pat, Token, parse::Parse, token::Brace};

use super::{ControlFLowToken, parse_cf_directive_followed_by_comma};

pub struct BranchMatch {
    pub match_token: Token![match],
    pub fut_expr: Expr,
    pub brace_token: Brace,
    pub arms: Vec<MatchArm>,
}

pub struct MatchArm {
    pub pat: Pat,
    pub guard: Option<(Token![if], Expr)>,
    pub fat_arrow_token: Token![=>],
    pub control_flow: ControlFLowToken,
    pub body: Option<Expr>,
    pub comma: Option<Token![,]>,
}

impl Parse for BranchMatch {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        // See https://docs.rs/syn/2.0.100/src/syn/expr.rs.html#2333-2355

        let match_token: Token![match] = input.parse()?;
        let fut_expr = Expr::parse_without_eager_brace(input)?;

        let content;
        let brace_token = syn::braced!(content in input);
        let arms =
            std::iter::from_fn(|| (!content.is_empty()).then(|| content.parse::<MatchArm>()))
                .collect::<Result<Vec<_>, _>>()?;

        Ok(Self {
            match_token,
            fut_expr,
            brace_token,
            arms,
        })
    }
}

impl Parse for MatchArm {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let pat = Pat::parse_multi_with_leading_vert(input)?;
        let guard = input
            .peek(Token![if])
            .then(|| -> syn::Result<_> { Ok((input.parse()?, input.parse()?)) })
            .transpose()?;
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

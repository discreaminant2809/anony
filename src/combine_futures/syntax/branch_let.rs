use std::ops::ControlFlow;

use syn::{Expr, Pat, Token, parse::Parse};

use super::{
    ControlFLowToken, parse_cf_directive_followed_by_comma, parse_control_flow,
    parse_expr_followed_by_comma,
};

pub struct BranchLet {
    let_token: Token![let],
    pat: Pat,
    eq_token: Token![=],
    fut_expr: Expr,
    else_arm: Option<(Token![else], Token![=>], ControlFLowToken, Expr)>,
    fat_arrow_token: Token![=>],
    control_flow: ControlFLowToken,
    body: Option<Expr>,
    comma: Option<Token![,]>,
}

impl BranchLet {
    pub fn always_breaks(&self) -> bool {
        !matches!(self.else_arm, Some((_, _, ControlFlow::Continue(_), _)))
            && matches!(self.control_flow, ControlFlow::Break(_))
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
                Ok((
                    else_token,
                    input.parse()?,
                    parse_control_flow(input)?,
                    input.parse()?,
                ))
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

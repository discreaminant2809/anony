use std::ops::ControlFlow;

use syn::{Expr, Token, parse::Parse};

use super::{ControlFLowToken, parse_control_flow, parse_expr_followed_by_comma, requires_comma};

pub struct BranchShortHand {
    pub control_flow: ControlFLowToken,
    pub fut_expr: Expr,
    pub comma: Option<Token![,]>,
}

impl BranchShortHand {
    pub fn always_breaks(&self) -> bool {
        matches!(self.control_flow, ControlFlow::Break(_))
    }
}

impl Parse for BranchShortHand {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        // We can't omit the expression since it's this branch's future expression,
        // so no `parse_cf_directive_followed_by_comma`.
        let control_flow = parse_control_flow(input)?;
        let (fut_expr, comma) = parse_expr_followed_by_comma(input)?;

        Ok(Self {
            control_flow,
            fut_expr,
            comma,
        })
    }
}

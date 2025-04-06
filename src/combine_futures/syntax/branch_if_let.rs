use std::ops::ControlFlow;

use syn::{Block, Expr, Pat, Token, parse::Parse};

use super::{ControlFLowToken, parse_control_flow};

pub struct BranchIfLet {
    pub if_token: Token![if],

    pub let_token: Token![let],
    pub pat: Pat,
    pub eq_token: Token![=],
    pub fut_expr: Expr,

    pub fat_arrow_token: Token![=>],
    pub control_flow: ControlFLowToken,
    pub then_arm: Block,

    // It's required for exhaustiveness, so that:
    // - we can shift the error of check for `always break` property later. (more user-friendly)
    // - the user has to specify whether to break or continue, even if they intend to return a `()`.
    pub else_arm: ElseArm,
}

pub struct ElseArm {
    pub else_token: Token![else],
    pub arm_continuation: ControlFlow<(Token![=>], ControlFLowToken, Block), Box<IfArm>>,
}

pub struct IfArm {
    pub if_token: Token![if],
    pub cond: Expr,

    pub fat_arrow_token: Token![=>],
    pub control_flow: ControlFLowToken,
    pub then_arm: Expr,

    pub else_arm: ElseArm,
}

impl BranchIfLet {
    /// Recursively determine the "always break" property across arms.
    /// `false` if any arm is `continue`.
    pub fn always_breaks(&self) -> bool {
        matches!(self.control_flow, ControlFlow::Break(_)) && self.else_arm.always_breaks()
    }
}

impl ElseArm {
    fn always_breaks(&self) -> bool {
        match self.arm_continuation {
            // We reach the last `else` branch. No more to go further
            ControlFlow::Break((_, controlflow, _)) => matches!(controlflow, ControlFlow::Break(_)),
            ControlFlow::Continue(ref if_arm) => if_arm.always_breaks(),
        }
    }
}

impl IfArm {
    fn always_breaks(&self) -> bool {
        matches!(self.control_flow, ControlFlow::Break(_)) && self.else_arm.always_breaks()
    }
}

impl Parse for BranchIfLet {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let if_token = input.parse()?;
        let let_token = input.parse()?;
        let pat = Pat::parse_multi_with_leading_vert(input)?;
        let eq_token = input.parse()?;
        let fut_expr = input.parse()?;
        let fat_arrow_token = input.parse()?;
        let control_flow = parse_control_flow(input)?;
        let then_arm = input.parse()?;
        let else_arm = input.parse()?;

        Ok(Self {
            if_token,
            let_token,
            pat,
            eq_token,
            fut_expr,
            fat_arrow_token,
            control_flow,
            then_arm,
            else_arm,
        })
    }
}

impl Parse for ElseArm {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let else_token = input.parse()?;
        let arm_continuation = if input.peek(Token![=>]) {
            ControlFlow::Break((input.parse()?, parse_control_flow(input)?, input.parse()?))
        } else if input.peek(Token![if]) {
            ControlFlow::Continue(Box::new(input.parse()?))
        } else {
            return Err(input.error("expected `=>` or `if`"));
        };

        Ok(Self {
            else_token,
            arm_continuation,
        })
    }
}

impl Parse for IfArm {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let if_token = input.parse()?;
        let cond = input.parse()?;
        let fat_arrow_token = input.parse()?;
        let control_flow = parse_control_flow(input)?;
        let then_arm = input.parse()?;
        let else_arm = input.parse()?;

        Ok(Self {
            if_token,
            cond,
            fat_arrow_token,
            control_flow,
            then_arm,
            else_arm,
        })
    }
}

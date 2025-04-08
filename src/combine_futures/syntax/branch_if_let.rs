use derive_quote_to_tokens::ToTokens;
use syn::{Block, Expr, Pat, Token, parse::Parse};

use super::CfToken;

#[derive(ToTokens)]
pub struct BranchIfLet {
    pub if_token: Token![if],

    pub let_token: Token![let],
    pub pat: Pat,
    pub eq_token: Token![=],
    pub fut_expr: Expr,

    pub fat_arrow_token: Token![=>],
    pub control_flow: CfToken,
    pub then_arm: Block,

    // It's required for exhaustiveness, so that:
    // - we can shift the error of check for `always break` property later. (more user-friendly)
    // - the user has to specify whether to break or continue, even if they intend to return a `()`.
    pub else_arm: BranchIfLetElseArm,
}

#[derive(ToTokens)]
pub struct BranchIfLetElseArm {
    pub else_token: Token![else],
    pub direction: BranchIfLetElseArmDirection,
}

#[derive(ToTokens)]
pub enum BranchIfLetElseArmDirection {
    End {
        fat_arrow_token: Token![=>],
        control_flow: CfToken,
        body: Block,
    },
    ElseIf(Box<BranchIfLetIfArm>),
}

#[derive(ToTokens)]
pub struct BranchIfLetIfArm {
    pub if_token: Token![if],
    pub cond: Expr,

    pub fat_arrow_token: Token![=>],
    pub control_flow: CfToken,
    pub then_arm: Block,

    pub else_arm: BranchIfLetElseArm,
}

impl BranchIfLet {
    /// Recursively determine the "always break" property across arms.
    /// `false` if any arm is `continue`.
    pub fn always_breaks(&self) -> bool {
        matches!(self.control_flow, CfToken::Break(_)) && self.else_arm.always_breaks()
    }
}

impl BranchIfLetElseArm {
    fn always_breaks(&self) -> bool {
        match &self.direction {
            // We reach the last `else` branch. No more to go further
            BranchIfLetElseArmDirection::End { control_flow, .. } => {
                matches!(control_flow, CfToken::Break(_))
            }
            BranchIfLetElseArmDirection::ElseIf(if_arm) => if_arm.always_breaks(),
        }
    }
}

impl BranchIfLetIfArm {
    fn always_breaks(&self) -> bool {
        matches!(self.control_flow, CfToken::Break(_)) && self.else_arm.always_breaks()
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
        let control_flow = input.parse()?;
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

impl Parse for BranchIfLetElseArm {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let else_token = input.parse()?;
        let direction = if input.peek(Token![=>]) {
            BranchIfLetElseArmDirection::End {
                fat_arrow_token: input.parse()?,
                control_flow: input.parse()?,
                body: input.parse()?,
            }
        } else if input.peek(Token![if]) {
            BranchIfLetElseArmDirection::ElseIf(Box::new(input.parse()?))
        } else {
            return Err(input.error("expected `=>` or `if`"));
        };

        Ok(Self {
            else_token,
            direction,
        })
    }
}

impl Parse for BranchIfLetIfArm {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let if_token = input.parse()?;
        let cond = input.parse()?;
        let fat_arrow_token = input.parse()?;
        let control_flow = input.parse()?;
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

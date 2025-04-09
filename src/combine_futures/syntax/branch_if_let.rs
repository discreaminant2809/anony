use derive_quote_to_tokens::ToTokens;
use syn::{Block, Expr, Token, parse::Parse};

use super::CfToken;

#[derive(ToTokens)]
pub struct BranchIfLet {
    pub if_arm: BranchIfLetIfArm,
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

    // It's required for exhaustiveness anyway. It's so we don't need to `unwrap` in the code.
    pub else_arm: BranchIfLetElseArm,
}

impl BranchIfLet {
    /// Recursively determine the "always break" property across arms.
    /// `false` if any arm is `continue`.
    pub fn pure_break(&self) -> bool {
        self.if_arm.pure_break()
    }
}

impl BranchIfLetElseArm {
    fn pure_break(&self) -> bool {
        match &self.direction {
            // We reach the last `else` branch. No more to go further
            BranchIfLetElseArmDirection::End { control_flow, .. } => control_flow.is_break(),
            BranchIfLetElseArmDirection::ElseIf(if_arm) => if_arm.pure_break(),
        }
    }
}

impl BranchIfLetIfArm {
    fn pure_break(&self) -> bool {
        self.control_flow.is_break() && self.else_arm.pure_break()
    }
}

impl Parse for BranchIfLet {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(Self {
            if_arm: input.parse()?,
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

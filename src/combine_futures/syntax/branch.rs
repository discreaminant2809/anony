use derive_quote_to_tokens::ToTokens;
use syn::{Expr, ExprLet, Token, parse::Parse};

use super::{BranchIfLet, BranchIfLetIfArm, BranchLet, BranchMatch, BranchShortHand};

/// Branch containing a future that will be run concurrently.
#[derive(ToTokens)]
pub enum Branch {
    /// `let`-like branch.
    Let(BranchLet),
    /// `if let`-like branch.
    IfLet(BranchIfLet),
    /// `match`-like branch.
    Match(BranchMatch),
    /// Short-hand for `let`-like branch.
    ShortHand(BranchShortHand),
}

impl Branch {
    pub fn is_pure_break(&self) -> bool {
        match self {
            Branch::Let(branch_let) => branch_let.pure_break(),
            Branch::IfLet(branch_if_let) => branch_if_let.pure_break(),
            Branch::Match(branch_match) => branch_match.pure_break(),
            Branch::ShortHand(branch_short_hand) => branch_short_hand.pure_break(),
        }
    }

    pub fn fut_expr(&self) -> &Expr {
        match self {
            Branch::Let(branch_let) => &branch_let.fut_expr,
            Branch::IfLet(BranchIfLet {
                if_arm:
                    BranchIfLetIfArm {
                        cond: Expr::Let(ExprLet { expr: fut_expr, .. }),
                        ..
                    },
            }) => fut_expr,
            Branch::IfLet(BranchIfLet {
                if_arm: BranchIfLetIfArm { cond: fut_expr, .. },
            }) => fut_expr,
            Branch::Match(branch_match) => &branch_match.fut_expr,
            Branch::ShortHand(branch_short_hand) => &branch_short_hand.fut_expr,
        }
    }
}

impl Parse for Branch {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        if input.peek(Token![let]) {
            Ok(Self::Let(input.parse()?))
        } else if input.peek(Token![if]) {
            Ok(Self::IfLet(input.parse()?))
        } else if input.peek(Token![match]) {
            Ok(Self::Match(input.parse()?))
        } else if input.peek(Token![break]) || input.peek(Token![continue]) {
            Ok(Self::ShortHand(input.parse()?))
        } else {
            Err(input.error("expected branch"))
        }
    }
}

use derive_quote_to_tokens::ToTokens;
use syn::{Token, parse::Parse};

use super::{BranchIfLet, BranchLet, BranchMatch, BranchShortHand};

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
    pub fn always_breaks(&self) -> bool {
        match self {
            Branch::Let(branch_let) => branch_let.always_breaks(),
            Branch::IfLet(branch_if_let) => branch_if_let.always_breaks(),
            Branch::Match(branch_match) => branch_match.always_breaks(),
            Branch::ShortHand(branch_short_hand) => branch_short_hand.always_breaks(),
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

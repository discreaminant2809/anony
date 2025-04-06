use super::{BranchIfLet, BranchLet, BranchMatch, BranchShortHand};

pub enum Branch {
    ShortHand(BranchShortHand),
    Let(BranchLet),
    IfLet(BranchIfLet),
    Match(BranchMatch),
}

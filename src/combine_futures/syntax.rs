use std::ops::ControlFlow;

use syn::{Expr, Lifetime, Token, parse::ParseStream};

mod branch;
mod branch_if_let;
mod branch_let;
mod branch_match;
mod branch_short_hand;

pub(super) use branch::*;
pub(super) use branch_if_let::*;
pub(super) use branch_let::*;
pub(super) use branch_match::*;
pub(super) use branch_short_hand::*;

pub(super) struct Input {
    pub movability: Option<Token![move]>,
    pub branches: Vec<Branch>,
}

pub(super) type ControlFLowToken = ControlFlow<Token![break], Token![continue]>;

fn parse_control_flow(input: ParseStream) -> syn::Result<ControlFLowToken> {
    if input.peek(Token![break]) {
        input.parse::<Token![break]>().map(ControlFlow::Break)
    } else if input.peek(Token![continue]) {
        input.parse::<Token![continue]>().map(ControlFlow::Continue)
    } else {
        Err(input.error("expected `break` or `continue`"))
    }
}

fn requires_comma(expr: &Expr) -> bool {
    // See https://docs.rs/syn/2.0.100/src/syn/classify.rs.html#24-68

    !matches!(
        expr,
        Expr::If(_)
            | Expr::Match(_)
            | Expr::Block(_)
            | Expr::Unsafe(_)
            | Expr::While(_)
            | Expr::Loop(_)
            | Expr::ForLoop(_)
            | Expr::TryBlock(_)
            | Expr::Const(_)
    )
}

fn parse_expr_followed_by_comma(input: ParseStream) -> syn::Result<(Expr, Option<Token![,]>)> {
    // See https://docs.rs/syn/2.0.100/src/syn/expr.rs.html#3004-3015

    let expr = Expr::parse_with_earlier_boundary_rule(input)?;
    let comma = if requires_comma(&expr) && !input.is_empty() {
        Some(input.parse()?)
    } else {
        input.parse()?
    };

    Ok((expr, comma))
}

/// Parse control flow directive, like `continue 3`, with comma awareness.
fn parse_cf_directive_followed_by_comma(
    input: ParseStream,
) -> syn::Result<(ControlFLowToken, Option<Expr>, Option<Token![,]>)> {
    let control_flow = parse_control_flow(input)?;

    if input.peek(Lifetime) {
        // See https://docs.rs/syn/2.0.100/src/syn/expr.rs.html#2710-2722
        return if input.peek2(Token![:]) {
            let expr = Expr::parse_with_earlier_boundary_rule(input)?;
            Err(syn::Error::new_spanned(expr, "parentheses required"))
        } else {
            // If not a loop label of a loop, we require no loop label for a control flow directive anyway.
            let lifetime: Lifetime = input.parse()?;
            Err(syn::Error::new_spanned(
                lifetime,
                "loop label is not allowed in control flow directives",
            ))
        };
    }

    match input.parse::<Token![,]>() {
        // Form: `break,` or `continue,`
        Ok(comma) => Ok((control_flow, None, Some(comma))),
        _ => {
            let (expr, comma) = parse_expr_followed_by_comma(input)?;
            Ok((control_flow, Some(expr), comma))
        }
    }
}

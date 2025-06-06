use quote::ToTokens;
use syn::{Expr, ExprLet, Ident};
use syntax::{
    Branch, BranchIf, BranchIfElseArm, BranchIfElseArmDirection, BranchIfIfArm, BranchLet,
    BranchLetElseArm, BranchMatch, BranchMatchArm, BranchMatchArmGuard, BranchShortHand, CfToken,
    Input,
};

use crate::{pm2, utils};

mod syntax;

pub(crate) fn imp(tt: crate::pm::TokenStream, is_cyclic: bool) -> syn::Result<pm2::TokenStream> {
    let input = syn::parse::<Input>(tt)?;

    let pure_breaks = if input.continue_collector.is_some() {
        let mut always_breaks_error = syn::Error::new_spanned(
            input.continue_collector.as_ref().unwrap(),
            "cannot specify continue collector because some branches above always break",
        );

        let pure_breaks = pure_breaks(&input, |branch| {
            always_breaks_error.combine(syn::Error::new_spanned(
                branch,
                "this branch always breaks, which causes the error",
            ))
        });

        if pure_breaks.is_some() {
            return Err(always_breaks_error);
        }

        pure_breaks
    } else {
        pure_breaks(&input, |_| {})
    };

    Ok(imp_impl(
        &input,
        pure_breaks,
        // We have to unify both `is_cyclic` case with only one branch here to avoid modulo zero and `clippy::modulo_one`.
        // Using round-robin on no branch and only 1 branch is just redundant regardless.
        // See https://rust-lang.github.io/rust-clippy/master/index.html#modulo_one.
        is_cyclic && input.branches.len() >= 2,
    ))
}

fn pure_breaks(input: &Input, mut on_pure_break_branch: impl FnMut(&Branch)) -> Option<Vec<bool>> {
    let mut pure_breaks = None;
    for (i, branch) in input.branches.iter().enumerate() {
        if branch.is_pure_break() {
            let pure_breaks = pure_breaks.get_or_insert_with(|| vec![false; input.branches.len()]);
            pure_breaks[i] = true;
            on_pure_break_branch(branch);
        }
    }

    pure_breaks
}

/// I'm tired of `quote_spanned! { Span::mixed_site()=> ... }` already, so it exists.
///
/// Rust analyzer, use this: `quote_mixed_site! {}`
macro_rules! quote_mixed_site {
    { $($tt:tt)* } => { ::quote::quote_spanned! { $crate::pm2::Span::mixed_site()=> $($tt)* } };
}

// TODO: cyclical polling
fn imp_impl(input: &Input, pure_breaks: Option<Vec<bool>>, is_cyclic: bool) -> pm2::TokenStream {
    let any_pure_break = pure_breaks.is_some();

    let fut_ty = Ident::new(
        if is_cyclic {
            "CombineFuturesCyclic"
        } else {
            "CombineFutures"
        },
        pm2::Span::mixed_site(),
    );

    let mut fut_count = pm2::Literal::usize_unsuffixed(input.branches.len());
    fut_count.set_span(pm2::Span::mixed_site());
    let fut_count = fut_count;

    let [to_skip_ty, to_skip_ident, to_skip_init] = to_skip(is_cyclic, input.branches.len() as _);
    let update_to_skip = is_cyclic.then(|| {
        quote_mixed_site! {
            let #to_skip_ident = (
                ::core::mem::replace(
                    #to_skip_ident,
                    *#to_skip_ident + Wrapping(1),
                )
                + Wrapping(<*mut _>::addr(cx) as _)
            ) % Wrapping(#fut_count);
        }
    });

    let fut_generics = utils::i_idents("F", input.branches.len());

    let continue_generics = if any_pure_break {
        vec![]
    } else {
        utils::i_idents("C", input.branches.len())
    };
    let fut_reprs: Vec<_> = if let Some(pure_breaks) = pure_breaks.as_ref() {
        pure_breaks
            .iter()
            .zip(&fut_generics)
            .map(|(&pure_break, fut_generic)| {
                if pure_break {
                    quote_mixed_site! { #fut_generic }
                } else {
                    quote_mixed_site! { ControlFlow<(), #fut_generic> }
                }
            })
            .collect()
    } else {
        continue_generics
            .iter()
            .zip(&fut_generics)
            .map(|(continue_generic, fut_generic)| {
                quote_mixed_site! { ControlFlow<Option<#continue_generic>, #fut_generic> }
            })
            .collect()
    };

    let generics = quote_mixed_site! {<#(#fut_generics,)* #(#continue_generics,)* S, O>};
    let selector_bound = quote_mixed_site! {
        FnMut(
            #(&mut #fut_reprs,)*
            &mut Context<'_>,
            #to_skip_ty
        ) -> Poll<O>
    };
    let bounded_generics = quote_mixed_site! {<
        #(#fut_generics: Future,)*
        #(#continue_generics,)*
        S: #selector_bound,
        O,
    >};

    let fut_idents = utils::i_idents(
        Ident::new(
            // Even tho we use mixed-site span, VS Code (prob the most common IDE for Rust) with rust-analyzer
            // can somehow still refer to such variables (altho it's a compile error anyway).
            // It's only a concern if they're in the selector closure where user code is interleaved with.
            //
            // Added `__` prefix to avoid messing up with the auto-completion.
            "__fut",
            pm2::Span::mixed_site(),
        ),
        input.branches.len(),
    );

    let wrap_futs = fut_idents.iter().enumerate().map(|(i, fut_ident)| {
        if pure_breaks
            .as_ref()
            .is_some_and(|pure_breaks| pure_breaks[i])
        {
            quote_mixed_site! { IntoFuture::into_future(#fut_ident) }
        } else {
            quote_mixed_site! { ControlFlow::Continue(IntoFuture::into_future(#fut_ident)) }
        }
    });

    let o_idents = utils::i_idents(
        Ident::new("__o", pm2::Span::mixed_site()),
        input.branches.len(),
    );

    let fut_exprs = input.branches.iter().map(Branch::fut_expr);
    let movability = input.movability;

    let handlers = input.branches.iter().zip(&fut_idents).enumerate().map(
        |(i, (branch, fut_ident))| {
            let handler = match branch {
                Branch::Let(branch) => {
                    handler_of_branch_let(branch, fut_ident, any_pure_break, is_cyclic)
                }
                Branch::If(branch_if) => {
                    handler_of_branch_if(branch_if, fut_ident, any_pure_break, is_cyclic)
                }
                Branch::Match(branch_match) => {
                    handler_of_branch_match(branch_match, fut_ident, any_pure_break, is_cyclic)
                }
                Branch::ShortHand(branch_short_hand) => handler_of_branch_short_hand(
                    branch_short_hand,
                    fut_ident,
                    any_pure_break,
                    is_cyclic,
                ),
            };

            if pure_breaks
                .as_ref()
                .is_some_and(|pure_breaks| pure_breaks[i])
            {
                quote_mixed_site! {
                    'poll_scope: {
                        if let ::core::task::Poll::Ready(__o) = ::core::future::Future::poll(
                            // SAFETY: pinning projection: the future is structurally pinned.
                            unsafe { ::core::pin::Pin::new_unchecked(#fut_ident) },
                            __cx,
                        ) {
                            // Only `continue` arm sets the future, so no worry.
                            #handler
                        } else {
                            __done = false;
                        }
                    }
                }
            } else {
                quote_mixed_site! {
                    'poll_scope: {
                        if let ::core::ops::ControlFlow::Continue(__fut_inner) = #fut_ident {
                            if let ::core::task::Poll::Ready(__o) = ::core::future::Future::poll(
                                // SAFETY: pinning projection: the future is structurally pinned.
                                unsafe { ::core::pin::Pin::new_unchecked(__fut_inner) },
                                __cx,
                            ) {
                                #handler
                            } else {
                                __done = false;
                            }
                        }
                    }
                }
            }
        },
    );

    let continue_collector = if any_pure_break {
        quote_mixed_site! {}
    } else {
        let continue_collector = input.continue_collector.as_ref().map_or_else(
            || quote_mixed_site! { |#(#o_idents),*| (#(#o_idents,)*) },
            |expr| expr.to_token_stream(),
        );

        let many_none = (0..input.branches.len()).map(|_| quote_mixed_site! { None });

        let macro_name = if is_cyclic {
            "combine_futures_cyclic"
        } else {
            "combine_futures"
        };

        quote_mixed_site! {
            if __done {
                let (#(::core::ops::ControlFlow::Break(#o_idents),)*) = (#(#fut_idents,)*) else {
                    unsafe { ::core::hint::unreachable_unchecked() }
                };

                match (#(::core::option::Option::take(#o_idents),)*) {
                    (#(::core::option::Option::Some(#o_idents),)*) => {
                        return ::core::task::Poll::Ready((#continue_collector)(#(#o_idents),*));
                    }
                    (#(#many_none,)*) => ::core::panic!("`{}!` future polled after completion`", #macro_name),
                    // SAFETY: it's all (all continue values are available) or nothing (all have been taken).
                    // It does lead to better code gen.
                    _ => unsafe { ::core::hint::unreachable_unchecked() },
                }
            }
        }
    };

    let polling_logic = if input.branches.is_empty() {
        // This case exists since the `is_cyclic` case causes `unreachable_code` if there's no branch.
        quote_mixed_site! {}
    } else if is_cyclic {
        let handlers = handlers.enumerate().map(|(i, handler)| {
            let mut i = pm2::Literal::usize_unsuffixed(i);
            i.set_span(pm2::Span::mixed_site());

            quote_mixed_site! { #i => #handler }
        });

        quote_mixed_site! {
            if let ::core::ops::ControlFlow::Break(__o) = ::core::iter::Iterator::try_for_each(
                &mut ::core::iter::Iterator::chain(
                    // If `COUNT` is 0, this loop will never be run!
                    #to_skip_ident.0..#fut_count, 0..#to_skip_ident.0
                ),
                |i| {
                    match i {
                        #(#handlers)*
                        // SAFETY: the index is always within the number of futures (`COUNT`).
                        _ => unsafe { ::core::hint::unreachable_unchecked() },
                    }

                    ::core::ops::ControlFlow::Continue(())
                }
            ) {
                return ::core::task::Poll::Ready(__o);
            }
        }
    } else {
        quote_mixed_site! { #(#handlers)* }
    };

    quote_mixed_site! {({
        use ::core::future::{Future, IntoFuture};
        use ::core::marker::Unpin;
        use ::core::num::Wrapping;
        use ::core::ops::{ControlFlow, FnMut};
        use ::core::option::Option;
        use ::core::pin::Pin;
        use ::core::task::{Context, Poll};

        // #[pin_project]
        enum #fut_ty #bounded_generics {
            Inner(
                #(
                    // #[pin]
                    // Even pinned, only the future is, not the result wrapped in `Option` is not pinned,
                    // since taking the value does not require pinning.
                    #fut_reprs,
                )*
                // not pinned, since the closure can be used without being pinned.
                S,
                #to_skip_ty
            ),
        }

        impl #bounded_generics #fut_ty #generics {
            fn new(
                #(#fut_idents: impl IntoFuture<IntoFuture = #fut_generics>,)*
                selector: S,
            ) -> Self {
                Self::Inner(
                    #(#wrap_futs,)*
                    selector,
                    #to_skip_init
                )
            }
        }

        impl<
            #(#fut_generics: Future + Unpin,)*
            #(#continue_generics,)*
            S: #selector_bound,
            O,
        > Unpin for #fut_ty #generics {}

        impl #bounded_generics Future for #fut_ty #generics {
            type Output = O;

            fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
                // SAFETY: pinning projection.
                let Self::Inner(#(#fut_idents,)* selector, #to_skip_ident) = unsafe { Pin::get_unchecked_mut(self) };

                #update_to_skip

                selector(
                    #(#fut_idents,)*
                    cx,
                    #to_skip_ident
                )
            }
        }

        #fut_ty::new
    })(
        #(#fut_exprs,)*
        #movability |#(#fut_idents,)* __cx, #to_skip_ident| {
            let mut __done = true;
            #polling_logic
            #continue_collector

            ::core::task::Poll::Pending
        }
    )}
}

fn to_skip(is_cyclic: bool, n: u128) -> [pm2::TokenStream; 3] {
    if !is_cyclic {
        return Default::default();
    }

    let to_skip_ty = if n <= u8::MAX as _ {
        quote_mixed_site! { u8 }
    } else if n <= u16::MAX as _ {
        quote_mixed_site! { u16 }
    } else if n <= u32::MAX as _ {
        quote_mixed_site! { u32 }
    } else if n <= u64::MAX as _ {
        quote_mixed_site! { u64 }
    } else {
        quote_mixed_site! { u128 }
    };

    [
        quote_mixed_site! { Wrapping<::core::primitive::#to_skip_ty> },
        quote_mixed_site! { __to_skip },
        quote_mixed_site! { Wrapping(0) },
    ]
}

fn decision(
    body: Option<impl ToTokens>,
    control_flow: &CfToken,
    fut_ident: &Ident,
    any_pure_break: bool,
    is_cyclic: bool,
) -> pm2::TokenStream {
    let body = body.map_or_else(|| quote_mixed_site! {{}}, |expr| quote_mixed_site! {#expr});
    if control_flow.is_break() {
        if is_cyclic {
            quote_mixed_site! {
                return ::core::ops::ControlFlow::Break((|| #body)());
            }
        } else {
            quote_mixed_site! {
                return ::core::task::Poll::Ready((|| #body)());
            }
        }
    } else if any_pure_break {
        // Not relevant to store a continue value, so the `value` will be discarded
        quote_mixed_site! {
            ::core::mem::drop((|| #body)());
            *#fut_ident = ::core::ops::ControlFlow::Break(());
            break 'poll_scope;
        }
    } else {
        quote_mixed_site! {
            *#fut_ident = ::core::ops::ControlFlow::Break(::core::option::Option::Some((|| #body)()));
            break 'poll_scope;
        }
    }
}

fn handler_of_branch_let(
    BranchLet {
        let_token,
        pat,
        eq_token,
        else_arm,
        control_flow,
        body,
        ..
    }: &BranchLet,
    fut_ident: &Ident,
    any_pure_break: bool,
    is_cyclic: bool,
) -> pm2::TokenStream {
    let else_branch = if let Some(BranchLetElseArm {
        else_token,
        control_flow,
        body,
        ..
    }) = else_arm
    {
        let decision = decision(
            body.as_ref(),
            control_flow,
            fut_ident,
            any_pure_break,
            is_cyclic,
        );
        quote_mixed_site! {
            #else_token {
                #decision
            }
        }
    } else {
        quote_mixed_site! {}
    };

    let decision = decision(
        body.as_ref(),
        control_flow,
        fut_ident,
        any_pure_break,
        is_cyclic,
    );

    quote_mixed_site! {
        #let_token #pat #eq_token __o #else_branch;
        #decision
    }
}

fn handler_of_branch_short_hand(
    BranchShortHand { control_flow, .. }: &BranchShortHand,
    fut_ident: &Ident,
    any_pure_break: bool,
    is_cyclic: bool,
) -> pm2::TokenStream {
    let body = quote_mixed_site! {__o};
    decision(
        Some(&body),
        control_flow,
        fut_ident,
        any_pure_break,
        is_cyclic,
    )
}

fn handler_of_branch_match(
    BranchMatch {
        match_token,
        brace_token,
        arms,
        ..
    }: &BranchMatch,
    fut_ident: &Ident,
    any_pure_break: bool,
    is_cyclic: bool,
) -> pm2::TokenStream {
    let mut match_content = pm2::TokenStream::new();
    brace_token.surround(&mut match_content, |match_content| {
        let arms = arms.iter().map(
            |BranchMatchArm {
                 pat,
                 guard,
                 fat_arrow_token,
                 control_flow,
                 body,
                 ..
             }: &BranchMatchArm| {
                let decision = decision(
                    body.as_ref(),
                    control_flow,
                    fut_ident,
                    any_pure_break,
                    is_cyclic,
                );

                let guard = if let Some(BranchMatchArmGuard { if_token, expr }) = guard {
                    quote_mixed_site! { #if_token (|| #expr)() }
                } else {
                    quote_mixed_site! {}
                };

                quote_mixed_site! {
                   #pat #guard #fat_arrow_token { #decision }
                }
            },
        );

        *match_content = quote_mixed_site! { #(#arms)* };
    });

    quote_mixed_site! {
        #match_token __o #match_content
    }
}

fn handler_of_branch_if(
    BranchIf { if_arm }: &BranchIf,
    fut_ident: &Ident,
    any_pure_break: bool,
    is_cyclic: bool,
) -> pm2::TokenStream {
    fn handler_of_else_arm(
        BranchIfElseArm {
            else_token,
            direction,
        }: &BranchIfElseArm,
        fut_ident: &Ident,
        any_pure_break: bool,
        is_cyclic: bool,
    ) -> pm2::TokenStream {
        match direction {
            BranchIfElseArmDirection::End {
                control_flow, body, ..
            } => {
                let decision = decision(
                    Some(body),
                    control_flow,
                    fut_ident,
                    any_pure_break,
                    is_cyclic,
                );

                quote_mixed_site! { #else_token { #decision } }
            }
            BranchIfElseArmDirection::ElseIf(branch_if_if_arm) => {
                let handler_of_if_arm = handler_of_if_arm(
                    branch_if_if_arm,
                    fut_ident,
                    any_pure_break,
                    is_cyclic,
                    false,
                );

                quote_mixed_site! { #else_token #handler_of_if_arm }
            }
        }
    }

    fn handler_of_if_arm(
        BranchIfIfArm {
            if_token,
            cond,
            control_flow,
            then_arm,
            else_arm,
            ..
        }: &BranchIfIfArm,
        fut_ident: &Ident,
        any_pure_break: bool,
        is_cyclic: bool,
        is_fut_output: bool,
    ) -> pm2::TokenStream {
        let decision = decision(
            Some(then_arm),
            control_flow,
            fut_ident,
            any_pure_break,
            is_cyclic,
        );
        let handler_of_else_arm =
            handler_of_else_arm(else_arm, fut_ident, any_pure_break, is_cyclic);

        // What if `cond` is a macro that expands to a `let` expression?
        // Consider this:
        // ```
        // fn compile() {
        //     if let 2 = 2 {}
        // }
        //
        // fn not_compile() {
        //     macro_rules! foo {
        //         ($x:expr) => { let 2 = $x };
        //     }
        //
        //     if foo!(2) {} // compile error!
        //
        //     if true {
        //     } else if foo!(2) { // also compile error!
        //     }
        // }
        // ```
        // The reason is that the `let` in `if-let` expression must not come from macro.
        // In such case, the `let` injected from the macro will still be understood
        // as a part of the expression of the macro, and not something that can be merged with `if`.
        let cond = match cond {
            Expr::Let(ExprLet {
                attrs,
                let_token,
                pat,
                eq_token,
                ..
            }) if is_fut_output => quote_mixed_site! { #(#attrs)* #let_token #pat #eq_token __o },
            Expr::Let(ExprLet {
                attrs,
                let_token,
                pat,
                eq_token,
                expr,
            }) => quote_mixed_site! { #(#attrs)* #let_token #pat #eq_token (|| #expr)() },
            _ if is_fut_output => quote_mixed_site! { __o },
            cond => quote_mixed_site! { (|| #cond)() },
        };

        quote_mixed_site! { #if_token #cond { #decision } #handler_of_else_arm }
    }

    handler_of_if_arm(if_arm, fut_ident, any_pure_break, is_cyclic, true)
}

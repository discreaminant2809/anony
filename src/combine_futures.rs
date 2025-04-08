use quote::ToTokens;
use syn::Ident;
use syntax::{
    Branch, BranchIfLet, BranchIfLetElseArm, BranchIfLetElseArmDirection, BranchIfLetIfArm,
    BranchLet, BranchLetElseArm, BranchMatch, BranchMatchArm, BranchShortHand, CfToken, Input,
};

use crate::{pm2, utils};

mod syntax;

pub(crate) fn imp(tt: crate::pm::TokenStream, is_cyclic: bool) -> syn::Result<pm2::TokenStream> {
    let input = syn::parse::<Input>(tt)?;

    let any_pure_break = if input.continue_collector.is_some() {
        let mut always_breaks_error = syn::Error::new_spanned(
            input.continue_collector.as_ref().unwrap(),
            "cannot specify continue collector because some branches above always break",
        );

        let any_always_breaks = any_pure_break(&input, |branch| {
            always_breaks_error.combine(syn::Error::new_spanned(
                branch,
                "this branch always breaks, which causes the error",
            ));
        });

        if any_always_breaks {
            return Err(always_breaks_error);
        }

        any_always_breaks
    } else {
        any_pure_break(&input, |_| {})
    };

    Ok(imp_impl(&input, any_pure_break, is_cyclic))
}

fn any_pure_break(input: &Input, mut on_found_one: impl FnMut(&Branch)) -> bool {
    let mut any_always_breaks = false;
    for branch in &input.branches {
        if branch.always_breaks() {
            any_always_breaks = true;
            on_found_one(branch);
        }
    }

    any_always_breaks
}

/// I'm tired of `quote_spanned! { Span::mixed_site()=> ... }` already.
macro_rules! quote_mixed_site {
    ($($tt:tt)*) => { ::quote::quote_spanned! { $crate::pm2::Span::mixed_site()=> $($tt)* } };
}

// TODO: cyclical polling
fn imp_impl(input: &Input, any_pure_break: bool, is_cyclic: bool) -> pm2::TokenStream {
    let necessary_imports = necessary_imports();
    let fut_ty = Ident::new(
        if is_cyclic {
            "CombineFuturesCyclic"
        } else {
            "CombineFutures"
        },
        pm2::Span::mixed_site(),
    );
    let [to_skip_ty, to_skip_ident, to_skip_init] = to_skip(is_cyclic, input.branches.len() as _);
    let mut_ref_to_skip_ty = if is_cyclic {
        quote_mixed_site! { &mut #to_skip_ty }
    } else {
        quote_mixed_site! {}
    };

    let fut_generics = utils::i_idents("F", input.branches.len());

    let continue_generics = if any_pure_break {
        vec![]
    } else {
        utils::i_idents("C", input.branches.len())
    };
    let continue_reprs: Vec<_> = if any_pure_break {
        (0..input.branches.len())
            .map(|_| quote_mixed_site! { () })
            .collect()
    } else {
        continue_generics
            .iter()
            .map(|generic| quote_mixed_site! { Option<#generic> })
            .collect()
    };

    let generics = quote_mixed_site! {<#(#fut_generics,)* #(#continue_generics,)* S, O>};
    let selector_bound = quote_mixed_site! {
        FnMut(
            #(&mut ControlFlow<#continue_reprs, #fut_generics>,)*
            &mut Context<'_>,
            #mut_ref_to_skip_ty
        ) -> Poll<O>
    };
    let bounded_generics = quote_mixed_site! {<
        #(#fut_generics: Future,)*
        #(#continue_generics,)*
        S: #selector_bound,
        O,
    >};

    let fut_idents = utils::i_idents(
        Ident::new("fut", pm2::Span::mixed_site()),
        input.branches.len(),
    );

    let o_idents = utils::i_idents(
        Ident::new("o", pm2::Span::mixed_site()),
        input.branches.len(),
    );

    let fut_exprs = input.branches.iter().map(Branch::fut_expr);
    let movability = input.movability;
    let handlers = input
        .branches
        .iter()
        .zip(&fut_idents)
        .map(|(branch, fut_ident)| {
            let handler = match branch {
                Branch::Let(branch) => {
                    handler_of_branch_let(branch, fut_ident, any_pure_break, is_cyclic)
                }
                Branch::IfLet(branch_if_let) => {
                    handler_of_branch_if_let(branch_if_let, fut_ident, any_pure_break, is_cyclic)
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

            quote_mixed_site! { // we're interacting with user code. (highly unhygienic)
                // Full qualification, for the same reason as above. (Mixed-site span does not help)
                // `fut_ident` has mixed-site span, so the user code can't refer to it.
                'poll_scope: { // facility for `let-else` branch.
                    if let ::core::ops::ControlFlow::Continue(fut_inner) = #fut_ident {
                        if let ::core::task::Poll::Ready(o) = ::core::future::Future::poll(
                            // SAFETY: pinning projection: the future is structurally pinned.
                            unsafe { ::core::pin::Pin::new_unchecked(fut_inner) },
                            cx,
                        ) {
                            #handler
                        }
                    }
                }
            }
        });

    let continue_collector = if any_pure_break {
        quote_mixed_site! {}
    } else {
        let continue_collector = input.continue_collector.as_ref().map_or_else(
            || quote_mixed_site! { |#(#o_idents),*| (#(#o_idents),*) },
            |expr| expr.to_token_stream(),
        );

        let many_none = (0..input.branches.len()).map(|_| quote_mixed_site! { None });

        quote_mixed_site! {
            if let (#(::core::ops::ControlFlow::Break(#o_idents),)*) = (#(#fut_idents,)*) {
                match (#(::core::option::Option::take(#o_idents),)*) {
                    (#(::core::option::Option::Some(#o_idents),)*) => {
                        return ::core::task::Poll::Ready((#continue_collector)(#(#o_idents),*));
                    }
                    (#(#many_none,)*) => ::core::panic!("`combine_futures!` future polled after completion`"),
                    // SAFETY: it's all (all continue values are available) or nothing (all have been taken).
                    // It does lead to better code gen.
                    _ => unsafe { ::core::hint::unreachable_unchecked() },
                }
            }
        }
    };

    let mut fut_count = pm2::Literal::usize_unsuffixed(input.branches.len());
    fut_count.set_span(pm2::Span::mixed_site());
    let fut_count = fut_count;

    // let shift_to_skip = if is_cyclic {
    //     quote_mixed_site! {
    //         // Do this to enforce the type of the count.
    //         const COUNT: #to_skip_ty = #fut_count;
    //         let #to_skip_ident = ::core::mem::replace(
    //             #to_skip_ident,
    //             (*#to_skip_ident + 1) % COUNT
    //         );
    //     }
    // } else {
    //     quote_mixed_site! {}
    // };

    let polling_logic = if is_cyclic {
        let handlers = handlers.enumerate().map(|(i, handler)| {
            let mut i = pm2::Literal::usize_unsuffixed(i);
            i.set_span(pm2::Span::mixed_site());

            quote_mixed_site! { #i => #handler }
        });

        quote_mixed_site! {
            const COUNT: #to_skip_ty = #fut_count;
            let #to_skip_ident = ::core::mem::replace(
                #to_skip_ident,
                (*#to_skip_ident + 1) % COUNT
            );
            if let ::core::ops::ControlFlow::Break(o) = ::core::iter::Iterator::try_for_each(
                &mut ::core::iter::Iterator::chain(
                    #to_skip_ident..COUNT, 0..#to_skip_ident
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
                return ::core::task::Poll::Ready(o);
            }
        }
    } else {
        quote_mixed_site! { #(#handlers)* }
    };

    quote_mixed_site! {({
        #necessary_imports

        // #[pin_project]
        enum #fut_ty #bounded_generics {
            Inner(
                #(
                    // #[pin]
                    // Even pinned, only the future is, not the result wrapped in `Option` is not pinned,
                    // since taking the value does not require pinning.
                    ControlFlow<#continue_reprs, #fut_generics>,
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
                    #(ControlFlow::Continue(IntoFuture::into_future(#fut_idents)),)*
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

                selector(#(#fut_idents,)* cx, #to_skip_ident)
            }
        }

        #fut_ty::new
    })(
        #(#fut_exprs,)*
        #movability |#(#fut_idents,)* cx, #to_skip_ident| {
            #polling_logic

            #continue_collector

            ::core::task::Poll::Pending
        }
    )}
}

// NOTE: this does not import for the selector, since it's unhygiene by user code.
fn necessary_imports() -> pm2::TokenStream {
    quote_mixed_site! {
        use ::core::future::{Future, IntoFuture};
        use ::core::marker::Unpin;
        use ::core::ops::ControlFlow;
        use ::core::option::Option;
        use ::core::pin::Pin;
        use ::core::task::{Context, Poll};
    }
}

fn to_skip(is_cyclic: bool, n: u128) -> [pm2::TokenStream; 3] {
    if !is_cyclic {
        return Default::default();
    }

    let to_skip_ty = if n <= u8::MAX as _ {
        quote_mixed_site! { ::core::primitive::u8 }
    } else if n <= u16::MAX as _ {
        quote_mixed_site! { ::core::primitive::u16 }
    } else if n <= u32::MAX as _ {
        quote_mixed_site! { ::core::primitive::u32 }
    } else if n <= u64::MAX as _ {
        quote_mixed_site! { ::core::primitive::u64 }
    } else {
        quote_mixed_site! { ::core::primitive::u128 }
    };

    [
        to_skip_ty,
        quote_mixed_site! { to_skip },
        quote_mixed_site! { 0 },
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
        #let_token #pat #eq_token o #else_branch;
        #decision
    }
}

fn handler_of_branch_short_hand(
    BranchShortHand { control_flow, .. }: &BranchShortHand,
    fut_ident: &Ident,
    any_pure_break: bool,
    is_cyclic: bool,
) -> pm2::TokenStream {
    let body = quote_mixed_site! {o};
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

                quote_mixed_site! {
                   #pat #guard #fat_arrow_token { #decision }
                }
            },
        );

        *match_content = quote_mixed_site! { #(#arms)* };
    });

    quote_mixed_site! {
        #match_token o #match_content
    }
}

fn handler_of_branch_if_let(
    BranchIfLet {
        if_token,
        let_token,
        pat,
        eq_token,
        control_flow,
        then_arm,
        else_arm,
        ..
    }: &BranchIfLet,
    fut_ident: &Ident,
    any_pure_break: bool,
    is_cyclic: bool,
) -> pm2::TokenStream {
    fn handler_of_else_arm(
        BranchIfLetElseArm {
            else_token,
            direction,
        }: &BranchIfLetElseArm,
        fut_ident: &Ident,
        any_pure_break: bool,
        is_cyclic: bool,
    ) -> pm2::TokenStream {
        match direction {
            BranchIfLetElseArmDirection::End {
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
            BranchIfLetElseArmDirection::ElseIf(branch_if_let_if_arm) => {
                let handler_of_if_arm =
                    handler_of_if_arm(branch_if_let_if_arm, fut_ident, any_pure_break, is_cyclic);

                quote_mixed_site! { #else_token #handler_of_if_arm }
            }
        }
    }

    fn handler_of_if_arm(
        BranchIfLetIfArm {
            if_token,
            cond,
            control_flow,
            then_arm,
            else_arm,
            ..
        }: &BranchIfLetIfArm,
        fut_ident: &Ident,
        any_pure_break: bool,
        is_cyclic: bool,
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

        quote_mixed_site! { #if_token #cond { #decision } #handler_of_else_arm }
    }

    let decision = decision(
        Some(then_arm),
        control_flow,
        fut_ident,
        any_pure_break,
        is_cyclic,
    );
    let handler_of_else_arm = handler_of_else_arm(else_arm, fut_ident, any_pure_break, is_cyclic);

    quote_mixed_site! {
        #if_token #let_token #pat #eq_token o { #decision } #handler_of_else_arm
    }
}

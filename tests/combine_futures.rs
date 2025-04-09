#[test]
#[ignore = "syntax test"]
fn syntax_test() {
    macro_rules! pure_break {
        ($macro_name:ident) => {
            let fut_expr0 = async { 3 };
            let condition = false;

            let _fut = ::anony::$macro_name! {
                break async {2},

                let 2 = async {2} else => break 2 => continue,

                let x = fut_expr0 => continue x,

                if let x @ -1 = async {-1} => break {
                    x
                } else if condition => continue {
                    0
                } else if condition => break {
                    0
                } else => break {
                    0
                }

                match async{ Some(2) } {
                    Some(x) if x == 0 => break x,
                    Some(x) => continue { x }
                    None => break 0,
                }
            };
        };
    }

    pure_break!(combine_futures);
    pure_break!(combine_futures_cyclic);

    fn assert_static<T: 'static>(_: &T) {}

    macro_rules! no_pure_break {
        ($macro_name:ident) => {
            let fut_expr0 = async { 3 };
            let condition = false;
            let coefficient = -8;
            let fut = ::anony::$macro_name! {
                // removing it will cause error in the `assert_static`, since the captured variable is captured by reference.
                move

                continue async {2},

                let 2 = async {2} else => break 2 => continue,

                let x = fut_expr0 => continue x,

                if let x @ -1 = async {-1} => break {
                    x
                } else if condition => continue {
                    0
                } else if let false = condition => break {
                    0
                } else => break {
                    0
                }

                if async { true } => continue {
                    "abc"
                } else => break {
                    dbg!(0)
                }

                match async{ Some(2) } {
                    Some(x) if x == 0 => break x,
                    Some(x) => continue { x }
                    None => break 0,
                }

                |a, _, c: i32, d, _: &str, e| (a + c + d + e) * coefficient,
            };

            assert_static(&fut);
        }
    }

    no_pure_break!(combine_futures);
    no_pure_break!(combine_futures_cyclic);
}

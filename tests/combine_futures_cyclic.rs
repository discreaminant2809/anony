#[test]
#[ignore = "syntax test"]
fn syntax_test() {
    let fut_expr0 = async { 3 };
    let condition = false;

    let _fut = anony::combine_futures_cyclic! {
        break async {2},

        let _x = async {2} else => break 2 => continue,

        let x = fut_expr0 => continue x,

        if let x = async {-1} => break {
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

    let fut_expr0 = async { 3 };
    let fut = anony::combine_futures_cyclic! {
        // removing it will cause error in the `assert_static`, since the captured variable is captured by reference.
        move

        continue async {2},

        let _x = async {2} else => break 2 => continue,

        let x = fut_expr0 => continue x,

        if let x = async {-1} => break {
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

        |a, _, c, d, e| a + c + d + e
    };

    fn assert_static<T: 'static>(_: &T) {}
    assert_static(&fut);
}

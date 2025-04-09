use anony::combine_futures;

fn predicate() -> bool {
    true
}

fn expansion1() {
    let fut_expr0 = async { 3 };
    let condition = false;
    let _fut = combine_futures! {
        break async {2},

        let 2 = async {2} else => break 2 => continue,

        let x = fut_expr0 => continue x,

        if let x @ -1 = async {-1} => break {
            x
        } else if condition => continue {
            println!("Hello");
            if predicate() {
                return 2;
            }

            1
        } else if let true = condition => break {
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
            Some(_x) => continue,
            None => break 0,
        }
    };
}

fn expansion2() {
    let fut_expr0 = async { 3 };
    let condition = false;
    let coefficient = -8;
    let fut = combine_futures! {
        move

        continue async {2},

        let 2 = async {2} else => break 2 => continue,

        let x = fut_expr0 => continue x,

        if let x @ -1 = async {-1} => break {
            x
        } else if condition => continue {
            println!("Hello");
            if predicate() {
                return 2;
            }

            1
        } else if let None = condition.then_some("abc") => break {
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
}

fn expansion3() {
    let _fut = combine_futures! {};
}

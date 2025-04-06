#[test]
#[ignore = "syntax test"]
fn syntax_test() {
    anony::combine_futures! {
        move

        continue {async {2}},

        continue fut_expr,

        let pat = fut_expr                    => continue,

        let pat = async {} else => break expr => continue expr,

        if let pat = fut_expr => continue {

        } else if condition => break {

        } else => break {

        }

        match fut_expr {
            pat => continue expr,
            pat if guard => break expr,
        }

        |s| 2
    };
}

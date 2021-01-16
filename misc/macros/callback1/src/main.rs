#[feature(decl_macro)]
use if_chain;

fn main() {
    let x1 = Some(20);
    let x = if_chain::if_chain! {
        if let Some(i) = x1;
        if let 20 = i;
        then { true }
        else {false}
    };
    println!("x={}", x);
}

#![feature(decl_macro)]
use if_chain::if_chain;

macro mk_chain_impl {
    ($callback:ident $($tt:tt)*) => {
        stringify!($callback! $($tt)* )
    }
}
macro mk_chain {
    ($($tt:tt)*) => {mk_chain_impl!(if_chain $($tt)*)},
}

fn main() {
    let x1 = Some(20);
    let x = if_chain! {
        if let Some(i) = x1;
        if let 20 = i;
        then { true }
        else {false}
    };
    println!("x={}", x);

    let x = mk_chain! {
        {if let Some(i) = x1;
         if let 20 = i; },
        true,
        false,
    };
    println!("x={}", x);
}

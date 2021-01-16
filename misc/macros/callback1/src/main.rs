#![feature(decl_macro)]
use if_chain::if_chain;

/*
macro mk_chain_impl {
    ($callback:ident $($tt:tt)*) => {
        $callback!{$($tt)*}
    }
}
*/
macro mk_chain_impl {
    (@callback $cb:ident $($body:tt)*) => {
        $cb! { $($body)* }
    },
    (@accum (then $then:tt else $else:tt) -> ($($body:tt)*)) => {
        mk_chain_impl!(@callback stringify {$($body)* then $then else $else})
    },
    (@accum (let $pat:pat = $expr:expr; $($tail:tt)*) -> ($($body:tt)*)) => {
        mk_chain_impl!(@accum ($($tail)*) -> ($($body)* if let $pat = $expr;))
    },
    (@accum ($expr:expr, $($tail:tt)*) -> ($($body:tt)*)) => {
        mk_chain_impl!(@accum ($($tail)*) -> ($($body)*; if $expr;))
    },
}
macro mk_chain {
    ($($tt:tt)*) => {
        mk_chain_impl!(@accum ($($tt)*) -> ())
    },
//        mk_chain_impl!(if_chain $($tt)*)},
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
        let Some(i) = x1;
        let 20 = i;
        then {true} else {false}
    };
    println!("x={}", x);
}

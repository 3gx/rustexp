#![feature(decl_macro)]
#![feature(destructuring_assignment)]
#![feature(let_chains)]
#![allow(incomplete_features)]
use if_chain;
/*
#[allow(unused_macros)]
macro mymatch {
   ( |$obj:expr;| $($matcher:pat $(if $pred:expr)* => $result:expr),*) => {
       match $obj {
           $($matcher $(if $pred)* => $result),*
       }
   }
}
#[allow(unused_macros)]
macro mymatch1 {
   ( [ $obj:expr ]  $($matcher:pat $(if $pred:expr)* => $result:expr),*) => {
       match $obj {
           $($matcher $(if $pred)* => $result),*
       }
   }

}
#[allow(unused_macros)]
macro mymatch2 {
   (@unfolded [ $obj:expr ] $( $(@[$($guard:expr),*])? $matcher:pat $(if $pred:expr)* => $result:expr),*) => {
       match $obj {
           $($matcher $(if $pred)* => {stringify!($($($guard),*),*); $result}),*
       }
   },
//   (@guard $($guard:tt)*) => {
 //      stringify!($($guard)*);
  // },
   (@guard let $pat:pat = $expr:expr $(,)?) => {
       let $pat = $expr
   },
   (@guard $guard:expr $(,)?) => {
       $guard
   },
   (@guard $guard:expr, $($tail:tt)*) => {
       mymatch2!(@guard $guard) && mymatch2!(@guard $($tail)*)
   },
   (@guard let $pat:pat = $expr:expr, $($tail:tt)*) => {
       mymatch2!(@guard let $pat = $expr) && mymatch2!(@guard $($tail)*)
   },
   ([ $obj:expr ] $( $matcher:pat $(if {$($guard:tt)*})* => $result:expr),*) => {
       match $obj {
           $($matcher $(if mymatch2!(@guard $($guard)*))* =>
                    {stringify!($($($guard)*),*); $result}),*
       }
   },

   /*
   (@case) => {},
   (@case $matcher:pat $(if $pred:expr)* => $result:expr $(,$tail:tt)*) => {
       $matcher $(if $pred)* => $result,
       mymatch2!(@case $($tail)*)
   },

//   (@pat $matcher:pat $(if $pred:expr)* => $result:expr) => {
 //      $matcher $(if $pred)*
  // },
   //(@rule $matcher:pat $(if $pred:expr)* => $result:expr) => {
    //   $result
//   }//,
   ( [ $obj:expr ]  $($tail:tt)*) => {
       match $obj {
           mymatch2!(@case $($tail)*) => (),
       }
   }
   */
}
*/

macro mymatch3 {
   (@guard let $pat:pat = $expr:expr $(,)?) => {
       let $pat = $expr
   },
   (@guard $guard:expr $(,)?) => {
       $guard
   },
   (@guard $guard:expr, $($tail:tt)*) => {
       mymatch3!(@guard $guard) && mymatch3!(@guard $($tail)*)
   },
   (@guard let $pat:pat = $expr:expr, $($tail:tt)*) => {
       mymatch3!(@guard let $pat = $expr) && mymatch3!(@guard $($tail)*)
   },
   ([ $obj:expr ] $( $matcher:pat $(if {$($guard:tt)*})* => $result:expr),*) => {
       match $obj {
           $($matcher $(if
                   if_chain::if_chain!
                   {
                       mymatch3!(@guard $($guard)*)
                   })* =>
                    {stringify!($($($guard)*),*); $result}),*
       }
   },

}
fn main() {
    /*
    let matchme = |x| {
        mymatch! { |x;|
            Some(Some(7)) => println!("seven!"),
            Some(x) => println!("Some(x), x=  {:?}", x),
            None => println!("None (jawdrop)")
        }
    };

    let x = Some(Some(7));
    matchme(x);
    matchme(Some(Some(5)));
    matchme(Some(None));
    matchme(None);

    let x = 10;
    let y = mymatch! {
        |x;|
     10 => "Ten",
     _ => "something else"
    };
    println!("y={:?}", y);

    macro pat($x:expr) {
        $x
    }

    let x0 = 10;
    let y0 = mymatch1! {
            [x0]
                pat!(10) => "Ten",
    //     with_guard[10 => "Ten"],
         _ => "something else"
        };
    println!("y0={:?}", y0);
    */

    let x1 = Some(20);
    let y1 = mymatch3! {
        [x1]
           Some(10) => "Ten",
           Some(n) if {n == 20, n == 21} => "twice Ten",
           n if {let Some(n) = n, let 20 = n} => "twice Ten",
    //     with_guard[10 => "guarded Ten"],
         _ => "something else"
        };
    println!("y1={:?}", y1);

    let x = if_chain::if_chain! {
        if let Some(i) = x1;
        if let 20 = i;
        then { true }
        else {false}
    };
    println!("x={}", x);
}

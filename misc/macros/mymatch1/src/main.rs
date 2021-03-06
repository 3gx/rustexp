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
} #[allow(unused_macros)]
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
   (@guard $ret:expr, let $pat:pat = $expr:expr $(,)?) => {
       if let $pat = $expr { return $ret }
   },
   (@guard $ret:expr, $guard:expr $(,)?) => {
       if $guard { return $ret }
   },
   (@guard $ret:expr, $guard:expr, $($tail:tt)*) => {
       if $guard { mymatch3!(@guard $ret, $($tail)*) }
   },
   (@guard $ret:expr, let $pat:pat = $expr:expr, $($tail:tt)*) => {
       if let $pat = $expr {  mymatch3!(@guard $ret, $($tail)*) }
   },
   (@guard @unused $ret:expr, let $pat:pat = $expr:expr $(,)?) => {
       #[allow(unused_variables)]
       if let $pat = $expr { return $ret }
   },
   (@guard @unused $ret:expr, $guard:expr $(,)?) => {
       if $guard { return $ret }
   },
   (@guard @unused $ret:expr, $guard:expr, $($tail:tt)*) => {
       if $guard { mymatch3!(@guard $ret, $($tail)*) }
   },
   (@guard @unused $ret:expr, let $pat:pat = $expr:expr, $($tail:tt)*) => {
       #[allow(unused_variables)]
       if let $pat = $expr {  mymatch3!(@guard $ret, $($tail)*) }
   },
   ([ $obj:expr ] $( $matcher:pat $(if {$($guard:tt)*})? => $result:expr),*) => {
       match $obj {
           $($matcher $(if
                   (|| {
                       mymatch3!(@guard @unused true, $($guard)*);
                       return false; }
                   )())* => {
                       (||
                           {
                               mymatch3!(@guard $result, true, $($($guard)*)*);
                                 panic!("unreacahble");
                           }
                       )()
                     }),*
       }
   },
}

macro mymatch4 {
    /* terminal case */
   ((@cases)
    (@obj $($obj:tt)*)
    (@rules $($rules:tt)*)) => {
           match $($obj)* {
               $($rules)*
           }
   },
   /* inject attribute allow_unused */
   (@allow_unused $($tt:tt)*) => {
      #[allow(unused_variables)]
      $($tt)*
   },
   /* carbon_copy */
   (@carbon_copy $($tt:tt)*) => {
      $($tt)*
   },
   (@guard $cb:ident $ret:expr, let $pat:pat = $expr:expr $(,)?)
       => {
       mymatch4!(@$cb if let $pat = $expr { return $ret })
   },
   /*
   (@guard $cb:ident $ret:expr, $pat:pat = $expr:expr $(,)?)
       => {
       mymatch4!(@$cb if let $pat = $expr { return $ret })
   },
   */
   (@guard $cb:ident $ret:expr, $guard:expr $(,)?) => {
       if $guard { return $ret }
   },
   (@guard $cb:ident $ret:expr, let $pat:pat = $expr:expr, $($tail:tt)*) => {
       mymatch4!(@$cb if let $pat = $expr {  mymatch4!(@guard $cb $ret, $($tail)*) })
   },
   /*
   (@guard $cb:ident $ret:expr, $pat:pat = $expr:expr, $($tail:tt)*) => {
       mymatch4!(@$cb if let $pat = $expr {  mymatch4!(@guard $cb $ret, $($tail)*) })
   },
   */
   (@guard $cb:ident $ret:expr, $guard:expr, $($tail:tt)*) => {
       if $guard { mymatch4!(@guard $cb $ret, $($tail)*) }
   },

   ((@cases $pat:pat $(if @{$($guard:tt)*})? => $result:expr, $($tail:tt)*)
    (@obj $($obj:tt)*)
    (@rules $($rules:tt)*)) => {
       mymatch4!(
               (@cases $($tail)*)
               (@obj $($obj)*)
               (@rules $($rules)* $pat
                    $(if
                        (|| {
                            mymatch4!(@guard allow_unused true, $($guard)*);
                            return false;
                        })()
                     )? => (||{
                        mymatch4!(@guard carbon_copy $result, true, $($($guard)*)?);
                        panic!("unreacahble");
                          })(),))
   },
   ((@cases $pat:pat $(if $guard:expr)? => $result:expr, $($tail:tt)*)
    (@obj $($obj:tt)*)
    (@rules $($rules:tt)*)) => {
       mymatch4!(
               (@cases $($tail)*)
               (@obj $($obj)*)
               (@rules $($rules)* $pat $(if $guard)? => $result,))
   },
   ( [ $obj:expr ] $($tail:tt)* ) => {
       mymatch4!(
           (@cases $($tail)*)
           (@obj $obj)
           (@rules)
       )
   }
}

/*
macro mymatch4 {
   (@ifs if $expr:expr; $($tt:tt)*) => {
       if $expr; mymatch4!($($tt)*)
   },
   ([ $obj:expr ] $( $matcher:pat $(if {$($guard:tt)*})* => $result:expr),*) => {
       match $obj {
           $($matcher $(if
                   {
                    if_chain::if_chain!{
                       mymatch4!(@ifs $($guard)*)
                       then { true } else {false }
                   }}
                   )* =>
                    {stringify!($($($guard)*),*); $result}),*
       }
   },
}
*/

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

    let x1 = Some(23);
    let y1 = mymatch3! {
    [x1]
       Some(10) => "Ten".to_string(),
       Some(n) if {n == 20, n == 21} => "twice Ten A".to_string(),
       n if {let Some(m) = n} => {println!("{}", m); m.to_string()},
     _ => "something else".to_string()
    };
    println!("y1={:?}", y1);

    let y1 = mymatch4! {
    [x1]
       Some(10) => "Ten".to_string(),
       Some(n) if n == 20 => "twice Ten A".to_string(),
       Some(n) if @{n == 22} => "twice Ten @A".to_string(),
       Some(n) if @{let 23 = n} => {let x=5; format!("{:?} {}", "twice Ten @A".to_string(),x)},
       Some(n) if {
           let abc = || n == 24;
           abc() } => "twice Ten A 23".to_string(),
       n if @{let Some(m) = n, m == 23} => {println!("m is {}", m); m.to_string()},
     _ => "something else".to_string(),
    };
    println!("y1={:?}", y1);

    let y2 = match x1 {
        Some(10) => "Ten".to_string(),
        Some(n) if n == 20 && n == 21 => "twice Ten A".to_string(),
        n if {
            #[allow(unused_variables)]
            if true {
                if let Some(m) = n {
                    if let Some(k) = n {
                        true
                    } else {
                        true
                    }
                } else {
                    false
                }
            } else {
                false
            }
        } =>
        {
            if let Some(m) = n {
                m.to_string()
            } else {
                panic!("unreachable")
            }
        }
        _ => "something else".to_string(),
    };
    println!("y2={:?}", y2);

    let x = if_chain::if_chain! {
        if let Some(i) = x1;
        if let 20 = i;
        then { true }
        else {false}
    };
    println!("x={}", x);

    println!("y2={:?}", y2);
}

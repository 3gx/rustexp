pub macro r#match {
    // terminal case, generate match expression
    ((@cases)
     (@obj $($obj:tt)*)
     (@rules $($rules:tt)*)) => {
           match $($obj)* {
               $($rules)*
           }
    },

    // generate terimal guard code for `if let` guard
    (@guard ($then:expr, $else:expr), let $pat:pat = $expr:expr $(,)?) => {
        match $expr { $pat => $then, _ => $else }
    },

    // generate recursive guard for pattern amtching `if let ..' guard
    (@guard ($then:expr, $else:expr), let $pat:pat = $expr:expr, $($tail:tt)*) => {
        match $expr { $pat => r#match!(@guard ($then,$else), $($tail)*), _ => $else }
    },

    // same but w/o let keyword
    (@guard ($then:expr, $else:expr), $pat:pat = $expr:expr $(,)?) => {
        match $expr { $pat => $then, _ => $else }
    },
    (@guard ($then:expr, $else:expr), $pat:pat = $expr:expr, $($tail:tt)*) => {
        match $expr { $pat => r#match!(@guard ($then,$else), $($tail)*), _ => $else }
    },

    // generate terimal guard code for boolean 'if {..}' guard
    (@guard ($then:expr, $_else:expr), true, $(,)?) => {
        $then
    },
    (@guard ($then:expr, $else:expr), $guard:expr $(,)?) => {
        match $guard { true => $then, _ => $else }
    },
    (@guard ($then:expr,$else:expr), true, $($tail:tt)*) => {
        r#match!(@guard ($then,$else), $($tail)*)
    },
    (@guard  ($then:expr,$else:expr), $guard:expr, $($tail:tt)*) => {
       match $guard { true => r#match!(@guard ($then,$else), $($tail)*), _ => $else }
    },

    // recursive cases with 'if @{..}' guard w/ or w/o `{ }` but with `,'
    ((@cases $($pat:pat)|+ $(if @{$($guard:tt)*})? => $result:expr, $($tail:tt)*)
     (@obj $($obj:tt)*)
     (@rules $($rules:tt)*)) => {
           r#match!(
               (@cases $($tail)*)
               (@obj $($obj)*)
               (@rules $($rules)* $($pat)|+
                    $(if {{#![allow(unused_variables)]
                           r#match!(@guard (true,false), $($guard)*) }}
                     )? => {r#match!(@guard ($result, panic!("unreachable")),
                                      true, $($($guard)*)?) },
                          ))
    },

    // recursive cases with 'if @{..}' guard but `{ }` rule w/o `,'
    ((@cases $($pat:pat)|+ $(if @{$($guard:tt)*})? => {$($result:tt)*} $($tail:tt)*)
     (@obj $($obj:tt)*)
     (@rules $($rules:tt)*)) => {
          r#match!(
               (@cases $($pat)|+ $(if @{$($guard)*})? => {$($result)*}, $($tail)*)
               (@obj $($obj)*)
               (@rules $($rules)*))
    },

    // single pattern with 'if @{..}' guard
    ((@cases $($pat:pat)|+ $(if @{$($guard:tt)*})? => $result:expr)
     (@obj $($obj:tt)*)
     (@rules $($rules:tt)*)) => {
          r#match!(
               (@cases $($pat)|+ $(if @{$($guard)*})? => $result,)
               (@obj $($obj)*)
               (@rules $($rules)*))
    },

    // recursive cases with boolean 'if guard'
    ((@cases $($pat:pat)|+ $(if $guard:expr)? => $result:expr, $($tail:tt)*)
     (@obj $($obj:tt)*)
     (@rules $($rules:tt)*)) => {
         r#match!(
               (@cases $($tail)*)
               (@obj $($obj)*)
               (@rules $($rules)* $($pat)|+ $(if $guard)? => $result,))
    },
    /* keep-it, maybe not needed. similar patter above for 'if @{..}' guard w/o ','
    ((@cases $($pat:pat)|+ $(if $guard:expr)? => {$($result:tt)*} $($tail:tt)*)
     (@obj $($obj:tt)*)
     (@rules $($rules:tt)*)) => {
          r#match!(
               (@cases $($pat)|+ $(if $guard)? => {$($result)*}, $($tail)*)
               (@obj $($obj)*)
               (@rules $($rules)*))
    },
    */
    // single pattern with boolean 'if guard'
    ((@cases $($pat:pat)|+ $(if $guard:expr)? => $result:expr)
     (@obj $($obj:tt)*)
     (@rules $($rules:tt)*)) => {
          r#match!(
               (@cases $($pat)|+ $(if $guard)? => $result,)
               (@obj $($obj)*)
               (@rules $($rules)*))
    },

    // entry point: r#match!{ [expr] match }
    ( [ $obj:expr ] $($tail:tt)* ) => {
        r#match!(
            (@cases $($tail)*)
            (@obj $obj)
            (@rules)
        )
    },

    // alterantive entry point: r#match!{ expr, match }
    (  $obj:expr, $($tail:tt)* ) => {
        r#match!(
            (@cases $($tail)*)
            (@obj $obj)
            (@rules)
        )
    }
}

pub macro bx {
    ($($tt:tt)*) => {Box::new($($tt)*)},
}

pub trait IntoTerm<T> {
    fn into_term(&self) -> T;
}

pub macro __mk_op {
    ( (@args) (@expr (@ctor $($ctor:tt)*) $($tt:tt)*) ) => { $($ctor)*($($tt)*) },
    ( (@args $i:ident)  (@expr $($tt:tt)*) ) => {
        __mk_op!((@args)
                 (@expr $($tt)* Box::new(stringify!($i).into_term())))
    },
    ( (@args $e:expr)  (@expr $($tt:tt)*) ) => {
        __mk_op!((@args) (@expr $($tt)* Box::new($e.into_term())))
    },
    ( (@args $i:ident, $($tail:tt)*)  (@expr $($tt:tt)*) ) => {
        __mk_op!((@args $($tail)*)
                 (@expr $($tt)* Box::new(stringify!($i).into_term()),))
    },
    ( (@args $e:expr, $($tail:tt)*)  (@expr $($tt:tt)*) ) => {
        __mk_op!((@args $($tail)*) (@expr $($tt)* Box::new($e.into_term()),))
    },
}

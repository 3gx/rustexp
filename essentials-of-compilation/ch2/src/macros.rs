pub macro r#match {
    // terminal case, generate match expression
    ((@cases)
     (@obj $($obj:tt)*)
     (@rules $($rules:tt)*)) => {
           match $($obj)* {
               $($rules)*
           }
    },

    // inject token-tree with attributes
    (@allow_unused $($tt:tt)*) => {
        #[allow(unused_variables)]
        $($tt)*
    },
    /* carbon_copy */
    (@carbon_copy $($tt:tt)*) => {
        $($tt)*
    },

    // generate terimal guard code for `if let` guard
    (@guard $cb:ident ($then:expr, $else:expr), let $pat:pat = $expr:expr $(,)?) => {
        r#match!(@$cb
            match $expr { $pat => $then, _ => $else }
            //if let $pat = $expr { $then } else {$else }
            )
    },

    // generate recursive guard for pattern amtching `if let ..' guard
    (@guard $cb:ident ($then:expr, $else:expr), let $pat:pat = $expr:expr, $($tail:tt)*) => {
        r#match!(@$cb
//          if let $pat = $expr { r#match!(@guard $cb ($then,$else), $($tail)*) } else { $else }
          match $expr { $pat => r#match!(@guard $cb ($then,$else), $($tail)*), _ => $else }
        )
    },
    (@guard $cb:ident ($then:expr, $else:expr), $pat:pat = $expr:expr $(,)?) => {
        r#match!(@$cb
            match $expr { $pat => $then, _ => $else }
            //if let $pat = $expr { $then } else {$else }
            )
    },
    (@guard $cb:ident ($then:expr, $else:expr), $pat:pat = $expr:expr, $($tail:tt)*) => {
        r#match!(@$cb
//          if let $pat = $expr { r#match!(@guard $cb ($then,$else), $($tail)*) } else { $else }
          match $expr { $pat => r#match!(@guard $cb ($then,$else), $($tail)*), _ => $else }
        )
    },

    // generate terimal guard code for boolean 'if {..}' guard
    (@guard $cb:ident ($then:expr, $_else:expr), true, $(,)?) => {
        $then
    },
    (@guard $cb:ident ($then:expr, $else:expr), $guard:expr $(,)?) => {
        //if $guard { $then } else { $else }
        match $guard { true => $then, _ => $else }
    },
    (@guard $cb:ident ($then:expr,$else:expr), true, $($tail:tt)*) => {
        r#match!(@guard $cb ($then,$else), $($tail)*)
    },
    (@guard $cb:ident ($then:expr,$else:expr), $guard:expr, $($tail:tt)*) => {
        match $guard { true => r#match!(@guard $cb ($then,$else), $($tail)*), _ => $else }
//        if $guard { r#match!(@guard $cb ($then,$else), $($tail)*) } else { $else }
    },

    // recursive cases with 'if @{..}' guard w/ or w/o `{ }` but with `,'
    ((@cases $($pat:pat)|+ $(if @{$($guard:tt)*})? => $result:expr, $($tail:tt)*)
     (@obj $($obj:tt)*)
     (@rules $($rules:tt)*)) => {
           r#match!(
               (@cases $($tail)*)
               (@obj $($obj)*)
               (@rules $($rules)* $($pat)|+
                    $(if {r#match!(@guard allow_unused
                                   (true,false), $($guard)*) }
                     )? => {r#match!(@guard carbon_copy
                                      ($result,
                                       panic!("unreachable")),
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

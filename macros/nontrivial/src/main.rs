macro_rules! visit_members1 {
    (
      $( #[$attr:meta] )*
      fn $name:ident(&self $(, $arg_name:ident : $arg_ty:ty)*) $(-> $ret:ty)?;
      $ ( $rest:tt )*
      ) => {};

    (
        $callback:ident;
        $( #[$attr:meta] )*
        fn $name:ident(&self $(, $arg_name:ident : $arg_ty:ty)*) $(-> $ret:ty)?;
        $( $rest:tt )*
      ) => {
        $callback!(
            $( #[$attr] )*
            fn $name(&self, $(, $arg_name : $arg_ty)*) $(-> $ret)?
        );
        visit_members![ $callback; $($rest)* ];
      };
    ($callback:ident;) => {};
    () => {}
}
/*
macro_rules! my_callback {
    ( $($whatever:tt)* ) => {}
}
*/
macro_rules! visit_members {
    (
        $callback:ident;

        $( #[$attr:meta] )*
        fn $name:ident(&self $(, $arg_name:ident : $arg_ty:ty )*) $(-> $ret:ty)?;

        $( $rest:tt )*
    ) => {
        $callback!(
            $( #[$attr] )*
            fn $name(&self $(, $arg_name : $arg_ty )*) $(-> $ret)?
        );

        visit_members! { $callback; $($rest)* }
    };
    (
        $callback:ident;

        $( #[$attr:meta] )*
        fn $name:ident(&mut self $(, $arg_name:ident : $arg_ty:ty )*) $(-> $ret:ty)?;

        $( $rest:tt )*
    ) => {
        $callback!(
            $( #[$attr] )*
            fn $name(&mut self $(, $arg_name : $arg_ty )*) $(-> $ret)?
        );

        visit_members! { $callback; $($rest)* }
    };
    ($callback:ident;) => {};
}

macro_rules! call_via_deref {
    (
        $( #[$attr:meta] )*
        fn $name:ident(&self $(, $arg_name:ident : $arg_ty:ty )*) $(-> $ret:ty)?
    ) => {
        fn $name(&self $(, $arg_name : $arg_ty )*) $(-> $ret)? {
            (**self).$name( $($arg_name),* )
        }
    };
    (
        $( #[$attr:meta] )*
        fn $name:ident(&mut self $(, $arg_name:ident : $arg_ty:ty )*) $(-> $ret:ty)?
    ) => {
        fn $name(&mut self $(, $arg_name : $arg_ty )*) $(-> $ret)? {
            (**self).$name( $($arg_name),* )
        }
    };
}

macro_rules! impl_trait_for_boxed {
    (
        $( #[$attr:meta] )*
        $vis:vis trait $name:ident {
            $( $body:tt )*
        }
    ) => {
        impl<F: $name + ?Sized> $name for Box<F> {
            visit_members!( call_via_deref; $($body)* );
        }
    };
}
#[macro_export]
macro_rules! impl_trait_for_ref {
    (
        $( #[$attr:meta] )*
        $vis:vis trait $name:ident {
            $( $body:tt )*
        }
    ) => {
        impl<'f, F: $name + ?Sized> $name for &'f F {
            visit_members!( call_via_deref; $($body)* );
        }
    };
}

#[macro_export]
macro_rules! impl_trait_for_mut_ref {
    (
        $( #[$attr:meta] )*
        $vis:vis trait $name:ident {
            $( $body:tt )*
        }
    ) => {
        impl<'f, F: $name + ?Sized> $name for &'f mut F {
            visit_members!( call_via_deref; $($body)* );
        }
    };
}
#[macro_export]
macro_rules! trait_with_dyn_impls {
    (
        $( #[$attr:meta] )*
        $vis:vis trait $name:ident { $( $body:tt )* }
    ) => {
        // emit the trait declaration
        $( #[$attr] )*
        $vis trait $name { $( $body )* }

        impl_trait_for_ref! {
            $( #[$attr] )*
            $vis trait $name { $( $body )* }
        }
        impl_trait_for_mut_ref! {
            $( #[$attr] )*
            $vis trait $name { $( $body )* }
        }
        impl_trait_for_boxed! {
            $( #[$attr] )*
            $vis trait $name { $( $body )* }
        }
    };
}

/// Scans through a stream of tokens looking for `&mut self`. If nothing is
/// found a callback is invoked.
macro_rules! search_for_mut_self {
    // if we see `&mut self`, stop and don't invoke the callback
    ($callback:ident!($($callback_args:tt)*); &mut self $($rest:tt)*) => { };
    ($callback:ident!($($callback_args:tt)*); (&mut self $($other_args:tt)*) $($rest:tt)*) => { };

    // haven't found it yet, drop the first item and keep searching
    ($callback:ident!($($callback_args:tt)*); $_head:tt $($tokens:tt)*) => {
        search_for_mut_self!($callback!( $($callback_args)* ); $($tokens)*);

    };
    // we completed without hitting `&mut self`, invoke the callback and exit
    ($callback:ident!($($callback_args:tt)*);) => {
        $callback!( $($callback_args)* )
    }
}

fn main() {
    visit_members1! { fn get_x(&self) -> u32; }
    visit_members1! { fn get_x(&self, x : i32) -> u32; }
    visit_members1! { fn get_x(&self, x : i32, y : impl X()) -> u32; }
    visit_members1!(
        fn get_x(&self, x: i32);
    );
    visit_members![
        stringify;
        #[disallow]
        fn get_x(&self, x: i32) -> i32;
        #[derive(Debug)]
        fn get_y(&self, x: i32) -> i32;
    ];
    visit_members! {
        stringify;
        #[disallow]
        fn get_x(&self, x: i32) -> i32;
    };

    {
        trait GetX {
            fn get_x(&self) -> u32;
        }

        impl GetX for u32 {
            fn get_x(&self) -> u32 {
                *self
            }
        }

        impl GetX for Box<u32> {
            call_via_deref!( fn get_x(&self) -> u32 );
        }

        fn assert_is_get_x<G: GetX>() {}

        assert_is_get_x::<u32>();
        assert_is_get_x::<Box<u32>>();
    }

    {
        trait Foo {
            fn get_x(&self) -> u32;
            fn execute(&self, expression: &str);
        }

        impl Foo for u32 {
            fn get_x(&self) -> u32 {
                unimplemented!()
            }

            fn execute(&self, _expression: &str) {
                unimplemented!()
            }
        }

        impl_trait_for_boxed! {
            trait Foo {
                fn get_x(&self) -> u32;
                fn execute(&self, expression: &str);
            }
        }

        fn assert_is_foo<F: Foo>() {}

        assert_is_foo::<u32>();
        assert_is_foo::<Box<u32>>();
        assert_is_foo::<Box<dyn Foo>>();
    }

    {
        trait Foo {
            fn get_x(&self) -> u32;
            fn execute(&mut self, expression: &str);
        }

        impl_trait_for_boxed! {
            trait Foo {
                fn get_x(&self) -> u32;
                fn execute(&mut self, expression: &str);
            }
        }
    }

    /*
    {
        trait_with_dyn_impls! {
            trait Foo {
                fn get_x(&self) -> u32;
                fn execute(&mut self, expression: &str);
            }
        }

        fn assert_is_foo<F: Foo>() {}

        assert_is_foo::<&dyn Foo>();
        assert_is_foo::<Box<dyn Foo>>();
    }
    */

    {
        search_for_mut_self! {
            compile_error!("This callback shouldn't have been invoked");

            &mut self asdf
        }
        search_for_mut_self! {
            compile_error!("This callback shouldn't have been invoked");

            fn foo(&mut self);
        }
        macro_rules! declare_struct {
            ($name:ident) => {
                struct $name;
            };
        }

        search_for_mut_self! {
            declare_struct!(Foo);

            blah blah ... blah
        }

        // we should have declared Foo as a unit struct
        let _: Foo;
    }
}

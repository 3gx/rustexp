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
}

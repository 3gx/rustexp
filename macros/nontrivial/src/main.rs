macro_rules! visit_members {
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
fn main() {
    visit_members! { fn get_x(&self) -> u32; }
    visit_members! { fn get_x(&self, x : i32) -> u32; }
    visit_members! { fn get_x(&self, x : i32, y : impl X()) -> u32; }
    visit_members!(
        fn get_x(&self, x: i32);
    );
    visit_members![
        #[disallow]
        fn get_x(&self, x: i32) -> i32;
        #[derive(Debug)]
        fn get_y(&self, x: i32) -> i32;
    ];
    visit_members!{
        stringify;
        #[disallow]
        fn get_x(&self, x: i32) -> i32;
    };
}

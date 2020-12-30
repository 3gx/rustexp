macro_rules! visit_members {
    ( $( #[$attr:meta] )*
      fn $name:ident(&self $(, $arg_name:ident : $arg_ty:ty)*) $(-> $ret:ty)?; ) => {};
}
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
    ];
}

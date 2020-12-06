use hello_macro::HelloMacro;
use hello_macro_derive::HelloMacro;

struct Pancakes;

impl HelloMacro for Pancakes {
    fn hello_macro() {
        println!("Hell, Macro! My name is Pancakes");
    }
}

#[derive(HelloMacro)]
struct Pancakes2;


fn main() {
    Pancakes::hello_macro();
    Pancakes2::hello_macro();
}

use hello_macro::HelloMacro;

struct Pancakes;

impl HelloMacro for Pancakes {
    fn hello_macro() {
        println!("Hell, Macro! My name is Pancakes");
    }
}

fn main() {
    Pancakes::hello_macro();
}

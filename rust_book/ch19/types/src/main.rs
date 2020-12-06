type Km = i32;
type Thunk = Box<dyn Fn() + Send + 'static>;

fn takes_long_type(f: Thunk) {
    f()
}

fn returns_long_type() -> Thunk {
    Box::new(|| println!("hi"))
}

fn _bar() -> ! {
    panic!("oops") // type-checks
}

fn main() {
    let x: i32 = 5;
    let y: Km = 10;
    println!("x+y={}", x + y);

    let f: Thunk = returns_long_type();
    takes_long_type(f);

    // _bar(); // panics

    // let _s1 : str = "Hello there!";     // FAIL, size can't be known at compile time
    let _s2: &str = "How's it going"; // OK
}

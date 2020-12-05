use std::ops::Deref;

struct MyBox<T>(T);

impl<T> MyBox<T> {
    fn new(x: T) -> MyBox<T> {
        MyBox(x)
    }
}

impl<T> Deref for MyBox<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.0
    }
}

fn hello(name: &str) {
    println!("Hello, {}!", name);
}

fn main() {
    let x = 5;
    let y = &x;
    let z = Box::new(x);
    let w = MyBox::new(x);

    assert_eq!(5,x);
    assert_eq!(5,*y);
    assert_eq!(5,*z);
    assert_eq!(5,w.0);
    assert_eq!(5,*w);

    let m = MyBox::new(String::from("Rust"));
    hello(&m);
    let m = MyBox::new(String::from("Rusty"));
    hello(&(*m)[..]);
}

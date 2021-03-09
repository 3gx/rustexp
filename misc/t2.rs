fn fun<T, U, F: Fn(T) -> T, G: Fn(U) -> U>(a: T, b: U, f: F, g: G) -> (T, U) {
    (f(a), g(b))
}

trait FUnary {
    fn call<T>(&self, a: T) -> T;
}

struct Id;
impl FUnary for Id {
    fn call<T>(&self, a: T) -> T {
        a
    }
}

fn main() {
    let ret = fun('a', 43, |x| x, |x| x);
    println!("ret= {:?}", ret);
}

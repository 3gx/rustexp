fn gun<T, U, F: FUnary>(a: T, b: U, f: F) -> (T, U) {
    (f.call(a), f.call(b))
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
    let ret = gun('a', 43, Id);
    println!("ret= {:?}", ret);
}

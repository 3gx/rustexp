fn gun<T, U, F: FUnary>(a: T, b: U, f: F) -> (T, U) {
    (f.call(a), f.call(b))
}

trait FUnary {
    fn call<T>(&self, a: T) -> T;
}

struct Fun<T>(T);
impl<T> FUnary for Fun<T> {
    fn call<T1>(&self, a: T1) -> T1 {
        (self.0)(a)
    }
}

fn main() {
    let ret = gun('a', 43, Fun(|x| x));
    println!("ret= {:?}", ret);
}

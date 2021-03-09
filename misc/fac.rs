fn main() {
    struct Fix<'a, T, U> {
        f: &'a dyn Fn(&Fix<'a, T, U>, T) -> U,
    }
    impl<'a, T, U> Fix<'a, T, U> {
        fn call(&self, x: T) -> U {
            (self.f)(self, x)
        }
    }

    let init = 1;
    let fac = Fix {
        f: &|f, n| if n == 1 { init } else { n * f.call(n - 1) },
    };
    println!("fac= {}", fac.call(5));
}

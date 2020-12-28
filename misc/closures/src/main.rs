fn apply(f: &impl Fn(i32) -> i32) -> i32 {
    f(3)
}
struct S<'a> {
    f: &'a dyn Fn(i32) -> i32,
}
impl<'a> S<'a> {
    fn call(&self, x: i32) -> i32 {
        (self.f)(x)
    }
}
struct G<T: Fn(i32) -> i32> {
    f: T,
}
trait Callable {
    fn call(&self, x: i32) -> i32;
}
impl<T: Fn(i32) -> i32> Callable for G<T> {
    fn call(&self, x: i32) -> i32 {
        (self.f)(x)
    }
}

fn fun() -> impl Callable {
    // -> impl Fn(i32) -> i32 {
    let x = 4;
    let g = "Abx".to_string();
    let fun = move |y| {
        println!("{:?}", g);
        x + y
    };
    let gun = fun;
    println!("{}", apply(&gun));
    //  println!("{}", fun(3));
    let s = S { f: &gun };
    println!("{}", s.call(5));
    // println!("{}",g);
    let g = G { f: gun };
    println!("{}", g.call(5));
    g
}

macro_rules! bx {
    ($expr:expr) => {
        Box::new($expr)
    };
}
use std::rc::Rc;
macro_rules! lam {
    ({$($vars:tt),*}  $lam:expr) => {
        { $(let $vars = $vars.clone();)*
          Rc::new($lam)
        }
    };
}
fn gun() {
    let f = |x: &i32| {
        let &x = x;
        move |y: &i32| x + y
    };
    let v;
    {
        let v1 = f(&3);
        v = v1(&4);
    }
    println!("v={}", v);
}

type VFun = Rc<dyn Fn(i32)>;
enum V1 {
    F1(VFun),
    F2(Box<V1>, VFun),
}
macro_rules! lam1 {
    ({$($vars:tt),*}  $lam:expr) => {
        { $(let $vars = $vars.clone();)*
          Rc::new($lam)
        }
    };
}
fn hun() -> Rc<impl Fn(i32) -> i32> {
    let g = &"Abx".to_string();
    let fun = lam![{g} move |y| {println!("rclam{:?}", g); y }];
    fun
}

#[derive(Debug)]
enum Expr {
    I32(i32),
    F32(f32),
    IF32(i32, f32),
}

fn main() {
    let mut v = Vec::<Box<dyn Callable>>::new();
    let x = fun();
    v.push(Box::new(x));
    //let _ :() = x;
    println!("--{}--", v[0].call(7));
    let x = 3;
    match x {
        _ if x == 3 => println!("three"),
        _ => println!("go figure.."),
    }
    gun();
    use Expr::*;
    let i = bx![I32(32)];
    let f = bx![F32(32.0)];
    let g = bx![IF32(32, 32.0)];
    println!("{:?}", i);
    println!("{:?}", f);
    println!("{:?}", g);

    let fun = hun();
    let x = fun(4);
    println!("x= {}", x);

    fn hun2() -> V1 {
        let g = &"fun_factr".to_string();
        let f1 = V1::F1(lam1![{g} move |_| println!("f1: {}", g)]);
        f1
    }
    let _x = hun2();

    fn hun3() -> V1 {
        let g = &"fun_factr".to_string();
        let f1 = V1::F2(
            Box::new(V1::F1(lam1![{g} move |_| println!("f1a: {}", g)])),
            lam1![{g} move |_| println!("f2a: {}", g)],
        );
        f1
    }
    let _x = hun3();

    fn hun4() -> V1 {
        let g = &"fun_factr".to_string();
        let f1 = V1::F1(lam1![{g} move |_| {println!("f1b: {}", g);
            V1::F1(lam1![{g} move |_| println!("f1c: {}", g)]);}]);
        f1
    }
    let _x = hun4();
}

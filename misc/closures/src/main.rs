fn apply(f : &impl Fn(i32) -> i32) -> i32 {
    f(3)
}
struct S<'a> {
    f : &'a dyn Fn(i32) -> i32,
}
impl<'a> S<'a> {
    fn call(&self, x: i32) -> i32 {
        (self.f)(x)
    }
}
struct G<T : Fn(i32) -> i32> {
    f : T
}
trait Callable {
    fn call(&self, x:i32) -> i32;
}
impl<T: Fn(i32)->i32> Callable for G<T> {
    fn call(&self, x: i32) -> i32 {
        (self.f)(x)
    }
}


fn fun() -> impl Callable { // -> impl Fn(i32) -> i32 {
    let x = 4;
    let g = "Abx".to_string();
    let fun = move |y| {println!("{:?}", g); x + y };
    let gun = fun;
    println!("{}", apply(&gun));
  //  println!("{}", fun(3));
    let s = S { f : &gun };
    println!("{}", s.call(5));
   // println!("{}",g);
   let g = G { f : gun };
   println!("{}", g.call(5));
   g
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
        _ => println!("go figure..")
    }
}

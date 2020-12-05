#[derive(Debug)]
enum List {
    Cons(i32, Rc<List>),
    Nil,
}

use crate::List::{Cons, Nil};
use std::rc::Rc;

fn main() {
    let a = Rc::new(Cons(5, Rc::new(Cons(10, Rc::new(Nil)))));
    println!("count after create a = {}", Rc::strong_count(&a));
    println!("weak_count after create a = {}", Rc::weak_count(&a));
    let b = Cons(3, Rc::clone(&a));
    println!("count after create b = {}", Rc::strong_count(&a));
    println!("weak_count after create b = {}", Rc::weak_count(&a));
    {
        let c = Cons(4, Rc::clone(&a));
        println!("c= {:?}", c);
        println!("count after create c = {}", Rc::strong_count(&a));
        println!("weak_count after create c = {}", Rc::weak_count(&a));
    }
    println!("count after c goes out of scope = {}", Rc::strong_count(&a));
    println!(
        "weak_count after c goes out of scope = {}",
        Rc::weak_count(&a)
    );
    println!("a= {:?}", a);
    println!("b= {:?}", b);
}

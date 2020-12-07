use linkedlists::first::List0;

fn main() {
    let list: List0<i32> = List0::Cons(1, Box::new(List0::Cons(2, Box::new(List0::Nil))));
    println!("{:?}", list);
}

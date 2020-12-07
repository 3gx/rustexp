#[derive(Debug)]
pub enum List0<T> {
    Cons(T, Box<List0<T>>),
    Nil,
}

pub struct List {
    head: Link,
}

enum Link {
    Empty,
    More(Box<Node>),
}

struct Node {
    elem: i32,
    next: Link,
}

impl List {
    pub fn new() -> Self {
        List { head: Link::Empty }
    }
}
